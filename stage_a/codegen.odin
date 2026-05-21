package main

import "core:fmt"
import "core:strings"

// C code generator for Stage A. Targets the imperative subset of Qoz-v2 plus
// ADTs and pattern matching. The driver compiles the emitted C with clang,
// linking the tgc-based runtime in stage_a/runtime/.

Closure_Sig :: struct {
    name:    string,                           // typedef name, e.g. "qoz_fn_i32_RET_i32"
    params:  []string,                         // C type spellings of params
    ret:     string,                           // C type spelling of return
}

Closure_Capture :: struct {
    name:   string,
    c_type: string,
}

Closure_Info :: struct {
    id:        int,
    fn_name:   string,                         // "qoz_lifted_<id>"
    env_name:  string,                         // "qoz_env_<id>"
    sig_index: int,                            // index into cg.closure_sigs
    params:    []Closure_Param,
    param_cs:  []string,                       // C type spellings of params
    ret_c:     string,                         // C type spelling of return
    captures:  []Closure_Capture,
    body:      Expr,
}

Mono_Key :: struct {
    fn_name: string,
    mangled: string,
}

Codegen :: struct {
    sb:         strings.Builder,
    tc:         ^Ty_Context,
    locals:     map[string]string,             // local name -> C type spelling
    local_fn_types: map[string]^Type_Fn,       // local name -> function signature, if any
    enums:      map[string]^Decl_Enum,         // enum name -> declaration
    records:    map[string]^Decl_Struct,       // record name -> declaration

    type_param_subst:  map[string]string,      // active during a generic instantiation: T -> C type
    fn_instantiations: map[string]map[string][]Ty,  // fn name -> mangled -> type args
    fn_mangled_emitted: map[string]bool,       // mangled names already emitted
    call_mangled:      map[^Expr_Call]string,  // call expr -> mangled callee name

    mangled_record_decl:  map[string]^Decl_Struct,    // "Pair__int64_t__..." -> decl
    mangled_record_subst: map[string]map[string]string, // "Pair__..." -> T->C type subst
    ty_subst:             map[int]Ty,                 // active during a generic instantiation
    indent_lvl: int,
    cur_ret:    ^Type_Expr,                    // current function's return type
    in_return_ctx: bool,                       // tail expressions become `return ...`

    closures:        [dynamic]Closure_Info,
    closure_id_of:   map[^Expr_Closure]int,    // closure expr -> index in closures
    closure_sigs:    [dynamic]Closure_Sig,
    sig_index_by_key: map[string]int,
    tmp_counter:     int,

    derive_hash_for: map[string]Ty,            // mangled name -> concrete Ty needing auto-hash
    derive_eq_for:   map[string]Ty,            // mangled name -> concrete Ty needing auto-eq

    origin_spans:    map[string]Span,          // mangled fn name -> user call site that triggered the instantiation
    current_mangled: string,                   // mangled name of the body currently being emitted, "" for entry/main
    current_origin:  Span,                     // span to attribute new fn-instantiation registrations to

    errors:     [dynamic]string,
}

Codegen_Result :: struct {
    c_source: string,
    errors:   []string,
}

codegen_file :: proc(f: File, tc: ^Ty_Context, allocator := context.allocator) -> Codegen_Result {
    cg := Codegen{
        sb         = strings.builder_make(allocator),
        tc         = tc,
        locals     = make(map[string]string, allocator),
        local_fn_types = make(map[string]^Type_Fn, allocator),
        enums      = make(map[string]^Decl_Enum, allocator),
        records    = make(map[string]^Decl_Struct, allocator),
        indent_lvl = 0,
        closures        = make([dynamic]Closure_Info, allocator),
        closure_id_of   = make(map[^Expr_Closure]int, allocator),
        closure_sigs    = make([dynamic]Closure_Sig, allocator),
        sig_index_by_key = make(map[string]int, allocator),
        type_param_subst   = make(map[string]string, allocator),
        fn_instantiations  = make(map[string]map[string][]Ty, allocator),
        fn_mangled_emitted = make(map[string]bool, allocator),
        call_mangled       = make(map[^Expr_Call]string, allocator),
        mangled_record_decl  = make(map[string]^Decl_Struct, allocator),
        mangled_record_subst = make(map[string]map[string]string, allocator),
        ty_subst             = make(map[int]Ty, allocator),
        derive_hash_for      = make(map[string]Ty, allocator),
        derive_eq_for        = make(map[string]Ty, allocator),
        origin_spans         = make(map[string]Span, allocator),
        errors     = make([dynamic]string, allocator),
    }
    context.allocator = allocator

    flat := f.decls

    if tc != nil {
        for name, e in tc.enums   do cg.enums[name]   = e
        for name, r in tc.structs do cg.records[name] = r
    } else {
        for d in flat {
            if e, is_enum := d.(^Decl_Enum); is_enum   do cg.enums[e.name]   = e
            if r, is_rec  := d.(^Decl_Struct); is_rec do cg.records[r.name] = r
        }
    }

    gather_closures(&cg, f)
    gather_fn_instantiations(&cg)
    discover_instantiations(&cg, f)

    cg_emit(&cg, "#include \"qoz_runtime.h\"\n\n")

    for _, e in cg.enums {
        cg_emit_enum_forwards(&cg, e)
    }
    cg_emit(&cg, "\n")

    cg_emit_closure_typedefs(&cg)

    for _, r in cg.records {
        cg_emit_record_forwards(&cg, r)
    }
    cg_emit(&cg, "\n")

    cg_emit_all_records_topo(&cg)

    for _, e in cg.enums {
        cg_emit_enum_defs(&cg, e)
    }

    for _, e in cg.enums {
        cg_emit_variant_ctors_all(&cg, e)
    }

    cg_emit_closure_env_structs(&cg)
    cg_emit_closure_fn_protos(&cg)
    cg_emit_closure_fn_bodies(&cg)

    auto_derive_close(&cg)
    cg_emit_auto_derive_protos(&cg)

    for d in flat {
        if fn, is_fn := d.(^Decl_Fn); is_fn {
            if len(fn.type_params) > 0 {
                cg_emit_generic_fn_prototypes(&cg, fn)
            } else {
                cg_emit_fn_prototype(&cg, fn)
                cg_emit(&cg, ";\n")
            }
        }
    }
    cg_emit(&cg, "\n")

    for d in flat {
        if fn, is_fn := d.(^Decl_Fn); is_fn && len(fn.type_params) > 0 {
            cg_emit_generic_fn_bodies(&cg, fn)
        } else {
            cg_decl(&cg, d)
        }
    }

    cg_emit_auto_derive_bodies(&cg)

    return Codegen_Result{
        c_source = strings.to_string(cg.sb),
        errors   = cg.errors[:],
    }
}

// --- Emit helpers ---

cg_emit :: proc(cg: ^Codegen, s: string) {
    strings.write_string(&cg.sb, s)
}

cg_emitf :: proc(cg: ^Codegen, format: string, args: ..any) {
    fmt.sbprintf(&cg.sb, format, ..args)
}

cg_emit_indent :: proc(cg: ^Codegen) {
    for _ in 0..<cg.indent_lvl do cg_emit(cg, "    ")
}

cg_error :: proc(cg: ^Codegen, message: string) {
    if span, ok := cg_current_origin(cg); ok {
        append(&cg.errors, fmt.tprintf("%s:%d:%d: %s", span.file, span.line, span.column, message))
        return
    }
    append(&cg.errors, message)
}

cg_current_origin :: proc(cg: ^Codegen) -> (Span, bool) {
    if cg.current_mangled == "" do return Span{}, false
    span, ok := cg.origin_spans[cg.current_mangled]
    if !ok do return Span{}, false
    if span.file == "" do return Span{}, false
    return span, true
}

// --- Type spelling ---

c_type_of_type_expr :: proc(cg: ^Codegen, t: ^Type_Expr) -> string {
    if t == nil do return "void"
    switch v in t^ {
    case ^Type_Named:
        if len(v.path) == 1 {
            name := v.path[0]
            if sub, ok := cg.type_param_subst[name]; ok do return sub
            if prim, is_prim := primitive_c_name(name); is_prim {
                return prim
            }
            if _, is_enum := cg.enums[name]; is_enum {
                if len(v.args) > 0 {
                    return fmt.tprintf("qoz_%s*", adt_base_from_typeexpr(cg, name, v.args))
                }
                return fmt.tprintf("qoz_%s*", name)
            }
            if _, is_rec := cg.records[name]; is_rec {
                if len(v.args) > 0 {
                    return fmt.tprintf("qoz_%s", adt_base_from_typeexpr(cg, name, v.args))
                }
                return fmt.tprintf("qoz_%s", name)
            }
            return fmt.tprintf("qoz_%s", name)
        }
        return "void"
    case ^Type_Ptr:
        return fmt.tprintf("%s*", c_type_of_type_expr(cg, v.inner))
    case ^Type_Fn:
        param_cs := make([dynamic]string, context.temp_allocator)
        for p in v.params do append(&param_cs, c_type_of_type_expr(cg, p))
        ret_c := "void"
        if v.ret != nil do ret_c = c_type_of_type_expr(cg, v.ret)
        idx := register_signature(cg, param_cs[:], ret_c)
        return cg.closure_sigs[idx].name
    case ^Type_Tuple:
        return "void*"
    case ^Type_Unit:
        return "void"
    }
    return "void"
}

path_c_type :: proc(cg: ^Codegen, segs: []string) -> string {
    if len(segs) == 0 do return "int64_t"
    if len(segs) == 2 {
        if _, is_enum := cg.enums[segs[0]]; is_enum {
            return fmt.tprintf("qoz_%s*", segs[0])
        }
    }
    if t, is_local := cg.locals[segs[0]]; is_local {
        current := t
        for i in 1..<len(segs) {
            current = c_field_type_of(cg, current, segs[i])
        }
        return current
    }
    return "int64_t"
}

c_field_type_of :: proc(cg: ^Codegen, base_c_type: string, field: string) -> string {
    s := base_c_type
    for strings.has_suffix(s, "*") {
        s = s[:len(s)-1]
    }
    if strings.has_prefix(s, "qoz_") {
        name := s[4:]
        if r, ok := cg.records[name]; ok {
            for fld in r.fields {
                if fld.name == field do return c_type_of_type_expr(cg, fld.type)
            }
        }
        if d, ok := cg.mangled_record_decl[name]; ok {
            saved := cg.type_param_subst
            if subst, has := cg.mangled_record_subst[name]; has {
                cg.type_param_subst = subst
            }
            defer cg.type_param_subst = saved
            for fld in d.fields {
                if fld.name == field do return c_type_of_type_expr(cg, fld.type)
            }
        }
    }
    return "int64_t"
}

// --- Generic monomorphisation ---

ty_to_c_type :: proc(cg: ^Codegen, t: Ty) -> string {
    t := t
    if len(cg.ty_subst) > 0 do t = ty_substitute(t, cg.ty_subst)
    switch v in t {
    case ^Ty_Int:
        if v.untyped do return "int64_t"
        if v.signed do return fmt.tprintf("int%d_t", v.width)
        return fmt.tprintf("uint%d_t", v.width)
    case ^Ty_Float:
        if v.untyped do return "double"
        if v.width == 32 do return "float"
        return "double"
    case ^Ty_Bool:   return "bool"
    case ^Ty_Char:   return "char"
    case ^Ty_String:  return "qoz_string"
    case ^Ty_Cstring: return "const char *"
    case ^Ty_Unit, ^Ty_Nil, ^Ty_Error: return "void"
    case ^Ty_Ptr:    return fmt.tprintf("%s*", ty_to_c_type(cg, v.inner))
    case ^Ty_Adt:
        if len(v.args) > 0 {
            return fmt.tprintf("qoz_%s*", adt_mangled_base(cg, v.name, v.args))
        }
        return fmt.tprintf("qoz_%s*", v.name)
    case ^Ty_Record:
        if len(v.args) > 0 {
            return fmt.tprintf("qoz_%s", adt_mangled_base(cg, v.name, v.args))
        }
        return fmt.tprintf("qoz_%s", v.name)
    case ^Ty_Fn:
        param_cs := make([]string, len(v.params))
        for p, i in v.params do param_cs[i] = ty_to_c_type(cg, p)
        ret_c := ty_to_c_type(cg, v.ret)
        idx := register_signature(cg, param_cs, ret_c)
        return cg.closure_sigs[idx].name
    case ^Ty_Tuple:  return "void*"
    case ^Ty_Var:    return "void*"
    }
    return "void"
}

mangle_generic :: proc(base: string, type_args: []Ty, cg: ^Codegen) -> string {
    sb := strings.builder_make()
    strings.write_string(&sb, "qoz_")
    strings.write_string(&sb, base)
    for arg in type_args {
        strings.write_string(&sb, "__")
        strings.write_string(&sb, sanitize_sig_part(ty_to_c_type(cg, arg)))
    }
    return strings.to_string(sb)
}

// After the type checker recorded every call's instantiation, codegen needs
// the *transitive* set: when a generic fn body calls another generic, the
// inner call's type args may still contain Ty_Vars from the outer fn's
// signature. We walk all reachable bodies, substituting outer Ty_Vars with
// the outer instantiation's concrete args, and register every concrete
// instantiation we discover. Iterates to fixed point so chains of generic
// calls (vec_push -> vec_grow -> qoz_realloc) all surface.
discover_instantiations :: proc(cg: ^Codegen, f: File) {
    if cg.tc == nil do return

    for d in f.decls {
        if fn, is_fn := d.(^Decl_Fn); is_fn && len(fn.type_params) == 0 && fn.body != nil {
            empty: map[int]Ty
            discover_in_expr(cg, fn.body, empty)
        }
    }

    visited := make(map[string]bool, context.temp_allocator)
    for {
        added := false
        for fn_name, instances in cg.fn_instantiations {
            for mangled, type_args in instances {
                if visited[mangled] do continue
                visited[mangled] = true
                added = true
                fn := cg.tc.fns[fn_name]
                if fn == nil do continue
                if fn.body == nil do continue
                ty_subst := make(map[int]Ty, context.temp_allocator)
                if ids, ok := cg.tc.fn_type_var_ids[fn_name]; ok {
                    for id, i in ids do if i < len(type_args) do ty_subst[id] = type_args[i]
                }
                saved_origin := cg.current_origin
                if origin, ok := cg.origin_spans[mangled]; ok do cg.current_origin = origin
                discover_in_expr(cg, fn.body, ty_subst)
                cg.current_origin = saved_origin
            }
        }
        if !added do break
    }
}

discover_in_expr :: proc(cg: ^Codegen, e: Expr, ty_subst: map[int]Ty) {
    switch v in e {
    case ^Expr_Call:
        saved_origin := cg.current_origin
        if cg.current_origin.file == "" do cg.current_origin = v.span
        defer cg.current_origin = saved_origin
        if cg.tc != nil {
            if raw_args, has := cg.tc.call_instantiations[v]; has && len(raw_args) > 0 {
                substituted := make([]Ty, len(raw_args))
                for a, i in raw_args do substituted[i] = ty_substitute(a, ty_subst)
                if id, is_id := v.callee.(^Expr_Ident); is_id {
                    if _, in_fns := cg.tc.fns[id.name]; in_fns {
                        register_fn_instantiation(cg, id.name, substituted)
                    } else if enum_name, has_e := cg.tc.call_enum[v]; has_e {
                        intern_adt(cg.tc, enum_name, substituted)
                    }
                } else if path, is_path := v.callee.(^Expr_Path); is_path {
                    if len(path.segs) == 2 {
                        if _, in_enums := cg.tc.enums[path.segs[0]]; in_enums {
                            intern_adt(cg.tc, path.segs[0], substituted)
                        }
                    }
                }
            }
            if id, is_id := v.callee.(^Expr_Ident); is_id && id.name == "hash" && len(v.args) == 1 {
                discover_late_bound_hash(cg, v.args[0], ty_subst)
            }
            if id, is_id := v.callee.(^Expr_Ident); is_id && id.name == "len" && len(v.args) == 1 {
                discover_late_bound_len(cg, v.args[0], ty_subst)
            }
        }
        discover_in_expr(cg, v.callee, ty_subst)
        for a in v.args do discover_in_expr(cg, a, ty_subst)
    case ^Expr_Path:
        if cg.tc != nil {
            if raw_args, has := cg.tc.path_instantiations[v]; has && len(raw_args) > 0 {
                substituted := make([]Ty, len(raw_args))
                for a, i in raw_args do substituted[i] = ty_substitute(a, ty_subst)
                if len(v.segs) == 2 {
                    if _, in_enums := cg.tc.enums[v.segs[0]]; in_enums {
                        intern_adt(cg.tc, v.segs[0], substituted)
                    }
                }
            }
        }
    case ^Expr_Record:
        if cg.tc != nil {
            if t, has := cg.tc.expr_types[e]; has {
                if rt, is_rec := t.(^Ty_Record); is_rec && len(rt.args) > 0 {
                    substituted := make([]Ty, len(rt.args))
                    for a, i in rt.args do substituted[i] = ty_substitute(a, ty_subst)
                    intern_record(cg.tc, rt.name, substituted)
                }
            }
        }
        for fld in v.fields do discover_in_expr(cg, fld.value, ty_subst)
        if v.base != nil do discover_in_expr(cg, v.base, ty_subst)
    case ^Expr_Block:
        for s in v.stmts {
            switch ss in s {
            case ^Stmt_Let:      discover_in_expr(cg, ss.value, ty_subst)
            case ^Stmt_Var:      discover_in_expr(cg, ss.value, ty_subst)
            case ^Stmt_Let_Else:
                discover_in_expr(cg, ss.value, ty_subst)
                if ss.else_block != nil do discover_in_expr(cg, ss.else_block, ty_subst)
            case ^Stmt_Expr:     discover_in_expr(cg, ss.expr, ty_subst)
            }
        }
        if v.tail != nil do discover_in_expr(cg, v.tail, ty_subst)
    case ^Expr_If:
        discover_in_expr(cg, v.cond, ty_subst)
        discover_in_expr(cg, v.then_b, ty_subst)
        if v.else_b != nil do discover_in_expr(cg, v.else_b, ty_subst)
    case ^Expr_Match:
        discover_in_expr(cg, v.scrutinee, ty_subst)
        for arm in v.arms {
            if arm.guard != nil do discover_in_expr(cg, arm.guard, ty_subst)
            discover_in_expr(cg, arm.body, ty_subst)
        }
    case ^Expr_While:
        discover_in_expr(cg, v.cond, ty_subst)
        discover_in_expr(cg, v.body, ty_subst)
    case ^Expr_For:
        discover_in_expr(cg, v.iter, ty_subst)
        discover_in_expr(cg, v.body, ty_subst)
    case ^Expr_Binary:
        saved_bin := cg.current_origin
        if cg.current_origin.file == "" do cg.current_origin = v.span
        defer cg.current_origin = saved_bin
        if cg.tc != nil {
            if disp, has := cg.tc.binary_dispatches[v]; has {
                args := disp.type_args
                if len(args) > 0 {
                    sub := make([]Ty, len(args), context.temp_allocator)
                    for a, i in args do sub[i] = ty_substitute(a, ty_subst)
                    register_fn_instantiation(cg, disp.fn_name, sub)
                }
            } else {
                discover_late_bound_binary(cg, v, ty_subst)
            }
        }
        discover_in_expr(cg, v.lhs, ty_subst)
        discover_in_expr(cg, v.rhs, ty_subst)
    case ^Expr_Unary:
        saved_un := cg.current_origin
        if cg.current_origin.file == "" do cg.current_origin = v.span
        defer cg.current_origin = saved_un
        if cg.tc != nil {
            if disp, has := cg.tc.unary_dispatches[v]; has {
                args := disp.type_args
                if len(args) > 0 {
                    sub := make([]Ty, len(args), context.temp_allocator)
                    for a, i in args do sub[i] = ty_substitute(a, ty_subst)
                    register_fn_instantiation(cg, disp.fn_name, sub)
                }
            }
        }
        discover_in_expr(cg, v.rhs, ty_subst)
    case ^Expr_Assign:
        saved_as := cg.current_origin
        if cg.current_origin.file == "" do cg.current_origin = v.span
        defer cg.current_origin = saved_as
        if cg.tc != nil {
            if disp, has := cg.tc.assign_dispatches[v]; has {
                args := disp.type_args
                if len(args) > 0 {
                    sub := make([]Ty, len(args), context.temp_allocator)
                    for a, i in args do sub[i] = ty_substitute(a, ty_subst)
                    register_fn_instantiation(cg, disp.fn_name, sub)
                }
            }
        }
        discover_in_expr(cg, v.target, ty_subst)
        discover_in_expr(cg, v.value, ty_subst)
    case ^Expr_Field:    discover_in_expr(cg, v.base, ty_subst)
    case ^Expr_Index:
        if cg.tc != nil {
            if disp, has := cg.tc.index_dispatches[v]; has {
                args := disp.type_args
                if len(args) > 0 {
                    sub := make([]Ty, len(args), context.temp_allocator)
                    for a, i in args do sub[i] = ty_substitute(a, ty_subst)
                    register_fn_instantiation(cg, disp.fn_name, sub)
                }
            }
        }
        discover_in_expr(cg, v.base, ty_subst)
        discover_in_expr(cg, v.index, ty_subst)
    case ^Expr_Cast:     discover_in_expr(cg, v.value, ty_subst)
    case ^Expr_New:      discover_in_expr(cg, v.value, ty_subst)
    case ^Expr_Try:      discover_in_expr(cg, v.value, ty_subst)
    case ^Expr_Return:
        if v.value != nil do discover_in_expr(cg, v.value, ty_subst)
    case ^Expr_Defer:    discover_in_expr(cg, v.body, ty_subst)
    case ^Expr_Closure:  discover_in_expr(cg, v.body, ty_subst)
    case ^Expr_Tuple:
        for el in v.elems do discover_in_expr(cg, el, ty_subst)
    case ^Expr_Size_Of:
    case ^Expr_Array_Lit:
        for el in v.elems do discover_in_expr(cg, el, ty_subst)
        if t, has := cg.tc.expr_types[v]; has {
            t2 := ty_substitute(t, ty_subst)
            if rec, ok := t2.(^Ty_Record); ok && rec.name == "Vec" && len(rec.args) == 1 {
                register_fn_instantiation(cg, "vec_make", rec.args)
                register_fn_instantiation(cg, "vec_push", rec.args)
            }
        }
    case ^Expr_Int_Lit, ^Expr_Float_Lit, ^Expr_String_Lit, ^Expr_Char_Lit,
         ^Expr_Bool_Lit, ^Expr_Nil_Lit, ^Expr_Ident:
    }
}

gather_fn_instantiations :: proc(cg: ^Codegen) {
    if cg.tc == nil do return
    for call, type_args in cg.tc.call_instantiations {
        if !args_all_concrete(type_args) do continue
        id, is_id := call.callee.(^Expr_Ident)
        if !is_id do continue
        if _, in_fns := cg.tc.fns[id.name]; !in_fns do continue
        fn_name := id.name
        mangled := mangle_generic(fn_name, type_args, cg)
        cg.call_mangled[call] = mangled
        if _, ok := cg.fn_instantiations[fn_name]; !ok {
            cg.fn_instantiations[fn_name] = make(map[string][]Ty)
        }
        inner := &cg.fn_instantiations[fn_name]
        inner^[mangled] = type_args
    }
}

args_all_concrete :: proc(args: []Ty) -> bool {
    for a in args do if !is_concrete(a) do return false
    return true
}

cg_emit_generic_fn_prototypes :: proc(cg: ^Codegen, fn: ^Decl_Fn) {
    instances, ok := cg.fn_instantiations[fn.name]
    if !ok do return
    for mangled, type_args in instances {
        setup_type_param_subst(cg, fn, type_args)
        cg_emit_one_generic_signature(cg, fn, mangled, type_args)
        cg_emit(cg, ";\n")
        clear(&cg.type_param_subst)
    }
}

cg_emit_generic_fn_bodies :: proc(cg: ^Codegen, fn: ^Decl_Fn) {
    instances, ok := cg.fn_instantiations[fn.name]
    if !ok do return
    for mangled, type_args in instances {
        if cg.fn_mangled_emitted[mangled] do continue
        cg.fn_mangled_emitted[mangled] = true
        cg_emit_one_generic_body(cg, fn, mangled, type_args)
    }
}

// Assumes cg.type_param_subst is already set for this instantiation.
cg_emit_one_generic_signature :: proc(cg: ^Codegen, fn: ^Decl_Fn, mangled: string, type_args: []Ty) {
    if fn.ret == nil {
        cg_emit(cg, "void")
    } else {
        cg_emit(cg, c_type_of_type_expr(cg, fn.ret))
    }
    cg_emit(cg, " ")
    cg_emit(cg, mangled)
    cg_emit(cg, "(")
    for p, i in fn.params {
        if i > 0 do cg_emit(cg, ", ")
        cg_emitf(cg, "%s %s", c_type_of_type_expr(cg, p.type), p.name)
    }
    cg_emit(cg, ")")
}

cg_emit_one_generic_body :: proc(cg: ^Codegen, fn: ^Decl_Fn, mangled: string, type_args: []Ty) {
    setup_type_param_subst(cg, fn, type_args)
    defer clear(&cg.type_param_subst)

    saved_mangled := cg.current_mangled
    cg.current_mangled = mangled
    defer cg.current_mangled = saved_mangled

    cg_emit_one_generic_signature(cg, fn, mangled, type_args)
    cg_emit(cg, " ")

    clear(&cg.locals)
    for p in fn.params {
        cg.locals[p.name] = c_type_of_type_expr(cg, p.type)
    }
    cg.cur_ret = fn.ret
    cg.in_return_ctx = fn.ret != nil
    cg_block_body(cg, fn.body)
    cg_emit(cg, "\n\n")
    cg.in_return_ctx = false
    cg.cur_ret = nil
}

setup_type_param_subst :: proc(cg: ^Codegen, fn: ^Decl_Fn, type_args: []Ty) {
    clear(&cg.type_param_subst)
    clear(&cg.ty_subst)
    for tp, i in fn.type_params {
        if i < len(type_args) {
            cg.type_param_subst[tp] = ty_to_c_type(cg, type_args[i])
        }
    }
    if cg.tc != nil {
        if ids, ok := cg.tc.fn_type_var_ids[fn.name]; ok {
            for id, i in ids {
                if i < len(type_args) do cg.ty_subst[id] = type_args[i]
            }
        }
    }
}

// Apply cg.ty_subst to a list of type args. Used when a generic call inside
// a generic function body has raw Ty_Var args from type checking; the outer
// instantiation pins them to concrete types.
substitute_args :: proc(cg: ^Codegen, args: []Ty) -> []Ty {
    out := make([]Ty, len(args), context.temp_allocator)
    for a, i in args do out[i] = ty_substitute(a, cg.ty_subst)
    return out
}

register_fn_instantiation :: proc(cg: ^Codegen, fn_name: string, type_args: []Ty) -> string {
    mangled := mangle_generic(fn_name, type_args, cg)
    if _, ok := cg.fn_instantiations[fn_name]; !ok {
        cg.fn_instantiations[fn_name] = make(map[string][]Ty)
    }
    inner := &cg.fn_instantiations[fn_name]
    if _, has := inner^[mangled]; !has {
        inner^[mangled] = type_args
    }
    if _, has := cg.origin_spans[mangled]; !has {
        if cg.current_origin.file != "" {
            cg.origin_spans[mangled] = cg.current_origin
        }
    }
    return mangled
}

primitive_c_name :: proc(name: string) -> (string, bool) {
    switch name {
    case "i8":     return "int8_t",   true
    case "i16":    return "int16_t",  true
    case "i32":    return "int32_t",  true
    case "i64":    return "int64_t",  true
    case "u8":     return "uint8_t",  true
    case "u16":    return "uint16_t", true
    case "u32":    return "uint32_t", true
    case "u64":    return "uint64_t", true
    case "f32":    return "float",    true
    case "f64":    return "double",   true
    case "bool":   return "bool",     true
    case "char":   return "char",     true
    case "string":  return "qoz_string",  true
    case "cstring": return "const char *", true
    case "unit":    return "void",        true
    case "void":    return "void",        true
    }
    return "", false
}

// --- ADT codegen ---

cg_emit_enum_forwards :: proc(cg: ^Codegen, e: ^Decl_Enum) {
    if len(e.type_params) == 0 {
        cg_emit_enum_forward_for_base(cg, e.name)
        return
    }
    if cg.tc == nil do return
    if insts, ok := cg.tc.adt_instances[e.name]; ok {
        for _, args in insts {
            base := adt_mangled_base(cg, e.name, args)
            cg_emit_enum_forward_for_base(cg, base)
        }
    }
}

cg_emit_enum_defs :: proc(cg: ^Codegen, e: ^Decl_Enum) {
    if len(e.type_params) == 0 {
        cg_emit_enum_def_for_base(cg, e, e.name, nil)
        return
    }
    if cg.tc == nil do return
    if insts, ok := cg.tc.adt_instances[e.name]; ok {
        for _, args in insts {
            base := adt_mangled_base(cg, e.name, args)
            subst := build_type_param_subst(cg, e.type_params, args)
            cg_emit_enum_def_for_base(cg, e, base, subst)
        }
    }
}

cg_emit_variant_ctors_all :: proc(cg: ^Codegen, e: ^Decl_Enum) {
    if len(e.type_params) == 0 {
        cg_emit_variant_ctors_for_base(cg, e, e.name, nil)
        return
    }
    if cg.tc == nil do return
    if insts, ok := cg.tc.adt_instances[e.name]; ok {
        for _, args in insts {
            base := adt_mangled_base(cg, e.name, args)
            subst := build_type_param_subst(cg, e.type_params, args)
            cg_emit_variant_ctors_for_base(cg, e, base, subst)
        }
    }
}

adt_mangled_base :: proc(cg: ^Codegen, name: string, type_args: []Ty) -> string {
    sb := strings.builder_make()
    strings.write_string(&sb, name)
    for a in type_args {
        strings.write_string(&sb, "__")
        strings.write_string(&sb, sanitize_sig_part(ty_to_c_type(cg, a)))
    }
    return strings.to_string(sb)
}

adt_base_from_typeexpr :: proc(cg: ^Codegen, name: string, args: []^Type_Expr) -> string {
    sb := strings.builder_make()
    strings.write_string(&sb, name)
    for a in args {
        strings.write_string(&sb, "__")
        strings.write_string(&sb, sanitize_sig_part(c_type_of_type_expr(cg, a)))
    }
    return strings.to_string(sb)
}

adt_prefix_from_ty :: proc(cg: ^Codegen, t: Ty) -> string {
    inner := t
    if ptr, is_ptr := t.(^Ty_Ptr); is_ptr do inner = ptr.inner
    if adt, is_adt := inner.(^Ty_Adt); is_adt {
        if len(adt.args) > 0 do return fmt.tprintf("qoz_%s", adt_mangled_base(cg, adt.name, adt.args))
        return fmt.tprintf("qoz_%s", adt.name)
    }
    return ""
}

build_type_param_subst :: proc(cg: ^Codegen, type_params: []string, args: []Ty) -> map[string]string {
    subst := make(map[string]string)
    for tp, i in type_params {
        if i < len(args) do subst[tp] = ty_to_c_type(cg, args[i])
    }
    return subst
}

cg_emit_enum_forward_for_base :: proc(cg: ^Codegen, base: string) {
    cg_emitf(cg, "typedef struct qoz_%s qoz_%s;\n", base, base)
}

cg_emit_enum_def_for_base :: proc(cg: ^Codegen, e: ^Decl_Enum, base: string, subst: map[string]string) {
    saved := cg.type_param_subst
    if subst != nil do cg.type_param_subst = subst
    defer cg.type_param_subst = saved

    cg_emit(cg, "typedef enum ")
    cg_emit(cg, "{\n")
    for v, i in e.variants {
        if i == 0 {
            cg_emitf(cg, "    qoz_%s_%s = 0,\n", base, v.name)
        } else {
            cg_emitf(cg, "    qoz_%s_%s,\n", base, v.name)
        }
    }
    cg_emit(cg, "} ")
    cg_emitf(cg, "qoz_%s_tag;\n\n", base)

    for v in e.variants {
        if v.kind == .Positional && len(v.pos) > 0 {
            cg_emit(cg, "typedef struct {\n")
            for t, i in v.pos {
                cg_emitf(cg, "    %s f%d;\n", c_type_of_type_expr(cg, t), i)
            }
            cg_emit(cg, "} ")
            cg_emitf(cg, "qoz_%s_%s_payload;\n\n", base, v.name)
        } else if v.kind == .Named && len(v.named) > 0 {
            cg_emit(cg, "typedef struct {\n")
            for fld in v.named {
                cg_emitf(cg, "    %s %s;\n", c_type_of_type_expr(cg, fld.type), fld.name)
            }
            cg_emit(cg, "} ")
            cg_emitf(cg, "qoz_%s_%s_payload;\n\n", base, v.name)
        }
    }

    cg_emitf(cg, "struct qoz_%s ", base)
    cg_emit(cg, "{\n")
    cg_emitf(cg, "    qoz_%s_tag tag;\n", base)
    has_any_payload := false
    for v in e.variants {
        if (v.kind == .Positional && len(v.pos) > 0) || (v.kind == .Named && len(v.named) > 0) {
            has_any_payload = true
            break
        }
    }
    if has_any_payload {
        cg_emit(cg, "    union {\n")
        for v in e.variants {
            if v.kind == .Positional && len(v.pos) > 0 {
                cg_emitf(cg, "        qoz_%s_%s_payload %s;\n", base, v.name, v.name)
            } else if v.kind == .Named && len(v.named) > 0 {
                cg_emitf(cg, "        qoz_%s_%s_payload %s;\n", base, v.name, v.name)
            }
        }
        cg_emit(cg, "    } payload;\n")
    }
    cg_emit(cg, "};\n\n")
}

cg_emit_all_records_topo :: proc(cg: ^Codegen) {
    visited := make(map[string]bool, context.temp_allocator)
    // Visit all non-generic records, recursing into deps (including generic instantiations).
    for _, d in cg.records {
        if len(d.type_params) == 0 {
            topo_emit_record_unit(cg, d, nil, d.name, &visited)
        }
    }
    // Emit any remaining generic instantiations that weren't pulled in transitively.
    if cg.tc != nil {
        for _, d in cg.records {
            if len(d.type_params) == 0 do continue
            if insts, ok := cg.tc.record_instances[d.name]; ok {
                for _, args in insts {
                    mangled := adt_mangled_base(cg, d.name, args)
                    topo_emit_record_unit(cg, d, args, mangled, &visited)
                }
            }
        }
    }
}

topo_emit_record_unit :: proc(cg: ^Codegen, d: ^Decl_Struct, type_args: []Ty, mangled: string, visited: ^map[string]bool) {
    if visited[mangled] do return
    visited[mangled] = true
    // Build the type-parameter substitution for this unit.
    params_env := make(map[string]Ty, context.temp_allocator)
    if len(d.type_params) > 0 {
        for tp, i in d.type_params {
            if i < len(type_args) do params_env[tp] = type_args[i]
        }
    }
    // Walk fields and visit value-typed dependencies first.
    for fld in d.fields {
        topo_visit_field_value_deps(cg, fld.type, &params_env, visited)
    }
    // Emit this unit.
    if len(d.type_params) == 0 {
        cg_emit_record_def_for_base(cg, d, d.name, nil)
    } else {
        subst := build_type_param_subst(cg, d.type_params, type_args)
        cg_emit_record_def_for_base(cg, d, mangled, subst)
    }
}

topo_visit_field_value_deps :: proc(cg: ^Codegen, t: ^Type_Expr, params_env: ^map[string]Ty, visited: ^map[string]bool) {
    if t == nil do return
    named, is_named := t^.(^Type_Named)
    if !is_named do return
    if len(named.path) != 1 do return
    name := named.path[0]
    // Resolve a bare type-param to its instantiation.
    if params_env != nil {
        if subst_ty, in_params := params_env[name]; in_params {
            // Substitute type-parameter usage with the actual type.
            if rec, is_rec := subst_ty.(^Ty_Record); is_rec {
                visit_rec(cg, rec.name, rec.args, visited)
            }
            return
        }
    }
    // Direct record reference. Resolve args first.
    args := make([]Ty, len(named.args))
    for a, i in named.args do args[i] = resolve_type(cg.tc, a, params_env)
    if dep, has := cg.records[name]; has {
        if len(dep.type_params) == 0 {
            topo_emit_record_unit(cg, dep, nil, dep.name, visited)
        } else {
            mangled := adt_mangled_base(cg, name, args)
            topo_emit_record_unit(cg, dep, args, mangled, visited)
        }
    }
}

visit_rec :: proc(cg: ^Codegen, name: string, args: []Ty, visited: ^map[string]bool) {
    if dep, has := cg.records[name]; has {
        if len(dep.type_params) == 0 {
            topo_emit_record_unit(cg, dep, nil, dep.name, visited)
        } else {
            mangled := adt_mangled_base(cg, name, args)
            topo_emit_record_unit(cg, dep, args, mangled, visited)
        }
    }
}

topo_sort_records :: proc(cg: ^Codegen) -> []^Decl_Struct {
    visited := make(map[string]bool, context.temp_allocator)
    out := make([dynamic]^Decl_Struct, context.temp_allocator)
    for _, d in cg.records {
        if len(d.type_params) == 0 do topo_visit_record(cg, d, &visited, &out)
    }
    return out[:]
}

topo_visit_record :: proc(cg: ^Codegen, d: ^Decl_Struct, visited: ^map[string]bool, out: ^[dynamic]^Decl_Struct) {
    if visited[d.name] do return
    visited[d.name] = true
    if len(d.type_params) == 0 {
        for fld in d.fields {
            if name, ok := record_field_value_dep(fld.type); ok {
                if dep, has := cg.records[name]; has {
                    topo_visit_record(cg, dep, visited, out)
                }
            }
        }
    }
    append(out, d)
}

record_field_value_dep :: proc(t: ^Type_Expr) -> (string, bool) {
    if t == nil do return "", false
    named, is_named := t^.(^Type_Named)
    if !is_named do return "", false
    if len(named.path) != 1 do return "", false
    return named.path[0], true
}

cg_emit_record_forwards :: proc(cg: ^Codegen, d: ^Decl_Struct) {
    if len(d.type_params) == 0 {
        cg_emitf(cg, "typedef struct qoz_%s qoz_%s;\n", d.name, d.name)
        return
    }
    if cg.tc == nil do return
    if insts, ok := cg.tc.record_instances[d.name]; ok {
        for _, args in insts {
            base := adt_mangled_base(cg, d.name, args)
            cg_emitf(cg, "typedef struct qoz_%s qoz_%s;\n", base, base)
        }
    }
}

cg_emit_record_defs :: proc(cg: ^Codegen, d: ^Decl_Struct) {
    if len(d.type_params) == 0 {
        cg_emit_record_def_for_base(cg, d, d.name, nil)
        return
    }
    if cg.tc == nil do return
    if insts, ok := cg.tc.record_instances[d.name]; ok {
        for _, args in insts {
            base := adt_mangled_base(cg, d.name, args)
            subst := build_type_param_subst(cg, d.type_params, args)
            cg_emit_record_def_for_base(cg, d, base, subst)
        }
    }
}

cg_emit_record_def_for_base :: proc(cg: ^Codegen, d: ^Decl_Struct, base: string, subst: map[string]string) {
    saved := cg.type_param_subst
    if subst != nil do cg.type_param_subst = subst
    defer cg.type_param_subst = saved

    cg_emitf(cg, "struct qoz_%s ", base)
    cg_emit(cg, "{\n")
    for fld in d.fields {
        cg_emitf(cg, "    %s %s;\n", c_type_of_type_expr(cg, fld.type), fld.name)
    }
    cg_emit(cg, "};\n\n")

    cg.mangled_record_decl[base] = d
    if subst != nil do cg.mangled_record_subst[base] = subst
}

cg_emit_variant_ctors_for_base :: proc(cg: ^Codegen, e: ^Decl_Enum, base: string, subst: map[string]string) {
    saved := cg.type_param_subst
    if subst != nil do cg.type_param_subst = subst
    defer cg.type_param_subst = saved

    for v in e.variants {
        cg_emitf(cg, "static qoz_%s *qoz_make_%s_%s(", base, base, v.name)
        if v.kind == .Positional {
            for t, i in v.pos {
                if i > 0 do cg_emit(cg, ", ")
                cg_emitf(cg, "%s f%d", c_type_of_type_expr(cg, t), i)
            }
        } else if v.kind == .Named {
            for fld, i in v.named {
                if i > 0 do cg_emit(cg, ", ")
                cg_emitf(cg, "%s %s", c_type_of_type_expr(cg, fld.type), fld.name)
            }
        }
        cg_emit(cg, ") {\n")
        cg_emitf(cg, "    qoz_%s *p = qoz_alloc(sizeof(qoz_%s));\n", base, base)
        cg_emitf(cg, "    p->tag = qoz_%s_%s;\n", base, v.name)
        if v.kind == .Positional {
            for _, i in v.pos {
                cg_emitf(cg, "    p->payload.%s.f%d = f%d;\n", v.name, i, i)
            }
        } else if v.kind == .Named {
            for fld in v.named {
                cg_emitf(cg, "    p->payload.%s.%s = %s;\n", v.name, fld.name, fld.name)
            }
        }
        cg_emit(cg, "    return p;\n}\n\n")
    }
}

// --- Top-level declarations ---

cg_decl :: proc(cg: ^Codegen, d: Decl) {
    switch v in d {
    case ^Decl_Import:                                       // handled by driver
    case ^Decl_Link:                                         // handled by driver
    case ^Decl_External:
        cg_external(cg, v)
    case ^Decl_Const:
        cg_error(cg, "top-level constants will be added in a later milestone")
    case ^Decl_Struct:
                                                                 // emitted in the type-declaration pass
    case ^Decl_Enum:
                                                            // already emitted in the earlier passes
    case ^Decl_Type_Alias:
        cg_error(cg, "type aliases will be added with the inference milestone")
    case ^Decl_Fn:
        cg_fn(cg, v)
    }
}

cg_external :: proc(cg: ^Codegen, d: ^Decl_External) {
    cg_emit(cg, "extern ")
    cg_emit(cg, c_type_of_type_expr(cg, d.ret))
    cg_emit(cg, " ")
    cg_emit(cg, d.symbol)
    cg_emit(cg, "(")
    for p, i in d.params {
        if i > 0 do cg_emit(cg, ", ")
        cg_emit(cg, c_type_of_type_expr(cg, p.type))
    }
    cg_emit(cg, ");\n")
}

cg_emit_fn_prototype :: proc(cg: ^Codegen, d: ^Decl_Fn) {
    if d.name == "main" {
        cg_emit(cg, "int main(int argc, char **argv)")
        return
    }
    if d.ret == nil {
        cg_emit(cg, "void")
    } else {
        cg_emit(cg, c_type_of_type_expr(cg, d.ret))
    }
    cg_emitf(cg, " qoz_%s(", d.name)
    for p, i in d.params {
        if i > 0 do cg_emit(cg, ", ")
        cg_emitf(cg, "%s %s", c_type_of_type_expr(cg, p.type), p.name)
    }
    cg_emit(cg, ")")
}

cg_fn :: proc(cg: ^Codegen, d: ^Decl_Fn) {
    if d.name == "main" {
        cg_main_fn(cg, d)
        return
    }
    cg_emit_fn_prototype(cg, d)
    cg_emit(cg, " ")

    clear(&cg.locals)
    for p in d.params {
        cg.locals[p.name] = c_type_of_type_expr(cg, p.type)
    }
    cg.cur_ret = d.ret
    cg.in_return_ctx = d.ret != nil
    cg_block_body(cg, d.body)
    cg_emit(cg, "\n\n")
    cg.in_return_ctx = false
    cg.cur_ret = nil
}

cg_main_fn :: proc(cg: ^Codegen, d: ^Decl_Fn) {
    cg_emit(cg, "int main(int argc, char **argv) {\n")
    cg.indent_lvl = 1
    cg_emit_indent(cg); cg_emit(cg, "qoz_set_argv(argc, argv);\n")
    cg_emit_indent(cg); cg_emit(cg, "int qoz_stack_anchor;\n")
    cg_emit_indent(cg); cg_emit(cg, "qoz_init(&qoz_stack_anchor);\n")

    clear(&cg.locals)
    cg.in_return_ctx = false
    cg_block_inline(cg, d.body)

    cg_emit_indent(cg); cg_emit(cg, "qoz_shutdown();\n")
    cg_emit_indent(cg); cg_emit(cg, "return 0;\n")
    cg.indent_lvl = 0
    cg_emit(cg, "}\n")
}

// --- Statements and blocks ---

cg_block_body :: proc(cg: ^Codegen, b: ^Expr_Block) {
    cg_emit(cg, "{\n")
    cg.indent_lvl += 1
    cg_block_inline(cg, b)
    cg.indent_lvl -= 1
    cg_emit_indent(cg); cg_emit(cg, "}")
}

cg_block_inline :: proc(cg: ^Codegen, b: ^Expr_Block) {
    defers := make([dynamic]Expr, context.temp_allocator)
    for s in b.stmts {
        if se, is_se := s.(^Stmt_Expr); is_se {
            if d, is_def := se.expr.(^Expr_Defer); is_def {
                append(&defers, d.body)
                continue
            }
        }
        cg_stmt(cg, s)
    }

    flush_defers :: proc(cg: ^Codegen, defers: [dynamic]Expr) {
        for j := len(defers) - 1; j >= 0; j -= 1 {
            cg_emit_indent(cg)
            cg_expr(cg, defers[j])
            cg_emit(cg, ";\n")
        }
    }

    if b.tail != nil {
        if cg.in_return_ctx && len(defers) > 0 {
            t := infer_expr_c_type(cg, b.tail)
            tmp := fmt.tprintf("_qoz_ret_%d", next_tmp_id(cg))
            cg_emit_indent(cg)
            cg_emitf(cg, "%s %s = ", t, tmp)
            cg_expr(cg, b.tail)
            cg_emit(cg, ";\n")
            flush_defers(cg, defers)
            cg_emit_indent(cg)
            cg_emitf(cg, "return %s;\n", tmp)
        } else if cg.in_return_ctx {
            cg_tail_expr(cg, b.tail)
        } else {
            cg_emit_tail_as_statement(cg, b.tail)
            flush_defers(cg, defers)
        }
    } else {
        flush_defers(cg, defers)
    }
}

cg_stmt :: proc(cg: ^Codegen, s: Stmt) {
    switch v in s {
    case ^Stmt_Let:
        cg_let_stmt(cg, v.name, v.type, v.value, false)
    case ^Stmt_Var:
        cg_let_stmt(cg, v.name, v.type, v.value, true)
    case ^Stmt_Let_Else:
        cg_let_else_stmt(cg, v)
    case ^Stmt_Expr:
        if m, is_match := v.expr.(^Expr_Match); is_match {
            cg_match_as_statement(cg, m)
            return
        }
        if w, is_while := v.expr.(^Expr_While); is_while {
            cg_while_stmt(cg, w)
            return
        }
        if fr, is_for := v.expr.(^Expr_For); is_for {
            cg_for_stmt(cg, fr)
            return
        }
        if ie, is_if := v.expr.(^Expr_If); is_if {
            cg_if_stmt(cg, ie)
            return
        }
        if blk, is_blk := v.expr.(^Expr_Block); is_blk {
            cg_emit_indent(cg)
            cg_block_body(cg, blk)
            cg_emit(cg, "\n")
            return
        }
        cg_emit_indent(cg)
        cg_expr(cg, v.expr)
        cg_emit(cg, ";\n")
    }
}

cg_if_stmt :: proc(cg: ^Codegen, v: ^Expr_If) {
    cg_emit_indent(cg)
    cg_emit(cg, "if (")
    cg_expr(cg, v.cond)
    cg_emit(cg, ") ")
    cg_emit_block_as_statement(cg, v.then_b)
    if v.else_b != nil {
        cg_emit_indent(cg)
        cg_emit(cg, "else ")
        if eb, is_block := v.else_b.(^Expr_Block); is_block {
            cg_emit_block_as_statement(cg, eb)
        } else if ei, is_if := v.else_b.(^Expr_If); is_if {
            cg_emit(cg, "if (")
            cg_expr(cg, ei.cond)
            cg_emit(cg, ") ")
            cg_emit_block_as_statement(cg, ei.then_b)
            if ei.else_b != nil {
                if eb2, ok := ei.else_b.(^Expr_Block); ok {
                    cg_emit_indent(cg)
                    cg_emit(cg, "else ")
                    cg_emit_block_as_statement(cg, eb2)
                } else if ei2, ok := ei.else_b.(^Expr_If); ok {
                    cg_emit_indent(cg)
                    cg_emit(cg, "else ")
                    cg_if_stmt(cg, ei2)
                }
            }
        }
    }
}

cg_emit_block_as_statement :: proc(cg: ^Codegen, b: ^Expr_Block) {
    cg_emit(cg, "{\n")
    cg.indent_lvl += 1
    saved_return_ctx := cg.in_return_ctx
    cg.in_return_ctx = false
    for s in b.stmts {
        cg_stmt(cg, s)
    }
    if b.tail != nil {
        cg_emit_tail_as_statement(cg, b.tail)
    }
    cg.in_return_ctx = saved_return_ctx
    cg.indent_lvl -= 1
    cg_emit_indent(cg)
    cg_emit(cg, "}\n")
}

cg_emit_tail_as_statement :: proc(cg: ^Codegen, e: Expr) {
    if blk, is_blk := e.(^Expr_Block); is_blk {
        cg_emit_indent(cg)
        cg_block_body(cg, blk)
        cg_emit(cg, "\n")
        return
    }
    if ie, is_if := e.(^Expr_If); is_if {
        cg_if_stmt(cg, ie)
        return
    }
    if m, is_match := e.(^Expr_Match); is_match {
        cg_match_as_statement(cg, m)
        return
    }
    if w, is_while := e.(^Expr_While); is_while {
        cg_while_stmt(cg, w)
        return
    }
    if fr, is_for := e.(^Expr_For); is_for {
        cg_for_stmt(cg, fr)
        return
    }
    if _, is_return := e.(^Expr_Return); is_return {
        cg_emit_indent(cg)
        cg_expr(cg, e)
        cg_emit(cg, ";\n")
        return
    }
    cg_emit_indent(cg)
    cg_expr(cg, e)
    cg_emit(cg, ";\n")
}

cg_while_stmt :: proc(cg: ^Codegen, w: ^Expr_While) {
    cg_emit_indent(cg); cg_emit(cg, "while (")
    cg_expr(cg, w.cond)
    cg_emit(cg, ") ")
    saved := cg.in_return_ctx
    cg.in_return_ctx = false
    cg_block_body(cg, w.body)
    cg.in_return_ctx = saved
    cg_emit(cg, "\n")
}

cg_for_stmt :: proc(cg: ^Codegen, fr: ^Expr_For) {
    if b, is_bin := fr.iter.(^Expr_Binary); is_bin && (b.op == .Range || b.op == .Range_Inclusive) {
        op := "<"
        if b.op == .Range_Inclusive do op = "<="
        cg_emit_indent(cg)
        cg_emitf(cg, "for (int64_t %s = ", fr.binding)
        cg_expr(cg, b.lhs)
        cg_emitf(cg, "; %s %s ", fr.binding, op)
        cg_expr(cg, b.rhs)
        cg_emitf(cg, "; %s++) ", fr.binding)
        cg.locals[fr.binding] = "int64_t"
        cg_block_body(cg, fr.body)
        cg_emit(cg, "\n")
        return
    }
    iter_ty := concrete_ty_of(cg, fr.iter)
    if rec, is_rec := iter_ty.(^Ty_Record); is_rec && rec.name == "Vec" && len(rec.args) == 1 {
        elem_c := ty_to_c_type(cg, rec.args[0])
        vec_c  := ty_to_c_type(cg, iter_ty)
        idx := fmt.tprintf("_qoz_fi_%d", next_tmp_id(cg))
        col := fmt.tprintf("_qoz_fc_%d", next_tmp_id(cg))
        cg_emit_indent(cg)
        cg_emit(cg, "{\n")
        cg.indent_lvl += 1
        cg_emit_indent(cg)
        cg_emitf(cg, "%s %s = ", vec_c, col)
        cg_expr(cg, fr.iter)
        cg_emit(cg, ";\n")
        cg_emit_indent(cg)
        cg_emitf(cg, "for (int64_t %s = 0; %s < %s.len; %s++) ", idx, idx, col, idx)
        cg_emit(cg, "{\n")
        cg.indent_lvl += 1
        cg_emit_indent(cg)
        cg_emitf(cg, "%s %s = %s.data[%s];\n", elem_c, fr.binding, col, idx)
        cg.locals[fr.binding] = elem_c
        for s in fr.body.stmts do cg_stmt(cg, s)
        if fr.body.tail != nil do cg_emit_tail_as_statement(cg, fr.body.tail)
        cg.indent_lvl -= 1
        cg_emit_indent(cg); cg_emit(cg, "}\n")
        cg.indent_lvl -= 1
        cg_emit_indent(cg); cg_emit(cg, "}\n")
        return
    }
    cg_error(cg, "for-in over non-range, non-Vec iterators is not yet supported in codegen")
}

cg_let_else_stmt :: proc(cg: ^Codegen, s: ^Stmt_Let_Else) {
    pat, ok := s.pat.(^Pat_Variant)
    if !ok {
        cg_error(cg, "Stage A let-else supports variant patterns only")
        return
    }
    enum_name := pat.path.segs[0]
    variant_name := pat.path.segs[len(pat.path.segs)-1]
    enum_decl, found := cg.enums[enum_name]
    if !found {
        cg_error(cg, fmt.tprintf("let-else: unknown enum `%s`", enum_name))
        return
    }
    variant_decl := find_variant(enum_decl, variant_name)
    if variant_decl == nil {
        cg_error(cg, fmt.tprintf("let-else: `%s` has no variant `%s`", enum_name, variant_name))
        return
    }

    tmp_name := fmt.tprintf("_qoz_le_%d", next_tmp_id(cg))
    tmp_type := fmt.tprintf("qoz_%s*", enum_name)

    cg_emit_indent(cg)
    cg_emitf(cg, "%s %s = ", tmp_type, tmp_name)
    cg_expr(cg, s.value)
    cg_emit(cg, ";\n")

    cg_emit_indent(cg)
    cg_emitf(cg, "if (%s->tag != qoz_%s_%s) ", tmp_name, enum_name, variant_name)
    cg_emit(cg, "{\n")
    cg.indent_lvl += 1
    if s.else_block != nil {
        for sub in s.else_block.stmts do cg_stmt(cg, sub)
        if s.else_block.tail != nil {
            cg_emit_indent(cg)
            cg_expr(cg, s.else_block.tail)
            cg_emit(cg, ";\n")
        }
    }
    cg.indent_lvl -= 1
    cg_emit_indent(cg); cg_emit(cg, "}\n")

    if variant_decl.kind == .Positional {
        for sub_pat, i in pat.pos {
            if b, is_bind := sub_pat.(^Pat_Bind); is_bind {
                c_type := c_type_of_type_expr(cg, variant_decl.pos[i])
                cg_emit_indent(cg)
                cg_emitf(cg, "%s %s = %s->payload.%s.f%d;\n", c_type, b.name, tmp_name, variant_decl.name, i)
                cg.locals[b.name] = c_type
            }
        }
    } else if variant_decl.kind == .Named {
        for nf in pat.named {
            field_index := -1
            for fld, idx in variant_decl.named {
                if fld.name == nf.name { field_index = idx; break }
            }
            if field_index < 0 do continue
            if b, is_bind := nf.pat.(^Pat_Bind); is_bind {
                c_type := c_type_of_type_expr(cg, variant_decl.named[field_index].type)
                cg_emit_indent(cg)
                cg_emitf(cg, "%s %s = %s->payload.%s.%s;\n", c_type, b.name, tmp_name, variant_decl.name, nf.name)
                cg.locals[b.name] = c_type
            }
        }
    }
}

next_tmp_id :: proc(cg: ^Codegen) -> int {
    id := cg.tmp_counter
    cg.tmp_counter += 1
    return id
}

cg_let_stmt :: proc(cg: ^Codegen, name: string, type_ann: ^Type_Expr, value: Expr, is_var: bool) {
    type_str: string
    if type_ann != nil {
        type_str = c_type_of_type_expr(cg, type_ann)
    } else {
        type_str = infer_expr_c_type(cg, value)
    }
    cg.locals[name] = type_str
    cg_emit_indent(cg)
    cg_emit(cg, type_str)
    cg_emit(cg, " ")
    cg_emit(cg, name)
    cg_emit(cg, " = ")
    cg_expr(cg, value)
    cg_emit(cg, ";\n")
}

cg_tail_expr :: proc(cg: ^Codegen, e: Expr) {
    if cg.in_return_ctx {
        if m, is_match := e.(^Expr_Match); is_match {
            cg_match_as_return(cg, m)
            return
        }
        if _, is_return := e.(^Expr_Return); is_return {
            cg_emit_indent(cg)
            cg_expr(cg, e)
            cg_emit(cg, ";\n")
            return
        }
        cg_emit_indent(cg)
        cg_emit(cg, "return ")
        cg_expr(cg, e)
        cg_emit(cg, ";\n")
        return
    }
    if m, is_match := e.(^Expr_Match); is_match {
        cg_match_as_statement(cg, m)
        return
    }
    if w, is_while := e.(^Expr_While); is_while {
        cg_while_stmt(cg, w)
        return
    }
    if fr, is_for := e.(^Expr_For); is_for {
        cg_for_stmt(cg, fr)
        return
    }
    cg_emit_indent(cg)
    cg_expr(cg, e)
    cg_emit(cg, ";\n")
}

// --- Type inference (minimal, codegen-only) ---

infer_expr_c_type :: proc(cg: ^Codegen, e: Expr) -> string {
    #partial switch v in e {
    case ^Expr_Int_Lit:    return "int64_t"
    case ^Expr_Float_Lit:  return "double"
    case ^Expr_String_Lit: return "qoz_string"
    case ^Expr_Bool_Lit:   return "bool"
    case ^Expr_Char_Lit:   return "char"
    case ^Expr_Ident:
        if t, ok := cg.locals[v.name]; ok do return t
        if cg.tc != nil {
            if t, ok := cg.tc.expr_types[e]; ok && !ty_is_error(t) {
                return ty_to_c_type(cg, t)
            }
        }
        return "int64_t"
    case ^Expr_Path:
        return path_c_type(cg, v.segs)
    case ^Expr_Binary:
        return infer_expr_c_type(cg, v.lhs)
    case ^Expr_Cast:
        return c_type_of_type_expr(cg, v.target)
    case ^Expr_New:
        return infer_new_type(cg, v.value)
    case ^Expr_Call:
        if cg.tc != nil {
            if t, ok := cg.tc.expr_types[e]; ok && !ty_is_error(t) {
                return ty_to_c_type(cg, t)
            }
        }
        if path, is_path := v.callee.(^Expr_Path); is_path && len(path.segs) == 2 {
            if _, is_enum := cg.enums[path.segs[0]]; is_enum {
                return fmt.tprintf("qoz_%s*", path.segs[0])
            }
        }
        return "int64_t"
    case ^Expr_Unary:
        if v.op == .Deref {
            inner := infer_expr_c_type(cg, v.rhs)
            if strings.has_suffix(inner, "*") {
                return inner[:len(inner)-1]
            }
        }
        return infer_expr_c_type(cg, v.rhs)
    case ^Expr_Closure:
        if id, ok := cg.closure_id_of[v]; ok {
            return cg.closure_sigs[cg.closures[id].sig_index].name
        }
        return "void"
    case ^Expr_Record:
        if cg.tc != nil {
            if t, has := cg.tc.expr_types[e]; has && !ty_is_error(t) {
                return ty_to_c_type(cg, t)
            }
        }
        if v.type != nil {
            if tn, ok := v.type^.(^Type_Named); ok && len(tn.path) == 1 {
                if len(tn.args) > 0 {
                    return fmt.tprintf("qoz_%s", adt_base_from_typeexpr(cg, tn.path[0], tn.args))
                }
                return fmt.tprintf("qoz_%s", tn.path[0])
            }
        }
        return "void"
    case ^Expr_Index:
        if cg.tc != nil {
            if t, has := cg.tc.expr_types[e]; has && !ty_is_error(t) {
                return ty_to_c_type(cg, t)
            }
        }
        base_t := infer_expr_c_type(cg, v.base)
        if strings.has_suffix(base_t, "*") {
            return base_t[:len(base_t)-1]
        }
        return "int64_t"
    case ^Expr_Field:
        base_t := infer_expr_c_type(cg, v.base)
        s := base_t
        for strings.has_suffix(s, "*") {
            s = s[:len(s)-1]
        }
        if strings.has_prefix(s, "qoz_") {
            name := s[4:]
            if r, ok := cg.records[name]; ok {
                for fld in r.fields {
                    if fld.name == v.name do return c_type_of_type_expr(cg, fld.type)
                }
            }
        }
        return "int64_t"
    }
    if cg.tc != nil {
        if t, ok := cg.tc.expr_types[e]; ok && !ty_is_error(t) {
            return ty_to_c_type(cg, t)
        }
    }
    return "int64_t"
}

infer_new_type :: proc(cg: ^Codegen, inner: Expr) -> string {
    if call, is_call := inner.(^Expr_Call); is_call {
        if path, is_path := call.callee.(^Expr_Path); is_path && len(path.segs) == 2 {
            if _, is_enum := cg.enums[path.segs[0]]; is_enum {
                return fmt.tprintf("qoz_%s*", path.segs[0])
            }
        }
    }
    return fmt.tprintf("%s*", infer_expr_c_type(cg, inner))
}

// --- Expressions ---

cg_expr :: proc(cg: ^Codegen, e: Expr) {
    switch v in e {
    case ^Expr_Int_Lit:
        cg_emit(cg, v.text)
    case ^Expr_Float_Lit:
        cg_emit(cg, v.text)
    case ^Expr_String_Lit:
        if cg.tc != nil && cg.tc.cstring_literals[v] {
            cg_emit(cg, v.text)
        } else {
            cg_emitf(cg, "QOZ_STR_LIT(%s)", v.text)
        }
    case ^Expr_Char_Lit:
        cg_emit(cg, v.text)
    case ^Expr_Bool_Lit:
        cg_emit(cg, v.value ? "true" : "false")
    case ^Expr_Nil_Lit:
        cg_emit(cg, "NULL")
    case ^Expr_Ident:
        if cg.tc != nil {
            if enum_name, ok := cg.tc.ident_enum[v]; ok {
                base := enum_name
                if type_args, ok2 := cg.tc.ident_instantiations[v]; ok2 && len(type_args) > 0 {
                    base = adt_mangled_base(cg, enum_name, type_args)
                }
                cg_emitf(cg, "qoz_make_%s_%s()", base, v.name)
                break
            }
        }
        cg_emit(cg, v.name)
    case ^Expr_Path:
        if len(v.segs) == 2 {
            if _, is_enum := cg.enums[v.segs[0]]; is_enum {
                base := v.segs[0]
                if cg.tc != nil {
                    if type_args, ok := cg.tc.path_instantiations[v]; ok && len(type_args) > 0 {
                        base = adt_mangled_base(cg, v.segs[0], type_args)
                    }
                }
                cg_emitf(cg, "qoz_make_%s_%s()", base, v.segs[1])
                break
            }
        }
        if t, is_local := cg.locals[v.segs[0]]; is_local {
            cg_emit(cg, v.segs[0])
            cur_type := t
            for i in 1..<len(v.segs) {
                if strings.has_suffix(cur_type, "*") {
                    cg_emit(cg, "->")
                } else {
                    cg_emit(cg, ".")
                }
                cg_emit(cg, v.segs[i])
                cur_type = c_field_type_of(cg, cur_type, v.segs[i])
            }
            break
        }
        for s, i in v.segs {
            if i > 0 do cg_emit(cg, "_")
            cg_emit(cg, s)
        }
    case ^Expr_Binary:
        if cg.tc != nil {
            if disp, has := cg.tc.binary_dispatches[v]; has {
                cg_emit_operator_binary_call(cg, v, disp)
                break
            }
            if cg_try_emit_late_bound_binary(cg, v) {
                break
            }
        }
        cg_emit(cg, "(")
        cg_expr(cg, v.lhs)
        cg_emit(cg, " ")
        cg_emit(cg, binary_op_c_str(v.op))
        cg_emit(cg, " ")
        cg_expr(cg, v.rhs)
        cg_emit(cg, ")")
    case ^Expr_Unary:
        if cg.tc != nil {
            if disp, has := cg.tc.unary_dispatches[v]; has {
                cg_emit_operator_unary_call(cg, v, disp)
                break
            }
        }
        cg_emit(cg, unary_op_c_str(v.op))
        cg_expr(cg, v.rhs)
    case ^Expr_Assign:
        if cg.tc != nil {
            if disp, has := cg.tc.assign_dispatches[v]; has {
                cg_emit_operator_assign_call(cg, v, disp)
                break
            }
        }
        cg_expr(cg, v.target)
        cg_emit(cg, " ")
        cg_emit(cg, assign_op_c_str(v.op))
        cg_emit(cg, " ")
        cg_expr(cg, v.value)
    case ^Expr_Cast:
        cg_emit(cg, "((")
        cg_emit(cg, c_type_of_type_expr(cg, v.target))
        cg_emit(cg, ")")
        cg_expr(cg, v.value)
        cg_emit(cg, ")")
    case ^Expr_Field:
        cg_expr(cg, v.base)
        if strings.has_suffix(infer_expr_c_type(cg, v.base), "*") {
            cg_emit(cg, "->")
        } else {
            cg_emit(cg, ".")
        }
        cg_emit(cg, v.name)
    case ^Expr_Call:
        cg_call(cg, v)
    case ^Expr_New:
        cg_new(cg, v)
    case ^Expr_Index:
        if cg.tc != nil {
            if disp, has := cg.tc.index_dispatches[v]; has {
                args := disp.type_args
                if len(args) > 0 && len(cg.ty_subst) > 0 {
                    sub := make([]Ty, len(args), context.temp_allocator)
                    for a, i in args do sub[i] = ty_substitute(a, cg.ty_subst)
                    args = sub
                }
                mangled := disp.fn_name
                if len(args) > 0 {
                    mangled = mangle_generic(disp.fn_name, args, cg)[len("qoz_"):]
                    register_fn_instantiation(cg, disp.fn_name, args)
                }
                cg_emitf(cg, "qoz_%s(&(", mangled)
                cg_expr(cg, v.base)
                cg_emit(cg, "), ")
                cg_expr(cg, v.index)
                cg_emit(cg, ")")
                break
            }
        }
        cg_expr(cg, v.base)
        cg_emit(cg, "[")
        cg_expr(cg, v.index)
        cg_emit(cg, "]")
    case ^Expr_Return:
        if v.value != nil {
            cg_emit(cg, "return ")
            cg_expr(cg, v.value)
        } else {
            cg_emit(cg, "return")
        }
    case ^Expr_Closure:
        cg_emit_closure_value(cg, v)
    case ^Expr_Record:
        cg_emit_record_literal(cg, v)
    case ^Expr_Try:
        cg_emit_try(cg, v)
    case ^Expr_Size_Of:
        c_type := c_type_of_type_expr(cg, v.target)
        cg_emitf(cg, "(int64_t)sizeof(%s)", c_type)
    case ^Expr_Array_Lit:
        cg_emit_array_lit(cg, v)
    case ^Expr_If:
        cg_emit_if_expression(cg, v)
    case ^Expr_Match:
        cg_emit_match_as_expression(cg, v)
    case ^Expr_Defer:
        cg_error(cg, "`defer` is only allowed as a statement, not in expression position")
        cg_emit(cg, "/*defer*/0")
    case ^Expr_Tuple,
         ^Expr_Block, ^Expr_While, ^Expr_For:
        cg_error(cg, fmt.tprintf("expression kind will be added in a later milestone"))
        cg_emit(cg, "/*unsupported*/0")
    }
}

cg_emit_if_expression :: proc(cg: ^Codegen, v: ^Expr_If) {
    cg_emit(cg, "((")
    cg_expr(cg, v.cond)
    cg_emit(cg, ") ? (")
    cg_emit_block_as_expr(cg, v.then_b)
    cg_emit(cg, ") : (")
    if v.else_b != nil {
        if eb, is_block := v.else_b.(^Expr_Block); is_block {
            cg_emit_block_as_expr(cg, eb)
        } else if ei, is_if := v.else_b.(^Expr_If); is_if {
            cg_emit_if_expression(cg, ei)
        } else {
            cg_expr(cg, v.else_b)
        }
    } else {
        cg_emit(cg, "(void)0")
    }
    cg_emit(cg, "))")
}

cg_emit_block_as_expr :: proc(cg: ^Codegen, b: ^Expr_Block) {
    if len(b.stmts) == 0 {
        if b.tail != nil {
            cg_expr(cg, b.tail)
            return
        }
        cg_emit(cg, "(void)0")
        return
    }
    cg_emit(cg, "({ ")
    for s in b.stmts {
        cg_stmt(cg, s)
    }
    if b.tail != nil {
        cg_expr(cg, b.tail)
        cg_emit(cg, "; ")
    }
    cg_emit(cg, "})")
}

cg_emit_try :: proc(cg: ^Codegen, t: ^Expr_Try) {
    inner_prefix := ""
    if cg.tc != nil {
        if ty, ok := cg.tc.expr_types[t.value]; ok {
            if adt, is_adt := ty.(^Ty_Adt); is_adt && adt.name == "Result" && len(adt.args) == 2 {
                inner_prefix = adt_mangled_base(cg, "Result", adt.args)
            }
        }
    }
    if inner_prefix == "" {
        cg_error(cg, "`?` codegen: operand type unresolved")
        cg_emit(cg, "0")
        return
    }

    outer_prefix := ""
    if cg.cur_ret != nil {
        if tn, ok := cg.cur_ret^.(^Type_Named); ok && len(tn.path) == 1 && tn.path[0] == "Result" && len(tn.args) == 2 {
            outer_prefix = adt_base_from_typeexpr(cg, "Result", tn.args)
        }
    }
    if outer_prefix == "" {
        cg_error(cg, "`?` codegen: enclosing function does not return Result")
        cg_emit(cg, "0")
        return
    }

    tmp := fmt.tprintf("_qoz_try_%d", next_tmp_id(cg))
    cg_emit(cg, "({ ")
    cg_emitf(cg, "qoz_%s *%s = ", inner_prefix, tmp)
    cg_expr(cg, t.value)
    cg_emit(cg, "; ")
    cg_emitf(cg, "if (%s->tag == qoz_%s_Err) ", tmp, inner_prefix)
    cg_emit(cg, "{ ")
    cg_emitf(cg, "return qoz_make_%s_Err(%s->payload.Err.f0); ", outer_prefix, tmp)
    cg_emit(cg, "} ")
    cg_emitf(cg, "%s->payload.Ok.f0; ", tmp)
    cg_emit(cg, "})")
}

discover_late_bound_binary :: proc(cg: ^Codegen, v: ^Expr_Binary, ty_subst: map[int]Ty) {
    op_str := ""
    switch v.op {
    case .Add: op_str = "+"
    case .Sub: op_str = "-"
    case .Mul: op_str = "*"
    case .Div: op_str = "/"
    case .Mod: op_str = "%"
    case .Eq:  op_str = "=="
    case .Ne:  op_str = "!="
    case .Lt:  op_str = "<"
    case .Gt:  op_str = ">"
    case .Le:  op_str = "<="
    case .Ge:  op_str = ">="
    case .And, .Or, .Range, .Range_Inclusive:
        return
    }
    raw, has := cg.tc.expr_types[v.lhs]
    if !has do return
    t := ty_substitute(raw, ty_subst)
    container, ok := operand_container_name(t)
    if !ok do return
    key := Operator_Key{op = op_str, type_name = container}
    if fn_name, found := cg.tc.operator_table[key]; found {
        type_args := operand_type_args(t)
        if len(type_args) > 0 {
            register_fn_instantiation(cg, fn_name, type_args)
        }
        return
    }
    if op_str == "==" || op_str == "!=" {
        if rec, is_rec := t.(^Ty_Record); is_rec {
            mangled := auto_type_mangled(cg, rec.name, rec.args)
            cg.derive_eq_for[mangled] = t
        }
    }
}

discover_late_bound_len :: proc(cg: ^Codegen, arg: Expr, ty_subst: map[int]Ty) {
    raw, has := cg.tc.expr_types[arg]
    if !has do return
    t := ty_substitute(raw, ty_subst)
    container, ok := operand_container_name(t)
    if !ok do return
    key := Operator_Key{op = "len", type_name = container}
    fn_name, found := cg.tc.operator_table[key]
    if !found do return
    type_args := operand_type_args(t)
    if len(type_args) > 0 {
        register_fn_instantiation(cg, fn_name, type_args)
    }
}

discover_late_bound_hash :: proc(cg: ^Codegen, arg: Expr, ty_subst: map[int]Ty) {
    raw, has := cg.tc.expr_types[arg]
    if !has do return
    t := ty_substitute(raw, ty_subst)
    container, ok := operand_container_name(t)
    if !ok {
        return
    }
    key := Operator_Key{op = "hash", type_name = container}
    if fn_name, found := cg.tc.operator_table[key]; found {
        type_args := operand_type_args(t)
        if len(type_args) > 0 {
            register_fn_instantiation(cg, fn_name, type_args)
        }
        return
    }
    if rec, is_rec := t.(^Ty_Record); is_rec {
        mangled := auto_type_mangled(cg, rec.name, rec.args)
        cg.derive_hash_for[mangled] = t
    }
}

cg_try_emit_late_bound_binary :: proc(cg: ^Codegen, v: ^Expr_Binary) -> bool {
    op_str := ""
    switch v.op {
    case .Add: op_str = "+"
    case .Sub: op_str = "-"
    case .Mul: op_str = "*"
    case .Div: op_str = "/"
    case .Mod: op_str = "%"
    case .Eq:  op_str = "=="
    case .Ne:  op_str = "!="
    case .Lt:  op_str = "<"
    case .Gt:  op_str = ">"
    case .Le:  op_str = "<="
    case .Ge:  op_str = ">="
    case .And, .Or, .Range, .Range_Inclusive:
        return false
    }
    lhs_ty := concrete_ty_of(cg, v.lhs)
    if lhs_ty == nil do return false
    container, ok := operand_container_name(lhs_ty)
    if !ok do return false
    key := Operator_Key{op = op_str, type_name = container}
    if fn_name, found := cg.tc.operator_table[key]; found {
        type_args := operand_type_args(lhs_ty)
        mangled := fn_name
        if len(type_args) > 0 {
            mangled = mangle_generic(fn_name, type_args, cg)[len("qoz_"):]
            register_fn_instantiation(cg, fn_name, type_args)
        }
        cg_emitf(cg, "qoz_%s(&(", mangled)
        cg_expr(cg, v.lhs)
        cg_emit(cg, "), &(")
        cg_expr(cg, v.rhs)
        cg_emit(cg, "))")
        return true
    }
    if op_str == "==" || op_str == "!=" {
        if rec, is_rec := lhs_ty.(^Ty_Record); is_rec {
            mangled := auto_type_mangled(cg, rec.name, rec.args)
            cg.derive_eq_for[mangled] = lhs_ty
            if op_str == "!=" do cg_emit(cg, "(!")
            cg_emitf(cg, "qoz_auto_eq__%s(&(", mangled)
            cg_expr(cg, v.lhs)
            cg_emit(cg, "), &(")
            cg_expr(cg, v.rhs)
            cg_emit(cg, "))")
            if op_str == "!=" do cg_emit(cg, ")")
            return true
        }
        if adt, is_adt := lhs_ty.(^Ty_Adt); is_adt {
            cg_error(cg, fmt.tprintf(
                "no `%s` for `%s`. ADTs are not auto-derived. Register one with `@operator(\"%s\") let your_eq(a: *%s, b: *%s): bool = ...`",
                op_str, ty_to_string(lhs_ty), op_str, adt.name, adt.name))
            cg_emit(cg, "false")
            return true
        }
    }
    return false
}

concrete_ty_of :: proc(cg: ^Codegen, e: Expr) -> Ty {
    if cg.tc == nil do return nil
    t, has := cg.tc.expr_types[e]
    if !has do return nil
    if len(cg.ty_subst) > 0 do t = ty_substitute(t, cg.ty_subst)
    return t
}

auto_type_mangled :: proc(cg: ^Codegen, name: string, type_args: []Ty) -> string {
    if len(type_args) == 0 do return name
    return adt_mangled_base(cg, name, type_args)
}

auto_derive_close :: proc(cg: ^Codegen) {
    for {
        added := false
        for _, t in cg.derive_hash_for {
            if auto_expand_record(cg, t, &cg.derive_hash_for) do added = true
        }
        for _, t in cg.derive_eq_for {
            if auto_expand_record(cg, t, &cg.derive_eq_for) do added = true
        }
        if !added do break
    }
}

auto_expand_record :: proc(cg: ^Codegen, t: Ty, table: ^map[string]Ty) -> bool {
    rec, ok := t.(^Ty_Record)
    if !ok do return false
    decl, has := cg.records[rec.name]
    if !has do return false
    params_env := make(map[string]Ty, context.temp_allocator)
    for tp, i in decl.type_params {
        if i < len(rec.args) do params_env[tp] = rec.args[i]
    }
    added := false
    for fld in decl.fields {
        fty := resolve_type(cg.tc, fld.type, &params_env)
        if frec, is_rec := fty.(^Ty_Record); is_rec {
            mangled := auto_type_mangled(cg, frec.name, frec.args)
            if _, in_table := table[mangled]; !in_table {
                table[mangled] = fty
                added = true
            }
        }
    }
    return added
}

cg_emit_auto_derive_protos :: proc(cg: ^Codegen) {
    for mangled, t in cg.derive_hash_for {
        if user_has_hash_for(cg, t) do continue
        cg_emitf(cg, "static uint64_t qoz_auto_hash__%s(qoz_%s* p);\n", mangled, mangled)
    }
    for mangled, t in cg.derive_eq_for {
        if user_has_eq_for(cg, t) do continue
        cg_emitf(cg, "static bool qoz_auto_eq__%s(qoz_%s* a, qoz_%s* b);\n", mangled, mangled, mangled)
    }
    cg_emit(cg, "\n")
}

user_has_hash_for :: proc(cg: ^Codegen, t: Ty) -> bool {
    container, ok := operand_container_name(t)
    if !ok do return false
    _, found := cg.tc.operator_table[Operator_Key{op = "hash", type_name = container}]
    return found
}

user_has_eq_for :: proc(cg: ^Codegen, t: Ty) -> bool {
    container, ok := operand_container_name(t)
    if !ok do return false
    _, found := cg.tc.operator_table[Operator_Key{op = "==", type_name = container}]
    return found
}

cg_emit_auto_derive_bodies :: proc(cg: ^Codegen) {
    for mangled, t in cg.derive_hash_for {
        if user_has_hash_for(cg, t) do continue
        cg_emit_auto_hash_body(cg, mangled, t)
    }
    for mangled, t in cg.derive_eq_for {
        if user_has_eq_for(cg, t) do continue
        cg_emit_auto_eq_body(cg, mangled, t)
    }
}

cg_emit_auto_hash_body :: proc(cg: ^Codegen, mangled: string, t: Ty) {
    rec, ok := t.(^Ty_Record)
    if !ok do return
    decl, has := cg.records[rec.name]
    if !has do return
    params_env := make(map[string]Ty, context.temp_allocator)
    for tp, i in decl.type_params {
        if i < len(rec.args) do params_env[tp] = rec.args[i]
    }
    cg_emitf(cg, "static uint64_t qoz_auto_hash__%s(qoz_%s* p) ", mangled, mangled)
    cg_emit(cg, "{\n")
    cg_emit(cg, "    uint64_t h = 14695981039346656037ULL;\n")
    for fld in decl.fields {
        fty := resolve_type(cg.tc, fld.type, &params_env)
        cg_emit(cg, "    h = (h ^ ")
        cg_emit_auto_hash_field(cg, fld.name, fty)
        cg_emit(cg, ") * 1099511628211ULL;\n")
    }
    cg_emit(cg, "    return h;\n")
    cg_emit(cg, "}\n\n")
}

cg_emit_auto_hash_field :: proc(cg: ^Codegen, field_name: string, fty: Ty) {
    if _, is_str := fty.(^Ty_String); is_str {
        cg_emitf(cg, "qoz_string_hash(p->%s)", field_name)
        return
    }
    if rec, is_rec := fty.(^Ty_Record); is_rec {
        m := auto_type_mangled(cg, rec.name, rec.args)
        if user_has_hash_for(cg, fty) {
            type_args := rec.args
            user_name := cg.tc.operator_table[Operator_Key{op = "hash", type_name = rec.name}]
            mangled_user := user_name
            if len(type_args) > 0 {
                mangled_user = mangle_generic(user_name, type_args, cg)[len("qoz_"):]
                register_fn_instantiation(cg, user_name, type_args)
            }
            cg_emitf(cg, "qoz_%s(&p->%s)", mangled_user, field_name)
            return
        }
        cg_emitf(cg, "qoz_auto_hash__%s(&p->%s)", m, field_name)
        return
    }
    cg_emitf(cg, "((uint64_t)(p->%s))", field_name)
}

cg_emit_auto_eq_body :: proc(cg: ^Codegen, mangled: string, t: Ty) {
    rec, ok := t.(^Ty_Record)
    if !ok do return
    decl, has := cg.records[rec.name]
    if !has do return
    params_env := make(map[string]Ty, context.temp_allocator)
    for tp, i in decl.type_params {
        if i < len(rec.args) do params_env[tp] = rec.args[i]
    }
    cg_emitf(cg, "static bool qoz_auto_eq__%s(qoz_%s* a, qoz_%s* b) ", mangled, mangled, mangled)
    cg_emit(cg, "{\n")
    for fld in decl.fields {
        fty := resolve_type(cg.tc, fld.type, &params_env)
        cg_emit(cg, "    if (!(")
        cg_emit_auto_eq_field(cg, fld.name, fty)
        cg_emit(cg, ")) return false;\n")
    }
    cg_emit(cg, "    return true;\n")
    cg_emit(cg, "}\n\n")
}

cg_emit_auto_eq_field :: proc(cg: ^Codegen, field_name: string, fty: Ty) {
    if _, is_str := fty.(^Ty_String); is_str {
        cg_emitf(cg, "qoz_string_eq(a->%s, b->%s)", field_name, field_name)
        return
    }
    if rec, is_rec := fty.(^Ty_Record); is_rec {
        m := auto_type_mangled(cg, rec.name, rec.args)
        if user_has_eq_for(cg, fty) {
            type_args := rec.args
            user_name := cg.tc.operator_table[Operator_Key{op = "==", type_name = rec.name}]
            mangled_user := user_name
            if len(type_args) > 0 {
                mangled_user = mangle_generic(user_name, type_args, cg)[len("qoz_"):]
                register_fn_instantiation(cg, user_name, type_args)
            }
            cg_emitf(cg, "qoz_%s(&a->%s, &b->%s)", mangled_user, field_name, field_name)
            return
        }
        cg_emitf(cg, "qoz_auto_eq__%s(&a->%s, &b->%s)", m, field_name, field_name)
        return
    }
    cg_emitf(cg, "(a->%s == b->%s)", field_name, field_name)
}

cg_emit_len :: proc(cg: ^Codegen, arg: Expr) {
    t := concrete_ty_of(cg, arg)
    if t == nil {
        cg_error(cg, "len: cannot resolve argument type")
        cg_emit(cg, "((int64_t)0)")
        return
    }
    if _, is_str := t.(^Ty_String); is_str {
        cg_emit(cg, "((")
        cg_expr(cg, arg)
        cg_emit(cg, ").len)")
        return
    }
    container, ok := operand_container_name(t)
    if !ok {
        cg_error(cg, fmt.tprintf("len: unsupported type %s", ty_to_string(t)))
        cg_emit(cg, "((int64_t)0)")
        return
    }
    key := Operator_Key{op = "len", type_name = container}
    fn_name, found := cg.tc.operator_table[key]
    if !found {
        cg_error(cg, fmt.tprintf("no `len` for `%s`. Register one with `@operator(\"len\")`", ty_to_string(t)))
        cg_emit(cg, "((int64_t)0)")
        return
    }
    type_args := operand_type_args(t)
    mangled := fn_name
    if len(type_args) > 0 {
        mangled = mangle_generic(fn_name, type_args, cg)[len("qoz_"):]
        register_fn_instantiation(cg, fn_name, type_args)
    }
    cg_emitf(cg, "qoz_%s(&(", mangled)
    cg_expr(cg, arg)
    cg_emit(cg, "))")
}

cg_emit_hash :: proc(cg: ^Codegen, arg: Expr) {
    t := concrete_ty_of(cg, arg)
    if t == nil {
        cg_error(cg, "hash: cannot resolve argument type")
        cg_emit(cg, "((uint64_t)0)")
        return
    }
    if user_emit_hash_call(cg, arg, t) do return
    if auto_emit_hash_call(cg, arg, t) do return
    cg_error(cg, hash_missing_message(t))
    cg_emit(cg, "((uint64_t)0)")
}

hash_missing_message :: proc(t: Ty) -> string {
    type_str := ty_to_string(t)
    if adt, ok := t.(^Ty_Adt); ok {
        return fmt.tprintf(
            "no `hash` for `%s`. ADTs are not auto-derived. Register one with `@operator(\"hash\") let your_hash(p: *%s): u64 = ...`",
            type_str, adt.name)
    }
    return fmt.tprintf("no `hash` for `%s`. Register one with `@operator(\"hash\")`", type_str)
}

user_emit_hash_call :: proc(cg: ^Codegen, arg: Expr, t: Ty) -> bool {
    container, ok := operand_container_name(t)
    if !ok do return false
    key := Operator_Key{op = "hash", type_name = container}
    fn_name, found := cg.tc.operator_table[key]
    if !found do return false
    type_args := operand_type_args(t)
    mangled := fn_name
    if len(type_args) > 0 {
        mangled = mangle_generic(fn_name, type_args, cg)[len("qoz_"):]
        register_fn_instantiation(cg, fn_name, type_args)
    }
    cg_emitf(cg, "qoz_%s(&(", mangled)
    cg_expr(cg, arg)
    cg_emit(cg, "))")
    return true
}

auto_emit_hash_call :: proc(cg: ^Codegen, arg: Expr, t: Ty) -> bool {
    #partial switch _ in t {
    case ^Ty_Int, ^Ty_Float, ^Ty_Bool, ^Ty_Char, ^Ty_Cstring, ^Ty_Ptr:
        cg_emit(cg, "((uint64_t)(")
        cg_expr(cg, arg)
        cg_emit(cg, "))")
        return true
    }
    if rec, ok := t.(^Ty_Record); ok {
        mangled := auto_type_mangled(cg, rec.name, rec.args)
        cg.derive_hash_for[mangled] = t
        cg_emitf(cg, "qoz_auto_hash__%s(&(", mangled)
        cg_expr(cg, arg)
        cg_emit(cg, "))")
        return true
    }
    return false
}

cg_emit_array_lit :: proc(cg: ^Codegen, v: ^Expr_Array_Lit) {
    t, ok := cg.tc.expr_types[v]
    if !ok {
        cg_error(cg, "array literal: cannot resolve type")
        cg_emit(cg, "/* err */0")
        return
    }
    if len(cg.ty_subst) > 0 do t = ty_substitute(t, cg.ty_subst)
    rec, is_rec := t.(^Ty_Record)
    if !is_rec || rec.name != "Vec" || len(rec.args) != 1 {
        cg_error(cg, "array literal: not a Vec<T>")
        cg_emit(cg, "/* err */0")
        return
    }
    elem_args := rec.args
    register_fn_instantiation(cg, "vec_make", elem_args)
    register_fn_instantiation(cg, "vec_push", elem_args)
    vec_c := ty_to_c_type(cg, t)
    make_mangled := mangle_generic("vec_make", elem_args, cg)
    push_mangled := mangle_generic("vec_push", elem_args, cg)
    tmp := fmt.tprintf("_qoz_arr_%d", next_tmp_id(cg))
    cg_emit(cg, "({ ")
    cg_emitf(cg, "%s %s = %s(); ", vec_c, tmp, make_mangled)
    for el in v.elems {
        cg_emitf(cg, "%s(&%s, ", push_mangled, tmp)
        cg_expr(cg, el)
        cg_emit(cg, "); ")
    }
    cg_emitf(cg, "%s; })", tmp)
}

cg_emit_size_of :: proc(cg: ^Codegen, arg: Expr) {
    name := ""
    if id, ok := arg.(^Expr_Ident); ok do name = id.name
    if p, ok := arg.(^Expr_Path); ok && len(p.segs) >= 1 do name = p.segs[0]

    type_str := ""
    if name != "" {
        if sub, has := cg.type_param_subst[name]; has {
            type_str = sub
        } else if prim, has := primitive_c_name(name); has {
            type_str = prim
        } else if _, has := cg.enums[name]; has {
            type_str = fmt.tprintf("qoz_%s*", name)
        } else if _, has := cg.records[name]; has {
            type_str = fmt.tprintf("qoz_%s", name)
        }
    }
    if type_str == "" {
        cg_error(cg, fmt.tprintf("size_of: unknown type `%s`", name))
        cg_emit(cg, "(int64_t)0")
        return
    }
    cg_emitf(cg, "(int64_t)sizeof(%s)", type_str)
}

cg_emit_record_literal :: proc(cg: ^Codegen, r: ^Expr_Record) {
    base_name := ""
    if cg.tc != nil {
        if t, has := cg.tc.expr_types[r]; has {
            if rt, is_rec := t.(^Ty_Record); is_rec {
                args := rt.args
                if len(args) > 0 && len(cg.ty_subst) > 0 {
                    substituted := make([]Ty, len(args), context.temp_allocator)
                    for a, i in args do substituted[i] = ty_substitute(a, cg.ty_subst)
                    args = substituted
                }
                if len(args) > 0 {
                    base_name = adt_mangled_base(cg, rt.name, args)
                    intern_record(cg.tc, rt.name, args)
                } else {
                    base_name = rt.name
                }
            }
        }
    }
    if base_name == "" && r.type != nil {
        if tn, ok := r.type^.(^Type_Named); ok && len(tn.path) == 1 {
            if len(tn.args) > 0 {
                base_name = adt_base_from_typeexpr(cg, tn.path[0], tn.args)
            } else {
                base_name = tn.path[0]
            }
        }
    }
    if base_name == "" {
        cg_error(cg, "record literal without a resolved type")
        cg_emit(cg, "/*record*/0")
        return
    }
    if r.base != nil {
        cg_emit(cg, "({ ")
        cg_emitf(cg, "qoz_%s _qr = ", base_name)
        cg_expr(cg, r.base)
        cg_emit(cg, "; ")
        for fld in r.fields {
            cg_emitf(cg, "_qr.%s = ", fld.name)
            cg_expr(cg, fld.value)
            cg_emit(cg, "; ")
        }
        cg_emit(cg, "_qr; })")
        return
    }
    cg_emit(cg, "((")
    cg_emitf(cg, "qoz_%s", base_name)
    cg_emit(cg, ")")
    cg_emit(cg, "{ ")
    for fld, i in r.fields {
        if i > 0 do cg_emit(cg, ", ")
        cg_emitf(cg, ".%s = ", fld.name)
        cg_expr(cg, fld.value)
    }
    cg_emit(cg, " })")
}

cg_emit_closure_value :: proc(cg: ^Codegen, ce: ^Expr_Closure) {
    id, ok := cg.closure_id_of[ce]
    if !ok {
        cg_error(cg, "internal: closure not registered")
        cg_emit(cg, "/*missing-closure*/")
        return
    }
    info := cg.closures[id]
    sig := cg.closure_sigs[info.sig_index]

    cg_emit(cg, "((")
    cg_emit(cg, sig.name)
    cg_emit(cg, "){ .env = ")
    if len(info.captures) > 0 {
        cg_emit(cg, "({ ")
        cg_emitf(cg, "%s *e = qoz_alloc(sizeof(%s)); ", info.env_name, info.env_name)
        for c in info.captures {
            cg_emitf(cg, "e->%s = %s; ", c.name, c.name)
        }
        cg_emit(cg, "(void*)e; })")
    } else {
        cg_emit(cg, "NULL")
    }
    cg_emitf(cg, ", .fn = %s ", info.fn_name)
    cg_emit(cg, "})")
}

cg_emit_operator_binary_call :: proc(cg: ^Codegen, v: ^Expr_Binary, disp: Index_Dispatch) {
    args := disp.type_args
    if len(args) > 0 && len(cg.ty_subst) > 0 {
        sub := make([]Ty, len(args), context.temp_allocator)
        for a, i in args do sub[i] = ty_substitute(a, cg.ty_subst)
        args = sub
    }
    mangled := disp.fn_name
    if len(args) > 0 {
        mangled = mangle_generic(disp.fn_name, args, cg)[len("qoz_"):]
        register_fn_instantiation(cg, disp.fn_name, args)
    }
    cg_emitf(cg, "qoz_%s(&(", mangled)
    cg_expr(cg, v.lhs)
    cg_emit(cg, "), &(")
    cg_expr(cg, v.rhs)
    cg_emit(cg, "))")
}

cg_emit_operator_assign_call :: proc(cg: ^Codegen, v: ^Expr_Assign, disp: Index_Dispatch) {
    idx, _ := v.target.(^Expr_Index)
    args := disp.type_args
    if len(args) > 0 && len(cg.ty_subst) > 0 {
        sub := make([]Ty, len(args), context.temp_allocator)
        for a, i in args do sub[i] = ty_substitute(a, cg.ty_subst)
        args = sub
    }
    mangled := disp.fn_name
    if len(args) > 0 {
        mangled = mangle_generic(disp.fn_name, args, cg)[len("qoz_"):]
        register_fn_instantiation(cg, disp.fn_name, args)
    }
    cg_emitf(cg, "qoz_%s(&(", mangled)
    cg_expr(cg, idx.base)
    cg_emit(cg, "), ")
    cg_expr(cg, idx.index)
    cg_emit(cg, ", ")
    cg_expr(cg, v.value)
    cg_emit(cg, ")")
}

cg_emit_operator_unary_call :: proc(cg: ^Codegen, v: ^Expr_Unary, disp: Index_Dispatch) {
    args := disp.type_args
    if len(args) > 0 && len(cg.ty_subst) > 0 {
        sub := make([]Ty, len(args), context.temp_allocator)
        for a, i in args do sub[i] = ty_substitute(a, cg.ty_subst)
        args = sub
    }
    mangled := disp.fn_name
    if len(args) > 0 {
        mangled = mangle_generic(disp.fn_name, args, cg)[len("qoz_"):]
        register_fn_instantiation(cg, disp.fn_name, args)
    }
    cg_emitf(cg, "qoz_%s(&(", mangled)
    cg_expr(cg, v.rhs)
    cg_emit(cg, "))")
}

cg_new :: proc(cg: ^Codegen, n: ^Expr_New) {
    if call, is_call := n.value.(^Expr_Call); is_call {
        if path, is_path := call.callee.(^Expr_Path); is_path && len(path.segs) == 2 {
            if _, is_enum := cg.enums[path.segs[0]]; is_enum {
                cg_call(cg, call)
                return
            }
        }
    }
    cg_error(cg, "`new` on non-variant values will be added in a later milestone")
    cg_emit(cg, "NULL")
}

cg_call :: proc(cg: ^Codegen, call: ^Expr_Call) {
    if path, is_path := call.callee.(^Expr_Path); is_path {
        if len(path.segs) == 2 && path.segs[0] == "fmt" && path.segs[1] == "println" {
            cg_fmt_println(cg, call.args)
            return
        }
        if len(path.segs) == 2 && path.segs[0] == "fmt" && path.segs[1] == "format" {
            cg_fmt_format(cg, call.args)
            return
        }
        if len(path.segs) == 2 {
            if _, is_enum := cg.enums[path.segs[0]]; is_enum {
                base := path.segs[0]
                if cg.tc != nil {
                    if type_args, ok := cg.tc.call_instantiations[call]; ok && len(type_args) > 0 {
                        base = adt_mangled_base(cg, path.segs[0], type_args)
                    }
                }
                cg_emitf(cg, "qoz_make_%s_%s(", base, path.segs[1])
                for a, i in call.args {
                    if i > 0 do cg_emit(cg, ", ")
                    cg_expr(cg, a)
                }
                cg_emit(cg, ")")
                return
            }
            if cg.tc != nil {
                if _, in_modules := cg.tc.packages[path.segs[0]]; in_modules {
                    qualified := fmt.tprintf("%s_%s", path.segs[0], path.segs[1])
                    fake_id := new(Expr_Ident, context.temp_allocator)
                    fake_id.span = path.span; fake_id.name = qualified
                    saved := call.callee
                    call.callee = fake_id
                    cg_call(cg, call)
                    call.callee = saved
                    return
                }
            }
        }
    }
    if id, is_id := call.callee.(^Expr_Ident); is_id {
        if id.name == "println" {
            cg_fmt_println(cg, call.args)
            return
        }
        if id.name == "size_of" && len(call.args) == 1 {
            cg_emit_size_of(cg, call.args[0])
            return
        }
        if id.name == "hash" && len(call.args) == 1 {
            cg_emit_hash(cg, call.args[0])
            return
        }
        if id.name == "len" && len(call.args) == 1 {
            cg_emit_len(cg, call.args[0])
            return
        }
        if cg.tc != nil {
            if enum_name, ok := cg.tc.call_enum[call]; ok {
                base := enum_name
                if type_args, ok2 := cg.tc.call_instantiations[call]; ok2 && len(type_args) > 0 {
                    base = adt_mangled_base(cg, enum_name, type_args)
                }
                cg_emitf(cg, "qoz_make_%s_%s(", base, id.name)
                for a, i in call.args {
                    if i > 0 do cg_emit(cg, ", ")
                    cg_expr(cg, a)
                }
                cg_emit(cg, ")")
                return
            }
        }
        if cg.tc != nil {
            if ext, ok := cg.tc.externs[id.name]; ok {
                cg_emitf(cg, "%s(", ext.symbol)
                for a, i in call.args {
                    if i > 0 do cg_emit(cg, ", ")
                    cg_expr(cg, a)
                }
                cg_emit(cg, ")")
                return
            }
        }
        if local_type, ok := cg.locals[id.name]; ok && strings.has_prefix(local_type, "qoz_fn_") {
            cg_emitf(cg, "%s.fn(%s.env", id.name, id.name)
            for a in call.args {
                cg_emit(cg, ", ")
                cg_expr(cg, a)
            }
            cg_emit(cg, ")")
            return
        }
        callee_name := id.name
        if cg.tc != nil {
            if raw_args, has := cg.tc.call_instantiations[call]; has && len(raw_args) > 0 {
                substituted := substitute_args(cg, raw_args)
                mangled := register_fn_instantiation(cg, id.name, substituted)
                callee_name = mangled[len("qoz_"):]
            } else if mangled, ok := cg.call_mangled[call]; ok {
                callee_name = mangled[len("qoz_"):]
            }
        } else if mangled, ok := cg.call_mangled[call]; ok {
            callee_name = mangled[len("qoz_"):]
        }
        cg_emitf(cg, "qoz_%s(", callee_name)
        for a, i in call.args {
            if i > 0 do cg_emit(cg, ", ")
            cg_expr(cg, a)
        }
        cg_emit(cg, ")")
        return
    }
    cg_expr(cg, call.callee)
    cg_emit(cg, "(")
    for a, i in call.args {
        if i > 0 do cg_emit(cg, ", ")
        cg_expr(cg, a)
    }
    cg_emit(cg, ")")
}

// --- Pattern matching ---

cg_match_as_statement :: proc(cg: ^Codegen, m: ^Expr_Match) {
    enum_name := enum_of_scrutinee(cg, m.scrutinee)
    if enum_name == "" {
        cg_error(cg, "match scrutinee must resolve to an ADT in Stage A")
        return
    }
    eff_scrut := materialise_scrutinee(cg, m.scrutinee)
    cg_emit_indent(cg); cg_emit(cg, "switch (")
    cg_match_scrutinee_tag(cg, eff_scrut)
    cg_emit(cg, ") {\n")
    cg.indent_lvl += 1
    for arm in m.arms {
        cg_match_arm_statement(cg, eff_scrut, enum_name, arm)
    }
    cg.indent_lvl -= 1
    cg_emit_indent(cg); cg_emit(cg, "}\n")
}

// If the scrutinee is not already a simple identifier, bind it to a fresh
// temporary so per-arm payload access has a stable name to use. The synthetic
// ident inherits the scrutinee's resolved type via cg.locals and (for ADT
// arg recovery) via tc.expr_types.
materialise_scrutinee :: proc(cg: ^Codegen, scrut: Expr) -> Expr {
    if _, is_id := scrut.(^Expr_Ident); is_id do return scrut
    if u, is_un := scrut.(^Expr_Unary); is_un && u.op == .Deref {
        if _, is_id := u.rhs.(^Expr_Ident); is_id do return scrut
    }
    tmp := fmt.tprintf("_qoz_s_%d", next_tmp_id(cg))
    scrut_c_type := infer_expr_c_type(cg, scrut)
    cg_emit_indent(cg)
    cg_emitf(cg, "%s %s = ", scrut_c_type, tmp)
    cg_expr(cg, scrut)
    cg_emit(cg, ";\n")
    cg.locals[tmp] = scrut_c_type

    synth := new(Expr_Ident)
    synth.name = tmp
    synth.span = expr_span(scrut)
    if cg.tc != nil {
        if t, ok := cg.tc.expr_types[scrut]; ok {
            cg.tc.expr_types[synth] = t
        }
    }
    return synth
}

cg_match_arm_statement :: proc(cg: ^Codegen, scrut: Expr, enum_name: string, arm: Match_Arm) {
    pat, ok := arm.pat.(^Pat_Variant)
    if !ok {
        cg_error(cg, "Stage A supports only variant patterns in match arms")
        return
    }
    variant_name := pat.path.segs[len(pat.path.segs)-1]
    prefix := fmt.tprintf("qoz_%s", enum_name)
    if cg.tc != nil {
        if t, has := cg.tc.expr_types[scrut]; has {
            if p := adt_prefix_from_ty(cg, t); p != "" do prefix = p
        }
    }

    cg_emit_indent(cg)
    cg_emitf(cg, "case %s_%s: ", prefix, variant_name)
    cg_emit(cg, "{\n")
    cg.indent_lvl += 1

    variant_decl: ^Variant_Decl
    if enum_decl, ok2 := cg.enums[enum_name]; ok2 {
        variant_decl = find_variant(enum_decl, variant_name)
    }
    if variant_decl != nil {
        cg_emit_variant_bindings(cg, scrut, enum_name, variant_decl, pat)
    }

    saved_locals := make(map[string]string, context.temp_allocator)
    for k, val in cg.locals do saved_locals[k] = val

    if blk, is_blk := arm.body.(^Expr_Block); is_blk {
        cg_block_body(cg, blk)
        cg_emit(cg, "\n")
    } else {
        cg_emit_indent(cg)
        cg_expr(cg, arm.body)
        cg_emit(cg, ";\n")
    }
    cg_emit_indent(cg); cg_emit(cg, "break;\n")

    clear(&cg.locals)
    for k, val in saved_locals do cg.locals[k] = val

    cg.indent_lvl -= 1
    cg_emit_indent(cg); cg_emit(cg, "}\n")
}

cg_emit_match_as_expression :: proc(cg: ^Codegen, m: ^Expr_Match) {
    enum_name := enum_of_scrutinee(cg, m.scrutinee)
    if enum_name == "" {
        cg_error(cg, "match scrutinee must resolve to an ADT in Stage A")
        cg_emit(cg, "0")
        return
    }
    result_c := "int64_t"
    if cg.tc != nil {
        if t, has := cg.tc.expr_types[m]; has && !ty_is_error(t) {
            result_c = ty_to_c_type(cg, t)
        }
    }
    tmp_res := fmt.tprintf("_qoz_mv_%d", next_tmp_id(cg))
    tmp_scrut := fmt.tprintf("_qoz_ms_%d", next_tmp_id(cg))
    scrut_c := infer_expr_c_type(cg, m.scrutinee)

    cg_emit(cg, "({ ")
    cg_emitf(cg, "%s %s = ", scrut_c, tmp_scrut)
    cg_expr(cg, m.scrutinee)
    cg_emit(cg, "; ")
    cg_emitf(cg, "%s %s; ", result_c, tmp_res)

    synth := new(Expr_Ident, context.temp_allocator)
    synth.name = tmp_scrut
    synth.span = expr_span(m.scrutinee)
    if cg.tc != nil {
        if t, ok := cg.tc.expr_types[m.scrutinee]; ok {
            cg.tc.expr_types[synth] = t
        }
    }
    cg.locals[tmp_scrut] = scrut_c

    cg_emit(cg, "switch (")
    cg_match_scrutinee_tag(cg, synth)
    cg_emit(cg, ") ")
    cg_emit(cg, "{ ")
    for arm in m.arms {
        cg_match_arm_expression(cg, synth, enum_name, arm, tmp_res)
    }
    cg_emit(cg, "} ")
    cg_emitf(cg, "%s; ", tmp_res)
    cg_emit(cg, "})")
}

cg_match_arm_expression :: proc(cg: ^Codegen, scrut: Expr, enum_name: string, arm: Match_Arm, result_tmp: string) {
    pat, ok := arm.pat.(^Pat_Variant)
    if !ok {
        cg_error(cg, "Stage A supports only variant patterns in match arms")
        return
    }
    variant_name := pat.path.segs[len(pat.path.segs)-1]
    prefix := fmt.tprintf("qoz_%s", enum_name)
    if cg.tc != nil {
        if t, has := cg.tc.expr_types[scrut]; has {
            if p := adt_prefix_from_ty(cg, t); p != "" do prefix = p
        }
    }
    cg_emitf(cg, "case %s_%s: ", prefix, variant_name)
    cg_emit(cg, "{ ")

    variant_decl: ^Variant_Decl
    if enum_decl, ok2 := cg.enums[enum_name]; ok2 {
        variant_decl = find_variant(enum_decl, variant_name)
    }
    saved_locals := make(map[string]string, context.temp_allocator)
    for k, val in cg.locals do saved_locals[k] = val
    if variant_decl != nil {
        cg_emit_variant_bindings(cg, scrut, enum_name, variant_decl, pat)
    }

    cg_emitf(cg, "%s = ", result_tmp)
    cg_expr(cg, arm.body)
    cg_emit(cg, "; break; } ")

    clear(&cg.locals)
    for k, val in saved_locals do cg.locals[k] = val
}

cg_match_as_return :: proc(cg: ^Codegen, m: ^Expr_Match) {
    enum_name := enum_of_scrutinee(cg, m.scrutinee)
    if enum_name == "" {
        cg_error(cg, "match scrutinee must resolve to an ADT in Stage A")
        cg_emit_indent(cg); cg_emit(cg, "return 0;\n")
        return
    }

    eff_scrut := materialise_scrutinee(cg, m.scrutinee)
    cg_emit_indent(cg); cg_emit(cg, "switch (")
    cg_match_scrutinee_tag(cg, eff_scrut)
    cg_emit(cg, ") {\n")
    cg.indent_lvl += 1
    for arm in m.arms {
        cg_match_arm_return(cg, eff_scrut, enum_name, arm)
    }
    cg.indent_lvl -= 1
    cg_emit_indent(cg); cg_emit(cg, "}\n")
    cg_emit_indent(cg); cg_emit(cg, "__builtin_unreachable();\n")
}

cg_match_scrutinee_tag :: proc(cg: ^Codegen, scrut: Expr) {
    if u, is_un := scrut.(^Expr_Unary); is_un && u.op == .Deref {
        cg_expr(cg, u.rhs)
        cg_emit(cg, "->tag")
        return
    }
    t := infer_expr_c_type(cg, scrut)
    if strings.has_suffix(t, "*") {
        cg_emit(cg, "(")
        cg_expr(cg, scrut)
        cg_emit(cg, ")->tag")
        return
    }
    cg_emit(cg, "(")
    cg_expr(cg, scrut)
    cg_emit(cg, ").tag")
}

enum_of_scrutinee :: proc(cg: ^Codegen, scrut: Expr) -> string {
    if cg.tc != nil {
        if t, has := cg.tc.expr_types[scrut]; has {
            inner := t
            if ptr, is_ptr := t.(^Ty_Ptr); is_ptr do inner = ptr.inner
            if adt, is_adt := inner.(^Ty_Adt); is_adt {
                if _, ok := cg.enums[adt.name]; ok do return adt.name
            }
        }
    }
    s := infer_expr_c_type(cg, scrut)
    for strings.has_suffix(s, "*") {
        s = s[:len(s)-1]
    }
    if strings.has_prefix(s, "qoz_") {
        name := s[4:]
        if _, ok := cg.enums[name]; ok do return name
    }
    return ""
}

cg_match_arm_return :: proc(cg: ^Codegen, scrut: Expr, enum_name: string, arm: Match_Arm) {
    pat, ok := arm.pat.(^Pat_Variant)
    if !ok {
        cg_error(cg, "Stage A supports only variant patterns in match arms")
        return
    }
    variant_name := pat.path.segs[len(pat.path.segs)-1]
    prefix := fmt.tprintf("qoz_%s", enum_name)
    if cg.tc != nil {
        if t, has := cg.tc.expr_types[scrut]; has {
            if p := adt_prefix_from_ty(cg, t); p != "" do prefix = p
        }
    }

    cg_emit_indent(cg)
    cg_emitf(cg, "case %s_%s: ", prefix, variant_name)
    cg_emit(cg, "{\n")
    cg.indent_lvl += 1

    variant_decl := find_variant(cg.enums[enum_name], variant_name)
    if variant_decl != nil {
        cg_emit_variant_bindings(cg, scrut, enum_name, variant_decl, pat)
    }

    saved_locals_snapshot: map[string]string
    saved_locals_snapshot = make(map[string]string, context.temp_allocator)
    for k, v in cg.locals do saved_locals_snapshot[k] = v

    if arm.guard != nil {
        cg_emit_indent(cg)
        cg_emit(cg, "if (")
        cg_expr(cg, arm.guard)
        cg_emit(cg, ") {\n")
        cg.indent_lvl += 1
        cg_emit_indent(cg); cg_emit(cg, "return ")
        cg_expr(cg, arm.body)
        cg_emit(cg, ";\n")
        cg.indent_lvl -= 1
        cg_emit_indent(cg); cg_emit(cg, "} else { break; }\n")
    } else {
        cg_emit_indent(cg); cg_emit(cg, "return ")
        cg_expr(cg, arm.body)
        cg_emit(cg, ";\n")
    }

    clear(&cg.locals)
    for k, v in saved_locals_snapshot do cg.locals[k] = v

    cg.indent_lvl -= 1
    cg_emit_indent(cg); cg_emit(cg, "}\n")
}

cg_emit_variant_bindings :: proc(cg: ^Codegen, scrut: Expr, enum_name: string, vd: ^Variant_Decl, pat: ^Pat_Variant) {
    base := scrutinee_access_str(cg, scrut)
    pushed := push_subst_from_scrutinee(cg, scrut, enum_name)
    defer if pushed do clear(&cg.type_param_subst)

    if vd.kind == .Positional {
        for sub_pat, i in pat.pos {
            if b, is_bind := sub_pat.(^Pat_Bind); is_bind {
                c_type := c_type_of_type_expr(cg, vd.pos[i])
                cg_emit_indent(cg)
                cg_emitf(cg, "%s %s = %s.payload.%s.f%d;\n", c_type, b.name, base, vd.name, i)
                cg.locals[b.name] = c_type
            }
        }
    } else if vd.kind == .Named {
        for nf in pat.named {
            field_index := -1
            for fld, idx in vd.named {
                if fld.name == nf.name { field_index = idx; break }
            }
            if field_index < 0 do continue
            if b, is_bind := nf.pat.(^Pat_Bind); is_bind {
                c_type := c_type_of_type_expr(cg, vd.named[field_index].type)
                cg_emit_indent(cg)
                cg_emitf(cg, "%s %s = %s.payload.%s.%s;\n", c_type, b.name, base, vd.name, nf.name)
                cg.locals[b.name] = c_type
            }
        }
    }
}

push_subst_from_scrutinee :: proc(cg: ^Codegen, scrut: Expr, enum_name: string) -> bool {
    if cg.tc == nil do return false
    t, has := cg.tc.expr_types[scrut]
    if !has do return false
    inner := t
    if ptr, is_ptr := t.(^Ty_Ptr); is_ptr do inner = ptr.inner
    adt, is_adt := inner.(^Ty_Adt)
    if !is_adt do return false
    enum_decl, ok := cg.enums[enum_name]
    if !ok do return false
    if len(adt.args) != len(enum_decl.type_params) do return false
    if len(adt.args) == 0 do return false
    clear(&cg.type_param_subst)
    for tp, i in enum_decl.type_params {
        cg.type_param_subst[tp] = ty_to_c_type(cg, adt.args[i])
    }
    return true
}

scrutinee_access_str :: proc(cg: ^Codegen, scrut: Expr) -> string {
    if u, is_un := scrut.(^Expr_Unary); is_un && u.op == .Deref {
        if id, is_id := u.rhs.(^Expr_Ident); is_id {
            return fmt.tprintf("(*%s)", id.name)
        }
    }
    if id, is_id := scrut.(^Expr_Ident); is_id {
        t := infer_expr_c_type(cg, scrut)
        if strings.has_suffix(t, "*") {
            return fmt.tprintf("(*%s)", id.name)
        }
        return id.name
    }
    return "(/*scrut*/)"
}

find_variant :: proc(e: ^Decl_Enum, name: string) -> ^Variant_Decl {
    if e == nil do return nil
    for i in 0..<len(e.variants) {
        if e.variants[i].name == name do return &e.variants[i]
    }
    return nil
}

// --- Println lowering ---

cg_fmt_format :: proc(cg: ^Codegen, args: []Expr) {
    if len(args) < 1 {
        cg_emit(cg, "QOZ_STR_LIT(\"\")")
        return
    }
    lit, is_lit := args[0].(^Expr_String_Lit)
    if !is_lit {
        cg_error(cg, "fmt.format template must be a string literal")
        cg_emit(cg, "QOZ_STR_LIT(\"\")")
        return
    }
    raw := lit.text
    if len(raw) < 2 || raw[0] != '"' || raw[len(raw)-1] != '"' {
        cg_error(cg, "fmt.format: malformed template literal")
        cg_emit(cg, "QOZ_STR_LIT(\"\")")
        return
    }
    body := raw[1:len(raw)-1]
    segments := make([dynamic]string, context.temp_allocator)
    start := 0
    i := 0
    for i < len(body) {
        if i + 1 < len(body) && body[i] == '{' && body[i+1] == '}' {
            append(&segments, body[start:i])
            start = i + 2
            i += 2
        } else {
            i += 1
        }
    }
    append(&segments, body[start:])

    arg_count := len(args) - 1
    expected := len(segments) - 1
    if arg_count != expected {
        cg_error(cg, fmt.tprintf("fmt.format: template has %d placeholders but %d args were given", expected, arg_count))
    }

    sb := fmt.tprintf("_qoz_sb_%d", next_tmp_id(cg))
    cg_emit(cg, "({ ")
    cg_emitf(cg, "qoz_strbuf %s; qoz_strbuf_init(&%s); ", sb, sb)
    for seg, idx in segments {
        if len(seg) > 0 {
            cg_emitf(cg, "qoz_strbuf_append_str(&%s, QOZ_STR_LIT(\"%s\")); ", sb, seg)
        }
        if idx < arg_count {
            cg_fmt_append_arg(cg, sb, args[idx+1])
        }
    }
    cg_emitf(cg, "qoz_strbuf_finish(&%s); })", sb)
}

cg_fmt_append_arg :: proc(cg: ^Codegen, sb: string, e: Expr) {
    t := infer_expr_c_type(cg, e)
    switch t {
    case "qoz_string":
        cg_emitf(cg, "qoz_strbuf_append_str(&%s, ", sb)
        cg_expr(cg, e)
        cg_emit(cg, "); ")
    case "int8_t", "int16_t", "int32_t":
        cg_emitf(cg, "qoz_strbuf_append_i64(&%s, (int64_t)(", sb)
        cg_expr(cg, e)
        cg_emit(cg, ")); ")
    case "int64_t":
        cg_emitf(cg, "qoz_strbuf_append_i64(&%s, ", sb)
        cg_expr(cg, e)
        cg_emit(cg, "); ")
    case "uint8_t", "uint16_t", "uint32_t", "uint64_t":
        cg_emitf(cg, "qoz_strbuf_append_i64(&%s, (int64_t)(", sb)
        cg_expr(cg, e)
        cg_emit(cg, ")); ")
    case "float", "double":
        cg_emitf(cg, "qoz_strbuf_append_f64(&%s, (double)(", sb)
        cg_expr(cg, e)
        cg_emit(cg, ")); ")
    case "bool":
        cg_emitf(cg, "qoz_strbuf_append_bool(&%s, ", sb)
        cg_expr(cg, e)
        cg_emit(cg, "); ")
    case "const char *":
        cg_emitf(cg, "qoz_strbuf_append_cstr(&%s, ", sb)
        cg_expr(cg, e)
        cg_emit(cg, "); ")
    case:
        cg_error(cg, fmt.tprintf("fmt.format: cannot format argument of C type `%s`", t))
        cg_emitf(cg, "qoz_strbuf_append_str(&%s, QOZ_STR_LIT(\"<?>\")); ", sb)
    }
}

cg_fmt_println :: proc(cg: ^Codegen, args: []Expr) {
    if len(args) == 0 {
        cg_emit(cg, "qoz_print_nl()")
        return
    }
    cg_emit(cg, "(")
    for a, i in args {
        if i > 0 {
            cg_emit(cg, ", qoz_print_sep(), ")
        }
        cg_print_one(cg, a)
    }
    cg_emit(cg, ", qoz_print_nl())")
}

cg_print_one :: proc(cg: ^Codegen, e: Expr) {
    t := infer_expr_c_type(cg, e)
    switch t {
    case "qoz_string":
        cg_emit(cg, "qoz_print_str(")
        cg_expr(cg, e)
        cg_emit(cg, ")")
    case "int8_t", "int16_t", "int32_t":
        cg_emit(cg, "qoz_print_i32(")
        cg_expr(cg, e)
        cg_emit(cg, ")")
    case "int64_t":
        cg_emit(cg, "qoz_print_i64(")
        cg_expr(cg, e)
        cg_emit(cg, ")")
    case "uint8_t", "uint16_t", "uint32_t":
        cg_emit(cg, "qoz_print_i32((int32_t)(")
        cg_expr(cg, e)
        cg_emit(cg, "))")
    case "uint64_t":
        cg_emit(cg, "qoz_print_i64((int64_t)(")
        cg_expr(cg, e)
        cg_emit(cg, "))")
    case "float", "double":
        cg_emit(cg, "qoz_print_f64((double)(")
        cg_expr(cg, e)
        cg_emit(cg, "))")
    case "bool":
        cg_emit(cg, "qoz_print_bool(")
        cg_expr(cg, e)
        cg_emit(cg, ")")
    case:
        cg_emit(cg, "qoz_print_cstr(\"<unprintable>\")")
    }
}

binary_op_c_str :: proc(op: Binary_Op) -> string {
    switch op {
    case .Add: return "+"
    case .Sub: return "-"
    case .Mul: return "*"
    case .Div: return "/"
    case .Mod: return "%"
    case .Eq:  return "=="
    case .Ne:  return "!="
    case .Lt:  return "<"
    case .Gt:  return ">"
    case .Le:  return "<="
    case .Ge:  return ">="
    case .And: return "&&"
    case .Or:  return "||"
    case .Range, .Range_Inclusive:
        return "+"                                          // range lowering arrives with the iteration milestone
    }
    return "+"
}

unary_op_c_str :: proc(op: Unary_Op) -> string {
    switch op {
    case .Neg:   return "-"
    case .Not:   return "!"
    case .Deref: return "*"
    case .Addr:  return "&"
    }
    return ""
}

assign_op_c_str :: proc(op: Assign_Op) -> string {
    switch op {
    case .Set:     return "="
    case .Add_Set: return "+="
    case .Sub_Set: return "-="
    case .Mul_Set: return "*="
    case .Div_Set: return "/="
    case .Mod_Set: return "%="
    }
    return "="
}

// --- Closure pre-pass ---

gather_closures :: proc(cg: ^Codegen, f: File) {
    for d in f.decls {
        if fn, is_fn := d.(^Decl_Fn); is_fn {
            scope := make(map[string]string, context.temp_allocator)
            for p in fn.params {
                scope[p.name] = c_type_of_type_expr(cg, p.type)
            }
            walk_expr_collect(cg, &scope, fn.body)
        }
    }
}

walk_expr_collect :: proc(cg: ^Codegen, scope: ^map[string]string, e: Expr) {
    switch v in e {
    case ^Expr_Closure:
        register_closure(cg, v, scope^)
        inner := make(map[string]string, context.temp_allocator)
        for k, val in scope do inner[k] = val
        for p in v.params {
            inner[p.name] = c_type_of_closure_param(cg, p)
        }
        walk_expr_collect(cg, &inner, v.body)
    case ^Expr_Block:
        inner := make(map[string]string, context.temp_allocator)
        for k, val in scope do inner[k] = val
        for s in v.stmts {
            switch ss in s {
            case ^Stmt_Let:
                walk_expr_collect(cg, &inner, ss.value)
                inner[ss.name] = c_type_for_let(cg, ss.type, ss.value)
            case ^Stmt_Var:
                walk_expr_collect(cg, &inner, ss.value)
                inner[ss.name] = c_type_for_let(cg, ss.type, ss.value)
            case ^Stmt_Let_Else:
                walk_expr_collect(cg, &inner, ss.value)
                if ss.else_block != nil do walk_expr_collect(cg, &inner, ss.else_block)
            case ^Stmt_Expr:
                walk_expr_collect(cg, &inner, ss.expr)
            }
        }
        if v.tail != nil do walk_expr_collect(cg, &inner, v.tail)
    case ^Expr_If:
        walk_expr_collect(cg, scope, v.cond)
        walk_expr_collect(cg, scope, v.then_b)
        if v.else_b != nil do walk_expr_collect(cg, scope, v.else_b)
    case ^Expr_Match:
        walk_expr_collect(cg, scope, v.scrutinee)
        for arm in v.arms {
            walk_expr_collect(cg, scope, arm.body)
            if arm.guard != nil do walk_expr_collect(cg, scope, arm.guard)
        }
    case ^Expr_While:
        walk_expr_collect(cg, scope, v.cond)
        walk_expr_collect(cg, scope, v.body)
    case ^Expr_For:
        walk_expr_collect(cg, scope, v.iter)
        walk_expr_collect(cg, scope, v.body)
    case ^Expr_Binary:
        walk_expr_collect(cg, scope, v.lhs)
        walk_expr_collect(cg, scope, v.rhs)
    case ^Expr_Unary:
        walk_expr_collect(cg, scope, v.rhs)
    case ^Expr_Assign:
        walk_expr_collect(cg, scope, v.target)
        walk_expr_collect(cg, scope, v.value)
    case ^Expr_Call:
        walk_expr_collect(cg, scope, v.callee)
        for a in v.args do walk_expr_collect(cg, scope, a)
    case ^Expr_Field:
        walk_expr_collect(cg, scope, v.base)
    case ^Expr_Index:
        walk_expr_collect(cg, scope, v.base)
        walk_expr_collect(cg, scope, v.index)
    case ^Expr_Cast:
        walk_expr_collect(cg, scope, v.value)
    case ^Expr_New:
        walk_expr_collect(cg, scope, v.value)
    case ^Expr_Try:
        walk_expr_collect(cg, scope, v.value)
    case ^Expr_Return:
        if v.value != nil do walk_expr_collect(cg, scope, v.value)
    case ^Expr_Defer:
        walk_expr_collect(cg, scope, v.body)
    case ^Expr_Tuple:
        for el in v.elems do walk_expr_collect(cg, scope, el)
    case ^Expr_Array_Lit:
        for el in v.elems do walk_expr_collect(cg, scope, el)
    case ^Expr_Size_Of:
    case ^Expr_Record:
        for fld in v.fields do walk_expr_collect(cg, scope, fld.value)
        if v.base != nil do walk_expr_collect(cg, scope, v.base)
    case ^Expr_Int_Lit, ^Expr_Float_Lit, ^Expr_String_Lit, ^Expr_Char_Lit,
         ^Expr_Bool_Lit, ^Expr_Nil_Lit, ^Expr_Ident, ^Expr_Path:
                                                            // leaves: no recursion needed
    }
}

c_type_of_closure_param :: proc(cg: ^Codegen, p: Closure_Param) -> string {
    if p.type != nil do return c_type_of_type_expr(cg, p.type)
    return "int64_t"
}

c_type_for_let :: proc(cg: ^Codegen, ann: ^Type_Expr, value: Expr) -> string {
    if ann != nil do return c_type_of_type_expr(cg, ann)
    return infer_expr_c_type(cg, value)
}

register_closure :: proc(cg: ^Codegen, ce: ^Expr_Closure, scope: map[string]string) {
    bound := make(map[string]bool, context.temp_allocator)
    for p in ce.params do bound[p.name] = true
    free := make(map[string]bool, context.temp_allocator)
    collect_free_vars(ce.body, &bound, &free)

    captures := make([dynamic]Closure_Capture)
    for name in free {
        if c_type, in_scope := scope[name]; in_scope {
            append(&captures, Closure_Capture{name = name, c_type = c_type})
        }
    }

    param_cs := make([dynamic]string)
    for p in ce.params do append(&param_cs, c_type_of_closure_param(cg, p))
    ret_c: string = "void"
    if ce.ret != nil do ret_c = c_type_of_type_expr(cg, ce.ret)

    sig_index := register_signature(cg, param_cs[:], ret_c)
    id := len(cg.closures)
    info := Closure_Info{
        id        = id,
        fn_name   = fmt.tprintf("qoz_lifted_%d", id),
        env_name  = fmt.tprintf("qoz_env_%d", id),
        sig_index = sig_index,
        params    = ce.params,
        param_cs  = param_cs[:],
        ret_c     = ret_c,
        captures  = captures[:],
        body      = ce.body,
    }
    append(&cg.closures, info)
    cg.closure_id_of[ce] = id
}

collect_free_vars :: proc(e: Expr, bound: ^map[string]bool, free: ^map[string]bool) {
    switch v in e {
    case ^Expr_Ident:
        if !bound[v.name] do free[v.name] = true
    case ^Expr_Path:
                                                            // module-prefixed paths are not local captures
    case ^Expr_Closure:
        inner_bound := make(map[string]bool, context.temp_allocator)
        for k, val in bound do inner_bound[k] = val
        for p in v.params do inner_bound[p.name] = true
        collect_free_vars(v.body, &inner_bound, free)
    case ^Expr_Block:
        inner_bound := make(map[string]bool, context.temp_allocator)
        for k, val in bound do inner_bound[k] = val
        for s in v.stmts {
            switch ss in s {
            case ^Stmt_Let:
                collect_free_vars(ss.value, &inner_bound, free)
                inner_bound[ss.name] = true
            case ^Stmt_Var:
                collect_free_vars(ss.value, &inner_bound, free)
                inner_bound[ss.name] = true
            case ^Stmt_Let_Else:
                collect_free_vars(ss.value, &inner_bound, free)
                if ss.else_block != nil do collect_free_vars(ss.else_block, &inner_bound, free)
                if pat, is_var := ss.pat.(^Pat_Variant); is_var {
                    for sub in pat.pos do if b, ok := sub.(^Pat_Bind); ok do inner_bound[b.name] = true
                    for nf in pat.named do if b, ok := nf.pat.(^Pat_Bind); ok do inner_bound[b.name] = true
                }
            case ^Stmt_Expr:
                collect_free_vars(ss.expr, &inner_bound, free)
            }
        }
        if v.tail != nil do collect_free_vars(v.tail, &inner_bound, free)
    case ^Expr_If:
        collect_free_vars(v.cond, bound, free)
        collect_free_vars(v.then_b, bound, free)
        if v.else_b != nil do collect_free_vars(v.else_b, bound, free)
    case ^Expr_Match:
        collect_free_vars(v.scrutinee, bound, free)
        for arm in v.arms {
            if arm.guard != nil do collect_free_vars(arm.guard, bound, free)
            collect_free_vars(arm.body, bound, free)
        }
    case ^Expr_While:
        collect_free_vars(v.cond, bound, free)
        collect_free_vars(v.body, bound, free)
    case ^Expr_For:
        collect_free_vars(v.iter, bound, free)
        collect_free_vars(v.body, bound, free)
    case ^Expr_Binary:
        collect_free_vars(v.lhs, bound, free)
        collect_free_vars(v.rhs, bound, free)
    case ^Expr_Unary:
        collect_free_vars(v.rhs, bound, free)
    case ^Expr_Assign:
        collect_free_vars(v.target, bound, free)
        collect_free_vars(v.value, bound, free)
    case ^Expr_Call:
        collect_free_vars(v.callee, bound, free)
        for a in v.args do collect_free_vars(a, bound, free)
    case ^Expr_Field:
        collect_free_vars(v.base, bound, free)
    case ^Expr_Index:
        collect_free_vars(v.base, bound, free)
        collect_free_vars(v.index, bound, free)
    case ^Expr_Cast:
        collect_free_vars(v.value, bound, free)
    case ^Expr_New:
        collect_free_vars(v.value, bound, free)
    case ^Expr_Try:
        collect_free_vars(v.value, bound, free)
    case ^Expr_Return:
        if v.value != nil do collect_free_vars(v.value, bound, free)
    case ^Expr_Defer:
        collect_free_vars(v.body, bound, free)
    case ^Expr_Tuple:
        for el in v.elems do collect_free_vars(el, bound, free)
    case ^Expr_Array_Lit:
        for el in v.elems do collect_free_vars(el, bound, free)
    case ^Expr_Size_Of:
    case ^Expr_Record:
        for fld in v.fields do collect_free_vars(fld.value, bound, free)
        if v.base != nil do collect_free_vars(v.base, bound, free)
    case ^Expr_Int_Lit, ^Expr_Float_Lit, ^Expr_String_Lit, ^Expr_Char_Lit,
         ^Expr_Bool_Lit, ^Expr_Nil_Lit:
                                                            // leaves
    }
}

signature_key :: proc(params: []string, ret: string) -> string {
    sb := strings.builder_make(context.temp_allocator)
    for p in params {
        strings.write_string(&sb, p)
        strings.write_string(&sb, "|")
    }
    strings.write_string(&sb, "=>")
    strings.write_string(&sb, ret)
    return strings.to_string(sb)
}

sanitize_sig_part :: proc(s: string) -> string {
    sb := strings.builder_make(context.temp_allocator)
    for r in s {
        if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_' {
            strings.write_rune(&sb, r)
        } else {
            strings.write_rune(&sb, '_')
        }
    }
    return strings.to_string(sb)
}

register_signature :: proc(cg: ^Codegen, params: []string, ret: string) -> int {
    key := signature_key(params, ret)
    if idx, ok := cg.sig_index_by_key[key]; ok do return idx
    sb := strings.builder_make()
    strings.write_string(&sb, "qoz_fn_")
    for p, i in params {
        if i > 0 do strings.write_string(&sb, "_")
        strings.write_string(&sb, sanitize_sig_part(p))
    }
    strings.write_string(&sb, "_RET_")
    strings.write_string(&sb, sanitize_sig_part(ret))
    sig := Closure_Sig{
        name   = strings.to_string(sb),
        params = params,
        ret    = ret,
    }
    idx := len(cg.closure_sigs)
    append(&cg.closure_sigs, sig)
    cg.sig_index_by_key[key] = idx
    return idx
}

cg_emit_closure_typedefs :: proc(cg: ^Codegen) {
    if len(cg.closure_sigs) == 0 do return
    for sig in cg.closure_sigs {
        cg_emit(cg, "typedef struct {\n")
        cg_emit(cg, "    void *env;\n")
        cg_emitf(cg, "    %s (*fn)(void *env", sig.ret)
        for p in sig.params {
            cg_emitf(cg, ", %s", p)
        }
        cg_emit(cg, ");\n")
        cg_emit(cg, "} ")
        cg_emit(cg, sig.name)
        cg_emit(cg, ";\n\n")
    }
}

cg_emit_closure_env_structs :: proc(cg: ^Codegen) {
    for info in cg.closures {
        if len(info.captures) == 0 do continue
        cg_emit(cg, "typedef struct {\n")
        for cap in info.captures {
            cg_emitf(cg, "    %s %s;\n", cap.c_type, cap.name)
        }
        cg_emit(cg, "} ")
        cg_emit(cg, info.env_name)
        cg_emit(cg, ";\n\n")
    }
}

cg_emit_closure_fn_protos :: proc(cg: ^Codegen) {
    for info in cg.closures {
        cg_emitf(cg, "static %s %s(void *env", info.ret_c, info.fn_name)
        for p, i in info.params {
            cg_emitf(cg, ", %s %s", info.param_cs[i], p.name)
        }
        cg_emit(cg, ");\n")
    }
    if len(cg.closures) > 0 do cg_emit(cg, "\n")
}

cg_emit_closure_fn_bodies :: proc(cg: ^Codegen) {
    for info in cg.closures {
        cg_emitf(cg, "static %s %s(void *env_raw", info.ret_c, info.fn_name)
        for p, i in info.params {
            cg_emitf(cg, ", %s %s", info.param_cs[i], p.name)
        }
        cg_emit(cg, ") {\n")
        cg.indent_lvl = 1
        if len(info.captures) > 0 {
            cg_emit_indent(cg)
            cg_emitf(cg, "%s *env = (%s *)env_raw;\n", info.env_name, info.env_name)
        } else {
            cg_emit_indent(cg)
            cg_emit(cg, "(void)env_raw;\n")
        }

        saved_locals := cg.locals
        cg.locals = make(map[string]string, context.temp_allocator)
        for p, i in info.params {
            cg.locals[p.name] = info.param_cs[i]
        }
        for cap in info.captures {
            cg.locals[cap.name] = cap.c_type
        }

        cg_emit_indent(cg)
        cg_emit(cg, "return ")
        if len(info.captures) > 0 {
            cg_emit_closure_body_with_env(cg, info.body, info.captures)
        } else {
            cg_expr(cg, info.body)
        }
        cg_emit(cg, ";\n")

        cg.locals = saved_locals
        cg.indent_lvl = 0
        cg_emit(cg, "}\n\n")
    }
}

// Emit a closure body where captured identifiers resolve as `env->name`.
cg_emit_closure_body_with_env :: proc(cg: ^Codegen, e: Expr, caps: []Closure_Capture) {
    cap_set := make(map[string]bool, context.temp_allocator)
    for c in caps do cap_set[c.name] = true
    cg_emit_with_env(cg, e, &cap_set)
}

cg_emit_with_env :: proc(cg: ^Codegen, e: Expr, caps: ^map[string]bool) {
    if id, is_id := e.(^Expr_Ident); is_id {
        if caps[id.name] {
            cg_emitf(cg, "env->%s", id.name)
            return
        }
        cg_emit(cg, id.name)
        return
    }
    #partial switch v in e {
    case ^Expr_Binary:
        cg_emit(cg, "(")
        cg_emit_with_env(cg, v.lhs, caps)
        cg_emit(cg, " ")
        cg_emit(cg, binary_op_c_str(v.op))
        cg_emit(cg, " ")
        cg_emit_with_env(cg, v.rhs, caps)
        cg_emit(cg, ")")
    case ^Expr_Unary:
        cg_emit(cg, unary_op_c_str(v.op))
        cg_emit_with_env(cg, v.rhs, caps)
    case ^Expr_Call:
        cg_call_with_env(cg, v, caps)
    case ^Expr_Cast:
        cg_emit(cg, "((")
        cg_emit(cg, c_type_of_type_expr(cg, v.target))
        cg_emit(cg, ")")
        cg_emit_with_env(cg, v.value, caps)
        cg_emit(cg, ")")
    case:
        cg_expr(cg, e)
    }
}

cg_call_with_env :: proc(cg: ^Codegen, call: ^Expr_Call, caps: ^map[string]bool) {
    cg_emit_with_env(cg, call.callee, caps)
    cg_emit(cg, "(")
    for a, i in call.args {
        if i > 0 do cg_emit(cg, ", ")
        cg_emit_with_env(cg, a, caps)
    }
    cg_emit(cg, ")")
}
