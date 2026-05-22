package main

import "core:fmt"
import "core:os"
import "core:path/filepath"
import "core:strings"

main :: proc() {
    if len(os.args) < 3 {
        usage()
        os.exit(1)
    }

    cmd := os.args[1]
    path := os.args[2]

    data, read_err := os.read_entire_file(path, context.allocator)
    if read_err != nil {
        fmt.eprintfln("could not read %s: %v", path, read_err)
        os.exit(1)
    }

    tokens, tok_err := tokenize(path, string(data))
    if e, has := tok_err.(Tokenize_Error); has {
        fmt.eprintfln("%s:%d:%d: %s", e.file, e.line, e.column, e.message)
        os.exit(1)
    }

    switch cmd {
    case "lex":
        for tok in tokens {
            fmt.printfln("%d:%d %v %q", tok.line, tok.column, tok.kind, tok.source)
        }
    case "parse":
        file, errs := load_project(path)
        if len(errs) > 0 {
            print_parse_errors(errs)
            os.exit(1)
        }
        print_file(file^)
    case "gen":
        file, errs := load_project(path)
        if len(errs) > 0 {
            print_parse_errors(errs)
            os.exit(1)
        }
        tc := type_check_file(file^)
        if len(tc.errors) > 0 {
            print_type_errors(tc.errors[:])
            os.exit(1)
        }
        result := codegen_file(file^, tc)
        for e in result.errors {
            fmt.eprintfln("codegen: %s", e)
        }
        fmt.print(result.c_source)
    case "check":
        file, errs := load_project(path)
        if len(errs) > 0 {
            print_parse_errors(errs)
            os.exit(1)
        }
        tc := type_check_file(file^)
        if len(tc.errors) > 0 {
            print_type_errors(tc.errors[:])
            os.exit(1)
        }
        fmt.println("type check passed")
    case "build", "run":
        file, errs := load_project(path)
        if len(errs) > 0 {
            print_parse_errors(errs)
            os.exit(1)
        }
        tc := type_check_file(file^)
        if len(tc.errors) > 0 {
            print_type_errors(tc.errors[:])
            os.exit(1)
        }
        result := codegen_file(file^, tc)
        for e in result.errors {
            fmt.eprintfln("codegen: %s", e)
        }
        if len(result.errors) > 0 do os.exit(1)
        out_name := output_name_for(path)
        ok := compile_and_link(result.c_source, out_name)
        if !ok do os.exit(1)
        if cmd == "run" {
            run_program(out_name)
            os.remove(out_name)
        } else {
            fmt.printfln("built ./%s", out_name)
        }
    case:
        fmt.eprintfln("unknown command: %s", cmd)
        usage()
        os.exit(1)
    }
}

usage :: proc() {
    fmt.eprintln("usage: stage_a <command> <file.qoz>")
    fmt.eprintln("commands:")
    fmt.eprintln("  lex     tokenize and print tokens")
    fmt.eprintln("  parse   parse and print the AST")
    fmt.eprintln("  check   run the type checker only")
    fmt.eprintln("  gen     emit generated C to stdout")
    fmt.eprintln("  build   compile to a native binary")
    fmt.eprintln("  run     compile, run, and remove the binary")
}

// Loads the entry file and every file it imports (transitively). Each
// `import std/X` resolves to `std/X/X.qoz` relative to cwd, the binary's
// directory, or the entry file's parent. Decls from every loaded file are
// merged into a single File for type checking and codegen.
load_project :: proc(entry_path: string, allocator := context.allocator) -> (^File, []Parse_Error) {
    visited := make(map[string]bool, allocator)
    all_decls := make([dynamic]Decl, allocator)
    all_errors := make([dynamic]Parse_Error, allocator)

    Pending :: struct { path: string, pkg: string }
    queue := make([dynamic]Pending, allocator)
    append(&queue, Pending{path = entry_path, pkg = ""})

    pkg_seen := make(map[string]bool, allocator)

    for len(queue) > 0 {
        cur := queue[0]
        ordered_remove(&queue, 0)
        abs_cur, _ := filepath.abs(cur.path, context.temp_allocator)
        if visited[abs_cur] do continue
        visited[abs_cur] = true

        data, read_err := os.read_entire_file(cur.path, context.temp_allocator)
        if read_err != nil {
            append(&all_errors, Parse_Error{file = cur.path, line = 0, column = 0,
                message = fmt.tprintf("could not read file: %v", read_err)})
            continue
        }
        tokens, tok_err := tokenize(cur.path, string(data), context.temp_allocator)
        if e, has := tok_err.(Tokenize_Error); has {
            append(&all_errors, Parse_Error{file = e.file, line = e.line, column = e.column, message = e.message})
            continue
        }

        f, errs := parse_file(cur.path, tokens[:], allocator)
        for e in errs do append(&all_errors, e)

        for d in f.decls {
            if imp, is_imp := d.(^Decl_Import); is_imp {
                resolved, found := resolve_import(imp.path)
                if !found {
                    append(&all_errors, Parse_Error{
                        file = cur.path, line = imp.span.line, column = imp.span.column,
                        message = fmt.tprintf("could not resolve import: %s", strings.join(imp.path, "/", context.temp_allocator)),
                    })
                    continue
                }
                pkg_name := imp.alias
                if pkg_name == "" do pkg_name = imp.path[len(imp.path)-1]
                if !visited[resolved] do append(&queue, Pending{path = resolved, pkg = pkg_name})
            }
            if cur.pkg != "" {
                if fn, is_fn := d.(^Decl_Fn); is_fn {
                    fn.name = fmt.aprintf("%s_%s", cur.pkg, fn.name, allocator = allocator)
                }
                if ext, is_ext := d.(^Decl_External); is_ext {
                    ext.name = fmt.aprintf("%s_%s", cur.pkg, ext.name, allocator = allocator)
                }
            }
            append(&all_decls, d)
        }
        if cur.pkg != "" do pkg_seen[cur.pkg] = true
    }

    pkgs := make([dynamic]string, allocator)
    for name in pkg_seen do append(&pkgs, name)

    aggregated := new(File, allocator)
    aggregated.path = entry_path
    aggregated.decls = all_decls[:]
    aggregated.packages = pkgs[:]
    return aggregated, all_errors[:]
}

resolve_import :: proc(imp_path: []string) -> (string, bool) {
    if len(imp_path) < 1 do return "", false

    parts := make([dynamic]string, context.temp_allocator)
    for p in imp_path do append(&parts, p)
    last := imp_path[len(imp_path)-1]
    append(&parts, fmt.tprintf("%s.qoz", last))
    rel, _ := filepath.join(parts[:], context.temp_allocator)

    if _, err := os.stat(rel, context.temp_allocator); err == nil {
        abs, _ := filepath.abs(rel, context.temp_allocator)
        return strings.clone(abs), true
    }

    exe_path := os.args[0]
    abs_exe, _ := filepath.abs(exe_path, context.temp_allocator)
    exe_dir := filepath.dir(abs_exe, context.temp_allocator)
    candidates := []string{
        filepath.join({exe_dir, rel}, context.temp_allocator) or_else "",
        filepath.join({exe_dir, "..", rel}, context.temp_allocator) or_else "",
    }
    for c in candidates {
        if c == "" do continue
        if _, err := os.stat(c, context.temp_allocator); err == nil {
            abs, _ := filepath.abs(c, context.temp_allocator)
            return strings.clone(abs), true
        }
    }
    return "", false
}

print_parse_errors :: proc(errs: []Parse_Error) {
    for e in errs {
        fmt.eprintfln("%s:%d:%d: %s", e.file, e.line, e.column, e.message)
    }
}

print_type_errors :: proc(errs: []Type_Error) {
    for e in errs {
        fmt.eprintfln("%s:%d:%d: type error: %s", e.span.file, e.span.line, e.span.column, e.message)
    }
}

output_name_for :: proc(src_path: string) -> string {
    base := filepath.base(src_path)
    if dot := strings.last_index(base, "."); dot >= 0 {
        return strings.clone(base[:dot])
    }
    return strings.clone(base)
}

// Runtime C/H sources are baked into the driver binary so Stage A does
// not depend on the stage_a/runtime/ directory existing at compile time.
// On first use, ensure_runtime_dir writes them to a session temp path
// and returns it; subsequent calls reuse the same path.
@(private)
runtime_qoz_runtime_c := #load("runtime/qoz_runtime.c")
@(private)
runtime_qoz_runtime_h := #load("runtime/qoz_runtime.h")
@(private)
runtime_gc_c := #load("runtime/gc.c")
@(private)
runtime_gc_h := #load("runtime/gc.h")

@(private)
runtime_dir_cache: string = ""

ensure_runtime_dir :: proc() -> string {
    if runtime_dir_cache != "" do return runtime_dir_cache

    tmp_root := os.get_env("TMPDIR", context.temp_allocator)
    if tmp_root == "" do tmp_root = "/tmp"
    rt, _ := filepath.join({tmp_root, "qoz-stage-a-runtime"}, context.allocator)

    if _, err := os.stat(rt, context.temp_allocator); err != nil {
        if mk_err := os.make_directory(rt); mk_err != nil {
            fmt.eprintfln("could not create runtime cache %s: %v", rt, mk_err)
        }
    }

    write_if_changed :: proc(path: string, data: []byte) {
        existing, read_err := os.read_entire_file(path, context.temp_allocator)
        if read_err == nil && len(existing) == len(data) {
            same := true
            for b, i in existing { if b != data[i] { same = false; break } }
            if same do return
        }
        _ = os.write_entire_file(path, data)
    }

    p1, _ := filepath.join({rt, "qoz_runtime.c"}, context.temp_allocator)
    p2, _ := filepath.join({rt, "qoz_runtime.h"}, context.temp_allocator)
    p3, _ := filepath.join({rt, "gc.c"},          context.temp_allocator)
    p4, _ := filepath.join({rt, "gc.h"},          context.temp_allocator)
    write_if_changed(p1, runtime_qoz_runtime_c)
    write_if_changed(p2, runtime_qoz_runtime_h)
    write_if_changed(p3, runtime_gc_c)
    write_if_changed(p4, runtime_gc_h)

    runtime_dir_cache = rt
    return rt
}

compile_and_link :: proc(c_source: string, out_name: string) -> bool {
    tmp_c := fmt.tprintf("%s.qoz.c", out_name)
    if werr := os.write_entire_file(tmp_c, transmute([]byte)c_source); werr != nil {
        fmt.eprintfln("could not write %s: %v", tmp_c, werr)
        return false
    }

    rt := ensure_runtime_dir()
    qoz_rt_c, _ := filepath.join({rt, "qoz_runtime.c"}, context.temp_allocator)
    gc_c, _     := filepath.join({rt, "gc.c"},          context.temp_allocator)
    include_flag := fmt.tprintf("-I%s", rt)

    cmd := []string{
        "clang",
        "-std=c11",
        "-O2",
        "-Wall",
        "-Werror",
        // The codegen emits some style patterns that clang would flag, none
        // of which represent real bugs:
        //
        //   -Wno-unused-function:        variant constructors and auto-derived
        //                                hash/eq are emitted per type and may
        //                                not be called; linker drops them.
        //   -Wno-unused-variable:        user-written `let x = ...` bindings
        //                                that the user never reads. Treated as
        //                                a style choice, not a bug.
        //   -Wno-unused-but-set-variable: same shape on the assignment side.
        //   -Wno-parentheses-equality:   binary expressions defensively wrap
        //                                their operands, producing `((a == b))`
        //                                inside if-conditions.
        "-Wno-unused-function",
        "-Wno-unused-variable",
        "-Wno-unused-but-set-variable",
        "-Wno-parentheses-equality",
        include_flag,
        tmp_c,
        qoz_rt_c,
        gc_c,
        "-o", out_name,
    }

    state, stdout, stderr, err := os.process_exec({command = cmd}, context.temp_allocator)
    if err != nil {
        fmt.eprintfln("failed to invoke clang: %v", err)
        return false
    }
    if state.exit_code != 0 {
        fmt.eprintfln("clang exited %d", state.exit_code)
        if len(stdout) > 0 do fmt.eprint(string(stdout))
        if len(stderr) > 0 do fmt.eprint(string(stderr))
        return false
    }
    if os.get_env("QOZ_KEEP_C", context.temp_allocator) == "" {
        os.remove(tmp_c)
    }
    return true
}

run_program :: proc(out_name: string) {
    rel := fmt.tprintf("./%s", out_name)
    state, stdout, stderr, err := os.process_exec({command = []string{rel}}, context.temp_allocator)
    if err != nil {
        fmt.eprintfln("failed to run program: %v", err)
        return
    }
    if len(stdout) > 0 do fmt.print(string(stdout))
    if len(stderr) > 0 do fmt.eprint(string(stderr))
    if state.exit_code != 0 {
        os.exit(state.exit_code)
    }
}

// --- AST pretty printers (kept from the parser milestone) ---

print_file :: proc(f: File) {
    fmt.printfln("file %s (%d decls)", f.path, len(f.decls))
    for d in f.decls {
        print_decl(d, 1)
    }
}

print_decl :: proc(d: Decl, depth: int) {
    indent(depth)
    switch v in d {
    case ^Decl_Import:
        fmt.printf("Import path=")
        for i, idx in v.path {
            if idx > 0 do fmt.print("/")
            fmt.print(i)
        }
        if v.alias != "" do fmt.printf(" as %s", v.alias)
        fmt.println()
    case ^Decl_Fn:
        fmt.printfln("Fn %s (%d params)", v.name, len(v.params))
        for p in v.params {
            indent(depth+1)
            fmt.printf("Param %s: ", p.name)
            print_type(p.type)
            fmt.println()
        }
        if v.ret != nil {
            indent(depth+1)
            fmt.print("Ret ")
            print_type(v.ret)
            fmt.println()
        }
        print_expr(v.body, depth+1)
    case ^Decl_Struct:
        fmt.printfln("Struct %s (%d fields)", v.name, len(v.fields))
        for fld in v.fields {
            indent(depth+1)
            fmt.printf("%s: ", fld.name)
            print_type(fld.type)
            fmt.println()
        }
    case ^Decl_Enum:
        fmt.printfln("Enum %s (%d variants)", v.name, len(v.variants))
        for va in v.variants {
            indent(depth+1)
            fmt.printfln("Variant %s kind=%v", va.name, va.kind)
        }
    case ^Decl_Const:
        fmt.printfln("Const %s", v.name)
        print_expr(v.value, depth+1)
    case ^Decl_External:
        fmt.printfln("External %s -> %s", v.name, v.symbol)
    case ^Decl_Link:
        fmt.printfln("Link %v %s", v.kind, v.name)
    case ^Decl_Type_Alias:
        fmt.printf("TypeAlias %s = ", v.name)
        print_type(v.target)
        fmt.println()
    }
}

print_type :: proc(t: ^Type_Expr) {
    if t == nil { fmt.print("<unspec>"); return }
    switch v in t^ {
    case ^Type_Named:
        for s, idx in v.path {
            if idx > 0 do fmt.print(".")
            fmt.print(s)
        }
        if len(v.args) > 0 {
            fmt.print("<")
            for a, idx in v.args {
                if idx > 0 do fmt.print(", ")
                print_type(a)
            }
            fmt.print(">")
        }
    case ^Type_Ptr:
        fmt.print("*")
        print_type(v.inner)
    case ^Type_Fn:
        fmt.print("fun(")
        for a, idx in v.params {
            if idx > 0 do fmt.print(", ")
            print_type(a)
        }
        fmt.print(")")
        if v.ret != nil {
            fmt.print(" -> ")
            print_type(v.ret)
        }
    case ^Type_Tuple:
        fmt.print("(")
        for a, idx in v.elems {
            if idx > 0 do fmt.print(", ")
            print_type(a)
        }
        fmt.print(")")
    case ^Type_Unit:
        fmt.print("()")
    }
}

print_expr :: proc(e: Expr, depth: int) {
    indent(depth)
    switch v in e {
    case ^Expr_Int_Lit:    fmt.printfln("Int %s", v.text)
    case ^Expr_Float_Lit:  fmt.printfln("Float %s", v.text)
    case ^Expr_String_Lit: fmt.printfln("Str %s", v.text)
    case ^Expr_Char_Lit:   fmt.printfln("Char %s", v.text)
    case ^Expr_Bool_Lit:   fmt.printfln("Bool %v", v.value)
    case ^Expr_Nil_Lit:    fmt.println("Nil")
    case ^Expr_Ident:      fmt.printfln("Ident %s", v.name)
    case ^Expr_Path:
        fmt.print("Path ")
        for s, i in v.segs {
            if i > 0 do fmt.print(".")
            fmt.print(s)
        }
        fmt.println()
    case ^Expr_Unary:
        fmt.printfln("Unary %v", v.op)
        print_expr(v.rhs, depth+1)
    case ^Expr_Binary:
        fmt.printfln("Binary %v", v.op)
        print_expr(v.lhs, depth+1)
        print_expr(v.rhs, depth+1)
    case ^Expr_Assign:
        fmt.printfln("Assign %v", v.op)
        print_expr(v.target, depth+1)
        print_expr(v.value, depth+1)
    case ^Expr_Call:
        fmt.printfln("Call (%d args)", len(v.args))
        print_expr(v.callee, depth+1)
        for a in v.args {
            print_expr(a, depth+1)
        }
    case ^Expr_Field:
        fmt.printfln("Field .%s", v.name)
        print_expr(v.base, depth+1)
    case ^Expr_Index:
        fmt.println("Index")
        print_expr(v.base, depth+1)
        print_expr(v.index, depth+1)
    case ^Expr_Cast:
        fmt.print("Cast as ")
        print_type(v.target)
        fmt.println()
        print_expr(v.value, depth+1)
    case ^Expr_New:
        fmt.println("New")
        print_expr(v.value, depth+1)
    case ^Expr_Try:
        fmt.println("Try")
        print_expr(v.value, depth+1)
    case ^Expr_Tuple:
        fmt.printfln("Tuple (%d)", len(v.elems))
        for e2 in v.elems {
            print_expr(e2, depth+1)
        }
    case ^Expr_Record:
        fmt.printfln("Record (%d fields)", len(v.fields))
        for fld in v.fields {
            indent(depth+1)
            fmt.printfln("%s =", fld.name)
            print_expr(fld.value, depth+2)
        }
        if v.base != nil {
            indent(depth+1)
            fmt.println("..base")
            print_expr(v.base, depth+2)
        }
    case ^Expr_Closure:
        fmt.printfln("Closure (%d params)", len(v.params))
        print_expr(v.body, depth+1)
    case ^Expr_Block:
        fmt.printfln("Block (%d stmts, tail=%v)", len(v.stmts), v.tail != nil)
        for s in v.stmts {
            print_stmt(s, depth+1)
        }
        if v.tail != nil {
            indent(depth+1)
            fmt.println("tail:")
            print_expr(v.tail, depth+2)
        }
    case ^Expr_If:
        fmt.println("If")
        indent(depth+1); fmt.println("cond:")
        print_expr(v.cond, depth+2)
        indent(depth+1); fmt.println("then:")
        print_expr(v.then_b, depth+2)
        if v.else_b != nil {
            indent(depth+1); fmt.println("else:")
            print_expr(v.else_b, depth+2)
        }
    case ^Expr_Match:
        fmt.printfln("Match (%d arms)", len(v.arms))
        print_expr(v.scrutinee, depth+1)
    case ^Expr_While:
        fmt.println("While")
        print_expr(v.cond, depth+1)
        print_expr(v.body, depth+1)
    case ^Expr_For:
        fmt.printfln("For %s in", v.binding)
        print_expr(v.iter, depth+1)
        print_expr(v.body, depth+1)
    case ^Expr_Return:
        fmt.println("Return")
        if v.value != nil do print_expr(v.value, depth+1)
    case ^Expr_Defer:
        fmt.println("Defer")
        print_expr(v.body, depth+1)
    case ^Expr_Size_Of:
        fmt.print("SizeOf ")
        print_type(v.target)
        fmt.println()
    case ^Expr_Array_Lit:
        fmt.printfln("ArrayLit (%d elems)", len(v.elems))
        for el in v.elems do print_expr(el, depth+1)
    }
}

print_stmt :: proc(s: Stmt, depth: int) {
    indent(depth)
    switch v in s {
    case ^Stmt_Let:
        fmt.printfln("Let %s", v.name)
        if v.type != nil {
            indent(depth+1); fmt.print("type: "); print_type(v.type); fmt.println()
        }
        print_expr(v.value, depth+1)
    case ^Stmt_Var:
        fmt.printfln("Var %s", v.name)
        if v.type != nil {
            indent(depth+1); fmt.print("type: "); print_type(v.type); fmt.println()
        }
        print_expr(v.value, depth+1)
    case ^Stmt_Let_Else:
        fmt.println("LetElse")
        print_expr(v.value, depth+1)
        indent(depth+1); fmt.println("else:")
        print_expr(v.else_block, depth+2)
    case ^Stmt_Expr:
        fmt.println("ExprStmt")
        print_expr(v.expr, depth+1)
    }
}

indent :: proc(n: int) {
    for _ in 0..<n do fmt.print("  ")
}
