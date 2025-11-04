package main 

import "core:strings"
import "core:path/filepath"
import "core:fmt"
import "core:mem"

Symbol :: struct {
    name:       string,
    type:       Type_Info,
    defined_at: Span,
    pkg_dir:    string,
}

Scope :: struct {
    symbols: map[string]Symbol,
    parent: ^Scope
}

Semantic_Error :: struct {
    message: string,
    span: Span,
}

Package_Symbols :: struct {
    symbols: map[string]Symbol,
    scope: ^Scope,
}

Semantic_Context :: struct {
    current_scope: ^Scope,
    errors: [dynamic]Semantic_Error,
    allocator: mem.Allocator,
    current_function_return_type: Maybe(Type_Info),
    current_struct_name: Maybe(string), 
    external_functions: map[string]bool,
    global_symbols: map[string]bool,

    packages: map[string]Package_Symbols, // package_dir -> symbols
    current_package: string,              // Which package we're analyzing
    import_aliases: map[string]string,    // alias -> package_dir
}

semantic_push_scope :: proc(ctx: ^Semantic_Context) {
    scope := new(Scope, ctx.allocator)
    scope.parent = ctx.current_scope
    scope.symbols = make(map[string]Symbol, ctx.allocator)
    ctx.current_scope = scope
}

semantic_pop_scope :: proc(ctx: ^Semantic_Context) {
    old := ctx.current_scope
    ctx.current_scope = old.parent
    delete(old.symbols)
}

semantic_define_symbol :: proc(ctx: ^Semantic_Context, name: string, type: Type_Info, span: Span, pkg_dir := "") -> bool {
    scope := ctx.current_scope
    if name in ctx.current_scope.symbols {
        add_error(ctx, span, "Symbol '%s' already defined in this scope", name)
        return false
    }
    ctx.current_scope.symbols[name] = Symbol { name=name, type=type, defined_at=span, pkg_dir=pkg_dir }
    if scope.parent == nil {
        ctx.global_symbols[name] = true
    }
    return true
}

semantic_lookup_symbol :: proc(ctx: ^Semantic_Context, name: string) -> (Symbol, bool) {
    scope := ctx.current_scope
    for scope != nil {
        if sym, ok := scope.symbols[name]; ok {
            return sym, true
        }
        scope = scope.parent
    }
    return {}, false
}
is_untyped_int :: proc(t: Type_Info) -> bool {
    _, ok := t.(Untyped_Int)
    return ok
}

is_untyped_float :: proc(t: Type_Info) -> bool {
    _, ok := t.(Untyped_Float)
    return ok
}

is_typed_int :: proc(t: Type_Info) -> bool {
    p, ok := t.(Primitive_Type)
    if !ok do return false
    return p == .I8 || p == .U8 || p == .I32 || p == .I64
}

is_typed_float :: proc(t: Type_Info) -> bool {
    p, ok := t.(Primitive_Type)
    if !ok do return false
    return p == .F32 || p == .F64
}

// Resolve a potentially qualified type name (e.g., "mem.Allocator" or "string")
resolve_named_type :: proc(ctx: ^Semantic_Context, named: Named_Type) -> (Type_Info, bool) {
    // Check if it's a qualified name (contains dot)
    if dot_idx := strings.index(named.name, "."); dot_idx != -1 {
        pkg_alias := named.name[:dot_idx]
        type_name := named.name[dot_idx+1:]
        
        // Look up the package
        if pkg_dir, is_pkg := ctx.import_aliases[pkg_alias]; is_pkg {
            if pkg_info, has_pkg := ctx.packages[pkg_dir]; has_pkg {
                if symbol, found := pkg_info.symbols[type_name]; found {
                    return symbol.type, true
                }
            }
        }
        return {}, false
    }
    
    // It's a simple name, look it up normally
    if sym, ok := semantic_lookup_symbol(ctx, named.name); ok {
        return sym.type, true
    }
    
    return {}, false
}

semantic_analyze_project :: proc(
    asts: map[string]^Node,
    sorted_packages: []string,
    entry_package: string,
    allocator := context.allocator
) -> Semantic_Context {
    ctx := Semantic_Context{
        allocator = allocator,
        errors = make([dynamic]Semantic_Error, allocator),
        external_functions = make(map[string]bool, allocator),
        global_symbols = make(map[string]bool, allocator),
        packages = make(map[string]Package_Symbols, allocator),
        import_aliases = make(map[string]string, allocator),
    }

    fields := make([dynamic]Struct_Field, allocator)
    i8_type: Type_Info = Primitive_Type.I8
    i8_ptr_type := Pointer_Type{pointee = new_clone(i8_type, allocator)}
    append_elems(&fields,
        Struct_Field{name = "data", type = i8_ptr_type},
        Struct_Field{name = "len", type = Primitive_Type.I64},
    )
    string_struct := Struct_Type{fields = fields}
    
    semantic_push_scope(&ctx)
    semantic_define_symbol(&ctx, "string", string_struct, Span{})

    // Pass 1: Collect all top-level declarations per package
    // - Each package gets its own isolated symbol table
    // - Walks through each package's files, registering symbols (functions, types, globals)
    // - Determines types syntactically without analyzing function bodies or initializers
    // - Enables forward references within the same package
    // - Does NOT recurse into function bodies, only collects signatures
    // - Symbols are only accessible within their own package unless explicitly qualified
    // - Cross-package symbols stored separately in ctx.packages map
    // - Processes packages in dependency order to ensure imported packages are collected first
    // - After this pass, all package-level names are known and can be referenced via qualification
    for pkg_dir in sorted_packages {
        pkg_scope := new(Scope, allocator)
        pkg_scope.symbols = make(map[string]Symbol, allocator)
        pkg_scope.parent = ctx.current_scope  // Link to global scope so builtins are accessible
        
        // Temporarily make this the current scope
        old_scope := ctx.current_scope
        ctx.current_scope = pkg_scope
        
        for file_path, ast in asts {
            if filepath.dir(file_path, allocator) == pkg_dir {
                collect_declarations(&ctx, ast, pkg_dir, entry_package)
            }
        }
        
        // Save package symbols separately
        ctx.packages[pkg_dir] = Package_Symbols{symbols = pkg_scope.symbols}
        
        // Restore scope
        ctx.current_scope = old_scope
    }

    // Pass 2: Full semantic analysis and validation
    // - Performs complete type checking on all expressions and statements
    // - Analyzes function bodies, checking control flow and return types
    // - Validates initializers and function calls against declared types
    // - For local variables with self-references:
    //   - Detects struct literals syntactically
    //   - Pre-registers the local variable with its type before checking initializer
    //   - Then validates the initializer, at which point the variable exists in scope
    // - Uses the global symbol table built in pass 1 for lookups
    // - Reports all semantic errors (type mismatches, undefined symbols, etc.)
    // Multi-package: processes all packages in dependency order
    for pkg_dir in sorted_packages {
        // Get the package scope that was created in Pass 1
        if pkg_info, ok := ctx.packages[pkg_dir]; ok {
            pkg_scope := new(Scope, allocator)
            pkg_scope.symbols = pkg_info.symbols
            pkg_scope.parent = ctx.current_scope  // Link to global scope for builtins
            
            old_scope := ctx.current_scope
            ctx.current_scope = pkg_scope
            ctx.current_package = pkg_dir
            
            for file_path, ast in asts {
                file_dir := filepath.dir(file_path, context.temp_allocator)
                if file_dir == pkg_dir {
                    check_node(&ctx, ast)
                }
            }
            
            ctx.current_scope = old_scope
        }
    }

    // Validate main exists in entry package
    main_span := Span{start = 0, end = 0}
    entry_pkg_info, has_entry_pkg := ctx.packages[entry_package]
    main_sym, has_main := entry_pkg_info.symbols["main"]
    
    for file_path, ast in asts {
        file_dir := filepath.dir(file_path, context.temp_allocator)
        if file_dir == entry_package && ast.node_kind == .Program {
            for stmt in ast.payload.(Node_Statement_List).nodes {
                if stmt.node_kind == .Var_Def {
                    var_def := stmt.payload.(Node_Var_Def)
                    if var_def.name == "main" {
                        main_span = stmt.span
                        break
                    }
                }
            }
        }
    }

    if !has_entry_pkg || !has_main {
        append(&ctx.errors, Semantic_Error{
            message = "Program must define a 'main' function",
            span = Span{start = 0, end = 0},
        })
    } else {
        fn_type, is_fn := main_sym.type.(Function_Type)
        if !is_fn {
            append(&ctx.errors, Semantic_Error{
                message = "'main' must be a function",
                span = main_span,
            })
        } else {
            if len(fn_type.params) != 0 {
                append(&ctx.errors, Semantic_Error{
                    message = "'main' must take no parameters",
                    span = main_span,
                })
            }
            ret_type, is_void := fn_type.return_type^.(Primitive_Type)
            if !is_void || ret_type != .Void {
                append(&ctx.errors, Semantic_Error{
                    message = "'main' must return void",
                    span = main_span,
                })
            }
        }
    }

    return ctx
}


@(private="file")
add_error :: proc(ctx: ^Semantic_Context, span: Span, format: string, args: ..any) {
    error := Semantic_Error {
        message = fmt.tprintf(format, ..args),
        span = span,
    }
    append(&ctx.errors, error)
}

@(private="file")
extract_alias_from_path :: proc(import_path: string) -> string {
    path := import_path
    
    // Strip prefix (std:, etc.)
    if colon_idx := strings.index(path, ":"); colon_idx != -1 {
        path = path[colon_idx+1:]
    }
    
    // Strip ./ or ../
    if strings.has_prefix(path, "./") {
        path = path[2:]
    } else if strings.has_prefix(path, "../") {
        path = path[3:]
    }
    
    return filepath.base(path)
}

// Qualify unqualified type names with package prefix
qualify_type :: proc(ctx: ^Semantic_Context, t: Type_Info, pkg_name: string, allocator: mem.Allocator) -> Type_Info {
    #partial switch typ in t {
    case Named_Type:
        // If already qualified, leave it
        if strings.contains(typ.name, ".") {
            return t
        }
        // Don't qualify built-in types (string is defined globally)
        if typ.name == "string" {
            return t
        }
        // Qualify with package name
        qualified_name := fmt.tprintf("%s.%s", pkg_name, typ.name)
        return Named_Type{name = qualified_name}
    case Pointer_Type:
        qualified_pointee := qualify_type(ctx, typ.pointee^, pkg_name, allocator)
        return Pointer_Type{pointee = new_clone(qualified_pointee, allocator)}
    case Array_Type:
        qualified_elem := qualify_type(ctx, typ.element_type^, pkg_name, allocator)
        return Array_Type{element_type = new_clone(qualified_elem, allocator), size = typ.size}
    case Function_Type:
        qualified_params := make([]Type_Info, len(typ.params), allocator)
        for param, i in typ.params {
            qualified_params[i] = qualify_type(ctx, param, pkg_name, allocator)
        }
        qualified_return := qualify_type(ctx, typ.return_type^, pkg_name, allocator)
        return Function_Type{params = qualified_params, return_type = new_clone(qualified_return, allocator)}
    case Struct_Type:
        qualified_fields := make([dynamic]Struct_Field, allocator)
        for field in typ.fields {
            qualified_field_type := qualify_type(ctx, field.type, pkg_name, allocator)
            append(&qualified_fields, Struct_Field{name = field.name, type = qualified_field_type})
        }
        return Struct_Type{fields = qualified_fields}
    case:
        return t
    }
}

// Pass 1: Collect declarations without analyzing bodies
collect_declarations :: proc(ctx: ^Semantic_Context, node: ^Node, pkg_dir, project_root: string) {
    if node == nil do return
    
    pkg_name := filepath.base(pkg_dir)
    
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            collect_declarations(ctx, stmt, pkg_dir, project_root)
        }
    
    case .Import: 
        import_node := node.payload.(Node_Import)
        alias: string
        if import_alias, has_alias := import_node.alias.?; has_alias {
            alias = import_alias
        } else {
            alias = extract_alias_from_path(import_node.path)
        }

        
        resolved_path := resolve_import_path(pkg_dir, project_root, import_node.path)
        ctx.import_aliases[alias] = resolved_path 

    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        
        if var_def.content.node_kind == .Fn_Def {
            fn_def := var_def.content.payload.(Node_Fn_Def)
            if fn_def.is_external {
                ctx.external_functions[var_def.name] = true
            }
        }
        
        declared_type: Maybe(Type_Info)
        
        #partial switch var_def.content.node_kind {
        case .Fn_Def:
            fn_def := var_def.content.payload.(Node_Fn_Def)
            param_types := make([]Type_Info, len(fn_def.params), ctx.allocator)
            for param, i in fn_def.params {
                param_types[i] = param.type
            }
            declared_type = Function_Type{
                params = param_types,
                return_type = new_clone(fn_def.return_type, ctx.allocator),
            }
            var_def.content.inferred_type = declared_type
        
        case .Type_Expr:
            type_expr := var_def.content.payload.(Node_Type_Expr)
            declared_type = type_expr.type_info
        
        case .Struct_Literal:
            // Can determine type from literal syntactically
            struct_lit := var_def.content.payload.(Node_Struct_Literal)
            declared_type = Named_Type{name = struct_lit.type_name}
        
        case:
            // Check for explicit type annotation
            if explicit, has_explicit := var_def.explicit_type.?; has_explicit {
                declared_type = explicit
            }
        }
        
        if type, ok := declared_type.?; ok {
            semantic_define_symbol(ctx, var_def.name, type, node.span, pkg_dir)
        }
    }
}

// Pass 2: Full checking with bodies
check_node :: proc(ctx: ^Semantic_Context, node: ^Node) -> Type_Info {
    if node == nil do return Primitive_Type.Void
    
    #partial switch node.node_kind {
    case .Import:
        // Already processed during parse phase for dependency resolution
        // Multi-package semantic analysis comes later
        return Primitive_Type.Void
    case .Program: 
        for stmt in node.payload.(Node_Statement_List).nodes {
            check_node(ctx, stmt)
        }
        return Primitive_Type.Void
    
    case .Expr_Statement:
        expr_stmt := node.payload.(Node_Expr_Statement)
        expr_type := check_node(ctx, expr_stmt.expr)
        node.inferred_type = expr_type
        return expr_type

    case .Assignment:
        assign := node.payload.(Node_Assign)
        
        #partial switch assign.target.node_kind {
        case .Identifier, .Index, .Field_Access:
        case:
            add_error(ctx, assign.target.span, "Invalid assignment target")
        }
        
        target_type := check_node(ctx, assign.target)
        value_type := check_node(ctx, assign.value)
        
        if !types_compatible(target_type, value_type) {
            add_error(ctx, node.span, "Cannot assign %v to %v", value_type, target_type)
        }
        
        node.inferred_type = value_type
        return value_type

    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        left_type := check_node(ctx, binop.left)
        right_type := check_node(ctx, binop.right)
        
        // Coerce untyped operands and update their inferred types
        if is_untyped_int(left_type) && is_typed_int(right_type) {
            binop.left.inferred_type = right_type
            left_type = right_type
        } else if is_typed_int(left_type) && is_untyped_int(right_type) {
            binop.right.inferred_type = left_type
            right_type = left_type
        } else if is_untyped_int(left_type) && is_untyped_int(right_type) {
            // Both untyped - default to i64
            binop.left.inferred_type = Primitive_Type.I64
            binop.right.inferred_type = Primitive_Type.I64
            left_type = Primitive_Type.I64
            right_type = Primitive_Type.I64
        }
        
        // Coerce untyped floats
        if is_untyped_float(left_type) && is_typed_float(right_type) {
            binop.left.inferred_type = right_type
            left_type = right_type
        } else if is_typed_float(left_type) && is_untyped_float(right_type) {
            binop.right.inferred_type = left_type
            right_type = left_type
        } else if is_untyped_float(left_type) && is_untyped_float(right_type) {
            binop.left.inferred_type = Primitive_Type.F64
            binop.right.inferred_type = Primitive_Type.F64
            left_type = Primitive_Type.F64
            right_type = Primitive_Type.F64
        }
        
        result_type := semantic_binop_type_resolve(left_type, right_type, binop.op, node.span, ctx)
        node.inferred_type = result_type
        return result_type

    case .Cast:
        cast_node := node.payload.(Node_Cast)
        source_type := check_node(ctx, cast_node.expr)
        target_type := cast_node.target_type

        if !validate_type_exists(ctx, target_type, node.span) {
            node.inferred_type = source_type
            return source_type
        }
        
        // Untyped integers can be cast to any numeric or pointer type
        if is_untyped_int(source_type) {
            if is_typed_int(target_type) || is_typed_float(target_type) {
                node.inferred_type = target_type
                return target_type
            }
            if _, is_ptr := target_type.(Pointer_Type); is_ptr {
                node.inferred_type = target_type
                return target_type
            }
        }
        
        source_ptr, source_is_ptr := source_type.(Pointer_Type)
        target_ptr, target_is_ptr := target_type.(Pointer_Type)
        
        valid := false
        if source_is_ptr && target_is_ptr {
            valid = true
        } else if source_prim, ok := source_type.(Primitive_Type); ok {
            if target_prim, ok2 := target_type.(Primitive_Type); ok2 {
                valid = (source_prim == .I32 || source_prim == .I64 || source_prim == .F32 || source_prim == .F64) &&
                        (target_prim == .I32 || target_prim == .I64 || target_prim == .F32 || target_prim == .F64)
            }
        }
        
        if !valid {
            add_error(ctx, node.span, "Invalid cast from %v to %v", source_type, target_type)
        }
        
        node.inferred_type = target_type
        return target_type

    case .Del:
        del_node := node.payload.(Node_Del)
        ptr_type := check_node(ctx, del_node.pointer)
        allocator_type := check_node(ctx, del_node.allocator)
        
        // Validate first argument - must be pointer, string, or array
        valid_first_arg := false
        #partial switch t in ptr_type {
        case Pointer_Type:
            valid_first_arg = true
        case Array_Type:
            valid_first_arg = true
        case Named_Type:
            if t.name == "string" {
                valid_first_arg = true
            }
        }
        
        if !valid_first_arg {
            add_error(ctx, del_node.pointer.span, "del() requires pointer, string, or array type, got %v", ptr_type)
        }
        
        // Validate allocator type (should be mem.Allocator or compatible)
        if named, is_named := allocator_type.(Named_Type); is_named {
            if named.name != "mem.Allocator" {
                add_error(ctx, del_node.allocator.span, "del() requires mem.Allocator, got %v", allocator_type)
            }
        } else {
            add_error(ctx, del_node.allocator.span, "del() requires mem.Allocator, got %v", allocator_type)
        }
        
        return .Void

    case .Field_Access:
        field_access := node.payload.(Node_Field_Access)

        // Check for qualified name (package.symbol)
        if field_access.object.node_kind == .Identifier {
            iden := field_access.object.payload.(Node_Identifier)
            
            // Is this a package alias?
            if pkg_dir, is_pkg := ctx.import_aliases[iden.name]; is_pkg {
                if pkg_info, has_pkg := ctx.packages[pkg_dir]; has_pkg {
                    if symbol, found := pkg_info.symbols[field_access.field_name]; found {
                        // Qualify the type with the package name since it's from another package
                        pkg_name := filepath.base(pkg_dir)
                        qualified_type := qualify_type(ctx, symbol.type, pkg_name, ctx.allocator)
                        node.inferred_type = qualified_type
                        return qualified_type
                    }
                }
                add_error(ctx, node.span, "Package '%s' has no symbol '%s'", iden.name, field_access.field_name)
                return Primitive_Type.Void
            }
        }

        object_type := check_node(ctx, field_access.object)
        
        actual_type := object_type
        if named, is_named := object_type.(Named_Type); is_named {
            resolved, ok := resolve_named_type(ctx, named)
            if !ok {
                add_error(ctx, node.span, "Undefined type '%s'", named.name)
                node.inferred_type = Primitive_Type.Void
                return Primitive_Type.Void
            }
            actual_type = resolved
        }

        if ptr_type, is_ptr := actual_type.(Pointer_Type); is_ptr {
            actual_type = ptr_type.pointee^
            if named, is_named := actual_type.(Named_Type); is_named {
                resolved, ok := resolve_named_type(ctx, named)
                if !ok {
                    add_error(ctx, node.span, "Undefined type '%s'", named.name)
                    node.inferred_type = Primitive_Type.Void
                    return Primitive_Type.Void
                }
                actual_type = resolved
            }
        }
        
        struct_type, is_struct := actual_type.(Struct_Type)
        if !is_struct {
            add_error(ctx, node.span, "Cannot access field of non-struct type")
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        for field in struct_type.fields {
            if field.name == field_access.field_name {
                node.inferred_type = field.type
                return field.type
            }
        }
        
        add_error(ctx, node.span, "Struct has no field '%s'", field_access.field_name)
        node.inferred_type = Primitive_Type.Void
        return Primitive_Type.Void

    case .For_In:
        for_in := node.payload.(Node_For_In)
        
        iterable_type := check_node(ctx, for_in.iterable)
        
        arr_type, is_array := iterable_type.(Array_Type)
        if !is_array {
            add_error(ctx, for_in.iterable.span, "Cannot iterate over non-iterable type %v", iterable_type)
            return Primitive_Type.Void
        }
        
        semantic_push_scope(ctx)
        semantic_define_symbol(ctx, for_in.iterator, arr_type.element_type^, node.span)
        for stmt in for_in.body do check_node(ctx, stmt)
        semantic_pop_scope(ctx)
        return Primitive_Type.Void
    
    case .For_C:
        for_c := node.payload.(Node_For_C)
        
        // Create scope for the entire loop (init variable lives here)
        semantic_push_scope(ctx)
        
        // Check init (var def or assignment)
        check_node(ctx, for_c.init)
        
        // Check condition (must be bool)
        cond_type := check_node(ctx, for_c.condition)
        if cond_prim, ok := cond_type.(Primitive_Type); !ok || cond_prim != .Bool {
            add_error(ctx, for_c.condition.span, "For loop condition must be bool, got %v", cond_type)
        }
        
        // Check post statement (usually increment)
        check_node(ctx, for_c.post)
        
        // Check body
        for stmt in for_c.body {
            check_node(ctx, stmt)
        }
        
        semantic_pop_scope(ctx)
        return Primitive_Type.Void

    case .Index:
        index_node := node.payload.(Node_Index)
        object_type := check_node(ctx, index_node.object)
        index_type := check_node(ctx, index_node.index)
        
        if index_prim, is_prim := index_type.(Primitive_Type); !is_prim || 
        (index_prim != .I32 && index_prim != .I64) {
            add_error(ctx, index_node.index.span, "Index must be integer")
        }
        
        element_type: Type_Info
        if arr_type, is_arr := object_type.(Array_Type); is_arr {
            element_type = arr_type.element_type^
        } else if ptr_type, is_ptr := object_type.(Pointer_Type); is_ptr {
            element_type = ptr_type.pointee^
        } else {
            add_error(ctx, node.span, "Cannot index non-array/pointer type")
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        node.inferred_type = element_type
        return element_type
    
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)

        if var_def.content.node_kind == .Type_Expr {
            ctx.current_struct_name = var_def.name
        }

        // Check if this is a package-level symbol (already defined in Pass 1)
        existing_sym, already_defined := semantic_lookup_symbol(ctx, var_def.name)
        
        // If it's a package-level symbol (has pkg_dir), it was collected in Pass 1
        // Just validate it, don't redefine
        if already_defined && existing_sym.pkg_dir != "" {
            // Symbol was collected in pass 1, just validate
            value_type := check_node(ctx, var_def.content)
            actual_type := existing_sym.type
            
            if !types_compatible(actual_type, value_type) {
                add_error(ctx, node.span, "Type mismatch: declared as %v but initialized with %v", 
                    actual_type, value_type)
            }
            
            node.inferred_type = actual_type
        } else {
            // Local variable - check for redefinition or shadowing
            
            // Check if symbol exists in current scope (redefinition)
            if var_def.name in ctx.current_scope.symbols {
                add_error(ctx, node.span, "Symbol '%s' is already defined in this scope", var_def.name)
                node.inferred_type = Primitive_Type.Void
                return Primitive_Type.Void
            }
            
            // Check if symbol exists in parent scope (shadowing)
            if already_defined {
                add_error(ctx, node.span, "Symbol '%s' shadows variable from outer scope", var_def.name)
                node.inferred_type = Primitive_Type.Void
                return Primitive_Type.Void
            }
            
            // Local variable not in pass 1, need to define it first
            // Determine type syntactically without full validation
            declared_type: Type_Info
            
            #partial switch var_def.content.node_kind {
            case .Struct_Literal:
                struct_lit := var_def.content.payload.(Node_Struct_Literal)
                declared_type = Named_Type{name = struct_lit.type_name}
            
            case:
                if explicit, has_explicit := var_def.explicit_type.?; has_explicit {
                    declared_type = explicit
                } else {
                    // Need to infer, call check_node but this might fail for self-ref
                    declared_type = check_node(ctx, var_def.content)
                }
            }
            
            // Define symbol BEFORE validating initializer contents
            semantic_define_symbol(ctx, var_def.name, declared_type, node.span)
            // Now validate the initializer and recurse the node_check
            value_type := check_node(ctx, var_def.content)
            
            if !types_compatible(declared_type, value_type) {
                add_error(ctx, node.span, "Type mismatch: declared as %v but initialized with %v", 
                    declared_type, value_type)
            }
            
            node.inferred_type = declared_type
        }

        if var_def.content.node_kind == .Type_Expr {
            ctx.current_struct_name = nil
        }
        
        return node.inferred_type.?
    
    case .Fn_Call:
        call := node.payload.(Node_Call)

        // Check for built-in len() function
        // Short circuit
        if call.callee.node_kind == .Identifier {
            callee_name := call.callee.payload.(Node_Identifier).name
            if callee_name == "len" {
                // Validate: exactly 1 argument
                if len(call.args) != 1 {
                    add_error(ctx, node.span, "len() requires exactly 1 argument")
                    node.inferred_type = Primitive_Type.I64
                    return Primitive_Type.I64
                }
                
                // Check argument type
                arg_type := check_node(ctx, call.args[0])
                
                // Validate type
                valid := false
                #partial switch t in arg_type {
                case Array_Type: valid = true
                case Named_Type:
                    if t.name == "string" do valid = true
                }
                
                if !valid {
                    add_error(ctx, node.span, "len() requires array or string, got %v", arg_type)
                }
                
                node.inferred_type = Primitive_Type.I64
                return Primitive_Type.I64
            }
        }

        callee_type := check_node(ctx, call.callee)
        
        arg_types := make([dynamic]Type_Info, context.temp_allocator)
        for arg in call.args {
            arg_type := check_node(ctx, arg)
            append(&arg_types, arg_type)
        }
        
        fn_type, ok := callee_type.(Function_Type)
        if !ok {
            add_error(ctx, node.span, "Cannot call non-function")
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        if len(call.args) != len(fn_type.params) {
            add_error(ctx, node.span, "Expected %d arguments, got %d", len(fn_type.params), len(call.args))
        }
        
        for arg_type, i in arg_types {
            if i >= len(fn_type.params) do break
            expected := fn_type.params[i]
            
            arg_prim, arg_ok := arg_type.(Primitive_Type)
            exp_prim, exp_ok := expected.(Primitive_Type)
            
            if arg_ok && exp_ok && arg_prim != exp_prim {
                add_error(ctx, call.args[i].span, "Argument %d: expected %v, got %v", i, exp_prim, arg_prim)
            }
        }
        
        return_type := fn_type.return_type^
        node.inferred_type = return_type
        return return_type

    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)
        
        param_types := make([]Type_Info, len(fn_def.params), ctx.allocator)
        for param, i in fn_def.params {
            param_types[i] = param.type
        }
        fn_type := Function_Type{
            params = param_types,
            return_type = new_clone(fn_def.return_type, ctx.allocator),
        }
        node.inferred_type = fn_type
        
        if fn_def.is_external do return fn_type

        old_return_type := ctx.current_function_return_type
        ctx.current_function_return_type = fn_def.return_type
        
        semantic_push_scope(ctx)
        {
            for param in fn_def.params {
                semantic_define_symbol(ctx, param.name, param.type, param.span)
            }

            for stmt in fn_def.body {
                check_node(ctx, stmt)
            }

            requires_return := true
            if ret_type_prim, ok := fn_def.return_type.(Primitive_Type); ok {
                if ret_type_prim == .Void {
                    requires_return = false
                }
            }

            if requires_return {
                if !block_returns(fn_def.body) {
                    add_error(ctx, node.span, "Non-void function must return on all code paths")
                }
            }
        }
        semantic_pop_scope(ctx)
        ctx.current_function_return_type = old_return_type
        
        return fn_type

    case .If:
        if_stmt := node.payload.(Node_If)
        cond_type := check_node(ctx, if_stmt.condition)
        
        if cond_prim, ok := cond_type.(Primitive_Type); !ok || cond_prim != .Bool {
            add_error(ctx, if_stmt.condition.span, "If condition must be bool")
        }
        
        for stmt in if_stmt.if_body {
            check_node(ctx, stmt)
        }
        
        for stmt in if_stmt.else_body {
            check_node(ctx, stmt)
        }
        
        return Primitive_Type.Void

    case .Literal_Arr:
        arr_lit := node.payload.(Node_Array_Literal)
        
        elem_types := make([dynamic]Type_Info, context.temp_allocator)
        for elem in arr_lit.elements {
            elem_type := check_node(ctx, elem)
            append(&elem_types, elem_type)
        }
        
        if len(arr_lit.elements) != arr_lit.size {
            add_error(ctx, node.span, "Array literal has %d elements but size is %d", 
                len(arr_lit.elements), arr_lit.size)
        }
        
        for elem_type, i in elem_types {
            if !types_equal(elem_type, arr_lit.element_type) {
                add_error(ctx, arr_lit.elements[i].span, "Array element %d: expected %v, got %v", 
                    i, arr_lit.element_type, elem_type)
            }
        }
        
        array_type := Array_Type{
            element_type = new_clone(arr_lit.element_type, ctx.allocator),
            size = arr_lit.size,
        }
        node.inferred_type = array_type
        return array_type

    case .Print:
        content_type := check_node(ctx, node.payload.(Node_Print).content)
        return content_type

    case .Return:
        ret := node.payload.(Node_Return)
        expected_ret_type, in_function := ctx.current_function_return_type.?
        if !in_function {
            add_error(ctx, node.span, "Return statement outside function")
            return Primitive_Type.Void
        }

        if ret.value != nil {
            ret_type := check_node(ctx, ret.value)
            if !types_equal(ret_type, ctx.current_function_return_type.?) {
                add_error(ctx, node.span, "Return type mismatch: expected %v, got %v", expected_ret_type, ret_type)
            }
        } else {
            exp_prim_type, is_prim := expected_ret_type.(Primitive_Type)
            if !is_prim || exp_prim_type != .Void {
                add_error(ctx, node.span, "Function expects return value")
            }
        }
        
        return Primitive_Type.Void
    
    case .Len:
        n := node.payload.(Node_Len)
        value_type := check_node(ctx, n.value)
        
        valid := false
        #partial switch t in value_type {
        case Array_Type:
            valid = true
        case Named_Type:
            if t.name == "string" {
                valid = true
            }
        }
        
        if !valid {
            add_error(ctx, node.span, "len() requires array or string, got %v", value_type)
        }
        
        node.inferred_type = Primitive_Type.I64
        return Primitive_Type.I64
        
    case .Size_Of:
        n := node.payload.(Node_Size_Of)
        check_node(ctx, n.type)
        
        node.inferred_type = Primitive_Type.I64
        return Primitive_Type.I64

    case .Struct_Literal:
        struct_lit := node.payload.(Node_Struct_Literal)
        
        resolved_type, ok := resolve_named_type(ctx, Named_Type{name = struct_lit.type_name})
        if !ok {
            add_error(ctx, node.span, "Undefined type '%s'", struct_lit.type_name)
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        struct_type, is_struct := resolved_type.(Struct_Type)
        if !is_struct {
            add_error(ctx, node.span, "'%s' is not a struct type", struct_lit.type_name)
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        field_types := make(map[string]Type_Info, context.temp_allocator)
        for field_init in struct_lit.field_inits {
            value_type := check_node(ctx, field_init.value)
            field_types[field_init.name] = value_type
        }
        
        for field in struct_type.fields {
            value_type, has_field := field_types[field.name]
            if !has_field {
                add_error(ctx, node.span, "Missing field '%s' in struct literal", field.name)
                continue
            }
            
            // Coerce untyped literals to field type
            coerced_type := value_type
            if is_untyped_int(value_type) && is_typed_int(field.type) {
                coerced_type = field.type
                // Update the field value's inferred type
                for field_init in struct_lit.field_inits {
                    if field_init.name == field.name {
                        field_init.value.inferred_type = field.type
                        break
                    }
                }
            }
            
            if !types_compatible(coerced_type, field.type) {
                add_error(ctx, node.span, "Field '%s': expected %v, got %v", 
                    field.name, field.type, value_type)
            }
        }
        
        result := Named_Type{name = struct_lit.type_name}
        node.inferred_type = result
        return result
        
    case .Type_Expr:
        type_expr := node.payload.(Node_Type_Expr)

        if struct_type, is_struct := type_expr.type_info.(Struct_Type); is_struct {
            field_names := make(map[string]bool, context.temp_allocator)
            for field in struct_type.fields {
                _, field_exists := field_names[field.name]
                if field_exists {
                    add_error(ctx, node.span, "Duplicate field name '%s' in struct", field.name)
                }
                field_names[field.name] = true
            }

            if struct_name, has_name := ctx.current_struct_name.?; has_name {
                for field in struct_type.fields {
                    if check_infinite_size(ctx, struct_name, field.type, node.span) {
                        add_error(ctx, node.span, "Struct '%s' has infinite size (field '%s' creates direct recursion). You may want to use indirection for this field's type, such as *%s.", 
                            struct_name, field.name, struct_name)
                    }
                }
            }
        }
        
        node.inferred_type = type_expr.type_info
        return type_expr.type_info

    case .Un_Op:
        unop := node.payload.(Node_Un_Op)
        operand_type := check_node(ctx, unop.operand)
        
        #partial switch unop.op.kind {
        case .Plus, .Minus:
            prim, ok := operand_type.(Primitive_Type)
            if prim == .Void {
                add_error(ctx, node.span, "Cannot apply unary operator to void")
                node.inferred_type = Primitive_Type.I32
                return Primitive_Type.I32
            }
            node.inferred_type = prim
            return prim
        case .Amp:
            if unop.operand.node_kind != .Identifier {
                add_error(ctx, node.span, "Address-of requires an lvalue")
            }
            result := Pointer_Type{
                pointee = new_clone(operand_type, ctx.allocator),
            }
            node.inferred_type = result
            return result
        case .Star:
            ptr_type, ok := operand_type.(Pointer_Type)
            if !ok {
                add_error(ctx, node.span, "Cannot dereference non-pointer type %v", operand_type)
                node.inferred_type = Primitive_Type.I32
                return Primitive_Type.I32
            }
            result := ptr_type.pointee^
            node.inferred_type = result
            return result
        case:
            add_error(ctx, node.span, "Unary operator %v not defined", unop.op.kind)
            node.inferred_type = operand_type
            return operand_type
        }
    
    case .Literal_Number:
        node.inferred_type = Untyped_Int{}
        return Untyped_Int{}

    case .Literal_String:
        result := Named_Type{name = "string"}
        node.inferred_type = result
        return result
    
    case .Literal_Nil:
        // nil is a pointer to void
        void_type: Type_Info = Primitive_Type.Void
        result := Pointer_Type{pointee = new_clone(void_type, ctx.allocator)}
        node.inferred_type = result
        return result
    
    case .Identifier:
        iden := node.payload.(Node_Identifier)
        if sym, ok := semantic_lookup_symbol(ctx, iden.name); ok {
            // Check if symbol is from a different package
            if sym.pkg_dir != "" && sym.pkg_dir != ctx.current_package {
                // Symbol is from another package - must be qualified
                add_error(ctx, node.span, "Cannot access '%s' from package '%s' without qualification", 
                    iden.name, filepath.base(sym.pkg_dir))
                node.inferred_type = Primitive_Type.I32
                return Primitive_Type.I32
            }
            
            node.inferred_type = sym.type
            return sym.type
        }
        add_error(ctx, node.span, "Undefined variable '%s'", iden.name)
        node.inferred_type = Primitive_Type.I32
        return Primitive_Type.I32
        
    case: 
        fmt.panicf("Cannot check %v yet", node.node_kind)
    }
    
    return Primitive_Type.Void
}

types_equal :: proc(a, b: Type_Info) -> bool {
    #partial switch a_val in a {
    case Primitive_Type:
        b_val, ok := b.(Primitive_Type)
        return ok && a_val == b_val
    case Function_Type:
        b_val, ok := b.(Function_Type)
        if !types_equal(a_val.return_type^, b_val.return_type^) do return false
        for param_a, i in a_val.params {
            param_b := b_val.params[i]
            if !types_equal(param_a, param_b) do return false
        }
        return true
    case Array_Type:
        b_val, ok := b.(Array_Type)
        if !ok do return false
        return b_val.size == a_val.size && types_equal(a_val.element_type^, b_val.element_type^)
    case Pointer_Type:
        b_val, ok := b.(Pointer_Type)
        if !ok do return false
        return types_equal(a_val.pointee^, b_val.pointee^)
    case Struct_Type:
        b_val, ok := b.(Struct_Type)
        if !ok do return false
        if len(a_val.fields) != len(b_val.fields) do return false
        for field_a, i in a_val.fields {
            field_b := b_val.fields[i]
            if field_a.name != field_b.name do return false
            if !types_equal(field_a.type, field_b.type) do return false
        }
        return true
    case Named_Type:
        b_val, ok := b.(Named_Type)
        return ok && a_val.name==b_val.name
    }
    return false
}

types_compatible :: proc(target: Type_Info, source: Type_Info) -> bool {
    if types_equal(target, source) do return true
    
    // Untyped int can coerce to any typed integer
    if is_untyped_int(source) && is_typed_int(target) {
        return true
    }
    
    // Untyped float can coerce to any typed float
    if is_untyped_float(source) && is_typed_float(target) {
        return true
    }
    
    target_ptr, target_is_ptr := target.(Pointer_Type)
    source_ptr, source_is_ptr := source.(Pointer_Type)
    if target_is_ptr && source_is_ptr {
        if prim, ok := target_ptr.pointee.(Primitive_Type); ok && prim == .Void {
            return true
        }
        if prim, ok := source_ptr.pointee.(Primitive_Type); ok && prim == .Void {
            return true
        }
    }
    
    return false
}

semantic_binop_type_resolve :: proc(t1, t2: Type_Info, op: Token, span: Span, ctx: ^Semantic_Context) -> Type_Info {
    // Handle untyped integer coercion
    left_type := t1
    right_type := t2
    
    if is_untyped_int(left_type) && is_typed_int(right_type) {
        left_type = right_type  // Coerce untyped to match typed
    } else if is_typed_int(left_type) && is_untyped_int(right_type) {
        right_type = left_type  // Coerce untyped to match typed
    } else if is_untyped_int(left_type) && is_untyped_int(right_type) {
        // Both untyped - default to i64
        left_type = Primitive_Type.I64
        right_type = Primitive_Type.I64
    }
    
    // Handle untyped float coercion
    if is_untyped_float(left_type) && is_typed_float(right_type) {
        left_type = right_type
    } else if is_typed_float(left_type) && is_untyped_float(right_type) {
        right_type = left_type
    } else if is_untyped_float(left_type) && is_untyped_float(right_type) {
        left_type = Primitive_Type.F64
        right_type = Primitive_Type.F64
    }
    
    // Handle untyped int with pointer operations
    if is_untyped_int(left_type) {
        left_type = Primitive_Type.I64
    }
    if is_untyped_int(right_type) {
        right_type = Primitive_Type.I64
    }
    
    #partial switch op.kind {
    case .Plus:
        // Pointer + integer
        if ptr_type, is_ptr := left_type.(Pointer_Type); is_ptr {
            if int_type, is_int := right_type.(Primitive_Type); is_int && 
               (int_type == .I32 || int_type == .I64) {
                return left_type  // Returns pointer type
            }
        }
        // Integer + pointer
        if ptr_type, is_ptr := right_type.(Pointer_Type); is_ptr {
            if int_type, is_int := left_type.(Primitive_Type); is_int && 
               (int_type == .I32 || int_type == .I64) {
                return right_type
            }
        }
        
    case .Minus:
        // Pointer - integer
        if ptr_type, is_ptr := left_type.(Pointer_Type); is_ptr {
            if int_type, is_int := right_type.(Primitive_Type); is_int && 
               (int_type == .I32 || int_type == .I64) {
                return left_type
            }
        }
        // Pointer - pointer
        if _, is_ptr1 := left_type.(Pointer_Type); is_ptr1 {
            if _, is_ptr2 := right_type.(Pointer_Type); is_ptr2 {
                return Primitive_Type.I64
            }
        }
    }
    
    // Regular type checking
    if !types_equal(left_type, right_type) {
        add_error(ctx, span, "Type mismatch in binary operation")
        return Primitive_Type.I64
    }
    
    #partial switch t in left_type {
    case Primitive_Type:
        return semantic_binop_primitive(t, op, span, ctx)
    case:
        add_error(ctx, span, fmt.tprintf("Cannot use %v in binary operation", t))
        return Primitive_Type.I32
    }
}

semantic_binop_primitive :: proc(t: Primitive_Type, op: Token, span: Span, ctx: ^Semantic_Context) -> Type_Info {
    #partial switch op.kind {
    case .Plus, .Minus, .Star, .Slash:
        if t == .Void || t == .Bool {
            add_error(ctx, span, "Cannot apply arithmetic to %v", t)
            return Primitive_Type.I32
        }
        return t
    
    case .Lt, .Gt, .Lt_Eq, .Gt_Eq:
        if t == .Void || t == .Bool {
            add_error(ctx, span, "Cannot compare %v", t)
            return Primitive_Type.Bool
        }
        return Primitive_Type.Bool
    
    case .Percent:
        if t == .I32 || t == .I64 do return t
        return .I32

    case .Eq_Eq, .Not_Eq:
        if t == .Void {
            add_error(ctx, span, "Cannot compare void")
            return Primitive_Type.Bool
        }
        return Primitive_Type.Bool
    
    case:
        add_error(ctx, span, "Operator %v not defined for type %v", op.kind, t)
        return t
    }
}

@(private="file")
block_returns :: proc(stmts: [dynamic]^Node) -> bool {
    if len(stmts) == 0 do return false
    
    last := stmts[len(stmts) - 1]
    
    #partial switch last.node_kind {
    case .Return:
        return true
    case .If:
        if_node := last.payload.(Node_If)
        if len(if_node.else_body) == 0 do return false
        if_returns := block_returns(if_node.if_body)
        else_returns := block_returns(if_node.else_body)
        return if_returns && else_returns
    }
    
    return false
}

@(private="file")
check_infinite_size :: proc(ctx: ^Semantic_Context, struct_name: string, field_type: Type_Info, span: Span) -> bool {
    seen := make(map[string]bool, context.temp_allocator)
    return check_infinite_size_impl(ctx, struct_name, field_type, &seen)
}

@(private="file")
check_infinite_size_impl :: proc(ctx: ^Semantic_Context, struct_name: string, field_type: Type_Info, seen: ^map[string]bool) -> bool {
    #partial switch t in field_type {
    case Named_Type:
        if t.name == struct_name {
            return true
        }
        
        if t.name in seen {
            return false
        }
        seen[t.name] = true
        
        resolved, ok := resolve_named_type(ctx, t)
        if !ok do return false
        
        return check_infinite_size_impl(ctx, struct_name, resolved, seen)
    
    case Pointer_Type:
        return false
    
    case Array_Type:
        return check_infinite_size_impl(ctx, struct_name, t.element_type^, seen)
    
    case Struct_Type:
        for field in t.fields {
            if check_infinite_size_impl(ctx, struct_name, field.type, seen) {
                return true
            }
        }
        return false
    }
    
    return false
}

@(private="file")
validate_type_exists :: proc(ctx: ^Semantic_Context, t: Type_Info, span: Span) -> bool {
    #partial switch typ in t {
    case Named_Type:
        _, ok := resolve_named_type(ctx, typ)
        if !ok {
            add_error(ctx, span, "Undefined type '%s'", typ.name)
            return false
        }
        return true
    case Pointer_Type:
        return validate_type_exists(ctx, typ.pointee^, span)
    case Array_Type:
        return validate_type_exists(ctx, typ.element_type^, span)
    case:
        return true
    }
}