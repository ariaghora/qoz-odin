package main

import "core:path/filepath"
import "core:fmt"
import "core:mem"
import "core:strings"

Codegen_Context :: struct {
    output_buf: strings.Builder,
    indent_level: int,
    ctx_sem: ^Semantic_Context,
    func_nesting_depth: int, // TODO(Aria): track depth for lambda lifting
    loop_counter: int,
    in_for_header: bool, 
    current_pkg_name: string,
    skip_struct_defs: bool,
    generated_array_wrappers: map[string]bool, // Track which array wrapper structs we've generated
    current_function_locals: map[string]bool, // Track parameters and local variables in current function
    defer_stack: [dynamic][dynamic]^Node, // Stack of defer lists per scope
}

MANGLE_PREFIX :: "qoz__"

should_mangle :: proc(name: string, is_external: bool) -> bool {
    if name == "main" do return false
    if is_external do return false
    return true
}

mangle_name :: proc(name: string) -> string {
    return fmt.tprintf("%s%s", MANGLE_PREFIX, name)
}

// Generate a unique name for array wrapper struct (handles nested arrays recursively)
get_array_wrapper_name :: proc(elem_type: ^Type_Info, size: int) -> string {
    type_name: string
    #partial switch t in elem_type^ {
    case Primitive_Type:
        #partial switch t {
        case .I8:  type_name = "i8"
        case .U8:  type_name = "u8"
        case .I32: type_name = "i32"
        case .I64: type_name = "i64"
        case .F32: type_name = "f32"
        case .F64: type_name = "f64"
        case: type_name = "unknown"
        }
    case Array_Type:
        // Nested array: recursively get inner array's wrapper name
        inner_wrapper := get_array_wrapper_name(t.element_type, t.size)
        // Strip the prefix to avoid double prefixing
        if strings.has_prefix(inner_wrapper, MANGLE_PREFIX) {
            type_name = inner_wrapper[len(MANGLE_PREFIX):]
        } else {
            type_name = inner_wrapper
        }
    case Pointer_Type:
        type_name = "ptr"
    case Named_Type:
        type_name, _ = strings.replace_all(t.name, ".", "_", context.temp_allocator)
    case:
        type_name = "unknown"
    }
    return fmt.tprintf("%sArray_%s_%d", MANGLE_PREFIX, type_name, size)
}

// Check if a type contains any named struct types
type_contains_struct :: proc(t: Type_Info) -> bool {
    #partial switch typ in t {
    case Named_Type:
        return true // Could be a struct
    case Array_Type:
        return type_contains_struct(typ.element_type^)
    case Pointer_Type:
        return type_contains_struct(typ.pointee^)
    case:
        return false
    }
}

// Generate struct wrapper for array type (needed for returning arrays by value in C)
// Recursively generates wrappers for nested arrays
// only_primitives: if true, only generate wrappers for arrays of primitives (skip struct-based)
codegen_array_wrapper_struct :: proc(ctx_cg: ^Codegen_Context, arr_type: Array_Type, only_primitives := false) {
    // If element type is also an array, generate its wrapper first
    if nested_arr, is_nested := arr_type.element_type^.(Array_Type); is_nested {
        codegen_array_wrapper_struct(ctx_cg, nested_arr, only_primitives)
    }
    
    wrapper_name := get_array_wrapper_name(arr_type.element_type, arr_type.size)
    
    // Check if we already generated this wrapper
    if wrapper_name in ctx_cg.generated_array_wrappers {
        return
    }
    
    // Skip struct-based arrays if only_primitives is true
    if only_primitives && type_contains_struct(arr_type.element_type^) {
        return
    }
    
    ctx_cg.generated_array_wrappers[wrapper_name] = true
    
    strings.write_string(&ctx_cg.output_buf, "typedef struct ")
    strings.write_string(&ctx_cg.output_buf, wrapper_name)
    strings.write_string(&ctx_cg.output_buf, " {\n    ")
    codegen_type(ctx_cg, arr_type.element_type^)
    fmt.sbprintf(&ctx_cg.output_buf, " data[%d];\n} ", arr_type.size)
    strings.write_string(&ctx_cg.output_buf, wrapper_name)
    strings.write_string(&ctx_cg.output_buf, ";\n\n")
}

// Returns true if the built-in was handled, false otherwise
codegen_builtin_function :: proc(ctx_cg: ^Codegen_Context, name: string, args: []^Node) -> bool {
    switch name {
    case "len":
        if len(args) != 1 do return false
        
        arg := args[0]
        arg_type := arg.inferred_type.? or_else panic("Type not inferred for len() argument")
        
        #partial switch t in arg_type {
        case Array_Type:
            // Array length is compile-time constant
            fmt.sbprintf(&ctx_cg.output_buf, "%d", t.size)
        case Named_Type:
            if t.name == "string" {
                // String length is runtime field access
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, arg)
                strings.write_string(&ctx_cg.output_buf, ").len")
            } else {
                panic("len() called on invalid type in codegen")
            }
        case:
            panic("len() called on invalid type in codegen")
        }
        return true
    
    // TODO(Aria): Add more built-ins here:
    // case "size_of":
    //     ...
    //     return true
    // case "typeof":
    //     ...
    //     return true
    
    case:
        return false
    }
}

codegen :: proc(asts: map[string]^Node, sorted_packages: []string, ctx_sem: ^Semantic_Context, allocator := context.allocator) -> (res: string, err: mem.Allocator_Error) {
    sb := strings.builder_make(allocator) or_return
    ctx_cg := Codegen_Context { 
        output_buf = sb, 
        indent_level = 0, 
        ctx_sem = ctx_sem,
        generated_array_wrappers = make(map[string]bool, allocator=allocator),
    }

    strings.write_string(&ctx_cg.output_buf, string(#load("qoz_runtime.c")))
    strings.write_string(&ctx_cg.output_buf, "\n\n")
    strings.write_string(&ctx_cg.output_buf, "#include <stdio.h>\n")

    // Pass 1: Struct forward declarations for ALL packages
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_struct_forward_decl(&ctx_cg, ast)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 1.5: Collect and forward-declare all array wrappers
    array_wrapper_names := make([dynamic]string, context.temp_allocator)
    forward_decl_map := make(map[string]bool, context.temp_allocator)
    ctx_cg.generated_array_wrappers = forward_decl_map
    for pkg_dir in sorted_packages {
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_collect_array_wrappers(&ctx_cg, ast, &array_wrapper_names)
            }
        }
    }
    // Forward declare array wrappers
    for name in array_wrapper_names {
        fmt.sbprintf(&ctx_cg.output_buf, "typedef struct %s %s;\n", name, name)
    }
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Reset the map for actual generation
    ctx_cg.generated_array_wrappers = make(map[string]bool, allocator)
    
    // Pass 1.75: Generate array wrapper bodies for primitive types ONLY
    // (These are needed by struct fields before struct definitions)
    for pkg_dir in sorted_packages {
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_scan_array_returns(&ctx_cg, ast, only_primitives = true)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 2: Struct definitions for ALL packages (complete typedefs with bodies)
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_struct_defs(&ctx_cg, ast)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 2.5: Generate array wrapper bodies for struct-based arrays
    // (These can now reference fully defined struct types)
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_scan_array_returns(&ctx_cg, ast, only_primitives = false)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 3: Function forward declarations for ALL packages
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_forward_decl(&ctx_cg, ast)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 4: Function implementations for ALL packages (skip struct defs)
    ctx_cg.skip_struct_defs = true
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_node(&ctx_cg, ast)
            }
        }
    }

    return strings.to_string(ctx_cg.output_buf), nil
}

indented :: proc(content: string, n_space: int) -> string {
    return fmt.tprintf("%s%s", strings.repeat(" ", n_space), content)
}

codegen_indent :: proc(ctx_cg: ^Codegen_Context, level: int) {
    strings.write_string(&ctx_cg.output_buf, strings.repeat(" ", ctx_cg.indent_level * 4, context.temp_allocator))
}

// Recursively scan a type for arrays and collect wrapper names (no generation)
codegen_collect_type_array_names :: proc(ctx_cg: ^Codegen_Context, type: Type_Info, names: ^[dynamic]string) {
    #partial switch t in type {
    case Array_Type:
        wrapper_name := get_array_wrapper_name(t.element_type, t.size)
        if wrapper_name not_in ctx_cg.generated_array_wrappers {
            append(names, wrapper_name)
            ctx_cg.generated_array_wrappers[wrapper_name] = true
        }
        // Recurse for nested arrays
        codegen_collect_type_array_names(ctx_cg, t.element_type^, names)
    case Pointer_Type:
        codegen_collect_type_array_names(ctx_cg, t.pointee^, names)
    case Struct_Type:
        for field in t.fields {
            codegen_collect_type_array_names(ctx_cg, field.type, names)
        }
    case Function_Type:
        codegen_collect_type_array_names(ctx_cg, t.return_type^, names)
        for param in t.params {
            codegen_collect_type_array_names(ctx_cg, param, names)
        }
    }
}

// Collect all array wrapper names without generating
codegen_collect_array_wrappers :: proc(ctx_cg: ^Codegen_Context, node: ^Node, names: ^[dynamic]string) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_collect_array_wrappers(ctx_cg, stmt, names)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        if var_def.content.node_kind == .Fn_Def {
            fn_def := var_def.content.payload.(Node_Fn_Def)
            codegen_collect_type_array_names(ctx_cg, fn_def.return_type, names)
            // Scan function body for local variables with array types
            for stmt in fn_def.body {
                codegen_collect_array_wrappers(ctx_cg, stmt, names)
            }
        } else if var_def.content.node_kind == .Type_Expr {
            // Struct definition
            type_expr := var_def.content.payload.(Node_Type_Expr)
            codegen_collect_type_array_names(ctx_cg, type_expr.type_info, names)
        } else {
            // Local variable - check if it has an explicit array type
            if explicit_type, has_explicit := var_def.explicit_type.?; has_explicit {
                codegen_collect_type_array_names(ctx_cg, explicit_type, names)
            }
            // Also check the inferred type from the initialization expression
            if inferred_type, has_inferred := var_def.content.inferred_type.?; has_inferred {
                codegen_collect_type_array_names(ctx_cg, inferred_type, names)
            }
        }
    }
}

// Recursively scan a type for arrays and generate wrappers
codegen_scan_type_for_arrays :: proc(ctx_cg: ^Codegen_Context, type: Type_Info, only_primitives := false) {
    #partial switch t in type {
    case Array_Type:
        codegen_array_wrapper_struct(ctx_cg, t, only_primitives)
    case Pointer_Type:
        codegen_scan_type_for_arrays(ctx_cg, t.pointee^, only_primitives)
    case Struct_Type:
        for field in t.fields {
            codegen_scan_type_for_arrays(ctx_cg, field.type, only_primitives)
        }
    case Function_Type:
        codegen_scan_type_for_arrays(ctx_cg, t.return_type^, only_primitives)
        for param in t.params {
            codegen_scan_type_for_arrays(ctx_cg, param, only_primitives)
        }
    }
}

// Scan AST for functions that return arrays and generate wrapper structs
codegen_scan_array_returns :: proc(ctx_cg: ^Codegen_Context, node: ^Node, only_primitives := false) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_scan_array_returns(ctx_cg, stmt, only_primitives)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        if var_def.content.node_kind == .Fn_Def {
            fn_def := var_def.content.payload.(Node_Fn_Def)
            codegen_scan_type_for_arrays(ctx_cg, fn_def.return_type, only_primitives)
            // Scan function body for local variables with array types
            for stmt in fn_def.body {
                codegen_scan_array_returns(ctx_cg, stmt, only_primitives)
            }
        } else if var_def.content.node_kind == .Type_Expr {
            // Struct definition
            type_expr := var_def.content.payload.(Node_Type_Expr)
            codegen_scan_type_for_arrays(ctx_cg, type_expr.type_info, only_primitives)
        } else {
            // Local variable - scan both explicit and inferred types
            if explicit_type, has_explicit := var_def.explicit_type.?; has_explicit {
                codegen_scan_type_for_arrays(ctx_cg, explicit_type, only_primitives)
            }
            if inferred_type, has_inferred := var_def.content.inferred_type.?; has_inferred {
                codegen_scan_type_for_arrays(ctx_cg, inferred_type, only_primitives)
            }
        }
    }
}

// Walk AST and generate only struct forward declarations (typedef struct X X;)
codegen_struct_forward_decl :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_struct_forward_decl(ctx_cg, stmt)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        
        if var_def.content.node_kind == .Type_Expr {
            type_expr := var_def.content.payload.(Node_Type_Expr)
            
            if _, is_struct := type_expr.type_info.(Struct_Type); is_struct {
                strings.write_string(&ctx_cg.output_buf, "typedef struct ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                strings.write_string(&ctx_cg.output_buf, var_def.name)
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                strings.write_string(&ctx_cg.output_buf, var_def.name)
                strings.write_string(&ctx_cg.output_buf, ";\n")
            }
        }
    }
}

// Walk AST and generate only struct type definitions
codegen_struct_defs :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_struct_defs(ctx_cg, stmt)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        
        if var_def.content.node_kind == .Type_Expr {
            // Struct typedef
            type_expr := var_def.content.payload.(Node_Type_Expr)
                
            if struct_type, is_struct := type_expr.type_info.(Struct_Type); is_struct {
                strings.write_string(&ctx_cg.output_buf, "typedef struct ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                strings.write_string(&ctx_cg.output_buf, var_def.name) 
                strings.write_string(&ctx_cg.output_buf, " {\n")
                ctx_cg.indent_level += 1
                
                for field in struct_type.fields {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)

                    // Resolve the field type to handle type aliases
                    resolved_field_type := field.type
                    if named_type, is_named := field.type.(Named_Type); is_named {
                        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                            resolved_field_type = resolved
                        }
                    }

                    if _, is_fn := resolved_field_type.(Function_Type); is_fn {
                        codegen_type(ctx_cg, field.type, field.name)
                    } else {
                        codegen_type(ctx_cg, field.type)
                        strings.write_string(&ctx_cg.output_buf, " ")
                        strings.write_string(&ctx_cg.output_buf, field.name)
                    }

                    strings.write_string(&ctx_cg.output_buf, ";\n")
                }
                
                ctx_cg.indent_level -= 1
                strings.write_string(&ctx_cg.output_buf, "} ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                strings.write_string(&ctx_cg.output_buf, var_def.name)
                strings.write_string(&ctx_cg.output_buf, ";\n\n")
            }
        }
    }
}

codegen_emit_defers :: proc(ctx_cg: ^Codegen_Context) {
    if len(ctx_cg.defer_stack) == 0 do return
    
    defer_list := ctx_cg.defer_stack[len(ctx_cg.defer_stack) - 1]
    
    // Execute defers in reverse order (LIFO)
    for i := len(defer_list) - 1; i >= 0; i -= 1 {
        defer_node := defer_list[i].payload.(Node_Defer)
        for stmt in defer_node.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
    }
}

codegen_print_impl :: proc(ctx_cg: ^Codegen_Context, args: [dynamic]^Node, add_newline: bool) {
    if len(args) == 0 {
        if add_newline {
            strings.write_string(&ctx_cg.output_buf, "printf(\"\\n\");\n")
        }
        return
    }
    
    // Build format string and collect arguments
    format_parts := make([dynamic]string, context.temp_allocator)
    
    for arg in args {
        expr_type := arg.inferred_type.? or_else panic("Type not annotated")
        
        // Determine format specifier
        format_spec := ""
        #partial switch t in expr_type {
        case Primitive_Type:
            switch t {
            case .I8, .U8, .I32, .I64: format_spec = "%d"
            case .F32, .F64: format_spec = "%f"
            case .Bool: format_spec = "%d"
            case .Cstring: format_spec = "%s"
            case .Void: panic("Cannot print void")
            }
        case Named_Type:
            if t.name == "string" {
                format_spec = "%.*s"
            } else {
                panic("Cannot print named type")
            }
        case Untyped_Int: format_spec = "%d"
        case Untyped_Float: format_spec = "%f"
        case Untyped_String: format_spec = "%.*s"  // Treat like string
        case:
            panic("Cannot print this type")
        }
        
        append(&format_parts, format_spec)
    }
    
    // Write printf call
    strings.write_string(&ctx_cg.output_buf, "printf(\"")
    for part, i in format_parts {
        strings.write_string(&ctx_cg.output_buf, part)
        if i < len(format_parts) - 1 {
            strings.write_string(&ctx_cg.output_buf, " ")
        }
    }
    if add_newline {
        strings.write_string(&ctx_cg.output_buf, "\\n")
    }
    strings.write_string(&ctx_cg.output_buf, "\"")
    
    // Write arguments
    for arg in args {
        strings.write_string(&ctx_cg.output_buf, ", ")
        
        expr_type := arg.inferred_type.? or_else panic("Type not annotated")
        
        // Special handling for string and untyped string
        is_str := false
        if named, is_named := expr_type.(Named_Type); is_named && named.name == "string" {
            is_str = true
        }
        if _, is_untyped_str := expr_type.(Untyped_String); is_untyped_str {
            is_str = true
        }
        
        if is_str {
            strings.write_string(&ctx_cg.output_buf, "(int)(")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ").len, (")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ").data")
        } else {
            codegen_node(ctx_cg, arg)
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, ");\n")
}

codegen_node :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_node(ctx_cg, stmt)
        }

    case .Import: // NOTE(Aria): Imports are metadata for the compiler, no C code generated
    case .Link:   // NOTE(Aria): Link directives are metadata for the compiler, no C code generated
    case .Expr_Statement:
        codegen_node(ctx_cg, node.payload.(Node_Expr_Statement).expr)
        strings.write_string(&ctx_cg.output_buf, ";\n")

    case .Assignment:
        assign := node.payload.(Node_Assign)
        
        // For compound assignments, generate: target = target op value
        if op_kind, has_op := assign.compound_op.?; has_op {
            codegen_node(ctx_cg, assign.target)
            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, assign.target)
            
            #partial switch op_kind {
            case .Plus:  strings.write_string(&ctx_cg.output_buf, "+")
            case .Minus: strings.write_string(&ctx_cg.output_buf, "-")
            case .Star:  strings.write_string(&ctx_cg.output_buf, "*")
            case .Slash: strings.write_string(&ctx_cg.output_buf, "/")
            }
            
            codegen_node(ctx_cg, assign.value)
        } else {
            // Regular assignment
            codegen_node(ctx_cg, assign.target)
            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, assign.value)
        }
        
        if !ctx_cg.in_for_header {
            strings.write_string(&ctx_cg.output_buf, ";\n")
        }
        
    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_node(ctx_cg, binop.left)
        strings.write_string(&ctx_cg.output_buf, binop.op.source)
        codegen_node(ctx_cg, binop.right)
        strings.write_string(&ctx_cg.output_buf, ")")
    
    case .Cast:
        cast_node := node.payload.(Node_Cast)
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_type(ctx_cg, cast_node.target_type)
        strings.write_string(&ctx_cg.output_buf, ")(")
        codegen_node(ctx_cg, cast_node.expr)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Un_Op:
        binop := node.payload.(Node_Un_Op)
        strings.write_string(&ctx_cg.output_buf, binop.op.source)
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_node(ctx_cg, binop.operand)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Field_Access:
        field_access := node.payload.(Node_Field_Access)
        
            
        // Check if this is a qualified name (package.symbol)
        if field_access.object.node_kind == .Identifier {
            iden := field_access.object.payload.(Node_Identifier)
            // Check if this identifier is a package alias
            if pkg_dir, is_pkg := ctx_cg.ctx_sem.import_aliases[iden.name]; is_pkg {
                // Get actual package name from directory path
                actual_pkg_name := filepath.base(pkg_dir)  
                symbol_name := field_access.field_name
                
                if symbol_name == "main" {
                    strings.write_string(&ctx_cg.output_buf, symbol_name)
                } else {
                    // Check if this is an external function
                    qualified_symbol_name := fmt.tprintf("%s.%s", actual_pkg_name, symbol_name)
                    is_external := qualified_symbol_name in ctx_cg.ctx_sem.external_functions
                    
                    if is_external {
                        // Look up the symbol to get the external_name
                        external_c_name := symbol_name  // Default to symbol name
                        if pkg_info, pkg_exists := ctx_cg.ctx_sem.packages[pkg_dir]; pkg_exists {
                            if sym, sym_exists := pkg_info.symbols[symbol_name]; sym_exists {
                                if ext_name, has_ext := sym.external_name.?; has_ext {
                                    external_c_name = ext_name
                                }
                            }
                        }
                        strings.write_string(&ctx_cg.output_buf, external_c_name)
                    } else {
                        // Regular package symbols get mangled
                        fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, actual_pkg_name, symbol_name)
                    }
                }
                return
            }
        }

        object_type := field_access.object.inferred_type.? or_else panic("Internal error: type not annotated")

        // Resolve Named_Type if needed
        actual_type := object_type
        if named, is_named := object_type.(Named_Type); is_named {
            sym, _ := semantic_lookup_symbol(ctx_cg.ctx_sem, named.name)
            actual_type = sym.type
        }

        codegen_node(ctx_cg, field_access.object)
        if _, is_ptr := actual_type.(Pointer_Type); is_ptr {
            strings.write_string(&ctx_cg.output_buf, "->")
        } else {
            strings.write_string(&ctx_cg.output_buf, ".")
        }

        strings.write_string(&ctx_cg.output_buf, field_access.field_name)

    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)

    case .Fn_Call:
        fn_call := node.payload.(Node_Call)
        
        // Check if this is a built-in function
        handled := false
        if fn_call.callee.node_kind == .Identifier {
            callee_name := fn_call.callee.payload.(Node_Identifier).name
            handled = codegen_builtin_function(ctx_cg, callee_name, fn_call.args[:])
        }
        
        // If not a built-in, generate regular function call
        if !handled {
            // Get function type to check parameter types
            callee_type := fn_call.callee.inferred_type.? or_else Primitive_Type.Void
            fn_type, is_fn := callee_type.(Function_Type)
            
            codegen_node(ctx_cg, fn_call.callee)
            strings.write_string(&ctx_cg.output_buf, "(")
            for arg, i in fn_call.args {
                // Check if we need to convert string to cstring
                arg_type := arg.inferred_type.? or_else Primitive_Type.Void
                needs_conversion := false
                
                if is_fn && i < len(fn_type.params) {
                    param_type := fn_type.params[i]
                    // Check if arg is string and param expects cstring
                    // (Allows passing string variables to cstring parameters for FFI convenience)
                    if is_string_type(arg_type) && is_cstring_type(param_type) {
                        needs_conversion = true
                    }
                }
                
                if needs_conversion {
                    // Emit .data to get underlying C string
                    strings.write_string(&ctx_cg.output_buf, "(")
                    codegen_node(ctx_cg, arg)
                    strings.write_string(&ctx_cg.output_buf, ").data")
                } else {
                    codegen_node(ctx_cg, arg)
                }
                
                if i < len(fn_call.args)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
            }
            
            strings.write_string(&ctx_cg.output_buf, ")")
        }
    
    case .For_C:
        for_c := node.payload.(Node_For_C)
        
        strings.write_string(&ctx_cg.output_buf, "for (")
        
        ctx_cg.in_for_header = true
        codegen_node(ctx_cg, for_c.init)
        strings.write_string(&ctx_cg.output_buf, "; ")
        codegen_node(ctx_cg, for_c.condition)
        strings.write_string(&ctx_cg.output_buf, "; ")
        codegen_node(ctx_cg, for_c.post)
        ctx_cg.in_for_header = false
        
        strings.write_string(&ctx_cg.output_buf, ") {\n")
        
        // Push new defer scope for loop body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        for stmt in for_c.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting loop body (while still at correct indent level)
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1
        
        // Pop defer scope after emitting
        pop(&ctx_cg.defer_stack)
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")

    case .For_In:
        for_in := node.payload.(Node_For_In)
        
        iterable_type := for_in.iterable.inferred_type.? or_else panic("Type not annotated")
        arr_type := iterable_type.(Array_Type)
        
        // Generate unique index variable
        loop_idx := ctx_cg.loop_counter
        ctx_cg.loop_counter += 1
        
        strings.write_string(&ctx_cg.output_buf, "for (int32_t __i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, " = 0; __i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, " < ")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", arr_type.size)
        strings.write_string(&ctx_cg.output_buf, "; __i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, "++) {\n")
        
        // Push new defer scope for loop body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        codegen_type(ctx_cg, arr_type.element_type^)
        strings.write_string(&ctx_cg.output_buf, " ")
        strings.write_string(&ctx_cg.output_buf, for_in.iterator)
        strings.write_string(&ctx_cg.output_buf, " = ")
        codegen_node(ctx_cg, for_in.iterable)
        strings.write_string(&ctx_cg.output_buf, ".data[__i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, "];\n")
        
        for stmt in for_in.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting loop body
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1
        
        // Pop defer scope
        pop(&ctx_cg.defer_stack)
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    
    case .While:
        while_loop := node.payload.(Node_While)
        
        strings.write_string(&ctx_cg.output_buf, "while (")
        codegen_node(ctx_cg, while_loop.condition)
        strings.write_string(&ctx_cg.output_buf, ") {\n")
        
        // Push new defer scope for loop body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        for stmt in while_loop.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting loop body (while still at correct indent level)
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1
        
        // Pop defer scope after emitting
        pop(&ctx_cg.defer_stack)
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")

    case .If: 
        if_node := node.payload.(Node_If)

        strings.write_string(&ctx_cg.output_buf, "if (")
        codegen_node(ctx_cg, if_node.condition)
        strings.write_string(&ctx_cg.output_buf, ") {\n")

        // Push new defer scope for if body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        for stmt in if_node.if_body {
            codegen_indent(ctx_cg, ctx_cg.indent_level) 
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting if body
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1

        // Pop defer scope
        pop(&ctx_cg.defer_stack)

        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}")

        if len(if_node.else_body) > 0 {
            if len(if_node.else_body) == 1 && if_node.else_body[0].node_kind == .If {
                strings.write_string(&ctx_cg.output_buf, " else ")
                codegen_node(ctx_cg, if_node.else_body[0]) 
            } else {
                strings.write_string(&ctx_cg.output_buf, " else {\n")
                
                // Push new defer scope for else body
                append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
                
                ctx_cg.indent_level += 1
                for stmt in if_node.else_body {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    codegen_node(ctx_cg, stmt)
                }
                
                // Emit defers before exiting else body
                codegen_emit_defers(ctx_cg)
                
                ctx_cg.indent_level -= 1
                
                // Pop defer scope
                pop(&ctx_cg.defer_stack)
                
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "}\n")
            }
        } else {
            strings.write_string(&ctx_cg.output_buf, "\n")
        }

    case .Identifier:
        iden := node.payload.(Node_Identifier)

        // Identifiers should be mangled at some conditions.
        // 1) Don't mangle main or external functions
        if iden.name == "main" || iden.name in ctx_cg.ctx_sem.external_functions {
            strings.write_string(&ctx_cg.output_buf, iden.name)
            return
        }
        
        // 2) Check if it's a local variable or parameter (no mangling)
        if iden.name in ctx_cg.current_function_locals {
            strings.write_string(&ctx_cg.output_buf, iden.name)
            return
        }
        
        // 3) Check if it's a package-level symbol (needs mangling)
        // Look in current package's symbols
        pkg_dir := ""
        for dir, pkg_info in ctx_cg.ctx_sem.packages {
            if filepath.base(dir) == ctx_cg.current_pkg_name {
                pkg_dir = dir
                break
            }
        }
        
        is_package_symbol := false
        if pkg_dir != "" {
            if pkg_info, ok := ctx_cg.ctx_sem.packages[pkg_dir]; ok {
                if _, found := pkg_info.symbols[iden.name]; found {
                    is_package_symbol = true
                }
            }
        }
        
        if is_package_symbol || iden.name in ctx_cg.ctx_sem.global_symbols {
            // Mangle with package name
            pkg_name := ctx_cg.current_pkg_name
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, pkg_name, iden.name)
            return
        }
        
        // Local variable or parameter - don't mangle
        strings.write_string(&ctx_cg.output_buf, iden.name)
    
    case .Index:
        index_node := node.payload.(Node_Index)
        
        // Check if we're indexing an array type (which uses wrapper struct)
        obj_type := index_node.object.inferred_type.? or_else Primitive_Type.Void
        if _, is_array := obj_type.(Array_Type); is_array {
            // Access through .data field of wrapper struct
            codegen_node(ctx_cg, index_node.object)
            strings.write_string(&ctx_cg.output_buf, ".data[")
            codegen_node(ctx_cg, index_node.index)
            strings.write_string(&ctx_cg.output_buf, "]")
        } else {
            // Regular pointer/array indexing
            codegen_node(ctx_cg, index_node.object)
            strings.write_string(&ctx_cg.output_buf, "[")
            codegen_node(ctx_cg, index_node.index)
            strings.write_string(&ctx_cg.output_buf, "]")
        }
    
    case .Len:
        len_node := node.payload.(Node_Len)
        value_type := len_node.value.inferred_type.? or_else panic("Type not inferred")
        
        #partial switch t in value_type {
        case Array_Type:
            // Array length is compile-time constant
            fmt.sbprintf(&ctx_cg.output_buf, "%d", t.size)
        case Named_Type:
            if t.name == "string" {
                // String length is runtime field access
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, len_node.value)
                strings.write_string(&ctx_cg.output_buf, ").len")
            } else {
                panic("len() called on invalid type")
            }
        case:
            panic("len() called on invalid type")
        }

    case .Literal_Arr:
        arr_lit := node.payload.(Node_Array_Literal)
        arr_type := node.inferred_type.?.(Array_Type)
        wrapper_name := get_array_wrapper_name(arr_type.element_type, arr_type.size)
        
        // Generate: (WrapperStruct){{elem1, elem2, ...}}
        strings.write_string(&ctx_cg.output_buf, "(")
        strings.write_string(&ctx_cg.output_buf, wrapper_name)
        strings.write_string(&ctx_cg.output_buf, "){{")
        for elem, i in arr_lit.elements {
            codegen_node(ctx_cg, elem)
            if i < len(arr_lit.elements) - 1 {
                strings.write_string(&ctx_cg.output_buf, ", ")
            }
        }
        strings.write_string(&ctx_cg.output_buf, "}}")

    case .Literal_Number:
        lit := node.payload.(Node_Literal_Number)
        src := lit.content.source
        strings.write_string(&ctx_cg.output_buf, src)
    
    case .Literal_Nil:
        strings.write_string(&ctx_cg.output_buf, "NULL")
    
    case .Literal_String:
        lit := node.payload.(Node_Literal_String)
    
        str_content := lit.content.source
        if strings.has_prefix(str_content, "\"") {
            str_content = str_content[1:len(str_content)-1]
        }
        
        // Check inferred type - if cstring, emit C string literal, otherwise Qoz_String
        inferred_type := node.inferred_type.? or_else Untyped_String{}
        
        is_cstring := false
        if prim, ok := inferred_type.(Primitive_Type); ok && prim == .Cstring {
            is_cstring = true
        }
        
        if is_cstring {
            // Emit C string literal directly
            strings.write_string(&ctx_cg.output_buf, "\"")
            strings.write_string(&ctx_cg.output_buf, str_content)
            strings.write_string(&ctx_cg.output_buf, "\"")
        } else {
            // Emit Qoz_String compound literal
            strings.write_string(&ctx_cg.output_buf, "(Qoz_String){.data = \"")
            strings.write_string(&ctx_cg.output_buf, str_content)
            strings.write_string(&ctx_cg.output_buf, "\", .len = ")
            fmt.sbprintf(&ctx_cg.output_buf, "%d", len(str_content))
            strings.write_string(&ctx_cg.output_buf, "}")
        }
    
    case .Defer:
        // Collect defer, don't emit yet
        defer_node := node.payload.(Node_Defer)
        if len(ctx_cg.defer_stack) > 0 {
            append(&ctx_cg.defer_stack[len(ctx_cg.defer_stack) - 1], node)
        }
    
    case .Return:
        ret := node.payload.(Node_Return)
        
        // Emit all defers in current scope in reverse order
        codegen_emit_defers(ctx_cg)
        
        strings.write_string(&ctx_cg.output_buf, "return")
        if ret.value != nil {
            strings.write_string(&ctx_cg.output_buf, " ")
            codegen_node(ctx_cg, ret.value)
        }
        strings.write_string(&ctx_cg.output_buf, ";\n")

    case .Print:
        print := node.payload.(Node_Print)
        codegen_print_impl(ctx_cg, print.args, false)
    
    case .Println:
        println := node.payload.(Node_Println)
        codegen_print_impl(ctx_cg, println.args, true)

    case .Size_Of:
        sizeof_op := node.payload.(Node_Size_Of)
        strings.write_string(&ctx_cg.output_buf, "sizeof(")
        codegen_node(ctx_cg, sizeof_op.type)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Struct_Literal:
        struct_lit := node.payload.(Node_Struct_Literal)
        
        // Emit type name for compound literal
        strings.write_string(&ctx_cg.output_buf, "(")
        // Use inferred_type if available, otherwise fall back to struct_lit.type_name
        type_to_generate: Type_Info
        if inferred, ok := node.inferred_type.?; ok {
            // If it's a Named_Type and not qualified, qualify it with current package
            if named, is_named := inferred.(Named_Type); is_named {
                if !strings.contains(named.name, ".") && ctx_cg.current_pkg_name != "" {
                    // Qualify with current package
                    type_to_generate = Named_Type{name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, named.name)}
                } else {
                    type_to_generate = inferred
                }
            } else {
                type_to_generate = inferred
            }
        } else {
            type_to_generate = Named_Type{name = struct_lit.type_name}
        }
        codegen_type(ctx_cg, type_to_generate)
        strings.write_string(&ctx_cg.output_buf, ")")
        
        // Then the initializer list
        strings.write_string(&ctx_cg.output_buf, "{")
        for field_init, i in struct_lit.field_inits {
            strings.write_string(&ctx_cg.output_buf, ".")
            strings.write_string(&ctx_cg.output_buf, field_init.name)
            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, field_init.value)
            if i < len(struct_lit.field_inits) - 1 {
                strings.write_string(&ctx_cg.output_buf, ", ")
            }
        }
        strings.write_string(&ctx_cg.output_buf, "}")

    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)

        //| Special case, where variable is function definition
        //+----------------------------------------------------
        if var_def.content.node_kind == .Fn_Def {
            if ctx_cg.func_nesting_depth > 0 {
                panic("TODO(Aria): implement lambda lifting for nested function")
            }

            fn_def := var_def.content.payload.(Node_Fn_Def)

            if fn_def.is_external do return

            // Look up qualified type from current package's symbol table
            qualified_fn_type: Maybe(Function_Type)
            // Find the package dir that matches current package name
            for pkg_dir, pkg_info in ctx_cg.ctx_sem.packages {
                if filepath.base(pkg_dir) == ctx_cg.current_pkg_name {
                    if sym, found := pkg_info.symbols[var_def.name]; found {
                        if fn_type, is_fn := sym.type.(Function_Type); is_fn {
                            qualified_fn_type = fn_type
                            break
                        }
                    }
                }
            }

            ctx_cg.func_nesting_depth += 1
            ctx_cg.indent_level += 1
            
            // Track parameters and local variables in this function
            clear(&ctx_cg.current_function_locals)
            for param in fn_def.params {
                ctx_cg.current_function_locals[param.name] = true
            }
            
            // Push a new defer scope for this function
            append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
            
            codegen_func_signature(ctx_cg, var_def.name, fn_def, qualified_fn_type)
            strings.write_string(&ctx_cg.output_buf, " {\n")
            for stmt in fn_def.body {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                codegen_node(ctx_cg, stmt)
            }
            
            // Emit defers before function exit
            codegen_emit_defers(ctx_cg)
            
            // Special case: add return 0 to main 
            if var_def.name == "main" {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "return 0;\n")
            }

            strings.write_string(&ctx_cg.output_buf, "}\n\n")
            
            // Pop defer scope
            pop(&ctx_cg.defer_stack)
            
            ctx_cg.func_nesting_depth -= 1
            ctx_cg.indent_level -= 1
        } else if var_def.content.node_kind == .Type_Expr {
            // Struct typedef - skip if already generated in Pass 2
            if ctx_cg.skip_struct_defs do return
            
            type_expr := var_def.content.payload.(Node_Type_Expr)
                
            if struct_type, is_struct := type_expr.type_info.(Struct_Type); is_struct {
                strings.write_string(&ctx_cg.output_buf, "typedef struct ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                strings.write_string(&ctx_cg.output_buf, var_def.name) 
                strings.write_string(&ctx_cg.output_buf, " {\n")
                ctx_cg.indent_level += 1
                
                for field in struct_type.fields {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)

                    // Resolve the field type to handle type aliases
                    resolved_field_type := field.type
                    if named_type, is_named := field.type.(Named_Type); is_named {
                        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                            resolved_field_type = resolved
                        }
                    }

                    if _, is_fn := resolved_field_type.(Function_Type); is_fn {
                        codegen_type(ctx_cg, field.type, field.name)
                    } else {
                        codegen_type(ctx_cg, field.type)
                        strings.write_string(&ctx_cg.output_buf, " ")
                        strings.write_string(&ctx_cg.output_buf, field.name)
                    }

                    strings.write_string(&ctx_cg.output_buf, ";\n")
                }
                
                ctx_cg.indent_level -= 1
                strings.write_string(&ctx_cg.output_buf, "} ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                strings.write_string(&ctx_cg.output_buf, var_def.name)
                strings.write_string(&ctx_cg.output_buf, ";\n\n")
            }
        } else {
            //| Regular case
            //+-------------
            var_type := node.inferred_type.(Type_Info)

            // Track local variable in current function
            if ctx_cg.func_nesting_depth > 0 {
                ctx_cg.current_function_locals[var_def.name] = true
            }

            codegen_type(ctx_cg, node.inferred_type.(Type_Info))
            strings.write_string(&ctx_cg.output_buf, " ")
            
            // Add package prefix for top-level variables
            if ctx_cg.func_nesting_depth == 0 {
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
            }
            strings.write_string(&ctx_cg.output_buf, var_def.name)

            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, var_def.content)
            if !ctx_cg.in_for_header {
                strings.write_string(&ctx_cg.output_buf, ";\n")
            }
        }
    
    case .Del:
        del_node := node.payload.(Node_Del)
        
        // Check what we're freeing
        ptr_type := del_node.pointer.inferred_type.? or_else Primitive_Type.Void
        
        // Call allocator.free(allocator.data, ptr)
        #partial switch t in ptr_type {
        case Named_Type:
            if t.name == "string" {
                // Free string's internal data
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".free(")
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".data, (void*)")
                codegen_node(ctx_cg, del_node.pointer)
                strings.write_string(&ctx_cg.output_buf, ".data);\n")
            }
        case Pointer_Type:
            // Direct pointer free
            codegen_node(ctx_cg, del_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".free(")
            codegen_node(ctx_cg, del_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".data, ")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        case Array_Type:
            // Free array pointer
            codegen_node(ctx_cg, del_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".free(")
            codegen_node(ctx_cg, del_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".data, ")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        case:
            // Fallback - direct pointer free
            codegen_node(ctx_cg, del_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".free(")
            codegen_node(ctx_cg, del_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".data, ")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        }
        
    case: fmt.panicf("Internal error: cannot generate code for node %v", node.node_kind)
    }
}

codegen_type :: proc(ctx_cg: ^Codegen_Context, type: Type_Info, name: string = "") {
    #partial switch t in type {
    case Function_Type:
        // C function pointer format: return_type (*)(params)
        codegen_type(ctx_cg, t.return_type^)
        strings.write_string(&ctx_cg.output_buf, " (*")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, ")(")

        if len(t.params) == 0 {
            strings.write_string(&ctx_cg.output_buf, "void")
        } else {
            for param, i in t.params {
                codegen_type(ctx_cg, param)
                if i < len(t.params) - 1 {
                    strings.write_string(&ctx_cg.output_buf, ", ")
                }
            }
        }
        strings.write_string(&ctx_cg.output_buf, ")")

    case Primitive_Type:
        #partial switch t {
        case .I8:  strings.write_string(&ctx_cg.output_buf, "int8_t")
        case .U8:  strings.write_string(&ctx_cg.output_buf, "uint8_t")
        case .I32: strings.write_string(&ctx_cg.output_buf, "int32_t")
        case .I64: strings.write_string(&ctx_cg.output_buf, "int64_t")
        case .F32: strings.write_string(&ctx_cg.output_buf, "float")
        case .F64: strings.write_string(&ctx_cg.output_buf, "double")
        case .Bool: strings.write_string(&ctx_cg.output_buf, "_Bool")
        case .Cstring: strings.write_string(&ctx_cg.output_buf, "const char*")
        case .Void: strings.write_string(&ctx_cg.output_buf, "void")
        case: fmt.panicf("Internal error: cannot generate code for type %v", t)
        }
    case Array_Type:
        // Arrays are wrapped in structs for return values
        wrapper_name := get_array_wrapper_name(t.element_type, t.size)
        strings.write_string(&ctx_cg.output_buf, wrapper_name)
    case Pointer_Type:
        codegen_type(ctx_cg, t.pointee^)
        strings.write_string(&ctx_cg.output_buf, "*")
    case Struct_Type:
        panic("Internal error: FIXME(Aria): Cannot inline anonymous struct type")
    case Named_Type:
        // Check for builtin string type
        if t.name == "string" || t.name == "strings.string" {
            strings.write_string(&ctx_cg.output_buf, "Qoz_String")
            return
        }
        
        // Check if this is a type alias - if so, resolve and generate the underlying type
        // But only for non-struct types (primitives, function types, etc.)
        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, t, ctx_cg.current_pkg_name); ok {
            // If resolved to a non-struct, non-named type, it's a pure alias
            #partial switch r in resolved {
            case Function_Type, Primitive_Type, Pointer_Type, Array_Type:
                // This is a type alias to a concrete type, generate the underlying type
                codegen_type(ctx_cg, resolved, name)
                return
            case Struct_Type:
                // This is a typedef for a struct, keep using the name (don't inline the struct)
                // Fall through to normal Named_Type handling
            case Named_Type:
                // Still named after resolution, fall through
            }
        }
        
        // Check if type name contains a dot (qualified)
        if strings.contains(t.name, ".") {
            // Qualified name like "mem.Allocator" -> "qoz__mem__Allocator"
            mangled_name, _ := strings.replace_all(t.name, ".", "__", context.temp_allocator)
            strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
            strings.write_string(&ctx_cg.output_buf, mangled_name)
        } else {
            // Unqualified name like "Arena" -> qualify with current package
            strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
            if ctx_cg.current_pkg_name != "" {
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
            }
            strings.write_string(&ctx_cg.output_buf, t.name)
        }
    case Untyped_Int:
        strings.write_string(&ctx_cg.output_buf, "int64_t")
    case Untyped_Float:
        strings.write_string(&ctx_cg.output_buf, "double")
    case Untyped_String:
        // Default to Qoz_String for untyped strings
        strings.write_string(&ctx_cg.output_buf, "Qoz_String")
    case: fmt.panicf("Internal error: cannot generate code for type %v", t)
    }
    
}

codegen_forward_decl :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    if node.node_kind == .Program {
        for stmt in node.payload.(Node_Statement_List).nodes {
            if stmt.node_kind == .Var_Def {
                var_def := stmt.payload.(Node_Var_Def)
                if var_def.content.node_kind == .Fn_Def {
                    // function signature generation
                    // type func_name(..params)
                    // Look up qualified type from current package's symbol table
                    fn_def := var_def.content.payload.(Node_Fn_Def)
                    qualified_fn_type: Maybe(Function_Type)
                    // Find the package dir that matches current package name
                    for pkg_dir, pkg_info in ctx_cg.ctx_sem.packages {
                        if filepath.base(pkg_dir) == ctx_cg.current_pkg_name {
                            if sym, found := pkg_info.symbols[var_def.name]; found {
                                if fn_type, is_fn := sym.type.(Function_Type); is_fn {
                                    qualified_fn_type = fn_type
                                    break
                                }
                            }
                        }
                    }
                    codegen_func_signature(ctx_cg, var_def.name, fn_def, qualified_fn_type)
                    strings.write_string(&ctx_cg.output_buf, ";\n")
                    
                    // Generate #define alias for external functions with external_name
                    if fn_def.is_external {
                        if ext_name, has_ext := fn_def.external_name.?; has_ext {
                            // Mangle the Qoz name according to package
                            mangled_name: string
                            if ctx_cg.current_pkg_name != "" {
                                mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, var_def.name)
                            } else {
                                mangled_name = fmt.tprintf("%s%s", MANGLE_PREFIX, var_def.name)
                            }
                            fmt.sbprintf(&ctx_cg.output_buf, "#define %s %s\n", mangled_name, ext_name)
                        }
                    }
                }

                // Forward declare structs
                if var_def.content.node_kind == .Type_Expr {
                    type_expr := var_def.content.payload.(Node_Type_Expr)
                    if _, is_struct := type_expr.type_info.(Struct_Type); is_struct {
                        strings.write_string(&ctx_cg.output_buf, "typedef struct ")
                        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                        strings.write_string(&ctx_cg.output_buf, "__")
                        strings.write_string(&ctx_cg.output_buf, var_def.name)
                        strings.write_string(&ctx_cg.output_buf, " ")
                        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                        strings.write_string(&ctx_cg.output_buf, "__")
                        strings.write_string(&ctx_cg.output_buf, var_def.name)
                        strings.write_string(&ctx_cg.output_buf, ";\n")
                    }
                }
            }
        }
    }
}

codegen_func_signature :: proc(ctx_cg: ^Codegen_Context, fn_name: string, node: Node_Fn_Def, qualified_type: Maybe(Function_Type) = nil) {
    // Special case for main, we alter the signature to match strict C main requirement
    if fn_name == "main" {
        strings.write_string(&ctx_cg.output_buf, "int32_t main(void)")
        return
    }

    // Use qualified type if available, otherwise fall back to node's type
    return_type := node.return_type
    param_types := node.params
    if fn_type, ok := qualified_type.?; ok {
        return_type = fn_type.return_type^
        // Note: params still come from node for names, but types from qualified_type
    }

    codegen_type(ctx_cg, return_type)
    strings.write_string(&ctx_cg.output_buf, " ")

    // Mangle unless external
    if node.is_external {
        // Declare with the C symbol name (external_name if provided)
        c_symbol_name := fn_name
        if ext_name, has_ext := node.external_name.?; has_ext {
            c_symbol_name = ext_name
        }
        strings.write_string(&ctx_cg.output_buf, c_symbol_name)
    } else {
        // Use current package name from context
        if ctx_cg.current_pkg_name != "" {
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, fn_name)
        } else {
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s", MANGLE_PREFIX, fn_name)
        }
    }

    strings.write_string(&ctx_cg.output_buf, "(")
    if len(node.params) == 0 {
        strings.write_string(&ctx_cg.output_buf, "void")
    } else {
        for param, i in node.params {
            // Use qualified param type if available
            param_type := param.type
            if fn_type, ok := qualified_type.?; ok && i < len(fn_type.params) {
                param_type = fn_type.params[i]
            }
            
            // Resolve named types to check if it's actually a function pointer
            resolved_param_type := param_type
            if named, is_named := param_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
                    resolved_param_type = resolved
                }
            }
            
            // For function pointer types, the name must be inside the type declaration
            if _, is_fn_ptr := resolved_param_type.(Function_Type); is_fn_ptr {
                codegen_type(ctx_cg, param_type, param.name)
            } else {
                codegen_type(ctx_cg, param_type)
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, param.name)
            }
            if i < len(node.params)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
        }
    }
    strings.write_string(&ctx_cg.output_buf, ")")
}