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
    skip_struct_defs: bool, // Set to true when struct defs already generated
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

codegen :: proc(asts: map[string]^Node, sorted_packages: []string, ctx_sem: ^Semantic_Context, allocator := context.allocator) -> (res: string, err: mem.Allocator_Error) {
    sb := strings.builder_make(allocator) or_return
    ctx_cg := Codegen_Context { output_buf=sb, indent_level=0, ctx_sem=ctx_sem }

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

                    if _, is_fn := field.type.(Function_Type); is_fn {
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

codegen_node :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_node(ctx_cg, stmt)
        }

    case .Import: // NOTE(Aria): Imports are metadata for the compiler, no C code generated
    case .Expr_Statement:
        codegen_node(ctx_cg, node.payload.(Node_Expr_Statement).expr)
        strings.write_string(&ctx_cg.output_buf, ";\n")

    case .Assignment:
        assign := node.payload.(Node_Assign)
        codegen_node(ctx_cg, assign.target)
        strings.write_string(&ctx_cg.output_buf, " = ")
        codegen_node(ctx_cg, assign.value)
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
                
                if symbol_name == "main" || symbol_name in ctx_cg.ctx_sem.external_functions {
                    strings.write_string(&ctx_cg.output_buf, symbol_name)
                } else {
                    fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, actual_pkg_name, symbol_name)
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
        codegen_node(ctx_cg, fn_call.callee)
        strings.write_string(&ctx_cg.output_buf, "(")
        for arg, i in fn_call.args {
            codegen_node(ctx_cg, arg)
            if i < len(fn_call.args)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
        }
        
        strings.write_string(&ctx_cg.output_buf, ")")
    
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
        
        ctx_cg.indent_level += 1
        for stmt in for_c.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        ctx_cg.indent_level -= 1
        
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
        
        ctx_cg.indent_level += 1
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        codegen_type(ctx_cg, arr_type.element_type^)
        strings.write_string(&ctx_cg.output_buf, " ")
        strings.write_string(&ctx_cg.output_buf, for_in.iterator)
        strings.write_string(&ctx_cg.output_buf, " = ")
        codegen_node(ctx_cg, for_in.iterable)
        strings.write_string(&ctx_cg.output_buf, "[__i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, "];\n")
        
        for stmt in for_in.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        ctx_cg.indent_level -= 1
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")

    case .If: 
        if_node := node.payload.(Node_If)

        strings.write_string(&ctx_cg.output_buf, "if (")
        codegen_node(ctx_cg, if_node.condition)
        strings.write_string(&ctx_cg.output_buf, ") {\n")

        ctx_cg.indent_level += 1
        for stmt in if_node.if_body {
            codegen_indent(ctx_cg, ctx_cg.indent_level) 
            codegen_node(ctx_cg, stmt)
        }
        ctx_cg.indent_level -= 1

        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}")

        if len(if_node.else_body) > 0 {
            if len(if_node.else_body) == 1 && if_node.else_body[0].node_kind == .If {
                strings.write_string(&ctx_cg.output_buf, " else ")
                codegen_node(ctx_cg, if_node.else_body[0]) 
            } else {
                strings.write_string(&ctx_cg.output_buf, " else {\n")
                ctx_cg.indent_level += 1
                for stmt in if_node.else_body {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    codegen_node(ctx_cg, stmt)
                }
                ctx_cg.indent_level -= 1
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
        // 2) Only mangle if it's from global scope (scope 0)
        if iden.name in ctx_cg.ctx_sem.global_symbols {
            // Look up the symbol to get its package info
            sym, found := semantic_lookup_symbol(ctx_cg.ctx_sem, iden.name)
            pkg_name := ctx_cg.current_pkg_name  // Default to current package
            if found && sym.pkg_dir != "" {
                // Use the symbol's package if available
                pkg_name = filepath.base(sym.pkg_dir)
            }
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, pkg_name, iden.name)
            return
        }
        
        // Local variable or parameter - don't mangle
        strings.write_string(&ctx_cg.output_buf, iden.name)
    
    case .Index:
        index_node := node.payload.(Node_Index)
        codegen_node(ctx_cg, index_node.object)
        strings.write_string(&ctx_cg.output_buf, "[")
        codegen_node(ctx_cg, index_node.index)
        strings.write_string(&ctx_cg.output_buf, "]")
    
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
        strings.write_string(&ctx_cg.output_buf, "{")
        for elem, i in arr_lit.elements {
            codegen_node(ctx_cg, elem)
            if i < len(arr_lit.elements) - 1 {
                strings.write_string(&ctx_cg.output_buf, ", ")
            }
        }
        strings.write_string(&ctx_cg.output_buf, "}")

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
        
        // Emit compound literal
        strings.write_string(&ctx_cg.output_buf, "(Qoz_String){.data = \"")
        strings.write_string(&ctx_cg.output_buf, str_content)
        strings.write_string(&ctx_cg.output_buf, "\", .len = ")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", len(str_content))
        strings.write_string(&ctx_cg.output_buf, "}")
    
    case .Return:
        ret := node.payload.(Node_Return)
        strings.write_string(&ctx_cg.output_buf, "return ")
        codegen_node(ctx_cg, ret.value)
        strings.write_string(&ctx_cg.output_buf, ";\n")

    case .Print:
        print := node.payload.(Node_Print)
        
        // Get type of expression being printed
        expr_type := print.content.inferred_type.? or_else panic("Type not annotated")

        // Special handling for string
        if named, is_named := expr_type.(Named_Type); is_named && named.name == "string" {
            strings.write_string(&ctx_cg.output_buf, "{ Qoz_String __tmp = ")
            codegen_node(ctx_cg, print.content)
            strings.write_string(&ctx_cg.output_buf, "; printf(\"%.*s\\n\", (int)__tmp.len, __tmp.data); }\n")
            return
        }
        
        // Determine format specifier
        format_spec := ""
        switch t in expr_type {
        case Primitive_Type:
            switch t {
            case .I8, .U8, .I32, .I64: format_spec = "%d"
            case .F32, .F64: format_spec = "%f"
            case .Bool: format_spec = "%d"
            case .Void: panic("Cannot print void")
            }
        case Function_Type: panic("Cannot print function")
        case Array_Type: panic("Cannot print array")
        case Pointer_Type: panic("Cannot print pointer")
        case Struct_Type: panic("Cannot print pointer")
        case Named_Type: panic("Cannot print named type")
        case Untyped_Int: format_spec = "%d"
        case Untyped_Float: format_spec = "%f"
        }
        
        strings.write_string(&ctx_cg.output_buf, "printf(\"")
        strings.write_string(&ctx_cg.output_buf, format_spec)
        strings.write_string(&ctx_cg.output_buf, "\\n\", ")
        codegen_node(ctx_cg, print.content)
        strings.write_string(&ctx_cg.output_buf, ");\n")

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
            codegen_func_signature(ctx_cg, var_def.name, fn_def, qualified_fn_type)
            strings.write_string(&ctx_cg.output_buf, " {\n")
            for stmt in fn_def.body {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                codegen_node(ctx_cg, stmt)
            }
            // Special case: add return 0 to main 
            if var_def.name == "main" {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "return 0;\n")
            }

            strings.write_string(&ctx_cg.output_buf, "}\n\n")
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

                    if _, is_fn := field.type.(Function_Type); is_fn {
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

            codegen_type(ctx_cg, node.inferred_type.(Type_Info))
            strings.write_string(&ctx_cg.output_buf, " ")
            strings.write_string(&ctx_cg.output_buf, var_def.name)

            // If array, add size to declarator
            if arr_type, is_arr := var_type.(Array_Type); is_arr {
                strings.write_string(&ctx_cg.output_buf, "[")
                fmt.sbprintf(&ctx_cg.output_buf, "%d", arr_type.size)
                strings.write_string(&ctx_cg.output_buf, "]")
            }

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
        case .Void: strings.write_string(&ctx_cg.output_buf, "void")
        case: fmt.panicf("Internal error: cannot generate code for type %v", t)
        }
    case Array_Type:
        codegen_type(ctx_cg, t.element_type^)
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
        strings.write_string(&ctx_cg.output_buf, fn_name)
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
            codegen_type(ctx_cg, param_type)
            strings.write_string(&ctx_cg.output_buf, " ")
            strings.write_string(&ctx_cg.output_buf, param.name)
            if i < len(node.params)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
        }
    }
    strings.write_string(&ctx_cg.output_buf, ")")
}