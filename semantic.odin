package main 

import "core:fmt"
import "core:mem"

Symbol :: struct {
    name: string,
    type: Type_Info,
}

Scope :: struct {
    symbols: map[string]Symbol,
    parent: ^Scope
}

Semantic_Error :: struct {
    message: string,
    span: Span,
}

Semantic_Context :: struct {
    current_scope: ^Scope,
    errors: [dynamic]Semantic_Error,
    allocator: mem.Allocator,
    current_function_return_type: Maybe(Type_Info),
    current_struct_name: Maybe(string), 
    external_functions: map[string]bool,
    global_symbols: map[string]bool
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

semantic_define_symbol :: proc(ctx: ^Semantic_Context, name: string, type: Type_Info, span: Span) -> bool {
    scope := ctx.current_scope
    if name in ctx.current_scope.symbols {
        add_error(ctx, span, "Symbol '%s' already defined in this scope", name)
        return false
    }
    ctx.current_scope.symbols[name] = Symbol { name=name, type=type }
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

semantic_analyze :: proc(root: ^Node, allocator := context.allocator) -> Semantic_Context {
    ctx := Semantic_Context {
        allocator=allocator,
        errors=make([dynamic]Semantic_Error, allocator),
        external_functions=make(map[string]bool, allocator),
        global_symbols=make(map[string]bool, allocator),
    }

    semantic_push_scope(&ctx)
    
    // Pass 1: Collect all top-level declarations
    // - Walks through global scope only (Program node and top-level Var_Defs)
    // - Registers all global symbols (functions, types, global variables) in symbol table
    // - Determines types syntactically without analyzing function bodies or initializers
    // - Enables forward references: functions can call other functions defined later
    // - Does NOT recurse into function bodies, only collects their signatures
    // - After this pass, all global names are known and can be referenced
    collect_declarations(&ctx, root)
    
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
    check_node(&ctx, root)

    main_span := Span{start = 0, end = 0}
    main_sym, has_main := ctx.current_scope.symbols["main"]

    if root.node_kind == .Program {
        for stmt in root.payload.(Node_Statement_List).nodes {
            if stmt.node_kind == .Var_Def {
                var_def := stmt.payload.(Node_Var_Def)
                if var_def.name == "main" {
                    main_span = stmt.span
                    break
                }
            }
        }
    }

    if !has_main {
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

// Pass 1: Collect declarations without analyzing bodies
collect_declarations :: proc(ctx: ^Semantic_Context, node: ^Node) {
    if node == nil do return
    
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            collect_declarations(ctx, stmt)
        }
    
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
            semantic_define_symbol(ctx, var_def.name, type, node.span)
        }
    }
}

// Pass 2: Full checking with bodies
check_node :: proc(ctx: ^Semantic_Context, node: ^Node) -> Type_Info {
    if node == nil do return Primitive_Type.Void
    
    #partial switch node.node_kind {
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
        
        if !types_equal(left_type, right_type) {
            add_error(ctx, node.span, "Type mismatch: %v vs %v", left_type, right_type)
        }

        result_type := semantic_binop_type_resolve(left_type, right_type, binop.op, node.span, ctx)
        node.inferred_type = result_type
        return result_type

    case .Cast:
        cast_node := node.payload.(Node_Cast)
        source_type := check_node(ctx, cast_node.expr)
        target_type := cast_node.target_type
        
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

    case .Field_Access:
        field_access := node.payload.(Node_Field_Access)
        object_type := check_node(ctx, field_access.object)
        
        actual_type := object_type
        if named, is_named := object_type.(Named_Type); is_named {
            sym, ok := semantic_lookup_symbol(ctx, named.name)
            if !ok {
                add_error(ctx, node.span, "Undefined type '%s'", named.name)
                node.inferred_type = Primitive_Type.Void
                return Primitive_Type.Void
            }
            actual_type = sym.type
        }

        if ptr_type, is_ptr := actual_type.(Pointer_Type); is_ptr {
            actual_type = ptr_type.pointee^
            if named, is_named := actual_type.(Named_Type); is_named {
                sym, ok := semantic_lookup_symbol(ctx, named.name)
                if !ok {
                    add_error(ctx, node.span, "Undefined type '%s'", named.name)
                    node.inferred_type = Primitive_Type.Void
                    return Primitive_Type.Void
                }
                actual_type = sym.type
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

        // Check if symbol already exists (from pass 1)
        existing_sym, already_defined := semantic_lookup_symbol(ctx, var_def.name)
        
        if already_defined {
            // Symbol was collected in pass 1, just validate
            value_type := check_node(ctx, var_def.content)
            actual_type := existing_sym.type
            
            if !types_compatible(actual_type, value_type) {
                add_error(ctx, node.span, "Type mismatch: declared as %v but initialized with %v", 
                    actual_type, value_type)
            }
            
            node.inferred_type = actual_type
        } else {
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
        
        #partial switch t in value_type {
        case Array_Type, String_Type:
        case:
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
        
        sym, ok := semantic_lookup_symbol(ctx, struct_lit.type_name)
        if !ok {
            add_error(ctx, node.span, "Undefined type '%s'", struct_lit.type_name)
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        struct_type, is_struct := sym.type.(Struct_Type)
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
            
            if !types_compatible(value_type, field.type) {
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
        node.inferred_type = Primitive_Type.I32
        return Primitive_Type.I32

    case .Literal_String:
        node.inferred_type = String_Type{}
        return String_Type{}
    
    case .Identifier:
        iden := node.payload.(Node_Identifier)
        if sym, ok := semantic_lookup_symbol(ctx, iden.name); ok {
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
    case String_Type:
        b_val, ok := b.(String_Type)
        return ok && a_val == b_val
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
    if !types_equal(t1, t2) {
        add_error(ctx, span, "Type mismatch in binary operation")
        return Primitive_Type.I32
    }
    
    #partial switch t in t1 {
    case Primitive_Type:
        return semantic_binop_primitive(t, op, span, ctx)
    case:
        add_error(ctx, span, fmt.tprintf("Cannot use %v in binary operation", t))
        return Primitive_Type.I32
    }
    
    return Primitive_Type.I32
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
        
        sym, ok := semantic_lookup_symbol(ctx, t.name)
        if !ok do return false
        
        return check_infinite_size_impl(ctx, struct_name, sym.type, seen)
    
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