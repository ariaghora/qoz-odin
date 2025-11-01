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
    // This is to track the current struct's name while analyzing
    // recursively within it
    current_struct_name: Maybe(string), 
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
    if name in ctx.current_scope.symbols {
        add_error(ctx, span, "Symbol '%s' already defined in this scope", name)
        return false
    }
    ctx.current_scope.symbols[name] = Symbol { name=name, type=type }
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

// Semantic analysis entry point. Should be called in, e.g., main.
semantic_analyze :: proc(root: ^Node, allocator := context.allocator) -> Semantic_Context {
    ctx := Semantic_Context {
        allocator=allocator,
        errors=make([dynamic]Semantic_Error, allocator)
    }

    semantic_push_scope(&ctx) // global
    semantic_analyze_node(&ctx, root)

    // Validate main function exists
    main_span := Span{start = 0, end = 0}
    main_sym, has_main := ctx.current_scope.symbols["main"]

    // Find main's definition span
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

semantic_analyze_node :: proc(ctx: ^Semantic_Context, node: ^Node) {
    if node == nil do return 

    #partial switch node.node_kind {
    case .Program: 
        for stmt in node.payload.(Node_Statement_List).nodes {
            semantic_analyze_node(ctx, stmt)
        }
    
    case .Expr_Statement:
        semantic_analyze_node(ctx, node.payload.(Node_Expr_Statement).expr)

    case .Assignment:
        assign := node.payload.(Node_Assign)
        
        // Check variable exists
        sym, ok := semantic_lookup_symbol(ctx, assign.target)
        if !ok {
            add_error(ctx, node.span, "Undefined variable '%s'", assign.target)
            return
        }
        
        // Analyze and infer type of value
        semantic_analyze_node(ctx, assign.value)
        value_type := semantic_infer_type(ctx, assign.value)
        
        // Check types match
        if !types_equal(sym.type, value_type) {
            add_error(ctx, node.span, "Cannot assign %v to variable '%s' of type %v", 
                value_type, assign.target, sym.type)
        }
        
        node.inferred_type = sym.type
    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        semantic_analyze_node(ctx, binop.left)
        semantic_analyze_node(ctx, binop.right)

    case .Field_Access:
        field_access := node.payload.(Node_Field_Access)
        semantic_analyze_node(ctx, field_access.object)

    case .For_In:
        for_in := node.payload.(Node_For_In)
        
        // Analyze iterable expression
        semantic_analyze_node(ctx, for_in.iterable)
        iterable_type := semantic_infer_type(ctx, for_in.iterable)
        
        // Check iterable is an array
        arr_type, is_array := iterable_type.(Array_Type)
        if !is_array {
            add_error(ctx, for_in.iterable.span, "Cannot iterate over non-iterable type %v", iterable_type)
            return
        }
        
        // For loop body will have its own scope, so let's create it
        semantic_push_scope(ctx)
        // Define iterator variable with element type
        semantic_define_symbol(ctx, for_in.iterator, arr_type.element_type^, node.span)
        for stmt in for_in.body do semantic_analyze_node(ctx, stmt)
        semantic_pop_scope(ctx)

    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)

        // If defining a struct, track the name
        if var_def.content.node_kind == .Type_Expr {
            ctx.current_struct_name = var_def.name
        }

        value_type := semantic_infer_type(ctx, var_def.content)

        // Override the inferenced type if explicit type is defined
        actual_type: Type_Info
        if explicit, has_explicit := var_def.explicit_type.?; has_explicit {
            if !types_compatible(explicit, value_type) {
                add_error(ctx, node.span, "Type mismatch: declared as %v but initialized with %v", 
                    explicit, value_type)
            }
            actual_type = explicit
        } else {
            actual_type = value_type
        }

        node.inferred_type = actual_type
        semantic_define_symbol(ctx, var_def.name, actual_type, var_def.content.span)
        semantic_analyze_node(ctx, var_def.content)  

        // Reset the current struct name since we're done with the business here
        // in this struct
        if var_def.content.node_kind == .Type_Expr {
            ctx.current_struct_name = nil
        }

    
    case .Fn_Call:
        call := node.payload.(Node_Call)
        semantic_analyze_node(ctx, call.callee)
        
        for arg in call.args {
            semantic_analyze_node(ctx, arg)
        }

    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)
        if fn_def.is_external do return 

        old_return_type := ctx.current_function_return_type
        ctx.current_function_return_type = fn_def.return_type
        
        semantic_push_scope(ctx)
        {
            for param in fn_def.params {
                semantic_define_symbol(ctx, param.name, param.type, param.span)
            }

            for stmt in fn_def.body {
                semantic_analyze_node(ctx, stmt)
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

    case .If:
        if_stmt := node.payload.(Node_If)
        semantic_analyze_node(ctx, if_stmt.condition)
        
        // Primary condition must be of boolean type
        cond_type := semantic_infer_type(ctx, if_stmt.condition)
        if cond_prim, ok := cond_type.(Primitive_Type); !ok || cond_prim != .Bool {
            add_error(ctx, if_stmt.condition.span, "If condition must be bool")
        }
        
        for stmt in if_stmt.if_body {
            semantic_analyze_node(ctx, stmt)
        }
        
        for stmt in if_stmt.else_body {
            semantic_analyze_node(ctx, stmt)
        }

    case .Literal_Arr:
        arr_lit := node.payload.(Node_Array_Literal)
        
        // Analyze all elements
        for elem in arr_lit.elements {
            semantic_analyze_node(ctx, elem)
        }
        
        if len(arr_lit.elements) != arr_lit.size {
            add_error(ctx, node.span, "Array literal has %d elements but size is %d", 
                len(arr_lit.elements), arr_lit.size)
        }
        
        for elem, i in arr_lit.elements {
            elem_type := semantic_infer_type(ctx, elem)
            if !types_equal(elem_type, arr_lit.element_type) {
                add_error(ctx, elem.span, "Array element %d: expected %v, got %v", 
                    i, arr_lit.element_type, elem_type)
            }
        }
        
        // Infer array type
        array_type := Array_Type{
            element_type = new_clone(arr_lit.element_type, ctx.allocator),
            size = arr_lit.size,
        }
        node.inferred_type = array_type

    case .Print:
        semantic_analyze_node(ctx, node.payload.(Node_Print).content)
        infered_type := semantic_infer_type(ctx, node.payload.(Node_Print).content)

    case .Return:
        // TODO(Aria): analyze multi-codepath return type
        ret := node.payload.(Node_Return)
        expected_ret_type, in_function := ctx.current_function_return_type.?
        if !in_function {
            add_error(ctx, node.span, "Return statement outside function")
            return
        }

        if ret.value != nil { // Has return val. It is either Type_Info or Void
            semantic_analyze_node(ctx, ret.value)
            ret_type := semantic_infer_type(ctx, ret.value)
            if !types_equal(ret_type, ctx.current_function_return_type.?) {
                add_error(ctx, node.span, "Return type mismatch: expected %v, got %v", expected_ret_type, ret_type)
            }
        } else { // If expected type is NOT Primitive_Type.Void, error.
            exp_prim_type, is_prim := expected_ret_type.(Primitive_Type)
            if !is_prim || exp_prim_type != .Void {
                add_error(ctx, node.span, "Function expects return value")
            }
        }
    
    case .Size_Of:
        sizeof_op := node.payload.(Node_Size_Of)
        // Validate the type is valid
        _ = semantic_infer_type(ctx, sizeof_op.type)
        
        // sizeof always returns i64 (or i32 if you prefer)
        result := Primitive_Type.I64
        node.inferred_type = result
        return 
    
    case .Struct_Literal:
        struct_lit := node.payload.(Node_Struct_Literal)
        
        // Lookup the struct type by name
        sym, ok := semantic_lookup_symbol(ctx, struct_lit.type_name)
        if !ok {
            add_error(ctx, node.span, "Undefined type '%s'", struct_lit.type_name)
            node.inferred_type = Primitive_Type.Void
            return
        }
        
        struct_type, is_struct := sym.type.(Struct_Type)
        if !is_struct {
            add_error(ctx, node.span, "'%s' is not a struct type", struct_lit.type_name)
            return
        }
        
        // Analyze all field values
        for field_init in struct_lit.field_inits {
            semantic_analyze_node(ctx, field_init.value)
        }
        
        // Validate all fields are present and types match
        field_map := make(map[string]^Node, context.temp_allocator)
        for field_init in struct_lit.field_inits {
            field_map[field_init.name] = field_init.value
        }
        
        for field in struct_type.fields {
            value_node, has_field := field_map[field.name]
            if !has_field {
                add_error(ctx, node.span, "Missing field '%s' in struct literal", field.name)
                continue
            }
            
            value_type := semantic_infer_type(ctx, value_node)
            if !types_compatible(value_type, field.type) {
                add_error(ctx, value_node.span, "Field '%s': expected %v, got %v", 
                    field.name, field.type, value_type)
            }
        }
    case .Type_Expr:
        type_expr := node.payload.(Node_Type_Expr)

        // Validate struct fields if it's a struct type
        if struct_type, is_struct := type_expr.type_info.(Struct_Type); is_struct {
            field_names := make(map[string]bool, context.temp_allocator)
            for field in struct_type.fields {
                _, field_exists := field_names[field.name]
                if field_exists {
                    add_error(ctx, node.span, "Duplicate field name '%s' in struct", field.name)
                }
                field_names[field.name] = true
            }

            // Check for infinite size (direct recursion without pointer)
            // We need the struct name, that we can get from parent, i.e., Var_Def
            if struct_name, has_name := ctx.current_struct_name.?; has_name {
                for field in struct_type.fields {
                    if check_infinite_size(ctx, struct_name, field.type, node.span) {
                        add_error(ctx, node.span, "Struct '%s' has infinite size (field '%s' creates direct recursion). You may want to use indirection for this field's type, such as *%s.", 
                            struct_name, field.name, struct_name)
                    }
                }
            }
        }

    case .Un_Op:
        semantic_analyze_node(ctx, node.payload.(Node_Un_Op).operand)
    
    case .Literal_Number, .Literal_String, .Identifier:
        // nothing happened. These are leaf nodes in expressions.
    case: fmt.panicf("Cannot analyze %v yet", node.node_kind)
    }
}

semantic_infer_type :: proc(ctx: ^Semantic_Context, node: ^Node) -> Type_Info {
    if node == nil do return Primitive_Type.Void
    
    #partial switch node.node_kind {
    case .Literal_Arr:
        arr_lit := node.payload.(Node_Array_Literal)
        return Array_Type{
            element_type = new_clone(arr_lit.element_type, ctx.allocator),
            size = arr_lit.size,
        }
    case .Literal_Number:
        type := Primitive_Type.I32
        node.inferred_type = type
        return type

    case .Literal_String:
        type := String_Type{}
        node.inferred_type = type
        return type
    
    case .Identifier:
        iden := node.payload.(Node_Identifier)
        if sym, ok := semantic_lookup_symbol(ctx, iden.name); ok {
            type := sym.type
            node.inferred_type = type
            return type
        }
        add_error(ctx, node.span, "Undefined variable '%s'", iden.name)
        type := Primitive_Type.I32
        node.inferred_type = type
        return type
    
    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        left_type := semantic_infer_type(ctx, binop.left)
        right_type := semantic_infer_type(ctx, binop.right)
        
        if !types_equal(left_type, right_type) {
            add_error(ctx, node.span, "Type mismatch: %v vs %v", left_type, right_type)
        }

        result_type := semantic_binop_type_resolve(left_type, right_type, binop.op, node.span, ctx)
        node.inferred_type = result_type
        return result_type
    
    case .Field_Access:
        field_access := node.payload.(Node_Field_Access)
        object_type := semantic_infer_type(ctx, field_access.object)
        
        // Most of cases, the accessed object is a Named_Type (user-defined struct).
        // In this case, we need to resolve it to actual type.
        actual_type := object_type
        if named, is_named := object_type.(Named_Type); is_named {
            sym, ok := semantic_lookup_symbol(ctx, named.name)
            if !ok {
                add_error(ctx, node.span, "Undefined type '%s'", named.name)
                result := Primitive_Type.Void
                node.inferred_type = result
                return result
            }
            actual_type = sym.type
        }

        // Unwrap pointer if accessing through pointer
        if ptr_type, is_ptr := actual_type.(Pointer_Type); is_ptr {
            actual_type = ptr_type.pointee^
            // Resolve Named_Type again if the pointee is named
            if named, is_named := actual_type.(Named_Type); is_named {
                sym, ok := semantic_lookup_symbol(ctx, named.name)
                if !ok {
                    add_error(ctx, node.span, "Undefined type '%s'", named.name)
                    result := Primitive_Type.Void
                    node.inferred_type = result
                    return result
                }
                actual_type = sym.type
            }
        }
        
        // Gotta ensure that that named type is a struct. Cannot access field
        // from anything other than struct.
        struct_type, is_struct := actual_type.(Struct_Type)
        if !is_struct {
            add_error(ctx, node.span, "Cannot access field of non-struct type")
            result := Primitive_Type.Void
            node.inferred_type = result
            return result
        }
        
        // Find the field
        // NOTE(Aria): be patient with linear search, we're preserving the order
        // of the fields.
        for field in struct_type.fields {
            if field.name == field_access.field_name {
                node.inferred_type = field.type
                return field.type
            }
        }
        
        add_error(ctx, node.span, "Struct has no field '%s'", field_access.field_name)
        result := Primitive_Type.Void
        node.inferred_type = result
        return result

    case .Un_Op:
        unop := node.payload.(Node_Un_Op)
        operand_type := semantic_infer_type(ctx, unop.operand)
        
        #partial switch unop.op.kind {
        case .Plus, .Minus:
            prim, ok := operand_type.(Primitive_Type)
            if prim == .Void {
                add_error(ctx, node.span, "Cannot apply unary operator to void")
                type := Primitive_Type.I32
                node.inferred_type = type
                return type
            }
            type := prim
            node.inferred_type = type
            return prim
        case .Amp:
            // Address-of: &x
            if unop.operand.node_kind != .Identifier {
                add_error(ctx, node.span, "Address-of requires an lvalue")
            }
            result := Pointer_Type{
                pointee = new_clone(operand_type, ctx.allocator),
            }
            node.inferred_type = result
            return result
        case .Star:
            // Dereference: *ptr
            ptr_type, ok := operand_type.(Pointer_Type)
            if !ok {
                add_error(ctx, node.span, "Cannot dereference non-pointer type %v", operand_type)
                result := Primitive_Type.I32
                node.inferred_type = result
                return result
            }
            result := ptr_type.pointee^
            node.inferred_type = result
            return result
        case:
            add_error(ctx, node.span, "Unary operator %v not defined", unop.op.kind)
            node.inferred_type = operand_type
            return operand_type
        }
    
    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)
        param_types := make([]Type_Info, len(fn_def.params), ctx.allocator)
        for param, i in fn_def.params {
            param_types[i] = param.type
        }
        type := Function_Type{
            params = param_types,
            return_type = new_clone(fn_def.return_type, ctx.allocator),
        }
        node.inferred_type = type
        return type
    
    case .Fn_Call:
        call := node.payload.(Node_Call)
        callee_type := semantic_infer_type(ctx, call.callee)
        
        fn_type, ok := callee_type.(Function_Type)
        if !ok {
            add_error(ctx, node.span, "Cannot call non-function")
            node.inferred_type = Primitive_Type.Void
            return Primitive_Type.Void
        }
        
        if len(call.args) != len(fn_type.params) {
            add_error(ctx, node.span, "Expected %d arguments, got %d", len(fn_type.params), len(call.args))
        }
        
        for arg, i in call.args {
            if i >= len(fn_type.params) do break
            arg_type := semantic_infer_type(ctx, arg)
            expected := fn_type.params[i]
            
            arg_prim, arg_ok := arg_type.(Primitive_Type)
            exp_prim, exp_ok := expected.(Primitive_Type)
            
            if arg_ok && exp_ok && arg_prim != exp_prim {
                add_error(ctx, arg.span, "Argument %d: expected %v, got %v", i, exp_prim, arg_prim)
            }
        }
        type := fn_type.return_type^
        node.inferred_type = type
        return type

    case .Size_Of:
        sizeof_op := node.payload.(Node_Size_Of)
        // Validate the type is valid
        _ = semantic_infer_type(ctx, sizeof_op.type)
        
        // sizeof always returns i64 (or i32 if you prefer)
        result := Primitive_Type.I64
        node.inferred_type = result
        return result

    case .Type_Expr:  
        type_expr := node.payload.(Node_Type_Expr)
        node.inferred_type = type_expr.type_info
        return type_expr.type_info

    case .Struct_Literal:
        struct_lit := node.payload.(Node_Struct_Literal)
        
        sym, ok := semantic_lookup_symbol(ctx, struct_lit.type_name)
        if !ok {
            add_error(ctx, node.span, "Undefined type '%s'", struct_lit.type_name)
            result := Primitive_Type.Void
            node.inferred_type = result
            return result
        }

         // Validate it's a struct (using resolved type)
        _, is_struct := sym.type.(Struct_Type)
        if !is_struct {
            add_error(ctx, node.span, "'%s' is not a struct type", struct_lit.type_name)
        }
        
        result := Named_Type{name = struct_lit.type_name}
        node.inferred_type = result
        return result
    case:
        type := Primitive_Type.Void
        node.inferred_type = type
        return type
    }
}

types_equal :: proc(a, b: Type_Info) -> bool {
    #partial switch a_val in a {
    case Primitive_Type:
        b_val, ok := b.(Primitive_Type)
        return ok && a_val == b_val
    case Function_Type:
        fmt.println("TODO(Aria): types are uncomparable")
        return false
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

// In general, we are striving for strict type checking. Type compatibility is 
// limited for *void type as our escape hatch into unsafety.
types_compatible :: proc(target: Type_Info, source: Type_Info) -> bool {
    if types_equal(target, source) do return true
    
    // void* is compatible with any pointer
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

    // TODO(Aria): check other type compatibility
    // ...
    
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
        return t  // Numeric type stays numeric
    
    case .Lt, .Gt, .Lt_Eq, .Gt_Eq:
        if t == .Void || t == .Bool {
            add_error(ctx, span, "Cannot compare %v", t)
            return Primitive_Type.Bool
        }
        return Primitive_Type.Bool  // Comparison returns bool
    
    case .Percent:
        if t == .I32 || t == .I64 do return t
        return .I32

    case .Eq_Eq, .Not_Eq:
        if t == .Void {
            add_error(ctx, span, "Cannot compare void")
            return Primitive_Type.Bool
        }
        return Primitive_Type.Bool  // Equality returns bool
    
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
        // Must have else clause
        if len(if_node.else_body) == 0 do return false
        // Both branches must return
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
        // Direct reference to self without pointer
        if t.name == struct_name {
            return true
        }
        
        // Avoid infinite loop in mutual recursion
        if t.name in seen {
            return false
        }
        seen[t.name] = true
        
        // Resolve and check recursively
        sym, ok := semantic_lookup_symbol(ctx, t.name)
        if !ok do return false
        
        return check_infinite_size_impl(ctx, struct_name, sym.type, seen)
    
    case Pointer_Type:
        // Pointer breaks recursion - fixed size
        return false
    
    case Array_Type:
        // Check element type
        return check_infinite_size_impl(ctx, struct_name, t.element_type^, seen)
    
    case Struct_Type:
        // Check all fields
        for field in t.fields {
            if check_infinite_size_impl(ctx, struct_name, field.type, seen) {
                return true
            }
        }
        return false
    }
    
    return false
}