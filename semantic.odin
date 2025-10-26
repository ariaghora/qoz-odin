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
    current_function_return_type: Maybe(Type_Info)
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
    free(old, ctx.allocator)
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

    return ctx
}

semantic_free :: proc(ctx: ^Semantic_Context) {
    for ctx.current_scope != nil {
        semantic_pop_scope(ctx)
    }
    delete(ctx.errors)
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
    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        semantic_analyze_node(ctx, binop.left)
        semantic_analyze_node(ctx, binop.right)
    case .Program: 
        for stmt in node.payload.(Node_Statement_List).nodes {
            semantic_analyze_node(ctx, stmt)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        inferred_type := semantic_infer_type(ctx, var_def.content)
        semantic_define_symbol(ctx, var_def.name, inferred_type, var_def.content.span)
        semantic_analyze_node(ctx, var_def.content)  
    case .Fn_Call:
        call := node.payload.(Node_Call)
        semantic_analyze_node(ctx, call.callee)
        
        for arg in call.args {
            semantic_analyze_node(ctx, arg)
        }
    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)
        old_return_type := ctx.current_function_return_type
        ctx.current_function_return_type = fn_def.return_type
        
        semantic_push_scope(ctx)
        for param in fn_def.params {
            semantic_define_symbol(ctx, param.name, param.type, param.span)
        }

        for stmt in fn_def.body {
            semantic_analyze_node(ctx, stmt)
        }
        semantic_pop_scope(ctx)
        ctx.current_function_return_type = old_return_type
    case .Print:
        semantic_analyze_node(ctx, node.payload.(Node_Print).content)
        _ = semantic_infer_type(ctx, node.payload.(Node_Print).content)
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

    case .Un_Op:
        semantic_analyze_node(ctx, node.payload.(Node_Un_Op).operand)
    case .Literal, .Identifier:
        // nothing happened. These are leaf nodes in expressions.
    case: fmt.panicf("Cannot analyze %v yet", node.node_kind)
    }
}

semantic_infer_type :: proc(ctx: ^Semantic_Context, node: ^Node) -> Type_Info {
    if node == nil do return Primitive_Type.Void
    
    #partial switch node.node_kind {
    case .Literal:
        return Primitive_Type.I32
    
    case .Identifier:
        iden := node.payload.(Node_Identifier)
        if sym, ok := semantic_lookup_symbol(ctx, iden.name); ok {
            return sym.type
        }
        add_error(ctx, node.span, "Undefined variable '%s'", iden.name)
        return Primitive_Type.I32
    
    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        left_type := semantic_infer_type(ctx, binop.left)
        right_type := semantic_infer_type(ctx, binop.right)
        
        if !types_equal(left_type, right_type) {
            add_error(ctx, node.span, "Type mismatch: %v vs %v", left_type, right_type)
        }

        result_type := semantic_binop_type_resolve(left_type, right_type, binop.op, node.span, ctx)
        return result_type
    
    case .Un_Op:
        unop := node.payload.(Node_Un_Op)
        return semantic_infer_type(ctx, unop.operand)
    
    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)
        param_types := make([]Type_Info, len(fn_def.params), ctx.allocator)
        for param, i in fn_def.params {
            param_types[i] = param.type
        }
        return Function_Type{
            params = param_types,
            return_type = new_clone(fn_def.return_type, ctx.allocator),
        }
    
    case .Fn_Call:
        call := node.payload.(Node_Call)
        callee_type := semantic_infer_type(ctx, call.callee)
        
        fn_type, ok := callee_type.(Function_Type)
        if !ok {
            add_error(ctx, node.span, "Cannot call non-function")
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
        return fn_type.return_type^
    case:
        return Primitive_Type.Void
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
    }
    return false
}

semantic_binop_type_resolve :: proc(t1, t2: Type_Info, op: Token, span: Span, ctx: ^Semantic_Context) -> Type_Info {
    if !types_equal(t1, t2) {
        add_error(ctx, span, "Type mismatch in binary operation")
        return Primitive_Type.I32
    }
    
    switch t in t1 {
    case Primitive_Type:
        return semantic_binop_primitive(t, op, span, ctx)
    case Function_Type:
        add_error(ctx, span, "Cannot use function in binary operation")
        return Primitive_Type.I32
    }
    
    return Primitive_Type.I32
}

semantic_binop_primitive :: proc(t: Primitive_Type, op: Token, span: Span, ctx: ^Semantic_Context) -> Type_Info {
    #partial switch op.kind {
    case .Plus, .Minus, .Star, .Slash:
        // Arithmetic: numeric types only
        if t == .Void {
            add_error(ctx, span, "Cannot apply arithmetic to void")
            return Primitive_Type.I32
        }
        return t
    
    // TODO(Aria): more ops
    // case .Less, .Greater, .Less_Eq, .Greater_Eq:
    //     if t == .Void { error }
    //     return Primitive_Type.Bool
    
    case:
        add_error(ctx, span, "Operator %v not defined for type %v", op.kind, t)
        return t
    }
}