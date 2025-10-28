package main

import "core:fmt"

Parse_Error :: union { string }

Node_Kind :: enum {
    Bin_Op,
    Fn_Call,
    Fn_Def,
    Identifier,
    If,
    Literal,
    Print,
    Program,
    Return,
    Statement_List,
    Un_Op,
    Var_Def,
}

Type_Info :: union { Primitive_Type, Function_Type }

Primitive_Type :: enum { I32, I64, F32, F64, Bool, Void }

Function_Type :: struct {
    params: []Type_Info,
    return_type: ^Type_Info,  
}

Span :: struct {
    start: int,
    end: int,
}

Parsing_State :: struct {
    tokens: [dynamic]Token,
    current_token: Token,
    idx: int,
}

Fn_Param :: struct { name: string, type: Type_Info, span: Span }

Node_Bin_Op         :: struct { left, right: ^Node, op: Token }
Node_Call           :: struct { callee: ^Node, args: [dynamic]^Node }
Node_Fn_Def         :: struct { params: [dynamic]Fn_Param, body: [dynamic]^Node, return_type: Type_Info }
Node_Identifier     :: struct { name:string }
Node_If             :: struct { condition: ^Node, if_body: [dynamic]^Node, else_body: [dynamic]^Node }
Node_Literal        :: struct { content: Token }
Node_Print          :: struct { content: ^Node }
Node_Return         :: struct { value: ^Node } 
Node_Statement_List :: struct { nodes: [dynamic]^Node }
Node_Un_Op          :: struct { operand: ^Node, op: Token }
Node_Var_Def        :: struct { name: string, content: ^Node }

Node :: struct {
    node_kind: Node_Kind,
    parent: ^Node,
    span: Span,
    inferred_type: Maybe(Type_Info),
    payload: union {
        Node_Bin_Op,
        Node_Call,
        Node_Fn_Def,
        Node_Identifier,
        Node_If,
        Node_Literal,
        Node_Print,
        Node_Return,
        Node_Statement_List,
        Node_Var_Def,
        Node_Un_Op,
    }
}

current_span :: proc(ps: ^Parsing_State) -> Span {
    return Span{start = ps.idx, end = ps.idx}
}

@(require_results)
parser_expect :: proc(ps: ^Parsing_State, expected: Token_Kind) -> Parse_Error {
    if ps.current_token.kind != expected {
        return fmt.tprintf(
            "[%d:%d] Expected %v, got %v", 
            ps.current_token.line,
            ps.current_token.column,
            expected,
            ps.current_token.kind)
    }
    return nil
}

parser_advance :: proc(ps: ^Parsing_State) {
    ps.idx += 1
    if ps.idx < len(ps.tokens) {
        ps.current_token = ps.tokens[ps.idx]
    }
}

parser_consume :: proc(ps: ^Parsing_State, expected: Token_Kind) -> Parse_Error {
    parser_expect(ps, expected) or_return
    parser_advance(ps)
    return nil
}

parse :: proc(tokens: [dynamic]Token, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    ps := Parsing_State {
        tokens = tokens,
        current_token = tokens[0],
        idx = 0,
    }

    program_node := new(Node, allocator)
    program_node.node_kind = .Program
    program_node.parent = nil
    program_node.span = current_span(&ps)

    stmts := make([dynamic]^Node, allocator)
    for ps.current_token.kind != .EOF {
        statement := parse_statement(&ps, program_node, allocator) or_return
        append(&stmts, statement)
    }

    program_node.payload = Node_Statement_List{nodes = stmts}
    return program_node, nil
}

parse_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    #partial switch ps.current_token.kind {
    case .Iden: return parse_var_def(ps, parent, allocator)
    case .KW_If: return parse_if_statement(ps, parent, allocator)
    case .KW_Print: return parse_print_statement(ps, parent, allocator)
    case .KW_Return: return parse_return_statement(ps, parent, allocator)
    case:
        return nil, fmt.tprintf("Cannot parse statement starting with %v at position %d", ps.current_token.kind, ps.idx)
    }
}

parse_var_def :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    name_tok := ps.current_token
    
    parser_advance(ps)
    parser_consume(ps, .Assign) or_return
    
    var_node := new(Node, allocator)
    var_node.node_kind = .Var_Def
    var_node.parent = parent
    var_node.span = Span{start = span_start, end = ps.idx}
    
    expr := parse_expression(ps, var_node, allocator) or_return
    var_node.payload = Node_Var_Def{name = name_tok.source, content = expr}
    
    return var_node, nil
}

parse_if_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    
    parser_advance(ps) // eat 'if'
    
    cond := parse_expression(ps, parent, allocator) or_return
    
    parser_consume(ps, .Left_Brace) or_return
    
    // Parse if body
    if_body := make([dynamic]^Node, allocator)
    for ps.current_token.kind != .Right_Brace {
        if ps.current_token.kind == .EOF {
            return nil, "Unexpected EOF in if statement"
        }
        stmt := parse_statement(ps, parent, allocator) or_return
        append(&if_body, stmt)
    }
    
    parser_consume(ps, .Right_Brace) or_return
    
    else_body: [dynamic]^Node
    if ps.current_token.kind == .KW_Else {
        parser_advance(ps)

        // Check for else if
        if ps.current_token.kind == .KW_If {
            else_if_stmt := parse_if_statement(ps, parent, allocator) or_return
            else_body = make([dynamic]^Node, allocator)
            append(&else_body, else_if_stmt)
        } else {
            // Regular else block
            parser_consume(ps, .Left_Brace) or_return
            
            else_body = make([dynamic]^Node, allocator)
            for ps.current_token.kind != .Right_Brace {
                if ps.current_token.kind == .EOF {
                    return nil, "Unexpected EOF in else statement"
                }
                stmt := parse_statement(ps, parent, allocator) or_return
                append(&else_body, stmt)
            }
            
            parser_consume(ps, .Right_Brace) or_return
        }
    }
    
    if_node := new(Node, allocator)
    if_node.node_kind = .If
    if_node.parent = parent
    if_node.span = Span{start = span_start, end = ps.idx - 1}
    if_node.payload = Node_If{
        condition = cond,
        if_body = if_body,
        else_body = else_body,
    }
    
    return if_node, nil
}

parse_print_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    
    parser_advance(ps)
    parser_consume(ps, .Left_Paren) or_return
    
    print_node := new(Node, allocator)
    print_node.node_kind = .Print
    print_node.parent = parent
    print_node.span = Span{start = span_start, end = ps.idx}
    
    content := parse_expression(ps, print_node, allocator) or_return
    print_node.payload = Node_Print{content = content}
    
    parser_consume(ps, .Right_Paren) or_return
    
    return print_node, nil
}

parse_expression :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    return parse_logical(ps, parent, allocator)
}

parse_logical :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    left := parse_equality(ps, parent, allocator) or_return
    return left, nil
}

parse_equality :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    left := parse_comparison(ps, parent, allocator) or_return

    for ps.current_token.kind == .Eq_Eq || ps.current_token.kind == .Not_Eq {
        op_idx := ps.idx
        op := ps.current_token
        parser_advance(ps)
        right := parse_comparison(ps, parent, allocator) or_return
        
        left = new_clone(Node {
            node_kind = .Bin_Op,
            parent = parent,
            span = Span{start = left.span.start, end = right.span.end},
            payload = Node_Bin_Op{left = left, right = right, op = op}
        }, allocator)
    }

    return left, nil
}

parse_comparison :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    left := parse_term(ps, parent, allocator) or_return
    return left, nil
}

parse_term :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    left := parse_factor(ps, parent, allocator) or_return
    for ps.current_token.kind == .Plus || ps.current_token.kind == .Minus {
        op := ps.current_token
        parser_advance(ps)
        right := parse_factor(ps, parent, allocator) or_return
        left = new_clone(Node {
            node_kind=.Bin_Op,
            parent=parent,
            span=Span{start=left.span.start, end=right.span.end},
            payload=Node_Bin_Op {
                left=left, right=right, op=op
            }
        }, allocator)
    }
    return left, nil
}

parse_factor :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    left := parse_unary(ps, parent, allocator) or_return
    for ps.current_token.kind == .Star || ps.current_token.kind == .Slash {
        op := ps.current_token
        parser_advance(ps)
        right := parse_unary(ps, parent, allocator) or_return
        left = new_clone(Node {
            node_kind=.Bin_Op,
            parent=parent,
            span=Span{start=left.span.start, end=right.span.end},
            payload=Node_Bin_Op {
                left=left, right=right, op=op
            }
        }, allocator)
    }
    return left, nil
}

parse_unary :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    if ps.current_token.kind == .Minus || ps.current_token.kind == .Plus {
        op_idx := ps.idx
        op := ps.current_token
        parser_advance(ps)
        
        operand := parse_unary(ps, parent, allocator) or_return
        
        unop_node := new(Node, allocator)
        unop_node.node_kind = .Un_Op
        unop_node.parent = parent
        unop_node.span = Span{start = op_idx, end = operand.span.end}
        unop_node.payload = Node_Un_Op{op = op, operand = operand}
        
        operand.parent = unop_node
        
        return unop_node, nil
    }

    return parse_fn_call(ps, parent, allocator)
}

parse_fn_call :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    expr := parse_primary(ps, parent, allocator) or_return

    // "do while `(` means function call chaining, e.g., f()()()
    // It is possible for a function to return another callable function
    for ps.current_token.kind == .Left_Paren {
        parser_advance(ps)
        fn_args := make([dynamic]^Node, allocator)
        for ps.current_token.kind != .Right_Paren {
            arg := parse_expression(ps, expr, allocator) or_return
            append(&fn_args, arg)
            if ps.current_token.kind == .Comma {
                parser_advance(ps)
            } else {
                break
            }
        }
        parser_consume(ps, .Right_Paren) or_return

        expr = new_clone(Node {
            node_kind=.Fn_Call,
            parent=parent,
            span=Span{start=expr.span.start, end=ps.idx-1},
            payload=Node_Call {
                callee=expr,
                args=fn_args,
            }
        }, allocator)
    }

    return expr, nil
}

parse_primary :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    #partial switch ps.current_token.kind {
    case .Iden: return parse_identifier(ps, parent, allocator)
    case .KW_Fn: return parse_fn_def(ps, parent, allocator)
    case .Lit_Number: return parse_literal(ps, parent, allocator)
    case:
        fmt.println(ps.current_token)
        return nil, fmt.tprintf("Expected expression at position %d, got %v", ps.idx, ps.current_token.kind)
    }
}

parse_identifier :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    name := ps.current_token.source
    
    iden_node := new(Node, allocator)
    iden_node.node_kind = .Identifier
    iden_node.parent = parent
    iden_node.payload = Node_Identifier{name = name}
    
    parser_advance(ps)
    iden_node.span = Span{start = span_start, end = ps.idx - 1}
    
    return iden_node, nil
}

parse_literal :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    tok := ps.current_token
    
    lit_node := new(Node, allocator)
    lit_node.node_kind = .Literal
    lit_node.parent = parent
    lit_node.payload = Node_Literal{content = tok}
    
    parser_advance(ps)
    lit_node.span = Span{start = span_start, end = ps.idx - 1}
    
    return lit_node, nil
}

parse_fn_def :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    
    parser_advance(ps)
    parser_consume(ps, .Left_Paren) or_return

    // Here we parse function parameters (or lack there of)
    param_list := make([dynamic]Fn_Param, allocator)
    if ps.current_token.kind == .Right_Paren {
        // Right paren
        parser_consume(ps, .Right_Paren) or_return
    } else if ps.current_token.kind == .Iden {
        // Or param list
        for ps.current_token.kind == .Iden {
            param_span_start := ps.idx
            param_name :=ps.current_token.source
            parser_advance(ps) 
            parser_consume(ps, .Colon) or_return

            param_type := parse_type(ps) or_return
            append(&param_list, Fn_Param{
                name = param_name, 
                type = param_type,
                span = Span{start = param_span_start, end = ps.idx - 1},
            })

            if ps.current_token.kind == .Comma {
                parser_advance(ps)
            } else {
                break
            }
        }

        parser_consume(ps, .Right_Paren) or_return
    } else {
        return nil, fmt.tprintfln("Expected ')' or identifier for parameter(s), found %v", ps.current_token.source)
    }

    // Return type
    return_type: Type_Info = .Void
    if ps.current_token.kind == .Colon {
        parser_advance(ps)
        return_type = parse_type(ps) or_return

    }

    // beginning of fn body
    parser_consume(ps, .Left_Brace) or_return
    
    fn_node := new(Node, allocator)
    fn_node.node_kind = .Fn_Def
    fn_node.parent = parent
    fn_node.span = Span{start = span_start, end = ps.idx}
    
    stmts := make([dynamic]^Node, allocator)
    for ps.current_token.kind != .Right_Brace {
        if ps.current_token.kind == .EOF {
            return nil, fmt.tprintf("Unexpected EOF in function body starting at position %d", span_start)
        }
        stmt := parse_statement(ps, fn_node, allocator) or_return
        append(&stmts, stmt)
    }
    
    fn_node.payload = Node_Fn_Def{params=param_list, body = stmts, return_type = return_type}
    parser_consume(ps, .Right_Brace) or_return
    fn_node.span.end = ps.idx - 1  
    
    return fn_node, nil
}

parse_return_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    parser_advance(ps)
    
    ret_node := new(Node, allocator)
    ret_node.node_kind = .Return
    ret_node.parent = parent
    ret_node.span = Span{start = span_start, end = ps.idx}
    
    if ps.current_token.kind == .Right_Brace || ps.current_token.kind == .EOF {
        ret_node.payload = Node_Return{value = nil}
        return ret_node, nil
    }
    
    expr := parse_expression(ps, ret_node, allocator) or_return
    ret_node.payload = Node_Return{value = expr}
    
    return ret_node, nil
}

parse_type :: proc(ps: ^Parsing_State) -> (Type_Info, Parse_Error) {
    prim: Type_Info
    #partial switch ps.current_token.kind {
    case .KW_I32: prim = .I32
    case .KW_I64: prim = .I64
    case .KW_F32: prim = .F32
    case .KW_F64: prim = .F64
    case: 
        return nil, fmt.tprintf("Expected type at position %d, got %v", ps.idx, ps.current_token.kind)
    }
    // TODO(Aria): function type and other types parsing

    parser_advance(ps)

    return prim, nil
}