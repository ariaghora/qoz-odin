package main

import "core:os"
import "core:path/filepath"
import "core:strings"
import "core:mem"
import "core:strconv"
import "core:fmt"

Parse_Error :: union { string }

Node_Kind :: enum {
    Assignment,
    Bin_Op,
    Cast,
    Del,
    Expr_Statement,
    Field_Access,
    Fn_Call,
    Fn_Def,
    For_In,
    For_C,
    Identifier,
    If,
    Import,
    Index,
    Len,
    Literal_Arr,
    Literal_Nil,
    Literal_Number,
    Literal_String,
    Print,
    Program,
    Return,
    Size_Of,
    Statement_List,
    Struct_Literal,
    Type_Expr,
    Un_Op,
    Var_Def,
}

Type_Info :: union {
    Primitive_Type,
    Array_Type,
    Function_Type,
    Pointer_Type,
    Struct_Type,
    Named_Type,
    Untyped_Int,
    Untyped_Float,
}

Primitive_Type :: enum { I8, U8, I32, I64, F32, F64, Bool, Void }

Function_Type :: struct {
    params: []Type_Info,
    return_type: ^Type_Info,  
}

Array_Type :: struct {
    element_type: ^Type_Info,
    size: int,
}

Pointer_Type :: struct {
    pointee: ^Type_Info,
}

Struct_Field :: struct { name: string, type: Type_Info }
Struct_Type :: struct {
    fields: [dynamic]Struct_Field,
}

Named_Type :: struct {
    name: string,
}

Untyped_Int :: struct {}
Untyped_Float :: struct {}

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
Struct_Field_Init :: struct { name: string, value: ^Node }

Node_Array_Literal  :: struct { element_type: Type_Info, size: int, elements: [dynamic]^Node }
Node_Assign         :: struct { target: ^Node, value: ^Node }
Node_Bin_Op         :: struct { left, right: ^Node, op: Token }
Node_Call           :: struct { callee: ^Node, args: [dynamic]^Node }
Node_Cast           :: struct { expr: ^Node, target_type: Type_Info }
Node_Del            :: struct { pointer: ^Node, allocator: ^Node }
Node_Field_Access   :: struct { object: ^Node, field_name: string }
Node_Fn_Def         :: struct { params: [dynamic]Fn_Param, body: [dynamic]^Node, return_type: Type_Info, is_external: bool}
Node_For_C          :: struct { init: ^Node, condition: ^Node, post: ^Node, body: [dynamic]^Node }
Node_For_In         :: struct { iterator: string, iterable: ^Node, body: [dynamic]^Node }
Node_Identifier     :: struct { name:string }
Node_If             :: struct { condition: ^Node, if_body: [dynamic]^Node, else_body: [dynamic]^Node }
Node_Import         :: struct { path: string, alias: Maybe(string) }
Node_Index          :: struct { object, index: ^Node }
Node_Len            :: struct { value: ^Node }
Node_Literal_Nil    :: struct {}
Node_Literal_Number :: struct { content: Token }
Node_Literal_String :: struct { content: Token }
Node_Print          :: struct { content: ^Node }
Node_Return         :: struct { value: ^Node } 
Node_Statement_List :: struct { nodes: [dynamic]^Node }
Node_Un_Op          :: struct { operand: ^Node, op: Token }
Node_Var_Def        :: struct { name: string, content: ^Node, explicit_type: Maybe(Type_Info) }
Node_Expr_Statement :: struct { expr: ^Node }
Node_Type_Expr      :: struct { type_info: Type_Info, }
Node_Size_Of        :: struct { type: ^Node }
Node_Struct_Literal :: struct { type_name: string, field_inits: [dynamic]Struct_Field_Init }


Node :: struct {
    node_kind: Node_Kind,
    parent: ^Node,
    span: Span,
    inferred_type: Maybe(Type_Info),
    payload: union {
        Node_Array_Literal,
        Node_Assign,
        Node_Bin_Op,
        Node_Call,
        Node_Cast,
        Node_Expr_Statement,
        Node_Field_Access,
        Node_Fn_Def,
        Node_For_C,
        Node_For_In,
        Node_Del,
        Node_Identifier,
        Node_Import,
        Node_Index,
        Node_If,
        Node_Len,
        Node_Literal_Nil,
        Node_Literal_Number,
        Node_Literal_String,
        Node_Print,
        Node_Return,
        Node_Size_Of,
        Node_Statement_List,
        Node_Struct_Literal,
        Node_Type_Expr,
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

resolve_import_path :: proc(current_file_dir: string, project_root: string, import_path: string) -> string {
    if strings.has_prefix(import_path, "std:") {
        qoz_root := os.get_env("QOZ_ROOT", context.temp_allocator)
        if qoz_root == "" {
            panic("QOZ_ROOT environment variable not set")
        }
        
        // e.g., std:strings → $QOZ_ROOT/std/strings
        rel_path := import_path[4:]  // Remove "std:"
        return filepath.join({qoz_root, "std", rel_path}, context.temp_allocator)
    }
    
    // Check for relative import
    if strings.has_prefix(import_path, "./") || strings.has_prefix(import_path, "../") {
        // Relative to current file's directory
        return filepath.clean(filepath.join({current_file_dir, import_path}, context.temp_allocator), context.temp_allocator)
    }
    
    // Default: relative to project root
    return filepath.clean(filepath.join({project_root, import_path}, context.temp_allocator))
}

parse_project :: proc(entry_package_dir: string, arena_lexer, arena_parser: mem.Allocator) -> (asts: map[string]^Node, err: Parse_Error) {
    asts = make(map[string]^Node, arena_parser)
    visited_packages := make(map[string]bool, arena_parser)
    packages_to_parse := make([dynamic]string, arena_parser)
    
    append(&packages_to_parse, entry_package_dir)
    
    for len(packages_to_parse) > 0 {
        pkg_dir := pop(&packages_to_parse)
        if pkg_dir in visited_packages do continue
        visited_packages[pkg_dir] = true
        
        // Find all .qoz files in this directory
        qoz_files := find_qoz_files_in_dir(pkg_dir, arena_parser) or_return
        
        // Parse all files in the package
        for file_path in qoz_files {
            source, read_ok := os.read_entire_file(file_path, arena_parser)
            if !read_ok {
                return nil, fmt.tprintf("Failed to read file: %s", file_path)
            }
            
            tokens, err_tokenize := tokenize(string(source), arena_lexer)
            if err_tokenize != nil do return nil, err_tokenize.(string)

            ast := parse(tokens, arena_parser) or_return
            asts[file_path] = ast
            
            // Extract imports and add packages to queue
            if ast.node_kind == .Program {
                for stmt in ast.payload.(Node_Statement_List).nodes {
                    if stmt.node_kind == .Import {
                        import_node := stmt.payload.(Node_Import)
                        // Resolve import path relative to pkg_dir
                        current_file_dir := filepath.dir(file_path, context.temp_allocator)
                        imported_pkg := resolve_import_path(current_file_dir, entry_package_dir, import_node.path)
                        append(&packages_to_parse, imported_pkg)
                    }
                }
            }
        }
    }
    
    return asts, nil
}

find_qoz_files_in_dir :: proc(dir: string, allocator := context.allocator) -> (files: []string, err: Parse_Error) {
    handle, open_err := os.open(dir)
    if open_err != 0 {
        return nil, fmt.tprintf("Failed to open directory: %s", dir)
    }
    defer os.close(handle)
    
    file_infos, read_err := os.read_dir(handle, -1, allocator)
    if read_err != 0 {
        return nil, fmt.tprintf("Failed to read directory: %s", dir)
    }
    
    result := make([dynamic]string, allocator)
    for info in file_infos {
        if !info.is_dir && strings.has_suffix(info.name, ".qoz") {
            full_path := filepath.join({dir, info.name}, allocator)
            append(&result, full_path)
        }
    }
    
    return result[:], nil
}

resolve_package_path :: proc(current_pkg_dir: string, import_path: string) -> string {
    return filepath.clean(filepath.join({current_pkg_dir, import_path}, context.temp_allocator), context.temp_allocator)
}

parse :: proc(tokens: [dynamic]Token, allocator := context.allocator, loc:=#caller_location) -> (res: ^Node, err: Parse_Error) {
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
    case .KW_Import:
        span_start := ps.idx
        parser_advance(ps) // eat 'import'
        
        alias: Maybe(string)
        if ps.current_token.kind == .Iden {
            alias = ps.current_token.source
            parser_advance(ps)
        }
        
        if ps.current_token.kind != .Lit_String {
            return nil, fmt.tprintf("Expected import path (string), got %v", ps.current_token.kind)
        }
        
        path := strings.trim(ps.current_token.source, "\"")
        parser_advance(ps)
        
        return new_clone(Node{
            node_kind = .Import,
            parent = parent,
            span = Span{start = span_start, end = ps.idx - 1},
            payload = Node_Import{
                path = path,
                alias = alias,
            },
        }, allocator), nil

    case .Iden: 
        next_tok := ps.tokens[ps.idx + 1].kind
        if next_tok == .Assign || next_tok == .Colon {
            // x := val  or  x: type = val
            return parse_var_def(ps, parent, allocator)
        } 

        // Parse lvalue expression (could be x, x[i], x.field, etc.)
        expr := parse_expression(ps, parent, allocator) or_return
        if ps.current_token.kind == .Eq {
            // x = val
            parser_advance(ps)  // eat '='
            
            // Validate lvalue
            #partial switch expr.node_kind {
            case .Identifier, .Index, .Field_Access:
                // Valid
            case:
                return nil, "Invalid assignment target"
            }
            
            value := parse_expression(ps, parent, allocator) or_return
            
            assign_node := new(Node, allocator)
            assign_node.node_kind = .Assignment
            assign_node.parent = parent
            assign_node.span = Span{start = expr.span.start, end = ps.idx - 1}
            assign_node.payload = Node_Assign{
                target = expr,
                value = value,
            }
        
            return assign_node, nil
        } 

        expr_stmt := new(Node, allocator)
        expr_stmt.node_kind = .Expr_Statement
        expr_stmt.payload = Node_Expr_Statement{expr = expr}
        return expr_stmt, nil

    case .KW_Del:
        span_start := ps.idx
        parser_advance(ps) // eat 'free'
        parser_consume(ps, .Left_Paren) or_return
        ptr := parse_expression(ps, parent, allocator) or_return
        parser_consume(ps, .Comma) or_return
        alloc := parse_expression(ps, parent, allocator) or_return
        parser_consume(ps, .Right_Paren) or_return
        return new_clone(Node{
            node_kind = .Del,
            parent = parent,
            span = Span{start = span_start, end = ps.idx - 1},
            payload = Node_Del{
                pointer=ptr,
                allocator=alloc,
            },
        }, allocator), nil

    case .KW_For: return parse_for_statement(ps, parent, allocator)
    case .KW_If: return parse_if_statement(ps, parent, allocator)
    case .KW_Print: return parse_print_statement(ps, parent, allocator)
    case .KW_Return: return parse_return_statement(ps, parent, allocator)
    case .KW_Size_Of: 
        return nil, fmt.tprintf("Cannot use `%v` here as a statement", ps.current_token.source)
    case:
        fmt.println(ps.current_token)
        return nil, fmt.tprintf("Cannot parse statement starting with %v at position %d", ps.current_token.kind, ps.idx)
    }
}

parse_var_def :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    name_tok := ps.current_token
    parser_advance(ps)

    explicit_type: Maybe(Type_Info)
    // Check for explicit type annotation: name: type = expr
    if ps.current_token.kind == .Colon {
        parser_advance(ps)
        explicit_type = parse_type(ps, allocator) or_return
        parser_consume(ps, .Eq) or_return
    } else {
        // Inferred type: name := expr
        parser_consume(ps, .Assign) or_return
    }

    expr := parse_expression(ps, parent, allocator) or_return
    
    var_node := new(Node, allocator)
    var_node.node_kind = .Var_Def
    var_node.parent = parent
    var_node.span = Span{start = span_start, end = ps.idx-1}
    // 
    var_node.payload = Node_Var_Def{name = name_tok.source, content = expr, explicit_type=explicit_type}
    
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

parse_len_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    
    parser_advance(ps)
    parser_consume(ps, .Left_Paren) or_return
    
    len_node := new(Node, allocator)
    len_node.node_kind = .Len
    len_node.parent = parent
    len_node.span = Span{start = span_start, end = ps.idx}
    
    content := parse_expression(ps, len_node, allocator) or_return
    len_node.payload = Node_Len{value = content}
    
    parser_consume(ps, .Right_Paren) or_return
    
    return len_node, nil
}

parse_size_of_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    
    parser_advance(ps)
    parser_consume(ps, .Left_Paren) or_return
    
    size_of_node := new(Node, allocator)
    size_of_node.node_kind = .Size_Of
    size_of_node.parent = parent
    size_of_node.span = Span{start = span_start, end = ps.idx}
    
    content := parse_expression(ps, size_of_node, allocator) or_return
    size_of_node.payload = Node_Size_Of{type = content}
    
    parser_consume(ps, .Right_Paren) or_return
    
    return size_of_node, nil
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
    left := parse_cast(ps, parent, allocator) or_return
    
    for ps.current_token.kind == .Gt || ps.current_token.kind == .Gt_Eq || ps.current_token.kind == .Lt || ps.current_token.kind == .Lt_Eq {
        op_idx := ps.idx
        op := ps.current_token
        parser_advance(ps)
        right := parse_cast(ps, parent, allocator) or_return 
        
        left = new_clone(Node {
            node_kind = .Bin_Op,
            parent = parent,
            span = Span{start = left.span.start, end = right.span.end},
            payload = Node_Bin_Op{left = left, right = right, op = op}
        }, allocator)
    }
    return left, nil
}

parse_cast :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    left := parse_term(ps, parent, allocator) or_return
    
    if ps.current_token.kind == .KW_As {
        parser_advance(ps)
        target_type := parse_type(ps, allocator) or_return
        
        return new_clone(Node{
            node_kind = .Cast,
            parent = parent,
            span = Span{start = left.span.start, end = ps.idx - 1},
            payload = Node_Cast{expr = left, target_type = target_type}
        }, allocator), nil
    }
    
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
    for ps.current_token.kind == .Star || ps.current_token.kind == .Slash || ps.current_token.kind == .Percent {
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
    if ps.current_token.kind == .Minus || ps.current_token.kind == .Plus || ps.current_token.kind == .Amp || ps.current_token.kind == .Star {
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
    } else if ps.current_token.kind == .KW_Size_Of {
        op_idx := ps.idx
        op := ps.current_token
        parser_advance(ps)
        parser_consume(ps, .Left_Paren)
        type_node := parse_expression(ps, parent, allocator) or_return
        parser_consume(ps, .Right_Paren)

        size_of_node := new_clone(Node {
            node_kind=.Size_Of,
            parent=parent,
            span=Span{start = op_idx, end = type_node.span.end},
            payload=Node_Size_Of {type=type_node}
        }, allocator)

        return size_of_node, nil
    } else if ps.current_token.kind == .KW_Len {
        op_idx := ps.idx
        op := ps.current_token
        parser_advance(ps)
        parser_consume(ps, .Left_Paren)
        value := parse_expression(ps, parent, allocator) or_return
        parser_consume(ps, .Right_Paren)

        len_node := new_clone(Node {
            node_kind=.Len,
            parent=parent,
            span=Span{start = op_idx, end = value.span.end},
            payload=Node_Len {value=value}
        }, allocator)

        return len_node, nil
    }

    return parse_postfix(ps, parent, allocator)
}

parse_postfix :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res:^Node, err:Parse_Error) {
    left := parse_primary(ps, parent, allocator) or_return
    
    for {
        #partial switch ps.current_token.kind {
        case .Left_Paren:
            // Function call
            parser_advance(ps) // eat '('
            fn_args := make([dynamic]^Node, allocator)
            for ps.current_token.kind != .Right_Paren {
                arg := parse_expression(ps, left, allocator) or_return
                append(&fn_args, arg)
                if ps.current_token.kind == .Comma {
                    parser_advance(ps)
                } else {
                    break
                }
            }
            parser_consume(ps, .Right_Paren) or_return

            left = new_clone(Node {
                node_kind=.Fn_Call,
                parent=parent,
                span=Span{start=left.span.start, end=ps.idx-1},
                payload=Node_Call {
                    callee=left,
                    args=fn_args,
                }
            }, allocator)

        case .Dot:
            // Field access
            parser_advance(ps) // eat '.'
            
            if ps.current_token.kind != .Iden {
                return nil, "Expected field name after '.'"
            }
            
            field_name := ps.current_token.source
            parser_advance(ps)
            
            field_node := new(Node, allocator)
            field_node.node_kind = .Field_Access
            field_node.parent = parent
            field_node.span = Span{start = left.span.start, end = ps.idx - 1}
            field_node.payload = Node_Field_Access{
                object = left,
                field_name = field_name,
            }
            
            left.parent = field_node
            left = field_node
        
        case .Left_Bracket:
            parser_advance(ps)  // eat '['
            index_expr := parse_expression(ps, left, allocator) or_return
            parser_consume(ps, .Right_Bracket) or_return
            
            index_node := new(Node, allocator)
            index_node.node_kind = .Index
            index_node.parent = parent
            index_node.span = Span{start = left.span.start, end = ps.idx - 1}
            index_node.payload = Node_Index{
                object = left,
                index = index_expr,
            }
            left.parent = index_node
            left = index_node
            
        case:
            return left, nil
        }
    }
}

parse_primary :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx        
    #partial switch ps.current_token.kind {
    case .Left_Paren: // Parenthesized expression
        parser_advance(ps)  // eat '('
        expr := parse_expression(ps, parent, allocator) or_return
        parser_consume(ps, .Right_Paren) or_return
        return expr, nil
    case .Iden: return parse_identifier(ps, parent, allocator)
    case .KW_Fn: return parse_fn_def(ps, parent, allocator)
    case .KW_Struct: 
        struct_type := parse_type(ps, allocator) or_return
        type_node := new(Node, allocator)
        type_node.node_kind = .Type_Expr
        type_node.parent = parent
        type_node.span = Span{start = span_start, end = ps.idx - 1}
        type_node.payload = Node_Type_Expr{type_info = struct_type}
        return type_node, nil
    case .Lit_Number: return parse_literal_number(ps, parent, allocator)
    case .Lit_String: return parse_literal_string(ps, parent, allocator)
    case .Lit_Nil: return parse_literal_nil(ps, parent, allocator)
    case .KW_Arr: return parse_literal_array(ps, parent, allocator)
    case:
        fmt.println(ps.current_token)
        return nil, fmt.tprintf("Expected expression at position %d, got %v", ps.idx, ps.current_token.kind)
    }
}

parse_identifier :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    name := ps.current_token.source
    
    parser_advance(ps)

    // Check if it's a struct literal: TypeName{ field: value, ... }
    // Lookahead to distinguish from regular blocks
    if ps.current_token.kind == .Left_Brace {
        next_idx := ps.idx + 1
        // Struct literal has pattern: { identifier : value }
        if next_idx < len(ps.tokens) && 
            ps.tokens[next_idx].kind == .Iden &&
            next_idx + 1 < len(ps.tokens) &&
            ps.tokens[next_idx + 1].kind == .Colon {
            // It's a struct literal
            return parse_struct_literal(ps, name, parent, allocator)
        }
        // Otherwise just an identifier, let caller handle the {
    }

    iden_node := new(Node, allocator)
    iden_node.node_kind = .Identifier
    iden_node.parent = parent
    iden_node.payload = Node_Identifier{name = name}
    iden_node.span = Span{start = span_start, end = ps.idx - 1}
    
    return iden_node, nil
}

parse_struct_literal :: proc(ps: ^Parsing_State, type_name: string, parent: ^Node, allocator := context.allocator) -> (res:^Node, err:Parse_Error) {
    span_start := ps.idx - 1
    parser_consume(ps, .Left_Brace) or_return
    
    field_inits := make([dynamic]Struct_Field_Init, allocator)
    
    for ps.current_token.kind != .Right_Brace {
        if ps.current_token.kind == .EOF {
            return nil, "Unexpected EOF in struct literal"
        }
        
        if ps.current_token.kind != .Iden do return nil, "Expected field name"
        
        field_name := ps.current_token.source
        parser_advance(ps)
        parser_consume(ps, .Colon) or_return
        
        // Parse field value
        field_value := parse_expression(ps, parent, allocator) or_return
        append(&field_inits, Struct_Field_Init{name = field_name, value = field_value})
        
        if ps.current_token.kind == .Comma {
            parser_advance(ps)
        } else if ps.current_token.kind != .Right_Brace {
            return nil, "Expected ',' or '}' in struct literal"
        }
    }
    
    parser_consume(ps, .Right_Brace) or_return
    
    struct_node := new(Node, allocator)
    struct_node.node_kind = .Struct_Literal
    struct_node.parent = parent
    struct_node.span = Span{start = span_start, end = ps.idx - 1}
    struct_node.payload = Node_Struct_Literal{
        type_name = type_name,
        field_inits = field_inits,
    }
    
    return struct_node, nil
}

parse_literal_number :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    tok := ps.current_token
    
    lit_node := new(Node, allocator)
    lit_node.node_kind = .Literal_Number
    lit_node.parent = parent
    lit_node.payload = Node_Literal_Number{content = tok}
    
    parser_advance(ps)
    lit_node.span = Span{start = span_start, end = ps.idx - 1}
    
    return lit_node, nil
}

parse_literal_string :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    tok := ps.current_token
    
    lit_node := new(Node, allocator)
    lit_node.node_kind = .Literal_String
    lit_node.parent = parent
    lit_node.payload = Node_Literal_String{content = tok}
    
    parser_advance(ps)
    lit_node.span = Span{start = span_start, end = ps.idx - 1}
    
    return lit_node, nil
}

parse_literal_nil :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res: ^Node, err: Parse_Error) {
    span_start := ps.idx
    
    lit_node := new(Node, allocator)
    lit_node.node_kind = .Literal_Nil
    lit_node.parent = parent
    lit_node.payload = Node_Literal_Nil{}
    
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

            param_type := parse_type(ps, allocator) or_return
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
        return_type = parse_type(ps, allocator) or_return

    }

    is_external := false
    if ps.current_token.kind == .KW_External {
        is_external = true
        parser_advance(ps)
    }

    fn_node := new(Node, allocator)
    stmts := make([dynamic]^Node, allocator)

    if !is_external {
        parser_consume(ps, .Left_Brace) or_return
        for ps.current_token.kind != .Right_Brace {
            if ps.current_token.kind == .EOF {
                return nil, fmt.tprintf("Unexpected EOF in function body starting at position %d", span_start)
            }
            stmt := parse_statement(ps, fn_node, allocator) or_return
            append(&stmts, stmt)
        }
        parser_consume(ps, .Right_Brace) or_return
    }
    
    fn_node.node_kind = .Fn_Def
    fn_node.parent = parent
    fn_node.span = Span{start = span_start, end = ps.idx}
    fn_node.payload = Node_Fn_Def{params=param_list, body=stmts, return_type=return_type, is_external=is_external}
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

parse_literal_array :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res:^Node, err:Parse_Error) {
    span_start := ps.idx
    
    parser_advance(ps) // eat 'arr'
    parser_consume(ps, .Lt) or_return
    
    // Parse element type
    elem_type := parse_type(ps, allocator) or_return
    
    parser_consume(ps, .Comma) or_return
    
    // Parse size (must be literal number)
    if ps.current_token.kind != .Lit_Number {
        return nil, "Array size must be a number literal"
    }
    size_str := ps.current_token.source
    size := strconv.atoi(size_str)  // You'll need core:strconv
    parser_advance(ps)
    
    parser_consume(ps, .Gt) or_return
    parser_consume(ps, .Left_Brace) or_return
    
    // Parse elements
    elements := make([dynamic]^Node, allocator)
    for ps.current_token.kind != .Right_Brace {
        elem := parse_expression(ps, parent, allocator) or_return
        append(&elements, elem)
        
        if ps.current_token.kind == .Comma {
            parser_advance(ps)
        } else if ps.current_token.kind != .Right_Brace {
            return nil, "Expected ',' or '}' in array literal"
        }
    }
    
    parser_consume(ps, .Right_Brace) or_return
    
    arr_node := new(Node, allocator)
    arr_node.node_kind = .Literal_Arr
    arr_node.parent = parent
    arr_node.span = Span{start = span_start, end = ps.idx - 1}
    arr_node.payload = Node_Array_Literal{
        element_type = elem_type,
        size = size,
        elements = elements,
    }
    
    return arr_node, nil
}

parse_for_statement :: proc(ps: ^Parsing_State, parent: ^Node, allocator := context.allocator) -> (res:^Node, err:Parse_Error) {
    span_start := ps.idx
    parser_advance(ps) // eat 'for'
    
    // Parse first identifier
    if ps.current_token.kind != .Iden {
        return nil, "Expected identifier after 'for'"
    }
    
    // Lookahead to determine which type of for loop
    next_tok := ps.tokens[ps.idx + 1].kind
    
    if next_tok == .KW_In {
        // For-in loop: for i in arr { }
        iterator := ps.current_token.source
        parser_advance(ps)
        parser_consume(ps, .KW_In) or_return
        
        iterable := parse_expression(ps, parent, allocator) or_return
        
        parser_consume(ps, .Left_Brace) or_return
        body := make([dynamic]^Node, allocator)
        for ps.current_token.kind != .Right_Brace {
            if ps.current_token.kind == .EOF {
                return nil, "Unexpected EOF in for statement"
            }
            stmt := parse_statement(ps, parent, allocator) or_return
            append(&body, stmt)
        }
        parser_consume(ps, .Right_Brace) or_return
        
        return new_clone(Node{
            node_kind = .For_In,
            parent = parent,
            span = Span{start = span_start, end = ps.idx - 1},
            payload = Node_For_In{
                iterator = iterator,
                iterable = iterable,
                body = body,
            }
        }, allocator), nil
    }
    
    // C-style for loop: for i := 0; i < n; i = i + 1 { }
    init := parse_statement(ps, parent, allocator) or_return
    parser_consume(ps, .Semicolon) or_return
    
    condition := parse_expression(ps, parent, allocator) or_return
    parser_consume(ps, .Semicolon) or_return
    
    post := parse_statement(ps, parent, allocator) or_return
    
    parser_consume(ps, .Left_Brace) or_return
    body := make([dynamic]^Node, allocator)
    for ps.current_token.kind != .Right_Brace {
        if ps.current_token.kind == .EOF {
            return nil, "Unexpected EOF in for statement"
        }
        stmt := parse_statement(ps, parent, allocator) or_return
        append(&body, stmt)
    }
    parser_consume(ps, .Right_Brace) or_return
    
    return new_clone(Node{
        node_kind = .For_C,
        parent = parent,
        span = Span{start = span_start, end = ps.idx - 1},
        payload = Node_For_C{
            init = init,
            condition = condition,
            post = post,
            body = body,
        }
    }, allocator), nil
}

parse_type :: proc(ps: ^Parsing_State, allocator: mem.Allocator) -> (res:Type_Info, err:Parse_Error) {
    // Pointer type
    if ps.current_token.kind == .Star {
        parser_advance(ps)
        pointee_type := parse_type(ps, allocator) or_return
        return Pointer_Type{
            pointee = new_clone(pointee_type, allocator),
        }, nil
    }
    
    // Array type: arr<T, N>
    if ps.current_token.kind == .KW_Arr {
        parser_advance(ps)
        parser_consume(ps, .Lt) or_return
        
        elem_type := parse_type(ps, allocator) or_return
        
        parser_consume(ps, .Comma) or_return
        
        if ps.current_token.kind != .Lit_Number {
            return nil, "Array size must be an integer and known at compile-time"
        }
        size_str := ps.current_token.source
        size := strconv.atoi(size_str)
        parser_advance(ps)
        
        parser_consume(ps, .Gt) or_return
        
        return Array_Type{
            element_type = new_clone(elem_type, allocator),
            size = size,
        }, nil
    }

    if ps.current_token.kind == .KW_Struct {
        parser_advance(ps)
        parser_consume(ps, .Left_Brace) or_return
        fields := make([dynamic]Struct_Field, allocator)
        
        for ps.current_token.kind != .Right_Brace {
            if ps.current_token.kind == .EOF {
                return nil, "Unexpected EOF in struct definition"
            }
            
            // Parse field name
            if ps.current_token.kind != .Iden {
                return nil, "Expected field name"
            }
            field_name := ps.current_token.source
            parser_advance(ps)
            
            parser_consume(ps, .Colon) or_return
            
            // Parse field type
            field_type := parse_type(ps, allocator) or_return
            
            append(&fields, Struct_Field{name = field_name, type = field_type})
            
            // Optional comma
            if ps.current_token.kind == .Comma {
                parser_advance(ps)
            }
        }
        
        parser_consume(ps, .Right_Brace) or_return
        
        return Struct_Type{fields = fields}, nil
    }

    // User-defined type name (identifier or qualified name like pkg.Type)
    if ps.current_token.kind == .Iden {
        type_name := ps.current_token.source
        parser_advance(ps)
        
        // Check for qualified name: pkg.Type
        if ps.current_token.kind == .Dot {
            parser_advance(ps) // eat '.'
            if ps.current_token.kind != .Iden {
                return nil, "Expected type name after '.'"
            }
            // Construct qualified name as "pkg.Type"
            type_name = fmt.tprintf("%s.%s", type_name, ps.current_token.source)
            parser_advance(ps)
        }
        
        return Named_Type{name = type_name}, nil
    } 

    prim: Type_Info
    #partial switch ps.current_token.kind {
    case .KW_Void: prim = .Void
    case .KW_I8: prim = .I8
    case .KW_U8: prim = .U8
    case .KW_I32: prim = .I32
    case .KW_I64: prim = .I64
    case .KW_F32: prim = .F32
    case .KW_F64: prim = .F64
    case .KW_Fn:
        parser_advance(ps)
        parser_consume(ps, .Left_Paren) or_return
        
        param_types := make([dynamic]Type_Info, allocator)
        for ps.current_token.kind != .Right_Paren {
            param_type := parse_type(ps, allocator) or_return
            append(&param_types, param_type)
            
            if ps.current_token.kind == .Comma {
                parser_advance(ps)
            } else if ps.current_token.kind != .Right_Paren {
                return nil, "Expected ',' or ')' in function type"
            }
        }
        parser_consume(ps, .Right_Paren) or_return
        
        parser_consume(ps, .Colon) or_return
        return_type := parse_type(ps, allocator) or_return
        
        return Function_Type{
            params = param_types[:],
            return_type = new_clone(return_type, allocator),
        }, nil
    case: 
        fmt.println(ps.current_token)
        return nil, fmt.tprintf("Expected type at position %d, got %v", ps.idx, ps.current_token.kind)
    }
    // TODO(Aria): function type and other types parsing

    parser_advance(ps)

    return prim, nil
}