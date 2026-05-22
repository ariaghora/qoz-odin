package main

import "core:fmt"
import "core:os"
import "core:strings"

Parse_Error :: struct {
    file:    string,
    line:    int,
    column:  int,
    message: string,
}

Parser :: struct {
    file:   string,
    tokens: []Token,
    pos:    int,
    errors: [dynamic]Parse_Error,
}

parse_file :: proc(file: string, tokens: []Token, allocator := context.allocator) -> (f: File, errs: []Parse_Error) {
    p := Parser{
        file   = file,
        tokens = tokens,
        pos    = 0,
        errors = make([dynamic]Parse_Error, allocator),
    }
    context.allocator = allocator

    decls := make([dynamic]Decl, allocator)
    for !at(&p, .EOF) {
        // Skip ASI-synthesised semicolons between top-level declarations.
        for accept(&p, .Semicolon) { }
        if at(&p, .EOF) do break
        d, ok := parse_decl(&p)
        if ok {
            append(&decls, d)
        } else {
            recover_to_top_level(&p)
        }
    }

    return File{path = file, decls = decls[:]}, p.errors[:]
}

// --- Cursor helpers ---

peek_tok :: proc(p: ^Parser, n: int = 0) -> Token {
    i := p.pos + n
    if i >= len(p.tokens) do return p.tokens[len(p.tokens)-1]
    return p.tokens[i]
}

at :: proc(p: ^Parser, k: Token_Kind) -> bool {
    return peek_tok(p).kind == k
}

at_any :: proc(p: ^Parser, kinds: ..Token_Kind) -> bool {
    cur := peek_tok(p).kind
    for k in kinds do if cur == k do return true
    return false
}

accept :: proc(p: ^Parser, k: Token_Kind) -> bool {
    if at(p, k) {
        p.pos += 1
        return true
    }
    return false
}

expect :: proc(p: ^Parser, k: Token_Kind, what: string) -> (Token, bool) {
    if at(p, k) {
        t := p.tokens[p.pos]
        p.pos += 1
        return t, true
    }
    t := peek_tok(p)
    append(&p.errors, Parse_Error{
        file = p.file, line = t.line, column = t.column,
        message = fmt.tprintf("expected %s, got %v `%s`", what, t.kind, t.source),
    })
    return Token{}, false
}

eat :: proc(p: ^Parser, k: Token_Kind, what: string) -> bool {
    _, ok := expect(p, k, what)
    return ok
}

error_here :: proc(p: ^Parser, message: string) {
    t := peek_tok(p)
    append(&p.errors, Parse_Error{
        file = p.file, line = t.line, column = t.column,
        message = message,
    })
}

current_span :: proc(p: ^Parser) -> Span {
    t := peek_tok(p)
    return Span{file = p.file, line = t.line, column = t.column}
}

recover_to_top_level :: proc(p: ^Parser) {
    for !at(p, .EOF) {
        if at_any(p, .KW_Let, .KW_Type, .KW_Import, .KW_External, .Hash) {
            return
        }
        p.pos += 1
    }
}

// --- Top-level declarations ---

parse_decl :: proc(p: ^Parser) -> (Decl, bool) {
    attrs := parse_attributes(p)
    // ASI may inject a `;` between an attribute line and the declaration
    // it adorns; consume it so the attribute attaches to the next decl.
    for accept(p, .Semicolon) { }

    decl: Decl
    ok: bool
    #partial switch peek_tok(p).kind {
    case .KW_Import:   decl, ok = parse_import(p)
    case .KW_Type:     decl, ok = parse_type_decl(p)
    case .KW_External: decl, ok = parse_external(p)
    case .Hash:        decl, ok = parse_link(p)
    case .KW_Let:      decl, ok = parse_top_let(p)
    case:
        error_here(p, fmt.tprintf("expected top-level declaration, got %v", peek_tok(p).kind))
        return nil, false
    }
    if !ok do return nil, false

    apply_attributes(p, decl, attrs)
    return decl, true
}

parse_attributes :: proc(p: ^Parser) -> []Attribute {
    out := make([dynamic]Attribute)
    for at(p, .At_Sign) {
        span := current_span(p)
        if !eat(p, .At_Sign, "`@`") do return out[:]
        nm, ok := expect(p, .Ident, "attribute name")
        if !ok do return out[:]
        attr := Attribute{span = span, name = nm.source}
        if accept(p, .Left_Paren) {
            s, sok := expect(p, .Lit_String, "string argument")
            if !sok do return out[:]
            attr.string_arg = unquote_string(s.source)
            attr.has_arg = true
            if !eat(p, .Right_Paren, "`)`") do return out[:]
        }
        append(&out, attr)
    }
    return out[:]
}

apply_attributes :: proc(p: ^Parser, decl: Decl, attrs: []Attribute) {
    if len(attrs) == 0 do return
    for a in attrs {
        switch a.name {
        case "link_name":
            ext, is_ext := decl.(^Decl_External)
            if !is_ext {
                append(&p.errors, Parse_Error{
                    file = a.span.file, line = a.span.line, column = a.span.column,
                    message = "`@link_name` only applies to external declarations",
                })
                continue
            }
            if !a.has_arg {
                append(&p.errors, Parse_Error{
                    file = a.span.file, line = a.span.line, column = a.span.column,
                    message = "`@link_name` requires a string argument",
                })
                continue
            }
            ext.symbol = a.string_arg
        case "operator":
            fn, is_fn := decl.(^Decl_Fn)
            if !is_fn {
                append(&p.errors, Parse_Error{
                    file = a.span.file, line = a.span.line, column = a.span.column,
                    message = "`@operator` only applies to function declarations",
                })
                continue
            }
            if !a.has_arg {
                append(&p.errors, Parse_Error{
                    file = a.span.file, line = a.span.line, column = a.span.column,
                    message = "`@operator` requires a string argument naming the operator (e.g. `[]`)",
                })
                continue
            }
            if !is_recognized_operator(a.string_arg) {
                append(&p.errors, Parse_Error{
                    file = a.span.file, line = a.span.line, column = a.span.column,
                    message = fmt.tprintf("unknown operator `%s` in `@operator(...)`", a.string_arg),
                })
                continue
            }
            fn.operator = a.string_arg
        case:
            append(&p.errors, Parse_Error{
                file = a.span.file, line = a.span.line, column = a.span.column,
                message = fmt.tprintf("unknown attribute `@%s`", a.name),
            })
        }
    }
}

is_recognized_operator :: proc(op: string) -> bool {
    switch op {
    case "[]", "[]=",
         "+", "-", "*", "/", "%",
         "==", "!=", "<", ">", "<=", ">=",
         "unary-", "unary!",
         "hash", "len":
        return true
    }
    return false
}

parse_import :: proc(p: ^Parser) -> (Decl, bool) {
    span := current_span(p)
    if !eat(p, .KW_Import, "`import`") do return nil, false
    segs := make([dynamic]string)
    for {
        ident, ok := expect(p, .Ident, "module name")
        if !ok do return nil, false
        append(&segs, ident.source)
        if !accept(p, .Slash) do break
    }
    alias: string = ""
    if accept(p, .KW_As) {
        ident, ok := expect(p, .Ident, "alias name")
        if !ok do return nil, false
        alias = ident.source
    }
    d := new(Decl_Import)
    d.span = span; d.path = segs[:]; d.alias = alias
    return d, true
}

// `let name = expr`                            top-level constant
// `let name<T>(params): T = expr`              function
// `let name(params): T = expr`                 function (no type params)
// `let name: T = expr`                         typed constant
parse_top_let :: proc(p: ^Parser) -> (Decl, bool) {
    span := current_span(p)
    if !eat(p, .KW_Let, "`let`") do return nil, false
    name, ok := expect(p, .Ident, "name")
    if !ok do return nil, false

    type_params: []string
    if accept(p, .Lt) {
        tp, tpok := parse_type_params(p)
        if !tpok do return nil, false
        type_params = tp
    }

    if at(p, .Left_Paren) {
        return finish_fn_decl(p, span, name.source, type_params)
    }

    if len(type_params) > 0 {
        error_here(p, "type parameters are only allowed on functions")
        return nil, false
    }

    var_type: ^Type_Expr = nil
    if accept(p, .Colon) {
        t, tok := parse_type(p)
        if !tok do return nil, false
        var_type = t
    }
    if !eat(p, .Eq, "`=`") do return nil, false
    val, vok := parse_expr(p)
    if !vok do return nil, false
    d := new(Decl_Const)
    d.span = span; d.name = name.source; d.type = var_type; d.value = val
    return d, true
}

finish_fn_decl :: proc(p: ^Parser, span: Span, name: string, type_params: []string) -> (Decl, bool) {
    params, pok := parse_fn_params(p)
    if !pok do return nil, false
    ret: ^Type_Expr = nil
    if accept(p, .Colon) {
        t, tok := parse_type(p)
        if !tok do return nil, false
        ret = t
    }
    if !eat(p, .Eq, "`=` before function body") do return nil, false
    body_expr, bok := parse_expr(p)
    if !bok do return nil, false
    // A function's body is an expression. If it's a block, store directly;
    // otherwise wrap in a one-tail block for uniformity.
    body_block: ^Expr_Block
    if b, is_block := body_expr.(^Expr_Block); is_block {
        body_block = b
    } else {
        body_block = new(Expr_Block)
        body_block.span = expr_span(body_expr)
        body_block.stmts = nil
        body_block.tail = body_expr
    }
    d := new(Decl_Fn)
    d.span        = span
    d.name        = name
    d.type_params = type_params
    d.params      = params
    d.ret         = ret
    d.body        = body_block
    return d, true
}

parse_fn_params :: proc(p: ^Parser) -> ([]Fn_Param, bool) {
    if !eat(p, .Left_Paren, "`(`") do return nil, false
    out := make([dynamic]Fn_Param)
    for !at(p, .Right_Paren) && !at(p, .EOF) {
        name, ok := expect(p, .Ident, "parameter name")
        if !ok do return nil, false
        if !eat(p, .Colon, "`:` after parameter name") do return nil, false
        t, tok := parse_type(p)
        if !tok do return nil, false
        append(&out, Fn_Param{name = name.source, type = t})
        if !accept(p, .Comma) do break
    }
    if !eat(p, .Right_Paren, "`)`") do return nil, false
    return out[:], true
}

parse_type_params :: proc(p: ^Parser) -> ([]string, bool) {
    out := make([dynamic]string)
    for !at(p, .Gt) && !at(p, .EOF) {
        n, ok := expect(p, .Ident, "type parameter")
        if !ok do return nil, false
        append(&out, n.source)
        if !accept(p, .Comma) do break
    }
    if !eat(p, .Gt, "`>` after type parameters") do return nil, false
    return out[:], true
}

// `type Name<...> = { ... }`            -> Decl_Struct (record)
// `type Name<...> = | A | B | C(...)`   -> Decl_Enum
// `type Name<...> = SomeType`           -> Decl_Type_Alias
parse_type_decl :: proc(p: ^Parser) -> (Decl, bool) {
    span := current_span(p)
    if !eat(p, .KW_Type, "`type`") do return nil, false
    name, ok := expect(p, .Ident, "type name")
    if !ok do return nil, false
    type_params: []string
    if accept(p, .Lt) {
        tp, tpok := parse_type_params(p)
        if !tpok do return nil, false
        type_params = tp
    }
    if !eat(p, .Eq, "`=` after type name") do return nil, false

    if at(p, .Left_Brace) {
        return finish_record_decl(p, span, name.source, type_params)
    }
    if at(p, .Pipe) {
        return finish_enum_decl(p, span, name.source, type_params)
    }
    // Alias: parse the rhs as a type expression.
    t, tok := parse_type(p)
    if !tok do return nil, false
    d := new(Decl_Type_Alias)
    d.span = span; d.name = name.source; d.type_params = type_params; d.target = t
    return d, true
}

finish_record_decl :: proc(p: ^Parser, span: Span, name: string, type_params: []string) -> (Decl, bool) {
    if !eat(p, .Left_Brace, "`{`") do return nil, false
    fields := make([dynamic]Struct_Field)
    for !at(p, .Right_Brace) && !at(p, .EOF) {
        fname, ok := expect(p, .Ident, "field name")
        if !ok do return nil, false
        if !eat(p, .Colon, "`:`") do return nil, false
        t, tok := parse_type(p)
        if !tok do return nil, false
        append(&fields, Struct_Field{name = fname.source, type = t})
        if !accept(p, .Comma) do break
    }
    if !eat(p, .Right_Brace, "`}`") do return nil, false
    d := new(Decl_Struct)
    d.span = span; d.name = name; d.type_params = type_params; d.fields = fields[:]
    return d, true
}

finish_enum_decl :: proc(p: ^Parser, span: Span, name: string, type_params: []string) -> (Decl, bool) {
    variants := make([dynamic]Variant_Decl)
    for accept(p, .Semicolon) { }
    for at(p, .Pipe) {
        p.pos += 1                                     // consume `|`
        v, vok := parse_variant(p)
        if !vok do return nil, false
        append(&variants, v)
        for accept(p, .Semicolon) { }
    }
    d := new(Decl_Enum)
    d.span = span; d.name = name; d.type_params = type_params; d.variants = variants[:]
    return d, true
}

parse_variant :: proc(p: ^Parser) -> (Variant_Decl, bool) {
    span := current_span(p)
    name, ok := expect(p, .Ident, "variant name")
    if !ok do return Variant_Decl{}, false
    v := Variant_Decl{span = span, name = name.source, kind = .None}
    if accept(p, .Left_Paren) {
        v.kind = .Positional
        pos := make([dynamic]^Type_Expr)
        for !at(p, .Right_Paren) && !at(p, .EOF) {
            t, tok := parse_type(p)
            if !tok do return Variant_Decl{}, false
            append(&pos, t)
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Paren, "`)`") do return Variant_Decl{}, false
        v.pos = pos[:]
    } else if accept(p, .Left_Brace) {
        v.kind = .Named
        fields := make([dynamic]Struct_Field)
        for !at(p, .Right_Brace) && !at(p, .EOF) {
            fname, fok := expect(p, .Ident, "field name")
            if !fok do return Variant_Decl{}, false
            if !eat(p, .Colon, "`:`") do return Variant_Decl{}, false
            t, tok := parse_type(p)
            if !tok do return Variant_Decl{}, false
            append(&fields, Struct_Field{name = fname.source, type = t})
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Brace, "`}`") do return Variant_Decl{}, false
        v.named = fields[:]
    }
    return v, true
}

parse_external :: proc(p: ^Parser) -> (Decl, bool) {
    span := current_span(p)
    if !eat(p, .KW_External, "`external`") do return nil, false
    if !eat(p, .KW_Let, "`let` after `external`") do return nil, false
    name, ok := expect(p, .Ident, "function name")
    if !ok do return nil, false
    params, pok := parse_fn_params(p)
    if !pok do return nil, false
    ret: ^Type_Expr = nil
    if accept(p, .Colon) {
        t, tok := parse_type(p)
        if !tok do return nil, false
        ret = t
    }
    d := new(Decl_External)
    d.span = span; d.name = name.source; d.symbol = name.source
    d.params = params; d.ret = ret
    return d, true
}

parse_link :: proc(p: ^Parser) -> (Decl, bool) {
    span := current_span(p)
    if !eat(p, .Hash, "`#`") do return nil, false
    keyword, ok := expect(p, .Ident, "link directive name")
    if !ok do return nil, false
    if !eat(p, .Left_Paren, "`(`") do return nil, false
    name_tok, ok2 := expect(p, .Lit_String, "library or framework name")
    if !ok2 do return nil, false
    if !eat(p, .Right_Paren, "`)`") do return nil, false
    d := new(Decl_Link)
    d.span = span; d.name = unquote_string(name_tok.source)
    switch keyword.source {
    case "link":           d.kind = .Library
    case "link_framework": d.kind = .Framework
    case:
        error_here(p, fmt.tprintf("unknown link directive `#%s`", keyword.source))
        return nil, false
    }
    return d, true
}

// --- Types ---

parse_type :: proc(p: ^Parser) -> (^Type_Expr, bool) {
    span := current_span(p)
    if accept(p, .Star) {
        inner, ok := parse_type(p)
        if !ok do return nil, false
        out := new(Type_Ptr); out.span = span; out.inner = inner
        result := new(Type_Expr); result^ = out
        return result, true
    }
    if accept(p, .Left_Paren) {
        if accept(p, .Right_Paren) {
            if accept(p, .Arrow) {
                r, ok := parse_type(p)
                if !ok do return nil, false
                out := new(Type_Fn); out.span = span; out.params = nil; out.ret = r
                result := new(Type_Expr); result^ = out
                return result, true
            }
            out := new(Type_Unit); out.span = span
            result := new(Type_Expr); result^ = out
            return result, true
        }
        elems := make([dynamic]^Type_Expr)
        for {
            t, ok := parse_type(p)
            if !ok do return nil, false
            append(&elems, t)
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Paren, "`)`") do return nil, false
        if accept(p, .Arrow) {
            r, ok := parse_type(p)
            if !ok do return nil, false
            out := new(Type_Fn); out.span = span; out.params = elems[:]; out.ret = r
            result := new(Type_Expr); result^ = out
            return result, true
        }
        if len(elems) == 1 {
            return elems[0], true
        }
        out := new(Type_Tuple); out.span = span; out.elems = elems[:]
        result := new(Type_Expr); result^ = out
        return result, true
    }
    if at_any(p, .Ident, .KW_I8, .KW_I16, .KW_I32, .KW_I64, .KW_U8, .KW_U16, .KW_U32, .KW_U64,
              .KW_F32, .KW_F64, .KW_Bool, .KW_Char, .KW_String, .KW_Cstring, .KW_Unit, .KW_Void) {
        first := p.tokens[p.pos]
        p.pos += 1
        segs := make([dynamic]string)
        append(&segs, first.source)
        for accept(p, .Dot) {
            n, ok := expect(p, .Ident, "type path segment")
            if !ok do return nil, false
            append(&segs, n.source)
        }
        args: []^Type_Expr
        if accept(p, .Lt) {
            arg_list := make([dynamic]^Type_Expr)
            for !at(p, .Gt) && !at(p, .EOF) {
                t, ok := parse_type(p)
                if !ok do return nil, false
                append(&arg_list, t)
                if !accept(p, .Comma) do break
            }
            if !eat(p, .Gt, "`>`") do return nil, false
            args = arg_list[:]
        }
        out := new(Type_Named); out.span = span; out.path = segs[:]; out.args = args
        result := new(Type_Expr); result^ = out
        return result, true
    }
    error_here(p, fmt.tprintf("expected type, got %v", peek_tok(p).kind))
    return nil, false
}

// --- Blocks and statements ---

parse_block :: proc(p: ^Parser) -> (^Expr_Block, bool) {
    span := current_span(p)
    if !eat(p, .Left_Brace, "`{`") do return nil, false
    stmts := make([dynamic]Stmt)
    tail: Expr = nil
    for !at(p, .Right_Brace) && !at(p, .EOF) {
        if at_any(p, .KW_Let, .KW_Var) {
            s, ok := parse_let_or_var_stmt(p)
            if !ok do return nil, false
            accept(p, .Semicolon)
            append(&stmts, s)
            continue
        }
        e, ok := parse_expr(p)
        if !ok do return nil, false
        accept(p, .Semicolon)
        if at(p, .Right_Brace) {
            tail = e
            break
        }
        es := new(Stmt_Expr); es.span = expr_span(e); es.expr = e
        append(&stmts, es)
    }
    if !eat(p, .Right_Brace, "`}`") do return nil, false
    out := new(Expr_Block); out.span = span; out.stmts = stmts[:]; out.tail = tail
    return out, true
}

parse_let_or_var_stmt :: proc(p: ^Parser) -> (Stmt, bool) {
    span := current_span(p)
    is_var := accept(p, .KW_Var)
    if !is_var { if !eat(p, .KW_Let, "`let`") do return nil, false }

    if !is_var && looks_like_let_else(p) {
        return parse_let_else(p, span)
    }

    name, ok := expect(p, .Ident, "binding name")
    if !ok do return nil, false
    if at(p, .Left_Paren) {
        error_here(p, "local functions are not supported; bind a closure: `let f = (x) -> ...`")
        return nil, false
    }
    var_type: ^Type_Expr = nil
    if accept(p, .Colon) {
        t, tok := parse_type(p)
        if !tok do return nil, false
        var_type = t
    }
    if !eat(p, .Eq, "`=`") do return nil, false
    val, vok := parse_expr(p)
    if !vok do return nil, false
    if is_var {
        s := new(Stmt_Var); s.span = span; s.name = name.source; s.type = var_type; s.value = val
        return s, true
    }
    s := new(Stmt_Let); s.span = span; s.name = name.source; s.type = var_type; s.value = val
    return s, true
}

// A `let` introduces a let-else statement when the LHS is a refutable
// pattern. The lookahead approximation: an upper-cased identifier followed
// by `.`, `(`, or `{`, which starts a variant-constructor pattern.
looks_like_let_else :: proc(p: ^Parser) -> bool {
    head := peek_tok(p, 0)
    if head.kind != .Ident do return false
    if !is_upper_first(head.source) do return false
    next := peek_tok(p, 1).kind
    return next == .Dot || next == .Left_Paren || next == .Left_Brace
}

parse_let_else :: proc(p: ^Parser, span: Span) -> (Stmt, bool) {
    pat, pok := parse_pattern(p)
    if !pok do return nil, false
    if !eat(p, .Eq, "`=`") do return nil, false
    val, vok := parse_expr(p)
    if !vok do return nil, false
    if !eat(p, .KW_Else, "`else` after let-else binding") do return nil, false
    blk, bok := parse_block(p)
    if !bok do return nil, false
    s := new(Stmt_Let_Else)
    s.span = span; s.pat = pat; s.value = val; s.else_block = blk
    return s, true
}

is_block_ending_expr :: proc(e: Expr) -> bool {
    #partial switch _ in e {
    case ^Expr_Block, ^Expr_If, ^Expr_Match, ^Expr_While, ^Expr_For:
        return true
    }
    return false
}

// --- Expressions: precedence-climbing ---

parse_expr :: proc(p: ^Parser) -> (Expr, bool) {
    return parse_assign(p)
}

parse_assign :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_or(p)
    if !ok do return nil, false
    span := current_span(p)
    op: Assign_Op
    matched := false
    #partial switch peek_tok(p).kind {
    case .Eq:         op = .Set;     matched = true
    case .Plus_Eq:    op = .Add_Set; matched = true
    case .Minus_Eq:   op = .Sub_Set; matched = true
    case .Star_Eq:    op = .Mul_Set; matched = true
    case .Slash_Eq:   op = .Div_Set; matched = true
    case .Percent_Eq: op = .Mod_Set; matched = true
    }
    if matched {
        p.pos += 1
        rhs, rok := parse_assign(p)
        if !rok do return nil, false
        out := new(Expr_Assign)
        out.span = span; out.op = op; out.target = lhs; out.value = rhs
        return out, true
    }
    return lhs, true
}

parse_or :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_and(p)
    if !ok do return nil, false
    for accept(p, .Or_Or) {
        span := current_span(p)
        rhs, rok := parse_and(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = .Or; out.lhs = lhs; out.rhs = rhs
        lhs = out
    }
    return lhs, true
}

parse_and :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_eq(p)
    if !ok do return nil, false
    for accept(p, .And_And) {
        span := current_span(p)
        rhs, rok := parse_eq(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = .And; out.lhs = lhs; out.rhs = rhs
        lhs = out
    }
    return lhs, true
}

parse_eq :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_cmp(p)
    if !ok do return nil, false
    for {
        op: Binary_Op
        if accept(p, .Eq_Eq) { op = .Eq }
        else if accept(p, .Not_Eq) { op = .Ne }
        else { break }
        span := current_span(p)
        rhs, rok := parse_cmp(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = op; out.lhs = lhs; out.rhs = rhs
        lhs = out
    }
    return lhs, true
}

parse_cmp :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_range(p)
    if !ok do return nil, false
    for {
        op: Binary_Op
        if accept(p, .Lt) { op = .Lt }
        else if accept(p, .Gt) { op = .Gt }
        else if accept(p, .Lt_Eq) { op = .Le }
        else if accept(p, .Gt_Eq) { op = .Ge }
        else { break }
        span := current_span(p)
        rhs, rok := parse_range(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = op; out.lhs = lhs; out.rhs = rhs
        lhs = out
    }
    return lhs, true
}

parse_range :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_add(p)
    if !ok do return nil, false
    if accept(p, .Range_Incl) {
        span := current_span(p)
        rhs, rok := parse_add(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = .Range_Inclusive; out.lhs = lhs; out.rhs = rhs
        return out, true
    }
    if accept(p, .Range_Excl) {
        span := current_span(p)
        rhs, rok := parse_add(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = .Range; out.lhs = lhs; out.rhs = rhs
        return out, true
    }
    return lhs, true
}

parse_add :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_mul(p)
    if !ok do return nil, false
    for {
        op: Binary_Op
        if accept(p, .Plus) { op = .Add }
        else if accept(p, .Minus) { op = .Sub }
        else { break }
        span := current_span(p)
        rhs, rok := parse_mul(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = op; out.lhs = lhs; out.rhs = rhs
        lhs = out
    }
    return lhs, true
}

parse_mul :: proc(p: ^Parser) -> (Expr, bool) {
    lhs, ok := parse_unary(p)
    if !ok do return nil, false
    for {
        op: Binary_Op
        if accept(p, .Star) { op = .Mul }
        else if accept(p, .Slash) { op = .Div }
        else if accept(p, .Percent) { op = .Mod }
        else { break }
        span := current_span(p)
        rhs, rok := parse_unary(p)
        if !rok do return nil, false
        out := new(Expr_Binary); out.span = span; out.op = op; out.lhs = lhs; out.rhs = rhs
        lhs = out
    }
    return lhs, true
}

parse_unary :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if accept(p, .Minus) {
        rhs, ok := parse_unary(p); if !ok do return nil, false
        out := new(Expr_Unary); out.span = span; out.op = .Neg; out.rhs = rhs
        return out, true
    }
    if accept(p, .Bang) {
        rhs, ok := parse_unary(p); if !ok do return nil, false
        out := new(Expr_Unary); out.span = span; out.op = .Not; out.rhs = rhs
        return out, true
    }
    if accept(p, .Star) {
        rhs, ok := parse_unary(p); if !ok do return nil, false
        out := new(Expr_Unary); out.span = span; out.op = .Deref; out.rhs = rhs
        return out, true
    }
    if accept(p, .Amp) {
        rhs, ok := parse_unary(p); if !ok do return nil, false
        out := new(Expr_Unary); out.span = span; out.op = .Addr; out.rhs = rhs
        return out, true
    }
    return parse_postfix(p)
}

parse_postfix :: proc(p: ^Parser) -> (Expr, bool) {
    e, ok := parse_primary(p)
    if !ok do return nil, false
    for {
        span := current_span(p)
        if id, is_id := e.(^Expr_Ident); is_id && id.name == "size_of" && at(p, .Left_Paren) {
            p.pos += 1
            t, tok := parse_type(p)
            if !tok do return nil, false
            if !eat(p, .Right_Paren, "`)` after size_of argument") do return nil, false
            so := new(Expr_Size_Of); so.span = id.span; so.target = t
            e = so
            continue
        }
        // (#load_string handling is in parse_primary; nothing to do here.)
        if accept(p, .Left_Paren) {
            args := make([dynamic]Expr)
            for !at(p, .Right_Paren) && !at(p, .EOF) {
                a, aok := parse_expr(p)
                if !aok do return nil, false
                append(&args, a)
                if !accept(p, .Comma) do break
            }
            if !eat(p, .Right_Paren, "`)`") do return nil, false
            c := new(Expr_Call); c.span = span; c.callee = e; c.args = args[:]
            e = c
            continue
        }
        if accept(p, .Dot) {
            n, nok := expect(p, .Ident, "field name")
            if !nok do return nil, false
            f := new(Expr_Field); f.span = span; f.base = e; f.name = n.source
            e = f
            continue
        }
        if accept(p, .Left_Bracket) {
            idx, iok := parse_expr(p)
            if !iok do return nil, false
            if !eat(p, .Right_Bracket, "`]`") do return nil, false
            i := new(Expr_Index); i.span = span; i.base = e; i.index = idx
            e = i
            continue
        }
        if accept(p, .Question) {
            t := new(Expr_Try); t.span = span; t.value = e
            e = t
            continue
        }
        if accept(p, .KW_As) {
            tt, tok := parse_type(p)
            if !tok do return nil, false
            c := new(Expr_Cast); c.span = span; c.value = e; c.target = tt
            e = c
            continue
        }
        // Record literal: <path> { ... }
        if at(p, .Left_Brace) && looks_like_record_literal(p) {
            if path_expr, is_path := e.(^Expr_Path); is_path {
                r, rok := finish_record_literal(p, span, path_expr)
                if !rok do return nil, false
                e = r
                continue
            }
            if id, is_id := e.(^Expr_Ident); is_id {
                path := new(Expr_Path); path.span = id.span
                segs := make([]string, 1)
                segs[0] = id.name
                path.segs = segs
                r, rok := finish_record_literal(p, span, path)
                if !rok do return nil, false
                e = r
                continue
            }
        }
        break
    }
    return e, true
}

parse_primary :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    tok := peek_tok(p)
    #partial switch tok.kind {
    case .Hash:
        // Compile-time directive in expression position. Currently
        // supported: #load_string("path") yields the file's bytes as
        // a string. #load_bytes is reserved for a future Vec<u8> form.
        // Mirrors the #link / #link_framework decl-level directives.
        p.pos += 1
        ident_tok, idok := expect(p, .Ident, "directive name after `#`")
        if !idok do return nil, false
        if !eat(p, .Left_Paren, "`(` after directive name") do return nil, false
        path_tok, sok := expect(p, .Lit_String, "string literal path argument")
        if !sok do return nil, false
        if !eat(p, .Right_Paren, "`)` after directive argument") do return nil, false
        raw_path := path_tok.source
        if len(raw_path) >= 2 && raw_path[0] == '"' && raw_path[len(raw_path)-1] == '"' {
            raw_path = raw_path[1:len(raw_path)-1]
        }
        if ident_tok.source != "load_string" {
            append(&p.errors, Parse_Error{
                file = p.file, line = span.line, column = span.column,
                message = fmt.tprintf("unknown directive `#%s`; supported in expression position: #load_string", ident_tok.source),
            })
            return nil, false
        }
        bytes, read_err := os.read_entire_file(raw_path, context.allocator)
        if read_err != nil {
            append(&p.errors, Parse_Error{
                file = p.file, line = span.line, column = span.column,
                message = fmt.tprintf("#load_string: cannot read `%s`: %v", raw_path, read_err),
            })
            return nil, false
        }
        sb := strings.builder_make(context.allocator)
        strings.write_byte(&sb, '"')
        for b in bytes {
            switch b {
            case '\\': strings.write_string(&sb, "\\\\")
            case '"':  strings.write_string(&sb, "\\\"")
            case '\n': strings.write_string(&sb, "\\n")
            case '\r': strings.write_string(&sb, "\\r")
            case '\t': strings.write_string(&sb, "\\t")
            case:
                if b < 0x20 || b == 0x7f {
                    strings.write_string(&sb, fmt.tprintf("\\x%02x", b))
                } else {
                    strings.write_byte(&sb, b)
                }
            }
        }
        strings.write_byte(&sb, '"')
        sl := new(Expr_String_Lit); sl.span = span; sl.text = strings.to_string(sb)
        return sl, true
    case .Lit_Int:
        p.pos += 1
        out := new(Expr_Int_Lit); out.span = span; out.text = tok.source
        return out, true
    case .Lit_Float:
        p.pos += 1
        out := new(Expr_Float_Lit); out.span = span; out.text = tok.source
        return out, true
    case .Lit_String:
        p.pos += 1
        out := new(Expr_String_Lit); out.span = span; out.text = tok.source
        return out, true
    case .Lit_Char:
        p.pos += 1
        out := new(Expr_Char_Lit); out.span = span; out.text = tok.source
        return out, true
    case .Lit_True:
        p.pos += 1
        out := new(Expr_Bool_Lit); out.span = span; out.value = true
        return out, true
    case .Lit_False:
        p.pos += 1
        out := new(Expr_Bool_Lit); out.span = span; out.value = false
        return out, true
    case .Lit_Nil:
        p.pos += 1
        out := new(Expr_Nil_Lit); out.span = span
        return out, true
    case .KW_New:
        p.pos += 1
        v, ok := parse_unary(p); if !ok do return nil, false
        out := new(Expr_New); out.span = span; out.value = v
        return out, true
    case .KW_Return:
        p.pos += 1
        val: Expr = nil
        if !at_any(p, .Semicolon, .Right_Brace, .Comma, .EOF) {
            v, ok := parse_expr(p); if !ok do return nil, false
            val = v
        }
        out := new(Expr_Return); out.span = span; out.value = val
        return out, true
    case .KW_Defer:
        p.pos += 1
        v, ok := parse_expr(p); if !ok do return nil, false
        out := new(Expr_Defer); out.span = span; out.body = v
        return out, true
    case .KW_If:    return parse_if(p)
    case .KW_Match: return parse_match(p)
    case .KW_While: return parse_while(p)
    case .KW_For:   return parse_for(p)
    case .Left_Brace: return parse_block(p)
    case .Left_Paren:
        if looks_like_arrow_closure(p) {
            return parse_arrow_closure(p)
        }
        return parse_paren_or_tuple(p)
    case .Left_Bracket: return parse_array_literal(p)
    case .Ident, .KW_String, .KW_Bool, .KW_I32, .KW_I64, .KW_F32, .KW_F64,
         .KW_U8, .KW_U16, .KW_U32, .KW_U64, .KW_I8, .KW_I16, .KW_Char, .KW_Unit, .KW_Void:
        return parse_path(p)
    }
    error_here(p, fmt.tprintf("expected expression, got %v `%s`", tok.kind, tok.source))
    return nil, false
}

parse_paren_or_tuple :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .Left_Paren, "`(`") do return nil, false
    if accept(p, .Right_Paren) {
        out := new(Expr_Tuple); out.span = span; out.elems = nil
        return out, true
    }
    first, ok := parse_expr(p); if !ok do return nil, false
    if accept(p, .Comma) {
        elems := make([dynamic]Expr); append(&elems, first)
        for !at(p, .Right_Paren) && !at(p, .EOF) {
            e, eok := parse_expr(p); if !eok do return nil, false
            append(&elems, e)
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Paren, "`)`") do return nil, false
        out := new(Expr_Tuple); out.span = span; out.elems = elems[:]
        return out, true
    }
    if !eat(p, .Right_Paren, "`)`") do return nil, false
    return first, true
}

parse_array_literal :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .Left_Bracket, "`[`") do return nil, false
    elems := make([dynamic]Expr)
    for !at(p, .Right_Bracket) && !at(p, .EOF) {
        e, ok := parse_expr(p); if !ok do return nil, false
        append(&elems, e)
        if !accept(p, .Comma) do break
    }
    if !eat(p, .Right_Bracket, "`]`") do return nil, false
    out := new(Expr_Array_Lit); out.span = span; out.elems = elems[:]
    return out, true
}

// `() -> body`                       no params
// `(x) -> body`                       single untyped param
// `(x, y) -> body`                    multiple untyped params
// `(x: T) -> body`                    typed single param
// `(x: T, y: U): R -> body`           full annotation
//
// The parser dispatches into here only after `looks_like_arrow_closure`
// confirmed that the matching `)` is followed by `->` or `:`.
parse_arrow_closure :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .Left_Paren, "`(`") do return nil, false
    params := make([dynamic]Closure_Param)
    for !at(p, .Right_Paren) && !at(p, .EOF) {
        n, ok := expect(p, .Ident, "closure parameter")
        if !ok do return nil, false
        cp := Closure_Param{name = n.source, type = nil}
        if accept(p, .Colon) {
            t, tok := parse_type(p); if !tok do return nil, false
            cp.type = t
        }
        append(&params, cp)
        if !accept(p, .Comma) do break
    }
    if !eat(p, .Right_Paren, "`)` after closure parameters") do return nil, false
    ret: ^Type_Expr = nil
    if accept(p, .Colon) {
        r, ok := parse_type(p); if !ok do return nil, false
        ret = r
    }
    if !eat(p, .Arrow, "`->` in closure") do return nil, false
    body, ok := parse_expr(p); if !ok do return nil, false
    out := new(Expr_Closure)
    out.span = span; out.params = params[:]; out.ret = ret; out.body = body
    return out, true
}

// One-token lookahead: scan from the current `(` to its matching `)` and
// peek the next token. A trailing `->` or `:` marks a closure parameter list.
looks_like_arrow_closure :: proc(p: ^Parser) -> bool {
    if peek_tok(p).kind != .Left_Paren do return false
    depth := 1
    i := p.pos + 1
    for i < len(p.tokens) {
        k := p.tokens[i].kind
        if k == .EOF do return false
        if k == .Left_Paren do depth += 1
        if k == .Right_Paren {
            depth -= 1
            if depth == 0 {
                next := i + 1
                if next >= len(p.tokens) do return false
                nk := p.tokens[next].kind
                return nk == .Arrow || nk == .Colon
            }
        }
        i += 1
    }
    return false
}

parse_path :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    first := p.tokens[p.pos]
    p.pos += 1
    if !at(p, .Dot) {
        out := new(Expr_Ident); out.span = span; out.name = first.source
        return out, true
    }
    segs := make([dynamic]string); append(&segs, first.source)
    for accept(p, .Dot) {
        n, ok := expect(p, .Ident, "path segment")
        if !ok do return nil, false
        append(&segs, n.source)
    }
    out := new(Expr_Path); out.span = span; out.segs = segs[:]
    return out, true
}

looks_like_record_literal :: proc(p: ^Parser) -> bool {
    if peek_tok(p, 0).kind != .Left_Brace do return false
    a := peek_tok(p, 1).kind
    b := peek_tok(p, 2).kind
    if a == .Dot_Dot do return true
    if a == .Ident && b == .Colon do return true
    if a == .Right_Brace do return true
    return false
}

finish_record_literal :: proc(p: ^Parser, span: Span, path: ^Expr_Path) -> (Expr, bool) {
    if !eat(p, .Left_Brace, "`{`") do return nil, false
    fields := make([dynamic]Record_Field)
    var_base: Expr = nil
    for !at(p, .Right_Brace) && !at(p, .EOF) {
        if accept(p, .Dot_Dot) {
            b, ok := parse_expr(p); if !ok do return nil, false
            var_base = b
            break
        }
        n, ok := expect(p, .Ident, "field name")
        if !ok do return nil, false
        if !eat(p, .Colon, "`:`") do return nil, false
        v, vok := parse_expr(p); if !vok do return nil, false
        append(&fields, Record_Field{name = n.source, value = v})
        if !accept(p, .Comma) do break
    }
    if !eat(p, .Right_Brace, "`}`") do return nil, false
    tn := new(Type_Named); tn.span = path.span; tn.path = path.segs; tn.args = nil
    te := new(Type_Expr); te^ = tn
    out := new(Expr_Record)
    out.span = span; out.type = te; out.fields = fields[:]; out.base = var_base
    return out, true
}

parse_if :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .KW_If, "`if`") do return nil, false
    // `if let PAT = EXPR { ... } else { ... }` desugars to a match with the
    // user's pattern as the first arm and a wildcard as the second.
    if accept(p, .KW_Let) {
        pat, pok := parse_pattern(p); if !pok do return nil, false
        if !eat(p, .Eq, "`=` after if-let pattern") do return nil, false
        scrut, sok := parse_expr(p); if !sok do return nil, false
        then_b, tok := parse_block(p); if !tok do return nil, false
        else_b: Expr = nil
        if accept(p, .KW_Else) {
            if at(p, .KW_If) {
                e, eok := parse_if(p); if !eok do return nil, false
                else_b = e
            } else {
                b, bok := parse_block(p); if !bok do return nil, false
                else_b = b
            }
        }
        if else_b == nil {
            else_blk := new(Expr_Block)
            else_blk.span = span
            else_blk.stmts = {}
            else_blk.tail = nil
            else_b = else_blk
        }
        wild := new(Pat_Wild); wild.span = span
        arms := make([dynamic]Match_Arm)
        append(&arms, Match_Arm{pat = pat, guard = nil, body = then_b})
        append(&arms, Match_Arm{pat = wild,  guard = nil, body = else_b})
        out := new(Expr_Match); out.span = span; out.scrutinee = scrut; out.arms = arms[:]
        return out, true
    }
    cond, ok := parse_expr(p); if !ok do return nil, false
    then_b, tok := parse_block(p); if !tok do return nil, false
    else_b: Expr = nil
    if accept(p, .KW_Else) {
        if at(p, .KW_If) {
            e, eok := parse_if(p); if !eok do return nil, false
            else_b = e
        } else {
            b, bok := parse_block(p); if !bok do return nil, false
            else_b = b
        }
    }
    out := new(Expr_If); out.span = span; out.cond = cond; out.then_b = then_b; out.else_b = else_b
    return out, true
}

parse_while :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .KW_While, "`while`") do return nil, false
    cond, ok := parse_expr(p); if !ok do return nil, false
    body, bok := parse_block(p); if !bok do return nil, false
    out := new(Expr_While); out.span = span; out.cond = cond; out.body = body
    return out, true
}

parse_for :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .KW_For, "`for`") do return nil, false
    n, ok := expect(p, .Ident, "binding name"); if !ok do return nil, false
    binding2 := ""
    if accept(p, .Comma) {
        n2, ok2 := expect(p, .Ident, "second binding name"); if !ok2 do return nil, false
        binding2 = n2.source
    }
    if !eat(p, .KW_In, "`in`") do return nil, false
    iter, iok := parse_expr(p); if !iok do return nil, false
    body, bok := parse_block(p); if !bok do return nil, false
    out := new(Expr_For); out.span = span; out.binding = n.source; out.binding2 = binding2; out.iter = iter; out.body = body
    return out, true
}

parse_match :: proc(p: ^Parser) -> (Expr, bool) {
    span := current_span(p)
    if !eat(p, .KW_Match, "`match`") do return nil, false
    scrutinee, ok := parse_expr(p); if !ok do return nil, false
    if !eat(p, .Left_Brace, "`{`") do return nil, false
    arms := make([dynamic]Match_Arm)
    for accept(p, .Semicolon) { }
    for at(p, .Pipe) {
        p.pos += 1                                                  // consume `|`
        pats := make([dynamic]Pattern)
        first_pat, pok := parse_pattern(p); if !pok do return nil, false
        append(&pats, first_pat)
        for at(p, .Pipe) {
            p.pos += 1
            np, npok := parse_pattern(p); if !npok do return nil, false
            append(&pats, np)
        }
        guard: Expr = nil
        if accept(p, .KW_If) {
            g, gok := parse_expr(p); if !gok do return nil, false
            guard = g
        }
        if !eat(p, .Arrow, "`->` in match arm") do return nil, false
        body, bok := parse_expr(p); if !bok do return nil, false
        for pat in pats {
            append(&arms, Match_Arm{pat = pat, guard = guard, body = body})
        }
        for accept(p, .Semicolon) { }
    }
    if !eat(p, .Right_Brace, "`}` to close match") do return nil, false
    out := new(Expr_Match); out.span = span; out.scrutinee = scrutinee; out.arms = arms[:]
    return out, true
}

// --- Patterns ---

parse_pattern :: proc(p: ^Parser) -> (Pattern, bool) {
    span := current_span(p)
    tok := peek_tok(p)
    #partial switch tok.kind {
    case .Ident:
        if tok.source == "_" {
            p.pos += 1
            w := new(Pat_Wild); w.span = span
            return w, true
        }
        if is_variant_pattern_start(p) {
            return parse_variant_pattern(p)
        }
        p.pos += 1
        b := new(Pat_Bind); b.span = span; b.name = tok.source
        return b, true
    case .Lit_Int:
        p.pos += 1
        l := new(Pat_Lit_Int); l.span = span; l.text = tok.source
        return l, true
    case .Lit_String:
        p.pos += 1
        l := new(Pat_Lit_String); l.span = span; l.text = tok.source
        return l, true
    case .Lit_True:
        p.pos += 1
        l := new(Pat_Lit_Bool); l.span = span; l.value = true
        return l, true
    case .Lit_False:
        p.pos += 1
        l := new(Pat_Lit_Bool); l.span = span; l.value = false
        return l, true
    case .Left_Paren:
        p.pos += 1
        elems := make([dynamic]Pattern)
        for !at(p, .Right_Paren) && !at(p, .EOF) {
            e, ok := parse_pattern(p); if !ok do return nil, false
            append(&elems, e)
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Paren, "`)`") do return nil, false
        t := new(Pat_Tuple); t.span = span; t.elems = elems[:]
        return t, true
    }
    error_here(p, fmt.tprintf("expected pattern, got %v `%s`", tok.kind, tok.source))
    return nil, false
}

// A variant pattern starts with an identifier path (qualified or bare),
// where any leading uppercase identifier signals a variant constructor.
is_variant_pattern_start :: proc(p: ^Parser) -> bool {
    if !at(p, .Ident) do return false
    if peek_tok(p, 1).kind == .Dot do return true
    first_src := peek_tok(p).source
    if is_upper_first(first_src) do return true
    return false
}

parse_variant_pattern :: proc(p: ^Parser) -> (Pattern, bool) {
    span := current_span(p)
    segs := make([dynamic]string)
    first := p.tokens[p.pos]; p.pos += 1
    append(&segs, first.source)
    for accept(p, .Dot) {
        n, ok := expect(p, .Ident, "variant name")
        if !ok do return nil, false
        append(&segs, n.source)
    }
    path := new(Expr_Path); path.span = span; path.segs = segs[:]
    v := new(Pat_Variant); v.span = span; v.path = path
    if accept(p, .Left_Paren) {
        pos := make([dynamic]Pattern)
        for !at(p, .Right_Paren) && !at(p, .EOF) {
            pp, ok := parse_pattern(p); if !ok do return nil, false
            append(&pos, pp)
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Paren, "`)`") do return nil, false
        v.pos = pos[:]
    } else if accept(p, .Left_Brace) {
        named := make([dynamic]Pat_Named_Field)
        for !at(p, .Right_Brace) && !at(p, .EOF) {
            n, nok := expect(p, .Ident, "field name"); if !nok do return nil, false
            if !eat(p, .Colon, "`:`") do return nil, false
            sub, ok := parse_pattern(p); if !ok do return nil, false
            append(&named, Pat_Named_Field{name = n.source, pat = sub})
            if !accept(p, .Comma) do break
        }
        if !eat(p, .Right_Brace, "`}`") do return nil, false
        v.named = named[:]
    }
    return v, true
}

// --- Small helpers ---

is_upper_first :: proc(s: string) -> bool {
    if len(s) == 0 do return false
    c := s[0]
    return c >= 'A' && c <= 'Z'
}

unquote_string :: proc(s: string) -> string {
    if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
        return s[1:len(s)-1]
    }
    return s
}

expr_span :: proc(e: Expr) -> Span {
    switch v in e {
    case ^Expr_Int_Lit:    return v.span
    case ^Expr_Float_Lit:  return v.span
    case ^Expr_String_Lit: return v.span
    case ^Expr_Char_Lit:   return v.span
    case ^Expr_Bool_Lit:   return v.span
    case ^Expr_Nil_Lit:    return v.span
    case ^Expr_Ident:      return v.span
    case ^Expr_Path:       return v.span
    case ^Expr_Unary:      return v.span
    case ^Expr_Binary:     return v.span
    case ^Expr_Assign:     return v.span
    case ^Expr_Call:       return v.span
    case ^Expr_Field:      return v.span
    case ^Expr_Index:      return v.span
    case ^Expr_Cast:       return v.span
    case ^Expr_New:        return v.span
    case ^Expr_Try:        return v.span
    case ^Expr_Tuple:      return v.span
    case ^Expr_Record:     return v.span
    case ^Expr_Closure:    return v.span
    case ^Expr_Block:      return v.span
    case ^Expr_If:         return v.span
    case ^Expr_Match:      return v.span
    case ^Expr_While:      return v.span
    case ^Expr_For:        return v.span
    case ^Expr_Return:     return v.span
    case ^Expr_Defer:      return v.span
    case ^Expr_Size_Of:    return v.span
    case ^Expr_Array_Lit:  return v.span
    }
    return Span{}
}
