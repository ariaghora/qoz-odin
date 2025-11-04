package main

import "core:fmt"
import "core:strings"
import "core:unicode"

Token_Kind :: enum {
    Eq, Eq_Eq, Not_Eq, Assign, Colon, Semicolon, Comma, Dot,
    Lt, Gt, Lt_Eq, Gt_Eq, Amp, 
    Plus, Minus, Star, Slash, Percent,
    Left_Paren, Right_Paren,
    Left_Brace, Right_Brace,
    Left_Bracket, Right_Bracket,
    Lit_Number, Lit_String, Lit_Nil,
    KW_Fn, KW_External, KW_If, KW_Else, KW_Print, KW_Return, KW_As,
    KW_I8, KW_U8, KW_I32, KW_I64, KW_F32, KW_F64, KW_Void, 
    KW_Arr, KW_Map, KW_For, KW_In, KW_Struct,
    KW_Size_Of, KW_Len, KW_Del,
    KW_Import, Iden, EOF,
}

Tokenize_Error :: union {
    string
}

Token :: struct {
    source: string,
    kind: Token_Kind,
    line: int,
    column: int,
}

Tokenizer :: struct {
    source: string,
    offset: int,
    line: int,
    column: int,
    tokens: [dynamic]Token,
}

current_char :: proc(t: ^Tokenizer) -> rune {
    if t.offset >= len(t.source) do return 0
    return rune(t.source[t.offset])
}

tokenizer_advance :: proc(t: ^Tokenizer) {
    if t.offset < len(t.source) {
        if t.source[t.offset] == '\n' {
            t.line += 1
            t.column = 1
        } else {
            t.column += 1
        }
        t.offset += 1
    }
}

skip_whitespace :: proc(t: ^Tokenizer) {
    for strings.is_space(current_char(t)) {
        tokenizer_advance(t)
    }
}

skip_comment :: proc(t: ^Tokenizer) {
    // Skip the two slashes
    tokenizer_advance(t)
    tokenizer_advance(t)
    
    // Skip until end of line or end of file
    for current_char(t) != '\n' && t.offset < len(t.source) {
        tokenizer_advance(t)
    }
}

make_tok :: proc(t: ^Tokenizer, kind: Token_Kind, source: string) {
    append(&t.tokens, Token{
        source = source,
        kind = kind,
        line = t.line,
        column = t.column,
    })
    for _ in 0..<len(source) {
        tokenizer_advance(t)
    }
}

peek :: proc(t: ^Tokenizer, source: string) -> Maybe(rune) {
    if t.offset > len(source)-1 do return nil
    return rune(source[t.offset+1])
}

make_id_or_kw :: proc(t: ^Tokenizer) {
    start := t.offset
    start_line := t.line
    start_column := t.column
    
    c := current_char(t)
    for unicode.is_alpha(c) || unicode.is_digit(c) || c == '_' {
        tokenizer_advance(t)
        c = current_char(t)
    }
    
    tok_src := t.source[start:t.offset]
    tok_kind: Token_Kind
    
    switch tok_src {
    case "import":   tok_kind = .KW_Import
    case "as":       tok_kind = .KW_As
    case "func":     tok_kind = .KW_Fn
    case "external": tok_kind = .KW_External
    case "del":      tok_kind = .KW_Del
    case "for":      tok_kind = .KW_For
    case "in":       tok_kind = .KW_In
    case "if":       tok_kind = .KW_If
    case "else":     tok_kind = .KW_Else
    case "print":    tok_kind = .KW_Print
    case "return":   tok_kind = .KW_Return
    case "nil":      tok_kind = .Lit_Nil
    case "i8":       tok_kind = .KW_I8
    case "u8":       tok_kind = .KW_U8
    case "i32":      tok_kind = .KW_I32
    case "i64":      tok_kind = .KW_I64
    case "f32":      tok_kind = .KW_F32
    case "f64":      tok_kind = .KW_F64
    case "void":     tok_kind = .KW_Void
    case "arr":      tok_kind = .KW_Arr
    case "struct":   tok_kind = .KW_Struct
    case "size_of":  tok_kind = .KW_Size_Of
    case:            tok_kind = .Iden
    }
    
    append(&t.tokens, Token{
        source = tok_src,
        kind = tok_kind,
        line = start_line,
        column = start_column,
    })
}

make_number :: proc(t: ^Tokenizer) {
    start := t.offset
    start_line := t.line
    start_column := t.column
    
    c := current_char(t)
    for unicode.is_number(c) {
        tokenizer_advance(t)
        c = current_char(t)
    }
    
    append(&t.tokens, Token{
        source = t.source[start:t.offset],
        kind = .Lit_Number,
        line = start_line,
        column = start_column,
    })
}

make_string :: proc(t: ^Tokenizer) {
    start := t.offset
    start_line := t.line
    start_column := t.column
    delimiter := current_char(t)
    tokenizer_advance(t)
    c := current_char(t)
    for c != delimiter && t.offset < len(t.source)-1{
        tokenizer_advance(t)
        c = current_char(t)
    }
    if t.offset == len(t.source)-1 do panic("Unexpected EOF, string not closed?")

    tokenizer_advance(t)
    append(&t.tokens, Token{
        source = t.source[start:t.offset],
        kind = .Lit_String,
        line = start_line,
        column = start_column,
    })
}

tokenize :: proc(source: string, allocator := context.allocator) -> (tokens: [dynamic]Token, err: Tokenize_Error) {
    t := Tokenizer {
        source = source,
        offset = 0,
        line = 1,
        column = 1,
        tokens = make([dynamic]Token, allocator),
    }
    
    for t.offset < len(t.source) {
        skip_whitespace(&t)
        if t.offset >= len(t.source) do break
        
        c := current_char(&t)
        
        switch c {
        case ',': make_tok(&t, .Comma, ",")
        case '.': make_tok(&t, .Dot, ".")
        case '(': make_tok(&t, .Left_Paren, "(")
        case ')': make_tok(&t, .Right_Paren, ")")
        case '{': make_tok(&t, .Left_Brace, "{")
        case '}': make_tok(&t, .Right_Brace, "}")
        case '[': make_tok(&t, .Left_Bracket, "[")
        case ']': make_tok(&t, .Right_Bracket, "]")
        case '+': make_tok(&t, .Plus, "+")
        case '-': make_tok(&t, .Minus, "-")
        case '*': make_tok(&t, .Star, "*")
        case '/':
            if peek(&t, source) == '/' {
                skip_comment(&t)
            } else {
                make_tok(&t, .Slash, "/")
            }
        case '%': make_tok(&t, .Percent, "%")
        case '&': make_tok(&t, .Amp, "&")
        case ';': make_tok(&t, .Semicolon, ";")
        case '"', '\'': make_string(&t)
        case ':': 
            if peek(&t, source) == '=' {
                make_tok(&t, .Assign, ":=")
            } else {
                make_tok(&t, .Colon, ":")
            }
        case '>': 
            if peek(&t, source) == '=' {
                make_tok(&t, .Gt_Eq, ">=")
            } else {
                make_tok(&t, .Gt, ">")
            }
        case '<': 
            if peek(&t, source) == '=' {
                make_tok(&t, .Lt_Eq, "<=")
            } else {
                make_tok(&t, .Lt, "<")
            }
        case '=': 
            if peek(&t, source) == '=' {
                make_tok(&t, .Eq_Eq, "==")
            } else {
                make_tok(&t, .Eq, "=")
            }
        case:
            if unicode.is_alpha(c) {
                make_id_or_kw(&t)
            } else if unicode.is_number(c) {
                make_number(&t)
            } else {
                return nil, fmt.tprintf("Unknown symbol `%v` at line %d, column %d", c, t.line, t.column)
            }
        }
    }
    
    append(&t.tokens, Token{
        source = "",
        kind = .EOF,
        line = t.line,
        column = t.column,
    })
    
    return t.tokens, nil
}