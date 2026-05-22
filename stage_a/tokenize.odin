package main

import "core:fmt"
import "core:strings"
import "core:unicode"

Token_Kind :: enum {
    // Punctuation
    Left_Paren, Right_Paren,
    Left_Brace, Right_Brace,
    Left_Bracket, Right_Bracket,
    Comma, Semicolon, Colon, Dot, Hash, At_Sign,

    // Operators
    Eq,            // =
    Eq_Eq, Not_Eq, // == !=
    Lt, Gt,        // < >
    Lt_Eq, Gt_Eq,  // <= >=
    Plus, Minus, Star, Slash, Percent,
    Plus_Eq, Minus_Eq, Star_Eq, Slash_Eq, Percent_Eq,
    Amp, Pipe, Bang,           // & | !
    And_And, Or_Or,            // && ||
    Arrow,                     // ->
    Dot_Dot,                   // ..   (record spread only)
    Range_Excl,                // ..<  (half-open range)
    Range_Incl,                // ..=  (closed range)
    Question,                  // ?

    // Literals
    Lit_Int, Lit_Float, Lit_String, Lit_Char,
    Lit_True, Lit_False, Lit_Nil,

    // Keywords
    KW_Let, KW_Var,
    KW_Return,
    KW_Type,
    KW_If, KW_Else, KW_Match,
    KW_While, KW_For, KW_In,
    KW_Defer, KW_As, KW_New,
    KW_Import, KW_External,

    // Primitive type keywords
    KW_I8, KW_I16, KW_I32, KW_I64,
    KW_U8, KW_U16, KW_U32, KW_U64,
    KW_F32, KW_F64,
    KW_Bool, KW_Char, KW_String, KW_Cstring, KW_Unit, KW_Void,

    // Identifiers and end of input
    Ident,
    EOF,
}

Token :: struct {
    kind:   Token_Kind,
    source: string,
    line:   int,
    column: int,
}

Tokenize_Error :: struct {
    file:    string,
    line:    int,
    column:  int,
    message: string,
}

Tokenizer :: struct {
    file:   string,
    source: string,
    offset: int,
    line:   int,
    column: int,
    tokens: [dynamic]Token,
}

tokenize :: proc(file: string, source: string, allocator := context.allocator) -> (tokens: [dynamic]Token, err: Maybe(Tokenize_Error)) {
    t := Tokenizer{
        file   = file,
        source = source,
        offset = 0,
        line   = 1,
        column = 1,
        tokens = make([dynamic]Token, allocator),
    }

    for t.offset < len(t.source) {
        skip_trivia(&t) or_return
        if t.offset >= len(t.source) do break

        c := current_char(&t)

        switch c {
        case '(': emit_one(&t, .Left_Paren,    "(")
        case ')': emit_one(&t, .Right_Paren,   ")")
        case '{': emit_one(&t, .Left_Brace,    "{")
        case '}': emit_one(&t, .Right_Brace,   "}")
        case '[': emit_one(&t, .Left_Bracket,  "[")
        case ']': emit_one(&t, .Right_Bracket, "]")
        case ',': emit_one(&t, .Comma,         ",")
        case ';': emit_one(&t, .Semicolon,     ";")
        case '#': emit_one(&t, .Hash,          "#")
        case '@': emit_one(&t, .At_Sign,       "@")
        case '?': emit_one(&t, .Question,      "?")
        case '%':
            if peek(&t, 1) == '=' { emit(&t, .Percent_Eq, 2) }
            else                  { emit(&t, .Percent, 1) }
        case '+':
            if peek(&t, 1) == '=' { emit(&t, .Plus_Eq, 2) }
            else                  { emit(&t, .Plus, 1) }
        case '-':
            if peek(&t, 1) == '=' { emit(&t, .Minus_Eq, 2) }
            else if peek(&t, 1) == '>' { emit(&t, .Arrow, 2) }
            else                  { emit(&t, .Minus, 1) }
        case '*':
            if peek(&t, 1) == '=' { emit(&t, .Star_Eq, 2) }
            else                  { emit(&t, .Star, 1) }
        case '/':
            if peek(&t, 1) == '=' { emit(&t, .Slash_Eq, 2) }
            else                  { emit(&t, .Slash, 1) }
        case '=':
            if peek(&t, 1) == '=' { emit(&t, .Eq_Eq, 2) }
            else                  { emit(&t, .Eq, 1) }
        case '!':
            if peek(&t, 1) == '=' { emit(&t, .Not_Eq, 2) }
            else                  { emit(&t, .Bang, 1) }
        case '<':
            if peek(&t, 1) == '=' { emit(&t, .Lt_Eq, 2) }
            else                  { emit(&t, .Lt, 1) }
        case '>':
            if peek(&t, 1) == '=' { emit(&t, .Gt_Eq, 2) }
            else                  { emit(&t, .Gt, 1) }
        case '&':
            if peek(&t, 1) == '&' { emit(&t, .And_And, 2) }
            else                  { emit(&t, .Amp, 1) }
        case '|':
            if peek(&t, 1) == '|' { emit(&t, .Or_Or, 2) }
            else                  { emit(&t, .Pipe, 1) }
        case ':':
            emit(&t, .Colon, 1)
        case '.':
            if peek(&t, 1) == '.' && peek(&t, 2) == '=' { emit(&t, .Range_Incl, 3) }
            else if peek(&t, 1) == '.' && peek(&t, 2) == '<' { emit(&t, .Range_Excl, 3) }
            else if peek(&t, 1) == '.'                  { emit(&t, .Dot_Dot, 2) }
            else                                        { emit(&t, .Dot, 1) }
        case '"':
            read_string(&t) or_return
        case '\'':
            read_char(&t) or_return
        case:
            if unicode.is_alpha(c) || c == '_' {
                read_ident_or_keyword(&t)
            } else if unicode.is_number(c) {
                read_number(&t) or_return
            } else {
                return nil, Tokenize_Error{
                    file = t.file, line = t.line, column = t.column,
                    message = fmt.tprintf("unexpected character `%v`", c),
                }
            }
        }
    }

    append(&t.tokens, Token{kind = .EOF, source = "", line = t.line, column = t.column})
    // Stage A keeps explicit-terminator semantics. ASI lives in Stage B
    // (compiler/tokenize/tokenize.qoz). compiler/main.qoz uses explicit
    // structure where needed, so Stage A does not run the post-pass.
    return t.tokens, nil
}

// Post-process pass: walk the token stream and inject synthetic
// semicolons at line breaks where the surrounding tokens permit it.
// Insertion happens when the previous token can end a statement, the
// next token is on a later line, the next token is not a continuation
// (operator, comma, closing bracket, `else`, `as`, ...), and the
// position is not inside `(` or `[` brackets.
insert_asi_semicolons :: proc(input: [dynamic]Token, allocator := context.allocator) -> [dynamic]Token {
    out := make([dynamic]Token, allocator)
    paren_depth := 0
    for tok, i in input {
        if i > 0 && paren_depth <= 0 {
            prev := input[i - 1]
            if tok.line > prev.line && is_stmt_ender(prev) && !is_line_continuation(tok.kind) {
                append(&out, Token{
                    kind   = .Semicolon,
                    source = ";",
                    line   = prev.line,
                    column = prev.column,
                })
            }
        }
        append(&out, tok)
        #partial switch tok.kind {
        case .Left_Paren, .Left_Bracket:  paren_depth += 1
        case .Right_Paren, .Right_Bracket: paren_depth -= 1
        }
    }
    return out
}

current_char :: proc(t: ^Tokenizer) -> rune {
    if t.offset >= len(t.source) do return 0
    return rune(t.source[t.offset])
}

peek :: proc(t: ^Tokenizer, n: int) -> rune {
    pos := t.offset + n
    if pos >= len(t.source) do return 0
    return rune(t.source[pos])
}

advance :: proc(t: ^Tokenizer) {
    if t.offset >= len(t.source) do return
    if t.source[t.offset] == '\n' {
        t.line += 1
        t.column = 1
    } else {
        t.column += 1
    }
    t.offset += 1
}

emit_one :: proc(t: ^Tokenizer, kind: Token_Kind, lex: string) {
    emit(t, kind, len(lex))
}

emit :: proc(t: ^Tokenizer, kind: Token_Kind, width: int) {
    start_line := t.line
    start_col  := t.column
    start_off  := t.offset
    for _ in 0..<width { advance(t) }
    append(&t.tokens, Token{
        kind   = kind,
        source = t.source[start_off:t.offset],
        line   = start_line,
        column = start_col,
    })
}

is_stmt_ender :: proc(tok: Token) -> bool {
    #partial switch tok.kind {
    case .Ident, .Lit_Int, .Lit_Float, .Lit_String, .Lit_Char,
         .Lit_True, .Lit_False, .Lit_Nil,
         .Right_Paren, .Right_Bracket, .Right_Brace,
         .KW_Return:
        return true
    }
    return false
}

// Returns true when the next token is a continuation token that consumes
// the previous expression (binary operators, postfix operators, closing
// brackets, alternation `|`, the `else`/`as` keywords, etc.). When this
// holds, do not synthesise a semicolon at the line break: the source
// clearly intends to keep one expression flowing across lines.
is_line_continuation :: proc(kind: Token_Kind) -> bool {
    #partial switch kind {
    case .Pipe, .Or_Or, .And_And, .Amp,
         .Plus, .Minus, .Star, .Slash, .Percent,
         .Eq, .Eq_Eq, .Not_Eq, .Lt, .Gt, .Lt_Eq, .Gt_Eq,
         .Plus_Eq, .Minus_Eq, .Star_Eq, .Slash_Eq, .Percent_Eq,
         .Arrow, .Dot, .Comma, .Colon, .Question,
         .Right_Paren, .Right_Bracket, .Right_Brace,
         .Dot_Dot, .Range_Excl, .Range_Incl,
         .KW_Else, .KW_As, .KW_In:
        return true
    }
    return false
}

skip_trivia :: proc(t: ^Tokenizer) -> Maybe(Tokenize_Error) {
    for t.offset < len(t.source) {
        c := current_char(t)
        if strings.is_space(c) {
            advance(t)
        } else if c == '/' && peek(t, 1) == '/' {
            for t.offset < len(t.source) && current_char(t) != '\n' { advance(t) }
        } else if c == '/' && peek(t, 1) == '*' {
            skip_block_comment(t) or_return
        } else {
            break
        }
    }
    return nil
}

skip_block_comment :: proc(t: ^Tokenizer) -> Maybe(Tokenize_Error) {
    start_line := t.line
    start_col  := t.column
    advance(t); advance(t)                     // consume /*
    depth := 1
    for depth > 0 {
        if t.offset >= len(t.source) {
            return Tokenize_Error{
                file = t.file, line = start_line, column = start_col,
                message = "unterminated block comment",
            }
        }
        c := current_char(t)
        if c == '/' && peek(t, 1) == '*' {
            advance(t); advance(t)
            depth += 1
        } else if c == '*' && peek(t, 1) == '/' {
            advance(t); advance(t)
            depth -= 1
        } else {
            advance(t)
        }
    }
    return nil
}

read_ident_or_keyword :: proc(t: ^Tokenizer) {
    start_off  := t.offset
    start_line := t.line
    start_col  := t.column

    for {
        c := current_char(t)
        if unicode.is_alpha(c) || unicode.is_number(c) || c == '_' {
            advance(t)
        } else {
            break
        }
    }

    src := t.source[start_off:t.offset]
    kind := keyword_kind(src)
    append(&t.tokens, Token{
        kind   = kind,
        source = src,
        line   = start_line,
        column = start_col,
    })
}

keyword_kind :: proc(src: string) -> Token_Kind {
    switch src {
    case "let":      return .KW_Let
    case "var":      return .KW_Var
    case "return":   return .KW_Return
    case "type":     return .KW_Type
    case "if":       return .KW_If
    case "else":     return .KW_Else
    case "match":    return .KW_Match
    case "while":    return .KW_While
    case "for":      return .KW_For
    case "in":       return .KW_In
    case "defer":    return .KW_Defer
    case "as":       return .KW_As
    case "new":      return .KW_New
    case "import":   return .KW_Import
    case "external": return .KW_External
    case "true":     return .Lit_True
    case "false":    return .Lit_False
    case "nil":      return .Lit_Nil
    case "i8":       return .KW_I8
    case "i16":      return .KW_I16
    case "i32":      return .KW_I32
    case "i64":      return .KW_I64
    case "u8":       return .KW_U8
    case "u16":      return .KW_U16
    case "u32":      return .KW_U32
    case "u64":      return .KW_U64
    case "f32":      return .KW_F32
    case "f64":      return .KW_F64
    case "bool":     return .KW_Bool
    case "char":     return .KW_Char
    case "string":   return .KW_String
    case "cstring":  return .KW_Cstring
    case "unit":     return .KW_Unit
    case "void":     return .KW_Void
    }
    return .Ident
}

read_number :: proc(t: ^Tokenizer) -> Maybe(Tokenize_Error) {
    start_off  := t.offset
    start_line := t.line
    start_col  := t.column

    // 0x or 0b prefix
    if current_char(t) == '0' && (peek(t, 1) == 'x' || peek(t, 1) == 'b') {
        advance(t); advance(t)
        for {
            c := current_char(t)
            if unicode.is_alpha(c) || unicode.is_number(c) || c == '_' { advance(t) }
            else { break }
        }
        append(&t.tokens, Token{
            kind = .Lit_Int, source = t.source[start_off:t.offset],
            line = start_line, column = start_col,
        })
        return nil
    }

    is_float := false
    for unicode.is_number(current_char(t)) || current_char(t) == '_' { advance(t) }

    if current_char(t) == '.' && unicode.is_number(peek(t, 1)) {
        is_float = true
        advance(t)
        for unicode.is_number(current_char(t)) || current_char(t) == '_' { advance(t) }
    }

    if current_char(t) == 'e' || current_char(t) == 'E' {
        is_float = true
        advance(t)
        if current_char(t) == '+' || current_char(t) == '-' { advance(t) }
        if !unicode.is_number(current_char(t)) {
            return Tokenize_Error{
                file = t.file, line = t.line, column = t.column,
                message = "exponent missing digits",
            }
        }
        for unicode.is_number(current_char(t)) { advance(t) }
    }

    kind: Token_Kind = .Lit_Int
    if is_float do kind = .Lit_Float
    append(&t.tokens, Token{
        kind = kind, source = t.source[start_off:t.offset],
        line = start_line, column = start_col,
    })
    return nil
}

read_string :: proc(t: ^Tokenizer) -> Maybe(Tokenize_Error) {
    start_off  := t.offset
    start_line := t.line
    start_col  := t.column

    advance(t)                                  // opening "
    for t.offset < len(t.source) && current_char(t) != '"' {
        if current_char(t) == '\\' {
            advance(t)
            if t.offset >= len(t.source) { break }
            advance(t)
        } else {
            advance(t)
        }
    }
    if t.offset >= len(t.source) {
        return Tokenize_Error{
            file = t.file, line = start_line, column = start_col,
            message = "unterminated string literal",
        }
    }
    advance(t)                                  // closing "
    append(&t.tokens, Token{
        kind = .Lit_String, source = t.source[start_off:t.offset],
        line = start_line, column = start_col,
    })
    return nil
}

read_char :: proc(t: ^Tokenizer) -> Maybe(Tokenize_Error) {
    start_off  := t.offset
    start_line := t.line
    start_col  := t.column

    advance(t)                                  // opening '
    if t.offset >= len(t.source) {
        return Tokenize_Error{
            file = t.file, line = start_line, column = start_col,
            message = "unterminated char literal",
        }
    }
    if current_char(t) == '\\' { advance(t) }
    advance(t)                                  // the character itself
    if t.offset >= len(t.source) || current_char(t) != '\'' {
        return Tokenize_Error{
            file = t.file, line = start_line, column = start_col,
            message = "char literal must contain exactly one character",
        }
    }
    advance(t)                                  // closing '
    append(&t.tokens, Token{
        kind = .Lit_Char, source = t.source[start_off:t.offset],
        line = start_line, column = start_col,
    })
    return nil
}
