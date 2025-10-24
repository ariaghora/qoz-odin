#+feature dynamic-literals

package main

import "core:fmt"
import "core:strings"
import "core:unicode"

Token_Kind :: enum {
	Assign, Colon, Comma,
	Plus, Minus, Star, Slash,
	Left_Paren, Right_Paren,
	Left_Brace, Right_Brace,
    Lit_Number, Lit_String,
	KW_Fn, KW_If, KW_Print, KW_Return,
	KW_I32, KW_I64, KW_F32, KW_F64,
    Iden,
    EOF,
}

Tokenize_Error :: union {
    string
}

Token :: struct {
	source: string,
	kind:   Token_Kind,
}

make_id_or_kw :: proc(tokens: ^[dynamic]Token, offset: ^int, source: string) {
	c := rune(source[offset^])
	start, strlen := offset^, 0
	for unicode.is_alpha(c) || unicode.is_digit(c) || c == '_' && offset^ < len(source) {
		strlen += 1
		offset^ += 1
		c = rune(source[offset^])
	}
    tok_src := source[start:start+strlen]
    tok_kind: Token_Kind
    switch tok_src {
    case "func"  : tok_kind = .KW_Fn
    case "if"    : tok_kind = .KW_If
    case "print" : tok_kind = .KW_Print
    case "return": tok_kind = .KW_Return
    case "i32"   : tok_kind = .KW_I32
    case "i64"   : tok_kind = .KW_I64
    case "f32"   : tok_kind = .KW_F32
    case "f64"   : tok_kind = .KW_F64
    // default to identifier
    case         : tok_kind = .Iden
    }

	append(tokens, Token{tok_src, tok_kind})
}

make_number :: proc(tokens: ^[dynamic]Token, offset: ^int, source: string) {
	c := rune(source[offset^])
	start, strlen := offset^, 0
	for unicode.is_number(c) && offset^ < len(source) {
		strlen += 1
		offset^ += 1
		c = rune(source[offset^])
	}
	append(tokens, Token{source[start:start+strlen], .Lit_Number})
}

make_tok :: proc(tokens: ^[dynamic]Token, tok: Token, offset: ^int, source_len: int) {
	append(tokens, tok)
	offset^ += source_len
}

tokenize :: proc(source: string, allocator := context.allocator) -> (tokens: [dynamic]Token, err: Tokenize_Error) {
	i := 0
	for i < len(source) {
		c := rune(source[i])
		for strings.is_space(c) && i < len(source) - 1 {
			i += 1; c = rune(source[i])
		}

		switch c {
		case ',': make_tok(&tokens, Token{",", .Comma}, &i, 1) 
		case '=': make_tok(&tokens, Token{"=", .Assign}, &i, 1) 
		case ':': make_tok(&tokens, Token{":", .Colon}, &i, 1) 
		case '(': make_tok(&tokens, Token{"(", .Left_Paren}, &i, 1)
		case ')': make_tok(&tokens, Token{")", .Right_Paren}, &i, 1)
		case '{': make_tok(&tokens, Token{"{", .Left_Brace}, &i, 1)
		case '}': make_tok(&tokens, Token{"}", .Right_Brace}, &i, 1)
		case '+': make_tok(&tokens, Token{"+", .Plus}, &i, 1)
		case '-': make_tok(&tokens, Token{"-", .Minus}, &i, 1)
		case '*': make_tok(&tokens, Token{"*", .Star}, &i, 1)
		case '/': make_tok(&tokens, Token{"/", .Slash}, &i, 1)
		case:
			if false {}
            else if unicode.is_alpha(c) { make_id_or_kw(&tokens, &i, source) }
			else if unicode.is_number(c) { make_number(&tokens, &i, source) }
            else { return nil, fmt.tprintf("Unknown symbol `%v`", rune(c)) }
		}

        if i < len(source) do c = rune(source[i])
	}
	append(&tokens, Token{"", .EOF})

    return tokens, nil
}