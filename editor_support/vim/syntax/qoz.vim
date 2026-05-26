" Vim syntax file for the Qoz programming language.
"
" To enable: copy or symlink this directory into ~/.vim/, or point a
" plugin manager at editor-support/vim. Vim picks up ftdetect/qoz.vim
" automatically and switches to filetype qoz for *.qoz files.

if exists("b:current_syntax")
    finish
endif

" -- Keywords --------------------------------------------------------
syntax keyword qozStatement    let var return defer
syntax keyword qozConditional  if elif else match
syntax keyword qozRepeat       while for in
syntax keyword qozImport       import external as new type
syntax keyword qozBoolean      true false
syntax keyword qozConstant     nil

" -- Compile-time directives ----------------------------------------
syntax match qozDirective "#\(link_library\|link_framework\|link_path\|load_string\|if\|elif\|else\)\>"

" -- Primitive types -------------------------------------------------
syntax keyword qozType i8 i16 i32 i64 u8 u16 u32 u64 f32 f64
syntax keyword qozType bool char string cstring void unit

" -- Numbers ---------------------------------------------------------
syntax match qozNumber "\<\d\(\d\|_\)*\(\.\d\(\d\|_\)*\)\=\([eE][+-]\=\d\+\)\=\>"
syntax match qozNumber "\<0x[0-9A-Fa-f_]\+\>"
syntax match qozNumber "\<0b[01_]\+\>"
syntax match qozNumber "\<0o[0-7_]\+\>"

" -- Strings ---------------------------------------------------------
syntax region qozString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=qozEscape,@Spell
syntax region qozString start=+`+ end=+`+ contains=qozEscape,qozInterp,qozBraceEscape,@Spell
syntax match  qozEscape "\\[nrtbf0\\\"'`]" contained
syntax match  qozEscape "\\x[0-9A-Fa-f]\{2\}" contained
syntax match  qozEscape "\\u[0-9A-Fa-f]\{4\}" contained
" `{` opens an interpolation slot. `{{` and `}}` are literal-brace
" escapes that must not open one. The negative lookahead on the
" start pattern keeps a doubled brace from triggering the region.
" The explicit qozBraceEscape match consumes both characters with
" higher priority because it is defined later.
syntax region qozInterp  start=+{\({\)\@!+ end=+}+ contained contains=ALLBUT,qozInterp
syntax match  qozBraceEscape "{{\|}}" contained

" -- Char literal ---------------------------------------------------
" A char literal is one byte or one escape sequence surrounded by
" single quotes: 'a', '\n', '\''. The fixed-length pattern keeps an
" apostrophe inside a comment or identifier from opening a runaway
" region.
syntax match qozChar /'\\\?.'/

" -- Attributes -----------------------------------------------------
syntax match qozAttribute "@\(link_name\|operator\)\>"

" -- Operators ------------------------------------------------------
syntax match qozOperator "[+\-*/%<>=!&|^~?]"
syntax match qozOperator "->"
syntax match qozOperator "::"
syntax match qozOperator "\.\."
syntax match qozOperator "\.\.<"
syntax match qozOperator "\.\.="

" -- Comments (defined last so the // region wins over the / match) -
syntax keyword qozTodo contained TODO FIXME XXX NOTE HACK
syntax region  qozComment       start=+//+ end=+$+   keepend contains=qozTodo,@Spell
syntax region  qozBlockComment  start=+/\*+ end=+\*/+ contains=qozBlockComment,qozTodo,@Spell

" -- Highlight links ------------------------------------------------
highlight default link qozTodo         Todo
highlight default link qozComment      Comment
highlight default link qozBlockComment Comment
highlight default link qozStatement    Statement
highlight default link qozConditional  Conditional
highlight default link qozRepeat       Repeat
highlight default link qozImport       Include
highlight default link qozBoolean      Boolean
highlight default link qozConstant     Constant
highlight default link qozDirective    PreProc
highlight default link qozType         Type
highlight default link qozNumber       Number
highlight default link qozString       String
highlight default link qozChar         Character
highlight default link qozEscape       SpecialChar
highlight default link qozInterp       Special
highlight default link qozBraceEscape  SpecialChar
highlight default link qozAttribute    Identifier
highlight default link qozOperator     Operator

let b:current_syntax = "qoz"
