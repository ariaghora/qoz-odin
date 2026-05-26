# Qoz syntax highlighting for Vim

This directory ships a minimal Vim plugin layout. To enable
highlighting on `*.qoz` files, drop the directory contents into Vim's
runtime path.

## Manual install

```
mkdir -p ~/.vim/{ftdetect,syntax,ftplugin}
cp editor-support/vim/ftdetect/qoz.vim ~/.vim/ftdetect/qoz.vim
cp editor-support/vim/syntax/qoz.vim   ~/.vim/syntax/qoz.vim
cp editor-support/vim/ftplugin/qoz.vim ~/.vim/ftplugin/qoz.vim
```

## Symlink install

```
mkdir -p ~/.vim/ftdetect ~/.vim/syntax ~/.vim/ftplugin
ln -s "$PWD/editor-support/vim/ftdetect/qoz.vim" ~/.vim/ftdetect/qoz.vim
ln -s "$PWD/editor-support/vim/syntax/qoz.vim"   ~/.vim/syntax/qoz.vim
ln -s "$PWD/editor-support/vim/ftplugin/qoz.vim" ~/.vim/ftplugin/qoz.vim
```

## Plugin manager

Most plugin managers can point at a subdirectory of a checked-out
repository. For vim-plug:

```
Plug 'your-fork/qoz-odin', { 'rtp': 'editor-support/vim' }
```

## What it highlights

- Keywords: `let`, `var`, `return`, `defer`, `if`, `elif`, `else`,
  `match`, `while`, `for`, `in`, `import`, `external`, `as`, `new`,
  `type`.
- Compile-time directives: `#if`, `#elif`, `#else`, `#link_library`,
  `#link_framework`, `#link_path`, `#load_string`.
- Primitive types: `i8`..`i64`, `u8`..`u64`, `f32`, `f64`, `bool`,
  `char`, `string`, `cstring`, `void`, `unit`.
- Numbers including hex, binary, and octal with underscore separators.
- Double-quoted strings and backtick interpolation, with escape
  sequences and `{expr}` slot highlighting.
- Character literals.
- `@link_name`, `@operator` attribute prefixes.
- Operators including `->`, `::`, `..`, `..<`, `..=`.
- Line and block comments with `TODO` / `FIXME` highlighting inside.
