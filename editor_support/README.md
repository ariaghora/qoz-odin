# Editor support

A Vim runtime plugin lives at [vim/](vim/). Neovim consumes the same
files, so there is one plugin and one install procedure regardless of
which of the two editors you run.

Layout:

```
editor_support/vim/
  ftdetect/qoz.vim    map *.qoz to filetype qoz
  syntax/qoz.vim      highlight definitions
  ftplugin/qoz.vim    commentstring and 4-space indentation
```

## Install

### Manual

```
mkdir -p ~/.config/nvim/{ftdetect,syntax,ftplugin}    # Neovim
mkdir -p ~/.vim/{ftdetect,syntax,ftplugin}            # Vim
```

Copy or symlink each file into the corresponding subdirectory of
`~/.config/nvim/` (Neovim) or `~/.vim/` (Vim). Opening a `.qoz` file
then triggers the filetype and the syntax module loads.

### lazy.nvim

```lua
return {
  {
    dir = "/absolute/path/to/qoz-odin/editor_support/vim",
    name = "qoz.vim",
    ft = "qoz",
    init = function()
      vim.filetype.add({ extension = { qoz = "qoz" } })
    end,
  },
}
```

### Plug-style managers

Most plugin managers can point at a subdirectory of a checked-out
repo. vim-plug example:

```
Plug 'your-fork/qoz-odin', { 'rtp': 'editor_support/vim' }
```

## What is highlighted

- Keywords: `let`, `var`, `return`, `defer`, `if`, `elif`, `else`,
  `match`, `while`, `for`, `in`, `import`, `external`, `as`, `new`,
  `type`.
- Compile-time directives: `#if`, `#elif`, `#else`, `#link_library`,
  `#link_framework`, `#link_path`, `#load_string`.
- Primitive types: `i8`..`i64`, `u8`..`u64`, `f32`, `f64`, `bool`,
  `char`, `string`, `cstring`, `void`, `unit`.
- Numeric literals including `0x`, `0b`, `0o` prefixes and `_`
  separators.
- Double-quoted strings, backtick interpolation with `{expr}` slots,
  character literals, escape sequences, `{{` / `}}` brace escapes.
- `@link_name` and `@operator` attributes.
- Line and block comments with `TODO` / `FIXME` recognition.

## Treesitter

A Treesitter grammar would give finer-grained highlighting and
incremental updates. It is not in this directory yet. The Vim regex
syntax above is correct for the language as it stands, and a
grammar is a separate project.
