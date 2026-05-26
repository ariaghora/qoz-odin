# Qoz syntax highlighting for Neovim

Neovim reads Vim runtime files directly, so the Vim plugin at
`editor-support/vim/` works as-is. Two install paths.

## 1. Plain Neovim, no plugin manager

Drop the Vim plugin into `~/.config/nvim/`:

```
mkdir -p ~/.config/nvim/{ftdetect,syntax,ftplugin}
cp editor-support/vim/ftdetect/qoz.vim ~/.config/nvim/ftdetect/qoz.vim
cp editor-support/vim/syntax/qoz.vim   ~/.config/nvim/syntax/qoz.vim
cp editor-support/vim/ftplugin/qoz.vim ~/.config/nvim/ftplugin/qoz.vim
```

Open a `.qoz` file. `:set ft?` reports `filetype=qoz` and colors appear.

## 2. lazy.nvim spec

Create `~/.config/nvim/lua/plugins/qoz.lua` with the spec below.
Replace the path with your local checkout, or point `dir` at a Git
URL.

```lua
return {
  {
    dir = "/absolute/path/to/qoz-odin/editor-support/vim",
    name = "qoz.vim",
    ft = "qoz",
    init = function()
      vim.filetype.add({ extension = { qoz = "qoz" } })
    end,
  },
}
```

Restart Neovim or run `:Lazy reload qoz.vim`.

## What is highlighted

Same surface as the Vim plugin:

- Keywords: `let`, `var`, `return`, `defer`, `if`, `elif`, `else`,
  `match`, `while`, `for`, `in`, `import`, `external`, `as`, `new`,
  `type`.
- Compile-time directives: `#if`, `#elif`, `#else`, `#link_library`,
  `#link_framework`, `#link_path`, `#load_string`.
- Primitive types and numeric literals (hex, binary, octal,
  underscore separators).
- Double-quoted strings, backtick interpolation with `{expr}` slot
  highlighting, character literals, escape sequences.
- `@link_name`, `@operator` attributes.
- Line and block comments with `TODO` / `FIXME` recognition.

## Treesitter

A Treesitter grammar would give finer-grained highlighting and
incremental updates, plus power text-object plugins. It is not in
this directory yet. The Vim regex syntax above is correct for the
language as it stands; a Treesitter grammar is a separate project.
