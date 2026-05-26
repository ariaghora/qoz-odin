# Qoz syntax highlighting for Neovim

Neovim reads Vim runtime files transparently, so the Vim plugin at
`editor-support/vim/` works as-is. Three install paths, pick whichever
matches your setup.

## 1. Plain Neovim, no plugin manager

Drop the Vim plugin into `~/.config/nvim/`:

```
mkdir -p ~/.config/nvim/{ftdetect,syntax,ftplugin}
cp editor-support/vim/ftdetect/qoz.vim ~/.config/nvim/ftdetect/qoz.vim
cp editor-support/vim/syntax/qoz.vim   ~/.config/nvim/syntax/qoz.vim
cp editor-support/vim/ftplugin/qoz.vim ~/.config/nvim/ftplugin/qoz.vim
```

Open a `.qoz` file. `:set ft?` reports `filetype=qoz` and colors appear.

## 2. lazy.nvim (used by AstroNvim, LazyVim, kickstart.nvim)

Create `~/.config/nvim/lua/plugins/qoz.lua` (or `~/.config/nvim/lua/user/plugins/qoz.lua` for older AstroNvim) with the spec below. Replace
`/Users/you/Codes/qoz-odin` with the absolute path to your checkout, or
push the repo to a Git host and point `dir` at a Git URL via `{ 'owner/qoz-odin' }`.

```lua
return {
  {
    dir = "/Users/you/Codes/qoz-odin/editor-support/vim",
    name = "qoz.vim",
    ft = "qoz",
    init = function()
      vim.filetype.add({ extension = { qoz = "qoz" } })
    end,
  },
}
```

Restart Neovim or run `:Lazy reload qoz.vim`.

## 3. AstroNvim community-style user plugin

If you keep an AstroNvim user config under `~/.config/nvim/lua/user/`,
the same lazy.nvim spec works inside
`~/.config/nvim/lua/user/plugins/qoz.lua`. AstroNvim's lazy setup picks
it up on the next launch. The `vim.filetype.add` call inside `init`
makes the filetype detection fire even before the plugin loads.

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

## Treesitter (future)

A Treesitter grammar would give finer-grained highlighting and
incremental updates, plus power `nvim-treesitter-textobjects` and
related plugins. It is not in this directory yet. The Vim regex syntax
above is correct for the language as it stands; a Treesitter grammar is
a separate, larger project.
