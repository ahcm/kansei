# Kansei Interpreter

## Usage
```
kansei [options] [script] [args...]
```

### Options
- `-h`, `--help` — show help
- `-e`, `--evaluate <src>` — evaluate a one-liner (can be repeated)
- `--dump-ast` — dump AST
- `--dump-ast-sexpr` — dump AST as S-Expr
- `--dump-bytecode` — dump bytecode
- `--bytecode <mode>` — bytecode mode: `off|simple|advanced`
- `-l`, `--log <path>` — write log output to a file (default: stderr)

### Commands
- `kansei fmt <path>` — format `.ks` files in place
- `kansei check <path>` — parse `.ks` files and exit non-zero on errors
- `kansei test <path>` — run `.ks` files and compare against `.out`/`.err` if present
- `kansei install [path]` — install modules from `kansei.toml` or local paths
- `kansei lsp` — start Language Server over stdio (diagnostics only)

## LSP (Language Server) Usage
The language server is started with:
```
kansei lsp
```
It provides basic diagnostics on file open/change.

### Vim (vim-lsp)
Example `~/.vimrc`:
```vim
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'

if executable('kansei')
  augroup kansei_lsp
    autocmd!
    autocmd User lsp_setup call lsp#register_server({
      \ 'name': 'kansei',
      \ 'cmd': {server_info->['kansei', 'lsp']},
      \ 'whitelist': ['ks'],
      \ })
  augroup END
endif
```

### Neovim (nvim-lspconfig)
Example `init.lua`:
```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.kansei then
  configs.kansei = {
    default_config = {
      cmd = { 'kansei', 'lsp' },
      filetypes = { 'ks' },
      root_dir = lspconfig.util.root_pattern('.git', 'kansei.toml'),
    },
  }
end

lspconfig.kansei.setup({})
```

### Helix
Example `languages.toml` entry:
```toml
[[language]]
name = "kansei"
scope = "source.ks"
file-types = ["ks"]
language-servers = ["kansei"]

[language-server.kansei]
command = "kansei"
args = ["lsp"]
```
