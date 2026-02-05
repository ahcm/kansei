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
- `--dump-wat` — dump WASM Text that you can run with e.g. wasmtime
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
It provides diagnostics and hover on symbols defined in the current document.

### LSP Debug Logging
Set `KANSEI_LSP_LOG` to a file path to capture basic LSP diagnostics:
```
KANSEI_LSP_LOG=/tmp/kansei-lsp.log kansei lsp
```

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

lspconfig.kansei.setup({
  capabilities = vim.lsp.protocol.make_client_capabilities(),
})
```

### Helix
Example `languages.toml` entry:
```toml
[[language]]
name = "kansei"
scope = "source.ks"
file-types = ["ks"]
language-servers = ["kansei"]
formatter = { command = "kansei", args = ["fmt", "--stdin"] }

[language-server.kansei]
command = "kansei"
args = ["lsp"]
```

To enable textobjects and indent queries in Helix, copy these files:
```
helix/runtime/queries/kansei/textobjects.scm
helix/runtime/queries/kansei/indents.scm
```
into your Helix runtime at `~/.config/helix/runtime/queries/kansei/`.
