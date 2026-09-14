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
- `kansei fmt <path>` — format indentation in `.ks` files in place, preserving comments and literal contents
- `kansei check <path>` — parse `.ks` files and exit non-zero on errors
- `kansei test <path>` — run `.ks` files in all execution modes; compare `.out`/`.err` and expected `.status` if present
- `kansei install [path]` — install modules from `kansei.toml` or local paths
- `kansei lsp` — start Language Server over stdio (syntax diagnostics, hover, and definitions)

### Test runner options

`kansei test [--bytecode off|simple|advanced|all] [--timeout seconds] <paths...>`
runs sorted, deduplicated files. Defaults are all modes and 30 seconds per run.
See [tests/README.md](tests/README.md) for snapshots and expected failures.

### Local module installation

Dependency names must contain only ASCII letters, digits, `_`, or `-`. Manifest
paths are resolved relative to `kansei.toml`. Installation stages a complete
replacement before moving the existing module aside, with rollback on replacement
failure. Source trees must contain `.ks` files; symbolic links within the tree and
overlapping source/destination trees are rejected.

## LSP (Language Server) Usage
The language server is started with:
```
kansei lsp
```
It reports syntax diagnostics on open/change, clears them after correction, and provides symbol hover and definitions.

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
