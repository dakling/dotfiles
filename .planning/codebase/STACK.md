# Technology Stack

**Analysis Date:** 2026-02-19

## Languages

**Primary:**
- Emacs Lisp (elisp) - All Doom Emacs configuration (`doom/config.el`, `doom/init.el`, `doom/packages.el`, `doom/custom.el`)
- Lua - All Neovim configuration (`nvim/init.lua`, `nvim/lua/**/*.lua`)
- Python 3 - MCP server bridge (`doom/bin/codex-ediff-mcp-server.py`)

**Secondary:**
- Bash/Shell - Custom utility scripts (`binCustom/`, `installPackages`, `binCustom/*.sh`)
- Python - Legacy utility scripts in `binCustom/` (volume control, file management helpers)
- Common Lisp / Scheme - Supported language targets in both editors; StumpWM config (`stumpwm.lisp`), Nyxt config (`nyxt.lisp`)
- Nix - Raspberry Pi NixOS configuration (`configuration.nix`)
- Guix - Package manifest (`guix-manifest.scm`, `guix.scm`)

## Runtime

**Environment:**
- macOS (Darwin) - Primary platform; `(eq system-type 'gnu/linux)` guards Linux-only code
- Linux (Arch Linux) - Secondary platform; package management via pacman/yay
- Linux (NixOS) - Raspberry Pi configuration (`configuration.nix`)

**Emacs:**
- Doom Emacs framework (managed via `~/.config/emacs/bin/doom`)
- Package manager: straight.el (managed by Doom, stored in `~/.config/emacs/.local/straight/`)
- Emacs packages fetched from MELPA, ELPA, emacsmirror, and GitHub

**Neovim:**
- LazyVim base framework (`nvim/lazyvim.json` records install_version: 8)
- Package manager: lazy.nvim (bootstrapped in `nvim/init.lua`)
- Lazy.nvim version: stable branch

**Python:**
- Standard CPython; MCP server uses only stdlib (`json`, `subprocess`, `tempfile`, `ast`)
- No virtual environment for `doom/bin/codex-ediff-mcp-server.py` - runs as system Python via shebang `#!/usr/bin/env python3`

## Frameworks

**Core Editors:**
- Doom Emacs - Primary editor framework; config in `doom/`
- LazyVim - Neovim distribution; config in `nvim/`

**Doom Emacs Modules (from `doom/init.el`):**
- Completion: corfu (+icons +orderless +dabbrev), helm (+fuzzy +icons)
- UI: doom theme, hl-todo, minimap, modeline (+light), treemacs (-lsp), vc-gutter (+pretty), window-select (+numbers)
- Editor: evil (+everywhere), lispy, multiple-cursors, snippets, word-wrap
- Emacs: dired (+icons +dirvish), undo (+tree), vc
- Terminal: eshell, vterm
- Checkers: syntax (flycheck), grammar (langtool)
- OS: tty, macos
- Tools: biblio, debugger, direnv, editorconfig, eval (+overlay), llm, lookup (+dictionary +offline +docsets), lsp (+eglot), magit, pass, pdf, tree-sitter
- Languages: common-lisp, emacs-lisp, haskell (+lsp +tree-sitter), json (+lsp +tree-sitter), javascript (+lsp +tree-sitter), julia (+lsp +tree-sitter +snail), latex (+lsp +fold +cdlatex), markdown, org (heavily flagged), python (+pyright +lsp +tree-sitter), sh (+lsp), web (+lsp +tree-sitter), yaml (+lsp +tree-sitter)
- Email: mu4e (+gmail)
- App: calendar, everywhere, rss (elfeed)

**LazyVim Extras (from `nvim/lua/config/lazy.lua`):**
- Languages: python, haskell, json, yaml, markdown, tex, clojure
- Editor: dial, illuminate
- Coding: luasnip, mini-surround
- DAP: core debugger
- LSP: none-ls

## Key Dependencies

**Emacs - AI Integration (`doom/packages.el`):**
- `claude-code-ide` - Claude Code IDE integration (`manzaltu/claude-code-ide.el`)
- `claude-code-ide-mcp-tools` - MCP tools for claude-code-ide (`Kaylebor/claude-code-ide-mcp-tools`)
- `claude-code-ide-extras` - Extra claude-code-ide features (`acmorrow/claude-code-ide-extras`)
- `mcp` - MCP protocol client for Emacs (`lizqwerscott/mcp.el`)
- `goose` - Goose AI assistant (`aq2bq/goose.el`)
- `codex-cli` - OpenAI Codex CLI integration (MELPA)
- `prompt-compose` - Prompt composition UI (`dakling/prompt-compose`, local package)
- `emacs-claude-bridge` - Claude bridge (`dakling/emacs-claude-bridge`)
- `ai-workflows` - AI workflow orchestration (local at `~/code/emacs-packages/ai-workflows`)
- `gptel` - LLM chat (loaded via Doom's `llm` module, used in `doom/config.el`)

**Emacs - Custom/Local Packages (`doom/packages.el`):**
- `ediff-chunk-select` - Per-hunk chunk selection for ediff (`dakling/ediff-chunk-select`)
- `codex-ediff-mcp` - Bridges Claude/Codex diffs into ediff (local at `doom/lisp/`)

**Emacs - Editor Enhancement:**
- `ultra-scroll` - Smooth scrolling (`jdtsmith/ultra-scroll`)
- `evil-tex` - Evil text objects for LaTeX (`itai33/evil-tex`)
- `wgrep` - Writable grep buffers
- `ace-link` - Jump to links
- `string-inflection` - Naming convention cycling
- `beacon` - Cursor position flash
- `rotate` - Window rotation
- `shelldon` - Shell command runner (`Overdr0ne/shelldon`)
- `jinx` - Fast spell checking (uses libenchant, replaces flyspell)
- `lispy` / `lispyville` - Structural editing for Lisps (via Doom `lispy` module)

**Emacs - Org-mode:**
- `org-modern` - Modern org visual styling
- `org-super-links` - Backlinks for org (`toshism/org-super-links`)
- `org-ref` - Citations and references
- `el-igo` - Go (igo) board for org (`misohena/el-igo`)

**Emacs - Email:**
- `mu4e` - Email client (via Doom's `mu4e +gmail` module)
- `mu4e-alert` - Email alerts/notifications
- `mbsync` - IMAP synchronization (external tool, config at `mbsyncrc`)

**Emacs - Misc:**
- `elfeed` - RSS reader
- `alert` - Notification system
- `pinentry` - GPG PIN entry
- `system-packages` + `helm-system-packages` - Package manager UI
- `nov` - EPUB reader
- `async-await` - Async/await for elisp
- `web-server` - HTTP server within Emacs
- `systemd` - systemd unit file mode
- `sly` - Common Lisp REPL (unpinned from Doom's version)
- `lsp-ltex` - LanguageTool LSP for grammar

**Neovim - AI:**
- `coder/claudecode.nvim` - Claude Code IDE integration via WebSocket
- `folke/snacks.nvim` - Dependency for claudecode

**Neovim - Core:**
- `folke/lazy.nvim` - Package manager
- `LazyVim/LazyVim` - Base distribution
- `folke/flash.nvim` - Jump navigation
- `stevearc/oil.nvim` - Dired-like file manager
- `NeogitOrg/neogit` - Git UI (Magit replacement)
- `sindrets/diffview.nvim` - Diff viewer
- `nvim-telescope/telescope.nvim` - Fuzzy finder
- `mg979/vim-visual-multi` - Multiple cursors
- `akinsho/toggleterm.nvim` - Terminal management
- `stevearc/conform.nvim` - Code formatting
- `mfussenegger/nvim-dap` + `nvim-dap-python` - Debug adapter

**Neovim - Language:**
- `neovim/nvim-lspconfig` - LSP client; configured servers: pyright, clangd, bashls, hls, yamlls, jsonls, ltex
- `nvim-treesitter/nvim-treesitter` - Tree-sitter parser
- `lervag/vimtex` - LaTeX support
- `mrcjkb/haskell-tools.nvim` - Haskell tooling
- `JuliaEditorSupport/julia-vim` - Julia language support
- `julienvincent/nvim-paredit` - Structural editing for Lisp
- `Olical/conjure` - Lisp/language REPL
- `benlubas/molten-nvim` - Python/Jupyter REPL

**External Tools (system-level):**
- `mu` / `mu4e` - Email indexer
- `mbsync` - IMAP sync
- `pass` - Password manager (GPG-based)
- `pandoc` - Document conversion
- `latexmk` - LaTeX build tool
- `SBCL` - Common Lisp interpreter
- `emacsclient` - Used by MCP server to call into running Emacs
- `langtool` / LanguageTool - Grammar checking (Java, `/usr/share/languagetool`)
- `yay` - AUR helper (Linux only)
- `lazygit` - Git TUI (used in Neovim toggleterm)

## Configuration

**Environment:**
- No `.env` files; secrets managed via `pass` (GPG password store)
- Email passwords retrieved via `PassCmd "pass <name>"` in `mbsyncrc`
- GPG agent managed via `pinentry` package in Emacs
- Abbreviations file at `~/Dropbox/Dario/abbrev.el` (Dropbox-synced)
- Custom Elisp scripts loaded from `~/Dropbox/Helen+Dario/` at startup

**Build/Sync:**
- Doom Emacs: `~/.config/emacs/bin/doom sync` after modifying `doom/init.el` or `doom/packages.el`
- Doom Emacs: `M-x doom/reload` for `doom/config.el`-only changes
- Neovim: `:Lazy sync` via lazy.nvim

**Symlinks:**
- `doom/` → `~/.config/doom/`
- `nvim/` → `~/.config/nvim/`
- Managed via `./INSTALL` script (no Stow)

## Platform Requirements

**Development (macOS):**
- Emacs 28+ (with native compilation recommended)
- Neovim 0.9+ (for built-in editorconfig support)
- Python 3 (system, for MCP server)
- GPG + pass
- mu + mbsync (for email)
- pandoc, latexmk (for document workflows)
- Alacritty terminal emulator (config at `alacritty.yml`)
- Amethyst tiling window manager (config at `amethyst.yml`)

**Development (Linux/Arch):**
- Same editors plus yay (AUR), systemd, pulseaudio
- Window managers: Qtile (`qtile.py`), StumpWM (`stumpwm.lisp`)
- Notification daemon: dunst (`dunstrc`)
- Application launcher: rofi (`rofi.rasi`)

**Production:**
- No deployment targets; this is a personal configuration repository

---

*Stack analysis: 2026-02-19*
