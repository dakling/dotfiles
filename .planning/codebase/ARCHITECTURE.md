# Architecture

**Analysis Date:** 2026-02-19

## Pattern Overview

**Overall:** Multi-editor configuration system with a primary Doom Emacs setup and secondary Neovim setup, both unified by shared shell configurations and custom utility scripts.

**Key Characteristics:**
- Symlink-based configuration management (no Stow): configs in dotfiles repo linked to `~/.config/` directories
- Modular Doom Emacs with external AI/MCP tools integration
- Platform-aware (primary macOS/Darwin, secondary Linux with system-level gating)
- Custom Elisp modules for Emacs integration with Claude Code IDE via MCP
- LazyVim-based Neovim with lazy plugin loading

## Layers

**Emacs Configuration (Doom):**
- Purpose: Main editor configuration for macOS, highly extensible AI-integrated environment
- Location: `doom/` → symlinked to `~/.config/doom/`
- Contains: Module declarations, customizations, package recipes, LSP/language configurations, AI integrations
- Depends on: Doom Emacs runtime, external packages from MELPA/GitHub, MCP server bridge
- Used by: Editor and IDE interactions via Claude Code IDE MCP tools

**Neovim Configuration (LazyVim):**
- Purpose: Secondary editor for quick edits, portable across platforms
- Location: `nvim/` → symlinked to `~/.config/nvim/`
- Contains: Lua-based plugin specs, option/keymap/autocommand configs
- Depends on: LazyVim base, Treesitter, LSP clients
- Used by: Text editing on systems where Emacs is not preferred

**Shell Configuration:**
- Purpose: Cross-platform shell environment (zsh/bash) for terminal operations
- Location: `zshrc`, `bashrc` (symlinked to `~/`)
- Contains: Aliases, completion, environment variables, history settings
- Depends on: No external dependencies (pure shell)
- Used by: Terminal sessions on macOS and Linux

**Utility Scripts:**
- Purpose: Custom scripts for system-level operations (volume, brightness, window management)
- Location: `binCustom/` (38 scripts, ad-hoc collection)
- Contains: Bash/Python/shell scripts for audio, display, file operations, meeting utilities
- Depends on: System tools (pactl, brightnessctl, etc.)
- Used by: Keybindings in window managers, Emacs, or manual invocation

**Window Managers:**
- Purpose: X11/macOS window management layer
- Location: `qtile.py` (Qtile for Xorg), `stumpwm.lisp` (StumpWM for X11), `amethyst.yml` (macOS)
- Contains: Layout definitions, keybindings, window groups, system hooks
- Depends on: Window manager runtimes
- Used by: Desktop/GUI session management

**Application Configurations:**
- Purpose: Single-purpose app configurations (mail, browser, terminal)
- Location: Root-level config files
- Contains: `alacritty.yml`, `dunstrc`, `mbsyncrc`, `vimrc`, `rofi.rasi`, `termite`, `nyxt.lisp`, `bottom.toml`
- Depends on: Respective applications
- Used by: Individual applications when launched

**System-Level Configs:**
- Purpose: System package management and Nix/Guix configurations
- Location: `configuration.nix`, `guix.scm`, `pacmanfile.txt`, `installPackages`
- Contains: Declarative system state definitions
- Depends on: NixOS or Guix package manager
- Used by: Linux system provisioning

## Data Flow

**Installation Flow:**

1. User runs `./INSTALL` script
2. Script creates `~/.config/` directories
3. Script symlinks configuration files into place
4. Doom Emacs: runs `doom sync` to resolve packages via straight.el
5. Neovim: lazy.nvim downloads plugins on first startup
6. Shell configs loaded on terminal startup

**Emacs-to-Claude Integration Flow:**

1. Claude Code (IDE) calls MCP tool via HTTP/stdin
2. `doom/bin/codex-ediff-mcp-server.py` receives MCP request
3. Server generates diff in temp file
4. Server calls emacsclient with elisp command to open diff
5. `ediff-chunk-select` package enables per-hunk selection in ediff buffer
6. User reviews and accepts/rejects chunks
7. Result sent back through MCP to Claude

**Configuration Reload Flow:**

1. User modifies `doom/config.el` → immediate effect with `M-x doom/reload`
2. User modifies `doom/init.el` or `doom/packages.el` → requires `doom sync` + restart
3. User modifies `nvim/` → lazy.nvim auto-reloads on next edit
4. User modifies shell config → sourced on new shell session

## Key Abstractions

**Doom Module System:**
- Purpose: Declare which Emacs modules/features are enabled
- Examples: `doom/init.el` lines 20-192 define completion (corfu+helm), UI, editor, lang, tools modules
- Pattern: Configuration is declarative at top-level, details in `config.el` or package-level setups

**Package Declaration (External):**
- Purpose: Pin and version external Elisp packages from GitHub
- Examples: `doom/packages.el` lines 72-154 declare custom packages
  - `ediff-chunk-select` — enables per-hunk diff selection
  - `claude-code-ide` and `claude-code-ide-mcp-tools` — AI IDE integration
  - `prompt-compose` — local recipe at `~/code/emacs-packages/prompt-compose`
  - `ai-workflows` — local recipe at `/Users/darioklingenberg/code/emacs-packages/ai-workflows`
- Pattern: Use `:recipe (:host github :repo "user/name")` or `:local-repo` for local packages

**MCP Server Bridge:**
- Purpose: Forward diffs from Claude Code to Emacs ediff with per-hunk selection
- Location: `doom/bin/codex-ediff-mcp-server.py`
- Pattern: Implements MCP protocol (2024-11-05), exposes two tools: `openDiff()`, `closeAllDiffTabs()`
- Mechanism: Calls emacsclient to evaluate elisp commands that open diffs in Emacs

**LazyVim Plugin Spec:**
- Purpose: Declaratively load plugins and language support
- Examples: `nvim/lua/config/lazy.lua` imports base LazyVim, language extras (Python, Haskell, JSON, etc.), and local plugin specs
- Pattern: Each plugin category (lang, editor, ai, tools, org) is a separate Lua file in `nvim/lua/plugins/`

**Custom Elisp Predicates:**
- Purpose: Gate platform-specific code paths
- Examples: `doom/config.el` defines `(system-name= "name1" "name2")` to check current system
- Pattern: Used throughout config for macOS (`ns-alternate-modifier`, `macos` module) vs Linux behaviors

## Entry Points

**Emacs:**
- Location: `doom/early-init.el` → `doom/init.el` → `doom/config.el` → `doom/custom.el`
- Triggers: User launches Emacs or invokes `emacsclient`
- Responsibilities: Load Doom framework, declare modules, apply customizations, set up keybindings, load packages

**Neovim:**
- Location: `nvim/init.lua` → `nvim/lua/config/lazy.lua` → plugin specs
- Triggers: User launches Neovim
- Responsibilities: Bootstrap lazy.nvim, load LazyVim base, load language/editor extras, apply custom configs

**Shell:**
- Location: `.zshrc` / `.bashrc`
- Triggers: New terminal session
- Responsibilities: Set options, aliases, PATH, completion, history settings

**Installation:**
- Location: `./INSTALL` script
- Triggers: User runs during initial setup
- Responsibilities: Create config directories, symlink files, guide Doom sync

**MCP Server:**
- Location: `doom/bin/codex-ediff-mcp-server.py`
- Triggers: Claude Code calls MCP tool
- Responsibilities: Parse MCP requests, manage diff temp files, invoke emacsclient

## Error Handling

**Strategy:** Non-fatal with fallback

**Patterns:**
- Doom Emacs uses `use-package!` macro which gracefully handles missing packages
- `doom/config.el` line 86-89: Uses `use-package! jinx` hook that loads if available, silent if not
- Shell scripts use `[ -r FILE ] && . FILE` pattern to source optional files without error
- Neovim uses lazy plugin loader which skips missing plugins at startup
- MCP server catches JSON decode errors and supports both LSP-framed and line-delimited formats (`doom/bin/codex-ediff-mcp-server.py` lines 32-52)

## Cross-Cutting Concerns

**Logging:**
- Emacs: Uses `doom/output` macros and message buffer for debug output
- Neovim: Uses `vim.notify()` for UI notifications
- Scripts: Use `echo` to stdout/stderr
- MCP server: Logs to stderr via print() after message handling

**Validation:**
- Emacs: defcustom with type validation, use `setq!` for Doom-managed vars
- Neovim: Lua table validation at startup via lazy.nvim
- Shell: No built-in validation; relies on script author discipline

**Authentication:**
- Emacs: GPG integration via `pinentry` package, mu4e email auth, pass password manager
- Neovim: LSP server authentication (language-specific)
- MCP: No authentication (local Unix socket/stdio, trust based)

**Platform Compatibility:**
- Emacs: `(eq system-type 'gnu/linux)` vs macOS checks; `ns-alternate-modifier = meta` on macOS
- Neovim: OS detection via `vim.fn.has()` in Lua; Neovide GUI-specific settings at `nvim/lua/config/options.lua` lines 78-123
- Shell: $OSTYPE checks, Linux-specific conditionals in zshrc

---

*Architecture analysis: 2026-02-19*
