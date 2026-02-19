# Codebase Structure

**Analysis Date:** 2026-02-19

## Directory Layout

```
/Users/darioklingenberg/.dotfiles/
├── doom/                           # Doom Emacs configuration (symlinked to ~/.config/doom/)
│   ├── init.el                     # Module declarations and flags
│   ├── config.el                   # Main customizations (~1373 lines)
│   ├── custom.el                   # Emacs custom-set-variables (generated)
│   ├── packages.el                 # Package declarations with GitHub recipes
│   └── bin/
│       └── codex-ediff-mcp-server.py  # MCP bridge for Claude Code diffs
├── nvim/                           # Neovim configuration (symlinked to ~/.config/nvim/)
│   ├── init.lua                    # Entry point, lazy.nvim bootstrap
│   └── lua/
│       ├── config/
│       │   ├── options.lua         # Editor options and Neovide GUI settings
│       │   ├── keymaps.lua         # Keybindings
│       │   ├── autocmds.lua        # Autocommands
│       │   └── lazy.lua            # LazyVim plugin spec imports
│       └── plugins/
│           ├── colorscheme.lua     # Theme configuration
│           ├── editor.lua          # Editor plugins (surround, illuminate, etc)
│           ├── lang.lua            # Language support (Python, Haskell, JSON, etc)
│           ├── ai.lua              # AI plugins
│           ├── tools.lua           # Tool plugins (LSP, DAP, none-ls)
│           ├── org.lua             # Org-mode plugins
│           └── (imports LazyVim extras)
├── binCustom/                      # 38 custom utility scripts
│   ├── volume*.sh                  # Audio control
│   ├── brightness*.sh              # Display control
│   ├── toggle_touchpad*.sh         # System settings
│   ├── search-recoll.sh            # File search
│   ├── openMeetingLatest*          # Meeting PDF/LaTeX utilities
│   ├── launch_polybar.sh           # Polybar launcher
│   ├── nterm                       # Terminal wrapper
│   └── (10+ other utilities)
├── .planning/                      # Planning/analysis output (generated)
│   └── codebase/                   # This analysis lives here
├── INSTALL                         # Installation script (symlinks configs)
├── zshrc                           # Zsh configuration
├── bashrc                          # Bash configuration
├── alacritty.yml                   # Alacritty terminal emulator config
├── vimrc                           # Vim configuration
├── amethyst.yml                    # macOS tiling window manager (Amethyst)
├── qtile.py                        # Linux X11 window manager (Qtile)
├── stumpwm.lisp                    # Linux X11 window manager (StumpWM)
├── dunstrc                         # Dunst notification daemon
├── mbsyncrc                        # Mbsync mail sync config
├── rofi.rasi                       # Rofi application launcher
├── termite                         # Termite terminal config
├── nyxt.lisp                       # Nyxt browser config
├── bottom.toml                     # Bottom system monitor config
├── configuration.nix               # NixOS system config
├── guix.scm                        # Guix package definitions
├── guix-manifest.scm               # Guix manifest
├── pacmanfile.txt                  # Arch Linux package list
├── installPackages                 # Package installation script
├── desktopFiles/                   # Desktop launcher entries
├── plover/                         # Stenotype system config
├── early-init.el                   # Emacs pre-initialization (before init.el)
├── xinitrc                         # X11 initialization
├── .git/                           # Git repository
├── CLAUDE.md                       # Project-specific Claude instructions
├── LICENSE                         # License file
└── README (implied)
```

## Directory Purposes

**`doom/`:**
- Purpose: Doom Emacs configuration, the primary editor on macOS
- Contains: Elisp configuration files, module declarations, package recipes, MCP server bridge
- Key files: `init.el` (modules), `config.el` (customizations), `packages.el` (packages)
- Symlink target: `~/.config/doom/`
- Scope: ~1400 lines of config, 30+ custom packages declared

**`nvim/`:**
- Purpose: Neovim configuration (LazyVim-based), secondary editor
- Contains: Lua configuration files, plugin specs by category
- Key files: `init.lua` (bootstrap), `lua/config/lazy.lua` (plugin imports), `lua/plugins/*.lua` (specs)
- Symlink target: `~/.config/nvim/`
- Scope: Modular plugin-based, uses LazyVim extras

**`binCustom/`:**
- Purpose: Personal utility scripts for system/editor operations
- Contains: Bash/Python/shell scripts for audio, display, file ops, meetings
- Organization: Ad-hoc collection, no subdirectories, scripts prefixed with function name (volume-, brightness-, toggle_, etc)
- Executable: All files are chmod +x
- Usage: Invoked via Emacs keybindings, window manager keybindings, or terminal

**`desktopFiles/`:**
- Purpose: Linux desktop application launcher entries (.desktop files)
- Contains: Application metadata for rofi, desktop environments
- Organization: One file per application

**`plover/`:**
- Purpose: Stenotype system configuration (steno keyboard)
- Contains: Plover plugins/dictionaries/configs

**Shell configs (zshrc, bashrc, xinitrc):**
- Purpose: Shell environment setup, aliases, PATH, completion
- Target: `~/` (root home)
- Organization: Single files at repo root

**Application configs (root-level .yml, .rc, .lisp files):**
- Purpose: Per-application configuration
- Target: Symlinked to `~/.config/` or `~/` as appropriate
- Examples: `alacritty.yml` → `~/.config/alacritty/alacritty.yml`, `mbsyncrc` → `~/.mbsyncrc`

## Key File Locations

**Entry Points:**
- `doom/early-init.el`: Pre-Emacs initialization (disable tool/menu bars, set package repos)
- `doom/init.el`: Doom module declarations (completion, ui, editor, lang modules)
- `doom/config.el`: Main Doom Emacs customization (~1373 lines)
- `nvim/init.lua`: Neovim entry, lazy.nvim bootstrap
- `zshrc`: Zsh shell initialization
- `INSTALL`: Installation script that symlinks all configs

**Configuration:**
- `doom/init.el`: Declares enabled Doom modules and flags
- `doom/packages.el`: Declares external Elisp packages
- `doom/custom.el`: Emacs-generated custom variables (auto-updated)
- `nvim/lua/config/options.lua`: Neovim editor options, Neovide GUI settings
- `nvim/lua/config/keymaps.lua`: Neovim keybindings
- `nvim/lua/config/autocmds.lua`: Neovim autocommands
- `amethyst.yml`: macOS window manager layout/behavior
- `qtile.py`: Linux Qtile window manager Python config
- `stumpwm.lisp`: Linux StumpWM Common Lisp config

**Core Logic:**
- `doom/config.el`: Doom customizations including evil keybindings, LSP setup, org-mode, mu4e email, language configs
- `doom/bin/codex-ediff-mcp-server.py`: MCP protocol implementation for Claude Code diff integration
- `nvim/lua/plugins/lang.lua`: Language support plugin specs
- `nvim/lua/plugins/ai.lua`: AI integration plugins
- `nvim/lua/plugins/tools.lua`: LSP, DAP, linting plugins

**Testing:**
- `.planning/codebase/`: Analysis documents (ARCHITECTURE.md, STRUCTURE.md, etc)
- No unit tests in dotfiles repo (configs are end-to-end tested by runtime)

## Naming Conventions

**Files:**
- Emacs: `*.el` for Elisp; `config.el`, `init.el`, `packages.el` are Doom conventions
- Neovim: `*.lua` for Lua; config/ for base configs, plugins/ for plugin specs
- Scripts: `snake_case.sh` for bash (e.g., `brightness_down.sh`, `toggle_touchpad.sh`)
- Python scripts: `snake_case.py` (e.g., `codex-ediff-mcp-server.py`, `deleteTimeSteps.py`)
- Window managers: `qtile.py` (Python), `stumpwm.lisp` (Common Lisp), `amethyst.yml` (YAML)
- Configs: application name or purpose (e.g., `alacritty.yml`, `dunstrc`, `rofi.rasi`)

**Directories:**
- `doom/`: Doom Emacs configs
- `nvim/`: Neovim configs (Lua-based)
- `binCustom/`: Custom scripts (no subdirectories)
- `lua/config/`: Base Neovim configs
- `lua/plugins/`: Neovim plugin specs
- `desktopFiles/`: Desktop launcher entries
- `plover/`: Stenotype system

## Where to Add New Code

**New Doom Emacs Feature:**
- Implementation: `doom/config.el` (end of file before final line)
- Package declaration: `doom/packages.el` if external package needed
- Testing: Manual in Emacs, use `M-x doom/reload` to reload without restart
- Keybindings: Add to `doom/config.el` using `map!` macro

**New Neovim Plugin/Config:**
- Plugin spec: `nvim/lua/plugins/[category].lua` (choose category: lang, editor, ai, tools, org)
- Options: `nvim/lua/config/options.lua`
- Keybindings: `nvim/lua/config/keymaps.lua`
- Autocommands: `nvim/lua/config/autocmds.lua`
- Testing: Manual in Neovim

**New Utility Script:**
- Location: `binCustom/[script_name]` (no extension or .sh for bash)
- Template: Bash shebang `#!/bin/bash` or `#!/usr/bin/env python3`
- Mode: Set executable: `chmod +x binCustom/[script_name]`
- Naming: Use snake_case, prefix with function (volume-, brightness-, toggle-, etc)

**New System Configuration (macOS):**
- Amethyst: `amethyst.yml`
- Shell: `zshrc`
- Terminal: `alacritty.yml`

**New System Configuration (Linux):**
- Qtile window manager: `qtile.py`
- StumpWM window manager: `stumpwm.lisp`
- Shell: `bashrc` or `zshrc`
- Notifications: `dunstrc`

**Shared Utilities:**
- Emacs-specific: Custom functions in `doom/config.el`
- Cross-editor: Shell functions in `zshrc` or `bashrc`
- System-level: Scripts in `binCustom/`

## Special Directories

**`.planning/codebase/`:**
- Purpose: GSD codebase analysis documents
- Generated: Yes (by Claude Code mapper)
- Committed: Yes (to git)
- Contents: ARCHITECTURE.md, STRUCTURE.md, CONVENTIONS.md, TESTING.md, STACK.md, INTEGRATIONS.md, CONCERNS.md

**`.git/`:**
- Purpose: Git version control
- Generated: Yes (by git init)
- Committed: No (git metadata)

**`nvim/lua/plugins/` (generated at runtime):**
- Note: Plugin code is installed by lazy.nvim into `~/.local/share/nvim/` or similar, NOT committed
- Committed: Only specs (*.lua) are committed, not plugin source

**`doom/.emacs.d/` (external):**
- Note: Doom framework itself lives in `~/.emacs.d/`, NOT in this repo
- This repo contains only config; Doom runtime is installed separately or via Doom bootstrap
- `~/.config/doom/` is symlinked from `doom/` in this repo

## File Interdependencies

**Doom Emacs Bootstrap Order:**
1. `early-init.el` (pre-frame, package repos)
2. `init.el` (module declarations)
3. `packages.el` (external packages)
4. `config.el` (customizations)
5. `custom.el` (auto-generated)

**Neovim Bootstrap Order:**
1. `init.lua` (entry, lazy.nvim bootstrap)
2. `lua/config/lazy.lua` (plugin specs, imports)
3. `lua/config/options.lua` (editor options)
4. `lua/config/keymaps.lua` (keybindings)
5. `lua/config/autocmds.lua` (autocommands)
6. `lua/plugins/*.lua` (loaded by lazy.nvim)

**Installation Order:**
1. Run `./INSTALL` script
2. Creates `~/.config/nvim`, `~/.config/doom`, `~/.config/stumpwm`
3. Symlinks `doom/*` to `~/.config/doom/`
4. Symlinks `nvim/init.lua` to `~/.config/nvim/init.lua`
5. Run `doom sync` to download/install Doom packages

## Cross-Reference Summary

**For adding Emacs features:** See `doom/config.el` structure (lines 1-52 for conventions, lines 53+ for custom code)

**For adding Neovim features:** See `nvim/lua/plugins/` directory (separate file per category)

**For adding scripts:** See `binCustom/` naming and location patterns

**For platform-specific code:** Use `(eq system-type 'gnu/linux)` in Elisp or `vim.fn.has("unix")` in Lua

**For external integrations:** Declare in `doom/packages.el` or `nvim/lua/plugins/` as appropriate

---

*Structure analysis: 2026-02-19*
