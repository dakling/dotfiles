# Coding Conventions

**Analysis Date:** 2026-02-19

## Naming Patterns

**Elisp functions:**
- Kebab-case for all functions: `my/close-buffer`, `my/brightness+`, `qtile/move-focus`, `ediff-copy-both-to-C`
- Prefix with namespace/module before `/`: `my/`, `qtile/` for custom functions, `ediff-` for Ediff-related code
- Private/internal functions often have descriptive suffixes: `my/haskell-insert-type`, `my/run-command-ssh`

**Variables:**
- Kebab-case in Elisp: `evil-respect-visual-line-mode`, `display-line-numbers-type`, `helm-mini-default-sources`
- snake_case in Python: `content_length`, `emacsclient`, `old_file_path`, `new_file_contents`
- snake_case in shell: `get_volume`, `is_mute`, `send_notification`, `icon_name`

**Lua:**
- local variables: snake_case: `mapleader`, `scrolloff`, `split_side`, `input_lines`
- Global vim settings use camelCase: `opt.number`, `opt.relativenumber`, `opt.ignorecase`
- Functions: snake_case: `grab_terminal_input`, `is_border_line`, `is_status_line`

**Types/Constants:**
- Elisp: `PROTOCOL_VERSION`, `SERVER_NAME`, `SERVER_VERSION` (UPPERCASE for constants)
- Python: `PROTOCOL_VERSION = "2024-11-05"`, `SERVER_NAME`, `SERVER_VERSION` (module-level constants in UPPERCASE)

## Code Style

**Formatting:**
- **Elisp:** No automatic formatter configured. Uses Doom Emacs default indentation (2-space). Comments use `;` for single line, `;;` for section headers
- **Lua:** LazyVim defaults; 2-space indentation (set in `nvim/lua/config/options.lua`: `opt.shiftwidth = 2, opt.tabstop = 2`)
- **Python:** No linter/formatter detected. Uses 4-space indentation implicitly (PEP 8 standard)
- **Shell:** Uses 4-space indentation in complex nested structures, though inconsistent in places (see `doom/bin/codex-ediff-mcp-server.py` uses 4-space)

**Indentation:**
- Elisp: 2-space default
- Lua: 2-space (configured in options.lua)
- Python: 4-space (implicit PEP 8)
- Shell: Mixed (4-space in Python scripts, variable in shell scripts)

**Line length:**
- No hard limit enforced. Code files range from 150+ character lines (Lua configs) to wrapped at natural boundaries in Elisp

## Import Organization

**Elisp:**
- No import sorting convention. Packages declared in `doom/packages.el` with recipes
- `use-package!` and `after!` macros manage lazy loading and configuration
- Example from `doom/config.el`:
  ```elisp
  (use-package! helm
    :defer t
    :config
    (setq helm-mini-default-sources '(...)))

  (after! mu4e
    (setq! mu4e-context-policy 'always-ask))
  ```

**Lua:**
- Requires follow this pattern in LazyVim:
  ```lua
  require("config.lazy")
  require("config.options")
  require("flash").jump()
  ```
- Local requires inline where used: `require("dap").continue()`
- Plugins imported via lazy.nvim `return { ... }` table structure

**Python:**
- Standard library imports first, then third-party, then local (implicit Python convention)
- Example from `doom/bin/codex-ediff-mcp-server.py`:
  ```python
  import ast
  import json
  import os
  import subprocess
  import sys
  import tempfile
  from typing import Any, Dict, Optional, Tuple
  ```

## Error Handling

**Elisp patterns:**
- `condition-case` for exception handling: `(condition-case nil (evil-window-right 1) (error (qtile/move-focus "right")))`
- `ignore-errors` for non-critical operations: `(ignore-errors (shell-command "xinput ..."))`
- `run-hook-with-args-until-failure` for hook-based error propagation in `shutdown` and `reboot` functions
- Try-catch via `when` conditions: `(when (system-name= "klingenberg-tablet") ...)`

**Lua patterns:**
- Conditional checks before operations: `if not term_buf then return "" end`
- Guard clauses early: `if not bufname or bufname == "" then ... end`
- No explicit try-catch (Lua patterns used instead): Functions return nil/empty on failure, caller decides how to handle

**Python patterns:**
- Explicit exception handling with context: `process = subprocess.run(..., check=False)` then check `process.returncode`
- Custom `RuntimeError` for semantic failures: `raise RuntimeError(details)`
- Type checking before operations: `if not isinstance(value, dict): raise ValueError(...)`
- Defensive checks with defaults: `arguments.get("old_file_path")` with fallback validation

**Shell patterns:**
- No error handling visible; relies on exit codes implicit in piped commands
- Direct command invocation without error checking: `amixer -D pulse set Master on > /dev/null`

## Logging

**Framework:**
- **Elisp:** Uses `message` function for user-facing output
  - `(message "Made alert for %s at %s" mesg time)`
  - Used in interactive functions to provide feedback
- **Python:** `sys.stderr` for internal MCP server messages; stdout for JSON-RPC communication only
- **Lua:** `vim.notify()` for user-facing notifications with severity levels: `vim.log.levels.WARN`, `vim.log.levels.INFO`
- **Shell:** Uses custom `notify-send.sh` wrapper for desktop notifications

**Patterns:**
- Elisp: Log only user-visible events via `message` in interactive functions
- Python: Direct print to stderr for debugging (internal utility scripts); protocol output to stdout
- Lua: Use `vim.notify()` for status updates, warnings, errors: `vim.notify("Compose prompt...", vim.log.levels.INFO)`
- Shell: Log through system notification daemon with icons and timeouts

## Comments

**When to Comment:**
- **Elisp:** Section headers with `;;; Defining some useful functions`. Inline comments for non-obvious logic
  - Example: `(setq ns-alternate-modifier 'meta)       ; Left Option = Meta`
  - Example: `(setq langtool-java-classpath "/usr/share/...")  ; Path to LanguageTool`
- **Lua:** Inline for keybinding descriptions: `{ desc = "Copy" }`, section headers with `-- ──────` dividers
- **Python:** Module-level docstrings and function docstrings for public APIs
  - Example: `"""Minimal MCP server that forwards Codex diff reviews to Emacs Ediff. This bridge exposes two MCP tools: openDiff, closeAllDiffTabs"""`
- **Shell:** Comments explain conditionals and special cases; minimal inline comments

**JSDoc/TSDoc:**
- Not used in this codebase (no TypeScript/JavaScript component files)
- Python docstrings follow implicit docstring pattern but not strictly documented

## Function Design

**Size:**
- Elisp: Small functions preferred; interactive commands typically 5-15 lines. Example: `my/close-buffer` is 5 lines
- Lua: Inline functions within keybindings are common; extracted for reuse. Example: `grab_terminal_input` is ~40 lines for complexity
- Python: Utility functions 20-50 lines; JSON-RPC handlers can be longer (~50+ lines)

**Parameters:**
- Elisp: Minimal parameters; often use closure variables or `with-current-buffer` for context
- Lua: Functions take single dict argument for keybinding functions: `function() ... end`
- Python: Named parameters with type hints: `def _call_open_diff(arguments: Dict[str, Any]) -> Dict[str, Any]:`

**Return Values:**
- Elisp: Interactive commands return nil (side effects). Utility functions return values implicitly
- Lua: Functions return nil on error, truthy on success. Example: `return nil, nil` or `return b, chan`
- Python: Always return dict with structure matching MCP protocol: `{"jsonrpc": "2.0", "id": message_id, "result": {...}}`

## Module Design

**Exports:**
- **Elisp:** No module system; `defun` exposes publicly, private functions use `(defun-- ...)` or rely on scope
- **Lua:** LazyVim uses `return { ... }` at end of plugin file to export plugin spec table
- **Python:** MCP server uses functions prefixed with `_` for private (not exported): `_read_message`, `_write_message`, `_jsonrpc_result`

**Barrel Files:**
- **Elisp:** `doom/packages.el` is the package declaration manifest; config split across `config.el` (main) and hooks
- **Lua:** Each plugin file in `nvim/lua/plugins/` is independent; loaded by lazy.nvim plugin manager
- **Python:** No barrel pattern; single script exports via MCP tools

## Platform-Specific Code

**Conventions:**
- Use `(eq system-type 'gnu/linux)` for Linux-only Elisp blocks: `(when (eq system-type 'gnu/linux) (defun shutdown () ...))`
- Use `(system-name= "name1" "name2")` predicate to check hostname: `(system-name= "klingenberg-laptop" "klingenberg-pc")`
- Lua: Conditional on `vim.g.neovide` to detect GUI: `if vim.g.neovide then ... end`
- Python/Shell: No platform detection; used only on target platforms

## Configuration Macro Patterns

**Doom Emacs conventions:**
- `setq!` (not `setq`) for defcustom variables: `(setq! doom-theme 'doom-palenight)`
- `use-package!` for lazy-loaded package config
- `after!` for code that runs after a package loads
- `map!` for keybinding definitions
- `add-hook!` for event handlers

## Special Conventions

**Mode-specific:**
- **Evil (Vim mode):** Maps use mode indicators `:n` (normal), `:v` (visual), `:i` (insert), `:nvi` (all)
  - Example: `(map! :n "gb" 'org-mark-ring-goto)` — normal mode only
- **Helm:** Maps use `:map helm-map` to configure completion UI navigation
- **Lispyville (Lisp editing):** Numeric operators like `<`, `>` for structural navigation

---

*Convention analysis: 2026-02-19*
