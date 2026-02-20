# Phase 1: Core Reliability — Research

**Completed:** 2026-02-20

## Source Code Analysis

### Package Structure
- **Location:** `~/code/emacs-packages/claude-code-emacs-panes/`
- **Files:** `claude-code-emacs-panes.el` (382 lines), `bin/tmux` (484 lines)
- **GitHub:** `dakling/claude-code-emacs-panes`

### straight.el Build State (current)
- **Repos dir:** `~/.config/emacs/.local/straight/repos/claude-code-emacs-panes/` — has `bin/tmux` with `-rwxr-xr-x`
- **Build dir:** `~/.config/emacs/.local/straight/build-30.2/claude-code-emacs-panes/`
  - `.el` file is a **symlink** to repos dir
  - `.elc` compiled file exists
  - `bin/` dir exists with `tmux` as **symlink** to repos `bin/tmux`
- **Key insight:** straight.el creates the bin/ symlink in build dir ONLY because the `:files` recipe explicitly includes `"bin"`. Without it, bin/ would be missing.

### packages.el Entry (current — commented out)
```elisp
;; (package! claude-code-emacs-panes
;;   :recipe (:host github :repo "dakling/claude-code-emacs-panes"
;;            :files ("claude-code-emacs-panes.el" "bin")))
```

## Requirement-by-Requirement Findings

### CORE-01: Shim Path Resolution

**Current implementation:**
```elisp
(defvar claude-code-emacs-panes--package-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Captured at load time because load-file-name is nil at runtime.")

(defun claude-code-emacs-panes--shim-dir ()
  (expand-file-name "bin" claude-code-emacs-panes--package-dir))
```

**Analysis:**
- Uses `load-file-name` at load time → resolves to build dir when loaded via straight.el
- Build dir has `bin/tmux` as symlink → path resolution works IF the `:files` recipe includes `"bin"`
- **Risk:** If `load-file-name` resolves through the symlink to repos dir, the `bin/` path is still valid (repos also has bin/)
- **Risk:** The `:files` spec must include `"bin"` (not just `"bin/tmux"`) to get the directory linked
- **Status:** Current code is sound. The issue is ensuring the `:files` recipe is correct and the package is loaded.

**Auto-fix implementation (from CONTEXT.md decisions):**
- Search order for shim: (1) package-dir/bin/tmux, (2) repos dir, (3) source dev dir
- If found but not executable → `chmod +x`
- If not found at primary location → copy from alternate location

### CORE-02: Environment Variable Propagation

**Current implementation:**
```elisp
(defun claude-code-emacs-panes--env-vars ()
  (list (format "PATH=%s:%s" shim-dir (getenv "PATH"))
        "TMUX=emacs-panes,0,0"
        "TMUX_PANE=%0"
        "CLAUDE_CODE_EMACS_PANES=1"
        "CLAUDE_CODE_EMACS_PANES_DEBUG=1"
        (format "CLAUDE_CODE_EMACS_PANES_PID=%s" pid-tag)
        (format "EMACS_PANES_SERVER=%s" (or server-name "server"))))
```

**Injection mechanism:**
- `claude-code-emacs-panes--inject-env` is advice around `claude-code-ide--start-session`
- Sets both `vterm-environment` (for vterm) and `process-environment` (for eat)
- **How it propagates to subagents:** Claude Code spawns in vterm with these env vars → inherits them → when Claude Code spawns teammates, it calls `tmux split-window` → our shim intercepts → creates new vterm pane. The new pane also needs these env vars.

**Gap found:** When the shim creates a new pane via `emacsclient --eval (claude-code-emacs-panes-create-pane)`, the new vterm buffer gets a fresh shell WITHOUT the injected env vars. The new pane's process won't have PATH pointing to the shim, TMUX, etc. This means nested agent spawning (agent spawning sub-agents) would fail.

**Fix needed:** The `create-pane` function should inject env vars into the new vterm buffer's environment, OR the shim should pass env vars when sending commands to the new pane.

### CORE-03: Tmux Subcommand Coverage

**Claude Code v2.1.47 uses exactly these 13 tmux subcommands** (confirmed by binary string analysis):
1. display-message
2. has-session
3. kill-pane
4. list-panes
5. list-windows
6. new-session
7. new-window
8. resize-pane
9. select-layout
10. select-pane
11. send-keys
12. set-option
13. split-window

**Shim coverage:** All 13 are handled. The catch-all `*)` case also handles any unknown subcommands as no-ops.

**Status:** COMPLETE — no gaps.

### CORE-04: --teammate-mode Flag Compatibility

**Finding: --teammate-mode EXISTS in v2.1.47** (hidden from --help):
```javascript
// From binary analysis:
R.addOption(new a7("--teammate-mode <mode>",
  'How to spawn teammates: "tmux", "in-process", or "auto"')
  .choices(["auto","tmux","in-process"])
  .hideHelp())
```

- Valid values: `"auto"`, `"tmux"`, `"in-process"`
- Default: `"auto"` (falls back to config `teammateMode` or `"auto"`)
- Hidden flag — not shown in `claude --help` but fully functional
- When `--teammate-mode tmux` is passed, Claude forces tmux backend for teammates

**Teammate mode resolution (from binary):**
```javascript
function OKB() {
  if (hcT) _$T = hcT;  // CLI override takes precedence
  else _$T = LR().teammateMode ?? "auto";  // Config or default
}
```

**Auto mode behavior:**
- Checks `process.env.TMUX` to detect if inside tmux
- Since we set `TMUX=emacs-panes,0,0`, auto mode WILL select tmux backend
- So `--teammate-mode tmux` is technically redundant but harmless

**Recommendation:** Keep `--teammate-mode tmux` for explicitness. It's supported, not deprecated, just hidden. This ensures Claude Code always uses tmux backend regardless of future "auto" logic changes.

### CORE-05: Fresh Session Reliability

**Current setup function:**
```elisp
(defun claude-code-emacs-panes-setup ()
  (unless (server-running-p) (server-start))
  (advice-add 'claude-code-ide--start-session :around
              #'claude-code-emacs-panes--inject-env)
  (when (boundp 'claude-code-ide-cli-extra-flags)
    (let ((existing (or claude-code-ide-cli-extra-flags "")))
      (unless (string-match-p "--teammate-mode" existing)
        (setq claude-code-ide-cli-extra-flags
              (string-trim (concat existing " --teammate-mode tmux")))))))
```

**Issues for fresh sessions:**
1. Package is commented out in packages.el → not loaded at all
2. No `after!` or `use-package!` in doom config to call `setup`
3. If loaded but setup not called, advice isn't added → env vars not injected
4. `server-start` may fail silently if socket already exists

**Fix needed:** When package is enabled (Phase 3), need proper doom config with `(after! claude-code-ide (claude-code-emacs-panes-setup))`.

## Claude's Discretion Recommendations

### Auto-fix search order for shim
1. `claude-code-emacs-panes--package-dir/bin/tmux` (primary — where straight.el builds)
2. `~/.config/emacs/.local/straight/repos/claude-code-emacs-panes/bin/tmux` (repos fallback)
3. `~/code/emacs-packages/claude-code-emacs-panes/bin/tmux` (dev source fallback)

### Internal error logging
- Use `(message "claude-code-emacs-panes: %s" msg)` for warnings → goes to *Messages* per context decision
- Use `(warn ...)` only for critical failures (shim not found after auto-fix attempt)

### Version targeting
- Target v2.1.47+ only. Don't try to support older versions.
- `--teammate-mode tmux` is valid and should be kept.

## Critical Findings Summary

| Requirement | Status | Action Needed |
|-------------|--------|---------------|
| CORE-01 | Code is sound | Add auto-fix logic, verify `:files` recipe |
| CORE-02 | **GAP FOUND** | New panes lack env vars; create-pane must inject them |
| CORE-03 | Complete | All 13 subcommands handled |
| CORE-04 | Flag is valid | Keep `--teammate-mode tmux`; it's hidden but supported |
| CORE-05 | Not wired up | Package commented out; needs doom config integration |

## Risks

1. **Env propagation gap (CORE-02)** — Most critical bug. New vterm panes from `create-pane` don't inherit the injected env vars, breaking nested agent spawning.
2. **Emacs server socket name** — If `server-name` differs between main session and what the shim expects, `emacsclient --socket-name` calls fail silently (stderr goes to /dev/null).
3. **Race condition** — Multiple `emacsclient --eval` calls arriving simultaneously could conflict if Emacs serializes them.
