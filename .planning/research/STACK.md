# Stack Research: Claude Code Emacs Multiplexing

## Core Stack

### Emacs Lisp Package
- **Emacs 28.1+** with lexical-binding
- **vterm** (0.0.1+) — terminal emulation for agent panes
- **Doom Emacs** package management via straight.el
- **Emacs server** — IPC backbone for shim-to-Emacs communication

### Shell Shim
- **Bash** script at `bin/tmux` intercepting ~15 tmux subcommands
- **emacsclient** for calling back into Emacs from the shim
- PATH manipulation to shadow real tmux binary

### Claude Code Integration
- **claude-code-ide.el** — existing integration (active, working)
- **claude-code-ide-cli-extra-flags** — mechanism for injecting CLI flags
- Advice on `claude-code-ide--start-session` for environment injection

## Claude Code Agent Spawning (v2.1.47)

### How Subagents Work
Claude Code's Task tool spawns subagent processes as child processes of the main Claude Code process. Key behaviors:
- **Subagents inherit environment** from the parent process
- **tmux is used** when Claude Code detects it's running inside a tmux session (checks `$TMUX` env var)
- When in tmux, Claude Code uses `tmux split-window`, `tmux send-keys`, etc. to create visible panes for teammates
- **`--teammate-mode tmux`** flag: NOT in current `--help` output. May have been removed or internalized. Need to verify if Claude Code auto-detects tmux via `$TMUX` env var instead.

### Environment Detection
Claude Code checks these env vars to determine its execution context:
- `$TMUX` — if set, assumes tmux is available for pane management
- `$TMUX_PANE` — current pane identifier
- The shim sets both: `TMUX=emacs-panes,0,0` and `TMUX_PANE=%0`

### Implication for Shim Approach
The tmux shim approach **should still be viable** because:
1. Claude Code detects tmux via `$TMUX` env var (not via `--teammate-mode` flag)
2. The shim intercepts all tmux commands before they reach the real binary
3. Environment inheritance ensures subagents also see the fake `$TMUX`

**Risk**: If Claude Code changed its tmux detection or command patterns, the shim may miss interceptions.

## Emacsclient IPC

### Socket Resolution
- `emacsclient --socket-name SERVER_NAME` — connects to named Emacs server
- Default server name is `"server"` — the shim uses `$EMACS_PANES_SERVER` which defaults to this
- **macOS socket location**: `/tmp/emacs{UID}/` directory
- **Critical**: Doom Emacs may not auto-start the server; `claude-code-emacs-panes-setup` calls `(server-start)` but timing matters

### Reliability Patterns
- Always use `--socket-name` (not `--server-file`) for local IPC
- Redirect stderr to capture errors: `emacsclient --eval "..." 2>&1`
- Current shim redirects stderr to `/dev/null` — **this hides failures silently**
- Timeout consideration: `emacsclient` has no built-in timeout; long-running evals can block

## Package Path Resolution

### straight.el Installation
When installed via GitHub recipe, straight.el clones to:
```
~/.config/emacs/.local/straight/repos/claude-code-emacs-panes/
```
Build output goes to:
```
~/.config/emacs/.local/straight/build-{version}/claude-code-emacs-panes/
```

### `load-file-name` Capture
The package captures its directory at load time:
```elisp
(defvar claude-code-emacs-panes--package-dir
  (file-name-directory (or load-file-name buffer-file-name)))
```
- When loaded from straight.el build dir: points to build dir (which may NOT contain `bin/`)
- When loaded during development: points to source repo (which contains `bin/`)
- **This is the most likely cause of the fresh-session bug** — `bin/tmux` exists in the repo but not in the build dir

### Fix Strategy
straight.el's `:files` directive in the package recipe controls what gets copied to the build dir:
```elisp
:recipe (:host github :repo "dakling/claude-code-emacs-panes"
         :files ("claude-code-emacs-panes.el" "bin"))
```
The `"bin"` entry should copy the bin directory, but verify this actually works.

## Confidence Levels

| Component | Confidence | Notes |
|-----------|-----------|-------|
| vterm for panes | High | Proven, widely used |
| emacsclient IPC | High | Standard Emacs pattern |
| tmux shim approach | Medium | Depends on Claude Code's tmux detection not changing |
| PATH shim interception | Medium | Works but fragile across package managers |
| straight.el bin/ bundling | Low | Needs verification — likely root cause of bug |
| `--teammate-mode tmux` | Low | Flag missing from CLI help, may be deprecated |
