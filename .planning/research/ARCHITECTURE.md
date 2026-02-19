# Architecture Research: Claude Code Emacs Multiplexing

## Component Overview

```
┌─────────────────────────────────────┐
│         Emacs (Doom)                │
│  ┌───────────────────────────────┐  │
│  │ claude-code-emacs-panes.el    │  │
│  │  - Pane registry (hash table) │  │
│  │  - vterm buffer management    │  │
│  │  - Navigation commands        │  │
│  │  - Process sentinels          │  │
│  │  - Env injection (advice)     │  │
│  └─────────────▲─────────────────┘  │
│                │ emacsclient --eval  │
│  ┌─────────────┴─────────────────┐  │
│  │ Emacs Server (IPC endpoint)   │  │
│  └─────────────▲─────────────────┘  │
│                │                    │
│  ┌─────────────┴─────────────────┐  │
│  │ Main Claude Code session      │  │
│  │ (vterm buffer)                │  │
│  │  - Env: TMUX=emacs-panes     │  │
│  │  - Env: PATH=shim_dir:...    │  │
│  └─────────────┬─────────────────┘  │
│                │ spawns              │
│  ┌─────────────┴─────────────────┐  │
│  │ Subagent processes            │  │
│  │  - Inherit env from parent    │  │
│  │  - Call "tmux" (finds shim)   │  │
│  └─────────────┬─────────────────┘  │
│                │                    │
│  ┌─────────────┴─────────────────┐  │
│  │ bin/tmux (shim script)        │  │
│  │  - Intercepts tmux commands   │  │
│  │  - Calls emacsclient --eval   │  │
│  │  - Falls through to real tmux │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
```

## Data Flow

### 1. Session Startup
```
User calls claude-code-ide (or claude-code-emacs-panes-start-claude)
  → advice fires: claude-code-emacs-panes--inject-env
    → sets vterm-environment with:
        PATH=<shim_dir>:$PATH
        TMUX=emacs-panes,0,0
        TMUX_PANE=%0
        CLAUDE_CODE_EMACS_PANES=1
        EMACS_PANES_SERVER=<server-name>
    → also sets process-environment (for eat backend)
  → claude-code-ide--start-session creates vterm
    → vterm inherits env vars
      → Claude Code binary starts with fake TMUX env
```

### 2. Agent Spawning
```
Claude Code decides to spawn subagent (Task tool)
  → Claude Code checks $TMUX → finds "emacs-panes,0,0"
  → Calls: tmux split-window -t ... -P -F "#{pane_id}"
    → PATH lookup finds shim at <shim_dir>/tmux
      → Shim checks $CLAUDE_CODE_EMACS_PANES=1
      → Shim calls: emacsclient --socket-name $EMACS_PANES_SERVER \
                     --eval '(claude-code-emacs-panes-create-pane)'
        → Emacs creates vterm buffer
        → Returns pane-id string
      → Shim outputs pane-id to stdout
    → Claude Code captures pane-id for future send-keys calls
```

### 3. Agent Communication
```
Claude Code sends command to agent pane:
  → tmux send-keys -t <pane-id> "command" Enter
    → Shim parses target pane and command
    → emacsclient --eval '(claude-code-emacs-panes-send-keys "pane-id" "command")'
      → Emacs finds buffer in registry
      → vterm-send-string + vterm-send-return
```

### 4. Agent Cleanup
```
Agent process exits:
  → vterm process sentinel fires (set in create-pane)
  → Marks pane as :finished in registry
  → (With auto-close: timer kills buffer after delay)

OR Claude Code explicitly kills:
  → tmux kill-pane -t <pane-id>
    → Shim calls: emacsclient --eval '(claude-code-emacs-panes-kill-pane "pane-id")'
    → Buffer killed, registry entry removed
```

## Fresh-Session Failure Analysis

### Root Cause Hypothesis

The `claude-code-emacs-panes--package-dir` variable captures the directory at **load time**:

```elisp
(defvar claude-code-emacs-panes--package-dir
  (file-name-directory (or load-file-name buffer-file-name)))
```

**During development**: `load-file-name` → `~/code/emacs-packages/claude-code-emacs-panes/claude-code-emacs-panes.el`
→ `--package-dir` → `~/code/emacs-packages/claude-code-emacs-panes/`
→ `--shim-dir` → `~/code/emacs-packages/claude-code-emacs-panes/bin/`
→ `bin/tmux` EXISTS ✓

**After doom sync (straight.el)**: `load-file-name` → `~/.config/emacs/.local/straight/build-{ver}/claude-code-emacs-panes/claude-code-emacs-panes.el`
→ `--package-dir` → `~/.config/emacs/.local/straight/build-{ver}/claude-code-emacs-panes/`
→ `--shim-dir` → `~/.config/emacs/.local/straight/build-{ver}/claude-code-emacs-panes/bin/`
→ `bin/tmux` MAY NOT EXIST if `:files` directive doesn't copy it ✗

### Verification Steps
1. Check if `bin/` directory exists in straight.el build output
2. Verify `:files` directive in packages.el includes `"bin"` (it does)
3. Check if `bin/tmux` has execute permission after straight.el copies it
4. Test `emacsclient --socket-name server --eval "(+ 1 1)"` in fresh session

### Secondary Causes
- **Server not started**: `(server-start)` in setup might race with package loading
- **Socket name mismatch**: If Doom uses a custom server name vs default "server"
- **Shim not executable**: straight.el may not preserve file permissions on copy

## Suggested Build Order

### Phase 1: Diagnose & Fix (Core Bug)
1. Add diagnostic command to verify all components
2. Fix shim path resolution (use repo path as fallback, or absolute path)
3. Fix emacsclient error handling (don't swallow errors)
4. Verify against Claude Code v2.1.47 tmux detection
5. Test in fresh session

### Phase 2: Robustness & Observability
1. Auto-close finished panes
2. Better status indicators
3. Shim error reporting back to Emacs
4. emacsclient retry logic

### Phase 3: Polish & Ship
1. Uncomment in config
2. Push package updates to GitHub
3. doom sync + verify
4. Final fresh-session test
