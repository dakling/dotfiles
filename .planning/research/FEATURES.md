# Features Research: Claude Code Emacs Multiplexing

## Table Stakes

Features that must work or the integration is broken.

### Pane Creation
- **Each subagent gets a vterm buffer** — when Claude Code spawns an agent, a new vterm buffer appears in Emacs
- Complexity: Low (existing implementation handles this)
- Dependency: Environment injection working correctly

### Environment Propagation
- **Env vars pass from main session to subagents** — `$TMUX`, `$TMUX_PANE`, `$CLAUDE_CODE_EMACS_PANES`, shim `$PATH`
- Complexity: Medium (the known bug area)
- Dependency: Package installation path resolving correctly

### Shim Interception
- **tmux commands redirect to emacsclient** — all tmux subcommands that Claude Code uses must be intercepted
- Complexity: Medium (need to verify against current Claude Code version)
- Dependency: Shim on PATH, emacsclient socket available

### Fresh Session Reliability
- **Works on first launch after doom sync** — no manual setup steps required beyond config
- Complexity: High (this is the core bug to fix)
- Dependency: Path resolution, server startup timing, `:files` directive

### Basic Navigation
- **Switch between agent panes** — next/prev/select commands
- Complexity: Low (existing implementation)
- Dependency: Pane registry tracking correctly

## Differentiators

Nice-to-have features that improve UX significantly.

### Agent Status Indicators
- **Header-line shows agent name and status** — running/finished/errored via color-coded indicator dot
- Complexity: Low (partially implemented via `set-pane-info`)
- Dependency: Claude Code sending title/status info via tmux select-pane

### Auto-Close Finished Panes
- **Completed agent buffers close automatically** — after process exits, buffer is cleaned up
- Complexity: Low-Medium (need process sentinel + timer)
- Dependency: Reliable process exit detection

### Diagnostic Command
- **`M-x claude-code-emacs-panes-diagnose`** — checks server running, shim path exists, shim is executable, env vars set
- Complexity: Low
- Dependency: None

### Debug Log Viewer
- **View shim debug log from Emacs** — open `/tmp/claude-emacs-panes.log` with auto-refresh
- Complexity: Low
- Dependency: Debug logging enabled in shim

### Robust Error Reporting
- **Shim errors surface in Emacs** — instead of silently failing (current behavior: stderr → /dev/null)
- Complexity: Medium (need error channel from shim back to Emacs)
- Dependency: IPC reliability

## Anti-Features

Things to deliberately NOT build.

### Complex Layout Management
- Don't build a tiling window manager inside Emacs — let the user's existing window management handle it
- `display-buffer` with sensible defaults is sufficient

### Agent Spawning UI
- Don't build UI for creating/managing agents — Claude Code handles this
- We only intercept and display, never initiate

### Persistent Session State
- Don't try to save/restore agent pane state across Emacs restarts — agent processes won't survive anyway

### Custom Terminal Emulator
- Don't replace vterm — it's battle-tested and handles all terminal escape sequences

## Feature Dependencies

```
Environment Propagation → Shim Interception → Pane Creation → Navigation
                                                            → Status Indicators
                                                            → Auto-Close
Fresh Session Reliability → (encompasses all above)
Diagnostic Command → (standalone, no dependencies)
```

## Complexity Summary

| Feature | Complexity | Priority |
|---------|-----------|----------|
| Fresh session fix | High | P0 |
| Environment propagation fix | Medium | P0 |
| Shim interception verification | Medium | P0 |
| Auto-close finished panes | Low-Medium | P1 |
| Diagnostic command | Low | P1 |
| Status indicators | Low | P1 |
| Error reporting improvements | Medium | P2 |
| Debug log viewer | Low | P2 |
