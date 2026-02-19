# Research Summary: Claude Code Emacs Multiplexing

## Stack Recommendation

**Keep existing stack** — vterm + emacsclient IPC + tmux shim is the right approach:
- vterm is the standard Emacs terminal emulator, battle-tested
- emacsclient is the standard IPC mechanism for external-to-Emacs communication
- The tmux shim intercepts Claude Code's native tmux management transparently

**Key verification needed**: Claude Code v2.1.47 no longer shows `--teammate-mode tmux` in `--help`. The `$TMUX` environment variable detection is likely sufficient — Claude Code checks `$TMUX` to decide whether to use tmux commands. This needs testing.

## Table Stakes Features

1. **Pane creation** — each subagent gets its own vterm buffer
2. **Environment propagation** — env vars pass correctly to all subagent processes
3. **Shim interception** — tmux commands redirect to emacsclient calls
4. **Fresh session reliability** — works after doom sync, not just during development

## Key Findings

### Root Cause of Fresh-Session Bug (High Confidence)
The `claude-code-emacs-panes--package-dir` variable captures `load-file-name` at load time. When installed via straight.el, this points to the build directory, which may NOT contain the `bin/tmux` shim (or it may lose execute permissions). During development, it correctly points to `~/code/emacs-packages/claude-code-emacs-panes/` where `bin/tmux` exists.

### Silent Failure Pattern
The tmux shim redirects all stderr to `/dev/null`. When emacsclient fails (wrong socket, server not running), the failure is invisible. Claude Code gets empty responses and proceeds without creating panes.

### `--teammate-mode tmux` Flag
This flag is NOT in Claude Code v2.1.47 `--help`. It may be rejected (breaking session start) or silently ignored. The setup function injects this flag, which needs to be verified and potentially removed.

## Critical Pitfalls

1. **Shim path resolution** — straight.el build dir may not contain `bin/tmux`
2. **Silent failures** — stderr swallowed, no error feedback
3. **Server timing** — `(server-start)` in setup may race with Doom init
4. **Flag compatibility** — `--teammate-mode tmux` may not exist anymore
5. **Command pattern drift** — Claude Code may change which tmux commands it uses

## Architecture Decision

The existing 3-layer architecture is sound:
```
Elisp (pane registry + vterm) ← emacsclient → Shim (tmux interceptor) ← Claude Code (agent spawner)
```

No architectural changes needed. The fix is primarily:
1. Make shim path resolution robust (fallback strategies)
2. Add error visibility (capture stderr, diagnostic commands)
3. Verify Claude Code v2.1.47 compatibility
4. Add auto-close for finished panes

## Suggested Phase Structure

| Phase | Focus | Priority |
|-------|-------|----------|
| 1 | Diagnose & fix core bug (path, server, flags) | P0 |
| 2 | Robustness & observability improvements | P1 |
| 3 | Re-enable in config, push to GitHub, verify | P1 |
