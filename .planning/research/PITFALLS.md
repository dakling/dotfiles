# Pitfalls Research: Claude Code Emacs Multiplexing

## Critical Pitfalls

### 1. Shim Path Not Found in Production Install

**The Problem**: `load-file-name` resolves to straight.el's build directory, not the source repo. The `bin/` directory may not be copied there, or may lose execute permissions.

**Warning Signs**:
- Feature works when loading package manually (`load-file`)
- Feature breaks after `doom sync` or fresh Emacs start
- Debug log at `/tmp/claude-emacs-panes.log` is empty (shim never called)

**Prevention**:
1. Verify `:files` directive actually copies `bin/` to build dir
2. Add a check in `setup` that verifies `(file-executable-p (expand-file-name "tmux" (claude-code-emacs-panes--shim-dir)))`
3. If shim not found in build dir, fall back to source repo path
4. Add `doom doctor` integration to catch this at config time

**Phase**: Phase 1 (core fix)

### 2. Silent emacsclient Failures

**The Problem**: The shim redirects stderr to `/dev/null` (`2>/dev/null`). When emacsclient fails (wrong socket, server not running, eval error), the shim returns empty/nil but Claude Code may interpret this as success or get confused by unexpected output.

**Warning Signs**:
- Shim log shows `emacs_eval: (create-pane) -> ` (empty result)
- Claude Code reports "pane created" but no vterm buffer appears
- Intermittent failures that are hard to reproduce

**Prevention**:
1. Capture stderr and log it: `result=$(emacsclient ... 2>>"$LOG_FILE")`
2. Check for emacsclient exit code
3. Return meaningful error codes to Claude Code on failure
4. Add health-check function: `emacs_eval "(+ 1 1)"` should return `2`

**Phase**: Phase 1 (diagnostic), Phase 2 (full error handling)

### 3. Emacs Server Not Running at Advice Time

**The Problem**: `claude-code-emacs-panes-setup` calls `(server-start)` but this may not succeed if another Emacs instance holds the socket, or if called too early in Doom's init sequence.

**Warning Signs**:
- `(server-running-p)` returns nil after setup
- emacsclient refuses connection
- Feature works if you manually run `M-x server-start` after init

**Prevention**:
1. Check `(server-running-p)` after `(server-start)` and warn if nil
2. Use `after-init-hook` or Doom's `doom-after-init-hook` for setup timing
3. Support specifying a custom server name to avoid conflicts

**Phase**: Phase 1

### 4. `--teammate-mode tmux` Flag Removal

**The Problem**: The setup function injects `--teammate-mode tmux` into `claude-code-ide-cli-extra-flags`. This flag doesn't appear in Claude Code v2.1.47 `--help`. If Claude Code rejects unknown flags, the whole session may fail to start.

**Warning Signs**:
- Claude Code exits immediately with an error about unknown flags
- Claude Code starts but ignores the flag (agents don't use tmux)
- `claude --help` doesn't list `--teammate-mode`

**Prevention**:
1. Test whether Claude Code v2.1.47 accepts or rejects this flag
2. If rejected: remove it from setup, rely on `$TMUX` env var detection alone
3. If ignored: same — Claude Code likely auto-detects tmux via `$TMUX`
4. Add version-checking to conditionally include the flag

**Phase**: Phase 1 (must verify before anything else)

### 5. Claude Code tmux Command Pattern Changes

**The Problem**: Claude Code may change which tmux subcommands it uses, or change argument patterns. The shim must handle exactly what Claude Code sends.

**Warning Signs**:
- Unknown subcommand entries in debug log
- Agents fail to spawn with errors about tmux commands
- Claude Code works fine outside Emacs (with real tmux)

**Prevention**:
1. Log ALL unhandled commands in the shim (current: silent exit 0)
2. Periodically check Claude Code's tmux usage patterns
3. Keep the catch-all `*) exit 0` handler but make it log loudly
4. Test with real Claude Code team operations, not just unit tests

**Phase**: Phase 1 (verification), ongoing

### 6. Environment Variable Leakage

**The Problem**: The injected env vars (`TMUX`, `PATH` with shim dir) propagate to ALL child processes of the Claude Code session, not just subagents. This could interfere with other tools that check for tmux or use PATH.

**Warning Signs**:
- Other tools behave differently inside the Claude Code session
- `tmux` commands in shell prompts hit the shim instead of real tmux
- Git operations or other tools unexpectedly slow (hitting emacsclient for tmux checks)

**Prevention**:
1. The shim's guard clause (`if [[ -z "$CLAUDE_CODE_EMACS_PANES" ]]`) falls through to real tmux — this is already in place
2. But verify the fallthrough works: the shim must correctly find and exec the real tmux
3. Consider more targeted env injection (only for Claude's process, not shell subprocesses)

**Phase**: Phase 2

### 7. vterm Buffer/Process Lifecycle Mismatch

**The Problem**: vterm buffers and their processes have independent lifecycles. A killed buffer doesn't always kill the process, and a finished process doesn't always kill the buffer.

**Warning Signs**:
- Zombie processes accumulating (check `list-processes`)
- "Buffer has a running process" prompts when closing Emacs
- Registry shows panes as "running" but they're actually finished

**Prevention**:
1. `vterm-kill-buffer-on-exit` is set to nil (current) — good, prevents premature kills
2. Process sentinel marks pane as `:finished` (current) — good
3. Add periodic cleanup: scan registry, remove entries with dead buffers
4. Auto-close: timer-based buffer kill after process exit (user requested)

**Phase**: Phase 2

### 8. Race Condition in Pane ID Management

**The Problem**: Both the shim and Emacs maintain pane ID counters. If multiple agents spawn simultaneously, IDs could collide or get out of sync.

**Warning Signs**:
- `send-keys` goes to wrong pane
- `kill-pane` kills wrong buffer
- Duplicate pane IDs in registry

**Prevention**:
1. Emacs is the source of truth for pane IDs (it generates them in `create-pane`)
2. Shim stores the returned ID and uses it for subsequent commands
3. Ensure `cl-incf` on `--next-id` is atomic (it is in single-threaded Emacs)
4. Verify shim correctly captures the pane-id from emacs_eval output

**Phase**: Phase 1 (verify), Phase 2 (harden)

## Summary

| Pitfall | Severity | Phase |
|---------|----------|-------|
| Shim path not found | Critical | 1 |
| Silent emacsclient failures | Critical | 1-2 |
| Server not running | High | 1 |
| `--teammate-mode` removal | High | 1 |
| tmux command changes | Medium | 1+ |
| Env var leakage | Medium | 2 |
| Buffer/process lifecycle | Medium | 2 |
| Pane ID races | Low | 1-2 |
