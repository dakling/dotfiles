# Phase 1: Core Reliability - Context

**Gathered:** 2026-02-19
**Status:** Ready for planning

<domain>
## Phase Boundary

Fix fresh-session bugs so the tmux shim, environment propagation, and flag compatibility work reliably when the package is installed via straight.el. Requirements CORE-01 through CORE-05. No new features — just make the existing prototype work.

</domain>

<decisions>
## Implementation Decisions

### Failure behavior
- Fall back to native tmux + warn in *Messages* buffer when pane creation fails (shim not found, wrong permissions, env missing)
- Best-effort on partial env: try to create pane with whatever env vars are available, only fall back to native tmux if it actually fails
- After first failure, latch into fallback mode for the rest of the Emacs session (no repeated retry/error noise)
- Warnings go to standard *Messages* buffer — non-intrusive but visible

### Startup checks
- Validate shim path and permissions on first agent spawn, not on Emacs startup (lazy check)
- Auto-fix with notification: if shim is missing or not executable, attempt to locate it from known paths (source dir, straight.el build dir) and fix permissions, then notify user what was done
- Auto-fix covers both permissions (chmod +x) and path resolution (find/copy shim from known locations)
- After first-spawn validation passes (or auto-fixes), skip checks for the rest of the session — consistent with the latch-into-fallback decision

### Claude's Discretion
- Version targeting strategy for Claude Code v2.1.47+ (how to handle --teammate-mode flag)
- Specific tmux subcommands to intercept
- Internal error logging mechanism
- Exact auto-fix search order for shim locations

</decisions>

<specifics>
## Specific Ideas

- The latch pattern: check once on first spawn, commit to the result (working or fallback) for the session. This keeps the design simple and predictable.
- Auto-fix should handle the core CORE-01 scenario: straight.el build dir doesn't have the shim or loses execute permissions after doom sync.

</specifics>

<deferred>
## Deferred Ideas

- Diagnostic command (M-x claude-code-emacs-panes-diagnose) — tracked as OBSV-02 in v2 requirements
- Shim errors as Emacs notifications — tracked as OBSV-04 in v2 requirements
- Dedicated log buffer for pane events — could be useful but not needed for core reliability

</deferred>

---

*Phase: 01-core-reliability*
*Context gathered: 2026-02-19*
