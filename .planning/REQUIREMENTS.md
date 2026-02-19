# Requirements: Claude Code Emacs Panes

**Defined:** 2026-02-19
**Core Value:** When Claude Code spawns subagents, each one appears as a live vterm buffer in Emacs that I can observe and navigate between.

## v1 Requirements

Requirements for initial release. Each maps to roadmap phases.

### Core Integration

- [ ] **CORE-01**: Shim path resolves correctly when package installed via straight.el (not just during development)
- [ ] **CORE-02**: Environment variables (PATH, TMUX, TMUX_PANE, CLAUDE_CODE_EMACS_PANES, EMACS_PANES_SERVER) propagate from main session to all subagent processes
- [ ] **CORE-03**: Tmux shim intercepts all tmux subcommands that Claude Code v2.1.47+ uses for agent management
- [ ] **CORE-04**: Package is compatible with Claude Code v2.1.47+ (verify/remove --teammate-mode tmux flag if deprecated)
- [ ] **CORE-05**: Feature works reliably in fresh Emacs sessions after doom sync without manual setup

### Agent Lifecycle

- [ ] **LIFE-01**: Each subagent spawned by Claude Code creates a new vterm buffer in Emacs
- [ ] **LIFE-02**: User can navigate between agent panes (next/prev/select commands)
- [ ] **LIFE-03**: Finished agent panes auto-close when their process exits
- [ ] **LIFE-04**: Header-line shows agent name and status (running/finished) with color indicator

### Observability

- [ ] **OBSV-01**: Emacsclient errors are captured and logged (not silently discarded to /dev/null)

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Observability

- **OBSV-02**: Diagnostic command (`M-x claude-code-emacs-panes-diagnose`) checks server, shim path, env vars
- **OBSV-03**: Debug log viewer opens `/tmp/claude-emacs-panes.log` from Emacs with auto-refresh
- **OBSV-04**: Shim errors surface as Emacs notifications (not just log entries)

### UX Polish

- **UX-01**: Dashboard view shows all panes in tabulated list with status
- **UX-02**: Show-all command arranges agent panes side-by-side

## Out of Scope

| Feature | Reason |
|---------|--------|
| Tiling layout management | User's window manager handles layout; display-buffer defaults are sufficient |
| Agent spawning UI | Claude Code handles agent creation; we only intercept and display |
| Persistent session state | Agent processes don't survive Emacs restarts |
| Neovim integration | Emacs-only feature |
| Custom terminal emulator | vterm is battle-tested; no need to replace |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| CORE-01 | Phase 1 | Pending |
| CORE-02 | Phase 1 | Pending |
| CORE-03 | Phase 1 | Pending |
| CORE-04 | Phase 1 | Pending |
| CORE-05 | Phase 1 | Pending |
| LIFE-01 | Phase 2 | Pending |
| LIFE-02 | Phase 2 | Pending |
| LIFE-03 | Phase 2 | Pending |
| LIFE-04 | Phase 2 | Pending |
| OBSV-01 | Phase 2 | Pending |

**Coverage:**
- v1 requirements: 10 total
- Mapped to phases: 10
- Unmapped: 0

---
*Requirements defined: 2026-02-19*
*Last updated: 2026-02-19 after roadmap creation*
