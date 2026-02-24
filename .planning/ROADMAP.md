# Roadmap: Claude Code Emacs Panes

## Overview

A working prototype exists but fails in fresh Emacs sessions. The roadmap moves through three phases: diagnose and fix the core reliability bugs, complete the agent lifecycle and observability features, then wire the package into the dotfiles config and ship it to GitHub as a usable package.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [x] **Phase 1: Core Reliability** - Fix fresh-session bugs (path resolution, env propagation, flag compatibility)
- [x] **Phase 2: Agent Lifecycle** - Complete pane creation, navigation, auto-close, status display, error logging
- [ ] **Phase 3: Ship** - Enable in config, push to GitHub, verify end-to-end in production dotfiles

## Phase Details

### Phase 1: Core Reliability
**Goal**: The package works reliably in a fresh Emacs session installed via straight.el
**Depends on**: Nothing (first phase)
**Requirements**: CORE-01, CORE-02, CORE-03, CORE-04, CORE-05
**Success Criteria** (what must be TRUE):
  1. After `doom sync` and a fresh Emacs launch, spawning a subagent creates a vterm pane without any manual setup steps
  2. The bin/tmux shim is found at the correct path regardless of whether the package was loaded from source or straight.el build dir
  3. Environment variables (TMUX, TMUX_PANE, CLAUDE_CODE_EMACS_PANES, EMACS_PANES_SERVER) are present in the subagent process environment
  4. Claude Code v2.1.47+ agent spawning works (no errors from deprecated --teammate-mode flag or unrecognized tmux subcommands)
**Plans**: 2 plans
- [x] 01-01-PLAN.md — Core reliability fixes (shim auto-fix, env propagation, compatibility verification)
- [ ] 01-02-PLAN.md — Gap closure: uncomment package in doom/packages.el and doom/config.el

### Phase 2: Agent Lifecycle
**Goal**: Users can observe and navigate between agent panes with clear status indicators and surfaced errors
**Depends on**: Phase 1
**Requirements**: LIFE-01, LIFE-02, LIFE-03, LIFE-04, OBSV-01
**Success Criteria** (what must be TRUE):
  1. Each subagent spawned by Claude Code opens a new vterm buffer named after the agent
  2. User can cycle through agent panes with next/prev commands and jump to a specific agent by name
  3. When an agent process exits, its pane stays open with dimmed visual treatment (header-line + face-remap)
  4. Each agent pane's header line shows the agent name and a color-coded running/finished indicator
  5. When emacsclient fails (wrong socket, server not running), the error is captured in a log file rather than silently discarded
**Plans**: 3 plans
- [ ] 02-01-PLAN.md — Shim color extraction and unconditional error logging
- [ ] 02-02-PLAN.md — Pane lifecycle visual treatment (header-line, dimming, team completion)
- [ ] 02-03-PLAN.md — Navigation fix, close-finished command, dashboard auto-refresh, keybindings

### Phase 3: Ship
**Goal**: The package is active in the production dotfiles config and published to GitHub for use across machines
**Depends on**: Phase 2
**Requirements**: (integration and deployment — all v1 requirements transitively satisfied)
**Success Criteria** (what must be TRUE):
  1. The package is uncommented and active in doom/packages.el and doom/config.el
  2. After cloning the dotfiles repo to a new machine and running doom sync, the feature is available without additional steps
  3. The package is pushed to GitHub under dakling/claude-code-emacs-panes with a README describing setup
**Plans**: 2 plans
Plans:
- [ ] 03-01-PLAN.md — Publish package with smoke test and README
- [ ] 03-02-PLAN.md — Integrate, verify, and ship

## Progress

**Execution Order:**
Phases execute in numeric order: 1 → 2 → 3

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Core Reliability | 2/2 | Verified | 2026-02-23 |
| 2. Agent Lifecycle | 3/3 | Verified | 2026-02-24 |
| 3. Ship | 0/2 | Not started | - |
