# Roadmap: claude-notify

## Overview

claude-notify delivers macOS system notifications when Claude Code sessions need user input, enriched with the macOS Space number so the user knows which workspace to switch to. The roadmap progresses from a working end-to-end notification pipeline, through the core differentiator (Space numbers), to intelligent notification suppression, and finally polished daily-driver experience with rich notification features.

## Phases

**Phase Numbering:**
- Integer phases (1, 2, 3): Planned milestone work
- Decimal phases (2.1, 2.2): Urgent insertions (marked with INSERTED)

Decimal phases appear between their surrounding integers in numeric order.

- [ ] **Phase 1: Core Notification Pipeline** - Hook into Claude Code CLI events and deliver basic macOS notifications with session context
- [ ] **Phase 2: Space Number Resolution** - Resolve and include the macOS Space number in notifications via Swift CLI
- [ ] **Phase 3: Notification Intelligence** - Suppress noise with cooldowns, focus detection, same-Space suppression, and debounced idle handling
- [ ] **Phase 4: Enhanced Experience** - Rich notifications with grouping, click-to-activate, sound, icon, modeline indicator, and distinct event types

## Phase Details

### Phase 1: Core Notification Pipeline
**Goal**: User receives a macOS system notification when a Claude Code session needs input (permission prompt), with session identification, triggered via the official CLI hook mechanism
**Depends on**: Nothing (first phase)
**Requirements**: DET-01, DET-02, DET-03, NTF-01, NTF-02, NTF-03, NTF-05
**Success Criteria** (what must be TRUE):
  1. Running `M-x claude-notify-install-hook` configures `~/.claude/settings.json` so Claude Code fires the Emacs callback on permission prompts
  2. When Claude Code hits a permission prompt, a macOS notification appears within seconds showing which project/session needs attention
  3. User can toggle notifications on/off via `M-x claude-notify-mode` and the setting persists across Emacs restarts
  4. Notifications work on a fresh macOS system with zero external tool installs (osascript fallback if ns-do-applescript unavailable)
**Plans**: TBD

Plans:
- [ ] 01-01: TBD
- [ ] 01-02: TBD

### Phase 2: Space Number Resolution
**Goal**: Notifications include the macOS Space number where the Claude session lives, so the user knows exactly which workspace to switch to
**Depends on**: Phase 1
**Requirements**: SPC-01, SPC-02, SPC-03, SPC-04, NTF-04
**Success Criteria** (what must be TRUE):
  1. Notification text includes "Space N" indicating which macOS Space hosts the session's Emacs frame
  2. The Swift Space-query binary compiles automatically on first use from bundled source (no manual build step)
  3. When the Space binary is unavailable or fails, notifications still fire with project name only (no crash, no missing notification)
  4. With multiple Emacs frames across different Spaces, each notification reports the correct Space for its specific session
**Plans**: TBD

Plans:
- [ ] 02-01: TBD
- [ ] 02-02: TBD

### Phase 3: Notification Intelligence
**Goal**: Notifications are reliable and non-spammy -- users keep the feature enabled because it notifies at the right time and stays quiet otherwise
**Depends on**: Phase 2
**Requirements**: DET-04, INT-01, INT-02, INT-03, INT-04, NTF-06
**Success Criteria** (what must be TRUE):
  1. `idle_prompt` events produce notifications only after a configurable debounce window (default 30-60s), avoiding false positives from normal Claude responses
  2. No notification fires if the user is already viewing the Claude buffer or is on the same macOS Space as the session
  3. Rapid state changes (multiple events within cooldown period) produce at most one notification per session
  4. `M-x claude-notify-test` verifies the full pipeline (hook -> detection -> Space query -> notification) and reports pass/fail for each stage
**Plans**: TBD

Plans:
- [ ] 03-01: TBD
- [ ] 03-02: TBD

### Phase 4: Enhanced Experience
**Goal**: Notifications are rich, actionable, and visually integrated -- clicking activates Emacs, notifications group by session, modeline shows attention state
**Depends on**: Phase 3
**Requirements**: ENH-01, ENH-02, ENH-03, ENH-04, ENH-05, ENH-06
**Success Criteria** (what must be TRUE):
  1. Clicking a notification activates the Emacs application (brings it to front)
  2. Multiple notifications from the same session coalesce in macOS Notification Center (not stacked duplicates)
  3. The Doom modeline shows an indicator for sessions currently needing attention, clearing when the user interacts
  4. Permission prompts, idle waits, and task completions produce visually distinct notifications (different titles/context)
**Plans**: TBD

Plans:
- [ ] 04-01: TBD
- [ ] 04-02: TBD

## Progress

**Execution Order:**
Phases execute in numeric order: 1 -> 2 -> 3 -> 4

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. Core Notification Pipeline | 0/? | Not started | - |
| 2. Space Number Resolution | 0/? | Not started | - |
| 3. Notification Intelligence | 0/? | Not started | - |
| 4. Enhanced Experience | 0/? | Not started | - |
