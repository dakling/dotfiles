---
phase: 01-core-reliability
plan: 01
subsystem: infra
tags: [emacs, vterm, tmux, elisp, straight.el, claude-code]

# Dependency graph
requires: []
provides:
  - Shim auto-fix with 3-location search and latch-into-fallback pattern
  - Env var propagation to new vterm panes via vterm-environment let-binding
  - Improved emacs_eval error handling in bin/tmux shim (stderr capture + logging)
  - Documentation of all 13 tmux subcommands handled (CORE-03)
  - Documentation of --teammate-mode tmux flag validity in v2.1.47+ (CORE-04)
affects: [02-observability, 03-doom-wiring]

# Tech tracking
tech-stack:
  added: []
  patterns:
    - Latch pattern: validate once on first use, commit to t or 'fallback for rest of session
    - Lazy validation: shim check deferred to first agent spawn, not Emacs startup
    - Best-effort env injection: inject if available, fall back gracefully on failure
    - vterm-environment let-binding in create-pane for nested agent env inheritance

key-files:
  created: []
  modified:
    - ~/code/emacs-packages/claude-code-emacs-panes/claude-code-emacs-panes.el
    - ~/code/emacs-packages/claude-code-emacs-panes/bin/tmux

key-decisions:
  - "Latch pattern chosen: check once on first spawn, latch into t or 'fallback — no repeated retries or error noise"
  - "Auto-fix covers both permissions (chmod +x) and path resolution (copy shim from fallback to primary)"
  - "Fallback behavior: native tmux passes through untouched when shim unavailable"
  - "vterm-environment let-binding in create-pane ensures nested agents can find shim"
  - "emacsclient stderr captured to file instead of /dev/null to enable debugging"

patterns-established:
  - "Latch pattern: (unless var (setq var ...)) for one-time lazy initialization"
  - "defvar forward declarations near top of file before first use (byte-compile warning fix)"

requirements-completed: [CORE-01, CORE-02, CORE-03, CORE-04, CORE-05]

# Metrics
duration: 4min
completed: 2026-02-20
---

# Phase 1 Plan 01: Core Reliability Summary

**Shim auto-fix with 3-location fallback search, latch pattern, and env var inheritance in new vterm panes for fresh-session reliability**

## Performance

- **Duration:** 4 min
- **Started:** 2026-02-20T19:05:32Z
- **Completed:** 2026-02-20T19:08:59Z
- **Tasks:** 2
- **Files modified:** 2

## Accomplishments

- Added `claude-code-emacs-panes--find-shim` searching primary build dir, repos dir, and dev source dir in order — resolves CORE-01 fresh-session shim path failure
- Added `claude-code-emacs-panes--validate-and-fix-shim` with auto-fix (chmod +x, copy to primary), latch pattern (check once, commit to result), and fallback with *Messages* warning — resolves CORE-05
- Fixed `create-pane` to inject env vars (PATH with shim, TMUX, TMUX_PANE, CLAUDE_CODE_EMACS_PANES, EMACS_PANES_SERVER) into new vterm buffers via `vterm-environment` let-binding so nested agent spawning works — resolves CORE-02
- Fixed `emacs_eval` in `bin/tmux` to capture stderr to file and log failures instead of silently discarding them
- Documented all 13 tmux subcommands handled (CORE-03) and --teammate-mode tmux flag validity in v2.1.47+ (CORE-04)

## Task Commits

Each task was committed atomically (in the package repo):

1. **Task 1: Add shim auto-fix with latch pattern and failure handling** - `634db66` (feat)
2. **Task 2: Fix env var propagation to new panes and verify compatibility** - `3a7904c` (feat)

**Plan metadata:** (see final commit in dotfiles repo)

## Files Created/Modified

- `~/code/emacs-packages/claude-code-emacs-panes/claude-code-emacs-panes.el` — Added find-shim, validate-and-fix-shim, shim-validated/shim-path state vars; fixed create-pane env injection; fixed inject-env fallback handling; added teammate-mode comment
- `~/code/emacs-packages/claude-code-emacs-panes/bin/tmux` — Added 13-subcommand documentation comment; fixed emacs_eval to capture stderr and log failures

## Decisions Made

- **Latch pattern:** Check shim once on first agent spawn, never retry — keeps design simple and predictable, avoids error noise in long sessions
- **Auto-fix vs fail:** Copy shim from fallback location to primary bin/ dir so future spawns work from cached path without re-searching
- **vterm-environment position:** `defvar vterm-environment` moved to top-level declarations (before `create-pane`) to fix byte-compile warning about variable declared after first use
- **emacsclient stderr:** Captured to `$STATE_DIR/emacsclient-err` (per-session temp file) instead of `/dev/null` — enables debugging while not polluting terminal output

## Deviations from Plan

### Auto-fixed Issues

**1. [Rule 1 - Bug] Fixed byte-compile warning: vterm-environment declared after first use**
- **Found during:** Task 2 (vterm-environment injection in create-pane)
- **Issue:** `defvar vterm-environment` was placed in the "Environment injection / setup" section (after create-pane), but create-pane now uses it — causing byte-compile warning "Variable declared after first use"
- **Fix:** Moved `defvar vterm-environment` to the top-level declarations section (line 33) alongside other forward declarations; removed duplicate from later section
- **Files modified:** claude-code-emacs-panes.el
- **Verification:** Byte-compile produces only docstring-width warnings, no variable order errors
- **Committed in:** 3a7904c (Task 2 commit)

---

**Total deviations:** 1 auto-fixed (Rule 1 - Bug)
**Impact on plan:** Necessary fix for correctness. No scope creep.

## Issues Encountered

- The `claude-code-emacs-panes` package is currently commented out in `doom/packages.el` (Phase 3 scope). Doom sync reports it as "out-of-tree" (ai-workflows skip). The `.elc` in the straight.el build dir is therefore from the previous version. This is expected — Phase 3 wiring will add the package declaration and doom sync will rebuild it.

## User Setup Required

None - no external service configuration required. Changes pushed to GitHub (`dakling/claude-code-emacs-panes`). Package wiring into Doom is Phase 3 scope.

## Next Phase Readiness

- Phase 2 (Observability) can proceed — the package internals are now correct
- Phase 3 (Doom wiring) can wire the package knowing that:
  - `claude-code-emacs-panes-setup` sets up advice and --teammate-mode tmux flag
  - The package self-heals on first agent spawn (shim auto-fix)
  - New panes automatically inherit the correct env vars for nested agent spawning
- Remaining blocker cleared: CORE-04 concern about --teammate-mode tmux is documented as valid in v2.1.47+

---
*Phase: 01-core-reliability*
*Completed: 2026-02-20*
