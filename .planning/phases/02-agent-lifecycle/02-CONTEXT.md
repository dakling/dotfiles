# Phase 2: Agent Lifecycle - Context

**Gathered:** 2026-02-23
**Status:** Ready for planning

<domain>
## Phase Boundary

Users can observe and navigate between agent panes with clear status indicators and surfaced errors. Panes are created when agents spawn, visually indicate running/finished state, and can be navigated via keybindings. emacsclient failures are logged rather than silently discarded.

</domain>

<decisions>
## Implementation Decisions

### Auto-close behavior
- Finished agent panes stay open, marked as finished (NOT auto-closed)
- Visual treatment: header line changes AND buffer text gets a dimmed face overlay so finished panes visually recede
- Bulk action: add a "close all finished" command to SPC o C prefix and dashboard
- Team completion: minibuffer notification ("All N agents finished") when the last agent in a team exits

### Navigation & layout
- New panes arranged as side-by-side vertical splits (current behavior, keep it)
- next/prev/select switches buffer in the current window (not pop-up split)
- "Show all" (SPC o C a) restores the full side-by-side view
- Pane creation does NOT steal focus — split appears but cursor stays in current window
- Dashboard auto-refreshes on a timer while visible (live status view)

### Claude's Discretion
- Pane naming format (agent name in header, color from Claude Code's --agent-color flag)
- Header line exact format and color coding
- Dashboard refresh interval
- emacsclient error logging format and location
- Dimmed face overlay implementation details

</decisions>

<specifics>
## Specific Ideas

- The dimmed overlay on finished panes should make it immediately obvious which panes are still active vs done, even from a distance
- "Close all finished" should work from both the SPC o C prefix (quick command) and the dashboard (button/key)

</specifics>

<deferred>
## Deferred Ideas

None — discussion stayed within phase scope

</deferred>

---

*Phase: 02-agent-lifecycle*
*Context gathered: 2026-02-23*
