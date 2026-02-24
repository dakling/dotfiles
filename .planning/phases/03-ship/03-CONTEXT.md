# Phase 3: Ship - Context

**Gathered:** 2026-02-24
**Status:** Ready for planning

<domain>
## Phase Boundary

The package is active in the production dotfiles config and published to GitHub for use across machines. After cloning the dotfiles repo to a new machine and running doom sync, the feature is available without additional steps. The package has a README describing setup.

</domain>

<decisions>
## Implementation Decisions

### Cross-machine portability
- Audience: personal use across multiple machines (not a public package for other users)
- Platform: macOS only for now; Linux support deferred
- Dependencies: document what's needed in README (bash, emacsclient, vterm, Claude Code version) — no runtime dependency checks
- First-run assumption: `doom sync` always runs before first Emacs launch — safe to assume straight.el checkout exists

### Config completeness
- Audit the existing `use-package!` block in doom/config.el for anything missing, stale, or experimental
- Review all keybindings under SPC o C — present full list for user to adjust before shipping
- Clean up panes-related test artifacts and uncommitted files in the dotfiles repo
- Do NOT touch unrelated untracked files (amethyst.yml, etc.) — only panes-related changes

### Verification approach
- Success bar: actually spawn an agent team, verify panes appear with headers and status indicators
- Method: automated smoke test elisp function that checks package loads, shim exists, server running, env vars correct
- Live agent interaction verified manually by user after smoke test passes
- Fix strategy: fix issues inline during verification (no gap-closure plans)
- First load behavior: silent — no welcome messages or popups on package load

### Claude's Discretion
- README structure and level of detail (personal reference, not public documentation)
- Smoke test function implementation details
- Which test artifacts to clean up vs leave alone
- Keybinding conflict detection approach

</decisions>

<specifics>
## Specific Ideas

- The README is a personal reference for setting up on a new machine, not public-facing documentation
- Smoke test should be an interactive command the user can run to verify the setup

</specifics>

<deferred>
## Deferred Ideas

- Linux platform support — future work if needed
- Public package support for non-Doom Emacs users — not planned

</deferred>

---

*Phase: 03-ship*
*Context gathered: 2026-02-24*
