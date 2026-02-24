# claude-notify -- Research Summary

**Synthesized:** 2026-02-24
**Inputs:** STACK.md, FEATURES.md, ARCHITECTURE.md, PITFALLS.md
**Overall Confidence:** MEDIUM-HIGH

---

## Executive Summary

claude-notify is a macOS-only Emacs package that sends system notifications when a Claude Code session needs user input, enriched with the macOS Space number so the user knows which workspace to switch to. The recommended approach combines three proven techniques: Claude Code CLI Notification hooks (the official, stable mechanism) to detect when a session needs attention, a minimal Swift CLI (~50 lines) calling private SkyLight/CoreGraphics APIs to resolve Space numbers, and `ns-do-applescript` (or async `start-process` with `osascript` as fallback) to deliver macOS notifications. All three techniques are well-documented through community implementations (notably Boris Buliga's yabai-based setup, alt-tab-macos, and Hammerspoon).

The primary risk is the `idle_prompt` hook matcher producing false positives -- it fires after every Claude response, not only when genuinely waiting for input. This is a known upstream issue (GitHub #12048, #13024). The mitigation is a dual-strategy detection approach: use the reliable `permission_prompt` matcher as the primary signal, supplement with debounced `idle_prompt`, and optionally fall back to timer-based terminal buffer sampling. A secondary risk is that macOS Space number detection relies on private APIs with no public alternative; however, these APIs have been stable for 10+ years across every macOS release, and the package degrades gracefully if they break.

The package should ship as a single `claude-notify.el` file (split later if needed) with a companion Swift source file, following the user's existing convention of `~/code/emacs-packages/` development and GitHub distribution under `dakling/`. Zero hard dependencies beyond Emacs 29+ and macOS.

---

## Key Findings

### From STACK.md -- Technology Choices

- **Claude Code CLI Hooks** (HIGH confidence): The only reliable detection method. Configure `Notification` hook in `~/.claude/settings.json` to call `emacsclient --eval` on `permission_prompt` and `idle_prompt` events. Avoids fragile terminal output parsing entirely.
- **`ns-do-applescript`** (HIGH confidence): Built-in to macOS Emacs (NS builds). Runs AppleScript in-process so the notification sender is Emacs itself, bypassing the Sequoia `osascript` permission bug. Use `start-process "osascript"` as async fallback.
- **Custom Swift CLI** (MEDIUM confidence): ~50-line Swift tool using SkyLight private APIs (`SLSMainConnectionID`, `SLSGetActiveSpace`, `SLSCopyManagedDisplaySpaces`, `SLSCopySpacesForWindows`). Compiled with `swiftc`, no Xcode project needed. Prefer C bridging header over `@_silgen_name` for robustness.
- **Fallback chain**: `ns-do-applescript` -> `terminal-notifier` -> `osascript`. Detect backend at load time via `fboundp` and `executable-find`.
- **Rejected alternatives**: yabai (not installed), Hammerspoon (not installed), alert.el (unnecessary abstraction), terminal output parsing (fragile).

### From FEATURES.md -- Feature Prioritization

**Table stakes (must ship):**
1. macOS system notification on "needs input" events
2. Session/project identifier in notification text
3. macOS Space number in notification (core differentiator)
4. Duplicate/repeat notification suppression
5. Respect macOS Focus/Do Not Disturb (automatic with standard APIs)
6. `claude-notify-mode` enable/disable toggle
7. Zero external dependencies (macOS-only tools suffice)
8. Graceful degradation when Space number unavailable

**Differentiators (high value, build soon):**
- Notification grouping by session (terminal-notifier `-group` flag)
- Distinct notification types with context (permission vs. idle vs. complete)
- Per-session cooldown timer (configurable, default 30s)
- Click-to-activate Emacs on notification click (`-activate org.gnu.Emacs`)
- Sound on notification (configurable, default system sound)
- Modeline indicator for attention-needed sessions

**Defer to v2+:**
- Notification history buffer
- Customizable title templates
- Auto-focus specific Emacs frame on click
- alert.el integration
- Space labels (yabai/Amethyst)
- Team/subagent awareness

**Anti-features (never do):**
- Bypass Do Not Disturb
- Require terminal-notifier as hard dependency
- Fork/monkey-patch claude-code-ide.el
- Custom notification daemon/process
- Linux/Windows support in v1
- Polling-based detection as primary strategy

### From ARCHITECTURE.md -- System Design

- **Three-layer architecture**: State Detection -> Space Resolution -> Notification Delivery
- **Primary detection**: CLI hook calls `emacsclient --eval '(claude-notify--on-cli-notification)'` with session context. Fallback: timer-based vterm buffer sampling.
- **Space resolution**: Swift helper takes Emacs PID, returns JSON array of `{windowId, space, x, y}`. Emacs correlates frames to CGS windows by matching pixel positions (`frame-parameter 'left` / `'top`). Verified: position match is exact.
- **Notification suppression rules**: (1) same-space suppression, (2) cooldown timer per session, (3) focus suppression if Claude buffer is visible, (4) reset on user interaction.
- **Key patterns**: advice-based hooking (`advice-add` on `claude-code-ide--cleanup-on-exit` and `claude-code-ide--start-session`), async subprocess calls for Space binary, `defcustom` with Doom `setq!` convention.
- **Start with single file**: `claude-notify.el` + `claude-notify-space.swift`. Split into multiple `.el` files only if exceeding ~500 lines.

### From PITFALLS.md -- Top Risks and Mitigations

1. **`idle_prompt` false positives** (CRITICAL): Fires after every response, not just when idle. Mitigation: use `permission_prompt` as primary signal; debounce `idle_prompt` with 30-60s window; implement Emacs-side state machine.
2. **Synchronous subprocess calls freeze Emacs** (CRITICAL): Always use `start-process` (async), never `call-process` for notifications or Space queries.
3. **macOS suppresses notifications from focused app** (CRITICAL): Do NOT use `-sender org.gnu.Emacs` with terminal-notifier. Use a different sender bundle ID, or gate notifications on `(frame-focus-state)` to skip when the Claude buffer is already visible.
4. **Sequoia/Tahoe `osascript` permission breakage** (MODERATE): `ns-do-applescript` bypasses this (runs in-process as Emacs). Provide `M-x claude-notify-test` verification command. Document permission requirements.
5. **Notification spam from rapid state changes** (MODERATE): Timer-based debounce + terminal-notifier `-group` flag for notification coalescing. Make debounce interval configurable.
6. **Multi-frame Space mapping confusion** (MODERATE): Use `get-buffer-window BUFFER 'visible` to find the correct frame, not `selected-frame`. Query Space for the specific window, not just the active Space. Guard against daemon mode.
7. **terminal-notifier square bracket bug** (MINOR): Sanitize all text passed to terminal-notifier -- strip or replace `[` and `]`.

---

## Implications for Roadmap

### Suggested Phase Structure

**Phase 1: Core Notification Pipeline** -- Build the minimum end-to-end path
- **Delivers**: Working notification from Claude Code hook -> Emacs -> macOS notification
- **Features**: CLI hook configuration helper, `emacsclient` callback handler, `ns-do-applescript` / `osascript` notification dispatch, session identification in notification text, `claude-notify-mode` toggle
- **Pitfalls to avoid**: #2 (async only), #4 (focused-app suppression), #5 (Sequoia permissions)
- **Rationale**: The hook-to-notification path is the foundation everything else builds on. Space number is not needed yet -- notifications are useful even without it.

**Phase 2: Space Number Resolution** -- Add the core differentiator
- **Delivers**: macOS Space number in notifications, compile-on-first-use Swift binary
- **Features**: Swift CLI source + Makefile, async Space query from Emacs, frame-to-CGS-window correlation via position matching, graceful fallback when binary unavailable
- **Pitfalls to avoid**: #3 (private API limitations), #6 (multi-frame confusion), #13 ("Displays have separate Spaces" setting)
- **Rationale**: Space number is the entire value proposition but depends on the notification pipeline being solid first. The Swift helper is an independent compilation unit that can be developed and tested in parallel.

**Phase 3: Notification Intelligence** -- Make notifications smart, not noisy
- **Delivers**: Reliable, non-spammy notifications that users keep enabled
- **Features**: Per-session cooldown timer, same-Space suppression, focus-state suppression, debounced `idle_prompt` handling, state machine for rising-edge detection, `claude-notify-test` verification command
- **Pitfalls to avoid**: #1 (false positives), #7 (notification spam), #10 (Focus mode)
- **Rationale**: Without intelligence, the `idle_prompt` false positive problem will cause alert fatigue within minutes. This phase transforms the package from "technically works" to "genuinely useful."

**Phase 4: Enhanced Notification Experience** -- Polish for daily use
- **Delivers**: Rich notifications with grouping, sound, click-to-activate, modeline indicator
- **Features**: terminal-notifier backend with `-group`, `-sound`, `-activate`, `-sender` flags; modeline segment for attention-needed sessions; distinct notification types (permission vs. idle vs. complete)
- **Pitfalls to avoid**: #9 (bracket escaping), #11 (notification icon)
- **Rationale**: These features layer cleanly on the existing pipeline and justify the package being a proper Emacs package rather than a shell script. Each is independently toggleable.

### Research Flags

| Phase | Needs Phase Research? | Rationale |
|-------|----------------------|-----------|
| Phase 1 | NO | Well-documented patterns: CLI hooks have official docs, `ns-do-applescript` is standard Emacs, `emacsclient --eval` is textbook |
| Phase 2 | YES (light) | Private SkyLight API behavior should be validated on the actual system during implementation. The architecture research verified it works, but edge cases (multi-display, Spaces settings) need live testing |
| Phase 3 | YES (light) | The `idle_prompt` false positive behavior may evolve upstream. Check GitHub issues #12048 and #21238 for any changes before implementing the state machine |
| Phase 4 | NO | terminal-notifier flags are well-documented. Modeline integration follows standard Doom patterns |

---

## Confidence Assessment

| Area | Confidence | Notes |
|------|------------|-------|
| Stack | HIGH | All core technologies verified: CLI hooks (official docs), `ns-do-applescript` (built-in), Swift/SkyLight (tested live on target system) |
| Features | MEDIUM-HIGH | Table stakes and differentiators well-defined from community implementations. The Emacs-side detection vs. hook-only debate is the main uncertainty -- resolved in favor of hooks as primary |
| Architecture | HIGH | Three-layer design verified against live system. Frame-to-window position matching confirmed exact. Process management patterns standard Emacs |
| Pitfalls | HIGH | 13 pitfalls identified with concrete mitigations. Critical ones (#1, #2, #3, #4) have multiple confirming sources including upstream GitHub issues |

### Gaps to Address During Planning

1. **`idle_prompt` behavior on current Claude Code version**: The false-positive issue may have been partially fixed since the GitHub issues were filed. Verify actual behavior before investing in the state machine (Phase 3).
2. **Multi-display Space indexing convention**: Should Space numbers be global (1-N across all displays) or per-display? Architecture research recommends global to match Mission Control. Confirm this matches the user's mental model.
3. **Hook installation UX**: The CLI hook requires writing to `~/.claude/settings.json`. Need to decide between interactive setup command vs. documented manual setup. Architecture research recommends an interactive `claude-notify-install-hook` command.
4. **`ns-do-applescript` vs. async `start-process`**: STACK.md recommends `ns-do-applescript` as preferred; PITFALLS.md warns it is synchronous and its future in Emacs is uncertain. Resolution: use `start-process "osascript"` as the default (async, safe), offer `ns-do-applescript` as an opt-in fast path for users who want it.

---

## Sources

### Primary (HIGH confidence)
- [Claude Code Hooks Documentation](https://code.claude.com/docs/en/hooks-guide) -- hook events, matchers, JSON schema
- [Hammerspoon spaces/private.h](https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/spaces/private.h) -- SkyLight private API signatures
- [alt-tab-macos Spaces.swift](https://github.com/lwouis/alt-tab-macos/blob/master/src/logic/Spaces.swift) -- production SkyLight usage, space ID-to-index mapping
- claude-code-ide.el source (verified: hash tables, process management, no public event hooks)
- GNU Emacs Lisp Reference (Processes, Timers, Filter Functions, Input Focus, Frames)
- Live CGS API testing on target system (macOS Tahoe 26.3, Emacs NS build)

### Secondary (MEDIUM confidence)
- [Boris Buliga: Claude Code Notifications](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications) -- yabai Space awareness, terminal-notifier bracket bug, sender flag
- [terminal-notifier GitHub](https://github.com/julienXX/terminal-notifier) -- grouping, sound, sender, known Sequoia issues
- [Claude Code GitHub issues #8320, #12048, #13024, #21238](https://github.com/anthropics/claude-code/issues) -- idle_prompt false positives, notification matcher evolution
- [alert.el issue #42](https://github.com/jwiegley/alert/issues/42) -- macOS foreground suppression
- [Late Night Software forum](https://forum.latenightsw.com/t/trying-to-use-terminal-for-display-notification/5068) -- Sequoia osascript permission issues

### Tertiary (LOW confidence)
- [ianyh: Identifying Spaces in Mac OS X](https://ianyh.com/blog/identifying-spaces-in-mac-os-x/) -- historical Space identification approaches (Amethyst author)
- [CGSInternal/CGSSpace.h](https://github.com/NUIKit/CGSInternal/blob/master/CGSSpace.h) -- private CGS function signatures
- [macOS defaults: spans-displays](https://macos-defaults.com/mission-control/spans-displays.html) -- "Displays have separate Spaces" behavior

---

*Synthesized: 2026-02-24*
*Research valid until: 2026-04-24*
