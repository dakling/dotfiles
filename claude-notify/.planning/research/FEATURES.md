# claude-notify: Feature Landscape

**Researched:** 2026-02-24
**Domain:** Emacs notification packages, macOS system notifications, workspace-aware tooling
**Overall Confidence:** MEDIUM-HIGH (well-established ecosystem, but Space-number integration is niche)

---

## Table Stakes

These features are expected by any user who would install a package claiming to notify when Claude Code needs attention. Omitting any of these makes the package feel broken or incomplete.

| Feature | Why Expected | Complexity | Notes |
|---------|-------------|------------|-------|
| **Fire macOS system notification on "needs input" events** | The entire reason the package exists. Users need a banner/alert when Claude asks a question, requests permission, or finishes a task. Without this, there is no package. | LOW | Use `osascript -e 'display notification'` as the zero-dependency baseline. Every macOS Emacs user has this available. |
| **Include session/project identifier in notification** | With multiple Claude sessions, a notification saying "Claude needs input" is useless without saying *which* session. Users need the project name or directory at minimum. | LOW | Extract from `claude-code-ide--get-working-directory` or buffer name pattern `*claude-code[PROJECT]*`. |
| **Include macOS Space number in notification** | This is the core differentiator stated in the project description, but it is also table stakes for *this specific package* because the project's entire value proposition is "know which Space has a waiting session." | MEDIUM | No public Apple API exists. Options: (1) compile a small C/ObjC helper using `CGSGetActiveSpace` private API, (2) shell out to `yabai -m query --spaces --space` if yabai is installed, (3) use the WhichSpace app's accessibility data. The private-API helper is most reliable for users who do not run yabai. |
| **Suppress duplicate/repeat notifications** | If Claude asks three permission questions in a row, the user should not get three rapid-fire banners. One notification per "attention needed" episode is expected. A new notification should only fire after the user has interacted. | LOW-MEDIUM | Implement a simple state flag per session: `needs-attention-p`. Set on trigger, clear when session buffer becomes visible or receives input. Only notify on rising edge (transition from not-needing to needing attention). |
| **Respect macOS Focus/Do Not Disturb** | `osascript display notification` and `terminal-notifier` both honor the system DND setting automatically. Users will be confused and annoyed if notifications bypass Focus mode. | ZERO (free) | Both `osascript` and `terminal-notifier` respect DND natively. Do NOT use `-ignoreDnD` flag from terminal-notifier. |
| **Configurable enable/disable toggle** | Users must be able to turn notifications on/off without uninstalling the package. Standard Emacs convention. | LOW | A `claude-notify-mode` global minor mode, plus a `claude-notify-enabled` defcustom. |
| **No external dependencies beyond macOS** | The package must work out-of-the-box on macOS without requiring Homebrew packages. `osascript` is always available. Optional enhanced backends can require extra tools. | LOW | Default to `osascript`. Detect and prefer `terminal-notifier` if present, but never require it. |
| **Graceful degradation when Space number is unavailable** | If the private API helper is not compiled or the user has only one Space, the notification should still fire -- just without the Space number. Never error out or swallow the notification because of a missing Space number. | LOW | Wrap Space-number lookup in `condition-case`. Fall back to project name only. |

**Rationale:** These features collectively deliver the minimum viable product. A user who installs claude-notify and triggers it for the first time will see a macOS notification with the project name, Space number (if available), and the type of attention needed. That is enough to decide whether to switch Spaces.

---

## Differentiators

Features that go beyond what existing tools provide and make claude-notify genuinely more useful than a simple hook script. These justify the package existing as a proper Emacs package rather than a snippet in the user's config.

| Feature | Value Proposition | Complexity | Notes |
|---------|------------------|------------|-------|
| **Notification grouping by session** | terminal-notifier's `-group` flag replaces previous notifications from the same session. This prevents notification pile-up when Claude asks multiple questions. Only the most recent notification for a given session is visible. | LOW | Use the project directory or session ID as the group key with terminal-notifier. For osascript fallback, track "last notification time" and suppress within a cooldown window. |
| **Distinct notification types with context** | Different notification text for different events: "Claude needs permission" vs "Claude is asking a question" vs "Claude finished the task." Tells the user *what kind* of attention is needed so they can prioritize. | LOW-MEDIUM | Map Claude Code hook event types (`permission_prompt`, `idle_prompt`, `Stop`) to human-readable notification titles and messages. |
| **Notification fires from Emacs, not from Claude Code hooks** | By detecting state changes inside Emacs (process sentinel, terminal output patterns, or MCP events), the package can fire immediately when the terminal shows a prompt. Claude Code's built-in `Notification` hook with `idle_prompt` has a 60-second delay, which is far too slow. Emacs-side detection is instant. | MEDIUM-HIGH | Two approaches: (1) Monitor the vterm/eat process output filter for prompt patterns (e.g., regex matching "Enter to select" or the waiting-for-input prompt chrome). (2) Hook into claude-code-ide.el's MCP session events if/when upstream exposes them. Approach (1) is more reliable today since claude-code-ide.el does not currently expose public event hooks. |
| **Click-to-activate: bring Emacs frame to front** | When the user clicks the notification, macOS activates the Emacs application. Combined with the Space number, the user knows exactly where to go. terminal-notifier's `-activate org.gnu.Emacs` does this. | LOW | Requires terminal-notifier. For osascript fallback, notifications open Script Editor (unhelpful), so this feature gracefully degrades to "Space number only" with osascript. |
| **Claude's icon on the notification** | Use terminal-notifier's `-sender com.anthropic.claudefordesktop` to show Claude's icon instead of Script Editor's icon. Makes notifications instantly recognizable in a stream of other system notifications. | LOW | Only works with terminal-notifier. Requires Claude Desktop to be installed (for the icon). Fall back to Emacs icon otherwise. |
| **Per-session cooldown timer** | After firing a notification, suppress further notifications for that session for a configurable period (e.g., 5 seconds). Prevents rapid-fire notifications when Claude makes multiple tool calls that each need permission. | LOW | `defcustom claude-notify-cooldown-seconds` with default 5. Track last-notification-time per session in a hash table. |
| **Modeline indicator for "attention needed" sessions** | Show a visual indicator in the Doom modeline when any Claude session needs attention, even when notifications are suppressed or DND is on. Provides an always-visible fallback. | MEDIUM | Add a segment to `doom-modeline` (or `mode-line-format` for non-Doom) that checks the `needs-attention-p` flag across all sessions. Use a distinctive face (e.g., warning color). |
| **Sound on notification** | Play a short sound when the notification fires, using terminal-notifier's `-sound` flag or `(shell-command "afplay /System/Library/Sounds/Glass.aiff &")`. Audible feedback is critical when the user is looking at a different screen or Space. | LOW | `defcustom claude-notify-sound` defaulting to `"default"`. Set to nil to disable. |

**Rationale:** These features transform claude-notify from "a notification script" into "a proper workflow tool." The Emacs-side detection (rather than relying on Claude Code's 60-second idle hook) is the single most important differentiator -- it is the reason this needs to be an Emacs package and not just a shell script in `.claude/settings.json`.

---

## Nice-to-Have

Features that would be pleasant but are not worth delaying the initial release. Some are speculative; others depend on upstream changes in claude-code-ide.el or Claude Code CLI.

| Feature | Value Proposition | Complexity | Why Defer |
|---------|------------------|------------|-----------|
| **Notification history buffer** | An `*claude-notify-log*` buffer recording all notifications with timestamps, session, and type. Useful for debugging or reviewing what happened across sessions. | LOW | macOS Notification Center already provides history. An Emacs buffer adds marginal value. Build only if users request it. |
| **Customizable notification title template** | Let users define a format string for notification titles, e.g., `"[Space %s] %s needs input"`. Power users want control over notification wording. | LOW | The default format will be fine for most users. Expose as defcustom later. |
| **Auto-focus Emacs frame on notification click** | Beyond `-activate`, use `emacsclient` or AppleScript to raise the specific Emacs frame (not just any Emacs window) and switch to the Claude buffer. | MEDIUM-HIGH | Requires knowing which frame corresponds to which Space. Frame-to-Space mapping is not straightforward. Defer until core features are solid. |
| **Integration with alert.el** | Use alert.el as a notification backend, gaining access to its 14+ notification styles, severity routing, and rule system. | LOW-MEDIUM | alert.el is a popular package, but adding it as a dependency is unnecessary for macOS-only use. The `osascript`/`terminal-notifier` approach is simpler and more native. Offer as an optional backend for users who already use alert.el. |
| **Selective notification filtering** | Let users choose which event types trigger notifications (e.g., "only permission prompts, not idle" or "only Stop events"). | LOW | Over-engineering for v1. Start with "notify on everything that needs input." Add filtering if users find the defaults too noisy. |
| **Multiple notification backends** | Support alert.el, libnotify (Linux), toast (Windows) as alternative backends. | MEDIUM | Explicitly out of scope per PROJECT.md ("macOS only"). Revisit only if the package gains users on other platforms. |
| **Emacs-native notification popup (posframe/childframe)** | Display an Emacs-native popup instead of/in addition to macOS notifications. Useful when Emacs is in the foreground and the user wants an in-editor alert. | MEDIUM | Adds visual complexity and a dependency (posframe). The modeline indicator covers the "Emacs is visible" case adequately. |
| **Notification actions/buttons** | Add action buttons to notifications (e.g., "Approve", "Deny"). macOS UNUserNotificationCenter supports this but terminal-notifier dropped action button support in v2.0. Would require the `alerter` tool or a custom Swift helper. | HIGH | Fragile dependency chain. The user must still go to Emacs to provide full input. Buttons create a false expectation of completing the interaction from the notification. |
| **Space label support (yabai/Amethyst)** | For users of yabai or Amethyst, show the Space *label* (e.g., "code", "browser") rather than the numeric index. | LOW-MEDIUM | Requires detecting and querying the active WM. Boris Buliga's implementation (yabai) shows this is feasible. Defer to v2 -- Space number alone is sufficient for v1. |
| **Upstream claude-code-ide.el hooks** | If claude-code-ide.el gains a public event hook system (e.g., `claude-code-ide-event-hook`), use it instead of terminal output scraping. Cleaner, more reliable, forward-compatible. | LOW (once available) | Upstream does not currently expose public hooks. Monitor the repository. Terminal scraping works today. |
| **Team/subagent awareness** | Distinguish notifications from the main Claude session vs. subagents in agent teams. Subagent completions may be less urgent. | LOW-MEDIUM | Agent teams are a new Claude Code feature. Wait for the feature to stabilize before building awareness around it. |

---

## Anti-Features

Things the package should explicitly NOT do. Including these would hurt the user experience or create maintenance burden.

| Anti-Feature | Why Avoid | What to Do Instead |
|-------------|-----------|-------------------|
| **Bypass macOS Do Not Disturb** | Users enable DND for a reason. Overriding it destroys trust. terminal-notifier's `-ignoreDnD` flag exists but should never be used. | Respect DND. The modeline indicator provides a fallback when DND is active. |
| **Require terminal-notifier as a hard dependency** | Forces users to install a Homebrew package. Breaks the "works out of the box on macOS" requirement. | Default to `osascript`. Detect and prefer terminal-notifier when available. |
| **Fork or monkey-patch claude-code-ide.el** | Creates a maintenance nightmare. Upstream updates would constantly break the fork. | Hook into public interfaces. Use advice-add if needed. Monitor upstream for proper hook support. |
| **Custom notification daemon/process** | Running a separate process adds complexity, potential crashes, and port conflicts. Everything should run inside Emacs. | Use Emacs timers and process filters. No external watchers. |
| **Linux/Windows support in v1** | Splits focus, adds conditional codepaths, and the core feature (macOS Space number) is inherently platform-specific. | macOS only. Use `(eq system-type 'darwin)` guard. Document the limitation clearly. |
| **Notification flood without rate limiting** | Sending a notification on every single Claude Code event (each tool call, each output chunk) would overwhelm the user and render notifications useless. | Rate-limit per session. One notification per "needs attention" state transition. Cooldown timer. |
| **Polling-based detection** | Repeatedly checking terminal buffer content on a timer is wasteful and introduces latency. | Use process filter / output hooks for event-driven detection. |

---

## Feature Dependencies

```
[Space Number Helper]
        |
        v
[Core Notification Engine] <--- [Session State Tracking]
        |                              |
        v                              v
[macOS Notification Dispatch]   [Duplicate Suppression]
        |                              |
        v                              v
[osascript backend]             [Cooldown Timer]
[terminal-notifier backend]
        |
        v
[Click-to-activate]
[Notification grouping]
[Custom icon]
[Sound]
```

Core dependency chain:
1. **Session state tracking** must exist before any notification can fire (need to know *which* session and *what* happened)
2. **Space number helper** is independent and can be developed in parallel
3. **Notification dispatch** depends on state tracking and benefits from Space number
4. **Duplicate suppression** wraps the dispatch layer
5. **Backend-specific features** (grouping, icon, sound, click-to-activate) layer on top of the dispatch

---

## MVP Recommendation

**Ship these first (Phase 1):**
1. Session state detection via terminal output monitoring (vterm/eat process filter)
2. macOS Space number query via compiled helper or yabai fallback
3. `osascript` notification dispatch with project name + Space number
4. Duplicate suppression with per-session cooldown
5. `claude-notify-mode` toggle

**Add in Phase 2:**
6. terminal-notifier backend with grouping, icon, sound, click-to-activate
7. Modeline indicator
8. Configurable notification types

**Defer to Phase 3+:**
9. alert.el integration
10. Space label support
11. Notification history buffer
12. Upstream hook integration (when available)

**Rationale:** Phase 1 delivers the core promise with zero external dependencies. Phase 2 enhances the experience for users willing to install terminal-notifier. Phase 3 addresses edge cases and upstream evolution.

---

## Sources

### Primary (HIGH confidence)
- [alert.el GitHub repository](https://github.com/jwiegley/alert) -- complete feature list, API, 14 notification styles
- [terminal-notifier GitHub repository](https://github.com/julienXX/terminal-notifier) -- CLI options, grouping, sound, sender spoofing, action button removal in v2.0
- [Claude Code Hooks reference](https://code.claude.com/docs/en/hooks) -- complete hook event types, Notification hook matchers, JSON schemas
- claude-code-ide.el source (v0.2.6) at `/Users/darioklingenberg/.config/emacs/.local/straight/repos/claude-code-ide.el/` -- no public event hooks, process sentinel on exit only, vterm/eat process filter architecture

### Secondary (MEDIUM confidence)
- [Claude Code GitHub issue #13024](https://github.com/anthropics/claude-code/issues/13024) -- community workarounds for "waiting for input" detection, PermissionRequest hook for AskUserQuestion
- [Boris Buliga's Claude Code notifications](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications) -- yabai Space index + label in notifications, terminal-notifier sender spoofing, git repo name extraction
- [WhichSpace](https://github.com/gechr/WhichSpace) -- macOS 14+ menu bar app showing current Space number, uses private APIs internally
- [karthink/timeout](https://github.com/karthink/timeout) -- Emacs throttle/debounce library
- [Emacs Desktop Notifications manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html) -- D-Bus notifications (Linux only, not applicable to macOS)

### Tertiary (LOW confidence)
- [Apple discussions on Space number APIs](https://discussions.apple.com/thread/254642341) -- confirms no public API, suggests CGSGetActiveSpace private API
- [osx-space-id](https://github.com/shabble/osx-space-id) -- Carbon/Cocoa utility for Space ID, may be outdated
- [sauron.el](https://github.com/djcb/sauron) -- Emacs event log with priority-based filtering, activation callbacks; marked WIP, low activity
