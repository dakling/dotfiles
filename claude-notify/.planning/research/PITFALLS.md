# claude-notify -- Domain Pitfalls

**Project:** claude-notify (Emacs package for macOS Claude Code session notifications with Space number)
**Researched:** 2026-02-24
**Confidence:** HIGH (verified across official docs, source code, and community reports)

---

## Critical Pitfalls

These will cause the package to fail silently, spam users, or freeze Emacs if not addressed.

### Pitfall 1: Claude Code `idle_prompt` Notification Fires on Every Response (False Positives)

**What goes wrong:** The `idle_prompt` notification matcher in Claude Code's hook system fires after *every* response completion, not only when Claude is genuinely waiting for user input. This creates constant false-positive notifications that train users to ignore all notifications, defeating the purpose of the package entirely.

**Why it happens:** The Claude Code CLI does not currently expose a reliable `notification_type` field that distinguishes between "response complete" and "genuinely waiting for user action." The `idle_prompt` matcher was designed for a 60-second idle timeout, but in practice it fires immediately after every turn. This is tracked in [anthropics/claude-code#12048](https://github.com/anthropics/claude-code/issues/12048) and [#13024](https://github.com/anthropics/claude-code/issues/13024).

**Consequences:** Users experience alert fatigue within minutes. Real input-required scenarios get lost in noise. The package becomes unusable without mitigation.

**Prevention:**
- Do NOT rely solely on the `idle_prompt` notification type from Claude Code hooks.
- Use the `PermissionRequest` hook (which fires reliably for `AskUserQuestion` prompts) as a primary signal. One commenter in #13024 confirmed this works as a workaround.
- Implement a **state machine** on the Emacs side that tracks whether Claude's terminal process is producing output vs. idle. Parse terminal output for known TUI patterns (e.g., "Enter to select", input prompts) rather than trusting the hook classification.
- Apply aggressive debouncing: suppress duplicate notifications within a configurable cooldown window (recommend 30--60 seconds minimum).

**Detection:** If users report "notifications fire every time Claude finishes talking," this pitfall was not mitigated.

**Confidence:** HIGH -- verified via GitHub issues #8320, #12048, #13024 with reproduction steps and confirmed by multiple users.

---

### Pitfall 2: `call-process` for Notifications Freezes Emacs

**What goes wrong:** Using `call-process` (synchronous) to invoke `osascript`, `terminal-notifier`, or any external notification command blocks Emacs until the subprocess completes. If the notification system is slow (Focus mode processing, high system load, stalled DNS for icon fetching), Emacs hangs for the duration.

**Why it happens:** `call-process` causes the Lisp program to wait for the subprocess to terminate. On macOS, notification delivery can involve inter-process communication with the Notification Center daemon, which is not guaranteed to be fast. Additionally, `terminal-notifier` with the `-sender` flag performs bundle ID resolution.

**Consequences:** Emacs freezes for 100ms--5s per notification. In rapid-fire scenarios (see Pitfall 1), this can make Emacs unusable.

**Prevention:**
- **Always use `start-process` (async)** for sending notifications. Never use `call-process`, `shell-command`, or `call-process-region`.
- If return value is needed (e.g., for error detection), use `set-process-sentinel` on the async process to handle completion.
- For simple fire-and-forget notifications, `start-process` with no sentinel is sufficient.
- Consider `make-process` with `:connection-type 'pipe` for full control.

**Code pattern:**
```elisp
;; WRONG: blocks Emacs
(call-process "osascript" nil nil nil "-e" script)

;; RIGHT: non-blocking
(start-process "claude-notify" nil "osascript" "-e" script)
```

**Detection:** If users report momentary freezes when notifications fire, this pitfall is the cause.

**Confidence:** HIGH -- documented in [GNU Emacs Lisp Reference: Synchronous Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Synchronous-Processes.html) and [Asynchronous Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html).

---

### Pitfall 3: macOS Space Number Detection Is Unreliable Without a Window Manager

**What goes wrong:** There is no public macOS API to query the current Space number. Every approach has significant limitations:

| Method | Limitation |
|--------|-----------|
| `CGSGetActiveSpace` (private API) | Returns an opaque space ID, not a user-visible number. Mapping ID to number requires `CGSCopyManagedDisplaySpaces` which only works when "Displays have separate Spaces" is enabled. Can break across macOS versions without warning. |
| `defaults read com.apple.spaces` | **Stale data.** The plist only updates when spaces are created or deleted, NOT when the user switches spaces. Completely unreliable for real-time detection. |
| Cross-referencing `CGWindowListCopyWindowInfo` with space preferences | Requires at least one window per space. Fails on empty spaces. Complex and fragile. |
| Yabai `yabai -m query --spaces --space` | Reliable and returns a real index, but requires yabai to be installed and running. Not universal. |

**Why it happens:** Apple treats Spaces as a private implementation detail. There has never been a public API for Space enumeration or identification. Private APIs change without deprecation notices.

**Consequences:** Space number is either wrong, stale, or unavailable depending on the user's setup.

**Prevention:**
- Design a **tiered detection strategy** with graceful fallback:
  1. First: check for yabai (`yabai -m query --spaces --space | jq .index`) -- most reliable when available
  2. Second: check for Amethyst or other window managers with query APIs
  3. Third: attempt `CGSGetActiveSpace` via a small Swift/ObjC helper binary (compile once, cache binary)
  4. Last resort: omit Space number from notification; show project name only
- Make the Space detection method **user-configurable** via a defcustom (`:type '(choice (const :tag "Auto-detect" auto) (const :tag "Yabai" yabai) (const :tag "None" none) (function :tag "Custom function"))`).
- **Cache the detection method** after first successful probe. Do not re-probe on every notification.
- **Never block Emacs** waiting for space detection. Run it async and use last-known value if detection takes too long.

**Detection:** If Space number is wrong or always "1," this pitfall was not addressed.

**Confidence:** HIGH for the problem; MEDIUM for the yabai workaround (verified via [d12frosted blog](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications) and [ianyh blog](https://ianyh.com/blog/identifying-spaces-in-mac-os-x/)).

---

### Pitfall 4: macOS Suppresses Notifications When Sender App Is Focused

**What goes wrong:** macOS Notification Center suppresses notifications from the currently focused application. If Emacs sends a notification (either via `osascript` where the sender is Script Editor/Terminal, or via `terminal-notifier -sender org.gnu.Emacs`), and that sender app is in the foreground, the notification is silently dropped.

**Why it happens:** This is intentional macOS behavior -- the OS assumes you do not need to be notified about an app you are actively using. But Emacs is not a normal app; it is more like an operating system. A user editing code in one buffer absolutely needs to be notified that Claude is waiting in another buffer. This is [a known issue with alert.el](https://github.com/jwiegley/alert/issues/42).

**Consequences:** Notifications never appear when Emacs is focused, which is the most common state. The package appears broken.

**Prevention:**
- **Do NOT use `-sender org.gnu.Emacs`** with terminal-notifier. Use a different sender bundle ID (e.g., `com.anthropic.claudefordesktop` as d12frosted does, or a custom app bundle).
- If using `osascript`, the notification is attributed to Script Editor (or whatever runs osascript), which avoids the Emacs focus issue by default.
- **Consider the focused-frame check:** Only send macOS notifications when the Claude buffer's frame is not focused. Use `(frame-focus-state)` to check. If the user is looking at the Claude buffer already, use a lighter signal (mode-line indicator, echo area message, or sound only).
- The combination of "different sender" + "focus-aware gating" eliminates this class of problem entirely.

**Detection:** If notifications work when Emacs is in the background but never when focused, this is the cause.

**Confidence:** HIGH -- confirmed in [alert.el issue #42](https://github.com/jwiegley/alert/issues/42) and macOS Notification Center documentation.

---

## Moderate Pitfalls

These cause degraded behavior, user annoyance, or platform-specific failures.

### Pitfall 5: `osascript display notification` Broken in macOS Sequoia Terminal Context

**What goes wrong:** In macOS Sequoia (and likely Tahoe), `osascript -e 'display notification'` no longer works when invoked from Terminal.app or processes spawned by it. The notification is silently dropped. It works from Script Editor.

**Why it happens:** macOS Sequoia tightened notification permissions. The calling application (Terminal, Emacs.app, etc.) must have explicit notification permission granted in System Settings > Notifications. Since Emacs processes are typically spawned from a terminal context, they inherit the terminal's restricted permissions.

**Consequences:** Zero notifications on Sequoia unless the user manually grants permissions or uses a different mechanism.

**Prevention:**
- **Prefer `terminal-notifier`** over bare `osascript`. `terminal-notifier` is a standalone `.app` bundle that registers its own notification permissions independently.
- If using `osascript`, document the permission requirement prominently: users must go to System Settings > Notifications and enable notifications for their terminal application AND for Script Editor.
- For Emacs specifically, `ns-do-applescript` (available in NS builds only, check with `(featurep 'ns)`) bypasses the terminal permission chain because it runs AppleScript directly within the Emacs process. However, this makes the notification attributed to Emacs itself, triggering Pitfall 4.
- Provide a **setup verification command** (`M-x claude-notify-test`) that sends a test notification and reports success/failure.

**Detection:** If notifications work from `M-x shell` but not from the package, this is a permissions issue.

**Confidence:** HIGH -- verified via [Late Night Software forum reports](https://forum.latenightsw.com/t/trying-to-use-terminal-for-display-notification/5068) and [MacScripter](https://www.macscripter.net/t/trying-to-use-terminal-for-display-notification/76593).

---

### Pitfall 6: `ns-do-applescript` Removal Risk and Portability

**What goes wrong:** `ns-do-applescript` is faster than `call-process osascript` (avoids forking a process), but it is only available in NS (Cocoa) builds of Emacs, and there have been [discussions about removing it from Emacs](https://lists.gnu.org/archive/html/emacs-devel/2022-05/msg01373.html).

**Why it happens:** Apple platform-specific functions in Emacs are periodically audited and removed. The function is not available in terminal Emacs, Emacs built with X11, or GNUStep builds.

**Consequences:** Hard crash (`void-function ns-do-applescript`) on non-NS builds. Future Emacs versions may remove it.

**Prevention:**
- **Always guard** with `(fboundp 'ns-do-applescript)` or `(featurep 'ns)`.
- Use `ns-do-applescript` as an optimization path, not the only path. Fall back to `start-process "osascript"`.
- Given the async requirement (Pitfall 2), `ns-do-applescript` is actually inferior because it is synchronous. Prefer `start-process` universally and avoid `ns-do-applescript` entirely.

**Detection:** Errors on non-macOS or non-NS Emacs builds.

**Confidence:** HIGH -- confirmed in [emacs-devel mailing list](https://lists.gnu.org/archive/html/emacs-devel/2022-05/msg01373.html) and [Irreal blog](https://irreal.org/blog/?p=4865).

---

### Pitfall 7: Notification Spam from Rapid State Changes

**What goes wrong:** Without debouncing, a single Claude interaction can trigger multiple notifications in rapid succession: permission prompt, tool approval, response complete, idle timeout. Each fires a separate macOS notification, flooding the Notification Center.

**Why it happens:** Claude Code's hook system fires events independently. Multiple hook types can trigger within seconds of each other. The Emacs process filter receives output in small chunks (1024 bytes on macOS), which can cause intermediate state detections.

**Consequences:** Notification Center fills with stale notifications. Sound effects play repeatedly. User disables notifications system-wide.

**Prevention:**
- Implement **notification coalescing** with two mechanisms:
  1. **Emacs-side debounce:** Use a timer-based debounce pattern. Cancel pending notification timer on each new event; only fire after N seconds of quiet.
     ```elisp
     (defvar claude-notify--timer nil)
     (defvar claude-notify-debounce-seconds 5)

     (defun claude-notify--schedule (message)
       (when claude-notify--timer
         (cancel-timer claude-notify--timer))
       (setq claude-notify--timer
             (run-with-timer claude-notify-debounce-seconds nil
                             #'claude-notify--send message)))
     ```
  2. **terminal-notifier group:** Use `-group` flag with a session-specific ID. New notifications with the same group ID replace the previous one rather than stacking.
     ```bash
     terminal-notifier -group "claude-SESSION_ID" -title "..." -message "..."
     ```
- Make debounce interval configurable via `defcustom`.
- Consider the `timeout.el` library (karthink/timeout, being upstreamed to Emacs) for production-quality throttle/debounce. Use `timeout-debounce` for notification suppression and `timeout-throttle` if you want at most one notification per N seconds.

**Detection:** If Notification Center shows multiple stacked notifications from a single Claude interaction, debouncing is insufficient.

**Confidence:** HIGH -- debounce pattern verified in [GNU Emacs Lisp Reference: Timers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html) and [karthink/timeout](https://github.com/karthink/timeout).

---

### Pitfall 8: Multiple Emacs Frames Across Spaces Confuse Frame-to-Space Mapping

**What goes wrong:** When a user has multiple Emacs frames on different macOS Spaces (e.g., Frame A on Space 1, Frame B on Space 3), the package must determine which frame contains the Claude buffer to report the correct Space number. Using `selected-frame` returns whichever frame was last focused, which may be on a different Space than the Claude buffer.

**Why it happens:** Emacs' `selected-frame` is a global concept that does not track which macOS Space a frame is on. When running as a daemon, the "selected frame" can be a virtual terminal frame with no Space association at all. macOS frame refocusing behavior is inconsistent: [closing a frame can switch you to a different Space](https://github.com/d12frosted/homebrew-emacs-plus/issues/628).

**Consequences:** Notification says "Space 1" but the Claude session is actually on Space 3. User navigates to wrong Space.

**Prevention:**
- Track the **buffer-to-frame mapping** explicitly. Use `get-buffer-window BUFFER 'visible` to find which window (and thus frame) displays the Claude buffer.
- When multiple frames exist, use `frame-list` and check each frame's windows for the Claude buffer.
- For Space detection, query the window manager for the specific window/frame that contains the Claude buffer, not just "the active space."
- With yabai: use the Emacs frame's window ID to query its space: `yabai -m query --windows --window WID | jq .space`.
- Consider caching the frame-to-space mapping and updating it only on `after-focus-change-function` hooks.
- Guard against daemon mode: check `(daemonp)` and skip frame-based detection when no graphical frames exist.

**Detection:** If Space number is wrong in multi-frame setups but correct in single-frame setups, this is the cause.

**Confidence:** MEDIUM -- frame behavior verified in [Emacs docs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Focus.html) and [homebrew-emacs-plus#628](https://github.com/d12frosted/homebrew-emacs-plus/issues/628). Space mapping specifics are LOW confidence (limited direct evidence for Emacs+yabai window ID queries).

---

### Pitfall 9: `terminal-notifier` Square Bracket Escaping Bug

**What goes wrong:** Square brackets `[` and `]` in notification title or subtitle cause the text to silently disappear. The notification is sent but the bracketed content is missing.

**Why it happens:** `terminal-notifier` has an argument-passing or escaping issue with square brackets. The exact mechanism is undocumented but confirmed empirically by d12frosted in his [Claude Code notifications implementation](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications).

**Consequences:** Notification appears but with missing or empty subtitle/title. Looks like a bug in the package.

**Prevention:**
- **Sanitize all notification text** before passing to `terminal-notifier`. Strip or replace square brackets.
  ```elisp
  (defun claude-notify--sanitize (text)
    (replace-regexp-in-string "[][]" "" text))
  ```
- Alternatively, replace `[` with `(` and `]` with `)` to preserve visual grouping.
- This does NOT affect `osascript display notification`, only `terminal-notifier`.

**Detection:** If notification titles/subtitles are occasionally blank or truncated, check for bracket characters in the source text.

**Confidence:** MEDIUM -- single source (d12frosted blog), but with empirical confirmation and code fix shown.

---

## Minor Pitfalls

These cause cosmetic issues or affect niche configurations.

### Pitfall 10: macOS Focus Mode Silently Swallows Notifications

**What goes wrong:** When macOS Focus mode (Do Not Disturb, Work, etc.) is active, notifications are silently queued and never delivered in real time.

**Why it happens:** macOS Focus mode suppresses notifications from applications that are not in the user's allow-list. There is no reliable public API to check if Focus mode is active (the `NSStatusItem` visible focus modes approach is limited to status bar items).

**Prevention:**
- Document this as a known limitation.
- Consider adding an optional **audible alert** (e.g., `(start-process "afplay" nil "afplay" "/System/Library/Sounds/Glass.aiff")`) as a secondary signal that bypasses Focus mode's notification suppression (sound alerts have separate Focus mode controls).
- If using terminal-notifier, the `-sound` flag adds a sound to the notification, but this sound is also subject to Focus mode.

**Confidence:** MEDIUM -- Focus mode behavior is documented by Apple; lack of programmatic detection API is based on web search (no official Apple docs found confirming or denying this).

---

### Pitfall 11: Notification Icon Shows Script Editor Instead of Claude

**What goes wrong:** When using `osascript display notification`, the notification icon shows the Script Editor icon (or Terminal icon), not Claude's icon. Since macOS Big Sur, custom icons in notifications are ignored; the sender application's icon is always used.

**Why it happens:** macOS enforces that the notification icon matches the sending application's bundle. `osascript` is part of Script Editor, so its icon appears.

**Prevention:**
- Use `terminal-notifier -sender com.anthropic.claudefordesktop` to display the Claude app icon. This requires Claude for Desktop to be installed on the system.
- Fall back to `-sender com.apple.Terminal` or omit the sender flag for a generic icon.
- Do NOT waste time trying to set custom icons via `-appIcon` or `-contentImage` -- they are ignored on Big Sur and later.

**Confidence:** HIGH -- confirmed by Apple documentation changes and [terminal-notifier README](https://github.com/julienXX/terminal-notifier).

---

### Pitfall 12: Process Filter Performance With High-Frequency Terminal Output

**What goes wrong:** If the package uses a process filter to monitor the Claude terminal for state changes (e.g., watching for "waiting for input" text), and Claude is producing high-volume output (large code blocks, streaming), the process filter can cause GC pressure and UI jank.

**Why it happens:** On macOS, subprocess output is routed through a 1024-byte bottleneck. Each chunk invokes the process filter, which runs arbitrary elisp. If the filter does string matching (regex or substring search) on every chunk, the overhead accumulates. `inhibit-quit` is set during filter execution, preventing the user from interrupting.

**Prevention:**
- Keep process filters **minimal**: buffer the output and process it on a timer, not synchronously in the filter.
- Avoid regex in process filters. Use simple `string-match-p` on small buffers only.
- Consider using `after-change-functions` on the terminal buffer instead of a raw process filter, which lets eat/vterm handle the buffering.
- If monitoring terminal output, use a **sampling approach**: check the buffer content on a periodic timer (e.g., every 2 seconds) rather than on every output chunk.
- Set `process-adaptive-read-buffering` to non-nil (default) to let Emacs batch small reads.

**Confidence:** HIGH for the mechanism -- documented in [GNU Emacs Manual: Output from Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Output-from-Processes.html) and [Filter Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html). MEDIUM for real-world impact in this specific use case (depends on implementation).

---

### Pitfall 13: `"Displays have separate Spaces"` Setting Changes Space Detection Behavior

**What goes wrong:** `CGSCopyManagedDisplaySpaces` (the private API for enumerating spaces) only works when "Displays have separate Spaces" is enabled in System Settings > Desktop & Dock. When disabled, the API returns different data structures and multi-monitor space detection breaks.

**Why it happens:** When "Displays have separate Spaces" is off, all monitors share a single set of Spaces. The private API structures assume per-display space stacks.

**Prevention:**
- Read the system preference: `defaults read com.apple.spaces spans-displays` (0 = separate spaces ON, 1 = separate spaces OFF).
- If using private APIs, handle both configurations or document the requirement.
- With yabai, this is handled transparently -- yabai normalizes the data.
- For most users, this setting is enabled by default and rarely changed.

**Confidence:** MEDIUM -- behavior described in [macOS defaults documentation](https://macos-defaults.com/mission-control/spans-displays.html) and community reports.

---

## Phase-Specific Warnings

| Phase Topic | Likely Pitfall | Mitigation |
|-------------|---------------|------------|
| Notification delivery | Pitfalls 2, 4, 5 compound: sync calls + focused suppression + Sequoia permissions = zero notifications | Use async `start-process` + non-Emacs sender + test command |
| Space detection | Pitfalls 3, 8, 13 compound: no public API + wrong frame + display settings = wrong space | Tiered detection with yabai preference, configurable, with graceful "unknown" fallback |
| Claude state detection | Pitfalls 1, 12 compound: false positives from hooks + perf overhead from filters = spam + jank | PermissionRequest hook as primary signal + debounced timer-based buffer sampling |
| User configuration | Pitfall 10 + permissions: Focus mode swallows + Sequoia permissions = silent failure | Mandatory test command on setup, prominent documentation of permission requirements |
| Multi-session support | Pitfall 7 + 8: multiple Claude sessions across frames/spaces = notification flood + wrong space | Per-session notification grouping + buffer-to-frame tracking |

---

## Sources

### Primary (HIGH confidence)
- [GNU Emacs Lisp Reference: Synchronous Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Synchronous-Processes.html)
- [GNU Emacs Lisp Reference: Asynchronous Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html)
- [GNU Emacs Lisp Reference: Filter Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html)
- [GNU Emacs Lisp Reference: Timers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html)
- [GNU Emacs Lisp Reference: Input Focus](https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Focus.html)
- [GNU Emacs Lisp Reference: Visibility of Frames](https://www.gnu.org/software/emacs/manual/html_node/elisp/Visibility-of-Frames.html)
- [Apple: NSUserNotificationCenter Deprecation](https://developer.apple.com/documentation/foundation/nsusernotificationcenter)
- [Apple: UNUserNotificationCenter](https://developer.apple.com/documentation/usernotifications/unusernotificationcenter)
- [anthropics/claude-code#8320: 60-Second Idle Notifications Not Triggering](https://github.com/anthropics/claude-code/issues/8320)
- [anthropics/claude-code#12048: Add notification matcher for waiting for user input](https://github.com/anthropics/claude-code/issues/12048)
- [anthropics/claude-code#13024: Add hook for when Claude is waiting for user input](https://github.com/anthropics/claude-code/issues/13024)
- [claude-code-ide.el source code](https://github.com/manzaltu/claude-code-ide.el) -- verified hooks, process management, WebSocket architecture

### Secondary (MEDIUM confidence)
- [d12frosted: Claude Code Notifications That Don't Suck](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications) -- yabai space detection, terminal-notifier bracket bug, sender flag approach
- [ianyh: Identifying Spaces in Mac OS X](https://ianyh.com/blog/identifying-spaces-in-mac-os-x/) -- CGWindowListCopyWindowInfo cross-referencing, private API limitations
- [alert.el issue #42: macOS foreground suppression](https://github.com/jwiegley/alert/issues/42)
- [homebrew-emacs-plus#628: Frame refocusing behavior broken](https://github.com/d12frosted/homebrew-emacs-plus/issues/628)
- [emacs-devel: ns-do-applescript discussion](https://lists.gnu.org/archive/html/emacs-devel/2022-05/msg01373.html)
- [karthink/timeout: Throttle/debounce for Emacs](https://github.com/karthink/timeout)
- [terminal-notifier GitHub](https://github.com/julienXX/terminal-notifier) -- group flag, sender flag, icon behavior
- [Late Night Software Forum: Sequoia display notification broken](https://forum.latenightsw.com/t/trying-to-use-terminal-for-display-notification/5068)

### Tertiary (LOW confidence)
- [macOS defaults: spans-displays setting](https://macos-defaults.com/mission-control/spans-displays.html) -- "Displays have separate Spaces" behavior
- [defaults read com.apple.spaces gist](https://gist.github.com/shlomiv/f61f300abdf6d7266d36dc2f7d3c3c32) -- stale plist data observation (single source)
- `CGSGetActiveSpace` / `CGSCopyManagedDisplaySpaces` stability across macOS Tahoe -- no direct evidence found, flagged for validation

---

## Metadata

**Confidence breakdown:**
- Notification delivery pitfalls: HIGH -- multiple official sources, verified with source code and bug reports
- Space detection pitfalls: MEDIUM -- private APIs inherently undocumented; community evidence strong but not Apple-official
- Process/performance pitfalls: HIGH -- documented in GNU Emacs manual
- Claude Code hook behavior: HIGH -- verified via GitHub issues with reproduction and maintainer acknowledgment
- macOS Sequoia/Tahoe changes: MEDIUM -- community reports from 2025-2026, no official Apple API documentation changes found

**Research date:** 2026-02-24
**Valid until:** 2026-04-24 (30 days -- macOS API stability is unpredictable; re-check after macOS updates)
