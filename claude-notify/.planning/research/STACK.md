# claude-notify -- Technology Stack

**Researched:** 2026-02-24
**Domain:** Emacs package development, macOS system integration (notifications + Spaces)
**Overall Confidence:** MEDIUM-HIGH

---

## Recommended Stack

### Core Framework

| Technology | Version | Purpose | Why | Confidence |
|-----------|---------|---------|-----|------------|
| Emacs Lisp (package) | Emacs 29+ | Package runtime | Target platform; Doom Emacs is the host | HIGH |
| Claude Code CLI Hooks | Current | Idle/input detection | Native `Notification` event with `idle_prompt` and `permission_prompt` matchers; fires when Claude genuinely needs attention. The only reliable detection method -- avoids fragile terminal output parsing entirely | HIGH |
| `ns-do-applescript` | Built-in (macOS Emacs) | Send macOS notifications | Ships with every macOS Emacs build; calls AppleScript **in-process** so the notification sender is Emacs itself (`org.gnu.Emacs`), avoiding the osascript permission bug on Sequoia/Tahoe where Terminal must separately be granted notification rights | HIGH |
| Custom Swift CLI (`claude-notify-space`) | Swift 6.2+ / swiftc | Query macOS Space number | No public API exists for Space numbers; must call private SkyLight functions. A ~50-line Swift tool compiled with `swiftc` is the minimal, dependency-free approach | MEDIUM |

### Supporting Libraries

| Technology | Version | Purpose | When to Use | Confidence |
|-----------|---------|---------|-------------|------------|
| `terminal-notifier` | 2.0.0 (Homebrew) | Fallback notification sender | Only if `ns-do-applescript` is unavailable (non-NS Emacs builds, e.g. emacs-nox) or user prefers custom icon/sound. Install via `brew install terminal-notifier` | MEDIUM |
| `alert.el` (jwiegley) | Latest from GitHub | Notification abstraction layer | NOT recommended as a dependency -- adds indirection for a single-platform package. Mention in docs as optional integration point for users who already use alert.el | LOW |

### Alternatives Considered

| Category | Recommended | Alternative | Why Not Default |
|----------|------------|-------------|-----------------|
| Notification delivery | `ns-do-applescript` | `terminal-notifier` via `start-process` | Adds an external dependency; `ns-do-applescript` is zero-dep on macOS Emacs. terminal-notifier uses deprecated `NSUserNotification` (broken on some Sequoia machines, GitHub issue #312). Keep as opt-in fallback. |
| Notification delivery | `ns-do-applescript` | `osascript` via `call-process` | Forks a separate process; on macOS Sequoia+ the osascript binary needs separate notification permission grant (user must run `display notification` in Script Editor first). `ns-do-applescript` runs in-process, so Emacs IS the sender. |
| Notification delivery | `ns-do-applescript` | `alert.el` | Adds a dependency for multi-platform abstraction we do not need (macOS-only package). Users who want alert.el integration can wire it themselves. |
| Space querying | Custom Swift CLI | yabai (`yabai -m query --spaces`) | Requires yabai to be installed and running. User has Amethyst, not yabai. Cannot assume yabai is present. |
| Space querying | Custom Swift CLI | Hammerspoon `hs.spaces` | Requires Hammerspoon running. Not installed on user's system. Heavy dependency for a single query. |
| Space querying | Custom Swift CLI | AppleScript / JXA | No AppleScript API for Spaces exists. Mission Control / System Events cannot expose Space index. |
| Space querying | Custom Swift CLI | SpaceInfo (davidpurnell) | Last release 2023, only 20 commits. Pre-built binary may not work on macOS Tahoe (26.x). Better to own the ~50 lines of Swift. |
| Idle detection | Claude Code CLI Hooks | Terminal output parsing (vterm) | Fragile; Claude Code's terminal output format is not a stable API. Hooks are the official, documented mechanism. |
| Idle detection | Claude Code CLI Hooks | `preferredNotifChannel terminal_bell` + bell detection | Only produces a bell character; no structured data about notification type. Cannot distinguish permission prompt from idle. |

---

## Detection Architecture: Claude Code Hooks

### How It Works

Claude Code CLI has a native hook system. The `Notification` event fires when Claude needs user attention. The hook receives JSON on stdin:

```json
{
  "session_id": "abc123",
  "cwd": "/Users/dario/project",
  "hook_event_name": "Notification",
  "notification_type": "idle_prompt"
}
```

**Available matchers for `Notification` event:**

| Matcher | Fires When |
|---------|-----------|
| `idle_prompt` | Claude finished responding, waiting for next user prompt |
| `permission_prompt` | Claude needs tool/action approval |
| `auth_success` | Authentication completed |
| `elicitation_dialog` | Claude is showing an interactive dialog |

### Hook Configuration

Add to `~/.claude/settings.json`:

```json
{
  "hooks": {
    "Notification": [
      {
        "matcher": "permission_prompt|idle_prompt",
        "hooks": [
          {
            "type": "command",
            "command": "emacsclient --eval '(claude-notify--on-hook-event)' --socket-name=/path/to/socket",
            "timeout": 5
          }
        ]
      }
    ]
  }
}
```

**Key design decision:** The hook calls `emacsclient --eval` to invoke an elisp function. This bridges the CLI hook into Emacs, where the package has access to frame/window context for Space number resolution.

### Why Not Parse Terminal Output

claude-code-ide.el uses vterm as its terminal backend. Monitoring vterm output for idle detection would require:
- Matching against unstable terminal escape sequences
- Detecting prompt patterns that change across Claude Code versions
- Handling ANSI color codes and cursor movements
- Racing with vterm's batched rendering (`claude-code-ide-vterm-render-delay`)

The CLI hooks approach is officially documented, stable, and provides structured JSON data.

---

## Notification Delivery: `ns-do-applescript`

### How It Works

On macOS, Emacs builds with the NS (NextStep/Cocoa) toolkit include `ns-do-applescript`, a built-in function that executes AppleScript synchronously within the Emacs process:

```elisp
(ns-do-applescript
 (format "display notification %s with title %s subtitle %s"
         (prin1-to-string message)
         (prin1-to-string "Claude Code")
         (prin1-to-string subtitle)))
```

### Why This Over osascript

| Factor | `ns-do-applescript` | `osascript` (call-process) | `terminal-notifier` |
|--------|--------------------|-----------------------------|---------------------|
| External dependency | None | None | Homebrew install |
| Process fork | No (in-process) | Yes | Yes |
| Notification sender | Emacs (`org.gnu.Emacs`) | osascript / Script Editor | terminal-notifier app |
| Sequoia permission bug | Not affected | Affected (must grant Terminal notification permission separately) | Not affected (own app bundle) |
| Icon in notification | Emacs icon | Script Editor icon | Configurable via `-sender` |
| Sound support | Yes (`sound name`) | Yes (`sound name`) | Yes (`-sound`) |
| Async capability | No (blocks briefly) | Via `start-process` | Via `start-process` |

**Critical advantage:** `ns-do-applescript` runs AppleScript as Emacs itself, so the notification appears under Emacs in System Settings > Notifications. The user only needs to allow notifications for Emacs once. With `osascript`, the user must separately grant notification permission to Terminal (or whatever process spawns osascript), which is confusing and breaks on fresh Sequoia installs.

### Fallback Chain

```
1. ns-do-applescript (preferred, zero-dep, in-process)
2. terminal-notifier via start-process (if ns-do-applescript unavailable)
3. osascript via call-process (last resort)
```

Detection at load time:

```elisp
(defvar claude-notify--notification-backend
  (cond
   ((fboundp 'ns-do-applescript) 'applescript)
   ((executable-find "terminal-notifier") 'terminal-notifier)
   ((executable-find "osascript") 'osascript)
   (t (warn "claude-notify: no notification backend available"))))
```

---

## Space Number Querying: Custom Swift CLI

### Why a Custom Tool

Apple provides **no public API** for querying macOS Space numbers. The information is behind private SkyLight framework functions. Every tool that provides Space numbers (yabai, Hammerspoon, SpaceId, alt-tab-macos) uses the same private APIs.

The user's system:
- macOS Tahoe 26.3 (latest)
- Amethyst WM (no CLI query API for spaces)
- No yabai, no Hammerspoon installed
- Swift 6.2.3 available via Xcode Command Line Tools

### The Private API Surface

From Hammerspoon's `private.h` and alt-tab-macos `Spaces.swift`:

```c
// SkyLight framework functions (undocumented, stable since macOS 10.11)
int SLSMainConnectionID(void);
uint64_t SLSGetActiveSpace(int cid);
CFArrayRef SLSCopyManagedDisplaySpaces(int cid);
CFArrayRef SLSCopySpacesForWindows(int cid, int selector, CFArrayRef window_list);
```

These functions live in `/System/Library/PrivateFrameworks/SkyLight.framework`. They have been stable across macOS versions since 10.11 (El Capitan) through 26.x (Tahoe), as evidenced by yabai, Hammerspoon, and alt-tab-macos continuing to work.

### Implementation Approach

A minimal Swift command-line tool (~50 lines) compiled with `swiftc`:

```swift
import Foundation

// Private SkyLight API declarations via C interop
@_silgen_name("SLSMainConnectionID")
func SLSMainConnectionID() -> Int32

@_silgen_name("SLSGetActiveSpace")
func SLSGetActiveSpace(_ cid: Int32) -> UInt64

@_silgen_name("SLSCopyManagedDisplaySpaces")
func SLSCopyManagedDisplaySpaces(_ cid: Int32) -> CFArray

@_silgen_name("SLSCopySpacesForWindows")
func SLSCopySpacesForWindows(_ cid: Int32, _ selector: Int32, _ windows: CFArray) -> CFArray

// ... implementation that maps space ID to 1-based index
```

**Compilation:** `swiftc -O -o claude-notify-space space-query.swift`

No Xcode project needed, no Swift Package Manager needed. Single file, single binary.

### Space ID to Index Mapping

`SLSGetActiveSpace()` returns an opaque space ID (e.g., `72`). To get the human-readable index (e.g., `3`):

1. Call `SLSCopyManagedDisplaySpaces()` to get all displays and their ordered space lists
2. For each display, iterate the `Spaces` array
3. Match the active space ID against the ordered list
4. The position in the list (1-based) is the Space number

For querying which space a **specific window** (not just the active space) belongs to:
1. Get the Emacs frame's window number via `(frame-parameter frame 'window-id)` -- this returns the NSWindow number
2. Pass it to `SLSCopySpacesForWindows()` to get the space ID
3. Map the space ID to index as above

### Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| Apple removes/renames SLS functions | LOW (stable 10+ years, used by yabai/Hammerspoon/alt-tab) | Graceful degradation: if binary fails, omit Space number from notification |
| `@_silgen_name` behavior changes in future Swift | LOW (used extensively in ecosystem) | Alternative: use C bridging header instead |
| Binary doesn't work on future macOS | MEDIUM (private API, no guarantees) | Check exit code; ship updated binary when needed |
| SIP blocks access | LOW (reading space info doesn't require SIP bypass) | These are read-only queries, not space manipulation |

**Important note on `@_silgen_name`:** While it works, the more robust approach is a C bridging header file declaring the functions as `extern`. This avoids reliance on the underscore-prefixed Swift attribute. Both approaches produce identical binaries.

Alternative to `@_silgen_name` -- C header approach:

```c
// private_apis.h
#pragma once
#include <CoreGraphics/CoreGraphics.h>
extern int SLSMainConnectionID(void);
extern uint64_t SLSGetActiveSpace(int cid);
extern CFArrayRef SLSCopyManagedDisplaySpaces(int cid);
extern CFArrayRef SLSCopySpacesForWindows(int cid, int selector, CFArrayRef windows);
```

Compile: `swiftc -O -import-objc-header private_apis.h -o claude-notify-space space-query.swift`

---

## Emacs Package Structure

### Recommended Layout

```
claude-notify/
  claude-notify.el          # Main package file (provide 'claude-notify)
  claude-notify-space.swift # Swift source for Space query tool
  claude-notify-space.h     # C bridging header for private APIs
  Makefile                  # Compiles Swift tool
  README.md                 # (only if requested)
```

### Package Conventions

Following Doom Emacs and the user's existing package patterns:

| Convention | Value | Rationale |
|-----------|-------|-----------|
| Feature name | `claude-notify` | Matches package name, standard Emacs convention |
| Prefix | `claude-notify-` / `claude-notify--` | Public / private symbols |
| Package header | Standard library header | Required by `package.el` and straight.el |
| Autoloaded commands | `claude-notify-mode`, `claude-notify-setup` | Entry points for users |
| Location | `~/code/emacs-packages/claude-notify` | User's convention per MEMORY.md |
| Distribution | GitHub `dakling/claude-notify` | User's convention per MEMORY.md |
| Doom integration | `doom/packages.el` recipe + `doom/config.el` `use-package!` | Standard Doom pattern |
| Dependencies in header | None (or `emacs "29.1"`) | Minimal deps per project constraint |

### Key Elisp Patterns

**Defcustom for all user-facing configuration** (use `setq!` in Doom):

```elisp
(defcustom claude-notify-enabled t
  "Whether to send notifications when Claude needs input."
  :type 'boolean
  :group 'claude-notify)

(defcustom claude-notify-space-binary
  (expand-file-name "claude-notify-space"
                    (file-name-directory (locate-library "claude-notify")))
  "Path to the Space query binary."
  :type 'file
  :group 'claude-notify)

(defcustom claude-notify-suppress-if-focused t
  "Suppress notification if Emacs frame with the session is focused."
  :type 'boolean
  :group 'claude-notify)
```

**Async process calls** for the Space binary (never block Emacs):

```elisp
(defun claude-notify--query-space (callback)
  "Query the current Space number asynchronously, call CALLBACK with result."
  (let ((proc (make-process
               :name "claude-notify-space"
               :command (list claude-notify-space-binary "--active-space")
               :filter (lambda (_proc output)
                         (funcall callback (string-trim output))))))
    proc))
```

### Integration with claude-code-ide.el

The package does NOT patch or fork claude-code-ide.el. Integration approach:

1. **Hook-side integration:** Claude Code CLI `Notification` hook calls `emacsclient --eval` to invoke `claude-notify--on-hook-event`
2. **Emacs-side integration:** The function identifies which claude-code-ide session buffer triggered the notification by matching `session_id` or `cwd` against `claude-code-ide--processes`
3. **Frame resolution:** From the session buffer, find which frame displays it, get the frame's `window-id`, pass to Space query tool
4. **Notification dispatch:** Fire notification with Space number context

```elisp
;; Access claude-code-ide session registry (read-only, no patching)
(defun claude-notify--find-session-frame (cwd)
  "Find the Emacs frame displaying the claude-code-ide session for CWD."
  (let* ((buf-name (funcall claude-code-ide-buffer-name-function cwd))
         (buf (get-buffer buf-name)))
    (when buf
      (car (cl-remove-if-not
            (lambda (frame)
              (memq buf (mapcar #'window-buffer (window-list frame))))
            (frame-list))))))
```

---

## Installation

### For Users

In `doom/packages.el`:
```elisp
(package! claude-notify
  :recipe (:host github :repo "dakling/claude-notify"))
```

In `doom/config.el`:
```elisp
(use-package! claude-notify
  :after claude-code-ide
  :config
  (claude-notify-setup))  ;; Compiles Swift binary if needed, registers hooks
```

### First-Time Setup

The `claude-notify-setup` function should:

1. Check if the Swift binary exists at `claude-notify-space-binary`
2. If not, compile it from the bundled `.swift` source using `swiftc`
3. Register the Claude Code CLI notification hook by writing to `~/.claude/settings.json` (or prompt user to do it manually)
4. Verify Emacs has notification permissions (test with a silent `ns-do-applescript` call)

---

## Sources

### Primary (HIGH confidence)

- [Claude Code Hooks Documentation](https://code.claude.com/docs/en/hooks-guide) -- Complete hook event types, matchers, JSON schema, configuration
- [Hammerspoon spaces/private.h](https://github.com/Hammerspoon/hammerspoon/blob/master/extensions/spaces/private.h) -- SkyLight private API function signatures
- [alt-tab-macos Spaces.swift](https://github.com/lwouis/alt-tab-macos/blob/master/src/logic/Spaces.swift) -- Production usage of SLSCopyManagedDisplaySpaces, space ID to index mapping
- [Hammerspoon hs.spaces docs](https://www.hammerspoon.org/docs/hs.spaces.html) -- API surface for space querying, confirmed experimental/private status

### Secondary (MEDIUM confidence)

- [Boris Buliga: Claude Code Notifications That Don't Suck](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications) -- Production notification implementation with yabai space awareness, terminal-notifier with `-sender` flag
- [alexop.dev: Claude Code Notification Hooks](https://alexop.dev/posts/claude-code-notification-hooks/) -- Hook configuration examples, matcher documentation
- [terminal-notifier GitHub](https://github.com/julienXX/terminal-notifier) -- v2.0.0, Homebrew availability, known Sequoia issues (issue #312)
- [Late Night Software forum: display notification Sequoia](https://forum.latenightsw.com/t/trying-to-use-terminal-for-display-notification/5068) -- osascript permission issues on Sequoia, fix requires running in Script Editor first
- [jwiegley/alert GitHub](https://github.com/jwiegley/alert) -- 13 notification backends, osx-notifier style uses AppleScript
- [claudemacs](https://github.com/cpoile/claudemacs) -- Uses `preferredNotifChannel terminal_bell` + AppleScript for notifications
- [w3tutorials: notification icon with osascript](https://www.w3tutorials.net/blog/change-icon-of-notification-when-using-osascript-e-display-notification/) -- osascript sender identity and icon behavior

### Tertiary (LOW confidence)

- [ianyh: Identifying Spaces in Mac OS X](https://ianyh.com/blog/identifying-spaces-in-mac-os-x/) -- Historical reference for space identification approach (NSUserDefaults + CGWindowListCopyWindowInfo cross-referencing). Author of Amethyst.
- [davidpurnell/SpaceInfo](https://github.com/davidpurnell/SpaceInfo) -- Swift CLI for space info, last release 2023. Validates the approach but stale.
- [dshnkao/SpaceId](https://github.com/dshnkao/SpaceId) -- Menu bar space indicator, last release 2021. Confirms private API usage pattern.
- [GitHub issue #12048: notification matcher for waiting](https://github.com/anthropics/claude-code/issues/12048) -- Feature request for better notification matchers (closed as duplicate of #10168, work underway)

---

## Metadata

**Confidence breakdown:**

| Area | Level | Reason |
|------|-------|--------|
| Idle detection (CLI Hooks) | HIGH | Official Claude Code documentation with examples, multiple community implementations confirm |
| Notification delivery (ns-do-applescript) | HIGH | Built-in Emacs function, well-documented, confirmed working on macOS |
| Space querying (SkyLight private API) | MEDIUM | Private API, no official documentation; but stable 10+ years, used by major projects (yabai, Hammerspoon, alt-tab-macos). Verified function signatures from multiple sources. |
| Package structure | HIGH | Standard Emacs conventions, user's existing patterns documented in CLAUDE.md and MEMORY.md |
| osascript Sequoia issues | MEDIUM | Multiple forum reports confirm the permission issue; workaround documented but not officially acknowledged by Apple |

**Research date:** 2026-02-24
**Valid until:** 2026-04-24 (stable domain; private API risk is the main volatility factor)
**User's macOS version:** Tahoe 26.3 (BuildVersion 25D125)
**User's Swift version:** 6.2.3
