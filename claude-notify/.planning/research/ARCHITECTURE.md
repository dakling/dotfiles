# claude-notify Architecture Research

**Researched:** 2026-02-24
**Domain:** Emacs package + macOS system integration (notifications, Spaces)
**Confidence:** HIGH (verified via live testing on the target system)

## Recommended Architecture

claude-notify is a three-layer system:

1. **State Detection Layer** -- hooks into claude-code-ide.el to detect when a session needs user input
2. **Space Resolution Layer** -- determines which macOS Space hosts the Emacs frame for that session
3. **Notification Delivery Layer** -- fires a macOS notification with session context and Space number

```
+-----------------------+
| claude-code-ide.el    |  Upstream (unmodified)
|  process sentinel     |
|  MCP session state    |
|  hash tables          |
+---------+-------------+
          | advice / polling
+---------v-------------+
| claude-notify.el      |  This package
|  state-detection      |
|  space-resolution     |
|  notification-delivery|
+---------+-------------+
          |
   +------+------+
   |              |
   v              v
Swift helper   osascript
(space query)  (notification)
```

### Component Boundaries

| Component | Responsibility | Communicates With |
|-----------|---------------|-------------------|
| `claude-notify.el` | Orchestrator: detection, suppression, delivery | claude-code-ide.el (read-only), Swift helper (subprocess), osascript (subprocess) |
| `claude-notify-space` (Swift binary) | Query macOS Space index for a given PID + window position | CoreGraphics private APIs via CGS |
| `osascript` (system binary) | Deliver macOS notification center alerts | macOS Notification Center |

## Data Flow

### Session State Detection

**Problem:** claude-code-ide.el does not expose hooks for "Claude is waiting for input." There is no `claude-code-ide-session-idle-hook` or similar.

**Available state signals (verified by reading source):**

1. **`claude-code-ide--processes` hash table** (project-dir -> process) -- can enumerate all live sessions
2. **`claude-code-ide-mcp--sessions` hash table** (project-dir -> mcp-session struct) -- has client connection state
3. **Process sentinels** on the vterm process -- fire on exit/kill/signal, NOT on idle
4. **`claude-code-ide-mcp-session-client`** -- non-nil when WebSocket is connected (Claude is running)
5. **Buffer content** -- the vterm buffer contains terminal output, but parsing it for prompt state is fragile

**Claude Code CLI hooks (external, not in Emacs):**

Claude Code CLI has a `Notification` hook system configured in `~/.claude/settings.json`:
- Matcher `idle_prompt` fires when Claude finishes and waits for input (but with known false-positive issues)
- Matcher `permission_prompt` fires when Claude needs permission approval
- Matcher `elicitation_dialog` fires for interactive dialogs
- Hook commands receive JSON on stdin with `session_id`, `cwd`, `message`

**Recommended approach: Dual-strategy detection**

| Strategy | Mechanism | Latency | Reliability |
|----------|-----------|---------|-------------|
| **Primary: CLI hook -> emacsclient** | Configure Claude Code `Notification` hook to call `emacsclient --eval` | Immediate | HIGH -- uses Claude Code's own notification system |
| **Fallback: Terminal output polling** | Timer-based scan of vterm buffer for prompt patterns | 1-5 seconds | MEDIUM -- prompt format may change across CLI versions |

**Rationale for Primary strategy:** Claude Code's hook system fires a shell command when the `Notification` event occurs. That command can invoke `emacsclient --eval '(claude-notify--on-cli-notification ...)'` with the session_id and notification type. This avoids all heuristics -- Claude Code itself tells us when it needs input.

**Rationale for Fallback:** If the user hasn't configured Claude Code hooks, or if hooks fail, a timer can scan the Claude vterm buffer for the presence of a prompt (the `>` character at the end of terminal output with no active subprocess output). This is less reliable but works without configuration.

### Space Resolution

**Problem:** Given an Emacs frame hosting a Claude session, determine which macOS Space (1-based index) it occupies.

**Verified approach (tested live on this system):**

1. Emacs provides `(emacs-pid)` and `(frame-parameter f 'left)` / `(frame-parameter f 'top)` for each frame
2. A Swift helper uses `CGSCopyManagedDisplaySpaces()` + `CGSCopySpacesForWindows()` (private CoreGraphics APIs) to:
   - Enumerate all Spaces across all displays, building a Space-ID-to-index mapping
   - Find CGS windows belonging to the Emacs PID
   - Return the Space index for each window
3. Emacs correlates frames to CGS windows by matching pixel positions (left, top)

**Verified test results:**
- Emacs frame reports `left: 1057, top: (+ -1080)` (the `(+` prefix means offset)
- CGS window reports `x: 1057, y: -1080, space: 8`
- Position match is exact -- this is a reliable correlation

**Swift helper design:**

```
Input:  PID (integer, via command-line argument)
Output: JSON array of {windowId, space, x, y, width, height}
Binary: ~50 lines of Swift, compiles to standalone binary, no frameworks needed beyond CoreGraphics
```

The helper must be compiled once (e.g., `swiftc -O -o claude-notify-space claude-notify-space.swift`). It does NOT require SIP to be disabled -- these CGS functions work on stock macOS.

**Alternative approaches considered:**

| Approach | Why Not |
|----------|---------|
| **WhichSpace.app AppleScript** | Requires installing a third-party app; user doesn't have it |
| **yabai query** | Requires yabai; user doesn't have it |
| **NSWindow.isOnActiveSpace via ObjC bridge** | Only tells if window is on *active* space, not *which* space |
| **AppleScript "System Events"** | Cannot query Space numbers -- no scripting dictionary for Spaces |
| **Pure Emacs (no helper)** | Emacs has no FFI to call CGS functions; `ns-do-applescript` can't call C functions |

### Notification Delivery

**Verified approach:**

```bash
osascript -e 'display notification "Claude needs input on Space 8" with title "Claude Code" subtitle "Project: dotfiles"'
```

This is available on every macOS system (`/usr/bin/osascript`), requires no installation, and delivers to macOS Notification Center natively.

**From Emacs, two delivery options:**

| Method | Pros | Cons |
|--------|------|------|
| `(ns-do-applescript ...)` | No subprocess spawn, faster (~5ms) | Only available in NS Emacs builds |
| `(call-process "osascript" ...)` | Works in any Emacs build | Subprocess spawn (~50ms) |

**Recommendation: Use `ns-do-applescript` with `call-process` fallback.** The NS build check is `(fboundp 'ns-do-applescript)`.

**Notification content template:**

```
Title:    "Claude Code"
Subtitle: "Space {N} - {project-name}"
Body:     "{notification-type}: {message-excerpt}"
```

**Notification suppression rules:**

1. **Same-space suppression:** Do not notify if the user is currently on the same Space as the session (they can already see it)
2. **Cooldown timer:** After notifying for a session, suppress further notifications for that session for N seconds (configurable, default 30)
3. **Focus suppression:** If the Claude buffer is currently visible in a window, suppress
4. **Interaction reset:** When the user sends input to a session (detected via `post-command-hook` in the claude buffer), reset the suppression state

## Patterns to Follow

### Pattern 1: Advice-based hooking into upstream packages

**What:** Use `advice-add` to observe upstream function calls without modifying the upstream package.

**When to use:** When the upstream package (claude-code-ide.el) doesn't provide hooks but you need to react to its state changes.

**Example -- advising process cleanup to detect session end:**

```elisp
(defun claude-notify--after-cleanup (orig-fn directory)
  "Advice after claude-code-ide cleanup to clear notification state."
  (let ((result (funcall orig-fn directory)))
    (claude-notify--session-ended directory)
    result))

(advice-add 'claude-code-ide--cleanup-on-exit :around #'claude-notify--after-cleanup)
```

**Example -- advising session start to register for notifications:**

```elisp
(defun claude-notify--after-session-start (orig-fn &optional continue resume)
  "Advice after session start to set up notification tracking."
  (let ((result (funcall orig-fn continue resume)))
    (claude-notify--session-started (claude-code-ide--get-working-directory))
    result))

(advice-add 'claude-code-ide--start-session :around #'claude-notify--after-session-start)
```

### Pattern 2: emacsclient callback from CLI hooks

**What:** Configure Claude Code's hook system to call back into Emacs when notification events occur.

**When to use:** For the primary detection strategy -- receiving immediate "needs input" signals from Claude Code.

**Example -- hook configuration auto-generated by the package:**

```elisp
(defun claude-notify--ensure-cli-hook ()
  "Ensure Claude Code's Notification hook calls back to Emacs."
  (let* ((server-socket (expand-file-name server-name server-socket-dir))
         (hook-cmd (format "emacsclient -s %s --eval '(claude-notify--on-cli-notification)'"
                           (shell-quote-argument server-socket))))
    ;; Write or verify hook in ~/.claude/settings.json
    ;; Only if user has opted in via customize
    ))
```

**Rationale:** Using the full socket path (`server-socket-dir` + `server-name`) avoids hitting stale/wrong Emacs instances (per MEMORY.md convention).

### Pattern 3: Async subprocess for Space resolution

**What:** Call the Swift helper asynchronously so Emacs doesn't block while querying CGS.

**When to use:** Every time a notification is about to fire.

**Example:**

```elisp
(defun claude-notify--get-frame-space (frame callback)
  "Get the Space index for FRAME, calling CALLBACK with the result."
  (let* ((pid (emacs-pid))
         (left (frame-parameter frame 'left))
         (top (let ((raw (frame-parameter frame 'top)))
                (if (and (listp raw) (eq (car raw) '+))
                    (cadr raw)
                  raw))))
    (make-process
     :name "claude-notify-space"
     :command (list claude-notify-space-binary (number-to-string pid))
     :filter (lambda (proc output)
               ;; Parse JSON, find window matching (left, top), extract space index
               (let* ((json (json-read-from-string output))
                      (match (seq-find
                              (lambda (w)
                                (and (= (alist-get 'x w) left)
                                     (= (alist-get 'y w) top)))
                              json)))
                 (funcall callback (if match (alist-get 'space match) nil)))))))
```

### Pattern 4: Customization group following Doom conventions

**What:** Use `defcustom` with `setq!` in Doom config.

**Example:**

```elisp
(defgroup claude-notify nil
  "macOS notifications for Claude Code sessions."
  :group 'claude-code-ide
  :prefix "claude-notify-")

(defcustom claude-notify-enabled t
  "Enable macOS notifications for Claude Code sessions."
  :type 'boolean
  :group 'claude-notify)

(defcustom claude-notify-cooldown 30
  "Seconds to suppress repeat notifications for the same session."
  :type 'integer
  :group 'claude-notify)

(defcustom claude-notify-suppress-same-space t
  "Suppress notifications when the user is on the same Space as the session."
  :type 'boolean
  :group 'claude-notify)
```

## Anti-Patterns to Avoid

- **Modifying claude-code-ide.el directly:** Never patch upstream. Use advice, hash table inspection, and CLI hooks instead. The upstream package is installed via straight.el and will be overwritten on `doom sync -u`.

- **Synchronous subprocess calls in tight loops:** The Swift Space helper and osascript must be called asynchronously or only on notification events (not on every `post-command-hook`).

- **Parsing vterm buffer content with regex as the primary strategy:** Terminal escape sequences, cursor positioning, and CLI version changes make this fragile. Use it only as a fallback.

- **Installing the Claude Code CLI hook without user consent:** The package should provide a command to set up the hook, not do it automatically on load. Modifying `~/.claude/settings.json` is a side effect the user must opt into.

- **Assuming single-frame Emacs:** The user runs multiple Emacs frames across Spaces. The architecture must handle N frames, each potentially hosting a different Claude session.

## Scalability Considerations

| Concern | At 1 session | At 3 sessions | At 10 sessions |
|---------|-------------|---------------|----------------|
| Space queries | 1 subprocess call per notification | 1 call (queries all windows by PID) | Same -- 1 call returns all frames |
| Notification suppression | Simple per-session timer | Hash table of cooldowns | Same hash table |
| vterm polling (fallback) | 1 buffer to scan | 3 buffers to scan on timer | May need longer timer interval |
| CLI hook callbacks | 1 emacsclient call | 3 independent calls | 10 independent calls (parallel OK) |

## Recommended Project Structure

```
claude-notify/
  claude-notify.el              -- Main package: setup, customization, orchestrator
  claude-notify-detect.el       -- State detection (CLI hook callback + fallback polling)
  claude-notify-space.el        -- Space resolution (Swift helper management, frame correlation)
  claude-notify-deliver.el      -- Notification delivery (osascript / ns-do-applescript)
  bin/
    claude-notify-space.swift   -- Swift helper source (compiled on first use or install)
  Makefile                      -- Compiles Swift helper
```

**Rationale for splitting into 4 elisp files:**
- Each file handles one concern (detection, space, delivery)
- The main file ties them together and provides `claude-notify-setup` / `claude-notify-mode`
- Users who don't want Space resolution (e.g., single-Space users) can disable just that layer

**Simpler alternative (if premature):** Put everything in a single `claude-notify.el` with clear section headers. Split later if it exceeds ~500 lines.

**Recommendation: Start with single file.** The package is small enough that separation adds overhead without benefit at MVP. Split when complexity warrants it.

## Sources

### Primary (HIGH confidence)

- **claude-code-ide.el source** (`~/.config/emacs/.local/straight/repos/claude-code-ide.el/`) -- Read all major files: `claude-code-ide.el`, `claude-code-ide-mcp.el`, `claude-code-ide-mcp-server.el`, `claude-code-ide-debug.el`. Verified hash table structures, process management, session lifecycle, and absence of user-facing hooks.

- **Live CGS API testing** -- Verified `CGSCopyManagedDisplaySpaces`, `CGSGetActiveSpace`, `CGSCopySpacesForWindows` all work on this macOS system (Darwin 25.3.0) without SIP modification. Confirmed Emacs frame positions match CGS window positions exactly.

- **Claude Code hooks documentation** ([code.claude.com/docs/en/hooks-guide](https://code.claude.com/docs/en/hooks-guide)) -- Verified `Notification` hook with `idle_prompt` and `permission_prompt` matchers. Confirmed JSON stdin format with `session_id`, `cwd`, `message` fields. Confirmed `osascript` as recommended notification delivery on macOS.

- **Emacs frame parameters** -- Verified `(frame-parameter f 'left)`, `(frame-parameter f 'top)`, `(emacs-pid)` provide the data needed for CGS window correlation.

### Secondary (MEDIUM confidence)

- **[WhichSpace](https://github.com/gechr/WhichSpace)** -- Confirms AppleScript API `tell application "WhichSpace" to get current space number` works for getting space index, but requires installing the app. Not suitable as a dependency.

- **[alt-tab-macos Spaces.swift](https://github.com/lwouis/alt-tab-macos/blob/master/src/logic/Spaces.swift)** -- Reference implementation for CGS Space enumeration in Swift. Confirmed the `id64` key and sequential index computation pattern.

- **[Claude Code issue #12048](https://github.com/anthropics/claude-code/issues/12048)** -- Documents known issues with `idle_prompt` matcher: fires on every response completion, not just when genuinely waiting. False positives are a known problem. The `permission_prompt` matcher is more reliable.

- **[Claude Code issue #21238](https://github.com/anthropics/claude-code/issues/21238)** -- Feature request for immediate notification when Claude awaits input. Still open as of Feb 2026. Indicates the CLI notification system is actively evolving.

- **[jwiegley/alert](https://github.com/jwiegley/alert)** -- Emacs notification framework with `osx-notifier` style. Considered but rejected: adds a dependency for something achievable with 3 lines of `ns-do-applescript`. Could be an optional backend later.

### Tertiary (LOW confidence)

- **[osx-space-id](https://github.com/shabble/osx-space-id)** -- Old Carbon+Cocoa utility for Space ID queries. Likely outdated but confirms the approach is well-established.

- **[CGSInternal/CGSSpace.h](https://github.com/NUIKit/CGSInternal/blob/master/CGSSpace.h)** -- Header file documenting private CGS Space functions. Useful reference for function signatures.

## Metadata

**Confidence breakdown:**
- State detection: HIGH -- verified by reading claude-code-ide.el source and Claude Code CLI docs
- Space resolution: HIGH -- verified with live Swift code execution on the target system
- Notification delivery: HIGH -- verified with live `osascript` call
- Frame-to-window correlation: HIGH -- verified position match between Emacs frame params and CGS window bounds

**Research date:** 2026-02-24
**Valid until:** 2026-04-24 (stable -- CGS APIs change rarely, claude-code-ide.el API may evolve)

## Open Questions

1. **idle_prompt reliability:** The `idle_prompt` matcher has known false-positive issues (fires on every response, not just when genuinely idle). The fallback polling strategy may need to be the primary strategy until Claude Code improves its hook matchers. Monitor [issue #21238](https://github.com/anthropics/claude-code/issues/21238) for upstream improvements.

2. **Multi-display Space indexing:** The current Swift helper builds a global index across all displays. If the user has 3 displays with separate Space sets, Space "3" might be on Display 2. Need to decide: global index or per-display index? The live test showed 9 spaces across 3 displays. WhichSpace uses global indexing. **Recommendation: Use global indexing to match macOS Mission Control behavior.**

3. **Swift binary distribution:** Should the package ship a pre-compiled binary, compile on first use, or require the user to compile manually? Pre-compiled risks architecture mismatch (arm64 vs x86_64). Compile-on-first-use requires Xcode CLI tools. **Recommendation: Compile on first use with `swiftc`, with clear error message if Swift compiler is not found.**

4. **Hook installation UX:** The primary detection strategy requires adding a hook to `~/.claude/settings.json`. Should the package auto-install it, provide a setup command, or document manual setup? **Recommendation: Provide `claude-notify-install-hook` interactive command that reads current settings, merges the hook, and writes back. Never auto-install.**

5. **CGS API stability across macOS versions:** These are private APIs. They have been stable for 10+ years (since Snow Leopard era) but could theoretically break. The package should handle gracefully if the Swift helper returns an error. **Recommendation: Fall back to "Space ?" in the notification if Space resolution fails.**
