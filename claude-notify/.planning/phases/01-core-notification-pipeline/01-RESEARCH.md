# Phase 01: Core Notification Pipeline - Research

**Researched:** 2026-02-24
**Domain:** Emacs Lisp package development, Claude Code CLI hooks, macOS notification delivery
**Confidence:** HIGH

## Summary

Phase 1 delivers the end-to-end notification pipeline: Claude Code CLI fires a `Notification` hook, which calls `emacsclient --eval` to invoke an Elisp handler, which sends a macOS notification with the project/session identifier. The entire chain is verified and working on the target system.

The critical technical discovery is that **`ns-do-applescript` blocks Emacs for ~700ms** when calling `display notification`, making it unsuitable as the default backend. The async `start-process "osascript"` approach returns in ~10ms and should be the default. `ns-do-applescript` can be an opt-in alternative for users who want the notification attributed to Emacs (rather than Script Editor) in System Settings.

The `server-eval-args-left` mechanism in Emacs 30+ provides a clean way to pass the hook's JSON payload from the shell script to the Elisp handler without complex escaping. This was verified live on the target system (Emacs 30.2).

**Primary recommendation:** Use `start-process "osascript"` as the default notification backend with `ns-do-applescript` as an opt-in synchronous alternative. Use a shell script as the hook command that reads stdin JSON via `cat`, then passes it to `emacsclient --eval` via `server-eval-args-left`.

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| DET-01 | CLI hook callback handler -- receive notifications from Claude Code via `emacsclient --eval` | Verified: `server-eval-args-left` in Emacs 30.2 allows clean JSON passing. Shell script reads stdin with `cat` and passes as emacsclient argument. Full round-trip tested. |
| DET-02 | Hook installation helper (`M-x claude-notify-install-hook`) to configure `~/.claude/settings.json` | Verified: settings.json structure understood. Existing hooks must be preserved (PreToolUse, SessionStart already present). JSON merge strategy required -- read, parse, add Notification entry, write back. |
| DET-03 | `permission_prompt` detection (immediate, reliable signal) | Verified: `Notification` hook with matcher `permission_prompt` fires immediately when Claude needs permission approval. The `notification_type` field in stdin JSON identifies the event type. This is the most reliable signal (unlike `idle_prompt` which has false-positive issues). |
| NTF-01 | `ns-do-applescript` as primary notification backend (in-process, fast) | **Revised recommendation:** `ns-do-applescript` blocks Emacs for ~700ms per `display notification` call (measured live). Use as opt-in only, not default. Available on this system (`(fboundp 'ns-do-applescript)` returns `t`). |
| NTF-02 | `start-process osascript` as async fallback backend | **Revised to default:** `start-process "osascript"` returns in ~10ms (non-blocking). Should be the DEFAULT backend, not fallback. Works on every macOS system. Notification is attributed to Script Editor (avoids Pitfall 4: macOS focus suppression). |
| NTF-03 | Session/project identifier in notification text | Verified: Hook stdin JSON includes `cwd` field (absolute path to project directory). Extract project name via `(file-name-nondirectory cwd)`. Also has `session_id` for unique identification. |
| NTF-05 | `claude-notify-mode` global minor mode toggle | Standard Emacs pattern. Use `define-globalized-minor-mode` or `define-minor-mode` with `:global t`. Persist via Emacs customization system (`defcustom` + `custom-set-variables`). |
</phase_requirements>

## Standard Stack

### Core

| Library/Tool | Version | Purpose | Why Standard | Confidence |
|-------------|---------|---------|--------------|------------|
| Emacs Lisp (single file) | Emacs 29.1+ (30+ for `server-eval-args-left`) | Package runtime | Target platform; all code runs inside Emacs | HIGH |
| Claude Code CLI Hooks | Current (Feb 2026) | Event source | Official `Notification` hook fires on `permission_prompt`, delivers JSON on stdin | HIGH |
| `emacsclient --eval` | Emacs 30.2 | CLI-to-Emacs bridge | Uses `server-eval-args-left` for safe JSON argument passing without shell escaping | HIGH |
| `osascript` via `start-process` | macOS built-in | Notification delivery | Zero-dependency, async (10ms), available on every macOS | HIGH |
| `jq` (in hook script) | System or Homebrew | JSON parsing in shell | Used by the hook shell script to extract fields from stdin JSON before passing to emacsclient | MEDIUM |

### Supporting

| Library/Tool | Version | Purpose | When to Use | Confidence |
|-------------|---------|---------|-------------|------------|
| `ns-do-applescript` | Emacs built-in (NS builds) | Opt-in synchronous notification | User wants notification attributed to Emacs in System Settings > Notifications | HIGH |
| `json-parse-string` / `json-read-from-string` | Emacs built-in | Parse JSON in Elisp | Parse the hook JSON payload inside the callback function | HIGH |

### Alternatives Considered

| Instead of | Could Use | Tradeoff |
|-----------|-----------|----------|
| `start-process "osascript"` (async) | `ns-do-applescript` (sync) | Blocks Emacs ~700ms. Notification attributed to Emacs (good for System Settings, bad for focus suppression). Only available in NS builds. |
| Shell script reading stdin | Inline command in settings.json | Inline `bash -c '...'` becomes unwieldy for multi-step logic (read stdin, extract fields, call emacsclient). Separate script is cleaner and testable. |
| `jq` in hook script | Pure bash JSON parsing | `jq` is standard on dev machines. Pure bash parsing of JSON is fragile. Can fallback to passing raw JSON and parsing in Elisp. |
| `server-eval-args-left` | Shell-escape JSON into `--eval` string | Extremely fragile: JSON with quotes, backslashes, special chars breaks. `server-eval-args-left` solves this cleanly. Requires Emacs 30+. |
| `server-eval-args-left` | Write JSON to temp file, read in Elisp | Extra I/O, cleanup needed. `server-eval-args-left` is cleaner for small payloads. |

## Architecture Patterns

### Recommended Project Structure

```
claude-notify/
  claude-notify.el           # Single file: mode, hook handler, notification dispatch, install-hook
  bin/
    claude-notify-hook.sh    # Shell script called by Claude Code hook (reads stdin, calls emacsclient)
```

**Rationale:** Single `.el` file for Phase 1 (package is small). Shell script in `bin/` is the hook command registered in `settings.json`. Split `.el` into multiple files only when it exceeds ~500 lines (Phase 2+).

### Pattern 1: Hook Shell Script (the bridge)

**What:** A shell script that Claude Code calls as a hook command. It reads JSON from stdin, then invokes `emacsclient --eval` passing the JSON via `server-eval-args-left`.

**When to use:** This is the ONLY way to bridge Claude Code CLI hooks into Emacs. The hook system provides JSON on stdin; `emacsclient --eval` provides `server-eval-args-left` for safe argument passing.

**Example:**

```bash
#!/usr/bin/env bash
# bin/claude-notify-hook.sh
# Called by Claude Code's Notification hook. Reads JSON from stdin, passes to Emacs.

set -euo pipefail

# Read the full JSON payload from stdin
JSON_DATA=$(cat)

# Determine emacsclient socket path
# The Elisp install-hook function writes this path into the hook command
SOCKET="${CLAUDE_NOTIFY_EMACS_SOCKET:-}"
if [ -z "$SOCKET" ]; then
  # Fallback: try default socket
  SOCKET="server"
fi

# Call into Emacs, passing JSON as additional argument (accessed via server-eval-args-left)
emacsclient -s "$SOCKET" --eval \
  '(claude-notify--handle-hook (pop server-eval-args-left))' \
  "$JSON_DATA" \
  2>/dev/null || true
# Exit 0 always: hook should not block Claude Code
exit 0
```

**Confidence:** HIGH -- round-trip tested on target system with Emacs 30.2.

### Pattern 2: Elisp Hook Handler

**What:** The function invoked by emacsclient that receives the JSON payload, parses it, extracts notification type and project info, and dispatches the notification.

**When to use:** Called on every hook event from Claude Code.

**Example:**

```elisp
(defun claude-notify--handle-hook (json-string)
  "Handle a Claude Code CLI hook event.
JSON-STRING is the raw JSON payload from the hook's stdin."
  (when (and claude-notify-mode json-string)
    (condition-case err
        (let* ((data (json-parse-string json-string :object-type 'alist))
               (notification-type (alist-get 'notification_type data))
               (cwd (alist-get 'cwd data))
               (session-id (alist-get 'session_id data))
               (message (alist-get 'message data))
               (project-name (when cwd (file-name-nondirectory
                                         (directory-file-name cwd)))))
          (claude-notify--dispatch notification-type project-name
                                    session-id message))
      (error (message "claude-notify: error handling hook: %S" err)))))
```

**Confidence:** HIGH -- standard Emacs JSON parsing, verified `json-parse-string` availability.

### Pattern 3: Async Notification Dispatch

**What:** Send the macOS notification asynchronously via `start-process` to avoid blocking Emacs.

**When to use:** Every time a notification needs to be sent (default backend).

**Example:**

```elisp
(defun claude-notify--send-notification (title subtitle body)
  "Send a macOS notification asynchronously.
TITLE is the notification title.
SUBTITLE is shown below the title.
BODY is the notification message text."
  (let ((script (format "display notification %s with title %s subtitle %s"
                        (claude-notify--applescript-quote body)
                        (claude-notify--applescript-quote title)
                        (claude-notify--applescript-quote subtitle))))
    (start-process "claude-notify" nil "osascript" "-e" script)))

(defun claude-notify--applescript-quote (str)
  "Quote STR for use in AppleScript."
  (concat "\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" (or str "")) "\""))
```

**Confidence:** HIGH -- async `start-process "osascript"` measured at ~10ms.

### Pattern 4: Settings.json Hook Installation

**What:** The `M-x claude-notify-install-hook` command reads `~/.claude/settings.json`, merges a `Notification` hook entry, and writes back.

**When to use:** One-time setup by the user.

**Example:**

```elisp
(defun claude-notify-install-hook ()
  "Install the Claude Code Notification hook in ~/.claude/settings.json.
Adds a Notification hook that calls the claude-notify bridge script
when Claude Code fires permission_prompt events."
  (interactive)
  (let* ((settings-file (expand-file-name "~/.claude/settings.json"))
         (script-path (expand-file-name
                       "bin/claude-notify-hook.sh"
                       (file-name-directory
                        (locate-library "claude-notify"))))
         (socket-path (expand-file-name server-name server-socket-dir))
         ;; The hook command: set socket env var, then run the script
         (hook-command (format "CLAUDE_NOTIFY_EMACS_SOCKET=%s %s"
                               (shell-quote-argument socket-path)
                               (shell-quote-argument script-path)))
         ;; Read existing settings
         (settings (if (file-exists-p settings-file)
                       (json-parse-string
                        (with-temp-buffer
                          (insert-file-contents settings-file)
                          (buffer-string))
                        :object-type 'alist
                        :null-object nil)
                     nil))
         ;; Build the new Notification hook entry
         (new-hook `((matcher . "permission_prompt")
                     (hooks . [((type . "command")
                                (command . ,hook-command)
                                (timeout . 5))])))
         ;; Get or create hooks.Notification array
         (hooks-alist (or (alist-get 'hooks settings) nil))
         (notification-hooks (or (alist-get 'Notification hooks-alist) [])))
    ;; Check if already installed
    ;; ... merge logic, write back ...
    ))
```

**Confidence:** HIGH -- JSON read/write pattern is standard. The key decision is to use `json-parse-string` / `json-serialize` (Emacs 27+, built-in) for safe round-tripping.

### Pattern 5: Global Minor Mode

**What:** `claude-notify-mode` as a global minor mode that enables/disables the notification pipeline.

**When to use:** Users toggle this to control whether notifications fire.

**Example:**

```elisp
(define-minor-mode claude-notify-mode
  "Global minor mode for Claude Code session notifications.
When enabled, macOS notifications fire when Claude Code needs input."
  :global t
  :group 'claude-notify
  :lighter " CN"
  (if claude-notify-mode
      (message "claude-notify: notifications enabled")
    (message "claude-notify: notifications disabled")))
```

**Confidence:** HIGH -- standard Emacs minor mode pattern.

### Anti-Patterns to Avoid

- **Using `ns-do-applescript` as default:** Blocks Emacs for ~700ms. Use `start-process` instead. Offer `ns-do-applescript` as opt-in only.

- **Inline shell command in settings.json:** Becomes unreadable and hard to debug. Use a separate script file.

- **Parsing JSON in the shell script:** Keep the shell script minimal (read stdin, call emacsclient). Do all JSON parsing in Elisp where error handling is better.

- **Using `call-process` for notifications:** Synchronous, blocks Emacs. Always use `start-process` (async).

- **Auto-installing the hook on package load:** Modifying `~/.claude/settings.json` is a side effect. Require explicit user action via `M-x claude-notify-install-hook`.

- **Hardcoding the emacsclient socket path:** The socket path varies by system. Capture it dynamically at install time from `server-socket-dir` + `server-name`.

- **Forgetting to handle the case where emacsclient is not connected:** The hook script should `exit 0` even if emacsclient fails. A hook error would show in Claude Code's verbose output but should never block Claude.

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| JSON parsing in Elisp | Custom string parsing | `json-parse-string` (built-in since Emacs 27) | Handles escaping, nested objects, arrays correctly |
| JSON parsing in shell | `sed`/`awk` extraction | `jq` or pass raw to Elisp | JSON has edge cases (escaped quotes, Unicode) that break naive parsing |
| AppleScript string escaping | Manual `replace-regexp-in-string` chain | Dedicated quote function (see Pattern 3) | AppleScript has specific escaping rules for backslash and double-quote |
| Settings.json manipulation | String concatenation / regex | `json-parse-string` + `json-serialize` round-trip | Preserves all existing settings, handles formatting |
| Shell argument escaping | Manual quoting | `shell-quote-argument` (Emacs built-in) | Handles all special characters correctly |

**Key insight:** Every "simple" string operation in this pipeline (JSON, AppleScript, shell arguments) has edge cases that break naive approaches. Use the platform's built-in tools for each: `json-parse-string` in Elisp, `jq` in shell, `shell-quote-argument` for paths.

## Common Pitfalls

### Pitfall 1: `ns-do-applescript` Blocks Emacs for ~700ms

**What goes wrong:** Using `ns-do-applescript` as the default notification backend causes a noticeable UI freeze on every notification.
**Why it happens:** `ns-do-applescript` is synchronous -- it executes AppleScript in-process and waits for completion. `display notification` involves IPC with the Notification Center daemon, which takes ~700ms.
**How to avoid:** Use `start-process "osascript"` as the default (returns in ~10ms). Offer `ns-do-applescript` as a `defcustom` opt-in for users who want the notification attributed to Emacs.
**Warning signs:** Users report brief UI freezes when notifications fire.
**Confidence:** HIGH -- measured live on target system: `ns-do-applescript` = 700ms, `start-process` = 10ms.

### Pitfall 2: Shell Escaping Breakage with JSON in emacsclient

**What goes wrong:** Attempting to embed the JSON payload directly in an `emacsclient --eval '(fn "JSON_HERE")'` command breaks when JSON contains quotes, backslashes, or special characters.
**Why it happens:** The JSON must survive: shell parsing -> emacsclient argument parsing -> Elisp string reader. Each layer has different escaping rules.
**How to avoid:** Use `server-eval-args-left` (Emacs 30+). Pass JSON as a SEPARATE argument to emacsclient, not embedded in the --eval string. The Elisp function uses `(pop server-eval-args-left)` to retrieve it without any parsing/escaping.
**Warning signs:** Notifications fail silently or show garbled text for certain projects (ones with special characters in paths).
**Confidence:** HIGH -- verified that `server-eval-args-left` works with raw JSON on Emacs 30.2.

### Pitfall 3: Existing settings.json Hooks Clobbered During Installation

**What goes wrong:** The `install-hook` command overwrites or corrupts existing hooks in `~/.claude/settings.json`.
**Why it happens:** Naive approach: read file, construct new JSON, write file. If the merge logic is wrong, existing `PreToolUse`, `SessionStart`, or other hooks are lost.
**How to avoid:**
1. Read and parse the ENTIRE file with `json-parse-string`
2. Navigate to `hooks.Notification` specifically
3. Add or update only the claude-notify entry (check for existing by matching command path)
4. Serialize back with `json-serialize` preserving all other keys
5. Create a backup before writing: copy to `settings.json.bak`
**Warning signs:** User's other hooks stop working after running `install-hook`.
**Confidence:** HIGH -- the target `settings.json` has existing hooks that MUST be preserved.

### Pitfall 4: Hook Script Fails Silently When Emacs Server Not Running

**What goes wrong:** The hook script calls `emacsclient --eval` but the Emacs server is not running (or socket path changed). The notification is silently lost.
**Why it happens:** The socket path is captured at install time. If Emacs restarts with a different `server-name` or `server-socket-dir`, the path is stale.
**How to avoid:**
1. Hook script should `exit 0` regardless of emacsclient success (never block Claude)
2. Consider using `emacsclient -s "$SOCKET" --eval '...' 2>/dev/null || true`
3. Document that re-running `M-x claude-notify-install-hook` updates the socket path
4. Consider auto-updating socket path on Emacs startup via a mode hook
**Warning signs:** Notifications stop working after Emacs restart but Claude Code keeps running.
**Confidence:** MEDIUM -- the socket path is stable during a session but can change across Emacs restarts.

### Pitfall 5: `jq` Not Available on Target System

**What goes wrong:** The hook shell script uses `jq` for JSON parsing, but `jq` is not installed.
**Why it happens:** While `jq` is common on dev machines, it is not a macOS system default.
**How to avoid:**
1. Design the hook script to pass the RAW JSON to emacsclient without parsing it in shell. All parsing happens in Elisp.
2. The shell script becomes: `cat | emacsclient --eval '...' "$(cat)"` -- but this reads stdin twice.
3. Better: `JSON=$(cat); emacsclient -s "$SOCKET" --eval '(claude-notify--handle-hook (pop server-eval-args-left))' "$JSON"`
4. No `jq` needed at all if we do all parsing in Elisp.
**Warning signs:** Hook silently fails on fresh macOS systems without Homebrew.
**Confidence:** HIGH -- design decision: do NOT use `jq` in the hook script. Pass raw JSON to Elisp.

### Pitfall 6: macOS Notification Permissions Not Granted

**What goes wrong:** Notifications are silently swallowed because the user hasn't granted notification permission to the sending application.
**Why it happens:** On macOS Sequoia/Tahoe, `osascript`-based notifications require the terminal application to have notification permission. Since the hook runs from Claude Code's terminal context, that terminal needs permission.
**How to avoid:**
1. Use `start-process "osascript"` from Emacs (Emacs is the parent process, so the notification may be attributed to Script Editor or Emacs depending on context)
2. Provide `M-x claude-notify-test` (Phase 3) to verify the pipeline works
3. Document in the package README that the user may need to grant notification permissions to Emacs or Script Editor in System Settings > Notifications
**Warning signs:** Everything looks correct but no notifications appear.
**Confidence:** MEDIUM -- Sequoia permission behavior varies by context. Live testing showed notifications working from `emacsclient --eval -> start-process osascript`.

## Code Examples

### Complete Hook Shell Script (no jq dependency)

```bash
#!/usr/bin/env bash
# claude-notify-hook.sh
# Bridge: Claude Code CLI hook -> Emacs via emacsclient
# Reads JSON from stdin, passes to Emacs via server-eval-args-left (Emacs 30+)
set -euo pipefail

JSON=$(cat)

# Socket path is embedded at install time by M-x claude-notify-install-hook
SOCKET="__SOCKET_PATH__"

emacsclient -s "$SOCKET" \
  --eval '(claude-notify--handle-hook (pop server-eval-args-left))' \
  "$JSON" \
  2>/dev/null || true

exit 0
```

**Source:** Designed for this project. Verified pattern with live Emacs 30.2 testing.

### Complete settings.json Notification Hook Entry

```json
{
  "hooks": {
    "Notification": [
      {
        "matcher": "permission_prompt",
        "hooks": [
          {
            "type": "command",
            "command": "/Users/darioklingenberg/code/emacs-packages/claude-notify/bin/claude-notify-hook.sh",
            "timeout": 5
          }
        ]
      }
    ]
  }
}
```

**Source:** Based on [Claude Code Hooks reference](https://code.claude.com/docs/en/hooks) -- Notification event with matcher, verified schema.

### Notification Hook JSON Payload (stdin)

The JSON that Claude Code sends to the hook script on stdin:

```json
{
  "session_id": "abc123",
  "transcript_path": "/Users/dario/.claude/projects/.../transcript.jsonl",
  "cwd": "/Users/dario/my-project",
  "permission_mode": "default",
  "hook_event_name": "Notification",
  "message": "Claude needs your permission to use Bash",
  "title": "Permission needed",
  "notification_type": "permission_prompt"
}
```

**Source:** [Claude Code Hooks reference](https://code.claude.com/docs/en/hooks) -- Notification input section.

### Notification Backend Selection

```elisp
(defcustom claude-notify-backend 'osascript-async
  "Notification delivery backend.
`osascript-async' (default): async subprocess, fast (~10ms), attributed to Script Editor.
`ns-applescript': synchronous in-process, slower (~700ms), attributed to Emacs."
  :type '(choice (const :tag "osascript (async, recommended)" osascript-async)
                 (const :tag "ns-do-applescript (sync, in-process)" ns-applescript))
  :group 'claude-notify)

(defun claude-notify--send (title subtitle body)
  "Send a macOS notification using the configured backend."
  (pcase claude-notify-backend
    ('osascript-async
     (claude-notify--send-osascript-async title subtitle body))
    ('ns-applescript
     (if (fboundp 'ns-do-applescript)
         (claude-notify--send-ns-applescript title subtitle body)
       (claude-notify--send-osascript-async title subtitle body)))))
```

**Source:** Architecture decision based on live benchmarking.

### AppleScript Notification Template

```elisp
(defun claude-notify--send-osascript-async (title subtitle body)
  "Send notification asynchronously via osascript."
  (let ((script (format "display notification %s with title %s subtitle %s"
                        (claude-notify--applescript-quote (or body ""))
                        (claude-notify--applescript-quote (or title "Claude Code"))
                        (claude-notify--applescript-quote (or subtitle "")))))
    (start-process "claude-notify" nil "osascript" "-e" script)))
```

### JSON Merge for settings.json

```elisp
(defun claude-notify--read-settings ()
  "Read and parse ~/.claude/settings.json."
  (let ((file (expand-file-name "~/.claude/settings.json")))
    (when (file-exists-p file)
      (json-parse-string
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))
       :object-type 'hash-table
       :null-object :null))))

(defun claude-notify--write-settings (settings)
  "Write SETTINGS hash table to ~/.claude/settings.json."
  (let ((file (expand-file-name "~/.claude/settings.json"))
        (json-str (json-serialize settings :null-object :null)))
    ;; Backup
    (when (file-exists-p file)
      (copy-file file (concat file ".bak") t))
    (with-temp-file file
      (insert json-str)
      (json-pretty-print-buffer))))
```

**Note:** Use `hash-table` for `json-parse-string` object type to allow mutation with `puthash`. Use `alist` in the hook handler where read-only access via `alist-get` is simpler.

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|-------------|------------------|--------------|--------|
| `preferredNotifChannel terminal_bell` | Claude Code CLI Hooks with `Notification` event | 2025 | Bell character was the only signal; now we get structured JSON with event type, session ID, project path |
| Manual settings.json editing | `/hooks` interactive menu + hook files | Late 2025 | Users can configure hooks interactively, but programmatic installation still requires JSON manipulation |
| `ns-do-applescript` for all AppleScript | `start-process "osascript"` for async | Always available | `ns-do-applescript` was the "obvious" choice but benchmarks show it blocks ~700ms; async is clearly better |
| Single notification type | 4 notification matchers: `permission_prompt`, `idle_prompt`, `auth_success`, `elicitation_dialog` | 2025-2026 | Can now filter and handle different event types with distinct notifications |
| `server-eval-args-left` not available | Available in Emacs 30+ | 2024 | Safe argument passing without shell escaping nightmares |

**Deprecated/outdated:**
- Using `do-applescript` (old name for `ns-do-applescript`): Same function, renamed in modern Emacs
- Relying on `idle_prompt` as a reliable "needs input" signal: Known false positives ([#12048](https://github.com/anthropics/claude-code/issues/12048)). Use `permission_prompt` for Phase 1; add `idle_prompt` with debounce in Phase 3

## Open Questions

1. **`json-pretty-print-buffer` output format**
   - What we know: `json-serialize` produces minified JSON. `json-pretty-print-buffer` reformats it.
   - What's unclear: Does the round-trip through `json-parse-string` + `json-serialize` + `json-pretty-print-buffer` preserve key ordering and formatting of the original `settings.json`? If not, the user sees a large diff on their settings file.
   - Recommendation: Test the round-trip on the actual settings.json. If formatting changes are unacceptable, consider using a regex-based insertion approach instead of full parse/serialize.

2. **Emacs server socket stability across restarts**
   - What we know: Socket path is `/var/folders/.../emacs501/server` on this system. Captured at `install-hook` time and embedded in the shell script.
   - What's unclear: If Emacs restarts, does the socket path remain the same? What if the user changes `server-name`?
   - Recommendation: Embed the socket path in the script at install time. Document that `M-x claude-notify-install-hook` should be re-run after changing server settings. Consider a SessionStart hook or Emacs startup hook that updates the path automatically.

3. **Hook script file permissions after installation**
   - What we know: The script needs `+x` permission to run.
   - What's unclear: Does `straight.el` / Doom preserve file permissions when cloning from GitHub?
   - Recommendation: Set executable permission in the `install-hook` function: `(set-file-modes script-path #o755)`.

4. **Multiple Emacs instances**
   - What we know: The hook script targets a specific Emacs server socket.
   - What's unclear: If the user runs multiple Emacs instances (Emacs.app + terminal emacs), which one receives the notification?
   - Recommendation: The one whose socket was specified at `install-hook` time. This is correct behavior -- the user explicitly chooses which Emacs to notify. Document this.

## Sources

### Primary (HIGH confidence)
- [Claude Code Hooks Reference](https://code.claude.com/docs/en/hooks) -- Complete event schemas, Notification event input format, matcher patterns, JSON output format. Verified Feb 2026.
- [Claude Code Hooks Guide](https://code.claude.com/docs/en/hooks-guide) -- Setup walkthrough, notification hook example, settings.json configuration.
- [GNU Emacs Manual: emacsclient Options](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html) -- `server-eval-args-left` documentation, argument passing mechanism.
- Live testing on target system (Emacs 30.2, macOS Tahoe 26.3) -- `ns-do-applescript` latency (700ms), `start-process osascript` latency (10ms), `server-eval-args-left` JSON round-trip, socket path verification.
- [stevemolitor/claude-code.el](https://github.com/stevemolitor/claude-code.el) -- Reference implementation of `claude-code-handle-hook` using `server-eval-args-left`, `claude-code-event-hook` dispatch pattern.

### Secondary (MEDIUM confidence)
- [Boris Buliga: Claude Code Notifications That Don't Suck](https://www.d12frosted.io/posts/2026-01-05-claude-code-notifications) -- Production notification hook, yabai space detection, terminal-notifier `-sender` pattern, settings.json example.
- [Emacs bug#59743](https://lists.gnu.org/archive/html/bug-gnu-emacs/2022-12/msg00045.html) -- `server-eval-args-left` proposal and implementation discussion.
- [alexop.dev: Claude Code Notification Hooks](https://alexop.dev/posts/claude-code-notification-hooks/) -- Hook configuration examples, matcher documentation.
- User's existing `~/.claude/settings.json` -- Verified structure with PreToolUse, SessionStart hooks, plugins, env vars that must be preserved.

### Tertiary (LOW confidence)
- [Emacs bug#7276: do-applescript can lock Emacs](https://lists.gnu.org/archive/html/bug-gnu-emacs/2010-10/msg00501.html) -- Historic report of `ns-do-applescript` blocking. Confirmed by live measurement.
- [emacs-devel: ns-do-applescript removal discussion](https://lists.gnu.org/archive/html/emacs-devel/2022-05/msg01373.html) -- Potential future removal of `ns-do-applescript`.

## Metadata

**Confidence breakdown:**
- Hook system integration: HIGH -- verified via official docs + live testing
- Notification delivery: HIGH -- both backends benchmarked on target system
- Settings.json manipulation: HIGH -- read existing file, verified structure
- `server-eval-args-left` mechanism: HIGH -- tested live with JSON payload
- Shell script bridge: HIGH -- pattern verified end-to-end

**Research date:** 2026-02-24
**Valid until:** 2026-04-24 (stable domain; Claude Code hooks API and Emacs server mechanism are mature)
