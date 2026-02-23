# Phase 02: Agent Lifecycle - Research

**Researched:** 2026-02-23
**Domain:** Emacs Lisp — vterm buffer lifecycle, process sentinels, header-line, overlays, timers, display-buffer
**Confidence:** HIGH (core Emacs APIs) / MEDIUM (vterm-specific visual effects)

---

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

#### Auto-close behavior
- Finished agent panes stay open, marked as finished (NOT auto-closed)
- Visual treatment: header line changes AND buffer text gets a dimmed face overlay so finished panes visually recede
- Bulk action: add a "close all finished" command to SPC o C prefix and dashboard
- Team completion: minibuffer notification ("All N agents finished") when the last agent in a team exits

#### Navigation & layout
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

### Deferred Ideas (OUT OF SCOPE)
None — discussion stayed within phase scope
</user_constraints>

---

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| LIFE-01 | Each subagent spawned by Claude Code creates a new vterm buffer in Emacs | `create-pane` already works; needs color extraction fix in shim |
| LIFE-02 | User can navigate between agent panes (next/prev/select commands) | `next`/`prev`/`select` exist; need behavior update: switch-to-buffer in current window |
| LIFE-03 | Finished agent panes stay open with dimmed visual treatment (overrides SC3 auto-close) | Process sentinel already marks `:finished t`; need header-line update + visual dimming |
| LIFE-04 | Header-line shows agent name and status (running/finished) with color indicator | Partial: `set-pane-info` sets header-line; needs status indicator and color from shim |
| OBSV-01 | emacsclient errors captured and logged (not silently discarded to /dev/null) | Shim captures to temp file; needs persistent log path and surfacing mechanism |
</phase_requirements>

---

## Summary

Phase 2 builds on the working shim/vterm infrastructure from Phase 1. The existing codebase has stubs for nearly every feature in this phase — the work is completing and wiring up what's already sketched out. This makes the research focus on **API correctness** rather than architecture discovery.

The most important finding is about color extraction: Claude Code sends the agent color via `select-pane -t PANEID -P bg=default,fg=COLOR` **before** it sends the title via `select-pane -t PANEID -T TITLE`. The shim currently drops the `-P` argument entirely. Capturing `fg=COLOR` from `-P` and threading it through to `set-pane-info` is the key missing link for LIFE-04.

The second important finding is about vterm visual dimming: vterm renders text through libvterm (a C library) that applies its own color faces (`vterm-color-*`). Standard Emacs overlays and `face-remap-add-relative` on the `default` face **do not reliably affect terminal-rendered content**. The reliable signal for "finished" state is the **header-line**, which is fully Emacs-managed and will respond correctly to any face changes. The header-line is also the most visually prominent element for at-a-distance identification. A secondary approach — `face-remap-add-relative` on `default` — may partially dim the background and empty areas; this should be attempted but must not be relied on as the primary signal.

**Primary recommendation:** Capture color from shim's `-P` argument → pass to `set-pane-info` → apply to header-line. On finish: change header-line to show "[FINISHED]" in `shadow` face, attempt `face-remap-add-relative` as best-effort background dim.

---

## Standard Stack

### Core — already in use, no changes
| Library | Version | Purpose | Notes |
|---------|---------|---------|-------|
| `vterm` | any | Agent pane terminal emulator | Already required |
| `cl-lib` | built-in | `cl-incf`, `cl-find-if`, `cl-position` | Already required |
| `subr-x` | built-in | `string-join`, `when-let` | Already required |
| `tabulated-list-mode` | built-in | Dashboard | Derived mode already exists |
| `server` | built-in | Emacs server start/check | Already used |

### Supporting APIs — need to be used correctly

| API | Purpose | Key Functions |
|-----|---------|---------------|
| Process Sentinels | Detect vterm process exit | `set-process-sentinel`, `process-status` |
| Face Remapping | Buffer-local visual dimming (best-effort) | `face-remap-add-relative`, `face-remap-remove-relative` |
| Header-line | Per-buffer status bar | `header-line-format`, `propertize` with `face` property |
| Timers | Dashboard auto-refresh | `run-with-timer`, `cancel-timer` |
| Display Buffer | No-focus pane creation | `display-buffer` with `inhibit-switch-frame` |

### No New Packages Needed

All required functionality is in Emacs built-ins plus already-declared `vterm`. Do not introduce any new package dependencies.

---

## Architecture Patterns

### Pattern 1: Color Extraction from Claude Code's tmux Protocol

**What:** Claude Code sends agent color via `select-pane -P bg=default,fg=COLOR` before setting the title via `select-pane -T TITLE`.

**Verified from shim log** (HIGH confidence — observed in actual log):
```
select-pane -t %emacs-2 -P bg=default,fg=blue       # color call
set-option -p -t %emacs-2 pane-border-style fg=blue  # redundant, can ignore
set-option -p -t %emacs-2 pane-active-border-style fg=blue  # redundant
select-pane -t %emacs-2 -T explorer-1                # title call
set-option -p -t %emacs-2 pane-border-format #[fg=blue,bold] #{pane_title} #[default]  # redundant
```

**What to fix in shim:** Extract `fg=COLOR` from the `-P` argument:
```bash
# In select-pane handler, capture -P value:
-P)
    shift
    style_str="$1"
    # Extract fg= value from "bg=default,fg=blue" or "fg=blue" etc.
    if [[ "$style_str" =~ fg=([a-zA-Z#0-9]+) ]]; then
        color="${BASH_REMATCH[1]}"
    fi
    ;;
```

Then call `set-pane-info` with both title and color when either arrives. Since color arrives before title, cache the color by pane-id in the STATE_DIR and use it when the title arrives.

**Alternative:** Extract color from the `set-option -p -t PANEID pane-border-style fg=COLOR` call instead — same `fg=COLOR` value, same pane-id target, simpler parsing since it's already the `fg=` value directly.

**Recommendation:** Use `set-option` handler since `pane-border-style fg=COLOR` gives the color directly without needing regex on a compound style string. The shim currently no-ops `set-option` — change it to cache the color and call `set-pane-info` when both title and color are known.

### Pattern 2: Process Sentinel → Finished State → Visual Update

**What:** The sentinel is already set in `create-pane` and already sets `:finished t`. It needs to additionally:
1. Update the header-line to show finished state with dimmed styling
2. Apply `face-remap-add-relative` as best-effort visual dim
3. Check if all panes in the team are finished and emit the minibuffer notification

**Current sentinel (existing code):**
```elisp
(set-process-sentinel
 proc
 (lambda (process _event)
   (when (memq (process-status process) '(exit signal))
     (let ((entry (gethash pane-id claude-code-emacs-panes--registry)))
       (when entry (plist-put entry :finished t))))))
```

**Extended sentinel (what to add):**
```elisp
(set-process-sentinel
 proc
 (lambda (process _event)
   (when (memq (process-status process) '(exit signal))
     (let ((entry (gethash pane-id claude-code-emacs-panes--registry)))
       (when entry
         (plist-put entry :finished t)
         ;; Update header-line to show finished state
         (claude-code-emacs-panes--mark-finished-visually
          pane-id entry)
         ;; Check for team completion notification
         (claude-code-emacs-panes--check-team-completion))))))
```

Note: `process-status` returns `'exit` for normal exit and `'signal` for killed processes. The condition `(memq (process-status process) '(exit signal))` is correct for detecting any terminal state. Note that vterm sets `vterm-kill-buffer-on-exit nil` to prevent auto-kill — the buffer persists.

IMPORTANT: The sentinel fires from vterm's process, not from Claude Code calling `kill-pane`. Based on the shim log, Claude Code does NOT call `tmux kill-pane` when agents finish — the Claude process just exits and the shell continues. The shell exiting is what triggers the sentinel. This means `:finished t` is correct detection, but `kill-pane` is a separate explicit action (user-initiated or "close all finished").

### Pattern 3: Header-Line Format for Status

**What:** The header-line is set via `setq header-line-format` with a propertized string. This is the most reliable visual indicator for vterm buffers.

**Running state** (already partially implemented in `set-pane-info`):
```elisp
(setq header-line-format
      (format " %s %s"
              (propertize "\u25cf" 'face `(:foreground ,color))  ; colored dot
              (or title pane-id)))
```

**Recommended running format:**
```elisp
(setq header-line-format
      (list
       " "
       (propertize "\u25cf" 'face `(:foreground ,(or color "white")))
       " "
       (propertize (or title pane-id) 'face 'bold)
       " [running]"))
```

**Finished state** (new):
```elisp
(setq header-line-format
      (list
       " "
       (propertize "\u25cf" 'face 'shadow)    ; dimmed dot, no color
       " "
       (propertize (or title pane-id) 'face 'shadow)
       " "
       (propertize "[finished]" 'face '(:inherit shadow :weight bold))))
```

The `shadow` face is a standard Emacs face (defined in all themes) that provides a muted, low-contrast appearance. Using it ensures the finished header-line visually recedes on ANY theme without needing custom face definitions.

**NOTE on `header-line-format` with `(:eval ...)`:** Do NOT use `(:eval ...)` in the header-line for vterm buffers — vterm frequently updates the buffer and `(:eval ...)` will be called on every vterm refresh, which has performance implications. Set the header-line string directly (as shown above) and update it explicitly when state changes.

### Pattern 4: Best-Effort Visual Dimming (face-remap)

**What:** `face-remap-add-relative` remaps a face for the current buffer only. This affects Emacs-rendered elements: background color, fringes, empty lines. It does NOT reliably affect terminal-rendered text in vterm (libvterm applies its own `vterm-color-*` faces).

**Pattern from official Emacs docs (HIGH confidence):**
```elisp
;; In create-pane, add a buffer-local variable:
(defvar-local claude-code-emacs-panes--dim-cookie nil
  "Cookie from face-remap-add-relative for finished-state dimming.")

;; In mark-finished-visually:
(with-current-buffer buf
  (unless claude-code-emacs-panes--dim-cookie
    (setq claude-code-emacs-panes--dim-cookie
          (face-remap-add-relative 'default
                                   :foreground (face-foreground 'shadow)
                                   :background (face-background 'shadow)))))

;; To restore (if pane is killed/reused):
(when claude-code-emacs-panes--dim-cookie
  (face-remap-remove-relative claude-code-emacs-panes--dim-cookie)
  (setq claude-code-emacs-panes--dim-cookie nil))
```

**Honest assessment:** This WILL dim the buffer background and Emacs-managed display elements. It will NOT dim terminal text content rendered by libvterm. The combination of:
1. Dimmed header-line (reliable, clear signal)
2. Dimmed buffer background via face-remap (partial, best-effort)

...is the correct implementation strategy. Do not attempt to use overlays on vterm content — they won't work for terminal-rendered characters.

### Pattern 5: No-Focus Pane Creation

**What:** The existing `create-pane` already uses `display-buffer` but may still cause focus issues. The correct pattern uses `inhibit-switch-frame`:

**Current (existing):**
```elisp
(display-buffer buf '((display-buffer-reuse-window
                       display-buffer-pop-up-window)
                      (inhibit-same-window . t)))
```

**Correct for no-focus (confirmed from official docs):**
```elisp
(let ((target-frame (or (car (filtered-frame-list #'frame-visible-p))
                        (selected-frame))))
  (with-selected-frame target-frame
    (display-buffer buf '((display-buffer-reuse-window
                           display-buffer-pop-up-window)
                          (inhibit-same-window . t)
                          (inhibit-switch-frame . t)))))
```

The `inhibit-switch-frame . t` in the alist prevents the new window from stealing frame focus. The existing code already uses `with-selected-frame target-frame` which handles the daemon-frame issue — adding `inhibit-switch-frame` ensures cursor stays in the user's active window.

However: `display-buffer` creates the window but does NOT change `selected-window`. The actual "focus steal" that users notice is the cursor jumping. This doesn't happen with `display-buffer` unless something calls `select-window` afterward. The current code should already be non-stealing. Verify in testing; add `inhibit-switch-frame` as a safeguard.

### Pattern 6: Dashboard Auto-Refresh Timer

**What:** A repeating timer calls a refresh function while the dashboard buffer is visible.

**Standard Emacs pattern (HIGH confidence from Timers docs):**
```elisp
(defvar claude-code-emacs-panes--dashboard-timer nil
  "Repeating timer for dashboard auto-refresh.")

(defun claude-code-emacs-panes--start-dashboard-timer ()
  "Start the dashboard refresh timer."
  (unless claude-code-emacs-panes--dashboard-timer
    (setq claude-code-emacs-panes--dashboard-timer
          (run-with-timer 2 2 #'claude-code-emacs-panes--refresh-dashboard-if-visible))))

(defun claude-code-emacs-panes--stop-dashboard-timer ()
  "Stop the dashboard refresh timer."
  (when claude-code-emacs-panes--dashboard-timer
    (cancel-timer claude-code-emacs-panes--dashboard-timer)
    (setq claude-code-emacs-panes--dashboard-timer nil)))

(defun claude-code-emacs-panes--refresh-dashboard-if-visible ()
  "Refresh the dashboard buffer if it is currently visible."
  (let ((buf (get-buffer "*claude-panes-dashboard*")))
    (when (and buf (get-buffer-window buf 'visible))
      ;; Refresh by re-running the tabulated list population
      (with-current-buffer buf
        (claude-code-emacs-panes--populate-dashboard)
        (tabulated-list-print t)))))  ; t = preserve position
```

Start the timer when the dashboard is opened; stop it when the dashboard buffer is killed (use `kill-buffer-hook`). 2-second interval is appropriate for live status view.

**CRITICAL:** `tabulated-list-print` with `t` (preserve-pos argument) keeps the cursor on the same entry. Without this, the cursor jumps to top on every refresh — bad UX.

### Pattern 7: Team Completion Notification

**What:** When the last running pane in a team finishes, show a minibuffer message.

**Challenge:** The registry does not currently track which panes belong to which team. The team name is available in the Claude Code command that spawns agents (visible in the log: `--team-name cryptic-sauteeing-finch`), but it arrives via `send-keys`, not via a dedicated tmux command.

**Two options:**
1. **Extract team name from `send-keys` command text in the shim** and pass it via a new `set-pane-team` emacsclient call. This is clean but requires shim changes.
2. **Infer completion by checking all panes**: When any pane finishes, check if ALL registered live panes are now finished. If so, emit "All N agents finished" notification. This doesn't distinguish teams but works for the common case of running one team at a time.

**Recommendation:** Use option 2 (all-panes check) for simplicity. If the user runs overlapping teams simultaneously, the notification may fire prematurely, but this edge case is rare and the notification is non-blocking. Revisit if per-team tracking becomes needed.

**Implementation:**
```elisp
(defun claude-code-emacs-panes--check-team-completion ()
  "Check if all registered live panes are finished; notify if so."
  (let ((running 0)
        (finished 0))
    (maphash (lambda (_id entry)
               (when (buffer-live-p (plist-get entry :buffer))
                 (if (plist-get entry :finished)
                     (cl-incf finished)
                   (cl-incf running))))
             claude-code-emacs-panes--registry)
    (when (and (> finished 0) (= running 0))
      (message "All %d agent%s finished"
               finished
               (if (= finished 1) "" "s")))))
```

### Pattern 8: emacsclient Error Logging (OBSV-01)

**What:** The shim already captures emacsclient errors to `$STATE_DIR/emacsclient-err` (a temp file). The issue is that this is per-call and temporary — errors are logged to debug log only if `CLAUDE_CODE_EMACS_PANES_DEBUG=1`. Without debug mode, failures are silent.

**Current shim behavior:**
```bash
result=$(emacsclient --socket-name "$EMACS_PANES_SERVER" --eval "$1" 2>"$STATE_DIR/emacsclient-err")
exit_code=$?
if [[ $exit_code -ne 0 ]]; then
    err_msg=$(< "$STATE_DIR/emacsclient-err" 2>/dev/null)
    log_debug "emacsclient FAILED (exit $exit_code): $err_msg"
    ...
fi
```

**What needs to change:** Always append errors to the persistent log (not just when DEBUG=1):
```bash
# Always write errors to the persistent log
LOG_FILE="${TMPDIR:-/tmp}/claude-emacs-panes.log"

if [[ $exit_code -ne 0 ]]; then
    err_msg=$(< "$STATE_DIR/emacsclient-err" 2>/dev/null)
    # Always log errors (not just in debug mode)
    echo "[$(date '+%H:%M:%S')] [$$] ERROR: emacsclient failed (exit $exit_code): $err_msg" >> "$LOG_FILE"
    echo "  Command was: $1" >> "$LOG_FILE"
fi
```

The log file location `${TMPDIR}/claude-emacs-panes.log` is already established and used for debug logging. The error format should include timestamp, PID, exit code, error text, and the elisp expression that failed.

**Surfacing:** OBSV-01 says "captured in a log file" — no requirement to display in Emacs itself. The log at `$TMPDIR/claude-emacs-panes.log` is sufficient. No Emacs-side changes needed for OBSV-01; only shim changes.

### Pattern 9: Navigation Behavior Fix

**What:** Current `next`/`prev` use `switch-to-buffer` which switches in current window — this is CORRECT per the decision. Current `select` uses `pop-to-buffer` which may split — this should be `switch-to-buffer` for consistency.

**Fix for `claude-code-emacs-panes-select`:**
```elisp
;; Replace pop-to-buffer with switch-to-buffer:
(when buf (switch-to-buffer buf))  ; was: (pop-to-buffer buf)
```

### Pattern 10: "Close All Finished" Command

**What:** Kill all panes with `:finished t`. This must work as a standalone command AND be exposed from the dashboard.

```elisp
(defun claude-code-emacs-panes-close-finished ()
  "Kill all finished panes."
  (interactive)
  (let ((count 0))
    (maphash (lambda (id entry)
               (when (plist-get entry :finished)
                 (let ((buf (plist-get entry :buffer)))
                   (when (and buf (buffer-live-p buf))
                     (kill-buffer buf)))
                 (remhash id claude-code-emacs-panes--registry)
                 (cl-incf count)))
             claude-code-emacs-panes--registry)
    (message "Closed %d finished pane%s" count (if (= count 1) "" "s"))))
```

Bind at `SPC o C D` (capital D for "Delete finished") or `SPC o C k`. Add to doom config as `:desc "Close finished panes"`.

For the dashboard: add a keybinding in `claude-code-emacs-panes-dashboard-mode-map` at `D` or `K` that calls `claude-code-emacs-panes-close-finished` and then refreshes.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Buffer-local face for dimming | Custom overlay spanning (point-min, point-max) | `face-remap-add-relative` | Overlays don't work on libvterm-rendered text; face-remap is the correct API |
| Timer for dashboard | Custom `while` loop or `sleep` | `run-with-timer` + `cancel-timer` | Emacs timer system handles scheduling, cleanup, and error isolation |
| Check if buffer visible | Manual `dolist` over `window-list` | `get-buffer-window buf 'visible'` | The `'visible` argument checks all visible frames, not just current frame |
| Process exit detection | Polling loop or hooks on buffer changes | `set-process-sentinel` with `process-status` check | Sentinel fires immediately on exit; polling is slow and unreliable |

---

## Common Pitfalls

### Pitfall 1: Trying to Overlay vterm Content

**What goes wrong:** Developer places `(make-overlay (point-min) (point-max) buf)` with a face property expecting to dim terminal text. The overlay exists but vterm's libvterm renders characters with its own color system, ignoring the overlay's face for terminal-rendered characters.

**Why it happens:** vterm is backed by a C library (libvterm) that writes characters directly to the buffer using its own face application. The Emacs text property/overlay system interacts with this in unpredictable ways.

**How to avoid:** Use `face-remap-add-relative` for background dimming and rely primarily on header-line changes for status signaling.

**Warning signs:** Overlay is created but you see no visual change in terminal content, or only the background color (not foreground text) changes.

### Pitfall 2: sentinel fires on `vterm-kill-buffer-on-exit nil`

**What goes wrong:** Developer sets `vterm-kill-buffer-on-exit nil` (already done in the codebase) and then expects the process sentinel to fire when the vterm session's shell exits. It does fire correctly — but the buffer stays alive (the shell process is dead, the buffer persists).

**Clarification:** The sentinel fires when the vterm subprocess (the shell) exits. After that, the buffer is still live (because `vterm-kill-buffer-on-exit nil`). This is the intended behavior: we mark `:finished t` and update visuals, but the buffer stays open for reading history. `kill-pane` is a separate explicit action.

**Warning signs:** `(get-buffer-process buf)` returns `nil` or dead after sentinel fires — that's correct. Don't treat it as an error.

### Pitfall 3: `tabulated-list-print` without `t` arg in timer

**What goes wrong:** Dashboard auto-refresh calls `tabulated-list-print` without `t`, causing the cursor to jump to the top on every refresh tick (every 2 seconds). This makes the dashboard unusable when a user is reading it.

**How to avoid:** Always call `(tabulated-list-print t)` — the `t` argument preserves the cursor position.

### Pitfall 4: Timer not stopped when dashboard buffer killed

**What goes wrong:** Dashboard timer runs indefinitely. After the dashboard buffer is killed, the timer still fires every 2 seconds, calling `get-buffer "*claude-panes-dashboard*"` which returns nil, and then returning. It's harmless but wasteful.

**How to avoid:** Add a `kill-buffer-hook` in the dashboard buffer that calls `stop-dashboard-timer`:
```elisp
(add-hook 'kill-buffer-hook #'claude-code-emacs-panes--stop-dashboard-timer nil t)
```
The final `t` makes it buffer-local.

### Pitfall 5: Color extraction from shim — timing issue

**What goes wrong:** The shim receives `select-pane -P bg=default,fg=blue` (color) before `select-pane -T title` (name). If the shim calls `set-pane-info` with the color immediately but nil title, the header-line shows `%emacs-2` as the title instead of the agent name.

**How to avoid:** Cache color in STATE_DIR keyed by pane-id. When title arrives, call `set-pane-info` with both cached color and the new title. The color and title calls are always paired within milliseconds, so the cache just needs to survive a few bash invocations (STATE_DIR persists for the session).

### Pitfall 6: `(message ...)` from within process sentinel

**What goes wrong:** The sentinel fires in an asynchronous context. `message` should be safe to call from sentinels, but if multiple sentinels fire simultaneously (team of 3 agents all finishing at once), multiple messages can flash briefly.

**How to avoid:** This is acceptable UX — the last message wins. The team completion message "All N agents finished" will show correctly because `check-team-completion` counts all panes atomically. No workaround needed.

### Pitfall 7: The `set-option` handler currently drops color info

**What goes wrong:** The shim log shows Claude Code calling `set-option -p -t %emacs-2 pane-border-style fg=blue` immediately after `select-pane -P`. This `set-option` call contains the color for the pane. Currently the shim no-ops all `set-option` calls, so color info is lost.

**Best path forward:** Capture color from `set-option -p -t PANEID pane-border-style fg=COLOR` — this is the cleanest signal because:
- `pane-border-style` is always called with just `fg=COLOR` (no `bg=default` prefix)
- The pane-id (`-t`) is always explicit
- This call always precedes the title call

Parse it with: `if [[ "$*" =~ -p.*-t[[:space:]]+(%.+)[[:space:]]pane-border-style[[:space:]]+fg=([a-zA-Z#0-9]+) ]]`

---

## Code Examples

Verified/confirmed from log observation and official Emacs docs:

### Color Extraction in Shim (set-option handler)

```bash
# --- set-option ---
set-option|set)
    # Parse: set-option -p -t PANEID pane-border-style fg=COLOR
    target_pane=""
    option_name=""
    option_value=""
    local_flag=false
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -p) local_flag=true ;;
            -t) shift; target_pane="$1" ;;
            -*) ;;  # skip other flags
            *)
                if [[ -z "$option_name" ]]; then
                    option_name="$1"
                elif [[ -z "$option_value" ]]; then
                    option_value="$1"
                fi
                ;;
        esac
        shift
    done

    # Capture color from pane-border-style call
    if [[ "$local_flag" == true && "$option_name" == "pane-border-style" && -n "$target_pane" ]]; then
        if [[ "$option_value" =~ fg=([a-zA-Z#0-9]+) ]]; then
            color="${BASH_REMATCH[1]}"
            # Cache color for this pane
            echo "$color" > "$STATE_DIR/color-${target_pane//%/_}"
            log_debug "Cached color for $target_pane: $color"
        fi
    fi
    exit 0
    ;;
```

### Color + Title Flush in select-pane Handler

```bash
if [[ -n "$target_pane" && -n "$title" ]]; then
    escaped_pane=$(elisp_escape "$target_pane")
    escaped_title=$(elisp_escape "$title")
    # Load cached color if available
    color_file="$STATE_DIR/color-${target_pane//%/_}"
    color=""
    if [[ -f "$color_file" ]]; then
        color=$(< "$color_file")
    fi
    escaped_color=$(elisp_escape "$color")
    emacs_eval "(claude-code-emacs-panes-set-pane-info \"$escaped_pane\" \"$escaped_title\" \"$escaped_color\")" > /dev/null
fi
```

### Updated set-pane-info with Running Header-line

```elisp
(defun claude-code-emacs-panes-set-pane-info (pane-id title color)
  "Set TITLE and COLOR for the pane identified by PANE-ID.
COLOR is a color name string (e.g., \"blue\", \"#3399ff\") or nil.
Updates the header-line to show running status with colored indicator."
  (let* ((entry (gethash pane-id claude-code-emacs-panes--registry))
         (buf (and entry (plist-get entry :buffer))))
    (when entry
      (plist-put entry :title title)
      (plist-put entry :color (if (string-empty-p color) nil color)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (setq header-line-format
              (list " "
                    (propertize "\u25cf"
                                'face `(:foreground ,(or (and (not (string-empty-p color)) color)
                                                         "white")))
                    " "
                    (propertize (or title pane-id) 'face 'bold))))))
  "ok")
```

### Mark-Finished Visual Update

```elisp
(defun claude-code-emacs-panes--mark-finished-visually (pane-id entry)
  "Update visual state of PANE-ID's buffer to indicate finished status."
  (let ((buf (plist-get entry :buffer)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        ;; Update header-line to show finished state
        (let ((title (or (plist-get entry :title) pane-id)))
          (setq header-line-format
                (list " "
                      (propertize "\u25cb" 'face 'shadow)  ; open circle = finished
                      " "
                      (propertize title 'face 'shadow)
                      " "
                      (propertize "[finished]" 'face '(:inherit shadow :weight bold)))))
        ;; Best-effort background dim via face remapping
        (unless (bound-and-true-p claude-code-emacs-panes--dim-cookie)
          (setq-local claude-code-emacs-panes--dim-cookie
                      (face-remap-add-relative 'default
                                               :foreground (face-foreground 'shadow nil t)
                                               :background (face-background 'shadow nil t))))))))
```

### Dashboard Tabulated List with Color Coding

```elisp
;; In dashboard population:
(let* ((finished (plist-get entry :finished))
       (status-str (cond ((not buf-live) (propertize "dead" 'face 'error))
                         (finished (propertize "finished" 'face 'shadow))
                         (t (propertize "running" 'face 'success)))))
  (push (list id (vector id title status-str created)) entries))
```

---

## State of the Art

| Old Approach | Current Approach | Impact |
|--------------|------------------|--------|
| `select-pane -P` color dropped | Extract from `set-option -p pane-border-style fg=COLOR` | Enables agent color in header-line |
| Silent emacsclient failures | Always-on error logging to persistent log | OBSV-01 satisfied |
| Sentinel only sets `:finished t` | Sentinel also updates header-line and face-remap | LIFE-03 + LIFE-04 complete |
| `pop-to-buffer` in `select` | `switch-to-buffer` in current window | LIFE-02 consistent behavior |
| Dashboard: static, no refresh | Timer-driven refresh while visible | Live status view |

**Deprecated/outdated:**
- Auto-close on process exit (SC3): OVERRIDDEN by user decision — panes stay open

---

## Open Questions

1. **Does `face-remap-add-relative` on `default` face visually affect vterm buffers at all?**
   - What we know: dimmer.el uses this approach; works on regular buffers; no explicit vterm testing found
   - What's unclear: whether libvterm respects `face-remapping-alist` for its rendered characters
   - Recommendation: implement it, test manually with a real agent run. If it doesn't visually dim the terminal content, the header-line change alone is sufficient and correct per the design.

2. **What happens if user has multiple simultaneous teams?**
   - What we know: `check-team-completion` counts all panes globally, not per-team
   - What's unclear: is this a real use case the user cares about?
   - Recommendation: implement global check for now. The context says "team completion: minibuffer notification when the last agent in a team exits" — if multiple teams are running, the notification fires when the last pane of ALL teams finishes, which is acceptable for the current scope.

3. **Should the "close all finished" keybinding be `SPC o C D` or something else?**
   - What we know: existing bindings use `a/t/n/p/s/d/c` under `SPC o C`
   - Recommendation: use `SPC o C K` (K for "Kill finished") or `SPC o C x` — Claude's discretion, check for conflicts with existing doom keybindings.

---

## Sources

### Primary (HIGH confidence)
- Direct shim log observation at `/var/folders/93/7fz8hhbn0cs927m25sy9nr8w0000gn/T/claude-emacs-panes.log` — exact tmux command sequences from real Claude Code v2.1.50 agent team runs
- `/Users/darioklingenberg/code/emacs-packages/claude-code-emacs-panes/claude-code-emacs-panes.el` — existing 456-line implementation
- `/Users/darioklingenberg/code/emacs-packages/claude-code-emacs-panes/bin/tmux` — existing 496-line shim
- GNU Emacs Lisp Reference Manual: Sentinels — `set-process-sentinel`, `process-status`
- GNU Emacs Lisp Reference Manual: Timers — `run-with-timer`, `cancel-timer`
- GNU Emacs Lisp Reference Manual: Face Remapping — `face-remap-add-relative`, `face-remap-remove-relative`
- GNU Emacs Lisp Reference Manual: Tabulated List Mode — `tabulated-list-print` with `t` arg

### Secondary (MEDIUM confidence)
- protesilaos.com/codelog (2022-01-08): "Emacs: buffer-local faces (face-remap-add-relative)" — cookie pattern for minor-mode face remapping
- github.com/gonewest818/dimmer.el source — confirmed `face-remap-add-relative` approach, confirmed no vterm-specific handling
- github.com/anthropics/claude-code issue #24385 — confirmed Claude Code does NOT call `kill-pane` when agents finish (shell stays alive)
- GNU Emacs Lisp Reference Manual: Buffer Display Action Functions — `inhibit-switch-frame` parameter

### Tertiary (LOW confidence — needs validation)
- Claim that `face-remap-add-relative` on `default` face does NOT reliably dim vterm terminal-rendered text — inferred from vterm architecture (libvterm C library), not directly tested

---

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH — all built-in Emacs APIs, confirmed in existing code
- Architecture (shim changes): HIGH — based on direct log observation of real Claude Code v2.1.50
- Architecture (Emacs side): HIGH — standard Emacs Lisp patterns from official docs
- vterm visual dimming: MEDIUM — face-remap approach is standard; effectiveness on libvterm content unconfirmed
- Pitfalls: HIGH — derived from code inspection and log analysis

**Research date:** 2026-02-23
**Valid until:** 2026-09-23 (stable APIs — vterm and Emacs core do not change rapidly)
