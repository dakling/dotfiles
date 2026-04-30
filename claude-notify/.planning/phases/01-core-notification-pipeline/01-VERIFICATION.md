---
phase: 01-core-notification-pipeline
verified: 2026-02-24T17:15:00Z
status: gaps_found
score: 3/4 must-haves verified
gaps:
  - truth: "User can toggle notifications on/off via M-x claude-notify-mode and the setting persists across Emacs restarts"
    status: partial
    reason: "The mode exists and toggles correctly, but the package is not registered in doom/packages.el or doom/config.el, and has no GitHub remote. Without Doom integration, the package cannot load on startup, so mode state cannot persist across restarts."
    artifacts:
      - path: "~/.dotfiles/doom/packages.el"
        issue: "No claude-notify entry -- package not declared as a Doom dependency"
      - path: "~/.dotfiles/doom/config.el"
        issue: "No claude-notify configuration -- no use-package!/after! block to load or configure the mode"
      - path: "~/code/emacs-packages/claude-notify"
        issue: "No git remote -- package not pushed to GitHub (required by project convention for Doom recipe)"
    missing:
      - "Push package to GitHub under dakling/claude-notify"
      - "Add entry to doom/packages.el: (package! claude-notify :recipe (:host github :repo \"dakling/claude-notify\"))"
      - "Add configuration in doom/config.el to load and optionally auto-enable the mode"
      - "Run doom sync to make the package available"
human_verification:
  - test: "Run M-x claude-notify-install-hook and verify settings.json"
    expected: "~/.claude/settings.json gains a Notification hook entry for permission_prompt; all existing hooks/plugins/env preserved; .bak backup created"
    why_human: "Modifies user config file; need to verify no data loss in actual settings.json which may differ from test assumptions"
  - test: "Trigger a real Claude Code permission_prompt event"
    expected: "macOS notification appears within seconds showing project name and permission message"
    why_human: "End-to-end pipeline requires live Claude Code session; cannot simulate CLI hook invocation programmatically"
  - test: "Disable mode and trigger permission_prompt"
    expected: "No notification appears"
    why_human: "Requires live Claude Code session to verify mode gating under real conditions"
---

# Phase 1: Core Notification Pipeline Verification Report

**Phase Goal:** User receives a macOS system notification when a Claude Code session needs input (permission prompt), with session identification, triggered via the official CLI hook mechanism
**Verified:** 2026-02-24T17:15:00Z
**Status:** gaps_found
**Re-verification:** No -- initial verification

## Goal Achievement

### Observable Truths

| # | Truth | Status | Evidence |
|---|-------|--------|----------|
| 1 | Running `M-x claude-notify-install-hook` configures `~/.claude/settings.json` so Claude Code fires the Emacs callback on permission prompts | VERIFIED | `claude-notify-install-hook` exists (line 222), is `interactive` (line 233), autoloaded (line 221). Reads settings.json via `claude-notify--read-settings` (hash-table), adds Notification hook with `permission_prompt` matcher (line 266), preserves existing settings via hash-table merge, creates .bak backup (line 213), sets script executable, enables mode |
| 2 | When Claude Code hits a permission prompt, a macOS notification appears within seconds showing which project/session needs attention | VERIFIED | Full pipeline wired: hook script reads stdin JSON (line 9), passes via `server-eval-args-left` (line 15) to `claude-notify--handle-hook` (line 146), parses JSON (line 158), extracts project name from `cwd` (line 164-166), dispatches `permission_prompt` (line 182), calls `claude-notify--send` with project as subtitle (line 183-186). Needs human verification for real Claude events. |
| 3 | User can toggle notifications on/off via `M-x claude-notify-mode` and the setting persists across Emacs restarts | PARTIAL | Mode exists (`define-minor-mode` line 59, `:global t` line 65), toggles correctly, is autoloaded (line 58). **However**, package is NOT registered in `doom/packages.el` and NOT configured in `doom/config.el`. No GitHub remote exists. Without Doom integration, the package cannot load on Emacs startup, so persistence is impossible. |
| 4 | Notifications work on a fresh macOS system with zero external tool installs (osascript fallback if ns-do-applescript unavailable) | VERIFIED | Default backend is `osascript-async` (line 41) which uses `/usr/bin/osascript` (confirmed present on macOS). `ns-do-applescript` backend has `condition-case` fallback (lines 112-122) that catches `void-function` and falls back to osascript-async. `claude-notify--send` dispatch (line 136) also checks `fboundp` before calling ns backend (line 140-142). Zero external tools required. |

**Score:** 3/4 truths verified (1 partial)

### Required Artifacts

| Artifact | Expected | Status | Details |
|----------|----------|--------|---------|
| `~/code/emacs-packages/claude-notify/claude-notify.el` | Package with mode, backends, dispatch, hook handler, install command | VERIFIED | 302 lines. Contains: package header, customization group, defcustom, global minor mode, AppleScript quoting, 2 backends, dispatch, hook handler, JSON dispatch, settings.json helpers, install-hook command, provide statement. All min_lines thresholds met (>150 for plan 02, >80 for plan 01). |
| `~/code/emacs-packages/claude-notify/bin/claude-notify-hook.sh` | Shell bridge from Claude Code hook stdin to emacsclient | VERIFIED | 19 lines. Executable. Reads stdin via `cat`, passes JSON via `server-eval-args-left`, uses `CLAUDE_NOTIFY_EMACS_SOCKET` env var for socket path, `|| true` and `exit 0` for resilience. |
| `~/.dotfiles/doom/packages.el` | claude-notify package declaration | MISSING | No entry for claude-notify. Other custom packages follow pattern: `(package! NAME :recipe (:host github :repo "dakling/NAME"))` |
| `~/.dotfiles/doom/config.el` | claude-notify configuration block | MISSING | No `use-package!` or `after!` block for claude-notify |

### Key Link Verification

| From | To | Via | Status | Details |
|------|-----|-----|--------|---------|
| `bin/claude-notify-hook.sh` | `claude-notify--handle-hook` | `emacsclient --eval` with `server-eval-args-left` | WIRED | Line 15: `--eval '(claude-notify--handle-hook (pop server-eval-args-left))'` with `"$JSON"` argument |
| `claude-notify--handle-hook` | `claude-notify--dispatch` | Function call after JSON parse | WIRED | Line 167-168: `(claude-notify--dispatch notification-type project-name session-id message title)` |
| `claude-notify--dispatch` | `claude-notify--send` | Function call with title/subtitle/body | WIRED | Line 183-186: `(claude-notify--send (or title "Claude Code") (or project-name "Unknown project") (or message "Permission needed"))` |
| `claude-notify-install-hook` | `~/.claude/settings.json` | `json-parse-string` + `json-serialize` round-trip | WIRED | Line 210: `json-serialize`, lines 196-203: `claude-notify--read-settings` with hash-table, lines 205-217: `claude-notify--write-settings` |
| `claude-notify-install-hook` | `bin/claude-notify-hook.sh` | `locate-library` to find script path | WIRED | Line 237: `(locate-library "claude-notify")`, line 241: `(expand-file-name "bin/claude-notify-hook.sh" lib-dir)` |
| `doom/packages.el` | `claude-notify` package | Doom straight.el recipe | NOT_WIRED | No entry in packages.el -- package not available in Doom |
| `doom/config.el` | `claude-notify-mode` | `use-package!` or `after!` block | NOT_WIRED | No configuration -- mode cannot auto-enable or persist |

### Requirements Coverage

| Requirement | Source Plan | Description | Status | Evidence |
|-------------|-----------|-------------|--------|----------|
| DET-01 | 01-02 | CLI hook callback handler -- receive notifications from Claude Code via `emacsclient --eval` | SATISFIED | `claude-notify--handle-hook` (line 146) receives JSON via `server-eval-args-left`, called by hook shell script |
| DET-02 | 01-02 | Hook installation helper (`M-x claude-notify-install-hook`) to configure `~/.claude/settings.json` | SATISFIED | `claude-notify-install-hook` (line 222), interactive, autoloaded, idempotent merge with backup |
| DET-03 | 01-02 | `permission_prompt` detection (immediate, reliable signal) | SATISFIED | `claude-notify--dispatch` (line 181-182) matches on `"permission_prompt"` string from JSON payload |
| NTF-01 | 01-01 | `ns-do-applescript` as primary notification backend (in-process, fast) | SATISFIED | `claude-notify--send-ns-applescript` (line 102) with `condition-case` fallback |
| NTF-02 | 01-01 | `start-process osascript` as async fallback backend | SATISFIED | `claude-notify--send-osascript-async` (line 87) uses `start-process "claude-notify" nil "osascript" "-e"` |
| NTF-03 | 01-02 | Session/project identifier in notification text | SATISFIED | Project name derived from `cwd` (line 164-166), passed as subtitle (line 185) |
| NTF-05 | 01-01 | `claude-notify-mode` global minor mode toggle | SATISFIED | `define-minor-mode claude-notify-mode` (line 59), `:global t`, toggles with messages (lines 68-70) |

No orphaned requirements found -- all 7 Phase 1 requirement IDs are covered by the two plans.

### Anti-Patterns Found

| File | Line | Pattern | Severity | Impact |
|------|------|---------|----------|--------|
| (none) | - | - | - | No TODO/FIXME/HACK/placeholder comments found |
| (none) | - | - | - | No empty implementations found |
| (none) | - | - | - | No stub returns found |

No anti-patterns detected. The codebase is clean.

### Human Verification Required

### 1. End-to-end notification with real Claude Code session

**Test:** Start a Claude Code session, trigger a permission prompt (e.g., let it try to use Bash without auto-approve), verify macOS notification appears.
**Expected:** macOS notification with title from hook JSON, project directory name as subtitle, permission message as body. Appears within seconds.
**Why human:** Requires live Claude Code CLI session with the hook configured in settings.json.

### 2. Settings.json preservation

**Test:** Run `M-x claude-notify-install-hook` and diff the resulting `~/.claude/settings.json` against the `.bak` backup.
**Expected:** Only the `hooks.Notification` array changed; all other top-level keys (PreToolUse, SessionStart, plugins, env, model, statusLine, etc.) are byte-identical.
**Why human:** The actual settings.json contents vary per user; automated check cannot know all expected keys.

### 3. Mode gating under live conditions

**Test:** Disable `claude-notify-mode`, trigger a permission prompt, verify no notification. Re-enable, trigger again, verify notification appears.
**Expected:** Notifications only fire when mode is enabled.
**Why human:** Requires live Claude Code session.

### Gaps Summary

**1 gap found blocking full goal achievement:**

The package code is complete and all internal wiring is correct. The gap is **external wiring into Doom Emacs**:

1. **No GitHub remote** -- The package has no git remote, so it cannot be referenced by a Doom straight.el recipe. Other custom packages (ediff-chunk-select, prompt-compose, claude-code-emacs-panes, emacs-claude-bridge) all follow the convention of being pushed to GitHub under `dakling/` and declared in `doom/packages.el` with `:recipe (:host github :repo "dakling/PACKAGE")`.

2. **No doom/packages.el entry** -- Without a package declaration, Doom cannot install or manage the package. The user cannot `require` it in config.el.

3. **No doom/config.el configuration** -- Without a config block, the mode cannot be auto-enabled on startup, and `customize-save-variable` cannot persist it (because the package would not be loaded after restart).

These three items together mean that while the package works perfectly when manually loaded (`load-file`), it does not integrate into the user's normal Emacs startup flow. Success criterion 3 ("persists across Emacs restarts") is not achievable without this wiring.

**Root cause:** The plans focused on the package code itself but did not include tasks for publishing to GitHub or integrating into the Doom config. This is a scope gap in the plans, not an implementation bug.

---

_Verified: 2026-02-24T17:15:00Z_
_Verifier: Claude (flow-verifier)_
