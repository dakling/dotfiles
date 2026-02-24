# Requirements: claude-notify

**Defined:** 2026-02-24
**Core Value:** Know which Space has a Claude session waiting for you — without checking each one manually.

## v1 Requirements

Requirements for initial release. Each maps to roadmap phases.

### Detection & Hook Integration

- [ ] **DET-01**: CLI hook callback handler — receive notifications from Claude Code via `emacsclient --eval`
- [ ] **DET-02**: Hook installation helper (`M-x claude-notify-install-hook`) to configure `~/.claude/settings.json`
- [ ] **DET-03**: `permission_prompt` detection (immediate, reliable signal)
- [ ] **DET-04**: `idle_prompt` detection with 30-60s debounce to mitigate false positives

### Space Resolution

- [ ] **SPC-01**: Swift CLI for Space number query using SkyLight private APIs
- [ ] **SPC-02**: Compile-on-first-use from bundled Swift source via `swiftc`
- [ ] **SPC-03**: Frame-to-CGS-window correlation via pixel position matching
- [ ] **SPC-04**: Graceful fallback when Space number unavailable (show project name only)

### Notification Delivery

- [ ] **NTF-01**: `ns-do-applescript` as primary notification backend (in-process, fast)
- [ ] **NTF-02**: `start-process osascript` as async fallback backend
- [ ] **NTF-03**: Session/project identifier in notification text
- [ ] **NTF-04**: macOS Space number in notification text
- [ ] **NTF-05**: `claude-notify-mode` global minor mode toggle
- [ ] **NTF-06**: `claude-notify-test` verification command for validating setup

### Notification Intelligence

- [ ] **INT-01**: Per-session cooldown timer (configurable, default 30s)
- [ ] **INT-02**: Same-Space suppression (skip notification if user is on that Space)
- [ ] **INT-03**: Focus suppression (skip notification if Claude buffer is visible)
- [ ] **INT-04**: Rising-edge detection (notify only on transition to "needs input" state)

### Enhanced Experience

- [ ] **ENH-01**: terminal-notifier backend with `-group` flag for notification coalescing
- [ ] **ENH-02**: Click-to-activate Emacs on notification click (`-activate org.gnu.Emacs`)
- [ ] **ENH-03**: Sound on notification (configurable, default system sound)
- [ ] **ENH-04**: Claude's icon on notification via `-sender com.anthropic.claudefordesktop`
- [ ] **ENH-05**: Modeline indicator for attention-needed sessions
- [ ] **ENH-06**: Distinct notification types with context (permission vs. idle vs. complete)

## v2 Requirements

Deferred to future release. Tracked but not in current roadmap.

### Extended Features

- **EXT-01**: Notification history buffer (`*claude-notify-log*`)
- **EXT-02**: Customizable notification title template (format string)
- **EXT-03**: Auto-focus specific Emacs frame on notification click
- **EXT-04**: alert.el integration as optional backend
- **EXT-05**: Space label support (yabai/Amethyst named spaces)
- **EXT-06**: Team/subagent awareness (distinguish main session from subagents)
- **EXT-07**: Upstream claude-code-ide.el hook integration (when available)

## Out of Scope

Explicitly excluded. Documented to prevent scope creep.

| Feature | Reason |
|---------|--------|
| Linux/Windows support | macOS only — Space number is platform-specific, matching user's primary platform |
| Custom notification daemon/process | Everything runs within Emacs — no external watchers |
| Fork/monkey-patch claude-code-ide.el | Maintenance nightmare — hook into public interfaces only |
| Bypass macOS Do Not Disturb | Users enable DND for a reason — respect it |
| Polling-based detection as primary | Wasteful and laggy — use event-driven hooks |
| Notification action buttons | Fragile dependency (alerter tool), user must go to Emacs anyway |
| terminal-notifier as hard dependency | Must work out-of-box on macOS with zero installs |

## Traceability

Which phases cover which requirements. Updated during roadmap creation.

| Requirement | Phase | Status |
|-------------|-------|--------|
| DET-01 | — | Pending |
| DET-02 | — | Pending |
| DET-03 | — | Pending |
| DET-04 | — | Pending |
| SPC-01 | — | Pending |
| SPC-02 | — | Pending |
| SPC-03 | — | Pending |
| SPC-04 | — | Pending |
| NTF-01 | — | Pending |
| NTF-02 | — | Pending |
| NTF-03 | — | Pending |
| NTF-04 | — | Pending |
| NTF-05 | — | Pending |
| NTF-06 | — | Pending |
| INT-01 | — | Pending |
| INT-02 | — | Pending |
| INT-03 | — | Pending |
| INT-04 | — | Pending |
| ENH-01 | — | Pending |
| ENH-02 | — | Pending |
| ENH-03 | — | Pending |
| ENH-04 | — | Pending |
| ENH-05 | — | Pending |
| ENH-06 | — | Pending |

**Coverage:**
- v1 requirements: 24 total
- Mapped to phases: 0
- Unmapped: 24 ⚠️

---
*Requirements defined: 2026-02-24*
*Last updated: 2026-02-24 after initial definition*
