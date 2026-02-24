# claude-notify

## What This Is

An Emacs package that sends macOS system notifications when a Claude Code session needs user input. Each notification includes the macOS Space number where the session lives, so the user can quickly switch to the right workspace when running multiple sessions across Spaces.

## Core Value

Know which Space has a Claude session waiting for you — without checking each one manually.

## Requirements

### Validated

(None yet — ship to validate)

### Active

- [ ] Detect when a Claude Code session becomes idle / needs input (permission prompts, questions, task completion)
- [ ] Query the macOS Space number for the Emacs frame hosting that session
- [ ] Fire a macOS system notification with session context and Space number
- [ ] Suppress repeat notifications once the user interacts with the session
- [ ] Fallback: include session topic hint if Space number is unavailable

### Out of Scope

- Linux/Windows support — macOS only, matching the user's primary platform
- Notification grouping / history UI — native macOS notification center handles this
- Custom notification sounds or actions — keep it simple, just inform
- Separate watcher process — everything runs within Emacs

## Context

- User runs Doom Emacs with claude-code-ide.el for Claude Code integration
- Multiple Emacs frames across macOS Spaces, each with its own Claude session
- Amethyst tiling WM is active but reuses macOS Spaces — no special workspace API
- macOS Space number is queryable via private CoreGraphics APIs (`CGSGetActiveSpace`) or tools like `yabai`
- Custom packages follow the `~/code/emacs-packages/` → GitHub → `doom/packages.el` convention
- Notifications from Emacs typically use `osascript` with `display notification` or `terminal-notifier`

## Constraints

- **Platform**: macOS only — uses platform-specific APIs for Space number and notifications
- **Integration**: Must hook into claude-code-ide.el's existing state management — no forking or patching the upstream package
- **Packaging**: Standalone Emacs package in `~/code/emacs-packages/claude-notify`, published to GitHub under `dakling/`
- **Dependencies**: Minimal — avoid requiring external tools beyond what's available on macOS or easily installable via Homebrew

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Notification fires from Emacs, not external watcher | Simpler architecture, direct access to session state | — Pending |
| macOS Space number via private API or helper tool | Apple doesn't provide a public API for this; need to research best approach | — Pending |
| Package lives in ~/code/emacs-packages/ per convention | Consistent with existing custom package workflow | — Pending |

---
*Last updated: 2026-02-23 after project initialization*
