# Claude Code Emacs Panes — Multiplexing Subagents in Emacs

## What This Is

A Doom Emacs integration that makes Claude Code subagent/teammate processes visible as individual vterm buffers inside Emacs. When Claude Code spawns parallel agents (via Task tool, GSD teams, or manual team requests), each agent gets its own vterm pane that you can watch, switch between, and interact with — instead of agents running invisibly in the background.

## Core Value

When Claude Code spawns subagents, each one appears as a live vterm buffer in Emacs that I can observe and navigate between.

## Requirements

### Validated

(None yet — ship to validate)

### Active

- [ ] Subagent processes appear as individual vterm buffers in Emacs
- [ ] Feature works reliably in fresh Emacs sessions (not just during development)
- [ ] Tmux shim correctly intercepts Claude Code's agent management calls
- [ ] Environment variables propagate correctly from main session to subagents
- [ ] Finished agent panes auto-close when their process exits
- [ ] Emacsclient communication is robust (error handling, retries, fallbacks)
- [ ] Shim path resolution works regardless of how the package is loaded (straight.el, local dev, etc.)
- [ ] Observable agent status — indicators showing which agents are running, activity state
- [ ] Debug/diagnostic mode to verify the shim and env injection are working
- [ ] Compatible with current Claude Code version (v2.1.47+) agent spawning mechanism

### Out of Scope

- Managed tiling dashboard layout — simple buffer-per-agent is sufficient for v1
- Background-with-notification model — agents should be directly visible
- Neovim integration — Emacs only
- Custom agent spawning UI — we hook into Claude Code's existing mechanism

## Context

### Existing Implementation

A working prototype exists as `claude-code-emacs-panes` package at `~/code/emacs-packages/claude-code-emacs-panes/` with:
- **Elisp package** (`claude-code-emacs-panes.el`): pane registry, vterm buffer creation/management, navigation commands (next/prev/select/show-all/dashboard), environment injection via advice on `claude-code-ide--start-session`
- **Tmux shim** (`bin/tmux`): bash script intercepting ~15 tmux subcommands (split-window, send-keys, kill-pane, new-session, etc.), translating them to `emacsclient --eval` calls
- **5 commits** of progressive fixes (env injection, shim dir resolution, session function targeting, cli flags)

### Known Bug

The feature worked during the development session but failed in fresh Emacs sessions. Agent spawning succeeded (Claude Code thought tmux was working) but vterm panes didn't appear. Likely causes:
1. `claude-code-emacs-panes--package-dir` uses `load-file-name` at load time — may resolve differently when installed via straight.el vs loaded from source
2. `emacsclient --socket-name` may not match the actual server name
3. Environment variable propagation may break in the package-managed installation path
4. The `--teammate-mode tmux` CLI flag used in setup may no longer exist in Claude Code v2.1.47

### Integration Points

- `claude-code-ide` — main Claude Code Emacs integration (active, working)
- `claude-code-ide-mcp-tools` — MCP tools support
- `claude-code-ide-extras` — additional IDE enhancements
- Doom Emacs config at `~/.dotfiles/doom/` (config.el, packages.el)
- Currently commented out in `packages.el` (lines 97-99) and `config.el` (lines 936-947)

## Constraints

- **Package system**: Must work when installed via straight.el from GitHub (Doom's package manager), not just when loaded from local source
- **Emacs server**: Requires running Emacs server for emacsclient communication
- **Claude Code API**: Must work with how Claude Code actually spawns agents — the `--teammate-mode tmux` flag may no longer exist, need to verify and adapt
- **Platform**: macOS (Darwin) primary, via Doom Emacs with evil-mode
- **Terminal backend**: Must work with both vterm and eat backends (claude-code-ide supports both)

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Tmux shim approach | Intercepts Claude Code's native tmux management rather than building a custom agent spawner | — Pending (need to verify still viable with current Claude Code) |
| vterm for agent panes | Consistent with how claude-code-ide runs the main session | — Pending |
| Auto-close finished panes | User preference — keep UI clean | — Pending |
| Package on GitHub (dakling/) | Follows established pattern for local Emacs packages | ✓ Good |

---
*Last updated: 2026-02-19 after initialization*
