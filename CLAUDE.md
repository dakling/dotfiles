# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal dotfiles managing configurations across macOS (primary) and Linux (Arch, secondary). The primary editor is Doom Emacs with extensive customization; Neovim (LazyVim) is a secondary editor.

## Installation

Configs are symlinked directly (no Stow):
```bash
./INSTALL   # Symlinks doom/, nvim/, stumpwm configs to ~/.config/
```

## Doom Emacs Configuration

Config lives in `doom/` and is symlinked to `~/.config/doom/`.

### Key files
- `doom/init.el` — Module activation and flags
- `doom/config.el` — Main customization (~1500 lines)
- `doom/packages.el` — Package declarations with GitHub recipes
- `doom/lisp/` — Local elisp modules (ediff-chunk-select, codex-ediff-mcp)
- `doom/bin/` — Support scripts (MCP server for ediff bridge)

### Completion & navigation stack
Helm + corfu + orderless (NOT vertico, NOT company).

### Evil mode setup
- Evil-snipe is **disabled**
- Surround is bound to `s` in visual state via `evil-visual-state-map`
- Local leader key is `-`

### Conventions
- Use `setq!` (not `setq`) for defcustom variables — Doom overrides `setq` for these
- Platform-specific code uses `(eq system-type 'gnu/linux)` and `(system-name=)` predicates
- Custom packages live in `~/code/emacs-packages/` and are published to GitHub under `dakling/`
- Reference custom packages in `packages.el` with `:recipe (:host github :repo "dakling/PACKAGE-NAME")`

### AI integration
- `claude-code-ide.el` with MCP tools enabled
- `prompt-compose` for prompt composition (local package at `~/code/emacs-packages/prompt-compose`)
- `codex-ediff-mcp` — bridges Claude Code diffs into Emacs ediff with per-hunk selection

### Syncing after changes
```bash
~/.config/emacs/bin/doom sync   # After modifying init.el or packages.el
```
Reload config within Emacs: `M-x doom/reload` (sufficient for config.el-only changes).

## Neovim Configuration

`nvim/` uses LazyVim (lua-based). Entry point is `nvim/init.lua`, plugins in `nvim/lua/plugins/`, config in `nvim/lua/config/`.

## Shell & Other Configs

- `zshrc`, `bashrc` — Shell configuration
- `alacritty.yml` — Terminal emulator
- `amethyst.yml` — macOS tiling window manager
- `binCustom/` — Custom shell scripts and utilities
- `stumpwm.lisp`, `qtile.py` — Linux window managers

## Platform Notes

macOS (Darwin) is the primary platform. Linux-specific code (shutdown helpers, AUR checking, system management) is gated behind system checks. On macOS, `ns-alternate-modifier` is set to `meta` for Option key behavior.
