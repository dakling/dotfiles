# External Integrations

**Analysis Date:** 2026-02-19

## APIs & External Services

**AI / LLM Services:**
- Anthropic Claude - Primary AI assistant via Claude Code CLI
  - Client: `claude-code-ide` Emacs package (`doom/packages.el`, `doom/config.el` lines 901-934)
  - Client: `claudecode.nvim` Neovim plugin (`nvim/lua/plugins/ai.lua`)
  - Protocol: WebSocket (same protocol as VS Code extension)
  - MCP Bridge: `doom/bin/codex-ediff-mcp-server.py` (MCP protocol over stdio, JSON-RPC 2024-11-05)
  - MCP Tools exposed: `openDiff`, `closeAllDiffTabs` (bridge into Emacs ediff)
  - Auth: API key managed via Claude Code CLI (not stored in dotfiles)

- OpenAI Codex - Via `codex-cli` Emacs package
  - Client: `codex-cli` (`doom/packages.el` line 90, `doom/config.el` lines 1046-1050)
  - MCP bridge: `codex-ediff-mcp` local package (`doom/lisp/`)
  - Auth: Via Codex CLI credentials (not in dotfiles)

- Goose AI - Via `goose.el` Emacs package
  - Client: `goose` (`doom/packages.el` line 87-88)
  - Accessible via `prompt-compose` backend
  - Auth: Managed externally

- gptel (LLM framework) - Configured in `doom/config.el` lines 867-894
  - Provides multi-provider chat within Emacs
  - Keybindings: `SPC o l L` to open, `SPC o l f` to add file context

**RSS / News Aggregation:**
- Elfeed RSS reader - Configured in `doom/config.el` lines 1133-1216
  - Sources include: Anthropic, OpenAI, DeepMind, arXiv (cs.LG, cs.AI), MIT, Technology Review, BAIR, distill.pub, Wired, NVIDIA, AWS ML blogs
  - Tag system: `AI`, `ML`, `anthropic`, `podcast` tags

## Data Storage

**Databases:**
- None - This is a configuration repository, no database connections

**File Storage:**
- Local filesystem: org files at `~/org/` (`doom/config.el` line 27)
- Dropbox sync: Abbreviations at `~/Dropbox/Dario/abbrev.el` (`doom/config.el` line 119)
- Dropbox sync: Custom scripts at `~/Dropbox/Helen+Dario/` (loaded at startup, `doom/config.el` lines 1368-1369)
- Email storage: Local Maildir at `~/.mail/gmail/` and `~/.mail/web/` (configured in `mbsyncrc`)
- Password store: `~/.password-store/` (GPG-encrypted, managed via `pass`)

**Caching:**
- Zsh completion cache at `~/.zsh/cache` (`zshrc`)
- AUR failure log at `~/.cache/aur-failures.log` (Linux only, `doom/config.el` lines 1352-1365)

## Authentication & Identity

**Email Identity:**
- Primary: `dario@ellamind.com` (user identity, `doom/config.el` line 10)
- Gmail account: `dario.klingenberg@gmail.com` via SMTP `smtp.gmail.com:465` with STARTTLS
- Web.de account: `dario.klingenberg@web.de` via `smtp.web.de` with STARTTLS
- Both accounts configured in `doom/config.el` lines 477-512

**GPG / Pass:**
- GPG agent managed via `pinentry` Emacs package
- `epg-pinentry-mode` set to `loopback` (`doom/config.el` lines 1230-1238)
- Email passwords retrieved at sync time: `PassCmd "pass web"` and `PassCmd "pass gmail-mbsync"` in `mbsyncrc`
- `+pass/copy-secret` keybinding at `s-P` for password store access

**SSH:**
- No SSH key configs in dotfiles; used for remote commands via `my/run-command-ssh` function

## Monitoring & Observability

**Error Tracking:**
- None - Personal config, no error tracking services

**Logs:**
- AUR build failures logged to `~/.cache/aur-failures.log` (Linux only)
- Emacs checks this every 2 hours and fires alert if failures > 0 (`doom/config.el` lines 1352-1365)

**Alerts / Notifications:**
- `alert` Emacs package with platform-specific backends:
  - Linux: `libnotify` style
  - macOS: `osx-notifier` style
- `mu4e-alert` for email notifications; monitors unread non-trashed mail excluding Web INBOX
- Mode-line display for unread email count

## CI/CD & Deployment

**Hosting:**
- GitHub - Source repository for dotfiles
- GitHub - Hosts custom Emacs packages under `dakling/` organization:
  - `dakling/ediff-chunk-select`
  - `dakling/prompt-compose`
  - `dakling/emacs-claude-bridge`

**CI Pipeline:**
- None - Personal dotfiles, no automated testing or deployment

## Email Integration

**IMAP Sync:**
- Tool: mbsync (isync), config at `mbsyncrc`
- Gmail: `imap.gmail.com` IMAPS with `pass gmail-mbsync` for password
- Web.de: `imap.web.de` IMAPS with `pass web` for password
- Sync interval: every 120 seconds (`mu4e-update-interval 120`, `doom/config.el` line 551)
- Sync command: `mbsync -a` invoked from within Emacs mu4e

**Calendar:**
- icalendar integration via `mu4e-icalendar` (`doom/config.el` lines 556-561)
- Calendar events captured to `~/org/notes.org` under "Inbox" headline
- `gnus-icalendar-org-setup` bridges iCal invites into Org agenda

## MCP (Model Context Protocol)

**MCP Server (outgoing):**
- `doom/bin/codex-ediff-mcp-server.py` - Python stdio MCP server
  - Protocol version: `2024-11-05`
  - Tools: `openDiff` (opens Emacs ediff for reviewing proposed file changes), `closeAllDiffTabs`
  - Calls into running Emacs via `emacsclient --eval`
  - Environment variables: `EMACSCLIENT`, `EMACS_SERVER_FILE`, `EMACS_SOCKET_NAME` for configuration

**MCP Client (incoming):**
- `mcp.el` package connects Emacs as MCP client
- `gptel-mcp-connect` bridges MCP tools into gptel LLM chat (`doom/config.el` lines 895-898)
- `claude-code-ide-mcp-tools` - Additional MCP tool handlers registered with claude-code-ide

## Version Control Integration

**Git:**
- Doom Emacs: `magit` module enabled; repository search path `~/` depth 1 (`doom/config.el` line 236)
- Neovim: `neogit` with telescope and diffview integration (`nvim/lua/plugins/tools.lua`)
- Neovim: `diffview.nvim` for enhanced diffs and file history
- Neovim: `lazygit` launched via toggleterm float (`nvim/lua/plugins/tools.lua` lines 53-65)

## Language Server Protocol (LSP)

**Doom Emacs LSP servers (configured via eglot or lsp-mode):**
- Python: pyright
- Haskell: hls
- LaTeX: lsp-ltex (LanguageTool grammar)
- JavaScript/TypeScript: via `javascript +lsp` module
- Shell: via `sh +lsp` module
- Web: via `web +lsp` module
- YAML: via `yaml +lsp` module
- JSON: via `json +lsp` module
- Julia: via `julia +lsp` module

**Neovim LSP servers (`nvim/lua/plugins/lang.lua`):**
- `pyright` - Python
- `clangd` - C/C++
- `bashls` - Shell scripts
- `hls` - Haskell
- `yamlls` - YAML
- `jsonls` - JSON
- `ltex` - Grammar/LaTeX (en-GB language)

## Platform-Specific Services

**macOS:**
- Amethyst tiling window manager, config at `amethyst.yml`
- `browse-url-default-browser` for URL opening
- `osx-notifier` for system notifications
- Option key mapped to Meta: `ns-alternate-modifier 'meta` (`doom/config.el` line 70)

**Linux (Arch):**
- yay AUR helper for package management
- pacman package list tracked at `~/.dotfiles/pacmanfile.txt`
- dunst notification daemon, config at `dunstrc`
- rofi application launcher, config at `rofi.rasi`
- xbacklight for brightness control
- PulseAudio for volume control (tablet-specific)
- Qtile window manager, config at `qtile.py`
- StumpWM window manager, config at `stumpwm.lisp`
- `libnotify` for desktop notifications

**Linux (NixOS/Raspberry Pi):**
- NixOS configuration at `configuration.nix`
- Configured for `klingenberg-pi` host
- Display manager: lightdm with EXWM session

## Webhooks & Callbacks

**Incoming:** None detected

**Outgoing:** None detected

---

*Integration audit: 2026-02-19*
