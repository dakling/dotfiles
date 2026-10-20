# pi

Configuration for [pi](https://github.com/earendil-works/pi), the coding agent
harness. Everything here is linked into `~/.pi/agent/` by `link.sh`.

## Layout

| Path | Purpose |
|---|---|
| `link.sh` | Creates the symlinks into `~/.pi/agent/` |
| `settings.json` | Models, packages, default tools, compaction |
| `mcp.json` | MCP server definitions (GitHub, Linear, Notion, Slack, context7) |
| `models.json` | Local model provider definitions (Ollama, llama.cpp) |
| `lsp.json` | Language server configuration for the `pi-lsp` extension |
| `bash-guard.json` | Extra deny/ask rules for the `bash-guard` extension |
| `AGENTS.md` | Pi-specific instructions, loaded as agent context |
| `extensions/` | Extensions written by hand |

## Usage

```bash
sh ~/.dotfiles/pi/link.sh           # link config into ~/.pi/agent
sh ~/.dotfiles/pi/link.sh --check   # verify the links are in place
```

`link.sh` replaces real files in `~/.pi/agent/` with symlinks and keeps a
timestamped backup of whatever it replaces.

## What is deliberately not linked

- **Skills and prompt templates.** `~/.pi/agent/skills/claude` and
  `~/.pi/agent/prompts` are already symlinks into `~/.claude/`, set up by pi's
  own installer. They stay there, so the Claude Code and pi versions of a
  skill never drift apart.
- **`extensions/herdr-agent-state.ts`.** Written and overwritten by herdr on
  every update.
- **Runtime state.** `npm/`, `sessions/`, `missions/`, `install/`, `bin/`,
  `models-store.json`, and the credential files `auth.json` and
  `mcp-auth.json` are machine-local and are not tracked.

## Credentials

No secrets are stored here. `mcp.json` authenticates the GitHub MCP server by
running `gh auth token` at request time, so the token itself never lands in a
file.
