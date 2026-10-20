# Pi-specific instructions

These complement the global `AGENTS.md` (`~/.claude/CLAUDE.md`). That file is
written for Claude Code; this one describes how to work inside **pi**.

## Tool names

Pi's tools are `read`, `bash`, `edit`, `write`, `grep`, `find`, `ls` — plus
whatever the installed extensions register (`subagent`, `lsp_*`, `web_search`,
`web_fetch`, and `mcp__<server>__<tool>` for MCP servers).

Claude Code tool names in the global instructions do not exist here. Map them:

| Claude Code | Pi |
|---|---|
| `Read` | `read` |
| `Bash` | `bash` |
| `Edit` / `Write` | `edit` / `write` |
| `Grep` / `Glob` | `grep` / `find` |
| `Task` / `Explore` | `subagent` (see below) |
| `AskUserQuestion` | ask in plain chat text; there is no such tool |
| `mcp__plugin_github_github__*` | `mcp__github__*` |

There is no `AskUserQuestion` tool. Put the question and the options in your
reply as text and stop there.

## Plan mode

`/plan` (or `Ctrl+Alt+P`, or `pi --plan`) toggles a read-only exploration mode
for safe analysis before making changes.

- `edit` and `write` are disabled; other active tools stay available.
- `bash` is restricted to a read-only allowlist.
- Agent-spawning tools (`subagent`, `contact_supervisor`) are blocked: a child
  agent runs with its own tool set and would write files anyway.
- MCP tools whose `readOnlyHint` annotation is `false` are blocked, since they
  can change external state. Read-only MCP tools still work.
- The agent answers with a numbered plan under a `Plan:` header. Choosing
  "Execute the plan" restores full access and tracks steps via `[DONE:n]`.

State survives `/reload` and session resume. Use `/todos` to show progress.

## GitHub

Prefer the GitHub MCP tools (`mcp__github__*`) over the `gh` CLI. The MCP
server authenticates with `gh auth token` and returns structured results.

## Delegation

`pi-subagents` is installed. Delegate focused work with the `subagent` tool and
ask for a specific agent by name — `scout` (understand code), `reviewer`
(review a diff), `researcher`, `oracle` (second opinion), `worker`
(implementation). Ask in plain language, e.g. "Use scout to understand the
auth flow, then ask me clarification questions." Do not spawn a second agent
for work the main session can do directly.

## Safety

`bash-guard` (in `~/.pi/agent/extensions/bash-guard.ts`) enforces the same
policy as the Claude Code `permissions.deny` list:
- Destructive commands (`rm -rf /`, `git reset --hard`, force-push to
  `main`/`master`, `mkfs`, `dd`) are **blocked outright**.
- Risky-but-legitimate commands (`git push`, `npm publish`, `docker compose
  up/down`, `sudo`, `curl | sh`, `chmod 777`) **require your confirmation**.
- The `read` tool refuses to open `.env`, `.netrc`, `.pgpass`, `.ssh/*`, and
  similar credential files.

If a command is blocked, do not try to work around it. Say what was blocked and
ask. Add exceptions in `~/.pi/agent/bash-guard.json` if the block is wrong.

## Skills and commands

Nine skills from `~/.claude/skills` are exposed as `/skill:<name>` commands
(`docker-check`, `loop-engineering`, `post-meeting`, `duplication-review`,
`revealjs-presentation`, `doom-emacs-dev`, `demo-video-pipeline`,
`linear-loop-triage`, `selbstzahler-einstufung`).

The `flow` commands are exposed as prompt templates: `/flow:new-project`,
`/flow:discuss-phase`, `/flow:plan-phase`, `/flow:execute-phase`,
`/flow:progress`, `/flow:add-phase`, `/flow:insert-phase`.

## Models

The default is `ollama/minimax-m3.1-flash:cloud` (1M context, reasoning).
`Ctrl+P` cycles through the models in `enabledModels`. The `ollama` provider
serves local models, so per-token cost is $0 — do not report a dollar figure
for local inference.

## Conventions

- Use `uv` / `uvx` for Python, never bare `python`.
- Prefer `fd`/`rg` over `find`/`grep` in shell commands.
- Do not compose or start Docker containers; ask instead.
- Never post a reply to a human's PR review comment on your own.
