# tmux cheat sheet

This lists every custom binding in [tmux.conf](/Users/darioklingenberg/.dotfiles/tmux.conf).

## Core ideas

- Main leader: `C-Space`
- Fallback leader: `C-b`
- Optional Doom-like alias: `M-Space`, if your terminal sends Meta-Space
- tmux windows are treated like "buffers"
- tmux sessions are treated like "projects/workspaces"

## Global keys

| Keys | Action |
| --- | --- |
| `C-Space` | tmux leader |
| `C-b` | tmux fallback leader |
| `M-Space` | alias into the tmux leader table |
| `Cmd+Ctrl+h` | move to left pane, if terminal sends `[1000u` |
| `Cmd+Ctrl+j` | move to lower pane, if terminal sends `[1001u` |
| `Cmd+Ctrl+k` | move to upper pane, if terminal sends `[1002u` |
| `Cmd+Ctrl+l` | move to right pane, if terminal sends `[1003u` |

## Prefix keys

Use these after `C-Space` or `C-b`.

| Keys | Action |
| --- | --- |
| `?` | show a short help message |
| `'` | open popup shell in current path |
| `:` | open tmux command prompt |
| `q` | detach current client |
| `z` | toggle pane zoom |
| `R` | reload `~/.tmux.conf` |
| `r` | reload `~/.tmux.conf` |
| `c` | create new window in current path |
| `"` | split below in current path |
| `%` | split right in current path |
| `h` | move to left pane |
| `j` | move to lower pane |
| `k` | move to upper pane |
| `l` | move to right pane |
| `C-h` | resize pane left |
| `C-j` | resize pane down |
| `C-k` | resize pane up |
| `C-l` | resize pane right |
| `H` | swap with left pane |
| `J` | swap with lower pane |
| `K` | swap with upper pane |
| `L` | swap with right pane |
| `Escape` | enter copy mode |
| `w` | enter window submap |
| `b` | enter buffer submap |
| `p` | enter project submap |

## Window submap

Use these after `C-Space w`.

| Keys | Action |
| --- | --- |
| `h` | move to left pane |
| `j` | move to lower pane |
| `k` | move to upper pane |
| `l` | move to right pane |
| `s` | split below |
| `v` | split right |
| `c` | kill current pane |
| `d` | kill current pane |
| `q` | kill current pane |
| `o` | keep only current pane |
| `m` | toggle pane zoom |
| `=` | balance layout with `tiled` |
| `w` | show numbered pane chooser |
| `W` | open full tree chooser |
| `r` | rename current tmux window |
| `H` | swap with left pane |
| `J` | swap with lower pane |
| `K` | swap with upper pane |
| `L` | swap with right pane |

## Buffer submap

Use these after `C-Space b`.

| Keys | Action |
| --- | --- |
| `b` | choose tmux window from tree |
| `n` | next tmux window |
| `p` | previous tmux window |
| `l` | last tmux window |
| `c` | create new tmux window |
| `d` | kill current tmux window |
| `k` | kill current tmux window |
| `q` | kill current tmux window |
| `.` | choose tmux window |
| `r` | rename current tmux window |

## Project submap

Use these after `C-Space p`.

| Keys | Action |
| --- | --- |
| `p` | choose tmux session from tree |
| `l` | jump to last session |
| `c` | open project launcher popup |
| `f` | open project launcher popup |
| `n` | create or attach named session in current path |
| `r` | rename current session |
| `d` | kill current session |
| `k` | kill current session |

## Copy mode

Use these after entering copy mode with `C-Space Escape`.

| Keys | Action |
| --- | --- |
| `v` | begin selection |
| `V` | select whole line |
| `C-v` | toggle rectangular selection |
| `y` | copy selection and exit copy mode |
| `Y` | copy current line |
| `H` | jump to start of line |
| `L` | jump to end of line |

## Recommended muscle memory

| Intent | Keys |
| --- | --- |
| move panes like Doom windows | `C-Space w h/j/k/l` |
| split below | `C-Space w s` |
| split right | `C-Space w v` |
| close current pane | `C-Space w q` |
| keep only this pane | `C-Space w o` |
| zoom pane | `C-Space w m` |
| pick pane visually | `C-Space w w` |
| switch "buffers" | `C-Space b b` |
| new "buffer" | `C-Space b c` |
| kill "buffer" | `C-Space b k` |
| switch "project" | `C-Space p p` |
| launch project picker | `C-Space p c` |
| create named session | `C-Space p n` |
| popup shell | `C-Space '` |
| reload config | `C-Space R` |
| detach from tmux | `C-Space q` |

## iTerm2 notes

To make `M-Space` and `Cmd+Ctrl+h/j/k/l` work well in iTerm2:

- set left Option to `Esc+`
- map `Cmd+Ctrl+h` to `Send Escape Sequence` with `[1000u`
- map `Cmd+Ctrl+j` to `Send Escape Sequence` with `[1001u`
- map `Cmd+Ctrl+k` to `Send Escape Sequence` with `[1002u`
- map `Cmd+Ctrl+l` to `Send Escape Sequence` with `[1003u`

## Project launcher behavior

- Search roots default to `~/code`, `~/.dotfiles`, `~/org`, and `~/obsidian-vault`
- Override roots with `TMUX_PROJECT_ROOTS`, separated by `:`
- The launcher prefers `fzf` if installed
- Without `fzf`, it asks for a query and then a number inside the tmux popup
- Session names are derived from the project path, so nested repos stay distinct

## Claude/Codex indicators

- Per-pane staleness countdown in the pane title bar (fresh `Xm`, expired `stale`)
- Per-window activity dot in the tab line (green when any pane has recent tool use, red when idle)
- TTL override: `CLAUDE_CACHE_TTL` (default 3600s)
- Activity threshold is inside `~/.local/bin/tmux-claude-status.sh` (`IDLE_THRESHOLD=30`)
