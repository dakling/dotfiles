# Doom-like tmux

This config aims for Doom Emacs ergonomics, not literal feature parity.

## What maps well

- `C-Space` is the reliable tmux leader. `M-Space` can additionally act as the Doom-like alias if your terminal is configured to send Meta-Space.
- This gives you `C-Space w ...`, `C-Space b ...`, and `C-Space p ...` everywhere, with `M-Space ...` available in terminals that can map left Option to Meta.
- `w` behaves like a Doom window prefix: navigation on `h/j/k/l`, splits on `s` and `v`, zoom on `m`, delete on `c`, delete-others on `o`, number overlay on `w`.
- `b` treats tmux windows as "buffer-like" units: switch, rename, kill, next, previous, chooser.
- `p` treats tmux sessions as project/workspace-like units: create, rename, kill, jump back, and open a chooser.
- `p c` opens a popup project launcher that scans common roots for Git repositories and switches to a matching session.
- Copy mode is vi-first and tries to feel like Evil selection rather than stock tmux copy mode.
- The status line and pane borders use a `doom-palenight` palette instead of generic tmux colors.
- `C-Space '` opens a popup shell, which is the closest built-in tmux analog to Doom popups.
- `Cmd+Ctrl+h/j/k/l` is possible, but only through terminal-emulator key translation. tmux is ready for it via private user keys.

## What only approximates Doom

- tmux panes are closer to Doom windows than Emacs buffers. tmux windows are closer to tabs or lightweight workspaces than real buffers.
- tmux sessions are a decent approximation for Projectile/workspaces, but tmux has no native project awareness.
- `display-panes` approximates Doom's numbered window selection, but it is more primitive than `ace-window` or Doom's `window-select`.
- Popup shells are possible. Popup rules based on buffer type, major mode, or command metadata are not.

## What tmux cannot really do

- Real Evil modal semantics across every program running inside tmux. tmux only sees terminal key input; it does not know whether an app is in normal state, insert state, minibuffer state, or visual state.
- A literal `SPC` leader like Doom. In tmux, plain space is normal terminal input, so making it the leader would be intrusive and fragile.
- Native detection of macOS `Cmd` inside tmux. The terminal must translate that key combo before tmux can react to it.
- Doom completion behavior (`helm`, `vertico`, `ivy`, minibuffer actions, persistent actions, previews).
- Doom buffer/project/file semantics. tmux does not know what files are open inside your editor.
- Magit-, Org-, LSP-, or popup-rule-level intelligence. Those are editor features, not multiplexer features.
- Per-mode localleaders. There is only tmux's key table model, not Emacs major/minor modes.

## Key cheatsheet

- `C-Space w h/j/k/l`: move between panes
- `C-Space w s`: split below
- `C-Space w v`: split right
- `C-Space w c`: kill pane
- `C-Space w o`: keep only current pane
- `C-Space w w`: numbered pane chooser
- `C-Space w m`: toggle zoom
- `C-Space b b`: choose tmux window
- `C-Space b n/p/l`: next, previous, last window
- `C-Space b c`: new window
- `C-Space p p`: choose session
- `C-Space p c`: open project launcher
- `C-Space p n`: create or attach named session
- `C-Space p l`: jump to last session
- `C-Space '`: popup shell
- `C-Space R`: reload tmux config
- `C-Space q`: detach tmux client
- `M-Space ...`: same leader tree, if your terminal sends Meta-Space
- `Cmd+Ctrl+h/j/k/l`: pane navigation, once your terminal maps those combos to the tmux user-key sequences
- `Prefix C-h/j/k/l`: resize pane
- `Prefix H/J/K/L`: swap panes
- Full reference: [tmux-cheatsheet.md](/Users/darioklingenberg/.dotfiles/tmux-cheatsheet.md)

## Likely next refinements

- Add optional TPM plugins if you want persistence, fuzzy pickers, or better clipboard integration.
- Mirror more of your actual Doom leader tree if there are specific `SPC` sequences you use every day and want ported over.

## iTerm2 setup

iTerm2 is a good macOS choice here because it can emit the exact sequences tmux is expecting.

- Keep `C-Space` as the reliable leader. This works even before any terminal customization.
- In iTerm2, set left Option to act as `Esc+`. That makes `Option-Space` become tmux's Doom-like `M-Space` alias.
- Add four key mappings in iTerm2 for `Cmd+Ctrl+h/j/k/l` that use `Send Escape Sequence`.
- Important: `Send Escape Sequence` prepends the `ESC` byte for you, so enter only the tail text in the field.
- Use these exact values in the text field:
- `h`: `[1000u`
- `j`: `[1001u`
- `k`: `[1002u`
- `l`: `[1003u`
- If you prefer `Send Hex Code` instead, use:
- `h`: `0x1b 0x5b 0x31 0x30 0x30 0x30 0x75`
- `j`: `0x1b 0x5b 0x31 0x30 0x30 0x31 0x75`
- `k`: `0x1b 0x5b 0x31 0x30 0x30 0x32 0x75`
- `l`: `0x1b 0x5b 0x31 0x30 0x30 0x33 0x75`
- After updating tmux, reload it with `tmux source-file ~/.tmux.conf` or start a fresh tmux session.
