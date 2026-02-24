# Phase 3: Ship - Research

**Researched:** 2026-02-24
**Domain:** Doom Emacs package publishing, dotfiles config finalization, smoke testing, GitHub deployment
**Confidence:** HIGH

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions

#### Cross-machine portability
- Audience: personal use across multiple machines (not a public package for other users)
- Platform: macOS only for now; Linux support deferred
- Dependencies: document what's needed in README (bash, emacsclient, vterm, Claude Code version) — no runtime dependency checks
- First-run assumption: `doom sync` always runs before first Emacs launch — safe to assume straight.el checkout exists

#### Config completeness
- Audit the existing `use-package!` block in doom/config.el for anything missing, stale, or experimental
- Review all keybindings under SPC o C — present full list for user to adjust before shipping
- Clean up panes-related test artifacts and uncommitted files in the dotfiles repo
- Do NOT touch unrelated untracked files (amethyst.yml, etc.) — only panes-related changes

#### Verification approach
- Success bar: actually spawn an agent team, verify panes appear with headers and status indicators
- Method: automated smoke test elisp function that checks package loads, shim exists, server running, env vars correct
- Live agent interaction verified manually by user after smoke test passes
- Fix strategy: fix issues inline during verification (no gap-closure plans)
- First load behavior: silent — no welcome messages or popups on package load

### Claude's Discretion
- README structure and level of detail (personal reference, not public documentation)
- Smoke test function implementation details
- Which test artifacts to clean up vs leave alone
- Keybinding conflict detection approach

### Deferred Ideas (OUT OF SCOPE)
- Linux platform support — future work if needed
- Public package support for non-Doom Emacs users — not planned
</user_constraints>

---

## Summary

Phase 3 is an integration and deployment phase. The package code is functionally complete (Phases 1 and 2 verified), but the local dev version in `~/code/emacs-packages/claude-code-emacs-panes/` is 4 commits ahead of what GitHub has and what straight.el has checked out. The dotfiles `use-package!` block and `packages.el` declaration are already active and correctly formed. The only substantive work is: push the Phase 2 commits to GitHub, write a README, write a smoke test function, perform end-to-end verification, and commit the finalized dotfiles config state.

The current `doom/config.el` `use-package!` block is clean and complete. The keybinding namespace SPC o C is confirmed free in Doom's default evil bindings — no conflicts exist. The straight.el recipe correctly includes the `bin` directory via `:files ("claude-code-emacs-panes.el" "bin")`, and the build directory symlinks the shim correctly with executable permissions.

**Primary recommendation:** Push the 4 pending commits to GitHub first, then write README and smoke test, then run `doom sync` and verify end-to-end. The config itself needs no changes; the work is publishing and verifying.

---

## Current State Assessment

### What is already done (confirmed by static analysis)

| Item | Status | Evidence |
|------|--------|----------|
| `packages.el` declaration active | DONE | Lines 97-99, uncommented, correct recipe |
| `use-package!` block active | DONE | Lines 936-953, uncommented, calls `(claude-code-emacs-panes-setup)` |
| All 8 SPC o C keybindings bound | DONE | a, t, n, p, s, d, K, c — all mapped |
| Dashboard evil bindings | DONE | RET, D, gr, q in `claude-code-emacs-panes-dashboard-mode-map` |
| Package on GitHub | PARTIAL | Remote exists at `dakling/claude-code-emacs-panes`, but 4 commits behind local dev |
| README | MISSING | No README.md in package directory |
| Smoke test function | MISSING | Not yet written |

### What is NOT done (must be done in Phase 3)

1. Push 4 local commits to GitHub (Phase 2 features: color extraction, visual treatment, navigation, dashboard)
2. Write README.md in the package repo
3. Write `claude-code-emacs-panes-smoke-test` interactive function in the package or config
4. Run `doom sync` to pull the new commits into the straight.el checkout
5. End-to-end verification: spawn agent team, confirm panes appear with headers

### Panes-related test artifacts in dotfiles

From `git status --short` in the dotfiles repo:
- `doom/custom.el` — Emacs Custom UI generated file, NOT panes-related. Already tracked? Check git status. It's listed as `??` — untracked. This is standard Doom behavior, not a panes artifact.
- `test-edit.txt` — codex-ediff-mcp testing artifact (content says "Test edit: created by Codex on 2026-02-13"), NOT panes-related.
- `doom/bin/codex-ediff-mcp-server.py` — codex-ediff MCP server, NOT panes-related (listed as `doom/bin/` in status).
- `CLAUDE.md` — project instructions file for Claude Code, NOT panes-related.
- `amethyst.yml` — window manager config, explicitly out of scope.
- `.planning/phases/01-core-reliability/01-VERIFICATION.md` — planning artifact, should be committed as part of phase close.
- `.planning/phases/02-agent-lifecycle/02-VERIFICATION.md` — planning artifact, should be committed as part of phase close.

**Conclusion:** There are NO panes-specific test artifacts cluttering the dotfiles working tree. The untracked files are from unrelated work. The ROADMAP.md has unstaged changes (phases 1 and 2 marked as done) that should be committed.

---

## Standard Stack

### Core (already in use — no new dependencies)

| Component | Version/Details | Role | Status |
|-----------|----------------|------|--------|
| `claude-code-emacs-panes.el` | local dev, 4 commits ahead of GitHub | Main package | Needs push to GitHub |
| `bin/tmux` | bash shim | tmux interceptor | Needs push to GitHub |
| straight.el | Doom managed | Package manager | Recipe correctly configured |
| Emacs server | built-in | emacsclient target | Started by `claude-code-emacs-panes-setup` |
| vterm | Doom module | Agent terminal backend | Already in use |

### No new libraries needed

Phase 3 does not introduce new dependencies. All tools already exist.

---

## Architecture Patterns

### Pattern 1: Straight.el GitHub package with binary file

The `packages.el` recipe uses `:files ("claude-code-emacs-panes.el" "bin")` to include the `bin/` directory. This is the correct pattern for shipping a shell script alongside an Elisp package.

**How straight.el handles this:** The build directory at `~/.config/emacs/.local/straight/build-30.2/claude-code-emacs-panes/` contains:
- `claude-code-emacs-panes.el` (symlink to repos/)
- `claude-code-emacs-panes.elc` (byte-compiled)
- `bin/tmux` (symlink to repos/bin/tmux)

The shim is already executable in the repos directory (`-rwxr-xr-x`). The build dir symlinks point to the repos checkout, so permissions are preserved.

**After `doom sync` on a new machine:** straight.el clones from GitHub, builds the package, and the shim will be in the build dir. The package's `claude-code-emacs-panes--find-shim` function searches the build dir first, then the repos dir.

**Important:** On a fresh clone, straight.el does NOT automatically make scripts executable if they lack the execute bit in the git repo. The current shim is committed with execute permissions (`-rwxr-xr-x`), but this must be verified after pushing. The package's `validate-and-fix-shim` auto-fixes permissions via `(set-file-modes found #o755)` as a safety net regardless.

### Pattern 2: Doom `use-package!` with lazy loading via `:after-call`

```elisp
;; Source: doom/config.el lines 936-953 (current working config)
(use-package! claude-code-emacs-panes
  :after-call (claude-code-ide claude-code-ide-menu)
  :config
  (claude-code-emacs-panes-setup)
  (map! :leader :prefix ("o C" . "claude panes")
        :desc "Show all panes" "a" #'claude-code-emacs-panes-show-all
        ...))
```

`:after-call` defers loading until `claude-code-ide` or `claude-code-ide-menu` is called. This is correct — the package must be loaded before the first `claude-code-ide` session starts, and `:after-call` triggers on function call rather than package load, which is the right granularity here.

### Pattern 3: Smoke test function structure

The smoke test should be an interactive `defun` that checks prerequisites and reports pass/fail. Recommended implementation in the package itself (so it's available after doom sync without editing config):

```elisp
;; To be added to claude-code-emacs-panes.el
(defun claude-code-emacs-panes-smoke-test ()
  "Run a smoke test to verify the panes integration is correctly set up.
Checks: package loaded, shim exists and is executable, Emacs server running,
env vars injectable, advice installed.
Reports results to *Messages*."
  (interactive)
  (let ((pass 0) (fail 0))
    (cl-flet ((check (label condition)
                (if condition
                    (progn (cl-incf pass)
                           (message "  PASS: %s" label))
                  (progn (cl-incf fail)
                         (message "  FAIL: %s" label)))))
      (message "claude-code-emacs-panes smoke test:")
      ;; 1. Package is loaded
      (check "package loaded" (featurep 'claude-code-emacs-panes))
      ;; 2. Shim file exists
      (check "shim exists" (claude-code-emacs-panes--find-shim))
      ;; 3. Shim is executable
      (let ((shim (claude-code-emacs-panes--find-shim)))
        (check "shim executable" (and shim (file-executable-p shim))))
      ;; 4. Emacs server is running
      (check "emacs server running" (server-running-p))
      ;; 5. Advice installed on claude-code-ide--start-session
      (check "env-inject advice installed"
             (advice-member-p #'claude-code-emacs-panes--inject-env
                              'claude-code-ide--start-session))
      ;; 6. cli-extra-flags includes --teammate-mode tmux
      (check "--teammate-mode tmux in cli flags"
             (and (boundp 'claude-code-ide-cli-extra-flags)
                  (stringp claude-code-ide-cli-extra-flags)
                  (string-match-p "--teammate-mode" claude-code-ide-cli-extra-flags)))
      ;; Summary
      (message "Smoke test: %d passed, %d failed" pass fail)
      (= fail 0))))
```

**Where to put it:** In the package's `claude-code-emacs-panes.el`, before `(provide ...)`. This makes it available on any machine after `doom sync`.

**Bind it:** Add to the `use-package!` block in `doom/config.el`:
```elisp
:desc "Run smoke test" "T" #'claude-code-emacs-panes-smoke-test
```
(SPC o C T — capital T, not yet used.)

### Pattern 4: README for personal reference

Personal-reference README (not public docs). The key sections a future-self needs on a new machine:

```markdown
# claude-code-emacs-panes

Manages vterm buffers as panes for Claude Code subagents in Emacs.

## Requirements
- Emacs 28.1+
- vterm package
- emacsclient (comes with Emacs)
- Claude Code v2.1.47+ (--teammate-mode tmux flag required)
- bash (for the tmux shim)

## Setup (Doom Emacs)
Add to packages.el:
(package! claude-code-emacs-panes
  :recipe (:host github :repo "dakling/claude-code-emacs-panes"
           :files ("claude-code-emacs-panes.el" "bin")))

Add to config.el:
[the use-package! block]

Run: doom sync

## Keybindings (SPC o C prefix)
[table of keybindings]

## Verification
M-x claude-code-emacs-panes-smoke-test

## How it works
[brief description of shim interception]
```

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Byte-compile on doom sync | custom build script | straight.el + doom sync | Already works: `.elc` file exists in build dir |
| Shim permission check | custom installer | `validate-and-fix-shim` already in package | Already implemented with latch pattern |
| Package availability on new machine | manual copy | straight.el GitHub recipe | Already configured in packages.el |
| Keybinding conflict detection | manual audit | Static analysis of Doom's `+evil-bindings.el` | SPC o C is confirmed free (see below) |

**Key insight:** The package infrastructure is already fully wired. Phase 3 is publish-and-verify, not build.

---

## Common Pitfalls

### Pitfall 1: Pushing without verifying shim execute bit on GitHub

**What goes wrong:** Git tracks the execute bit. If the shim was committed without `+x`, `git clone` on a new machine gives a non-executable script. The `validate-and-fix-shim` function catches this (calls `set-file-modes`), but only at runtime, not at `doom sync` time. A user who runs `doom sync` but never triggers the lazy load won't see an error until first use.

**Why it happens:** The shim is currently executable (`-rwxr-xr-x`) in the repos dir. This should be preserved through git push/pull since the execute bit is tracked. But worth verifying after push with `git ls-tree HEAD bin/tmux`.

**How to avoid:** After pushing, run `git ls-tree HEAD bin/tmux` in the package repo. The mode should be `100755` (not `100644`).

**Warning signs:** On a fresh machine, `(claude-code-emacs-panes-smoke-test)` reports "FAIL: shim executable" despite "PASS: shim exists" — means the shim exists but lacks +x.

### Pitfall 2: Straight.el repos is stale after push

**What goes wrong:** After pushing Phase 2 commits to GitHub, the local straight.el repos checkout at `~/.config/emacs/.local/straight/repos/claude-code-emacs-panes/` is still at the old commit (confirmed: currently at `6ed9e86`, missing 4 commits). Running `doom sync` alone does NOT update the checkout unless straight.el is told to do so.

**Why it happens:** Straight.el pins packages. The local repos may be at a specific commit until `doom/upgrade` or explicit `straight-pull-package` is run.

**How to avoid:** After pushing, run `M-x straight-pull-package RET claude-code-emacs-panes RET` in Emacs, then `doom sync`, then reload. Or manually: `git -C ~/.config/emacs/.local/straight/repos/claude-code-emacs-panes pull` then `doom sync`.

**Warning signs:** `(featurep 'claude-code-emacs-panes)` is true but smoke test shows old behavior (missing dashboard auto-refresh, no `close-finished` command). Check `git log` in the repos dir.

### Pitfall 3: Server not running at config load time

**What goes wrong:** `(claude-code-emacs-panes-setup)` calls `(server-start)` if server isn't running. With `:after-call (claude-code-ide ...)` lazy loading, setup runs the first time `claude-code-ide` or `claude-code-ide-menu` is invoked. This is fine — server starts before the first session. But if the user somehow triggers the advice path before calling setup (impossible with current config), the server won't be running.

**How to avoid:** Current config structure is correct. `:config` block runs at load time which is after-call triggers, so `(claude-code-emacs-panes-setup)` always runs before any session is started.

**Warning signs:** Smoke test reports "FAIL: emacs server running" — means the package was loaded without `setup` being called first.

### Pitfall 4: `claude-code-ide-cli-extra-flags` type mismatch

**What goes wrong:** `claude-code-emacs-panes-setup` appends to `claude-code-ide-cli-extra-flags` with `string-trim (concat existing " --teammate-mode tmux")`. If `claude-code-ide-cli-extra-flags` is nil (unbound or nil), `(or nil "")` gives `""`, which is correct. But if it's a list (some versions might use a list), this breaks silently.

**Current state:** The package checks `(boundp 'claude-code-ide-cli-extra-flags)` and treats it as a string. Confirmed to work in current claude-code-ide version.

**How to avoid:** Smoke test checks for `(string-match-p "--teammate-mode" claude-code-ide-cli-extra-flags)` which covers this.

### Pitfall 5: Keybinding SPC o C conflicts

**What goes wrong:** Some other package or Doom module claims SPC o C.

**Research finding (HIGH confidence):** Doom's `+evil-bindings.el` SPC o namespace (lines 683-730) uses: A, a, b, d, f, F, r, R, -, p, P, /, t, T, e, E, o, O, s, D. Capital C is not claimed by Doom defaults. The existing config's `use-package!` block already has SPC o C active.

**How to avoid:** Already verified. No action needed.

---

## Code Examples

### Smoke test function (to add to claude-code-emacs-panes.el)

```elisp
;; Source: Phase 3 research — recommended implementation
;; Place before (provide 'claude-code-emacs-panes) in the package file

(defun claude-code-emacs-panes-smoke-test ()
  "Verify the panes integration is correctly set up.
Checks: package loaded, shim exists and is executable, Emacs server running,
advice installed on claude-code-ide--start-session, --teammate-mode configured.
Reports pass/fail to *Messages*.  Returns t if all checks pass."
  (interactive)
  (let ((pass 0) (fail 0))
    (cl-flet ((check (label condition)
                (if condition
                    (progn (cl-incf pass)
                           (message "  PASS: %s" label))
                  (progn (cl-incf fail)
                         (message "  FAIL: %s" label)))))
      (message "=== claude-code-emacs-panes smoke test ===")
      (check "package loaded" (featurep 'claude-code-emacs-panes))
      (let ((shim (claude-code-emacs-panes--find-shim)))
        (check "shim found" (not (null shim)))
        (check "shim executable" (and shim (file-executable-p shim)))
        (when shim
          (message "    shim path: %s" shim)))
      (check "emacs server running" (server-running-p))
      (check "env-inject advice active"
             (advice-member-p #'claude-code-emacs-panes--inject-env
                              'claude-code-ide--start-session))
      (check "--teammate-mode in cli flags"
             (and (boundp 'claude-code-ide-cli-extra-flags)
                  (stringp claude-code-ide-cli-extra-flags)
                  (string-match-p "--teammate-mode tmux"
                                  claude-code-ide-cli-extra-flags)))
      (message "=== Result: %d passed, %d failed ===" pass fail))
    (= fail 0)))
```

### Git shim execute bit verification

```bash
# Run after pushing to GitHub to confirm shim is committed with +x
git -C ~/code/emacs-packages/claude-code-emacs-panes ls-tree HEAD bin/tmux
# Expected output (mode 100755 = executable):
# 100755 blob <hash>    bin/tmux
```

### Update straight.el checkout after GitHub push

```elisp
;; In Emacs, to pull the new commits into straight.el repos dir:
;; M-x straight-pull-package RET claude-code-emacs-panes RET
;; Then: doom sync (in terminal)
;; Then: M-x doom/reload (in Emacs)
```

### README.md structure for the package

Key things to document for future-self on new machine:
1. Dependencies (Emacs 28.1+, vterm, emacsclient, Claude Code v2.1.47+, bash)
2. Doom installation snippet (packages.el + config.el use-package block)
3. Post-install step: `doom sync`
4. Verification command: `M-x claude-code-emacs-panes-smoke-test`
5. Keybindings table (SPC o C prefix)
6. Brief "how it works" — tmux shim intercepts Claude Code's pane management

---

## Current Keybindings Audit

Complete list of keybindings under SPC o C (verified from doom/config.el):

| Key | Command | Description |
|-----|---------|-------------|
| SPC o C a | `claude-code-emacs-panes-show-all` | Show all panes |
| SPC o C t | `claude-code-emacs-panes-toggle-all` | Toggle pane layout |
| SPC o C n | `claude-code-emacs-panes-next` | Next pane |
| SPC o C p | `claude-code-emacs-panes-prev` | Previous pane |
| SPC o C s | `claude-code-emacs-panes-select` | Select pane (completing-read) |
| SPC o C d | `claude-code-emacs-panes-dashboard` | Dashboard |
| SPC o C K | `claude-code-emacs-panes-close-finished` | Close finished panes |
| SPC o C c | `claude-code-emacs-panes-start-claude` | Start Claude with panes |
| (proposed) SPC o C T | `claude-code-emacs-panes-smoke-test` | Run smoke test |

Dashboard buffer keybindings (in `claude-code-emacs-panes-dashboard-mode-map`):

| Key | Command |
|-----|---------|
| RET (normal state) | `claude-code-emacs-panes-dashboard-open` |
| D (normal state) | `claude-code-emacs-panes-close-finished` |
| gr (normal state) | `claude-code-emacs-panes-dashboard` (refresh) |
| q (normal state) | `quit-window` |

**SPC o C conflict analysis:** Doom default evil bindings use these letters under SPC o: A, a, b, d, f, F, r, R, -, p, P, /, t, T, e, E, o, O, s, D. None conflict with the panes namespace at C. Confirmed HIGH confidence via direct inspection of `~/.config/emacs/modules/config/default/+evil-bindings.el`.

---

## Deployment Checklist (for Planner)

In order:

1. **Push Phase 2 commits to GitHub** — `git push` in the package repo. 4 commits pending.
2. **Verify shim execute bit** — `git ls-tree HEAD bin/tmux` must show `100755`.
3. **Write README.md** — in the package repo. Personal reference format.
4. **Add smoke test function** — to `claude-code-emacs-panes.el` before `(provide ...)`.
5. **Add smoke test keybinding** — `SPC o C T` in the `use-package!` block in `doom/config.el`.
6. **Push again** — commit README + smoke test function.
7. **Update straight.el checkout** — `M-x straight-pull-package` then `doom sync`.
8. **Run smoke test** — `M-x claude-code-emacs-panes-smoke-test`. Fix any failures inline.
9. **Live agent verification** — spawn actual agent team, verify panes appear with headers and color indicators.
10. **Commit dotfiles** — commit `doom/config.el` changes and the ROADMAP.md update.

---

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| Package commented out in packages.el | Active declaration with `:files ("claude-code-emacs-panes.el" "bin")` | Phase 1 | Package installs via doom sync |
| No use-package! block | Active use-package! block with 8 keybindings | Phase 1 | Package configures on load |
| No Phase 2 features on GitHub | 4 commits pending push (color, visual treatment, navigation, dashboard) | Phase 3 (now) | New machine gets full feature set |
| No README | Needs to be created | Phase 3 (now) | New machine setup is documented |
| No smoke test | Needs to be created | Phase 3 (now) | Fast verification on new machine |

---

## Open Questions

1. **Smoke test location: package or config.el?**
   - What we know: Adding it to the package makes it available on any machine after doom sync without touching dotfiles config. Adding it to config.el keeps the package minimal.
   - Recommendation: Add to the package (`claude-code-emacs-panes.el`). It's intrinsic to the package's operational correctness and should be part of the published artifact. The keybinding stays in `doom/config.el`.

2. **straight.el update strategy for existing machine**
   - What we know: After pushing to GitHub, the straight.el repos checkout is 4 commits behind. `doom sync` alone won't pull these (straight pins). Explicit pull needed.
   - Recommendation: Use `M-x straight-pull-package RET claude-code-emacs-panes RET`, then `doom sync`, then `M-x doom/reload`. Document this in the planner's verification step.

3. **ROADMAP.md and VERIFICATION.md — should these be committed as part of Phase 3?**
   - What we know: `ROADMAP.md` has unstaged changes (phases 1 and 2 marked done). `01-VERIFICATION.md` and `02-VERIFICATION.md` are untracked.
   - Recommendation: Commit these planning artifacts as part of Phase 3's "record completion" step. They are not "test artifacts" to clean up — they are the project's planning record.

---

## Sources

### Primary (HIGH confidence)
- Direct inspection of `/Users/darioklingenberg/code/emacs-packages/claude-code-emacs-panes/claude-code-emacs-panes.el` — full package code
- Direct inspection of `/Users/darioklingenberg/code/emacs-packages/claude-code-emacs-panes/bin/tmux` — shim script
- Direct inspection of `/Users/darioklingenberg/.dotfiles/doom/config.el` — use-package! block lines 936-953
- Direct inspection of `/Users/darioklingenberg/.dotfiles/doom/packages.el` — package declaration lines 97-99
- `~/.config/emacs/modules/config/default/+evil-bindings.el` lines 683-730 — SPC o namespace audit
- `git log --oneline` in both package repos — commit state comparison
- `ls -la` of straight.el build and repos directories — file/permission state

### Secondary (MEDIUM confidence)
- straight.el `:files` behavior — observed from build directory structure (symlinks confirm bin/ is included)
- Git execute bit preservation — standard git behavior, confirmed by current repos state

---

## Metadata

**Confidence breakdown:**
- Current state (what's done vs not done): HIGH — direct inspection of all files
- Keybinding conflict analysis: HIGH — direct inspection of Doom evil bindings file
- Straight.el behavior on doom sync: HIGH — observed from build directory structure
- Smoke test implementation: HIGH — elisp patterns are stable Emacs core APIs
- Git execute bit behavior: MEDIUM — standard behavior, not re-verified via push test

**Research date:** 2026-02-24
**Valid until:** 2026-03-24 (stable domain — Doom/straight.el/Emacs APIs change slowly)
