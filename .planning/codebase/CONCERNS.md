# Codebase Concerns

**Analysis Date:** 2026-02-19

## Tech Debt

### config.el Size and Complexity

**Issue:** `doom/config.el` is 1,373 lines - monolithic configuration with mixed concerns (UI, keybindings, language-specific settings, AI integrations, window management)

Files: `doom/config.el`

Impact: Difficult to maintain, locate specific functionality, and reload changes. Single point of failure for Doom Emacs configuration. Risk of conflicts between different subsystems.

Fix approach: Break into modular files in a `config/` subdirectory by concern (ai-config.el, keymaps.el, language-modes.el, platform-specific.el). Load them from config.el using `load!`. This preserves single entry point while improving organization.

### Commented-Out Code

**Issue:** Multiple blocks of commented code throughout codebase indicate incomplete refactoring or disabled features

Files:
- `doom/config.el` (lines 214, 357, 571, 847, 845, 937-947)
- `stumpwm.lisp` (lines 194-195, 265, 307-313)
- `INSTALL` (lines 8, 10, 13-14, 22-23)
- `qtile.py` (commented functions for Emacs window navigation)

Impact: Clutters codebase, confuses maintainers about intent. Dead code path risk during modifications.

Fix approach: Delete or use git history for reference. If experimental, document why with date and link to issue. Use feature flags or environment variables for conditional enablement rather than comments.

### Shell Script Complexity in volume.sh

**Issue:** `binCustom/volume.sh` has deeply nested if-statements (5 levels) and mixed concerns - notification logic interleaved with volume control

Files: `binCustom/volume.sh` (lines 22-40)

Impact: Difficult to modify notification behavior independently from volume logic. Bug in one path affects others.

Fix approach: Extract notification logic into separate function. Use case statement for icon selection. Simplify nesting by returning early.

## Known Issues

### INSTALL Script Outdated

**Issue:** `INSTALL` script references `nvim/init.vim` but Neovim config is `nvim/init.lua` (LazyVim format). Some symlinks are commented out but unclear why.

Files: `INSTALL` (line 5)

Symptoms: Running INSTALL may create broken symlinks for Neovim

Trigger: Executing `./INSTALL` when `nvim/init.vim` doesn't exist

Workaround: Manual symlink creation or update to correct path

Fix approach: Update line 5 to `ln -sf $(pwd)/nvim/init.lua ~/.config/nvim/init.lua` and document why certain symlinks are commented (if intentional).

### Platform-Specific Code Not Fully Gated

**Issue:** Some platform-specific code in `doom/config.el` uses system predicates but not consistently. Linux-only commands may execute on macOS without guards.

Files: `doom/config.el` (lines 340-373, 475)

Symptoms: Functions like `my/brightness+`, `my/brightness-`, `my/fix-touchscreen` are defined only when `(eq system-type 'gnu/linux)` but called functions may be invoked regardless

Fix approach: Ensure all Linux-only function definitions wrap both definition AND any keybindings that call them. Use `:when` conditions in `map!` macros.

## Security Considerations

### Environment Variable Leakage in MCP Server

**Issue:** `doom/bin/codex-ediff-mcp-server.py` reads `EMACSCLIENT`, `EMACS_SERVER_FILE`, `EMACS_SOCKET_NAME` environment variables without validation

Files: `doom/bin/codex-ediff-mcp-server.py` (lines 99-107)

Risk: If environment variables are set to malicious values (e.g., socket pointing to attacker-controlled server), subprocess calls could interact with wrong Emacs instance or allow privilege escalation

Current mitigation: Only reads from environment, doesn't parse untrusted input. But no validation of paths.

Recommendations:
- Validate that socket names and server files are in safe locations (e.g., `/tmp` or home directory)
- Use absolute path verification before passing to `emacsclient`
- Document that server file/socket name must be trusted

### Subprocess Command Injection Risk

**Issue:** Multiple locations execute shell commands with potential for command injection

Files:
- `doom/config.el` (line 408: `async-shell-command` with string concatenation)
- `stumpwm.lisp` (lines 51-53, 61-64, 67-73: `format nil` building shell commands)
- `doom/bin/codex-ediff-mcp-server.py` (line 110: subprocess with shell commands)

Risk: If user input (file paths, buffer names, etc.) is used in command construction without quoting, attackers could inject shell metacharacters

Current mitigation: Some uses have `shell-quote-argument` (`doom/config.el` line 706) but not universally applied

Recommendations:
- Use `shell-quote-argument` consistently in `doom/config.el`
- In `stumpwm.lisp`, validate/quote all command arguments before building format strings
- In Python, never use `shell=True`; pass commands as argument lists (already done: line 110 uses list form)

### Credentials/Secrets in Configuration

**Issue:** Email credentials and OAuth tokens are set directly in config files

Files: `doom/config.el` (lines 481-512)

Risk: If dotfiles repo is ever made public or shared, mail server credentials would be exposed

Current mitigation: Files are local-only (not in git), but dotfiles live in `~/.dotfiles` which is accessible to any user with shell access

Recommendations:
- Move mail credentials to `.env` file or password manager
- Load credentials via `gpg` decryption or secret manager integration
- Use `mu4e` context system but fetch credentials from external source rather than hardcoding

## Performance Bottlenecks

### MCP Server Message Parsing

**Issue:** `doom/bin/codex-ediff-mcp-server.py` `_decode_emacs_json()` function uses fallback exception handling with bare `except Exception:` (line 127)

Files: `doom/bin/codex-ediff-mcp-server.py` (lines 123-139)

Problem: Catches all exceptions (AttributeError, TypeError, etc.) making it difficult to debug parsing failures. If Emacs returns unexpected format, error is silently swallowed.

Improvement path:
- Catch specific exceptions (json.JSONDecodeError, ValueError)
- Log failures with full context
- Return error response with details rather than attempting fallback parsing

### Emacs eval-in-subprocess Bridge Latency

**Issue:** Every diff operation in `_call_open_diff()` spawns subprocess for emacsclient call (line 174)

Files: `doom/bin/codex-ediff-mcp-server.py` (line 110, 174)

Problem: Subprocess creation overhead + IPC roundtrip adds 100-500ms per diff operation. Significant when reviewing many hunks.

Improvement path:
- Batch multiple diff requests in single subprocess call
- Cache emacsclient socket connection if possible
- Use direct Emacs module API if available instead of subprocess

## Fragile Areas

### Ediff Integration with Claude Code

Files: `doom/config.el` (lines 921-934), `doom/bin/codex-ediff-mcp-server.py`

Why fragile:
- Relies on undocumented Emacs function `my/codex-ediff-review-from-file` (called but not defined in config.el)
- Python server assumes Emacs bridge responds with specific JSON format - no schema validation
- Hardcoded file suffixes (`.codex-ediff`) could conflict
- Temporary file cleanup in finally block could leak files if temp directory permissions change

Safe modification:
- Define bridge contract in comments with JSON schema examples
- Add integration tests that mock Emacs responses
- Add logging to trace ediff operations
- Use UUID-based temp filenames instead of fixed suffix

Test coverage gaps:
- No tests for ediff MCP bridge - error cases untested
- Python server error handling not tested (lines 176-180, 214-218)

### Qt/stumpwm Window Manager Integration

Files: `doom/config.el` (lines 410-437, 439-441), `stumpwm.lisp` (lines 118-143)

Why fragile:
- Functions like `qtile/move-focus()` use `condition-case` to silently fall back to qtile commands if Emacs window movement fails
- Emacs and window manager state can get out of sync - condition-case masks real errors
- No feedback if window manager command fails

Safe modification:
- Log failed condition-case catches
- Test with multiple window manager configurations
- Add explicit state verification rather than silent fallback

### Email Configuration (mu4e)

Files: `doom/config.el` (lines 477-561)

Why fragile:
- Hardcoded context policies always ask (line 478) - no auto-detection
- Multiple defalias overwrites (lines 527-528) suggest API compatibility issues with mu4e versions
- Regular expression for email subjects is complex (lines 530-546) - prone to bugs with international characters
- icalendar setup (lines 556-561) assumes specific org file paths

Safe modification:
- Extract email accounts to external config
- Version-gate mu4e API calls
- Test email regex against international examples
- Document mu4e version requirements

## Scaling Limits

### Emacs Config Reload Performance

**Issue:** `doom sync` required after `init.el` or `packages.el` changes, but no mechanism to detect if changes require full sync vs partial reload

Files: `doom/init.el`, `doom/packages.el`, `doom/config.el` (lines 1374)

Current capacity: Config.el can reload in-place, but package changes require full rebuild

Limit: As package list grows (currently 50+), sync time increases. First sync on clean install takes 5+ minutes.

Scaling path:
- Use Doom's incremental package detection if available
- Cache package graph to avoid full recompilation
- Document which changes require sync vs reload

### Org Mode Performance with Multiple Capture Templates

**Issue:** `doom/config.el` defines 9 capture templates (lines 260-279) with multiple file targets

Files: `doom/config.el` (lines 252-279)

Current capacity: Works fine with typical usage (1-2 captures/day)

Limit: If files grow to 10K+ lines, capture template loading and agenda queries may slow

Scaling path:
- Use org-capture-templates-contexts to activate templates conditionally
- Archive old captures to separate files
- Use org-roam for more scalable note organization

## Dependencies at Risk

### evil-collection Pinning

**Issue:** `doom/packages.el` unpins evil-collection (line 54) with commented-out branch specification, suggesting compatibility issues

Files: `doom/packages.el` (lines 54-56)

Risk: Unpinned packages can break on new releases. Commented code suggests previous solution was abandoned.

Impact: If evil-collection updates and breaks mu4e integration, config breaks

Migration plan:
- Either re-pin to known working version with clear reason in comment
- Or test against latest and document known issues
- Set up CI to test config against evil-collection updates

### haskell-mode Pinning

**Issue:** `doom/packages.el` unpins haskell-mode (line 153) without explanation

Files: `doom/packages.el` (line 153)

Risk: Same as evil-collection - unpredictable updates

Migration plan: Add comment explaining why unpin is necessary, or re-pin with version

### sly Pinning

**Issue:** `doom/packages.el` unpins sly (line 125) - Common Lisp IDE

Files: `doom/packages.el` (line 125)

Risk: Unpinned dependency. Custom `inferior-lisp-program` paths in config.el (lines 224-226) are system-specific and may not work with updated sly

Migration plan: Version-gate sly version or document minimum version requirements

## Missing Critical Features

### No Error Recovery for Mail Sync

**Issue:** `mu4e` is configured but no retry logic or error notification for mail sync failures

Files: `doom/config.el` (lines 550-551)

Problem: Mail sync runs every 120 seconds. If network fails, user may not notice for hours. No fallback alert mechanism beyond mu4e's default.

Blocks: Reliable email notification system

### No Backup/Snapshot System for Config Changes

**Issue:** No mechanism to rollback config changes if `doom sync` breaks something

Files: All config files - no version isolation

Problem: Breaking change in packages.el or init.el requires manual git revert + re-sync

Blocks: Safe experimentation with Doom config

### Incomplete Window Manager Support

**Issue:** qtile/stumpwm window movement code has fallbacks but Neovim has different keybindings

Files: `doom/config.el` (keybindings using super-key), `nvim/lua/config/keymaps.lua`, `stumpwm.lisp`, `qtile.py`

Problem: Inconsistent window navigation across editors and window managers. No shared abstraction.

Blocks: Seamless multi-editor/multi-platform workflow

## Test Coverage Gaps

### No Tests for Doom Configuration

**Issue:** No test suite for config.el, init.el, packages.el

Files: All `doom/` files

What's not tested:
- Keybinding conflicts (super-key overrides, local-leader overrides)
- Package load order and initialization
- Platform-specific code paths (Linux vs macOS)
- Mail account configuration (mu4e)
- Language-specific mode configuration

Risk: Configuration regressions go unnoticed until runtime

Priority: High - breaking changes in dependencies are detected only when Emacs starts

### No Tests for MCP Bridge

**Issue:** Python MCP server has no test coverage

Files: `doom/bin/codex-ediff-mcp-server.py`

What's not tested:
- Message parsing (valid and invalid JSON)
- emacsclient communication failures
- Malformed Emacs responses
- Temporary file cleanup on error
- Edge cases in elisp quoting

Risk: Silent failures in diff bridge make debugging difficult

Priority: High - MCP bridge is critical for Claude Code integration

### No Tests for Shell Scripts

**Issue:** Shell scripts in `binCustom/` are untested

Files: All `.sh` files

What's not tested:
- amixer availability/failure handling
- Icon file paths (hardcoded, may not exist)
- notify-send.sh wrapper existence and arguments

Risk: Silent failures when called (notifications just don't appear)

Priority: Medium - user-visible but not critical

### No Integration Tests for Cross-Editor Workflows

**Issue:** No tests verifying consistent behavior across Emacs and Neovim

Files: `doom/config.el`, `nvim/lua/`, `stumpwm.lisp`

What's not tested:
- Window navigation commands work in both editors
- File opening paths are consistent
- AI integration (Claude Code) works the same way

Risk: Subtle differences in editor behavior compound across complex workflows

Priority: Medium

---

*Concerns audit: 2026-02-19*
