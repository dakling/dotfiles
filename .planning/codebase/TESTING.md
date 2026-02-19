# Testing Patterns

**Analysis Date:** 2026-02-19

## Test Framework

**Status:** No automated test framework detected.

This codebase is configuration-driven (dotfiles). Testing is implicit through:
- Manual Emacs/Neovim configuration reload and testing (`M-x doom/reload` in Emacs)
- Python MCP server tested via protocol validation (JSON-RPC frames)
- Shell scripts tested via manual execution (e.g., `./volume.sh up`)

**No runners configured:**
- No Jest, Vitest, pytest, or other test runners found
- No test configuration files (jest.config.*, vitest.config.*, pytest.ini, setup.cfg, pyproject.toml)
- No `__tests__/`, `tests/`, `spec/` directories

**Manual testing approach:**
- Emacs: Changes require `doom sync` (to reload packages) and `M-x doom/reload` (to reload config)
- Neovim: Changes to `nvim/` require Neovim restart
- Python MCP server: Tested by sending JSON-RPC messages and verifying Emacs integration
- Shell scripts: Tested by direct invocation with arguments

## Test File Organization

**Location:** Not applicable — no test files found in codebase.

Example file structure would be:
```
doom/
├── config.el          (main configuration)
├── init.el            (module activation)
├── packages.el        (package declarations)
└── bin/               (support scripts; codex-ediff-mcp-server.py)

nvim/
├── init.lua           (entry point)
├── lua/
│   ├── config/        (options, keymaps, autocmds)
│   └── plugins/       (plugin configurations)

binCustom/
├── volume.sh          (tested by manual invocation)
├── brightness_*.sh    (tested by manual invocation)
└── *.py               (tested by manual invocation)
```

**Testing convention if added:**
- Elisp unit tests should follow ERT (Emacs Regression Testing) pattern: `(ert-deftest my/test-function () ...)`
- Python scripts should use pytest: `test_codex_ediff_mcp_server.py` in same directory
- Lua tests (if needed) could use busted: `spec/` directory with `*_spec.lua`

## Validation and Integration Testing

**Implicit validation:**

**Emacs configuration:**
- Byte-compilation can be run: `emacs --batch -f batch-byte-compile ~/.config/doom/config.el`
- Doom provides `doom doctor` to validate environment
- `doom sync` validates package recipes against GitHub

**Python MCP server (`doom/bin/codex-ediff-mcp-server.py`):**
- Validates JSON-RPC protocol compliance via `_jsonrpc_result()` and `_jsonrpc_error()` helpers
- Type validation in `_call_open_diff()`: checks `old_file_path` and `new_file_contents` types before use
- Subprocess error handling: validates `process.returncode` and emacsclient availability

**Lua configuration:**
- Lazy.nvim validates plugin specs at startup
- `vim.g.neovide` detection prevents errors if running without GUI
- File operation guards: `vim.fn.fnamemodify()` with path checks

## Error Handling Patterns

**Pattern 1: Condition-case (Elisp)**
```elisp
(condition-case nil
    (evil-window-right 1)
  (error (qtile/move-focus "right")))
```
Maps window motion errors to Qtile fallback.

**Pattern 2: Ignore-errors (Elisp)**
```elisp
(ignore-errors
  (shell-command "xinput --map-to-output $(xinput list ...) eDP-1"))
```
For non-critical operations where failure is acceptable.

**Pattern 3: Type validation (Python)**
```python
if not isinstance(old_file_path, str) or not old_file_path:
    return {
        "isError": True,
        "content": [{"type": "text", "text": "openDiff: old_file_path is required"}],
    }
```
Returns MCP error response with human-readable message.

**Pattern 4: Subprocess error checking (Python)**
```python
process = subprocess.run(command, capture_output=True, text=True, check=False)
if process.returncode != 0:
    stderr = process.stderr.strip()
    raise RuntimeError(stderr or stdout or "emacsclient failed")
```
Captures stderr; raises on failure with context.

**Pattern 5: Lua guard clauses**
```lua
if not term_buf then return "" end
if not bufname or bufname == "" then
  local dir = vim.fn.getcwd()
end
```
Early return with empty/nil; conditional chains.

## Mocking and Test Doubles

**Not applicable:** No test framework means no mocking library integration.

**If tests were added:**

**Elisp:**
- Use `flet` or `cl-letf` to mock functions: `(cl-letf ((helm-find-files (lambda () "test-value"))) ...)`
- Mock subprocess calls with `spy` functionality via `with-mock-processes`

**Python:**
- Use `unittest.mock` for subprocess mocking:
  ```python
  from unittest.mock import patch, MagicMock

  @patch('subprocess.run')
  def test_emacs_eval(mock_run):
      mock_run.return_value = MagicMock(returncode=0, stdout="result")
      result = _run_emacs_eval("(+ 1 2)")
      assert result == "result"
  ```
- Mock `sys.stdin.buffer.readline()` for JSON-RPC message reading

**Lua:**
- Mock `vim.api` functions for unit tests (would require test harness)

## Fixtures and Test Data

**Not implemented in codebase.**

**Convention if added:**

**Elisp fixtures:**
```elisp
(defvar test-buffer-contents
  "test data for buffer operations")

(defun setup-test-buffer ()
  (with-current-buffer (get-buffer-create "*test*")
    (insert test-buffer-contents)))
```

**Python test data:**
```python
TEST_DIFF_RESPONSE = {
    "old_file_path": "/tmp/test.py",
    "new_file_contents": "def main():\n    pass",
    "tab_name": "test_diff"
}

@pytest.fixture
def mock_ediff_env(monkeypatch):
    monkeypatch.setenv("EMACS_SOCKET_NAME", "/tmp/emacs.socket")
```

## Coverage

**Requirements:** None enforced.

**Gap analysis:**
- Python MCP server has no test coverage; all functionality is protocol-dependent
- Elisp configuration is untested; relies on manual validation at runtime
- Shell scripts lack test coverage; tested only via manual invocation
- Lua/Neovim config has implicit validation via lazy.nvim but no explicit tests

**Critical untested areas:**
- `doom/bin/codex-ediff-mcp-server.py`: Edge cases in `_read_message()` (framed vs. line-delimited), JSON parsing error recovery
- `qtile.py`: Async hook callbacks (`@lazy.function`) — no way to test without running Qtile
- `doom/config.el`: Platform-specific code (`when (eq system-type 'gnu/linux)`) — untested on macOS
- Shell scripts: All volume/brightness operations rely on external tools (amixer, notify-send) — fragile to missing dependencies

## Development Workflow

**Manual testing approach:**

**For Emacs changes:**
1. Edit `~/.dotfiles/doom/config.el` (or `packages.el`, `init.el`)
2. Run `doom sync` if package declarations changed
3. Run `M-x doom/reload` in running Emacs instance OR restart Emacs
4. Manually test functionality (keybindings, modes, features)

**For Neovim changes:**
1. Edit `~/.dotfiles/nvim/` files
2. Restart Neovim
3. Manually test (plugins load, keybindings work, etc.)

**For Python MCP server changes:**
1. Edit `~/.dotfiles/doom/bin/codex-ediff-mcp-server.py`
2. Restart Claude Code integration in Emacs
3. Send diffs from Claude Code; verify Emacs ediff opens correctly

**For shell scripts:**
1. Edit `~/.dotfiles/binCustom/*.sh`
2. Run directly: `./volume.sh up` or `./brightness_up.sh`
3. Verify output (desktop notifications, audio level, screen brightness)

## Testing Recommendations

**If tests were to be added, prioritize:**

1. **Python MCP server (highest risk):** Protocol-critical; if broken, Claude Code integration fails
   - Test `_read_message()` with both framed and line-delimited JSON
   - Test `_call_open_diff()` with invalid/missing parameters
   - Test `_run_emacs_eval()` with emacsclient errors

2. **Platform-specific Elisp (medium risk):** Linux vs. macOS code diverges
   - Test `system-name=` predicate on actual systems
   - Test Linux-only shutdown/brightness functions with mocked subprocess

3. **Shell scripts (medium risk):** External dependencies fragile
   - Test volume scripts with mocked amixer output
   - Test brightness scripts with mocked xbacklight

4. **Lua configuration (low risk):** Lazy.nvim validates plugin specs; errors caught at startup

---

*Testing analysis: 2026-02-19*
