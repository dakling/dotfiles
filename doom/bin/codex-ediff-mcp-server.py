#!/usr/bin/env python3
"""Minimal MCP server that forwards Codex diff reviews to Emacs Ediff.

This bridge exposes two MCP tools:
  - openDiff(old_file_path, new_file_contents, tab_name)
  - closeAllDiffTabs()

The tools call into the running Emacs instance through emacsclient.
"""

from __future__ import annotations

import ast
import json
import os
import subprocess
import sys
import tempfile
from typing import Any, Dict, Optional, Tuple


PROTOCOL_VERSION = "2024-11-05"
SERVER_NAME = "emacs-ediff-bridge"
SERVER_VERSION = "0.1.0"


def _elisp_quote(text: str) -> str:
    escaped = text.replace("\\", "\\\\").replace('"', '\\"')
    return f'"{escaped}"'


def _read_message() -> Optional[Tuple[Dict[str, Any], str]]:
    """Read one JSON-RPC request.

    Supports both Content-Length framed MCP stdio messages and line-delimited
    JSON-RPC (for clients that don't use LSP-style framing).
    """
    first_line = sys.stdin.buffer.readline()
    if not first_line:
        return None

    stripped = first_line.lstrip()
    if stripped.startswith(b"{"):
        raw = first_line
        while True:
            try:
                return json.loads(raw.decode("utf-8")), "line"
            except json.JSONDecodeError:
                next_line = sys.stdin.buffer.readline()
                if not next_line:
                    return None
                raw += next_line

    headers: Dict[str, str] = {}
    line = first_line
    while True:
        if line in (b"\r\n", b"\n"):
            break
        raw = line.decode("ascii", errors="replace")
        if ":" in raw:
            key, value = raw.split(":", 1)
            headers[key.strip().lower()] = value.strip()
        line = sys.stdin.buffer.readline()
        if not line:
            return None

    content_length_raw = headers.get("content-length", "0")
    try:
        content_length = int(content_length_raw)
    except ValueError:
        return None
    if content_length <= 0:
        return None
    payload = sys.stdin.buffer.read(content_length)
    if not payload:
        return None
    return json.loads(payload.decode("utf-8")), "framed"


def _write_message(message: Dict[str, Any], wire_mode: str = "framed") -> None:
    payload = json.dumps(message, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    if wire_mode == "line":
        sys.stdout.buffer.write(payload + b"\n")
    else:
        sys.stdout.buffer.write(f"Content-Length: {len(payload)}\r\n\r\n".encode("ascii"))
        sys.stdout.buffer.write(payload)
    sys.stdout.buffer.flush()


def _jsonrpc_result(message_id: Any, result: Dict[str, Any]) -> Dict[str, Any]:
    return {"jsonrpc": "2.0", "id": message_id, "result": result}


def _jsonrpc_error(message_id: Any, code: int, message: str) -> Dict[str, Any]:
    return {"jsonrpc": "2.0", "id": message_id, "error": {"code": code, "message": message}}


def _run_emacs_eval(expr: str) -> str:
    emacsclient = os.environ.get("EMACSCLIENT", "emacsclient")
    command = [emacsclient]

    server_file = os.environ.get("EMACS_SERVER_FILE")
    socket_name = os.environ.get("EMACS_SOCKET_NAME")
    if server_file:
        command.extend(["--server-file", server_file])
    elif socket_name:
        command.extend(["--socket-name", socket_name])

    command.extend(["--eval", expr])
    process = subprocess.run(command, capture_output=True, text=True, check=False)
    if process.returncode != 0:
        stderr = process.stderr.strip()
        stdout = process.stdout.strip()
        details = stderr or stdout or "emacsclient failed"
        raise RuntimeError(details)

    lines = [line.strip() for line in process.stdout.splitlines() if line.strip()]
    if not lines:
        raise RuntimeError("emacsclient returned no output")
    return lines[-1]


def _decode_emacs_json(raw_output: str) -> Dict[str, Any]:
    raw = raw_output.strip()
    try:
        parsed = ast.literal_eval(raw)
    except Exception:
        parsed = raw

    if isinstance(parsed, str):
        value = json.loads(parsed)
    elif isinstance(parsed, dict):
        value = parsed
    else:
        value = json.loads(raw)

    if not isinstance(value, dict):
        raise ValueError("Expected object response from Emacs bridge")
    return value


def _call_open_diff(arguments: Dict[str, Any]) -> Dict[str, Any]:
    old_file_path = arguments.get("old_file_path")
    new_file_contents = arguments.get("new_file_contents")
    tab_name = arguments.get("tab_name", "")

    if not isinstance(old_file_path, str) or not old_file_path:
        return {
            "isError": True,
            "content": [{"type": "text", "text": "openDiff: old_file_path is required"}],
        }
    if not isinstance(new_file_contents, str):
        return {
            "isError": True,
            "content": [{"type": "text", "text": "openDiff: new_file_contents must be a string"}],
        }
    if not isinstance(tab_name, str):
        tab_name = ""

    temp_path = ""
    try:
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".codex-ediff", encoding="utf-8", delete=False
        ) as handle:
            handle.write(new_file_contents)
            temp_path = handle.name

        expr = (
            "(my/codex-ediff-review-from-file "
            f"{_elisp_quote(old_file_path)} "
            f"{_elisp_quote(temp_path)} "
            f"{_elisp_quote(tab_name)})"
        )
        raw_result = _run_emacs_eval(expr)
        result = _decode_emacs_json(raw_result)
    except Exception as error:
        return {
            "isError": True,
            "content": [{"type": "text", "text": f"openDiff bridge failed: {error}"}],
        }
    finally:
        if temp_path:
            try:
                os.unlink(temp_path)
            except OSError:
                pass

    error_text = result.get("error")
    accepted = bool(result.get("accepted"))
    edited_contents = result.get("new_file_contents", "")

    if error_text:
        return {
            "isError": True,
            "content": [{"type": "text", "text": f"Ediff failed: {error_text}"}],
        }
    if not accepted:
        return {
            "isError": True,
            "content": [{"type": "text", "text": "Ediff review was rejected by the user."}],
        }

    if not isinstance(edited_contents, str):
        edited_contents = str(edited_contents)
    return {"content": [{"type": "text", "text": edited_contents}]}


def _call_close_all_diffs() -> Dict[str, Any]:
    try:
        raw_result = _run_emacs_eval("(my/codex-ediff-close-all)")
        result = _decode_emacs_json(raw_result)
        closed_count = result.get("closed_count", 0)
        return {"content": [{"type": "text", "text": f"Closed {closed_count} diff tab(s)."}]}
    except Exception as error:
        return {
            "isError": True,
            "content": [{"type": "text", "text": f"closeAllDiffTabs bridge failed: {error}"}],
        }


TOOLS = [
    {
        "name": "openDiff",
        "description": (
            "Open an Emacs Ediff session to review a proposed file update. "
            "Returns the accepted (optionally edited) new file contents."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "old_file_path": {"type": "string", "description": "Absolute path of the target file"},
                "new_file_contents": {"type": "string", "description": "Proposed full replacement content"},
                "tab_name": {"type": "string", "description": "Label for the diff session"},
            },
            "required": ["old_file_path", "new_file_contents", "tab_name"],
        },
    },
    {
        "name": "closeAllDiffTabs",
        "description": "Close all currently open Codex Ediff sessions in Emacs.",
        "inputSchema": {"type": "object", "properties": {}},
    },
]


def _handle_request(request: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    method = request.get("method")
    message_id = request.get("id")
    params = request.get("params", {})

    if method == "initialize":
        return _jsonrpc_result(
            message_id,
            {
                "protocolVersion": PROTOCOL_VERSION,
                "capabilities": {"tools": {"listChanged": False}},
                "serverInfo": {"name": SERVER_NAME, "version": SERVER_VERSION},
            },
        )

    if method == "notifications/initialized":
        return None

    if method == "tools/list":
        return _jsonrpc_result(message_id, {"tools": TOOLS})

    if method == "tools/call":
        name = params.get("name")
        arguments = params.get("arguments", {})
        if name == "openDiff":
            return _jsonrpc_result(message_id, _call_open_diff(arguments))
        if name == "closeAllDiffTabs":
            return _jsonrpc_result(message_id, _call_close_all_diffs())
        return _jsonrpc_result(
            message_id,
            {"isError": True, "content": [{"type": "text", "text": f"Unknown tool: {name}"}]},
        )

    if method == "ping":
        return _jsonrpc_result(message_id, {})

    return _jsonrpc_error(message_id, -32601, f"Method not found: {method}")


def main() -> int:
    while True:
        incoming = _read_message()
        if incoming is None:
            return 0
        message, wire_mode = incoming

        if not isinstance(message, dict):
            continue
        if "id" not in message:
            # Notification
            continue

        try:
            response = _handle_request(message)
        except Exception as error:  # pragma: no cover
            response = _jsonrpc_error(
                message.get("id"),
                -32000,
                f"Unhandled bridge exception: {error}",
            )

        if response is not None:
            _write_message(response, wire_mode)


if __name__ == "__main__":
    raise SystemExit(main())
