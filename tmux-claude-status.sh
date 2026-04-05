#!/usr/bin/env bash
# tmux-claude-status.sh — show AI tool status in tmux status/pane borders
# Usage: tmux-claude-status.sh <window_index> <session_name> <context> [staleness]

WINDOW_IDX="${1:-0}"
SESSION="${2:-}"
CONTEXT="${3:-window-inactive}"
STALENESS="${4:-}"

# AI tools to detect
AI_TOOLS=("opencode" "claude" "codex" "gemini" "aider" "cursor-agent")

# Colors (Palenight theme)
COLOR_ACTIVE="#c3e88d" # green
COLOR_IDLE="#676e95"   # grey
COLOR_NONE=""

# Detect running AI processes in the session's panes
detect_ai() {
	local found=""
	for tool in "${AI_TOOLS[@]}"; do
		# Check if any pane in this session is running the tool
		local running
		running=$(tmux list-panes -t "$SESSION" -F "#{pane_current_command} #{pane_pid}" 2>/dev/null |
			grep -i "$tool" | head -1)
		if [ -n "$running" ]; then
			# Check if process is actually alive
			local pid
			pid=$(echo "$running" | awk '{print $NF}')
			if kill -0 "$pid" 2>/dev/null; then
				found="$tool"
				break
			fi
		fi
	done
	echo "$found"
}

# Also check pane_current_command from the calling context
check_pane_command() {
	local cmd="${TMUX_CLAUDE_STATUS_PANES:-}"
	if [ -n "$cmd" ]; then
		for tool in "${AI_TOOLS[@]}"; do
			if echo "$cmd" | grep -qi "$tool"; then
				echo "$tool"
				return
			fi
		done
	fi
}

main() {
	local ai
	ai=$(check_pane_command)
	if [ -z "$ai" ]; then
		ai=$(detect_ai)
	fi

	case "$CONTEXT" in
	pane-active | window-active)
		if [ -n "$ai" ]; then
			echo "#[fg=$COLOR_ACTIVE,bold][$ai] "
		else
			echo ""
		fi
		;;
	pane-inactive | window-inactive)
		if [ -n "$ai" ]; then
			echo "#[fg=$COLOR_ACTIVE][$ai] "
		else
			echo ""
		fi
		;;
	staleness)
		# Not used for staleness, return empty
		echo ""
		;;
	activity)
		if [ -n "$ai" ]; then
			echo "#[fg=$COLOR_ACTIVE,bold] [$ai]"
		else
			echo ""
		fi
		;;
	esac
}

main
