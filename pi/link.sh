#!/bin/sh
# Link pi's configuration into ~/.pi/agent.
#
# Everything that is pi-specific (settings, MCP servers, extensions, the
# pi-specific AGENTS.md) lives in this directory and is symlinked into place.
#
# Deliberately NOT linked, because they are owned by something else:
#   - skills/  and prompts/  -> symlinked to ~/.claude/... by pi's own installer
#   - extensions/herdr-agent-state.ts -> managed by herdr, reinstalled on update
#   - npm/, sessions/, missions/, install/, bin/ -> runtime state, not config
#
# Run from anywhere:  sh ~/.dotfiles/pi/link.sh
# Verify:             sh ~/.dotfiles/pi/link.sh --check

set -eu

DOTFILES_PI="$HOME/.dotfiles/pi"
PI_AGENT="$HOME/.pi/agent"

if [ ! -d "$DOTFILES_PI" ]; then
	echo "error: $DOTFILES_PI not found" >&2
	exit 1
fi

# Files that pi reads and pi may rewrite. A symlink is fine: pi writes
# through the link, so the dotfiles copy stays authoritative.
FILES="settings.json mcp.json models.json bash-guard.json lsp.json AGENTS.md"

# Extension directories and files. Copied as real content (not symlinked) so
# that the whole directory can be replaced atomically.
EXTENSIONS="bash-guard.ts questionnaire.ts plan-mode"

link() {
	src="$1"
	dst="$2"
	if [ ! -e "$src" ]; then
		echo "skip (missing source): $src"
		return 0
	fi
	if [ -L "$dst" ]; then
		# Already linked: refresh it if the target moved.
		if [ "$(readlink "$dst")" = "$src" ]; then
			echo "ok (linked):   $dst"
			return 0
		fi
		rm "$dst"
	elif [ -e "$dst" ]; then
		# Real file in place: keep a backup before replacing it.
		backup="${dst}.backup.$(date +%Y%m%d-%H%M%S)"
		mv "$dst" "$backup"
		echo "moved existing file to $backup"
	fi
	ln -s "$src" "$dst"
	echo "linked:       $dst -> $src"
}

if [ "${1:-}" = "--check" ]; then
	status=0
	for f in $FILES; do
		src="$DOTFILES_PI/$f"
		dst="$PI_AGENT/$f"
		if [ -L "$dst" ] && [ "$(readlink "$dst")" = "$src" ]; then
			echo "ok      $f"
		else
			echo "MISSING $f (expected $dst -> $src)"
			status=1
		fi
	done
	for e in $EXTENSIONS; do
		src="$DOTFILES_PI/extensions/$e"
		dst="$PI_AGENT/extensions/$e"
		if [ -L "$dst" ] && [ "$(readlink "$dst")" = "$src" ]; then
			echo "ok      extensions/$e (linked)"
		elif [ -e "$dst" ] && [ -e "$src" ] && diff -rq "$src" "$dst" >/dev/null 2>&1; then
			echo "ok      extensions/$e (identical copy)"
		else
			echo "MISSING extensions/$e (expected $dst -> $src)"
			status=1
		fi
	done
	exit "$status"
fi

mkdir -p "$PI_AGENT/extensions"

for f in $FILES; do
	link "$DOTFILES_PI/$f" "$PI_AGENT/$f"
done

for e in $EXTENSIONS; do
	link "$DOTFILES_PI/extensions/$e" "$PI_AGENT/extensions/$e"
done

echo
echo "Done. Run 'pi /reload' in a live session to pick up changes."
