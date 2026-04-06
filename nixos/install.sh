#!/usr/bin/env bash
set -euo pipefail

DOTFILES_DIR="$HOME/.dotfiles/nixos"
TARGET="/etc/nixos"

echo "=== NixOS Config Installer ==="
echo "Source: $DOTFILES_DIR"
echo "Target: $TARGET"
echo ""

# Check source exists
if [ ! -d "$DOTFILES_DIR" ]; then
	echo "ERROR: Source directory $DOTFILES_DIR does not exist"
	exit 1
fi

# Check if we have sudo
if ! sudo -n true 2>/dev/null; then
	echo "Requesting sudo access..."
	sudo true
fi

# Backup existing config if present
if [ -d "$TARGET" ] && [ "$(ls -A "$TARGET" 2>/dev/null)" ]; then
	BACKUP="$TARGET.bak.$(date +%Y%m%d_%H%M%S)"
	echo "Existing config found at $TARGET"
	echo "Backing up to $BACKUP"
	sudo mv "$TARGET" "$BACKUP"
fi

# Create target directory
sudo mkdir -p "$TARGET"

# Symlink all files
echo "Creating symlinks..."
for item in "$DOTFILES_DIR"/* "$DOTFILES_DIR"/.gitignore "$DOTFILES_DIR"/.sops.yaml; do
	[ -e "$item" ] || continue
	name=$(basename "$item")

	# Skip directories that shouldn't be linked
	case "$name" in
	secrets) continue ;;
	esac

	echo "  $name -> $TARGET/$name"
	sudo ln -sf "$item" "$TARGET/$name"
done

# Verify
echo ""
echo "Installed files:"
ls -la "$TARGET"
echo ""
echo "Done! You can now run:"
echo "  sudo nixos-rebuild switch --flake /etc/nixos#laptop"
