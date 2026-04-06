# NixOS Configuration

Personal NixOS configuration for a laptop with Intel i5-1135G7.

## Structure

```
config/
├── flake.nix                    # Flake entry point
├── configuration.nix            # Main NixOS config
├── hardware-configuration.nix   # Hardware-specific config (generate this!)
├── secrets/
│   └── secrets.yaml             # Encrypted secrets (sops-nix)
├── modules/
│   ├── core.nix                 # Core system: boot, users, locale
│   ├── hardware.nix             # Intel graphics, audio, bluetooth
│   ├── desktop.nix              # Qtile, XFCE, fonts, themes
│   ├── services.nix             # Docker, syncthing, cups, etc.
│   └── xinit.nix                # startx/xinitrc configuration
├── home.nix                     # Home Manager config
└── doom.d/                      # Doom Emacs configuration
    ├── init.el
    ├── config.el
    └── packages.el
```

## Installation

### 1. Generate Hardware Configuration

After booting the NixOS installer, generate the hardware configuration:

```bash
sudo nixos-generate-config --root /mnt
```

Copy the generated `/mnt/etc/nixos/hardware-configuration.nix` to this directory,
overwriting the template.

### 2. Set Up Secrets (Optional)

If using sops-nix for secrets:

```bash
# Generate age key
mkdir -p ~/.config/sops/age
age-keygen -o ~/.config/sops/age/keys.txt

# Update .sops.yaml with your public key
# Then edit and encrypt secrets
sops secrets/secrets.yaml
```

### 3. Install

```bash
# Copy configuration to /mnt/etc/nixos
sudo cp -r . /mnt/etc/nixos/

# Install NixOS
sudo nixos-install --root /mnt --flake /mnt/etc/nixos#laptop
```

### 4. Post-Installation

After reboot:

1. Change user password: `passwd`
2. Set up Doom Emacs: `git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install`
3. Configure git: `git config --global user.email "your@email.com"`

## Rebuilding

After making changes:

```bash
# Build and test (doesn't apply changes)
sudo nixos-rebuild build --flake .#laptop

# Apply changes
sudo nixos-rebuild switch --flake .#laptop

# Build and switch to new configuration
sudo nixos-rebuild boot --flake .#laptop
```

## Home Manager

Home Manager is integrated as a NixOS module. User packages and configurations
are defined in `home.nix`.

To rebuild Home Manager separately (if needed):

```bash
home-manager switch --flake .#klingenberg
```

## Desktop Environment

### Qtile

Primary window manager. Configuration is in `~/.config/qtile/config.py`.

Start with: `startx`

### XFCE

Alternative desktop environment available on TTY2.

Switch to TTY2 (Ctrl+Alt+F2) and run: `startxfce4`

## Services

- **NetworkManager**: Network management with nm-applet
- **Docker**: Container runtime
- **Syncthing**: File synchronization
- **CUPS**: Printing
- **Pipewire**: Audio server
- **Bluetooth**: With blueman GUI

## Customization

### Adding Packages

System packages go in `configuration.nix` or the appropriate module.
User packages go in `home.nix`.

### Adding Services

Add new services in `modules/services.nix` or create a new module.

### Changing Theme

Edit GTK theme in `modules/desktop.nix`:
```nix
environment.sessionVariables = {
  GTK_THEME = "Adwaita:dark";
};
```

## Troubleshooting

### Build Errors

1. Check syntax: `nix eval .#nixosConfigurations.laptop`
2. Check options: `nixos-option services.xserver.enable`
3. Search packages: `nix search nixpkgs firefox`

### Hardware Issues

1. Update `hardware-configuration.nix` with actual hardware info
2. Check kernel parameters for Intel graphics
3. Verify firmware is available

### Qtile Issues

1. Check logs: `journalctl -u display-manager.service`
2. Test config: `python -m py_compile ~/.config/qtile/config.py`
3. Run with debug: `qtile start -b wayland` (for Wayland testing)

## Resources

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [NixOS Wiki](https://nixos.wiki/)
- [Home Manager](https://github.com/nix-community/home-manager)
- [sops-nix](https://github.com/Mic92/sops-nix)
- [Qtile Documentation](http://docs.qtile.org/)