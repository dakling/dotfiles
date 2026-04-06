# Home Manager configuration
# User environment, shell, editors, applications

{config, pkgs, pkgs-unstable, inputs, ...}: {
  # Home Manager settings
  home.username = "klingenberg";
  home.homeDirectory = "/home/klingenberg";
  home.stateVersion = "24.11";

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # XDG directories
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
      documents = "$HOME/Documents";
      download = "$HOME/Downloads";
      music = "$HOME/Music";
      pictures = "$HOME/Pictures";
      videos = "$HOME/Videos";
      desktop = "$HOME/Desktop";
      templates = "$HOME/Templates";
      publicShare = "$HOME/Public";
    };
    mimeApps = {
      enable = true;
      defaultApplications = {
        "text/html" = ["firefox.desktop"];
        "x-scheme-handler/http" = ["firefox.desktop"];
        "x-scheme-handler/https" = ["firefox.desktop"];
        "x-scheme-handler/mailto" = ["thunderbird.desktop"];
        "application/pdf" = ["org.pwmt.zathura.desktop" "firefox.desktop"];
        "image/png" = ["org.gnome.eog.desktop"];
        "image/jpeg" = ["org.gnome.eog.desktop"];
        "video/mp4" = ["mpv.desktop"];
        "video/x-matroska" = ["mpv.desktop"];
      };
    };
  };

  # Shell configuration
  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      ll = "ls -alF";
      la = "ls -A";
      l = "ls -CF";
      grep = "grep --color=auto";
      fgrep = "fgrep --color=auto";
      egrep = "egrep --color=auto";
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";
      ga = "git add";
      gc = "git commit";
      gs = "git status";
      gp = "git push";
      gl = "git log --oneline --graph";
      gd = "git diff";
      nrs = "sudo nixos-rebuild switch --flake /home/klingenberg/Documents/programming/nix/config";
      nrb = "sudo nixos-rebuild build --flake /home/klingenberg/Documents/programming/nix/config";
      hms = "home-manager switch --flake /home/klingenberg/Documents/programming/nix/config";
    };
    bashrcExtra = ''
      # Custom bashrc additions
      export EDITOR=emacsclient
      export VISUAL=emacsclient

      # History settings
      shopt -s histappend
      HISTSIZE=10000
      HISTFILESIZE=20000
      HISTCONTROL=ignoreboth
      HISTIGNORE='ls:bg:fg:history'

      # Prompt
      PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    '';
    sessionVariables = {
      EDITOR = "emacsclient";
      VISUAL = "emacsclient";
    };
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      add_newline = true;
      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };
      directory = {
        truncation_length = 3;
      };
      git_branch = {
        symbol = " ";
      };
      nix_shell = {
        symbol = " ";
        format = "via [$symbol$state]($style) ";
      };
    };
  };

  # Git configuration
  programs.git = {
    enable = true;
    userName = "klingenberg";
    userEmail = "klingenberg@example.com"; # Should be set from secrets
    extraConfig = {
      init.defaultBranch = "main";
      pull.rebase = false;
      push.autoSetupRemote = true;
      core.editor = "emacsclient";
      credential.helper = "store";
    };
    ignores = [
      "*~"
      "*.swp"
      ".DS_Store"
      "__pycache__/"
      "*.pyc"
      ".envrc"
      ".direnv/"
    ];
    aliases = {
      st = "status";
      co = "checkout";
      br = "branch";
      ci = "commit";
      unstage = "reset HEAD --";
      last = "log -1 HEAD";
      visual = "!gitk";
    };
  };

  # Terminal emulator
  programs.kitty = {
    enable = true;
    settings = {
      font_family = "Fira Code";
      font_size = "11.0";
      background_opacity = "0.95";
      window_padding_width = "5";
    };
  };

  # Rofi launcher
  programs.rofi = {
    enable = true;
    terminal = "alacritty";
    theme = "Arc-Dark";
    extraConfig = {
      modi = "window,run,ssh,drun";
      show-icons = true;
      sidebar-mode = true;
      lines = 5;
      bw = 2;
      eh = 2;
    };
  };

  # Dunst notifications
  services.dunst = {
    enable = true;
    settings = {
      global = {
        monitor = 0;
        follow = "mouse";
        geometry = "300x5-30+50";
        indicate_hidden = "yes";
        shrink = "no";
        transparency = 0;
        notification_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        frame_width = 3;
        frame_color = "#aaaaaa";
        separator_color = "frame";
        sort = "yes";
        idle_threshold = 120;
        font = "Fira Code 10";
        line_height = 0;
        markup = "full";
        format = "<b>%s</b>\n%b";
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = "yes";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "left";
        max_icon_size = 32;
        icon_path = "/usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/";
        sticky_history = "yes";
        history_length = 20;
        dmenu = "${pkgs.rofi}/bin/rofi -p dunst:";
        browser = "${pkgs.firefox}/bin/firefox --new-tab";
        title = "Dunst";
        class = "Dunst";
      };
      urgency_low = {
        background = "#222222";
        foreground = "#888888";
        timeout = 10;
      };
      urgency_normal = {
        background = "#285577";
        foreground = "#ffffff";
        timeout = 10;
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = 0;
      };
    };
  };

  # Syncthing (user service)
  services.syncthing = {
    enable = true;
    tray = {
      enable = true;
      command = "syncthingtray";
    };
  };

  # User packages - Development
  home.packages = with pkgs; [
    # Development tools
    git
    git-crypt
    git-lfs
    gh
    lazygit

    # Languages
    python3
    python3Packages.pip
    python3Packages.virtualenv
    python3Packages.ipython
    julia-bin
    ghc
    haskellPackages.stack
    dotnet-sdk
    nodejs
    nodePackages.npm
    nodePackages.yarn
    go
    rustup
    cargo
    gcc
    clang
    cmake
    gnumake
    pkg-config

    # Neovim
    neovim
    tree-sitter

    # Emacs dependencies
    ripgrep
    fd
    cmake # for vterm

    # Terminal utilities
    alacritty
    kitty
    tmux
    screen

    # File managers
    ranger
    nnn
    pcmanfm
    xfce.thunar

    # Archive tools
    zip
    unzip
    p7zip
    xarchiver

    # System tools
    htop
    btop
    neofetch
    fastfetch
    lm_sensors
    acpi

    # Networking
    wget
    curl
    aria2
    transmission-gtk

    # Media
    mpv
    ffmpeg
    ffmpegthumbnailer
    playerctl
    pavucontrol
    blueberry
    simple-scan
    obs-studio
    kdenlive
    gimp
    inkscape
    darktable
    flameshot
    peek

    # Documents
    libreoffice
    okular
    evince
    zathura
    mupdf
    texlive.combined.scheme-full
    pandoc

    # Communication
    signal-desktop
    telegram-desktop
    discord
    zoom-us
    jami

    # Browsers
    firefox
    google-chrome
    qutebrowser

    # Email
    thunderbird

    # Gaming
    steam
    lutris
    wine
    winetricks
    bottles

    # Go games
    katago
    sabaki
    q5go

    # Office
    goldendict
    calibre

    # Nix tools
    nix-prefetch-git
    nix-prefetch-github
    nix-index
    nix-tree
    comma

    # Password management
    keepassxc
    bitwarden

    # Flatpak support
    flatpak

    # AppImage support
    appimage-run
  ] ++ (with pkgs-unstable; [
    # Packages from unstable
    # Add unstable packages here
  ]);

  # Emacs configuration
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      # Essential packages for Doom Emacs
      evil
      evil-collection
      evil-org
      magit
      which-key
      ivy
      counsel
      swiper
      projectile
      direnv
      vterm
      all-the-icons
      doom-modeline
      doom-themes
      treemacs
      company
      flycheck
      lsp-mode
      lsp-ui
      lsp-ivy
      nix-mode
      haskell-mode
      python-mode
      julia-mode
      rust-mode
      go-mode
      json-mode
      yaml-mode
      markdown-mode
      org
      org-roam
      org-bullets
      pdf-tools
      telega
      elfeed
      mu4e
    ];
  };

  # Doom Emacs setup
  home.file.".doom.d" = {
    source = ./doom.d;
    recursive = true;
  };

  # Qtile configuration
  home.file.".config/qtile/config.py".text = ''
    # Qtile configuration
    # This is a basic configuration - customize as needed

    from libqtile.config import Key, Screen, Group, Drag, Click
    from libqtile.command import lazy
    from libqtile import layout, bar, widget, hook
    from libqtile.utils import guess_terminal

    mod = "mod4"
    terminal = guess_terminal()

    keys = [
        # Switch between windows
        Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
        Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
        Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
        Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
        Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),

        # Move windows between left/right columns or move up/down in current stack.
        Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
        Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
        Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
        Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

        # Grow windows
        Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
        Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
        Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
        Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
        Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

        # Toggle between split and unsplit sides of stack
        Key([mod, "shift"], "Return", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),
        Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),

        # Toggle between different layouts
        Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
        Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),

        Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
        Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
        Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),

        # Rofi
        Key([mod], "p", lazy.spawn("rofi -show drun"), desc="Launch rofi"),
        Key([mod, "shift"], "p", lazy.spawn("rofi -show run"), desc="Launch rofi run"),

        # Multimedia keys
        Key([], "XF86AudioRaiseVolume", lazy.spawn("pamixer -i 5")),
        Key([], "XF86AudioLowerVolume", lazy.spawn("pamixer -d 5")),
        Key([], "XF86AudioMute", lazy.spawn("pamixer -t")),
        Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +5%")),
        Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 5%-")),

        # Screenshot
        Key([], "Print", lazy.spawn("flameshot gui")),
    ]

    groups = [Group(i) for i in "123456789"]

    for i in groups:
        keys.extend([
            Key([mod], i.name, lazy.group[i.name].toscreen(), desc="Switch to group {}".format(i.name)),
            Key([mod, "shift"], i.name, lazy.window.togroup(i.name), desc="Move focused window to group {}".format(i.name)),
        ])

    layouts = [
        layout.Max(),
        layout.Stack(num_stacks=2),
        layout.MonadTall(),
        layout.MonadWide(),
        layout.RatioTile(),
        layout.Tile(),
        layout.TreeTab(),
        layout.VerticalTile(),
        layout.Zoomy(),
    ]

    widget_defaults = dict(
        font="Fira Code",
        fontsize=12,
        padding=3,
    )
    extension_defaults = widget_defaults.copy()

    screens = [
        Screen(
            bottom=bar.Bar(
                [
                    widget.CurrentLayout(),
                    widget.GroupBox(),
                    widget.Prompt(),
                    widget.WindowName(),
                    widget.Chord(
                        chords_colors={
                            "launch": ("#ff0000", "#ffffff"),
                        },
                        name_transform=lambda name: name.lower(),
                    ),
                    widget.TextBox("default config", name="default"),
                    widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                    widget.NmCli(),
                    widget.PulseVolume(),
                    widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                    widget.QuickExit(),
                ],
                24,
            ),
        ),
    ]

    # Drag floating layouts.
    mouse = [
        Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
        Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
        Click([mod], "Button2", lazy.window.bring_to_front()),
    ]

    dgroups_key_binder = None
    dgroups_app_rules = []
    follow_mouse_focus = True
    bring_front_click = False
    cursor_warp = False
    floating_layout = layout.Floating(
        float_rules=[
            *layout.Floating.default_float_rules,
            # Custom float rules
        ]
    )
    auto_fullscreen = True
    focus_on_window_activation = "smart"
    reconfigure_screens = True
    auto_minimize = True
    wl_input_rules = None
  '';

  # Environment variables
  home.sessionVariables = {
    EDITOR = "emacsclient";
    VISUAL = "emacsclient";
    BROWSER = "firefox";
    TERMINAL = "alacritty";
    QT_QPA_PLATFORMTHEME = "gtk2";
    GTK_THEME = "Adwaita:dark";
    NIXOS_CONFIG = "/home/klingenberg/Documents/programming/nix/config";
  };

  # Session path additions
  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/.cargo/bin"
    "$HOME/.nix-profile/bin"
  ];

  # Programs that need additional configuration
  programs = {
    # Direnv for nix shells
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableBashIntegration = true;
    };

    # Fzf
    fzf = {
      enable = true;
      enableBashIntegration = true;
      defaultCommand = "fd --type f";
      defaultOptions = ["--height 40%" "--border"];
    };

    # Bat
    bat = {
      enable = true;
      config = {
        theme = "TwoDark";
        pager = "less -FR";
      };
    };

    # Eza (ls replacement)
    eza = {
      enable = true;
      enableBashIntegration = true;
      git = true;
      icons = true;
    };

    # Zoxide (cd replacement)
    zoxide = {
      enable = true;
      enableBashIntegration = true;
    };

    # GPG
    gpg = {
      enable = true;
      settings = {
        default-key = "CHANGE_ME";
        keyserver = "hkps://keys.openpgp.org";
      };
    };

    # SSH
    ssh = {
      enable = true;
      extraConfig = ''
        Host github.com
          HostName github.com
          User git
          IdentityFile ~/.ssh/id_ed25519

        Host gitlab.com
          HostName gitlab.com
          User git
          IdentityFile ~/.ssh/id_ed25519
      '';
    };
  };
}
