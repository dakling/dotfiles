# Home Manager configuration
# User environment, shell, editors, applications

{config, pkgs, pkgs-unstable, inputs, lib, ...}: {
  # Home Manager settings
  home.username = "helario";
  home.homeDirectory = lib.mkDefault "/home/helario";
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
      nrs = "sudo nixos-rebuild switch --flake /home/helario/.dotfiles/nixos";
      nrb = "sudo nixos-rebuild build --flake /home/helario/.dotfiles/nixos";
      hms = "home-manager switch --flake /home/helario/.dotfiles/nixos";
    };
    bashrcExtra = ''
      # Custom bashrc additions
      export EDITOR=nvim
      export VISUAL=nvim

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
      EDITOR = "nvim";
      VISUAL = "nvim";
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
    userName = "helario";
    userEmail = "helario@example.com"; # Should be set from secrets
    extraConfig = {
      init.defaultBranch = "main";
      pull.rebase = false;
      push.autoSetupRemote = true;
      core.editor = "nvim";
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
      shell = "/run/current-system/sw/bin/bash";
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
    yarn
    go
    rustup
    gcc
    cmake
    gnumake
    pkg-config

    # Neovim
    neovim
    tree-sitter

    # Emacs dependencies
    ripgrep
    fd # for vterm

    # Terminal utilities
    kitty
    tmux

    # File managers
    ranger
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

    # Media
    mpv
    ffmpeg
    ffmpegthumbnailer
    playerctl
    pavucontrol
    blueberry
    simple-scan
    obs-studio
    kdePackages.kdenlive
    gimp
    inkscape
    darktable
    flameshot
    peek

    # Documents
    # libreoffice  # heavy, uncomment after first build
    kdePackages.okular
    evince
    zathura
    mupdf
    # texlive.combined.scheme-full  # heavy, uncomment after first build
    pandoc

    # Communication
    # signal-desktop  # heavy, uncomment after first build
    telegram-desktop
    discord
    zoom-us
    jami

    # Browsers
    firefox
    google-chrome

    # Email
    thunderbird

    # Gaming
    # steam  # heavy, uncomment after first build
    lutris
    wine
    winetricks
    bottles

    # Go games
    katago
    (pkgs.appimageTools.wrapType2 {
      pname = "weiqihub";
      version = "0.1.13";
      src = pkgs.fetchurl {
        url = "https://github.com/ale64bit/WeiqiHub/releases/download/v0.1.13/WeiqiHub-v0.1.13-x86_64.AppImage";
        hash = "sha256-+mOWf3XTPuvskJV+TBD368LHRzxesBDBldSI1QvTggE=";
      };
      extraPkgs = pkgs: [ pkgs.libepoxy ];
    })

    # Office
    calibre

    # Nix tools
    nix-prefetch-git
    nix-prefetch-github
    nix-index
    nix-tree
    comma

    # Password management
    keepassxc
    bitwarden-desktop

    # Flatpak support
    flatpak

    # AppImage support
    appimage-run
  ] ++ (with pkgs-unstable; [
    # Packages from unstable
    freetube
  ]);

  # Qtile configuration (inside flake to avoid purity boundary issues)
  home.file.".config/qtile/config.py".source = ./qtile_config.py;

  # Environment variables
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
    BROWSER = "firefox";
    TERMINAL = "alacritty";
    QT_QPA_PLATFORMTHEME = "gtk2";
    GTK_THEME = "Adwaita:dark";
    NIXOS_CONFIG = "/home/helario/.dotfiles/nixos";
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
      icons = "auto";
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
      matchBlocks = {
        "github.com" = {
          hostname = "github.com";
          user = "git";
          identityFile = "~/.ssh/id_ed25519";
        };
        "gitlab.com" = {
          hostname = "gitlab.com";
          user = "git";
          identityFile = "~/.ssh/id_ed25519";
        };
      };
    };
  };
}
