# Tangled from README.org
# This is a nix-darwin config
{ pkgs, lib, inputs, config, username, ... }: {
  imports = [
    # import modules into our nix-darwin config

    ./emacs/nix-darwin.nix
    ./system/darwin
  ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # common Darwin packages
    asciinema
    bat
    checkmake
    exercism
    gh
    gleam
    gnumake
    gnupg
    gotop
    hexyl
    html-tidy
    htop
    httpie
    httplab
    jq
    kakoune
    nodePackages.typescript-language-server
    nodejs
    pqrs
    nixpkgs-fmt
    redis
    shell-gpt
    shellcheck
    shfmt
    sqlite-interactive
    tree
    tree-sitter
    vim
    xxd
    yq
    inputs.devenv.packages.${system}.default

    devbox
    pass
    nushell
    wezterm
    pdftk
    hunspell
    hunspellDicts.nl_NL
    hunspellDicts.en_US
    hunspellDicts.de_DE
    hunspellDicts.he_IL
    hunspellDicts.es-any
    hunspellDicts.fr-any
  ] ++ (if system == "aarch64-darwin" then [
    # ARM-only packages

  ] else [
    # Intel-only packages
    # Darwin packages for Intel-only
    gdb
    ghidra-bin
  ]);

  environment.interactiveShellInit = lib.strings.concatStrings [
    ''
      eval "''$(${config.homebrew.brewPrefix}/brew shellenv)";
    ''
  ];

  # General nix-darwin settings
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Adding trusted users for devenv to use Cachix
  nix.settings.trusted-users = [
    "root"
    "vidbina"
  ];

  # NOTE: Copied from home-linux.nix
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Create /etc/zshrc that loads the nix-darwin environment.
  # NOTE: Copied from common.nix
  programs.zsh = {
    enable = true; # default shell on catalina
    enableSyntaxHighlighting = true;
    # Used to be initExtraBeforeCompInit
    # in nix-darwin, interactiveShellInit is called before compinit
    # see https://github.com/LnL7/nix-darwin/blob/80bb201f4925cdda5a7a3c7b1900fb26bb2af2e8/modules/programs/zsh/default.nix#L168-L176

    promptInit = ''
      setopt histignorespace # keeps lines preceded with SPACE out of history

      setopt INTERACTIVE_COMMENTS  # allow inline comments like this one
      # https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
      vterm_printf(){
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }
      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
          alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi
      vterm_prompt_end() {
          vterm_printf "51;A";
      }
      setopt PROMPT_SUBST
      PROMPT="↪ %(?.%F{green}√.%F{red}%?)%f" # error state
      PROMPT="$PROMPT → %F{yellow}%~%f" # pwd
      PROMPT="$PROMPT @ %F{magenta}%D{%Y.%m.%d} %B%F{blue}%T%f%b" # date/time
      PROMPT="$PROMPT"$'\n'
      PROMPT="$PROMPT%F{green}>%f " # prompt
      PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
      vterm_cmd() {
          local vterm_elisp
          vterm_elisp=""
          while [ $# -gt 0 ]; do
              vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
              shift
          done
          vterm_printf "51;E$vterm_elisp"
      }

      # Workaround to open new tab at pwd
      # See https://apple.stackexchange.com/a/340778
      # http://superuser.com/a/315029/4952
      # Set Apple Terminal.app to resume directory... still necessary 2018-10-26
      if [[ $TERM_PROGRAM == "Apple_Terminal" ]] && [[ -z "$INSIDE_EMACS" ]] {
        function chpwd {
          local SEARCH=' '
          local REPLACE='%20'
          local PWD_URL="file://$HOSTNAME''${PWD//$SEARCH/$REPLACE}"
          printf '\e]7;%s\a' "$PWD_URL"
        }
        chpwd
      }

      # Use vim bindings in zsh
      bindkey -v
      # https://unix.stackexchange.com/a/30169
      bindkey '^R' history-incremental-search-backward
    '';
  };

  # Set Git commit hash for darwin-version.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  system = {
    defaults.CustomUserPreferences = {
      "com.microsoft.VSCode" = {
        "ApplePressAndHoldEnabled" = false;
      };
    };
  };

  users.users.vidbina = {
    home = "/Users/vidbina";
  };

  homebrew = {
    enable = true;
    global = {
      autoUpdate = true; # same as default
    };
    onActivation = {
      autoUpdate = false; # same as default
      cleanup = "uninstall";
      extraFlags = [
        "--verbose"
      ];
      upgrade = false; # same as default
    };
    brews = [
      "smudge/smudge/nightlight"
      "pidof"

      "coreutils"
      "wimlib"
      "wireguard-tools"
      "pcalc"
    ];
    casks = builtins.filter (x: x != null) [
      # Software Development
      "iterm2"
      "kitty"

      # Design
      "figma"
      "drawio"

      # Containerization & Virtualization
      "docker"
      "utm"

      # Productivity
      "google-drive"
      "linear-linear"
      "logseq" # FLOSS (compared to Obsidian) but no mobile app
      "notion"
      "obsidian" # best-in-class with mobile app support
      "raycast"
      "zoom"

      # Android
      "android-file-transfer"

      # Devtools
      # Go to top-right Settings gear > VSCode Import > Start Import
      "cursor"
      "warp"

      # Entertainment
      "spotify"
      "steam"
      "tidal"

      # Social
      "discord"
      "signal"
      "slack"
      "telegram"
      "whatsapp"

      "1password"
      "1password-cli"
      (if pkgs.system == "aarch64-darwin" then "chatgpt" else null)
      "obs" # for streaming
      "spotify"
    ];
    masApps = {
      "Xcode" = 497799835;
      "Bitwarden" = 1352778147;
      "Hidden Bar" = 1452453066;
      "Remarkable Desktop" = 1276493162;
    };
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "vscode"
  ];

  nixpkgs.overlays = [
    (self: super: {
      # nix-darwin overlays
      my-vscode-extensions = inputs.vscode-extensions.extensions.${pkgs.system};
    })
  ];

  environment.pathsToLink = [ "/share/myspell" "/share/hunspell" ];
}
