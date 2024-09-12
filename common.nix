# Tangled from README.org
{ config, lib, pkgs, ... }:

# TODO: Config mutt
let
  inherit (pkgs) stdenv;
  pathIfExists = (p: if (builtins.pathExists p) then [ p ] else [ ]);

  sources = import ./nix/sources.nix;
  nixpkgs-bleeding-src = sources."nixpkgs-bleeding";
  pkgs-bleeding = import nixpkgs-bleeding-src { };
in
{
  imports = [
    ./dev.nix
    ./vim.nix
  ]
  ++ (pathIfExists ./personal.nix);

  home.packages = [
    pkgs-bleeding.niv
    pkgs-bleeding.nixVersions.nix_2_13
    pkgs.slack
    pkgs.discord
  ];

  home.file.".config/ranger".source = config.lib.file.mkOutOfStoreSymlink ./ranger;

  # TODO: Remove, likely not necessary
  home.file.".direnvrc".source = config.lib.file.mkOutOfStoreSymlink ./direnv/direnvrc;

  #home.file.".profile".text = ''
  #  PATH=${toString ./bin}:$HOME/.nix-profile/bin:$PATH
  #  export PATH
  #'';

  home.sessionPath = [
    (toString ./bin)
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs = {
    overlays = [
      (self: super: {

      })
    ];

    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "slack"
      "discord"
      "discord-ptb"
      "discord-canary"
    ];
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "base16";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile (./. + "/tmux.conf");
  };
  manual = {
    # Use `home-manager-help`
    html.enable = false;

    # Use `man home-configuration.nix`
    manpages.enable = false;
  };
  programs.zsh = {
    enable = true;
    enableAutosuggestions = false;
    enableSyntaxHighlighting = true;

    defaultKeymap = "viins";

    initExtraBeforeCompInit = ''
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
    '';

    initExtra = ''
      # enable bash completion
      autoload -U +X bashcompinit && \
      bashcompinit
      zmodload -i zsh/complist
      #source ${./zsh/zstyle.zsh}
      source ${pkgs.fzf}/share/fzf/completion.zsh
      source ${pkgs.fzf}/share/fzf/key-bindings.zsh
      # enable gh completion
      eval "$(gh completion -s zsh)"
    '';
  };
  programs.pywal = {
    enable = true;
  };
}
