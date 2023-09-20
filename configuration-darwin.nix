# This is a nix-darwin config
{ pkgs, lib, inputs, ... }: {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    pkgs.asciinema
    pkgs.checkmake
    pkgs.exercism
    pkgs.gh
    pkgs.gnumake
    pkgs.gnupg
    pkgs.gotop
    pkgs.hexyl
    pkgs.html-tidy
    pkgs.htop
    pkgs.httpie
    pkgs.httplab
    pkgs.jq
    pkgs.kakoune
    pkgs.nodePackages.typescript-language-server
    pkgs.nodejs
    pkgs.nixpkgs-fmt
    pkgs.redis
    pkgs.rnix-lsp
    pkgs.shellcheck
    pkgs.shfmt
    pkgs.sqlite-interactive
    pkgs.tree
    pkgs.tree-sitter
    pkgs.vim
    pkgs.xxd
    pkgs.yq
  ] ++ (if pkgs.system == "aarch64-darwin" then [ ] else [
    # Drop non Apple Silicon compatible packages
    pkgs.gdb
    pkgs.ghidra-bin
  ]);

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;


  # NOTE: Copied from home-linux.nix
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

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
    '';
  };
  # programs.fish.enable = true;

  # Set Git commit hash for darwin-version.
  system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  users.users.vidbina = {
    home = "/Users/vidbina";
  };
}
