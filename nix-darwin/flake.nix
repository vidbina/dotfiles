# TODO: Tangle parts of the copied bits from source
{
  description = "Example Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
    let
      hmConfig = {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.vidbina = {
          home.username = "vidbina";
          home.stateVersion = "23.05";

          # NOTE: Copied from dev.nix
          # No corresponding option in nix-darwin, so we config this with hm
          programs.git = {
            enable = true;
            userName = "David Asabina";
            userEmail = "vid@bina.me";
            lfs.enable = true;
            extraConfig = {
              init = {
                defaultBranch = "main";
              };

              core = {
                editor = "nvim";
              };

              gpg = {
                program = "gpg2";
              };

              # NOTE: Commenting out to test if `git config -l` changes
              # sendemail = {
              #   annotate = true;
              #   smtpServer = "msmtp";
              #   smtpServerOption = "-a vidbina";
              # };
            };
          };

        }; # // import ../home-darwin.nix { inherit pkgs; lib = {}; };
      };

      # TODO: Isolate into separate file
      configuration = { pkgs, ... }: {
        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget
        environment.systemPackages =
          [
            pkgs.asciinema
            pkgs.checkmake
            pkgs.exercism
            pkgs.gdb
            pkgs.gh
            pkgs.ghidra-bin
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
            pkgs.redis
            pkgs.rnix-lsp
            pkgs.shellcheck
            pkgs.shfmt
            pkgs.sqlite-interactive
            pkgs.tree-sitter
            pkgs.vim
            pkgs.xxd
            pkgs.yq
          ];

        # Auto upgrade nix package and the daemon service.
        services.nix-daemon.enable = true;
        # nix.package = pkgs.nix;

        services.emacs = {
          enable = true;
          package = pkgs.emacs.pkgs.withPackages (epkgs: (
            with epkgs; [
              notmuch
              vterm
              pdf-tools
            ]
          ));
        };

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
          '';
        };
        # programs.fish.enable = true;

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 4;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "x86_64-darwin";

        users.users.vidbina = {
          home = "/Users/vidbina";
        };
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."simple" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
      };

      darwinConfigurations."Davids-MacBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [
          configuration
          home-manager.darwinModules.home-manager
          hmConfig
        ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."simple".pkgs;
    };
}
