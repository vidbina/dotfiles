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
          #home.homeDirectory = "/Users/vidbina";
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
            pkgs.vim
            pkgs.gh
            pkgs.xxd
            pkgs.hexyl
            pkgs.ghidra-bin
            pkgs.kakoune
            pkgs.shellcheck
            pkgs.shfmt
            pkgs.asciinema
            pkgs.exercism
            pkgs.html-tidy
            pkgs.httpie
            pkgs.httplab
            pkgs.gdb
            pkgs.checkmake
            pkgs.gnumake
            pkgs.rnix-lsp
            pkgs.nodejs
            pkgs.nodePackages.typescript-language-server
            pkgs.tree-sitter
            pkgs.jq
            pkgs.yq
            pkgs.sqlite-interactive
            pkgs.redis
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
        programs.zsh.enable = true; # default shell on catalina
        # programs.fish.enable = true;

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 4;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "x86_64-darwin";
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."simple" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
      };

      darwinConfigurations."Davids-MacBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [ home-manager.darwinModules.home-manager configuration ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."simple".pkgs;
    };
}
