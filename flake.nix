# TODO: Tangle parts of the copied bits from source
# The idea is that my dotfiles becomes the one-stop shop for my entire config
# setup. No more will I work with separate nixos-configuration and home-manager
# setups.
{
  description = "David's Darwin (macOS) system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }: {
    # Build with: nix run nix-darwin -- switch --flake ./nix-darwin/ --show-trace
    darwinConfigurations."Davids-MacBook-Pro" = nix-darwin.lib.darwinSystem {
      system = "x86_64-darwin";

      # See https://github.com/LnL7/nix-darwin#using-flake-inputs
      specialArgs = { inherit inputs; };
      modules = [
        ./configuration-darwin.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.vidbina = import ./home-darwin.nix;
        }
      ];
    };

    darwinConfigurations."tokyo23" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";

      # See https://github.com/LnL7/nix-darwin#using-flake-inputs
      specialArgs = { inherit inputs; };
      modules = [
        ./configuration-darwin.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.vidbina = import ./home-darwin.nix;
        }
      ];
    };

    # TODO: Bring Linux configuration into scope
    # See https://github.com/vidbina/nixos-configuration
    # nixosConfigurations.""
  };
}
