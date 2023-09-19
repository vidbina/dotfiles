# TODO: Tangle parts of the copied bits from source
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
      # Specify ./configuration.nix as module and pass inputs as arg
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
  };
}
