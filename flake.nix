# TODO: Tangle parts of the copied bits from source
# The idea is that my dotfiles becomes the one-stop shop for my entire config
# setup. No more will I work with separate nixos-configuration and home-manager
# setups.
{
  description = "David's Darwin (macOS) system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    linsk.url = "github:vidbina/linsk/vid/init-nix-flake";
    linsk.inputs.nixpkgs.follows = "nixpkgs";
    vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    vscode-extensions.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, ... }:
    let
      machines = [
        { name = "tokyo23-m2"; system = "aarch64-darwin"; username = "vidbina"; }
        { name = "berlin-4corei7"; system = "x86_64-darwin"; username = "vidbina"; }
      ];

      darwinConfigurationFor = machine: nix-darwin.lib.darwinSystem {
        inherit (machine) system;

        # See https://github.com/LnL7/nix-darwin#using-flake-inputs
        specialArgs = {
          inherit inputs;
          # TODO: Refactor to DRY-up shared specialArgs use across configs
          inherit (machine) username;
        };

        modules = [
          ./configuration-darwin.nix
          # https://nix-community.github.io/home-manager/nix-darwin-options.html
          home-manager.darwinModules.home-manager
          {
            home-manager.backupFileExtension = "backup";
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${machine.username} = import ./home-darwin.nix;
            home-manager.verbose = true;
          }
        ];
      };
    in
    {
      darwinConfigurations =
        builtins.listToAttrs (map
          (machine: {
            inherit (machine) name;
            value = darwinConfigurationFor machine;
          })
          machines);

      # TODO: Bring Linux configuration into scope
      # See https://github.com/vidbina/nixos-configuration
      # nixosConfigurations.""
    };
}
