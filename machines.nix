# Per-machine configuration. Copy or symlink to machines.nix.
# This is the single place to configure user and path settings.
[
  {
    name = "berlin26-m5pro";
    system = "aarch64-darwin";
    username = "vidbina";
    dotfilesPath = "/Users/vidbina/Code/vidbina/dotfiles";
    extraModules = [
      {
        networking.hostName = "berlin26-m5pro";
        ids.gids.nixbld = 350;
        system.stateVersion = 5;
      }
    ];
  }
  {
    name = "tokyo23-m2";
    system = "aarch64-darwin";
    username = "vidbina";
    dotfilesPath = "/Users/vidbina/Code/vidbina/dotfiles";
    extraModules = [ ];
  }
  {
    name = "berlin-4corei7";
    system = "x86_64-darwin";
    username = "vidbina";
    dotfilesPath = "/Users/vidbina/Code/vidbina/dotfiles";
    extraModules = [ ];
  }
]
