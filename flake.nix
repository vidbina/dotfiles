{
  description = "Configuration for Emacs, URxvt and more";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      home.file.".dictrc" = ./dict/dictrc;

      home.file.".config/kitty/kitty.conf" = ./kitty/kitty.conf;

      programs = {
      };
    });
}
