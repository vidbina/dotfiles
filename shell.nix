# From https://github.com/ryantm/home-manager-template

let

  sources = import ./nix/sources.nix;

  nixpkgs = sources."nixpkgs";
  home-manager = sources."home-manager";

  pkgs = import nixpkgs { };

in
pkgs.mkShell rec {

  name = "home-manager-shell";

  buildInputs = with pkgs; [
    niv
    (import sources.home-manager { inherit pkgs; }).home-manager

    cacert # to resolve CA cert issue
    git
    nix
    ncurses # to resolve tput issue
    which
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${nixpkgs}:home-manager=${home-manager}"
    export HOME_MANAGER_CONFIG="./home.nix"
  '';
}
