# From https://github.com/ryantm/home-manager-template

let

  sources = import ./nix/sources.nix;

  nixpkgs-src = sources."nixpkgs";
  hm-src = sources."home-manager";
  nur-src = sources."NUR";

  pkgs = import nixpkgs-src { };

in
pkgs.mkShell rec {

  name = "home-manager-shell";

  buildInputs = with pkgs; [
    niv
    (import hm-src { inherit pkgs; }).home-manager

    cacert # to resolve CA cert issue
    git
    nix
    ncurses # to resolve tput issue
    which
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${nixpkgs-src}:home-manager=${hm-src}:NUR=${nur-src}"
    export HOME_MANAGER_CONFIG="./home.nix"
  '';
}
