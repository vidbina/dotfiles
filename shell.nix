# Tangled from README.org
# From https://github.com/ryantm/home-manager-template

let
  sources = import ./nix/sources.nix;

  nixpkgs-src = sources."nixpkgs";
  pkgs = import nixpkgs-src { };

  hm-src = sources."home-manager";
  nur-src = sources."NUR";

  nixpkgs-bleeding-src = sources."nixpkgs-bleeding";
  pkgs-bleeding = import nixpkgs-bleeding-src { };

in
pkgs.mkShell rec {
  name = "home-manager-shell";

  buildInputs = with pkgs; [
    pkgs-bleeding.niv
    pkgs-bleeding.nixVersions.nix_2_13
    (import hm-src { inherit pkgs; }).home-manager
    cacert # to resolve CA cert issue
    hello
    git
    ncurses # to resolve tput issue
    which
  ];

  shellHook = with pkgs; let
    system = (
      if stdenv.isLinux
      then "./home-linux.nix"
      else
        (if stdenv.isDarwin
        then "./home-darwin.nix"
        else "./home.nix")
    );
  in
  ''
    export NIX_PATH="nixpkgs=${nixpkgs-src}:home-manager=${hm-src}:NUR=${nur-src}"
    export HOME_MANAGER_CONFIG=${system}
  '';
}
