# Tangled from README.org
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    mu

    python312
  ];

  nixpkgs.overlays = [

  ];

  homebrew = {
    casks = [
      {
        name = "emacs";
      }
    ];
  };
}
