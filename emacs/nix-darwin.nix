# Tangled from README.org
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    mu
  ];

  nixpkgs.overlays = [

  ];

  homebrew = {
    taps = [ "railwaycat/emacsmacport" ];
    brews = [
      {
        name = "emacs-mac";
        args = [ "with-no-title-bars" ];
        link = true;
      }
    ];
  };
}
