# Tangled from README.org
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    mu
  ];

  # https://www.aidanscannell.com/post/setting-up-an-emacs-playground-on-mac/

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
