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
    # Overlay custom Emacs build into pkgs
    (self: super: {
      my-emacs = pkgs.emacs29-macport.pkgs.withPackages (epkgs: (
        with epkgs; [
          notmuch
          vterm
          pdf-tools
        ]
      ));
    })
  ];

  homebrew = {
    taps = [ "d12frosted/emacs-plus" ];
    brews = [
      {
        name = "emacs-plus@30";
        args = [

        ];
        link = true;
      }
    ];
  };
}
