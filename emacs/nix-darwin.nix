# Tangled from README.org
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    my-emacs
    mu

    python312

    libgccjit
    ripgrep
  ];

  nixpkgs.overlays = [
    # Overlay custom Emacs build into pkgs
    (self: super: {
      my-emacs = pkgs.emacs30.pkgs.withPackages (
        epkgs:
        (with epkgs; [
          notmuch
          vterm
          pdf-tools
        ])
      );
    })
  ];

  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };
}
