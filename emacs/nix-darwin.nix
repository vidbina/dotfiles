# Tangled from README.org
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    mu

    python312
  ];

  nixpkgs.overlays = [

  ];

  # based on https://github.com/LnL7/nix-darwin/blob/0dd382b70c351f528561f71a0a7df82c9d2be9a4/modules/services/emacs.nix#L45-L50
  launchd.user.agents.my-emacs = {
    path = [ config.environment.systemPath ];
    serviceConfig = {
      ProgramArguments = [ "${pkgs.my-emacs}/bin/emacs" "--fg-daemon=main" ];
      RunAtLoad = true;
      StandardErrorPath = "/tmp/emacs.stderr.log";
      StandardOutPath = "/tmp/emacs.stdout.log";
    };
  };
}
