# Tangled from README.org
{ config, pkgs, username, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    my-emacs
    mu

    python3

    libgccjit
    ripgrep
  ];

  nixpkgs.overlays = [
    # Overlay custom Emacs build into pkgs
    (self: super: {
      my-emacs = pkgs.emacs30.pkgs.withPackages (epkgs: (
        with epkgs; [
          notmuch
          vterm
          pdf-tools
        ]
      ));
    })
  ];

  launchd.user.agents.my-emacs = {
    serviceConfig = {
      ProgramArguments = [
        "${pkgs.my-emacs}/bin/emacs"
        "--fg-daemon"
      ];

      # Enable logging for debugging
      StandardOutPath = "/Users/${username}/Library/Logs/my-emacs.stdout.log";
      StandardErrorPath = "/Users/${username}/Library/Logs/my-emacs.stderr.log";

      # Keep daemon running
      RunAtLoad = true;
      KeepAlive = true;
      ProcessType = "Interactive";

      # EnrivonmentVariables = {}
    };

    path = [
      config.environment.systemPath
    ];
  };
  environment.variables.EMACS_SOCKET_NAME = "/tmp/my-emacs/socket/server";

}
