# Tangled from emacs/README.org
{ config, pkgs, username, ... }:

{
  environment.systemPackages = with pkgs; [
    yamllint
    nodePackages.yaml-language-server
    my-emacs
    mu

    python3

    libgccjit
    (pkgs.writeShellScriptBin "gnoo" ''
      # Check if daemon is running
      if ${pkgs.my-emacs}/bin/emacsclient -e "(+ 1 1)" >/dev/null 2>&1; then
        echo "Daemon running: connecting through emacsclient"
        exec ${pkgs.my-emacs}/bin/emacsclient -n -c "$@"
      else
        echo "No daemon: starting regular Emacs"
        exec ${pkgs.my-emacs}/bin/emacs "$@"
      fi
    '')
    ripgrep
  ];

  nixpkgs.overlays = [
    # Overlay custom Emacs build into pkgs
    (self: super: {
      my-emacs = super.emacs30.pkgs.withPackages (epkgs: (
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
