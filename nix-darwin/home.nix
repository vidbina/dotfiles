{ config, pkgs, lib, ... }:

{
  home.stateVersion = "23.05";

  home.packages = [ ];

  # NOTE: Copied from dev.nix
  # No corresponding option in nix-darwin, so we config this with hm
  programs.git = {
    enable = true;
    userName = "David Asabina";
    userEmail = "vid@bina.me";
    lfs.enable = true;
    extraConfig = {
      init = {
        defaultBranch = "main";
      };

      core = {
        editor = "nvim";
      };

      gpg = {
        program = "gpg2";
      };

      # NOTE: Commenting out to test if `git config -l` changes
      # sendemail = {
      #   annotate = true;
      #   smtpServer = "msmtp";
      #   smtpServerOption = "-a vidbina";
      # };
    };
  };
}
