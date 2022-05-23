# Tangled from README.org
# Please modify by editing README.org and re-tangling to generate this nix file.
{ config, lib, pkgs, options, ... }:

{
  # Tangling individual dev tools through nix-devtools noweb reference
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

      sendemail = {
        annotate = true;
        smtpServer = "msmtp";
        smtpServerOption = "-a vidbina";
      };
    };
  };
  # Set global gitignore
  home.file = {
    ".config/git/ignore".source = config.lib.file.mkOutOfStoreSymlink ./git/ignore;
  };
}
