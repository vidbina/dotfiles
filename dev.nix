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

  home.packages = [
    pkgs.gh
    pkgs.xxd
    pkgs.hexyl
    pkgs.ghidra-bin
    pkgs.kakoune
    pkgs.shellcheck
    pkgs.shfmt
    pkgs.asciinema
    pkgs.exercism
    pkgs.html-tidy
    pkgs.httpie
    pkgs.httplab
    pkgs.gdb
    pkgs.checkmake
    #pkgs.cmakeCurses
    pkgs.gnumake
    pkgs.glibc
    pkgs.rnix-lsp
    pkgs.nodePackages.typescript-language-server
    pkgs.tree-sitter
    pkgs.jq
    pkgs.yq
    pkgs.sqlite-interactive
    pkgs.redis
    pkgs.evemu
  ];
}
