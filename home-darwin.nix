{ config, pkgs, lib, ... }:

{
  home.stateVersion = "23.05";

  home.packages = with pkgs; [ qemu ];


  # home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;
  # TODO: Fix hack of hardcoded dotfiles path
  # NOTE: This repo must be checked out to ~/Code/vidbina/dotfiles
  # A hardcoded .emacs.d source is used because mkOutOfStoreSymlink ./emacs
  # does not seem to work on macOS.
  # See https://discourse.nixos.org/t/accessing-home-manager-config-in-flakes/19864/8
  # See https://github.com/nix-community/home-manager/issues/2085#issuecomment-861427318
  home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Code/vidbina/dotfiles/emacs";

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

  # NOTE: Copied from vim.nix
  programs.neovim = {
    enable = true;
    # Warning: Just bailed on init.vim and opted for nix so, WIP!
    plugins = with pkgs.vimPlugins; [
      coc-nvim
      deoplete-notmuch
      elm-vim
      goyo-vim
      neoformat
      nerdtree
      nvim-treesitter
      orgmode
      plantuml-syntax
      tabular
      tagbar
      typescript-vim
      vim-airline
      vim-fugitive
      vim-gitgutter
      vim-graphql
      vim-markdown
      vim-nix
      vim-prettier
      vim-solidity
      vim-terraform
      wmgraphviz-vim
    ];
    vimdiffAlias = true;
    withRuby = true;
  };
}
