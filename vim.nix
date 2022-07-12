# Tangled from  README.org
{ config, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    # Warning: Just bailed on init.vim and opted for nix so, WIP!

    #extraConfig = builtins.readFile (./. + "/nvim/init.vim");
    plugins = with pkgs.vimPlugins; let
      #sentinel-vim = pkgs.vimUtils.buildVimPlugin {
      #  name = "sentinel-vim";
      #  src = pkgs.fetchFromGitHub {
      #    owner = "hashicorp";
      #    repo = "sentinel.vim";
      #    rev = "main";
      #    sha256 = pkgs.lib.fakeSha256;
      #  };
      #};
    in
    [
      #{
      #  plugin = vim-plug;
      #  optional = false;
      #}

      #'https://github.com/hrother/offlineimaprc.vim.git'
      #'isRuslan/vim-es6'
      #'jeffkreeftmeijer/vim-dim', { 'branch': 'main' }
      #'sigmike/vim-taskjuggler'
      #'vim-scripts/openvpn'
      #ale
      #vim-scala
      coc-nvim
      elm-vim
      goyo-vim
      neoformat
      nerdtree
      nvim-treesitter
      orgmode
      plantuml-syntax
      #sentinel-vim
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
