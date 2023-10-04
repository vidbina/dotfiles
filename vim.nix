# Tangled from  README.org
{ config, pkgs, ... }:

{
  home.sessionVariables.EDITOR = "nvim";

  programs.neovim = {
    enable = true;

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
