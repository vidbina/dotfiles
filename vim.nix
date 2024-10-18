# Tangled from  README.org
{ config, pkgs, ... }:

{
  home.sessionVariables.EDITOR = "nvim";

  programs.neovim = {
    enable = true;

    plugins = with pkgs.vimPlugins; [
      # deoplete-notmuch
      goyo-vim
      neoformat
      nerdtree
      tabular
      tagbar
      vim-airline
      vim-dim
      vim-fugitive
      vim-gitgutter
    ];
    vimdiffAlias = true;
    withRuby = true;
    extraConfig = ''
      set tabstop=2    " tab stop to 2 spaces
      set shiftwidth=2 " shift width by 2 spaces
      set expandtab    " expand tabs to spaces
      set mouse=a
      set number
      ":map <ScrollWheelUp> <C-Y>
      ":map <ScrollWheelDown> <C-E>
      colorscheme dim

      let g:vim_markdown_frontmatter = 1
      let g:vim_markdown_math = 1
      let g:vim_markdown_fenced_languages = ['nix=nix', 'Dockerfile=dockerfile']
      let g:vim_markdown_new_list_item_indent = 2
      let g:vim_markdown_auto_insert_bullets = 0

      set wrap
      set ignorecase
      set nofoldenable
    '';
    extraLuaConfig = ''
      require'lspconfig'.gleam.setup{}
    '';
  };
}
