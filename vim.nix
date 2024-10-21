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
      nvim-lspconfig
      nvim-treesitter.withAllGrammars
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
      " https://vi.stackexchange.com/a/45130
      set notermguicolors
      set t_Co=16


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
      -- Float diagnostics window through custom mapping
      vim.keymap.set('n', '<Leader>i', function() vim.diagnostic.open_float(nil, {focus=false, scope='cursor'}) end)
      require'nvim-treesitter.configs'.setup{
        highlight = {
          enable = true,
          -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
          -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
          -- Using this option may slow down your editor, and you may see some duplicate highlights.
          -- Instead of true it can also be a list of languages
          additional_vim_regex_highlighting = false,
        },
      }
    '';
  };
}
