" Tangled from README.org
call plug#begin("~/.local/share/nvim/plugged")
  Plug 'neovim/nvim-lspconfig'
  Plug 'airblade/vim-gitgutter'
  Plug 'aklt/plantuml-syntax'
  Plug 'dense-analysis/ale'
  Plug 'derekwyatt/vim-scala'
  Plug 'ElmCast/elm-vim'
  Plug 'godlygeek/tabular'
  Plug 'hashicorp/sentinel.vim'
  Plug 'hashivim/vim-terraform'
  "Plug 'https://github.com/hrother/offlineimaprc.vim.git'
  Plug 'https://github.com/junegunn/goyo.vim.git'
  Plug 'https://github.com/plasticboy/vim-markdown.git'
  Plug 'isRuslan/vim-es6'
  Plug 'jparise/vim-graphql'
  Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
  Plug 'LnL7/vim-nix'
  Plug 'majutsushi/tagbar'
  "https://github.com/neoclide/coc.nvim
  "Load VSCode extensions and host language servers
  "Plug 'neoclide/coc.nvim', { 'branch': 'release' }
  Plug 'preservim/nerdtree'
  Plug 'prettier/vim-prettier'
  Plug 'purescript-contrib/purescript-vim'
  Plug 'saltstack/salt-vim'
  "Plug 'sbdchd/neoformat'
  Plug 'sigmike/vim-taskjuggler'
  Plug 'tomlion/vim-solidity'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-scripts/openvpn'
  Plug 'wannesm/wmgraphviz.vim'
  Plug 'jeffkreeftmeijer/vim-dim', { 'branch': 'main' }
call plug#end()
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
