set tabstop=2    " tab stop to 2 spaces
set shiftwidth=2 " shift width by 2 spaces
set expandtab    " expand tabs to spaces
colorscheme molokai

call plug#begin("~/.local/share/nvim/plugged")
  Plug 'airblade/vim-gitgutter'
  Plug 'derekwyatt/vim-scala'
  Plug 'ElmCast/elm-vim'
  Plug 'godlygeek/tabular'
  Plug 'hashivim/vim-terraform'
  " Plug 'https://github.com/hrother/offlineimaprc.vim.git'
  Plug 'https://github.com/junegunn/goyo.vim.git'
  Plug 'https://github.com/plasticboy/vim-markdown.git'
  Plug 'isRuslan/vim-es6'
  Plug 'jparise/vim-graphql'
  Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
  Plug 'LnL7/vim-nix'
  Plug 'majutsushi/tagbar'
  Plug 'prettier/vim-prettier'
  Plug 'purescript-contrib/purescript-vim'
  Plug 'saltstack/salt-vim'
  " Plug 'sbdchd/neoformat'
  Plug 'sigmike/vim-taskjuggler'
  Plug 'tpope/vim-fugitive'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-scripts/openvpn'
call plug#end()

let g:vim_markdown_frontmatter = 1
let g:vim_markdown_math = 1
let g:vim_markdown_fenced_languages = ['nix=nix', 'Dockerfile=dockerfile']
let g:vim_markdown_new_list_item_indent = 2
let g:vim_markdown_auto_insert_bullets = 0

set mouse=a
set number
":map <ScrollWheelUp> <C-Y>
":map <ScrollWheelDown> <C-E>

set nowrap
