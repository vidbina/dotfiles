set tabstop=2    " tab stop to 2 spaces
set shiftwidth=2 " shift width by 2 spaces
set expandtab    " expand tabs to spaces
colorscheme molokai

call plug#begin("~/.local/share/nvim/plugged")
  Plug 'derekwyatt/vim-scala'
  Plug 'LnL7/vim-nix'
  Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
  Plug 'prettier/vim-prettier'
  Plug 'tpope/vim-fugitive'
  Plug 'majutsushi/tagbar'
  Plug 'airblade/vim-gitgutter'
  Plug 'vim-scripts/openvpn'
  Plug 'hashivim/vim-terraform'
  Plug 'saltstack/salt-vim'
  Plug 'isRuslan/vim-es6'
  Plug 'https://github.com/plasticboy/vim-markdown.git'
  Plug 'vim-airline/vim-airline'
  Plug 'https://github.com/junegunn/goyo.vim.git'
  Plug 'ElmCast/elm-vim'
  Plug 'sigmike/vim-taskjuggler'
  Plug 'jparise/vim-graphql'
  Plug 'purescript-contrib/purescript-vim'
  " Plug 'https://github.com/hrother/offlineimaprc.vim.git'
  " Plug 'sbdchd/neoformat'
call plug#end()

let g:vim_markdown_frontmatter = 1
let g:vim_markdown_math = 1
let g:vim_markdown_fenced_languages = ['nix=nix', 'Dockerfile=dockerfile']
let g:vim_markdown_new_list_item_indent = 2
let g:vim_markdown_auto_insert_bullets = 0

set mouse=a
":map <ScrollWheelUp> <C-Y>
":map <ScrollWheelDown> <C-E>
