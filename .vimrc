syntax on

set noerrorbells
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'junegunn/goyo.vim'
Plug 'tomasiser/vim-code-dark'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Nerd Tree Plugins
Plug 'preservim/nerdtree'
" Plug 'tsony-tsonev/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'ryanoasis/vim-webdevicons'
Plug 'scrooloose/nerdcommenter'

Plug 'jremmen/vim-ripgrep'
Plug 'tpope/vim-fugitive'
Plug 'leafgarland/typescript-vim'
Plug 'vim-utils/vim-man'
Plug 'lyuts/vim-rtags'
"Plug 'git@github.com:kien/ctrlp.vim.git'
"Plug 'git@github.com:ycm-core/YouCompleteMe.git'

"Plug 'mbbill/undotree'
Plug 'junegunn/fzf', {'do': { -> fzf#install()}}
Plug 'junegunn/fzf.vim'


call plug#end()

set guifont=JetBrains\ Mono\ 11
let g:airline_powerline_fonts = 1
set encoding=UTF-8
colorscheme codedark
set background=dark

if executable('rg')
    let g:rg_derive_root='true'
endif

let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

let mapleader=" "
let g:netrw_browse_split=2
let g:netrw_banner = 0

"ag is fast enough that CtrlP doesn't need to cache "
let g:ctrlp_use_caching = 0
let g:netrw_winsize = 25

nnoremap <leader>wh :wincmd h<CR>
nnoremap <leader>wj :wincmd j<CR>
nnoremap <leader>wk :wincmd k<CR>
nnoremap <leader>wl :wincmd l<CR>
nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>ps :Rg<SPACE>
nnoremap <silent> <Leader>+ :vertical resize +5<CR>
nnoremap <silent> <Leader>- :vertical resize -5<CR>
nnoremap <leader>op :NERDTreeToggle<CR>
nnoremap <silent> <Leader>gd :YcmCompleter GoTo<CR>
nnoremap <silent> <Leader>gf :YcmCompleter FixIt<CR>

