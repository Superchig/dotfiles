" Specify a directory for plugins, leaving it empty uses the default
" Default is ~/.config/nvim/plugged
call plug#begin()

" Make sure you use single quotes

" Alternatively,
" Plug 'jiangmiao/auto-pairs'
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'morhetz/gruvbox'
Plug 'scrooloose/nerdtree'
Plug 'artur-shaik/vim-javacomplete2'

" Initialize plugin system
call plug#end()

set number
set termguicolors
" Synchronize unnamed register with clipboard register
set clipboard^=unnamedplus
set completeopt-=preview

colorscheme gruvbox

" Split pane navigation shortcuts
map <C-h> <C-w>h
map <C-l> <C-w>l
map <C-j> <C-w>j
map <C-k> <C-w>k

" Make it easier to make new tabs
cabbrev tn tabnew

cabbrev ev e ~/.config/nvim/init.vim
cabbrev eb e ~/.bashrc
cabbrev soi source ~/.config/nvim/init.vim

" Changes the present working directory to the directory of the currently open
" file for all windows.
cabbrev updir cd %:p:h

autocmd Filetype bash,zsh set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd Filetype sh set autoindent noexpandtab tabstop=8 shiftwidth=8

autocmd Filetype java set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd FileType java setlocal omnifunc=javacomplete#Complete
