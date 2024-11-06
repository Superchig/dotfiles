syntax on

" Normal mode => block cursor, insert mode => line cursor
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

set ttimeout
set ttimeoutlen=50

set incsearch

set number relativenumber

" Switch buffers even if a buffer has changes
set hidden

let mapleader = " "

nnoremap <leader><leader> :ls<cr>:e
