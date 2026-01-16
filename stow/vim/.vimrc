syntax on

" Normal mode => block cursor, insert mode => line cursor
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

set timeout ttimeout timeoutlen=0 ttimeoutlen=50

set incsearch

set number relativenumber

" Switch buffers even if a buffer has changes
set hidden

let mapleader = " "

nnoremap <leader><leader> :ls<cr>:e
nnoremap Y y$

autocmd FileType json setlocal expandtab softtabstop=2 shiftwidth=2
autocmd FileType json setlocal autoindent nosmartindent nocindent
autocmd FileType json if getfsize(@%) > 1000000 | setlocal syntax=OFF | endif

autocmd FileType pdf setlocal expandtab softtabstop=2 shiftwidth=2
autocmd FileType pdf setlocal autoindent nosmartindent nocindent

autocmd FileType zig setlocal expandtab softtabstop=4 shiftwidth=4
autocmd FileType zig setlocal autoindent nosmartindent nocindent
