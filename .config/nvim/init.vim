" Specify a directory for plugins, leaving it empty uses the default
" Default is ~/.config/nvim/plugged
call plug#begin()

" Make sure you use single quotes

" Alternatively,
" Plug 'jiangmiao/auto-pairs'
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'morhetz/gruvbox'
Plug 'scrooloose/nerdtree'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'vim-airline/vim-airline'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'plasticboy/vim-markdown'

" Initialize plugin system
call plug#end()

set number
set termguicolors
" Synchronize unnamed register with clipboard register
set clipboard^=unnamedplus
set completeopt-=preview
set ignorecase
set nohlsearch

colorscheme gruvbox
hi Normal guibg=NONE ctermbg=NONE

" Split pane navigation shortcuts
map <C-h> <C-w>h
map <C-l> <C-w>l
map <C-j> <C-w>j
map <C-k> <C-w>k

" Disable highlighting after using gd
nnoremap <silent> gd gd:nohlsearch<cr>
nnoremap <silent> <C-n> :nohlsearch<cr>

" Make it easier to make new tabs
cabbrev tn tabnew
nmap gs :tabnew<cr>
nmap <S-k> gt
nmap <S-j> gT

" " Make it easier to make new buffers
" cabbrev tn enew
" nmap <S-t> :enew<cr>
" nmap gs :enew<cr>
"
" nmap gt :bnext<cr>
" nmap g<S-t> :bprevious<cr>

nmap <C-s> :w<esc>
imap <C-s> <C-o>:w<esc>

nmap <M-m> :make<esc><esc>
imap <M-m> <C-o>:make<esc>

" " Close the current buffer and move to the previous one
" " This replicates the idea of closing a tab
cabbrev bc :bp <bar> bd #<cr>

" Show all open buffers and their status
" nmap gl :ls<cr>

cabbrev ev e ~/.config/nvim/init.vim
cabbrev eb e ~/.bashrc
cabbrev soi source ~/.config/nvim/init.vim

cabbrev twospace set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

autocmd Filetype markdown nmap <buffer> <silent> <C-Left> :s/^  //<esc>
autocmd Filetype markdown imap <buffer> <silent> <C-Left> <C-o>:s/^  //<esc>

autocmd Filetype markdown nmap <buffer> <silent> <C-Right> :s/^/  /<esc>
autocmd Filetype markdown imap <buffer> <silent> <C-Right> <C-o>:s/^/  /<esc>

" Changes the present working directory to the directory of the currently open
" file for all windows.
cabbrev updir cd %:p:h

autocmd Filetype bash,zsh set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd Filetype sh set autoindent noexpandtab tabstop=8 shiftwidth=8

autocmd Filetype java set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd FileType java setlocal omnifunc=javacomplete#Complete

autocmd Filetype c set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

autocmd Filetype markdown set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd Filetype markdown set textwidth=80
autocmd Filetype markdown set colorcolumn=+0
let g:vim_markdown_new_list_item_indent = 2

autocmd Filetype yaml set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd Filetype yaml set colorcolumn=80

autocmd Filetype cpp set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

autocmd Filetype tex set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
autocmd Filetype tex set textwidth=80 colorcolumn=+0

autocmd BufWritePost config.h !sudo make install

" Automatically display all buffers when there's only one tab open
" let g:airline#extensions#tabline#enabled = 1

" Chagnges to make completion more like a typical IDE's completion
set completeopt=longest,menuone
inoremap <expr> <cr> pumvisible () ? "\<C-y>" : "\<C-g>u\<cr>"

inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

" function! OpenCompletion()
"     if !pumvisible() && ((v:char >= 'a' && v:char <= 'z') || (v:char >= 'A' && v:char <= 'Z')) && &ft=='go'
"         call feedkeys("\<C-x>\<C-o>", "n")
"     endif
" endfunction
"
" autocmd InsertCharPre * call OpenCompletion()
