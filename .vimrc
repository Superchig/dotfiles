set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" git repos on your local machine (i.e. when working on your own plugin)
" // Commented out because I don't wish to use it
" Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}

" Here we start (superchig's) personal plugins (sorry for the third-person,
" future me)

" C++ highlighting!
Plugin 'octol/vim-cpp-enhanced-highlight'

" Gruvbox theme (Oh yeah!)
" Unfortunately enough, only for gVim so far
Plugin 'morhetz/gruvbox'
Plugin 'altercation/vim-colors-solarized'

" The ever-popular nerdtree plugin, allows for better file-management
Plugin 'scrooloose/nerdtree'

" BufExplorer, so I can manage my buffers more easily
Plugin 'jlanzarotta/bufexplorer'

" vim-autoclose, closetag, and vim-matchit, all nearly self-explanatory
" see http://www.terminally-incoherent.com/blog/2014/04/02/3-tiny-vim-plugins-that-will-make-your-life-easier/
Plugin 'Townk/vim-autoclose'
Plugin 'vim-scripts/closetag.vim'
Plugin 'edsono/vim-matchit'

" vim-airline, too see if I can get a nice, working statusline. Though the
" vanilla one is perfectly competent already.
Plugin 'bling/vim-airline'

" vim-pencil, to see if I can use vim for writing. That would be cool
Plugin 'reedes/vim-pencil'

" let's see if vim-wheel can help me a bit
Plugin 'reedes/vim-wheel'

" Hue! Time to configure stuff for... uh... something...
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType text			call pencil#init({'wrap': 'soft'})
augroup END

" Ha! Themes are overrated, anyway.
Plugin 'reedes/vim-thematic'

let g:thematic#themes = {
	  \ 'pencil_lite' :{ 'colorscheme': 'solarized',
	  \				  'background': 'light',
	  \  },
	  \ 'gvim' :{ 
	  \			'background:': 'dark',
	  \			'colorscheme': 'railscast',
	  \  },
	  \ 'terminal' :{ 'colorscheme': 'elflord',
	  \				'background': 'dark',
	  \  },
	  \}

" Web development plugins
Plugin 'mattn/emmet-vim'
Plugin 'skammer/vim-css-color'
Plugin 'hail2u/vim-css3-syntax'

" Ho! These are apparently must-haves. Looks like they'll come in hadny
Plugin 'tpope/vim-surround'

" For syntax, of course
Plugin 'scrooloose/syntastic'

" Some ruby stuff, though it will come in handy later...
Plugin 'tpope/vim-endwise'
Plugin 'skalnik/vim-vroom'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-rails'

" Open man pages in vim! It's amazing!
Plugin 'bruno-/vim-man'

" Hopefully, I'll be able to learn tmux someday
Plugin 'christoomey/vim-tmux-navigator'

" YouCompleteMe ! The thing that will probably help me plenty to write code
" Disabled due to the fact that I can't get that annoying error message to
" stop showing up.
" Plugin 'Valloric/YouCompleteMe'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" For lolcode!
Plugin 'Xe/lolcode.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
"
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

source $VIMRUNTIME/vimrc_example.vim
" Ho! Removed because I need ctrl-a for tmux
" source $VIMRUNTIME/mswin.vim
behave mswin
syntax on
set showcmd
set showmatch
set ignorecase
set smartcase
set incsearch
set autowrite
set hidden
set mouse=a
set exrc
set secure
set number
set makeprg=g++\ -o\ %<\ %
set shiftwidth=2
set softtabstop=2
set tabstop=4
autocmd Filetype html setlocal ts=2 sts=2 shiftwidth=2 smartindent
autocmd Filetype css setlocal ts=2 sts=2 shiftwidth=2
autocmd Filetype rb setlocal ts=2 sts=2 shiftwidth=2
autocmd Filetype cpp setlocal ts=4 sts=4 shiftwidth=4
autocmd Filetype java setlocal ts=4 sts=4 shiftwidth=4
autocmd Filetype txt setlocal ts=4 sts=4 spell spelllang=en_us

" Gah! vim-closetag, why u no work?
if !exists("b:unaryTagsStack") || exists("b:closetag_html_style")
  if &filetype == "html" || exists("b:closetag_html_style")
	let b:unaryTagsStacktack="area base br dd dt hr img input link meta param"
  else " for xml and xsl
	let b:unaryTagsStack=""
  endif
endif

if !exists("b:unaryTagsStack")
  let b:unaryTagsStack=""
endif

let b:unaryTagsStack=""

" Remaps and shortcuts
nmap <S-Enter> O<Esc>
nmap <CR> o<Esc>

" Leader remapping...
let mapleader = ","

" So I can use visual block
nnoremap <TAB> <C-Q>

" Theme-type stuff
set background=dark
if has('gui_running')
  " Maybe blackbeauty, breeze, bluegreen, or more? 
  let g:thematic#theme_name = 'gvim'
  set guifont=Inconsolata\ Medium\ 12
endif

set diffexpr=MyDiff()
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
	if &sh =~ '\<cmd'
	  let cmd = '""' . $VIMRUNTIME . '\diff"'
	  let eq = '"'
	else
	  let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
	endif
  else
	let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction
