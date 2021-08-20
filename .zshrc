# vim: sw=2

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Basic settings taken from Arch wiki
autoload -Uz compinit promptinit
compinit
promptinit

# prompt adam1

# Use underscores (and other symbols) as word separators
autoload -U select-word-style
select-word-style bash

# History
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=${HOME}/.cache/zsh/history

if [ ! -f ${HISTFILE} ]; then
  mkdir -p ${HOME}/.cache/zsh
  touch ${HOME}/.cache/zsh/history
fi

# Completion information
zstyle ':completion:*' menu select list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion::complete:*' gain-privileges 1
setopt COMPLETE_ALIASES

# Don't put commands that start with a space in history
setopt histignorespace

# Bind shift-tab to tab backwards
bindkey '^[[Z' reverse-menu-complete

# Enable using just directory names to cd
setopt AUTO_CD

# History search enabled
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

# Set the name of the terminal to match the current working directory
chpwd() {
  window_title="\033]0;${PWD##*/}\007"
  echo -ne "$window_title"
}

export EDITOR=nvim
export VISUAL=${EDITOR}
export MANWIDTH=80

alias ls='ls --color=auto'
if [ -f /usr/bin/exa ]; then
  alias l='exa -la --group-directories-first'
else
  alias l='ls -lah'
fi

alias diff='diff --color=auto'

alias mv='mv --backup=numbered'
alias cp='cp --backup=numbered'

alias grep='grep --color=auto'

alias fzf="fzf --color='hl:yellow' --color='hl+:bright-yellow:bold'"

# alias xo='xdg-open &> /dev/null'
alias e=$VISUAL
alias se="sudo $VISUAL"
alias ez="e ~/.zshrc"
alias ef="e ~/.config/fish/config.fish"
alias eb="e ~/dotfiles/.bashrc"
alias ep="e ~/dotfiles/pacman-install.sh"
alias ei="e ~/.config/i3/config"
# alias en="e -u ~/dotfiles/.config/nvim/init.vim ~/tmp/zoom_items.md"
alias en="e ~/tmp/zoom_items.md"
alias ecl="emacsclient"
alias ea="e ~/.config/awesome/rc.lua"

alias fr="history | sk | cut -c 8-"
alias lg="lazygit"

alias pe="ps -e | grep"
alias pi="pacman -Qi | grep Description"
alias sps="sudo pacman -S"
alias spr="sudo pacman -R"
alias spk="sudo pacman -D --asexplicit"

alias mo="$HOME/dotfiles/minimalist/monitor_setup.bash"

alias orphans="sudo pacman -Rns $(pacman -Qtdq)"

alias gimme="sudo chown ${USER} *"

alias cdc="cd $HOME/Documents/CPSC_Courses/cpsc231_a1/"
alias cdo="cd $HOME/Downloads"
alias cdd="cd $HOME/dotfiles"
alias cds="cd $HOME/school/sophomore_fall"

alias activate="source env/bin/activate"
alias sz='source ~/.zshrc'

alias ssy="sudo systemctl"

alias gch="git checkout"
alias gb="git branch"

alias cgc="cargo check --color=always 2>&1 | head -n 25"
alias cgr="RUST_BACKTRACE=1 cargo run"
alias cgt="RUST_BACKTRACE=1 cargo test"
alias cgd="PURE_PYTHON=1 CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUNNER=rust-gdb cargo test"

export PATH="$PATH:$HOME/bin:$HOME/.local/bin"

export MANPATH="$MANPATH:/home/chiggie/.ghcup/share/man"

# Colored man output
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

# nnn settings
export NNN_FIFO="/tmp/nnn.fifo"
export NNN_PLUG="p:preview-tui"
# Open text files in $VISUAL -> $EDITOR -> vi
alias nnn="nnn -e"
# Start the preview-tui plugin automatically startup (toggled with ;p)
# alias nnn="nnn -e -P p"


# Source POSIX-compliant scripts
for FILE in $HOME/.config/zsh/scripts/*; do
  source $FILE
done

# Establishes lfcd as widget for zle that calls a shell function named lfcd
# zle -N lfcd lfcd
# bindkey -s '^O' '^Ulfcd^M'

rf() {
  rfcd < $TTY
  zle accept-line
}

zle -N rfcd rf
# bindkey -s '^O' '^Urfcd^M'
bindkey '^O' rfcd

# nv() { nvim }
# zle -N nv nv
# bindkey '^W' nv
lfcd_tty() {
  lfcd < $TTY
  zle accept-line
}
zle -N lfcd lfcd_tty
bindkey '^W' lfcd

fg_top() {
  fg %1
}
zle -N fg_top fg_top
bindkey '^Z' fg_top

# On Arch Linux, installed via packages
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# Bind history-substring-search plugin's keys
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Set the title of the terminal to the current working directory on startup
chpwd

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
source ~/.rvm/scripts/rvm # Rvm is now a function

if [ -f /usr/bin/go ]; then
  export PATH="$PATH:$(go env GOPATH)/bin"
fi


# export HOSTNAME=$(hostname)

# _rvm_completion() {
#   source $rvm_path/"scripts/zsh/Completion/_rvm"
# }
# compdef _rvm_completion rvm

# Just use the default powerlevel10k bar
export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

# opam configuration
test -r /home/chiggie/.opam/opam-init/init.zsh && . /home/chiggie/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
