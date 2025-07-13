# vim: sw=2

if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
  BREW_PREFIX="$(brew --prefix)"
fi

if [ -f "/usr/local/bin/brew" ]; then
  BREW_PREFIX="$(brew --prefix)"
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

UNAME="$(uname)"

# Basic settings taken from Arch wiki
autoload -Uz compinit promptinit
compinit
promptinit

# prompt adam1

autoload -U bashcompinit
bashcompinit

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
# zstyle ':completion:*' menu select list-colors "${(@s.:.)LS_COLORS}"
# NOTE(Chris): I should be able to use the line above. However, for a reason I
# don't entirely understand, this currently breaks autocompletion (something's
# going on with my LS_COLORS).
# In any case, the line below will work, albeit at the cost of losing color
# functionality for the currently-selected tab-complete item.
zstyle ':completion:*' menu select list-colors
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion::complete:*' gain-privileges 1
setopt COMPLETE_ALIASES

# Show dotfiles when using tab-completion
# As per https://unix.stackexchange.com/questions/308315/how-can-i-configure-zsh-completion-to-show-hidden-files-and-folders
_comp_options+=(globdots)

# Improve ssh completion
# https://stackoverflow.com/questions/54309712/zsh-doesnt-autocomplete-correctly-my-ssh-command
zstyle ':completion:*:(scp|rsync):*' tag-order ' hosts:-ipaddr:ip\ address hosts:-host:host files'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

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
export PAGER='less -S' # Useful for the pager in psql

export PATH="$HOME/bin:$PATH"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.cargo/bin"

# Set up gcr-ssh-agent
# Requires `systemctl enable --user gcr-ssh-agent.socket`
# See https://old.reddit.com/r/archlinux/comments/1aq97m8/gnomekeyring_14601_in_extratesting_disables/
if [ "$UNAME" = Linux ]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"
fi

# Use emacs keymap in zsh, even if EDITOR is set to vi or vim
bindkey -e

alias ls='ls --color=auto -F'
if command -v eza > /dev/null; then
  alias l='eza -la --group-directories-first'
else
  alias l='ls -lah'
fi

if [ -z "$BREW_PREFIX" ]; then
  alias diff='diff --color=auto'
fi

if [ -z "$BREW_PREFIX" ]; then
  alias mv='mv --backup=numbered'
  alias cp='cp --backup=numbered'
fi

alias grep='grep --color=auto'

alias fzf="fzf --color='hl:yellow' --color='hl+:bright-yellow:bold'"

if command -v helix > /dev/null; then
  alias hx='helix'
fi

if [ "$UNAME" = "Darwin" ]; then
  alias k9s="k9s --context staging"
fi

if [ "$UNAME" = "Linux" ]; then
  alias open="fork xdg-open"
fi

# alias xo='xdg-open &> /dev/null'
alias e=$VISUAL
alias n=neovide
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
alias eh="e ~/.config/hypr/hyprland.conf"

alias fr="history | sk | cut -c 8-"
alias lg="lazygit"

if command -v pacman-less 2>&1 > /dev/null; then
  alias pacman="pacman-less"
fi

alias pe="ps -e | grep"
alias pi="pacman -Qi | grep Description"
alias sps="sudo pacman -S"
alias spr="sudo pacman -R"
alias spk="sudo pacman -D --asexplicit"

alias mo="$HOME/dotfiles/multi/monitor_setup"

if [ -n "$BREW_PREFIX" ]; then
  alias rubymine="open -a 'RubyMine.app'"
fi

alias gimme="sudo chown ${USER} *"

alias cdc="cd $HOME/Documents/CPSC_Courses/cpsc231_a1/"
alias cdo="cd $HOME/Downloads"
alias cdd="cd $HOME/dotfiles"
alias cds="cd $HOME/school/sophomore_fall"

alias activate="source env/bin/activate"
alias sz='source ~/.zshrc'

alias ssy="sudo systemctl"

alias gca="git commit --amend"
alias gch="git checkout"
alias gb="git branch"
alias gits="git stash --include-untracked"

alias cgc="cargo check --color=always 2>&1 | head -n 25"
alias cgb="cargo build"
alias cgr="RUST_BACKTRACE=1 cargo run"
alias cgt="RUST_BACKTRACE=1 cargo test"
alias cgd="PURE_PYTHON=1 CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_RUNNER=rust-gdb cargo test"

export MANPATH="$MANPATH:/home/chiggie/.ghcup/share/man"

# Colored man output
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'
export GROFF_NO_SGR=1

# Light/dark theme settings
if [ "$UNAME" = "Darwin" ] && [ "$TERM" != "xterm-ghostty" ]; then
  echo "should not be in ghostty" > /tmp/ghostty.log

  export BAT_THEME="gruvbox-light"
  export MCFLY_LIGHT="TRUE"
  if command -v vivid 2>&1 > /dev/null; then
    export LS_COLORS="$(vivid generate one-light-modified)"
  fi
elif [ "$UNAME" = "Darwin" ] && [ "$TERM" = "xterm-ghostty" ]; then
  export BAT_THEME="ansi"
  if command -v vivid 2>&1 > /dev/null; then
    export LS_COLORS="$(vivid generate tokyonight-moon)"
  fi
elif [ "$UNAME" = "Linux" ]; then
  case "$TERM" in
    "xterm-kitty")
      export BAT_THEME="gruvbox-dark"
      # export MCFLY_LIGHT="TRUE"
      if command -v vivid 2>&1 > /dev/null; then
        export LS_COLORS="$(vivid generate gruvbox-dark)"
      fi
      ;;
    "xterm-ghostty")
      export BAT_THEME="ansi"
      if command -v vivid 2>&1 > /dev/null; then
        export LS_COLORS="$(vivid generate tokyonight-storm)"
      fi
      ;;
  esac
fi

if [ "$UNAME" = "Darwin" ]; then
  export JAVA_HOME="$(/usr/libexec/java_home)"
fi

# Mclfy settings

export MCFLY_FUZZY=2
export MCFLY_RESULTS=500

# nnn settings
export NNN_FIFO="/tmp/nnn.fifo"
export NNN_PLUG="p:preview-tui"
# Open text files in $VISUAL -> $EDITOR -> vi
alias nnn="nnn -e"
# Start the preview-tui plugin automatically startup (toggled with ;p)
# alias nnn="nnn -e -P p"

# Copy completions for ripgrep into rgl script

if command -v rg 2>&1 > /dev/null; then
  compdef rgl=rg
fi

# Source POSIX-compliant scripts
if [ -d "$HOME/.config/zsh/scripts" ]; then
  for FILE in $HOME/.config/zsh/scripts/*; do
    source $FILE
  done
fi

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
source_if() {
  if [ -f "$1" ]; then
    source "$1"
  fi
}

if [ -n "$BREW_PREFIX" ]; then
  PLUGINS="$BREW_PREFIX/share"
  source_if "$PLUGINS"/powerlevel10k/powerlevel10k.zsh-theme
else
  PLUGINS=/usr/share/zsh/plugins
  source_if /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
fi

source_if $PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source_if $PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh
source_if $PLUGINS/zsh-history-substring-search/zsh-history-substring-search.zsh

# Bind history-substring-search plugin's keys
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Bind Ctrl-X Ctrl-E to editing the current line in a text editor, like in bash
# Modified from https://stackoverflow.com/questions/890620/unable-to-have-bash-like-c-x-e-in-zsh
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Set the title of the terminal to the current working directory on startup
chpwd

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

if command -v go 2>&1 > /dev/null; then
  export PATH="$PATH:$(go env GOPATH)/bin"
fi

export PATH="$PATH:/opt/shader-slang-bin/bin"

# export HOSTNAME=$(hostname)

# Just use the default powerlevel10k bar
export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

# opam configuration
test -r /home/chiggie/.opam/opam-init/init.zsh && . /home/chiggie/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

if command -v zoxide > /dev/null; then
  eval "$(zoxide init zsh --cmd j)"
fi

if command -v zoxide > /dev/null; then
  eval "$(mcfly init zsh)"
fi

# Source any local (machine-specific) configuration files
if [ -d "$HOME/.config/zsh/local" ]; then
  for FILE in $HOME/.config/zsh/local/*(D); do
    source $FILE
  done
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$("$HOME/miniconda3/bin/conda" 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "$HOME/miniconda3/etc/profile.d/conda.sh" ]; then
#         . "$HOME/miniconda3/etc/profile.d/conda.sh"
#     else
#         export PATH="/home/chiggie/miniconda3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

# https://github.com/Schniz/fnm
if command -v fnm 2>&1 > /dev/null; then
  eval "$(fnm env --use-on-cd)"
fi

# https://github.com/TaKO8Ki/frum
if command -v frum 2>&1 > /dev/null; then
  eval "$(frum init)"
fi

# Add to path for flutterfire
export PATH="$PATH":"$HOME/.pub-cache/bin"

if command -v rockie 2>&1 > /dev/null; then
  eval "$(rockie env)"
fi


if command -v mise 2>&1 > /dev/null; then
  if [ "$UNAME" = "Darwin" ]; then
    export ASDF_HASHICORP_TERRAFORM_VERSION_FILE="unset_version_file.tf.bak"
  fi

  # https://unix.stackexchange.com/questions/26676/how-to-check-if-a-shell-is-login-interactive-batch
  if [[ -o interactive ]]; then
    eval "$(mise activate zsh)"
  else
    eval "$(mise activate zsh --shims)"
  fi
fi

# if command -v rbenv 2>&1 > /dev/null; then
#   eval "$(rbenv init - zsh)"
# fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
# if [ -f "$HOME/.rvm/scripts/rvm" ]; then
#   source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
# fi
