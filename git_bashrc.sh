export EDITOR=nvim
export VISUAL=nvim

alias e="$VISUAL"
alias ls="ls --color=auto"
alias l="ls -lah"

alias lg="lazygit"
alias gits="git stash save --all"

alias cdd="cd $HOME/Dotfiles"

bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'

export MSYS="winsymlinks:nativestrict"

export PATH="$PATH:$HOME/AppData/Local/mise/shims"
export PATH="$PATH:$HOME/git-bash-bin"

fork() {
  ($* &>/dev/null &)
}

eval "$(zoxide init bash --cmd j)"
