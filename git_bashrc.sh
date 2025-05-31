export EDITOR=nvim
export VISUAL=nvim

alias e="$VISUAL"
alias ls="ls --color=auto"
alias l="ls -lah"

alias lg="lazygit"
alias gits="git stash save --all"
alias gca="git commit --amend"

alias cdd="cd $HOME/dotfiles"

bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'

export MSYS="winsymlinks:nativestrict"

if [ -f "$HOME/msvc_env.sh" ]; then
  source "$HOME/msvc_env.sh"
fi

export PATH="$PATH:$HOME/AppData/Local/mise/shims"
export PATH="$PATH:$HOME/git-bash-bin"

lfcd() {
  tmp="$(mktemp)"
  lf -last-dir-path="$tmp" "$@"
  if [ -f "$tmp" ]; then
    dir="$(cat "$tmp")"
    rm -f "$tmp"
    if [ -d "$dir" ]; then
      if [ "$dir" != "$(pwd)" ]; then
        cd "$dir"
      fi
    fi
  fi
}

bind '"\C-w":"lfcd\C-m"'

fork() {
  ($* &>/dev/null &)
}

if command -v mcfly >/dev/null; then
  eval "$(mcfly init bash)"
fi

if command -v zoxide >/dev/null; then
  eval "$(zoxide init bash --cmd j)"
fi
