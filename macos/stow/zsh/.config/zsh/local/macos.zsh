# ssh-add -K ~/.ssh/id_rsa
# ssh-add -K ~/.ssh/id_rsa_superchig
# ssh-add --apple-load-keychain &> /dev/null

source "$HOME/work/general_tools/creds/bin/complete_creds.bash"

credsb() {
  if [ "$#" -eq 1 ]; then
    local FILE="$1"
  else
    local FILE="$(find $HOME/work/secrets $HOME/tmp/keys -type f | fzf --color=light)"
  fi

  if [ -n "$FILE" ]; then
    gpg -d "$FILE"
  fi

  # Add command to history
  print -s "credsb $FILE"
}
