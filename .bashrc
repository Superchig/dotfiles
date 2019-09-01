NON_COLOR_PROMPT="\u@\H \w bruh> "
COLOR_PROMPT="\[\e[34m\][\[\e[m\]\u@\h \[\e[32m\]\w\[\e[m\]\[\e[34m\]]\[\e[m\] $ "
export PS1=$COLOR_PROMPT

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
source ~/.rvm/scripts/rvm # Rvm is now a function

export PATH="$PATH:$HOME/Desktop/computer/intelliJ/idea-IC-141.178.9/bin" # Add intelliJ to path

export EDITOR='vim' # Vim for the default editor
export PATH="$PATH:$HOME/Desktop/computer/clang/bin"

export PATH="$PATH:$HOME/computer/LightTable"

alias ls='ls --color=auto'
alias l='ls -lah'
