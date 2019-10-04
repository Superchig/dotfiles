NON_COLOR_PROMPT="\u@\H \w bruh> "
COLOR_PROMPT="\[\e[34m\][\[\e[m\]\u@\h \[\e[32m\]\w\[\e[m\]\[\e[34m\]]\[\e[m\] $ "
export PS1=$COLOR_PROMPT

# Only run this if not using Windows Subsystem for Linux
if grep -qvE "(Microsoft|WSL)" /proc/version &> /dev/null ; then
    export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
    source ~/.rvm/scripts/rvm # Rvm is now a function
fi

# export PATH="$PATH:$HOME/Desktop/computer/intelliJ/idea-IC-141.178.9/bin" # Add intelliJ to path

export EDITOR='vim' # Vim for the default editor
# export PATH="$PATH:$HOME/Desktop/computer/clang/bin"

# export PATH="$PATH:$HOME/computer/LightTable"

alias ls='ls --color=auto'
alias l='ls -lah'

# Stops directory highlighting on Windows subsystem for Linux
LS_COLORS=$LS_COLORS:'ow=1;34:'
export LS_COLORS

fork() {
	($* &> /dev/null &)
}

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'
