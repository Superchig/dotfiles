NON_COLOR_PROMPT="\u@\H \w bruh> "
COLOR_PROMPT="\[\e[34m\][\[\e[m\]\u@\h \[\e[32m\]\w\[\e[m\]\[\e[34m\]]\[\e[m\] $ "
# From Luke Smith's dotfiles
LUKE_SMITH_PROMPT="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
export PS1=$COLOR_PROMPT

# Only run this if not using Windows Subsystem for Linux
if grep -qvE "(Microsoft|WSL)" /proc/version &> /dev/null ; then
    export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
    source ~/.rvm/scripts/rvm # Rvm is now a function
fi

# export PATH="$PATH:$HOME/Desktop/computer/intelliJ/idea-IC-141.178.9/bin" # Add intelliJ to path

export EDITOR='nvim'
# export PATH="$PATH:$HOME/Desktop/computer/clang/bin"

# export PATH="$PATH:$HOME/computer/LightTable"

# From CPSC-298
export PATH="$PATH:~/bin"

alias ls='ls --color=auto'
alias l='ls -lah'

alias xo='xdg-open &> /dev/null'
alias e='nvim'
alias pe="ps -e | grep"

# Stops directory highlighting on Windows subsystem for Linux
LS_COLORS=$LS_COLORS:'ow=1;34:'
export LS_COLORS

# Functions
fork() {
	($* &> /dev/null &)
}

# Move from downloads
# Takes one argument: number of minutes that files should be newer than
# Ex: mvdo -120
#       Moves files in ~/Downloads that are newer than 2 hours into directory
mvdo() {
  find ~/Downloads/ -mindepth 1 -mmin $1 -exec mv {} . \;
}

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'
