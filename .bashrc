ABBR_DIR='pwd2=$(sed "s:\([^/]\)[^/]*/:\1/:g" <<<$PWD)'

NON_COLOR_PROMPT="\u@\H \w bruh> "
COLOR_PROMPT="\[\e[34m\][\[\e[m\]\u@\h \[\e[32m\]\w\[\e[m\]\[\e[34m\]]\[\e[m\] $ "
# From Luke Smith's dotfiles
LUKE_SMITH_PROMPT="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

export PS1=$COLOR_PROMPT

# zenity_passphrase is a custom script: zenity --password --title="sudo password prompt" --timeout=10
export SUDO_ASKPASS=/usr/local/bin/zenity_passphrase

# Only run this if not using Windows Subsystem for Linux
if grep -qvE "(Microsoft|WSL)" /proc/version &> /dev/null ; then
  export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
  source ~/.rvm/scripts/rvm # Rvm is now a function
fi

# export PATH="$PATH:$HOME/Desktop/computer/intelliJ/idea-IC-141.178.9/bin" # Add intelliJ to path

export VISUAL='nvim'
export FCEDIT=$VISUAL
# export PATH="$PATH:$HOME/Desktop/computer/clang/bin"

# export PATH="$PATH:$HOME/computer/LightTable"

# From CPSC-298
export PATH="$PATH:$HOME/bin"

export PATH="$PATH:$HOME/go/bin"

# Use bash-completion, if available
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
  . /usr/share/bash-completion/bash_completion

alias ls='ls --color=auto'
if [ -f /usr/bin/exa ]; then
  alias l='exa --long'
else
  alias l='ls -lah'
fi

alias grep='grep --color=auto'

alias xo='xdg-open &> /dev/null'
alias e='nvim'
alias se="sudo nvim"

alias fr="history | sk | cut -c 8-"

alias pe="ps -e | grep"
alias pi="pacman -Qi | grep Description"

alias mo="$HOME/dotfiles/minimalist/monitor_setup.bash"

alias orphans="sudo pacman -Rns $(pacman -Qtdq)"

alias gimme="sudo chown ${USER} *"

alias cdc="cd $HOME/Documents/CPSC_Courses/cpsc231_a1/"
alias cdo="cd $HOME/Downloads"

alias activate="source env/bin/activate"

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

# Package dump
# Outputs all required packages to install a new package into text file
pdump() {
  if [ "$#" -ne 2 ]; then
    echo "Error: requires 2 inputs."
    return 1
  fi

  pacman -Sp "$1" --print-format '%n' --needed > "$2"
  echo "Successfully listed necessary packages for ${1} into ${2}"
}

# Fuzzy edit
# Uses skim or fzf to fuzzy find a file and then open it in an editor
fe() {
  local FIND_RESULT
  if [ -z "$1" ]; then
    FIND_RESULT=$(find -type f | sk)
  else
    FIND_RESULT=$(find "$1" -type f | sk)
  fi

  if [ ! -z ${FIND_RESULT} ]; then
    ${VISUAL} ${FIND_RESULT}
  fi
}

# Multi edit
# Fuzzy finds a file and then opens it in an editor of choice
me() { find . | sk | xargs "$1"; }

# cd today
# cd to today's directory in the CPSC 231 directory
cdtd() {
  cd $HOME/Documents/CPSC_Courses/cpsc231_a1/$(date +"%_m_%e_%Y")
}

mman() {
  man $(man -k "$1" | fzf | awk '{print $1}')
}

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/chiggie/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/chiggie/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/chiggie/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/chiggie/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


source /home/chiggie/.config/broot/launcher/bash/br
