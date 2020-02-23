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
