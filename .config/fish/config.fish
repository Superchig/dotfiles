set -x VISUAL nvim
set -x EDITOR nvim

set -x SUDO_ASKPASS /usr/local/bin/zenity_passphrase

abbr --add l exa -la
abbr --add e nvim
abbr --add se sudo nvim
abbr --add b br -h
# Uses custom mcd function
abbr --add m mcd

abbr --add pe "ps -e | grep"

abbr --add sp sudo pacman
abbr --add sps sudo pacman -S
abbr --add p pacman
abbr --add pq pacman -Q
abbr --add pm pulsemixer
abbr --add xf xset dpms force off

abbr --add sf source ~/.config/fish/config.fish
abbr --add ssy sudo systemctl
abbr --add ef nvim ~/.config/fish/config.fish
abbr --add ea nvim ~/.config/alacritty/alacritty.yml
abbr --add ei nvim ~/.config/i3/config
abbr --add cdc cd ~/Documents/CPSC_Courses/cpsc350_data_structures/
abbr --add cdo cd ~/Downloads
abbr --add cdd cd ~/dotfiles
abbr --add ce $HOME/dotfiles/msi_gs65/conn_eduroam
abbr --add ts amixer -q -D pulse sset Master toggle
abbr --add mo $HOME/dotfiles/minimalist/monitor_setup.bash

abbr --add chwm ~/dotfiles/minimalist/chwm

set -x LESS_TERMCAP_mb (printf '\e[1;32m')
set -x LESS_TERMCAP_md (printf '\e[1;32m')
set -x LESS_TERMCAP_me (printf '\e[0m')
set -x LESS_TERMCAP_se (printf '\e[0m')
set -x LESS_TERMCAP_so (printf '\e[01;33m')
set -x LESS_TERMCAP_ue (printf '\e[0m')
set -x LESS_TERMCAP_us (printf '\e[1;4;31m')

# Use custom program to start X at login
if status is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
      if test "$hostname" = alien
          # exec startx -- -keeptty
      else
          $HOME/bin/nvselect
      end
    end
end

# Activate the default Ruby manually
# rvm default 2> /dev/null
