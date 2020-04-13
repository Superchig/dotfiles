set -x VISUAL nvim
set -x EDITOR nvim

set -x SUDO_ASKPASS /usr/local/bin/zenity_passphrase
set -x MANWIDTH 80

set -x XDG_CONFIG_HOME ~/.config

if echo $fish_user_paths | grep -v --quiet util
  set -U fish_user_paths $fish_user_paths "$HOME/dotfiles/util/"
end

abbr --add l exa -la
abbr --add e nvim
abbr --add se sudo nvim
abbr --add b br -h
# Uses custom mcd function
abbr --add m mcd

abbr --add pe "ps -e | grep"

abbr --add sp sudo pacman
abbr --add sps sudo pacman -S
abbr --add spr sudo pacman -R
abbr --add p pacman
abbr --add pq pacman -Q
abbr --add pss pacman -Ss
abbr --add pm pulsemixer
abbr --add xf xset dpms force off

abbr --add sf source ~/.config/fish/config.fish
abbr --add ssy sudo systemctl
abbr --add sy systemctl
abbr --add ef nvim ~/.config/fish/config.fish
abbr --add ea nvim ~/.config/alacritty/alacritty.yml
abbr --add ei nvim ~/.config/i3/config
abbr --add eb nvim ~/.config/berry/autostart
abbr --add es nvim ~/.config/sxhkd/sxhkdrc
abbr --add ep nvim ~/dotfiles/pacman-install.sh
abbr --add cdc cd ~/Documents/CPSC_Courses/cpsc350_data_structures/
abbr --add cdo cd ~/Downloads
abbr --add cdd cd ~/dotfiles
abbr --add cds cd ~/go/src
abbr --add cdu cd /run/user/1000/gvfs/
abbr --add ce $HOME/dotfiles/msi_gs65/conn_eduroam
abbr --add ts amixer -q -D pulse sset Master toggle
abbr --add mo $HOME/dotfiles/multi/monitor_setup
abbr --add sx startx ~/.xinitrc

abbr --add ops xo $HOME/school/span_201/tarea.pdf
abbr --add opm xo $HOME/school/math_116/Calculus_Early_Transcendentals_8th_Editi.pdf

abbr --add chwm ~/dotfiles/minimalist/chwm

abbr --add ralt setxkbmap -option compose:ralt

set -x LESS_TERMCAP_mb (printf '\e[1;32m')
set -x LESS_TERMCAP_md (printf '\e[1;32m')
set -x LESS_TERMCAP_me (printf '\e[0m')
set -x LESS_TERMCAP_se (printf '\e[0m')
set -x LESS_TERMCAP_so (printf '\e[01;33m')
set -x LESS_TERMCAP_ue (printf '\e[0m')
set -x LESS_TERMCAP_us (printf '\e[1;4;31m')

# Source the file for z.fish
. $HOME/dotfiles/z.fish

# Use custom program to start X at login
if status is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
      if test "$hostname" = alien
          # exec startx -- -keeptty
      else if test "$hostname" = msi && test -f $HOME/bin/nvselect
          $HOME/bin/nvselect
      end
    end
end

# Activate the default Ruby manually
# rvm default 2> /dev/null
