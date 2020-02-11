set -x VISUAL nvim
set -x EDITOR nvim

abbr --add l exa -l
abbr --add e nvim
abbr --add se sudo nvim
abbr --add b br -h

abbr --add pe "ps -e | grep"

abbr --add sps sudo pacman -S
abbr --add pq pacman -Q
abbr --add pm pulsemixer

abbr --add sf source ~/.config/fish/config.fish
abbr --add ssy sudo systemctl
abbr --add ef nvim ~/.config/fish/config.fish
abbr --add cdc cd ~/Documents/CPSC_Courses/cpp_interterm/
abbr --add cdo cd ~/Downloads
abbr --add ce $HOME/dotfiles/msi_gs65/conn_eduroam
abbr --add ts amixer -q -D pulse sset Master toggle

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
        # exec startx -- -keeptty
	$HOME/bin/nvselect
    end
end

# Activate the default Ruby manually
rvm default 2> /dev/null
