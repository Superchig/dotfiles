set -x VISUAL nvim
set -x EDITOR nvim

set -x SUDO_ASKPASS /usr/local/bin/zenity_passphrase
set -x MANWIDTH 80

set -x XDG_CONFIG_HOME ~/.config

set -x DOTNET_CLI_TELEMETRY_OPTOUT 1

function add_path
  if echo $fish_user_paths | grep -v --quiet $argv[1]
    set -U fish_user_paths $fish_user_paths $argv[1]
  end
end

add_path "$HOME/dotfiles/util"

add_path "$HOME/.gem/ruby/2.7.0/bin"

add_path "$HOME/go/bin"

add_path "$HOME/.local/bin"

add_path "$HOME/bin"

add_path "$HOME/.dotnet/tools"

add_path "$HOME/.idris2/bin"

abbr --add l exa -la --group-directories-first
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
abbr --add ez nvim ~/.zshrc
# abbr --add ea nvim ~/.config/alacritty/alacritty.yml
abbr --add ea nvim ~/.config/awesome/rc.lua
abbr --add ei nvim ~/.config/i3/config
abbr --add eb nvim ~/.config/berry/autostart
abbr --add es nvim ~/.config/sway/config
abbr --add ep nvim ~/dotfiles/pacman-install.sh
abbr --add em nvim ~/.config/mimeapps.list
abbr --add en nvim ~/Dropbox/zoom_items.md
abbr --add cdc cd ~/Documents/CPSC_Courses/cpsc350_data_structures/
abbr --add cdo cd ~/Downloads
abbr --add cdd cd ~/dotfiles
abbr --add cds cd ~/go/src
abbr --add cdu cd /run/user/1000/gvfs/
abbr --add ce $HOME/dotfiles/msi_gs65/conn_eduroam
abbr --add ts amixer -q -D pulse sset Master toggle
abbr --add mo $HOME/dotfiles/multi/monitor_setup
abbr --add po $HOME/dotfiles/minimalist/bar/launch_polybar.sh
abbr --add sx startx ~/.xinitrc

abbr --add ops xo $HOME/school/span_201/tarea.pdf
abbr --add opm xo $HOME/school/math_116/Calculus_Early_Transcendentals_8th_Editi.pdf

abbr --add chwm ~/dotfiles/minimalist/chwm

abbr --add ralt setxkbmap -option compose:ralt

abbr --add orphans sudo pacman -Rns (pacman -Qtdq)

abbr --add fe ~/.fehbg

abbr --add lg lazygit

# abbr --add code code --enable-features=UseOzonePlatform --ozone-platform=wayland

abbr --add cgb cargo build
abbr --add cgr cargo run
abbr --add cgt cargo test
abbr --add cgc cargo check

abbr --add idris2 rlwrap idris2

alias au 'autorandr -c mobile && autorandr -c'

set -x LESS_TERMCAP_mb (printf '\e[1;32m')
set -x LESS_TERMCAP_md (printf '\e[1;32m')
set -x LESS_TERMCAP_me (printf '\e[0m')
set -x LESS_TERMCAP_se (printf '\e[0m')
set -x LESS_TERMCAP_so (printf '\e[01;33m')
set -x LESS_TERMCAP_ue (printf '\e[0m')
set -x LESS_TERMCAP_us (printf '\e[1;4;31m')

# You can see the definition of the `f` function with `functions f`
function rfcd
    set tmp (mktemp)
    rolf -last-dir-path $tmp $argv
    if test -f "$tmp"
        set dir (cat $tmp)
        rm -f $tmp
        if test -d "$dir"
            if test "$dir" != (pwd)
                cd $dir
            end
        end
    end
end

bind \cw 'f; commandline -f repaint'
# Bind file manager to Ctrl+o
bind \co 'rfcd; commandline -f repaint'

# Source the file for z.fish
# After some fish update, this apparently causes an awk message on directory
# change
# . $HOME/dotfiles/z.fish

# Use custom program to start X at login
if status is-login
    if test -z "$DISPLAY" -a $XDG_VTNR = 1
      if test "$hostname" = alien
          # exec startx -- -keeptty
      else if test "$hostname" = msi && test -f $HOME/bin/nvselect
          # $HOME/bin/nvselect
      end
    end
end

# Set the title of the terminal to e.g. "fish ~/p/rolf"  or "nvim ~/D/appimages"
function fish_title
  set -q argv[1]; or set argv[1] fish
  echo $argv (fish_prompt_pwd_dir_length=1 prompt_pwd)
end

# Activate the default Ruby manually
# rvm default 2> /dev/null

if [ -f /usr/bin/frum ]
  frum init | source
end

# Source opam (ocaml package manager) script
source /home/chiggie/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

zoxide init fish --cmd j | source

