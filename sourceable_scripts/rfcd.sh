# Change working dir in shell to last dir in rolf on exit (adapted from lf).
#
# You need to either copy the content of this file to your shell rc file
# (e.g. ~/.bashrc) or source this file directly:
#
#     rfcd="/path/to/rfcd.sh"
#     if [ -f "$rfcd" ]; then
#         source "$rfcd"
#     fi
#
# You may also like to assign a key to this command:
#
#     bind '"\C-o":"rfcd\C-m"'  # bash
#     bindkey -s '^o' 'rfcd\n'  # zsh
#

rfcd () {
    tmp="$(mktemp)"
    rolf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}
