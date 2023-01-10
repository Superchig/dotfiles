function xo
	xdg-open $argv 2> /dev/null > /dev/null &
	disown
end
