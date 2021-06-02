xo() {
	xdg-open $* &> /dev/null &
	disown
}
