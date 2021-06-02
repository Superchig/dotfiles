paclist() {
	pacman --color=always -Ss $1 | less -R
}
