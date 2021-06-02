mvdo() {
	find ~/Downloads/ -mindepth 1 -mmin $1 -exec mv {} . \;
}
