#!/bin/bash
# Requires fzf and fd

cdf() {
	if [ "$#" -eq 0 ]; then
		>&2 echo "This requires the name of a file to search for."
		return
	fi

	INPUT=$(fd --hidden --exclude .git $* | fzf)
	if [ "$INPUT" = "" ]; then
		return
	fi
	cd $(dirname "$INPUT")
}
