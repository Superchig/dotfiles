#!/bin/bash
# Requires fzf

cdl() {
	if [ "$#" -eq 0 ]; then
		>&2 echo "This requires the name of a file to search for."
		return
	fi

	INPUT=$(locate $* | fzf)
	if [ "$INPUT" = "" ]; then
		return
	fi
	cd $(dirname "$INPUT")
}
