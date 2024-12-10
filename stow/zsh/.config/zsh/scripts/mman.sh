#!/bin/bash

mman() {
	for (( ; ; )) do
		INPUT=$(man -k "$1" | fzf --reverse --height=15 | awk '{print $1}')
		if [ "$INPUT" = "" ]; then
			break
		fi
  	man "$INPUT"
	done
}
