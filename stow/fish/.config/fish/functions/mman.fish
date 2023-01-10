function mman
	man (man -k $argv | fzf | cut -d' ' -f1)
end
