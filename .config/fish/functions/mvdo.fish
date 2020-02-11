function mvdo
	find ~/Downloads/ -mindepth 1 -mmin $argv[1] -exec mv {} . \;
end
