# Run ranger, but choose directory after
function r
	ranger --choosedir=/tmp/ranger_dir $argv
	cd (cat /tmp/ranger_dir)
end
