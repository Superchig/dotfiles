function fork
	$argv &> /dev/null &
	disown
end
