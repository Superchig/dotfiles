#!/bin/bash

aurinfo() {
	if [ $? -gt 1 ]; then
		echo An aur package must be specified!
		return 1
	fi

	echo "Searching for $1..."
	PACKAGE=$(yay -Si $1 | grep 'AUR URL' | awk '{print $4}')

	if [ -n ${PACKAGE} ]; then
		${BROWSER:-firefox} ${PACKAGE}
		echo Opened AUR page for ${PACKAGE}.
	else
		echo ${PACKAGE} is not a valid aur package!
		return 2
	fi
}
