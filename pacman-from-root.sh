#!/bin/sh

echo "Creating non-root user"

useradd -G chiggie -G sudo

echo "This script will now allow to set the password for this new user"

passwd chiggie

echo 'This script will now run `visudo`. Please allow people in the `sudo` group to use the command.'

read -p "Press enter when you're ready: "
