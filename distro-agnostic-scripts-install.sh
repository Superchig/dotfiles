#!/usr/bin/env bash
PYTHON3_VERSION=3.5.2

# Oh-my-zsh
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# rvm
## Commented out because gpg keys are out of date
# gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
# \curl -sSL https://get.rvm.io | bash -s stable

## Pyenv and related are also disabled because I don't currently use them
# pyenv
# curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
# 
# pyenv install $PYTHON3_VERSION
# pyenv global $PYTHON3_VERSION
# 
# # youtube viewing
# pip3 install youtube-dl
# 
# pip3 install mps-youtube
