#!/usr/bin/env bash

bash "${BASH_SOURCE%/*}"/bar/lemonbar.bash

exec "${BASH_SOURCE%/*}"/poststartup
