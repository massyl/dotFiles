#!/bin/sh

export PREVPATH=$PATH
GHC_PACKAGE_PATH=$(stack exec env | grep GHC_PACKAGE_PATH | sed s/.\\+=//g 2>/dev/null)
export GHC_PACKAGE_PATH
PATH=$(stack exec env | grep ^PATH | sed s/.\\+=//g 2>/dev/null)
export PATH
exec $HOME/.local/bin/xmonad "$@"
