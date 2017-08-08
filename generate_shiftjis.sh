#!/bin/sh
mkdir -p shiftjis
cat .vimrc | nkf -s > shiftjis/.vimrc
cat .bashrc | nkf -s > shiftjis/.bashrc
cat .bash_profile | nkf -s > shiftjis/.bash_profile
