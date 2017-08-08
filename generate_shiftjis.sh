#!/bin/sh
cat .vimrc | nkf -s > .vimrc_shiftjis
cat .bashrc | nkf -s > .bashrc_shiftjis
cat .bash_profile | nkf -s > .bash_profile_shiftjis
