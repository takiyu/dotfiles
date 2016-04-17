#!/bin/bash

# create .vim dir
if [ ! -e $PWD/.vim ]; then
    mkdir $PWD/.vim
fi

# move old files
[ -e $HOME/.vim ] &&  mv $HOME/.vim $HOME/_.vim
[ -e $HOME/.vimrc ] &&  mv $HOME/.vimrc $HOME/_.vimrc

# create links
echo "ln -s $PWD/.vim $HOME"
ln -s $PWD/.vim $HOME
echo "ln -s $PWD/.vimrc $HOME"
ln -s $PWD/.vimrc $HOME

# NeoBundle
echo "Install NeoBundle"
git clone https://github.com/Shougo/neobundle.vim $PWD/.vim/bundle/neobundle.vim

echo "Please run vim and install plugins."
