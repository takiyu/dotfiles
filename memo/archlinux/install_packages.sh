#!/bin/bash

# Install Pacman packages
sudo pacman -S - < pacman_packages.txt

# Install Trizen packages
trizen -S ttf-ricty
trizen -S slack-desktop

# Others
sudo pip install pyflakes pep8
sudo pip install neovim
sudo npm install -g jsxhint
