#!/bin/bash

# TARGETS=.?*
TARGETS=(.Xmodmap .bash_profile .bashrc .jshintrc .gitconfig .latexmkrc .pep8 \
         .xinitrc .xmobarrc .xmonad .ctags .vimrc \
         .config/zathura/zathurarc .mplayer/config \
         .config/matplotlib/matplotlibrc \
         .config/nvim/*.vim .config/nvim/dein)
TARGET_DIRS=(.config/zathura/ .mplayer/ .config/matplotlib/ .config/nvim/)


# utility functions
yn_prompt() {
    while true ; do
        read INPUT
        case "$INPUT" in
            'y' )
                echo 0
                break ;;
            'n' )
                echo 1
                break ;;
            * )
                ;;
        esac
    done
}

create_link() {
    local pwd_target=$1
    local home_target=$2
    echo "ln -s $pwd_target $home_target"
    ln -s $pwd_target $home_target
}


# First, create directories
for target_dir in ${TARGET_DIRS[@]}; do
    home_target_dir="$HOME/$target_dir"
    if [ ! -e $home_target_dir ]; then
        echo "mkdir -p $home_target_dir"
        mkdir -p $home_target_dir
    fi
done

# Second, create links
for target in ${TARGETS[@]}; do
    pwd_target="$PWD/$target"
    home_target="$HOME/$target"
    home_backup="$HOME/$target"_
    if [ ! -e $pwd_target ]; then
        echo "[Error] not exist $pwd_target"
    elif [ -e $home_target ]; then
        # Check difference
        difference=`diff $pwd_target $home_target`
        if [ -n $difference ]; then
            echo "[Skip] already exists, but no difference"
        else
            echo "[Ask] $home_target is already exist."
            echo "$difference"
            echo -n "Overwrite? [y/n] "
            ret=`yn_prompt`
            if [ $ret -eq 0 ]; then
                # Backup and create new link
                echo "mv $home_target $home_backup"
                mv $home_target $home_backup
                create_link $pwd_target $home_target
            fi
        fi
    else
        create_link $pwd_target $home_target
    fi
done
