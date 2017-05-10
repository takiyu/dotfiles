#!/bin/bash

# TARGETS=.?*
TARGETS=(.Xmodmap .bash_profile .bashrc .jshintrc .gitconfig .latexmkrc .pep8 \
         .xinitrc .xmobarrc .xmonad \
         .config/zathura/zathurarc .mplayer/config)
TARGET_DIRS=(.config/zathura/ .mplayer/)


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


# create directories
for target_dir in ${TARGET_DIRS[@]}; do
    home_target_dir="$HOME/$target_dir"
    if [ ! -e $home_target_dir ]; then
        echo "mkdir -p $home_target_dir"
        mkdir -p $home_target_dir
    fi
done

# create links
for target in ${TARGETS[@]}; do
    pwd_target="$PWD/$target"
    home_target="$HOME/$target"
    home_backup="$HOME/$target"_
    if [ ! -e $pwd_target ]; then
        echo "[Error] not exist $pwd_target"
    elif [ -e $home_target ]; then
        echo -n "[Ask] $home_target is already exist. Move? [y/n] "
        ret=`yn_prompt`
        if [ $ret -eq 0 ]; then
            echo "mv $home_target $home_backup"
            mv $home_target $home_backup
            create_link $pwd_target $home_target
        fi
    else
        create_link $pwd_target $home_target
    fi
done
