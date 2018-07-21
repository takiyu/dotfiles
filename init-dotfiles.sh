#!/bin/bash

# utility scripts
dotfiles=$HOME/dotfiles
determ_platform=$dotfiles/utils/determ_platform.sh

# determine the platform
platform=`$determ_platform`

# utility functions
create_dir() {
    local target_dir=$1
    if [ ! -e $target_dir ]; then
        echo "mkdir -p $target_dir"
        mkdir -p $target_dir
    fi
}

create_link() {
    local dot_target=$1
    local home_target=$2
    echo "ln -s $dot_target $home_target"
    ln -s $dot_target $home_target
}

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

create_link_prompt() {
    local dot_target=$1
    local home_target=$2

    # Check file existence
    if [ ! -e $dot_target ]; then
        echo "[Error] not exist $dot_target"
    elif [ -e $home_target ]; then
        # Check difference
        difference=`diff $dot_target $home_target`
        if [ -n $difference ]; then
            echo "[Skip] already exists, but no difference ($home_target)"
        else
            echo "[Ask] already exists. ($home_target)"
            echo "$difference"
            echo -n "Overwrite? [y/n] "
            ret=`yn_prompt`
            if [ $ret -eq 0 ]; then
                # Backup and create new link
                home_backup="$home_target"_
                echo "mv $home_target $home_backup"
                mv $home_target $home_backup
                create_link $dot_target $home_target
            fi
        fi
    else
        create_link $dot_target $home_target
    fi
}

# Setup dotfiles for each platform
if [ $platform == 'Linux' ]; then
    # Linux setup
    echo "* Setup dotfiles for Linux Environment"
    TARGETS=(.Xmodmap .xinitrc .bash_profile .bashrc .xmonad .xmobarrc \
             .wgetrc .gitconfig .latexmkrc .ctags .clang-format .clang-tidy \
             .jshintrc .pep8 .config/zathura/zathurarc .mplayer/config \
             .config/matplotlib/matplotlibrc \
             .vimrc .config/nvim/init.vim .config/nvim/ginit.vim \
             .config/nvim/dein)
    TARGET_DIRS=(.config/zathura/ .mplayer/ .config/matplotlib/ .config/nvim/)

    # First, create directories
    for target_dir in ${TARGET_DIRS[@]}; do
        create_dir "$HOME/$target_dir"
    done

    # Second, create links
    for target in ${TARGETS[@]}; do
        create_link_prompt "$dotfiles/dotfiles/$target" "$HOME/$target"
    done

elif [ $platform == 'Windows' ]; then
    # Windows setup
    echo "* Setup dotfiles for Windows Environment"
    TARGETS=(.bashrc .gitconfig .minttyrc .config/nvim/dein)
    TARGET_DIRS=(.config/nvim/)

    # First, create directories
    for target_dir in ${TARGET_DIRS[@]}; do
        create_dir "$HOME/$target_dir"
    done

    # Second, create links to home folder
    for target in ${TARGETS[@]}; do
        create_link_prompt "$dotfiles/dotfiles/$target" "$HOME/$target"
    done

    # Third, create nvim entry for windows
    app_nvim=$HOME/AppData/Local/nvim
    create_dir "$app_nvim"
    cp "$dotfiles/dotfiles/.config/init.vim" "$app_nvim/"
    cp "$dotfiles/dotfiles/.config/ginit.vim" "$app_nvim/"
fi
