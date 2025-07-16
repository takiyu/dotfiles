# utility scripts
dotfiles=$HOME/dotfiles
determ_platform=$dotfiles/utils/determ_platform.sh
exist_command=$dotfiles/utils/exist_command.sh

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
    if [ $platform == 'Windows' ]; then
        echo "cp -r $dot_target $home_target"  # Copy
        cp -r $dot_target $home_target
    else
        echo "ln -s $dot_target $home_target"  # Soft link
        ln -s $dot_target $home_target
    fi
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
        difference=`diff -r $dot_target $home_target`
        if [ "$difference" == "" ]; then
            echo "[Skip] Already exists. No difference ($home_target)"
        else
            echo "[Ask] Already exists. ($home_target)"
            echo "$difference"
            echo -n "Overwrite? [y/n] "
            ret=`yn_prompt`
            if [ $ret -eq 0 ]; then
                # Backup and create new link
                home_backup="$home_target"_
                if [ -e $home_backup ]; then
                    echo "rm -r $home_backup"
                    rm -r $home_backup
                fi
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
    TARGETS=(.Xmodmap_default .Xmodmap .xinitrc .bash_profile .bashrc .xmonad \
             .xmobarrc .wgetrc .gitconfig .latexmkrc .ctags .clang-format \
             .clang-tidy .jshintrc .pep8 .tmux.conf .mplayer  .mime.types \
             .lesskey .vsnip .xbindkeysrc .clinerules \
             .config/matplotlib .config/nvim .config/pycodestyle  \
             .config/zathura .config/pypoetry .config/sway .config/fuzzel
             .config/foot .config/waybar .config/xfce4/terminal)
    TARGET_DIRS=()

    # First, create directories
    for target_dir in ${TARGET_DIRS[@]}; do
        create_dir "$HOME/$target_dir"
    done

    # Second, create links
    for target in ${TARGETS[@]}; do
        create_link_prompt "$dotfiles/dotfiles/$target" "$HOME/$target"
    done

    # Install fonts
    if [ "`$exist_command fc-cache`" == 'exist' ]; then
        ./fonts/scripts/install-font-linux.sh
    fi

elif [ $platform == 'Windows' ]; then
    # Windows setup
    echo "* Setup dotfiles for Windows Environment"
    TARGETS=(.bash_profile .bashrc .gitconfig .minttyrc .config/nvim/dein \
             .lesskey .vsnip)
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
    create_link_prompt "$dotfiles/dotfiles/.config/nvim/init.lua" "$app_nvim/init.lua"
    create_link_prompt "$dotfiles/dotfiles/.config/nvim/ginit.vim" "$app_nvim/ginit.vim"
fi
