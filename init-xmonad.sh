#シンボリックリンクを張る
echo ">シンボリックリンクを作成"

mkdir "$HOME/.xmonad"
ln -s "$PWD/.xmonad" $HOME
ln -s "$PWD/.Xmodmap" $HOME
ln -s "$PWD/.xmobarrc" $HOME
