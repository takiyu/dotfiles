#!/bin/bash

# .vimディレクトリを作成
mkdir .vim

# 元からある.vim,.vimrcを移動
mv $HOME/.vim $HOME/_vim
mv $HOME/.vimrc $HOME/_vimrc

# シンボリックリンクを貼る
echo ">シンボリックリンクを作成:.vim"
ln -s $PWD/.vim $HOME
echo ">シンボリックリンクを作成:.vimrc"
ln -s $PWD/.vimrc $HOME

# 成功したらneobundleの導入
echo ">NeoBundleをインストール"
git clone https://github.com/Shougo/neobundle.vim $PWD/.vim/bundle/neobundle.vim

# プラグインインストールして終了(起動時に聞かれるのでyを入力)
echo ">vimを起動:pluginをinstallでyを入力"
vim -c :q
