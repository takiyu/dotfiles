#!/bin/sh

#prefixを指定
PREFIX=$HOME/local
LUA_PREFIX=$HOME/local

cd vim
./configure --enable-multibyte --with-features=huge --disable-selinux --prefix=$PREFIX --enable-luainterp=yes --with-lua-prefix=$LUA_PREFIX --enable-rubyinterp=yes --enable-pythoninterp=yes > ../configure_result.log

cd ..
#configureの結果を確認
LANG_LIST='lua ruby python'
for item in $LANG_LIST; do
	echo "> $item :"
	cat configure_result.log | grep $item
	echo \n
done

rm configure_result.log
