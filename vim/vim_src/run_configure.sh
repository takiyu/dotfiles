#!/bin/sh

# lua5.2 liblua5.2-dev
# gpm libgpm-dev

cd vim
./configure \
--enable-multibyte \
--with-features=huge \
--disable-selinux \
--prefix=/usr/local \
--enable-gpm \
--enable-luainterp=dynamic \
--enable-rubyinterp=yes \
--enable-pythoninterp=yes \
--enable-python3interp=yes > ../configure_result.log

cd ..
LANG_LIST='lua ruby python'
for item in $LANG_LIST; do
	echo "> $item :"
	cat configure_result.log | grep $item
	echo \n
done

rm configure_result.log
