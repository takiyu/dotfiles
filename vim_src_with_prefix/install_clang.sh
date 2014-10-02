#!/bin/sh

#未完成、未確認
wget http://llvm.org/releases/3.4/llvm-3.4.src.tar.gz
wget http://llvm.org/releases/3.4/clang-3.4.src.tar.gz
tar xzf llvm-3.4.src.tar.gz
tar xzf clang-3.4.src.tar.gz
rm llvm-3.4.src.tar.gz
rm clang-3.4.src.tar.gz
mv clang-3.4 llvm-3.4/tools/clang
