#!/bin/sh

len=32
if [ $# -gt 0 ]; then
	len=$1
fi
cat /dev/urandom | tr -dc '[:alnum:]' | head -c $len
