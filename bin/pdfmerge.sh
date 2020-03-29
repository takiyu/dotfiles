#!/bin/sh

if [ $# -lt 2 ]; then
	echo '[usage]: ./pdfmerge [in_files] [out_file]'
	exit 0
fi

in_files=$1
out_file=$2

echo $in_files
pdftk $in_files cat output $out_file
