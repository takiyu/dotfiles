#!/bin/sh

if [ $# -lt 2 ]; then
	echo '[usage]: ./pdfsplit [in_file] [out_basename]'
	exit 0
fi

in_file=$1
out_basename=$2

n_page=`pdfinfo $in_file | gawk '/Pages/ {print $2}'`

for i in `seq -w 1 $n_page`; do
	out_file="$out_basename-$i.pdf"
	echo "[in_file]:$in_file, [page]:$i, [out_file]:$out_file"
	pdftk $in_file cat $i output $out_file
done
