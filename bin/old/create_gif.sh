#!/bin/sh

if [ $# -lt 2 ]; then
    echo 'argv[1] == input movie filename'
    echo 'argv[2] == output gif filename'
    echo 'argv[3] == palette image filename (optional)'
    exit 1
fi
in_filename=$1
out_filename=$2

if [ $# -lt 3 ]; then
    # Using ffmpeg
    ffmpeg -i $in_filename -y $out_filename
else
    # Using ffmpeg with Palette
    palette_filename=$3
    ffmpeg -i $in_filename -vf palettegen -y $palette_filename
    ffmpeg -i $in_filename -i $palette_filename -lavfi paletteuse -y $out_filename
fi
