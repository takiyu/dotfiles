#!/bin/sh
if [ $# -lt 1 ]; then
    echo 'invalid output file (argv[0])'
    exit 1
fi
output=$1.mp4
echo "out: $output"
ffmpeg -f v4l2 -framerate 15 -video_size 640x480 -i /dev/video0 \
       -standard pal -err_detect ignore_err \
       -y $output
