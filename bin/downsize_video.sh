#!/bin/bash
ffmpeg -i "$1" -vcodec libx265 -crf 24 -vf scale=-2:720 -movflags use_metadata_tags -map_metadata 0 "$2"
