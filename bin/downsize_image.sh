#!/bin/bash
mogrify -resize 1500x1500 -define preserve-timestamp=true -quality 80 "$@"
