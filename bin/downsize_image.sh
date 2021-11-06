#!/bin/bash
mogrify -resize 2000x2000 -define preserve-timestamp=true -quality 90 $@
