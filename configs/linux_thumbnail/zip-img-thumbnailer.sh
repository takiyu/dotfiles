#!/usr/bin/env bash
# Thumbnail creation script
# Args: $1 = input archive (zip)
#       $2 = output thumbnail image path
#       $3 = size (pixels)
set -euo pipefail

INFILE="$1"
OUTFILE="$2"
SIZE="${3:-128}"

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

# Find the first image file inside the archive (by extension)
FIRST_IMAGE=$(unzip -Z1 "$INFILE" 2>/dev/null | grep -iE '\.(jpe?g|png|gif|webp|bmp|tiff)$' | head -n1)

if [ -z "$FIRST_IMAGE" ]; then
  # If no image is found, exit with non-zero (the thumbnailer is treated as a failure)
  exit 1
fi

# Extract the found file to a temporary file
unzip -p "$INFILE" "$FIRST_IMAGE" > "$TMPDIR/input.img"

# Create the thumbnail (uses ImageMagick)
if command -v magick >/dev/null 2>&1; then
  magick "$TMPDIR/input.img" -auto-orient -thumbnail "${SIZE}x${SIZE}>" -background white -alpha remove "$OUTFILE"
else
  convert "$TMPDIR/input.img" -auto-orient -thumbnail "${SIZE}x${SIZE}>" -background white -alpha remove "$OUTFILE"
fi

exit 0
