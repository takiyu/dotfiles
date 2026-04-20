#!/bin/bash

# utility scripts
dotfiles=$HOME/dotfiles

# Create font directory
FONT_DIRNAME=~/.local/share/fonts/
mkdir -p $FONT_DIRNAME

# Copy fonts
cp $dotfiles/fonts/*.ttc $FONT_DIRNAME
cp $dotfiles/fonts/*.ttf $FONT_DIRNAME

# Update fontconfig cache if available (needed for X11, optional for SSH)
if command -v fc-cache >/dev/null 2>&1; then
    fc-cache -f -v
fi

# Clear matplotlib font cache so it re-scans and picks up new fonts
if command -v python3 >/dev/null 2>&1; then
    python3 -c "
import sys
try:
    import matplotlib
    import matplotlib.font_manager as fm
    import glob, os
    # _rebuild() is the private API; fallback to cache file deletion
    if hasattr(fm, '_rebuild'):
        fm._rebuild()
    else:
        for f in glob.glob(
                os.path.join(matplotlib.get_cachedir(), 'fontlist*.json')):
            os.remove(f)
    print('matplotlib font cache cleared')
except Exception as e:
    print('matplotlib font cache rebuild skipped: ' + str(e), file=sys.stderr)
"
fi
