#!/usr/bin/env python
import subprocess
import shutil
import glob
import os
import os.path as osp
from multiprocessing.pool import Pool


def process_one(src_filename):
    print(f'Process: {src_filename}')
    if not osp.isfile(src_filename):
        return

    # Create result filename
    base_dirname = osp.basename(src_dirname)
    base_filename = osp.basename(src_filename)
    basename = f'{base_dirname}  {base_filename}'
    dst_filename = osp.join(RESULT_DIRNAME, basename)
    if osp.exists(dst_filename):
        print(f' > Skip existing file: {dst_filename}')
        return

    # Downsize process
    _, ext = osp.splitext(dst_filename)
    ext = ext.lower()
    if ext in ['.jpg', '*.jepg', 'png']:
        # Copy (with meta data)
        shutil.copy2(src_filename, dst_filename)
        # Downsize image
        subprocess.run(['downsize_image.sh',
                        f'{osp.abspath(dst_filename)}'])
    elif ext in ['.mp4']:
        # Downsize video
        subprocess.run(['downsize_video.sh',
                        src_filename, dst_filename])
    else:
        print(f'Unsupported file extension: {ext}')


if __name__ == '__main__':
    # Create result directory
    RESULT_DIRNAME = './Downsized'
    os.makedirs(RESULT_DIRNAME, exist_ok=True)

    # Collect sub-directories
    src_dirnames = glob.glob('*')
    for src_dirname in src_dirnames:
        if not osp.isdir(src_dirname):
            continue

        # Skip result directory
        if osp.basename(src_dirname) == osp.basename(RESULT_DIRNAME):
            continue

        # Collect files
        src_filenames = glob.glob(osp.join(src_dirname, '*'))
        # Process in parallel
        pool = Pool()
        pool.map(process_one, src_filenames)
