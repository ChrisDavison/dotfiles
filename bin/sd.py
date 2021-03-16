#!/usr/bin/env python3
import os
import sys
import subprocess
from pathlib import Path


SD_DIR = Path(os.environ.get('SD_ROOT', '~/sd')).expanduser()


def recursively_find_script(args):
    root = SD_DIR
    for i, arg in enumerate(args):
        maybe_cmd = root / arg
        if maybe_cmd.exists() and maybe_cmd.is_file():
            return maybe_cmd, args[i+1:]
        root = root / arg
    return None, args


def recursively_find_folder(args):
    root = SD_DIR
    while args:
        maybe_dir = root
        for arg in args:
            maybe_dir /= arg
            if maybe_dir.is_dir():
                return maybe_dir
        args = args[:-1]
    return None


def print_with_depth(entry, only_dirs=False):
    if only_dirs and entry.is_file():
        return
    entry = entry.relative_to(SD_DIR)
    depth = len(entry.parents) - 1
    spacing = '  ' * depth
    filemarker = '+' if entry.is_file() else '>'
    print(f"{spacing}{filemarker} {entry.name}")


def print_dir(dir, only_dirs=False):
    for elem in Path(dir).glob('*'):
        print_with_depth(elem, only_dirs)
        if elem.is_dir():
            print_dir(elem, only_dirs)


if __name__ == "__main__":
    args = sys.argv[1:]
    only_dirs = False
    if args and args[0] == '-d':
        args = args[1:]
        only_dirs = True
    script, args = recursively_find_script(args)
    if not script:
        maybe_dir = recursively_find_folder(args)
        if maybe_dir:
            print(maybe_dir.relative_to(SD_DIR))
            print_dir(maybe_dir, only_dirs)
        else:
            print(f"Didn't find script: {' '.join(args)}")
            print_dir(SD_DIR, only_dirs)
    else:
        subprocess.run([script, *args])
