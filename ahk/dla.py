#!/usr/bin/env python3
"""
Add a url to my NAS download list.
"""
from pathlib import Path
from pyperclip import paste
import sys


def get_url(args):
    if not args:
        url = paste()
    else:
        url = args[0]
    if 'youtube' in url or 'youtu.be' in url:
        url = url.split('&')[0]
    return url.strip(), args[1:]


def main(args, quick):
    dl_dir = Path("Y:")
    dl_list = dl_dir / "to-download.txt"
    url, args = get_url(args)
    print("URL:", url)
    if args:
        name = '-'.join(args)
    else:
        name = ''
        if not quick:
            name = input("Name: ").replace(' ', '-')
    with dl_list.open('a') as f:
        if name and not 'mp3' in name and 'mp3' in url:
            name = name + '.mp3'
        print(' '.join([url, name]), file=f)


if __name__ == "__main__":
    quick = '-q' in sys.argv[1:]
    args = [arg for arg in sys.argv[1:] if arg != '-q']
    main(args, quick)
