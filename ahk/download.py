#!/usr/bin/env python3
"""
Download youtube and mp3 content to the refile folder of my NAS.
"""
import subprocess
from pathlib import Path
import sys
import time


CAPTURE_OUT=True


def list_downloads():
    nas_root = Path("Y:\\")
    dl_list = nas_root / "to-download.txt"
    lines = [line.strip() for line in dl_list.read_text().splitlines()
                 if line.strip()]
    for line in lines:
        print(line)


def dl_youtube(url, name, directory):
    """
    Download a video from youtube.
    """
    cmd = [
            'youtube-dl.exe', f"'{url}'",
           '--no-playlist',
           '-f', 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best',
           '--merge-output-format', 'mp4'
    ]
    if name:
        cmd.extend(['-o', name])
    else:
        cmd.extend(['-o', "%(title)s.%(ext)s"])
    proc = subprocess.run(cmd, check=True, cwd=directory, capture_output=CAPTURE_OUT)
    return proc.returncode


def dl_with_wget(url, name, directory):
    """
    Download something with wget (probably a podcast mp3).
    """
    cmd = ["wsl.exe", "--", "zsh", "-c", 'wget', url]
    if name:
        cmd.append(f'--output-document={name}')
    return subprocess.run(cmd, check=True, cwd=directory, capture_output=CAPTURE_OUT).returncode


def main():
    """
    Download all the files in my NAS' to-download.txt file.
    """
    # direc = Path(__file__).resolve()
    nas_root = Path("Y:\\")
    dl_dir = nas_root / "refile"
    dl_list = nas_root / "to-download.txt"
    dl_fails = nas_root / "failed-downloads.txt"
    print("DOWNLOADING")
    print(dl_list)
    time.sleep(1)

    while True:
        lines = [line.strip() for line in dl_list.read_text().splitlines()
                 if line.strip()]
        print(lines)
        time.sleep(1)
        if not lines:
            print("No links to download")
            time.sleep(1)
            sys.exit(0)
        url_and_name = lines[0].split(' ')
        url = url_and_name[0]
        name = '-'.join(url_and_name[1:])
        print('DL:', url, end='')
        sys.stdout.flush()
        time.sleep(1)
        if name:
            print(' to', name)
        if 'youtube' in url or 'youtu.be' in url:
            retval = dl_youtube(url, name, dl_dir)
        else:
            retval = dl_with_wget(url, name, dl_dir)
        if retval:
            print(f"FAIL: {url}")
            time.sleep(1)
            with dl_fails.open('a') as f:
                f.write(url)
        dl_list.write_text('\n'.join(lines[1:]))
    print(len(dl_list.read_text().splitlines()), "links left to download remain.")


if __name__ == "__main__":
    main()
