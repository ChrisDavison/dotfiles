#!/usr/bin/env python3
import subprocess
import os
import re
from pathlib import Path

bmFile=Path('~/Dropbox/org/asmr-bookmarks.org').expanduser()
rx = re.compile(r'.*\[\[(.*)]\[(.*)\]\].*')
entries = {}
for line in bmFile.read_text().splitlines():
    m = rx.match(line)
    if m:
        entries[m.group(2)] = m.group(1)

titles=sorted(entries.keys())

dmenu_config = Path("~/.config/dmenu.conf").expanduser().read_text().strip().split(' ')
print(dmenu_config)

choice = subprocess.run(["dmenu", *dmenu_config],
                        input='\n'.join(titles),
                        text=True,
                        capture_output=True)

tidy = choice.stdout.strip()
if tidy in entries:
    url = entries[choice.stdout.strip()]
    # subprocess.Popen(["mpv", url])
    subprocess.Popen(["firefox", "--new-tab", url])
    os._exit(0)
