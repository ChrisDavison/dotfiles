#!/usr/bin/env python3
import datetime
import os
import re
import sys


rx_date = re.compile('^\d+ (\d{4}-\d{2}-\d{2}) ')
    
today = datetime.date.today()
for line in sys.stdin:
    if line.startswith('x '):
        continue
    m = rx_date.match(line)
    if m and m.group(1) <= str(today):
        print(line.strip())
    elif not m:
        print(line.strip())
