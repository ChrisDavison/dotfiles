#!/usr/bin/env python3
import re
import sys

re_pri = re.compile("\d+ \(([A-Z])\)")

tasks_with_priority = []
tasks_no_priority = []
for line in sys.stdin:
    pri = re_pri.match(line)
    if pri:
        tasks_with_priority.append((pri.group(1), line.strip()))
    else:
        tasks_no_priority.append(line.strip())
for _pri, task in sorted(tasks_with_priority):
    print(task)
for task in tasks_no_priority:
    print(task)
