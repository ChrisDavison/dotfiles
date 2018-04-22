#!/usr/bin/env python
"""
due.py
Python 2/3 script for todo.txt add-on
Created by Rebecca Morgan 2017-03-10
Copyright (c) 2017 Rebecca Morgan. All rights reserved.

Edits by Steve Winslow 2017-06-25
Edits copyright (c) 2017 Steve Winslow. Licensed under MIT.
"""

from __future__ import print_function

import os
import sys
from datetime import datetime, timedelta
import re


date_rx = re.compile(r'due:\d{4}-\d{2}-\d{2}')


def get_tasks_with_dates(filename):
    lines = open(filename, 'r').read().split('\n')
    lines_with_matches = []
    for i, l in enumerate(lines):
        m = date_rx.search(l)
        if m:
            date_from_line = datetime.strptime(m.group()[4:], "%Y-%m-%d")
            lines_with_matches.append((date_from_line, i+1, l))
    return lines_with_matches


def split_tasks_by_date(tasks, future_days=0):
    d_today = datetime.today()
    overdue = set(filter(lambda x: x[0] < d_today, tasks))
    today = set(filter(lambda x: x[0] == d_today, tasks))

    d_tomorrow = datetime.today() + timedelta(days=1)
    tomorrow = set(filter(lambda x: x[0] == d_tomorrow, tasks))

    d_future = datetime.today() + timedelta(days=future_days)
    future = set(filter(lambda x: x[0] < d_future, tasks))

    only_future = []
    if future_days == 0:
        only_future = list(future - tomorrow - today - overdue)
    overdue = sorted(list(overdue), key=lambda x: x[0])
    return overdue, list(today), list(tomorrow), only_future


def print_tasklist(tasklist, message):
    if tasklist:
        print("=" * 20)
        print(message)
        print("=" * 20)
        for _, i, l in tasklist:
            print("{:3s} {}".format(str(i), l))


def main(todo_file, future_days=0):
    # Prepare lists to store tasks
    overdue         = list()
    due_today       = list()
    due_tmr         = list()
    due_future      = list()
    tasks_with_date = list()

    # Open todo.txt file
    tasks = get_tasks_with_dates(todo_file)
    overdue, today, tomorrow, future = split_tasks_by_date(tasks)
    print_tasklist(overdue, "OVERDUE")
    print_tasklist(today, "Tasks due today")
    print_tasklist(tomorrow, "Tasks due tomorrow")
    print_tasklist(future, "Tasks due in next {} days".format(future_days))


if __name__ == '__main__':
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print("Usage: due.py [TODO_FILE] <future_days>")
        sys.exit(1)

    if os.path.isfile(sys.argv[1]):
        if len(sys.argv) is 3:
            main(sys.argv[1], int(sys.argv[2]))
        else:
            main(sys.argv[1])
    else:
        print("Error: %s is not a file" % sys.argv[1])
        sys.exit(1)
