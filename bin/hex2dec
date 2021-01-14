#!/usr/bin/env python3
"""
Convert passed hex values to decimal.
"""
import sys

for arg in sys.argv[1:]:
    prefix = '' if arg.startswith('0x') else '0x'
    print(f"{prefix}{arg} = {int(arg, 16)}")
