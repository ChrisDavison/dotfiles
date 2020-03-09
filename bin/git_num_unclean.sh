#!/bin/bash

set -e

unclean=$(repoutil unclean | wc -l)
echo "Unclean: $unclean"
