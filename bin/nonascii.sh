#!/usr/bin/env bash
rg "[^\x00-\x7F£\p{Greek}]" -o --no-heading
