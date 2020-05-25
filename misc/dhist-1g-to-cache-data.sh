#!/bin/bash

infile="$1"; shift
[ -z "$infile" ] && infile="-"

tt-cut.awk '$1,$3,$4' "$infile" \
    | perl -ne 'print if (!/\t$/);' \
    | tt-1grams.perl -e=utf8 -union -lexsort "$@"
