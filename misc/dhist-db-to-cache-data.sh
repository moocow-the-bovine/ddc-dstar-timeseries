#!/bin/bash

dir0=$(dirname "$0")
infile="$1"; shift
[ -z "$infile" ] && infile="-"

tt-db2dict.perl -of='"$_[1]\t$_[0]"' "$infile" \
    | tt-cut.awk '$1,$3,$4' - | tt-1grams.perl -e=utf8 -union -lexsort "$@"
