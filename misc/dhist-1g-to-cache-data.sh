#!/bin/bash

infile="$1"; shift
[ -z "$infile" ] && infile="-"

tt-cut.awk '$1,$3,$4' "$infile" | tt-1grams.perl -e=utf8 -union -lexsort "$@"
