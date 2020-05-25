#!/bin/bash

infile="$1"; shift
[ -z "$infile" ] && infile="-"

perl -CSD -0777 -MJSON -ne '$cache=from_json($_,{utf8=>0}); print map {"$cache->{$_}\t$_\n"} keys %$cache;' "$infile" \
     | tt-1grams.perl -e=utf8 -union -lexsort "$@"
