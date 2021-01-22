#!/bin/bash

set -o pipefail
set -o errexit
set -o xtrace

tt-eval.perl '$_[2] =~ s/^([+-]?[0-9]+)(\-[0-9]{1,2})?.*/$1$2/;' "$@" \
    | tt-1grams.perl -union -lexsort -
