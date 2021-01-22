#!/bin/bash

dir0=$(dirname "$0")
if [ $# -lt 1 ] ; then
    echo "Usage: $0 CON_FILES..." >&2
    exit 1
fi

confiles=("$@")

set -o pipefail
set -o errexit
#set -o xtrace

mkdir -p fine
cond1g=()
for con in "${confiles[@]}"; do
    "$dir0/dhist-fine-con2ddat.sh" "$con" d >fine/$(basename "$con").d.1g
    cond1g[${#cond1g[@]}]=fine/$(basename "$con").d.1g
done

set -o xtrace
tt-1grams.perl -union "${cond1g[@]}" >"fine/dhist.d.1g"
"$dir0/dhist-fine-ddat2mdat.sh" "fine/dhist.d.1g" > "fine/dhist.m.1g"
"$dir0/dhist-fine-mdat2ydat.sh" "fine/dhist.m.1g" > "fine/dhist.y.1g"

    
