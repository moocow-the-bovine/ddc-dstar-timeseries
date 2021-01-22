#!/bin/bash

[ -n "$RML" ] && RML=/home/ddc-dstar/dstar/rml.d/rml
[ -n "$GENRE" ] && GENRE='textClass~s/:.*$//'
export RML GENRE

if [ $# -lt 1 ] ; then
    echo "Usge: $0 CONFILE [UNIT=y] >dhist.UNIT.1g" >&2
    exit 1
fi

CONFILE="$1"
UNIT="$2"
[ -z "$UNIT" ] && UNIT=y

if [ "$UNIT" = m ]; then
    dexpr='date~s/^([+-]?[0-9]+)(\-[0-9]{1,2})?.*/$1$2/'
elif [ "$UNIT" = d ] ; then
    dexpr='date'
else #if [ "$UNIT" = y ] ; then
    dexpr='date/1'
fi

qstr="count(* #sep) #by[$dexpr,$genre]"

set -o errexit
set -o pipefail
set -o xtrace
exec env -i RML="$RML" LC_CTYPE="en_US.UTF-8" LC_NUMERIC="C" \
     "$RML/bin/ddc_simple"  -v warn -limit -1 "$CONFILE" "$qstr"

