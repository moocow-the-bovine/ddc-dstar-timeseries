#!/bin/bash

#QUIET=""
#DUMMY=""
#KEEP=""

prog=$(basename "$0")
show_help() {
    cat <<EOF >&2

Usage: $prog [WEBDIR=.]

EOF
}

if [ "$1" = "-h" -o "$1" = "-help" -o "$1" = "-help" ]; then
    show_usage
    exit 1
fi

die() {
    echo "$prog ERROR: $*" >&2
    exit 255
}
runcmd() {
    [ -n "$DUMMY" -o -z "$QUIET" ] && echo "$prog: CMD $*" >&2
    [ -n "$DUMMY" ] && return 0
    "$@"
}

webdir="$1"; shift
[ -n "$webdir" ] || webdir="."
[ -d "$webdir" ] || die "web-dir '$webdir' not found"
[ -r "$webdir/dhist.db" ] || die "DB $webdir/dhist.db not readable"
[ -r "$webdir/dhist-cache.json" ] || die "cache $webdir/dhist-cache.json not readable"

tmpdir="$1"
[ -z "$tmpdir" ] && tmpdir="/tmp"

dir0=$(dirname "$0")
runcmd "$dir0/dhist-json-to-cache-data.sh" "$webdir/dhist-cache.json" -o "$tmpdir/dhist-cache-json.dat" \
    || die "failed to create $tmpdir/dhist-cache-json.dat"
runcmd "$dir0/dhist-db-to-cache-data.sh" "$webdir/dhist.db" -o "$tmpdir/dhist-cache-db.dat" \
    || die "failed to create $tmpdir/dhist-cache-db.dat"

rc=0
if runcmd diff -q "$tmpdir/dhist-cache-json.dat" "$tmpdir/dhist-cache-db.dat" ; then
    echo "ok $webdir"
else
    echo "FAILED $webdir"
fi

##-- cleanup
if [ -z "$KEEP" ] ; then
    runcmd rm -f "$tmpdir/dhist-cache-json.dat" "$tmpdir/dhist-cache-db.dat"
fi

exit $rc
