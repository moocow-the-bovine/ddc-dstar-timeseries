#!/bin/bash

## + requires perl-reversion from Perl::Version (debian package libperl-version-perl)
## + example call:
##    ./reversion.sh -bump -dryrun

pmfiles=(./ts-plot.perl lib/DDC/Dstar/TimeSeries.pm)

exec perl-reversion "$@" "${pmfiles[@]}"
