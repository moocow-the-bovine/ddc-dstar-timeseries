#!/usr/bin/perl -w

use lib qw(. ./lib ./lemmadist);

use DDC::Dstar::TimeSeries;
#use DDC::Dstar::TimeSeries::Outliers;

use File::Basename qw(basename dirname);
use File::Temp;

use Getopt::Long qw(:config no_ignore_case);
use CGI qw(:standard :cgi-lib);
use Encode qw(decode_utf8 encode_utf8);
use strict;

BEGIN {
  binmode(STDERR,':utf8');
}

##==============================================================
## Globals

our $prog    = basename($0);
our $progdir = $ENV{DSTAR_TS_ROOT} || '.';

##==============================================================
## Command-line
our ($help);
our %opts = (
	     gpVersionFile=>undef, ##-- don't cache gpversion.txt
	    );
our $outfile = '-';
our $initOnly = 0;
GetOptions(
	   'h|help' => \$help,
	   'd|dir|directory=s' => \$progdir,
	   'i|initialize|init-cache|initialize-cache!' => \$initOnly,
	   'o|out|output=s' => \$outfile,
	   'O|option|opt=s' => \%opts,
	   'D|debug!' => sub { $opts{debug}=($_[1] ? 255 : 0) },
	  );
if ($help) {
  print STDERR <<EOF;

Usage: $0 [OPTIONS] CGI_VARS...

Options:
  -h, -help        # this help message
  -d, -dir DIR     # base directory (default=\$ENV{DSTAR_TS_ROOT} || .)
  -o, -out OUTFILE # output file (default=-: stdout)
  -i, -initialize  # don't plot, just initialize cacheFile
  -O, -opt OPT=VAL # set DDC::Dstar::TimeSeries option (override dstar.rc, local.rc)
  -D, -debug       # enable debugging (like -O=debug=255)

Files:
  DIR/dstar.rc     # dstar configuration (perl code)
  DIR/local.rc     # configuration overrides (perl code)

EOF
    exit 1;
}

##==============================================================
## MAIN

our $ts = DDC::Dstar::TimeSeries->new(prog=>$prog, %opts);

##-- BEGIN dstar config
if (-r "$progdir/dstar.rc") {
  $ts->loadConfig("$progdir/dstar.rc")
    or die("$prog: failed to load '$progdir/dstar.rc': $@");

  ##-- check whether histograms were disabled by admin
  if (($ts->{dstar}{hist_enabled}//'yes') eq 'no') {
    print
      (header(-status=>500),
       start_html("Disabled"),
       h1("Disabled"),"\n",
       pre("Histogram function disabled for $progdir/"),
       end_html);
    exit 1;
  }
}
##-- END dstar config

##-- command-line options override dstar.rc
@$ts{keys %opts} = values %opts;

##------------------------------------------------------------------------------
## MAIN

##-- user parameters
our $vars = {};
if (param()) {
  $vars = { Vars() }; ##-- copy tied Vars()-hash, otherwise utf8 flag gets handled wrong!
}
$ts->parseRequest($vars)
  or die("$prog: failed to parse user request");

##-- local config (may override defaults)
if (-r "$progdir/local.rc") {
  $ts->loadConfig("$progdir/local.rc")
    or die("$prog: failed to load '$progdir/local.rc': $@");
}

##-- command-line options override local.rc too
@$ts{keys %opts} = values %opts;

##-- just initialize?
if ($initOnly) {
  $ts->init()
    or die("$prog: initialization FAILED.\n");
  print STDERR "$prog: initialization complete.\n";
  exit 0;
}

##-- plot guts
my $content = $ts->plot()
  or die("$prog: ", ref($ts)."::plot() failed: $!");

##-- dump output (no HTTP headers)
binmode(\*STDOUT, (utf8::is_utf8($content) ? ":utf8" : ":raw"));
print STDOUT ($content//'');

##-- cleanup temporaries, if any (should actually happen in $ts destructor)
END {
  unlink $vars->{tmpfile} if ($vars && defined($vars->{tmpfile}) && -e $vars->{tmpfile});
}
