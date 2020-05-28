#!/usr/bin/perl -w

use lib qw(. ./lib ./lemmadist);

use DDC::Dstar::TimeSeries;
#use DDC::Dstar::TimeSeries::Outliers;

use File::Basename qw(basename dirname);
use File::Temp;

use CGI qw(:standard :cgi-lib);
use Encode qw(decode_utf8 encode_utf8);
use strict;

BEGIN {
  binmode(STDERR,':utf8');
}

##------------------------------------------------------------------------------
## Globals

our $debug   = 0;
our $prog    = basename($0);
our $progdir = $ENV{DSTAR_TS_ROOT} || dirname($0);

our $ts = DDC::Dstar::TimeSeries->new(prog=>$prog, debug=>$debug);

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
       pre("Histogram function disabled for this corpus."),
       end_html);
    exit 1;
  }
}
##-- END dstar config

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

my ($content); ##-- content buffer
eval {
  $content = $ts->plot();
};

##-- check for errors
if ($@) {
  my $msg = $@;
  print STDERR "$prog: Error: $msg\n";
  charset('utf-8');
  print
    (header(-status=>500),
     start_html('Error'),
     h1('Error'),"\n",
     pre(escapeHTML($msg)),
     end_html);
  exit 1;
}

##-- dump buffered output
charset('utf-8');
binmode(\*STDOUT, ":utf8") if (utf8::is_utf8($content));
print
  (($vars->{rawdata} ? qw() : header(%{$vars->{pfmt}{header}//{}})),
   ($content//'')
  );

##-- cleanup temporaries, if any (should actually happen in $ts destructor)
END {
  unlink $vars->{tmpfile} if ($vars && defined($vars->{tmpfile}) && -e $vars->{tmpfile});
}

