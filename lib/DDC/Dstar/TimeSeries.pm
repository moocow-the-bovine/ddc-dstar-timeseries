##-*- Mode: CPerl; coding: utf-8; -*-
##
## File: DDC/Dstar/TimeSeries.pm
## Author: Bryan Jurish <jurish@bbaw.de>
## Description: class for DDC-based time series histograms ("Wortverläufe")
##==============================================================================

package DDC::Dstar::TimeSeries;

use DDC::Client::Distributed;
#use DDC::Dstar::TimeSeries::Outliers;
#use DB_File;

use File::Basename qw(basename dirname);
use Fcntl;
use JSON;
use POSIX qw(strftime);
#use Storable;
use File::Temp;
#use Date::Calc; ##-- required for slice units other than 'y' (=year)

use Encode qw(decode_utf8 encode_utf8);
use version;
use strict;

##==============================================================================
## Globals

##-- branched from dstar/corpus/web/dhist-plot.perl v0.37, svn r27690
our $VERSION = '0.60';

## $USE_DB_FAST : bitmask for 'useDB': fast regex parsing heuristics
our $USE_DB_FAST = 1;

## $USE_DB_PARSE : bitmask for 'useDB': full DDC parse with DDC::Any
our $USE_DB_PARSE = 2;

## $USE_DB_ANY : bitmask for 'useDB': fast or parse
our $USE_DB_ANY = ($USE_DB_FAST | $USE_DB_PARSE);

## $USE_DB_PREFIX : whether to use DB for prefix queries (fast)
our $USE_DB_PREFIX = 1;

## $USE_DB_SUFFIX : whether to use suffix-DB for suffix queries if present or auto-creatable (fast)
our $USE_DB_SUFFIX = 1;

## $CREATE_SUFFIX_DB : whether to auto-create suffix-DB (very slow)
our $CREATE_SUFFIX_DB = 0;

## $USE_DB_REGEX : whether to use DB for generic regex queries (SLOW: ($l=/ung$/)@kern: t(DDC)=7.3s ; t(DB)=30.5s)
our $USE_DB_REGEX = 0;

##==============================================================================
## Constructors etc.

##----------------------------------------------------------------------
## $ts = DDC::Dstar::TimeSeries->new(%opts)
##  + %$ts, %opts:
##    (
##     ##-- dstar configuration
##     prog => $prog,           ##-- program name for error reporting
##     dstar => {               ##-- common dstar configuration options
##      server_url => $url,     ##   - DDC server URL (new, requires DDC::Concordance >= v0.44)
##      server_host => $host,   ##   - DDC server host (old, backwards-compatible)
##      server_port => $port,   ##   - DDC server port (old, backwards-compatible)
##      corpus => $corpus,      ##   - DDC corpus label
##      hist_enabled => $bool,  ##   - histograms enabled?
##      hist_units => $units,   ##   - supported histogram time units (space-separated string; see 'unit' variable)
##      ##
##      ##-- stringification options (also available as globals)
##      stringifyComments => $bool,	##-- default=1
##      stringifyRoles => $bool,	##-- default=1
##      stringifyUser => $bool,		##-- default=1
##      stringifyPeer => $bool,		##-- default=1
##      stringifyOriginal => $bool,	##-- default=1
##     },
##     timeout => $timeout,     ##-- DDC client timeout (seconds)
##     limit => $limit,         ##-- DDC client limit (number of rows)
##     ##
##     ##-- plot parameters
##     ymin => $ymin,           ##-- minimum date (year)
##     ymax => $ymax,           ##-- maximum date
##     ##
##     ##-- DB_File options
##     useDB => $mask,          ##-- try and use local DB_File if available? (0:no, 1:fast(default), 2:parse, 3:fast-or-parse)
##     useDBPrefix => $bool,     ##-- use local DB_File for prefix queries? (fast)
##     useDBSuffix => $bool,     ##-- use local DB_File for suffix queries? (may be very slow, auto-generates suffix-db)
##     createSuffixDB => $bool,  ##-- auto-create suffix-db if required? (very slow)
##     useDBRegex => $bool,     ##-- use local DB_File for generic regex queries? (SLOW)
##     dbFile => $dbfile,       ##-- filename of local db (Berkeley DB; default="dhist.db")
##     dbIndices => \%indices,  ##-- indices for which to allow local DB queries (default={Lemma=>'Lemma', l=>'Lemma', ''=>'Lemma'})
##     dbExpand => \%expand,    ##-- expanders for which to allow local DB queries (default={Lemma=>'Lemma', l=>'Lemma', ''=>'Lemma', 'www'=>'www', 'web'=>'www', 'webLemma'=>'www'})
##     ##
##     ##-- low-level options
##     debug => $bool,          ##-- debug mode?
##     keeptmp => $bool,        ##-- keep temp files? (default=0)
##     useGenre => $bool,       ##-- auto-detect genres even if @genres is not set?
##     cacheFile => $file,      ##-- JSON file to load/store cache (undef=always update)
##     cacheMinAge => $secs,    ##-- minimum cache age for auto-update (in seconds, e.g. 60*60*24*7 ~ 1 week; undef=no minimum)
##     cacheUseInfo => $bool,   ##-- use slower but more reliable server 'info' request to get server timestamp? (default=0)
##     textClassKey => $key,    ##-- ddc-indexed textClass meta-attribute (should be merged with 'textClassTweak')
##     textClassTweak => $suff, ##-- ddc textClass meta-attribute count-by suffix ('~s/:.*//')
##     textClassU => $uclass,   ##-- "universal" genre for single-plot or grand-average mode
##     xBarClass => $xclass,    ##-- long x-bar class for bare plots
##     gpVersion => $version,   ##-- underlying gnuplot version string (output of `gnuplot --version`)
##     gpVersionFile => $file,  ##-- filename to cache gnuplot version (undef=don't cache ~ 14ms overhead)
##     gpVersionTTL => $secs,   ##-- max age of gpVersionFile for cache-read (default=60*60*24 = 1 day)
##     units => $units,         ##-- supported time units (space-separated string; see 'unit' variable and unit() method)
##     ##
##     ##-- guts
##     genres => \@genres,      ##-- genres to plot
##     client => $client,       ##-- underlying DDC::Client object
##     cache_y => \%cache_y,    ##-- global count-cache by year ( "${year}\t${class}" => $TOTAL, ... )
##     cache_m => \%cache_m,    ##-- global count-cache by year+month ( "${year}-${month}\t${class}" => $TOTAL, ... )
##     cache_d => \%cache_d,    ##-- global count-cache by year+month+day ( "${year}-${month}-${day}\t${class}" => $TOTAL, ... )
##     vars => \%vars,          ##-- parsed CGI request
##     dbhash => \%dbhash,      ##-- tied hash
##     dbtied => \$tied,        ##-- tied(%dbhash)
##     dbhashr => \%dbhashr,    ##-- tied hash (reversed lemmata, for suffix queries)
##     dbtiedr => \$tiedr,      ##-- tied(%dbhashr)
##    )
## + %vars
##   (
##    ##-- user parameters
##    ## see %defaults
##    ##
##    ##-- intermediate data (from plotInitialize())
##    sliceby => $sliceby,      ##-- numeric slice+0
##    pfmt => $pfmt,            ##-- convenience alias for $pformats{$vars{pformat}}
##    xr(min|max) => $xrminmax, ##-- parsed $vars{xrange} limits or ''
##    xu(min|max) => $xuminmax, ##-- parsed user $vars{xrange} limits or '*'
##    y(min|max) => $yminmax,   ##-- (min|max)-date at requested granularity, from rc-file, user request, or cache
##    classes => \@classes,     ##-- all genres for this query
##    dc2f => \%dc2f,           ##-- scaling constants (date+class): "$date\t$class"=>$f
##    c2f => \%c2f,             ##-- scaling constants (class): "$class"=>$f
##    d2f => \%d2f              ##-- scaling constants (date): "$date"=>$f
##    f_corpus => $f_corpus,    ##-- scaling constants (corpus): $f
##    slices => \@slices,       ##-- all slices for this query (honors $vars{gaps} request)
##    date2ymd => \&date2ymd,   ##-- ($y,$m,$d) = $date2ymd->($date_or_slice)
##    ymd2date => \&ymd2date,   ##-- $date = $ymd2date->($y,$m,$d)  # unused(?)
##    ymd2slice => \&ymd2slice, ##-- $slice = $ymd2slice->($y,$m,$d)
##    date2slice => \&date2slice,  ##-- $slice = $sliceof->($date)  # alias: 'sliceof'
##    date2int => \&date2int,   ##-- $dint = $date2int->($date) # at $unit-granularity (identity for $unit='y')
##    int2date => \&int2date,   ##-- $date = $int2date->($dint) # at $unit-granularity (identity for $unit='y')
##    sliceadd => \$sliceadd,   ##-- $slice_sum = $sliceadd->($date, $units) # implicitly uses $unit
##    ymd0 => \@ymd0,           ##-- ($y0,$m0,$d0) = @ymd0 : "origin" date for fine-slice computations (Mon 1-01-01; "Julian Day", also for Date::Calc::Date_to_Days)
##    alldates => \@alldates,   ##-- sorted list of all dates in $vars{xrange}, at $unit-resolution
##   )
sub new {
  my ($that,%args) = @_;
  my $prog = $args{prog} || basename($0);
  my $dir  = $args{dir} || $ENV{DSTAR_TS_ROOT} || dirname($0);
  my $obj  = bless({
		    ##-- dstar options
		    prog => $prog,
		    dir  => $dir,
		    dstar => {
			      #server_url  => 'inet://127.0.0.1:52000', ##-- NEW, preferred
			      server_host => '127.0.0.1',
			      server_port => '52000',
			      corpus => 'corpus',
			      hist_enabled => 'yes',
			      hist_units => 'y m d',
			      ##
			      stringifyComments => 1,
			      stringifyOriginal => 1,
			      stringifyUser => 1,
			      stringifyPeer => 1,
			      stringifyRoles => 1,
			     },
		    timeout => 300,
		    limit => 16384,

		    ##-- plot parameters
		    ymin => undef,
		    ymax => undef,

		    ##-- DB_File options
		    useDB => $USE_DB_ANY,
		    useDBPrefix => $USE_DB_PREFIX,
		    useDBSuffix => $USE_DB_SUFFIX,
		    useDBRegex => $USE_DB_REGEX,
		    createSuffixDB => $CREATE_SUFFIX_DB,
		    dbFile => "$dir/dhist.db",
		    dbIndices => {Lemma=>'Lemma', l=>'Lemma', ''=>'Lemma'},
		    dbExpand  => {Lemma=>'Lemma', l=>'Lemma', ''=>'Lemma', 'www'=>'www', 'web'=>'www', 'webLemma'=>'www'},

		    ##-- low-level options
		    debug => 0,
		    keeptmp => 0,
		    useGenre => 1,
		    cacheFile => "$dir/dhist-cache.json",
		    cacheMinAge => undef,
		    cacheUseInfo => 0,
		    textClassKey => 'textClass',
		    textClassTweak => '~s/:.*//',
		    textClassU => 'Gesamt',
		    xBarClass => '__XBARS__',

		    ##-- guts
		    genres => [],
		    client => undef,
		    cache_y => undef,
		    cache_m => undef,
		    cache_d => undef,
		    gpVersion => undef,
		    gpVersionFile => "$dir/gpversion.txt",
		    gpVersionTTL => (60*60*24),

		    ##-- user args
		    %args,
		   },
		   ref($that)||$that);
}

##----------------------------------------------------------------------
## undef = $ts->DESTROY()
##  + destructor removes tmpfile if defined
sub DESTROY {
  $_[0]->plotCleanup();
}

##----------------------------------------------------------------------
## $ts = $ts->loadConfig($rcfile)
##  + loads configuration from $rcfile
##  + hack uses a temporary package for backwards-compatibility with monolithic dhist-plot.perl
sub loadConfig {
  my ($ts,$rcfile) = @_;

  if (-r $rcfile) {
    no strict qw(vars refs);
    package DDC::Dstar::TimeSeries::Config;
    do $rcfile or die(__PACKAGE__, "::loadConfig(): failed to load '$rcfile': $@");

    my ($sym,$ref);
    foreach $sym (keys %DDC::Dstar::TimeSeries::Config::) {
      if     ( ($ref=*{"DDC::Dstar::TimeSeries::Config::$sym"}{HASH}) ) {
	if (!defined($ts->{$sym})) {
	  ##-- HASH: adopt
	  $ts->{$sym} = $ref;
	} else {
	  ##-- HASH: merge (clobber)
	  @{$ts->{$sym}}{keys %$ref} = values %$ref;
	}
      }
      elsif  ( ($ref=*{"DDC::Dstar::TimeSeries::Config::$sym"}{ARRAY}) )  { $ts->{$sym} = $ref; }
      elsif  ( ($ref=*{"DDC::Dstar::TimeSeries::Config::$sym"}{CODE}) )   { $ts->{$sym} = $ref; }
      elsif  ( ($ref=*{"DDC::Dstar::TimeSeries::Config::$sym"}{SCALAR}) ) { $ts->{$sym} = $$ref; }
    }
    %DDC::Dstar::TimeSeries::Config:: = qw(); ##-- clear temporary stash
  }

  return $ts;
}

##==============================================================================
## Subs: DDC client

##----------------------------------------------------------------------
## $client = $ts->ensureClient(%opts)
sub ensureClient {
  my $ts = shift;
  if (!$ts->{client}) {
    $ts->{client} = DDC::Client::Distributed->new(
						  connect => ($ts->{dstar}{server_url}
							      ? {url=>$ts->{dstar}{server_url}}
							      : {PeerAddr=>$ts->{dstar}{server_host},PeerPort=>$ts->{dstar}{server_port}}),
						  mode    => 'raw',
						  start    => 0,
						  limit    => $ts->{limit},
						  timeout  => $ts->{timeout},
						  encoding => 'utf8',
						 )
      or die(__PACKAGE__, "::ensureClient(): could not create DDC client: $!");
  }
  if (@_) {
    my %args = @_;
    @{$ts->{client}}{keys %args} = values(%args);
  }
  return $ts->{client};
}

##----------------------------------------------------------------------
## $responseData = $ts->requestData($ddcRequest,%opts)
sub ddcRequest {
  my ($ts,$reqstr,%opts) = @_;
  my $client = $ts->ensureClient(%opts);
  print STDERR __PACKAGE__, "::ddcRequest(".$client->addrStr."): $reqstr\n" if ($ts->{debug});
  my $rsp  = $client->queryRaw($reqstr)
    or die(__PACKAGE__, "::ddcRequest(): no response to request `$reqstr'");
  return from_json($rsp,{utf8=>(utf8::is_utf8($rsp) ? 0 : 1)});
}


##----------------------------------------------------------------------
## $responseData = $ts->ddcCounts($ddcQueryConditions, %ddcClientOpts)
sub ddcCounts {
  my ($ts,$qconds,%opts) = @_;

  ## Mon, 15 Feb 2016 13:54:09 +0100 moocow
  ##  + #has[textClass,/REGEX/] queries are expensive (~1600x slower than without)
  ##  + eliminate them here & parse requested genres elsewhere!
  #my $gconds = (@genres && $textClassKey ? (" #has[${textClassKey},/^(?:".join('|',@genres).")\\b/]") : '');

  ##-- hack: extract & preserve ddc-v2.1.16 lexer comments
  my $cmts = '';
  $qconds  =~ s{\s*(?:(?:#:[^\n]*\n?)|(?:#\[[^\]]*\]))}{$cmts .= ${^MATCH}; ''}gpe;

  ##-- hack: insert ddc-v2.1.16 lexer comments if requested
  if ($ts->{dstar}{stringifyComments}) {
    $cmts  = "\n$cmts" if ($cmts !~ /^\s*\R/);
    if ($ts->{dstar}{stringifyOriginal} && $cmts !~ /#:=/) {
      my $ostr = $qconds;
      $ostr =~ s/\n/ /sg;
      $cmts .= "#:=$ostr\n";
    }
    if ($ts->{dstar}{stringifyRoles} && $cmts !~ /#:~/) {
      $cmts .= "#:~dstar plot\n";
    }
    if (($ts->{dstar}{stringifyUser} || $ts->{dstar}{stringifyPeer}) && $cmts !~ /#:</) {
      $cmts .= ("#:<"
		.($ts->{dstar}{stringifyUser} ? ($ENV{REMOTE_USER} || "anonymous") : '?')
		.'@'
		.($ts->{dstar}{stringifyPeer} ? ($ENV{REMOTE_HOST} || $ENV{REMOTE_ADDR} || '?') : '-')
		."\n")
    }
    $cmts =~ s/\n+\z//;
  } else {
    #warn("comments disabled");
    $cmts = ''; ##-- supress even inherited comments
  }

  my $gkey = ($ts->{useGenre} && $ts->{textClassKey}
              ? ($ts->{textClassKey}.($ts->{textClassTweak}||''))
              : "@\'$ts->{textClassU}'");

  my $dunit = $ts->unit();
  my ($dexpr);
  if ($dunit eq 'm') {
    $dexpr = 'date~s/^([+-]?[0-9]+)(\-[0-9]{1,2})?.*/$1$2/';
  } elsif ($dunit eq 'd') {
    $dexpr = 'date';
  } else { #if ($dunit eq 'y')
    $dexpr = 'date/1';
  }

  my $qstr = "count($qconds #sep) #by[$dexpr,$gkey]".$cmts; #"count($qconds #sep $gconds) #by[$dexpr,$gkey]";
  $ts->ensureClient(mode=>'json', %opts);
  print STDERR __PACKAGE__, "::ddcCounts(".$ts->{client}->addrStr."): $qstr\n" if ($ts->{debug});
  my $rsp  = $ts->{client}->queryRaw($qstr)
    or die(__PACKAGE__, "::ddcCounts(): no response to count-query `$qstr'");
  my $data = from_json($rsp,{utf8=>(utf8::is_utf8($rsp) ? 0 : 1)});
  die (__PACKAGE__, "::ddcCounts(): error during count-query `$qstr': ", ($data->{error_}//'(unknown error)'))
    if (($data->{istatus_}//0)!=0 || ($data->{nstatus_}//0)!=0 || $data->{error_});

  #print STDERR "-> ", to_json($data,{canonical=>1,utf8=>1,pretty=>1}) if ($ts->{debug});
  return $data;
}

##==============================================================================
## Subs: local DB_File

##----------------------------------------------------------------------
## $bool = $ts->wantDB()
##  + returns true if we can at least try to use DB
##  + checks $ts->{useDB}, $ts->{dbFile}, $ts->{vars}{nodb}, $ts->{vars}{unit}
sub wantDB {
  my $ts = shift;
  return (($ts->{useDB} && $ts->{dbFile} && -r $ts->{dbFile})
	  #&& (!$ts->{vars} || ($ts->{vars}{usedb} && ($ts->{vars}{unit}||'y') eq 'y'))
	 );
}

##----------------------------------------------------------------------
## $tied_or_undef = $ts->ensureDB()
sub ensureDB {
  my $ts = shift;
  return undef if (!$ts->wantDB);
  return $ts->{dbtied} if (defined($ts->{dbtied}));

  require DB_File;
  $ts->{dbhash} = {};
  if (!tie(%{$ts->{dbhash}}, 'DB_File', $ts->{dbFile}, O_RDONLY, 0644, $DB_File::DB_BTREE)) {
    warn(__PACKAGE__, "::ensureDB(): failed to tie DB_File '$ts->{dbFile}': $!");
    return undef;
  }

  $ts->{dbtied} = tied(%{$ts->{dbhash}});
  return $ts->{dbtied};
}

##----------------------------------------------------------------------
## $tied_or_undef = $ts->ensureDBr()
sub ensureDBr {
  my $ts = shift;
  return undef if (!$ts->wantDB || !$ts->{useDBSuffix});
  return $ts->{dbtiedr} if (defined($ts->{dbtiedr}));

  my $tied    = $ts->ensureDB() or return undef;
  my $dbhashr = $ts->{dbhashr} = {};
  if (-e "$ts->{dbFile}r" && (!$ts->{createSuffixDB} || (file_mtime("$ts->{dbFile}r") >= file_mtime($ts->{dbFile})))) {
    ##-- open existing suffix database
    $ts->cachedebug("ensureDBr(): opening suffix DB $ts->{dbFile}r\n");
    if (!tie(%$dbhashr, 'DB_File', "$ts->{dbFile}r", O_RDONLY, 0644, $DB_File::DB_BTREE)) {
      warn(__PACKAGE__, "::ensureDB(): failed to tie suffix DB_File '$ts->{dbFile}r': $!");
      return undef;
    }
  }
  elsif ($ts->{createSuffixDB}) {
    ##-- auto-create suffix database
    $ts->cachedebug("ensureDBr(): auto-creating suffix DB $ts->{dbFile}r\n");
    if (!tie(%$dbhashr, 'DB_File', "$ts->{dbFile}r", O_RDWR|O_CREAT|O_TRUNC, 0644, $DB_File::DB_BTREE)) {
      warn(__PACKAGE__, "::ensureDBr(): failed to auto-create suffix DB_File '$ts->{dbFile}r': $!");
      return undef;
    }
    my ($status,$key,$val, $klemma,$krest);
    $key=$val=0;
    for ($status = $tied->seq($key,$val,&DB_File::R_FIRST);
	 $status == 0;
	 $status = $tied->seq($key,$val,&DB_File::R_NEXT)) {
      ($klemma,$krest) = split(/\t/,$key,2);

      utf8::decode($klemma) if (!utf8::is_utf8($klemma));
      $klemma = join('',reverse(split(//,$klemma)));  ##-- keep UTF-8 bytes in correct order
      utf8::encode($klemma) if (utf8::is_utf8($klemma));
      utf8::encode($krest)  if (utf8::is_utf8($krest));

      $dbhashr->{"$klemma\t$krest"} = $val;
    }
    $ts->cachedebug("ensureDBr(): suffix DB $ts->{dbFile}r created\n");
  }
  else {
    warn(__PACKAGE__, "::ensureDBr(): failed to open suffix DB $ts->{dbFile}r\n");
    return undef;
  }

  $ts->{dbtiedr} = tied(%$dbhashr);
  return $ts->{dbtiedr};
}

##----------------------------------------------------------------------
## $responseData = $ts->dbCounts(\@lemmata)
## $responseData = $ts->dbCounts($prefixQueryObject)
## $responseData = $ts->dbCounts($suffixQueryObject)
## $responseData = $ts->dbCounts(qr/REGEX/)
##  + variant of ddcCounts() using explicit lemma list to query a local DB
sub dbCounts {
  my ($ts,$lemmata) = @_;
  my $tied = $ts->ensureDB()
    or die(__PACKAGE__, "::dbCounts(): local DB ".($ts->{dbFile}//'-undef-')." not available");

  ##-- variables
  my ($useGenre,$textClassKey,$UCLASS) = @$ts{qw(useGenre textClassKey textClassU)};
  my ($lemma,%counts,$status,$key,$val, $klemma,$kdate,$kclass, $ftotal);

  if (UNIVERSAL::isa($lemmata,'ARRAY')) {
    ##-- guts: explicit lemma-list
    print STDERR __PACKAGE__, "::dbCounts($ts->{dbFile}): ", join(' ',@$lemmata), "\n" if ($ts->{debug});
    foreach $lemma (sort @$lemmata) {
      utf8::encode($lemma) if (utf8::is_utf8($lemma));

      for ($status = $tied->seq(($key="$lemma\t"),$val,&DB_File::R_CURSOR);
	   $status == 0;
	   $status = $tied->seq($key,$val,&DB_File::R_NEXT)) {
	($klemma,$kdate,$kclass) = split(/\t/,$key,3);
	last if ($klemma ne $lemma);
        utf8::decode($kclass) if ($useGenre && $textClassKey && $kclass && !utf8::is_utf8($kclass));
	$counts{$kdate."\t".($useGenre && $textClassKey ? ($kclass//'') : $UCLASS)} += $val;
      }
    }
  }
  elsif (UNIVERSAL::isa($lemmata,'DDC::Any::CQTokPrefix')) {
    ##-- guts: prefix query (should only happen if useDBPrefix was enabled)
    my $prefix = $lemmata->getValue;
    if ($ts->{debug}) {
      utf8::decode($prefix) if (!utf8::is_utf8($prefix));
      print STDERR __PACKAGE__, "::dbCounts($ts->{dbFile}): PREFIX $prefix\n";
    }

    utf8::encode($prefix) if (utf8::is_utf8($prefix));
    for ($status = $tied->seq(($key="$prefix"),$val,&DB_File::R_CURSOR);
	 $status == 0;
	 $status = $tied->seq($key,$val,&DB_File::R_NEXT)) {
      ($klemma,$kdate,$kclass) = split(/\t/,$key,3);
      last if (substr($klemma,0,length($prefix)) ne $prefix);
      utf8::decode($kclass) if ($useGenre && $textClassKey && $kclass && !utf8::is_utf8($kclass));
      $counts{$kdate."\t".($useGenre && $textClassKey ? ($kclass//'') : $UCLASS)} += $val;
    }
  }
  elsif (UNIVERSAL::isa($lemmata,'DDC::Any::CQTokSuffix')) {
    ##-- guts: suffix query (should only happen if useDBSuffix was enabled)
    my $rtied = $ts->ensureDBr()
      or die(__PACKAGE__, "::dbCounts(): local suffix DB ".($ts->{dbFile}//'-undef-')."r not available");

    my $suffix = $lemmata->getValue;
    utf8::decode($suffix) if (!utf8::is_utf8($suffix));
    print STDERR __PACKAGE__, "::dbCounts($ts->{dbFile}): SUFFIX $suffix\n" if ($ts->{debug});

    my $rsuffix = join('',reverse(split(//,$suffix)));
    utf8::encode($rsuffix) if (utf8::is_utf8($rsuffix));
    for ($status = $rtied->seq(($key="$rsuffix"),$val,&DB_File::R_CURSOR);
	 $status == 0;
	 $status = $rtied->seq($key,$val,&DB_File::R_NEXT)) {
      ($klemma,$kdate,$kclass) = split(/\t/,$key,3);
      last if (substr($klemma,0,length($rsuffix)) ne $rsuffix);
      utf8::decode($kclass) if ($useGenre && $textClassKey && $kclass && !utf8::is_utf8($kclass));
      $counts{$kdate."\t".($useGenre && $textClassKey ? ($kclass//'') : $UCLASS)} += $val;
    }
  }
  elsif (UNIVERSAL::isa($lemmata,'Regexp')) {
    ##-- guts: regular expression (should only happen if useDBRegex was enabled)
    print STDERR __PACKAGE__, "::dbCounts($ts->{dbFile}): REGEX $lemmata\n" if ($ts->{debug});
    $key=$val=0;
    for ($status = $tied->seq($key,$val,&DB_File::R_FIRST);
	 $status == 0;
	 $status = $tied->seq($key,$val,&DB_File::R_NEXT)) {
      ($klemma,$kdate,$kclass) = split(/\t/,$key,3);
      next if ($klemma !~ $lemmata);
      utf8::decode($kclass) if ($useGenre && $textClassKey && $kclass && !utf8::is_utf8($kclass));
      $counts{$kdate."\t".($useGenre && $textClassKey ? ($kclass//'') : $UCLASS)} += $val;
    }
  }
  else {
    die(__PACKAGE__, "::dbCounts(): can't handle query request of type ".(ref($lemmata)||'(scalar)')." with local DB");
  }

  return {counts_=>[map {[$counts{$_},split(/\t/,$_,2)]} keys %counts]};
}

##==============================================================================
## Subs: generic counts

##----------------------------------------------------------------------
## @uniq = luniq(@items)
sub luniq {
  my ($tmp);
  return map {defined($tmp) && $_ ne $tmp ? ($tmp=$_) : qw()} sort @_;
}

##----------------------------------------------------------------------
## $responseData = $ts->genericCounts($qstr,%ddcClientOpts)
##  + "smart" dispatch: use local DB if requested and available, otherwise DDC
sub genericCounts {
  my ($ts,$qstr,%ddcClientOpts) = @_;

  ##-- hack: ignore ddc-v2.1.16 lexer comments, sanitize query string
  my $qstr_in = $qstr;
  $qstr =~ s{
	      (?:\#(?:sep(?:arate)?|nojoin)(?:_hits)?)  ##-- #SEPARATE is implicit
	     |(?:\#(?:nosep(arate)?|join)(_hits)?)      ##-- #JOIN gets clobbered
             |(?:\#debug_rank)                          ##-- #DEBUG_RANK is vacuous for count()-queries
             |(?:\#(co?n?te?xt?|n))                     ##-- #CNTXT is vacuous for count()-queries
             |(?:\#rand(?:om)?(?:\s*\[[0-9]*\])?)	##-- #RANDOM is vacuous for count()-queries
             |(?:\#(?:less|asc|greater|de?sc)		##-- vacuous builtin sort operators
                 (?:_by)?
                 (?:_rank|_date|_size|_left|_right|_mid(?:dle))
                 (?:(?:\s*\[\s*\])
                   |(?!\s*\[)))
             |(?:(?:\#(?:less|asc|greater|de?sc)	##-- vacuous metadata sort operators
                 (?:_by)?
                 \s*\[[^,\]]+,*\]))
	    }{}sgix;
  $qstr =~ s/#:[^\n]*//sg;
  $qstr =~ s/\#\[[^\]]*\]//sg;
  $qstr =~ s/^\s+//s;
  $qstr =~ s/\s+$//s;
  print STDERR __PACKAGE__, "::genericCounts(): sanitized query string = \`$qstr'\n" if ($ts->{debug});

  if ($ts->wantDB && $qstr !~ m{[\s\#\[\]\"]|[\&\|]{2,}}) {
    ##-- try DB_File query
    my ($rsp);
    my $dbhow = ($ts->{vars}{usedb} // $ts->{useDB}) || '0';
    eval {
      if ($ts->unit() ne 'y') {
	die("slice unit '$ts->{vars}{unit}' not supported by DB");
      }
      elsif ($dbhow & $USE_DB_FAST) {
	##-- try DB_File query: fast regex hack
	print STDERR __PACKAGE__, "::genericCounts(): trying local DB [fast]\n" if ($ts->{debug});

	if ($qstr =~ m{^\s*\"?\s*(?:\$(?:l|Lemma)\s*=\s*)?\'?([[:alpha:]_\-\+\\]+)\'?\s*(?:\|([\w\s\-\|]+)\s*)?\"?\s*$}) {
	  my $lemma = unescapeDDC($1);
	  my $chain = $2 // $ts->{dbIndices}{''};
	  my $xvals = [$lemma];
	  if (grep {!exists($ts->{dbExpand}{$_})} split(/\s*\|\s*/,$chain)) {
	    ##-- expansion chain too complex (e.g. |Lemma for dta+dwds)
	    print STDERR __PACKAGE__, "::genericCounts(): expansion chain too complex for fast DB heuristics: ($chain)\n" if ($ts->{debug});
	  }
	  else {
	    ##-- expansion chain looks ok
	    if ($chain) {
	      print STDERR __PACKAGE__, "::genericCounts(): expand(chain=$chain,term=$lemma)\n" if ($ts->{debug});
	      $xvals = $ts->ensureClient(mode=>'raw')->expand($chain,$lemma)
		or die("failed to expand lemma '$lemma'");
	      foreach (@$xvals) {
		utf8::decode($_) if (!utf8::is_utf8($_));
	      }
	    }
	    $rsp = $ts->dbCounts($xvals)
	  }
	}
	die("query too complex for fast DB heuristics") if ( !($dbhow & $USE_DB_PARSE) );
      }
      if (!$rsp && ($dbhow & $USE_DB_PARSE)) {
	##-- try DB_File query: parse
	$@ = '';
	print STDERR __PACKAGE__, "::genericCounts(): trying local DB [parsed]\n" if ($ts->{debug});

	require DDC::Any or die("failed to require DDC::Any");
	DDC::Any->import if (!$DDC::Any::WHICH);
	die("no DDC query parser implementation") if (!$DDC::Any::WHICH);

	my $qobj = eval { ref($qstr_in) ? $qstr_in : DDC::Any->parse($qstr_in) };
	die("failed to parse query-string '$qstr_in' ".($@ ? ": $@" : '')) if (!$qobj);

	##-- can we realistically handle this query with the DB?
	my $qindex = ($qobj->can('getIndexName') ? $qobj->getIndexName : undef) // '';
	my $qclass = ref($qobj) // '';
	$qclass =~ s{.*::}{};

	die("detected non-trivial query `$qstr_in'")
	  if ($qclass !~ /^CQTok(?:Exact|Infl|Set|SetInfl|Prefix|Infix|Suffix|Regex)$/
	      || !exists($ts->{dbIndices}{$qindex})
	      || @{$qobj->getOptions->getFilters//[]}
	      || @{$qobj->getOptions->getSubcorpora//[]}
	      || ($qclass !~ /Infl/ && $qindex eq ''));

	if ($qclass =~ /Prefix$/ && $ts->{useDBPrefix}) {
	  ##-- special handling for prefix-conditions
	  $rsp = $ts->dbCounts($qobj);
	}
	elsif ($qclass =~ /Suffix$/ && $ts->{useDBSuffix}) {
	  ##-- special handling for suffix-conditions
	  $rsp = $ts->dbCounts($qobj);
	}
	elsif ($qclass =~ /(?:Prefix|Infix|Suffix|Regex)$/) {
	  ##-- special handling for generic regex-conditions
	  die("detected regex query `$qstr_in' but useDBRegex is disabled") if (!$ts->{useDBRegex});
	  my $qre = $ts->ddcRegex($qobj);
	  $rsp = $ts->dbCounts($qre);
	}
	else {
	  ##-- literal (set of) term-value(s): expand if requested
	  my @lemmata = $qclass =~ /Set/ ? @{$qobj->getValues} : ($qobj->getValue);
	  my $xvals   = \@lemmata;
	  my $chain   = $qobj->can('getExpanders') ? $qobj->getExpanders : [];
	  @$chain     = ('-') if ($qobj->can('getExpanders') && !@$chain);
	  @$chain     = map {($_//'-') =~ /^\-?$/ ? ($ts->{dbIndices}{$qindex}//$qindex) : $_} @$chain;
	  if (grep {!exists($ts->{dbExpand}{$_})} @$chain) {
	    ##-- expansion chain too complex (e.g. |Lemma for dta+dwds)
	    die("expansion chain too complex for parsed DB heuristics: (".join('|',@$chain).")\n");
	  } else {
	    ##-- expansion chain looks kosher: give it a whirl
	    if (@$chain) {
	      print STDERR __PACKAGE__, "::genericCounts(): expand(chain=".join('|',@$chain).",terms={".join(',',@lemmata)."})\n" if ($ts->{debug});
	      $xvals = $ts->ensureClient(mode=>'raw')->expand($chain,\@lemmata)
		or die("failed to expand lemmata");
	      foreach (@$xvals) {
		utf8::decode($_) if (!utf8::is_utf8($_));
	      }
	    }
	    $rsp = $ts->dbCounts($xvals);
	  }
	}
      }
    };
    return $rsp if ($rsp);
    print STDERR __PACKAGE__, "::genericCounts(): fetch from local DB failed: ", ($@||"???\n") if ($ts->{debug});
  }

  return $ts->ddcCounts($qstr_in,%ddcClientOpts);
}


##==============================================================================
## Subs: gnuplot version

##----------------------------------------------------------------------
## $gpVersionString = $ts->gpVersionString()
##  + gets full gnuplot version string
sub gpVersionString {
  my $ts = shift;
  return $ts->{gpVersion} if (defined($ts->{gpVersion}));

  my ($gpver,$cached);
  if ($ts->{gpVersionFile} && -r $ts->{gpVersionFile}) {
    if ($ts->file_mtime($ts->{gpVersionFile}) >= (time()-$ts->{gpVersionTTL})) {
      $ts->cachedebug("fetching gnuplot version from cache-file $ts->{gpVersionFile}\n") if ($ts->{debug});
      open(my $fh, "<$ts->{gpVersionFile}")
	or warn(__PACKAGE__, "::gpVersionString(): failed to open $ts->{gpVersionFile}: $!");
      $gpver  = <$fh> if ($fh);
      $cached = 1;
    } else {
      $ts->cachedebug("stale gnuplot version cache $ts->{gpVersionFile}",
		      " (mtime~", $ts->fileTimestamp($ts->{gpVersionFile}),
		      " < min~", $ts->timestamp(time()-$ts->{gpVersionTTL}),
		      ")\n") if ($ts->{debug});
      $cached = 0;
    }
  }
  if (!$gpver) {
    $ts->cachedebug("fetching gnuplot version from gnuplot\n");
    $gpver  = `gnuplot --version`;
    $cached = 0;
  }
  if ($gpver) {
    chomp($gpver);
    $ts->cachedebug("found gnuplot version string = \"$gpver\"\n");
  }

  ##-- store cached version
  if ($gpver && !$cached && $ts->{gpVersionFile}) {
    $ts->cachedebug("caching gnuplot version to $ts->{gpVersionFile}\n");
    open(my $fh, ">$ts->{gpVersionFile}")
      or warn(__PACKAGE__, "::gpVersionString(): failed to open $ts->{gpVersionFile}: $!");
    print $fh $gpver, "\n";
    close($fh)
      or warn(__PACKAGE__, "::gpVersionString(): failed to close $ts->{gpVersionFile}: $!");
  }

  ##-- return
  return $ts->{gpVersion} = $gpver;
}

## $gpVersion = $ts->gpVersion()
##  + gets major gnuplot version (numbers + dots), returned as a version object (or just string if unparseable)
sub gpVersion {
  my $ts = shift;
  my $gpver = $ts->gpVersionString or return undef;
  return version->parse($1) if ($gpver =~ /([0-9\.\-]+)/);
  return $gpver;
}


##==============================================================================
## Subs: cached scaling constants

##----------------------------------------------------------------------
## $timestamp = $ts->serverTimestamp()
##  + returns server timestamp as a string (via 'status' request)
sub serverTimestamp {
  my $ts = shift;

  if ($ts->{cacheUseInfo}) {
    ##-- use most recent 'indexed' entry for server 'info' request (slower, ca. 300 q/s)
    my $stamp = '1970-01-01T00:00:00Z';
    my @q     = ( $ts->ddcRequest('info', mode=>'raw') );
    my ($c);
    while (defined($c=shift(@q))) {
      $stamp = $c->{indexed} if ($c->{indexed} && $c->{indexed} gt $stamp);
      push(@q, @{$c->{corpora}}) if ($c->{corpora});
    }
    return $stamp;
  } else {
    ##-- use server start timestamp (faster but less reliable, ca. 1200 q/s)
    return $ts->ddcRequest("status", mode=>'raw')->{started};
  }
}

##----------------------------------------------------------------------
## $time = CLASS_OR_OBJECT->file_mtime($file)
sub file_mtime {
  my $that  = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  return (stat($_[0]))[9] // 0;
}

##----------------------------------------------------------------------
## $timestamp = CLASS_OR_OBJECT->fileTimestamp($file)
sub fileTimestamp {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  return $that->timestamp($that->file_mtime($_[0]))
}

##----------------------------------------------------------------------
## $timestamp = CLASS_OR_OBJECT->timestamp()
## $timestamp = CLASS_OR_OBJECT->timestamp($time)
sub timestamp {
  my $that  = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  return POSIX::strftime("%Y-%m-%dT%H:%M:%SZ", gmtime(@_ ? $_[0] : qw()));
}

##----------------------------------------------------------------------
## $bool = CLASS_OR_OBJECT->saveCache(\%cache,$cachefile)
sub saveCache {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my ($cache,$file) = @_;
  #return Storable::nstore($cache,$file);
  open(my $fh,">:utf8", "$file") or die(__PACKAGE__, "::saveCache(): open failed for $file: $!");
  print $fh to_json($cache, {utf8=>0,pretty=>1});
  close $fh;
}

##----------------------------------------------------------------------
## \%cache = CLASS_OR_OBJECT->loadCache($cachefile)
sub loadCache {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my $file = shift;
  #return Storable::retrieve($cachefile);

  open(my $fh,"<$file") or die(__PACKAGE__, "::loadCache(): open failed for $file: $!");
  local $/=undef;
  my $buf = <$fh>;
  close $fh;

  return from_json($buf, {utf8=>!utf8::is_utf8($buf)});
}

##----------------------------------------------------------------------
## undef = $ts->cachedebug(@msg)
##  + verbose cache debugging
sub cachedebug {
  my $ts = shift;
  if ($ts->{debug}) {
    print STDERR __PACKAGE__, " \[CACHEDEBUG]: ", @_;
  }
  return 1;
}

##----------------------------------------------------------------------
## \%cache = $ts->ensureCache()
## \%cache = $ts->ensureCache($unit)
sub ensureCache {
  my $ts = shift;
  my $unit = shift || $ts->unit();
  return $ts->{"cache_${unit}"} if (defined($ts->{"cache_${unit}"}));

  my $cachefile  = $ts->{cacheFile};
  $cachefile .= "_$unit" if ($unit ne 'y');

  my $cache_stamp      = $ts->fileTimestamp($cachefile);
  my $cache_ripe_stamp = $ts->timestamp(time() - ($ts->{cacheMinAge}//0));
  my ($server_stamp);

  $ts->cachedebug("cache:$cache_stamp ; ripe:$cache_ripe_stamp ; unit=$unit\n");

  if ( -r $cachefile && !-w $cachefile ) {
    ##-- read-only cache file: just load it
    $ts->cachedebug("read-only cache (stamp:$cache_stamp)\n");
  }
  elsif ( -r $cachefile && (-s $cachefile) && $cache_stamp ge $cache_ripe_stamp ) {
    ##-- cache file not yet old enough for update: just load it
    $ts->cachedebug("cache not yet ripe (cache:$cache_stamp > ripe:$cache_ripe_stamp)\n");
  }
  elsif ( !(-r $cachefile) || !(-s $cachefile) || $cache_stamp lt ($server_stamp=$ts->serverTimestamp()) ) {
    ##-- stale cache file: update it
    $server_stamp //= '(unknown)';
    print STDERR __PACKAGE__, "::ensureCache(): cache update required for unit=$unit (cache:$cache_stamp < server:$server_stamp)\n";
    my $data = $ts->ddcCounts("*");
    die (__PACKAGE__, "::ensureCache(): error during cache-update query: ", ($data->{error_}//($@||'(unknown error)')))
      if (!$data || ($data->{istatus_}//0)!=0 || ($data->{nstatus_}//0)!=0 || $data->{error_});

    ##-- populate cache data
    my $cache = {};
    my %classes = $ts->{useGenre} && @{$ts->{genres}//[]} ? (map {($_=>undef)} @{$ts->{genres}//[]}) : qw();
    foreach (@{$data->{counts_}}) {
      $_->[2] //= '';
      $_->[2] =~ s/:.*//;
      next if (($ts->{useGenre} && $_->[2] eq '') || (%classes && !exists($classes{$_->[2]})));
      $cache->{"$_->[1]\t$_->[2]"} += $_->[0];
    }

    ##-- store cache file
    $ts->cachedebug("saving cache-file $cachefile\n");
    if (!saveCache($cache,$cachefile)) {
      warn(__PACKAGE__, "::ensureCache(): failed to store cache to $cachefile: $!");
    } else {
      print STDERR __PACKAGE__, "::ensureCache(): cache updated for unit=$unit (new cache timestamp = ", $ts->fileTimestamp($cachefile), ")\n";
    }

    ##-- return
    return $ts->{"cache_${unit}"} = $cache;
  }

  if (!$ts->{"cache_${unit}"}) {
    ##-- just load cache-file
    $ts->cachedebug("re-loading cache data (cache~$cache_stamp ; server~".($server_stamp//'undef')." ; unit=$unit)\n");
    $ts->{"cache_${unit}"} = $ts->loadCache($cachefile)
      or die(__PACKAGE__, "::ensureCache(): failed to retrieve cache data from $cachefile: $!");
  }
  return $ts->{"cache_${unit}"};
}

##----------------------------------------------------------------------
## \%cache = $ts->cache()
## \%cache = $ts->cache($unit)
##  + does NOT auto-load or auto-populate
sub cache {
  my $ts = shift;
  return $ts->{"cache_".($_[0]||$ts->unit)};
}

##----------------------------------------------------------------------
## $unit = $ts->unit()
## $unit = $ts->unit($unit)
##  + gets unit from $ts->{vars}{unit} || $ts->{unit} || 'y'
##  + checks for unit support in $ts->{dstar}{hist_units}
sub unit {
  my $ts = shift;
  my $unit = (shift
	      || ($ts->{vars} ? $ts->{vars}{unit} : undef)
	      || $ts->{unit}
	      || 'y');

  ##-- sanity check
  my $units = ($ts->{units}
	       || ($ts->{dstar} ? $ts->{dstar}{hist_units} : undef)
	       || 'y'
	      );
  $units = $ts->{units} = {map {($_=>undef)} split(' ',$units)} if (!ref($units));
  if (defined($units) && !exists($units->{$unit})) {
    die(__PACKAGE__, " Error: unsupported unit '$unit' should be one of (".join(' ',sort keys %$units).")");
  }

  return $unit;
}

##==============================================================================
## Subs and Data: Parameters

##----------------------------------------------------------------------
## %defaults: parameter defaults
my %defaults =
  (
   query=>'',		##-- target query (aka "lemma")
   unit=>'',            ##-- slice unit (y:years:default, m:months, d:days)
   slice=>'10',		##-- slice width (UNITs), (UNITs+offset); general: (UNITS(+OFFSET)?UNIT?), UNIT={y,m,d}
   offset=>'',		##-- slice offset (empty uses xrange)
   norm=>'abs',		##-- normalization mode
   logproj=>0,		##-- do log-linear projection?
   logavg=>0,		##-- do log-linear smoothing?
   window=>0,		##-- moving-average smoothing window width (slices)
   wbase=>0,		##-- inverse-distance smoothing base
   totals=>0,		##-- plot totals?
   single=>0,		##-- plot single-curve only (true) or each genre separatately (false)?
   grand=>0,		##-- include grand-average curve (implied by single=1)?
   gaps=>0,		##-- allow gaps for missing (zero) values?
   prune=>0,		##-- inverse confidence level for outlier detection (0: no pruning, .05 ~ 95% confidence level)
   pformat=>'png',	##-- target plot format qw(text json gnuplot png ...)

   ##-- gnuplot-only options
   xlabel => 'date',
   ylabel => '',    ##-- default: from plot data
   xrange => "*:*",
   yrange => "0:*",
   logscale => 0,
   title => '',  ##-- default: auto
   size   => '640,480',  ##-- plot size (w,h)
   key    => '', #'outside right center box', ##-- legend; 'off' or 'none' to supporess
   smooth => 'none', #qw(none bezier csplines)
   style  => 'l',    #qw(lines points linespoints)
   grid   => 0,	     ##-- plot grid?
   bare   => 0,     ##-- produce "bare" curve for web display?

   ##-- json-only options
   pretty => 0,

   ##-- debugging options
   debug => 0,
   #usedb => 0, ##-- 0:no, 1:fast, 2:parse, 3:fast-or-parse; default=$ts->{usedb}
  );

##----------------------------------------------------------------------
## %aliases: parameter aliases
my %aliases =
  (
   query=>[qw(query qu q lemma lem l)],
   slice=>[qw(sliceby slice sl s)],
   unit=>[qw(unit u)],
   offset=>[qw(offset off)],
   norm=>[qw(normalize norm n)],
   logproj=>[qw(logproject logp lp)],
   logavg=>[qw(logavg loga la lognorm logn log ln)],
   window=>[qw(window win w)],
   wbase=>[qw(wbase wb W)],
   totals=>[qw(totals tot T)],
   single=>[qw(single sing sg)],
   grand=>[qw(grand gr g)],
   gaps=>[qw(gaps gap)],
   prune=>[qw(prune pr)],
   pformat=>[qw(pformat pfmt pf format fmt f)],

   ##-- gnuplot-only options
   xlabel=>[qw(xlabel xlab xl)],
   ylabel=>[qw(ylabel ylab yl)],
   xrange=>[qw(xrange xr)],
   yrange=>[qw(yrange yr)],
   logscale=>[qw(logscale lscale ls logy ly)],
   size=>[qw(psize psiz psz size siz sz)],
   key=>[qw(key legend leg)],
   smooth => [qw(smooth sm)], #qw(none bezier csplines)

   ##-- json only options
   pretty => [qw(pretty)],

   ##-- debugging options
   debug => [qw(debug)],
   usedb => [qw(usedb useDB trydb tryDB db)], ##-- 0:no, 1:fast, 2:parse, 3:any; default=$ts->{useDB}
  );

##----------------------------------------------------------------------
## %pformats: known plot-formats
my %pformats =
  (
   null=>{label=>'null',header=>{-type=>'text/plain'},charset=>'utf-8'},
   json=>{label=>'json',header=>{-type=>'application/json'}, charset=>'utf-8'},
   text=>{label=>'text',header=>{-type=>'text/plain'}, charset=>'utf-8'},
   gnuplot=>{label=>'gnuplot',header=>{-type=>'text/plain'}, charset=>'utf-8'},
   png=>{label=>'png',header=>{-type=>'image/png'},term=>'png size $SIZE'},
   svg=>{label=>'svg',header=>{-type=>'image/svg+xml'},term=>'svg size $SIZE'},
   eps=>{label=>'eps',header=>{-type=>'application/postscript',-attachment=>'dhist-plot.eps'},term=>'postscript eps color'},
   ps=>{label=>'ps',header=>{-type=>'application/postscript',-attachment=>'dhist-plot.ps'},term=>'postscript color'},
   pdf=>{label=>'pdf',header=>{-type=>'application/pdf'},term=>'pdf color'},
   epsmono=>{label=>'eps',header=>{-type=>'application/postscript',-attachment=>'dhist-plot.eps'},term=>'postscript eps mono'},
   psmono=>{label=>'psmono',header=>{-type=>'application/postscript',-attachment=>'dhist-plot.ps'},term=>'postscript mono'},
   pdfmono=>{label=>'pdf',header=>{-type=>'application/pdf'},term=>'pdfcairo mono'},
  );
$pformats{postscript} = $pformats{ps};
$pformats{$_}=$pformats{text} foreach (qw(txt tab tsv csv dat));
$pformats{gp}=$pformats{gnuplot};

##----------------------------------------------------------------------
## \%vars = $ts->parseRequest(\%vars)
##  + parses request in \%vars (usually { CGI::Vars() })
##  + may destructively alter \%vars, sets $ts->{vars}=\%vars
sub parseRequest {
  my ($ts,$vars) = @_;

  ##-- low-level variables
  $ts->{timeout} = $vars->{timeout} if ($vars->{timeout});
  $ts->{debug}   = $vars->{debug} if ($vars->{debug});

  ##-- sanitize vars
  foreach (keys %$vars) {
    next if (!defined($vars->{$_}));
    my $tmp = $vars->{$_};
    $tmp =~ s/\x{0}//g;
    eval {
      ##-- try to decode utf8 params e.g. "%C3%B6de" for "öde"
      $tmp = decode_utf8($tmp,1) if (!utf8::is_utf8($tmp) && utf8::valid($tmp));
    };
    if ($@) {
      ##-- decoding failed; treat as bytes (e.g. "%F6de" for "öde")
      utf8::upgrade($tmp);
      undef $@;
    }
    $vars->{$_} = $tmp;
  }

  ##-- parameter aliases
  my ($vkey,$akey);
  foreach $vkey (keys %aliases) {
    if (defined($akey = (grep {exists($vars->{$_})} @{$aliases{$vkey}})[0])) {
      $vars->{$vkey} = $vars->{$akey};
      next;
    }
  }

  ##-- adopt vars
  $ts->{vars} = $vars;
  return $vars;
}

##==============================================================================
## Subs: miscellaneous

##----------------------------------------------------------------------
## ($y,$m,$d) = CLASS_OR_OBJECT->dateSplit($date1,$date2)
sub dateSplit {
  shift if (UNIVERSAL::isa($_[0],__PACKAGE__));
  return map {($_||1)+0} (split(/(?<=[0-9])-/,$_[0],3))[0..2];
}

##----------------------------------------------------------------------
## $date = CLASS_OR_OBJECT->dateJoin($y,$m,$d)
sub dateJoin {
  shift if (UNIVERSAL::isa($_[0],__PACKAGE__));
  return join('-', map {($_||1)+0} @_[0..2]);
}

##----------------------------------------------------------------------
## $cmp = CLASS_OR_OBJECT->datecmp($date1,$date2)
##  + generic 3-way date comparison operator
sub dateCmp {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my @d1 = dateSplit($_[0]);
  my @d2 = dateSplit($_[1]);
  return ($d1[0] <=> $d2[0] || $d1[1] <=> $d2[1] || $d1[2] <=> $d2[2]);
}

##----------------------------------------------------------------------
## $str = CLASS_OR_OBJECT->unescapeDDC($str)
sub unescapeDDC {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my $str  = shift;
  $str =~ s{\\x\{([[:xdigit:]]+)\}}{pack('U0C',hex($1))}eg;
  $str =~ s{\\([0-7]{1,3})}{pack('U0C',oct($1))}eg;
  $str =~ s{\\x([[:xdigit:]]{1,2})}{pack('U',hex($1))}eg;
  $str =~ s{\\u([[:xdigit:]]{1,8})}{pack('U',hex($1))}eg;
  $str =~ s{\\a}{\a}g;
  $str =~ s{\\b}{\b}g;
  $str =~ s{\\t}{\t}g;
  $str =~ s{\\n}{\n}g;
  $str =~ s{\\v}{\x{0b}}g;
  $str =~ s{\\f}{\f}g;
  $str =~ s{\\r}{\r}g;
  $str =~ s{\\(.)}{$1}g;
  return $str;
}

##----------------------------------------------------------------------
## $max = CLASS_OR_OBJECT->rowmax($key,\@rows)
## $min = CLASS_OR_OBJECT->rowmax($key,\@rows,$cmp)
##  + returns max $_->{$key} foreach (@$rows), or -inf if none
sub rowmax {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my ($key,$rows,$cmp) = @_;
  $cmp //= sub { $_[0] <=> $_[1] }; ##-- default: numeric comparison
  my ($max);
  foreach (@$rows) {
    $max = $_->{$key} if (!defined($max) || (defined($_->{$key}) && $cmp->($_->{$key},$max) > 0));
  }
  return $max // '-inf';
}

##----------------------------------------------------------------------
## $min = CLASS_OR_OBJECT->rowmin($key,\@rows)
## $min = CLASS_OR_OBJECT->rowmin($key,\@rows,$cmp)
##  + returns min $_->{$key} foreach (@$rows), or inf if none
sub rowmin {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my ($key,$rows,$cmp) = @_;
  $cmp //= sub { $_[0] <=> $_[1] }; ##-- default: numeric comparison
  my ($min);
  foreach (@$rows) {
    $min = $_->{$key} if (!defined($min) || (defined($_->{$key}) && $cmp->($_->{$key},$min) < 0));
  }
  return $min // 'inf';
}


##----------------------------------------------------------------------
## $perl_regex  = CLASS_OR_OBJECT->ddcRegex($query_object)
##  + returns a perl qr// regex for DDC::Any query object $query_object,
##    which should be a CQ(Prefix|Infix|Suffix|Regex)
sub ddcRegex {
  my $that = UNIVERSAL::isa($_[0],__PACKAGE__) ? shift : __PACKAGE__;
  my $qobj = shift;
  return $qobj if (UNIVERSAL::isa($qobj,'Regexp'));

  my ($re_str);
  if (UNIVERSAL::isa($qobj,'DDC::Any::CQTokPrefix')) {
    $re_str = '^'.quotemeta($qobj->getValue);
  }
  elsif (UNIVERSAL::isa($qobj,'DDC::Any::CQTokSuffix')) {
    $re_str = quotemeta($qobj->getValue).'$';
  }
  elsif (UNIVERSAL::isa($qobj,'DDC::Any::CQTokSuffix')) {
    $re_str = quotemeta($qobj->getValue);
  }
  elsif (UNIVERSAL::isa($qobj,'DDC::Any::CQTokRegex')) {
    $re_str = $qobj->getValue;
  }
  else {
    die(__PACKAGE__, "::ddcRegex(): failed to extract regex from query (", $qobj->toStringFull, ")");
  }

  return qr{$re_str};
}



##==============================================================================
## Subs: top-level operations

##----------------------------------------------------------------------
## $data = $ts->plot()
## $data = $ts->plot(\%vars)
##  + wrapper for plotInitialize(), plotFetchCounts(), ...
##  + if \%vars is defined, calls parseRequest() first
sub plot {
  my $ts = shift;
  $ts->parseRequest(@_) if (@_);
  $ts->plotInitialize();
  $ts->plotFetchCounts();
  $ts->plotFill();
  $ts->plotNormalize();
  $ts->plotPrune();
  $ts->plotSmooth();
  $ts->plotCollect();
  my $content = $ts->plotContent();
  $ts->plotCleanup() if (!$ts->{keeptmp}); ##-- cleanup any stale leftovers
  return $content;
}

##----------------------------------------------------------------------
## $bool = $ts->init()
## $bool = $ts->init(\%vars)
##  + re-initialize cache data
##  + if \%vars is defined, calls parseRequest() first
##  + accepts additional %$ts options:
##     force => $bool,   ##-- if true or undefined, force-unlinks old cache file(s) first
sub init {
  my $ts = shift;
  $ts->parseRequest(@_) if (@_);

  ##-- remove old cache data
  if (($ts->{force}//1)) {
    $ts->cachedebug("forcing (re-)initialization\n");
    foreach my $fkey (qw(cacheFile gpVersionFile)) {
      my $file = $ts->{$fkey};
      (!$file)
	or (!-e $file)
	  or ( $ts->cachedebug("UNLINK $file\n") && unlink($file) )
	    or die(__PACKAGE__, "::init(): failed to unlink old $fkey '$file': $!");
    }
  }

  $ts->plotInitialize();
  $ts->gpVersionString() if ($ts->{gpVersionFile}); ##-- ensure gpversion.txt

  return $ts;
}

##==============================================================================
## Subs: mid-level guts

##----------------------------------------------------------------------
## $ts = $ts->plotCleanup()
##  + cleans up local state
sub plotCleanup {
  my $ts = shift;
  $ts->cachedebug("plotCleanup()\n");

  if (!$ts->{keeptmp} && $ts->{vars} && $ts->{vars}{tmpfile}) {
    my $tmpfile = $ts->{vars}{tmpfile};
    $ts->cachedebug("UNLINK $tmpfile\n");
    unlink($tmpfile) if (defined($tmpfile) && -e $tmpfile);
  }

  ##-- done with vars
  delete $ts->{vars};

  return $ts;
}

##----------------------------------------------------------------------
## $ts = $ts->plotInitialize()
##  + sets variable defaults, initializes local state
sub plotInitialize {
  my $ts = shift;
  my $vars = ($ts->{vars} //= {});
  $ts->cachedebug("plotInitialize(): ", __PACKAGE__, " v$VERSION\n");

  ##-- variable defaults
  $vars->{prune} = 0.05 if ($vars->{bare} && ($vars->{prune}//'') eq ''); ##-- default prune=.05 for 'bare' plots
  $vars->{usedb} = $ts->{useDB} if (($vars->{usedb}//'') eq '');
  $vars->{$_} = $ts->{defaults}{$_} foreach (grep {($vars->{$_}//'') eq ''} keys %{$ts->{defaults}//{}}); ##-- in case e.g. local.rc overrides %defaults
  $vars->{$_} = $defaults{$_}       foreach (grep {($vars->{$_}//'') eq ''} keys %defaults);

  ##-- dump vars (debug)
  # v-- errors "Thread 1 terminated abnormally: hash- or arrayref expected (not a simple scalar, use allow_nonref to allow this) at /usr/share/perl5/JSON.pm line 154."
  #     when running under forks.pm
  print STDERR "$ts->{prog} dstar: ", JSON::to_json($ts->{dstar},{utf8=>0,pretty=>1,canonical=>1}), "\n" if ($ts->{debug});
  print STDERR "$ts->{prog} variables: ", JSON::to_json($vars,{utf8=>0,pretty=>1,canonical=>1}), "\n" if ($ts->{debug});

  ##-- variable-dependent conveniences
  $vars->{sliceby} = do { no warnings 'numeric'; ($vars->{slice}+0); };
  $vars->{grand} ||= $vars->{single} || !$ts->{textClassKey};
  $vars->{smooth}  = ($vars->{smooth} =~ /^(?:no?(?:ne)?)?$/ ? '' : $vars->{smooth});
  $vars->{pfmt}    = $pformats{$vars->{pformat}//''};
  die(__PACKAGE__, "::plotInitialize(): unknown plot-format '$vars->{pformat}'") if (!defined($vars->{pfmt}));

  ##-- parse slice, offset, unit
  if ($vars->{slice} =~ m{^\s*([+-]?[0-9]+)(\s*[\s+-]\s*[0-9]+)?(\s*[ymd]?)}i) {
    $vars->{sliceby} = $1;
    $vars->{offset}  = $2 if (($vars->{offset}//'') eq '');
    $vars->{unit}    = lc($3) if (($vars->{unit}//'') eq '');
  }
  ##-- sanity checks: slice unit
  $vars->{unit} = 'y' if ($vars->{sliceby}==0);
  $vars->{unit} ||= 'y';
  my $unit = $ts->unit($vars->{unit});
  die("unknown slice unit '$unit' must be one of {y,m,d}") if ($unit !~ /^[ymd]$/);
  if ($unit ne 'y') {
    #die(__PACKAGE__, "::plotInitialize(): can't handle nonzero offset for slice unit '$unit'") if ($vars->{offset});
    require Date::Calc;
    #require Date::Parse;
  }

  ##-- range variables
  my ($xrmin,$xrmax) = map {$_//''} split(/:/, ($vars->{xrange}||'*:*'), 2);
  my ($xumin,$xumax) = map {$_||'*'} ($xrmin,$xrmax); ##-- user x-range request
  my ($ymin,$ymax)   = @$ts{qw(ymin ymax)};
  if (!defined($ymin) || !defined($ymax)) {
    $ymin //= $xrmin;
    $ymax //= $xrmax;
    if (($ymin//'') =~ /^\*?$/) {
      my $cache = $ts->ensureCache();
      $ymin = (sort {$a cmp $b} map {(split(/\t/,$_))[0]} keys %$cache)[0];
    }
    if (($ymax//'') =~ /^\*?$/) {
      my $cache = $ts->ensureCache();
      $ymax = (sort {$b cmp $a} map {(split(/\t/,$_))[0]} keys %$cache)[0];
    }
  }
  $ts->cachedebug("computed ymin=$ymin , ymax=$ymax\n");
  @$vars{qw(ymin ymax)} = ($ymin,$ymax);

  ##-- genre variables
  warn(__PACKAGE__, "::plotInitialize(): WARNING: {useGenre} is false, but {genres} is neither empty nor the singleton {textClassU}: expect weirdness")
    if (!$ts->{useGenre} && @{$ts->{genres}//[]}>=1 && $ts->{textClassU} ne $ts->{genres}[0]);
  if (!@{$ts->{genres}//[]}) {
    $ts->ensureCache();
    my %genres = (map {(split(/\t/,$_))[1]=>undef} keys %{$ts->cache});
    $ts->{genres} = [grep {($_//'') ne ''} sort keys %genres];
  }
  $vars->{grand} ||= !@{$ts->{genres}};


  ##-- slice arithmetic
  ## + TODO(?): figure out a good general way to assign "origin" date for units other than 'y'
  ##   - current code uses "Julian Day" Mon 1-01-01
  my ($sliceby,$offset) = @$vars{qw(sliceby offset)};
  my ($ymd0, $date2ymd,$ymd2date, $date2int,$int2date, $ymd2slice,$date2slice, $sliceadd,$datecmp);
  $ymd0       = $vars->{ymd0}     = [1,1,1]; #[$date2ymd->('1970-01-01')]; ##-- origin date (for fine-slices)
  $date2ymd   = $vars->{date2ymd}  = \&dateSplit;
  $ymd2date   = $vars->{ymd2date}  = \&dateJoin;

  if ($sliceby == 0) {
    ##-- SLICEBY=0 (global ~ years)
    $date2slice = $ymd2slice = $sliceadd = sub { 0 };
    $datecmp    = sub { $_[0] <=> $_[1] };
  }
  elsif ($unit eq 'd') {
    ##-- UNIT=d (days)
    $ymd2slice = sub { sprintf("%d-%02d-%02d", map {($_||1)} @_[0..2]) };
    $sliceadd  = sub { $ymd2slice->(Date::Calc::Add_Delta_Days($date2ymd->($_[0]), $_[1])) };
    $date2int  = sub { Date::Calc::Delta_Days(@$ymd0,$date2ymd->($_[0])); };
    $int2date  = sub { $ymd2slice->(Date::Calc::Add_Delta_Days(@$ymd0,$_[0])); };
    $date2slice = sub { return $int2date->(int(($date2int->($_[0])-$offset)/$sliceby) * $sliceby + $offset); };

    #sub { $date2days->($a) <=> $date2days->($b) };
    $datecmp = \&dateCmp;
    if (($vars->{offset}//'') eq '' && $vars->{sliceby} && $xumin ne '*') {
      ##-- default offset from user-specified xrange
      $offset = $vars->{offset} = Date::Calc::Delta_Days(@$ymd0,$date2ymd->($xumin)) % $vars->{sliceby};
    }
  }
  elsif ($unit eq 'm') {
    ##-- UNIT=m (months)
    $ymd2slice = sub { sprintf("%d-%02d", map {($_||1)} @_[0..1]) };
    $sliceadd  = sub { $ymd2slice->(Date::Calc::Add_Delta_YM($date2ymd->($_[0]), 0,$_[1])) };
    my ($Dy,$Dm,$Dd);
    $date2int  = sub { ($Dy,$Dm,$Dd) = Date::Calc::Delta_YMD(@$ymd0,$date2ymd->($_[0])); return 12*$Dy + $Dm; };
    $int2date  = sub { $ymd2slice->(Date::Calc::Add_Delta_YM(@$ymd0, 0,$_[0])); };
    $date2slice = sub { return $int2date->(int(($date2int->($_[0])-$offset)/$sliceby) * $sliceby + $offset); };
    #sub { $date2days->($a) <=> $date2days->($b) };
    $datecmp = \&dateCmp;
    if (($vars->{offset}//'') eq '' && $vars->{sliceby} && $xumin ne '*') {
      ##-- default offset from user-specified xrange
      ($Dy,$Dm,$Dd) = Date::Calc::Delta_YMD(@$ymd0,$date2ymd->($xumin));
      $offset = $vars->{offset} = ($Dm+12*$Dy) % $sliceby;
    }
  }
  else { #($unit eq 'y')
    ##-- UNIT=y (years)
    $ymd2slice = sub { $_[0] };
    $sliceadd  = sub { $_[0]+$_[1] };
    $date2int  = $int2date = sub { $_[0] };
    $date2slice = sub { int(($_[0]-$offset)/$sliceby)*$sliceby + $offset; };
    $datecmp    = sub { $_[0] <=> $_[1] };
    if (($vars->{offset}//'') eq '' && $vars->{sliceby} && $xumin ne '*') {
      ##-- default offset from user-specified xrange
      $offset = $vars->{offset} = $xumin % $vars->{sliceby};
    }
  }
  @$vars{qw(ymd2slice date2slice sliceadd date2int int2date datecmp)} = ($ymd2slice,$date2slice,$sliceadd,$date2int,$int2date,$datecmp);
  my $sliceof = $vars->{sliceof} = $date2slice; ##-- alias

  #my ($date2days,$days2date,$days2slice);
  #$date2days  = $vars->{date2days} = sub { Date::Calc::Date_to_Days($date2ymd->($_[0])) };
  #$days2date  = $vars->{days2date} = sub { Date::Calc::Add_Delta_Days($ymd0, $_[0]-1); };
  #$days2slice = $vars->{days2slice} = sub { $date2slice->( $days2date->($_[1]) ); };

  ##-- default offset if user specified trivial range
  $offset = $vars->{offset} = 0 if (!$offset);
  $ts->cachedebug("computed sliceby=$vars->{sliceby} ; offset=$vars->{offset} ; unit=$vars->{unit}\n");

  ##-- default xrmin,xrmax from corresponding slice values if user specified trivial limits
  $xrmin=$ymd2slice->($date2ymd->($ymin)) if ($xumin eq '*');
  $xrmax=$ymd2slice->($date2ymd->($ymax)) if ($xumax eq '*');
  $ts->cachedebug("computed xrmin=$xrmin ; xrmax=$xrmax\n");
  @$vars{qw(xrmin xrmax xumin xumax)} = ($xrmin,$xrmax, $xumin,$xumax);

  ##-- get list of all *dates* in range (at $unit-resolution)
  my @alldates = qw();
  if ($unit eq 'y') {
    @alldates = ($ymin..$ymax);
  } else {
    for (my $date=$ymin; $date le $ymax; $date=$sliceadd->($date,1)) {
      push(@alldates,$date);
    }
  }
  $vars->{alldates} = \@alldates;

  ##-- get scaling constants (from cache)
  my @classes = (@{$ts->{genres}} && $ts->{textClassKey} ? (grep {($_//'') ne ''} map {split(/[,\t]+/,$_)} @{$ts->{genres}}) : $ts->{textClassU});
  my %dc2f = qw();
  my %c2f = qw();
  my %d2f = qw();
  my $f_corpus = 0;
  my $cache = $ts->ensureCache();
  my ($class,$date,$slice,$val);
  foreach $class (@classes) {
    foreach $date (@alldates) {
      $slice = $sliceof->($date);
      next if (!($val = $cache->{"$date\t$class"}));
      next if ($datecmp->($date,$xrmin)<0 || $datecmp->($date,$xrmax)>0); ##-- ensure date restrictions are applied (redundant if iterating ymin..ymax?)
      $dc2f{ "$slice\t$class" } += $val;
      $d2f{ $slice } += $val;
      $c2f{ $class } += $val;
      $f_corpus += $val;
    }
  }
  @$vars{qw(classes dc2f c2f d2f f_corpus)} = (\@classes, \%dc2f, \%c2f, \%d2f, $f_corpus);

  ##-- get list of all slices
  my (@slices);
  if ($sliceby==0) {
    ##-- no slices
    @slices = (0);
  } elsif ($vars->{gaps}) {
    ##-- gaps allowed: instantiated slices only
    @slices = sort {$datecmp->($a,$b)} keys(%d2f);
  } elsif ($unit ne 'y') {
    ##-- fine-grained slices: extract from @alldates
    my %slices = qw();
    $slices{$sliceof->($_)} = undef foreach (@alldates);
    @slices = sort {$datecmp->($a,$b)} keys %slices;
  } else { #($unit eq 'y')
    ##-- year-slices, no gaps, but may contain offset
    @slices = map {$_*$sliceby+$offset} (int(($xrmin-$offset)/$sliceby)..int(($xrmax-$offset)/$sliceby)); ##-- for "gaps=0" (mantis bug #7562)
  }
  $vars->{slices} = \@slices;

  return $ts;
}

##----------------------------------------------------------------------
## \%rawCounts = $ts->plotFetchCounts()
##  + fetches raw plot counts into $ts->{vars}{counts} = { "${year}\t${class}" => $freq, ... }
##  + requires $ts->plotInitialize()
sub plotFetchCounts {
  my $ts = shift;
  $ts->cachedebug("plotFetchCounts()\n");

  ##-- variables
  my $vars = $ts->{vars};
  my ($query,$sliceof,$xrmin,$xrmax,$c2f,$d2f,$datecmp) = @$vars{qw(query sliceof xrmin xrmax c2f d2f datecmp)};

  ##-- real guts: acquire & scan ddc histogram data
  my %counts = qw();
  my $f_query   = 0;
  if ($vars->{totals}) {
    ##-- plot totals
    %counts = %{$ts->{dc2f}};
    $f_query += $_ foreach (values %counts);
  } else {
    ##-- plot nontrivial counts
    my $data = $ts->genericCounts($query); #$ts->ddcCounts($query); #$ts->ddcCounts(autoExpandQuery($query));
    my $minslice = $sliceof->($xrmin);
    my ($val,$kdate,$kclass,$kslice);
    foreach (@{$data->{counts_}}) {
      ($val,$kdate,$kclass) = map {$_//''} @$_[0..2];
      $kclass =~ s/:.*//;                             ##-- class-name trimming here should be redundant, we include it for extra paranoia
      next if (!exists($c2f->{$kclass}));             ##-- ignore "unknown" classes in count response
      next if (					      ##-- apply date restrictions if requested
	       $datecmp->($kdate,$xrmin) < 0
	       ||
	       $datecmp->($kdate,$xrmax) > 0
	      );
      $kslice = $sliceof->($kdate);
      next if ($datecmp->($kslice,$minslice) < 0
	       || !exists($d2f->{$kslice})
	       || !exists($c2f->{$kclass})
	      );
      $counts{"$kslice\t$kclass"} += $val;
      $f_query += $val;
    }
  }

  return $vars->{counts} = \%counts;
}

##----------------------------------------------------------------------
## \%rawCounts = $ts->plotFill()
##  + gets plot grand-averages if applicable
##  + fills plot gaps if requested
##  + adjusts @{$vars->{classes}}, sets %{$vars->{classesh}}
sub plotFill {
  my $ts = shift;
  $ts->cachedebug("plotFill()\n");

  my $vars = $ts->{vars};
  my $UCLASS = $ts->{textClassU};
  my ($counts,$dc2f,$c2f) = @$vars{qw(counts dc2f c2f)};

  ##-- guts: grand-average
  if ($vars->{grand} && @{$ts->{genres}} && $ts->{textClassKey}) {
    my ($key);
    foreach (keys %$counts) {
      ($key=$_) =~ s/(?<=\t).*$/$UCLASS/;
      $counts->{$key} += $counts->{$_} if ($_ ne $key); ##-- don't double counts
    }
    foreach (keys %$dc2f) {
      ($key=$_) =~ s/(?<=\t).*$/$UCLASS/;
      $dc2f->{$key} += $dc2f->{$_} if ($_ ne $key); ##-- don't double counts
    }
    foreach (keys %$c2f) {
      $c2f->{$UCLASS} += $c2f->{$_} if ($_ ne $UCLASS); ##-- don't double counts
    }
    push(@{$vars->{classes}},$UCLASS) if (!grep {$_ eq $UCLASS} @{$vars->{classes}});
  }

  ##-- guts: fill gaps
  if (!$vars->{gaps}) {
    my ($class,$date);
    foreach $class (@{$vars->{classes}}) {
      foreach $date (@{$vars->{slices}}) {
	$counts->{"$date\t$class"} += 0;
      }
    }
  }

  ##-- adjust @classes according to user request
  @{$vars->{classes}} = ($UCLASS) if ($vars->{single});
  $vars->{classesh} = { map {($_=>undef)} @{$vars->{classes}} };

  return $counts;
}

##----------------------------------------------------------------------
## \%normCounts = $ts->plotNormalize()
##  + normalizes counts in $ts->{vars}{counts}
##  + saves pre-normalization counts in $ts->{vars}{countsRaw}
sub plotNormalize {
  my $ts = shift;
  $ts->cachedebug("plotNormalize()\n");

  my $vars = $ts->{vars};
  my ($counts,$norm,$dc2f,$d2f,$c2f,$f_corpus) = @$vars{qw(counts norm dc2f d2f c2f f_corpus)};
  my ($logproj,$gaps) = @$vars{qw(logproj gaps)};

  ##-- normalize
  $vars->{countsRaw} = { %$counts };
  my $normsub = undef; ##-- $inverse_scaling_factor = $normsub->($key, $kdate, $kclass)
  if ($norm =~ /^(?:abs|none)?$/) {
    $norm    = 'abs';
  } elsif ($norm =~ /^(?:date[\+\-\,\s]?class|dc)$/ && !$vars->{totals}) {
    $norm    = 'date+class';
    $normsub = sub { $dc2f->{"$_[1]\t$_[2]"} };
  } elsif ($norm =~ /^(?:date[\+\-\,]?class|dc)$/ &&  $vars->{totals}) {
    $norm    = 'abs';
  } elsif ($norm =~ /^(?:d|date)$/) {
    $norm    = 'date';
    $normsub = sub { $d2f->{$_[1]} };
  } elsif ($norm =~ /^(?:textclass|tc|class|c|genre|g)$/) {
    $norm    = 'class';
    $normsub = sub { $c2f->{$_[2]} };
  } elsif ($norm =~ /^(?:corpus|C|full|N)$/) {
    $norm    = 'corpus';
    $normsub = sub { $f_corpus };
  } else {
    warn(__PACKAGE__, "::plotNormalize(): unknown normalization mode '$norm' - using absolute frequency");
    $norm = 'abs';
  }
  $vars->{norm} = $norm; ##-- track normalization mode used

  if (defined($normsub)) {
    my ($num,$denom,$kdate,$kclass,$val);
    foreach (keys %$counts) {
      ($kdate,$kclass) = split(/\t/,$_,2);
      $denom = $normsub ? ($normsub->($_, $kdate, $kclass) // 1) : 1;
      $num   = $counts->{$_};
      $val   = ($logproj
		? exp(log(1e6)*log($num+0.5)/log($denom+0.5))
		: ($denom==0 ? ($gaps ? "1/0" : 0) : (1e6*$num/$denom)));
      $counts->{$_} = $val;
    }
  }

  return $counts;
}

##----------------------------------------------------------------------
## \%prunedCounts = $ts->plotPrune()
##  + prunes counts in $vars->{counts} if requested
sub plotPrune {
  my $ts = shift;
  $ts->cachedebug("plotPrune()\n");

  my $vars = $ts->{vars};
  if ($vars->{prune} > 0) {
    $ts->cachedebug("plotPrune(): detected non-trivial pruning parameter prune=$vars->{prune}\n");
    require DDC::Dstar::TimeSeries::Outliers;
    die(__PACKAGE__, "::plotPrune(): could not load package DDC::Dstar::TimeSeries::Outliers: $@") if ($@);
    DDC::Dstar::TimeSeries::Outliers::prune_outliers($vars->{counts},
						     conf=>(1-$vars->{prune}),
						     classes=>$vars->{classes},
						     dates=>$vars->{slices},
						     sep=>"\t");
  }
  return $vars->{counts};
}

##----------------------------------------------------------------------
## \%smoothedCounts = $ts->plotSmooth()
##  + applies moving-average smoothing to $ts->{counts}
sub plotSmooth {
  my $ts = shift;
  $ts->cachedebug("plotSmooth()\n");

  my $vars = $ts->{vars};
  my ($counts,$window,$sliceby,$sliceadd,$datecmp,$wbase,$logavg,$xrmin,$xrmax)
    = @$vars{qw(counts window sliceby sliceadd datecmp wbase logavg xrmin xrmax)};

  ##-- apply moving-average smoothing window
  my $wcounts = $counts;
  if ($window > 0 && $sliceby != 0) {
    $wcounts = $vars->{counts} = {};
    my ($kdate,$kclass,$val,$di,$wtotal,$ddate,$dval,$wval);
    $wbase = exp(1) if (lc($wbase) eq 'e');
    foreach (keys %$counts) {
      ($kdate,$kclass) = split(/\t/,$_,2);
      $val = $wtotal = 0;
      for ($di=-$window; $di <= $window; ++$di) {
	$ddate = $sliceadd->($kdate,$di*$sliceby);
	next if ($datecmp->($ddate,$xrmin) < 0 || $datecmp->($ddate,$xrmax) > 0);
	$dval    = $counts->{$ddate."\t".$kclass} // 0;
	$wval    = $wbase ? ($wbase**-abs($di)) : 1;
	$val    += $wval * ($logavg ? log($dval+0.5) : $dval);
	$wtotal += $wval;
      }
      $val /= $wtotal if ($wtotal);
      $val  = exp($val)-0.5 if ($logavg);
      $wcounts->{$_} = $val;
    }
  }

  return $wcounts;
}

##----------------------------------------------------------------------
## \@countRows = $ts->plotCollect()
##  + collects count data from $vars->{counts} into $vars->{rows}
sub plotCollect {
  my $ts = shift;
  $ts->cachedebug("plotCollect()\n");

  my $vars = $ts->{vars};
  my ($classesh,$counts,$countsRaw,$datecmp) = @$vars{qw(classesh counts countsRaw datecmp)};
  #my ($xrmin,$xrmax) = @$vars{qw(xrmin xrmax)};

  ##-- collect data points
  my ($date,$class);
  my @rows = (
	      sort {
		$datecmp->($a->{date},$b->{date}) || ($a->{class}//'') cmp ($b->{class}//'')
	      }
	      grep { exists $classesh->{$_->{class}//''}
		       #-- 2017-02-15: apply xrmin,xrmax at DATE-level, not SLICE-level
		       #&& $_->{date} >= $xrmin && $_->{date} <= $xrmax
		   }
	      map {
		($date,$class) = split(/\t/,$_,2);
		{date=>$date, class=>$class, raw=>$countsRaw->{$_}, val=>$counts->{$_}}
	      }
	      keys %$counts
	     );

  return $vars->{rows} = \@rows;
}

##----------------------------------------------------------------------
## $plotContent = $ts->plotContent()
sub plotContent {
  my $ts = shift;
  $ts->cachedebug("plotContent()\n");

  my $vars = $ts->{vars};
  my ($pfmt,$rows,$bare,$classes) = @$vars{qw(pfmt rows bare classes)};
  my $unit = $ts->unit;

  ##-- output
  my ($content);
  if ($pfmt->{label} eq 'null') {
    $content = '';
  }
  elsif ($pfmt->{label} eq 'json') {
    $content = to_json($rows, {pretty=>$vars->{pretty}, utf8=>0});
  }
  elsif ($pfmt->{label} eq 'text') {
    $content .= join("\t", @$_{qw(val date class)})."\n" foreach (@$rows);
  }
  else {
    ##-- gnuplot: sanity check
    die(__PACKAGE__, "::plotContent(): no data for query=\`$vars->{query}'") if (!@$rows);

    ##-- gnuplot: check whether we can use gnuplot 5.0 features
    my $gpv5  = $ts->gpVersion() >= version->parse('5.0');

    ##-- gnuplot: ensure at least 2 points for @rows
    if (@$rows==1) {
      my $row = $rows->[0];
      @$rows = ({%$row,date=>$vars->{xrmin}},{%$row,date=>$vars->{xrmax}});
    }

    ##-- gnuplot: options string
    my $optstr = (""
		  .($vars->{slice} ne '' ? "slice=$vars->{slice}" : '')
		  .($vars->{smooth} ? " smooth=$vars->{smooth}" : '')
		  .(" norm=$vars->{norm}")
		  .($vars->{lognorm} ? " lognorm" : "")
		  .(($vars->{window}||0) ? " win=$vars->{window}" : '')
		  .(($vars->{wbase}||0) ? " wb=$vars->{wbase}" : '')
		 );

    ##-- gnuplot: title
    my ($set_title);
    if (!$vars->{title}) {
      $set_title = ('set title "'
		    .($vars->{totals} ? 'TOTALS' : ('\\"'.quotemeta($vars->{query}).'\\"'))
		    .(" [$optstr]")
		    .'"'
		   );
    } elsif ($vars->{title} eq 'none') {
      $set_title = '';
    } else {
      $set_title = "set title \"".quotemeta($vars->{title})."\"";
    }

    ##-- gnuplot: other options
    my $xlabel = $vars->{xlabel};
    my $ylabel = $vars->{ylabel} || ("frequency ".($vars->{norm} eq 'abs' ? "(absolute)" : "(per million)"));
    my $set_logscale = $vars->{logscale} ? "set logscale y ".($vars->{logscale}>1 ? $vars->{logscale} : "2") : '';
    $vars->{key} = 'off' if ($vars->{key} eq 'none');
    my $set_key  = $vars->{key} ? "set key $vars->{key}" : '';
    my $yrange = $vars->{yrange};
    $yrange =~ s/^\s*0\s*:/1:/ if ($vars->{logscale});
    my $set_grid = ($vars->{grid} ? ($vars->{grid} =~ /^(?:on|1|yes|y)$/ ? "set grid" : "set grid $vars->{grid}") : 'unset grid');

    ##-- gnuplot: bare: tics
    my ($set_xtics,$set_ytics);
    my ($xticmax, $yticmax) = (2000,1000);
    if ($bare) {
      #die(__PACKAGE__, "::plotContent(): can't handle 'bare'-plot for slice-unit '$unit'") if ($unit ne 'y');
      my ($xmin,$xmax) = ($vars->{xrange} =~ /(.*):(.*)/ ? ($1,$2) : ('*','*'));
      my ($ymin,$ymax) = ($vars->{yrange} =~ /(.*):(.*)/ ? ($1,$2) : ('*','*'));
      my $xcmp = $unit eq 'y' ? undef : \&dateCmp;
      $xmin    = $ts->rowmin('date',$rows,$xcmp) if ($xmin eq '*');
      $xmax    = $ts->rowmax('date',$rows,$xcmp) if ($xmax eq '*');
      $xmax    = $vars->{sliceadd}->($xmax, $vars->{sliceby});
      $ymax    = $ts->rowmax('val', $rows) if ($ymax eq '*');
      my $ticopts = "nomirror textcolor rgb \"#979797\"";

      ##-- integer x limits ($unit-granularity, for easier arithmetic)
      my ($date2int,$int2date) = @$vars{qw(date2int int2date)};
      my $xmini = $date2int->($xmin);
      my $xmaxi = $date2int->($xmax)-1;

      $set_xtics = 'unset xtics;';
      my @xtmax = ($unit eq 'd' ? qw(365 183 92 56 28 14 7 1)
		   : ($unit eq 'm' ? qw(480 240 120 72 48 24 12 6 4 3 2 1)
		      : qw(100 50 25 10 5 2 1) ##-- unit=y
		      ));
      my $xtrange = ($xmaxi-$xmini);
      my $nxtics  = 2;
      my (@xtics,@xticx);
      foreach (@xtmax) {
	if ($xtrange/$_ >= $nxtics) {
	  my $xtic    = $_;
	  my $xticmin = $xtic * int($xmini/$xtic);
	  $xticmax    = $xtic * int($xmaxi/$xtic + 0.5);
	  @xtics      = map { $xticmin + $_*$xtic } (0..int(($xticmax-$xticmin)/$xtic));
	  @xticx      = ($unit eq 'y'
			 ##-- cunning & devious fractional tic placement only for unit=y
			 ? (map { $xtics[$_]-$_/$#xtics*$vars->{sliceby} } (0..$#xtics))
			 : @xtics);
	  $set_xtics  = ("set xtics ("
			 .join(',',
			       map {
				 ('"'.$int2date->($xtics[$_]).'" '
				  .($unit ne 'y' ? '"' : '')
				  .$int2date->($xticx[$_])
				  .($unit ne 'y' ? '"' : '')
				 )
			       } (0..$#xtics))
			 .") $ticopts;"
			);
	  #print STDERR "xmin=$xmini ($xmin), xmax=$xmaxi ($xmax), xtic=$xtic, xticmin=$xticmin, xticmax=$xticmax\n";##-- DEBUG
	  last;
	}
      }

      $set_ytics = 'unset ytics;';
      my @ytmax = reverse map {($_/4,$_/2,$_)} map {10**$_} (-6..6);
      my $nytics = 2;
      foreach (@ytmax) {
	if ($ymax/$_ >= $nytics) {
	  my $ytic   = $_;
	  $set_ytics = qq(set ytics $ytic $ticopts;);
	  $yticmax   = $ytic * int($ymax/$ytic + 1);
	  #print STDERR "ymin=$ymin, ymax=$ymax, ytic=$ytic, yticmax=$yticmax\n";##-- DEBUG
	  last;
	}
      }

      ##-- bare plot: term options
      if ($vars->{pformat} eq 'svg') {
	#$vars->{termopts} .= " font \"Arial,8\"";
	#$vars->{termopts} .= " font \"Times Italic,8\"";
	#$vars->{termopts} .= " font \"Crimson Text Italic,8\""; ##-- works in inkscape with font installed, but not in web view
	$vars->{termopts} .= " font \"Serif Italic,8\""   ##-- workaround, gnuplot 4.x
      }

      ##-- bare plot: vertical bars (xBarClass pseudo-plot)
      unshift(@$classes, $ts->{xBarClass});
      $ymin = 0 if (($ymin//'') =~ /^\*?$/);
      push(@$rows, map { {class=>$ts->{xBarClass},date=>$int2date->($_),val=>$ymax} } @xticx);
    }

    ##-- gnuplot: set output
    my $set_output='';
    my $termopts = $vars->{termopts} // '';
    $termopts   .= " dashed" if ($bare && !$gpv5); ##-- termoption "dashed" only works in gnuplot < 5.0
    $termopts   .= " noenhanced" if ($gpv5 && $termopts !~ /\benhanced\b/); ##-- disable "enhanced text mode" (gnuplot >= 5.0)
    if ($ts->{debug} || $pfmt->{label} ne 'gnuplot') {
      my ($tmpfh,$tmpfile) = File::Temp::tempfile("dhist_XXXXX", DIR=>"/tmp", SUFFIX=>".$pfmt->{label}");
      close $tmpfh;
      $vars->{tmpfile} = $tmpfile;
      my $term = $pfmt->{term}//$vars->{pformat};
      $term    = 'svg' if ($pfmt->{label} eq 'gnuplot');
      $term    =~ s/\$SIZE/$vars->{size}/g;
      $set_output = "set term $term $termopts background \"white\"; set output \"$tmpfile\"";
      $ts->cachedebug("using tempfile $tmpfile\n") if ($ts->{debug});
    }

    ##-- gnuplot: x-axis format (especially for fine-slices)
    my ($set_xrange, $set_timefmt);
    if ($unit eq 'y') {
      $set_xrange = qq(set xrange [$vars->{xrange}]);
      $set_timefmt = '';
    } else {
      $set_xrange = ('set xrange ['
		     .($vars->{xumin} eq '*' ? '*' : ('"'.$vars->{sliceof}->($vars->{xrmin}).'"'))
		     .':'
		     .($vars->{xumax} eq '*' ? '*' : ('"'.$vars->{sliceof}->($vars->{xrmax}).'"'))
		     .']'
		    );
      my $timefmt = $unit eq 'm' ? '%Y-%m' : '%Y-%m-%d';
      $set_timefmt = join('',
			  map {"$_\n"}
			  qq(set xdata time;),
			  qq(set timefmt "$timefmt";),
			  qq(set format x "$timefmt";),
			  qq(set xtics rotate by 60 right;),
			 );
    }

    ##-- gnuplot: script
    my %pcmds = map {
      ($_ => ("\"-\""
	      .($unit ne 'y' ? ' using 1:2' : '')
	      .($vars->{smooth} ? " smooth $vars->{smooth}" : '')
	      ." title \"$_\""))
    } @$classes;
    $pcmds{$ts->{textClassU}} = "$pcmds{$ts->{textClassU}} w l ".($bare ? "lt 1 lw 3.5 lc rgb \"#0087c2\"" : "lt 7 lw 5") if ($vars->{grand});
    $pcmds{$ts->{xBarClass}}  = "\"-\" using 1:2 w i lt 1 lw 0.75 lc rgb \"#979797\" notitle" if (exists $pcmds{$ts->{xBarClass}});
    my $gp = join('',
		  map {"$_\n"}
		  qq(set style data $vars->{style};),
		  $set_timefmt,
		  ($xlabel && $xlabel ne 'none' ? qq(set xlabel "$xlabel";) : qq(unset xlabel;)),
		  ($ylabel && $ylabel ne 'none' ? qq(set ylabel "$ylabel";) : qq(unset ylabel;)),
		  $set_xrange,
		  qq(set yrange [$yrange];),
		  "$set_grid;",
		  "$set_logscale;",
		  "$set_title;",
		  "$set_key;",
		  "$set_output;",
		  ($bare
		   ? (
		      'set key off;',
		      'set border lw 1 lt 1 lc rgb "#979797";', ##-- also sets tic color
		      $set_xtics,
		      $set_ytics,
		      (map {"set $_ scale 0;"} qw(xtics ytics)),
		      'unset border;',
		      "set autoscale fix;",
		      #
		      #'set grid xtics ytics lc rgb "#979797" lw .75 lt 0;',
		      #'set grid xtics ytics lc rgb "#979797" lw .75 lt 1;',
		      'set grid noxtics ytics lc rgb "#979797" '.($gpv5 ? 'lw 1 lt 1 dt (2,8)' : 'lw .75 lt 3').';',
		      #
		      #"set autoscale y;",
		      #"set lmargin ".(1+length($yticmax)/1.25).";",
		      #"set bmargin 1;",
		      #"set rmargin ".(length($xticmax)/2).";",
		      #"set tmargin 1.0;",
		      (map {"unset $_;"} qw(xlabel ylabel title)), #ylabel border tics
		      #(map {"set ${_}margin 0;"} qw(l b r t)),
		      ##
		      #'set title "Frequenz / Mio Tokens" textcolor rgb "#979797";',
		      ##
		      #'set title "Frequenz / Mio Tokens" textcolor rgb "#979797";',
		      #'set tmargin 1.6;',
		      ##
		      'set title "\n";',
		      'set label 1 "Frequenz / Mio Tokens" at graph -.09,1.075 left textcolor rgb "#979797";',
		      'set tmargin 1.6;',
		     )
		   : qw()),
		  ("plot ".join(', ', @pcmds{@$classes}).";"),
		 );

    ##-- gnuplot: data
    my ($class);
    foreach $class (@$classes) {
      my @crows = grep {$_->{class} eq $class} @$rows;
      my $vmin  = $vars->{smooth} =~ /spline/ ? $ts->rowmin('val', [grep {$_->{val} > 0} @crows]) : 0;
      foreach (@crows) {
	$gp .= join("\t",$_->{date}, ($_->{val} <= 0 ? -$vmin : $_->{val}))."\n";
      }
      $gp .= "e\n";
    }

    ##-- raw gnuplot
    if ($vars->{pformat} =~ /^(?:gnuplot|gp)$/) {
      $content = $gp;
    } else {
      open(GNUPLOT,"|-:utf8", "gnuplot 2>/dev/null")
	or die(__PACKAGE__, "::plotContent(): open failed for pipe to gnuplot: $!");
      print GNUPLOT $gp;
      #print STDERR $gp; ##-- DEBUG
      close(GNUPLOT)
	or die(__PACKAGE__, "::plotContent(): close failed for pipe to gnuplot: $!");

      ##-- slurp temp file
      open(TMP,"<$vars->{tmpfile}")
	or die(__PACKAGE__, "::plotContent(): failed to open temp file '$vars->{tmpfile}' for reading: $!");
      local $/=undef;
      $content = <TMP>;
      close TMP;

      ##-- post-processing hacks for 'bare' svg plots under gnuplot v5
      if ($bare && $gpv5 && $pfmt->{label} eq 'svg') {
	$content =~ s{\b(font-size)=['"]8(?:\.0*)?[^'"]*['"]}{$1="11"}g;
      }
    }
  }

  return $content;
}
