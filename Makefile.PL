use ExtUtils::MakeMaker;

##----------------------------------------------------------------------
## MAIN
WriteMakefile
  (
   NAME		=> 'DDC::Dstar::TimeSeries',
   AUTHOR       => 'Bryan Jurish <moocow@cpan.org>',
   ABSTRACT	=> 'modular time series plots for DDC/D*',
   VERSION_FROM	=> 'lib/DDC/Dstar/TimeSeries.pm',

   PREREQ_PM    => {
		    'DDC::Concordance'=>0,	#v0.40
		    'Encode'=>0,		#v2.80_01
		    'File::Basename'=>0,	#v2.85
		    'JSON'=>0,			#v2.90
		    'Fcntl'=>0,			#v1.13
		    'POSIX'=>0,			#v1.65
		    'File::Temp'=>0,		#v0.2304
		    'version'=>0,		#v0.9917

		    ##-- for outlier pruning
		    'PDL' => 0,			#v2.017
		    'PDL::Stats'=>0,		#v0.70

		    ##-- for testing
		    'Test::More'=>0,		#v1.302075
		   },

   EXE_FILES    => ['dstar-ts-plot.perl'],
  );

