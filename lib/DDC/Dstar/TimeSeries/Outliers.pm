##-*- Mode: CPerl -*-

package DDC::Dstar::TimeSeries::Outliers;

use PDL;
use PDL::Stats::Basic;
use PDL::Stats::TS;
use strict;

## filter_exp2($p)
BEGIN { *PDL::filter_exp2 = \&filter_exp2; }
sub filter_exp2 {
  my ($inpdl,$q) = @_;
  my $f = $inpdl->filter_exp($q);
  $f   += $inpdl->slice("-1:0")->filter_exp($q)->slice("-1:0");
  $f   /= 2;
  return $f;
}

## prune_outliers(\%dc2f, %opts)
##  + removes outlier values from \%ldata
##  + if an outlier is detected, its {val} key becomes {val_outlier} and a new {val} key is defined
##  + known %opts:
##     sep => $sepstr,
##     classes => \@classes,
##     dates   => \@date_slices,
##     conf
##     expq
##     eps
sub prune_outliers {
  my ($dc2f, %opts) = @_;
  my $sep     = ($opts{sep}     // "\t");
  my $classes = ($opts{classes} //= []);
  my $dates   = ($opts{dates}   //= []);
  my $expq    = ($opts{expq}    //= 0.25);
  my $conf    = ($opts{conf}    //= 0.95);
  my $eps     = ($opts{eps}     //= 0);

  ##-- sanity check(s)
  die(__PACKAGE__, "::prune_outliers(): 'conf' parameter must be in range [0:1]") if ($conf<0 || $conf>1);

  ##-- setup enums
  my $nc = @$classes;
  my $nd = @$dates;

  ##-- setup data pdls
  foreach my $class (@$classes) {
    my $raw = pdl(double, [map {$dc2f->{"${_}${sep}${class}"}//0} @$dates]);

    my $expected = filter_exp2($raw,$expq);
    my $errs_abs = $raw - $expected;
    my $errs_rel = $errs_abs / ($expected+$eps);
    my $er_mu    = $errs_rel->daverage;
    my $er_sigma = $errs_rel->stdv;
    my $er_delta = erfi($conf) * sqrt(2) * $er_sigma;
    my $ex_hi    = ($expected + ($er_mu+$er_delta)*$expected);
    my $ex_lo    = ($expected + ($er_mu-$er_delta)*$expected)->lclip(0);
    my ($good,$bad) = which_both( ($raw <= $ex_hi) & ($raw >= $ex_lo) );
    my $fixed    = $raw->pdl;
    (my $tmp=$fixed->index($bad)) .= $bad->interpol(pdl(-1)->append($good)->append(pdl($raw->nelem)),
						    #$expected->slice("(0)")->append($expected->index($good))->append($expected->slice("(-1)"))
						    $raw->slice("(0)")->append($raw->index($good))->append($raw->slice("(-1)"))
						   ) if (!$good->isempty && !$bad->isempty);

    ##-- propagate fixes back to \%dc2f
    $dc2f->{"$dates->[$_]${sep}${class}"} = $fixed->at($_) foreach ($bad->list);
  }

  return $dc2f;
}


1; ##-- be happy
