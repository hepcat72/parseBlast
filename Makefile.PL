#Makefile.PL version 1.1

use 5.010001;
use ExtUtils::MakeMaker;

my @scripts = qw(parseBlast.pl);
my $srcdir  = 'src';
my $name    = $scripts[0];
$name =~ s/\..*//i;

WriteMakefile
  (
   NAME => $name,
   ($[ >= 5.005 ?  #Add these new keywords supported since 5.005
    (AUTHOR        => 'Robert W. Leach (rleach@princeton.edu)',
     ABSTRACT_FROM => "$srcdir/$scripts[0]") : ()),
   VERSION_FROM    => "$srcdir/$scripts[0]", # finds $VERSION
   PREREQ_PM       => {'Getopt::Long' => 2.38,
		       'File::Glob'   => 1.17,
		       'strict'       => 1.07,
		       'warnings'     => 1.13},
   EXE_FILES       => [ map { "$srcdir/$_" } @scripts ]
  );
