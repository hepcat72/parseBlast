# parseBlast.pl
Parses blast results for each high-scoring pair into a tab-delimited file.

## WHAT IS THIS:

This script parses blast result files and outputs select data summarizing each HSP (high-scoring pair) in tab-delimited format.

## INSTALLATION

    perl Makefile.PL
    make
    sudo make install

## USAGE

By example:

    parseBlast.pl -i "input file(s)" [--verbose] [--help] [--version] [-e] [--noheader] [--quiet] [--force] [--debug] [-o] [--ignore]
    parseBlast.pl [--verbose] [--help] [--version] [-e] [--noheader] [--quiet] [--force] [--debug] [-o] [--ignore] < input_file

     -i|--input-file*     REQUIRED Space-separated blast file(s inside quotes).
                                   Standard input via redirection is
                                   acceptable.  Perl glob characters (e.g. '*')
                                   are acceptable inside quotes (e.g.
                                   -i "*.txt *.text").  See --help for a
                                   description of the input file format.
                                   *No flag required.
     -e|--expect-filter   OPTIONAL [-1] Only output parsed hits if they are
                                   less than or equal to this value (where a
                                   negative value indicates "no filtering" or
                                   "output all parsed hits").  Values such as
                                   1e-10, 0.0, e-25, or 100 are acceptable.
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script will
                                   result in the output file name to be "STDIN"
                                   with your suffix appended.  See --help for a
                                   description of the output file format.
     --force|--overwrite  OPTIONAL Force overwrite of existing output files.
                                   Only used when the -o option is supplied.
     --ignore             OPTIONAL Ignore critical errors & continue
                                   processing.  (Errors will still be
                                   reported.)  See --force to not exit when
                                   existing output files are found.
     --verbose            OPTIONAL Verbose mode.  Cannot be used with the quiet
                                   flag.  Verbosity level can be increased by
                                   supplying a number (e.g. --verbose 2) or by
                                   supplying the --verbose flag multiple times.
     --quiet              OPTIONAL Quiet mode.  Suppresses warnings and errors.
                                   Cannot be used with the verbose or debug
                                   flags.
     --help|-?            OPTIONAL Help.  Print an explanation of the script
                                   and its input/output files.
     --version            OPTIONAL Print software version number.  If verbose
                                   mode is on, it also prints the template
                                   version used to standard error.
     --debug              OPTIONAL Debug mode.  Adds debug output to STDERR and
                                   prepends trace information to warning and
                                   error messages.  Cannot be used with the
                                   --quiet flag.  Debug level can be increased
                                   by supplying a number (e.g. --debug 2) or by
                                   supplying the --debug flag multiple times.
     --noheader           OPTIONAL Suppress commented header output.  Without
                                   this option, the script version, date/time,
                                   and command-line information will be printed
                                   at the top of all output files commented
                                   with '#' characters.


## INPUT

Blast result files generated using blastall with the default output format (-m 0).

## OUTPUT

Tab-delimited format with columns in this order:

    query_id, subject_id, query_length, subject_length, query_start, query_stop, subject_start, subject_stop, evalue, identity, query_defline, subject_defline