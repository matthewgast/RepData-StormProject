#!/usr/bin/perl

# This quick script converts files with double-quoted EVTYPES into a
# comma-separated list of EVTYPES for help in creation of
# data-reduction scripts.

use File::Slurp;
use Text::ParseWords;

# One argument: a filename, in the current working directory
my $filename = $ARGV[0];

# Read the file, and convert everything to uppercase
my $content = read_file($filename);
$content = uc($content);

# Convert file to lines
@contentlines = split(/\n/,$content);

# Split any lines on spaces, then rejoin with commas.  Print one EVTYPE
# per line, suitable for cut-and-paste into the R markdown doc where
# EVTYPES are consolidated
for my $line ( @contentlines ) {
	my @words = quotewords '\s+', 1, $line;
	print join(",\n",@words);
	print ",\n";
}

