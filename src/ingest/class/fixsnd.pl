#!/usr/local/bin/perl
#
# Convert the OFPS "class" format into something more along the lines of
# what we think of as the class format.
#

#
# Output column widths.  I sure as hell hope these are constant across
# the files.  All but the first are incrmented to create the space between
# columns.
#
@widths = (6, 7, 6, 6, 6, 7, 7, 6, 6, 6, 9, 8, 6, 6, 8, 5, 5, 5, 5, 5, 5);
#
# Plow through each file on the command line.
#
foreach $file (@ARGV) {
#
# Open files
#
    open (in, $file) || die "Can't open in file";
    open (out, ">" . $file . ".fixed");
#
# Plow past the header information.
#
    while (<in>) {
	print (out $_);
	/---/ && last;
    }
#
# Now we have to mangle things.
#
    while (<in>) {
	split (" ");
#	printf ("0 is .%s.\n", $_[0]);
#	$_[0] = "" && shift(@_);
	if ($_[5] == "9999.0") {
	    $_[5] = "999.0";
	}
	if ($_[6] == "9999.0") {
	    $_[6] = "999.0";
	}
	if ($_[10] == "9999.000") {
	    $_[10] = "999.000";
	}
	for ($i = 0; $i <= $#widths; $i++) {
	    $fmt = "%" . $widths[$i] . "s";
	    printf out sprintf ($fmt,$_[$i]);
	}
	printf out "\n";
    }
}
