#!/usr/local/bin/perl -n
#
# Take names of NMC SA ASCII surface hourly data files on the
# command line.  Extract lines for relevant stations from the file,
# and compress the result.
#
# For now, we are distinguishing between relevant stations by state.
# Compression is done with gzip.
#
# Make sure we keep the very first line of the file, which contains
# the time of the reports.
#

if ((substr($_,29,2) =~ /KS|OK/) || ($_ =~ /^HRLY-DTA/))
{
	print;
}


__END__


for file ($*)
{
	open (IN, $file) || 
		{ print "could not open $file"; continue; };
	open (OUT, "|gzip -c > $file.uav.gz") ||
 	{ 
		print "could not open pipe to gzip"; close (IN); continue;
	};



	close (OUT);
	close (IN);
}

