#! /bin/env perl

sub ztsys
{
    # print `ztsplit $_[0]`, "\n";
    ($year, $mon, $day, $hour, $min, $sec) = split(/\s+/,`ztsplit $_[0]`);
    timegm($sec,$min,$hour,$day,$mon-1,$year);
}

sub syszt
{
    ($sec,$min,$hour,$mday,$mon,$year) = gmtime($_[0]);
    strftime ("%d-%h-%Y,%H:%M:%S", $sec,$min,$hour,$mday,$mon,$year);
}

# 7-dec-1997,12:00:00
sub ztsplit
{
    %Months = ("jan", 1,
	       "feb", 2,
	       "mar", 3,
	       "apr", 4,
	       "may", 5,
	       "jun", 6,
	       "jul", 7,
	       "aug", 8,
	       "sep", 9,
	       "oct", 10,
	       "nov", 11,
	       "dec", 12);

    $sec = 0;
    ($day, $mon, $year, $hour, $min) =
	$_[0] =~ /(\d+)-(\w+)-(\d+),(\d+):(\d+)/;

    $month = `echo $mon | tr '[:upper:]' '[:lower:]'`;
    chop $month;
    #print "$month\n";
    #printf "%d %02d %02d %02d %02d %02d\n",
    #$year, $Months{$month}, $day, $hour, $min, $sec;

    return ($year, $Months{$month}, $day, $hour, $min, $sec);
}

1;
