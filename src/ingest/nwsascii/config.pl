#!/usr/local/bin/perl -n
#
# Take NWS ASCII report lines on input and print out subplatform config line
# containing name and location info.  The output of this should be passed
# through sort and uniq
#

$[ = 1;

$name = substr($_,1,5);
$name =~ tr/A-Z/a-z/;
$lat = substr($_,47,2);
$lat += (substr($_,50,2) / 60.0);
$lon = substr($_,54,3);
$lon += (substr($_,58,2) / 60.0);
$lon = 0 - $lon;
$alt = substr($_,40,4) / 1000.0;
printf "subplatform %s\t%6.2f\t%6.2f\t%6.3f\n",$name,$lat,$lon,$alt;

