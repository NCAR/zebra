#!/usr/bin/env python

""" ac_convert - convert convair data system aircraft tracks into Zebra format.
	which is ASCII lines of the form:
	<platform> <lat> <lon> alt_km unix_time_seconds

Convair flight track files have the form:
-------------------------------------------cut-here---------------
Flt 1846 - 2001-01-04
 Time           tans-lat        tans-lon        tans-alt
 UTC             deg             deg             meters 
 19:20:00          47.77         -122.39          1036.7
 19:20:01          47.77         -122.39          1036.7
 19:20:02          47.77         -122.39          1036.7

--------------------
Assume that if the first hour of the flight is not '0', that if later
see an hour of 0 that we've crossed into a new day.

Assume that at most one new day occurs per flight, given the limitations
of airplanes (and crews!) :-)


"""
import sys,os,string,time, Julian

# is data the bad value
def BAD_VALUE(value):
    diff = abs(value - -999)
    if diff < .01:
	return 1
    else:
	return 0

def main():
    PLATFORM = 'convair'
    args = sys.argv[1:]
    usageStr = 'filename'
    if len(args) != 1:
        print 'usage: %s %s' % (sys.argv[0], usageStr)
	sys.exit(-1)

    f = open(args[0], 'r')
    lines = f.readlines()
    f.close()
    first = string.split(lines[0])
    dateStr = first[3]

    year = int(dateStr[0:4])
    month = int(dateStr[5:7])
    day = int(dateStr[8:10])

    firstTime = 1
    newDay = 0
    
    for l in lines[3:] :
        s = string.split(l)
# insert any units conversions here to generate lines of the form
# <platform> lat lon alt_km Unix_time_seconds
	lat = float(s[1])
	lon = float(s[2])
	meters = float(s[3])
	alt_km = meters/1000.0    # convert to Km
	if BAD_VALUE(lat) or BAD_VALUE(lon) or BAD_VALUE(meters):
	    continue

	hour = int(s[0][0:2])
	minute = int(s[0][3:5])
	second = int(s[0][6:8])
        if firstTime:
	    firstTime = 0
            firstHour = hour
	   
        if firstHour != 0 and hour == 0:
	    newDay = 1
	 
        j = Julian.Julian(year,month,day,hour,minute,second, 'GMT')
        unixTime =   j.time
        if newDay:
            # flight track time has rolled over to a new day.
	    unixTime = unixTime + Julian.SECSADAY

	print '%s %s %s %s %d' % (PLATFORM, lat, lon, alt_km, unixTime)


if __name__ == '__main__': main()
