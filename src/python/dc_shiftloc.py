#!/bin/env python
#
# Update the plot bounds of a given display configuration to be centered
# on the location of a mobile platform at a selected time
#

displaysize = 400	# default to 400 km on a side

REarth = 6378.0		# Earth radius, km

import getopt
import math
import os
import string
import sys
import time

def deg_to_rad(r):
    return ((r / 180.0) * math.pi)

def usage():
    print "Usage: %s [options]" % sys.argv[0]
    print "Options:"
    print "  -h, --help             print this usage information and exit"
    print "  --dc <dc_file>         specify the template display config file"
    print "                         (required)"
    print "  --out_dc <dc_file>     specify the output display configuration"
    print "                         (default is template dc with _tmp added)"
    print "  --olat <lat>           set the origin latitude (required)"
    print "  --olon <lon>           set the origin longitude (required)"
    print "  --platform <plat>      set the platform to track (required)"
    print "  --size <size>          display size, km (default 400 km)"
    print "  --time <time>          use the location at this time, in seconds"
    print "                         since 1 Jan 1970 00:00 UTC (default now)"
    print
    print "The specified display configuration will be modified to show"
    print "an area centered on the location of a Zebra platform at a selected"
    print "time (default now).  By default, a new display configuration will"
    print "be written named like the original with a _tmp extension, e.g., if "
    print "'/path/test.dc' is the template display configuration, the new"
    print "configuration will be written in '/path/test_tmp.dc'."


try:
    haveplat = 0
    haveolat = 0
    haveolon = 0
    havedc = 0
    haveoutdc = 0
    
    try:
        optlist, rest = getopt.getopt(sys.argv[1:], "h",
                                      ["help", "dc=", "olat=", "olon=",
                                       "out_dc=", "platform=", "size=",
                                       "time="])
    except getopt.GetoptError, msg:
        print msg
        usage()
        sys.exit(1)

    plottime = long(time.time())  # default to now, may be overridden below

    if (len(rest) > 0):
        print "Unexpected options: ",
        for item in rest:
            print item,
            print
            usage()
            sys.exit(1)

    for i in range(len(optlist)):
        opt, val = optlist[i]
        if (opt == "-h" or opt == "--help"):
            usage()
            sys.exit(0)
        elif (opt == "--dc"):
            dcfullpath = val
            havedc = 1
        elif (opt == "--olat"):
            try:
                originlat = string.atof(val)
            except:
                print "Bad origin latitude: " + val
                sys.exit(1)
            haveolat = 1
        elif (opt == "--olon"):
            try:
                originlon = string.atof(val)
            except:
                print "Bad origin longitude: " + val
                sys.exit(1)
            haveolon = 1
        elif (opt == "--out_dc"):
            dest_dcfullpath = val
            haveoutdc = 1
        elif (opt == "--platform"):
            platform = val
            haveplat = 1
        elif (opt == "--size"):
            try:
                displaysize = string.atol(val)
            except:
                print "Bad display size: " + val
                sys.exit(1)
        elif (opt == "--time"):
            try:
                plottime = string.atoi(val)
            except:
                print "Bad time: " + val
                sys.exit(1)
        else:
            print "BUG: unhandled legal option " + opt
            sys.exit(1)

    #
    # make sure we have the required stuff
    #
    if (not havedc):
        print "No template display configuration specified!"
        sys.exit(1)

    if (not haveolat):
        print "No origin latitude specified!"
        sys.exit(1)

    if (not haveolon):
        print "No origin longitude specified!"
        sys.exit(1)

    if (not haveplat):
        print "No platform specified!"
        sys.exit(1)

    #
    # Make sure the ZEB_TOPDIR environment variable is set
    #
    try:
        ZEB_TOPDIR = os.environ["ZEB_TOPDIR"]
    except:
        print "ZEB_TOPDIR environment variable must be set!"
        sys.exit(1)
    
    #
    # Break apart the display configuration filename and create a destination
    # dc name (original dc name with a _tmp extension) and filename
    #
    try:
        os.stat(dcfullpath)
    except:
        print "No such file: '%s'" % dcfullpath
        sys.exit(1)

    dcdir, dcfile = os.path.split(dcfullpath)
    dc, dcext = os.path.splitext(dcfile)
    if (dcext != ".dc"):
        print "Display configuration file must end with a '.dc' extension"
        sys.exit(1)

    #
    # If we were given a destination file, verify that it ends in .dc
    # and extract the dc name from the full path name
    #
    if (haveoutdc):
        dest_dcdir, dest_dcfile = os.path.split(dest_dcfullpath)
        dest_dc, dest_dcext = os.path.splitext(dest_dcfile)
        if (dest_dcext != ".dc"):
            print "out_dc file must end with a '.dc' extension"
            sys.exit(1)
    #
    # Otherwise, build the default destination file based on the
    # template file name
    #
    else:
        dest_dc = dc + "_tmp"
        dest_dcfile = dest_dc + dcext
        dest_dcfullpath = os.path.join(dcdir, dest_dcfile)

    #
    # Put ZEB_TOPDIR/bin into our path, so the Zebra program execs below
    # will work
    #
    os.environ["PATH"] = ZEB_TOPDIR + "/bin:" + os.environ["PATH"]

    #
    # Make sure a data store is running, so we can run platloc
    #
    status = os.system("zquery -t 2 DS_Daemon >& /dev/null")
    if (status != 0):
        print "No Zebra datastore is running"
        sys.exit(1)
    
    #
    # use platloc to get platform location at the plot time
    #
    try:
        locstring = os.popen("platloc -s -t %d %s" %(plottime, platform),
                             "r").read()
    except:
        print "platloc error"
        sys.exit(1)

    lat, lon, datatime = string.split(locstring)
    lat = string.atof(lat)
    lon = string.atof(lon)
    datatime = string.atoi(datatime)
    
    #
    # find our plot bounds based on the location we got
    #
    xcenter = (math.cos(deg_to_rad(originlat)) * (lon - originlon) / 180.0 *
               math.pi * REarth)
    xmin = xcenter - 0.5 * displaysize
    xmax = xmin + displaysize

    ycenter = (lat - originlat) / 180.0 * math.pi * REarth
    ymin = ycenter - 0.5 * displaysize
    ymax = ymin + displaysize

    #
    # build the sed command that we'll use to change the plot bounds in
    # the template display configuration
    #
    sedcmd = 's/^config .*/config %s/g\n' % dest_dc
    sedcmd += 's/ y-min.*/ y-min  \'%d\'/g\n' % ymin
    sedcmd += 's/ y-max.*/ y-max  \'%d\'/g\n' % ymax
    sedcmd += 's/ x-min.*/ x-min  \'%d\'/g\n' % xmin
    sedcmd += 's/ x-max.*/ x-max  \'%d\'/g\n' % xmax
    
    #
    # execute the sed command, sending the output to the destination
    # display configuration
    #
    os.system('cat %s | sed "%s" > %s' % (dcfullpath, sedcmd, dest_dcfullpath))

except SystemExit:
    pass
