#
# General scalar ingest support.
#

import nc
import string
from time import *

BadValue = -99999.0

#
# Set up the destination netcdf file.
#
# BEGIN_TIME	is the initial time of data in the file.
# DESTDIR	is the directory in which the output file should be made
# PLATFORM	is the name of the platform that generated this data
# FIELDS	is a dictionary, indexed by each of the fields we will be
#		ingesting.  "lat", "lon", and "alt" need to be there if the
#		result is to be proper.  The value of each field in the
#		dictionary is a tuple (long name, units).  Additional fields
#		in the tuple are ignored here, though DecodeVars will use
#		tuple[2] and [3].
#
def Init (begin_time, destdir, platform, fields):
    global BadValue
    #
    # Format up the file name, and create it.
    #
    fname = destdir + "/" + platform + \
	    strftime (".%Y%m%d.%H%M.cdf", gmtime (begin_time))
    print "File name is " + fname
    global NCFile
    NCFile = nc.create (fname, nc.CLOBBER)

    #
    # Make the time dimensions and variables.
    #
    NCFile.def_dim ('time', nc.UNLIMITED)
    NCFile.def_var ('base_time', nc.LONG, ())
    NCFile.def_att ('long_name', 'base_time', nc.CHAR, 'Base time in Epoch')
    NCFile.def_att ('units', 'base_time', nc.CHAR,
		    'seconds since 1970-1-1 0:00:00 0:00')
    NCFile.def_var ('time_offset', nc.DOUBLE, ('time',))
    NCFile.def_att ('units', 'time_offset', nc.CHAR, 'seconds')
    NCFile.def_att ('long_name', 'time_offset', nc.CHAR,
		    'Time offset from base_time')
    #
    # Now go through the supplied fields and create them.
    #
    for field in fields.keys ():
	NCFile.def_var (field, nc.FLOAT, ('time',))
	NCFile.def_att ('long_name', field, nc.CHAR, fields[field][0]);
	NCFile.def_att ('units', field, nc.CHAR, fields[field][1])
	NCFile.def_att ('missing_value', field, nc.FLOAT, BadValue)
    #
    # Done with definitions.  Store the base time.
    #
    NCFile.endef ()
    base = NCFile.var ('base_time')
    base[0] = begin_time
    global BeginTime
    BeginTime = begin_time;

#
# Store some data in the file.  Everything is a list of data values; you
# can store one at a time, or accumulate the whole thing first.
#
def Store (times, data):
    #
    # Do the time offset first, since there is a calculation involved.
    #
    var = NCFile.var ('time_offset')
    ntime = len (var)
    i = ntime
    for time in times:
	var[i] = time - BeginTime
	i = i + 1
#	var.append (time - BeginTime)
    #
    # Now we slap in each field.
    #
    for field in data.keys ():
	var = NCFile.var (field)
	var[ntime:ntime + len (data[field])] = data[field]

#
# Finish.
#
def Done ():
    global NCFile
    NCFile.close ()
    NCFile = None


#
# Utility routine to decode ascii variables out of a string.  This one
# uses the same fields array that init does, and looks for the offset
# of the data for each field as the third entry in the dictionary entry
# for the field.  The bad value flag used by this field is in [4]
#
def DecodeVars (line, fields, result):
    global BadValue
    for field in fields.keys ():
	offset = fields[field][2]
	if offset < 0:
	    continue
#	v = eval (line[offset])
	v = string.atof (line[offset])
	if v == fields[field][3]:
	    v = BadValue
	result[field].append (v)
