#
# General scalar ingest support.
#
# $Id: IRGIngest.py,v 1.1 1997-04-17 16:10:36 corbet Exp $
#

import nc
import string
from time import *


#
# The ingestor class we pass back.
#
class IRGIngestor:
    BadValue = -99999.0

    def __init__ (self, destdir, platform, plist, latlons, fields):
	#
	# Just remember everything for now.
	#
	self.plats = plist
	self.positions = latlons
	self.fields = fields
	self.platform = platform
	self.destdir = destdir
	self.fname = None

    # 
    # Create a new file, with the given time.
    #
    def NewFile (self, when):
	#
	# Come up with the name for this file, and see if we maybe already
	# have it open.
	#
	(year, month, day, junk, junk, junk, junk, junk, junk) = gmtime (when)
	fname = "%s/%s.%02d%02d%02d.0000.cdf" % (self.destdir, self.platform,
	      year, month, day)
	if fname == self.fname:
	    return
	#
	# OK, our current file, if any, isn't going to do it.  Close it out,
	# and see if the file we want already exists.
	#
	print 'New file ' + fname
	if self.fname != None:
	    self.file.close ()
	try:
	    self.file = nc.open (fname, nc.WRITE)
	    v = self.file.var ('base_time')
	    self.beginTime = v[0]
	    self.fname = fname
	    return
	except nc.error:
	    pass
	#
	# OK, we need to create a new file.
	#
	self.file = nc.create (fname, nc.CLOBBER)
        self.fname = fname
	#
	# Make the time dimensions and variables.
	#
	self.file.def_dim ('time', nc.UNLIMITED)
	self.file.def_var ('base_time', nc.LONG, ())
	self.file.def_att ('long_name', 'base_time', nc.CHAR, 
	      'Base time in Epoch')
	self.file.def_att ('units', 'base_time', nc.CHAR,
	      'seconds since 1970-1-1 0:00:00 0:00')
	self.file.def_var ('time_offset', nc.DOUBLE, ('time',))
	self.file.def_att ('units', 'time_offset', nc.CHAR, 'seconds')
	self.file.def_att ('long_name', 'time_offset', nc.CHAR,
		    'Time offset from base_time')
	#
	# platform information.
	#
	self.file.def_dim ('platform', len (self.plats))
	self.file.def_dim ('fldlen', 50)
	self.file.def_var ('platform', nc.CHAR, ('platform', 'fldlen'))
	self.file.def_var ('lat', nc.FLOAT, ('platform',))
	self.file.def_var ('lon', nc.FLOAT, ('platform',))
	self.file.def_var ('alt', nc.FLOAT, ('platform',))
	#
	# Now go through the supplied fields and create them.
	# 
	for field in self.fields.keys ():
	    self.file.def_var (field, nc.FLOAT, ('time', 'platform'))
	    self.file.def_att ('long_name', field, nc.CHAR, 
		  self.fields[field][0]);
	    self.file.def_att ('units', field, nc.CHAR, self.fields[field][1])
	    self.file.def_att ('missing_value', field, nc.FLOAT, self.BadValue)
	#
	# Done with definitions.  Store the base time.
	#
	self.file.endef ()
	self.file.sync ()
	base = self.file.var ('base_time')
	base[0] = when
	self.beginTime = when;
	#
	# Store the platform names and locations
	#
	pv = self.file.var ('platform')
	lat = self.file.var ('lat')
	lon = self.file.var ('lon')
	alt = self.file.var ('alt')
	for i in range (len (self.plats)):
	    pv[i] = self.plats[i]
	    lat[i] = self.positions[i][0]
	    lon[i] = self.positions[i][1]
	    alt[i] = self.positions[i][2]
#	self.file.sync ()
    #
    # Get the index of a platform.
    #
    def platindex (self, platform):
	for i in range (len (self.plats)):
	    if self.plats[i] == platform:
		return i
	raise "Unknown platform in irgrid: " + platform
    #
    # Store some data in the file.  Everything is a list of data values; you
    # can store one at a time, or accumulate the whole thing first.
    #
    def Store (self, platform, time, data):
	pind = self.platindex (platform)
	ind = self.timeindex (time)
	#
	# Now we slap in each field.
	#
	for field in self.fields.keys ():
#	    print 'Store %s, pind %d ind %d' % (field, pind, ind)
#	    print data
	    var = self.file.var (field)
	    if data.has_key (field):
#		var[ind][pind] = data[field]
		d = var[ind]
		d[pind] = data[field]
		var[ind] = d
	    else:
		var[ind][pind] = self.BadValue
    #
    # Get an index to say where we store data from this time.
    #
    def timeindex (self, when):
	self.NewFile (when)
	off = when - self.beginTime
	var = self.file.var ('time_offset')
	ba = self.makebad ()
	for i in range (len (var)):
	    if var[i] == off:
#		print 'timeindex equals case'
		return i	# we should be so lucky
	    if var[i] > off:	# hmm...overshot.  Make space.
#		print 'Insert, off = %f, var = %f' % (off, var[i])
		newmax = len (var)
		for ind in range (newmax, i, -1):
		    var[ind] = var[ind - 1]
		var[i] = off
		for field in self.fields.keys ():
		    v = self.file.var (field)
		    for ind in range (newmax, i, -1):
		        v[ind] = v[ind - 1]
		    v[i] = ba
		return i
	i = len (var)
#	print 'Append case, len = %d' % i
	var[i] = off
#	print 'Len now is %d' % len(var)
	for field in self.fields.keys ():
	    v = self.file.var (field)
	    v[i] = ba
#	self.file.sync ()
	return i

    #
    # Make a list full of bad flags.
    #
    def makebad (self):
	l = [ ]
	for i in range (len (self.plats)):
	    l.append (self.BadValue)
	return l
    #
    # Finish.
    #
    def Done (self):
	if self.fname != None:
	    self.file.close ()
	self.fname = None
#
# Utility routine to decode ascii variables out of a string.  This one
# uses the same fields array that init does, and looks for the offset
# of the data for each field as the third entry in the dictionary entry
# for the field.  The bad value flag used by this field is in [4]
#
    def DecodeVars (self, line, fields, result):
	for field in fields.keys ():
	    offset = fields[field][2]
	    if offset < 0:
		continue
    #	v = eval (line[offset])
	    v = string.atof (line[offset])
	    if v == fields[field][3]:
		v = self.BadValue
	    result[field] = v
