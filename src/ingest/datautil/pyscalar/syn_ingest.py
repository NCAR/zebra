#!/usr/bin/python
#
# Ingest those messy synop messages.
#
# syn_ingest.py year file
#
import synop
import IRGIngest
from IRGIngest import IRGIngestor
import sys
import string

#
# Define the field list.
#
Fields = { "pres":	("Pressure", "mb"),
	   "tdry":	("Temperature", "C"),
	   "dp":	("Dewpoint temperature", "C"),
	   "wdir":	("Wind direction", "degrees"),
	   "wspd":	("Wind speed", "m/s"),
	   "spres":	("Station pressure", "mb"),
	   "cloud_base": ("Cloud base", "code"),
	   "cloud_cover": ("Clout cover", "1/8 sky"),
	   "ptend":	("Pressure tendency", "mb/3h"),
	   "rain":	("Rainfall", "secret code")
}
#
# The usual argument check.
#
if len (sys.argv) != 4:
    print 'Usage: syn_ingest.py year month file'
    sys.exit (1)

#
# Try to do the ingest.
#
stuff = synop.grok (sys.argv[3], string.atoi (sys.argv[1]), 
      string.atoi (sys.argv[2]))
if stuff == None:
    print 'Bummer, read of file failed'
    sys.exit (1)

#
# Well, that worked, so we're going to have to get off our lazy asses and 
# read the station information file.
#
latlons = [ ]
plats = [ ]
pdict = { }
sfile = open ("world.proc", "r")
for line in sfile.readlines ():
    sline = string.split (line[:-1])
    if len (sline) != 3:
	continue
    latlons.append ([string.atof (sline[1]), string.atof (sline[2]), 0.0])
    plats.append (sline[0])
    pdict[sline[0]] = 'howdy'
sfile.close ()

#
# Create our ingestor.
#
ing = IRGIngestor ('sdata', 'synoptic', plats, latlons, Fields)

#
# Store everything.
#
for sample in stuff:
    if pdict.has_key (sample[1]):
	ing.Store (sample[1], sample[0], sample[2])
    else:
	print 'Dropping station ' + sample[1]
#
# Done!
#
print "%d samples." % len (stuff)
ing.Done ()
