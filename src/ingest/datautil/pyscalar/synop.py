#
# Make sense out of a synop entry
#
import string
import time
import regsub

#
# Read a file containing synop entries, returning a list of each.
#
# Each entry in the big list will be: (time, station, data)
# where "data" is a dictionary indexed by field.
#
def grok (file, year, month):
    #
    # Open the file
    #
    infile = open (file, "r")
    #
    # Start cranking.
    #
    biglist = [ ]
    state = 'extern'
    while 1:
	#
	# Next line
	#
	line = infile.readline ()
	if line == '':
	    return biglist
	line = line[:-1]
	#
	# Attempt to figure out what to do with it, depending on our
	# current state.
	#
	if len (line) < 4:		# blank
	    continue
	if (line[-1] == '\r'):
	    line = line[:-1]
	#
	# Maybe it begins a set of observations, in which case we need
	# to extract the day/month pair.
	#
	if line[0:4] == 'AAXX':	# Begin set of observations
	    sline = string.split (line)
	    day = string.atoi (sline[1][0:2])
	    hour = string.atoi (sline[1][2:4])
	    print 'Begin section at %d/%d' % (day, hour)
	    otime = time.mktime (year, month, day, hour, 0, 0, 0, 0, 0)
	    otime = otime - time.timezone
	    state = 'obs'
	elif line[0:2] == 'SM':
	    state = 'extern'
	#
	# If we're in a state where we're reading observations, this really 
	# should be one.
	#
	elif state == 'obs':
	    try:
		string.atoi (line[0:5])
	    except:
		print 'Strange obs line: \n  ' + line
		continue
	    while line[-1] != '=':
		line = line + infile.readline ()[:-1]
		if (line[-1] == '\r'):
		    line = line[:-1]
	    biglist.append (grok_synop (line, otime))

#
# Here we try to make sense out of a single observation.
#
def grok_synop (line, otime):
    #
    # Fix up and split the line of this observation.
    #
    line = regsub.gsub ('/', '0', line)
    if line[-1] == '=':
	line = line[:-1]
    sline = string.split (line)
    data = { }
    #
    # Second group with station and cloud base info.  "table needed".
    #
    data['cloud_base'] = string.atof (sline[1][2])
    #
    # Third group is cloud cover, and wind information.
    #
    data['cloud_cover'] = string.atof (sline[2][0])
    data['wdir'] = string.atof (sline[2][1:3])*10.0
    data['wspd'] = string.atof (sline[2][3:5])*(1852.0/3600.0)
    #
    # From here on we have the number-tagged groups.
    #
    for group in sline[3:]:
	#
	# Group 1 is temperature info; 2 is dewpoint.
	#
	if group[0] == '1':
	    data['tdry'] = getTemp (group)
	elif group[0] == '2':
	    data['dp'] = getTemp (group)
	#
	# 3 and 4 pressure
	#
	elif group[0] == '3':
	    data['spres'] = getPres (group)
	elif group[0] == '4':
	    data['pres'] = getPres (group)
	#
	# 5 pressure tendency
	#
	elif group[0] == '5':
	    data['ptend'] = string.atof (group[2:])/10.0
	#
	# 6 Precip.  "table needed"
	#
	elif group[0] == '6':
	    data['rain'] = string.atof (group[1:4])/10.0  # this is wrong
	else:
	    break
    #
    # OK, now build up our list and it's done.
    #
    return [otime, sline[0], data]
	


#
# Decrypt pressure.
#
def getPres (group):
    pres = string.atof (group[1:])/10.0
    if pres < 100:
	pres = pres + 1000
    return pres

#
# Temperature decryption
#
def getTemp (group):
    temp = string.atof (group[2:])/10.0
    if group[1] == '1':
	temp = -temp
    return temp
