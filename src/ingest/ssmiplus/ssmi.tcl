#
# tcl script for specifying ssmi_ingest parameters
#

set Format level1b			;# "rss", "level1b", or "1b"
set Resolution 12.5			;# kilometers
set MinScans 32				;# min number of scan pairs per image
set DisableLimits true			;# take everything
set Radius 50.0				;# kilometers

Origin -9.42 159.97 			;# use the same origin for all images

#
# Set the range limits for accepting scans; these values for TRMM, Darwin
#
#ScanLimits 110.0 -25.0 150.0 0.0	;# wlon, slat, elon, nlat

#
# For the MSFC DAAC .intoga tapes, they've already selected our scans
# for us.
#
set DisableLimits true			;# take everything

#
# Specify which fields to calculate and ingest
#
#SetFields 	ta19v ta19h ta22v ta37v ta37h ta85v ta85h

#
# We had to split ta and tb into separate platforms because of the 
# hard-coded ten-field limit in raster files.  Arghhhh!!!  #@#$%#@!
#
SetFields 	tb19v tb19h tb22v tb37v tb37h tb85v tb85h rr

#Someday:
#AddField sfcidx

#ListFields	;# return list containing field names
#AddField <field>
#DeleteField <field>

#puts "Reading these fields:"
#puts [ListFields]

# ----------------------------------------------------------------
# For ingesting a directory of compressed level 1b files
#
proc Ingest {plat files} \
{
	global Platform

	set Platform "$plat"
	puts "Ingesting SSMI files for platform $Platform..."
	set i 0
	foreach f $files \
	{
		if {[string match *.Z $f]} \
		{
			puts "uncompress $f"
			exec uncompress $f
			regsub {.Z$} $f "" f
		}
		puts "reading file $f"
		ProcessFile $f
		puts "compress $f"
		exec compress $f
		incr i
	}
	puts "$i files ingested to platform $Platform."		
}
# ------------------------------------------------------- Ingest --


#
# split up the processing load based on the name we were invoked under
#

set dir "/net/sfcd/ssmi/raw"

if {$Ourname == "test"} \
{
	set Platform ssmi_s2_tb
	Ingest $Platform [glob $dir/f8/S2.D93001.S0555.intoga*]
	Ingest $Platform [glob $dir/f8/S2.D92306.*.intoga*]
	set Platform ssmi_s4_tb
	Ingest $Platform [glob $dir/f10/S4.D9236?.S*.intoga*]
	set Platform ssmi_s5_tb
	Ingest $Platform [glob $dir/f11/S5.D92306.*.intoga*]
	exit 0
}


if {$Ourname == "ssmi_s2"} \
{
	puts "Using instructions for $Ourname..."
	Ingest ssmi_s2_tb [glob $dir/f8/S2.*]
	exit 0
}

if {$Ourname == "ssmi_s4"} \
{
	puts "Using instructions for $Ourname..."
#	Ingest ssmi_s4_tb [glob $dir/f10/S4.*]
	Ingest ssmi_s4_tb [glob "$dir/f10/S4.D923\[23456\]*"]
	Ingest ssmi_s4_tb [glob $dir/f10/S4.D93*]
	exit 0
}

if {$Ourname == "ssmi_s5"} \
{
	puts "Using instructions for $Ourname..."
#	Ingest ssmi_s5_tb [glob $dir/f11/S5.*]
	Ingest ssmi_s5_tb [glob "$dir/f11/S5.D923\[23456\]*"]
	Ingest ssmi_s5_tb [glob $dir/f11/S5.D93*]
	exit 0
}

if {$Ourname == "ssmi_s5_92"} \
{
	puts "Using instructions for $Ourname..."
	Ingest ssmi_s5_tb [glob $dir/f11/S5.D92*]
	exit 0
}

if {$Ourname == "ssmi_s5_93"} \
{
	puts "Using instructions for $Ourname..."
	Ingest ssmi_s5_tb [glob $dir/f11/S5.D93*]
	exit 0
}

puts "Warning: don't have any instructions for $Ourname. Nothing done."
exit 1

