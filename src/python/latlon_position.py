#!/usr/bin/python

import sys
import os
import string
import time
import re
from Tkinter import *
import tkSimpleDialog

class EnterPosition( Frame ):

    def __init__( self, plat, title ):

        if (plat == "all"):
	        self.myname = "PositionEntry"
	else:
		self.myname = "%s" % plat
        
        #
        # Initialize our Frame self
        #
        Frame.__init__( self )
        self.pack()

        self.master.title( self.myname )
        self.master.iconname( self.myname )

        #
        # title
        #
	if (plat == "all"): title = "Position Entry" 
        self.title = Label( self, text=title )
        self.title.pack( side=TOP )

        #
        # Quit, enter, and peek buttons
        #
        frame = Frame( self )
        frame.pack( side=BOTTOM, fill=X )

        Button( frame, text="Quit", command=self.Quit ).pack( side=RIGHT )
        Button( frame, text="Enter", command=self.Enter ).pack( side=LEFT )
        #Button( frame, text="Redo Last",
        #        command=self.RedoLast ).pack( side=LEFT )

        #
        # status label
        #
        self.status = Label( self, text="\n", fg="blue3" )
        self.status.pack( side=BOTTOM, fill=X )
        
        #
        # platform buttons
        #
        self.platform = StringVar()
        plats = ["n308d", "n42rf", "arat", "merlin"]

	if (plat == "all"):

	        frame = Frame( self )
		frame.pack( side=LEFT )
        
	        self.platform.set( plats[0] )

		for p in plats:
		            b = Radiobutton( frame, text=p, 
					     variable=self.platform,
					     value=p )
			    b.pack( anchor=W )

	else:
		self.platform.set( plat )

        #
        # Lat/Lon/Alt/Time entry
        #
        entry_frame = Frame( self )
        entry_frame.pack( side=RIGHT )
        entrywidth = 10

        frame = Frame( entry_frame )
        frame.pack( side=TOP )
        Label( frame, text="Latitude (deg N)", width=14 ).pack( side=LEFT )
        self.lat_entry = Entry( frame, width=entrywidth )
        self.lat_entry.pack( side=RIGHT )

        frame = Frame( entry_frame )
        frame.pack( side=TOP )
        Label( frame, text="Longitude (deg E)", width=14 ).pack( side=LEFT )
        self.lon_entry = Entry( frame, width=entrywidth )
        self.lon_entry.pack( side=RIGHT )

        frame = Frame( entry_frame )
        frame.pack( side=TOP )
        Label( frame, text="Height (ft)", width=14 ).pack( side=LEFT )
        self.alt_entry = Entry( frame, width=entrywidth )
        self.alt_entry.pack( side=RIGHT )

        frame = Frame( entry_frame )
        frame.pack( side=TOP )
        Label( frame, text="hh:mm[:ss]", width=14 ).pack( side=LEFT )
        self.time_entry = Entry( frame, width=entrywidth )
        self.time_entry.pack( side=RIGHT )
	self.time_pattern = re.compile ("[0-2]?[0-9]:[0-5]?[0-9](:[0-5]?[0-9])?")
    #
    # Return the current az, range, alt, and time values
    #
    def GetVals( self ):
        string = self.lat_entry.get()
        try:
            lat = eval( string )
            if (type( lat ) != type( 1 ) and type( lat ) != type( 1.0 )):
                raise NameError
        except:
            self.SetStatus( "Bad latitude '%s'" % string, "red3" )
            self.lat_entry.delete( 0, END )
            raise NameError

        string = self.lon_entry.get()
        try:
            lon = eval( string )
            if (type( lon ) != type( 1 ) and type( lon ) != type( 1.0 )):
                raise NameError
        except:
            self.SetStatus( "Bad longitude '%s'" % string, "red3" )
            self.lon_entry.delete( 0, END )
            raise NameError

        string = self.alt_entry.get()
        try:
            alt = eval( string )
            if (type( alt ) != type( 1 ) and type( alt ) != type( 1.0 )):
                raise NameError
        except:
            self.SetStatus( "Bad altitude '%s'" % string, "red3" )
            self.alt_entry.delete( 0, END )
            raise NameError
        #
        # Time is a bit tricky, so we pass off the interpretation to the
	# ingestor.
        #
        string = self.time_entry.get()
        try:
	    if ( self.time_pattern.match (string) == None ):
	        raise NameError
	    time = string;
        except:
            self.SetStatus( "Bad time string '%s'" % string, "red3" )
            self.time_entry.delete( 0, END )
            raise NameError
        #
        # convert altitude from feet to km
        #
        alt = alt / 5280.0 * 1.609344;
        
        return ( lat, lon, alt, time )
        
    def Enter( self ):
        # self.lasttime = time.time()
	self.DoPoint ()
        
    def RedoLast( self ):
         self.DoPoint( self.lasttime )

    def DoPoint( self ):
        try:
            lat, lon, alt, time = self.GetVals()
        except:
            return
        
        command = "%s/bin/actrack %s %.1f %.1f %.3f %s" % \
           (os.environ["ZEB_TOPDIR"], self.platform.get(), lat, lon, alt, time)
	print command
        lines = os.popen( command, "r" ).readlines()
	print "latlon_position: ", lines[0]
        #print lines[0]
	#os.system( command )
	lat, lon, alt, when, junk = re.split ("\s+", lines[0])[-5:]

        self.SetStatus("Entered: %s\n"
		       " Stored: Lat: %.3f  Lon: %.3f  Alt(km): %.3f" %
		       (self.time_entry.get(), 
			float(lat), float(lon), float(alt)))
        
    #
    # Set the status text, with optional color
    #
    def SetStatus( self, string, color="blue3" ):
        self.status.config( text=string, fg=color )
        
    #
    # Done
    #
    def Quit( self ):
        self.quit()



if __name__ == "__main__":

    name = sys.argv[0]
    plane = "all"
    title = "Position Entry"

    if len( sys.argv ) > 3:
        print "Usage: %s [platform[title]]" % sys.argv[0]
        sys.exit( 1 )
    if len( sys.argv ) >= 2:
        plane = sys.argv[1]
	title = plane
    if len( sys.argv ) == 3:
        title = sys.argv[2]

    #
    # There isn't one of me out there already, so I get to exist...
    #
    EnterPosition(plane,title).mainloop()

