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
        # VOR buttons
        #
        self.vor = StringVar()

        frame = Frame( self )
        frame.pack( side=LEFT )
        
        vors = ["RON", "TZO", "SRN", "TOP", "VIL", "GEN", "CHI"]
        self.vor.set( vors[0] )

        for v in vors:
            b = Radiobutton( frame, text=v, variable=self.vor, value=v )
            b.pack( anchor=W )

        #
        # Az/Range/Alt/Time entry
        #
        entry_frame = Frame( self )
        entry_frame.pack( side=RIGHT )
        entrywidth = 10

        frame = Frame( entry_frame )
        frame.pack( side=TOP )
        Label( frame, text="Azimuth (deg)", width=14 ).pack( side=LEFT )
        self.az_entry = Entry( frame, width=entrywidth )
        self.az_entry.pack( side=RIGHT )

        frame = Frame( entry_frame )
        frame.pack( side=TOP )
        Label( frame, text="Range (nm)", width=14 ).pack( side=LEFT )
        self.rng_entry = Entry( frame, width=entrywidth )
        self.rng_entry.pack( side=RIGHT )

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
        string = self.az_entry.get()
        try:
            azim = eval( string )
            if (type( azim ) != type( 1 ) and type( azim ) != type( 1.0 )):
                raise NameError
        except:
            self.SetStatus( "Bad azimuth '%s'" % string, "red3" )
            self.az_entry.delete( 0, END )
            raise NameError

        string = self.rng_entry.get()
        try:
            range = eval( string )
            if (type( range ) != type( 1 ) and type( range ) != type( 1.0 )):
                raise NameError
        except:
            self.SetStatus( "Bad range '%s'" % string, "red3" )
            self.rng_entry.delete( 0, END )
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
        # Time is a bit tricky.  We use the system "date" command to
        # parse the time string for us and return a Unix time in
        # seconds
        #
        string = self.time_entry.get()
        try:
	    if ( self.time_pattern.match (string) == None ):
	        raise NameError
	    time = string;
            #
            # Make sure the time is interpreted as GMT
            #
            #os.putenv( "TZ", "GMT" )
            #
            # Warning!  The +%s on date (return Unix time in seconds)
            # is non-standard.  This will work for Linux only!
            #
            #lines = os.popen( "date -d'%s' +%%s" % string, "r" ).readlines()
            #print lines[0]
            #time = eval( lines[0] )
            #if (type( range ) != type( 1 )):
            #    raise NameError
        except:
            self.SetStatus( "Bad time string '%s'" % string, "red3" )
            self.time_entry.delete( 0, END )
            raise NameError
        #
        # convert range from nautical miles to km
        #
        range = range * (1.609344 * 6080.0 / 5280.0)

        #
        # convert altitude from feet to km
        #
        alt = alt / 5280.0 * 1.609344;
        
        return ( azim, range, alt, time )
        
    def Enter( self ):
        # self.lasttime = time.time()
	self.DoPoint ()
        
    def RedoLast( self ):
         self.DoPoint( self.lasttime )

    def DoPoint( self ):
        try:
            azim, range, alt, time = self.GetVals()
        except:
            return
        
        command = "actrack %s -vor %s %.1f %.1f %.3f %s" % \
                  (self.platform.get(), self.vor.get(), azim, range, alt, time)
        lines = os.popen( command, "r" ).readlines()
	print "vor_position: ", lines[0]
        #print lines[0]
	#os.system( command )
	when, lat, lon, junk = re.split ("\s+", lines[0])[-4:]

        self.SetStatus("Entered: %s %.0f deg @ %.1f nm, %s\n"
		       "%s    Lat: %.3f    Lon: %.3f" %
		       (self.vor.get(), azim, range, self.time_entry.get(), \
		       when, float(lat), float(lon)) )
        
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

    if (plane not in ["all", "n308d", "n42rf", "arat", "merlin"]):
        print "unrecognized platform name %s" % plane
	sys.exit(1)

    #
    # There isn't one of me out there already, so I get to exist...
    #
    EnterPosition(plane,title).mainloop()

