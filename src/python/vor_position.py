#!/usr/bin/python

import sys
import os
import string
import time
from Tkinter import *
import tkSimpleDialog

class EnterPosition( Frame ):

    def __init__( self ):
        self.myname = "PositionEntry"
        
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
        self.title = Label( self, text="Position Entry" )
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
        self.status = Label( self, text="", fg="blue3" )
        self.status.pack( side=BOTTOM, fill=X )
        
        #
        # platform buttons
        #
        self.platform = StringVar()

        frame = Frame( self )
        frame.pack( side=LEFT )
        
        plats = ["n308d", "n42rf", "arat", "falcon"]
        self.platform.set( plats[0] )

        for p in plats:
            b = Radiobutton( frame, text=p, variable=self.platform,
                             value=p )
            b.pack( anchor=W )

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
            if (type( range ) != type( 1 ) and type( range ) != type( 1.0 )):
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
            #
            # Make sure the time is interpreted as GMT
            #
            os.putenv( "TZ", "GMT" )
            #
            # Warning!  The +%s on date (return Unix time in seconds)
            # is non-standard.  This will work for Linux only!
            #
            lines = os.popen( "date -d'%s' +%%s" % string, "r" ).readlines()
            print lines[0]
            time = eval( lines[0] )
            if (type( range ) != type( 1 )):
                raise NameError
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
        
        return ( azim, range, alt )
        
    def Enter( self ):
        self.lasttime = time.time()
        self.DoPoint( self.lasttime )
        
    def RedoLast( self ):
        self.DoPoint( self.lasttime )

    def DoPoint( self, time ):
        try:
            azim, range, alt = self.GetVals()
        except:
            return
        
        command = "actrack %s -vor %s %.1f %.1f %.3f %.0f" % \
                  (self.platform.get(), self.vor.get(), azim, range, alt, time)
        os.system( command )

        self.SetStatus( "Entered: %s %.0f deg @ %.1f nm, %s" %
                        (self.vor.get(), azim, range, self.time_entry.get()) )
        
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

    if len( sys.argv ) != 1:
        print "Usage: %s" % sys.argv[0]
        sys.exit( 1 )

    #
    # There isn't one of me out there already, so I get to exist...
    #
    EnterPosition().mainloop()
