#!/usr/local/bin/python

import sys, os, string, signal
from zebra import msg, ds

from Tkinter import *
import tkSimpleDialog

class DataTimes( Frame ):

    def __init__( self, name, gp, platform ):
        self.gp = gp
        self.myname = name
	self.lister_name = name + "_LM"
        self.platname = platform
	self.lm_pid = 0		# pid of our child ListMaker

        #
        # Get connected to the message system
        #
        try:
            msg.connect( self.ZebraMsgHandler, self.myname )
            msg.deathHandler( self.connectionDied )
        except:
            print "(%s) %s" % (sys.exc_type, sys.exc_value)
            print "%s: msg.connect failed" % sys.argv[0]
            sys.exit( 1 )

        #
        # Initialize our Frame self
        #
        Frame.__init__( self )
        self.pack()

        self.master.title( gp + "_DataTimes" )
        self.master.iconname( gp + "_DataTimes" )

        #
        # Let Tk know that we need to deal with stuff from the message
        # system
        #
        self.tk.createfilehandler( msg.get_fd (), tkinter.READABLE,
                                   self.TkZebraMsgHandler )

        #
        # title and platform label
        #
        self.title = Label( self, text="Time chooser for '%s'" % self.gp )
        self.title.pack( side=TOP )
        self.platlabel = Label( self )
        self.platlabel.pack( side=TOP )

        #
        # status label
        #
        self.status = Label( self, text="" )
        self.status.pack( side=BOTTOM )
        
        #
        # "Change Platform" and "Quit" buttons
        #
        quit = Button( self, text="Quit", command=self.Quit )
        quit.pack( side=BOTTOM )

        change = Button( self, text="Change Platform",
                         command=self.ChangePlatform )
        change.pack( side=BOTTOM )

        #
        # The listbox, with scrollbar
        #
        self.list = Listbox( self, width=30, height=15 )
        self.scroll = Scrollbar( self )

        self.list.config( yscrollcommand=self.scroll.set )
        self.list.pack( side=LEFT, fill=BOTH )
        self.list.bind( "<ButtonRelease-1>", self.HandleChoice )

        self.scroll.config( command=self.list.yview )
        self.scroll.pack( side=RIGHT, fill=Y )

        self.NewPlatform()

    #
    # Here's the stuff we do when we get a new platform
    #
    def NewPlatform( self ):
        self.platlabel.config( text="(platform %s)" % self.platname )

        #
        # Clear the list if it isn't empty
        #
        len = self.list.size()
        if (len != 0):
            self.list.delete( 0, len - 1 )
            
        #
        # "Real time mode" is always the zeroth entry in our list.
        #
        self.list.insert( 0, "Real time mode" )

        #
        # Add all choices since zero time
        #
        self.latest = 0.0
        self.update()
	self.KillListMaker()
        self.StartListMaker()

	#
	# And request notification for new data
	#
        try:
            ds.RequestUpdates( self.platname, self.NewData )
        except:
            msg.ELog( msg.EF_PROBLEM, "(%s) %s.  Exiting." %
                           (sys.exc_type, sys.exc_value) )
            self.Quit( 1 )


    def StartListMaker( self ):
	#
	# See if we have a list generator still running.  If so, just
	# let it continue until it's done.  No need for a new one.
	#
	if (self.lm_pid):
	    pid, status = os.waitpid( self.lm_pid, os.WNOHANG )

	    if (not pid):
		return;	# if our child has not exited, it must still be running

	    self.lm_pid = 0
	#
	# Nope, we need to start a new list generator, which will send us
	# messages via the Zebra message system to generate our list
	#
	pid = os.fork()
	if (pid != 0):
	    self.lm_pid = pid
	else:
            try:
                os.execlp( "ListMaker", "ListMaker", self.lister_name,
                           self.myname, self.platname, "%.6f" % self.latest )
            except:
                msg.ELog( msg.EF_PROBLEM,
                               "Error exec'ing ListMaker: %s.  Exiting." %
                               sys.exc_value )
                self.Quit( 1 )


    def KillListMaker( self ):
	if (not self.lm_pid):
		return

	pid, status = os.waitpid( self.lm_pid, os.WNOHANG )
	if (not pid):
	    os.kill( self.lm_pid, signal.SIGTERM )

	self.lm_pid = 0
	return
    #
    # This gets called when new data are stored for our platform
    #
    def NewData( self, platname, param, dtime ):
	self.StartListMaker()
	
    #
    # Tk file handler module for Zebra messages.  Just pass them off to
    # msg.incoming()
    #
    def TkZebraMsgHandler( self, fd, mask ):
        msg.incoming()

    #
    # Handle a choice from the listbox
    #
    def HandleChoice( self, event ):
        index = self.list.index( "@%d,%d" % (event.x, event.y) )
        #
        # Index 0 is "real time mode"
        #
        if (index == 0):
            cmd = "parameter global plot-mode real-time\0"
            time = "real-time"
        #
        # The rest are time choices
        #
        else:
            time = self.list.get( index )
            time = string.split(time)[0]
            cmd = "parameter global plot-mode history;"
            cmd = cmd + "parameter global plot-time %s\0" % time

        msg.send( self.gp, msg.MT_COMMAND, 0, cmd )
        self.status.config( text="Changed to " + time )
        

    #
    # Change platform
    #
    def ChangePlatform( self ):
        reply = tkSimpleDialog.askstring( "Change Platform", "Platform name" )
        if (reply):
            self.platname = reply
	    self.status.config( text="" )
            self.NewPlatform()

    #
    # Clean shutdown, with optional exit status (defaults to zero)
    #
    def Quit( self, status = 0 ):
        msg.disconnect()
	self.KillListMaker()
        sys.exit( status )

    #
    # Default message handler.  Unfortunately, this has to be a separate
    # function rather than a method of DataTimes.
    #
    def ZebraMsgHandler( self, source, proto, data ):
        #
        # Special handling for MH_SHUTDOWN message from the message manager
        #
        if (proto == msg.MT_MESSAGE):
            import struct
            (type,) = struct.unpack( "i", data[:4] )
            if (type == msg.MH_SHUTDOWN):
                print "%s: Shutting down" % sys.argv[0]
		self.Quit()
        #
        # A command:
	#    raise
	#	raise our window to the top
	#
	#    <time_string>|<attrs>|<dtime>
	#	add a line to our list
        #
        elif (proto == msg.MT_COMMAND):
	    if (data[:-1] == "raise"):
	        self.master.deiconify()
                self.master.tkraise()
	        return( 1 )
	    else:
		newentry = string.splitfields( data, "|" )
		timestring = newentry[0]
		attrs = newentry[1]
		dtime = string.atof( newentry[2][:-1] );
		if (dtime > self.latest):
			self.latest = dtime

		self.list.insert( 1, "%s   %s" % (timestring, attrs) )
        #
        # Anything else is unexpected
        #
        else:
            infomsg = "proto %d message from '%s' ignored" % (proto, source)

            if msg.isConnected():
                msg.ELog( msg.EF_INFO, infomsg )
            else:
                print infomsg

        return( 1 )

    #
    # This handler gets called if the message system or connection to it
    # goes away abnormally
    #
    def connectionDied( self ):
        print "%s: Message connection gone.  Untimely death..." % sys.argv[0]
        sys.exit( 1 )



if __name__ == "__main__":

    if len( sys.argv ) != 3:
        print "Usage: %s <graphics_process> <platform>" % sys.argv[0]
        sys.exit( 1 )

    #
    # Make my name
    #
    gpname = sys.argv[1]
    platname = sys.argv[2]
    name = "%s_DT" % gpname

    #
    # Add ZEB_TOPDIR/lib/python to the module search path
    #
    try:
        topdir = os.environ["ZEB_TOPDIR"]
    except:
        print "ZEB_TOPDIR is not set in your environment."
        print "Please set the ZEB_TOPDIR environment variable to point"
        print "to your top-level Zebra directory and try again."
        sys.exit( 1 )

    sys.path = [topdir + "/lib/python"] + sys.path
    #
    # Also make sure that ZEB_TOPDIR/bin is in the execution path
    #
    os.environ["PATH"] = topdir + "/bin:" + os.environ["PATH"]
    
    #
    # See if one of "me" is out there already.  If so, just tell it to pop
    # up to the top, then I can go away.
    #
    status = os.system( "msg_ping -c %s -t 1 > /dev/null" % name )
    if (status == 0):
        os.system( "zrun %s 'raise'" % name )
        sys.exit( 0 )

    #
    # There isn't one of me out there already, so I get to exist...
    #
    DataTimes( name, gpname, platname ).mainloop()


