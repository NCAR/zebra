#!/usr/local/bin/python
#
# Test out message functions.  Assumes the existence of a message manager.
#
import sys
import os
import zebra

Defproto = 70
Proto2 = 71
RealHandler = None
GotOne = 0

#
# Get a pipe for the fd test now.
#
(RPipe, WPipe) = os.pipe ()

#
# Snarf a single line.
#
PSnarfedLine = "nothing"
def PSnarfOne (source, proto, data):
    global PSnarfedLine
    PSnarfedLine = data
    return 1


#
# Poll until we get a useful message or until things time out.
#
def AwaitMsg (timeout, name):
    global GotOne
    GotOne = 0
    while (GotOne == 0):
	result = zebra.msg.poll (2)
	if (result == zebra.msg.MSG_TIMEOUT):
	    PBail ("Timeout in " + name)
	if (GotOne):
	    return 1
#
# Give it up.
#
def PBail (s):
    print "ERROR: " + s
    os.kill (pid, 9)
    sys.exit (1)
    
#
# The parent process.
#
def Parent ():
    import time
    print ("Test starting, child pid is %d" % (pid))
    #
    # Connect up.
    #
    if zebra.msg.isConnected ():
	print 'ERROR: isConnected true before connect'
    zebra.msg.connect (PHandler, "PTParent")
    if (not zebra.msg.isConnected ()):
	print 'ERROR: Evidently the connect failed'
	os.kill (pid, 9)
	sys.exit (1)
    #
    # Give the child a sec to get its act together, then let's start poking it.
    #
    zebra.msg.ELog (zebra.msg.EF_INFO, "mtest is here")
    time.sleep (2)
    #
    # Do a simple echo.
    #
    global RealHandler
    RealHandler = PSnarfOne
    zebra.msg.send ("PTChild", Defproto, 0, "echo hi, cutie")
    print "Echo test...",
    AwaitMsg (2, "echo test")
    if (PSnarfedLine != "hi, cutie"):
	PBail ("Got %s back on echo test" % (PSnarfedLine))
    print ("passed.")
    #
    # Broadcast test.
    #
    print "Broadcast test...",
    zebra.msg.send ("PTest", Defproto, 1, "echo Broadcast")
    AwaitMsg (2, "broadcast test")
    if (PSnarfedLine != "Broadcast"):
	PBail ("Got %s back on broadcast test" % (PSnarfedLine))
    print ("passed")
    #
    # Make sure protocol handlers work.
    #
    print "Protocol handler test...",
    zebra.msg.AddProtoHandler (Proto2, P2Handler)
    zebra.msg.send ("PTChild", Proto2, 0, "echo P2Test")
    AwaitMsg (2, "protocol handler test")
    if (PSnarfedLine != "P2Test"):
	PBail ("Bad return on proto handler test (%s)" % (PSnarfedLine))
    print ("passed")

    #
    # Search.
    #
    print "Search test...",
    zebra.msg.send ("PTChild", Defproto, 0, "echo first")
    zebra.msg.send ("PTChild", Defproto, 0, "echo second")
    zebra.msg.Search (Defproto, PSearch, "second")
    AwaitMsg (1, "search test (second line)")
    if (PSnarfedLine != "first"):
	PBail ("Enqueued first line didn't come back (%s)" % (PSnarfedLine))
    print "passed"

    #
    # Extra fd's
    #
    print "add_fd (%d) test..." % (RPipe),
    zebra.msg.add_fd (RPipe, PPipeHandler)
    zebra.msg.send ("PTChild", Defproto, 0, "pipe SmokeThis")
    AwaitMsg (2, "pipe test")
    if (PSnarfedLine != "SmokeThis\n"):
	PBail ("Pipe line didn't come back")
    zebra.msg.delete_fd (RPipe)
    print "passed"
    #
    # The query interface.
    #
    print "query test...",
    zebra.msg.SendQuery ("PTChild", PQueryRoutine)
    AwaitMsg (2, "query test")
    if (PSnarfedLine != "Query response"):
	PBail ("Query response mismatch")
    print "passed"    

    #
    # Shut it down.
    #
    zebra.msg.send ("PTChild", Defproto, 0, "die")
    
#
# Handler for the protocol handler test.
#
def P2Handler (source, proto, data):
    global GotOne
    global PSnarfedLine
    GotOne = 1
    PSnarfedLine = data
    return 1
#
# Handler for the search test.
#
def PSearch (source, proto, data, test):
    if (data == test):
	return zebra.msg.MSG_DONE
    return zebra.msg.MSG_ENQUEUE

#
# For the pipe test.
#
def PPipeHandler (fd):
    global PSnarfedLine
    global GotOne
    PSnarfedLine = os.read (RPipe, 10)
    GotOne = 1

#
# For the query test
#
def PQueryRoutine (s):
    global GotOne
    global PSnarfedLine
    if (s != None):
	PSnarfedLine = s
    else:
	GotOne = 1

#
# The parents message handler.
#
def PHandler (source, proto, data):
    global GotOne
    GotOne = 1
    return (RealHandler (source, proto, data))








#
# Child process starts down here.
#
def Child ():
    #
    # Connect and just wait for something to happen.
    #
    zebra.msg.connect (CHandler, "PTChild")
    zebra.msg.join ("PTest")
    zebra.msg.SetQueryHandler (CQuery)
    zebra.msg.await ()
    #
    # Done.
    #
    sys.exit (0)


#
# Deal with an incoming command from the parent.
#
def CHandler (source, proto, data):
#    data = data[:-1]
    #
    # See what we're supposed to do.
    #
    if (data == "die"):
	return 1
    if (data[:4] == "echo"):
	zebra.msg.send (source, proto, 0, data[5:])
    elif (data[:4] == "pipe"):
	os.write (WPipe, data[5:] + "\n")
    else:
	print ("CHILD, got funky stuff %s" % data)
    return 0

#
# Deal with a query request.
#
def CQuery (source):
    zebra.msg.AnswerQuery (source, "Query response")
    zebra.msg.FinishQuery (source)
    
#####
# The main program, such as it is.
#
# Here we fork off a child to communicate with.
#
pid = os.fork ()
if (pid != 0):
    Parent ()
else:
    Child ()
