#!/bin/csh
#
# This is an attempt at a generalized zeb startup script.
#
# $Id: zstart.sh,v 1.4 1995-02-03 18:09:52 burghart Exp $
#
# Here we do basic location of directories, set environment variables,
# and try to hand things off to a project-specific startup file.
#

#
# The base directory must be right.  We need to either fix things up to
# substitute it during a zeb install, or this script needs to be edited.
#
	if (! $?ZEB_TOPDIR ) setenv ZEB_TOPDIR ##TOPDIRHERE
#
# Tweak some important variables.
#
	set path=($ZEB_TOPDIR/bin $path)

# 
# Add ZEB_TOPDIR/lib/resources directory for X application defaults.  Using
# XFILESEARCHPATH puts these resources very near the bottom of the search 
# heirarchy, so they're easily overridden.
#
	if ( $?XFILESEARCHPATH ) then
		setenv XFILESEARCHPATH \
			${XFILESEARCHPATH}:$ZEB_TOPDIR/lib/resources/%N
	else
		setenv XFILESEARCHPATH $ZEB_TOPDIR/lib/resources/%N
	endif

#
# Make pointers to all of our executables so that somebody can
# override them if desired.
#
	if (! $?ZEB_MESSAGE) setenv ZEB_MESSAGE $ZEB_TOPDIR/bin/message
	if (! $?ZEB_EVENTLOGGER) setenv ZEB_EVENTLOGGER \
					 $ZEB_TOPDIR/bin/EventLogger
	if (! $?ZEB_TIMER) setenv ZEB_TIMER $ZEB_TOPDIR/bin/timer
	if (! $?ZEB_DSDAEMON) setenv ZEB_DSDAEMON $ZEB_TOPDIR/bin/dsDaemon
	if (! $?ZEB_DM) setenv ZEB_DM $ZEB_TOPDIR/bin/dm
#
# Look at args
#
	unset projdir
	while ($#argv)
		switch ($argv[1])
		    case -preserve:
		    	setenv PRESERVE_ZEB yes
			breaksw
		    case -data*:
			setenv DATA_DIR $argv[2]
			shift
			breaksw
		    default:
		    	set projdir=$argv[1]
			breaksw
		endsw
		shift
	end
#
# Figure out where the project directory is.
#
	if ( ! $#argv && ! $?projdir ) then
		if ( -f proj_startup ) then
			set projdir=`pwd`
		else
			echo -n "Please enter the project directory name: "
			set projdir=$<
		endif
#	else
#		set projdir=$argv[1]
	endif

again:
#
# See if we can find it.
#
	if (-d $projdir ) then
		; # nothing
	else if ( -d $ZEB_TOPDIR/$projdir ) then
		set projdir=$ZEB_TOPDIR/$projdir
	else if ( -d $ZEB_TOPDIR/project/$projdir ) then
		set projdir=$ZEB_TOPDIR/project/$projdir
	else
		echo "I can't find project directory" $projdir
		echo -n "Try again: "
		set projdir=$<
		goto again
	endif
	echo 'Project dir is' $projdir
	setenv ZEB_PROJECT $projdir:t
#
# Data directory.
#
ddir_again:
	if ( ! $?DATA_DIR ) then
		echo -n "Where is your data directory? "
		set ddir=$<
		if ( ! -d $ddir ) then
			echo Directory $ddir does not exist!
			goto ddir_again
		endif
		setenv DATA_DIR $ddir
	endif
#
# Set the color map to something that will hopefully make outlines
# show up.  Test its exit status to see if we could connect to a display.
#
	setenv HOST `uname -n`
	tweakcolor red
	if ( $status != 0 ) then
		echo ' '
		echo 'An error occurred connecting to the X server.  Make sure'
		echo 'the DISPLAY environment variable is set correctly.'
		echo 'For example,'
		echo "   $HOST% setenv DISPLAY server:0.0"
		echo 'where "server" is the name of the machine whose display'
		echo "will be used.  Then run 'xhost $HOST' on the server."
		exit 1
	endif		
#
# Do everything possible to insure that we start clean.  That means either
# stopping a running Zeb or setting ZEB_SOCKET to a unique name. 'mstatus'
# exits with 0 status when message is running, 1 otherwise.
#
	mstatus >& /dev/null
	if ($status == 0) then
		echo "Someone is already running Zeb.  Enter"
restart_prompt:
		echo "  1) to stop the current Zeb session and start over, or"
		echo "  2) to run an additional, separate session of Zeb."
		echo -n "Please enter 1, 2, or [Qq]uit: "
		set ans=$<
		if ( "$ans" =~ [Qq]* ) then
		  exit 1
		else if ("$ans" == "1") then
		  echo "Stopping the current Zeb session."
		else if ("$ans" == "2") then
		  echo "Starting a new Zeb session."
		  if (! $?USER) then
			setenv ZEB_SOCKET /tmp/zeb.socket.$$
		  else
			setenv ZEB_SOCKET /tmp/zeb.$USER.$$
		  endif
		  echo "This Zeb session will use the socket $ZEB_SOCKET"
		  echo "Enter 'setenv ZEB_SOCKET $ZEB_SOCKET' at the C-shell"
		  echo "prompt to run Zeb programs from that shell."
		else
		  goto restart_prompt
		endif
	endif
#
# Now try to start clean, whether deliberately killing an existing Zeb
# or just cleaning a leftover socket file
#
	zstop
	sleep 1
	if ( ! $?ZEB_SOCKET ) then
		rm -f /tmp/fcc.socket
	else
		rm -f $ZEB_SOCKET
	endif
#
# Start core processes.  Message is started in foreground now; we'll wait
# for it to background itself.
#
	echo 'Starting core zeb processes: '
	echo '	message daemon'
	$ZEB_MESSAGE

	echo -n '	event logger '
	$ZEB_EVENTLOGGER &
	sleep 1

	echo -n '	timer '
	$ZEB_TIMER &
#
# Now we need to run the per-project startup file.
#
	cd $projdir
	if ( -f proj_startup ) source proj_startup
#
# Fire off the data store daemon and the display manager.  Daemon only if
# there is not a pointer to a remote host, however
#
	if ($?DS_DAEMON_HOST) then
		echo '	(DS Daemon running on' $DS_DAEMON_HOST ')'
	else
		$ZEB_DSDAEMON ds.config &
	endif
	sleep 5
	if ( ! $?DEFAULT_CONFIG ) setenv DEFAULT_CONFIG empty
	$ZEB_DM dm.config
#
# Maybe we shut down.
#
	if ( ! $?PRESERVE_ZEB ) zstop
