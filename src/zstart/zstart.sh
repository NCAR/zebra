#!/bin/csh -f
#
# This is an attempt at a generalized zebra startup script.
#
# $Id: zstart.sh,v 1.11 1999-11-01 21:49:59 granger Exp $
#
# Here we do basic location of directories, set environment variables,
# and try to hand things off to a project-specific startup file.
#

# Aliasing exit to do fancy logout stuff could really mess things up
unalias exit
unset ignoreeof
unalias rm

#
# If this user has a zebra resource file, source it immediately.  That
# way they can override settings for themselves, such as ZEB_TOPDIR,
# ZEB_PROJDIR, and ZEB_CONFIGS.
#
	if ($?HOME) then
		if (-f $HOME/.zebra) then
			source "$HOME/.zebra"
		endif
	endif

#
# The base directory must be right.  We need to either fix things up to
# substitute it during a zebra install, or this script needs to be edited.
#
	if (! $?ZEB_TOPDIR ) setenv ZEB_TOPDIR ##TOPDIRHERE
#
# Tweak some important variables.
#
	set path=($ZEB_TOPDIR/bin $path)

# 
# Add ZEB_TOPDIR/lib/resources directory for X application defaults.  Using
# XFILESEARCHPATH puts these resources very near the bottom of the search 
# hierarchy, so they're easily overridden.
#
	if ( $?XFILESEARCHPATH ) then
		setenv XFILESEARCHPATH \
			${XFILESEARCHPATH}:$ZEB_TOPDIR/lib/resources/%N
	else
		setenv XFILESEARCHPATH $ZEB_TOPDIR/lib/resources/%N
	endif

#
# Look at args
#
	unset projdir
	set dmonly=0
	set dsonly=0
	set execshell=0
	set unique=0
	set check=0
	set name=""
	while ($#argv)
		switch ($argv[1])
		    case -h*:
echo "  -preserve	Keep session and datastore running after dm exits."
echo "  -data*	Specify the data directory."
echo "  -dm		Execute the display manager start-up only."
echo "  -ds		Just begin a datastore session and nothing else."
echo "  -shell	Start a shell with the correct runtime environment."
echo "  -n|-check	Non-interactive.  Fail instead of asking questions."
echo "		Do nothing if session is already running, else start"
echo "		one and return zero."
echo "  -unique  	Generate a unique socket and session name if needed."
echo "  -session	Set a session name for the message manager."
echo "  -help		Print a summary of the options."
			exit 0
			breaksw
		    case -preserve:
		    	setenv PRESERVE_ZEB yes
			breaksw
		    case -data*:
			setenv DATA_DIR $argv[2]
			shift
			breaksw
		    case -dm:
			set dmonly=1
			breaksw
		    case -ds:
			set dsonly=1
			breaksw
		    case -shell:
			set execshell=1
			breaksw
		    case -u*:
			set unique=1
			breaksw
		    case -check:
		    case -n:
			set check=1
			breaksw
		    case -s*:
			set SESSION="$argv[2]"
			shift
			breaksw
		    default:
		    	set projdir="$argv[1]"
			breaksw
		endsw
		shift
	end
#
# Figure out where the project directory is.
#
	if ( ! $?projdir ) then
		if ( -f proj_startup || -f proj_env ) then
			set projdir=`pwd`
		else if ( $?ZEB_PROJDIR ) then
			set projdir="$ZEB_PROJDIR"
		else if ( $check ) then
			echo "Project directory unknown."
			exit 1
		else
			echo -n "Please enter the project directory name: "
			set projdir=$<
		endif
	endif

again:
#
# See if we can find it.
#
	if (-d "$projdir" ) then
		; # nothing
	else if ( -d $ZEB_TOPDIR/$projdir ) then
		set projdir=$ZEB_TOPDIR/$projdir
	else if ( -d $ZEB_TOPDIR/project/$projdir ) then
		set projdir=$ZEB_TOPDIR/project/$projdir
	else if ( $check ) then
		echo "Project directory $projdir cannot be found."
		exit 1
	else
		echo "I can't find project directory" $projdir
		echo -n "Try again: "
		set projdir=$<
		goto again
	endif
	echo 'Project dir is' $projdir
	setenv ZEB_PROJECT $projdir:t
#
# Give the project the chance to augment some settings, supply defaults, 
# or replace some settings.  If it does not set a data directory, zstart
# will prompt the user for one.
#
	cd $projdir
	setenv ZEB_INTERNET
	if (-f proj_env) source proj_env
	if (! $?ZEB_PROJDIR) setenv ZEB_PROJDIR "$projdir"
	if (! $?HOST) setenv HOST `uname -n`
	if (! $?SESSION) set SESSION="$HOST"
#
# If another machine is hosting the datastore, start that session now
#
	if (! $?ZEB_ZSTART ) setenv ZEB_ZSTART $ZEB_TOPDIR/bin/zstart
	if ( $?DS_DAEMON_HOST ) then

		if (! $?DS_DAEMON_SESSION ) then
			setenv DS_DAEMON_SESSION "$DS_DAEMON_HOST"
		endif
		if ($DS_DAEMON_SESSION != $SESSION) then

	 		rsh $DS_DAEMON_HOST $ZEB_ZSTART -ds -n \
				-s $DS_DAEMON_SESSION $ZEB_PROJDIR
			if ( $status != 0 ) then
			  echo "No datastore session on host $DS_DAEMON_HOST."
		 	  exit 1
			endif
			# Clients actually need the session name, 
			# not the host, in case they're different
			setenv DS_DAEMON_HOST "$DS_DAEMON_SESSION"
		else
			# Check for a datastore-session-specific socket
			if ($?DS_DAEMON_SOCKET) then
			    setenv ZEB_SOCKET "$DS_DAEMON_SOCKET"
			endif
			setenv ZEB_EVENTLOGGER \
   "$ZEB_TOPDIR/bin/EventLogger -n -f $ZEB_PROJDIR/${DS_DAEMON_SESSION}.log"
			unsetenv DS_DAEMON_HOST
			unsetenv DS_DAEMON_SESSION
			setenv ZEB_INTERNET "$ZEB_INTERNET -internet"
		endif
	endif
# 
# Jump to the shell exec if nothing else explicitly requested
#
	if (! $dmonly && ! $dsonly && $execshell) goto start_shell

#
# Data directory.
#
ddir_again:
	if ( ! $dmonly && ! $?DATA_DIR && ! $?DS_DAEMON_HOST ) then
		if ( $check ) then
			echo "Data directory unknown."
			exit 1
		endif
		echo -n "Where is your data directory? "
		set ddir=$<
		if ( ! -d "$ddir" ) then
			echo Directory $ddir does not exist!
			goto ddir_again
		endif
		setenv DATA_DIR $ddir
	endif

#
# Set the color map to something that will hopefully make outlines
# show up.  Test its exit status to see if we could connect to a display.
# If this is only a datastore startup, then we don't need to tweak colors
# and we don't need to connect to a display.  We may be starting up a
# datastore session for a remote machine.
#
   if (! $dsonly) then

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

   endif

#
# If the user told us to do only the dm startup for a supposedly
# existing message manager session, so be it
#
	if ($dmonly) goto start_dm

#
# Do everything possible to insure that we start clean.  That means either
# stopping a running Zebra or setting ZEB_SOCKET to a unique name. 'mstatus'
# exits with 0 status when message is running, 1 otherwise.  Use the -u
# option to get the name of the user.
#
	set someone=`mstatus -u`
	set mstatus=$status
	if ($mstatus == 0 && $check) then
	   echo "Zebra session is running."
	   exit 0
	else if ($mstatus == 0 && $unique > 0) then
	    # Its up to us to find a unique socket and session name
	    set i = 0
	    while (-e /tmp/zebra.$HOST.$USER.$i)
		@ i = $i + 1
	    end
	    set SESSION=$HOST.$USER.$i
	    setenv ZEB_SOCKET /tmp/zebra.$SESSION
	else if ($mstatus == 0) then
	   echo "User $someone is already running Zebra.  Enter"
restart_prompt:
	   echo "  1) to stop the current Zebra session and start over, or"
	   echo "  2) to run an additional, separate session of Zebra, or"
	   echo "  3) to start a display manager for this session."
	   echo -n "Please enter 1, 2, 3, or [Qq]uit: "
	   set ans=$<
	   if ( "$ans" =~ [Qq]* ) then
		exit 1
	   else if ("$ans" == "1") then
		echo "Stopping the current Zebra session."
	   else if ("$ans" == "2") then
		echo "Starting a new Zebra session."
	 	if (! $?USER) then
			setenv ZEB_SOCKET /tmp/zeb.socket.$$
	   	else
			setenv ZEB_SOCKET /tmp/zeb.$USER.$$
	   	endif
	   else if ("$ans" == "3") then
		echo "Starting a display manager."
		goto start_dm
	   else
		goto restart_prompt
	   endif
	endif
	if ( $?ZEB_SOCKET ) then
	    echo "This Zebra session will use the socket $ZEB_SOCKET"
	    echo "Enter 'setenv ZEB_SOCKET $ZEB_SOCKET' at the C-shell"
	    echo "prompt to run Zebra programs from that shell."
	endif
#
# Now try to start clean, whether deliberately killing an existing Zebra
# or just cleaning a leftover socket file
#
	zstop >& /dev/null
	sleep 1
	if ( ! $?ZEB_SOCKET ) then
		rm -f /tmp/fcc.socket
	else
		rm -f $ZEB_SOCKET
	endif
#
# Make pointers to all of our executables so that somebody can
# override them (such as in proj_env or .zebra) if desired.  We wait
# until this last possible moment to allow message options to be set
# above if necessary for unique sockets and such.
#
# message is no longer started with -internet just because there is a
# Sessions file, since datastore client sessions do not need to listen
# on an internet port.  It is now set explicitly by adding the -internet
# option to ZEB_INTERNET, which can be also set per session in proj_env.
# Not using -internet for client sessions allows multiple anonymous (-unique)
# sessions to run on a single host without explicitly assigning them
# unique ports in the Sessions file.
# 
	if (! $?ZEB_MESSAGE) then
		set sessions=""
		set name="-session $SESSION"
		if (-f Sessions) set sessions="-file Sessions"
		setenv ZEB_MESSAGE \
		"$ZEB_TOPDIR/bin/message $name $sessions $ZEB_INTERNET"
	endif
	if (! $?ZEB_EVENTLOGGER) setenv ZEB_EVENTLOGGER \
					 $ZEB_TOPDIR/bin/EventLogger
	if (! $?ZEB_TIMER) setenv ZEB_TIMER $ZEB_TOPDIR/bin/timer
	if (! $?ZEB_DSDAEMON) setenv ZEB_DSDAEMON $ZEB_TOPDIR/bin/dsDaemon
	if (! $?ZEB_DM) setenv ZEB_DM $ZEB_TOPDIR/bin/dm

#
# Start core processes.  Message is started in foreground now; we'll wait
# for it to background itself.  Use eval, though, in case someone wants to
# redefine it (such as to a debugger or testcenter) and use an explicit '&'.
#
	echo 'Starting core zebra processes: '
	echo '	message daemon'
	eval "$ZEB_MESSAGE"
	sleep 2

	echo '	event logger '
	eval "$ZEB_EVENTLOGGER &"
	sleep 2

	echo '	timer '
	eval "$ZEB_TIMER &"
	sleep 2
#
# Now we need to run the per-project startup file.  Things are running so
# this file can actually start processes and zebra clients (but none that
# need the datastore daemon, unless DS_DAEMON_HOST is set).
#
	if ( -f proj_startup ) source proj_startup
#
# Fire off the data store daemon and the display manager.  Daemon only if
# there is not a pointer to a remote host, however.
#
	if ($?DS_DAEMON_HOST) then
		echo '	(DS Daemon running on' $DS_DAEMON_HOST ')'
	else
		echo '	datastore '
		eval "$ZEB_DSDAEMON ds.config &"
	endif
#
# If they only wanted a baseline datastore running, we quit here
#
	if ( $dsonly && $execshell ) goto start_shell
	if ( $dsonly ) then
		echo "Datastore running..."
		exit 0
	endif
	sleep 5

#
# If a display manager is already running, start the second one in
# multiple mode.
#
start_dm:
	set multiple=""
	if ( ! $?DEFAULT_CONFIG ) setenv DEFAULT_CONFIG empty
	if ( ! $?ZEB_DM_CONFIG ) setenv ZEB_DM_CONFIG dm.config
	mstatus | grep Displaymgr > /dev/null
	if ( $status == 0) set multiple="-multiple -name Dmgr-$$"
	eval $ZEB_DM $multiple $ZEB_DM_CONFIG
#
# Maybe we shut down.
#
	if ( ! $?PRESERVE_ZEB ) zstop
	if ( ! $execshell ) exit 0
#
# Fall through on -shell option
#
start_shell:
	if ( ! $?ZEB_SHELL ) then
		if ( $?SHELL ) then
			setenv ZEB_SHELL "$SHELL"
		else
			setenv ZEB_SHELL 'csh -f'
		endif
	endif
	set path=(. $path)
	exec $ZEB_SHELL
