#!/bin/csh -f
# 
# $Id: dbgstart.csh,v 3.1 1996-11-21 21:47:56 granger Exp $
#

#
# Figure out where we are.
#
	setenv ZEB_SOCKET /tmp/test.$$.socket
	setenv ZEB_TOPDIR ##TOPDIRHERE
	setenv DSDLOADFILE `pwd`
#
# Do everything possible to insure that we start clean.
#
	setenv XAPPLRESDIR ${ZEB_TOPDIR}/lib/resources
	setenv PATH .:$ZEB_TOPDIR/bin:${PATH}
	zstop
	rm -f $ZEB_SOCKET
#	tweakcolor red
	sleep 1
#
# The message system.  Nothing else runs without it.
#
	echo 'Starting MESSAGE...'
	setenv HOST `uname -n`
#	../msg/message.tc -n &
#	xgdb ../msg/message &
#	../msg/message -nofork -debug &
	message
#	sleep 2
#
# Data store processes.
#
	echo -n 'Starting the event logger...'
#	EventLogger -n -f $cwd/zeb.logfile &
	EventLogger -w -f $cwd/zeb.logfile &
#	../EventLogger/EventLogger.tc -w -l 255 -f $cwd/zeb.logfile &
	sleep 1
#
# Other basic utilities needed by the system.
#
	echo -n 'Starting the timer...'
#	../timer/timer.tc &
	timer &
	echo 'Starting a shell: datastore still needs to be started'
	tcsh -f
	zstop
