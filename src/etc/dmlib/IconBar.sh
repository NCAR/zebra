#! /bin/sh
# $Id: IconBar.sh,v 1.1 2000-12-05 19:07:36 granger Exp $
#
# This is not UI code but a shell script which generates UI code
# for an IconBar and writes it to stdout.  It is up to dm.config
# to run this script (thereby passing its environment variables
# as well) and 'read' the output.
#
# Many of the menus are boilerplate, so those are easy to generate...
#
# The iconbar is sort of a 'unification' of all of the parts of the
# project, some of which are common to all projects, some parts which need
# to be customized to the kinds of platforms.  So the iconbar defines some
# 'standard' menus and categories and writes the defaults into file parts
# which sub-modules can extend in UI.  The file parts follow a standard
# naming convention so that other modules can add their own menus to the
# final iconbar by creating a file of the correct convention.  The idea is
# that this script should not contain any instrument- or project-specific
# definitions.

# Define the directory to which our iconbar modules will go.  If we ever
# expect to generate iconbars simultaneously, then this may need to be a
# process-specific directory path passed into the submodules.  Perhaps this
# script should be setup to only run once to initialize the iconbar, since
# most cases it may not need to change again.

# Here is the naming scheme for iconbar pieces:
#
# 00 -- Reserved for IconBar.sh
# 10 -- Start of sub-module init files
#       10listmenu
#
# 20 -- Standard IconBar menus
#   20opening    Opening of iconbar definition
# 30 -- Sub-module iconbar menus
# 50 -- Closing of IconBar definition

# If the IconBar file already exists, don't regenerate it.
if [ -f IconBar ]; then
    echo "IconBar already exists, not regenerating..."
    exit 0
fi
echo "Regenerating IconBar..."

idir=/tmp/iconbar.$$
if [ ! -d $idir ]; then
    mkdir $idir || exit 1
fi
(sleep 300; rm -rf $idir) &

# Clean out our iconbar directory
rm -f $idir/*

modules="$*"

initfile=$idir/00init
dslistmenu=$idir/10dslist
opening=$idir/20opening
helpmenu=$idir/21help
toolmenu=$idir/22tools
dconfigmenu=$idir/24dconfig
overlaymenu=$idir/26overlays
closing=$idir/50closing

# First the opening, iconbar.init
cat > $initfile <<EOF
!================================================================
! This IconBar is generated automatically!  Any changes made to
! this file will be lost the next time the IconBar is regenerated!
!================================================================

message "Initializing IconBar..."
require "cfg-tools"
require "template-menu"

EOF

# ----------------------------------------------------------------
# platform data listing categories
#
cat > $dslistmenu <<EOF
define widget dslistings intmenu dsdwidget-listings
title 'Platform Data Listings'
line
entry 'List All Platforms' \
   'shell "dsdwidget -a -t All\ Platforms &"'
EOF

# ----------------------------------------------------------------
# IconBar definition opening
#
cat > $opening <<EOF
!
! Project iconbar generated `date` with these modules:
! $modules
!
define widget iconbar menubar "iconbar"
	noheader
EOF


# ----------------------------------------------------------------
# Next the Help Menu
cat > $helpmenu <<HELP
	menu help bitmap iconbar-help
	title 'Zebra Help Access'
	line
	entry 'Introduction' 'help intro'
	entry 'On-line Help Index' 'help index'
	entry 'Starting Zebra' 'help start'
	entry 'Shutting Down' 'help shut-down'
	entry 'Keys and Mouse Buttons' 'help keys'
	entry 'The Icon Bar' 'help iconbar'
	entry 'Editing Display Configurations' 'help editing-configs'
	line
	entry 'Horizontal plots' 'help horizontal-plots'
	entry 'Skew-t plots' 'help skewt'
	entry 'XYGraph plots' 'help xy-graphs'
	entry 'Cross section plots' 'help cross-section'
HELP

# ----------------------------------------------------------------
# Tools menu
#
# Limit the available tools in 'operator' mode?  This might have
# to be a case of the 'iss' sub-module overriding the default.
#
cat > $toolmenu <<'TOOLS'
	menu tools bitmap tools-big
	title 'Tools'
	line
	entry 'Time Control Tool...' 'popup time'
	submenu 'Platform Data Listings' dslistings
	entry 'Printing Tool...' 'require hardcopy-widget; popup hardcopy'
	entry 'Configuration Tool...' 'popup ConfigEdit'
	entry 'XTerm Shell Tool' 'shell "xterm -T ZebraShell -bg grey90 -fg purple -fn 8x13bold -e zstart -shell &"'
	entry 'Data Store Management' 'shell "dsmanage &"'
	line
	entry 'Begin new configuration...' 'popup newconfig'
	submenu 'Add new window' 'template-menu'
	entry 'Kill window by cursor' 'point-and-shoot'
	entry 'Add window named...' 'popup NewWindow'
	entry 'Kill window named...' 'popup ZorchWindow'
	line
	entry 'Save this configuration' 'cfgsave #dm$config'
	entry 'Save this configuration as...' 'popup SaveConfig'
	entry 'Restore configuration' 'cfg-restore'
	entry 'Reset all windows in configuration' 'cfg-reset'
!	entry 'Regenerate Iconbar' \'iconbar-redefine \"$modules\"\'
	line
	entry 'Event Logger' 'shell "EventLogger -w &"'
	line
	entry 'Shutdown Zebra' 'shutdown'
TOOLS

# ----------------------------------------------------------------
# Display config menu
#
cat > $dconfigmenu <<'CONFIGS'
	menu configs bitmap configs
	title 'Display configurations'
	line
	entry 'Empty screen' 'display empty' (dm$config = 'empty')
CONFIGS


# ----------------------------------------------------------------
# Static overlays menu
#
cat > $overlaymenu <<OVERLAYS
	menu 'overlays' bitmap overlays
	title 'Static Overlays'
	line
	entry 'US Map' \
	      'putc3 p_map platform us color brown icon map'
	entry 'World Map' \
	      'putc3 p_map platform world color brown icon map'
	line
	entry 'X-Y Grid (km)' \
		'putc3 p_kmgrid x-spacing 300 y-spacing 300 color cyan'
	entry 'Lat/Lon Grid' \
		'putc3 p_llgrid x-spacing 300 y-spacing 300 color cyan'
	entry 'Range Rings' 'putc1 p_rings platform origin'
OVERLAYS

# ================================================================
# Run the sub-modules: first run the explicit modules, then run
# any requires.
# ================================================================

# This is essentially a 'require' written in sh.
# require module module-file
require()
{
    if test -f $2 && egrep $1 $2 >/dev/null 2>&1 ; then
	echo "$1 already loaded." 1>&2
    else
	echo $1 >> $2
	test -x config/$1/dm-$1 && config/$1/dm-$1 $idir
    fi	
}

# Setup our initial list of requires from the modules on the command line
requires=$idir/requires
cat < /dev/null > $requires
for mod in $modules ; do
    echo $mod >> $requires
done

# Then loop over the contents of the requires file until all have
# have been done.
mod=none
while [ $mod != DONE ]; do

    # Reset the list every time in case the previously called module
    # added another module to the requires list.
    set `cat $requires` DONE
    for mod in $* ; do
	if [ $mod = DONE ]; then 
	    break
	fi
	if [ ! -f $idir/done-$mod ]; then
	    config/$mod/dm-$mod $idir config/$mod > $idir/done-$mod 2>&1
	    break
	fi
    done

done

# Close any open menus
echo endmenu >> $dslistmenu
echo endmenu >> $helpmenu
echo endmenu >> $toolmenu
echo endmenu >> $dconfigmenu
echo endmenu >> $overlaymenu

# Create the closing of the iconbar definition
echo enddef > $closing

# ================================================================
# Now collect all of our modules into a single iconbar definition.
# ================================================================
#

cat $idir/[0-9]* > IconBar
rm -rf $idir
