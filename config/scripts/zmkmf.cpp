XCOMM!/bin/sh

XCOMM
XCOMM generate a Makefile from an Imakefile from inside or outside the sources
XCOMM
XCOMM This is borrowed from the Zeb distribution and modified to refer to
XCOMM RDSS Libraries where appropriate.  Functionality remains the same.
XCOMM
XCOMM $Id: zmkmf.cpp,v 1.1 1993-07-26 17:06:07 case Exp $

usage=\
"usage:  $0 [-a] top_of_rdss_source current_directory
   Directory paths can be relative or absolute.  Relative paths are 
   recommended if inside the RDSS Libraries distribution tree, absolute paths
   should be used if outside the RDSS Libraries distribution tree.
 Ex: zmkmf ./../.. ./ingest/ingestor"

topdir=
curdir=.
do_all=

case "$1" in
-a)
    do_all="yes"
    shift
    ;;
esac

case $# in 
    2) topdir=$1  curdir=$2 ;;
    *) echo "$usage" 1>&2; exit 1 ;;
esac

case "$topdir" in
    -*) echo "$usage" 1>&2; exit 1 ;;
esac

if [ -f Makefile ]; then 
    echo mv Makefile Makefile.bak
    mv Makefile Makefile.bak
fi

args="-I$topdir/config -I$topdir/imake -DTOPDIR=$topdir -DCURDIR=$curdir"

echo imake $args
case "$do_all" in
yes)
    imake $args && 
    echo "make Makefiles" &&
    make Makefiles &&
    echo "make includes" &&
    make includes &&
    echo "make depend" &&
    make depend
    ;;
*)
    imake $args
    ;;
esac
