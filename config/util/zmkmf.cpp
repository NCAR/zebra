XCOMM!/bin/sh

XCOMM
XCOMM generate a Makefile from an Imakefile from inside or outside the sources
XCOMM 
XCOMM $Id: zmkmf.cpp,v 1.5 1996-05-01 20:29:38 granger Exp $

usage=\
"usage:  $0 [-a] [top_of_zebra_source [current_directory]]
   Directory paths can be relative or absolute.  Relative paths are 
   recommended if inside the Zebra distribution tree, absolute paths
   should be used if outside the Zebra distribution tree.
 Ex: zmkmf ./../.. ./ingest/ingestor"

configdirspec=CONFIGDIRSPEC
imakebin=BINDIR
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
    0) ;;
    1) topdir=$1 ;;
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

if [ "$topdir" = "" ]; then
    imake=$imakebin/imake
    args="-DUseInstalledImake "$configdirspec
else
    imake=$topdir/config/imake/imake
    args="-I$topdir/config/project -I$topdir/config/cf -DTOPDIR=$topdir -DCURDIR=$curdir $*"
fi

echo $imake $args
case "$do_all" in
yes)
    $imake $args && 
    echo "make Makefiles" &&
    make Makefiles &&
    echo "make includes" &&
    make includes &&
    echo "make depend" &&
    make depend
    ;;
*)
    $imake $args
    ;;
esac
