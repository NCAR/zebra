XCOMM!/bin/sh

XCOMM
XCOMM generate a Makefile from an Imakefile from inside or outside the sources
XCOMM 
XCOMM $Id: zmkmf.cpp,v 1.3 1994-05-25 16:56:05 burghart Exp $

usage=\
"usage:  $0 [-a] top_of_zeb_source current_directory [imake_opt ...]
   Directory paths can be relative or absolute.  Relative paths are 
   recommended if inside the Zeb distribution tree, absolute paths
   should be used if outside the Zeb distribution tree.
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

if (test $# -ge 2) then
    topdir=$1
    curdir=$2
    shift 2
else
    echo "$usage" 1>&2
    exit 1
fi

case "$topdir" in
    -*) echo "$usage" 1>&2; exit 1 ;;
esac

if [ -f Makefile ]; then 
    echo mv Makefile Makefile.bak
    mv Makefile Makefile.bak
fi

args="-I$topdir/config -I$topdir/imake -DTOPDIR=$topdir -DCURDIR=$curdir $*"

echo $topdir/imake/imake $args
case "$do_all" in
yes)
    $topdir/imake/imake $args && 
    echo "make Makefiles" &&
    make Makefiles &&
    echo "make includes" &&
    make includes &&
    echo "make depend" &&
    make depend
    ;;
*)
    $topdir/imake/imake $args
    ;;
esac
