MFVERSION="$Id: Makefile.cpp,v 1.1 1991-09-18 16:50:04 martin Exp $"

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS= -g -O -I/fcc/include -I/rdss/include
LIBS=../lib/libfcc.a -lrdss /rdss/pam/cfg/src/libcfg.a -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext -lX11 -lm /rdss/suds/libsuds.a
# endif


BINDIR=../bin
LIBDIR=../lib
HDIR=../include

all:	class_ingest

install:	class_ingest
	install -c pam_ingest $(BINDIR)

include:

class_ingest:	class_ingest.o
	$(CC) $(CFLAGS) -o class_ingest class_ingest.o $(LIBS)

clean:
	rm -f *~ *.o class_ingest Makefile.bak

Makefile: mf

mf:
	mv Makefile Makefile~
	cp Makefile.cpp Makefile.c
	echo "# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD" > Makefile
	cc -E Makefile.c >> Makefile
	rm -f Makefile.c
	make depend

depend:
	makedepend $(CFLAGS) *.c
