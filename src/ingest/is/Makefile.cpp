MFVERSION="$Id: Makefile.cpp,v 1.3 1991-10-11 15:42:12 martin Exp $"

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS= -g -O -I$(ZEBHOME)/fcc/include -I$(ZEBHOME)/rdss/include
LIBS=$(ZEBHOME)/fcc/lib/libfcc.a $(ZEBHOME)/rdss/suds/libsuds.a -lrdss -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext -lX11 -lm 
# endif


BINDIR=../bin
LIBDIR=../lib
HDIR=../include

all:	is is.lf class_ingest prof_ingest testOrg1dGrid

install: class_ingest prof_ingest is is.lf
	install -c class_ingest $(BINDIR)
	install -c prof_ingest $(BINDIR)
	install -c is $(BINDIR)
	install -c is.lf $(BINDIR)

include:

class_ingest:	class_ingest.o
	$(CC) $(CFLAGS) -o class_ingest class_ingest.o $(LIBS)

prof_ingest:	prof_ingest.o
	$(CC) $(CFLAGS) -o prof_ingest prof_ingest.o $(LIBS)

testOrg1dGrid: testOrg1dGrid.o
	$(CC) $(CFLAGS) -o testOrg1dGrid testOrg1dGrid.o $(LIBS)

is:	is.o
	$(CC) $(CFLAGS) -o is is.o $(LIBS)

is.lf:	is.state
	uic < make-lf

clean:
	rm -f *~ *.o class_ingest prof_ingest testOrg1dGrid Makefile.bak *.BAK core \
	is is.lf

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
