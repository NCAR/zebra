MFVERSION="$Id: Makefile.cpp,v 1.4 1991-11-09 05:31:04 martin Exp $"

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS= -g -O -I$(ZEBHOME)/fcc/include -I$(ZEBHOME)/rdss/include
LIBS=$(ZEBHOME)/fcc/lib/libfcc.a $(ZEBHOME)/rdss/suds/libsuds.a -lrdss -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext -lX11 -lm 
# endif


BINDIR=../../bin
LIBDIR=../../lib
HDIR=../../include

all:	is is.lf

install:is is.lf
	install -c is $(BINDIR)
	install -c is.lf $(BINDIR)

include:

is:	is.o
	$(CC) $(CFLAGS) -o is is.o $(LIBS)

is.lf:	is.state
	uic < make-lf

clean:
	rm -f *~ *.o Makefile.bak *.BAK core \
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
