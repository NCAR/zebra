MFVERSION="$Id: Makefile.cpp,v 1.1 1991-08-05 16:58:33 corbet Exp $"

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS=-g -O -I/fcc/include -I/rdss/include
LIBS=../lib/libfcc.a -lrdss -ltermcap -lXaw -lXmu -lXt -lXext -lX11 -lm
# endif

# ifdef titan
/*
 * Ardent options
 */
CFLAGS = -g -DUNIX -43 -I/fcc/include -I/rdss/include
LIBS=../lib/libfcc.a -lrdss -ltermcap -L/usr/lib/X11 -lXaw -lXmu -lXt -lXext -lX11 -lm
# endif

BINDIR=../bin
LIBDIR=../lib
HDIR=../include

OBJS= dm.o dm_pd.o dm_ui.o dm_color.o DialBox.o dm_pick.o

all:	dm dm.lf

install:	dm dm.lf include
	install -c dm $(BINDIR)
	install -c dm.lf $(LIBDIR)

include:
	install -c -m 0444 dm.h $(HDIR);

dm:	$(OBJS)
	$(CC) $(CFLAGS) -o dm $(OBJS) $(LIBS)

dm.o:	dm.h

graphproc:	graphproc.c
	$(CC) $(CFLAGS) -o graphproc -I/rdss/include graphproc.c $(LIBS)

dm.lf:	dm.state
	uic < make-lf

clean:
	rm -f *~ dm graphproc *.o dm.lf

Makefile: Makefile.cpp
	mv Makefile Makefile~
	cp Makefile.cpp Makefile.c
	echo "# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD" > Makefile
	cc -E Makefile.c >> Makefile
	rm -f Makefile.c

coda:
	(cd /fcc; CODA=/fcc/.codarc; export CODA; coda dm)

depend:
	makedepend $(CFLAGS) *.c
