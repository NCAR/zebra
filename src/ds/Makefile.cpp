MFVERSION="$Id: Makefile.cpp,v 1.3 1991-09-26 22:50:31 gracio Exp $"

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS= -g -O -I$(ZEBHOME)/fcc/include -I$(ZEBHOME)/rdss/include
LIBS=../lib/libfcc.a -lrdss -ltermcap -lnetcdf -lm
XLIBS=-lXaw -lXmu -lXt -lXext -lX11
# endif


BINDIR=../bin
LIBDIR=../lib
HDIR=../include

DSOBJS= Daemon.o d_SharedMemory.o d_DataTables.o d_Config.o d_Notify.o
OBJS = Appl.o SharedMemory.o DataFileAccess.o DFA_NetCDF.o GetList.o \
	DFA_Boundary.o DFA_Raster.o

all:	dsDaemon dsDaemon.lf $(OBJS) dsdump dsdelete prt_Notify \
		NetXfr NetXfr.lf Archiver LastData dsdwidget

install:	dsDaemon dsDaemon.lf $(OBJS) dsdelete include prt_Notify \
		NetXfr NetXfr.lf dsdump Archiver LastData dsdwidget
	install -c dsDaemon $(BINDIR)
	install -c Archiver $(BINDIR)
	install -c LastData $(BINDIR)
	install -c -m 04555 -o root NetXfr $(BINDIR)
	install -c -s dsdelete $(BINDIR)
	install -c -s prt_Notify $(BINDIR)
	install -c -s dsdump $(BINDIR)
	install -c -s dsdwidget $(BINDIR)
	install -c -m 0444 dsDaemon.lf $(LIBDIR)
	install -c -m 0444 NetXfr.lf $(LIBDIR)
	ar ruv $(LIBDIR)/libfcc.a $(OBJS)
	ranlib $(LIBDIR)/libfcc.a

include:
	install -c -m 0444 DataStore.h $(HDIR)

dsDaemon:	$(DSOBJS) $(OBJS)
	$(CC) $(CFLAGS) -o dsDaemon $(DSOBJS) $(OBJS) $(LIBS) $(XLIBS)

dsDaemon.lf:	Daemon.state
	uic < make-lf

NetXfr.lf:	NetXfr.state
	uic < make-nx-lf

dsdump:	dsdump.o $(OBJS)
	$(CC) $(CFLAGS) -o dsdump dsdump.o $(OBJS) $(LIBS)

dsdwidget:	dsdwidget.o $(OBJS)
	$(CC) $(CFLAGS) -o dsdwidget dsdwidget.o $(OBJS) $(LIBS) $(XLIBS)

LastData:	LastData.o $(OBJS)
	$(CC) $(CFLAGS) -o LastData LastData.o $(OBJS) $(LIBS) $(XLIBS)

prt_Notify:	prt_Notify.o d_Notify.o $(OBJS)
	$(CC) $(CFLAGS) -o prt_Notify prt_Notify.o d_Notify.o $(OBJS) $(LIBS)

add:	add.o $(OBJS)
	$(CC) $(CFLAGS) -o add add.o $(OBJS) $(LIBS)

Archiver:	Archiver.o $(OBJS)
	$(CC) $(CFLAGS) -o Archiver Archiver.o $(OBJS) $(LIBS) $(XLIBS)

NetXfr:	NetXfr.o nx_BCast.o nx_PktGrabber.o nx_DirImage.o $(OBJS)
	$(CC) $(CFLAGS) -o NetXfr NetXfr.o nx_BCast.o nx_PktGrabber.o \
			nx_DirImage.o $(OBJS) $(LIBS) $(XLIBS);

notify:	notify.o $(OBJS)
	$(CC) $(CFLAGS) -o notify notify.o $(OBJS) $(LIBS)

dsdelete:	dsdelete.o $(OBJS)
	$(CC) $(CFLAGS) -o dsdelete dsdelete.o $(OBJS) $(LIBS)

clean:
	rm -f *~ dsDaemon dsdump dsdwidget dsdelete NetXfr prt_Notify core notify dsDaemon.lf *.o Makefile.bak

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
