MFVERSION="$Id: Makefile.cpp,v 1.5 1991-11-22 20:51:48 kris Exp $"

# include "../include/config.h"

# ifdef sun
/*
 * Sun options
 */
CC=CCompiler
CFLAGS= CCOptions -I$(FCCINC) -I$(RDSSINC)
LIBS=ZebLibrary -lrdss -ltermcap -lnetcdf -lm
XLIBS=-lXaw -lXmu -lXt -lXext -lX11
# endif



DSOBJS= Daemon.o d_SharedMemory.o d_DataTables.o d_Config.o d_Notify.o
OBJS = Appl.o SharedMemory.o DataFileAccess.o DFA_NetCDF.o GetList.o \
	DFA_Boundary.o DFA_Raster.o
SRCS = Appl.c SharedMemory.c DataFileAccess.c DFA_NetCDF.c GetList.c \
	DFA_Boundary.c DFA_Raster.c

# if BUILD_NETXFR
all:	dsDaemon dsDaemon.lf $(OBJS) dsdump dsdelete prt_Notify \
		NetXfr NetXfr.lf Archiver LastData dsdwidget

install:	dsDaemon dsDaemon.lf $(OBJS) dsdelete include prt_Notify \
		NetXfr NetXfr.lf dsdump Archiver LastData dsdwidget
	install -c dsDaemon D_BINDIR
	install -c Archiver D_BINDIR
	install -c LastData D_BINDIR
	install -c -m 04555 -o root NetXfr D_BINDIR
	install -c -s dsdelete D_BINDIR
	install -c -s prt_Notify D_BINDIR
	install -c -s dsdump D_BINDIR
	install -c -s dsdwidget D_BINDIR
	install -c -m 0444 dsDaemon.lf D_LIBDIR
	install -c -m 0444 NetXfr.lf D_LIBDIR
	ar ruv ZebLibrary $(OBJS)
	ranlib ZebLibrary

# else
all:	dsDaemon dsDaemon.lf $(OBJS) dsdump dsdelete prt_Notify \
		Archiver LastData dsdwidget

install:	dsDaemon dsDaemon.lf $(OBJS) dsdelete include prt_Notify \
		dsdump Archiver LastData dsdwidget
	install -c dsDaemon D_BINDIR
	install -c Archiver D_BINDIR
	install -c LastData D_BINDIR
	install -c -s dsdelete D_BINDIR
	install -c -s prt_Notify D_BINDIR
	install -c -s dsdump D_BINDIR
	install -c -s dsdwidget D_BINDIR
	install -c -m 0444 dsDaemon.lf D_LIBDIR
	ar ruv ZebLibrary $(OBJS)
	ranlib ZebLibrary

# endif

include:
	install -c -m 0444 DataStore.h D_FCCINC

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

/*
 * Saber stuff.  This is a bit complicated, depending on what you are
 * trying to debug.  The saber "#" construct makes it through the Sun
 * preprocessor; others may give trouble.
 */
saber_lib:
	#setopt ansi
	#load $(CFLAGS) $(SRCS)
	#load -Bstatic $(LIBS) /locallib/gcc-gnulib

saber_pn: saber_lib
	#load $(CFLAGS) prt_Notify.c d_Notify.c

clean:
	rm -f *~ dsDaemon LastData dsdump dsdwidget dsdelete NetXfr 
	rm -f prt_Notify core notify dsDaemon.lf *.o Makefile.bak

Makefile: mf

mf:
	mv Makefile Makefile~
	cp Makefile.cpp Makefile.c
	echo "# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD" > Makefile
	cc -E -DMAKING_MAKEFILE Makefile.c | cat -s >> Makefile
	rm -f Makefile.c
	make depend

depend:
	makedepend $(CFLAGS) *.c
