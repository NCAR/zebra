MFVERSION="$Id: Makefile.cpp,v 1.7 1991-12-20 17:48:56 corbet Exp $"

# include "../include/config.h"

CC=CCompiler
CFLAGS= CCOptions IncludeDirs
LIBS=ZebLibrary MiscLibs CDFLibrary
XLIBS=XLibraries


/* DSOBJS = Daemon-specific object modules */
DSOBJS= Daemon.o d_SharedMemory.o d_DataTables.o d_Config.o d_Notify.o
/* OBJS, SRCS = normal DS library code */
OBJS = Appl.o SharedMemory.o DataFileAccess.o DFA_NetCDF.o GetList.o \
	DFA_Boundary.o DFA_Raster.o Fields.o
SRCS = Appl.c SharedMemory.c DataFileAccess.c DFA_NetCDF.c GetList.c \
	DFA_Boundary.c DFA_Raster.c Fields.c

/* DCOBJS, DCSRCS =  data chunk modules */
DCOBJS = ConvertDObj.o DataChunk.o dc_Boundary.o dc_IRGrid.o dc_Image.o \
	dc_MetData.o dc_RGrid.o dc_Scalar.o dc_Transp.o dc_Attr.o
DCSRCS = ConvertDObj.c DataChunk.c dc_Boundary.c dc_IRGrid.c dc_Image.c \
	dc_MetData.c dc_RGrid.c dc_Scalar.c dc_Transp.c dc_Attr.c


all::	dsDaemon dsDaemon.lf $(OBJS) $(DCOBJS) dsdump dsdelete prt_Notify \
	dsdwidget

install::	dsDaemon dsDaemon.lf $(OBJS) dsdelete include prt_Notify \
		dsdump dsdwidget $(DCOBJS)
	install -c dsDaemon D_BINDIR
	install -c -s dsdelete D_BINDIR
	install -c -s prt_Notify D_BINDIR
	install -c -s dsdump D_BINDIR
	install -c -s dsdwidget D_BINDIR
	install -c -m 0444 dsDaemon.lf D_LIBDIR
	ar ruv ZebLibrary $(OBJS) $(DCOBJS)
	ranlib ZebLibrary

# if RT_DS_TOOLS
all::	NetXfr Archiver LastData

install::	NetXfr NetXfr.lf Archiver LastData
	install -c -m 0444 NetXfr.lf D_LIBDIR
	install -c -m 04555 -o root NetXfr D_BINDIR
	install -c Archiver D_BINDIR
	install -c -s LastData D_BINDIR
	
# endif


include:
	install -c -m 0444 DataStore.h D_FCCINC
	install -c -m 0444 ds_fields.h D_FCCINC
	install -c -m 0444 DataChunk.h D_FCCINC

dctest:		dctest.o $(DCOBJS)
	$(CC) $(CFLAGS) -o dctest dctest.o $(DCOBJS) $(LIBS)

dsDaemon:	$(DSOBJS) $(OBJS)
	$(CC) $(CFLAGS) -o dsDaemon $(DSOBJS) $(OBJS) $(LIBS) $(XLIBS)

dsDaemon.lf:	Daemon.state
	uic < make-lf


dsdump:	dsdump.o $(OBJS)
	$(CC) $(CFLAGS) -o dsdump dsdump.o $(OBJS) $(LIBS)

dsdwidget:	dsdwidget.o $(OBJS)
	$(CC) $(CFLAGS) -o dsdwidget dsdwidget.o $(OBJS) $(LIBS) $(XLIBS)

prt_Notify:	prt_Notify.o d_Notify.o $(OBJS)
	$(CC) $(CFLAGS) -o prt_Notify prt_Notify.o d_Notify.o $(OBJS) $(LIBS)


# if RT_DS_TOOLS
NetXfr:	NetXfr.o nx_BCast.o nx_PktGrabber.o nx_DirImage.o $(OBJS)
	$(CC) $(CFLAGS) -o NetXfr NetXfr.o nx_BCast.o nx_PktGrabber.o \
			nx_DirImage.o $(OBJS) $(LIBS) $(XLIBS);

LastData:	LastData.o $(OBJS)
	$(CC) $(CFLAGS) -o LastData LastData.o $(OBJS) $(LIBS) $(XLIBS)

NetXfr.lf:	NetXfr.state
	uic < make-nx-lf

Archiver:	Archiver.o $(OBJS)
	$(CC) $(CFLAGS) -o Archiver Archiver.o $(OBJS) $(LIBS) $(XLIBS)
# endif

dsdelete:	dsdelete.o $(OBJS)
	$(CC) $(CFLAGS) -o dsdelete dsdelete.o $(OBJS) $(LIBS)

/*
 * Saber stuff.  This is a bit complicated, depending on what you are
 * trying to debug.  The saber "#" construct makes it through the Sun
 * preprocessor; others may give trouble.
 */
saber_lib:
	#setopt ansi
	#load $(CFLAGS) $(SRCS) $(DCOBJS)
	#load -Bstatic $(LIBS) /locallib/gcc-gnulib

saber_dsd: saber_lib
	#load $(CFLAGS) dsdump.c

saber_pn: saber_lib
	#load $(CFLAGS) prt_Notify.c d_Notify.c

saber_dct:
	#setopt ansi
	#load $(CFLAGS) dctest.c $(DCSRCS)
	#load -Bstatic $(LIBS) /locallib/gcc-gnulib


clean:
	rm -f *~ dsDaemon LastData dsdump dsdwidget dsdelete NetXfr 
	rm -f prt_Notify core notify rfdump Archiver *.lf *.o Makefile.bak

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
