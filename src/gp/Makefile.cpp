/* $Id: Makefile.cpp,v 1.2 1991-09-12 20:27:54 corbet Exp $ */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS=-g -O -I/fcc/include -I/rdss/include -DSHM
/* CFLAGS=-g -O -I/fcc/include -I/rdss/include -DTIMING -DSHM */
FFLAGS=-g 
LIBS=../lib/libfcc.a -L/usr/lang/SC0.0 -lF77 -lV77 -lrdss -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext -lX11 -lm
# endif

# ifdef titan
/*
 * Ardent options.  -l doesn't seem to work for the fortran libraries, for
 * whatever reason, so they get listed explicitly.
 *
 * NOOPTCF is CFLAGS for files that break under optimization.  Never
 * set it to higher than O1.  In fact, don't turn optimization at all.
 * Ever.  Bad juju.
 *
 * -O3 doesn't seem to help in any case.  Too many graphprocs running
 * infinite loops on the other processors.
 */
CFLAGS = -O2 -DUNIX -Duse_XB -43 -I/dt/X/mit -I/fcc/include -I/rdss/include
NOOPTCF = -g -DUNIX -Duse_XB -43 -I/dt/X/mit -I/fcc/include -I/rdss/include
F77FLAGS=-O2 -cpp
LIBS=../lib/libfcc.a -lrdss -ltermcap -lXd -lXB -L/usr/lib/X11 -lXaw \
	-lXmu -lXt -lXext -lX11 -ltermcap -lm /usr/lib/libmF77.a \
	/usr/lib/libibF77.a /usr/lib/libuF77.a
# endif

BINDIR=../bin
LIBDIR=../lib
HDIR=../include

OBJS =  TimeSeries.o ColorTable.o EventQueue.o LLEvent.o PlotControl.o \
	PlotExec.o UserEvent.o GraphicsW.o Contour.o FillContour.o \
	DrawText.o RasterPlot.o VectorGrid.o derive.o \
	Lightning.o Track.o GridAccess.o Overlay.o  PositionWidget.o\
	FrameCache.o MovieControl.o rgrid.o cfit.o AltControl.o \
	Icons.o Skewt.o RBand.o Annotate.o XSection.o LimitWidgets.o \
	ConstAltPlot.o Utilities.o DataMenu.o malloc.o

all:	gp

install:	gp graphproc.lf include
	ar ruv $(LIBDIR)/libfcc.a GraphicsW.o
	ranlib $(LIBDIR)/libfcc.a
	install -c gp $(BINDIR)
	install -c -m 0444 graphproc.lf $(LIBDIR)

include:
	install -c -m 0444 GraphicsW.h $(HDIR)
	install -c -m 0444 GraphicsWP.h $(HDIR)

install.gp:	gp
	install -c gp $(BINDIR)

gp:	GraphProc.o $(OBJS)
	rm -f gp
	$(CC) $(CFLAGS) -o gp GraphProc.o $(OBJS) $(LIBS)


gtest:	gtest.o $(OBJS)
	$(CC) $(CFLAGS) -o gtest gtest.o $(OBJS) $(LIBS)

graphproc.lf: GraphProc.state
	uic <make-lf

# ifdef titan
/*
 * These routines break under Ardent's optimization.  What a pain.
 */
Contour.o:	Contour.c
	$(CC) $(NOOPTCF) -c Contour.c

PlotExec.o:	PlotExec.c
	$(CC) $(NOOPTCF) -c PlotExec.c

FillContour.o:	FillContour.c
	$(CC) $(NOOPTCF) -c FillContour.c
# endif

/*
 * "create" the lower-case .f file if necessary.
 */
# ifdef titan:
rgrid.f:	rgrid.F
	rm -f rgrid.f
	ln -s rgrid.F rgrid.f

cfit.f:		cfit.F
	rm -f cfit.f
	ln -s cfit.F cfit.f
# endif

clean:
	rm -f *~ gp *.o dm.lf Makefile.bak

Makefile: mf


mf:
	mv Makefile Makefile~
	cp Makefile.cpp Makefile.c
	echo "# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD" > Makefile
	cc -E Makefile.c >> Makefile
	rm -f Makefile.c
	make depend

coda:
	(cd /fcc; CODA=/fcc/.codarc; export CODA; coda gp)

depend:
	makedepend $(CFLAGS) *.c

