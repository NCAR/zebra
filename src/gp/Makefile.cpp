/* $Id: Makefile.cpp,v 1.7 1991-11-22 20:54:03 kris Exp $ */
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

# include "../include/config.h"


# ifdef sun
/*
 * Sun options
 */
CC=CCompiler
CFLAGS=CCOptions -I$(FCCINC) -I$(RDSSINC) -DSHM
/* CFLAGS=-g -O -I/fcc/include -I/rdss/include -DTIMING -DSHM */
FFLAGS=-g 
LIBS=ZebLibrary -L/usr/lang/SC0.0 -lF77 -lV77 -lrdss -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext -lX11 -lm
# endif

# if OPENWIN
OBJS =  TimeSeries.o ColorTable.o EventQueue.o LLEvent.o PlotControl.o \
	PlotExec.o UserEvent.o GraphicsW.o Contour.o FillContour.o \
	DrawText.o RasterPlot.o VectorGrid.o derive.o \
	Lightning.o Track.o GridAccess.o Overlay.o  PositionWidget.o\
	FrameCache.o MovieControl.o rgrid.o cfit.o AltControl.o \
	Icons.o Skewt.o RBand.o Annotate.o XSection.o LimitWidgets.o \
	ConstAltPlot.o Utilities.o DataMenu.o malloc.o \
	XYGraph.o PlotPrim.o LayoutControl.o AxisControl.o Label.o
# else
OBJS =  TimeSeries.o ColorTable.o EventQueue.o LLEvent.o PlotControl.o \
	PlotExec.o UserEvent.o GraphicsW.o Contour.o FillContour.o \
	DrawText.o RasterPlot.o VectorGrid.o derive.o \
	Lightning.o Track.o GridAccess.o Overlay.o  PositionWidget.o\
	FrameCache.o MovieControl.o rgrid.o cfit.o AltControl.o \
	Icons.o Skewt.o RBand.o Annotate.o XSection.o LimitWidgets.o \
	ConstAltPlot.o Utilities.o DataMenu.o malloc.o \
	XYGraph.o PlotPrim.o LayoutControl.o AxisControl.o
# endif

all:	gp

saber:	$(OBJS)
	# setopt ansi
	# load $(CFLAGS) GraphProc.o
	# load $(CFLAGS) $(OBJS) -Bstatic $(LIBS) /locallib/gcc-gnulib
	# link

install:	gp graphproc.lf include
	ar ruv D_LIBDIR/libfcc.a GraphicsW.o
	ranlib D_LIBDIR/libfcc.a
	install -c gp D_BINDIR
	install -c -m 0444 graphproc.lf D_LIBDIR

include:
	install -c -m 0444 GraphicsW.h D_FCCINC
	install -c -m 0444 GraphicsWP.h D_FCCINC

install.gp:	gp
	install -c gp D_BINDIR

gp:	GraphProc.o $(OBJS)
	rm -f gp
	$(CC) $(CFLAGS) -o gp GraphProc.o $(OBJS) $(LIBS)


gtest:	gtest.o $(OBJS)
	$(CC) $(CFLAGS) -o gtest gtest.o $(OBJS) $(LIBS)

graphproc.lf: GraphProc.state
	uic <make-lf

clean:
	rm -f *~ gp *.o dm.lf Makefile.bak

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

