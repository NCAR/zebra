/* $Id: Makefile.cpp,v 1.3 1991-11-21 21:35:14 corbet Exp $ */
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

CC=gcc
CFLAGS=-g -O -I$(FCCINC) -I$(RDSSINC) -DSHM
LIBS=ZebLibrary -lrdss -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext \
	-lX11 -lm


OBJS = Optimizer.o Bitmaps.o Boundary.o CommandWidget.o GenScan.o \
	LeftRightButtons.o MainWidget.o RadarWidget.o ScanOptions.o \
	SendWidget.o

all:	Optimizer Optimizer.lf

install:	Optimizer Optimizer.lf
	install -c Optimizer D_BINDIR
	install -c Optimizer.lf D_LIBDIR

test: $(OBJS)
	rm -f Optest
	$(CC) $(CFLAGS) -o Optest $(OBJS) $(LIBS)

Optimizer:	$(OBJS)
	rm -f Optimizer
	$(CC) $(CFLAGS) -o Optimizer $(OBJS) $(LIBS)

lf:	Optimizer.lf

Optimizer.lf: Optimizer.state keywords.h
	@ cc -P Optimizer.state
	uic < make-lf
	@ rm -f Optimizer.i

clean:
	rm -f *~ Optimizer *.o Makefile.bak

include:

Makefile:	mf

mf:
	mv Makefile Makefile~
	cp Makefile.cpp Makefile.c
	echo "# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD" > Makefile
	cc -E -DMAKING_MAKEFILE Makefile.c | cat -s >> Makefile
	rm -f Makefile.c
	make depend

depend:
	makedepend $(CFLAGS) *.c

