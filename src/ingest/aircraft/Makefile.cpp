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
MFVERSION="$Id: Makefile.cpp,v 1.1 1991-09-16 22:47:21 burghart Exp $"

# ifdef sun
/*
 * Sun options
 */
CC=gcc
CFLAGS=  -g -O -I/fcc/include -I/rdss/include
LIBS=/fcc/lib/libfcc.a -lnetcdf -lrdss -lXaw -lXt -lXmu -lXext -lX11 -ltermcap -lm
# endif


BINDIR=../../bin
LIBDIR=../../lib
HDIR=../../include

OBJS = ac_ingest.o
OBJS2 = ac_status.o

all:	ac_ingest ac_status ac_ingest.lf ac_status.lf 

install:	ac_ingest ac_ingest.lf ac_status ac_status.lf
	install -c ac_ingest $(BINDIR)
	install -c ac_status $(BINDIR)
	install -c -m 0444 ac_ingest.lf $(LIBDIR)
	install -c -m 0444 ac_status.lf $(LIBDIR)

include:

ac_ingest:	$(OBJS)
	$(CC) $(CFLAGS) -o ac_ingest $(OBJS) $(LIBS)

ac_status:	$(OBJS2)
	$(CC) $(CFLAGS) -o ac_status $(OBJS2) $(LIBS)

ac_ingest.lf: ac_ingest.state
	uic < make-lf

ac_status.lf: ac_status.state
	uic < make-sw-lf

clean:
	rm -f *~ ac_ingest *.o Makefile.bak

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
