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
MFVERSION="$Id: Makefile.cpp,v 1.2 1992-04-28 21:48:53 granger Exp $"

# include "../include/config.h"


# ifdef sun
/*
 * Sun options
 */
CC=CCompiler
CFLAGS=CCOptions IncludeDirs
LIBS=ZebLibrary CDFLibrary MiscLibs 
# endif

all:	gprotocdf mudtocdf convmud

convmud:	convmud.o mudutil.o
	$(CC)	$(CFLAGS) -o convmud convmud.o mudutil.o $(LIBS)

gprotocdf:	gprotocdf.o
	$(CC) $(CFLAGS) -o gprotocdf gprotocdf.o $(LIBS)

mudtocdf:	mudtocdf.o mudras.o
	$(CC) $(CFLAGS) -o mudtocdf mudtocdf.o mudras.o FortranLibs $(LIBS) 

saber_mc:
	#setopt ansi
	#load $(CFLAGS) mudtocdf.c mudras.o -Bstatic FortranLibs $(LIBS)

saber_cm:
	#setopt ansi
	#load $(CFLAGS) convmud.c mudutil.c -Bstatic $(LIBS)

saber_gp:
	#setopt ansi
	#load $(CFLAGS) gprotocdf.c -Bstatic $(LIBS)

install: mudtocdf gprotocdf convmud
	install mudtocdf D_BINDIR
	install gprotocdf D_BINDIR
	install -c -s convmud D_BINDIR

clean:
	rm -f gprotocdf mudtocdf convmud *~ *.o core

Makefile: mf

mf:
	mv Makefile Makefile~
	cp Makefile.cpp Makefile.c
	echo "# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD" > Makefile
	cc -E Makefile.c -DMAKING_MAKEFILE | cat -s >> Makefile
	rm -f Makefile.c
	make depend

depend:
	makedepend $(CFLAGS) *.c
