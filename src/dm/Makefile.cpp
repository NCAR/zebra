MFVERSION="$Id: Makefile.cpp,v 1.6 1992-02-07 21:03:48 corbet Exp $"
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

CC=CCompiler
CFLAGS=CCOptions IncludeDirs
LIBS=ZebLibrary MiscLibs XLibraries

OBJS= dm.o dm_pd.o dm_ui.o dm_color.o DialBox.o dm_pick.o dm_config.o

all:	dm dm.lf

install:	dm dm.lf include
	install -c dm D_BINDIR
	install -c -m 0444 dm.lf D_LIBDIR

include:
	HInstall (dm.h)
	/* install -c -m 0444 dm.h D_FCCINC */

dm:	$(OBJS)
	$(CC) $(CFLAGS) -o dm $(OBJS) $(LIBS)

dm.o:	dm.h

dm.lf:	dm.state dm.widgets
	uic < make-lf

saber:	$(OBJS)
	# setopt ansi
	# load $(CFLAGS) $(OBJS) -Bstatic $(LIBS) /locallib/gcc-gnulib
	# link

clean:
	rm -f *~ dm graphproc *.o dm.lf Makefile.bak

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
