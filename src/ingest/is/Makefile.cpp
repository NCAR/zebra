MFVERSION="$Id: Makefile.cpp,v 1.6 1992-01-17 16:35:22 martin Exp $"

# include "/zeb/include/config.h"

CC=CCompiler
CFLAGS=CCOptions IncludeDirs
LIBS=ZebLibrary MiscLibs XLibraries

ISS= ../..
ISSLIB = $(ISS)/lib
ISSINC = $(ISS)/include
ISSBIN = $(ISS)/bin

all:	is is.lf

install:is is.lf
	install -c is $(ISSBIN)
	install -c is.lf $(ISSBIN)

include:

is:	is.o
	$(CC) $(CFLAGS) -o is is.o $(LIBS)

is.lf:	is.state
	uic < make-lf

clean:
	rm -f *~ *.o Makefile.bak *.BAK core \
	is is.lf

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
