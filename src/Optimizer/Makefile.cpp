/* $Id: Makefile.cpp,v 1.2 1991-09-26 16:28:59 gracio Exp $ */

CC=gcc
CFLAGS=-g -O -I$(ZEBHOME)/fcc/include -I$(ZEBHOME)/rdss/include -DSHM
LIBS=../lib/libfcc.a -lrdss -ltermcap -lnetcdf -lXaw -lXmu -lXt -lXext \
	-lX11 -lm

BINDIR=../bin
LIBDIR=../lib
HDIR=../include

OBJS = Optimizer.o Bitmaps.o Boundary.o CommandWidget.o GenScan.o \
	LeftRightButtons.o MainWidget.o RadarWidget.o ScanOptions.o \
	SendWidget.o

all:	Optimizer Optimizer.lf

install:	Optimizer Optimizer.lf
	install -c Optimizer $(BINDIR)
	install -c Optimizer.lf $(LIBDIR)

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
	cc -E Makefile.c >> Makefile
	rm -f Makefile.c
	make depend

depend:
	makedepend $(CFLAGS) *.c

