CC = gcc
CFLAGS = -g -I/rdss/include -I/zeb/include
XLIBS = -lXaw -lXmu -lXt -lXext -lX11


all:	extractmap drawmap makehole

extractmap:	extractmap.o
	$(CC) ${CFLAGS} -o extractmap extractmap.o -lrdss

drawmap:	drawmap.o tablet.o GraphicsW.o
	$(CC) $(CFLAGS) -static -o drawmap drawmap.o tablet.o \
		GraphicsW.o -lrdss $(XLIBS) -ltermcap -lm

makehole:	makehole.o
	$(CC) $(CFLAGS) -o makehole makehole.o -lrdss $(XLIBS) -ltermcap -lm

clean:
	rm -f core *.o *~
