OBJS = readtape.o ebctoascii.o makeimage3.o filehead.o  navigate.o
CFLAGS = -O -q

doit: $(OBJS)
	cc $(OBJS) -o image

filehead.o : filehead.c filehead.h
	cc -c $(CFLAGS) filehead.c

navigate.o : navigate.c filehead.h
	cc -c $(CFLAGS) navigate.c

makeimage3.o : makeimage3.c
	cc -c $(CFLAGS) makeimage3.c
