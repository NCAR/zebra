#
# Build the derivation module.
#
CFLAGS = -g -I/usr/local/include/python1.5
MODULES = wdermodule.so ncmodule.so

all:	$(MODULES)

wdermodule.so:	wdermodule.o
	ld -G -o wdermodule.so wdermodule.o 

ncmodule.so:	ncmodule.o
	ld -G -o ncmodule.so ncmodule.o -L/usr/local/lib -lnetcdf -ludunits

clean:
	rm -f core *~ *.o $(MODULES) *.pyc
