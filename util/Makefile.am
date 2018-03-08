AUTOMAKE_OPTIONS = foreign

lib_LIBRARIES = librdssutil.a

include_HEADERS = met_formulas.h

librdssutil_a_SOURCES = batch.c bfile.c bio.c cpanic.c disk.c fixdir.c \
	getgnum.c log_time.c met_formulas.c ntdsk_client.c parsetime.c \
	rtape_lib.c sprintrmt.c strcpyUC.c swap4.c tcpip.c vm.c \
	\
	met_formulas.h netdisk.h

DEFS = -DNETACCESS
