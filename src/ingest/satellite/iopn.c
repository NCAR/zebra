#include <stdio.h>
#include <fcntl.h>

#if defined(sun)
      iopn_(cfile,length)
#else
      iopn(cfile,length)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 IOPN.C 21-Sep-90,7:52:26,`SMG' initial release                          */
/* 2 IOPN.C 11-Feb-91,14:58:12,`SMG' add conditional compile option          */
/* 3 IOPN.C 16-Feb-92,14:50:48,`USER' Released for McIDAS-X Only             */
/* 4 IOPN.C 18-May-92,14:58:36,`SUEG' change to open as RO if file is RO     */
/* 5 IOPN.C 29-May-92,16:02:34,`USER' Released for McIDAS-X Only             */
/**** McIDAS-AIX Revision History *** */
char cfile[];

{
    long int opret;
    ffiletoc(cfile,length);
    opret=open(cfile,O_RDWR|O_CREAT,0666);
    if (opret<0) opret=open(cfile,O_RDONLY);
    if (opret<0) return(-1);
    return (opret);
}

