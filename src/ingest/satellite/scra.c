#include <stdio.h>

#if defined(sun)
     void scra_(short *, long *);
#else
     void scra(short *, long *);
#endif

#if defined(sun)
     void scra_(short *fileid, long *off)
#else
     void scra(short *fileid, long *off)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 SCRA.C 18-Sep-90,12:44:04,`SMG' initial release                         */
/* 2 SCRA.C 11-Feb-91,14:58:10,`SMG' add conditional compile option          */
/* 3 SCRA.C 11-Jul-91,8:33:36,`SUEG' conform to ANSI standards               */
/* 4 SCRA.C 16-Feb-92,14:51:22,`USER' Released for McIDAS-X Only             */
/**** McIDAS-AIX Revision History *** */
{
    long int where,offset;
    offset = *off;
    where= lseek(*fileid,offset,0);
}

