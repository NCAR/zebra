#include <stdio.h>

#if defined(UNDERSCORE)
      long int leof_(short int *);
#else
      long int leof(short int *);
#endif
#if defined(UNDERSCORE)
      long int leof_(short int *fileid)
#else
      long int leof(short int *fileid)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 LEOF.C 18-Sep-90,12:48:32,`SMG' initial release                         */
/* 2 LEOF.C 11-Feb-91,14:58:14,`SMG' add conditional compile option          */
/* 3 LEOF.C 9-Jul-91,15:02:26,`SUEG' conform to ANSI standards               */
/* 4 LEOF.C 16-Feb-92,14:50:40,`USER' Released for McIDAS-X Only             */
/**** McIDAS-AIX Revision History *** */

{
    long where,lseek();
    where = lseek(*fileid,0L,2);
    return(where);
}

