#include <stdio.h>

#if defined(UNDERSCORE)
      void dc_ (short int *);
#else
      void dc (short int *);
#endif

#if defined(UNDERSCORE)
      void dc_ (short int *fileid)
#else
      void dc (short int *fileid)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 DC.C 18-Sep-90,12:53:08,`SMG' initial release                           */
/* 2 DC.C 11-Feb-91,14:58:12,`SMG' add conditional compile option            */
/* 3 DC.C 8-Jul-91,13:43:04,`SUEG' conform to ANSI standards                 */
/* 4 DC.C 9-Jul-91,13:39:42,`SUEG' resequence                                */
/* 5 DC.C 16-Feb-92,14:50:56,`USER' Released for McIDAS-X Only               */
/**** McIDAS-AIX Revision History *** */


{
    close(*fileid);
}
