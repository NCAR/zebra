#include <stdio.h>


#if defined(sun)
    short int dw_ (short int *, long int [], long int *, long int *);
#else
    short int dw (short int *, long int [], long int *, long int *);
#endif

#if defined(sun)
    short int dw_ (short int *fileid, long int ibuf[], long int *nb, long int *retstat)
#else
    short int dw (short int *fileid, long int ibuf[], long int *nb, long int *retstat)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 DW.C 18-Sep-90,12:52:08,`SMG' initial release                           */
/* 2 DW.C 11-Feb-91,14:55:26,`SMG' add conditional compile option            */
/* 3 DW.C 8-Jul-91,15:07:32,`SUEG' conform to ANSI standards                 */
/* 4 DW.C 9-Jul-91,13:41:08,`SUEG' resequence                                */
/* 5 DW.C 16-Feb-92,14:36:34,`USER' Released for McIDAS-X Only               */
/**** McIDAS-AIX Revision History *** */

/*short int *fileid;
long int *nb, ibuf[], *retstat;*/
{
    long int nbytes,stat;
    nbytes = *nb;
    stat = write(*fileid,ibuf,nbytes);
    *retstat = stat;
    if (stat >0) return(0);
    return((short)stat);
}
