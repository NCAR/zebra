#include <stdio.h>

#if defined(UNDERSCORE)
   int delfil_ (char [],int );
   void ffiletoc(char [],int );
#else
   int delfil (char [], int );
   void ffiletoc(char[], int);
#endif
          

#if defined(UNDERSCORE)
   int delfil_ (char file[],int length)
#else
   int delfil (char file[], int length)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 DELFIL.C 26-Sep-90,15:57:32,`SMG' Initial release                       */
/* 2 DELFIL.C 28-Sep-90,13:25:06,`SMG' correct subroutine name               */
/* 3 DELFIL.C 2-Oct-90,15:09:48,`SMG' fix problems found in testing          */
/* 4 DELFIL.C 2-Oct-90,15:30:32,`SMG' return status from unlink              */
/* 5 DELFIL.C 11-Feb-91,14:55:30,`SMG' add conditional compile code          */
/* 6 DELFIL.C 8-Jul-91,14:16:20,`SUEG' conform to ANSI standards             */
/* 7 DELFIL.C 9-Jul-91,13:40:00,`SUEG' resequence                            */
/* 8 DELFIL.C 16-Feb-92,14:31:46,`USER' Released for McIDAS-X Only           */
/**** McIDAS-AIX Revision History *** */
/*char file[];
short int length;*/
{ 
    ffiletoc(file,length);
    return(unlink(file));
}
   
