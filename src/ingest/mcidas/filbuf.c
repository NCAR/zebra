#include <string.h>

#if defined(UNDERSCORE)
    void filbuf_(long *nbytes, int *value, unsigned char buf[], int *offset)
#elif defined(AIXV3) || defined (__osf__)
    void filbuf(long *nbytes, int *value, unsigned char buf[], int *offset)
#else
     void fortran FILBUF(long *nbytes, int *value, unsigned char buf[], int *offset)
#endif

/**** McIDAS Revision History *** */
/* 1 FILBUF.C 17-Sep-90,14:24:18,`SMG' Initial release                       */
/* 2 FILBUF.C 12-Feb-91,13:35:52,`SMG' add conditional compile option        */
/* 3 FILBUF.C 12-Feb-91,13:35:52,`ERICN' released                            */
/* 4 FILBUF.C 12-Feb-91,13:35:52,`SUEG' resequence                           */
/* 5 FILBUF.C 17-Jul-91,8:30:16,`SUEG' conform to ANSI standards             */
/* 6 FILBUF.C 17-Jul-91,8:30:16,`TOMW' This is just a test                   */
/* 7 FILBUF.C 18-Jul-91,9:56:30,`SUEG' add function prototypes               */
/* 8 FILBUF.C 18-Jul-91,9:56:30,`USER' Released                              */
/**** McIDAS Revision History *** */
/* Fill buf with nbytes worth of value at offset */
/*    long int *nbytes;
    int *offset, *value;
    unsigned char buf[]; */
    {
        unsigned char cval;
        cval = *value;
        memset(&buf[*offset], cval, *nbytes);
    }
