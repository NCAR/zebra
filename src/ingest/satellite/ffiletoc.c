#include <stdio.h>
#include <string.h>

  void ffiletoc(char [],int);

  void ffiletoc(char fname[],int length)

/**** McIDAS-AIX Revision History *** */
/* 1 FFILETOC.C 26-Sep-90,15:45:10,`SMG' Initial release                     */
/* 2 FFILETOC.C 28-Sep-90,13:20:36,`SMG' change *length to length            */
/* 3 FFILETOC.C 11-Feb-91,14:58:06,`SMG' add conditional compile option      */
/* 4 FFILETOC.C 6-Mar-91,8:57:40,`SMG' remove conditional compile stuff      */
/* 5 FFILETOC.C 6-Mar-91,8:59:22,`SMG' released                              */
/* 6 FFILETOC.C 8-Jul-91,15:35:36,`SUEG' conform to ANSI standards           */
/* 7 FFILETOC.C 9-Jul-91,13:41:58,`SUEG' resequence                          */
/* 8 FFILETOC.C 16-Feb-92,14:31:24,`USER' Released for McIDAS-X Only         */
/**** McIDAS-AIX Revision History *** */
/*   convert fortran character string to C string */
{
    char cfile[256];
    int len;

    len = length;
    strncpy(cfile,fname,len);
    cfile[len] = 0;
    len = len-1;
    while ((cfile[len] == ' ') && (len > 0)){
        cfile[len] = 0;
        len = len - 1 ;
    }
    strcpy(fname,cfile);
}








