

#if defined(UNDERSCORE)
    long int ic_(unsigned char [], int *);
#else
    long int ic(unsigned char [] , int *);
#endif

#if defined(UNDERSCORE)
    long int ic_(unsigned char buf[], int *offset)
#else
    long int ic(unsigned char buf[] , int *offset)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 IC.C 18-Sep-90,9:36:38,`SMG' Initial release                            */
/* 2 IC.C 11-Feb-91,14:55:20,`SMG' add conditional compile option            */
/* 3 IC.C 8-Jul-91,16:44:56,`SUEG' conform to ANSI standards.                */
/* 4 IC.C 9-Jul-91,13:44:12,`SUEG' resequence                                */
/* 5 IC.C 21-Aug-91,10:18:42,`SUEG' change long to char                      */
/* 6 IC.C 16-Feb-92,14:49:46,`USER' Released for McIDAS-X Only               */
/**** McIDAS-AIX Revision History *** */
    {
        return(buf[*offset]);
    }

