
#if defined(sun)
    void stc_(int * ,unsigned char [], int *);
#else
    void stc( int *, unsigned char [], int *);
#endif
#if defined(sun)
    void stc_(int *val, unsigned char buf[], int *offset)
#else
    void stc( int *val, unsigned char buf[], int *offset)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 STC.C 17-Sep-90,15:05:02,`SMG' Initial release                          */
/* 2 STC.C 11-Feb-91,14:55:42,`SMG' add conditional compile option           */
/* 3 STC.C 11-Jul-91,9:19:40,`SUEG' conform to ANSI standards                */
/* 4 STC.C 16-Feb-92,14:53:54,`USER' Released for McIDAS-X Only              */
/**** McIDAS-AIX Revision History *** */
   /* int *offset, *val;
    unsigned char buf[]; */
    {
        buf[*offset] = *val;
    }

