#if defined(sun)    
    void movbmem_(long *, long *, long *, long *);
    void movbmem_(long *num, long *inaddr, long *outaddr, long *offset)
#else
    void movbmem(long *, long *, long *, long *);
    void movbmem(long *num, long *inaddr, long *outaddr, long *offset)
#endif

/* *** McIDAS-AIX Revision History *** */
/* *** McIDAS-AIX Revision History *** */
/* Move num bytes from memory to memory (inaddr-> outaddr), with offset */
/*    long int *num, *offset;
    long int *inaddr;
    long int *outaddr; */
    {
        long int newaddr;
        newaddr = *outaddr + *offset;
        memcpy(newaddr , *inaddr, *num);
    }
