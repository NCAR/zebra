    movb_(num, inbuf, outbuf, offset)
/* *** PC-McIDAS Revision History *** */
/* *** PC-McIDAS Revision History *** */
/* Move num bytes from inbuf to outbuf (with offset) */
    long int *num, *offset;
    char outbuf[];
    char inbuf[];
    {
        long int off;
        off = *offset;
        memcpy(&outbuf[off] , inbuf, *num);
    }
