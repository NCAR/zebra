/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0BUF.C 19-Feb-96,15:15:20,`DWS' reglue: new file                       */
/* 2 M0BUF.C 20-Feb-96,12:10:24,`USER' Released                              */
/**** McIDAS Revision History *** */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "mcidasp.h"

/* private declarations */

struct _M0buf
{
	char *data;	/* holds the data */
	size_t alloc;	/* allocated size */
	size_t count;	/* logical size - excluding terminal NUL */
};


/*
*| Name:
*|	M0bufalloc - allocate a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	M0buf *
*|	M0bufalloc(void)
*|
*| Input:
*|	none
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	A pointer to a new buffer.
*|
*| Remarks:
*|	If memory allocation fails the process terminates.
*|
*| Categories:
*|	utility
*/

M0buf *
M0bufalloc(void)
{
	size_t initial_size = 0;
	M0buf *ans;

	ans = malloc(sizeof *ans);
	assert(ans);

	ans->data = (char *)0;
	ans->alloc = 0;
	ans->count = 0;

	/* allocate data buffer */
	M0bufrealloc(ans, initial_size);

	return ans;
}

/*
*| Name:
*|	M0buffree - free a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|	void
*|	M0buffree(M0buf *buf)
*|
*| Input:
*|      buf     - dynamic buffer to free
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	none
*|
*| Categories:
*|	utility
*/

void
M0buffree(M0buf *buf)
{
	if (buf->data)
		free(buf->data);

	free(buf);
}

/*
*| Name:
*|	M0bufrealloc - set the length of a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|      void
*|      M0bufrealloc(M0buf *buf, size_t nbytes)
*|
*| Input:
*|	nbytes	- number of bytes to make the buffer occupy
*|
*| Input and Output:
*|	buf	- dynamic buffer to modify
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	The buffer grows as necessary.
*|
*|	If memory allocation fails the process terminates.
*|
*|	If the new logical length is greater than the old logical
*|      length, the newly allocated bytes are set to NUL.
*|
*| Categories:
*|	utility
*/

void
M0bufrealloc(M0buf *buf, size_t nbytes)
{
	/* lengthen the allocated memory if necessary */
	if (nbytes >= buf->alloc)
	{
		const size_t CHUNKSIZE = 128;
		size_t remainder;

		buf->alloc = nbytes+1;	/* +1 for NUL */

		/*
		 * To cut down on realloc() requests in case the
		 * program is appending to the buffer with many
		 * small M0bufcat()s, we round the allocation
		 * size up to the next multiple of CHUNKSIZE.
		 */

		if ((remainder = buf->alloc % CHUNKSIZE) != 0)
			buf->alloc += CHUNKSIZE - remainder;
		assert(buf->alloc >= nbytes+1);
		assert(buf->alloc % CHUNKSIZE == 0);

		if (buf->data)
			buf->data = realloc(buf->data, buf->alloc);
		else
			buf->data = malloc(buf->alloc);

		assert(buf->data);
	}

	/* if new size is larger than old size,
	 * zero out the new bytes */

	if (nbytes > buf->count)
	{
		memset(buf->data + buf->count, '\0', nbytes - buf->count);
	}

	/* Set the length */

	buf->count = nbytes;
	buf->data[buf->count] = '\0';
}

/*
*| Name:
*|	M0bufsplice - splice data into a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|      void
*|      M0bufsplice(M0buf *dst, size_t dstoff, size_t dstlen,
*|                  const void *src, size_t srclen)
*|
*| Input:
*|	dstoff	- offset of bytes in dst to remove
*|	dstlen	- number of bytes in dst to remove
*|	src   	- pointer to bytes to add
*|	srclen	- number of bytes to add
*|
*| Input and Output:
*|	dst	- dynamic buffer to modify
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	M0bufsplice() replaces the dstlen bytes starting at offset
*|	dstoff in dst with srclen bytes from src.  It adjusts the
*|	lenth of dst accordingly.
*|
*|      The sum of dstoff and dstlen must not exceed the length of dst.
*|
*|      If src is NULL, then srclen NUL bytes are inserted.
*|      If srclen is zero, the destination bytes are deleted,
*|      but no bytes are inserted.
*|      If dstlen is zero, the source bytes are inserted,
*|      but no bytes are deleted.
*|
*|	If memory allocation fails the process terminates.
*|
*|	If src points into the dst buffer, the results are undefined.
*|
*| Categories:
*|	utility
*/

void
M0bufsplice(M0buf *dst, size_t dstoff, size_t dstlen,
	    const void *src, size_t srclen)
{
	/* taillen is the number of bytes following the bytes to
	 * be replaced in the destination buffer */

	size_t taillen = dst->count - (dstoff + dstlen);

	assert(dst->count >= (dstoff + dstlen));

	/* adjust the size of the buffer if needed */

	if (srclen > dstlen)
	{
		/* prepare for srclen - dstlen additional bytes */

		size_t offset = srclen - dstlen;

		M0bufrealloc(dst, dst->count + offset);

		if (taillen > 0)
		{
			char  *tailptr = dst->data+dstoff+dstlen;

			memmove(tailptr+offset, tailptr, taillen);
		}
	}
	else
	if (dstlen > srclen)
	{
		/* prepare for dstlen - srclen fewer bytes */

		size_t offset = dstlen - srclen;

		if (taillen > 0)
		{
			char  *tailptr = dst->data+dstoff+dstlen;

			memmove(tailptr-offset, tailptr, taillen);
		}

		M0bufrealloc(dst, dst->count - offset);
	}

	/* copy the source bytes to the buffer */

	if (srclen > 0)
	{
		if (src)
			memcpy(dst->data+dstoff, src, srclen);
		else
			memset(dst->data+dstoff, '\0', srclen);
	}
}


/*
*| Name:
*|	M0buflen - get the length of a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|      size_t
*|      M0buflen(M0buf *buf)
*|
*| Input:
*|	buf	- dynamic buffer
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	The length of the dynamic buffer.
*|
*|	The length does not include the NUL byte that is always beyond
*|	the end of the data in the buffer.
*|
*| Remarks:
*|	none
*|
*| Categories:
*|	utility
*/

size_t
M0buflen(M0buf *buf)
{
	return buf->count;
}

/*
*| Name:
*|	M0bufptr - return a pointer to the data in a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|      void *
*|      M0bufptr(M0buf *buf)
*|
*| Input:
*|	buf	- dynamic buffer
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	A pointer to the data in the dynamic buffer.
*|
*|	The pointer is undefined after the buffer has been modified
*|	by M0bufcpy(), M0bufcat(), M0bufrealloc(), etc.
*|
*| Remarks:
*|	There is always a NUL byte just after the end of the data in
*|	the buffer, so if the buffer does not contain any NUL bytes,
*|	it is safe to use strcpy(), etc., with the result of M0bufptr().
*|
*| Categories:
*|	utility
*/

void *
M0bufptr(M0buf *buf)
{
	return buf->data;
}

/*
*| Name:
*|	M0bufcpy - set the contents of a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|      void
*|      M0bufcpy(M0buf *dst, const void *src, size_t nbytes)
*|
*| Input:
*|	src	- region to copy from
*|	nbytes	- number of bytes to copy
*|
*| Input and Output:
*|	dst	- dynamic buffer to modify
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	The buffer grows as necessary.
*|
*|	If memory allocation fails the process terminates.
*|
*|	If src points into the dst buffer, the results are undefined.
*|
*| Categories:
*|	utility
*/

void
M0bufcpy(M0buf *dst, const void *src, size_t nbytes)
{
	M0bufsplice(dst, 0, dst->count, src, nbytes);
}

/*
*| Name:
*|	M0bufcat - append to the contents of a dynamic buffer
*|
*| Interface:
*|	#include "mcidasp.h"
*|
*|      void
*|      M0bufcat(M0buf *dst, const void *src, size_t nbytes)
*|
*| Input:
*|	src	- region to copy from
*|	nbytes	- number of bytes to copy
*|
*| Input and Output:
*|	dst	- dynamic buffer to modify
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	The buffer grows as necessary.
*|
*|	If memory allocation fails the process terminates.
*|
*|	If src points into the dst buffer, the results are undefined.
*|
*| Categories:
*|	utility
*/

void
M0bufcat(M0buf *dst, const void *src, size_t nbytes)
{
	M0bufsplice(dst, dst->count, 0, src, nbytes);
}

#if 0
main()
{
	char *str = "hello there";
	M0buf *b;
	char c;

	b = M0bufalloc();

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	for (c = 'a'; c <= 'z'; c++)
		M0bufsplice(b, 0, 0, &c, 1);

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufsplice(b, 1, 24, (char *)0, 0);

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufrealloc(b, 0);
	for (c = 'a'; c <= 'z'; c++)
		M0bufsplice(b, 0, 0, &c, 1);
	M0bufsplice(b, 1, 24, "MIDDLE", 6);

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufsplice(b, 1, 6, "middle", 6);

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufsplice(b, 1, 6, "NEW MIDDLE", 10);

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufcpy(b, str, strlen(str));

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufcat(b, str, strlen(str));

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufrealloc(b, 0);
	for (c = ' '; c <= '~'; c++)
		M0bufcat(b, &c, sizeof(c));

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	for (c = ' '; c <= '~'; c++)
		M0bufcat(b, &c, sizeof(c));

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	M0bufrealloc(b, 0);

	printf("alloc %ld:", (long) b->alloc);
	printf("len %ld: '%s'\n", (long) M0buflen(b), M0bufptr(b));

	return 0;
}
#endif
