/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 FRAME.C 27-Feb-96,13:14:26,`ROBERTM' initial checkin of DMSP nav        */
/* 2 FRAME.C 17-Apr-96,14:48:22,`USER' Released                              */
/* 3 FRAME.C 27-Sep-96,16:29:54,`ROBERTM' Include mcidasp.h; remove M0assert(*/
/* 4 FRAME.C 22-Oct-96,19:41:16,`USER' Released                              */
/**** McIDAS Revision History *** */

#include <ctype.h>
#include <errno.h>
#include <limits.h>	/* for CHAR_BIT */
#include <stdio.h>	/* for size_t */
#include <stdlib.h>	/* for ANSI memory manager functions */
#include <string.h>
#include <stdarg.h>

#include "mcidas.h"	/* may have to or want to remove for CAMA */
#include "mcidasp.h"
#include "m0frame.h"


/* Update Log **********************************************************
96.01.13 rtm	Modify safe filesystem to use linked list for log
		(should work better for apps with much I/O)
95.06.05 rtm	Add M0fsMemory and M0strMemory() for McIDAS. Bring
		up to McIDAS documentation standards. Move prototypes
		for internal routines from m0frame.c
95.03.08 rtm	16-bit stuff removed
95.02.09 rtm	Many arguments 'const' qualified. Unit test rerun.
94.11.17 rtm	'lint' produces no unexplained remarks.
94.11.16 rtm	#elifs removed for compatibility with HP-UX
94.11.05 rtm	Safe file system and stdin drain added.
94.05.21 rtm	Documentation added
***********************************************************************/



#if defined(M0DEBUG)

/* 'byte*' is analogous to void* except that it allows pointer
 * arithmetic. They are used only within this module */

typedef char byte;



/* Linked list node of the allocated memory block list. It is
 * used only within this module */

typedef struct _blockinfo {
 	struct _blockinfo *pbNext;
 	byte		*pb;		/* Start of block.	*/
 	size_t		size;		/* Length of block	*/
 	M0flag		Referenced;	/* Ever referenced?     */
}
blockinfo;
 

/* head pointer for the linked list of memory blocks. It is
 * accessible only within this module */

static blockinfo *Memlog_head = NULL;

/* Linked list node of the open file list. It is
 * used only within this module */

typedef struct _fileinfo {
	struct _fileinfo	*pNext;		/* list link	*/
	char			*szName;	/* file name	*/
	char			*szMode;	/* file I/O mode*/
	FILE			*pfHandle;	/* file handle	*/
}
fileinfo;

/* Memory counters	*/

static long
Total_memory = 0;

static long
Total_alloc = 0;

static long
Total_freed = 0;

static long
Mx_memory = 0;


/* head pointer for the file list */

static fileinfo
*Filelog_head = NULL;

/* file counters */

static long
Total_opened = 0;

static long
Total_closed = 0;

static long
Mx_open = 0;


/* Prototypes of functions used only within this module 	*/

static M0flag
CreateBlockInfo(void *pNew, size_t size);

static void
FreeBlockInfo(void *pFreed);

static void
UpdateBlockInfo(void *pOld, void *pNew, size_t newsize);

static M0flag
GetBlockInfo(void *ptr, blockinfo **pbi);

static size_t
sizeofBlock(void *pBlock);

static unsigned long
AbsAddr (void *ptr);
 
static M0flag
PtrEqual(byte *pLeft, byte *pRight);

static M0flag
PtrLessEq(byte *pLeft, byte *pRight);

static M0flag
PtrGrtrEq(byte *pLeft, byte *pRight);


#endif

/*
*| Name:
*|	 yes - Prompt for a yes/no reply
*|
*| Interface:
*|
*|	M0flag
*|	yes(const char * const prompt)
*|
*| Input:
*|	prompt		- Yes/no question
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0FALSE	- user responded with 'N' or 'n'
*|	M0TRUE	- user responded with anything but 'N' or 'n'
*|
*| Remarks:
*|	    The yes/no prompt (Y/n) will be appended to the
*|	user-supplied prompt.
*|
*| Categories: 
*|	user interface -- non McIDAS
*|
*/

M0flag
yes ( const char * const prompt )
{
	int		reply;	/* user's response	*/
	M0flag		ok;	/* Boolean result	*/

	printf( "%s (Y/n):", prompt);
	reply = getchar();
	if ( toupper(reply) == 'N' )
		ok = M0FALSE;
	else
		ok = M0TRUE;
	fflush ( stdin );
	return ok;
}


/*
*| Name:
*|	M0safeOpen - open file and record in log
*|
*| Interface:
*|	#include "frame.h"
*|
*|	FILE
*|	M0safeOpen(char *szName, char *szMode)
*|
*| Input:
*|	szName	- path and name of file to open
*|	szMode	- access mode
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	pointer to stream associated with file, or NULL
*|	if file could not be opened
*|
*| Remarks:
*|	    This routine is available only if 'frame.c' is
*|	compiled with M0DEBUG defined. It should be called
*|	only via the.M0FOPEN() macro, and M0FCLOSE then used
*|	to close files to keep the list intact.
*|	    This routine issues a warning to stderr if the file
*|	has already been opened.
*|
*| Categories: 
*|	diagnostics
*|	file 
*/

#if defined(M0DEBUG)

FILE
*M0safeOpen(char *szName, char *szMode )
{
	FILE		*pfHandle_new;	/* handle to new file	*/
	char		*szName_new;	/* copy of new file name*/
	char		*szMode_new;	/* copy of new file mode*/
	fileinfo	*pFileinfo;	/* pointer to file info
					 * node */


	/* Scan the list of presently opened files. Display a
	 * warning if the current request is already opened, and
	 * return a handle to it */

	pFileinfo = Filelog_head;

	while ( pFileinfo != NULL ) {
		if( strcmp(szName,pFileinfo->szName) == 0 ) {
			fprintf(stderr,"M0safeOpen(): WARNING -- "
			  " file \"%s\" already open\n", szName);
			return pFileinfo->pfHandle;
		}
		pFileinfo = pFileinfo->pNext;
	}

	/* File is not already opened; open it, and if successful,
	 * add a list node for it */

	pfHandle_new = fopen( szName, szMode );
	if ( pfHandle_new != NULL ) {

		/* If there is not enough memory for another file
		 * record (including copies of the name and access
		 * mode) issue a warning and close the file, seen
		 * by the caller as a failure to open. The caller
		 * is on its last legs anyhow if this happens! */
		
		if( ! M0newMemory((void **)&pFileinfo,
		  sizeof(fileinfo)) ) {
#ifdef UNDEF
		pFileinfo = (fileinfo *)malloc( sizeof(fileinfo) );	
		if ( pFileinfo == NULL ) {
#endif
			fprintf(stderr,"M0safeOpen() could not "
			  "allocate file info block\n");
			fclose(pfHandle_new);
			return (FILE *)NULL;
		}
#ifdef UNDEF
		szName_new = (char *)malloc( strlen(szName)+1);
		if( szName_new == NULL ) {
#endif
		if( ! M0newMemory((void **)&szName_new,
		  strlen(szName)+1) ) {
			fprintf(stderr,"M0safeOpen() could not "
			  "allocate szName_new for file info block\n");
			M0freeMemory( (void **)&pFileinfo);
			fclose(pfHandle_new);
			return (FILE *)NULL;
		}
		strcpy(szName_new, szName);
#ifdef UNDEF
		szMode_new = (char *)malloc( strlen(szMode)+1 );
		if( szMode_new == NULL ) {
#endif
		if( ! M0newMemory((void **)&szMode_new,
		  strlen(szMode)+1 ) ) {
			fprintf(stderr,"M0safeOpen() could not "
			  "allocate szMode_new for file info block\n");
			M0freeMemory((void **)&pFileinfo);
			M0freeMemory((void **)&szName_new);
			fclose(pfHandle_new);
			return (FILE *)NULL;
		}
		strcpy(szMode_new, szMode);

		/* Populate the file list node and link it */

		pFileinfo->pfHandle = pfHandle_new;
		pFileinfo->szName = szName_new;
		pFileinfo->szMode = szMode_new;

		pFileinfo->pNext  = Filelog_head;
		Filelog_head	  = pFileinfo;

		/* Count successes	*/

		Total_opened++;
		if ( (Total_opened - Total_closed) > Mx_open ) {
		Mx_open = Total_opened - Total_closed;
	}
		

	} else {
		fprintf(stderr,"M0safeOpen() ERROR: "
		  "fopen(\"%s\", \"%s\") set errno=%d\n",
		  szName, szMode, errno);
	}

	return pfHandle_new;
}


/*
*| Name:
*|	M0safeClose - close file and delete from log
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	int
*|	M0safeClose(FILE *pfHandle)
*|
*| Input:
*|	pfHandle - handle to file to close
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	0	- closed successfully
*|	EOF	- errors occurred
*|
*| Remarks:
*|	    This routine issues a warning to stderr if the file
*|	handle is not found in the list of active files.
*|	    This routine is available only if 'frame.c' is compiled
*|	with M0DEBUG defined. It should be called only via the
*|	M0CLOSE() macro on files opened with M0FOPEN().
*|
*| Categories: 
*|	diagnostics
*/

int
M0safeClose( FILE *pfHandle )
{
	int		rc;		/* return code		*/
	fileinfo	*pFileinfo;	/* file list node	*/
	fileinfo	*pPrev;		/* previous list node	*/

	rc = fclose(pfHandle);

	/* search the file pointer list for 'pfHandle.' If found,
	 * remove it from the list and return. */

	pFileinfo = Filelog_head;
	pPrev     = NULL;

	while( pFileinfo != NULL ) {
		if( pFileinfo->pfHandle == pfHandle ) {
			M0freeMemory((void **)&(pFileinfo->szName));
			M0freeMemory((void **)&(pFileinfo->szMode));
			if( pPrev != NULL ) {
				pPrev->pNext = pFileinfo->pNext;
			} else {
				Filelog_head = pFileinfo->pNext;
			}
			M0freeMemory((void **)&pFileinfo);
			Total_closed++;
			return rc;
		}
		pPrev     = pFileinfo;
		pFileinfo = pFileinfo->pNext;
	}

	/* I searched the whole list and never found that handle.
	 * Issue a warning and return */

	fprintf(stderr,"\nM0safeClose() WARNING: no record of file "
	  "handle %p\n", pfHandle);
	
	return rc;
}


/*
*| Name:
*|	M0listOpenFiles - lists all open files to stderr
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0listOpenFiles(void)
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
*|	none
*|
*| Remarks:
*|	    This routine is available only if 'frame.c' is compiled
*|	with M0DEBUG defined. It should be called only via the
*|	M0FLIST() macro.
*|
*| Categories: 
*|	diagnostics
*/

void
M0listOpenFiles(void)
{
	fileinfo	*pFileinfo;	/* Pointer to list node	*/

	fprintf( stderr, "\n\n    Open files:\n");
	fprintf( stderr, "    handle mode name\n");
	fprintf( stderr, "------------------------------\n");
	
	pFileinfo = Filelog_head;

	while( pFileinfo != NULL ) {
		fprintf( stderr, "%10p %-4s %s\n",
		  pFileinfo->pfHandle, pFileinfo->szMode,
		  pFileinfo->szName);
		pFileinfo = pFileinfo->pNext;
	}
	fprintf( stderr, "------------------------------\n");
	fprintf( stderr, "\nActivity summary:\n");
	fprintf( stderr, "   Total opened     = %d\n", Total_opened);
	fprintf( stderr, "   Total closed     = %d\n", Total_closed);
	fprintf( stderr, "   Max open at once = %d\n", Mx_open);
	fprintf( stderr, "\n");
	return;
}

/* This notes the memory references associated with the safe
 * file system */

void
M0noteOpenFiles(void)
{
	fileinfo	*pFileinfo;	/* pointer to list node	*/
	
	pFileinfo = Filelog_head;
	while( pFileinfo != NULL ) {
		M0noteMemRef( pFileinfo );
		M0noteMemRef( pFileinfo->szMode );
		M0noteMemRef( pFileinfo->szName );
		pFileinfo = pFileinfo->pNext;
	}
	return;
}

#endif


/*
*| Name:
*|	M0newMemory - Allocate new memory 
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0newMemory(void **phNew, size_t size)
*|
*| Input:
*|	size	- size in bytes of new memory block
*|
*| Input and Output:
*|	phNew	- address of pointer to new memory block
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE	- success
*|	M0FALSE	- could not allocate memory
*|
*| Remarks:
*|	If allocation fails, *phNew is NULL.
*|	If M0DEBUG is defined when 'frame.c' is compiled,
*|	additional debugging support is provided. M0newMemory()
*|	then fills the block with 'garbage' and records its
*|	size and location on the list. It also validates the
*|	requested size.
*|
*| Categories: 
*|	diagnostics 
*/

 
M0flag
M0newMemory (void **phNew, size_t size )
{
    M0ASSERT ( phNew != NULL && size != 0 );  /* bad handle address */

    *phNew = malloc(size);
	
#if defined(M0DEBUG)
    if ( *phNew != NULL ) {

        memset ( *phNew, M0GARBAGE, size );

	/* If the additional space for the memory log record
	 * cannot be created, a failure to allocate the requested
	 * block is 'faked' */

	if( ! CreateBlockInfo(*phNew, size) ) {
            free ( *phNew );
            *phNew = NULL;
        }
    }

    Total_alloc	 += size;
    Total_memory += size;
    Mx_memory      = Total_memory > Mx_memory ?
      Total_memory : Mx_memory;

#endif
    if( *phNew != NULL ) 
	return M0TRUE;
    else
	return M0FALSE;
}


/*
*| Name:
*|	M0freeMemory - release dynamic memory
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0freeMemory(void **phOld)
*|
*| Input:
*|	none
*|
*| Input and Output:
*|	phOld	- address of pointer to memory block, NULL on return
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	If M0DEBUG is defined when 'frame.c' is compiled,
*|	additional debugging support is provided. M0freeMemory()
*|	fills the block with 'garbage' and removes it from
*|      the active list before releasing it.
*|
*| Categories: 
*|	diagnostics 
*/

void
M0freeMemory(void **phOld)
{
    M0ASSERT (  phOld != NULL ); /* bad pointer to memory
			          * block handle */
    M0ASSERT ( *phOld != NULL ); /* bad memory block handle */
	
#if defined(M0DEBUG)

    Total_freed  += sizeofBlock(*phOld);
    Total_memory -= sizeofBlock(*phOld);
    M0ASSERT(Total_memory >= 0);

    memset ( *phOld, M0GARBAGE, sizeofBlock(*phOld) );
    FreeBlockInfo ( *phOld );


#endif

    free ( *phOld );
    *phOld = NULL;
    return;
}




/*
*| Name:
*|	M0resizeMemory - Resize allocated memory block
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0resizeMemory(void **phActive, size_t newsize)
*|
*| Input:
*|	newsize	 - new size
*|
*| Input and Output:
*|	phActive - address of pointer to memory block
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE	- success
*|	M0FALSE	- could not resize memory
*|
*| Remarks:
*|	If M0DEBUG is defined when 'frame.c' is compiled,
*|	additional debugging support is provided. M0resizeMemory()
*|	first validates ownership of the active block. If the new
*|	size is smaller, the excess is filled with garbage and the 
*|	block is shrunk in place. If the new size is larger, a new
*|      block is allocated, the contents of the old one moved and
*|      padded with garbage, and the old one released. Unlike
*|      realloc(), increasing the size of the block ALWAYS causes
*|	it to move. The memory log is then updated.
*|
*| Categories: 
*|	diagnostics 
*/

M0flag
M0resizeMemory(void **phActive, size_t newsize)
{
    void	*pNew;		/* pointer to new block	*/

#if defined (M0DEBUG)
    size_t	oldsize;	/* old block size	*/
#endif

    M0ASSERT (  phActive != NULL );	/* uninitialized address
					 * of memory handle */
    M0ASSERT ( *phActive != NULL
      && newsize != 0 );	 	/* bad memory handle or
					 * new size of 0 */
#if defined (M0DEBUG)
    {
	byte *pbActive	= (byte *)(*phActive);
        oldsize		= sizeofBlock(*phActive);
		
        /* If the block is shrinking, pre-fill the
         * soon-to-be released memory with garbage.
	 * If the block is expanding, force it to move
	 * (instead of expanding in place) by
	 * copying it myself and then calling
         * realloc to expand the copy.  If the block
	 * is the same size, don't do anything */
		 
	if( newsize < oldsize ) {
	    memset( pbActive+newsize,
	      M0GARBAGE, oldsize-newsize);
	}
	else if ( newsize > oldsize ) {
	    void *hForceNew;
	    if ( M0newMemory(&hForceNew,newsize) ) {
	        memcpy(hForceNew, *phActive, oldsize);
		M0freeMemory(phActive);
		*phActive = hForceNew;
	    }
	}
    }
#endif
	
    pNew = realloc ( *phActive, newsize );
	
    if ( pNew != NULL ) {
#if defined (M0DEBUG)
	byte *hbNew = (byte *)pNew;	/* allows pointer arithmetic */

        UpdateBlockInfo ( *phActive, pNew, newsize );

        /* if expanding, initialize the new tail
         * with garbage */

        if ( newsize > oldsize ) {
            memset ( (void *)(hbNew+oldsize),M0GARBAGE,newsize-oldsize);
        }
#endif
        *phActive = pNew;
    }

#ifdef M0DEBUG
    /* If block shrunk, adjust the memory allocation counters.
     * An expand is done with a M0newMemory() and M0freeMemory() so
     * the counters are already changed. */

    if( newsize < oldsize ) {
        Total_memory -= oldsize;
        Total_memory += newsize;
        Total_freed  += oldsize;
        Total_freed  -= newsize;
    }
#endif	/* ifdef M0DEBUG */


    if( pNew != NULL ) 
	return M0TRUE;
    else
	return M0FALSE;
}
  



/*
*| Name:
*|	M0strdup - return a copy of an string in allocated memory
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	char *
*|	M0strdup(const char *szOld)
*|
*| Input:
*|	szOld	- pointer to null-terminated string
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	   pointer to copy of string
*|	or NULL if memory allocation failed
*|
*| Remarks:
*|	     The returned string is dynamically allocated. It
*|	is the responsibility of the caller to free it when it
*|	is no longer needed.
*|	     If M0DEBUG is defined when 'frame.c' is compiled,
*|	a record of the allocated block is kept.
*|
*| Categories: 
*|	diagnostics 
*/

char *
M0strdup(const char *szOld)
{
	char	*szNew;		/* pointer to copy of string	*/
	size_t	len;		/* length of old string,	*
				 * including trailing null	*/

	len	= strlen(szOld) + 1;
	if ( ! M0newMemory( (void **)&szNew, len ) ) {	
		szNew = (char *)NULL;
	} else {
		memcpy(szNew, szOld, len);
	}
	return szNew;
}
	


/*
*| Name:
*|	M0strMemory - concatenate strings to new string
*|
*| Interface:
*|	#include "mcidas.h"
*|	#include "m0frame.h"
*|
*|	(char *)
*|	M0strMemory(const char *s, ...)
*|
*| Input:
*|	s, ...	 - one or more strings to concatenate
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	pointer to concatenated string	- success
*|	(char *)0			- memory allocation failure
*|
*| Remarks:
*|	If M0DEBUG is defined when 'frame.c' is compiled,
*|	additional debugging support is provided. M0strMemory()
*|	calls M0newMemory, providing argument validation and
*|	adding the memory size and location to the active list.
*|
*| Categories: 
*|	diagnostics 
*/

#if defined(_MCIDAS_H)

char *
M0strMemory(char *s, ...)
{
	va_list ap;

	char *p = (char *)0;
	char *q = (char *)0;
	unsigned l = 1;		/* for the terminal null */

	/* calculate the length of the string to allocate */

	va_start(ap, s);
	for(p = s; p != (char *)0; p = va_arg(ap, char *))
		l += strlen(p);
	va_end(ap);
#if defined (UNDEF)
/* old 'stralloc' code */
	if((q = (char *)malloc(l)) == (char *)0)
		return (char *)0;
#endif
	if ( ! M0newMemory((void **)&q, l) )
		return (char *)0;

	*q = '\0';	/* initialize to null string */

	va_start(ap, s);
	for(p = s; p != (char *)0; p = va_arg(ap, char *))
		(void) strcat(q,p);
	va_end(ap);

	return q;
}

#endif


/*
*| Name:
*|	M0fsMemory - allocate C string from FORTRAN string
*|
*| Interface:
*|	#include "mcidas.h"
*|	#include "m0frame.h"
*|
*|	char *
*|	M0fsMemory(char *string, FsLen n)
*|
*| Input:
*|	string		- FORTRAN string (no null terminator,
*|			  right-filled with blanks)
*|	n		- number of characters in 'string'
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	pointer to allocated C string	- success
*|	(char *)0			- memory allocation failure
*|
*| Remarks:
*|	If M0DEBUG is defined when 'frame.c' is compiled,
*|	additional debugging support is provided. M0fsMemory()
*|	calls M0newMemory, providing argument validation and
*|	adding the memory size and location to the active list.
*|
*| Categories: 
*|	diagnostics 
*/

#if defined(_MCIDAS_H)

char *
M0fsMemory(const char *string, FsLen n)
{
	char           *ans = (char *)0;

	n--;			/* length -> index */
	while (n >= 0 && string[n] == SPACE)
	{
		/* string[n] = '\0'; */
		n--;
	}
	n++;			/* index -> length */

	if ( ! M0newMemory( (void **)&ans, n+1 )  )
		return (char *)0;
#if defined ( UNDEF )
/* old fsalloc code */
	if ((ans = malloc(n + 1)) == (char *) 0)
		return (char *) 0;
#endif

	strncpy(ans, string, n);
	ans[n] = '\0';

	return ans;
}

#endif





/* MEMORY AND POINTER VALIDATION function definitions.
 * These services are available only if M0DEBUG is defined
 * when this module is compiled.
 */

#if defined(M0DEBUG)


/*
*| Name:
*|	M0dumpMemInfo - list current dynamic memory blocks to stream
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0dumpMemInfo(FILE *pf)
*|
*| Input:
*|	pf		- pointer to stream or file for output
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
*|	This routine is available only if frame.c is compiled
*|	with M0DEBUG defined.
*|
*| Categories: 
*|	diagnostics 
*/

void
M0dumpMemInfo ( FILE *pf )
{
    /* writes the descriptions of all allocated memory blocks to
     * file pointer pf */
	 
    blockinfo *pbi;
	 
    M0ASSERT ( pf != NULL );	/* stream pf not open! */

    fprintf ( pf, "\n--- Active memory blocks -------------------");
    if ( Memlog_head == NULL ) {
        fprintf ( pf, "\n(no dynamic memory blocks allocated)");
    }
    else {
        for ( pbi = Memlog_head; pbi != NULL; pbi = pbi->pbNext ) {
	    fprintf ( pf, "\nStart=%p bytes=%8u ", pbi->pb,
	       pbi->size);
	    if( pbi->Referenced ) {
		fprintf( pf, " referenced");
	    }
	 }
    }
    fprintf ( pf, "\n--- End of list ----------------------------\n");
    fprintf( pf, "\nSummary:\n");
    fprintf( pf, "   Total allocated memory   %d\n", Total_memory);
    fprintf( pf, "   Total bytes requested    %d\n", Total_alloc);
    fprintf( pf, "   Total bytes freed        %d\n", Total_freed);
    fprintf( pf, "   Max allocated at once    %d\n", Mx_memory);
    fprintf( pf, "\n");
    return;
}
	 	 

/*
*| Name:
*|	M0clearMemRefs - Mark all logged memory blocks 'unreferenced'
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0clearMemRefs(void)
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
*|	none
*|
*| Remarks:
*|	This routine is available only if frame.c is compiled
*|	with M0DEBUG defined.
*|	Memory leaks and dangling pointers may be detected by
*|	calling M0clearMemRefs(), traversing all known memory
*|	blocks, calling M0noteMemRef() on each, and then calling
*|	M0checkMemRefs(). Dangling pointers (referencing blocks
*|	no longer owned by the process) will cause M0noteMemRef()
*|	assert, while memory leaks (blocks owned by the process but
*|	lacking a pointer to them) will cause M0checkMemRefs() to
*|	assert.
*|
*| Categories: 
*|	diagnostics 
*/

void
M0clearMemRefs(void)
{
    blockinfo *pbi;
	
    for ( pbi = Memlog_head; pbi != NULL; pbi = pbi->pbNext ) {
	pbi->Referenced = M0FALSE;
    }
    return;
}


/*
*| Name:
*|	M0noteMemRef - Mark current block as 'referenced'
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0noteMemRef(void *ptr)
*|
*| Input:
*|	ptr	- pointer to memory block believed owned
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
*|	This routine is available only if frame.c is compiled
*|	with M0DEBUG defined.
*|	See 'Remarks' on M0clearMemRefs() for a descripton of
*|	how this routine can be used in a strategy to detect
*|	memory leaks and dangling pointers.
*|
*| Categories: 
*|	diagnostics 
*/

void
M0noteMemRef(void *ptr )
{
    blockinfo	*pbi;
    M0flag	inblock;
	
    inblock	= GetBlockInfo ( ptr, &pbi );
    M0ASSERT ( inblock );		/* ptr not in active block */
    M0ASSERT ( (void *)pbi->pb == ptr );/* ptr is not start of block
					 * containing it */
    pbi->Referenced = M0TRUE;
    return;
}


/*
*| Name:
*|	M0checkMemRefs - Confirm that all blocks are 'referenced'
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0checkMemRefs(void)
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
*|	none
*|
*| Remarks:
*|	This routine is available only if frame.c is compiled
*|	with M0DEBUG defined.
*|	See 'Remarks' on M0clearMemRefs() for a descripton of
*|	how this routine can be used in a strategy to detect
*|	memory leaks and dangling pointers.
*|
*| Categories: 
*|	diagnostics 
*/

void
M0checkMemRefs(void)
{
    blockinfo *pbi;
	
    for ( pbi = Memlog_head; pbi != NULL; pbi = pbi->pbNext ) {
		
	/* if this asserts, either there is a problem in
	 * CreateBlockInfo or UpdateBlockInfo or somehow
	 * the memory log has been trashed. It is probably
	 * not caused by the caller (unless the caller scored
	 * a 'lucky hit' on the portion of memory containing
	 * the dynamic memory log! */
		 
        M0ASSERT ( pbi->pb != NULL && pbi->size != 0 );
		 
	/* if this assertion fires, it means that the
	 * indicated block (and possibly ones allocated
	 * earlier--testing ceases with the first) were
	 * not referenced on the most recent traverse of
	 * global data.  Either we missed a data structure
	 * or there is a dangling memory block */
		  
	M0ASSERT ( pbi->Referenced );
    }
    return;
}


/*
*| Name:
*|	M0validPointer - Confirm that pointer references owned memory
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0validPointer(void *ptr, size_t size)
*|
*| Input:
*|	ptr	- starting address to validate
*|	size	- bytes past 'ptr' to validate
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE	- always
*|
*| Remarks:
*|	This routine is available only if frame.c is compiled
*|	with M0DEBUG defined.
*|	If any of the 'size' bytes starting at 'ptr' do not
*|	lie within an owned memory block the routine will
*|	assert. The return type of 'M0flag' is chosen simply
*|	to allow M0validPointer to be called from M0ASSERT().
*|	In this way, compiling with M0DEBUG undefined will
*|	remove all of the M0validPointer() calls from the
*|	application.
*|
*| Categories: 
*|	diagnostics 
*/
 
M0flag
M0validPointer (void *ptr, size_t size ) 
{
	M0flag		inblock;
	blockinfo	*pbi;
	byte		*pb = (byte *)ptr;

	/* assert that ptr is valid and expected size is OK */
		
	M0ASSERT ( ptr != NULL && size != 0 );
	
	inblock = GetBlockInfo(ptr,&pbi);

	/* Assert that object start (ptr) lies within a
	 * memory block */

	M0ASSERT(inblock);

	/* Assert that end of object does not extend beyond
	 * end of memory block */
	 	
	M0ASSERT ( PtrLessEq(pb+size, pbi->pb+pbi->size));
	
	return(M0TRUE);
}


/* The following routines are 'for locals only' and are not
 * to be accessed from other modules. Their omission from
 * m0frame.h and their 'static' class specifier should make
 * access outside this module impossible */

static M0flag
GetBlockInfo ( void *ptr, blockinfo **ppbi )

/* Locate memory log information for the memory block
 * in which 'ptr' is located; Return 'false' if the pointer
 * does not reference an active memory block */
{
    M0flag	inblock = M0FALSE;
    byte	*pbyte = (byte *)ptr;
    blockinfo	*pbi;
 	
    for ( pbi = Memlog_head; pbi != NULL; pbi = pbi->pbNext ) {
        byte *pbStart = pbi->pb;
        byte *pbEnd   = pbi->pb + pbi->size - 1;
 		
 	if ( PtrGrtrEq(pbyte,pbStart)
	  && PtrLessEq(pbyte,pbEnd)) {
	    inblock = M0TRUE;
 	    break;
	}
    }
    *ppbi = pbi;
    return ( inblock );
}


static M0flag
CreateBlockInfo(void *pNew, size_t size )

/* Function creates a log entry for the memory block
 * defined by pbNew and sizeNew. 
 *
 * Returns:
 *    M0TRUE    if log entry created, 
 *    M0FALSE	otherwise (malloc for log node failed) */

{
    blockinfo *pbi;
	
    /* This assertion traps zero-size block or unallocated block */

    M0ASSERT ( pNew != NULL && size != 0 );
	
    pbi = (blockinfo *)malloc(sizeof(blockinfo));
    if ( pbi != NULL ) {
        pbi->pb 	= (byte *)pNew;
	pbi->size 	= size;
	pbi->pbNext 	= Memlog_head;
	pbi->Referenced = M0FALSE;
	Memlog_head 	= pbi;
    }
    if( pbi == NULL ) 
        return M0FALSE;
    else
        return M0TRUE; 
}



static void
FreeBlockInfo(void *pFreed )

/* Destroys log entry for the memory block that pFreed points to.
 * pFreed MUST point to the start of an allocated block or the
 * function will assert */
{
    blockinfo *pbi, *pbiPrev;
    byte	*pbFreed = (byte *)pFreed;
	
    pbiPrev = NULL;
    for ( pbi = Memlog_head; pbi != NULL; pbi = pbi->pbNext ) {
	if ( PtrEqual ( pbi->pb, pbFreed ) ) {
	    if ( pbiPrev == NULL ) {
		Memlog_head = pbi->pbNext;
	    }
	    else {
		pbiPrev->pbNext = pbi->pbNext;
	    }
	    break;	/* quit for loop */
	}
	pbiPrev = pbi;
    }

    M0ASSERT ( pbi != NULL ); /* You attempted to free a block
			       * you don't own; I couldn't find
			       * it in the list */
	
    /* destroy contents of memory log block before freeing */
	
    memset ( pbi, M0GARBAGE, sizeof(blockinfo) );
	
    free ( pbi );
}



static void
UpdateBlockInfo (void *pOld, void *pNew, size_t	newsize )

/* UpdateBlockInfo looks up the log information for the memory
 * block that pbOld points to and updates that information to
 * after the block has been relocated to pbNew and resized to
 * sizeNew.  pbOld must be found in the log or routine asserts */
{
    blockinfo	*pbi;
    M0flag	inblock;
	
    /* validate new memory block location and size */

    M0ASSERT ( pNew != NULL && newsize != 0 );

    inblock = GetBlockInfo(pOld,&pbi);
    M0ASSERT(inblock);		 /* 'old' pointer not in any
				  * active block */
    M0ASSERT( pOld == pbi->pb ); /* 'old' pointer does not
				  * reference the start of the block
				  * in which it is located */
    pbi->pb 	= (byte *)pNew;
    pbi->size 	= newsize;
}


static size_t sizeofBlock (void *ptr )

/* Returns the size of the block referenced by ptr.  ptr must be
 * located in the memory allocation log as the start of a valid
 * block or routine will assert */
{
    blockinfo	*pbi;
    M0flag	inblock;
	
    inblock = GetBlockInfo(ptr,&pbi);
    M0ASSERT(inblock);		/* ptr not located in active block */
    M0ASSERT(ptr == pbi->pb);	/* ptr does not reference the start
			  	 * of the block in which it is
				 * located */
    return ( pbi->size );
}



/* pointer validation routines.
 * Support routine AbsAddr converts the native pointer type
 * to an absolute address stored as a type flat_addr (long
 * enough to hold largest absolute address possible
 */
 
  
static unsigned long
AbsAddr(void *ptr)

/* compute an absolute address from the void pointer passed as
 * an argument. */

{
    /* for UNIX machines, pointers are flat 32 bit.  Just cast
     * to unsigned long and you're done */
	 
    return( (unsigned long)ptr );
}


/* pointer comparison routines */


static M0flag
PtrEqual(byte *pLeft, byte *pRight )
{
    if ( AbsAddr(pLeft) == AbsAddr(pRight) )
	return M0TRUE;	
    else
        return M0FALSE;
}

static M0flag
PtrLessEq(byte *pLeft, byte *pRight )
{
    if ( AbsAddr(pLeft) <= AbsAddr(pRight) )
        return M0TRUE;
    else
        return M0FALSE;
}

static M0flag
PtrGrtrEq(byte *pLeft, byte *pRight )
{
    if( AbsAddr(pLeft) >= AbsAddr(pRight) )
        return M0TRUE;
    else
        return M0FALSE;
}
                              
#endif



/* test sequence for this module -------------------------------------*/


#if defined (FILESYS)

int main ( void )
{
    FILE *fp1, *fp2, *fp3, *fp4;
    char achFilename[BUFSIZ];
    int		ncycles;	/* number of cycles for stress
				 * test	*/
    int		icycle;		/* cycle index */
	
    printf("\n\nFile subsystem validation.");
    printf("\nEnter file names as prompted." );
    printf("\nEnter name of  first file: " );
    fflush ( stdin );
    gets ( achFilename );
    fp1 = M0FOPEN ( achFilename, "a+" );
    if ( fp1 != NULL ) {
        printf("\n%s successfully opened", achFilename );
    }
    else {
        printf("\ncould not open %s\n", achFilename );
	return(-1);
    }
#if defined(M0DEBUG)
    printf("\n\nM0DEBUG was defined at compile time, so the");
    printf("\nfile system will issue a warning if you try to");
    printf("\nopen a file that is already open.");
#endif
    printf("\nEnter name of second file:  ");
    gets ( achFilename );
    fp2 = M0FOPEN ( achFilename, "a+" );
    if ( fp2 != NULL ) {
        printf("\n%s successfully opened", achFilename );
    }
    else {
        printf("\nCould not open %s\n", achFilename );
	return(-2);
    }


    printf("\nA third file will be opened without using");
    printf("\nthe safe file system (call \'fopen\' directly.");
    printf("\nEnter name of  third file:  ");
    fflush(stdin);
    gets(achFilename);
    fp3 = fopen(achFilename, "a+");
    if ( fp3 != NULL ) {
	printf("\n%s successfully opened", achFilename);
    }
    else {
        printf("\nCould not open %s\n", achFilename );
	return(-3);
    }
       
#if defined(M0DEBUG)
    printf("\n\nM0DEBUG was defined at compile time, so");
    printf("\nthe file system maintains a list of open files");
    printf("\nThis list is available via M0listOpenFiles().");
    printf("\nYour third file should not be listed as it was");
    printf("\nnot opened using the safe filesystem.");
#else
    printf("\n\nM0FLIST macro should not produce a file list");
    printf("\nsince M0DEBUG was not defined at compile time.");
#endif
    M0FLIST;
	
#if defined(M0DEBUG)
    printf("\nWith M0DEBUG defined at compile time, the file");
    printf("\nsystem can issue a warning if you attempt to");
    printf("\nclose a stream that you did not open.\n");
#else
    printf("\nTrying to close a stream that you don\'t own");
    printf("\nwill cause unpredictable results without the safe");
    printf("\nfile system active (M0DEBUG not defined at");
    printf("\ncompile time.\n");
#endif

    while( !yes("Ready to close files?") ) {
    };

    printf("\nNow closing the  first file you specified.");
    M0FCLOSE ( fp1 );
    printf("\nTry closing the  first file again.");
    M0FCLOSE ( fp1 );
    printf("\nNow closing the second file you specified.");
    M0FCLOSE ( fp2 );
#if defined(M0DEBUG)
    printf("\nM0FLIST should show no files open.");
#endif
    M0FLIST;

    printf("\n\nSafe file system stress test:\n");
    printf("Enter file name: ");
    gets(achFilename);
    fp1 = M0FOPEN( achFilename, "a+" );
    printf("Enter number of cycles: ");
    gets(achFilename);
    sscanf(achFilename, "%d", &ncycles);
    printf("Enter file name: ");
    gets(achFilename);

    for( icycle = 0; icycle < ncycles; icycle++ ) {
        fp2 = M0FOPEN( achFilename, "a+");
        printf("list head = %p\n", Filelog_head);
        M0FCLOSE(fp2);
    }
    M0FCLOSE(fp1);
    printf("\nChanging address of \"list head\" with time\n");
    printf("may mean a memory leak in file logging itself\n");

    M0FLIST;
    M0dumpMemInfo(stderr);
    
    printf("\nEnd of file system test\n\n");

    return(0);
}

#endif
	
#if defined(MEMSYS)

int main ( void )
{
    char	szBuf[BUFSIZ];		/* text input buffer	*/
    char	*szCopy;		/* copy of text		*/
    char	*szSecondCopy;		/* second copy of text	*/

    int		hcm	= 900630;	/* test constant	*
					 * (Hannah's b'day)	*/
    int		rtm	= 581227;	/* test constant (my	*
					 * birthday		*/
    int		*piSecond = NULL;	/* pointers to integer	*/
    int		*piVector = NULL;	/* vectors		*/
    int		*phcm	  = NULL;	/* location of 'hcm'	*/
    int		ncycles;		/* number of allocation	*
					 * cycles for stress	*
					 * test			*/
    int		icycle;			/* cycle counter	*/

    int 	newsize;		/* size of block	*/
    int 	sizeVector;		/* number of array	*
					 * elements		*/

    void	*pBlock;		/* pointer to arbitrary	*
					 * memory block		*/



    /* M0newMemory() test */
	
    printf("\nBeginning memory subsystem test");
    printf("\nTesting M0newMemory()" );

#if defined (M0DEBUG)
    printf("\nM0DEBUG was defined at compile time.");
    printf("\nM0newMemory validates the pointer and size.");
    printf("\nThe pointer must not point to an existing block.");
    printf("\nThe size must be greater than zero.");
    printf("\nInvalid arguments will cause an assertion.");
    printf("\nNewly allocated memory is filled with M0GARBAGE.");
#else
    printf("\nM0newMemory ship version test.");
#endif

    printf("\nEnter size of block (# ints) to allocate: ");
    fscanf(stdin,"%d",&sizeVector );

    if ( M0newMemory((void **)&piVector, sizeVector*sizeof(int)) ) {
	printf("\nAllocated. Pointer is %p", piVector);
	printf("\nFirst word is %p value is (hex) %x",
	  piVector, *piVector);
	printf("\nLast  word is %p value is (hex) %x",
	  piVector+sizeVector-1, *(piVector+sizeVector-1) );
        printf("\nPoking check values in first word and last word");
	piVector[0]		= rtm;
	piVector[sizeVector-1]	= hcm;
	printf("\nCheck values = %d %d", rtm, hcm );

	/* These should never fire. If they do, there is a bug
	 * in the memory management system */

	M0ASSERT( M0validPointer(piVector,             sizeof(int)));
	M0ASSERT( M0validPointer(piVector+sizeVector-1,sizeof(int)));

	printf("\nStored as    = %d %d", piVector[0],
	  piVector[sizeVector-1] );

	/* capture location of second check word to be sure that
	 * it gets 'shredded' if block is later shrunk */

	phcm = piVector+sizeVector-1;
    }
    else {
        printf("\nAllocation failed!" );
	return(-1);
    }
#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif


    /* M0resizeMemory() test */	

#if defined(M0DEBUG)
    printf("\n\nTesting M0resizeMemory");
    printf("\nM0DEBUG was defined at compile time.");
    printf("\nM0resizeMemory validates the pointer and size.");
    printf("\nThe pointer must point to an existing block.");
    printf("\nThe size must be greater than zero.");
    printf("\nInvalid arguments trigger an assertion.");
    printf("\nExpanding blocks always move. Newly allocated memory");
    printf("\nis filled with M0GARBAGE. Freed \'tail\' of shrinking");
    printf("\nblocks is filled with M0GARBAGE to cause unintended");
    printf("\nuse to fail during development");
#else
    printf("\M0resizeMemory() ship version test.");
#endif
    printf("\n\nEnter pointer and new size (# ints): " );
    fscanf(stdin,  "%p %d", &piVector, &newsize );
	
    if ( M0resizeMemory((void **)&piVector, newsize*sizeof(int)) ) {
	printf("\nResized. Pointer now %p", piVector);
	printf("\nFirst word should always contain check value.");
	printf("\nFirst word is %p value is %d",
	  piVector, *piVector);
	if ( newsize < sizeVector ) {
            printf("\nBlock shrunk, last check sum is lying out in");
	    printf("\nfreed memory.");
	    printf("\nValue at old address is (hex) %x or %d", *phcm);
	}
	else if ( newsize > sizeVector ) {
	    printf("\nBlock grew, last check sum is preserved");
	    printf("\nrelative to start of moved block.");
	    printf("\nOld location %p, new %p", phcm,
	      piVector+sizeVector-1);
	    printf("\nValue is (hex) %x or %d", piVector[sizeVector-1],
	      piVector[sizeVector-1] );
	    printf("\nEnd of resized block contains (hex) %x or %d",
	      piVector[newsize-1]);
	}
    }
    else {
	printf("\nResize failed!" );
	return(-2);
    }


    /* M0validPointer() test */

#if defined(M0DEBUG)
    printf("\n\nTesting M0validPointer");
    printf("\nM0validPointer requires the pointer to an object");
    printf("\nlies within a known memory block, and that the block");
    printf("\nis large enough to contain the object.");
    printf("\nMis-located or oversize objects return M0FALSE.");
    printf("\n\nEnter location and number of bytes in test object: ");
    fscanf(stdin,"%p %d", &piVector, &sizeVector );
	
    if ( M0validPointer ( (void *)piVector, sizeVector ) ) {
        printf("\nObject is located in known memory");
    }
    else {
	printf("\nObject is mislocated or too large!");
    }
#else
    printf("\n\nPointer validation requires that frame.c be");
    printf("\ncompiled with M0DEBUG defined");
#endif


    /* Memory leak and dangling pointer detection */

#if defined(M0DEBUG)
    printf("\n\nMemory leak/dangling pointer detection test.");
    printf("\n\nEnter size (# ints) for a second memory block: ");
    fscanf(stdin, "%d", &sizeVector );
    if ( M0newMemory((void **)&piSecond, sizeVector*sizeof(int)) ) {
	printf("\nAllocated");
    }
    else {
        printf("\nAllocation failed!");
	return(-3);
    }

    printf("\nCurrent memory status is shown. Leaks and dangling");
    printf("\npointers are detected by calling M0clearMemRefs()");
    printf("\nto clear flags.");

    M0clearMemRefs();
    M0dumpMemInfo(stdout);

    printf("\nAll \'referenced\' flags should test \'N\'");

    printf("\n\nNow enter block pointers as prompted. If you enter");
    printf("\na pointer that does not reference the start of an");
    printf("\nactive block (simulating a \'dangling pointer\' to");
    printf("\nmemory that has been freed) M0noteMemRef() will assert.");
    printf("\nTo simulate a memory leak, do not enter values for");
    printf("\nall the pointers listed, as though your application");
    printf("\nallocated memory and then lost the pointer. Here the");
    printf("\npointers are entered manually; for real development");
    printf("\na guy could write a routine that would first call");
    printf("\nM0clearMemRefs(), then traverse all of the dynamic");
    printf("\nstructures in the application and call M0noteMemRefs()");
    printf("\non each to find dangling pointers, then call");
    printf("\nM0checkMemRefs() to be sure all blocks are");
    printf("\naccounted for.");

    while(M0TRUE) {
        printf("\nEnter pointer (or return): ");
        fflush(stdin);
	fgets(szBuf,BUFSIZ,stdin);
	if ( szBuf[0] == '\n' )
	    break;
	sscanf(szBuf,"%p",&piVector);
	M0noteMemRef(piVector);     /* assert if not active */
    }

    printf("\nNow identify memory leaks M0checkMemRefs()");
    printf("\nAn assertion means one or more active blocks");
    printf("\nwere not marked as \'referenced\' by a call to");
    printf("\nM0noteMemRef()");

    M0checkMemRefs();		/* assert if you missed one */

#else

    printf("\n\nData structure integrity check with M0clearMemRefs(),");
    printf("\nM0noteMemRef(), and M0checkMemRefs() requires that");
    printf("\nframe.c be compiled with M0DEBUG defined.");

#endif

    /* pointer freeing tests */

    printf("\n\nAs prompted, enter pointers to blocks you");
    printf("\nwish to free.");
#if defined(M0DEBUG)
    printf("\nSince frame.c was compiled with M0DEBUG defined,");
    printf("\nattempts to free a pointer that does not actually");
    printf("\nreference an allocated memory block will assert.");
#endif

    while(M0TRUE) {
        printf("\nEnter pointer (return to quit): ");
        fflush(stdin);
	fgets(szBuf,BUFSIZ,stdin);
	if ( szBuf[0] == '\n' )
	    break;
	sscanf(szBuf,"%p",&piVector);
	M0freeMemory((void **)&piVector);
    }
#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif



    /* String duplication string */

    printf("\n\nM0strdup() test:\n");
    printf("\nEnter a string. I will make two copies and free them\n");
    gets(szBuf);

    szCopy 		= M0strdup(szBuf);
    szSecondCopy	= M0strdup(szCopy);
    printf("\n");
    printf("Entered       = \"%s\"\n", szBuf);
    printf("First  copy   = \"%s\"\n", szCopy);
    printf("Second copy   = \"%s\"\n", szSecondCopy);
#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif

    while( ! yes("Ready to free them?") ) {
    };

    M0freeMemory( (void **)&szCopy);
    M0freeMemory( (void **)&szSecondCopy);
    printf("\nCopies of constant string should now be freed\n");
#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif

    printf("\n\nStress test:\n");
    printf("Enter block size (bytes) and number of "
      "allocation cycles: ");
    fflush(stdin);
    fgets(szBuf,BUFSIZ,stdin);
    sscanf(szBuf,"%d %d", &newsize, &ncycles);
    M0newMemory( (void **)&pBlock, newsize);
    for( icycle = 0; icycle < ncycles; icycle++ ) {
        M0resizeMemory( (void **)&pBlock, newsize+icycle);
        printf("cycle=%d address=%p size=%d\n",
          icycle, pBlock, newsize+icycle);
    }
    M0freeMemory( (void **)&pBlock);
    printf("If block address is continually increasing, it is\n");
    printf("possible that the memory logging subsystem itself\n");
    printf("has a memory leak\n");
     
    printf ("\n\nEnd of memory subsystem test\n\n" );
    return(0);
}

#endif




#if defined(MCSTRING)

int test_(char *fstring, int flen)
{
    char	*cstring;

    /* FORTRAN string conversion test */

    cstring = M0fsMemory(fstring,flen);
    printf("C form of FORTRAN string   (%s)\n", cstring );
#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif

    return(0);
}

#endif


#if defined (STRCAT)


int main ( int argc, char *argv[] )
{
    size_t	cStrings = 3;
    size_t	i, len;
    char	**aStrings = NULL;
    char	*BigString = NULL;

    /* string concatenation test */

    printf("\n\nBeginning string concatenation test.");
    printf("\nAllocating storage space for %d string pointers.",
      cStrings);
    if(M0newMemory((void **)&aStrings,cStrings*sizeof(char *)) ) {
	printf("\n%d char* allocated", cStrings);
    }
    else {
	printf("\nAllocation of %d char* failed", cStrings);
	return(-4);
    }
    printf("\nAllocating %d string buffers.");
    for(i=0; i<cStrings; i++) {
	if(M0newMemory((void **)&aStrings[i],BUFSIZ) ) {
	    printf("\nString buffer %d allocated.",i);
	}
	else {
	    printf("\nCould not allocate string buffer %d",i);
	    return(-4);
	}
    }
#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif
    for(i=0; i<cStrings; i++ ) {
	printf("\nEnter string      %2.2d: ",i);
	fgets(aStrings[i],BUFSIZ-1,stdin);
	len = strlen(aStrings[i]);
	aStrings[i][len-1]='\0';
	printf("String (\\n stripped): %s", aStrings[i] );
    }
    BigString = M0strMemory( aStrings[0], aStrings[1], aStrings[2] );
    if ( BigString == (char *)0 ) {
	printf("\nM0strMemory allocation failed!");
	return(-5);
    }
    else {
	printf("\n\nBigString: %s\n", BigString);
    }

#if defined(M0DEBUG)
    M0dumpMemInfo(stdout);
#endif

    printf ("\n\nEnd of test of safe \'stralloc()\' M0strMemory()\n");
    return(0);
}

#endif
