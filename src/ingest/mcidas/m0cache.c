/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0CACHE.C 1-May-95,16:24:52,`DWS' open file cache (5402)                */
/* 2 M0CACHE.C 9-May-95,18:14:10,`DWS' MCPATH phase 3 (5429)                 */
/* 3 M0CACHE.C 6-Jun-95,15:06:28,`USER' Released                             */
/* 4 M0CACHE.C 9-Jun-95,10:35:46,`JMB' performance for XCD (5498)            */
/* 5 M0CACHE.C 13-Jun-95,11:36:56,`USER' Released                            */
/* 6 M0CACHE.C 19-Feb-96,15:58:00,`DWS' reglue: modified file                */
/* 7 M0CACHE.C 20-Feb-96,11:57:38,`USER' Released                            */
/**** McIDAS Revision History *** */

#include <assert.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

#include "mcidas.h"
#include "mcidasp.h"


/* collect the file name and file descriptor together in a structure */
struct Fent {
        char           *filename;
        char           *pathname;
        int             fd;
        struct Fent    *next;
};

typedef struct Fent fent;

/* This is the maximum number of file descriptors we will use */
#define MAX_FD 15


static fent    *list = 0;       /* head pointer for list of open files */
static int      listend = 0;    /* number of current list entries */

static int      fdcom1 = -1;    /* com ports for os2              */
static int      fdcom2 = -1;
static int      fdcom3 = -1;


/**
*| Name:
*|      M0cacheopen  - return file descriptor for a named file
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      int
*|      M0cacheopen(const char *filename)
*|
*| Input:
*|      filename - name of file
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      <0      - error
*|      >=0     - success, integer file descriptor
*|
*| Remarks:
*|      The Fortran version of this routine is m0cacheopen().
*|
*| Categories:
*|      file
*/

int
M0cacheopen(const char *file)
{
        const char     *path;   /* path name for file name */
        int             fd;
        fent           *entry;  /* current file entry being processed */

        if(!file) return -1;

#ifdef  __EMX__                 /* true in os/2 only  */
        /* special case of com ports */
        if (strcmp(file, "COM1") == 0) {
                if (fdcom1 == -1) {
                        fdcom1 = open(file, O_RDWR);
                }
                return fdcom1;
        }
        if (strcmp(file, "COM2") == 0) {
                if (fdcom2 == -1) {
                        fdcom2 = open(file, O_RDWR);
                }
                return fdcom2;
        }
        if (strcmp(file, "COM3") == 0) {
                if (fdcom3 == -1) {
                        fdcom3 = open(file, O_RDWR);
                }
                return fdcom3;
        }
#endif                          /* __EMX__  */


        /* see if the name is already open */
        for (entry = list; entry; entry = entry->next) {
                if (strcmp(file, entry->filename) == 0) {
                        return entry->fd;
                }
        }

        /* we did not find the file in the cache, so we will need its path */
        path = Mcpathname(file);
        if(path == 0) return -1;


        /*
         * see if there are too many names in use. if there are, the oldest
         * one goes.  Don't worry, if it is still active, it will come back
         * quickly, and soon enough there will be an inactive entry thrown
         * away instead, (if possible)
         */
        if (listend == MAX_FD) {
		off_t	eof;

                eof = lseek(list->fd, (off_t)0, SEEK_END);      /* find end of file */
                close(list->fd);

                /* if the file doesnt have at least 1 byte, trash it */
                if (eof == 0) {
                        remove(list->pathname);
                }
                free(list->filename);
                free(list->pathname);
                entry = list;
                list = list->next;
                free(entry);
                listend--;

        }                       /* end of deleting the oldest entry */
        /* now we can add the new entry to the list */
        if (list == (fent *) 0) {
                list = (fent *) malloc(sizeof(fent));
		assert(list);
                entry = list;
        } else {
                entry = list;
                while (entry->next != 0)
                        entry = entry->next;
                entry->next = (fent *) malloc(sizeof(fent));
		assert(entry->next);
                entry = entry->next;
        }                       /* end of allocating an entry */

        /* keep a copy of the filename */
        entry->filename = stralloc(file, (char *)0);
        assert(entry->filename != 0);
        entry->pathname = stralloc(path,(char *)0);
        assert(entry->pathname != 0);
        fd = open(entry->pathname, O_RDWR | O_CREAT, 0666);
        if (fd < 0)
                fd = open(entry->pathname, O_RDONLY);

#ifdef FD_CLOEXEC
        /* set the close_on_exec bit  if it is implemented */
        {
                int             flags;

                flags = fcntl(fd, F_GETFD, 0);
                flags = flags | FD_CLOEXEC;
                (void) fcntl(fd, F_SETFD, flags);
        }
#endif

        /* insert into data structure */
        entry->fd = fd;
        entry->next = 0;
        listend++;

        return fd;
}


/**
*| Name:
*|      m0cacheopen - return file descriptor for a named file
*|
*| Interface:
*|      integer function
*|      m0cacheopen(character*(*) file)
*|
*| Input:
*|      filename - name of file
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      <0      - error
*|      >=0     - success, integer file descriptor
*|
*| Remarks:
*|      The C version of this routine is M0cacheopen().
*|
*| Categories:
*|      file
*/

Fint
m0cacheopen_(char *file, FsLen length)
{
        char           *path;
        Fint            fd;

        path = fsalloc(file, length);
        fd = M0cacheopen(path);
        free(path);
        return fd;
}


/*
*| Name:
*|	kilcom - closes serial communication ports in OS/2
*|
*| Interface:
*|	subroutine
*|	kilcom()
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
*|	Useful only in OS/2.
*|
*| Categories: 
*|	file 
*/

void
kilcom_(void)
{
        if (fdcom1 != -1)
                close(fdcom1);
        if (fdcom2 != -1)
                close(fdcom2);
        if (fdcom3 != -1)
                close(fdcom3);
        fdcom1 = fdcom2 = fdcom3 = -1;
}

/**
*| Name:
*|      M0cacheflush - close all open files, delete those of 0 length
*|
*| Interface:
*|      #include "mcidasp.h"
*|
*|      void
*|      M0cacheflush(void)
*|
*| Input:
*|      none
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      none
*|
*| Remarks:
*|      This is used at process termination by the McIDAS library.
*|      The Fortran version of this routine is lwmop().
*|
*| Categories:
*|      file
*/

void
M0cacheflush(void)
{
        fent           *entry;

        while (list != 0) {
		off_t             eof;

                eof = lseek(list->fd, (off_t)0, SEEK_END);
                close(list->fd);
                if (eof == 0) {
                        remove(list->pathname);
                }
                free(list->filename);
                free(list->pathname);
                entry = list;
                list = list->next;
                free(entry);
        }


        listend = 0;
}


/**
*| Name:
*|      lwmop - close all open files, delete those of 0 length
*|
*| Interface:
*|      subroutine
*|      lwmop()
*|
*| Input:
*|      none
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|     none
*|
*| Remarks:
*|      This is used at process termination by the McIDAS library.
*|      The C version of this routine is M0cacheflush().
*|
*| Categories:
*|      file
*/

void
lwmop_(void)
{
        M0cacheflush();
}

/*
*| Name:
*|	m0cachedump - prints contents of mcidas filesystem cache
*|
*| Interface:
*|	subroutine
*|	m0cachedump()
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
*|	This routine is for debugging purposes only.
*|
*| Categories: 
*|	file 
*/

void
m0cachedump_(void)
{
	fent           *entry;	/* current file entry being processed */

	Mceprintf("CACHEDUMP: cache has %d entries:\n", listend);
	for (entry = list; entry; entry = entry->next)
	{
		Mceprintf("CACHEDUMP: %2d %s\n",
				entry->fd, entry->pathname);
	}
}

/**
*$ Name:
*$      Mcremove - remove a file
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      Mcremove(const char *name)
*$
*$ Input:
*$      name    - name of file to remove
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       0      - success, removed
*$      <0      - failure, not removed
*$
*$ Remarks:
*$      The Fortran version of this routine is lwd().
*$
*$ Categories: 
*$      file 
*/

int
Mcremove(const char *file)
{
        char           *path;   /* remember pathname of file to remove */
	fent           *entry;	/* current file entry being processed */
	fent           *follow;	/* pointer which follows one entry
				 * back */
        if(!file) return -1;

        path=0;
	
	/* remove open file descriptor for this file if there is one */
	follow = 0;
	for (entry = list; entry; entry = entry->next)
	{
		if (strcmp(file, entry->filename) == 0)
		{
			close(entry->fd);
			free(entry->filename);
			path=entry->pathname;

			/*
			 * remove entry from beginning or middle of
			 * list
			 */
			if (follow == 0)
			{
				list = entry->next;
			}
			else
			{
				follow->next = entry->next;
			}

			free(entry);
			listend--;
			break;	/* terminate the for() */
		}

		/*
		 * remember this one, may need it to connect to n+2
		 */
		follow = entry;
	}

	/*
	 * now we don't have an open fd for this bugger, so we can
	 * delete it
	 */
     if(path==0){
        /* file had not been referenced; just find current path bindings */
	return remove(Mcpathname(file));
    } else {
        /* we had a copy of this pathname; that's the one to delete */
        int rc;

        rc = remove(path);
        free(path);
        return rc;
    }
}

/**
*$ Name:
*$      lwd - remove a file
*$
*$ Interface:
*$      integer function
*$      lwd(character*(*) name)
*$
*$ Input:
*$      name    - name of file to remove
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       0      - success, removed
*$      <0      - failure, not removed
*$
*$ Remarks:
*$      The C version of this routine is Mcremove().
*$
*$ Categories: 
*$      file 
*/

Fint
lwd_(char *file, FsLen len)
{
	char           *this;	/* c-string for this file name */
	Fint		ans;

        this = fsalloc(file, len);
	if(!this)
		return -1;
        ans = Mcremove(this);
        free(this);
	return ans;
}
