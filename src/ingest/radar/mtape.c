/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */
/*
 * 
 * This file contains the following C routines
 * 
 * mt_backspace
 * mtclose_
 * mtfcre_
 * mtfmnt_
 * mtfname_
 * mtmnt_
 * mtgdev_
 * mtmntw_
 * mtskpf_
 * mtsdev_
 * mtspce_
 * mtread_
 * mtrwd_
 * mtwait_
 * mtweof_
 * mtwrit_
 * mtxdev_
 * 
 */


# define EXABYTE "/dev/nexa"
# define NINE_TRACK "/dev/ntape"
# define CARTRIDGE "/dev/ncartM"

# include <sys/types.h>
# include <sys/fcntl.h>
# include <sys/file.h>
# include <sys/mtio.h>
# include <stdio.h>
# include <unistd.h>

static char *MTdev=NULL;

# ifdef titan

/*
 * The titan uses string descriptors, which drastically confuses the way
 * that we handle characters here.  This routine tries to figure out whether
 * we are seeing a descriptor or a straight pointer, and does the right thing.
 */

struct titan_descr
{
	char *string;
	int len;
};
# define mtclose_  	MTCLOSE
# define mtfcre_  	MTFCRE
# define mtfmnt_  	MTFMNT
# define mtfname_  	MTFNAME
# define mtmnt_  	MTMNT
# define mtgdev_  	MTGDEV
# define mtmntw_  	MTMNTW
# define mtsdev_  	MTSDEV
# define mtskpf_  	MTSKPF     
# define mtspce_  	MTSPCE     
# define mtread_  	MTREAD     
# define mtrwd_   	MTRWD      
# define mtwait_  	MTWAIT     
# define mtweof_  	MTWEOF     
# define mtwrit_  	MTWRIT     
# define mtxdev_  	MTXDEV

# include <sys/ioctl.h>
# include <sys/fcntl.h>
# undef MTWEOF			/* should be after the mtio.h include */

# endif


# ifdef ultrix
# include <sys/ioctl.h>
# endif


# ifdef sun
# include <sys/ioccom.h>
# endif


# define NDRIVE 20	/* Must be bigger than 8 (to use device /dev/rmt8) */
static int Fds[NDRIVE] = { 0 };
static int Status[NDRIVE] = { 0 };
static int Nwords[NDRIVE] = { 0 };
static int Tape[NDRIVE] = { 0 };
static int DevType[NDRIVE] = { 0 };
# define NCARTM 1
# define CART_BLOCK 512
/*
 * The old status codes from way back in the dark ages.
 */
# define TS_OK	0
# define TS_EOF	1
# define TS_ERROR 2
# define TS_EOT 3

/* c----------------------------------------------------------------------- */
/* begin mtape.c */
/* c----------------------------------------------------------------------- */

mt_backspace (unit, nskip)
  int unit, nskip;
  /*
   * Perform a file backspace.
   */
{
    long int back;
    
    if( lseek (Fds[unit], 0, SEEK_CUR) < 8 ) /* at BOT */
	 return;

    for (; nskip > 0; nskip--){	/* record size at beginning and end */
	lseek (Fds[unit], -sizeof(back), SEEK_CUR);
	read (Fds[unit], &back, sizeof(back));
# ifdef ultrix
	back = swap32( back );
# endif
	if( lseek (Fds[unit], -(back+2*sizeof(back)), SEEK_CUR) < 8 )
	     return;
    }
}
/* c----------------------------------------------------------------------- */

int mtclose_ (unit)
  int *unit;
  /*
   * Close a file "tape"
   */
{
    if (Fds[*unit])
	 close (Fds[*unit]);
    return (1);
}
/* c----------------------------------------------------------------------- */

int mtfcre_ (unit, file)
  int *unit;
  char *file;
  /*
   * Create and mount a file "tape"
   */
{
    char realfile[100];
    char defdir[30], *getenv ();
    /*
     * If something is already open, close it.
     */
    if (Fds[*unit])
	 close (Fds[*unit]);
    /*
     * Put together the file name.
     */
    if (getenv ("USER"))
	 sprintf (defdir, "/dt/%s", getenv ("USER"));
    else
	 strcpy (defdir, ".");
    mtfname ("SCRATCH", defdir, file, realfile, ".tape");
    printf (" Real file is '%s' unit %d\n", realfile, *unit);
    /*
     * Attempt to open it.
     */
    if ((Fds[*unit] = open (realfile, O_CREAT | O_TRUNC | O_RDWR, 0666)) < 0)
	 {
	     printf (" Unable to create file '%s'\n", realfile);
	     perror ("Open error");
	     return (0);
	 }
    MTdev = realfile;
    Tape[*unit] = 0;
    return (1);
}
/* c----------------------------------------------------------------------- */

int mtfmnt_ (unit, file)
  int *unit;
  char *file;
  /*
   * Mount a file "tape"
   */
{
    char realfile[100];
    char defdir[30], *getenv ();
# ifdef titan
    char *titan_string();

    file = titan_string (file);
# endif
    /*
     * If something is already open, close it.
     */
    if (Fds[*unit])
	 close (Fds[*unit]);
    /*
     * Put together the file name.
     */
    if (getenv ("USER"))
	 sprintf (defdir, "/dt/%s", getenv ("USER"));
    else
	 strcpy (defdir, ".");
    mtfname ("SCRATCH", defdir, file, realfile, ".tape");
    printf (" Real file is '%s' unit %d\n", realfile, *unit);
    /*
     * Attempt to open it.
     */
    if ((Fds[*unit] = open (realfile, O_RDONLY)) < 0)
	 {
	     printf (" Unable to open file '%s'\n", realfile);
	     perror ("Open error");
	     return (0);
	 }
    MTdev = realfile;
    Tape[*unit] = 0;
    return (1);
}
/* c----------------------------------------------------------------------- */

mtfname(env, def, file, dest, type)
char *env, *def, *file, *dest, *type;
/*
 * This is a clone of Jon's fixdir_t routine
 *
 * Translate an environment variable, and qualify the
 * given FILE by the result.  If there is no translation for
 * ENV, the DEF name, if non-null, will be used.
 * The result is put into DEST.
 * Also, if a file lacks a type string, add it.
 */
{
	char *trans, *getenv (), *strchr (), *strrchr (), *slash;
/*
 * First of all, look at the file name.  If it starts with a slash,
 * we simply take it as it is.
 */
	if (file[0] == '/')
		strcpy (dest, file);
/*
 * If the environment variable translates, use it.
 */
 	else if (trans = getenv (env))
	{
		strcpy (dest, trans);
		strcat (dest, "/");
		strcat (dest, file);
	}
/*
 * Failing that, copy the def if it exists, then put in the file.
 */
 	else if (def)
	{
		strcpy (dest, def);
		strcat (dest, "/");
		strcat (dest, file);
	}
	else
		strcpy (dest, file);
/*
 * Look for an extension, in a rather simple sort of way.
 * Look for the last slash (strrchr) then
 * Look for the absence of a dot (strchr) after the last slash otherwise
 * append the type string.
 */
 	if ((slash = strrchr (dest, '/')) == 0)
	     slash = dest;
	if (! strchr (slash, '.'))
	     if( strlen(type))
		  strcat (dest, type);
}
/* c----------------------------------------------------------------------- */

void mtgdev_( dev, n )		/* get the last device name */
  char *dev;
  int *n;
{
# ifdef titan
    char *titan_string();
    dev = titan_string (dev);
# endif

    strcpy( dev, MTdev );
    *n = strlen( MTdev );
}

/* c----------------------------------------------------------------------- */

int mtmnt_ (unit)
  int *unit;
  /*
   * Mount a tape drive.
   */
{
    char fname[80];
    char *getenv();
    
    if (Fds[*unit])
	 close (Fds[*unit]);

    if ( MTdev != NULL )
         strcpy( fname, MTdev );
    else
	 sprintf (fname, "/dev/nrmt%d", *unit);
    /*
    printf( "Opening input device %s\n", fname );
     */

    if ((Fds[*unit] = open (fname, O_RDONLY)) < 0)
	 {
	     perror (fname);
	     Fds[*unit] = 0;
	     return (0);
	 }

    if( strcmp(fname, CARTRIDGE ) == 0 )
	  DevType[*unit] = NCARTM;
    Tape[*unit] = 1;
    return (1);
    
}
/* c----------------------------------------------------------------------- */

int mtmntw_ (unit)
  int *unit;
  /*
   * Mount a tape drive with read/write access
   */
{
    char fname[80];
    
    if (Fds[*unit])
	 close (Fds[*unit]);

    if ( MTdev != NULL )
         strcpy( fname, MTdev );
    else
	 sprintf (fname, "/dev/nrmt%d", *unit);

    printf( "Opening output device %s\n", fname );

    if ((Fds[*unit] = open (fname, O_RDWR)) < 0)
	 {
	     perror (fname);
	     Fds[*unit] = 0;
	     return (0);
	 }
    if( strcmp(fname, CARTRIDGE ) == 0 )
	  DevType[*unit] = NCARTM;
    Tape[*unit] = 1;
    return (1);
}
/* c----------------------------------------------------------------------- */

void mtsdev_( dev )		/* set the device name */
  char *dev;
{
# ifdef titan
    char *titan_string();
    dev = titan_string (dev);
# endif

    MTdev = dev;
}
/* c----------------------------------------------------------------------- */

mtskpf_ (unit, nskip)
  int *unit, *nskip;
  /*
   * Perform a file skip.
   */
{
    struct mtop op;
    int f, n, rlen;
    long recl, back;
    /*
     * No file skips for file data yet.
     */

    if( *nskip == 0 ) {
	Nwords[*unit] = *nskip;
	Status[*unit] = TS_OK;
    }
    /*
     * For tapes, we pass it on to the driver.
     */
    else if (Tape[*unit]) 
	 {
	     if (*nskip > 0)
		  {
		      op.mt_op = MTFSF;
		      op.mt_count = *nskip;
		  }
	     else
		  {
		      op.mt_op = MTBSF; /* SUN specific */
		      op.mt_count =  - (*nskip);
		  }		
	     if (ioctl (Fds[*unit], MTIOCTOP, &op) < 0 )
		  perror("mtskpf");
	     Nwords[*unit] = *nskip;
	     Status[*unit] = TS_OK;
	 }
    else if( *nskip > 0 ) {
	Nwords[*unit] = 0;
	Status[*unit] = 0;
	
	for( f=0; f < *nskip; f++ ) {
	    for(;;) {
		rlen = read( Fds[*unit], &recl, sizeof(recl));
# ifdef ultrix
		recl = swap32( recl );
# endif
		if( rlen < sizeof(recl)) {
		    Status[*unit] = TS_EOT;
		    return;
		}
		lseek (Fds[*unit],
		       (off_t) recl + sizeof (recl), SEEK_CUR);
		if( recl == 0 )
		     break;
	    }
	}
    }
    else if( *nskip < 0 ) {
	/*
	printf ("\n\tBACKWARD FILE SKIPS DON'T WORK ON FILE DATA YET!\n\n");
	 */
	Nwords[*unit] = 0;
	Status[*unit] = 0;
	n = -(*nskip);
	for( f=1; f <= n; f++ ) {
	    if( lseek (Fds[*unit], 0, SEEK_CUR) < 8 ) /* at BOT */
		 return;
	    for(;;) {
		lseek (Fds[*unit], -sizeof(back), SEEK_CUR);
		read (Fds[*unit], &back, sizeof(back));
# ifdef ultrix
		back = swap32( back );
# endif
		/*
		 * if EOF leave positioned after the EOF
		 */
		if( back == 0 && f == n ) 
		     return;

		if( lseek (Fds[*unit], -(back+2*sizeof(back)), SEEK_CUR) < 8 )
		     return;

		if( back == 0 )	/* EOF! */
		     break;
	    }
	}
    }
}
/* c----------------------------------------------------------------------- */

mtspce_ (unit, nskip)
  int *unit, *nskip;
  /*
   * Perform a tape skip.
   */
{
    struct mtop op;
    int ns = *nskip;
    /*
     * Skips have to be done by simply reading through the data.
     */

    if( *nskip == 0 ) {
	Nwords[*unit] = *nskip;
	Status[*unit] = TS_OK;
    }
    else if (! Tape[*unit])
	 {
	     long int recl, rlen;
	     if (ns < 0)
		  mt_backspace (*unit, -ns);
	     else
		  for (; ns > 0; ns--) {
		      rlen = read (Fds[*unit], &recl, sizeof(recl));
		      if( rlen < sizeof(recl)) {
			  Status[*unit] = TS_EOT;
			  return;
		      }
# ifdef ultrix
		      recl = swap32( recl );
# endif
		      lseek (Fds[*unit],
			     (off_t) recl + sizeof (recl), SEEK_CUR);
		      Status[*unit] = recl ? 0 : TS_EOF;
		      if( recl == 0 )
			   return;
		  }
	     Nwords[*unit] = Status[*unit] = 0;
	 }
    /*
     * For tapes, we pass it on to the driver.
     */
    else
	 {
	     if (*nskip > 0)
		  {
		      op.mt_op = MTFSR;
		      op.mt_count = *nskip;
		  }
	     else
		  {
		      op.mt_op = MTBSR;
		      op.mt_count =  - (*nskip);
		  }		
	     if (ioctl (Fds[*unit], MTIOCTOP, &op) < 0 )
		  perror("mtspce");
	     Nwords[*unit] = *nskip;
	     Status[*unit] = TS_OK;
	 }
}
/* c----------------------------------------------------------------------- */

mtread_ (unit, buffer, len)
  int *unit, *len;
  char *buffer;
  /*
   * Perform a tape read.
   */
{
    long int recl = 0, rlen, back, really=*len*2;
    char *cp;
    
    if (Tape[*unit])
	 {
	     if( DevType[*unit] == NCARTM ) {
		 /*
		  * make the request an integer multiple of the
		  * cartridge block size
		  */
		 really = ((really-1)/CART_BLOCK +1)*CART_BLOCK;
	     }
	     rlen = read (Fds[*unit], buffer, really);
	     if (rlen == 0)   	/* File mark */
		  {
		      for( cp=buffer; cp < (buffer + really);)
			   *cp++ = 0;
		      Status[*unit] = TS_EOF;
		  }
	     else if (rlen < 0)
		  {
		      perror ("Tape read error");
		      Status[*unit] = TS_ERROR;
		  }
	     Nwords[*unit] = rlen/2;
	 }
    else
	 {
	     /*
	      * Read in the record length.
	      */
	     rlen = read (Fds[*unit], &recl, sizeof(recl));
# ifdef ultrix
	     recl = swap32( recl );
# endif
	     if ( rlen < sizeof(recl))
		  {
	      printf (" End Of Tape  %d\n", rlen );
		      Status[*unit] = TS_EOT;
		      Nwords[*unit] = 0;
		      return;
		  }
	     /*
	      * Now get the actual data.
	      */
	     if( recl )
		  if (read (Fds[*unit], buffer, recl) < recl)
		       printf (" Funny, %d byte read returned less.\n", recl);
	     read (Fds[*unit], &back, sizeof (back));
	     Status[*unit] = recl ? TS_OK : TS_EOF;
	     Nwords[*unit] = recl/2;
	 }
}
/* c----------------------------------------------------------------------- */

mtrwd_ (unit)
  int *unit;
  /*
   * Perform a rewind.
   */
{
    
    if (Tape[*unit])
	 {
	     struct mtop op;
	     op.mt_op = MTREW;
	     op.mt_count = 1;	/* Only rewind once */
	     if (ioctl (Fds[*unit], MTIOCTOP, &op) < 0)
		  {
		      perror ("Rewind");
		      Status[*unit] = TS_ERROR;
		  }
	     else
		  Status[*unit] = TS_OK;
	     Nwords[*unit] = 0;
	 }
    else
	 {
	     lseek (Fds[*unit], (long) 0, 0);
	     Status[*unit] = Nwords[*unit] = 0;
	 }
}
/* c----------------------------------------------------------------------- */

mtwait_ (unit, status, nword)
  int *unit, *status, *nword;
  /*
   * Perform a "wait" for the last operation.
   */
{
    *status = Status[*unit];
    *nword = Nwords[*unit];
    Status[*unit] = Nwords[*unit] = 0;
}
/* c----------------------------------------------------------------------- */

# ifdef titan
# undef MTWEOF
# endif

mtweof_ (unit)
  int *unit;
  /*
   * Write an EOF.
   */
{
    int drlen=0;
    off_t back;

    if (Tape[*unit])
	 {
	     struct mtop op;
# ifdef titan
	     op.mt_op = 0;	/* MTWEOF from sys/mtio.h */
# else
	     op.mt_op = MTWEOF;
# endif
	     op.mt_count = 1;
	     if (ioctl (Fds[*unit], MTIOCTOP, &op) < 0)
		  {
		      perror ("Mag tape WEOF");
		      Status[*unit] = TS_ERROR;
		  }
	     else
		  Status[*unit] = TS_OK;
	     Nwords [*unit] = 0;
	 }
    else
	 {			/* just write two 4 byte zeroes */
	     write (Fds[*unit], &drlen, sizeof(drlen));
	     write (Fds[*unit], &drlen, sizeof(drlen));
	     Status[*unit] = TS_OK;
	     Nwords[*unit] = 0;
	 }
}
/* c----------------------------------------------------------------------- */

mtwrit_ (unit, buffer, len)
  int *unit, *len;
  char *buffer;
  /*
   * Perform a tape write.
   */
{
    long int rlen, really;
    
    rlen = really = *len*2;
    if (Tape[*unit])
	 {
	     if( DevType[*unit] == NCARTM ) {
		 /*
		  * make the request an integer multiple of the
		  * cartridge block size
		  */
		 really = ((really-1)/CART_BLOCK +1)*CART_BLOCK;
	     }
	     rlen = write (Fds[*unit], buffer, really);
	     if (rlen < 0)
		  {
		      perror ("Tape write");
		      Status[*unit] = TS_ERROR;
		  }
	     Nwords[*unit] = rlen;
	 }
    else
	 {
# ifdef ultrix
	     really = swap32(really);
# endif
	     /*
	      * Write out the record length before and after.
	      */
	     write (Fds[*unit], &really, sizeof(really));
	     /*
	      * Now write the actual data.
	      */
	     if (write (Fds[*unit], buffer, rlen) < rlen)
		  printf (" Funny, %d byte write returned less.\n", rlen);
	     write (Fds[*unit], &really, sizeof(really));
	     Status[*unit] = TS_OK;
	     Nwords[*unit] = *len;
	 }
}
/* c----------------------------------------------------------------------- */


void mtxdev_( dev )
  char *dev;
{
    /*
     * set the device name for the next tape mount
     */

# ifdef titan
    char *titan_string();
    dev = titan_string (dev);
# endif

    if( strcmp( dev, "EXABYTE" ) == 0 )
	 mtsdev_(EXABYTE);
    if( strcmp( dev, "NINE_TRACK" ) == 0 )
	 mtsdev_(NINE_TRACK);
    if( strcmp( dev, "CARTRIDGE" ) == 0 )
	 mtsdev_(CARTRIDGE);
}
/* c----------------------------------------------------------------------- */
/* end mtape.c */
/* c----------------------------------------------------------------------- */


