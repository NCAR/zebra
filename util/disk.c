/* 11/84 jc */
/* $Id: disk.c,v 1.2 1990-01-03 10:00:28 corbet Exp $ */
/*
 * Disk handling.
 *
 * Changes made 1/88 jc, for unix.  These routines are usable for text
 * files ONLY under unix.
 */
# ifdef VMS
# include <rmsdef.h>
# include <fab.h>
# include <rab.h>
# define LTYPE struct RAB *
# endif

# ifdef UNIX
# include <stdio.h>
# include <errno.h>
# define LTYPE long
FILE *fopen ();
long ftell (), Offset;
# endif

# define FALSE 0



static char *Def_name = "SYS$SCRATCH:TAPE.TAP";
static char Def_buf[200];
char *malloc ();



dsetdef (name)
char *name;
/*
 * Set the default file name to "name".  Useful under VMS only.
 */
{
	strcpy (Def_buf, name);
	Def_name = Def_buf;
}




LTYPE
dcreate (file)
char *file;
/*
 * Try to create the given file.
 */
{
# ifdef VMS
	struct FAB *f;
	struct RAB *r;
	int status;
/*
 * Allocate space for the RMS structures.
 */
	if ((f = (struct FAB *) malloc (sizeof (struct FAB))) == 0)
		c_panic ("Out of memory for FAB allocate.");
	if ((r = (struct RAB *) malloc (sizeof (struct RAB))) == 0)
		c_panic ("Out of memory for RAB allocate.");
/*
 * Fill in the FAB.
 */
	*f = cc$rms_fab;
	f->fab$l_fna = file;
	f->fab$b_fns = strlen (file);
	f->fab$l_dna = Def_name;
	f->fab$b_dns = strlen (Def_name);
	f->fab$b_fac = FAB$M_PUT;
	f->fab$l_fop = FAB$M_TEF;
	f->fab$b_rat = FAB$M_CR;
	f->fab$b_rfm = FAB$C_VAR;
/*
 * Attempt to create the file
 * (10/9/85 jc)	Check for RMS$_FILPURGED for version-limited files/directories.
 */
	if ((status = sys$create (f)) != RMS$_NORMAL &&
		status != RMS$_FILEPURGED)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Fill in the RAB.
 */
	*r = cc$rms_rab;
	r->rab$l_fab = f;
	r->rab$l_rop = RAB$M_EOF | RAB$M_WBH;
	r->rab$b_mbf = 4;	/* We LIKE multibuffering	*/
/*
 * Connect to the FAB.
 */
	if ((status = sys$connect (r)) != RMS$_NORMAL)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Return a pointer to the RAB.
 */
	return (r);
# endif

# ifdef UNIX
	FILE *fd;

	if ((fd = fopen (file, "w")) == NULL)
	{
		perror (file);
		printf ("\n");
		return (FALSE);
	}
	return ((LTYPE) fd);
# endif
}


# ifdef VMS

dcreate_nra (file)
char *file;
/*
 * Try to create the given file with no record attributes.
 */
{
	struct FAB *f;
	struct RAB *r;
	int status;
/*
 * Allocate space for the RMS structures.
 */
	if ((f = (struct FAB *) malloc (sizeof (struct FAB))) == 0)
		c_panic ("Out of memory for FAB allocate.");
	if ((r = (struct RAB *) malloc (sizeof (struct RAB))) == 0)
		c_panic ("Out of memory for RAB allocate.");
/*
 * Fill in the FAB.
 */
	*f = cc$rms_fab;
	f->fab$l_fna = file;
	f->fab$b_fns = strlen (file);
	f->fab$l_dna = Def_name;
	f->fab$b_dns = strlen (Def_name);
	f->fab$b_fac = FAB$M_PUT;
	f->fab$l_fop = FAB$M_TEF;
	f->fab$b_rat = 0;
	f->fab$b_rfm = FAB$C_VAR;
/*
 * Attempt to create the file
 * (10/9/85 jc)	Check for RMS$_FILPURGED for version-limited files/directories.
 */
	if ((status = sys$create (f)) != RMS$_NORMAL &&
		status != RMS$_FILEPURGED)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Fill in the RAB.
 */
	*r = cc$rms_rab;
	r->rab$l_fab = f;
	r->rab$l_rop = RAB$M_EOF | RAB$M_WBH;
	r->rab$b_mbf = 4;	/* We LIKE multibuffering	*/
/*
 * Connect to the FAB.
 */
	if ((status = sys$connect (r)) != RMS$_NORMAL)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Return a pointer to the RAB.
 */
	return (r);
}


# endif






LTYPE
dopen (file)
char *file;
/*
 * Attempt to open an already existing file.
 */
{
# ifdef VMS
	struct FAB *f;
	struct RAB *r;
	int status;
/*
 * Allocate space for the RMS structures.
 */
	if ((f = (struct FAB *) malloc (sizeof (struct FAB))) == 0)
		c_panic ("Out of memory for FAB allocate.");
	if ((r = (struct RAB *) malloc (sizeof (struct RAB))) == 0)
		c_panic ("Out of memory for RAB allocate.");
/*
 * Fill in the FAB.
 */
	*f = cc$rms_fab;
	f->fab$l_fna = file;
	f->fab$b_fns = strlen (file);
	f->fab$l_dna = Def_name;
	f->fab$b_dns = strlen (Def_name);
	f->fab$b_fac = FAB$M_GET | FAB$M_PUT | FAB$M_TRN;
	f->fab$l_fop = FAB$M_TEF;
	f->fab$b_rat = 0;
/*
 * Attempt to open the file
 */
	if ((status = sys$open (f)) != RMS$_NORMAL)
	{
		if (status != RMS$_FNF)
		{
			errmes (&status);
			printf ("\n");
		}
		return (FALSE);
	}
/*
 * Fill in the RAB.
 */
	*r = cc$rms_rab;
	r->rab$l_fab = f;
	r->rab$l_rop = RAB$M_TPT | RAB$M_WBH | RAB$M_RAH;
	r->rab$b_mbf = 4;	/* We LIKE multibuffering	*/
/*
 * Connect to the FAB.
 */
	if ((status = sys$connect (r)) != RMS$_NORMAL)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Return a pointer to the RAB.
 */
	return (r);
# endif

# ifdef UNIX
	FILE *fd;

	if ((fd = fopen (file, "rw")) == NULL)
	{
		if (errno != ENOENT)
		{
			perror (file);
			printf ("\n");
		}
		return (FALSE);
	}
	return ((LTYPE) fd);
# endif
}





LTYPE
dview (file)
char *file;
/*
 * Attempt to open an already existing file, for read only access.
 */
{
# ifdef VMS
	struct FAB *f;
	struct RAB *r;
	int status;
/*
 * Allocate space for the RMS structures.
 */
	if ((f = (struct FAB *) malloc (sizeof (struct FAB))) == 0)
		c_panic ("Out of memory for FAB allocate.");
	if ((r = (struct RAB *) malloc (sizeof (struct RAB))) == 0)
		c_panic ("Out of memory for RAB allocate.");
/*
 * Fill in the FAB.
 */
	*f = cc$rms_fab;
	f->fab$l_fna = file;
	f->fab$b_fns = strlen (file);
	f->fab$l_dna = Def_name;
	f->fab$b_dns = strlen (Def_name);
	f->fab$b_fac = FAB$M_GET;
	f->fab$l_fop = FAB$M_TEF;
	f->fab$b_rat = 0;
	f->fab$b_shr = FAB$M_SHRPUT | FAB$M_SHRGET;
/*
 * Attempt to open the file
 */
	if ((status = sys$open (f)) != RMS$_NORMAL)
	{
		if (status != RMS$_FNF)
		{
			errmes (&status);
			printf ("\n");
		}
		return (FALSE);
	}
/*
 * Fill in the RAB.
 */
	*r = cc$rms_rab;
	r->rab$l_fab = f;
	r->rab$l_rop = RAB$M_TPT | RAB$M_RAH;
	r->rab$b_mbf = 4;	/* We LIKE multibuffering	*/
	r->rab$b_mbc = 16;
/*
 * Connect to the FAB.
 */
	if ((status = sys$connect (r)) != RMS$_NORMAL)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Return a pointer to the RAB.
 */
	return (r);
# endif

# ifdef UNIX
	FILE *fd;

	if ((fd = fopen (file, "r")) == NULL)
	{
		if (errno != ENOENT)
		{
			perror (file);
			printf ("\n");
		}
		return (FALSE);
	}
	return ((LTYPE) fd);
# endif
}


# ifdef VMS
/* This is doable under unix, easily, but I'm lazy today */

struct RAB *
dappend (file)
char *file;
/*
 * Attempt to open an already existing file.  If successful, the file is 
 * positioned at EOT.
 */
{
	struct FAB *f;
	struct RAB *r;
	int status;
/*
 * Allocate space for the RMS structures.
 */
	if ((f = (struct FAB *) malloc (sizeof (struct FAB))) == 0)
		c_panic ("Out of memory for FAB allocate.");
	if ((r = (struct RAB *) malloc (sizeof (struct RAB))) == 0)
		c_panic ("Out of memory for RAB allocate.");
/*
 * Fill in the FAB.
 */
	*f = cc$rms_fab;
	f->fab$l_fna = file;
	f->fab$b_fns = strlen (file);
	f->fab$l_dna = Def_name;
	f->fab$b_dns = strlen (Def_name);
	f->fab$b_fac = FAB$M_GET | FAB$M_PUT | FAB$M_TRN;
	f->fab$l_fop = FAB$M_TEF;
	f->fab$b_rat = 0;
/*
 * Attempt to open the file
 */
	if ((status = sys$open (f)) != RMS$_NORMAL)
	{
		if (status != RMS$_FNF)
		{
			errmes (&status);
			printf ("\n");
		}
		return (FALSE);		/* Should deallocate space... */
	}
/*
 * Fill in the RAB.
 */
	*r = cc$rms_rab;
	r->rab$l_fab = f;
	r->rab$b_mbf = 4;	/* We LIKE multibuffering	*/
	r->rab$l_rop = RAB$M_EOF | RAB$M_TPT | RAB$M_WBH;
/*
 * Connect to the FAB.
 */
	if ((status = sys$connect (r)) != RMS$_NORMAL)
	{
		errmes (&status);
		printf ("\n");
		return (FALSE);
	}
/*
 * Return a pointer to the RAB.
 */
	return (r);
}

# endif



dput (r, buf, len)
LTYPE r;
char *buf;
int len;
/*
 * Attempt to write the given data to the file.
 */
{
# ifdef VMS
	int status;
/*
 * Fill in the relevant RAB info.
 */
	r->rab$l_rbf = buf;
	r->rab$w_rsz = len;
/*
 * Do the operation.
 */
	if ((status = sys$put (r)) != RMS$_NORMAL)
		errmes (&status);
	return (status);
# endif

# ifdef UNIX
	char cbuf[5000];

	memcpy (cbuf, buf, len);
	cbuf[len] = 0;
	fprintf ((FILE *) r, "%s\n", cbuf);
	return (1);
# endif
}







int
dget (r, buf, max)
LTYPE r;
char *buf;
int max;
/*
 * Read a record from the file.  Normally, the length of the record read
 * is returned.  A return value of -1 implies EOF.  Negative return values
 * imply some other error.
 */
{
# ifdef VMS
	int status;
/*
 * Fill in the RAB.
 */
	r->rab$l_ubf = buf;
	r->rab$w_usz = max;
/*
 * Do the GET.
 */
	if ((status = sys$get (r)) == RMS$_EOF)
		return (-1);
	else if (status == RMS$_NORMAL)
		return (r->rab$w_rsz);
/*
 * Uh oh, something did not work.
 */
	errmes (&status);
	return (- status);
# endif

# ifdef UNIX
	int len;
	Offset = ftell ((FILE *) r);
	if (! fgets (buf, max, (FILE *) r))
		return (-1);
	len = strlen (buf);
	buf[len-1] = '\0';	/* Zap newline */
	return (len - 1);
# endif
}



drfa (r, rfa)
LTYPE r;
short rfa[3];
/*
 * Return the RFA of the last disk operation in RFA.
 */
{
# ifdef VMS
	rfa[0] = r->rab$w_rfa[0];
	rfa[1] = r->rab$w_rfa[1];
	rfa[2] = r->rab$w_rfa[2];
# endif

# ifdef UNIX
	long *temp = (long *) rfa;

	*temp = ftell ((FILE *) r);
# endif
}






dagain (r)
LTYPE r;
/*
 * Position to the beginning of the last record read.
 */
{
# ifdef VMS
	int status;
/*
 * Theoretically, the RAB still contains the RFA of interest, so all we have
 * to do is set RFA mode.
 */
	r->rab$b_rac = RAB$C_RFA;

/*
 * Do the find instruction.
 */
	status = sys$find (r);
	if (status != RMS$_NORMAL)
	{
		printf ("\nFind error %d\n", status);
		errmes (&status);
	}
/*
 * Go back to sequential mode.
 */
	r->rab$b_rac = RAB$C_SEQ;
# endif

# ifdef UNIX
	if (!fseek ((FILE *) r, Offset, 0))
		printf ("\nImproper seek\n");
# endif
}





dfind (r, rfa)
LTYPE r;
short rfa[3];
/*
 * Position to the record indicated by the RFA.
 */
{
# ifdef VMS
	int status;
/*
 * Restore the RFA.
 */
	r->rab$w_rfa[0] = rfa[0];
	r->rab$w_rfa[1] = rfa[1];
	r->rab$w_rfa[2] = rfa[2];
/*
 * Do the find instruction.
 */
	r->rab$b_rac = RAB$C_RFA;
	status = sys$find (r);
	if (status != RMS$_NORMAL)
	{
		printf ("\nFind error %d\n", status);
		errmes (&status);
	}
/*
 * Go back to sequential mode.
 */
	r->rab$b_rac = RAB$C_SEQ;
# endif

# ifdef UNIX
	if (!fseek ((FILE *) r, (long) *rfa, 0))
		printf ("\nImproper seek\n");
# endif
}




dclose (r)
LTYPE r;
/*
 * Try to close the file
 */
{
# ifdef VMS
	int status;
/*
 * Close the file.
 */
	if ((status = sys$close (r->rab$l_fab)) != RMS$_NORMAL)
	{
		errmes (&status);
		return;
	}
/*
 * Deallocate the storage.
 */
	free (r->rab$l_fab);
	free (r);
# endif

# ifdef UNIX
	fclose ((FILE *) r);
# endif
}




drewind (r)
LTYPE r;
/*
 * Rewind the given file.
 */
{
# ifdef VMS
	int status;

	if ((status = sys$rewind (r)) != RMS$_NORMAL)
		errmes (&status);
	return (status);
# endif

# ifdef UNIX
	rewind ((FILE *) r);
	return (1);
# endif
}


# ifdef VMS

int
dget_ra (r, buf, max, recno)
struct RAB *r;
char *buf;
int max, recno;
/*
 * Read a record from the file.  Normally, the length of the record read
 * is returned.  A return value of -1 implies EOF.  Negative return values
 * imply some other error.
 *
 * This version attempts to do random access.
 */
{
	int status;
/*
 * Fill in the RAB.
 */
	r->rab$l_ubf = buf;
	r->rab$w_usz = max;
	r->rab$b_rac = RAB$C_KEY;
	r->rab$l_kbf = &recno;
	r->rab$b_ksz = 4;
	r->rab$l_rop |= RAB$M_NLK;	/* No locking! */
/*
 * Do the GET.
 */
	status = sys$get (r);
	r->rab$b_rac = RAB$C_SEQ;
	/* printf ("Free status: %d\n", sys$free (r)); */
	if (status == RMS$_EOF)
		return (-1);
	else if (status == RMS$_NORMAL)
		return (r->rab$w_rsz);
/*
 * Uh oh, something did not work.
 */
	errmes (&status);
	return (- status);
}
# endif
