/*
 * A C interface to the FORTRAN DECODE routine supplied by RSS with
 * the Ta SSM/I tapes.  The main routine, decode_ssmi(), takes care of
 * calling DECODE with the correct C<-->FORTRAN calling conventions.
 */

#include "outdat.h"

/*
 * Global reference to FORTRAN common block OUTDAT
 */
OUTDAT_BLOCK *C_OUTDAT = (struct _OUTDAT *)(&outdat_);

void
decode_ssmi (i85ghz, itb, iadj, irec, lrec)
int i85ghz;	/* decode 85 Ghz channels */
int itb;	/* provide antenna temps if 0, brightness if 1 */
int iadj;	/* If 1, along-track correction for F08 prior to 1989 */
int irec;	/* which logical record in physical rec, 1 to 16 */
char *lrec;	/* the logical record */
{
	decode_ (&i85ghz, &itb, &iadj, &irec, lrec);
}

