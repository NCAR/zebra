
/*******************************************************************************
 * FREEFORM : A Routine that returns floating point fields contained in
 *		a given character array seperated by any non-digit chars
 *
 * int freeform(inpstr,maxchr,outfields);
 *	char	inpstr[];	* input string	*
 *	int	maxchr;	* maximum characters to examine in string *
 *	double	outfields[];	* array of found numeric fields (output)  *
 *
 *	Routine returns the number of numeric fields found
 *
 * Written by Frank Hage -UNC  7/86
 *	Last Update : 3/15/87
 *
 */

freeform(inpstr,maxchr,outfields)
	char	inpstr[];
	int	maxchr;
	double	outfields[];
{
	double	atof();
	char	tmpbuf[256];	/* temporary buffer for conversion	*/
	char	blanks[256];
	register int	ii = 0;		/* input string counter		*/
	register int	jj = 0;		/* temp buffer counter		*/
	register int	kk = 0;		/* numeric field counter	*/
	int	fstat = 0;	/* field status: 0=empty, 1=in progress */

	for(ii = 0; ii < 256; ii++) {
		blanks[ii] = '\0';
	}
	ii = 0;

	while(ii < maxchr && inpstr[ii] != '\000'){ /* not the end */
		/* not a digit, decimal point, plus or minus */
		if(inpstr[ii] <= '\052' || inpstr[ii] == '\057' ||
			inpstr[ii] == '\054' || inpstr[ii] >= '\072' ){  
			if(inpstr[ii]=='\105' || inpstr[ii]=='\145' && jj!= 0) {
				/* is an 'e' - part of exponential number */
				tmpbuf[jj++] = inpstr[ii++];
			} else {
				tmpbuf[jj] = '\000'; /* terminate conv buffer */
				jj = 0;	/* start a new conversion	*/
				ii++;	/* look at next input character */
			}
		} else { 	/* is part of a numerical field */
			tmpbuf[jj++] = inpstr[ii++];
			fstat = 1;	/* conversion buffer is not empty */
			if(inpstr[ii] == '\055') { /* next char/num is - */
				tmpbuf[jj] = '\000'; /* terminate */
				jj = 0;	/* signal end of current conv */
			}
		}

		if( fstat == 1 && jj == 0 ) { /* field is complete */
			outfields[kk++] = atof(tmpbuf);
			fstat = 0;
			strncpy(tmpbuf,blanks,256); /* clear out buffer */
		}
	}

	if(fstat) {	/* something still in conversion buffer */
		tmpbuf[ii] = '\000';	/* add terminator	*/
		outfields[kk++] = atof(tmpbuf);	/* do conversion	*/
	}
	return(kk);
}
