

#include <math.h>

/*

Really we need the difference in magnitude between the step and the cval to
know how many digits we need to display of the cval to differentiate among
the steps...  If cval and step are on the same order, then a single digit
could suffice.

 */


/*
 * Print a numeric label trying to narrow its printed precision without
 * making it indistinguishable from neighboring values a 'step' apart and
 * without resorting to exponential format until the magnitude is several
 * places from the decimal point in either direction.  The precision is
 * chosen according to the magnitude of the step rather than cval.
 * Essentially we try to print two significant digits in cval on the same
 * order as the step on the thinking that should differentiate values
 * differing by 'step'.
 */
void
do_label (char *lbl, double step, double cval)
{
    int mag = 1;    /* Steps of zero will default to a precision of one. */

    if (step != 0.0)
    {
	double lg = log10(fabs(step));
	mag = (lg < 0) ? - (int) ceil ( -lg ) : (int) ceil (lg);
    }
    if (mag <= -6)
    {
	sprintf (lbl, "%.2g", cval);
    }
    else if (mag <= 0)
    {
	int prec = 1 - mag;
	sprintf (lbl, "%.*f", prec, cval);
    }
    else if (mag < 6)
    {
	int prec = 2 - mag;
	prec = (prec < 0) ? 0 : prec;
	sprintf (lbl, "%.*f", prec, cval);
    }
    else
    {
	sprintf (lbl, "%.2g", cval);
    }
}

do_label3 (char *lbl, double step, double cval)
{
    int prec = 0;
    if ((step < -1.0) || (step > 1.0) || (step == 0.0))
    {
	prec = 1;
    }
    else
    {
	prec = (int) ceil ( - log10(fabs(step)));
	++prec;
    }
    if (prec >= 6)
	sprintf (lbl, "%.4g", cval);
    else if (prec > 1)
	sprintf (lbl, "%.*g", prec, cval);
    else
	sprintf (lbl, "%.5g", cval);
}


void
do_label2 (char *lbl, double step, double cval)
{
    int prec = 1;
    if (step < 1.0)
    {
	prec = (int) ceil ( - log10(fabs(step)));
	/*	printf (" %g %i ", -log10(fabs(step)), prec); */
    }
    sprintf (lbl, "%.*g", prec, cval);
}


void compare (double step, double center)
{
    char label[128];
    char label2[128];

    do_label (label, step, center);
    /*    do_label2 (label2, step, center); */
    printf ("step:%g label:%s\n", step, label);
}


int main ()
{

    /*
     * Loop through center and step inputs and show the labels which
     * would be produced.
     */
    double step;
    for (step = 1500; step < 1e10; step = step * 2.0)
    {
	compare (step, 11*step);
    }
    for (step = 1000; step > 0.0000001; step = step / 2.0)
    {
	compare (step, 1000+step);
	compare (step, 1+step);
	compare (step, step);
    }
    for (step = 100000; step > 0.0000001; step = step / 2.0)
    {
	compare (step, step+1);
    }
    for (step = 1.0; step > 0.000001; step = step / 10.0)
    {
	compare (step, step);
    }
    for (step = 1e-30; step > 1e-34; step = step / 2.0)
    {
	compare (step, 0);
	compare (step, step);
    }
    return 0;
}


#ifdef notdef
void 
print_labels (float ccenter, float cstep)
{
    for (cndx = ; cndx <= cndx_max; cndx++)
	{
		Cval = ccenter + cndx * cstep;
	/*
	 * Assign the color and grab a graphics context with this
	 * color in the foreground and with the user's clip rectangle
	 */
		/*
		 * Play around to get the shortest label which will 
		 * unambiguously identify this contour
		 */
			prec = -(int) (log10 (cstep));

			ptest = fabs (fmod (ccenter, cstep)) / cstep;
			if (ptest > 0.1 && ptest < 0.9)
			prec += 1;

			ptest = cstep / pow (10.0, (double)(-prec)) - 1.0;

			if (prec < 0)
				prec = 0;
			else if (ptest < -0.0001)
				prec += 1;

			sprintf (Label, " %.*f ", prec, Cval);

			maxabs = fabs (max) > fabs (min) ? 
				fabs (max) : fabs (min);
			prec = (int) (log10 (maxabs / cstep));
			sprintf (testlabel, " %.*E ", prec, Cval);
			if ((int)strlen (testlabel) < (int)strlen (Label))
				strcpy (Label, testlabel);
		/*
		 * Alternate the toggle so that adjacent contours have slightly
		 * offset labels
		 */
			Ltoggle = ! Ltoggle;
		}
#endif
