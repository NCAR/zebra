/*
 * Handle plot window annotation.
 */
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "DrawText.h"


/*
 * Top annotation stuff
 */
static int	Annot_xpos, Annot_ypos;
static int	Annot_height;
static int	Annot_lmargin = 10, Annot_rmargin;

/*
 * Side annotation stuff.
 */
static int	SA_position;	/* Current position		*/
static int	SA_space;	/* How much space each gets	*/

static int	Ncomps;		/* How many components		*/




void
An_ResetAnnot (nc)
int nc;
/*
 * Reset the annotation position
 */
{
	int	height = GWHeight (Graphics);
/*
 * Get the line spacing for the top annotation
 */
	Annot_height = (int)(1.2 * TOPANNOTHEIGHT * GWHeight (Graphics));
/*
 * Set the right margin
 */
	Annot_rmargin = GWWidth (Graphics) - Annot_lmargin;
/*
 * Set the initial text position
 */
	Annot_xpos = Annot_lmargin;
	Annot_ypos = 2;
/*
 * Initialize the stuff for side annotation.
 */
	Ncomps = nc;
	SA_position  = (1.0 - F_Y1) * height;
	SA_space = ((F_Y1 - F_Y0) * height)/ (float) Ncomps;
}





void
An_TopAnnot (string, color)
char	*string;
Pixel	color;
/*
 * Add the string to the top annotation using the given color
 */
{
	int	i, brk, slen, swidth, dummy;
	char	*cstring;

	slen = strlen (string);
/*
 * If we're at the left margin, strip leading spaces
 */
	while ((Annot_xpos == Annot_lmargin) && (string[0] == ' '))
	{
		string++;
		slen--;
	}
/*
 * Just return for zero length string
 */
	if (slen == 0)
		return;
/*
 * Make a copy of the string so we can diddle with it
 */
	cstring = (char *) malloc ((slen + 1) * sizeof (char));
	strcpy (cstring, string);
/*
 * Handle newlines in the text
 */
	for (i = 0; i < slen; i++)
	{
		if (cstring[i] == '\n')
		{
		/*
		 * TopAnnot the string up to the newline
		 */
			cstring[i] = '\0';
			An_TopAnnot (cstring, color);
		/*
		 * Move the annotation location to start a new line
		 * and call An_TopAnnot for the remainder of the string
		 */
			Annot_ypos += Annot_height;
			Annot_xpos = Annot_lmargin;
			An_TopAnnot (cstring + i + 1, color);
		/*
		 * We're done
		 */
			free (cstring);
			return;
		}
	}
/*
 * Make sure the string will fit on the current line.  Break at
 * a space if necessary
 */
	DT_TextBox (Graphics, GWFrame (Graphics), 0, 0, cstring, 0.0, 
		TOPANNOTHEIGHT, JustifyLeft, JustifyTop, &dummy, &dummy, 
		&swidth, &dummy);

	brk = slen;

	while (swidth > Annot_rmargin - Annot_xpos)
	{
		for (brk-- ; cstring[brk] != ' ' && brk > 0; brk--)
			/* backing up to a space */;

		cstring[brk] = '\0';
	/*
	 * Get the new string width
	 */
		DT_TextBox (Graphics, GWFrame (Graphics), 0, 0, cstring, 0.0, 
			TOPANNOTHEIGHT, JustifyLeft, JustifyTop, &dummy, 
			&dummy, &swidth, &dummy);
	}
/*
 * If we're at the left margin and the break position is at zero, 
 * we have nowhere to break this string.  Just print what we can and
 * log an error.
 */
	if (brk == 0 && Annot_xpos == Annot_lmargin)
	{
		brk = slen;
		msg_ELog (EF_PROBLEM,
			"An_TopAnnot could not break annotation '%s'", string);
	}
/*
 * Draw the string up to the break, if any
 */
	DrawText (Graphics, GWFrame (Graphics), color, Annot_xpos, Annot_ypos, 
		cstring, 0.0, TOPANNOTHEIGHT, JustifyLeft, JustifyTop);
	Annot_xpos += swidth;
/*
 * If we have to break, move down to the next line and call TopAnnot with
 * the remainder of the string
 */
	if (brk < slen - 1)
	{
		Annot_ypos += Annot_height;
		Annot_xpos = Annot_lmargin;
		An_TopAnnot (string + brk, color);
	}
/*
 * Free up the string copy
 */
	free (cstring);
	return;
}




void
An_AnnotLimits (top, bottom, left, right)
int	*top, *bottom, *left, *right;
/*
 * Return the pixel limits for the right side annotation area for
 * the current plot component
 */
{
	int	width = GWWidth (Graphics);

	*top = SA_position;
	SA_position += SA_space;
	*bottom = SA_position - 1;

	*left = F_X1 * width;
	*right = width;
}


