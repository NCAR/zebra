/*
 * Handle plot window annotation.
 */
static char *rcsid = "$Id: Annotate.c,v 2.1 1991-09-12 20:27:54 corbet Exp $";
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
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "GC.h"

/*
 * Graphics context (don't use the global one in GC.h because we don't
 * want to have to worry about clipping)
 */
static GC	AnGcontext = 0;

/*
 * Top annotation stuff
 */
static int	Annot_xpos, Annot_ypos;
static int	Annot_height;
static int	Annot_lmargin = 10, Annot_rmargin;
static float	Annot_Scale = TOPANNOTHEIGHT;

/*
 * Side annotation stuff.
 */
static int	SA_position;	/* Current position		*/
static int	SA_space;	/* How much space each gets	*/
static int	SA_first;

static int	Ncomps;		/* How many components		*/


# ifdef __STDC__
	static void An_Divider (int, int, int);
# else
	static void An_Divider ();
# endif



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
	Annot_height = (int)(1.2 * Annot_Scale * GWHeight (Graphics));
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
	SA_position  = (1.0 - F_Y1) * USABLE_HEIGHT;
	SA_space = ((F_Y1 - F_Y0) * USABLE_HEIGHT)/ (float) Ncomps;
	SA_first = TRUE;
}


void
An_SetScale(scale)
float scale;
{
	Annot_Scale = scale;
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
		Annot_Scale, JustifyLeft, JustifyTop, &dummy, &dummy, 
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
			Annot_Scale, JustifyLeft, JustifyTop, &dummy, 
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
 * Make sure we have a graphics context
 */
	if (! AnGcontext)
		AnGcontext = XCreateGC(XtDisplay (Graphics),GWFrame (Graphics),
			0, NULL);
/*
 * Draw the string up to the break, if any
 */
	XSetForeground (XtDisplay (Graphics), AnGcontext, color);
	DrawText (Graphics, GWFrame (Graphics), AnGcontext, Annot_xpos, 
		Annot_ypos, cstring, 0.0, Annot_Scale, JustifyLeft, 
		JustifyTop);
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

	if (! SA_first)
		An_Divider (SA_position, F_X1 * width, width);
	else
		SA_first = FALSE;

	*top = SA_position + 1;
	SA_position += SA_space;
	*bottom = SA_position - 2;

	*left = F_X1 * width;
	*right = width;
}



void
An_SAUsed (bottom)
int bottom;
/*
 * Tell how far we really went.
 */
{
	SA_position = bottom + 1;
}




static void
An_Divider (y, x1, x2)
int y, x1, x2;
/*
 * Draw the divider line.
 */
{
	char color[40];
	XColor xc;

	ResetGC ();
	SetColor ("global", "divider-color", "sa", "gray50");
	XDrawLine (Disp, GWFrame (Graphics), Gcontext, x1, y, x2, y);
}
