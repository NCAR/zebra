/*
 * Handle plot window annotation.
 */
static char *rcsid = "$Id: Annotate.c,v 2.2 1991-10-31 20:28:02 kris Exp $";
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
# include <math.h>
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
	void An_AddAnnotProc (void (*)(), char *, char *, int, int, int, int);
	void An_DoSideAnnot ();
	static void An_AllocSpace ();
	static int An_ReduceOptionals (int, int);
	static void An_ReduceSpace (int, int);
	static void An_IncreaseSpace (int, int, int);
	void An_ColorString (char *, char *, int, int, int);
	void An_ColorBar (char *, char *, int, int, int);
	void An_ColorNumber (char *, char *, int, int, int);
	void An_ColorVector (char *, char *, int, int, int);
	int An_GetLeft ();
	void An_GetSideParams (char *, float *, int *);
	void An_GetTopParams (XColor *, int *);
# else
	static void An_Divider ();
	void An_AddAnnotProc ();
	void An_DoSideAnnot ();
	static void An_AllocSpace ();
	static int An_ReduceOptionals ();
	static void An_ReduceSpace ();
	static void An_IncreaseSpace ();
	void An_ColorString ();
	void An_ColorBar ();
	void An_ColorNumber ();
	void An_ColorVector ();
	int An_GetLeft ();
	void An_GetSideParams ();
	void An_GetTopParams ();
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


/*
 * New side annotation data structures and routines.
 */

# define DATALEN 100
# define DIVIDE 5

typedef struct ainfo {
	char comp[40];	/* Component name				*/
	void (*proc) ();/* Annotation procedure for this component	*/
	char data[DATALEN];/* Data needed to do the side annotation	*/	
	int datalen;	/* Length of the data				*/ 
	int minspace;	/* Minimum space required to do side annotation	*/
	int more;	/* Want more space if available?		*/
	int optional;	/* Annotation can be dropped completely?	*/
	int allocspace;	/* How much space has actually been allocated	*/
	struct ainfo *next;	/* Pointer to next table entry.		*/	
} AnnotInfo;

AnnotInfo *AnnotTable = NULL;	/* Table which saves info for each comp	*/


void
An_AddAnnotProc (proc, comp, data, datalen, minspace, more, optional)
void (*proc) ();
char *comp, *data;
int datalen, minspace;
int more, optional;
/*
 * Record the information for this component into the annotation table.
 */
{
	AnnotInfo *entry, *lastentry, *temp;

	msg_ELog (EF_DEBUG, "Adding procedure for comp %s", comp);
	msg_ELog (EF_DEBUG, "%s %d %d %d %d", data, datalen, minspace,
		more, optional);
/*
 * Get a new entry for the table.
 */
	if ((entry = (AnnotInfo *) malloc (sizeof (AnnotInfo))) == NULL)	
	{
		msg_ELog (EF_PROBLEM, 
			"Can't get an entry for the side annotation table.");
		return;
	}
/*
 * Fill in the entry.
 */
	strcpy (entry->comp, comp);
	entry->proc = proc;
	strcpy (entry->data, data);
	entry->datalen = datalen;
	entry->minspace = minspace;
	entry->more = more;
	entry->optional = optional;
	entry->allocspace = minspace;
/*
 * Link the new entry into the annotation table (at the end).
 */
	if (AnnotTable == NULL)
	{
		AnnotTable = entry;
		entry->next = NULL;
	}
	else
	{
		temp = AnnotTable;
		while (temp != NULL)
		{
			lastentry = temp;
			temp = temp->next;
		}
		lastentry->next = entry;
		entry->next = NULL;
	}
}


void
An_DoSideAnnot ()
/*
 * Allocate space and do side annotations for each entry in the 
 * annotation table.
 */
{
	AnnotInfo *entry, *temp;
	int begin, width;
/*
 * Allocate the space.
 */
	An_AllocSpace ();
/*
 * Call the side annotation procedures for each component, and then free
 * that entry in the annotation table.
 */
	begin = (1.0 - F_Y1) * USABLE_HEIGHT;
	width = GWWidth (Graphics);
	entry = AnnotTable;
	while (entry != NULL)
	{
		if (entry->allocspace > 0)
		{
			An_Divider (begin, F_X1 * width, width);
			begin += DIVIDE;
			entry->allocspace -= DIVIDE;
			(*entry->proc) (entry->comp, entry->data, 
				entry->datalen, begin, entry->allocspace);
			begin += entry->allocspace;
		}
		temp = entry;
		entry = entry->next;
		free (temp);
	}
	An_Divider (begin, F_X1 * width, width);
	AnnotTable = NULL;
}


static void
An_AllocSpace ()
/*
 * Allocate space in the side annotation for each entry in the
 * annotation table.
 */
{
	AnnotInfo *entry;
	int total = (F_Y1 - F_Y0) * USABLE_HEIGHT;
	int totalUsed = 0;
	int moreCount = 0;
/*
 * Loop through the annotation table totalling the minimum space needed
 * and how many want more.
 */
	entry = AnnotTable;
	while (entry != NULL)
	{
		totalUsed += entry->minspace;
		if (entry->more)
			moreCount++;
		entry = entry->next;
	}
/*
 * Check to se if we have used too much, too little or what and adjust it.
 */
	if (totalUsed > total)
	{
		totalUsed = An_ReduceOptionals (totalUsed, total);
		if (totalUsed > total)
		{
			msg_ELog (EF_INFO, "Side annotation space problem.");
			An_ReduceSpace (totalUsed, total);
		}
	}
	else if ((totalUsed < total) && (moreCount > 0))
	{
		An_IncreaseSpace (totalUsed, total, moreCount);
	} 
}


static int
An_ReduceOptionals (totalUsed, total)
int totalUsed, total;
/*
 * Too much space has been used, so reduce it by getting rid of optionals.
 */
{
	AnnotInfo *entry;

	entry = AnnotTable;
	while (entry != NULL)
	{
		if (entry->optional)
		{
			totalUsed -= entry->allocspace;
			entry->allocspace = 0; 
			if (totalUsed <= total)
				return (totalUsed);
		}
		entry = entry->next;
	}
	return (totalUsed);
}


static void
An_ReduceSpace (totalUsed, total)
int totalUsed, total;
/*
 * Too much space has been used, so reduce it by reducing the already
 * allocated space for each entry.
 */
{
	AnnotInfo *entry;
	int space;
	
	space = totalUsed - total;
	entry = AnnotTable;
	while (entry != NULL)
	{
		if (entry->allocspace > 0)
			entry->allocspace -= (int)
				(((float) entry->allocspace / (float) totalUsed)
				* (float) space); 
		entry = entry->next;
	}
}


static void
An_IncreaseSpace (totalUsed, total, moreCount)
int totalUsed, total, moreCount;
/*
 * There is side annotation space left over, distribute it among those
 * that want it.
 */
{
	AnnotInfo *entry;
	int space;

	space = total - totalUsed;
	entry = AnnotTable;
	while (entry != NULL)
	{
		if ((entry->allocspace > 0) && entry->more)
			entry->allocspace += (int) ((float) space / 
				(float) moreCount);
		entry = entry->next;
	}
}



void
An_ColorVector (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A standard side annotation routine for drawing a colored vector.
 */
{
        int limit, left;
        char string[40], color[40];
        float scale, used, u, v, unitlen;
        XColor xc;
/*
 * Get annotation parameters.
 */
        An_GetSideParams (comp, &scale, &limit);
/*
 * Get data.
 */
        sscanf (data, "%s %s %f %f %f", string, color, &u, &v, &unitlen);
        ct_GetColorByName (color, &xc);
/*
 * Draw the string.
 */
        left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                left, begin, string, 0.0, scale, JustifyLeft, JustifyTop);
	used = scale * (float) USABLE_HEIGHT;
        begin += used;
        space -= used;
/*
 * Draw the vector.
 */
	draw_vector (XtDisplay (Graphics), GWFrame (Graphics), AnGcontext,
		left, begin + 5, (double) u, (double) v, (double) unitlen); 
}



void
An_ColorString (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A standard side annotation routine for drawing a colored string.
 */
{
	int limit, left;
	float scale;
	char string[40], color[40];
	XColor xc;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * Get data.
 */
	sscanf (data, "%s %s", string, color);
	ct_GetColorByName (color, &xc);
/*
 * Draw the string.
 */
	left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, 
        	left, begin, string, 0.0, scale, JustifyLeft, JustifyTop);
}


void
An_ColorNumber (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A standard side annotation routine for drawing colored numbers.
 */
{
        int i, limit, left, ncolors, match, barHeight;
        char string[40], ctable[40];
        XColor xc, *colors;
        float scale, center, step, cval, used;
/*
 * Get top and side annotation plot description parameters.
 */
        An_GetSideParams (comp, &scale, &limit);
        An_GetTopParams (&xc, &match);
/*
 * Get the data.
 */
        sscanf (data, "%s %s %f %f", string, ctable, &center, &step);
        ct_LoadTable (ctable, &colors, &ncolors);
/*
 * Put in the string.
 */
        left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, left, begin,
                string, 0.0, scale, JustifyLeft, JustifyTop);
        used = scale * (float) USABLE_HEIGHT;
        begin += used;
        space -= used;
/*
 * Draw the numbers.
 */
	if (limit > 1)
		barHeight = (float)space / (float)ncolors * ((float)limit - .5);
	else
		barHeight = (float) space / (float) ncolors;
        for (i = 0; i < ncolors; i += limit)
        {
                cval = center + (i - ncolors / 2) * step;
                sprintf (string, "%.1f", cval);
                XSetForeground (XtDisplay (Graphics), AnGcontext,
                        colors[i].pixel);
                DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                        left + 15, (int) begin, string,
                        0.0, scale, JustifyLeft, JustifyTop);
		begin += barHeight;
        }
}


void
An_ColorBar (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A standard side annotation routine for drawing a color bar.
 */
{
	int i, limit, left, ncolors, match, barHeight;
	char string[40], ctable[40];
	XColor xc, *colors;
	float scale, center, step, cval, used;
/*
 * Get top and side annotation plot description parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
	An_GetTopParams (&xc, &match);
/*
 * Get the data.
 */
	sscanf (data, "%s %s %f %f", string, ctable, &center, &step);
	ct_LoadTable (ctable, &colors, &ncolors);
/*
 * Put in the string.
 */
	left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
	DrawText (Graphics, GWFrame (Graphics), AnGcontext, left, begin, 
		string, 0.0, scale, JustifyLeft, JustifyTop);
	used = scale * (float) USABLE_HEIGHT;
	begin += used;
	space -= used;
/*
 * Draw the bar.
 */
	if (limit > 1)
		barHeight = (float)space / (float)ncolors * ((float)limit - .5);
	else
		barHeight = (float) space / (float) ncolors;
	for (i = 0; i < ncolors; i += limit)
	{
		XSetForeground (XtDisplay (Graphics), AnGcontext, 
			colors[i].pixel);
		XFillRectangle (XtDisplay (Graphics), 
			GWFrame (Graphics), AnGcontext, left, 
			(int) begin, 10, barHeight);
		cval = center + (i - ncolors / 2) * step;
		sprintf (string, "%.1f", cval);
		XSetForeground (XtDisplay (Graphics), AnGcontext, 
			xc.pixel);
		DrawText (Graphics, GWFrame (Graphics), AnGcontext, 
			left + 15, (int) begin, string,
			0.0, scale, JustifyLeft, JustifyTop);
		begin += barHeight;
	}
}


int
An_GetLeft ()
/*
 * Return the left side of the side annotation space.
 */
{
	return (F_X1 *GWWidth (Graphics));
}


void
An_GetSideParams (comp, scale, limit)
char *comp;
float *scale;
int *limit;
/*
 * Get all side annotation parameters from the plot description.
 */
{
	if(! pda_Search(Pd, comp, "sa-scale", NULL, (char *) scale, SYMT_FLOAT))
		*scale = 0.02;
	if(! pda_Search(Pd, comp, "ct-limit", NULL, (char *) limit, SYMT_INT))
		*limit = 1;
}


void
An_GetTopParams (color, match)
XColor *color;
int *match;
/*
 * Get all side annotation parameters from the plot description.
 */
{
	char colorstr[40];

        if(! pd_Retrieve (Pd, "global", "ta-color", colorstr, SYMT_STRING))
                strcpy (colorstr, "white");
        if(! ct_GetColorByName (colorstr, color))
                ct_GetColorByName ("white", color);
        *match = FALSE;
        pd_Retrieve (Pd, "global", "ta-color-match", (char *) match, SYMT_BOOL);
}

