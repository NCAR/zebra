/*
 * Handle plot window annotation.
 */
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
# include <string.h>
# include <X11/Intrinsic.h>
# include <math.h>
# include <defs.h>
# include <draw.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "GC.h"
MAKE_RCSID ("$Id: Annotate.c,v 2.37 2001-04-20 05:04:54 granger Exp $")

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





static void An_Divider FP ((int, int, int));
void An_AddAnnotProc FP ((void (*)(), char *, char *, int, int, int, int));
void An_DoSideAnnot FP ((void));
static void An_AllocSpace FP ((void));
static int An_ReduceOptionals FP ((int, int));
static void An_ReduceSpace FP ((int, int));
static void An_IncreaseSpace FP ((int, int, int));
void An_ColorString FP ((char *, char *, int, int, int));
void An_ColorBar FP ((char *, char *, int, int, int));
void An_ColorNumber FP ((char *, char *, int, int, int));
void An_ColorVector FP ((char *, char *, int, int, int));
void An_ColorScale FP ((char *, char *, int, int, int));
void An_BarbLegend FP ((char *, char *, int, int, int));
int An_GetLeft FP ((void));
void An_GetSideParams FP ((char *, float *, int *));
void An_GetTopParams FP ((XColor *, int *));


void
An_ResetAnnot (nc)
int nc;
/*
 * Reset the annotation position
 */
{
/*
 * Get the line spacing for the top annotation. (If Annot_Scale is > 1, we
 * assume it's an absolute pixel height, otherwise a scale factor relative
 * to the window height).
 */
	if (Annot_Scale > 1)
		Annot_height = (int)(1.2 * Annot_Scale);
	else
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
	if (Ncomps > 0)
		SA_space = ((F_Y1 - F_Y0) * USABLE_HEIGHT)/ (float) Ncomps;
	else
		SA_space = ((F_Y1 - F_Y0) * USABLE_HEIGHT);
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
const char *string;
Pixel color;
/*
 * Older interface.
 */
{
	An_DoTopAnnot (string, color, 0, 0);
}




void
An_DoTopAnnot (string, color, comp, plat)
const char	*string;
Pixel	color;
const char *comp, *plat;
/*
 * Add the string to the top annotation using the given color.  If "comp"
 * and "plat" are provided, the annotation is made active using these
 * parameters.
 */
{
	int	i, brk, slen, swidth, sx, sy, ex, ey;
	char	*cstring;
/*
 * Turn off top annotations if the available space is zero.
 */
	if (AnnotateY0 == AnnotateY1)
		return;
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
			An_DoTopAnnot (cstring, color, comp, plat);
		/*
		 * Move the annotation location to start a new line
		 * and call An_TopAnnot for the remainder of the string
		 */
			Annot_ypos += Annot_height;
			Annot_xpos = Annot_lmargin;
			An_DoTopAnnot (cstring + i + 1, color, comp, plat);
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
	DT_TextBox (Graphics, GWFrame (Graphics), Annot_xpos, Annot_ypos,
			cstring, 0.0, Annot_Scale, JustifyLeft, JustifyTop,
			&sx, &sy, &ex, &ey);
	swidth = ex - Annot_xpos;

	brk = slen;

	while (swidth > Annot_rmargin - Annot_xpos)
	{
		for (brk-- ; cstring[brk] != ' ' && brk > 0; brk--)
			/* backing up to a space */;

		cstring[brk] = '\0';
	/*
	 * Get the new string width
	 */
		DT_TextBox (Graphics, GWFrame (Graphics), Annot_xpos,
				Annot_ypos, cstring, 0.0, Annot_Scale,
				JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
		swidth = ex - Annot_xpos;
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
 * If they want this thing activated, go for it.
 */
	if (comp)
		I_ActivateArea (sx - 2, ey + 2, ex - sx + 4, sy - ey + 3,
				"topannot", comp, plat, 0);
/*
 * If we have to break, move down to the next line and call TopAnnot with
 * the remainder of the string
 */
	if (brk < slen - 1)
	{
		Annot_ypos += Annot_height;
		Annot_xpos = Annot_lmargin;
		An_DoTopAnnot (string + brk, color, comp, plat);
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
		An_Divider (SA_position, (int) (LegendX0 * width), width);
	else
		SA_first = FALSE;

	*top = SA_position + 1;
	SA_position += SA_space;
	*bottom = SA_position - 2;

	*left = LegendX0 * width;
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
	ResetGC ();
	SetColor ("global", "divider-color", "sa", "gray50");
	XDrawLine (Disp, GWFrame (Graphics), Gcontext, x1, y, x2, y);
}


int
An_SaShow (char *comp, char *qual)
{
	zbool sashow = TRUE;
/*
 * For backwards compatibility we honor both sa-show and the xygraph
 * parameter, but sa-show takes precedence.  The only difference is that
 * do-side-annotation is no longer qualified with the xygraph
 * representation type.  That must be handled by an explicit call rather
 * than automatically in An_AddAnnotProc().
 */
	if (! pda_Search (Pd, comp, "sa-show", qual, (char *) &sashow, 
			  SYMT_BOOL))
	{
	    pda_Search (Pd, comp, "do-side-annotation", qual,
			(char *) &sashow, SYMT_BOOL);
	}	    
	return sashow;
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
/*
 * If side annotation has been disabled in this component, then ignore this
 * procedure.  
 */
	if (! An_SaShow (comp, NULL))
	{
	    msg_ELog (EF_DEBUG, "Skipping sa procedure for comp %s (disabled)",
		      comp);
	    return;
	}

	msg_ELog (EF_DEBUG, "Adding sa procedure for comp %s", comp);
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
	begin = (1.0 - LegendY1) * USABLE_HEIGHT;
	width = GWWidth (Graphics);
	entry = AnnotTable;
	while (entry != NULL)
	{
		if (entry->allocspace > 0)
		{
			An_Divider (begin, (int)(LegendX0 * width), width);
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
	An_Divider (begin, (int) (LegendX0 * width), width);
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
	int total = (LegendY1 - LegendY0) * USABLE_HEIGHT;
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
An_ColorScale (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A standard side annotation routine for drawing a (ruler) scale
 */
{
	float	ratio;	    /* The ratio of graph width to actual units */
	int	graphWidth; /* The graph width in pixels */
	Pixel	color;
	XColor	xc;
	int	left,width;
	int	limit;
	float	scale;
	char	label[32];
	char	field[32];
	int	nTic, i;
	float	ticPix, step;
	int	textlim, x1,x2,y1,y2;
/*
 * Get annotation parameters.
 */
        An_GetSideParams (comp, &scale, &limit);
/*
 * Get Data
 */
	sscanf (data, "%f|%d|%li|%[^|]", &ratio, &graphWidth , &color, field);
/*
 * Draw the (ruler) scale
 */
	left = An_GetLeft();
	left += 10; /* put in a buffer */

	width = GWWidth(Graphics) - left;

	ticPix = ratio * graphWidth;
	step = 1.0;
	
	nTic = (int)(width / ticPix);
/* 
 * decrease the number of ticks below 5 by increments of
 * 5 and 10.  This may need to get more sophisticated for
 * extreme cases, but hopefully this will suffice for now;
 */
	if ( nTic > 5 ) 
	{
		ticPix *= 5.0;
		nTic = (int)(width / ticPix);
		step *= 5.0;
	}
	if ( nTic > 5 ) 
	{
		ticPix *= 2.0;
		nTic = (int)(width / ticPix);
		step *= 2.0;
	}

	ct_GetColorByName("white", &xc);
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
	textlim = left-10;
	for ( i = 0; i <= nTic; i++ )
	{
	    sprintf ( label, "%0.01f", i * step);
	    DT_TextBox ( Graphics, GWFrame(Graphics), left+(int)(i*ticPix),0,
		label, 0.0, scale, JustifyCenter, JustifyTop, &x1,&y1,&x2,&y2);
	    if ( x1 > textlim ) 
	    {
                DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                    left+(int)(i*ticPix), begin+1, label, 0.0, scale, 
		    JustifyCenter, JustifyTop);
		textlim = x2;
	    }
	    XDrawLine( XtDisplay(Graphics),GWFrame(Graphics), AnGcontext,
		      left+(int)(i*ticPix), begin + 1+abs(y1-y2)+2, 
		      left+(int)(i*ticPix), begin+1+abs(y1-y2)+5);

	}
	XDrawLine( XtDisplay(Graphics),GWFrame(Graphics), AnGcontext,
	    left, begin + 1+abs(y1-y2)+5, 
	    left+(int)(ticPix*nTic), begin+1+abs(y1-y2)+5);
	/*
	 * Draw the scale name
	 */
        XSetForeground (XtDisplay (Graphics), AnGcontext, color);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext,
	     left+(int)(ticPix*nTic/2), begin+1+abs(y1-y2)+7, field, 0.0, 
	     scale, JustifyCenter, JustifyTop);
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
        char units[16], string[24];
        float scale, used, u, v, unitlen;
	Pixel color;
/*
 * Get annotation parameters.
 */
        An_GetSideParams (comp, &scale, &limit);
/*
 * Get data.
 */
        sscanf (data, "%[^|]|%li|%f|%f|%f", units, &color, &u, &v, &unitlen);
	sprintf (string, "%.0f %s", (float)(hypot (u, v)), units);
/*
 * Draw the string.
 */
        left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, color);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, left, begin, 
		  string, 0.0, scale, JustifyLeft, JustifyTop);
	used = DT_ApproxHeight (Graphics, scale, 1);
        begin += used;
        space -= used;
/*
 * Draw the vector.
 */
	draw_vector (XtDisplay (Graphics), GWFrame (Graphics), AnGcontext,
		left, begin + 5, (double) u, (double) v, (double) unitlen); 
}

void
An_BarbLegend (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A standard side annotation routine for showing barb speed
 */
{
        int limit, left;
        char string[40];
	Pixel color;
        float scale, used;
        int     unitlen;
        char    title[80];
	int	doKnot = 0;
/*
 * Get annotation parameters.
 */
        An_GetSideParams (comp, &scale, &limit);
/*
 * Get data.
 */
        sscanf (data, "%[^|]|%li|%d", string, &color, &unitlen);
/*
 * Draw the string.
 */
	doKnot = strcmp("knots", title ) == 0 ? 1 : 0;
        left = An_GetLeft ();
        sprintf( title, "barb flags in %s",string);
        XSetForeground (XtDisplay (Graphics), AnGcontext, color);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                left, begin, title, 0.0, scale, JustifyLeft, JustifyTop);
        used = 20;
        begin += used;
        space -= used;
/*
 * Draw the barbs.
 */
        if ( space > 0.0 )
        {
            draw_barb (XtDisplay (Graphics), GWFrame (Graphics), AnGcontext,
                left+1, begin + 9, 0.0, 100.0, unitlen, doKnot );
            DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                left+unitlen+2, begin+9, " == 100", 0.0, scale,
                JustifyLeft, JustifyBottom);
            begin += used;
            space -= used;
        }
        if ( space > 0.0 )
        {
            draw_barb (XtDisplay (Graphics), GWFrame (Graphics), AnGcontext,
                left+1, begin + 9, 0.0, 50.0, unitlen, doKnot );
            DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                left+unitlen+2, begin+9, " == 50", 0.0, scale,
                JustifyLeft, JustifyBottom);
            begin += used;
            space -= used;
        }
        if ( space > 0.0 )
        {
            draw_barb (XtDisplay (Graphics), GWFrame (Graphics), AnGcontext,
                left+1, begin + 9, 0.0, 10.0, unitlen, doKnot );
            DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                left+unitlen+2, begin+9, " == 10", 0.0, scale,
                JustifyLeft, JustifyBottom);
            begin += used;
            space -= used;
        }
        if ( space > 0.0 )
        {
            draw_barb (XtDisplay (Graphics), GWFrame (Graphics), AnGcontext,
                left+1, begin + 9, 0.0, 5.0, unitlen, doKnot );
            DrawText (Graphics, GWFrame (Graphics), AnGcontext,
                left+unitlen+2, begin+9, " == 5", 0.0, scale,
                JustifyLeft, JustifyBottom);
            begin += used;
            space -= used;
        }
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
	Pixel color;
	char *sp;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * The data string should be of the form <string>|<color>
 */
	if (! (sp = strrchr (data, '|')))
	{
		msg_ELog (EF_PROBLEM, "Trashed annot string: %s", data);
		return;
	}
	*sp = '\0';
	color = atoi (sp + 1);
/*
 * Draw the string.
 */
	left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, color);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, 
        	left, begin, data, 0.0, scale, JustifyLeft, JustifyTop);
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
        sscanf (data, "%[^|]|%[^|]|%f|%f", string, ctable, &center, &step);
        ct_LoadTable (ctable, &colors, &ncolors);
/*
 * Put in the string.
 */
        left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, left, begin,
                string, 0.0, scale, JustifyLeft, JustifyTop);
        used = DT_ApproxHeight (Graphics, scale, 1);
        begin += used;
        space -= used;
/*
 * Draw the numbers.
 */
	if (limit > 1)
		barHeight = (float) space / (float) ncolors * 
			((float) limit - .5);
	else
		barHeight = (float) space / (float) ncolors;

        for (i = 0; i < ncolors; i += limit)
        {
                cval = center + ((ncolors - 1) / 2 - i) * step;

		LabelStep (string, step, cval);

                XSetForeground (XtDisplay (Graphics), AnGcontext,
				colors[ncolors - i - 1].pixel);
                DrawText (Graphics, GWFrame (Graphics), AnGcontext, left + 15,
			  (int) begin, string, 0.0, scale, JustifyLeft, 
			  JustifyTop);
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
	char string[80], ctable[40];
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
	sscanf (data, "%[^|]|%[^|]|%f|%f", string, ctable, &center, &step);
	ct_LoadTable (ctable, &colors, &ncolors);
/*
 * Put in the string.
 */
	left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
	DrawText (Graphics, GWFrame (Graphics), AnGcontext, left, begin, 
		string, 0.0, scale, JustifyLeft, JustifyTop);
	used = DT_ApproxHeight (Graphics, scale, 1);
	begin += used;
	space -= used;
/*
 * Draw the bar.
 */
	if (limit > 1)
		barHeight = (float) space / (float) ncolors * 
			((float) limit - .5);
	else
		barHeight = (float) space / (float) ncolors;

	for (i = ncolors - 1; i >= 0; i -= limit)
	{
		XSetForeground (XtDisplay (Graphics), AnGcontext, 
				colors[i].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
				AnGcontext, left, (int) begin, 10, barHeight);

		cval = center + (i - ncolors/2) * step;

		LabelStep (string, step, cval);

		XSetForeground (XtDisplay (Graphics), AnGcontext, xc.pixel);
		DrawText (Graphics, GWFrame (Graphics), AnGcontext, 
			  left + 15, (int) (begin + barHeight/2), string,
			  0.0, scale, JustifyLeft, JustifyCenter);
		begin += barHeight;
	}
}


int
An_GetLeft ()
/*
 * Return the left side of the side annotation space.
 */
{
	return (LegendX0 * GWWidth (Graphics) + 2);
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
	if(scale && 
	   ! pda_Search(Pd, comp, "sa-scale", NULL, (char *)scale, SYMT_FLOAT))
		*scale = 0.02;
	if(limit &&
	   ! pda_Search(Pd, comp, "ct-limit", NULL, (char *)limit, SYMT_INT))
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
	zbool bmatch = FALSE;

        if(! pd_Retrieve (Pd, "global", "ta-color", colorstr, SYMT_STRING))
                strcpy (colorstr, "white");
        if(! ct_GetColorByName (colorstr, color))
                ct_GetColorByName ("white", color);
        pd_Retrieve (Pd, "global", "ta-color-match", (char *) &bmatch, 
		SYMT_BOOL);
        *match = (int) bmatch;
}





void
An_XYGString (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A specialized process for annotating XYGraph traces.
 *
 * The data string is expected to be formatted as:
 *
 *	style|color|platform[|line...]
 */
{
	int limit, left, cheight, nstuff, i, fxpos, active;
	int sx, sy, ex, ey;
	float scale;
	char *stuff[20];
	Pixel color;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * Pull the info out of the string.
 */
	nstuff = ParseLine (data, stuff, '|');
/*
 * Initial graphics setup.
 */
	color = atoi (stuff[1]);
	left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, color);
	cheight = DT_ApproxHeight (Graphics, scale, 1);
	begin -= 4; /* XXX */
/*
 * First line: symbol and platform name.
 */
	if (! strcmp (stuff[0], "line"))
		XDrawLine (Disp, GWFrame (Graphics), AnGcontext, left,
			   begin + cheight/2, left + cheight,
			   begin + cheight/2);
	else
		I_PositionIcon (comp, stuff[2], NULL, stuff[0], 
				left + cheight/2, begin + cheight/2, color);
	
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, 
        	left + cheight + 2, begin, stuff[2], 0.0, scale,
		JustifyLeft, JustifyTop);
/*
 * Make the symbol active.
 */
	I_ActivateArea (left, begin, cheight, cheight, "annot", comp,
			stuff[2], 0);
/*
 * X  field.
 */
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, left,
			begin + cheight, stuff[3], 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), left,
			begin + cheight, stuff[3], 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	if (pda_Search (Pd, comp, "x-annot-active", "xy", (char *) &active,
			SYMT_BOOL) && active)
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"xfield", comp, stuff[2], 0);
/*
 * Throw in the divider text
 */
	fxpos = ex;
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, fxpos,
			begin + cheight, " : ", 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), fxpos,
			begin + cheight, " : ", 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	fxpos = ex;
/*
 * Y field.
 */
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, fxpos,
			begin + cheight, stuff[4], 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), fxpos,
			begin + cheight, stuff[4], 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	if (pda_Search (Pd, comp, "y-annot-active", "xy", (char *) &active,
			SYMT_BOOL) && active)
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"yfield", comp, stuff[2], 0);
/*
 * Draw any additional lines.
 */
	for (i = 5; i < nstuff; i++)
		DrawText (Graphics, GWFrame (Graphics), AnGcontext,
			  left, begin + cheight*(i - 3),
			  stuff[i], 0.0, scale, JustifyLeft, JustifyTop);
}




void
An_XYZGString (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * A specialized process for annotating XYGraph traces with z fields.
 *
 * The data string is expected to be formatted as:
 *
 *	style|color|platform[|line...]
 */
{
	int limit, left, cheight, nstuff, i, fxpos, active;
	int sx, sy, ex, ey;
	float scale;
	char *stuff[20];
	Pixel color;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * Pull the info out of the string.
 */
	nstuff = ParseLine (data, stuff, '|');
/*
 * Initial graphics setup.
 */
	color = atoi (stuff[1]);
	left = An_GetLeft ();
        XSetForeground (XtDisplay (Graphics), AnGcontext, color);
	cheight = DT_ApproxHeight (Graphics, scale, 1);
	begin -= 4; /* XXX */
/*
 * First line: symbol and platform name.
 */
	if (! strcmp (stuff[0], "line"))
		XDrawLine (Disp, GWFrame (Graphics), AnGcontext, left,
			   begin + cheight/2, left + cheight,
			   begin + cheight/2);
	else
		I_PositionIcon (comp, stuff[2], NULL, stuff[0], 
				left + cheight/2, begin + cheight/2, color);
	
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, 
        	left + cheight + 2, begin, stuff[2], 0.0, scale,
		JustifyLeft, JustifyTop);
/*
 * Make the symbol active.
 */
	I_ActivateArea (left, begin, cheight, cheight, "annot", comp,
			stuff[2], 0);
/*
 * X  field.
 */
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, left,
			begin + cheight, stuff[3], 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), left,
			begin + cheight, stuff[3], 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	if (pda_Search (Pd, comp, "x-annot-active", "xy", (char *) &active,
			SYMT_BOOL) && active)
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"xfield", comp, stuff[2], 0);
/*
 * Throw in the divider text
 */
	fxpos = ex;
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, fxpos,
			begin + cheight, " : ", 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), fxpos,
			begin + cheight, " : ", 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	fxpos = ex;
/*
 * Y field.
 */
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, fxpos,
			begin + cheight, stuff[4], 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), fxpos,
			begin + cheight, stuff[4], 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	if (pda_Search (Pd, comp, "y-annot-active", "xy", (char *) &active,
			SYMT_BOOL) && active)
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"yfield", comp, stuff[2], 0);
/*
 * Divide again.
 */
	fxpos = ex;
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, fxpos,
			begin + cheight, " : ", 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), fxpos,
			begin + cheight, " : ", 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	fxpos = ex;
/*
 * Z field.
 */
        DrawText (Graphics, GWFrame (Graphics), AnGcontext, fxpos,
			begin + cheight, stuff[5], 0.0, scale,
			JustifyLeft, JustifyTop);
	DT_TextBox (Graphics, GWFrame (Graphics), fxpos,
			begin + cheight, stuff[5], 0.0, scale,
			JustifyLeft, JustifyTop, &sx, &sy, &ex, &ey);
	if (pda_Search (Pd, comp, "z-annot-active", "xy", (char *) &active,
			SYMT_BOOL) && active)
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"zfield", comp, stuff[2], 0);
/*
 * Draw any additional lines.
 */
	for (i = 6; i < nstuff; i++)
		DrawText (Graphics, GWFrame (Graphics), AnGcontext,
			  left, begin + cheight*(i - 4),
			  stuff[i], 0.0, scale, JustifyLeft, JustifyTop);
}
