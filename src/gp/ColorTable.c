/*
 * Color control / display manager interface code.
 */
static char *rcsid = "$Id: ColorTable.c,v 1.1 1990-05-07 16:08:01 corbet Exp $";


# include <X11/Intrinsic.h>
# include <ui.h>

# include "../include/defs.h"
# include "../include/dm.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"




/*
 * For now, we use a simple bitmap to keep track of the colors that
 * have been allocated.
 */
# define MAXCOLOR 4096		/* That oughtta be enough		*/
# define NCBYTES (MAXCOLOR/8)	/* Number of bytes to allocate.		*/
static unsigned char Cmap[NCBYTES] = { 0 };
static unsigned char BitMap[8] = { 0x01, 0x02, 0x04, 0x08,
				   0x10, 0x20, 0x40, 0x80 };
static int MaxAlloc = 0;

/*
 * Functions for accessing the color map.
 */
static inline void
ct_MarkColor (color)
int color;
{
	Cmap[color/8] |= BitMap[color & 0x7];
	if (color > MaxAlloc)
		MaxAlloc = color;
}


static inline int
ct_ColorIsAlloc (color)
int color;
{
	return (Cmap[color/8] & BitMap[color & 0x7]);
}



static void
ct_ClearColors ()
{
	memset (Cmap, 0, NCBYTES);
}




/*
 * Color tables are stored by name in a special symbol table.  Each entry
 * contains the following:
 */
typedef struct ColorTable
{
	char	ct_name[80];		/* The name of this table	*/
	int	ct_ncolor;		/* The number of colors		*/
	XColor	*ct_colors;		/* The actual color values	*/
	bool	ct_alloc;		/* The colors alloc'd from server */
} CTable;

static stbl Ctable = 0;


/*
 * Forward routine definitions.
 */
# ifdef __STDC__
	static CTable * ct_AskDMForTable (char *);
	static int ct_GetDMResponse (struct message *, struct dm_ctable *);
	static void ct_DoAlloc (CTable *);
	static int ct_MarkDealloc (char *, int, union usy_value *, int);
# else
	static CTable * ct_AskDMForTable ();
	static int ct_GetDMResponse ();
	static void ct_DoAlloc ();
	static int ct_MarkDealloc ();
# endif

ct_Init ()
/*
 * Initialize the color table code.
 */
{
	Ctable = usy_c_stbl ("colors");
	ct_ClearColors ();
}




bool
ct_LoadTable (name, colors, ncolor)
char *name;
XColor **colors;
int *ncolor;
/*
 * Obtain a named color table.
 * Entry:
 *	NAME	is the name of the color table of interest.
 * Exit:
 *	If the color table exists then:
 *	COLORS	is an array of XColor structs describing the table.
 *	NCOLOR	is the length of that array
 *		The return value is TRUE
 *	Else
 *		The return value is false.
 */
{
	union usy_value v;
	int type;
	CTable *ct;
/*
 * Try to find this table in our cache.
 */
	if (usy_g_symbol (Ctable, name, &type, &v))
		ct = (CTable *) v.us_v_ptr;
/*
 * Otherwise go and get it.
 */
	else if (! (ct = ct_AskDMForTable (name)))
		return (FALSE);
/*
 * If we need to go and do an alloc on this table, do it now.
 */
	if (! ct->ct_alloc)
		ct_DoAlloc (ct);
/*
 * Return the info.
 */
	*colors = ct->ct_colors;
	*ncolor = ct->ct_ncolor;
	return (TRUE);
}





static CTable *
ct_AskDMForTable (name)
char *name;
/*
 * Attempt to obtain this color table from the display manager.
 */
{
	struct dm_ctr ctr;
	struct dm_ctable repl;
	CTable *ct;
	union usy_value v;
/*
 * Send off the color table request.
 */
	ctr.dmm_type = DM_R_CTABLE;
	strcpy (ctr.dmm_table, name);
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &ctr, sizeof (ctr));
/*
 * Now await the reply.
 */
	repl.dmm_ncolor = -1;
	msg_Search (MT_DISPLAYMGR, ct_GetDMResponse, &repl);
/*
 * If it failed, we fail too.
 */
	if (repl.dmm_ncolor == -1)
		return (0);
/*
 * Otherwise it's time to allocate and fill in a ctable structure.
 */
	ct = ALLOC (CTable);
	strcpy (ct->ct_name, name);
	ct->ct_ncolor = repl.dmm_ncolor;
	ct->ct_colors = (XColor *) malloc (ct->ct_ncolor * sizeof (XColor));
	memcpy (ct->ct_colors, repl.dmm_cols, ct->ct_ncolor*sizeof (XColor));
	ct->ct_alloc = FALSE;
/*
 * Store a symbol table entry.
 */
	v.us_v_ptr = (char *) ct;
	usy_s_symbol (Ctable, name, SYMT_POINTER, &v);
/*
 * All done.
 */
	return (ct);
}






static int
ct_GetDMResponse (msg, ctr)
struct message *msg;
struct dm_ctable *ctr;
/*
 * The color table search routine, called out of msg_Search.
 */
{
	struct dm_ctable *dmm = (struct dm_ctable *) msg->m_data;
/*
 * If this is not a color table response, blow it off.
 */
	if (dmm->dmm_type != DM_TABLE && dmm->dmm_type != DM_NOTABLE)
		return (1);
/*
 * Got it.  Copy out the info and return.
 */
	if (dmm->dmm_type == DM_TABLE)
		*ctr = *dmm;
	return (0);
}






static void 
ct_DoAlloc (ct)
CTable *ct;
/*
 * Allocate all of the colors in this table from the server.
 */
{
	int color;
	Display *disp = XtDisplay (Top);
	Colormap cm = DefaultColormap (disp, 0);
/*
 * Just go through and get them all from the server.  In theory, DM has
 * already cleared all of these color values, so there should be no
 * problem with this.
 */
	for (color = 0; color < ct->ct_ncolor; color++)
	{
		if (! XAllocColor (disp, cm, ct->ct_colors + color))
			msg_log ("Color alloc failure");
		ct_MarkColor (ct->ct_colors[color].pixel);
	}
	ct->ct_alloc = TRUE;
}




void
ct_FreeColors ()
/*
 * Clear out the allocated colors.
 */
{
	int np = 0, i;
	unsigned long pixels[4096];
/*
 * Go through and make a list of allocated pixels.
 */
	for (i = 0; i < MaxAlloc; i++)
		if (ct_ColorIsAlloc (i))
			pixels[np++] = i;
/*
 * Release them all in one swell foop.
 */
	XFreeColors (XtDisplay (Top), DefaultColormap (XtDisplay (Top), 0),
		pixels, np, 0);
	ct_ClearColors ();
/*
 * Traverse through the symbol table, and mark every entry as unallocated.
 */
	usy_traverse (Ctable, ct_MarkDealloc, 0, FALSE);
}





static int
ct_MarkDealloc (ctable, type, v, junk)
char *ctable;
int type, junk;
union usy_value *v;
/*
 * Mark this color table as unallocated.
 */
{
	CTable *ct = (CTable *) v->us_v_ptr;

	ct->ct_alloc = FALSE;
	return (TRUE);
}




void
ct_DeleteTable (name)
char *name;
/*
 * Cause this color table not to exist.  This routine does *not* deallocate
 * colors used by this table.
 */
{
	CTable *ct;
	union usy_value v;
	int type;
/*
 * Look the table up.
 */
	if (! usy_g_symbol (Ctable, name, &type, &v))
		return;
/*
 * Deallocate memory.
 */
	ct = (CTable *) v.us_v_ptr;
	free (ct->ct_colors);
	free (ct);
/*
 * Get rid of the table entry.
 */
	usy_z_symbol (Ctable, name);
}





int
ct_GetColorByName (name, color)
char *name;
XColor *color;
/*
 * Grab an individual color by name.
 */
{
	XColor junk;

	if (XAllocNamedColor (XtDisplay (Top), 
		DefaultColormap (XtDisplay (Top), 0), name, color, &junk))
	{
		ct_MarkColor (color->pixel);
		return (TRUE);
	}
	return (FALSE);
}






int
ct_GetColorByRGB (color)
XColor *color;
/*
 * Grab an individual color by RGB values.
 */
{
	if (XAllocColor (XtDisplay (Top), 
			DefaultColormap (XtDisplay (Top), 0), color))
	{
		ct_MarkColor (color->pixel);
		return (TRUE);
	}
	return (FALSE);
}
