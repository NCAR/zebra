/*
 * Frame cache maintenance.
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
# include <unistd.h>
# include <X11/Intrinsic.h>
# include <errno.h>
# include <fcntl.h>

# include <config.h>
# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include "GraphProc.h"
# include "GraphicsW.h"
# include "ActiveArea.h"

MAKE_RCSID ("$Id: FrameCache.c,v 2.21 1996-11-19 07:17:33 granger Exp $")

# define BFLEN		500
# define FLEN		40
# define PMODE		0666
# define InvalidEntry	-1
# define FREE		-2
# define PAIRBLOCK	10


/*
 * PlatformId/FieldId pairs.
 */
typedef struct pf_pair
{
	PlatformId	pfp_platform;
	FieldId		pfp_field;
} PF_Pair;


/*
 * An entry in the frame cache.  Entries are indexed by the frame number.
 */
static struct FrameCache
{
	ZebTime	fc_time;	/* The time of this entry		*/
	int	fc_foffset;	/* forecast offset time			*/
	char	fc_base[BFLEN];	/* Base field				*/
	PF_Pair	*fc_pairs;	/* Platform/field pairs.		*/
	int	fc_numpairs;	/* Number of platform/field pairs.	*/
	float	fc_alt;		/* Altitude (for now) of this frame	*/
	int	fc_lru;		/* LRU counter				*/
	bool	fc_keep;	/* Save this frame if possible.		*/
	bool	fc_valid;	/* Is this frame valid?			*/
	bool	fc_inmem;  	/* Is this frame in memory? 	*/
	int	fc_index;  /* pixmap index if in memory file offset if in*/
                           /* FrameFile                                 */
	char	*fc_info;	/* Overlay time info string		*/
	AAList	*fc_active;	/* Active area list			*/
} FCache[NCACHE];


/*
 *  Table indicating which pixmaps are free and which are occupied by a
 *  frame.  For busy pixmaps, the value is the index of the frame which is
 *  using it.
 */
static int FreePixmaps[NCACHE];

/*
 *  Data used to manage the file which contains the frame data not in
 *  pixmaps.
 */
static int FrameFile = InvalidEntry; /*  File descriptor                 */
static char FileName[100];      /*  The name of the FrameFile.           */
static int BufferTable[NCACHE]; /*  Hold corresponding FrameFile index   */
                                /*    and FCache index.                  */
static int BytesPerLine;	/*  Holds the correct number of bytes per*/
				/*   line when shm is not possible       */
static int Lru = 0;

/*
 * Our routines.
 */
static int	fc_FileToPixmap FP ((int, int));
static int	fc_PixmapToFile FP ((int));
void		fc_PrintCache FP ((void));
static int	fc_GetFreeFile FP ((void)); 
static int	fc_GetFreeFrame FP ((void)); 
static int	fc_GetFreePixmap FP ((void));
static int	fc_SetPFPairs FP ((char **, PF_Pair **));
static int	fc_ComparePairs FP ((PF_Pair *, int, PF_Pair *, int));
static void	fc_CheckDup FP ((int index));
static void	fc_CleanFrame FP ((struct FrameCache *));
static void     fc_GetFrameInfo FP ((char **, char *, int *, PF_Pair **,
			float *));




void
fc_InitFrameCache ()
/*
 * Initialize some variables for the Frame Cache.
 */
{
	int	i;

	for (i = 0; i < NCACHE; i++)
	{
		FCache[i].fc_pairs = NULL;
		FCache[i].fc_info = NULL;
		FCache[i].fc_active = NULL;
	}

	fc_InvalidateCache ();
}




void
fc_InvalidateCache ()
/*
 * Clear out the frame cache.
 */
{
	int i;
/*
 *  Reset the Lru counter.
 */
	Lru = 0;
/*
 *  Invalidate the FCache and clear the BufferTable and FreePixmaps.
 */
	for (i = 0; i < NCACHE; i++)
	{
		fc_CleanFrame (FCache + i);
		BufferTable[i] = FREE;
	}
/*
 *  Initialize all existing pixmaps to free.
 */
	for (i = 0; i < FrameCount; i++)
		FreePixmaps[i] = FREE;
/*
 *  Re-create the FrameFile and open it for read/writing.  Truncation would
 *  be more efficient -- someday.  Someday came.
 */
	if (FrameFile >= 0)
	{
#ifdef notdef
		close (FrameFile);
		if ((FrameFile = open (FileName, O_RDWR | O_CREAT |O_TRUNC, 
			PMODE)) < 0)
			msg_ELog (EF_PROBLEM, "Can't open %s (%d).", FileName, 
				errno);
		unlink (FileName);
#endif
		if (ftruncate (FrameFile, (off_t) 0) < 0)
		{
			msg_ELog (EF_PROBLEM, "Error %d truncating %s",
				  errno, FileName);
		}
	}
}





static void
fc_CleanFrame (fc)
struct FrameCache *fc;
/*
 * Clean out this frame.
 */
{
	fc->fc_valid = fc->fc_keep = FALSE;
	if (fc->fc_pairs)
	{
		free (fc->fc_pairs);
		fc->fc_pairs = NULL;
	}
	if (fc->fc_info)
	{
		free (fc->fc_info);
		fc->fc_info = NULL;
	}
	if (fc->fc_active)
	{
		aa_FreeList (fc->fc_active);
		fc->fc_active = NULL;
	}
}






void
fc_CreateFrameFile ()
/*
 *  Put together FileName and create the FrameFile for the first time.
 */
{
/*
 * Are we doing a FrameFile at all?  If not, don't do anything here.
 */
	if (! FrameFileFlag)
		return;
/*
 * Make sure a FrameFile doesn't already exist.
 */
	if (FrameFile < 0)
	{
	/*
	 * Put together the FileName.
	 */
		sprintf (FileName, "%s/%s%dFrameFile", FrameFilePath, 
			 msg_myname(), getpid ());
		msg_ELog (EF_DEBUG, "FrameFile: %s", FileName);
	/*
	 * Open it.
	 */
		if ((FrameFile = open (FileName, O_RDWR | O_CREAT | O_TRUNC, 
			PMODE)) < 0)
			msg_ELog (EF_PROBLEM, "Can't open %s (error %d).",
				  FileName, errno);
	/*
	 * Unlink so that if we die unexpectedly (or even expectedly), 
	 * the FrameFile will go away.
	 */
		unlink (FileName);
	}
}





void
fc_AddFrame (when, number)
ZebTime *when;
int number;
/*
 * Add this frame to the cache.  <number> is the pixmap index of this frame.
 */
{
	char **complist, *info, string[16];
	int findex, foffset;
/*
 * Sanity checking.
 */
	if (number >= MaxFrames)
	{
		msg_ELog (EF_PROBLEM, "Frame number %d exceeds cache", number);
		return;
	}
	if (number >= FrameCount)
	{
		msg_ELog (EF_PROBLEM, "Frame number %d exceeds FrameCount.", 
			number);
		return;
	}
/*
 * Refuse to cache any plots with only a global component
 */
	complist = pd_CompList (Pd);
	if (!complist[1])
		return;
/*
 * Forecast offset time
 */
	foffset = 0;
	if (pd_Retrieve (Pd, "global", "forecast-offset", string, SYMT_STRING))
		foffset = pc_TimeTrigger (string);
/*
 * Get an empty frame which will correspond to the pixmap <number>.
 */
	findex = fc_GetFreeFrame();
/*
 * Get the lookup info.
 */
	fc_GetFrameInfo (complist, FCache[findex].fc_base,
			&FCache[findex].fc_numpairs, &FCache[findex].fc_pairs,
			&FCache[findex].fc_alt);
/*
 * Get overlay time info from the overlay time widget
 */
	info = ot_GetString ();
	FCache[findex].fc_info = (char *) malloc (strlen (info) + 1);
	strcpy (FCache[findex].fc_info, info);
/*
 * Everything else.
 */
	FCache[findex].fc_time = *when;
	FCache[findex].fc_foffset = foffset;
	FCache[findex].fc_lru = ++Lru;
	FCache[findex].fc_valid = TRUE;
	FCache[findex].fc_keep = FALSE;
	FCache[findex].fc_index = number;
	FCache[findex].fc_inmem = TRUE;
	if (CurrentAreas)
		fc_CheckDup (findex);
	FCache[findex].fc_active = CurrentAreas;
	FreePixmaps[number] = findex;
}




static void
fc_GetFrameInfo (complist, base, npair, pairs, alt)
char **complist, *base;
int *npair;
PF_Pair **pairs;
float *alt;
{
/*
 * Get a suitable base field.
 */
	base[0] = '\0';
	(void) (pd_Retrieve (Pd, complist[1], "field", base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field", base,
				SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field", base, SYMT_STRING));
/*
 * Get altitude.
 */
	*alt = 0.0;
	pd_Retrieve (Pd, "global", "altitude", (char *) alt, SYMT_FLOAT);
/*
 * Platform field pairs.
 */
	*npair = fc_SetPFPairs (complist, pairs);
}




/*
 * Temp debug stuff
 */
static void
fc_CheckDup (index)
int index;
{
	int i;
	for (i = 0; i < MaxFrames; i++)
		if (FCache[i].fc_active == CurrentAreas)
			msg_ELog (EF_PROBLEM, "Dup AA list fr %d %d", index,i);
}






static int
fc_SetPFPairs (complist, fc_pairs)
char	**complist;
PF_Pair	**fc_pairs;
/*
 * Set up the Platform/Field pair portion of the FCache.
 */
{
	int	numpairs = 0, numblocks = PAIRBLOCK;
	int	i = 2, j;
	int	nplats;
	char	field[FLEN], platform[PlatformListLen];
	char	*pnames[MaxPlatforms];
	PF_Pair	*pairs;
/*
 * Allocate some memory.
 */
	pairs = (PF_Pair *) malloc (PAIRBLOCK * sizeof (PF_Pair));
/*
 * Loop through each component.  Start at 2 since the "base" field is split
 * off and kept elsewhere.
 */
	for (i = 2; complist[i]; i++)
	{
	/*
	 * Get the field and platform parameters from the plot description.
	 */
		field[0] = '\0';
		(void) (pd_Retrieve (Pd, complist[i], "field", field,
				SYMT_STRING) ||
			pd_Retrieve (Pd, complist[i], "color-code-field",
				field, SYMT_STRING) ||
			pd_Retrieve (Pd, complist[i], "u-field",
				field, SYMT_STRING));
	/*
	 * Kludge of sorts: set platform to "null" for map overlays to
	 * avoid spurious platform lookups with the daemon.
	 */
		if (! strcmp (field, "map"))
			strcpy (platform, "null");
		else
		{
			platform[0] = '\0';
			pd_Retrieve (Pd, complist[i], "platform", platform, 
					SYMT_STRING);
		}
	/*
	 * Loop through platform names, if necessary, storing the pairs.
	 */
		nplats = CommaParse (platform, pnames);
		for (j = 0; j < nplats; j++)
		{
		/*
		 * Allocate more memory if necessary.
		 */
			if (numpairs >= numblocks)
			{
				numblocks += PAIRBLOCK;
				pairs = (PF_Pair *) realloc (pairs, 
					numblocks * sizeof (PF_Pair));
			}
		/*
		 * Store a pair.
		 */
			pairs[numpairs].pfp_platform =
				ds_LookupPlatform (pnames[j]);
			pairs[numpairs].pfp_field = F_Lookup (field);
			numpairs++;
		}
	}
	*fc_pairs = pairs;
	return (numpairs);
}





static int
fc_ComparePairs (p1, nump1, p2, nump2)
PF_Pair	*p1, *p2;
int	nump1, nump2;
/*
 * Compare p1 and p2, return true if they are the same.
 */
{
	int	i;
/*
 * Is a match even possible?
 */
	if (nump1 != nump2)
		return (FALSE);
/*
 * Do they match?
 */
	for (i = 0; i < nump1; i++)
		if ((p1[i].pfp_platform != p2[i].pfp_platform) ||
			(p1[i].pfp_field != p2[i].pfp_field))
				return (FALSE);
/*
 * Yes they do.
 */
	return (TRUE);
}






int
fc_LookupFrame (when, info_return)
ZebTime	*when;
char **info_return;
/*
 * Try to find a cache entry that matches PD at this time and return its
 * pixmap index.  Return the overlay times info too.
 */
{
	int	i, pindex, flag, numpairs, foffset;
	float	alt;
	char	**complist, string[16], base[BFLEN];
	PF_Pair	*pairs = NULL;
/*
 * Get the base field from the PD.
 */
	base[0] = '\0';
	complist = pd_CompList (Pd);
/*
 * If there are no components other than global, then we won't find a frame
 */
	if (!complist[1])
		return (-1);
        fc_GetFrameInfo (complist, base, &numpairs, &pairs, &alt);
/*
 * Forecast offset time
 */
	foffset = 0;
	if (pd_Retrieve (Pd, "global", "forecast-offset", string, SYMT_STRING))
		foffset = pc_TimeTrigger (string);
/*
 * Now go searching.
 */
	for (i = 0; i < MaxFrames; i++)
	{
	    if (FCache[i].fc_valid && 
		FCache[i].fc_time.zt_Sec == when->zt_Sec &&
		FCache[i].fc_time.zt_MicroSec == when->zt_MicroSec &&
		FCache[i].fc_alt >= (alt - 0.005) &&
		FCache[i].fc_alt <= (alt + 0.005) &&
		FCache[i].fc_foffset == foffset &&
		! strcmp (FCache[i].fc_base, base) &&
		fc_ComparePairs (FCache[i].fc_pairs, FCache[i].fc_numpairs, 
			pairs, numpairs))
	    {
		FCache[i].fc_lru = ++Lru;
		if(! FCache[i].fc_inmem)
		{
			pindex = fc_GetFreePixmap ();
			flag = fc_FileToPixmap (i, pindex); 
			if (! flag)
			{
				if (pairs) free (pairs);
				return (-1);
			}
		}
		else 
			pindex = FCache[i].fc_index;
		if (pairs) free (pairs);
		*info_return = FCache[i].fc_info;
		aa_ReloadAreas (FCache[i].fc_active);  /* XXX */
	    	return (pindex);
	    }
	}
	if (pairs) free (pairs);
	return (-1);
}




int
fc_GetFrame ()
/*
 * Return the pixmap index of a frame to draw in.  This pixmap is not
 * associated with any FCache entry at this time.
 */
{
	return(fc_GetFreePixmap());
}





void
fc_MarkFrames (times, ntime)
ZebTime	*times;
int	ntime;
/*
 * Go through and mark all frames that match one of these times to be kept.
 */
{
	int	frame, t, numpairs, foffset;
	float	alt;
	char	string[16], base[BFLEN], **complist = pd_CompList (Pd);
	PF_Pair	*pairs = NULL;
/*
 * Make sure we've got a base component
 */
	if (!complist[1])
		return;
	fc_GetFrameInfo (complist, base, &numpairs, &pairs, &alt);
/*
 * Forecast offset time
 */
	foffset = 0;
	if (pd_Retrieve (Pd, "global", "forecast-offset", string, SYMT_STRING))
		foffset = pc_TimeTrigger (string);
/*
 * Now go through all frames.
 */
	for (frame = 0; frame < MaxFrames; frame++)
	{
		struct FrameCache *fc = FCache + frame;

		fc->fc_keep = FALSE;
		for (t = 0; fc->fc_valid && t < ntime; t++)
		{
			if (fc->fc_time.zt_Sec == times[t].zt_Sec &&
			    fc->fc_time.zt_MicroSec == times[t].zt_MicroSec &&
			    fc->fc_alt >= (alt - 0.1) &&
			    fc->fc_alt <= (alt + 0.1) &&
			    fc->fc_foffset == foffset &&
			    ! strcmp (fc->fc_base, base) &&
			    fc_ComparePairs (fc->fc_pairs, fc->fc_numpairs,
				pairs, numpairs))
			{
				fc->fc_keep = TRUE;
				break;
			}
		}
	}
	if (pairs)
		free (pairs);
}




void
fc_MarkFramesByOffset (offsets, noffsets)
int	*offsets;
int	noffsets;
/*
 * Go through and mark all frames that match one of these forecast offset
 * times to be kept.
 */
{
	int	frame, t, numpairs;
	float	alt;
	char	base[BFLEN], **complist = pd_CompList (Pd);
	PF_Pair	*pairs = NULL;
/*
 * Make sure we've got a base component
 */
	if (!complist[1])
		return;
	fc_GetFrameInfo (complist, base, &numpairs, &pairs, &alt);
/*
 * Now go through all frames.
 */
	for (frame = 0; frame < MaxFrames; frame++)
	{
		struct FrameCache *fc = FCache + frame;

		fc->fc_keep = FALSE;

		if (! fc->fc_valid ||
		    ! TC_Eq (fc->fc_time, PlotTime) ||
		    fc->fc_alt < (alt - 0.1) || 
		    fc->fc_alt > (alt + 0.1) ||
		    strcmp (fc->fc_base, base) ||
		    ! fc_ComparePairs (fc->fc_pairs, fc->fc_numpairs, 
				       pairs, numpairs))
			continue;

		for (t = 0; t < noffsets; t++)
		{
			if (fc->fc_foffset == offsets[t])
			{
				fc->fc_keep = TRUE;
				break;
			}
		}
	}
	if (pairs)
		free (pairs);
}




void
fc_UnMarkFrames ()
/*
 * When the frames are no longer needed make fc_keep FALSE so that their
 * pixmaps can be reused.
 */
{

	int i;

	for(i = 0; i < MaxFrames; i++)
		if(FCache[i].fc_valid)
			FCache[i].fc_keep = FALSE;
}





static int
fc_FileToPixmap (frame, pixmap)
int frame, pixmap;  
/*
 *  <frame> is the FCache index of a frame which is currently in FrameFile   
 *  which is to be trasferred to a pixmap; pixmap is the index of a 
 *  <pixmap> in memory which should be data free i.e. invalid.
 *  Returns true if successful.
 */
{
	XImage *image = 0;
	int	framesize;

	msg_ELog(EF_DEBUG, "Moving frame %d to pixmap %d.", frame, pixmap);
/*
 * Does a FrameFile even exist?
 */
	if (! FrameFileFlag)
		return (FALSE);
/*
 *  Update important FrameFile values.
 */
	if(GWFrameShared(Graphics, pixmap))
		framesize = GWHeight(Graphics) * GWGetBPL(Graphics, pixmap);
 	else
	{
		image = XCreateImage(XtDisplay(Graphics), 0, GWDepth(Graphics),
			ZPixmap, 0, 0, GWWidth(Graphics), GWHeight(Graphics),
			8, BytesPerLine);
		framesize = GWHeight(Graphics) * image->bytes_per_line;
	 	if((image->data = (char *) malloc(framesize)) == NULL)
	 	{
	 		msg_ELog(EF_PROBLEM,"Can't allocate space for image.");
			XDestroyImage (image);
	 		return (FALSE);
	 	}
	}
/*
 *  Make sure pixmap to be swapped into is available.
 */
	if (FreePixmaps[pixmap] != FREE)
	{
		msg_ELog(EF_PROBLEM,
				"Pixmap %d to be swapped into is not free.",
				pixmap);
		if(image) 
			XDestroyImage (image);
		return(FALSE);
	}
/*
 *  Move in the frame file up to the one we want.
 */
	if (lseek (FrameFile, (long) framesize*FCache[frame].fc_index, 0) < 0)
	{
		msg_ELog(EF_PROBLEM, "Can't lseek in %s.", FileName);
		if(image) 
			XDestroyImage (image);
		return(FALSE);
	}
/*
 *  Transfer frame data from FrameFile to pixmap.
 */
	if(GWFrameShared(Graphics, pixmap))
	{
		if (read (FrameFile, GWGetFrameAddr (Graphics, pixmap), 
				framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Can't read into pixmap (%d).",
				 errno);
			return(FALSE);
		}
	}
	else
	{
		if (read (FrameFile, image->data, framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Can't read into image (%d).", 
				errno);
			XDestroyImage (image);
			return(FALSE);
		}
		XPutImage(XtDisplay(Graphics), GWGetFrame(Graphics, pixmap), 
			GWGetGC(Graphics), image, 0, 0, 0, 0, 
			GWWidth(Graphics), GWHeight(Graphics));  
		XDestroyImage (image);
	}

/*
 *  Update the BufferTable and FCache to
 *  reflect that <frame> is no longer in the FrameFile.
 */
	BufferTable[FCache[frame].fc_index] = FREE;
	FCache[frame].fc_inmem = TRUE;
	FCache[frame].fc_index = pixmap;
	FreePixmaps[pixmap] = frame;
	return(TRUE);
}





static int
fc_PixmapToFile (frame)
int frame;  
/*
 *  <frame> is the FCache index of a frame currently contained in a pixmap in 
 *  memory, but will be transferred to FrameFile.  Returns true if successful.
 */
{
	XImage *image = 0;
	int framesize, offset;
/*
 * Does a FrameFile even exist?
 */
	if (! FrameFileFlag)
		return (FALSE);
/*
 *  Update important FrameFile values.
 */
	if(GWFrameShared(Graphics, FCache[frame].fc_index))
		framesize = GWHeight(Graphics) * GWGetBPL(Graphics, 
			FCache[frame].fc_index);
	else
	{
		image = XGetImage(XtDisplay(Graphics), GWGetFrame(Graphics,
			FCache[frame].fc_index), 0, 0, GWWidth(Graphics), 
			GWHeight(Graphics), AllPlanes, ZPixmap);
		BytesPerLine = image->bytes_per_line;
		framesize = GWHeight(Graphics) * image->bytes_per_line;
	}
/*
 *  Find a free space in the file and move there. 
 */
	if((offset = fc_GetFreeFile()) < 0)
	{
		msg_ELog(EF_PROBLEM, "Can't get space in %s.", FileName);
		if (image)
			XDestroyImage(image);		
		return(FALSE);
	}
	msg_ELog(EF_DEBUG, "Moving frame %d to file offset %d.", frame,offset);
	if (lseek (FrameFile, (long) framesize * offset, 0) < 0)
	{
		msg_ELog(EF_PROBLEM, "Can't lseek in %s.", FileName);
		if (image)
			XDestroyImage(image);		
		return(FALSE);
	}
/*  
 *  Transfer frame data from pixmap into the FrameFile using shared memory
 *  pixmaps if possible.
 */
	if (GWFrameShared(Graphics, FCache[frame].fc_index))
	{
		if (write (FrameFile, GWGetFrameAddr(Graphics, 
			    FCache[frame].fc_index), framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Error (%d) writing pixmap to %s",
				 errno, FileName);
			return(FALSE);
		}
	}
	else
	{
		if (write (FrameFile, image->data, framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Error (%d) writing image to %s",
				 errno, FileName);
			XDestroyImage(image);
			return(FALSE);
		}
		XDestroyImage(image);
	}

/*
 *  Update the BufferTable and FCache to reflect that the frame data is 
 *  now in the file.
 */
	BufferTable[offset] = frame;
	FreePixmaps[FCache[frame].fc_index] = FREE;
	FCache[frame].fc_index = offset;
	FCache[frame].fc_inmem = FALSE;
	return(TRUE);
}


static int
fc_GetFreePixmap ()
/*
 *  Return the index of a pixmap that contains no data.  Move data
 *  in a pixmap to the FrameFile if necessary.  This pixmap is not
 *  at this time associated with any FCache entry.
 */
{
	int i, minlru = 999999, minframe = -1, kframe = -1, klru = 999999;
	int index;
	
	for(i = 0; i < FrameCount; i++)
	{
		if(FreePixmaps[i] == FREE)
			return(i);	
		if(FreePixmaps[i] == InvalidEntry)
			msg_ELog(EF_PROBLEM, "Problem with FreePix table.");
		index = FreePixmaps[i];
	/*
	 * Why would it not be inmem?
	 */
		if(FCache[index].fc_inmem)
		{
			if(FCache[index].fc_lru < minlru)
			{
				minframe = i;
				minlru = FCache[index].fc_lru;
			}
			if(! FCache[index].fc_keep && 
			     FCache[index].fc_lru < klru)
			{
				kframe = i;
				klru = FCache[index].fc_lru;
			}
		}
	}
/*
 * Try to dump this frame to disk and return the pixmap.
 */
	i = (kframe >= 0) ? kframe : minframe;
	if (! fc_PixmapToFile (FreePixmaps[i]))
		fc_CleanFrame (FCache + FreePixmaps[i]);  /* oops */
	return(i);
}




	
static int
fc_GetFreeFrame ()
/*
 *  Return the index of an entry in the FCache that contains no data.  
 *  Remove invalid or old data from the cache if necessary.  This
 *  FCache entry is not associated with any pixmap at this time. 
 */
{
	int i, minlru = 999999, minframe = -1, kframe = -1, klru = 999999;
/*
 * Pass through the frame list.  Maybe we luck out and find an invalid one;
 * otherwise keep track of which one we are happiest about zapping.
 */
	for(i = 0; i < MaxFrames; i++)
	{
		if(! FCache[i].fc_valid)
			return(i);	
		if(! FCache[i].fc_lru < minlru)
		{
			minframe = i;
			minlru = FCache[i].fc_lru;
		}
		if(! FCache[i].fc_keep && FCache[i].fc_lru < klru)
		{
			kframe = i;
			klru = FCache[i].fc_lru;
		}
	}
/*
 * OK, we need to zap a frame.  Go for it.
 */
	i = (kframe >= 0) ? kframe : minframe;
	fc_CleanFrame (FCache + i);
	if(FCache[i].fc_inmem)
		FreePixmaps[FCache[i].fc_index] = FREE;
	else
		BufferTable[FCache[i].fc_index] = FREE;
	return(i);
}	





void
fc_SetNumFrames (n)
int n;
/*
 *  Update the FreePixmaps table whenever the FrameCount changes.
 */
{
	static bool	firstcall = TRUE;
	int i;
/*
 * If this is the initial call, set everything to InvalidEntry.
 */
	if (firstcall)
	{
		firstcall = FALSE;
		for (i = 0; i < MaxFrames; i++)
			FreePixmaps[i] = InvalidEntry;
	}
/*
 * If we have added new frames enable the pixmaps.
 */
	for(i = 0; i < n; i++)
		if (FreePixmaps[i] == InvalidEntry)
			FreePixmaps[i] = FREE;
/*
 * Otherwise if we have reduced frames do some cleanup.
 */
	for(i = n; i < MaxFrames; i++)
	{
		if (FreePixmaps[i] >= 0)
			fc_CleanFrame (FCache + FreePixmaps[i]);
		FreePixmaps[i] = InvalidEntry;
	}
}





void
fc_PrintCache ()
/*
 *  Print out the contents of the cache.
 */
{
	int i;

	for(i = 0; i < MaxFrames; i++)
  		if(FCache[i].fc_valid)
    			msg_ELog(EF_DEBUG, "FCache[%d] base %s alt %f  lru %d  keep %d  valid %d inmem %d  index %d ", 
				i, FCache[i].fc_base,
				FCache[i].fc_alt, FCache[i].fc_lru,
				FCache[i].fc_keep, FCache[i].fc_valid,
				FCache[i].fc_inmem, FCache[i].fc_index);
}





static int
fc_GetFreeFile ()
/*
 *  Return an offset into the file that contains invalid data. 
 */
{
	int offset, index;
	int minlru = 999999, minframe = -1, kframe = -1, klru = 999999;
/*
 * Go through the cache looking for a frame we can hand back.
 */
	for(offset = 0; offset < MaxFrames; offset++)
	{
	/*
	 * Maybe nobody is using this slot.
	 */
		if(BufferTable[offset] == FREE)
			return(offset);
		if(BufferTable[offset] < 0 || BufferTable[offset] >= MaxFrames)
			msg_ELog(EF_PROBLEM, "Problem with BufferTable.");
	/*
	 * Hmm.. I guess it's busy.  Look at the LRU counts in case we end
	 * up zapping somebody.
	 */
		index = BufferTable[offset];
		if(! FCache[index].fc_inmem)
		{
			if(FCache[index].fc_lru < minlru)
			{
				minframe = offset;
				minlru = FCache[index].fc_lru;
			}
			if(! FCache[index].fc_keep && 
			     FCache[index].fc_lru < klru)
			{
				kframe = offset;
				klru = FCache[index].fc_lru;
			}
		}
	}
/*
 * OK we gotta nail somebody.  Take a frame without keep set if possible,
 * otherwise take what we can get.
 */
	offset = (kframe >= 0) ? kframe : minframe;
	fc_CleanFrame (FCache + BufferTable[offset]);
	BufferTable[offset] = FREE;
	return(offset);
}	
