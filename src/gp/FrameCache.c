/*
 * Frame cache maintenance.
 */
static char *rcsid = "$Id: FrameCache.c,v 1.4 1991-03-11 15:18:00 kris Exp $";
# include <X11/Intrinsic.h>
# include <errno.h>
# include <ui.h>
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"
# include "GraphicsW.h"

# define BFLEN 40
# define FLEN 200
# define OLEN 1024
# define NCACHE 40
# define PMODE 0777
# define InvalidEntry	-1
# define FREE -2

/*
 * An entry in the frame cache.  Entries are indexed by the frame number.
 */
static struct FrameCache
{
	time	fc_time;	/* The time of this entry		*/
	char	fc_base[BFLEN];	/* Base field				*/
	char	*fc_fields[FLEN]; /* Other fields			*/
	float	fc_alt;		/* Altitude (for now) of this frame	*/
	int	fc_lru;		/* LRU counter				*/
	bool	fc_keep;	/* Save this frame if possible.		*/
	bool	fc_valid;	/* Is this frame valid?			*/
	bool	fc_inmem;  	/* Is this frame in memory? 	*/
	int	fc_index;  /* pixmap index if in memory file offset if in*/
                           /* FrameFile                                  */
	char	fc_info[OLEN];	/* Holds info about the frame.  Used by */
				/* the overlay widget.		        */
} FCache[NCACHE];

/*
 *  Table indicating which pixmaps are free and which are occupied by a
 *  frame.
 */
static int FreePixmaps[NCACHE];

/*
 *  Data used to manage the file which contains the frame data not in
 *  pixmaps.
 */
static int FrameFile = InvalidEntry; /*  File descriptor                 */
static char FileName[70];       /*  The name of the FrameFile.           */
static int BufferTable[NCACHE]; /*  Hold corresponding FrameFile index   */
                                /*    and FCache index.                  */
static int BytesPerLine;	/*  Holds the correct number of bytes per*/
				/*   line when shm is not possible       */
static int Lru = 0;

void fc_PrintCache();
int fc_GetFreeFile(), fc_GetFreePixmap();


void
fc_InvalidateCache ()
/*
 * Clear out the frame cache.
 */
{
	int i;
/*
 *  Invalidate the FCache and clear the BufferTable and FreePixmaps.
 */
	for (i = 0; i < NCACHE; i++)
	{
		FCache[i].fc_valid = FCache[i].fc_keep = FALSE;
		BufferTable[i] = FREE;
		FreePixmaps[i] = InvalidEntry;
	}
/*
 *  Initialize all existing pixmaps to free.
 */
	for (i = 0; i < FrameCount; i++)
		FreePixmaps[i] = FREE;
/*
 *  Create the FrameFile and open it for read/writing.
 */
	if(FrameFileFlag)
	{
		if(FrameFile >= 0)
			close(FrameFile);
		if((FrameFile = creat(FileName, PMODE)) < 0)
			msg_ELog(EF_PROBLEM, "Can't create %s (%d).", FileName,
				 errno);
		if((FrameFile = open(FileName, 2)) < 0)
			msg_ELog(EF_PROBLEM, "Can't open %s (%d).", FileName, 
				errno);
	}
}


void fc_CreateFrameFile()
{
/*
 *  Create the FrameFile.
 */
	if(FrameFile < 0)
	{
		sprintf(FileName, "%s/%sFrameFile", FrameFilePath, Ourname);
		msg_ELog(EF_DEBUG, "FrameFile:  %s.", FileName);
		if((FrameFile = creat(FileName, PMODE)) < 0)
			msg_ELog(EF_PROBLEM, "Can't create %s (%d).", FileName,
				 errno);
		if((FrameFile = open(FileName, 2)) < 0)
			msg_ELog(EF_PROBLEM, "Can't open %s (%d).", FileName, 
				errno);
	}
}


void
fc_AddFrame (when, number)
time *when;
int number;
/*
 * Add this frame to the cache.  <number> is the pixmap index of this frame.
 */
{
	char **complist;
	int findex, i;
	char platform[BFLEN], field[BFLEN];
	
/*
 * Sanity checking.
 */
	if (number >= NCACHE)
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
 * Get an empty frame which will correspond to the pixmap <number>.
 */
	findex = fc_GetFreeFrame();
/*
 * Add the info.
 */
	FCache[findex].fc_time = *when;
	complist = pd_CompList (Pd);
	(void) (pd_Retrieve (Pd, complist[1], "field", FCache[findex].fc_base,
			SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field",
			FCache[findex].fc_base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field",
			FCache[findex].fc_base, SYMT_STRING));
	pd_Retrieve (Pd, "global", "altitude", (char *) &FCache[findex].fc_alt,
		SYMT_FLOAT);
	pd_Retrieve (Pd, complist[1], "platform", platform, SYMT_STRING);
	strcpy(FCache[findex].fc_fields, "");
	i = 2;
	while(complist[i])
	{
		(void) (pd_Retrieve (Pd, complist[i], "field", field,
				SYMT_STRING) ||
			pd_Retrieve (Pd, complist[i], "color-code-field",
				field, SYMT_STRING) ||
			pd_Retrieve (Pd, complist[i], "u-field",
				field, SYMT_STRING));
		pd_Retrieve (Pd, complist[i], "platform", platform, 
			SYMT_STRING);

		strcat(FCache[findex].fc_fields, platform);
		strcat(FCache[findex].fc_fields, field);
		i++;
	}
	msg_ELog(EF_DEBUG, "fields: (%s)", FCache[findex].fc_fields);
	FCache[findex].fc_lru = ++Lru;
	FCache[findex].fc_valid = TRUE;
	FCache[findex].fc_keep = FALSE;
	FCache[findex].fc_index = number;
	FCache[findex].fc_inmem = TRUE;
	FreePixmaps[number] = findex;
	sprintf(FCache[findex].fc_info, "%-15s%-11s%-12s%2d:%02d\n", 
		complist[1], platform, FCache[findex].fc_base, 
		when->ds_hhmmss/10000, (when->ds_hhmmss/100)%100);  
	msg_ELog (EF_DEBUG, "Cache %d, fld '%s' alt %.2f at %d %d, lru %d,
		index %d", findex, FCache[number].fc_base, 
		FCache[number].fc_alt, when->ds_yymmdd, when->ds_hhmmss, 
		FCache[number].fc_lru, number);
}


int
fc_LookupFrame (when)
time *when;
/*
 * Try to find a cache entry that matches PD at this time and return its
 * pixmap index.
 */
{
	int i, pindex, flag;
	float alt;
	char **complist, base[BFLEN], field[BFLEN], fieldlist[FLEN];
	char platform[BFLEN];

/*
 * Get the base field from the PD.
 */
	complist = pd_CompList (Pd);
	(void) (pd_Retrieve (Pd, complist[1], "field", base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field", base,
				SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field", base, SYMT_STRING));
	strcpy(fieldlist, "");
/*
 * Get the other fields from the PD.
 */
	i = 2;
	while(complist[i])
	{
		(void) (pd_Retrieve (Pd, complist[i], "field", field,
				SYMT_STRING) ||
			pd_Retrieve (Pd, complist[i], "color-code-field",
				field, SYMT_STRING) ||
			pd_Retrieve (Pd, complist[i], "u-field",
				field, SYMT_STRING));
		pd_Retrieve (Pd, complist[i], "platform", platform, 
			SYMT_STRING);

		strcat(fieldlist, platform);
		strcat(fieldlist, field);
		i++;
	}
	pd_Retrieve (Pd, "global", "altitude", (char *) &alt, SYMT_FLOAT);
/*
 * Now go searching.
 */
	for (i = 0; i < NCACHE; i++)
		if (FCache[i].fc_valid && 
		    FCache[i].fc_time.ds_yymmdd == when->ds_yymmdd &&
		    FCache[i].fc_time.ds_hhmmss == when->ds_hhmmss &&
		    FCache[i].fc_alt == alt &&
		    ! strcmp (FCache[i].fc_base, base) &&
		    ! strcmp (FCache[i].fc_fields, fieldlist))
		{
			FCache[i].fc_lru = ++Lru;
			if(! FCache[i].fc_inmem)
			{
				pindex = fc_GetFreePixmap();
				flag = fc_FileToPixmap(i, pindex); 
				if(! flag) return(-1);
			}
			else pindex = FCache[i].fc_index;
		    	return (pindex);
		}
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
time *times;
int ntime;
/*
 * Go through and mark all frames that match one of these times to be kept.
 */
{
	int frame, t;
	float alt;
	char base[BFLEN], **complist = pd_CompList (Pd);
/*
 * Get the current base field, and only mark those which match.
 */
	(void) (pd_Retrieve (Pd, complist[1], "field", base, SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "color-code-field", base,
				SYMT_STRING) ||
		pd_Retrieve (Pd, complist[1], "u-field", base, SYMT_STRING));
	pd_Retrieve (Pd, "global", "altitude", (char *) &alt, SYMT_FLOAT);
/*
 * Now go through all frames.
 */
	for (frame = 0; frame < NCACHE; frame++)
	{
		struct FrameCache *fc = FCache + frame;

		fc->fc_keep = FALSE;
		for (t = 0; fc->fc_valid && t < ntime; t++)
			if (fc->fc_time.ds_yymmdd == times[t].ds_yymmdd &&
			    fc->fc_time.ds_hhmmss == times[t].ds_hhmmss &&
			    fc->fc_alt == alt && ! strcmp (fc->fc_base, base))
			{
				fc->fc_keep = TRUE;
				break;
			}
	}
}


void
fc_UnMarkFrames()
/*
 * When the frames are no longer needed make fc_keep FALSE so that their
 * pixmaps can be reused.
 */
{

	int i;

	for(i = 0; i < NCACHE; i++)
		if(FCache[i].fc_valid)
			FCache[i].fc_keep = FALSE;
}


int
fc_FileToPixmap(frame, pixmap)
int frame, pixmap;  
/*
 *  <frame> is the FCache index of a frame which is currently in FrameFile   
 *  which is to be trasferred to a pixmap; pixmap is the index of a 
 *  <pixmap> in memory which should be data free i.e. invalid.
 *  Returns true if successful.
 */
{
	XImage *image = 0;
	int	i, framesize;

	msg_ELog(EF_DEBUG, "Moving frame %d to pixmap %d.", frame, pixmap);
/*
 *  Update important FrameFile values.
 */
# ifdef SHM
	if(GWShmPossible(Graphics))
		framesize = GWHeight(Graphics) * GWGetBPL(Graphics, pixmap);
 	else
# endif
	{
		image = XCreateImage(XtDisplay(Graphics), 0, GWDepth(Graphics),
			ZPixmap, 0, 0, GWWidth(Graphics), GWHeight(Graphics),
			8, BytesPerLine);
		framesize = GWHeight(Graphics) * image->bytes_per_line;
	 	if((image->data = (char *) malloc(framesize)) == NULL)
	 	{
	 		msg_ELog(EF_PROBLEM, "Can't allocate space for image.");
			XtFree((XImage **) image);
	 		return(FALSE);
	 	}
	}

/*
 *  Make sure pixmap to be swapped into is available.
 */
	if(FreePixmaps[pixmap] != FREE)
	{
		msg_ELog(EF_PROBLEM, "Pixmap %d to be swapped into is not
			free.", pixmap);
		if(image) 
		{
			free(image->data);
			XtFree((XImage **) image);
		}
		return(FALSE);
	}

/*
 *  Move in the frame file up to the one we want.
 */
	if(lseek(FrameFile, (long) framesize*FCache[frame].fc_index, 0) < 0)
	{
		msg_ELog(EF_PROBLEM, "Can't lseek in %s.", FileName);
		if(image) 
		{
			free(image->data);
			XtFree((XImage **) image);
		}
		return(FALSE);
	}

/*
 *  Transfer frame data from FrameFile to pixmap.
 */
#ifdef SHM
	if(GWShmPossible(Graphics))
	{
		if(read(FrameFile, GWGetFrameAddr(Graphics, pixmap), 
			framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Can't read into pixmap (%d).",
				 errno);
			return(FALSE);
		}
	}
	else
#endif
	{
		if(read(FrameFile, image->data, framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Can't read into image (%d).", 
				errno);
			free(image->data);
			XtFree((XImage **) image);
			return(FALSE);
		}
		XPutImage(XtDisplay(Graphics), GWGetFrame(Graphics, pixmap), 
			GWGetGC(Graphics), image, 0, 0, 0, 0, 
			GWWidth(Graphics), GWHeight(Graphics));  
		free(image->data);
		XtFree((XImage **) image);
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


int
fc_PixmapToFile(frame)
int frame;  
/*
 *  <frame> is the FCache index of a frame currently contained in a pixmap in 
 *  memory, but will be transferred to FrameFile.  Returns true if successful.
 */
{
	XImage *image = 0;
	int framesize, offset;

/*
 *  Update important FrameFile values.
 */
# ifdef SHM
	if(GWShmPossible(Graphics))
		framesize = GWHeight(Graphics) * GWGetBPL(Graphics, 
			FCache[frame].fc_index);
	else
# endif
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
		if(image) XDestroyImage(image);		
		return(FALSE);
	}
	msg_ELog(EF_DEBUG, "Moving frame %d to file offset %d.", frame, offset);
	if(lseek(FrameFile, (long) framesize * offset, 0) < 0)
	{
		msg_ELog(EF_PROBLEM, "Can't lseek in %s.", FileName);
		if(image) XDestroyImage(image);		
		return(FALSE);
	}

/*  
 *  Transfer frame data from pixmap into the FrameFile using shared memory
 *  pixmaps if possible.
 */
#ifdef SHM
	if(GWShmPossible(Graphics))
	{
		if(write(FrameFile, GWGetFrameAddr(Graphics, 
			FCache[frame].fc_index), framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Can't write from pixmap (%d).",
				errno);
			return(FALSE);
		}
	}
	else
#endif
	{
		if(write(FrameFile, image->data , framesize) != framesize)
		{
			msg_ELog(EF_PROBLEM, "Can't write from image (%d).",
				errno);
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


int
fc_GetFreePixmap()
/*
 *  Return the index of a pixmap that contains no data.  Move data
 *  in a pixmap to the FrameFile if necessary.  This pixmap is not
 *  at this time associated with any FCache entry.
 */
{
	int i, minlru = 9999, minframe = -1, kframe = -1, klru = 9999;
	int index;
	
	for(i = 0; i < FrameCount; i++)
	{
		if(FreePixmaps[i] == FREE)
			return(i);	
		if(FreePixmaps[i] == InvalidEntry)
			msg_ELog(EF_PROBLEM, "Problem with FreePix table.");
		index = FreePixmaps[i];
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
	
	i = (kframe >= 0) ? kframe : minframe;
	if(! fc_PixmapToFile(FreePixmaps[i]));
		FCache[FreePixmaps[i]].fc_valid = FALSE;
	return(i);
}

	
int
fc_GetFreeFrame()
/*
 *  Return the index of an entry in the FCache that contains no data.  
 *  Remove invalid or old data from the cache if necessary.  This
 *  FCache entry is not associated with any pixmap at this time. 
 */
{
	int i, minlru = 9999, minframe = -1, kframe = -1, klru = 9999;

	for(i = 0; i < NCACHE; i++)
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
	
	i = (kframe >= 0) ? kframe : minframe;
	FCache[i].fc_keep = FCache[i].fc_valid = FALSE;
	if(FCache[i].fc_inmem)
		FreePixmaps[FCache[i].fc_index] = FREE;
	else
		BufferTable[FCache[i].fc_index] = FREE;
	return(i);
}	


void
fc_SetNumFrames(n)
int n;
/*
 *  Update the FreePixmaps table whenever the FrameCount changes.
 */
{
	int i;

	for(i = 0; i < n; i++)
		if(FreePixmaps[i] == InvalidEntry)
			FreePixmaps[i] = FREE;
	for(i = n; i < NCACHE; i++)
	{
		if(FreePixmaps[i] >= 0)
			if(! fc_PixmapToFile(FreePixmaps[i]));
				FCache[FreePixmaps[i]].fc_valid = FALSE;
		FreePixmaps[i] = InvalidEntry;
	}
}


void
fc_PrintCache()
/*
 *  Print out the contents of the cache.
 */
{
	int i;

	for(i = 0; i < NCACHE; i++)
		if(FCache[i].fc_valid)
			msg_ELog(EF_DEBUG, "FCache[%d] base %s alt %f  lru %d  
				keep %d  valid %d inmem %d  index %d ", 
				i, FCache[i].fc_base,
				FCache[i].fc_alt, FCache[i].fc_lru,
				FCache[i].fc_keep, FCache[i].fc_valid,
				FCache[i].fc_inmem, FCache[i].fc_index);
}


char *
fc_GetInfo(index)
int index;
/*
 *  index is the pixmap index.  Get the FCache index and return the
 *  string in fc_info.
 */
{
	int findex;

/*  
 *  Get the frame index from the pixmap index.
 */
	findex = FreePixmaps[index];
	if(findex == FREE || findex == InvalidEntry)
	{
		msg_ELog(EF_PROBLEM, "Can't get info for pixmap %d.", index);
		return;
	}

/*
 *  Return the info.
 */
	return(usy_string(FCache[findex].fc_info));
}
	

int
fc_GetFreeFile()
/*
 *  Return an offset into the file that contains invalid data. 
 */
{
	int offset, index;
	int minlru = 9999, minframe = -1, kframe = -1, klru = 9999;

	for(offset = 0; offset < NCACHE; offset++)
	{
		if(BufferTable[offset] == FREE)
			return(offset);
		if(BufferTable[offset] < 0 || BufferTable[offset] >=NCACHE)
			msg_ELog(EF_PROBLEM, "Problem with BufferTable.");
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
	offset = (kframe >= 0) ? kframe : minframe;
	FCache[BufferTable[offset]].fc_valid = FALSE;
	BufferTable[offset] = FREE;
	return(offset);
}	
