/*
 * Routines to effect image transfer through shared memory.
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


# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <errno.h>

# include "defs.h"
# include "message.h"
# include <DataStore.h>
# include "ImageXfr.h"

MAKE_RCSID("$Id: ImageXfr.c,v 2.7 1994-10-11 16:26:52 corbet Exp $")

# define MAXATTR	100		/* Max attribute space	*/

# define MAXFRAME 10
/*
 * The header of a shared memory image transfer segment.
 */
struct ix_header
{
	int	ixh_Magic;		/* Magic number = IX_MAGIC	*/
	int	ixh_NSet;		/* Number of image sets		*/
	int	ixh_FrPerSet;		/* Frames per set		*/
	int	ixh_XSize, ixh_YSize;	/* Size of our frames		*/
	char	ixh_Consumer[80];	/* Proc name of consumer	*/
	char	ixh_Fields[MAXFRAME][40];	/* Field names		*/
};
# define IX_MAGIC 0x910714	/* A grim day	*/

/*
 * Immediately after the header come the set descriptors.
 */
typedef enum _SetOwner  { NoOwner, WriteOwner, ReadOwner } SetOwner;
struct set_desc
{
	SetOwner	sd_Owner;	/* Who owns this set		*/
	UItime		sd_Time;	/* When.			*/
	RGrid		sd_Rgrid;	/* Grid information		*/
	Location	sd_Where;	/* Location information		*/
	ScaleInfo	sd_Scale[MAXFRAME];	/* Scaling information	*/
	int	 	sd_Frames[MAXFRAME];	/* Where the data is	*/
	int		sd_XMin, sd_XMax; /* Parts of the grid that have*/
	int		sd_YMin, sd_YMax; /* real data in them.		*/
	char		sd_Attr[MAXATTR];	/* Attribute table	*/
};


/*
 * This is the structure we pass around to keep track of our stuff.
 */
typedef struct _ix_desc
{
	struct ix_header	*id_hdr;
	struct set_desc		*id_desc;
	int			id_shmid;	/* The shared mem segment */
	char			*id_shmseg;	/* The beginning of the seg */
	int			id_write;	/* Write access?	*/
	int			id_len;		/* Segment len		*/
} ix_desc;






ix_desc *
IX_Create (key, xres, yres, nframe, nset, fields)
int key, xres, yres, nframe, nset;
char **fields;
/*
 * Create an image transfer segment associated with this key.
 */
{
	int len, frame, set;
	ix_desc *desc = ALLOC (ix_desc);
	char *mem;
/*
 * Figure out how big this segment is going to need to be.
 */
	len = xres*yres*nframe*nset + sizeof (struct ix_header) +
			nframe*sizeof (struct set_desc); /* BIG */
	msg_ELog (EF_DEBUG, "IX seg len %d", len);
/*
 * Now we try to make it exist.
 */
	if ((desc->id_shmid = shmget (key, len, IPC_CREAT | 0777)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Error %d getting shmseg", errno);
		free (desc);
		return (0);
	}
/*
 * Attach to the silly thing.
 */
	if ((desc->id_shmseg = shmat (desc->id_shmid, 0, 0)) == (char *) -1)
	{
		msg_ELog (EF_EMERGENCY, "Error %d attaching shmseg", errno);
		free (desc);
		return (0);
	}
/*
 * Fill in the rest of our descriptor.
 */
	desc->id_hdr = (struct ix_header *) desc->id_shmseg;
	desc->id_desc = (struct set_desc *) (desc->id_shmseg +
			sizeof (struct ix_header));
	desc->id_write = TRUE;
	desc->id_len = len;
/*
 * Fill in the segment header.
 */
	desc->id_hdr->ixh_Magic = IX_MAGIC;
	desc->id_hdr->ixh_NSet = nset;
	desc->id_hdr->ixh_FrPerSet = nframe;
	desc->id_hdr->ixh_XSize = xres; 
	desc->id_hdr->ixh_YSize = yres; 
	desc->id_hdr->ixh_Consumer[0] = 0;
/*
 * Fill in the per-frame information.
 */
	for (frame = 0; frame < nframe; frame++)
		strcpy (desc->id_hdr->ixh_Fields[frame], fields[frame]);
/*
 * Finally, allocate the memory for each frame and initialize the set
 * ownership.
 */
	mem = desc->id_shmseg + sizeof (struct ix_header) +
				nset*sizeof (struct set_desc);
	for (set = 0; set < nset; set++)
	{
		struct set_desc *sd = desc->id_desc + set;
		sd->sd_Owner = NoOwner;
		for (frame = 0; frame < nframe; frame++)
		{
			sd->sd_Frames[frame] = mem - desc->id_shmseg;
			mem += xres*yres;
		}
	}
/*
 * Done!
 */
	return (desc);
}






ix_desc *
IX_HookIn (key, name, xres, yres, nfield, fields)
int key;
char *name;
int *xres, *yres, *nfield;
char **fields;
/*
 * Hook into this segment as a consumer.
 */
{
	ix_desc *desc = ALLOC (ix_desc);
	int field;
/*
 * Try to hook into this segment.
 */
	if ((desc->id_shmid = shmget (key, 0, 0)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Error %d getting shmseg", errno);
		free (desc);
		return (0);
	}
/*
 * Attach to the silly thing.
 */
	if ((desc->id_shmseg = shmat (desc->id_shmid, 0, 0)) == (char *) -1)
	{
		msg_ELog (EF_EMERGENCY, "Error %d attaching shmseg", errno);
		free (desc);
		return (0);
	}
/*
 * Initialize the descriptor.
 */
	desc->id_hdr = (struct ix_header *) desc->id_shmseg;
	desc->id_desc = (struct set_desc *) (desc->id_shmseg +
			sizeof (struct ix_header));
	desc->id_write = FALSE;
	if (desc->id_hdr->ixh_Magic != IX_MAGIC)
		msg_ELog (EF_PROBLEM, "SHM Magic number 0x%x bogus",
				desc->id_hdr->ixh_Magic);
/*
 * Assert ourselves as the consumer.
 */
	if (desc->id_hdr->ixh_Consumer[0])
		msg_ELog (EF_PROBLEM, "Replacing IX consumer %s",
			desc->id_hdr->ixh_Consumer);
	strcpy (desc->id_hdr->ixh_Consumer, name);
/*
 * Done.
 */
	*xres = desc->id_hdr->ixh_XSize;
	*yres = desc->id_hdr->ixh_YSize;
	*nfield = desc->id_hdr->ixh_FrPerSet;
	for (field = 0; field < *nfield; field++)
		fields[field] = desc->id_hdr->ixh_Fields[field];
	return (desc);
}



static char *
IX_POwner (owner)
SetOwner owner;
{
	switch (owner)
	{
	   case ReadOwner:
	   	return ("ReadOwner");
	   case WriteOwner:
	   	return ("WriteOwner");
	   case NoOwner:
	   	return ("NoOwner");
	   default:
	   	return ("BizarreOwner");
	}
}




int
IX_GetWriteFrame (desc, frames, verbose)
ix_desc *desc;
char **frames;
int verbose;
/*
 * Get a frame for write access.  If "verbose" is true, failures should be
 * logged.
 */
{
	int set, frame;
	struct set_desc *sd;
	static bool Failed = FALSE;
/*
 * Pass through the sets looking for one that is free.
 */
	for (set = 0; set < desc->id_hdr->ixh_NSet; set++)
		if (desc->id_desc[set].sd_Owner != ReadOwner)
			break;
	if (set >= desc->id_hdr->ixh_NSet)
	{
		if (! Failed && verbose)
		{
			msg_ELog (EF_PROBLEM,"No image segs (of %d) available",
				desc->id_hdr->ixh_NSet);
			for (set = 0; set < desc->id_hdr->ixh_NSet; set++)
				msg_ELog (EF_PROBLEM, "Set %d own %s", set,
				   IX_POwner (desc->id_desc[set].sd_Owner));
		}
		Failed = TRUE;
		return (-1);
	}
	Failed = FALSE;
/*
 * Mark the segment as owned by the writer, return the frames, and we are
 * done.
 */
	sd = desc->id_desc + set;
	sd->sd_Owner = WriteOwner;
	for (frame = 0; frame < desc->id_hdr->ixh_FrPerSet; frame++)
		frames[frame] = sd->sd_Frames[frame] + desc->id_shmseg;
	return (set);
}






void
IX_SendFrame (desc, set, when, rg, loc, scale, xmin, ymin, xmax, ymax, attr)
ix_desc *desc;
UItime *when;
RGrid *rg;
Location *loc;
ScaleInfo *scale;
int set, xmin, ymin, xmax, ymax;
char *attr;
/*
 * Send this frame to the recipient.
 */
{
	struct ix_header *hdr = desc->id_hdr;
	struct set_desc *sd = desc->id_desc + set;
	int i;
/*
 * If there is no consumer, we simply mark the frame unowned and return.
 */
	if (! hdr->ixh_Consumer[0])
	{
		sd->sd_Owner = NoOwner;
		return;
	}
/*
 * Otherwise set the owner to read and inform the consumer.
 */
	sd->sd_Owner = ReadOwner;
	sd->sd_XMin = xmin; sd->sd_YMin = ymin;
	sd->sd_XMax = xmax; sd->sd_YMax = ymax;
	sd->sd_Time = *when;
	sd->sd_Rgrid = *rg;
	sd->sd_Where = *loc;
	strncpy (sd->sd_Attr, attr, MAXATTR);
	for (i = 0; i < hdr->ixh_FrPerSet; i++)
		sd->sd_Scale[i] = scale[i];
	msg_send (hdr->ixh_Consumer, MT_IMAGEXFR, FALSE, &set, sizeof (int));
}





int
IX_GetReadFrame (desc, set, frames, when, rg, loc, scale, xmin, ymin,
			xmax, ymax, attr)
ix_desc *desc;
int set;
char **frames;
UItime *when;
RGrid *rg;
Location *loc;
ScaleInfo *scale;
int *xmin, *ymin, *xmax, *ymax;
char **attr;
/*
 * Get an available frame.
 */
{
	struct set_desc *sd = desc->id_desc + set;
	int frame;
/*
 * If this set is not set read owner then we can't have it.
 */
	if (sd->sd_Owner != ReadOwner)
	{
		msg_ELog (EF_PROBLEM, "Try to get nonread set %d", set);
		return (FALSE);
	}
/*
 * Copy out the info.
 */
	for (frame = 0; frame < desc->id_hdr->ixh_FrPerSet; frame++)
		frames[frame] = desc->id_shmseg + sd->sd_Frames[frame];
	*when = sd->sd_Time;
	*rg = sd->sd_Rgrid;
	*loc = sd->sd_Where;
	for (frame = 0; frame < desc->id_hdr->ixh_FrPerSet; frame++)
		scale[frame] = sd->sd_Scale[frame];
	*xmin = sd->sd_XMin;  *xmax = sd->sd_XMax;
	*ymin = sd->sd_YMin;  *ymax = sd->sd_YMax;
	*attr = sd->sd_Attr;

	return (TRUE);
}




void
IX_ReleaseFrame (desc, set)
ix_desc *desc;
int set;
/*
 * Release this frame set back to the writer.
 */
{
	struct set_desc *sd = desc->id_desc + set;
/*
 * If we don't own it, we can't release it.
 */
	if (sd->sd_Owner != ReadOwner)
		msg_ELog (EF_PROBLEM, "Try to release non-read set %d", set);
	else
		sd->sd_Owner = NoOwner;
}






void
IX_Detach (desc)
ix_desc *desc;
/*
 * Unhook from this segment.
 */
{
	shmdt (desc->id_shmseg);
	if (desc->id_write)
		shmctl (desc->id_shmid, IPC_RMID, 0);
	free (desc);
}





void
IX_LockMemory (desc)
ix_desc *desc;
/*
 * Attempt to lock this memory segment into physical core.  One must be
 * root to do this....
 */
{
# ifdef sun
	if (mlock (desc->id_shmseg, desc->id_len) < 0)
		msg_ELog (EF_PROBLEM, "Error %d locking shm segment", errno);
# endif
}




void
IX_Initialize (desc, value)
ix_desc *desc;
unsigned char value;
/*
 * Initialize the entire segment to the given value.
 */
{
	struct ix_header *hdr = desc->id_hdr;

	memset (desc->id_shmseg + desc->id_desc->sd_Frames[0], value,
		hdr->ixh_NSet*hdr->ixh_FrPerSet*hdr->ixh_XSize*hdr->ixh_YSize);
}


char *
IX_GetConsumer (desc)
ix_desc *desc;
/*
 * Return the consumer name
 */
{
	return (desc->id_hdr->ixh_Consumer);
}
