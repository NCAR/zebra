/*
 * Consume images into the data store.
 */

static char *rcsid = "$Id: ds_consumer.c,v 1.1 1991-04-28 17:38:15 corbet Exp $";

# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ImageXfr.h>



/*
 * Globals.
 */
static int XRes, YRes, NField;
static char *Fields[20];
struct _ix_desc *ShmDesc;

PlatformId Plat;

DataObject OutData;

# ifdef __STDC__
	static int MDispatcher (struct message *);
# else
# endif


main (argc, argv)
int argc;
char **argv;
{
	usy_init ();
	msg_connect (MDispatcher, "DS Consumer");
/*
 * Checking.
 */
	if (argc != 2)
	{
		msg_ELog (EF_PROBLEM, "ds_Consumer miscalled");
		exit (1);
	}
/*
 * Hook into our segment.
 */
	if (! (ShmDesc = IX_HookIn (0x910425, "DS Consumer", &XRes, &YRes,
				&NField, Fields)))
	{
		msg_ELog (EF_EMERGENCY, "NO SHM segment");
		exit (1);
	}
/*
 * DS initialization.
 */
	ds_Initialize ();
	if ((Plat = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "Bad platform %s", argv[1]);
		exit (1);
	}
/*
 * Static data object initialization.
 */
	OutData.do_id = Plat;
	OutData.do_org = OrgImage;
	OutData.do_npoint = 1;
	OutData.do_aloc = ALLOC (Location);
	/* XXXXXXXXX */
	OutData.do_nfield = 2;
	OutData.do_fields[0] = "reflectivity";
	OutData.do_fields[1] = "velocity";
	/* XXXXXXXXX */
	OutData.do_flags = 0;
/*
 * Wait for something.
 */
	msg_await ();
}





static int
MDispatcher (msg)
struct message *msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE)
		{
			IX_Detach (ShmDesc);
			exit (1);
		}
		break;

	   case MT_IMAGEXFR:
	   	DoImage (tmpl->mh_type);
		break;
	}
	return (0);
}   	





DoImage (set)
int set;
/*
 * Do this image set.
 */
{
	RGrid rg;
	Location loc;
	time t;
	ScaleInfo scale[2];
	char *images[4];
	int xmin, ymin, xmax, ymax, i;
	static float PrevAlt = -99;
/*
 * Grab it.
 */
	if (! IX_GetReadFrame (ShmDesc, set, images, &t, &rg, &loc, scale,
			&xmin, &ymin, &xmax, &ymax))
	{
		msg_ELog (EF_PROBLEM, "Can't get promised set %d", set);
		return;
	}
/*
 * Fill in the rest of our data object.
 */
	printf ("Set %d, [%d, %d] to [%d, %d]\n", set, xmin, ymin, xmax, ymax);
	OutData.do_begin = OutData.do_end = t;
	OutData.do_times = &t;
	OutData.do_desc.d_img.ri_rg = &rg;
	OutData.do_desc.d_img.ri_scale = scale;
	OutData.do_aloc = &loc;
	for (i = 0; i < OutData.do_nfield; i++)
		OutData.do_data[i] = (float *) images[i];
/*
 * Send it.
 */
	ds_PutData (&OutData, PrevAlt > loc.l_alt);
	PrevAlt = loc.l_alt;
/*
 * Clear and return the frames.
 */
	for (i = 0; i < OutData.do_nfield; i++)
		memset (images[i] + ymin*XRes, 0xff, (ymax - ymin + 1)*XRes);
	IX_ReleaseFrame (ShmDesc, set);
}
