/*
 * Ingest P3 composite image files.
 *
 * p3comp_ingest platform files....
 */
# include <sys/types.h>
# include <unistd.h>
# include <fcntl.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>

# include "P3Composite.h"

int MHandler (Message *msg) {}


main (argc, argv)
int argc;
char **argv;
{
	int fd, file, dv, i, len;
	P3Header ph;
	char string[80];
	FieldId fid;
	ScaleInfo scale;
	unsigned char *grid;
	short *srcgrid;
	DataChunk *dc;
	RGrid rg;
	PlatformId pid;
	Location origin;
	ZebTime zt;
/*
 * Initialization.
 */
	msg_connect (MHandler, "p3comp_ingest");
	usy_init ();
	ds_Initialize ();
	fid = F_Lookup ("reflectivity");
	if ((pid = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		printf ("Unknown platform: %s", argv[1]);
		exit (1);
	}
/*
 * Do scaling stuff.
 */
	scale.s_Scale = 2.0;
	scale.s_Offset = -30.0;
/*
 * Now we plow through the files.
 */
	for (file = 2; file < argc; file++)
	{
	/*
	 * Open up the file.
	 */
		if ((fd = open (argv[file], O_RDONLY)) < 0)
		{
			perror (argv[file]);
			continue;
		}
	/*
	 * Get the header.
	 */
		SkipRecordHeader (fd);
		read (fd, &ph, sizeof (ph));
		SkipRecordHeader (fd);
		strncpy (string, ph.ph_Flight, 8);  string[8] = 0;
		printf ("Flight: %s, ", string);
		strncpy (string, ph.ph_Name, 16); string[16] = 0;
		printf ("name: %s\n", string);
		printf("from %d/%d/%d %d:%02d:%02d to %d/%d/%d %d:%02d:%02d\n",
			ph.ph_StYear, ph.ph_StMonth, ph.ph_StDay, ph.ph_StHour,
			ph.ph_StMinute, ph.ph_StSecond, ph.ph_EndYear,
			ph.ph_EndMonth, ph.ph_EndDay, ph.ph_EndHour,
			ph.ph_EndMinute, ph.ph_EndSecond);
		printf ("Res %d by %d, spacing %d %dm, %d sweeps\n", ph.ph_Nx,
			ph.ph_Ny, ph.ph_XSpacing, ph.ph_YSpacing,
			ph.ph_NSweep);
		printf ("Origin at %.2f %.2f, alt %d; badval %d\n", 
			ph.ph_Lat/10000.0, ph.ph_Lon/10000.0, ph.ph_Alt,
			ph.ph_BadVal);
		len = ph.ph_Nx*ph.ph_Ny;
	/*
	 * Get the data grid.
	 */
	 	srcgrid = (short *) malloc (len*sizeof (short));
		grid = malloc (len);
		ReadGrid (fd, srcgrid, ph.ph_Nx, ph.ph_Ny);
	/*
	 * Start putting together our data chunk.
	 */
		dc = dc_CreateDC (DCC_Image);
		dc->dc_Platform = pid;
		dc_ImgSetup (dc, 1, &fid, &scale);
	/*
	 * Other descriptive stuff.
	 */
	 	rg.rg_nX = ph.ph_Nx;
		rg.rg_nY = ph.ph_Ny;
		rg.rg_nZ = 1;
		rg.rg_Xspacing = ph.ph_XSpacing/1000.0;
		rg.rg_Yspacing = ph.ph_YSpacing/1000.0;
		rg.rg_Zspacing = 0;
		origin.l_lat = ph.ph_Lat/10000.0;
		origin.l_lon = ph.ph_Lon/10000.0;
		origin.l_alt = ph.ph_Alt/1000.0;	/* -> km */
		TC_ZtAssemble (&zt, ph.ph_StYear - 1900, ph.ph_StMonth,
			ph.ph_StDay, ph.ph_StHour, ph.ph_StMinute,
			ph.ph_StSecond, 0);
	/*
	 * Convert the grid over.
	 */
		for (i = 0; i < len; i++)
		{
			dv = (srcgrid[i] - scale.s_Offset)/scale.s_Scale;
			grid[i] = (dv < 0 || dv > 255) ? 0xff :
				(unsigned char) dv;
		}
	/*
	 * Store and release it all.
	 */
		dc_ImgAddImage (dc, 0, fid, &origin, &rg, &zt, grid, len);
	 	ds_Store (dc, TRUE, 0, 0);
		dc_DestroyDC (dc);
		free (srcgrid);
		close (fd);
	}
}




ReadGrid (fd, grid, nx, ny)
int fd, nx, ny;
short *grid;
/*
 * Pull in a grid file.
 */
{
	for (; ny > 0; ny--)
	{
		SkipRecordHeader (fd);
		read (fd, grid + nx*(ny - 1), nx*sizeof (short));
		SkipRecordHeader (fd);
	}
}




SkipRecordHeader (fd)
int fd;
/*
 * Go past the fortran record headers.
 */
{
	lseek (fd, 4, SEEK_CUR);
}
