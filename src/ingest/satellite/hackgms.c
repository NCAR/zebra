/*
 * Program to rewrite old (uncalibrated) GMS images.
 *
 * Build calibration table
 * Get humungo list of data times.
 * for each time
 *	read image
 *	recalibrate each pixel
 *	set scale info
 *	write image.
 */

# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <timer.h>


int MHandler (Message *);
const int NTIME = 300;

/*
 * Lookup table for decoding GMS IR values.
 */
float GMS_table[256] = {
	349.438, 345.613, 342.456, 339.854, 337.711, 335.945, 334.488, 333.280,
	332.275, 331.430, 330.713, 330.096, 329.556, 329.075, 328.637, 328.232,
	327.850, 327.484, 327.127, 326.777, 326.430, 326.084, 325.737, 325.388,
	325.038, 324.685, 324.331, 323.973, 323.614, 323.254, 322.891, 322.528,
	322.164, 321.799, 321.433, 321.067, 320.700, 320.333, 319.966, 319.598,
	319.230, 318.861, 318.492, 318.122, 317.751, 317.380, 317.007, 316.633,
	316.258, 315.882, 315.504, 315.125, 314.745, 314.363, 313.980, 313.596,
	313.210, 312.823, 312.434, 312.045, 311.653, 311.261, 310.868, 310.473,
	310.077, 309.681, 309.283, 308.884, 308.484, 308.084, 307.682, 307.279,
	306.876, 306.472, 306.066, 305.660, 305.253, 304.844, 304.435, 304.024,
	303.613, 303.200, 302.786, 302.371, 301.954, 301.536, 301.116, 300.696,
	300.273, 299.849, 299.424, 298.997, 298.568, 298.138, 297.706, 297.272,
	296.836, 296.399, 295.960, 295.520, 295.077, 294.633, 294.187, 293.740,
	293.291, 292.840, 292.387, 291.933, 291.477, 291.019, 290.560, 290.099,
	289.636, 289.172, 288.706, 288.239, 287.769, 287.298, 286.826, 286.351,
	285.875, 285.397, 284.917, 284.435, 283.951, 283.465, 282.978, 282.488,
	281.996, 281.502, 281.005, 280.507, 280.006, 279.502, 278.997, 278.488,
	277.977, 277.464, 276.948, 276.429, 275.907, 275.383, 274.855, 274.325,
	273.792, 273.256, 272.717, 272.175, 271.630, 271.082, 270.531, 269.977,
	269.419, 268.859, 268.295, 267.728, 267.157, 266.583, 266.006, 265.426,
	264.842, 264.255, 263.664, 263.069, 262.471, 261.869, 261.264, 260.654,
	260.041, 259.423, 258.801, 258.175, 257.544, 256.909, 256.269, 255.625,
	254.975, 254.321, 253.661, 252.996, 252.325, 251.649, 250.967, 250.279,
	249.585, 248.885, 248.178, 247.464, 246.744, 246.017, 245.283, 244.542,
	243.749, 243.037, 242.274, 241.502, 240.722, 239.934, 239.138, 238.333,
	237.519, 236.696, 235.863, 235.021, 234.169, 233.307, 232.434, 231.549,
	230.654, 229.746, 228.826, 227.893, 226.946, 225.985, 225.009, 224.017,
	223.009, 221.984, 220.941, 219.878, 218.796, 217.692, 216.567, 215.418,
	214.245, 213.046, 211.820, 210.565, 209.280, 207.962, 206.611, 205.223,
	203.796, 202.327, 200.813, 199.251, 197.634, 195.959, 194.219, 192.405,
	190.511, 188.524, 186.433, 184.222, 181.874, 179.367, 176.678, 173.776,
	170.626, 167.188, 163.413, 159.246, 154.621, 149.460, 143.676, 137.163
};

main (argc, argv)
int argc;
char **argv;
{
	PlatformId inplat, outplat;
	ZebTime dtimes[NTIME], now;
	ScaleInfo scale, inscale;
	RGrid rg;
	FieldId fid;
	int ntime, i, t, len, b;
	unsigned char lookup[256], *grid;
	DataChunk *dc, *outdc;
	Location where;
/*
 * Initialization type stuff.
 */
	msg_connect (MHandler, "BrutalGMSHack");
	usy_init ();
	ds_Initialize ();
/*
 * See what our platforms look like.
 */
	if ((inplat = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		printf ("Bad platform %s\n", argv[1]);
		exit (1);
	}
	if ((outplat = ds_LookupPlatform (argv[2])) == BadPlatform)
	{
		printf ("Bad platform %s\n", argv[2]);
		exit (1);
	}
/*
 * Other initialization.
 */
	fid = F_Lookup ("ir");
/*
 * Throw together the IR calibration.
 */
	scale.s_Scale = 1.0;
	scale.s_Offset = GMS_table[255];
	for (i = 0; i < 255; i++)
	{
		int ival = GMS_table[i] - scale.s_Offset;
		lookup[i] = (unsigned char) ival;
	}
/*
 * See what our times are.
 */
	tl_Time (&now);
	ntime = ds_DataTimes (inplat, &now, NTIME, DsBefore, dtimes);
	printf ("Got %d times\n", ntime);
/*
 * Start plowing through.
 */
	for (t = 0; t < ntime; t++)
	{
	/*
	 * Babble.
	 */
		char fdate[30];
		TC_EncodeTime (dtimes + t, TC_Full, fdate);
		printf ("Doing time %d/%d at %s\n", t, ntime, fdate);
	/*
	 * Get the stuff.
	 */
		if (! (dc = ds_Fetch (inplat, DCC_Image, dtimes + t, 
				dtimes + t, &fid, 1, 0, 0)))
		{
			printf ("Fetch failure!\n");
			continue;
		}
		grid = dc_ImgGetImage (dc, 0, fid, &where, &rg, &len,&inscale);
	/*
	 * Maybe this one is already done.
	 */
		if (inscale.s_Offset != 0)
			printf ("...this one already calibrated\n");
	/*
	 * Nope.  Go through and tweak all the bytes.
	 */
		else
		{
			for (b = 0; b < len; b++)
				grid[b] = lookup[grid[b]];
		}
	/*
	 * Make up a new data chunk with this stuff.
	 */
	 	outdc = dc_CreateDC (DCC_Image);
		outdc->dc_Platform = outplat;
		dc_ImgSetup (outdc, 1, &fid, &scale);
		dc_ImgAddImage (outdc, 0, fid, &where, &rg, dtimes + t, 
			grid, len);
		dc_SetGlobalAttr (dc, "HackGMS version", "1.0");
	/*
	 * Store it and clean up.
	 */
		ds_Store (outdc, TRUE, 0, 0);
		dc_DestroyDC (dc);
		dc_DestroyDC (outdc);
	}
}


int MHandler (Message *msg) {}
