/* The header record is 10000 bytes long, contains satellite id
and sampling information, plus a calibration curve.
*/
struct {
	char 
		Sat_id[8],
		Skip[54],
		Start_line[5],	/* starting line # of sampled image */
		End_line[5],	/* final line # of sampled image */
		Start_element[5],	/* first pixel */
		Stop_element[5],	/* last pixel */
		Line_sample[5],	/* line sampling rate */
		Element_sample[5],	/* element sampling rate */
		Nefl[5],	/* number of effective lines */
		Nefp[5],	/* number of effective pixels */
		Ntop[5],	/* north top of earth line */
		Istop[5],	/* south top of earth line */
		Unused[286 - 112],
		Longitude[25],	/* satellite subpoint location in decimal degrees */
		Latitude[25],
		Un3[216],
		IRTable[256][7],
		Un4[3841],
		VisTable[256][7],
		Un5[210],
		LineEl[121][14], /* Line and Element in 2(f7.2) format */
		Un6[1000];
	} HEADER;
#define HEADSIZE	10000	/* number of data bytes in header */
