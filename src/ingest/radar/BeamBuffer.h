/*
 * The beam buffer stuff -- grab a live feed out of the radar ingestor.
 *
 * $Id: BeamBuffer.h,v 2.1 1995-06-23 19:39:08 corbet Exp $
 */
/*
 * Write side routines.
 */
int BB_Setup FP ((int key, int bsize, int nbeam));
unsigned char *BB_GetWriteBuffer FP ((void));
void BB_WriteDone FP ((int len));

/*
 * Read side.
 */
int BB_Attach FP ((int key));
unsigned char *BB_GetBeam FP ((int *len));

void BB_Done FP ((void));
