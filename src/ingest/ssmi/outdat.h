/* 
 * Define a C struct which maps onto the OUTDAT common block used by DECODE
 */

#ifndef _outdat_h_
#define _outdat_h_

extern char *outdat_;

typedef struct _OUTDAT {
	double rev, xtime;
	int itime, itimsc;
	float xlatsc, xlonsc, altsc, tht;
	float hltemp[3];
	int ivolt[2];
	float rftemp, frtemp;
	int iagc[6];
	int iasctm;
	float period, ascloc, anginc, axis, ecc, angper;
	int isat;
	float spacer[6];
	int icolda[7][5], ihota[7][5], icoldb[2][5], ihotb[2][5];
	float alat[128], alon[128], blat[128], blon[128];
	float talo[64][5], atahi[128][2], btahi[128][2];
	int iatoil[128], ibtoil[128];
} OUTDAT_BLOCK;

/*
 * Global pointer to the OUTDAT common block defined in decode_ssmi.c:
 *
 * OUTDAT_BLOCK *C_OUTDAT = (struct _OUTDAT *)(&outdat_);
 */

extern OUTDAT_BLOCK *C_OUTDAT;

void decode_ssmi FP((
  int i85ghz,	/* decode 85 Ghz channels */
  int itb,	/* provide antenna temps if 0, brightness if 1 */
  int iadj,	/* If 1, along-track correction for F08 prior to 1989 */
  int irec,	/* which logical record in physical rec, 1 to 16 */
  char *lrec	/* the logical record */
));


#endif /* _outdat_h_ */
