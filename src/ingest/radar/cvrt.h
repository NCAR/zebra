/*=======================================================================
 *
 * cvrt -- conversion functions for SUNrise
 * 
 *=======================================================================*/


/*
 * These are conversion functions (#define macros) for most of the necessary
 * data conversions and limits that are used within the SUNrise Weather
 * Radar System.  Refer to the man page cvrt(3) for a description of each
 * of these functions.
 */

/*
 * 
 * This is the release that was sent to England on April 16, 1991 for the
 * demonstration.
 * 
 * 
 * 
 * Revision 1.1  91/04/22  10:59:46  root
 * Initial revision
 * 
 * Revision 1.3  91/01/25  20:00:50  jkofler
 * Input Ian's change again.
 * 
 * Revision 1.2  91/01/08  14:48:31  root
 * *** empty log message ***
 * 
 * Revision 1.1  90/07/09  17:22:07  jlong
 * Initial revision
 * 
 * Revision 1.2  89/11/17  10:51:45  ian
 * added a circular_dec
 * 
 * Revision 1.1  89/11/09  19:55:21  bsmith
 * Initial revision
 * 
 */

/*
 * This program is copyright 1989 by Lassen Research, Manton CA 96059,
 * (916) 474-3966.  It is licensed for use on a specific cpu and is not
 * to be transferred or otherwise divulged.  Copies or modifications of
 * this program must carry this copyright notice.
 */

#ifndef _CVRT_
#define _CVRT_

#ifndef _RUNNING_ON_SUN_
#include "math.h"
#else
#include <math.h>
#endif
double floor();

/*#define ANGCONV ((float)0x03fff / 360.0000)*/


#define ANGCONV 45.50833
#define BINCONV 0.021974
#define angle2bin(A) ((unsigned short)(((A) * ANGCONV)+0.5))
#define bin2angle(B) ((unsigned short)(B) * BINCONV)


/*
** the next two macros don't work right
*/
#define bin2iangle(B) ((unsigned short) ((B*360) >> 14))
/*#define bin2iangle(B) ((unsigned short) (((unsigned int)B * 360) >> 14))*/

#define ibin2angle(B) ((int) (((float)B * 360.00)/16383.0))
#define rvp2db(I) ((float)(short)I / 16.0)
#define rvp2hz(I) (int)(0.5+(1000000.0*(1.00 / ((I?(float)I:0.0001)/6.0))))
/* #define angle2bin(A) ((unsigned short) (((unsigned int)(B) * 360) / 2^14))*/
#define bin2iangle_floor(B) ((unsigned short)(floor(B/ANGCONV)))
#define bin2iel25(B) ((unsigned short) (((unsigned int)(0x0b+B) * 360) >> 12))
#define limit_az(A) ((A>359)?359:(A<0)?0:A)
#define limit_el(E) ((E>90)?90:(E<0)?0:A)
#define circular_distance(A1,A2) ((A1<=A2)?(int)(A2-A1):(int)(A2+(360-A1)))
#define ccw_distance(A1,A2) ((A1>=A2)?(int)(A1-A2):(int)(A1+(360-A2)))
#define cw_distance(A1,A2) circular_distance(A1,A2)
#ifndef circular_inc
#define circular_inc(A) A = (((A+1) > 359) ? 0 : A+1)
#define circular_dec(A) A = (((A-1) < 0) ? 359 : A-1)
#endif
#ifndef MIN
#  define MIN(A,B) ((A<B) ? A : B)
#endif
#define DBZ2RVP(F) dbz2rvp((F))
#define dbz2rvp(F) (unsigned char)(2*F+64)
#define RVP2DBZI(I) rvp2dbz((I))
#define rvp2dbzi(I) (unsigned short)((I-64)/2)
#define RVP2DBZ(I) rvp2dbz((I))
#define rvp2dbz(I) ((((float)I)-64.0)/2.0)

/*
 *	We need PW_MAX so we can make a reasonable guess at array limits
 */
#define	PW_MAX	4
static float PWs[ PW_MAX ] = {0.0, 0.5, 1.5, 2.0};
#define index2pw(A) (PWs[A&3])
#define rlimit(R) (R & 0x0ff);
#define tlimit(T) limit_az(T)

/*
 * These are additions which are necessary for Scantool
 */

#ifdef _BSMITH_

/* LAMBDA is used in the calculation of unambiguous velocity		*/
#define LAMBDA		(0.054)

/* The number of bins per ray currently assumed by Scantool		*/
#define	NUM_BINS	(256)

/* PRF Calculations:
 *
 *	PRF_HIGH(RNG,PWDTH) returns the high prf value and takes
 *	index values into the range table and pulse_width tables.
 *
 *	PRF_LOW(PHIGH,UNFOLD) returns the low prf value and takes
 *	the high prf and an index into the unfolding table as input.
 */
#define PRF_HIGH(RNG,PWDTH) ((1.0/((float)(range_table[(RNG)]*(.00000667))))>max_prf[(PWDTH)][(RNG)]?max_prf[(PWDTH)][(RNG)]:(1.0/((float)(range_table[(RNG)]*(.00000667)))))
#define PRF_LOW(PHIGH,UNFOLD) (1.0/((float)(unfold_table[(UNFOLD)]*(1.0/PHIGH))))

/*
 * UAV(PLOW,UNFOLD) returns the unambiguous velocity given a low prf and an
 * index into the unfolding adjustment table
 */
#define UAV(PLOW,UNFOLD) (unfold_adjust[(UNFOLD)]+(LAMBDA/(4.0*(1/(PLOW)))))

/*
 * ASPEED(M,N,P) returns the antenna speed. 
 */
#define ASPEED(M,N,P) ((1.0/(((1.0/(P))*(N))+(ant_k[(int)(M)]*NUM_BINS)))<24?(1.0/(((1.0/(P))*(N))+(ant_k[(int)(M)]*NUM_BINS))):24)
#endif _BSMITH_
#define sgn(A) (A<0 ? -1 : 1)
#define abs(A) (A*sgn(A))
#define degrees(A) ((A*180.0)/3.1416)
#define radians(A) ((A * 3.1416)/180.0)
#define persec2rpm(P) (P/6.0)
#define bin2BCD(B) (((B/(int)10) << 4) + (B%(int)10))
#define z2dbz(Z) (((((*(int *)&Z ) - 0x3f800000) >>16) * 3083) >> 16)
#define pz2dbz(PZ) (((((*(int *)PZ ) - 0x3f800000) >>16) * 3083) >> 16)
#define iz2dbz(IZ) (64+((((IZ - 0x3f800000) >>16) * 3083) >> 16))

#endif _CVRT_
