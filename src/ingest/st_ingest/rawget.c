/* rawget -- Get a 1 sec record from P-3 slow tape data file created by
 * Dave Jorgensen for quick look purposes.  Returns a null pointer at
 * end-of-file.
 */

#include <stdio.h>
#include "rawget.h"

#define BFACT 10
#define BSIZE 3440

static int bsize,count = BFACT;
static struct p3slowblock dat;

struct p3slowblock *rawget()
{

/* should we expect an rtape header? */
  if (count >= BFACT) {
    count = 0;
    if (scanf("%d",&bsize) < 1) return((struct p3slowblock*)NULL);
    if (bsize != BSIZE) {
      fprintf(stderr,"rawget: got blocksize %d, expected %d\n",bsize,BSIZE);
      exit(1);
    }
  }

/* get the variables */
  scanf("%g",&dat.ac);
  scanf("%g",&dat.mo);
  scanf("%g",&dat.dy);
  scanf("%g",&dat.yr);
  scanf("%g",&dat.hr);
  scanf("%g",&dat.mn);
  scanf("%g",&dat.sc);
  scanf("%g",&dat.nav);
  scanf("%g",&dat.la1);
  scanf("%g",&dat.lo1);
  scanf("%g",&dat.la2);
  scanf("%g",&dat.lo2);
  scanf("%g",&dat.la4);
  scanf("%g",&dat.lo4);
  scanf("%g",&dat.gx1);
  scanf("%g",&dat.gy1);
  scanf("%g",&dat.gx2);
  scanf("%g",&dat.gy2);
  scanf("%g",&dat.gx4);
  scanf("%g",&dat.gy4);
  scanf("%g",&dat.hd);
  scanf("%g",&dat.ra1);
  scanf("%g",&dat.ra2);
  scanf("%g",&dat.ra3);
  scanf("%g",&dat.pc);
  scanf("%g",&dat.rl);
  scanf("%g",&dat.ps);
  scanf("%g",&dat.ta);
  scanf("%g",&dat.td);
  scanf("%g",&dat.gs);
  scanf("%g",&dat.tk);
  scanf("%g",&dat.ws);
  scanf("%g",&dat.wd);
  scanf("%g",&dat.rd);
  scanf("%g",&dat.rs);
  scanf("%g",&dat.ru);
  scanf("%g",&dat.uw);
  scanf("%g",&dat.ui);
  scanf("%g",&dat.lw);
  scanf("%g",&dat.tas);

/* increment buffer count */
  count++;

/* return pointer to the data structure */
  return(&dat);
}
