/* sprintrmt(buf,arg) does what sprintf(buf,"%r",arg) used to
 * 9/93 - calls to this routine were removed from ui, suds, robot which now 
 * use the variable argument C routines.  Any other programs still calling
 * this routine should follow suit
 */

void
sprintrmt(buf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
char   *buf, *fmt;
int	a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
{
    sprintf (buf, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
}
