/*
 * This module contains the C entries to the nvxini routines,
 * according to the navigation type passed in.  It depends on the
 * unique names given the nvxini, nvxopt, nvxeas, and nvxsae fortran 
 * routines in the zebra mcidas library.
 */

#include <string.h>
#include <ctype.h>

/*
 * These are to work around some unresolved symbols in puc.c and luc.c.
 * Presumably they have never been needed.
 */
long	*uc = NULL, *neguc = NULL;

typedef struct _NavMethods
{
	char *navtype;
	int (*nvxini)();
	int (*nvxopt)();
	int (*nvxeas)();
	int (*nvxsae)();
}
NavMethods;

#ifdef UNDERSCORE
#define fname(base) base ## _
#else
#define fname(base) base
#endif

#define NAV_PROTO(methods,name,base) \
extern int \
fname(base ## ini) (int *ifunc, int *iarr); \
extern int \
fname(base ## opt) (int *ifunc, float *xin, float *xout); \
extern int \
fname(base ## eas) (float *,float *,float *,float *,float *,float *);\
extern int \
fname(base ## sae) (float *,float *,float *,float *,float *,float *);\
\
static NavMethods methods = \
{ \
	  name, \
	  & fname(base ## ini), \
	  & fname(base ## opt), \
	  & fname(base ## eas), \
	  & fname(base ## sae) \
};

NAV_PROTO(AircMethods,"AIRC",nai)
NAV_PROTO(DmspMethods,"DMSP",ndm)
NAV_PROTO(GmsxMethods,"GMSX",ngx)
NAV_PROTO(GoesMethods,"GOES",ngs)
NAV_PROTO(GrafMethods,"GRAF",ngr)
NAV_PROTO(GvarMethods,"GVAR",ngv)
NAV_PROTO(LaloMethods,"LALO",nlo)
NAV_PROTO(LambMethods,"LAMB",nla)
NAV_PROTO(MercMethods,"MERC",nme)
NAV_PROTO(MollMethods,"MOLL",nmo)
NAV_PROTO(MsatMethods,"MSAT",nms)
NAV_PROTO(MsgMethods,"MSG",nmg)
NAV_PROTO(PSMethods,"PS",nps)
NAV_PROTO(RadrMethods,"RADR",nra)
NAV_PROTO(RectMethods,"RECT",nrc)
NAV_PROTO(SinMethods,"SIN",nsi)
NAV_PROTO(TancMethods,"TANC",nta)
/* NAV_PROTO(TiroMethods,"TIRO",nti) (currently generates compiler errors) */

static NavMethods *Navs[] =
{
    &AircMethods, &DmspMethods, &GmsxMethods, &GoesMethods, &GrafMethods, 
    &GvarMethods, &LaloMethods, &LambMethods, &MercMethods, &MollMethods, 
    &MsatMethods, &MsgMethods, &PSMethods, &RadrMethods,
    &RectMethods, &SinMethods, &TancMethods, /* &TiroMethods */
    
};
static int NumNavs = sizeof (Navs) / sizeof (Navs[0]);

/*
 * Keep track of the most recently initialized navigation type
 */
static NavMethods *Current = NULL;



int 
nvxini (int *navcod)
{
	int i, one = 1, two = 2;
	char nav[5];
	int status = -1;

	strncpy (nav, (char *)navcod, 4);
	nav[4] = 0;
	/* Eliminate white space from the end */
	i = 3;
	while (isspace (nav[i]))
		nav[i--] = '\0';

	for (i = 0; i < NumNavs; ++i)
	{
		if (strcmp (nav, Navs[i]->navtype) == 0)
		{
			Current = Navs[i];
		/*
		 * Initialize navigation
		 */
			status = (*Current->nvxini)(&one, navcod);
			if (status < 0)
			    break;
		/*
		 * Force the nav routines to return lat/lon
		 */
			status = (*Current->nvxini)(&two, (int*)("LL  "));
			break;
		}
	}
	return (status);
}



int
nvxopt (int ifunc, float *xin, float *xout)
{
	if (Current)
		return ((*Current->nvxopt)(&ifunc, xin, xout));
	return (1);
}



int
nvxsae (float xlin, float xele, float xdum, 
	float *xpar, float *ypar, float *zpar)
{
	float dummy = 0;

	if (! zpar)
		zpar = &dummy;
	if (Current)
		return ((*Current->nvxsae)(&xlin, &xele, &xdum,
					   xpar, ypar, zpar));
	return (-1);
}



int
nvxeas (float xpar, float ypar, float zpar, 
	float *xlin, float *xele, float *xdum)
{
	float dummy = 0;

	if (! xdum)
		xdum = &dummy;
	if (Current)
	{
		return ((*Current->nvxeas)(&xpar, &ypar, &zpar,
					   xlin, xele, xdum));
	}
	return (-1);
}


/*
 * squak.f calls ABORT(), but IRIX doesn't have it, and we don't really
 * need it, so we provide our own definition here.
 */

void
fname (abort) ()
{
	exit (1);
}

