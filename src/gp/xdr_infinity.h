/*		Copyright (C) 1993 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

/* 
 * "$Id: xdr_infinity.h,v 1.1 1993-05-26 20:11:09 corbet Exp $ 
 * XDR infinity stuff lifted from netcdf.h.
 */

/*
 * XDR_F_INFINITY is a float value whose EXTERNAL (xdr)
 * represention is ieee floating infinity.
 *
 * This section shows three techniques for setting these:
 *  Direct assignment (vax, cray) - works for non IEEE machines
 *		Doesn't work when IEEE machines don't allow
 *      float or double constants whose values are infinity.
 *  Use of a union (preferred portable method) - should work on
 *      any ANSI compiler with IEEE floating point representations,
 *      modulo byte order and sizeof() considerations.
 *  Use of pointer puns - may work with many older compilers
 *      which don't allow intialization of unions.
 *      Often doesn't work with compilers which have strict
 *      alignment rules.
 */ 

    /* Direct assignment. All cases should be mutually exclusive */

#ifdef vax
#	define	XDR_F_INFINITY	1.70141173e+38
#endif /* vax */

#ifdef cray
#	define	XDR_F_INFINITY	1.797693134862313000e+308
#endif /* cray */

#ifdef notdef /* you might want to try these, on an IEEE machine */
#	define XDR_F_INFINITY	3.40282357e+38
#endif

#ifdef __STDC__
    /* Use of a union, assumes IEEE representation and 1 byte unsigned char */
#  ifndef    XDR_F_INFINITY
#    define USE_F_UNION
     union xdr_f_union {unsigned char bb[4]; float ff;} ;
     extern union xdr_f_union xdr_f_infs ;  /* instantiated in array.c */
#    define  XDR_F_INFINITY    (xdr_f_infs.ff)
#  endif /* !XDR_F_INFINITY */


#else /* __STDC__ */
    /* Use of a pointer pun, assumes IEEE representation, 4 byte long */

#  ifndef    XDR_F_INFINITY
#    define USE_F_LONG_PUN
     extern long xdr_f_infinity ;  /* instantiated in array.c */
#    define XDR_F_INFINITY *((float *)&xdr_f_infinity)
#  endif /* !XDR_F_INFINITY */

#endif /* __STDC__ */
