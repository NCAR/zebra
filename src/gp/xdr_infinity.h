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
 * "$Id: xdr_infinity.h,v 1.2 1993-07-27 20:15:54 burghart Exp $ 
 * One somewhat reasonable definition for infinity in XDR, lifted from 
 * NetCDF code.  This may not be right on some machines, but it's here
 * as a kluge anyway, and it won't stop things from compiling anywhere.
 */

long xdr_f_infinity = 0x7f800000;
# define XDR_F_INFINITY (*((float *)&xdr_f_infinity))
