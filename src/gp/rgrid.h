C		Copyright (C) 1987,88,89,90,91 by UCAR
C	University Corporation for Atmospheric Research
C		   All rights reserved
C
C No part of this work covered by the copyrights herein may be reproduced
C or used in any form or by any means -- graphic, electronic, or mechanical,
C including photocopying, recording, taping, or information storage and
C retrieval systems -- without permission of the copyright owner.
C 
C This software and any accompanying written materials are provided "as is"
C without warranty of any kind.  UCAR expressly disclaims all warranties of
C any kind, either express or implied, including but not limited to the
C implied warranties of merchantibility and fitness for a particular purpose.
C UCAR does not indemnify any infringement of copyright, patent, or trademark
C through use or modification of this software.  UCAR does not provide 
C maintenance or updates for its software.
C
       PARAMETER (MAXDIM=25, MAXSTN=200, LUIN=5, LUOUT=6)
C MAXDIM = LARGEST DIMENSION ALLOWED FOR THE GRID USED FOR MESONET DATA.
C MAXSTN = MAXIMUM NO. OF PAM STATIONS.
C LUIN = LOGICAL UNIT NO. FOR INPUT.
C LUOUT = LOGICAL UNIT NO. FOR OUTPUT.
	CHARACTER*10	NAME
	CHARACTER*7	FSTRNG,OVFLD
	LOGICAL		DELETED
	COMMON /TRIAGD/	NP, ITRS, IUSED(MAXSTN), ITRNGL(MAXSTN,6),
     *			NAPROX, IBPTNO, IBPTS, IBNDRY(MAXSTN), NSTNS,
     *			NX, NY, IPAR, IALT(MAXSTN), NFLTR, 
     *			DELETED(MAXSTN), NUSRNO(MAXSTN), IDATE, ITIME,
     *			NTPLOT, NSTEP, IPAROV, JW(MAXDIM,MAXDIM),
     *			X(MAXSTN), Y(MAXSTN), FLAG, XMAX, XMIN, YMAX,
     *			YMIN, X0, Y0, DX, DY, OVRPAM(MAXSTN), 
     *			AREA(MAXSTN), U(MAXSTN), V(MAXSTN), Z(MAXSTN),
     *			RLAT(MAXSTN), RLON(MAXSTN), WEIGHT(MAXSTN,6),
     *			RUPLIM(200), RLOLIM(200)
	COMMON /TRGCHR/	FSTRNG, OVFLD, NAME
