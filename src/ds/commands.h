/*
 * $Id: commands.h,v 2.11 2002-01-19 06:50:02 granger Exp $
 *
 * Command keywords for the data store daemon.
 */

/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

# define DK_PLATFORM	 1
# define DK_ENDPLATFORM  2
# define DK_ORGANIZATION 3
# define DK_FILETYPE	 4
# define DK_KEEP	 5
# define DK_REGULAR	 6
# define DK_MOBILE	 7
# define DK_DONE	 8
# define DK_COMPOSITE	 9
# define DK_DISCRETE	10
# define DK_DIRECTORY	11
# define DK_SUBPLATFORM	12
# define DK_MAXSAMPLES	13
# define DK_SPLITSECONDS 26
# define DK_TRUNCATE	14
# define DK_EVERY	15
# define DK_REMOTE	16
# define DK_BROADCAST	17
# define DK_RECEIVE	18
# define DK_DAYSPLIT	19
# define DK_CACHE	20
# define DK_RESCAN	21
# define DK_MODEL	22

# define DK_CLASS	25
# define DK_ENDCLASS	23
# define DK_ABSTRACT	24
# define DK_VIRTUAL	33
# define DK_INSTANCE	28
# define DK_SUBPLATS	29
# define DK_NOSUBPLATS	31
# define DK_FIELD       34
# define DK_DERIVATION  35
# define DK_COMMENT	27
# define DK_SOURCE	54

# define DK_REQUIRE     126

/*
 * The instancedir keywords
 */
# define DK_INSTANCEDIR 40
# define DK_COPYCLASS	41
# define DK_SUBDIRCLASS	42
# define DK_COPYPARENT	43
# define DK_SUBDIRPARENT 44
# define DK_DEFAULT	45
# define DK_ROOT	46

/*
 * Directory class inheritance keywords
 */
# define DK_INHERITDIR	50
# define DK_APPEND	51
# define DK_COPY	52
# define DK_NONE	53

/*
 * A hook to kick things off
 */
# define DK_START	90
