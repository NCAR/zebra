/*
 * Image transfer global stuff.
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

# ifdef __STDC__
	struct _ix_desc * IX_HookIn (int, char *, int *, int *, int *,char **);
	struct _ix_desc * IX_Create (int, int, int, int, int, char **);
	int	IX_GetWriteFrame (struct _ix_desc *, char **, int);
	void	IX_SendFrame (struct _ix_desc *, int, UItime *, RGrid *,
			Location *, ScaleInfo *, int, int, int, int, char *);
	int	IX_GetReadFrame (struct _ix_desc *, int, char **, UItime *,
			RGrid *, Location *, ScaleInfo *, int *, int *,
			int *, int *, char **);
	void	IX_ReleaseFrame (struct _ix_desc *, int);
	void	IX_Detach (struct _ix_desc *);
	void	IX_LockMemory (struct _ix_desc *);
	char	*IX_GetConsumer (struct _ix_desc *);
# else
	struct _ix_desc * IX_HookIn ();
	struct _ix_desc * IX_Create ();
	int	IX_GetWriteFrame ();
	void	IX_SendFrame ();
	int	IX_GetReadFrame ();
	void	IX_ReleaseFrame ();
	void	IX_Detach ();
	void	IX_LockMemory ();
	char	*IX_GetConsumer ();
# endif

