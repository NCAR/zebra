/*
 * draw_text definitions
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
# define JustifyCenter		0
# define JustifyBottom		1
# define JustifyTop		2
# define JustifyBaseline	3
# define JustifyLeft		4
# define JustifyRight		5

# ifdef __STDC__
	void	DrawText (Widget, Drawable, GC, int, int, char *, double, 
			double, int, int);
	void	DT_StrokeText (Widget, Drawable, GC, int, int, char *, 
			double, double, int, int);
	void	DT_TextBox (Widget, Drawable, int, int, char *, double, 
			double, int, int, int *, int *, int *, int *);
# else
	void	DrawText (), DT_StrokeText (), DT_TextBox ();
# endif
