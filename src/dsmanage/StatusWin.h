//
// Status window for loading data.
//
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

class StatusWindow : public dsPopupWindow
{
	Widget sw_textline;
	Widget sw_scroll;
	int sw_nf, sw_bytes;
	int sw_abort;
public:
	StatusWindow (const char *, int nf, int bytes);
	~StatusWindow ();
	void popdown () { delete this; };
	int status (int nf, int bytes);
	void setAbort (int value) { sw_abort = value; }
};
