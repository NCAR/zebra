// Some -*- C++ -*- goodies for dealing with ZebraTimes.
//
// $Id: ZTime.hh,v 1.3 1998-10-20 20:44:46 granger Exp $
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

#ifndef _ZTime_h_
#define _ZTime_h_

#include <iostream.h>

#include <defs.h>
#undef bool
#include <zebra.h>

/*
 * Subclass the ZebraTime C struct and call it ZTime!
 * Then we can add some C++ methods to it.
 */

struct ZTime : public ZebraTime
{
	//static const ZebraTime ALHPA = { 0, 0 };
	//static const ZebraTime OMEGA = { 0x7fffffff, 999999 };
	//static const ZebraTime NONE = { -1, -1 };

	/// Return as system time
	inline long toSystem () const
	{
		return (TC_ZtToSys (this));
	}

	inline operator long () const
	{
		return (toSystem());
	}
	
	inline void setSystem (const long sys)
	{
		TC_SysToZt (sys, this);
	}

	inline void operator = (const long sys)
	{
		setSystem (sys);
	}

	inline void encode (char *dest, TimePrintFormat format = TC_Full) const
	{
		TC_EncodeTime (this, format, dest);
	}

	inline const char *ascTime (TimePrintFormat format = TC_Full) const
	{
		return (TC_AscTime (this, format));
	}

	inline int decode (const char *s)
	{
		return (TC_DecodeTime (s, this));
	}

	inline void split (int *year, 
			   int *month = 0, 
			   int *day = 0, 
			   int *hour = 0,
			   int *minute = 0,
			   int *second = 0,
			   int *microsec = 0) const
	{
		TC_ZtSplit (this, year, month, day, hour, 
			    minute, second, microsec);
	}

	inline int year() const
	{
		int y;
		split (&y);
		return (y);
	}

	inline int month() const
	{
		int m;
		split (0, &m);
		return (m);
	}

	inline int day() const
	{
		int d;
		split (0, 0, &d);
		return (d);
	}

	inline int hour() const
	{
		int h;
		split (0, 0, 0, &h);
		return (h);
	}

	inline int minute() const
	{
		int m;
		split (0, 0, 0, 0, &m);
		return (m);
	}

	inline int second() const
	{
		int s;
		split (0, 0, 0, 0, 0, &s);
		return (s);
	}

	inline void assemble (int year = 0, 
			      int month = 0, 
			      int day = 0, 
			      int hour = 0, 
			      int minute = 0, 
			      int second = 0,
			      int microsec = 0)
	{
		TC_ZtAssemble (this, year, month, day, hour, 
			       minute, second, microsec);
	}

	inline ZTime operator+= (int seconds)
	{
		zt_Sec += seconds;
		return *this;
	}

	inline ZTime operator+ (int seconds)
	{
		return ZTime(zt_Sec + seconds, zt_MicroSec);
	}

	inline ZTime operator+ (const ZebraTime &add)
	{
		unsigned long msecs = zt_MicroSec + add.zt_MicroSec;
		return ZTime(zt_Sec + add.zt_Sec + (msecs / 1000000),
			     msecs % 1000000);
	}

	inline ZTime &operator= (const ZebraTime &src)
	{
		zt_Sec = src.zt_Sec;
		zt_MicroSec = src.zt_MicroSec;
		return *this;
	}

	inline ZTime &operator= (const ZTime &src)
	{
		zt_Sec = src.zt_Sec;
		zt_MicroSec = src.zt_MicroSec;
		return *this;
	}

	// Constructors

	ZTime ()
	{
		zt_Sec = 0;
		zt_MicroSec = 0;
	}

	ZTime (const ZebraTime &zt)
	{
		zt_Sec = zt.zt_Sec;
		zt_MicroSec = zt.zt_MicroSec;
	}

	ZTime (const long sys)
	{
		TC_SysToZt (sys, this);
	}

private:
	ZTime (long sec, long micro)
	{
		zt_Sec = sec;
		zt_MicroSec = micro;
	}
};


inline
ostream & operator<< (ostream &out, const ZebraTime &t)
{
	out << TC_AscTime (&t, TC_Full);
	return (out);
}


#ifdef notdef
/* This shouldn't be necessary because of the above definition.
 * In that case a ZTime should be cast to a ZebraTime. */
inline
ostream & operator<< (ostream &out, const ZTime &t)
{
	out << t.ascTime();
	return (out);
}
#endif


inline
bool operator< (const ZebraTime &t1, const ZebraTime &t2)
{
	return (t1.zt_Sec < t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec < t2.zt_MicroSec));
}


inline
bool operator<= (const ZebraTime &t1, const ZebraTime &t2)
{
	return (t1.zt_Sec < t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec <= t2.zt_MicroSec));
}


inline
bool operator> (const ZebraTime &t1, const ZebraTime &t2)
{
	return (t1.zt_Sec > t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec > t2.zt_MicroSec));
}



inline
bool operator>= (const ZebraTime &t1, const ZebraTime &t2)
{
	return (t1.zt_Sec > t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec >= t2.zt_MicroSec));
}



inline
bool operator== (const ZebraTime &t1, const ZebraTime &t2)
{
	return ((t1.zt_Sec == t2.zt_Sec) && 
		(t1.zt_MicroSec == t2.zt_MicroSec));
}


#endif /* ndef _ZTime_h_ */
