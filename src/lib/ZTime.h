// -*- mode: c++; c-basic-offset: 8 -*- goodies for dealing with ZebraTimes.
//
// $Id: ZTime.h,v 2.5 2001-01-06 00:01:39 granger Exp $
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

#include "defs.h"

//
// Subclass the ZebraTime C struct and call it ZTime!
// Then we can add some C++ methods to it.
//
// Here is the basic idea for using this class as it is currently designed.
// There are no longer any cast operators since these can cause confusion.
// For example, assigning a double to a ZTime would silently first truncate
// the double to a long and lose the microseconds.  Defining both a double
// and long cast operator would cause ambiguities.  Instead, math operators
// are only provided between ZTimes, and constructors are provided for
// converting doubles, longs, and ZebraTimes into ZTimes.  For construction
// a scalar (long or double) is interpreted as seconds since the epoch.
// For math, a scalar can be just an interval in seconds.  This
// construction and possibly many copies will happen implicitly when
// writing mixed-mode expressions with ZTimes, such as zt + 60.  The
// comparison operators also work with ZTime, which allows comparisons to
// scalars by way of the same implicit constructions.  This may cause some
// unintended overhead when comparing ZebraTimes. However, this also means
// that ZebraTimes will be normalized before being compared (in the
// implicit constructor).  Without the normalization, it would be possible
// for two non-normalized but otherwise equal ZebraTimes to compare
// incorrectly.  The ZTime class tries to make sure that it is always
// normalized.  
//
// This class should be the same size as ZebraTime structs (see
// ztime_test), so arrays of ZTimes can be passed as arrays of ZebraTimes.
// This means this class should never be virtual in any way.

struct ZTime : public ZebraTime
{
	// ================
	// Class methods to access generic constants
	//
	static inline const ZebraTime& ALHPA()
	{
		return ZT_ALPHA;
	}
	static inline const ZebraTime& OMEGA()
	{
		return ZT_OMEGA;
	}
	static inline const ZebraTime& NONE()
	{
		return ZT_NONE;
	}

	// ================
	// Instance methods

	inline long toSystem () const
	{
		return zt_Sec;
	}

	inline ZTime &setSystem (const long sys)
	{
		zt_Sec = sys;
		zt_MicroSec = 0;
		return *this;
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
		normalize();
	}

	// Return a double scalar in seconds since the epoch.
	//
	double toDouble () const
	{
		return (zt_Sec + ((double)zt_MicroSec/1.0e6));
	}

	inline ZTime &operator+= (const ZTime &rhs)
	{
		zt_Sec += rhs.zt_Sec;
		zt_MicroSec += rhs.zt_MicroSec;
		normalize();
		return *this;
	}

	inline ZTime &operator-= (const ZTime &rhs)
	{
		zt_Sec -= rhs.zt_Sec;
		zt_MicroSec -= rhs.zt_MicroSec;
		normalize();
		return *this;
	}

	inline ZTime &operator= (const ZTime &src)
	{
		zt_Sec = src.zt_Sec;
		zt_MicroSec = src.zt_MicroSec;
		return *this;
	}

	inline ZTime &operator= (const ZebraTime &src)
	{
		zt_Sec = src.zt_Sec;
		zt_MicroSec = src.zt_MicroSec;
		normalize();
		return *this;
	}

	void normalize()
	{
		if (zt_MicroSec < 0) 
		{
			zt_Sec -= 1 + (-zt_MicroSec / 1000000);
			zt_MicroSec = 1000000 - (-zt_MicroSec % 1000000);
		}
		else
		{
			zt_Sec += zt_MicroSec / 1000000;
			zt_MicroSec = zt_MicroSec % 1000000;
		}
	}

	// ================
	// Constructors
	//
	// Warning: Because we inherit from a C structre, ZebraTime,
	// it does not have a constructor which initializes its members,
	// so these constructors have to make sure they do that.

	inline ZTime ()
	{
		setSystem(0);
	}

	inline ZTime (const ZebraTime &zt)
	{
		this->operator= (zt);
	}

	inline ZTime (const ZTime &zt)
	{
		this->operator= (zt);
	}

	// Construct a ZTime from [fractional] seconds since the epoch.
	//
	ZTime (const double seconds)
	{
		int sign = (seconds < 0 ? -1 : 1 );
		double asecs = (seconds < 0 ? -seconds : seconds );
		long lsecs = (long)(asecs);
		zt_Sec = sign*lsecs;
		zt_MicroSec = sign*((long)((asecs - lsecs)*1e6));
	}

	// Integer initialize requires both fields to disambiguate it
	// from the double constructor.
	//
	inline ZTime (long sec, long micro)
	{
		zt_Sec = sec;
		zt_MicroSec = micro;
		normalize();
	}

};


inline ZTime operator+ (const ZTime &lhs, const ZTime &rhs)
{
	return ZTime(lhs) += rhs;
}


inline ZTime operator- (const ZTime &lhs, const ZTime &rhs)
{
	return ZTime(lhs) -= rhs;
}


class ostream;
ostream & operator<< (ostream &out, const ZebraTime &t);

// ================================================================
// NOTE: The comparison operators assume the zebra time is normalized.
// If not, the comparison can be incorrect.
// ================================================================

inline
bool operator< (const ZTime &t1, const ZTime &t2)
{
	return (t1.zt_Sec < t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec < t2.zt_MicroSec));
}


inline
bool operator<= (const ZTime &t1, const ZTime &t2)
{
	return (t1.zt_Sec < t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec <= t2.zt_MicroSec));
}


inline
bool operator> (const ZTime &t1, const ZTime &t2)
{
	return (t1.zt_Sec > t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec > t2.zt_MicroSec));
}



inline
bool operator>= (const ZTime &t1, const ZTime &t2)
{
	return (t1.zt_Sec > t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec >= t2.zt_MicroSec));
}



inline
bool operator== (const ZTime &t1, const ZTime &t2)
{
	return ((t1.zt_Sec == t2.zt_Sec) && 
		(t1.zt_MicroSec == t2.zt_MicroSec));
}

inline
bool operator!= (const ZTime &t1, const ZTime &t2)
{
	return (! (t1 == t2));
}

#endif /* ndef _ZTime_h_ */
