/* $Id: SerialZTime.hh,v 1.1 1998-03-05 04:54:19 granger Exp $
 *
 * Add serialization operators for the ZTime class.
 */

#ifndef _SerialZTime_hh_
#define _SerialZTime_hh_

#include "SerialStream.hh"
#include "ZTime.hh"


inline SerialStream & operator<< (SerialStream &ss, ZTime &zt)
{
	ss << zt.zt_Sec;
	ss << zt.zt_MicroSec;
	return (ss);
}


inline SerialStream & operator>> (SerialStream &ss, ZTime &zt)
{
	ss >> zt.zt_Sec;
	ss >> zt.zt_MicroSec;
	return (ss);
}


#endif /* _SerialZTime_hh_ */
