//
// Some goodies for dealing with zebtimes.
//

inline
int operator< (const ZebTime &t1, const ZebTime &t2)
{
	return (t1.zt_Sec < t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec < t2.zt_MicroSec));
}


inline
int operator<= (const ZebTime &t1, const ZebTime &t2)
{
	return (t1.zt_Sec < t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec <= t2.zt_MicroSec));
}



inline
int operator> (const ZebTime &t1, const ZebTime &t2)
{
	return (t1.zt_Sec > t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec > t2.zt_MicroSec));
}


inline
int operator>= (const ZebTime &t1, const ZebTime &t2)
{
	return (t1.zt_Sec > t2.zt_Sec ||
		(t1.zt_Sec == t2.zt_Sec && t1.zt_MicroSec >= t2.zt_MicroSec));
}
