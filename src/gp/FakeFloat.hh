//
// A class encapsulating the horrible "fake float" integer kludge.
//
// As a further kludge, this thing expects a global "Endian" instance
// called FF_End around...
//
# ifndef _FakeFloat_hh_
# define _FakeFloat_hh_

# include <byteorder.h>

class FakeFloat
{
private:
	int iv;
	volatile short *sv;
public:
	FakeFloat (float fv)
	{
		iv = (int) (fv*65536);
		sv = (BigEndian () ? 0 : 1) + (short *) &iv;
	};

	FakeFloat (const FakeFloat &other)
	{
		iv = other.iv;
		sv = (BigEndian () ? 0 : 1) + (short *) &iv;
	};

	FakeFloat ()
	{
		iv = 0;
		sv = (BigEndian () ? 0 : 1) + (short *) &iv;
	};

	int ival () { return (*sv); };
	float fval () { return (iv / 65536.0); }
	void operator= (const FakeFloat &other) { iv = other.iv; };
	void operator+= (const FakeFloat &other) { iv += other.iv; };
};






# endif
