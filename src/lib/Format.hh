/*
 * $Id: Format.hh,v 1.3 1998-05-15 19:36:52 granger Exp $
 * 
 * An interesting, if possibly useful, interface for formatting strings
 * using printf-style format specifiers.
 */

/* Examples:

   Format format;
   Format int3("First: %5d Second: %5d Third: %5d");
	
   cout << format("%5d %5.2f") % 1234 % 56.7777 << endl;
   cout << format(int3) % 1 % 2 % 3 << endl;
		
   cout << format.reset() % 4 % 5 % 6 << endl;

   cout << format("%10s %30s") % "Hello" % "World" << endl;

   cout << Printf("%3d %4d %5d %6d %7d", 3, 4, 5, 6, 7) << endl;

   cout << Printf(128, "%020.5f", 3.1415927) << endl;

   cout << format("%3d %4d %5d %6d %7d") % 3 << endl;
   cout << format("%3d %4d %5d %6d %7d").quiet() % 3 << endl;
   cout << format.reset().quiet() % 3 % 4 % 5 % 6 % 7 % 8 << endl;

   cout << format.reset().quiet() % 3.0 % 4.0 % 5.0 % "6.0" << endl;
   cout << format("%f %lf %s").quiet() % 1 % 2 % 3.0 << endl;

 */

#ifndef _Format_hh_
#define _Format_hh_

#include <string>
#include <stdio.h>	// vsprintf() and sprintf()
#include <stdlib.h>
#include <stdarg.h>
//#include <std.h>
#include <iostream.h>

class Format
{
public:
	enum Error { OK = 0, BAD_FORMAT, MISSING_FORMAT, TYPE_MISMATCH };

	// Constructors

	Format (const string &s) : fmt(s)
	{ 
		reset();
	}

	Format (const char *s) : fmt(s)
	{ 
		reset();
	}

	Format ()
	{
		reset();
	}

	// Copy constructor
	Format (const Format &f) : 
		fmt(f.fmt), pos(f.pos), buf(f.buf)
	{
		// cout << "Format copy constructor called: (" 
		// << fmt << ", " << pos << ")" << endl;
	}
		
	// function() operators

	Format &operator() (const char *s)
	{
		fmt = s;
		return (reset());
	}

	Format &operator() (const string &s)
	{
		fmt = s;
		return (reset());
	}

	Format &operator() (const Format &f)
	{
		fmt = f.fmt;
		return (reset());
	}

	// assignment operator

	Format &operator= (const Format &f)
	{
		fmt = f.fmt;
		return (reset());
	}

	/// Reset to beginning of format and clear errors
	Format &reset()
	{
		pos = 0;
		buf = "";
		err = OK;
		verbose = true;
		return *this;
	}

	/// Suppress error strings
	Format &quiet()
	{
		verbose = false;
		return *this;
	}

	/// Report errors
	Format &loud()
	{
		verbose = true;
		return *this;
	}

	/// Return error status
	Error status()
	{
		return err;
	}

	friend ostream & operator<< (ostream &out, const Format &f);

	/* Add a method for checking the type against the format flag
	   then printing the parameter. */
#	define PERCENT(T,F) \
	Format & operator% (const T t) \
	{ \
		char temp[1024]; \
		char flag = 0; \
		string f; \
		do { \
			f = next_format(&flag); \
			if (flag == '%') \
			{ \
				sprintf (temp, f.c_str()); \
				buf += temp; \
			} \
		} \
		while (flag == '%'); \
		if (! strchr (F,flag)) \
			f = error(TYPE_MISMATCH); \
		sprintf (temp, f.c_str(), t); \
		buf += temp; \
		return *this; \
	}

	PERCENT(int,"idoxX");
	PERCENT(unsigned int,"u");
	PERCENT(double,"feEgG");
	PERCENT(char,"c");
	PERCENT(unsigned char,"c");
	PERCENT(char *,"s");
	PERCENT(void *,"p");

#	undef PERCENT

	operator const char * ()
	{
		return buf.c_str();
	}

	/// Return error string for a given error
	const string &errstr (Error e)
	{
		static const string blank("");
		static const string bad("*bad format*");
		static const string missing("*missing format*");
		static const string mismatch("*type mismatch*");
		static const string unknown("*unknown error*");

		switch (e)
		{
		case OK:
			return blank;
		case MISSING_FORMAT:
			return missing;
		case BAD_FORMAT:
			return bad;
		case TYPE_MISMATCH:
			return mismatch;
		}
		/* should not be reached */
		return unknown;
	}

protected:

	string next_format (char *flag = 0)
	{
		static const char *Specifiers = "iduoxXfeEgGcsp%";

		// Find the next % format operator in the lhs
		// and extract the whole format specifier

		string::size_type percent = fmt.find ('%', pos);
		if (percent == string::npos)
			// throw BadFormat(pos);
			return error(MISSING_FORMAT);
		string::size_type spec = 
			fmt.find_first_of (Specifiers, percent+1);
		if (spec == string::npos)
			// throw BadFormat(percent);
			return error(BAD_FORMAT);
		// Save the type flag if requested
		if (flag)
			*flag = fmt[spec];
		// Copy the text between last format and this format into buf
		buf.append (fmt, pos, percent-pos);
		pos = spec+1;
		return fmt.substr(percent, pos-percent);
	}

	const string &error (Error e)
	{
		err = e;
		if (! verbose)
			e = OK;
		return (errstr(e));
	}

	string fmt;
	string::size_type pos;
	string buf;

	Error err;
	bool verbose;
};



/* Derive a class from Format which allows a format and immediate
 * parameters in the constructor.  Temporary objects of this class can be
 * assigned to const references and written to streams.  The template
 * parameter can vary depending upon the expected size of the formatted
 * string.  We can't just add this constructor to Format since we want
 * to keep the (const char *) constructor, and these would conflict. */
// template <int N = 1024>
class Printf : public Format
{
public:
	/// Set the format and immediately apply the argument list
	Printf (const char *s ...) : Format(s)
	{
		char sbuf[1024];
		va_list vl;
		va_start (vl, s);
		vsprintf (sbuf, s, vl);
		va_end (vl);
		buf = sbuf;
	}
	/// Apply an argument list using a buffer of size 'n'
	Printf (unsigned int n, const char *s ...) : Format(s)
	{
		char sbuf[n];
		va_list vl;
		va_start (vl, s);
		vsprintf (sbuf, s, vl);
		va_end (vl);
		buf = sbuf;
	}

#ifdef notdef
	operator const char * ()
	{
		return buf.c_str();
	}
#endif
};



inline ostream & operator<< (ostream &out, const Format &f)
{
	out << f.buf;
	return out;
}

#endif /* _Format_hh_ */

