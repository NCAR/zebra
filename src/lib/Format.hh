/*
 * $Id: Format.hh,v 1.8 2002-09-17 20:00:19 granger Exp $
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

   We should allow just a simple % as a placeholder to which we stream the
   next operand:

	"The number % comes before %" % a % (a+1)

   We do not have to know or care about the type of our operand if we're
   willing to accept the default formatting of the ostream (<<) operator.

 */

#ifndef _Format_hh_
#define _Format_hh_

#include <string>
#include <stdio.h>	// vsprintf() and sprintf()
#include <stdlib.h>
#include <stdarg.h>
//#include <std.h>
#include <iostream>

class Format
{
public:
	typedef std::string string;

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
		buf = "";
		pos = 0;
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

	//friend ostream & operator<< (ostream &out, const Format &f);

	/* Add a method for checking the type against the format flag
	   then printing the parameter. */
	// Automatic reset after an evaluation or when format consumed
#	define PERCENT(T,F) \
	Format & operator% (const T t) \
	{ \
		char temp[1024]; \
		char flag = 0; \
		string f = parse_format (temp, &flag); \
		if (! strchr (F,flag)) \
			f = error(TYPE_MISMATCH); \
		if (strchr (F,'s') && t == 0) \
			f = "<null>"; \
		sprintf (temp, f.c_str(), t); \
		buf += temp; \
		return *this; \
	}

	PERCENT(int,"idoxX");
	PERCENT(long,"idoxX");
	PERCENT(unsigned int,"uoxX");
	PERCENT(unsigned long,"uoxX");
	PERCENT(double,"feEgG");
	PERCENT(char,"c");
	PERCENT(unsigned char,"c");
	PERCENT(char *,"s");
	PERCENT(void *,"p");

#	undef PERCENT

	operator const char * () const
	{
		return (eval()).c_str();
	}

	operator const string & () const
	{
		return eval();
	}

	/// "Evaluate" the format buffer as filled in so far, which includes
	/// the remainder of the format string not yet parsed, and which also
	/// resets the format to the beginning.
	const string &eval () const
	{
		// This method is labelled const so it can be called on
		// temporary objects, but we have to kludge things of course.
		Format &f = const_cast<Format &>(*this);
		f.buf.append (fmt, pos, fmt.length() - pos);
		f.pos = fmt.length();
		return f.buf;
	}

	/// Return error string for a given error
	const string &errstr (Error e)
	{
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

	// Error messages
	static const string blank;
	static const string bad;
	static const string missing;
	static const string mismatch;
	static const string unknown;

	// Flag string
	static const char *Specifiers;

	string parse_format (char *temp, char *flag)
	{
		if (pos >= fmt.length())
			reset();
		string f;
		do {
			f = next_format (flag);
			if (*flag == '%')
			{
				sprintf (temp, f.c_str());
				buf += temp;
			}
		}
		while (*flag == '%');
		return f;
	}


	string next_format (char *flag = 0)
	{
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
		pos = fmt.length();
	}
	/// Apply an argument list using a buffer of size 'n'
	Printf (unsigned int n, const char *s ...) : Format(s)
	{
		char *sbuf = new char[n];
		va_list vl;
		va_start (vl, s);
		vsprintf (sbuf, s, vl);
		va_end (vl);
		buf = sbuf;
		pos = fmt.length();
		delete[] sbuf;
	}
};



inline std::ostream & operator<< (std::ostream &out, const Format &f)
{
	//out << (const_cast<Format &>(f)).eval();
	out << f.eval();
	return out;
}

#endif /* _Format_hh_ */

