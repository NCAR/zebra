/*
 * $Id: Logger.hh,v 1.1 1997-08-01 19:34:28 granger Exp $
 *
 * Class for reporting error and log messages, which can be conveniently
 * subclassed to log messages through different facilities.  A subclass
 * need only override the Log() method to change the logging behavior
 * of all of the default Log methods.  Messages are logged with a
 * newline.
 *
 * The base class logs messages to the error stream, cerr.
 *
 * An example NullLogger subclass is provided which ignores all log
 * messages.
 */

#ifndef _Logger_hh_
#define _Logger_hh_

#include <iostream.h>
#include <stdarg.h>
#include <string.h>		// for strerror()
#include <errno.h>

class Logger
{
public:
	static const int EMERGENCY = 0x01;
	static const int PROBLEM = 0x02;
	static const int ERROR = 0x02;
	static const int CLIENT = 0x04;
	static const int DEBUG = 0x08;
	static const int INFO = 0x10;
	static const int DEVELOP = 0x20;
	static const int ALL = 0x3f;

	virtual const char *levelName (int level)
	{
		if (level & EMERGENCY)
			return ("Emergency");
		if (level & PROBLEM)
			return ("Problem");
		if (level & INFO)
			return ("Info");
		if (level & DEBUG)
			return ("Debug");
		if (level & CLIENT)
			return ("Client");
		if (level & DEVELOP)
			return ("Develop");
		return ("None");
	}

	// Log a va_list of printf format arguments.
	// This is the method to override in subclasses.

	virtual void Log (int levels, const char *s, va_list vl)
	{
		char sbuf[1024];
		vsprintf (sbuf, s, vl);
		if (name)
			cerr << name << ": ";
		cerr << levelName(levels) << ": " << sbuf << endl;
	}

	// Log a message using printf formatting at particular levels
	virtual void Log (int levels, const char *s ...)
	{
		va_list vl;
		va_start (vl, s);
		Log (levels, s, vl);
		va_end (vl);
	}

#define LOG_METHOD(name,level) \
	virtual void name (const char *s, ...) \
	{ \
		va_list vl; \
		va_start (vl, s); \
		Log (level, s, vl); \
		va_end (vl); \
	}

	// Log printf formatted message at PROBLEM/ERROR level
	LOG_METHOD(Error,ERROR);
	LOG_METHOD(Problem,PROBLEM);

	// Emergencies
	LOG_METHOD(Emergency,EMERGENCY);

	// Log messages at DEBUG level
	LOG_METHOD(Debug,DEBUG);

	// INFO level shortcut
	LOG_METHOD(Info,INFO);

	// Low, low priority development messages
	LOG_METHOD(Develop,DEVELOP);

	// Log system error messages
	virtual void System (const char *s ...)
	{
		char buf[1024];
		va_list vl;
		va_start (vl, s);
		vsprintf (buf, s, vl);
		Problem ("'%s' (%d): %s", strerror(errno), errno, buf);
		va_end (vl);
	}

	Logger ()
	{
		name = NULL;
	}

	Logger (const char *name)
	{
		this->name = (char *) malloc (strlen(name) + 1);
		strcpy (this->name, name);
	}

#ifdef notdef
	Logger (const char *name = NULL)
	{
		if (name)
		{
			this->name = (char *) malloc (strlen(name) + 1);
			strcpy (this->name, name);
		}
		else
			this->name = NULL;
	}
#endif

	~Logger ()
	{
		if (name)
			free (name);
	}

protected:
	char *name;

};



class NullLogger : public Logger
{
	virtual void Log (int levels, const char *s, va_list vl)
	{

	}
};



#endif /* _Logger_hh_ */
