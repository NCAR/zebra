/*
 * $Id: Logger.hh,v 1.2 1997-11-24 10:41:39 granger Exp $
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
#include <strstream.h>
#include <string.h>		// for strerror()
#include <errno.h>



/* NOTES

   Add methods for registering entrance and exit from routines or
   components, possibly taking advantage of automatic variables so that
   we're not messed up by exceptions.  Perhaps a method also for just
   announcing "i was reached" or "i am done."
   
   Allow selection of messages to be logged with a predicate function which
   matches the message string, level, or "stack trace".  After compiling
   the stack, message, and level of the log event, pass to a single method
   which subclasses can easily override for directing the output.

   Allow a global "default" logger?  Create a logger hierarchy so that log
   messages to one logger can be passed back up the tree, possibly to
   loggers with different behaviors or selection criteria.

   */
   

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

	// Log a message at the given level.
	// This is the method to override in subclasses.

	virtual void Log (int levels, const char *s)
	{
		if (name)
			cerr << name << ": ";
		cerr << levelName(levels) << ": " << s << endl;
	}

#define LOG_METHOD(name,level) \
	virtual void name (const char *s) \
	{ \
		Log (level, s); \
	}

	// Log messages at PROBLEM/ERROR level
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
	virtual void System (const char *s)
	{
		ostrstream buf;
		buf << "'" << strerror(errno) << "' (" << errno << "): ";
		buf << s;
		Problem (buf.str());
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

	virtual ~Logger ()
	{
		if (name)
			free (name);
	}

protected:
	char *name;

};



class NullLogger : public Logger
{
	virtual void Log (int /*levels*/, const char *)
	{

	}
};



#endif /* _Logger_hh_ */
