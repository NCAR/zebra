/*
 * $Id: Logger.hh,v 1.6 1998-05-15 21:47:25 granger Exp $
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

#include <stdlib.h>		// For malloc() and free()
#include <iostream.h>
#include <strstream.h>
#include <string>		// for strerror()
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

   Provide:

   log << ... << Logger::debug;
   log << ... << Logger::info;

   */
   

class Logger
{
public:
	/* ---------------- Class members ---------------- */

	static const int EMERGENCY;
	static const int PROBLEM;
	static const int ERROR;
	static const int CLIENT;
	static const int DEBUG;
	static const int INFO;
	static const int DEVELOP;
	static const int ALL;

	static Logger *For (const char *name = 0);
	static void SetPrototype (const Logger & proto);

	/* ---------------- Instance methods ---------------- */

	virtual const char *levelName (int level) const
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

	Logger (const char *_name = 0) : name (0)
	{
		Name (_name);
	}

		Logger (const Logger &log) : name (0)
	{
		Name (log.name);
	}
		
	Logger& operator= (const Logger &log)
	{
		Name (log.name);
	}

	/*
	 * Prototype pattern: Subclasses override this to return a logger
	 * of their particular class.
	 */
	virtual Logger *Clone (const char *_name = 0) const
	{
		return new Logger (_name);
	}

	virtual ~Logger ()
	{
		if (name)
			free (name);
	}

	// Not actually meant for public access, but needed for the template
	// operator<<
	ostrstream out;

protected:
	char *name;

	void Name (const char *_name)
	{
		if (this->name)
		{
			free (this->name);
			this->name = 0;
		}
		if (_name)
		{
			this->name = (char *) malloc (strlen(_name) + 1);
			strcpy (this->name, _name);
		}
	}

private:

	static Logger *prototype = 0;
};


/*
 * To make Logger objects behave like output streams.  Use the
 * log manipulators to send the message at a particular level.
 */
template <class T>
Logger &operator<< (Logger &log, T &t)
{
	return log;
}



class NullLogger : public Logger
{
public:
	NullLogger (const char *name = 0) : Logger (name)
	{ }

	virtual void Log (int /*levels*/, const char *)
	{ }

	virtual Logger *Clone (const char *name) const
	{
		return new NullLogger (name);
	}
};



#endif /* _Logger_hh_ */
