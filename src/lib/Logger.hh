/*
 * $Id: Logger.hh,v 1.10 2002-09-17 20:00:19 granger Exp $
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

#include <iostream>
#include <sstream>
#include <string>		// for strerror()
#include <errno.h>



/* NOTES

   Add a Log class with methods for reporting errno and a message and which
   includes an error stream (even an in-memory string) and flush method
   perhaps.  Subclasses can override as needed, such as redirecting error
   messages to the EventLogger.

   For logging: when a library layer or module selects a log object by
   name, use that name as a lookup in a class map to qualify the log mask,
   so the application can set the mask for modules by name when the module
   asks for its log object.  The application could also set the class of
   loggers to be used, such as log to file, stdout, EventLogger, composite,
   whatever, or disable logging altogether by always returning the
   NullLogger.

   Add methods for registering entrance and exit from routines or
   components, possibly taking advantage of automatic variables so that
   we're not messed up by exceptions.  Perhaps a method also for just
   announcing "i was reached" or "i am done."
   
   EnterBlock eb(log, "routine name", level = DEBUG);
   Checkpoint cp(log, "computations finished", level = DEBUG);

   Allow selection of messages to be logged with a predicate function which
   matches the message string, level, or "stack trace".  After compiling
   the stack, message, and level of the log event, pass to a single method
   which subclasses can easily override for directing the output.

   Allow a global "default" logger?  Create a logger hierarchy so that log
   messages to one logger can be passed back up the tree, possibly to
   loggers with different behaviors or selection criteria.  This would be a
   "composite" logger...

   Provide:

   log << ... << Logger::debug;
   log << ... << Logger::info;

   I like the idea of a circular buffer in Nana.  We should be able to
   implement such a class as a subclass of Logger.  How to keep logging
   possible in library modules like BlockFile without using lots of
   overhead when not needed?  I'd be ok with allowing the virtual call to a
   no-op logger, but why not avoid the Printf and Format constructors when
   the message will not even be used?  We could surround log calls with a
   macro which can be nullified for special builds, and otherwise tests
   whether the logger is null or no-op before evaluating the message text.

	LOG(log,Info(Printf(...))); becomes

	if (log) log->Info(Printf(...));

   Also add options for prepending timestamps to the log messages.

   It would be nice just to add a call to log a message:

	Log::Info(msg);

   but if we want to identify the context of the message, we need our own
   logger reference or another parameter.
 
   Add a simple object to a class or static module which gets intialized
   and destroyed automatically:

	Logger log("BlockFile");

   The constructor registers the context name, and log calls always use
   that logger instance.  It becomes a proxy for another logger, and/or it
   gets its mask/behavior/prototype from the logger configuration.  So
   Logger is an implementation which actually logs messages, and Sender is
   a subclass which uses a proxy to send messages to the actual logger.  By
   default a Sender does nothing.

   To shortcut Printf and Format construction, perhaps try
   
	log.Debug (log && Printf());
   
   where an inline boolean operator for log returns false if nothing will
   be logged.  Or log() && log.Debug (Printf())...

   Might be nice to pass the contexts up the call hierarchy as part of a
   list, rather than a string, so that the final logger can decide how to
   convert the list of contexts to a message or display string.
   
   Use Push() and Pop() methods to add to the context list.  Make the list
   a stack of context strings.

   Just pass an automatic object reference which gets cloned:

   Logger::Prototype (StreamLogger(cerr), "unit X");

   The prototype could have a flag vector for specific events, such as
   construction/destruction, which will be logged when the Sender or Logger
   object is created or destroyed, corresponding to the object which
   contains it.

   Move implementation into source file, especially the methods which
   cannot be inlined, to reduce dependency on implementation in header.

   Provide a macro to include logging code within which can be defined to
   empty to disable all logging code generation.  Or perhaps inline
   functions versus macros which can be used like:

   Debug(log,"...")

   which is either empty or becomes log.Debug("...").

   EnterBlock(...) becomes EnterBlock eb(...)

   Might be nice for a class to be able to subclass a Sender, so that
   initialization and destruction could be taken care of automatically,
   except that wouldn't work because the Sender still needs to be
   initialized explicitly to set the class or package name for logging.

   The default Logger should use a circular queue of messages so that
   applications can always dump the most recent library messages in case of
   an error or outstanding condition, or even on a request from the user.
   Sort of a glorified strerror().

   */
   

class LogRegistry;


class Logger
{
public:
	typedef std::string string;
	typedef std::ostringstream ostringstream;
        typedef std::ostream ostream;

	/* ---------------- Class members ---------------- */

	static const int EMERGENCY;
	static const int PROBLEM;
	static const int ERROR;
	static const int CLIENT;
	static const int DEBUG;
	static const int INFO;
	static const int DEVELOP;
	static const int ALL;

	/*
	 * To configure the logging of a particular unit, assign
	 * a prototype to a string which will match that unit's 
	 * name.  More recent prototype's take precedence.
	 */
	static void Prototype (const Logger &, const string &unit);

	/*
	 * To set the default prototype for all units, omit the unit.
	 */
	static void Prototype (const Logger &);

protected:

	static Logger *Register (Logger *log);
	static void Forget (Logger *log);

public:
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


	/* ----------------
	 * Log a message at the given level.
	 * This is the method to override in subclasses.
	 * ----------------
	 */
	virtual bool Log (int levels, const string &s) = 0;


#define LOG_METHOD(name,level) \
	virtual bool name (const string &s) \
	{ \
		return Log (level, s); \
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
	virtual bool System (const string &s)
	{
		ostringstream buf;
		buf << "'" << strerror(errno) << "' (" << errno << "): ";
		buf << s;
		Problem (buf.str());
		return true;
	}

	Logger (const string &_context = "")
	{
		Declare (_context);
	}

	Logger (const Logger &log)
	{
		Declare (log.context);
	}
		
	Logger& operator= (const Logger &log)
	{
		Declare (log.context);
		return *this;
	}

	/*
	 * Prototype pattern: Subclasses override this to return a logger
	 * of their particular class.
	 */
	virtual Logger *Clone (const string &_context) const = 0;

	Logger *Clone () const
	{
		return Clone (Context());
	}

	virtual ~Logger () { }

	// Return our unit context.
	const string &Context () const { return context; }

	// Set the current context string.
	virtual void Declare (const string &_context)
	{
		context = _context;
	}

	// Extend the current context.
	virtual void Extend (const string &_context)
	{
		context += "(" + _context + ")";
	}

protected:

	string context;

private:

	static LogRegistry *registry;
};



class NullLogger : public Logger
{
public:
	NullLogger (const string &name = "") : Logger (name)
	{ }

	virtual bool Log (int /*levels*/, const string &/*msg*/)
	{
		return false;
	}

	virtual Logger *Clone (const string &name) const
	{
		return new NullLogger (name);
	}
};



class StreamLogger : public Logger
{
public:

	StreamLogger (ostream &_out, const string &name = "") :
		Logger (name), out(_out)
	{ }

	StreamLogger (const string &name = "") :
		Logger (name), out(std::cerr)
	{ }

	virtual bool Log (int levels, const string &s)
	{
		out << context << ": ";
		out << levelName(levels) << ": " << s << std::endl;
		return true;
	}

	virtual Logger *Clone (const string &_context) const
	{
		return new StreamLogger (out, _context);
	}

protected:

	ostream &out;
};
   


/*
 * Inherit the Logger interface and implement a proxy mechanism.  The
 * constructor registers this object with the Logger class under its given
 * name and receives in reply its logger prototype, to which log messages
 * will actually be sent.  The destructor de-registers this object with the
 * class.  The Sender class includes a Change callback method which the
 * class can use to change the object's prototype.
 */
class Sender : public Logger
{
public:

	Sender (const string &name = "") : Logger (name), log(0)
	{
		log = Logger::Register (this);
	}

	Sender (const Sender &send) : Logger (*this), log(0)
	{
		Assign (send);
	}

	Sender &operator= (const Sender &send)
	{
		log = 0;
		Assign (send);
		return *this;
	}

	/*
	 * Derive a sender from an existing one and add a sub-context.
	 */
	Sender (const Sender &send, const string &sub)
	{
		Assign (send);
		Extend (sub);
	}

	virtual bool Log (int levels, const string &msg)
	{
		if (log)
			return log->Log (levels, msg);
		return false;
	}

	virtual Logger *Clone (const string &name) const
	{
		Sender *send = new Sender (*this);
		send->Declare (name);
		return send;
	}

	virtual void Declare (const string &_context)
	{
		Logger::Declare (_context);
		if (log) log->Declare (_context);
	}

	virtual void Extend (const string &_context)
	{
		Logger::Extend (_context);
		if (log) log->Extend (_context);
	}

	virtual ~Sender ()
	{
		Logger::Forget (this);
		if (log)
			delete log;
	}

	/* ---------------- Sender methods ---------------- */

	void Change (const Logger &prototype)
	{
		if (log)
			delete log;
		log = prototype.Clone (context);
	}

	operator bool() { return (log != 0); }

protected:

	void Assign (const Sender &send)
	{
		Logger::operator= (send);
		if (log)
			delete log;
		if ((log = send.log))
			log = log->Clone();
	}

	Logger *log;

};



/*
 * Define some helper classes which rely on automatic construction
 * and destruction to trigger their log messages.
 */
class EnterBlock 
{
public:
	typedef std::string string;

	EnterBlock (Logger &_log, const string &_msg, 
		    int _level = Logger::DEBUG) : 
		log(&_log), msg(_msg), level(_level)
	{
		log->Log (level, "Enter: " + msg);
	}

	~EnterBlock ()
	{
		log->Log (level, "Leave: " + msg);
	}

protected:

	Logger *log;
	string msg;
	int level;
};






#endif /* _Logger_hh_ */
