/*
 * Logger implementation.
 */

#include <algorithm>
#include <vector>

using std::vector;

#include "Logger.hh"

const int Logger::EMERGENCY = 0x01;
const int Logger::PROBLEM = 0x02;
const int Logger::ERROR = 0x02;
const int Logger::CLIENT = 0x04;
const int Logger::DEBUG = 0x08;
const int Logger::INFO = 0x10;
const int Logger::DEVELOP = 0x20;
const int Logger::ALL = 0x3f;

using std::string;

class MatchContext
{
public:
	MatchContext (const string &s) : target(s) {}

	inline bool operator() (const Logger *log) const
	{
		return (log->Context()).find (target) != string::npos;
	}

	const string &target;
};


/*
 * A singleton class which keeps track of log units and their
 * mask and prototype configurations.
 */
class LogRegistry
{

public:

	LogRegistry() : fallback(0) { }

	void Add (Logger *log)
	{
		known.push_back (log);
	}

	void Delete (Logger *log)
	{
		vector<Logger *>::iterator it;
		it = find (known.begin(), known.end(), log);
		if (it != known.end())
			known.erase (it);
	}
	
	/*
	 * Look up this name in our configuration and return the
	 * prototype it should use, possibly null.
	 */
	const Logger *Lookup (const string &s)
	{
		vector<Logger *>::reverse_iterator it;
		it = find_if (protos.rbegin(), protos.rend(), MatchContext(s));
		if (it != protos.rend())
			return *it;
		return fallback;
	}

	void AddProto (const Logger &proto, const string &unit)
	{
		protos.push_back (proto.Clone (unit));
	}

	void DefaultProto (const Logger &proto)
	{
		if (fallback)
			delete fallback;
		fallback = proto.Clone();
	}

	~LogRegistry ()
	{
		// Need to delete all of our prototypes.
		vector<Logger *>::iterator it;
		for (it = protos.begin(); it != protos.end(); ++it)
		{
			delete *it;
		}
	}

private:

	// Array of known logger units
	vector<Logger *> known;

	// Array of prototype patterns
	vector<Logger *> protos;

	// The default prototype
	Logger *fallback;
};



LogRegistry *Logger::registry = 0;



/*
 * These methods allow a unit's log proxy (Sender) to register itself 
 * and receive the configured logger.
 */
Logger *Logger::Register (Logger *log)
{
	if (! registry)
		registry = new LogRegistry;
	registry->Add (log);
	const Logger *proto = registry->Lookup (log->Context());
	if (proto)
		return proto->Clone (log->Context());
	return 0;
}



void Logger::Forget (Logger *log)
{
	if (registry)
		registry->Delete (log);
}



/*
 * These are the application entry points for configuring the 
 * logging prototypes to be used for named units.
 */
void Logger::Prototype (const Logger &proto, const string &unit)
{
	if (! registry)
		registry = new LogRegistry;
	registry->AddProto (proto, unit);
}


/*
 * Specify the default prototype.
 */
void Logger::Prototype (const Logger &proto)
{
	if (! registry)
		registry = new LogRegistry;
	registry->DefaultProto (proto);
}




#if 0
/*
 * Return a clone of the logger prototype for the given name.
 * The default prototype is the null logger so that applications
 * not worried about underlying logging will not need to care.
 */
Logger *
Logger::For (const char *name)
{
	if (prototype)
		return prototype->Clone (name);
	return new NullLogger (name);
}


void
Logger::SetPrototype (const Logger & proto)
{
	if (prototype)
	{
		delete prototype;
		prototype = 0;
	}
	prototype = proto.Clone (proto.name);
}
#endif

