/*
 * Logger implementation.
 */


#include "Logger.hh"

const int Logger::EMERGENCY = 0x01;
const int Logger::PROBLEM = 0x02;
const int Logger::ERROR = 0x02;
const int Logger::CLIENT = 0x04;
const int Logger::DEBUG = 0x08;
const int Logger::INFO = 0x10;
const int Logger::DEVELOP = 0x20;
const int Logger::ALL = 0x3f;


Logger * Logger::prototype = 0;


/*
 * Return a clone of the logger prototype for the given name.
 */
Logger *
Logger::For (const char *name)
{
	if (prototype)
		return prototype->Clone (name);
	return new Logger (name);
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

