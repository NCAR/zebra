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

