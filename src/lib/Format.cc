/* $Id: Format.cc,v 1.3 2001-08-24 22:23:14 granger Exp $
 *
 * Static class data for Format class.
 */

#include "Format.hh"

using std::string;

const string Format::blank("");
const string Format::bad("*bad format*");
const string Format::missing("*missing format*");
const string Format::mismatch("*type mismatch*");
const string Format::unknown("*unknown error*");

const char *Format::Specifiers = "iduoxXfeEgGcsp%";

