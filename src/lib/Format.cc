/* $Id: Format.cc,v 1.2 1998-10-20 20:44:42 granger Exp $
 *
 * Static class data for Format class.
 */

#include "Format.hh"

const string Format::blank("");
const string Format::bad("*bad format*");
const string Format::missing("*missing format*");
const string Format::mismatch("*type mismatch*");
const string Format::unknown("*unknown error*");

const char *Format::Specifiers = "iduoxXfeEgGcsp%";

