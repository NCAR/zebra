
#include <time.h>
#include <math.h>

#include <string>

#include "message.h"
#define inline
#include "ZTime.h"
#undef inline

int check (const char *test, bool cond, int line = 0)
{
    if (! cond)
    {
	msg_ELog (EF_PROBLEM, "test failed (line %d): %s", line, test);
	return 1;
    }
    return 0;
}


#define TEST(expr) errors += check (#expr, (expr), __LINE__)

int 
main (int, char *argv[])
{
    int errors = 0;

    msg_connect (0, argv[0]);

    ZTime zt;

    // Test default construction with 0 constructors
    TEST(zt == 0);
    TEST(zt == ZTime(0,0));

    long sys = time(0);

    // Compare long constructor with double constructor.
    TEST(ZTime(sys,0) == ZTime(sys));

    // Assignment from long
    zt = sys;
    TEST((zt = sys) == ZTime(sys));


    // Assembly, then individual fields.
    zt.assemble (2001, 8, 10, 9, 11, 12, 1234);
    TEST(zt.year() == 2001);
    TEST(zt.month() == 8);
    TEST(zt.day() == 10);
    TEST(zt.hour() == 9);
    TEST(zt.minute() == 11);
    TEST(zt.second() == 12);

    TEST(zt == zt);
    ZTime a;
    a.assemble (2001, 8, 10, 9, 11, 12, 0);
    TEST(a < zt);
    TEST(a <= zt);
    TEST(a != zt);
    TEST(zt > a);
    TEST(zt >= a);
    TEST(zt <= zt);

    // Convert to double, re-construct, and re-compare
    double seconds = zt.toDouble();
    ZTime zt2(0);
    zt2 = seconds;
    TEST(fabs(zt2.toDouble() - seconds) < 0.00001);
    
    // And their encodings should be equal also.
    string one(zt.ascTime());
    string two(zt2.ascTime());
    string cond = one + " == " + two;
    errors += check (cond.c_str(), (one == two), __LINE__);

    // Test normalization.  These should all be equal.
    ZTime norm ((long)seconds+2, 500000);
    ZTime t1 = ZTime((long)seconds, 2500000);
    TEST(t1 == norm);
    ZTime t2 = ZTime((long)seconds+3, -500000);
    TEST(t2 == norm);

    // Try our hand at decoding.
    zt.decode ("04-Jan-2001,15:46:13.25");
    // This also tests the y2k check.
    zt2.assemble (1, 1, 4, 15, 46, 13, 250000);
    TEST (zt == zt2);

    ZTime zt3 = zt2 + 0.25;
    ZTime zt4 = 0.25 + zt2;
    ZTime qtrsec = 0.25;
    TEST (zt3 == zt4);
    zt3 = qtrsec + zt2;
    TEST (zt3 == zt4);
    zt.assemble (1, 1, 4, 15, 46, 13, 2000000);
    zt2 += 2;
    zt2 -= 0.25;
    TEST (zt == zt2);

    TEST (sizeof(ZebraTime) == sizeof(ZTime));

    return errors;
}
