/* rawget.h -- include file that defines structure of single time
 * P-3 aircraft slow tape -- data in ascii form from Dave Jorgensen
 */

#define NVARS 40

struct p3slowblock {
  float ac;                          /* aircraft (42 or 43) */
  float mo,dy,yr,hr,mn,sc;           /* date and time */
  float nav;                         /* nav unit -- 1 = ine #1
                                                    2 = ine #2
                                                    4 = gps    */
  float la1,lo1,la2,lo2,la4,lo4;     /* lat-lon for each nav unit */
  float gx1,gy1,gx2,gy2,gx4,gy4;     /* x and y ground speed each nav */
  float hd;                          /* heading */
  float ra1,ra2,ra3;                 /* 3 radar altitudes (m) */
  float pc;                          /* pitch */
  float rl;                          /* roll */
  float ps;                          /* static pressure (mb) */
  float ta,td;                       /* temp, dewpt (C) */
  float gs;                          /* ground speed (m/s) */
  float tk;                          /* track angle */
  float ws,wd;                       /* wind speed (m/s) and direction */
  float rd,rs,ru;                    /* down, side, up radiometers (C) */
  float uw;                          /* vertical wind (m/s) */
  float ui;                          /* vertical ground speed (m/s) */
  float lw;                          /* J-W liquid water (g/m^3) */
  float tas;                         /* true air speed (m/s) */
};
