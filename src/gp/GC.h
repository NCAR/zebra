/*
 * A global graphics context available to any module.  Just leave it in
 * it's default state (as returned by XCreateGC with no special parameters)
 * when you're done with it.  The only exception is foreground color, which
 * can be left at any value.
 *
 * The GC is created in GraphProc.c
 */

extern GC	Gcontext;
