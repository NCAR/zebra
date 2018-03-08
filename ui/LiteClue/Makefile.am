AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = LiteClue.gif LiteClue.html LiteClue.ps LiteClue.txt

#
# Of course this only works if X11 is present
#
LiteClueTest_SOURCES = LiteClueTest.c LiteClue.c LiteClue.h LiteClueP.h

if WITH_X
  noinst_PROGRAMS = LiteClueTest
endif
