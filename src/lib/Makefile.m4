#		Copyright (C) 1987,88,89,90,91,92 by UCAR
#	University Corporation for Atmospheric Research
#		   All rights reserved
#
# No part of this work covered by the copyrights herein may be reproduced
# or used in any form or by any means -- graphic, electronic, or mechanical,
# including photocopying, recording, taping, or information storage and
# retrieval systems -- without permission of the copyright owner.
# 
# This software and any accompanying written materials are provided "as is"
# without warranty of any kind.  UCAR expressly disclaims all warranties of
# any kind, either express or implied, including but not limited to the
# implied warranties of merchantibility and fitness for a particular purpose.
# UCAR does not indemnify any infringement of copyright, patent, or trademark
# through use or modification of this software.  UCAR does not provide 
# maintenance or updates for its software.
#
# $Id: Makefile.m4,v 2.1 2000-12-11 22:22:20 granger Exp $

BeginComponent(ZebraLibrary)
ImportBase(zebra/Zebra)
ImportBase(Libtool)

ifdef HasWritev
WRITEVOBJ=
WRITEVSRC=
else
WRITEVOBJ=writev.o
WRITEVSRC=writev.c
endif

# [E]xport our library name and include spec for other modules using
# global variables.
LIBZEBRA = LibtoolLibraryTargetName(Zebra)
LIBZEBRAINCLUDES = -I$(SourceDir)

ComponentIncludes(-I$(SourceDir). $(CONFIGINCLUDES))

Local(DEPEND_DEFINES) = \
	$(SHMDEFINES) $(DIRDEFINES) $(XINCLUDES) $(RDSSINCLUDES)

NormalLibrary(Zebra, \
	\
	alarmwidget.c altunits.c barb.c Bitmaps.c byteorder.c cmd_proto.c \
	cmd_exec.c ConfigVars.c ConfigUI.c convert.c GraphicsW.c ImageXfr.c \
	Buttons.c msg_BCast.c msg_lib.c netread.c Parse.c pdlib.c pdretrv.c \
	pdaux.c RLEncode.c Sound_lib.c TCvt.c TInterp.c timer_lib.c twidget.c \
	vector.c $(WRITEVSRC) FindFile.c zl_regex.c dm_lib.c \
	location.c version.c zl_symbol.c zl_string.c T_Expect.c T_Profile.c \
	zebra_rpc_xdr.c map.c map_xdr.c \
	\
	BlockFile.cc FreeList.cc Journal.cc XDR.cc \
	Logger.cc Format.cc Buffer.cc AuxBlock.cc BlockObject.cc \
	SerialBuffer.cc SerialStream.cc Serializable.cc ZTime.cc)

InstallLibrary(Zebra, $(LIBDIR))

AddSourceFiles(cvt_test.c re_test.c msg_test.c)

XDRTarget(zebra_rpc)
XDRTarget(map)

SpecialSourceRule(alarmwidget.c,$(XINCLUDES) $(RDSSINCLUDES))
SpecialSourceRule(twidget.c,$(XINCLUDES) $(RDSSINCLUDES))
SpecialSourceRule(dm_lib.c,$(XINCLUDES) $(RDSSINCLUDES))

SpecialSourceRule(Bitmaps.c,$(XINCLUDES))
SpecialSourceRule(barb.c,$(XINCLUDES))
SpecialSourceRule(vector.c,$(XINCLUDES))
SpecialSourceRule(Buttons.c,$(XINCLUDES))

SpecialSourceRule(pdlib.c,$(RDSSINCLUDES))
SpecialSourceRule(pdretrv.c,$(RDSSINCLUDES))
SpecialSourceRule(pdaux.c,$(RDSSINCLUDES))
SpecialSourceRule(cmd_proto.c,$(RDSSINCLUDES))

SpecialSourceRule(ConfigVars.c,$(DIRDEFINES) $(RDSSINCLUDES))
SpecialSourceRule(ConfigUI.c,$(DIRDEFINES) $(RDSSINCLUDES))

SpecialSourceRule(GraphicsW.c, $(SHMDEFINES) $(XINCLUDES) $(RDSSINCLUDES))

SourceFiles(TESTOBJS, cvt_test.o re_test.o msg_test.o)

Local(TEMPLATES) = BTree.cc BTreeFile.cc

SourceFiles(HEADERS, \
	$(Local(TEMPLATES)) \
	\
	bitmaps.h copyright.h defs.h GraphicsW.h GraphicsWP.h message.h \
	pd.h timer.h ImageXfr.h zl_regex.h dm.h twidget.h byteorder.h \
	draw.h version.h zl_symbol.h zl_param.h Test.h zebra.h map.h \
	ZTime.h zl_mtio.h \
	\
	BTree.hh BTreeFile.hh BTreeStats.hh BlockFile.hh \
	Format.hh SerialZTime.hh Serialize.hh Logger.hh \
	ZTime.hh ZTime.h $(DOCXX_HEADERS))


RPCZEBRA = zebra_rpc.h zebra_rpc_xdr.c
ZEBRA.x = zebra_rpc.x

RPCMAP = map.h map_xdr.c
MAP.x = map.x

RPCTARGETS = $(RPCZEBRA) $(RPCMAP)
SOURCES.x = $(ZEBRA.x) $(MAP.x)

Local(PHEADERS) = setup.h BlockFileP.hh

# DISTFILES = $(SRCS) writev.c $(HEADERS) Imakefile $(PHEADERS) $(SOURCES.x)

BuildIncludesTop($(Local(HEADERS)))
InstallMultipleFlags($(Local(HEADERS)),$(INCDIR),$(INSTINCFLAGS))

LocalFiles(MSGOBJS, msg_lib.o netread.o version.o)
LocalFiles(CVTOBJS, cvt_test.o convert.o)
LocalFiles(REOBJS, re_test.o zl_regex.o version.o)

TestProgramTarget(cvt_test,$(Local(CVTOBJS)) $(Local(MSGOBJS)),,,$(LIBMATH))

TestProgramTarget(re_test,$(Local(REOBJS)),,,$(LIBREGEXP))

TestProgramTarget(msg_test, $(SourceDir)msg_test.o $(Local(MSGOBJS)),,,)

RunTestProgram(cvt_test,coordinate conversion,$(SourceDir)cvt_test)
RunTestProgram(msg_test,error logging,$(SourceDir)msg_test > /dev/null 2>&1)
RunTestProgram(re_test,regular expressions,$(SourceDir)re_test)

LocalFiles(BSOBJS, Buffer.o SerialBuffer.o SerialStream.o \
		Serializable.o XDR.o Logger.o \
		Format.o BlockFile.o FreeList.o Journal.o BlockObject.o)

CccTestProgramTarget(tblocks,T_Block.o $(BSOBJS),,,)

CccTestProgramTarget(dbf,dbf.o $(BSOBJS),,,)

LocalFiles(TBTREEOBJS, T_BTree.o TestTrees.o $(BSOBJS) TCvt.o version.o)
CccTestProgramTarget(tbtree,$(TBTREEOBJS),,,)

DistfilesTarget($(DISTFILES))

