
BeginComponent(ZebraDatastore)
ImportBase(zebra/Zebra)
ImportBase(Libtool)

RequireComponent(ZebraDatastore,../lib)

# --- global variables to export dependencies to other modules ---

LIBZEBRADS = LibtoolLibraryTargetName(ZebraDS)
LIBZEBRADSINCLUDES = -I$(SourceDir).

# ----------------------------------------------------------------

Local(MFVERSION) = \
	"$Id: Makefile.m4,v 3.1 2000-12-11 22:21:50 granger Exp $"

Local(DMNSYSLIBS) = $(LIBREGEXP) $(LIBMATH) $(LIBTERMCAP)
Local(DMNDEPLIBS) = $(LIBZEBRADS) $(ZLIBS)
Local(DMNLIBS) = $(LIBZEBRADS) $(ZLIBS) $(XLIBS) \
                 $(LIBNETCDF) $(LIBHDF) $(LIBUDUNITS) $(LIBSWF)

ComponentIncludes($(CONFIGINCLUDES) $(RDSSINCLUDES) $(LIBZEBRAINCLUDES))

Local(DEPEND_DEFINES) = 

#
# Data File Access modules
#
Local(DFASRCS) = \
	  DataFormat.c DFA_NetCDF.c DFA_Zebra.c DFA_HDF.c DFA_Grads.c \
          DFA_Boundary.c DFA_GRIB.c GRIB.c DFA_Raster.c Raster.c DFA_None.c \
	  DFA_SweepFile.cc

# 
# Data chunk modules 
#
Local(DCSRCS) = \
	 DataChunk.c DataAttr.c dc_Boundary.c dc_IRGrid.c dc_Image.c \
	 dc_MetData.c dc_MetAttr.c dc_RGrid.c dc_Scalar.c dc_Transp.c \
	 dc_TrAttr.c dc_Attr.c dc_Location.c dc_NSpace.c dc_Process.c \
	 dc_Elements.c dc_ADE.c dc_All.c Details.c dc_Polar.c

#
# Shared platform access interface
#
Local(PLATSRCS) = \
	    Platforms.c p_Appl.c p_Table.c DataTypes.c DataTypes_xdr.c

#
# Modules shared between client library and daemon
#
Local(SHAREDSRCS) = \
	d_Notify.c d_Source.cc DataFiles.cc ds_fields.cc Field.cc Source.cc
Local(SHAREDSRCS) += $(Local(DFASRCS)) $(Local(DCSRCS)) $(Local(PLATSRCS)) 

#
# Daemon-specific object modules 
#
Local(DSSRCS) = Daemon.c d_Config.c d_Scan.c d_Debug.c


Local(DSLIBSRCS) = \
	    Appl.c DFA_Appl.c SA_Appl.c ingest.c GetList.c \
	    p_Client.c ds_Derive.cc DerivNode.cc DerivTable.cc FDScan.cc \
	    FDParse.tab.cc
Local(DSLIBSRCS) += $(Local(SHAREDSRCS))

Local(UTILSRCS) = rfdump.c zfdump.c GRIBdump.c bfdump.c

#
# Text file of default derivation definitions
#
Local(DERIVDEF) = Derivations

Local(PRIVATE_HEADERS) = \
		  BoundaryFile.h DataChunkP.h dslib.h znfile.h \
		  RasterFile.h commands.h dfa.h dsDaemon.h dsPrivate.h \
		  GRIB.h Appl.h DataFormat.h Platforms.h apple.h GetList.h \
		  DerivNode.h DerivTable.h Field.h

Local(HEADERS) = \
	DataChunk.h DataFiles.h DataStore.h DataTypes.h ds_fields.h \
	ingest.h Platforms.h

CccProgramTarget(dsDaemon,$(Local(DSSRCS)),$(Local(DMNDEPLIBS)),\
                 $(Local(DMNLIBS)),$(Local(DMNSYSLIBS)))

UILoadFileTarget(dsDaemon.lf,Daemon.state)

#
# XDR targets
#
XDRTarget(DataTypes)

#
# Make the library.
#
NormalLibrary(ZebraDS, $(Local(DSLIBSRCS)))

SpecialSourceRule(DFA_NetCDF.c T_NetCDF.c, ,$(NETCDFINCLUDES))
SpecialSourceRule(DFA_HDF.c T_HDF.c, ,$(HDFDEFINES) $(HDFINCLUDES))
SpecialCplusplusSourceRule(Field.cc, ,$(UDUNITSINCLUDES))
SpecialCplusplusSourceRule(DFA_SweepFile.cc, ,$(SWFDEFINES) $(SWFINCLUDES))

InstallLibrary(ZebraDS, $(LIBDIR))
InstallNonExecFile($(Local(DERIVDEF)), $(LIBDIR))

BuildIncludesTop($(Local(HEADERS)))
InstallMultipleFlags($(Local(HEADERS)),$(INCDIR),$(INSTINCFLAGS))

# 
# Special stuff for the field derivation flex scanner/bison parser code.  The
# depend:: target assures that FDScan.cc and FDParse.tab.cc get built before 
# we generate the dependency list. 
#
$(SourceDir)FDScan.cc: $(SourceDir)FDScan.flex $(SourceDir)FDParse.tab.h
	flex -t -PFD $(SourceDir)FDScan.flex > $@
	$(CP) -f $@ $@.dist

$(SourceDir)FDParse.tab.h: $(SourceDir)FDParse.tab.cc
	$(CP) -f $@ $@.dist

$(SourceDir)FDParse.tab.cc: $(SourceDir)FDParse.bison
	bison -p FD -d --file-prefix=$(SourceDir)FDParse $(SourceDir)FDParse.bison
	$(MV) $(SourceDir)FDParse.tab.c $(SourceDir)FDParse.tab.cc
	$(CP) -f $@ $@.dist

dnl ZebraMakeDepend()
dnl CcDependTarget($(SRCS))
dnl CccDependTarget($(SRCS_CXX))

CcDependTarget($(Local(UTILSRCS)))

