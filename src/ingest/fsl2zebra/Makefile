LDLIBS = -lnetcdf_c++ -lnetcdf -ludunits
CXXFLAGS = -g -O0

SRCS = fsl2zebra.cc fslMesonetVar.cc outVar.cc zebraMesonetOutFile.cc
HEADERS = fslMesonetVar.hh outVar.hh zebraMesonetOutFile.hh Station.hh
OBJS = fsl2zebra.o fslMesonetVar.o outVar.o zebraMesonetOutFile.o

fsl2zebra: $(OBJS)
	$(CXX) -o $@ $(LDFLAGS) $(OBJS) $(LDLIBS)

clean:
	$(RM) *.o


MDFILE = Makefile.dep

dependencies::
	$(CXX) -M $(CXXFLAGS) $(SRCS) >> $(MDFILE)

depend:: $(MDFILE)
	@echo Appending $(MDFILE) to Makefile ;\
	$(RM) Makefile.tmp ;\
	sed -e '/^# -- Compiler dependencies --/,$$d' Makefile \
	> Makefile.tmp ; \
	cat $(MDFILE) >> Makefile.tmp ;\
	mv -f Makefile.tmp Makefile

$(MDFILE): $(SRCS) $(HEADERS)
	@(set $? NONE; if [ $$1 != NONE -o ! -f $(MDFILE) ]; then \
	echo Creating $(MDFILE); $(RM) $(MDFILE) ;\
	echo '# -- Compiler dependencies --' > $(MDFILE) ; \
	$(MAKE) MAKE=$(MAKE) $(MFLAGS) dependencies; fi)

# -- Compiler dependencies --
fsl2zebra.o: fsl2zebra.cc /usr/include/g++-3/fstream \
  /usr/include/g++-3/fstream.h /usr/include/g++-3/iostream.h \
  /usr/include/g++-3/streambuf.h /usr/include/libio.h \
  /usr/include/_G_config.h /usr/include/bits/types.h \
  /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/gnu/stubs.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h \
  /usr/include/bits/pthreadtypes.h /usr/include/bits/sched.h \
  /usr/include/wchar.h /usr/include/bits/wchar.h /usr/include/gconv.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stdarg.h \
  /usr/include/g++-3/map /usr/include/g++-3/stl_tree.h \
  /usr/include/g++-3/stl_algobase.h /usr/include/g++-3/stl_config.h \
  /usr/include/g++-3/stl_relops.h /usr/include/g++-3/stl_pair.h \
  /usr/include/g++-3/type_traits.h /usr/include/string.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/limits.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/syslimits.h \
  /usr/include/limits.h /usr/include/bits/posix1_lim.h \
  /usr/include/bits/local_lim.h /usr/include/linux/limits.h \
  /usr/include/bits/posix2_lim.h /usr/include/stdlib.h \
  /usr/include/sys/types.h /usr/include/time.h /usr/include/endian.h \
  /usr/include/bits/endian.h /usr/include/sys/select.h \
  /usr/include/bits/select.h /usr/include/bits/sigset.h \
  /usr/include/bits/time.h /usr/include/sys/sysmacros.h \
  /usr/include/alloca.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/new.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/new \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/exception \
  /usr/include/g++-3/stl_iterator.h /usr/include/g++-3/stl_alloc.h \
  /usr/include/assert.h /usr/include/pthread.h /usr/include/sched.h \
  /usr/include/signal.h /usr/include/bits/initspin.h \
  /usr/include/bits/sigthread.h /usr/include/g++-3/stl_construct.h \
  /usr/include/g++-3/stl_function.h /usr/include/g++-3/stl_map.h \
  /usr/include/g++-3/stl_multimap.h /usr/include/stdio.h \
  /usr/include/bits/stdio_lim.h /usr/include/netcdf.hh \
  /usr/include/netcdfcpp.h /usr/include/netcdf.h /usr/include/errno.h \
  /usr/include/bits/errno.h /usr/include/linux/errno.h \
  /usr/include/asm/errno.h /usr/include/ncvalues.h \
  /usr/include/g++-3/strstream.h /usr/include/g++-3/strfile.h \
  /usr/local/include/udunits.h outVar.hh /usr/include/g++-3/string \
  /usr/include/g++-3/std/bastring.h /usr/include/g++-3/cstddef \
  /usr/include/g++-3/std/straits.h /usr/include/g++-3/cctype \
  /usr/include/ctype.h /usr/include/g++-3/cstring \
  /usr/include/g++-3/alloc.h /usr/include/g++-3/iterator \
  /usr/include/g++-3/std/bastring.cc /usr/include/g++-3/vector \
  /usr/include/g++-3/stl_uninitialized.h /usr/include/g++-3/stl_vector.h \
  /usr/include/g++-3/stl_bvector.h udunits.hh fslMesonetVar.hh Station.hh
fslMesonetVar.o: fslMesonetVar.cc /usr/include/math.h \
  /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/gnu/stubs.h /usr/include/bits/huge_val.h \
  /usr/include/bits/mathdef.h /usr/include/bits/mathcalls.h \
  /usr/include/stdio.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h \
  /usr/include/bits/types.h /usr/include/bits/pthreadtypes.h \
  /usr/include/bits/sched.h /usr/include/libio.h /usr/include/_G_config.h \
  /usr/include/wchar.h /usr/include/bits/wchar.h /usr/include/gconv.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stdarg.h \
  /usr/include/bits/stdio_lim.h /usr/include/netcdf.hh \
  /usr/include/netcdfcpp.h /usr/include/netcdf.h /usr/include/errno.h \
  /usr/include/bits/errno.h /usr/include/linux/errno.h \
  /usr/include/asm/errno.h /usr/include/ncvalues.h \
  /usr/include/g++-3/iostream.h /usr/include/g++-3/streambuf.h \
  /usr/include/g++-3/strstream.h /usr/include/g++-3/strfile.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/limits.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/syslimits.h \
  /usr/include/limits.h /usr/include/bits/posix1_lim.h \
  /usr/include/bits/local_lim.h /usr/include/linux/limits.h \
  /usr/include/bits/posix2_lim.h /usr/include/string.h fslMesonetVar.hh \
  /usr/include/g++-3/string /usr/include/g++-3/std/bastring.h \
  /usr/include/g++-3/cstddef /usr/include/g++-3/std/straits.h \
  /usr/include/g++-3/cctype /usr/include/ctype.h /usr/include/endian.h \
  /usr/include/bits/endian.h /usr/include/g++-3/cstring \
  /usr/include/g++-3/alloc.h /usr/include/g++-3/stl_config.h \
  /usr/include/g++-3/stl_alloc.h /usr/include/stdlib.h \
  /usr/include/sys/types.h /usr/include/time.h /usr/include/sys/select.h \
  /usr/include/bits/select.h /usr/include/bits/sigset.h \
  /usr/include/bits/time.h /usr/include/sys/sysmacros.h \
  /usr/include/alloca.h /usr/include/assert.h /usr/include/pthread.h \
  /usr/include/sched.h /usr/include/signal.h /usr/include/bits/initspin.h \
  /usr/include/bits/sigthread.h /usr/include/g++-3/iterator \
  /usr/include/g++-3/stl_relops.h /usr/include/g++-3/stl_iterator.h \
  /usr/include/g++-3/std/bastring.cc udunits.hh \
  /usr/local/include/udunits.h
outVar.o: outVar.cc /usr/include/stdio.h /usr/include/features.h \
  /usr/include/sys/cdefs.h /usr/include/gnu/stubs.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h \
  /usr/include/bits/types.h /usr/include/bits/pthreadtypes.h \
  /usr/include/bits/sched.h /usr/include/libio.h /usr/include/_G_config.h \
  /usr/include/wchar.h /usr/include/bits/wchar.h /usr/include/gconv.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stdarg.h \
  /usr/include/bits/stdio_lim.h outVar.hh /usr/include/g++-3/string \
  /usr/include/g++-3/std/bastring.h /usr/include/g++-3/cstddef \
  /usr/include/g++-3/std/straits.h /usr/include/g++-3/cctype \
  /usr/include/ctype.h /usr/include/endian.h /usr/include/bits/endian.h \
  /usr/include/g++-3/cstring /usr/include/string.h \
  /usr/include/g++-3/alloc.h /usr/include/g++-3/stl_config.h \
  /usr/include/g++-3/stl_alloc.h /usr/include/g++-3/iostream.h \
  /usr/include/g++-3/streambuf.h /usr/include/stdlib.h \
  /usr/include/sys/types.h /usr/include/time.h /usr/include/sys/select.h \
  /usr/include/bits/select.h /usr/include/bits/sigset.h \
  /usr/include/bits/time.h /usr/include/sys/sysmacros.h \
  /usr/include/alloca.h /usr/include/assert.h /usr/include/pthread.h \
  /usr/include/sched.h /usr/include/signal.h /usr/include/bits/initspin.h \
  /usr/include/bits/sigthread.h /usr/include/g++-3/iterator \
  /usr/include/g++-3/stl_relops.h /usr/include/g++-3/stl_iterator.h \
  /usr/include/g++-3/std/bastring.cc /usr/include/g++-3/vector \
  /usr/include/g++-3/stl_algobase.h /usr/include/g++-3/stl_pair.h \
  /usr/include/g++-3/type_traits.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/limits.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/syslimits.h \
  /usr/include/limits.h /usr/include/bits/posix1_lim.h \
  /usr/include/bits/local_lim.h /usr/include/linux/limits.h \
  /usr/include/bits/posix2_lim.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/new.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/new \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/exception \
  /usr/include/g++-3/stl_construct.h \
  /usr/include/g++-3/stl_uninitialized.h /usr/include/g++-3/stl_vector.h \
  /usr/include/g++-3/stl_bvector.h /usr/include/netcdf.hh \
  /usr/include/netcdfcpp.h /usr/include/netcdf.h /usr/include/errno.h \
  /usr/include/bits/errno.h /usr/include/linux/errno.h \
  /usr/include/asm/errno.h /usr/include/ncvalues.h \
  /usr/include/g++-3/strstream.h /usr/include/g++-3/strfile.h udunits.hh \
  /usr/local/include/udunits.h fslMesonetVar.hh
zebraMesonetOutFile.o: zebraMesonetOutFile.cc /usr/include/stdio.h \
  /usr/include/features.h /usr/include/sys/cdefs.h \
  /usr/include/gnu/stubs.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h \
  /usr/include/bits/types.h /usr/include/bits/pthreadtypes.h \
  /usr/include/bits/sched.h /usr/include/libio.h /usr/include/_G_config.h \
  /usr/include/wchar.h /usr/include/bits/wchar.h /usr/include/gconv.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stdarg.h \
  /usr/include/bits/stdio_lim.h zebraMesonetOutFile.hh \
  /usr/include/netcdf.hh /usr/include/netcdfcpp.h /usr/include/netcdf.h \
  /usr/include/errno.h /usr/include/bits/errno.h \
  /usr/include/linux/errno.h /usr/include/asm/errno.h \
  /usr/include/ncvalues.h /usr/include/g++-3/iostream.h \
  /usr/include/g++-3/streambuf.h /usr/include/g++-3/strstream.h \
  /usr/include/g++-3/strfile.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/limits.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/syslimits.h \
  /usr/include/limits.h /usr/include/bits/posix1_lim.h \
  /usr/include/bits/local_lim.h /usr/include/linux/limits.h \
  /usr/include/bits/posix2_lim.h /usr/include/string.h Station.hh \
  /usr/include/g++-3/map /usr/include/g++-3/stl_tree.h \
  /usr/include/g++-3/stl_algobase.h /usr/include/g++-3/stl_config.h \
  /usr/include/g++-3/stl_relops.h /usr/include/g++-3/stl_pair.h \
  /usr/include/g++-3/type_traits.h /usr/include/stdlib.h \
  /usr/include/sys/types.h /usr/include/time.h /usr/include/endian.h \
  /usr/include/bits/endian.h /usr/include/sys/select.h \
  /usr/include/bits/select.h /usr/include/bits/sigset.h \
  /usr/include/bits/time.h /usr/include/sys/sysmacros.h \
  /usr/include/alloca.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/new.h \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/new \
  /usr/lib/gcc-lib/i386-redhat-linux/2.96/include/exception \
  /usr/include/g++-3/stl_iterator.h /usr/include/g++-3/stl_alloc.h \
  /usr/include/assert.h /usr/include/pthread.h /usr/include/sched.h \
  /usr/include/signal.h /usr/include/bits/initspin.h \
  /usr/include/bits/sigthread.h /usr/include/g++-3/stl_construct.h \
  /usr/include/g++-3/stl_function.h /usr/include/g++-3/stl_map.h \
  /usr/include/g++-3/stl_multimap.h /usr/include/g++-3/string \
  /usr/include/g++-3/std/bastring.h /usr/include/g++-3/cstddef \
  /usr/include/g++-3/std/straits.h /usr/include/g++-3/cctype \
  /usr/include/ctype.h /usr/include/g++-3/cstring \
  /usr/include/g++-3/alloc.h /usr/include/g++-3/iterator \
  /usr/include/g++-3/std/bastring.cc outVar.hh /usr/include/g++-3/vector \
  /usr/include/g++-3/stl_uninitialized.h /usr/include/g++-3/stl_vector.h \
  /usr/include/g++-3/stl_bvector.h udunits.hh \
  /usr/local/include/udunits.h fslMesonetVar.hh
