AUTOMAKE_OPTIONS = foreign

LDADD = librdssgraph.a ../ui/librdssui.a ../util/librdssutil.a
librdssgraph_a_SOURCES = \
	clip.c color.c control.c device.c dev_null.c \
	dev_ps.c dev_psc.c dev_x11.c image.c messages.c oplist.c \
	overlay.c pixel.c pixel_font_0.c sfont_0.c sfont_1.c \
	text.c util.c \
	\
	color.h device.h graphics.h image.h oplist.h overlay.h \
	param.h pixel.h pxl.h tty.h workstation.h

aspect_SOURCES = aspect.c

gt_SOURCES = gt.c

ov_test_SOURCES = ov_test.c

vpt_SOURCES = vpt.c

DEFS = -DCOLORDB=\"${rdssgraphdir}/rgb.txt\"

#
# Only generate targets if we have X11
#
if WITH_X
  noinst_PROGRAMS = aspect gt ov_test vpt

  lib_LIBRARIES = librdssgraph.a 

  rdssgraphdir = ${libdir}/rdssgraph
  rdssgraph_DATA = rgb.txt

  include_HEADERS = graphics.h
endif

EXTRA_DIST = rgb.txt
