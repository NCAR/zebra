AUTOMAKE_OPTIONS = foreign

#
# If we have X, use LiteClue as well
#
if WITH_X
  SUBDIRS = LiteClue
  LITECLUEOBJ = LiteClue/LiteClue.o
endif

LDADD = ${LITECLUEOBJ} librdssui.a ../util/librdssutil.a

lib_LIBRARIES = librdssui.a

include_HEADERS = SmeMenu.h RdssMenu.h ui.h ui_symbol.h ui_param.h \
	ui_error.h ui_date.h ui_expr.h

bin_PROGRAMS = uib uic

libuidir = ${libdir}/rdssui
libui_DATA = ui_commands ui.lf

librdssui_a_SOURCES = \
	RdssMenu.c SimpleMenu.c SmeMenu.c ui_bootstrap.c ui.c ui_cmds.c \
	ui_cstack.c ui_ctable.c ui_date.c ui_error.c ui_expr.c \
	ui_function.c ui_globals.c ui_interrupt.c ui_keys.c ui_menu.c \
	ui_OutHook.c ui_output.c ui_parser.c ui_prompt.c ui_state.c \
	ui_string.c ui_symbol.c ui_token.c ui_tty.c ui_types.c ui_util.c \
	ui_wAppl.c ui_wForm.c ui_window.c ui_wList.c ui_wPulldown.c \
	\
	RdssMenu.h RdssMenuP.h SmeMenu.h SmeMenuP.h submenu.h \
	ui_commands.h ui_cstack.h ui_date.h ui_error.h ui_expr.h \
	ui_globals.h ui.h ui_loadfile.h ui_menu.h ui_mode.h ui_param.h \
	ui_state.h ui_symbol.h ui_token.h ui_tty.h ui_window.h

librdssui_a_LIBADD = ${LITECLUEOBJ}

uib_SOURCES = uib.c

uic_SOURCES = uic.c

DEFS = @DEFS@ -DSMEMENU -DUIDIR=\"${libuidir}\" -DLF=\"${libuidir}/ui.lf\"

EXTRA_DIST = ui.state ui_commands

CLEANFILES = ui.lf

#
# UI loadfile
#
ui.lf: ui.state uib
	$(RM) ui.lf
	@echo "Making $@"
	@echo read ui.state\; set 'ui$pager_mode' false\; save ui.lf all | \
		./uib
