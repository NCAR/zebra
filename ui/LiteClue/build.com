$! Build LiteClue help system on VMS VAX or Alpha
$! Assumes VMS 5.5-2 onwards (tested on 5.5-2, 6.2 and 7.0)
$! using DEC C 5.3 onwards.
$!
$! Phil Ottewell <phil@yrl.co.uk>, <phil@pottsoft.demon.co.uk>  16-Jan-1997
$!
$! VMS doesn't ship with Xaw, it comes with Motif instead, so to build the
$! LiteClueTest, the test program, you may link against the Xaw3D libraries
$! that come with Ghostview, available from
$! ftp://ada.cenaath.cena.dgac.fr/decwindows/ghostview-vms-276.zip or you
$! can easily modify the source to use Motif. If you don't do either,
$! LiteClue will still compile and you can link against the object file,
$! but you won't be able to run the test program.
$!
$ is_alpha = F$GETSYI("NODE_HWTYPE").EQS."ALPH"
$ IF is_alpha
$ THEN
$   obj = "OBJ_ALPHA"
$   exe = "EXE_ALPHA"
$ ELSE
$   obj = "OBJ"
$   exe = "EXE"
$ ENDIF
$ CC/PREFIX=ALL/OBJECT=.'obj' LITECLUE
$ WRITE SYS$OUTPUT "Created LITECLUE.''obj'"
$!
$! The test program, on VMS, needs the Xaw3D package that comes with Ghostview
$ @PDS$DISK:[PDS.GHOSTVIEW.COMMAND]SETUP COMPILE
$!
$ CC/PREFIX=ALL/OBJECT=.'obj' LITECLUETEST
$ LINK/EXE=.'exe' LITECLUETEST.'obj',LITECLUE,X11_ROOT:[EXE_ALPHA]XAW3D_CLIENT.OPT/OPT
$ WRITE SYS$OUTPUT "Created LITECLUETEST.''exe'"
