C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 KSYS.FOR 31-Jul-90,10:43:44,`RCD' initial release on OS/2 McIDAS
C 2 KSYS.FOR 15-Aug-91,11:50:38,`SUEG' moved to common
C 3 KSYS.FOR 15-Aug-91,11:50:38,`USER' Released
C 4 KSYS.FOR 13-Jul-93,8:53:36,`TOMW' Modified to work across systems (3917)
C 5 KSYS.FOR 23-Jul-93,8:20:32,`USER' Released
C 6 KSYS.FOR 1-Apr-94,1:20:00,`BARRYR' Add proprietary statement
C 7 KSYS.FOR 2-May-94,16:53:54,`USER' Released
C 8 KSYS.FOR 22-Mar-96,11:50:46,`DWS' reglue: modified file
C 9 KSYS.FOR 25-Mar-96,13:54:06,`USER' Released
C 10 KSYS.FOR 10-Jun-96,9:39:08,`BILLL' Added programmer documentation
C      (6653).
C 11 KSYS.FOR 6-Sep-96,10:17:44,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      ksys - Fetches a word from system key table.
*$
*$ Interface:
*$      integer function
*$      ksys(integer index)
*$
*$ Input:
*$      index - Points to position in table of word to be retrieved.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       The value of designated word in the SYSKEY table.
*$
*$ Remarks:
*$      Checks to see if SYSKEY is initialized, and whether bytes
*$      should be flipped.
*$
*$ Categories: 
*$      system 
*$      sys_config 


      INTEGER FUNCTION KSYS(INDEX)
      IMPLICIT INTEGER (A-B,D-Z)
      IMPLICIT CHARACTER*12 (C)
      Include 'hex80.inc'
      DATA CFILE/'SYSKEY.TAB'/
c Read word 0 (defined as a date) and use it to determine byte ordering
      ival=lwi(cfile,0,1,test)
      if (ival.lt.0 .or. test.eq.hex80) then     ! <<<<< UPC mod 961219 >>>>>
          call edest('ERROR: SYSKEY.TAB file not found.',0)
          ksys=hex80
          return
      endif

c Now read the target word
      ival=lwi(cfile,index,1,Temp)

c If the test value is NOT a reasonable date AND is not a character
c   string, THEN flip the bytes...

      if ( (test.lt.0.or.test.gt.999999) .and.(ischar(Temp).eq.0) ) then
         call fbyte4(Temp,1)
      endif

      ksys=Temp
      RETURN
      END

