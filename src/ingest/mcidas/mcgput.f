C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 MCGPUT.FOR 18-Sep-95,14:15:44,`SUEG' initial release (5671)
C 2 MCGPUT.FOR 19-Sep-95,12:58:46,`SUEG' add version keyword
C 3 MCGPUT.FOR 20-Oct-95,16:48:14,`USER' Released
C 4 MCGPUT.FOR 10-Nov-95,14:19:20,`BETHA' updated to standard (5759)
C 5 MCGPUT.FOR 16-Nov-95,10:01:30,`USER' Released
C 6 MCGPUT.FOR 28-Aug-96,19:51:10,`BILLL' Added programmer documentation
C      (6869).
C 7 MCGPUT.FOR 23-Sep-96,12:40:46,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      mcgput - Writes grids to a grid file.
*$
*$ Interface:
*$      integer function
*$      mcgput(string name, int nsort, string sort(*), int msgflg,
*$             int numbytes)
*$
*$ Input:
*$      name         - DDE truename, nickname, or GRID file number.
*$      nsort        - Number of sort clauses.
*$      sort         - Array of clauses defining the request.
*$      msgflg       - 0 means do not output error messages.
*$      numbytes     - Number of bytes being output.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       0           - Success.
*$      <0           - Failure.       
*$  
*$ Remarks:
*$      The name in the dataset of gridfile to which data will be 
*$      written.  The sort clauses determine the structure of the gridfile
*$      to be written.  Possible clauses are limited to:
*$           GRID    -  Grid number at which to start writing.
*$           MAX     -  Maximum number of grids that can be written 
*$                      to a gridfile.
*$                      This sort is valid only if a gridfile is being 
*$                      deleted.
*$           APPEND  -  YES ...append the grids to the gridfile
*$           DEL     -  YES/NO...delete the gridfile before writing to it
*$           NUM     -  Number of grids to write
*$           LABEL   -  Character string for gridfile label, if gridfile 
*$                      is created.
*$
*$ Categories: 
*$      grid
*$

      INTEGER FUNCTION MCGput(NAME,NSORT,SORT,MSGFLG,numbytes)
      implicit none
      CHARACTER*(*) NAME                !name of dataset to write to 
      integer nsort                     !number of sorts in sort array
      CHARACTER*(*) SORT(*)             !array containing sort conditions
      integer msgflg                    !flag to turn on messaging
      integer numbytes                  !total number of bytes to write

C --- symbolic constants & shared data

      INCLUDE 'hex80.inc'

C**** Common block which contains read/write handles for this transaction

      integer irh                       !read handle from common block
      integer iwh                       !write handle from common block
      common/c1hndl/irh, iwh

C --- external functions

      integer iftok                     !get an integer token
      integer len_trim                  !get length of a string
      integer m0cxout                   !fire up server write request
      integer m0cxread                  !read from server
      integer m0cxreq                   !fire up a server request
      integer m0cxwrit                  !write to server
      integer mcgoutc                   !write a grid & header for C
      integer mcgoutf                   !write a grid& header for fortran
      integer m0split

C --- local variables

      character*100 append              !flag to append grid to file
      character*100 cname               !name of dataset
      character*100 delete              !flag to delete gridfile before writing
      character*100 grid                        !destination grid to write to
      character*100 label               !label to write to grid file
      character*100 maxgrd              !maximum # of grids in dest grid file
      character*100 numgrid             !numgrid number of grids being written
      character*100 pos                 !position of destination grid file
      character*500 cstr                !string containing sorts keywords
      character*500 cstr1               !string containing sorts keywords
      character*500 cstr2               !string containing sorts keywords
      character*500 reqst               !string of the request for server
      integer cstrlen                   !length of cstring
      integer grdhed(*)                 !Grid to write (for mcgout*)
      integer icx
      integer idum
      integer ig(*)                     !Grid header to write (for mcgout*)
      integer indx
      integer iret                      !return status 
      integer ja                        !just an index
      integer jb
      integer nb                        
      integer nc                        !number of columns
      integer nr                        !number of rows
      integer num_wrote                 !number of bytes written
      integer read_handle               !read handle for this transaction
      integer rc
      integer totgrids                  !total # of grids
      integer write_handle              !write handle for this transaction

      integer ival                      
      real val       
      EQUIVALENCE(IVAL,VAL )

      integer k
      character*4 token                 !token from gtokserv
      equivalence(k, token)

      cstr=' '
      reqst=' '

C------Figure out the truename
      rc=m0split(name,cname) 
      if (rc .lt. 0) then
	 iret=-500
	 goto 10
      endif

C**** Initialize sort keywords 
      grid=' '
      pos=' '
      maxgrd=' '
      append=' '
      numgrid=' '
      label=' '
 

C**** Make sure the request to the server will go through
      iret=m0cxreq('GTOK',cname//'VERSION=1',0,0,nb)
      if (iret .ne.0) then
		call m0cxerms
		goto 10
      endif
C**** Read the response
      iret=m0cxread(4,k)
      if (iret .ne. 0) goto 10
      iret=m0cxread(4,idum)
      if (iret .ne. 0) goto 10
      call m0cxfin

C
C-------Pick up sort conditions
      DO 8 JA=1,NSORT
      IF(SORT(JA)(1:4).EQ.'GRID') THEN
	  GRID='GRID='//SORT(JA)(6:)
      elseif(sort(ja)(1:3).eq.'POS') then
	  pos='POS='//sort(ja)(5:)
      elseif(sort(ja)(1:3) .eq. 'MAX') then
	  maxgrd='MAX='//sort(ja)(5:)
      elseif(sort(ja)(1:3) .eq. 'APP') then
	  append='APP='//sort(ja)(5:)
      elseif(sort(ja)(1:3) .eq. 'DEL') then
	  delete='DEL='//sort(ja)(5:)
      elseif(sort(ja)(1:3) .eq. 'NUM') then
	  numgrid='NUM='//sort(ja)(5:)
	  totgrids=iftok(sort(ja)(5:))
      elseif(sort(ja)(1:5) .eq. 'LABEL') then
	  label='LABEL='//"'"//sort(ja)(7:len_trim(sort(ja))+1)//"'"
      ELSE
	  IRET=-18
	  CSTR=SORT(JA)
	  cstrlen = len_trim(cstr)
	  if (msgflg .ne. 0) then
	     CALL EDEST('MCGPUT invalid sort condition;'
     &                   //CSTR(1:cstrlen),0)
	  endif
	  GOTO 10
      ENDIF
  8   CONTINUE

C**** make a string containing all the sort keywords

      cstr1 = grid//pos//maxgrd//append//'VERSION=1' 
      cstr2 = numgrid//label//delete//'TOKEN='//token

      call bsquez(cstr1)
      call bsquez(cstr2)

      CSTR=cstr1(1:len_trim(cstr1)+1)//cstr2

      call bsquez(cstr)

C**** prefix the string with the dataset name
      REQST=CNAME(1:LEN_TRIM(CNAME)+1)//CSTR  
      CALL BSQUEZ(REQST)
      cstrlen = len_trim(REQST)

      CALL DDEST('GPUT '//REQST(1:cstrlen),0)

C-------Call gput service
      IRET=M0CXout('GPUT',REQST,numbytes)
      IF(IRET.NE.0) THEN
	  IF(MSGFLG.NE.0) CALL M0CXERMS
	  GOTO 10  
      ENDIF

C**** save read/write handles for later use
      read_handle=irh
      write_handle=iwh
      num_wrote=0
      
      CALL DDEST('Transferring grid, number of bytes=',NUMBYTes)
 10   MCGput=IRET
      RETURN


*$ Name:
*$      mcgoutf - Outputs a grid in column-major order (fortran).
*$
*$ Interface:
*$      integer function
*$      mcgoutf(integer ig(*), integer grdhed(*))
*$
*$ Input:
*$      ig      - Array which will receive the grid.
*$      grdhed  - Array which will receive the grid header.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       0      - Success.
*$      <0      - Failure.
*$
*$ Remarks:
*$      This writes a 2-d array in column-major order (FORTRAN).
*$      Missing values in fixed point are returned as 'hex80'.
*$      Missing values in floating point are returned as greater than 1.0e30.
*$
*$      The values of the grid will be converted to the units specified
*$      in the mcgput call.  If a grid is being derived, all of the 
*$      component grids will be read, and the resultant grid will be 
*$      computed.
*$
*$ Categories:
*$      grid


      ENTRY MCGoutF(IG, GRDHED)

C**** Set the read/write handles to those set in mcgput
      irh=read_handle
      iwh=write_handle

      nr=grdhed(2)
      nc=grdhed(3)
C-------Swap integer values in grid header if not character data
      call m0flpgdr(grdhed)
      IRET=M0CXwrit(256,grdhed)

C**** calculate size of grid to send 
      ICX=0

C -   Do in batches to overlap io with byte swapping
      DO 2 JA=1,NR
	 CALL SWBYT4(IG(1+(JA-1)*NC),NC)
	 IRET=M0CXwrit(NC*4,IG(1+(JA-1)*NC))
	 IF(IRET.NE.0) GOTO 20
  2   continue 
C
      
C**** write out the trailer  word
      IRET=M0CXwrit(4,IDUM)
      num_wrote=num_wrote+1
      if (num_wrote .ge. totgrids) then
		iret=m0cxread(4,idum)
		call m0cxfin
      endif
      MCGoutf=0
      IF(IRET.EQ.0) GOTO 30
C
 20   MCGoutf=IRET
      GOTO 200

*$ Name:
*$      mcgoutc - Outputs a grid in row-major order (C).
*$
*$ Interface:
*$      integer function
*$      mcgoutc(integer ig(*), integer grdhed(*))
*$
*$ Input:
*$      ig      - Array which will recieve the grid.
*$      grdhed  - Array which will recieve the grid header.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$       0      - Success.
*$      <0      - Failure.
*$
*$ Remarks:
*$      This writes a 2-d array in C ordering.
*$      Missing values in fixed point are returned as 'hex80'
*$      Missing values in floating point are returned as greater than 1.0e30
*$
*$      The values of the grid will be converted to the units specified
*$      in the mcgget call.  If a grid is being derived, all of the 
*$      component grids will be read, and the resultant grid will be 
*$      computed.
*$
*$ Categories:
*$      grid

      ENTRY MCGoutc(IG, GRDHED)

C**** set the read/write handle to those saved in mcgput
      irh=read_handle
      iwh=write_handle

C-------Swap integer values in grid header if not character data
      nr=grdhed(2)
      nc=grdhed(3)
      call m0flpgdr(grdhed)

C**** send the grid header
      IRET=M0CXwrit(256,grdhed)


C**** write out the grid from a C ordered grid
      ICX=1
      DO 6 JA=1,NC
	 DO 6 JB=1,NR
	    INDX=NR*(JA-1)+NC
	    CALL SWBYT4(IG(INDX),1)
	    IRET=M0CXwrit(4,IG(INDX))
	    IF(IRET.NE.0) GOTO 21
  6   continue
C
      IRET=M0CXwrit(4,IDUM)
      MCgoutc=0
      num_wrote=num_wrote+1
      if (num_wrote .ge. totgrids) then
		iret=m0cxread(4,idum)
		call m0cxfin
      endif
      IF(IRET.EQ.0) GOTO 30
C
 21   MCgoutc=IRET
      GOTO 200
C
C+++++++++++++++Common code, after recieving grid in one of two orderings
30    continue
200   RETURN
      END
