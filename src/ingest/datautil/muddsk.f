c------------------------------------------------------------------------------

      PROGRAM MUDDSK
C
C   THIS PROGRAM COPIES MUDRAS FORMAT CARTESIAN VOLUMES FROM TAPE TO DISK
C
c***      PARAMETER NMD=510,NML=10,MAXLEN=3200,MXFLD=25,MSLOP=4
      PARAMETER (NMD=510,NML=10,MAXLEN=3200,MXFLD=25,MSLOP=4) 
c***      PARAMETER MAXRD=MAXLEN+MSLOP,MAXHD=NMD+MSLOP
      PARAMETER (MAXRD=MAXLEN+MSLOP,MAXHD=NMD+MSLOP)
      COMMON /MUDBLK/ IDMUD(NMD),IDMLV(NML)
      DIMENSION IBUF(MAXRD)
      DIMENSION LOCFLD(MXFLD)
      CHARACTER NAMOPN*40,IDEF*40,IBL*1
      INTEGER*2 IDMUD,IDMLV,IBUF,LOCFLD
      LOGICAL YES,NO
      DATA IBL,IDEF,ITYP / ' ',' ',3 /
      DATA IN,IOUT,IPR,ITM /11,12,7,33 /
      DATA ICC /' '/
C
C
c***      OPEN(UNIT=IPR,NAME='MUDDSK.PRI',STATUS='NEW', 
c***     +     CARRIAGECONTROL='FORTRAN') 

      OPEN(UNIT=IPR,FILE='MUDDSK.PRI',STATUS='NEW')

      CALL IOPEN(IN,'TAPE.MUD')

c***      OPEN(UNIT=ITM,NAME='TT',STATUS='UNKNOWN') 

      OPEN(UNIT=ITM,FILE='TT',STATUS='UNKNOWN')

 10   CONTINUE
c***      TYPE 110
      WRITE(6,110)
 110  FORMAT('NO. OF VOLUMES TO SKIP ON INPUT TAPE:  ')
      NVS=TTINP(0)
      NVS1=NVS+1
c***      TYPE 111, NVS,NVS1
      WRITE (6,111) NVS,NVS1
 111  FORMAT('$SKIP ',I2,' VOLUMES AND COPY VOLUME ',I2,'.  OK?  ')
      IF (NO(0)) GO TO 10
 20   CONTINUE

      CALL SKIPFL(IN,NVS)

      CALL INPUTD(IN,IBUF,MAXHD,NWDS,NST)

      IF (NST .NE. 0) GO TO 82
      DO 21 I=1,NMD
         IDMUD(I)=IBUF(I)
 21   CONTINUE
      NREC=IDMUD(96)
      NRPP=IDMUD(97)
      MX=IDMUD(98)+1
      IDMUD(100)=MX
      NLVL=IDMUD(106)
      NFLD=IDMUD(175)
      NPTS=IDMUD(301)
      NLND=IDMUD(302)+IDMUD(303)+1
      IF (NLND .GT. 7) NLND=7
      NRPF=NREC*NLVL
      IDMUD(400)=NRPF
      IF (NPTS .GE. MAXLEN) NSIZ=MAXLEN
      IF (NPTS .LT. MAXLEN) NSIZ=MAX0(NPTS,NMD)
      DO 25 L=1,NFLD
      LOCFLD(L)=NRPF*(L-1)+2
      IDMUD(400+L)=LOCFLD(L)
 25   CONTINUE
      CALL SWAP(IDMUD(1),20)
      CALL SWAP(IDMUD(43),16)
      CALL SWAP(IDMUD(62),1)
      CALL SWAP(IDMUD(66),1)
      CALL SWAP(IDMUD(71),24)
      CALL SWAP(IDMUD(101),4)
      CALL SWAP(IDMUD(151),1)
      K=171
      DO 26 KK=1,NFLD
      K=K+5
      CALL SWAP(IDMUD(K),4)
 26   CONTINUE
      K=300
      DO 28 KK=1,NLND
      K=K+6
      CALL SWAP(IDMUD(K),3)
 28   CONTINUE
      IYES=1HY
      CALL IMHSUM(ITM,ICC,IYES)
c***      TYPE 104
      WRITE (6,104)
 104  FORMAT ('$COPY THIS VOLUME? ')
      IF (NO(0)) GOTO 170
      CALL IMHSUM(IPR,ICC,IYES)
c***      ENCODE (8,220,IDEF)(IDMUD(I),I=101,104)
      WRITE (IDEF,220) (IDMUD(I),I=101,104)
 220  FORMAT (4A2)
      CALL NAMDFL(NAMOPN,IDEF,ITYP)
c***      OPEN(UNIT=IOUT,NAME=NAMOPN,STATUS='NEW',ACCESS='DIRECT', 
c***     X     RECORDSIZE=(NSIZ+1)/2,MAXREC=MX,ERR=80) 
      OPEN(UNIT=IOUT,FILE=NAMOPN,STATUS='NEW',ACCESS='DIRECT',
     X     RECL=(NSIZ+1)/2,ERR=80)
c***      TYPE 222
      WRITE (6,222)
 222  FORMAT(/' BEGIN COPYING VOLUME...')
      LREC=1
      CALL WRRAND(IOUT,LREC,IDMUD,NMD,IERR)
      IF (IERR .NE. 0) GO TO 81
C
C
      DO 39 KLVL=1,NLVL
      CALL SKIPRC(IN,1)
      DO 39 L=1,NFLD
      DO 39 M=1,NREC
      CALL INPUTD(IN,IBUF,MAXRD,NWDS,NST)
      IF (NST .NE. 0) GO TO 82
      IF (NPTS .LE. NSIZ) LENG=NPTS
      IF (NPTS .GT. NSIZ) LENG=MIN0(NSIZ,NPTS-(M-1)*NSIZ)
      LREC=LOCFLD(L)+(NREC*(KLVL-1))+(M-1)
      CALL WRRAND(IOUT,LREC,IBUF,LENG,IERR)
      IF (IERR .NE. 0) GO TO 81
 39   CONTINUE
c***      TYPE 105
      WRITE (6,105)
 105  FORMAT(/' VOLUME COPYING IS COMPLETED.'/)
 170  CONTINUE
c***      CLOSE(UNIT=IOUT,DISPOSE='KEEP',ERR=83) 
      CLOSE(UNIT=IOUT,ERR=83)
      CALL SKIPFL(IN,1)
c***      TYPE 106
      WRITE (6,106)
 106  FORMAT ('$DO YOU WISH TO TRANSFER ANY MORE VOLUMES? ')
      IF (YES(0)) GOTO 10
 55   CONTINUE
c***      TYPE 107
      WRITE (6,107)
  107 FORMAT(/4X,'VOLUME HEADER SUMMARIES HAVE BEEN WRITTEN',
     X           ' TO MUDDSK.PRI'/)
      CALL IOCLOS(IN)
      CLOSE(UNIT=IPR)
      STOP
C
C
 80   CONTINUE
c***      TYPE 180, NAMOPN
      WRITE (6,180) NAMOPN
 180  FORMAT(' ERROR OCCURRED WHILE OPENING FILE ',A12)
      GO TO 55
 81   CONTINUE
c***      TYPE 181, NAMOPN
      WRITE (6,181) NAMOPN
 181  FORMAT(' ERROR OCCURRED WHILE WRITING FILE ',A12)
      GO TO 55
 82   CONTINUE
c***      TYPE 182
      WRITE (6,182)
 182  FORMAT(' ERROR OCCURRED WHILE READING TAPE')
      CALL TAPMES(IOUT,NST)
      GO TO 55
 83   CONTINUE
c***      TYPE 183, NAMOPN
      WRITE (6,183) NAMOPN
 183  FORMAT(' ERROR OCCURRED WHILE CLOSING FILE ',A12)
      STOP
      END

c------------------------------------------------------------------------------

      SUBROUTINE INPUTD(ITAPE,IBUF,NMAX,NWDS,NST)
C
C
C         INPUTD- READS A PHYSICAL RECORD FROM A NOAA TIME SERIES TAPE
C         INTO THE NCAR/7600
C
C         ITAPE- LOGICAL UNIT NUMBER OF THE INPUT TAPE
C         IBUF- INPUT ARRAY
C         NMAX- MAX NUMBER OF WORDS TO READ
C         NWDS- ACTUAL NUMBER OF WORDS READ
C         NST- STATUS OF TAPE READ:
C                   0- GOOD READ
C                   1- END OF FILE
C                   2- PARITY ERROR
C                   3- END OF TAPE
C
C         ITAPE  AND  NMAX  ARE INPUT PARAMETERS, ALL OTHERS ARE OUTPUT
C
C
      INTEGER*2 IBUF(1)

      CALL RDTAPE(ITAPE,1,2,IBUF,NMAX*2)
      CALL IOWAIT(ITAPE,NST,NBYTES)

      IF(NST.NE.0) RETURN
      NWDS=(NBYTES+1)/2
C
C        SWAP IS CALLED HERE ONLY IF TAPE HAS BEEN WRITTEN ON
C             ANOTHER SYSTEM.
C
      CALL SWAP(IBUF,NWDS)
C
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE NAMDFL(INPSAV,IDEF,ITYP)
C
C        PRODUCES A VALID USER SUPPLIED FILE NAME
C           INPSAV- RESULTANT FILE NAME
C           IDEF-   DEFAULT NAME
C           ITYP- 1, INPUT FILE
C                 2, EDIT FILE
C                 3, OUTPUT FILE
C
      CHARACTER*40 INP,INPSAV,IDEF,LMENTS(6)
      CHARACTER NTYP(3)*6,PEXT*4,IBL*1,ISC*1
      LOGICAL YES,NO
      DATA NTYP/' INPUT','  EDIT','OUTPUT'/
      DATA PEXT,IBL,ISC/'.MUD',' ',';'/
   10 CONTINUE
C
C        SOLICIT A FIELD NAME
C
c***      TYPE 101, NTYP(ITYP)
      WRITE (6,101) NTYP(ITYP)
  101 FORMAT(/'$',A6,' FILE NAME? ')
c***      ACCEPT 201, INP
      READ 201, INP
  201 FORMAT(A)
      IF(INP.EQ.IBL) THEN
C    NULL USER RESPONSE
         IF(IDEF.EQ.IBL) GO TO 10
         INP=IDEF
         IF(ITYP.EQ.3) THEN
C       CHECK DEFAULT NAME
            ICHK=NFPARS(IDEF,LMENTS)
            IF(ICHK.NE.0) GO TO 10
            INP=LMENTS(4)
            K=INDEX(INP,IBL)
            INP(K:K+3)=PEXT
         END IF
      ELSE
C    CHECK USER SUPPLIED NAME
         ICHK=NFPARS(INP,LMENTS)
         IF(ICHK.NE.0) GO TO 10
         IF(LMENTS(5).EQ.IBL) THEN
C       ADD (.MUD) EXTENSION IF NOT PRESENT
            K=INDEX(INP,IBL)
            INP(K:K+3)=PEXT
         ELSE IF(ITYP.EQ.3) THEN
C       DO NOT PERMIT VERSION NUMBER ON OUTPUT FILE
            K=INDEX(INP,ISC)
            IF(K.NE.0) INP=INP(:K-1)
         END IF
      END IF
C
C        FINISHED MESSAGING FIELD NAME
C
c***      TYPE 102, NTYP(ITYP),INP
      WRITE (6,102) NTYP(ITYP),INP
  102 FORMAT(' ',A6,' FILE NAME= ',A/'$OK? ')
      IF(NO(0)) GO TO 10
      INPSAV=INP
      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION NFPARS(NAME,LMENTS)
C
C        PARSES A POTENTIAL FILE NAME
C          LMENTS(1)- NODE
C                (2)- DEVICE
C                (3)- DIRECTORY
C                (4)- FILE NAME
C                (5)- EXTENSION
C                (6)- VERSION
C        RETURNS 0, IF SUCCESSFUL
C                1, IF NOT.
C
      CHARACTER NAME*40,LMENTS(6)*40,LB(2)*1,RB(2)*1
      CHARACTER DCL*2,CL*1,P*1,SC*1,BLANK*1
      DATA DCL, CL, LB(1), LB(2), RB(1), RB(2),  P,  SC, BLANK/
     X    '::',':',   '[',   '<',   ']',   '>','.', ';',  ' ' /
      DO 10 I=1,6
      LMENTS(I)=BLANK
   10 CONTINUE
      L=INDEX(NAME,BLANK)-1
      IF(L.LE.0) GO TO 90
C
C        NODE NAME
C
      I=1
      J=INDEX(NAME(I:),DCL)
      IF(J.NE.0) THEN
         J=J+1
         LMENTS(1)=NAME(I:J)
         IF(J.GE.L) GO TO 90
         I=J+1
      END IF
C
C        DEVICE NAME
C
      J=INDEX(NAME(I:),CL)+I-1
      IF(J.GE.I) THEN
         LMENTS(2)=NAME(I:J)
         IF(J.GE.L) GO TO 90
         I=J+1
      END IF
C
C        DIRECTORY
C
      DO 20 M=1,2
      J=INDEX(NAME(I:),LB(M))+I-1
      IF(J.EQ.I) THEN
         K=INDEX(NAME(I:),RB(M))+I-1
         IF(K.EQ.0) GO TO 90
         LMENTS(3)=NAME(I:K)
         IF(K.GE.L) GO TO 90
         I=K+1
         GO TO 25
      END IF
   20 CONTINUE
   25 CONTINUE
C
C        FILE NAME, EXTENSION AND VERSION
C
      J=INDEX(NAME(I:),P)+I-1
      IF(J.LT.I) THEN
         IF(INDEX(NAME(I:),SC).NE.0) GO TO 90
         LMENTS(4)=NAME(I:L)
      ELSE
         LMENTS(4)=NAME(I:J-1)
         I=J
         J=INDEX(NAME(I:),SC)+I-1
         IF(J.LT.I) THEN
            LMENTS(5)=NAME(I:L)
         ELSE
            LMENTS(5)=NAME(I:J-1)
            IF(J.GE.L) GO TO 90
            LMENTS(6)=NAME(J:L)
         END IF
      END IF
      NFPARS=0
      RETURN
   90 CONTINUE
C
C        ERROR RETURN, IMPROPER SYNTAX FOR FILE NAME
C
      NFPARS=1
c***      TYPE 101, NAME
      WRITE (6,101) NAME
  101 FORMAT(5X,'+++  UNABLE TO PARSE ',A,'  +++')
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE WRRAND(IT,NREC,IBUF,NWDS,NST)
C 
C        WRITES AND READS DATA ACCORDING TO UNIT AND RECORD NUMBER
C 
      INTEGER*2 IBUF(NWDS)
C
C        WRITE A RECORD
C
c***      WRITE(IT'NREC,ERR=401) IBUF 
      WRITE(IT,NREC,ERR=401) IBUF
      NST=0
      RETURN
C
C        READ A RECORD
C
      ENTRY RDRAND(IT,NREC,IBUF,NWDS,NST)
      READ(IT,NREC,ERR=401) IBUF
      NST=0
      RETURN
  401 NST=2
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE TAPMES(ITAPE,NST)
C
C
C         TAPMES- PRINTS OUT THE APPROPRIATE STATUS MESSAGE ACCORDING TO THE
C                 VALUE IN NST
C
C         ITAPE- TAPE UNIT NUMBER
C         NST- NON-ZERO STATUS CHECK FROM SUBROUTINE INPUTD  (SAME AS RDTAPE)
C
C         ALL PARAMETERS ARE INPUT
C
C
C
      DATA IPR/6/
      CHARACTER*80 MESS(5)
      DATA MESS/'END OF FILE ENCOUNTERED',
     X          'PARITY ERROR DETECTED',
     X          'END OF TAPE ENCOUNTERED',
     X          'UNEXPECTED RECORD ORDER',
     X          'ACTUAL RECORD LENGTH DIFFERS FROM EXPECTED'   /
      WRITE(IPR,101) ITAPE,NST, MESS(NST) 
  101 FORMAT(///1X,'UNIT NO. ',I2,5X,'STATUS= ',I2/5X,A)   
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE IMHSUM(IDEV,ICC,IYES)
C
C        GENERATES A SUMMARY OF A MUDRAS 16-BIT VOLUME HEADER
C                                 IN CARTESIAN COORDINATES.
C
      PARAMETER (NMD=510, NML=10)
      COMMON /MUDBLK/ ID(NMD), IDMLV(NML)
      INTEGER*2 ID,IDMLV
      COMMON /UNITS/ IN,IUCRT,ITMPFL,ISCRAM,MUD,ITT,IPR,IPLTR
      CHARACTER*1 ITAX(3)
      DATA ITAX/ 'X', 'Y', 'Z' /
      DATA ITT / 33 /
      IF(IYES.NE.'Y') RETURN
      WRITE(IDEV,100) 
  100 FORMAT(A1)
      IF(ID(1).NE.0) GO TO 10
      WRITE(IDEV,200) 
  200 FORMAT(/5X,'NO MUDRAS FILE EXISTS AT PRESENT'/)
      RETURN
   10 CONTINUE
      SF=1./ID(68)
      CF=1./ID(69)
      WRITE(IDEV,105) (ID(I),I=1,4),ID(117),ID(118),ID(116), 
     X   (ID(I),I=71,73),(ID(I),I=10,12),(ID(I),I=119,121),
     X   (ID(I),I=13,15),(ID(I),I=48,50),(ID(I),I=125,127),
     X   (ID(I),I=5,7),  (ID(I),I=51,54),(ID(I),I=101,104),
     X    ID(8),ID(9),(ID(I),I=55,58),(ID(I),I=16,20),(ID(I),I=45,47)
  105 FORMAT(//' MUDRAS (.MUD)  VOLUME HEADER'15X,4A2
     X   /'  GENERAL INFORMATION...'
     X/'   DATE:      'I2,2('/'I2),5X'SOURCE:  '3A2,5X'SCIENTIST: '3A2
     X/'   BEG TIME:  'I2,2(':'I2),5X'RADAR:   '3A2,5X'SUBMITTER: '3A2
     X/'   END TIME:  'I2,2(':'I2),5X'PROGRAM: '3A2,5X'DATE RUN:  '4A2
     X/'   VOL. NAME: ', 4A2,      5X'PROJECT: '2A2,7X'TIME RUN:  '4A2
     X/'   SCAN MODE: ', 2A2,      9X'TAPE:    '3A2,5X'SEQUENCE:  '3A2)
      WRITE(IDEV,106) ID(62),ID(96),ID(301),ID(63),ID(97),ID(106), 
     X                ID(65),ID(98),ID(67)
  106 FORMAT(/'  DATA CHARACTERISTICS...'
     X/'   COMPUTER:   ',2X,A2,5X'RECS/FIELD:  'I4,5X'PTS/FIELD:  'I6
     X/'   BITS/DATUM: ',I4,   5X'RECS/PLANE:  'I4,5X'NO. PLANES: 'I6
     X/'   BLOCK SIZE: ',I4,   5X'RECS/VOLUME: 'I4,5X'BAD DATA:   'I6)
      IF(IDEV.EQ.ITT) IQ=IPAUSE(0)
      N=ID(175)
      WRITE(IDEV,107) N 
  107 FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR')
      K2=175
      DO 15 I=1,N
      K1=K2+1
      K2=K2+5
      WRITE(IDEV,108) I,(ID(K), K=K1,K2) 
  108 FORMAT(4X,I3,3X,4A2,5X,I5)
   15 CONTINUE
      IF(N.GT.8.AND.IDEV.EQ.ITT) IQ=IPAUSE(0)
      N=ID(302)
      WRITE(IDEV,109) N,ID(303) 
  109 FORMAT(/'  LANDMARKS PRESENT: ',I2,5X,'(',I2,' RADAR) ...'
     X   /4X,'NO.',3X,'NAME',6X,'X (KM)',4X,'Y (KM)',4X,'Z (KM)')
      K=306
      DO 20 I=1,N
      R1=ID(K+3)*SF
      R2=ID(K+4)*SF
      R3=ID(K+5)*0.001
      WRITE(IDEV,110) I,ID(K),ID(K+1),ID(K+2), R1,R2,R3
  110 FORMAT(4X,I3,3X,3A2,2F10.2,F10.3)
      K=K+6
   20 CONTINUE
      R1=ID(35)*SF
      R2=ID(38)*SF
      WRITE(IDEV,111) ID(33),ID(34),R1,ID(36),ID(37),R2
  111 FORMAT(/'  ORIGIN  LATITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC'
     X       /9X,      'LONGITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC')
      IF(IDEV.EQ.ITT) IQ=IPAUSE(0)
      WRITE(IDEV,112)
  112 FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X/3X'AXIS'5X'MINIMUM (KM)'5X'MAXIMUM (KM)'6X'DELTA (KM)'4X,
     X'NO. OF PTS.')
      K=160
C        CONVERT METERS TO KM FOR Z-AXIS HEIGHTS
      CKM=1.0
      DO 25 I=1,3
      IF(I.EQ.3) CKM=0.1
      R1=ID(K)*SF*CKM
      R2=ID(K+1)*SF*CKM
      R3=ID(K+3)*0.001
      WRITE(IDEV,114) ITAX(I),R1,R2,R3,ID(K+2)
  114 FORMAT(5X,A1,6X,F10.2,7X,F10.2,8X,F8.2,7X,I5)
      K=K+5
   25 CONTINUE
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      R1=ID(40)*CF
      WRITE(IDEV,115) ITAX(L1),ITAX(L2),ITAX(L3),R1
  115 FORMAT(/3X,'AXIS ORDER IS   ',3A3
     X      //3X,'ANGLE OF X-AXIS REL. TO NORTH= ',F7.2)
      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION IPAUSE(N)
c***      TYPE 101
      WRITE (*,101)
  101 FORMAT('$(HIT RETURN TO CONTINUE)')
c***      ACCEPT 201,IRESP
      READ 201,IRESP
  201 FORMAT(A1)
      IPAUSE=0
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE SWAP(IBUF,N16)
      BYTE IBUF(2,N16), ISAV
      DO 10 I=1,N16
      ISAV=IBUF(1,I)
      IBUF(1,I)=IBUF(2,I)
      IBUF(2,I)=ISAV
   10 CONTINUE
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE SKIPFL(IUNIT,ISKIP)
C
C        TWO ENTRIES FOR THIS ROUTINE
C
C           SKIPFL - SKIPS FILES
C           SKIPRC - SKIPS RECORDS
C
      DIMENSION JUNK(6400)
      DATA NBKI,MODE,NTYPE / 6400,1,2 /
      DATA ZERO / 0.0 /
      IF (ISKIP) 100,300,200
 100  CONTINUE
      REWIND IUNIT
      RETURN
 200  CONTINUE
      KTREC=0
      DO 250 I=1,ISKIP
 220     CONTINUE
         CALL RDTAPE(IUNIT,MODE,NTYPE,JUNK,NBKI)
         CALL IOWAIT(IUNIT,NST,IWD)
         IF (NST.EQ.1) GOTO 230
         IF (NST.EQ.2) PRINT 221,KTREC+1,I 
 221     FORMAT (3X,'+++ PARITY ERROR IN RECORD ',I6,
     X      ' OF FILE ',I3,'  - SKIPPING CONTINUED')
         IF (NST.EQ.3) GOTO 900
         KTREC=KTREC+1
         GOTO 220
 230     CONTINUE
         KTREC=0
 250  CONTINUE
 300  CONTINUE
      RETURN
C
      ENTRY SKIPRC(IUNIT,ISKIP)
C
      IF (ISKIP) 400,300,500
 400  CONTINUE
      REWIND IUNIT
      RETURN
 500  CONTINUE
      DO 550 I=1,ISKIP
         CALL RDTAPE(IUNIT,MODE,NTYPE,JUNK,NBKI)
         CALL IOWAIT(IUNIT,NST,IWD)
         IF (NST.EQ.1 .OR. NST.EQ.3) GOTO 900
         IF (NST.EQ.2) PRINT 501,I 
 501     FORMAT (3X,'+++ PARITY ERROR IN RECORD ',I6,
     X      '  - SKIPPING CONTINUED')
 550  CONTINUE
      RETURN
 900  CONTINUE
      PRINT 901,NST 
 901  FORMAT (3X,'+++ STATUS ',I1,' ENCOUNTERED DURING SKIPPING')
      Z=36/ZERO
      STOP 711
      END

c------------------------------------------------------------------------------

      LOGICAL FUNCTION NO(N)
      DATA IY,ILY / 'Y','y' /
c***      ACCEPT 101, IYN
      READ 101, IYN
  101 FORMAT(A1)
      NO=.FALSE.
      IF(IYN.NE.IY .AND. IYN.NE.ILY) NO=.TRUE.
      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION TTINP(N)
C
C        PERMITS THE INPUT OF NUMERICAL VALUES WITH ERROR RECOVERY
C
      DATA ITT/5/
    1 CONTINUE
      READ (ITT,101,ERR=90) VAL
  101 FORMAT(F10.0)
      TTINP=VAL
      RETURN
C
C       ERROR DETECTED ON INPUT
C
   90 CONTINUE
c***      TYPE 200
      WRITE (*,200)
  200 FORMAT(' +++  INPUT CONVERSION ERROR  +++')
c***      TYPE 201
      WRITE (*,201)
  201 FORMAT('$RE-TYPE THE NUMBER: ')
      GO TO 1
      END
      LOGICAL FUNCTION YES(N)
      DATA IY,ILY / 'Y','y' /
c***      ACCEPT 101, IYN
      READ 101, IYN
  101 FORMAT(A1)
      YES=.FALSE.
      IF(IYN.EQ.IY .OR. IYN.EQ.ILY) YES=.TRUE.
      RETURN
      END

c------------------------------------------------------------------------------

	subroutine rdtape (iunit, mode, ntype, junk, nbki)

	return
	end

c------------------------------------------------------------------------------

	subroutine iowait (itape, nst, nbytes)

	return
	end

c------------------------------------------------------------------------------

	subroutine ioclos (in)

	return
	end

c------------------------------------------------------------------------------

	subroutine iopen (in, file_name)

	integer		in
	character*(*)	file_name

	return
	end

c------------------------------------------------------------------------------
