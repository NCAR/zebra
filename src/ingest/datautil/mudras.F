      SUBROUTINE CDCLOS (LOPN,NST)
C
C     USER CALLABLE INTERFACE TO CLOSE OUT A CEDRIC FILE WHICH HAS BEEN
C          OPENED BY THE USER
C
C          LOPN- LOGICAL UNIT NUMBER OF THE OPEN FILE
C           NST- STATUS:   0-OK, 1-ERROR
C
      CLOSE(UNIT=LOPN,ERR=701)
      NST=0
      RETURN
  701 CONTINUE
      NST=1
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE CDOPNR(LOPN,NAMFIL,NUSSCI,NUSSUB,NUSVOL,IPROPT,NST)
C
C        CDOPNR- OPENS THE DISK FILE FOR READ ACCESS
C                HEADER INFORMATION AND CARTESIAN SPECIFICATIONS OF THE
C                VOLUME ARE RETURNED COMMON BLOCK /VOLUME/
C
C      LOPN- (I*4) LOGICAL UNIT NUMBER TO ATTACH TO THE FILE   -USER SUPPLIED
C    NAMFIL- (C*40) NAME OF THE FILE TO BE OPENED FOR READ             "
C    NUSSCI- (C*6) NAME OF THE INVESTIGATING SCIENTIST            -RETURNED
C    NUSSUB- (C*6) NAME OF THE SUBMITTER OF THE RUN                    "
C    NUSVOL- (C*8) NAME OF THE CARTESIAN SPACE VOLUME                  "
C    IPROPT- (I*4)  =0, DO NOT PRINT THE VOLUME HEADER         -USER SUPPLIED
C                   =1,    PRINT     THE VOLUME HEADER                 "
C       NST- (I*4) OUTPUT STATUS:   0-OK,  1-ERROR                 -RETURNED
C
C
      PARAMETER (NFMAX=25,NID=510)
      PARAMETER (NIDT2=NID*2)
C
C        UPDATES THE EDIT HEADER BASED UPON THE CURRENT INFORMATION
C           IN THE TABLE STRUCTURE, FIELD MAPPINGS AND COUNT.
C
      COMMON /VOLUME/ ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),NFL,
     X                CSP(3,3),NCX(3),ORLAT(3),ORLON(3),IVDATE,IVTIME
C
C          ID- 510 WORD HEADER (USER SHOULD NOT ALTER THIS)    -RETURNED
C        NAMF- ARRAY OF 8-CHAR (CHUNKS OF I*2) FIELD NAMES          "
C      SCLFLD- CORRESPONDING ARRAY OF INTEGER SCALING FACTORS       "
C         NFL- NUMBER OF FIELDS IN THESE ARRAYS                     "
C         CSP- ARRAY OF AXES SPECIFICATIONS IN KM                   "
C              (1,1) XMIN       (1,2) YMIN        (1,3) ZMIN        "
C              (2,1) XMAX       (2,2) YMAX        (2,3) ZMAX        "
C              (3,1) X SPAC     (3,2) Y SPAC      (3,3) Z SPAC      "
C         NCX- ARRAY OF AXES LENGTHS (INTEGER)                      "
C              1- NX, 2-NY, 3-NZ                                    "
C       ORLAT- ARRAY CONTAINING THE ORIGIN LATITUDE  (NORTH)        "
C              1- DEGREES, 2-MINUTES, 3-SECONDS                     "
C       ORLON- ARRAY CONTAINING THE ORIGIN LONGITUDE (WEST)         "
C              1- DEGREES, 2-MINUTES, 3-SECONDS                     "
C      IVDATE- DATE OF THE VOLUME  (YYMMDD)                         "
C      IVTIME- TIME OF THE VOLUME  (HHMMSS)                         "
C           
C       I*2 VARIABLES: ID,NAMF
C
      INTEGER*2 ID,NAMF
      INTEGER*2 IDVAX, idcray
C                          
      CHARACTER*(*) NAMFIL,NUSSCI,NUSSUB,NUSVOL
C
      CHARACTER NAMSCI*6,NAMSUB*6,NAMVOL*8
      CHARACTER IERLST(3)*8
C
      BYTE IB(NIDT2)
C                   
      EQUIVALENCE (IB(19),NAMSCI),(IB(95),NAMSUB),(IB(201),NAMVOL)
C
      DATA IERLST/'CLOSING','OPENING','READING'/
      DATA IUNSAV/0/
      DATA ITTDUM,ICC/0,' '/
      DATA IDVAX/'VX'/
      DATA idcray/'CX'/
      DATA IYES/'Y'/
C
      NST=0
C
C  LOCATE REQUESTED VOLUME 
C
      IF(IUNSAV.NE.0) THEN
         CALL CDCLOS(IUNSAV,NST)
         IF(NST.NE.0) GO TO 201
         IUNSAV=LOPN
      END IF

C
C     *** Determine largest record size ***
C
       OPEN(UNIT=LOPN,FILE=NAMFIL,ACCESS='DIRECT',RECL=6400,ERR=202,
     X      FORM='UNFORMATTED',STATUS='OLD')

          CALL RDRAND(LOPN,1,ID,NID,NST)
 
          IF(NST.NE.0) GO TO 203

       CLOSE(LOPN,ERR=202)

       IF(ID(62).EQ.IDVAX .or. id(62) .eq. idcray) THEN
         CALL SWAP(ID,NID)
       END IF

       NSIZ=ID(162)*ID(167)
       NSIZ=MIN0(ID(65),NSIZ)
       NSIZ=((MAX0(NSIZ,NID)+1)/2)*4

C
C     *** Now open the file! ***
C
      OPEN(UNIT=LOPN,FILE=NAMFIL,ACCESS='DIRECT',RECL=NSIZ,ERR=202,
     X     FORM='UNFORMATTED',STATUS='OLD')

         CALL RDRAND(LOPN,1,ID,NID,NST)
 
         IF(NST.NE.0) GO TO 203

C  UNIX version only-- CHECK FOR FTP'D VAX FILE
C

      IF(ID(62).EQ.IDVAX .or. id(62) .eq. idcray) THEN
         CALL SWAP(ID,NID)
C
C        EVERYTHING HAS BEEN SWAPPED-- RESWAP CHARACTER INFO
C
         CALL SWAP(ID( 1),20)
         CALL SWAP(ID(43),16)
         CALL SWAP(ID(62), 1)
         CALL SWAP(ID(66), 1)
         CALL SWAP(ID(71),24)
         CALL SWAP(ID(101),4)
         CALL SWAP(ID(151),1)
         K=171
         NFL=ID(175) 
         DO KK=1,NFL
            K=K+5
            CALL SWAP(ID(K),4)
         END DO
         K=300
         NLND=ID(302)+ID(303)+1
         IF(NLND.GT.7) NLND=7
         DO KK=1,NLND
            K=K+6
            CALL SWAP(ID(K),3)
         END DO
      END IF        
C
      SFI=1./FLOAT(ID(68))
C
C        SCIENTIST, SUBMITTER AND VOLUME NAMES
C
         NUSSCI=NAMSCI
         NUSSUB=NAMSUB
         NUSVOL=NAMVOL
C
C        ORIGIN LATITUDE AND LONGITUDE
C
         SFLLI=1.0
         DO I=1,3
            IF(I.EQ.3) SFLLI=SFI
            ORLAT(I)=ID(32+I)*SFLLI
            ORLON(I)=ID(35+I)*SFLLI
         END DO
C
C  FILL IN /VOLUME/ WITH HEADER INFORMATION
C
C     LIST OF OUTPUT FIELDS
C
      NFL=ID(175)
      J=175
      DO I=1,NFMAX
         DO L=1,4
            NAMF(L,I)=IBL
         END DO                
         SCLFLD(I)=0
         IF(I.LE.NFL) THEN
            CALL COPY2(NAMF(1,I),ID(J+1),4)
            SCLFLD(I)=ID(J+5)
         END IF
         J=J+5
      END DO   
C
C     STORE AXES CHARACTERISTICS
C
      J=160
      CMI=1.0
      DO I=1,3
         IF(I.EQ.3) CMI=0.1
         CSP(1,I)=ID( J ) * SFI*CMI
         CSP(2,I)=ID(J+1) * SFI*CMI
         CSP(3,I)=ID(J+3) *  0.001
C
         NCX(I)=ID(J+2)
C
         J=J+5
      END DO
C
C     ENCODE DATA AND TIME OF THE VOLUME
C
      CALL IPKDAT(IVDATE,ID(116),ID(117),ID(118))
      CALL IPKDAT(IVTIME,ID(119),ID(120),ID(121))
      CALL IPKDAT(IVDATE,ID(122),ID(123),ID(124))
      CALL IPKDAT(IVTIME,ID(125),ID(126),ID(127))
C             
C  PRINT THE HEADER
C
      IF(IPROPT.EQ.1) THEN
         CALL IMHSUM(ITTDUM,ICC,IYES,ID,NID)
      END IF
C            
      RETURN
C
C  ERROR EXITS 
C
  201 PRINT 401, IERLST(1),IUNSAV
  401 FORMAT(///5X,'CDOPNR.....'/' +++  ERROR ',A8,' CEDRIC FILE',
     X             ' ON UNIT ',I3,'  +++'/)
      NST=1
      GO TO 900
  202 PRINT 401, IERLST(2),LOPN
      NST=2 
      GO TO 900
  203 PRINT 401, IERLST(3),LOPN
      NST=3
      GO TO 900
  900 CONTINUE                                            
      RETURN
      END 

c------------------------------------------------------------------------------

      SUBROUTINE CDOPNW (LOPN,NAMFIL,NUSSCI,NUSSUB,NUSVOL,IPROPT,NST)
C                                    
C     USER CALLABLE INTERFACE TO:
C        1- INITIALIZE THE VOLUME HEADER
C        2- OPEN A CEDRIC DISK FORMAT FILE
C        3- WRITE THE HEADER ON THIS FILE
C
C      LOPN- (I*4) LOGICAL UNIT NUMBER TO ATTACH TO THE FILE   -USER SUPPLIED
C    NAMFIL- (C*40) NAME OF THE FILE TO BE OPENED FOR WRITE            "
C    NUSSCI- (C*6) NAME OF THE INVESTIGATING SCIENTIST                 "
C    NUSSUB- (C*6) NAME OF THE SUBMITTER OF THE RUN                    "
C    NUSVOL- (C*8) NAME OF THE CARTESIAN SPACE VOLUME                  "
C                                                                     
C            NUSSCI,NUSSUB, AND NUSVOL ARE ALL OPTIONAL
C
C    IPROPT- (I*4)  =0, DO NOT PRINT THE VOLUME HEADER                 "
C                   =1,    PRINT     THE VOLUME HEADER                 "
C       NST- (I*4) OUTPUT STATUS:   0-OK,  1-ERROR                 -RETURNED
C
      PARAMETER (NFMAX=25,NID=510)
      PARAMETER (IBLK=3200)
      PARAMETER (NIDT2=NID*2)
C
C        UPDATES THE EDIT HEADER BASED UPON THE CURRENT INFORMATION
C           IN THE TABLE STRUCTURE, FIELD MAPPINGS AND COUNT.
C
      COMMON /VOLUME/ ID(NID),NAMF(4,NFMAX),SCLFLD(NFMAX),NFL,
     X                CSP(3,3),NCX(3),ORLAT(3),ORLON(3),IVDATE,IVTIME
C
C          ID- 510 WORD HEADER (USER SHOULD NOT ALTER THIS)
C        NAMF- ARRAY OF 8-CHAR (CHUNKS OF I*2) FIELD NAMES     -USER SUPPLIED
C      SCLFLD- CORRESPONDING ARRAY OF INTEGER SCALING FACTORS       "
C         NFL- NUMBER OF FIELDS IN THESE ARRAYS                     "
C         CSP- ARRAY OF AXES SPECIFICATIONS IN KM                   "
C              (1,1) XMIN       (1,2) YMIN        (1,3) ZMIN        "
C              (2,1) XMAX       (2,2) YMAX        (2,3) ZMAX        "
C              (3,1) X SPAC     (3,2) Y SPAC      (3,3) Z SPAC      "
C         NCX- ARRAY OF AXES LENGTHS (INTEGER)                      "
C              1- NX, 2-NY, 3-NZ                                    "
C       ORLAT- ARRAY CONTAINING THE ORIGIN LATITUDE  (NORTH)        "
C              1- DEGREES, 2-MINUTES, 3-SECONDS                     "
C       ORLON- ARRAY CONTAINING THE ORIGIN LONGITUDE (WEST)         "
C              1- DEGREES, 2-MINUTES, 3-SECONDS                     "
C      IVDATE- DATE OF THE VOLUME  (YYMMDD)                         "
C      IVTIME- TIME OF THE VOLUME  (HHMMSS)                         "
C           
C       I*2 VARIABLES: ID,NAMF
C
      INTEGER*2 ID,NAMF
C
      CHARACTER*(*) NAMFIL,NUSSCI,NUSSUB,NUSVOL
C

      INTEGER*2 IBL
      INTEGER*2 INFO(10),NDRF,NBLK,NRFLD
      INTEGER ISCRB(3)
      CHARACTER IB(NIDT2)*1

      CHARACTER LNMDEF*40,LUNAM*40,LMENTS(6)*40
      CHARACTER NAMSCI*6,NAMSUB*6,NAMLAN*6
      CHARACTER NAMOUT*8,NAMVOL*8,LCTIME*8,LCDATE*8
      EQUIVALENCE (ID( 65),NBLK),(ID( 96), NDRF),
     X            (ID(400),NRFLD),(IB(1),NAMOUT),
     X            (INFO(1),NAMLAN),(ID(  1),IB(1)),
     X            (IB(19),NAMSCI),(IB(95),NAMSUB),
     X            (IB(201),NAMVOL),(IB(109),LCTIME),
     X            (IB(101),LCDATE)
      LOGICAL YES,NO
      DATA ITTDUM/0/
      DATA LNMDEF/'CEDUSR.MUD'/
      DATA INFO/'CD','GN','.0','IN','TA','CT',' ',' ',' ',' ' /
      DATA (ID(I),I=1,20)/4*' ','CD','GN','.0',8*' ','CR','T ',3*' '/
      DATA (ID(I),I=43,58)/16*' '/
      DATA (ID(I),I=60,94)/0,NID,'UX',16,2,IBLK,'OR',-32768,100,64,0,
     X                    24*' '/ 
      DATA (ID(I),I=301,311)/0,1,0,0,0,'OR','IG','IN',0,0,0/ 

      DATA IBL/' '/ 
      DATA IYES,ICC/'Y',' '/ 

    7 CONTINUE
C
C        PARSE THE NAME OF THE OUTPUT FILE
C

      I=NFPARS(NAMFIL,LMENTS)

      IF(I.NE.0) THEN
         LUNAM=LNMDEF
      ELSE
         LUNAM=NAMFIL
      END IF
     
      I=NFPARS(LUNAM,LMENTS)

      IF(I.NE.0) THEN
         PRINT 101, LUNAM
  101    FORMAT(5X,'+++  UNABLE TO PARSE ',A,'  +++')
         GO TO 701
      END IF
C
      NAMOUT=LMENTS(4)
      ID(5)=INFO(1)
      ID(6)=INFO(2)
      ID(7)=INFO(3)
      ID(45)=INFO(4)
      ID(46)=INFO(5)

      ID(47)=INFO(6)
      CALL COPY2(ID(101),ID(1),4)
      ISF=ID(68)
      SFI=1./ISF
C
C     SCIENTIST,SUBMITTER AND VOLUME NAMES
C
      NAMSCI=NUSSCI
      NAMSUB=NUSSUB
      NAMVOL=NUSVOL
C
C     COORDINATE SYSTEM LAT, LON
C
      ID(40)=NINT(90.0*ID(69))
      SFLL=1.0
      DO 10 I=1,3
         IF(I.EQ.3) SFLL=FLOAT(ISF)
         ID(32+I)=ORLAT(I)*SFLL
         ID(35+I)=ORLON(I)*SFLL
   10 CONTINUE
C
C     ENCODE CURRENT DATE AND TIME
C
      CALL IDATE(ISCRB)
      ISCRB(3)=ISCRB(3)-1900
      WRITE (LCDATE, 122) ISCRB(2),ISCRB(1),ISCRB(3) 
  122 FORMAT(I2,'/',I2,'/',I2)

      CALL ITIME(ISCRB)
      WRITE (LCTIME,123) ISCRB(1),ISCRB(2),ISCRB(3)
  123 FORMAT(I2,':',I2,':',I2)

      SF=ID(68)
      NPLANE=NCX(1)*NCX(2)
      IF (NPLANE.GT.32767)THEN
        ID(301)=NPLANE-65536
      ELSE
        ID(301)=NPLANE
      END IF
      ID(106)=NCX(3)
      ID(111)=1
C
C     ENCODE DATA AND TIME OF THE VOLUME
C
      CALL IUNDAT(IVDATE,ID(116),ID(117),ID(118))
      CALL IUNDAT(IVTIME,ID(119),ID(120),ID(121))
      CALL IUNDAT(IVDATE,ID(122),ID(123),ID(124))
      CALL IUNDAT(IVTIME,ID(125),ID(126),ID(127))
      ID(128)=0
C
C     STORE AXES CHARACTERISTICS
C
      J=160
      CM=1.0
      DO 30 I=1,3
      IF(I.EQ.3) CM=10.0
      ID(J)=NINT(CSP(1,I)*SF*CM)
      ID(J+1)=NINT(CSP(2,I)*SF*CM)
      ID(J+3)=NINT(CSP(3,I)*1000.)
C
C      SURFACE AND SINGLE PLANE DATA
C
       IF(NCX(I).EQ.1.AND.ID(J+3).GT.0) ID(J+3)=0
       IF(ID(J+3).LT.0) ID(J+3) = -1
C
      ID(J+2)=NCX(I)
      ID(J+4)=I
      J=J+5
   30 CONTINUE
C
C     DISK FORMAT CHARACTERISTICS
C

      ID(96)=(NPLANE-1)/ID(65)+1
      ID(97)=ID(96)*NFL
      ID(98)=ID(97)*ID(106)
      ID(99)=ID(98)+ID(106)+1
      ID(100)=ID(98)+1
      ID(400)=ID(96)*ID(106)
C
C     LIST OF OUTPUT FIELDS
C
      ID(175)=NFL
      J=175
      DO 35 I=1,NFMAX
      DO L=1,4
         ID(J+L)=IBL      
      END DO
      ID(J+5)=0
      ID(400+I)=0
      IF(I.LE.NFL) THEN
         CALL COPY2(ID(J+1),NAMF(1,I),4)
         ID(J+5)=SCLFLD(I)
         ID(400+I)=2 + NRFLD * (I-1)
      END IF
      J=J+5
   35 CONTINUE
C
C     GENERATE SUMMARY 
C                      
      IF(IPROPT.EQ.1) THEN
         CALL IMHSUM(ITTDUM,ICC,IYES,ID,NID)
      END IF
C
C     OPEN UP THE CEDRIC FILE
C
      NSIZ=MIN0(ID(65),NPLANE)
      NSIZ=MAX0(NSIZ,NID)
      NSIZ=((NSIZ+1)/2)*4
      MAXR=ID(100)

C      PRINT *,'FILE NAME, SIZ(I2), MAXREC',LUNAM,NSIZ,MAXR

      OPEN(UNIT=LOPN,FILE=LUNAM,ACCESS='DIRECT',RECL=NSIZ,
     X     STATUS='NEW',ERR=701)

C
C     WRITE THE VOLUME HEADER TO THE FILE
C
      CALL WRRAND(LOPN,1,ID,NID,NST)
      IF(NST.NE.0) GO TO 701
C
      PRINT 118
  118 FORMAT(' CEDRIC FILE SUCCESSFULLY OPENED FOR WRITE.')
      NST=0
      RETURN
  701 CONTINUE
      PRINT 801
  801 FORMAT(' +++  ERROR OPENING CEDRIC FILE FOR WRITE ',/, 
     X       '      FILE MAY ALREADY EXIST +++')
      NST=1
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE COPY2(IO,IN,N)
C          
C        COPIES ONE 16-BIT ARRAY TO ANOTHER
C
      INTEGER*2 IO(N),IN(N)

      DO 10 I=1,N
         IO(I)=IN(I)
   10 CONTINUE
      RETURN
      END        

c------------------------------------------------------------------------------

      SUBROUTINE FETCHD(IN,ID,NID,LEV,IFLD,IBUF,RBUF,N1,N2,
     X                  IFIXAX,BAD,NST)
C
C        FETCHES DATA FOR A SINGLE FIELD FROM A PLANE
C              -INVERSE FUNCTION OF PLACED
C
C                 IN- INPUT UNIT
C                 ID- FILE ID HEADER
C                NID- DIMENSION OF ID
C                LEV- LEVEL (PLANE) NUMBER
C               IFLD- FIELD NUMBER
C               IBUF- I/O BUFFER
C               RBUF- CONTENTS OF FIELD FOR THIS PLANE
C                 N1- FIRST DIMENSION OF RBUF (RETURNED BY FETCHD)
C                 N2- SECOND    "     "   "       "     "    "
C             IFIXAX- FIXED AXIS (1,2 OR 3)
C                BAD- BAD DATA FLAG IN RBUF
C                NST- STATUS OF OPERATION (0-OK, 1-ERROR)
C
      DIMENSION IBUF(1),RBUF(1),ID(NID)
      INTEGER*2 ID,IBUF
      INTEGER*2 IDVAX, idcray
      DATA IDVAX/'VX'/
      data idcray /'CX'/

      IBAD=ID(67)
      SCALE=1./ID(175+IFLD*5)
      NRC=ID(96)
      NBLK=ID(65)
      IF (ID(301).LT.0) THEN
        NPLANE=ID(301)+65536
      ELSE
        NPLANE=ID(301)
      END IF
      GO TO (40,30,10), IFIXAX
   10 CONTINUE
C
C        Z- PLANE IS REQUESTED
C
      N1=ID(162)
      N2=ID(167)
      LOCR=ID(400+IFLD)+(LEV-1)*NRC
      write (6, 99334) ifld, id(400 + ifld), lev, nrc
99334	format (' fd, fld ', i3, ' id ', i9, ' lev ', i5, ' nrc ', i5)
      K=1
      DO 15 I=1,NRC
         LSIZ=MIN0(NBLK,NPLANE-(I-1)*NBLK)
         CALL RDRAND(IN,LOCR,IBUF(K),LSIZ,NST)
         IF(NST.NE.0) GO TO 701

C        UNIX VERSION -- SWAP BYTES ON AN FTP'D VAX FILE
C
         IF(ID(62).EQ.IDVAX .or. id(62) .eq. idcray) THEN
            CALL SWAP(IBUF(K),LSIZ)
         END IF
C
         LOCR=LOCR+1
         K=K+LSIZ
   15 CONTINUE
      DO 20 I=1,NPLANE
         RBUF(I)=BAD
         IF(IBUF(I).NE.IBAD) RBUF(I)=IBUF(I)*SCALE
   20 CONTINUE
      GO TO 90
   30 CONTINUE
C
C        Y- PLANE IS DESIRED
C
      N1=ID(162)
      N2=ID(172)
      I1=(LEV-1)*ID(162)+1
      I2=I1+ID(162)-1
      INC=1
      GO TO 50
   40 CONTINUE
C
C        X- PLANE IS DESIRED
C
      N1=ID(167)
      N2=ID(172)
      I1=LEV
      I2=LEV+(ID(167)-1)*ID(162)
      INC=ID(162)
   50 CONTINUE
C
C        READ IN ALL LEVELS AND STRIP OUT THE REQUESTED CROSS-SECTION
C
      NZ=ID(172)
      J=0
      LOCR=ID(400+IFLD)
      DO 70 L=1,NZ
         K=1
         DO 55 I=1,NRC
            LSIZ=MIN0(NBLK,NPLANE-(I-1)*NBLK)
            CALL RDRAND(IN,LOCR,IBUF(K),LSIZ,NST)
            IF(NST.NE.0) GO TO 701
C
C           UNIX VERSION -- SWAP BYTES ON AN FTP'D VAX FILE
C
            IF(ID(62).EQ.IDVAX .or. id(62) .eq. idcray) THEN
               CALL SWAP(IBUF(K),LSIZ)
            END IF
C
            LOCR=LOCR+1
            K=K+LSIZ
   55    CONTINUE
         DO 60 I=I1,I2,INC
            J=J+1
            RBUF(J)=BAD
            IF(IBUF(I).NE.IBAD) RBUF(J)=IBUF(I)*SCALE
   60    CONTINUE
   70 CONTINUE
   90 CONTINUE
      NST=0
      RETURN
  701 CONTINUE
C
C        ERROR EXIT
C
      NST=1
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE IMHSUM(IDEV,ICC,IYES,ID,NMD)
C
C        GENERATES A SUMMARY OF A MUDRAS 16-BIT VOLUME HEADER
C                                 IN CARTESIAN COORDINATES.
C

      INTEGER*2 ID(NMD)
      CHARACTER*1 ITAX(3)
      LOGICAL ISTERM
      DATA ITAX/ 'X', 'Y', 'Z' /
C
C     IDEV IS A DUMMY VARIABLE
C     OUTPUT UNIT IS ASSUMED TO BE SYS$OUTPUT
C
      IF(IYES.NE.'Y'.AND.IYES.NE.'C') RETURN
      IF(ID(1).NE.0) GO TO 10
      PRINT 200
  200 FORMAT(/5X,'NO FILE EXISTS AT PRESENT.'/)
      RETURN
   10 CONTINUE
      SF=1./ID(68)
      CF=1./ID(69)
      IF(IYES.EQ.'C') GO TO 22
C
C        WRITE OUT THE FULL HEADER
C
      PRINT 100
  100 FORMAT(A1)
      PRINT 105, (ID(I),I=1,4),ID(117),ID(118),ID(116),
     X   (ID(I),I=71,74),(ID(I),I=10,12),(ID(I),I=119,121),
     X   (ID(I),I=13,15),(ID(I),I=48,50),(ID(I),I=125,127),
     X   (ID(I),I=5,7),  (ID(I),I=51,54),(ID(I),I=101,104),
     X    ID(8),ID(9),(ID(I),I=55,58),(ID(I),I=16,20),(ID(I),I=45,47)
  105 FORMAT(//' CARTESIAN FORMAT VOLUME HEADER'15X,4A2
     X   /'  GENERAL INFORMATION...'
     X/'   DATE:      'I2,2('/'I2),5X'SOURCE:  '4A2,3X'SCIENTIST: '3A2
     X/'   BEG TIME:  'I2,2(':'I2),5X'RADAR:   '3A2,5X'SUBMITTER: '3A2
     X/'   END TIME:  'I2,2(':'I2),5X'PROGRAM: '3A2,5X'DATE RUN:  '4A2
     X/'   VOL. NAME: ', 4A2,      5X'PROJECT: '2A2,7X'TIME RUN:  '4A2
     X/'   SCAN MODE: ', 2A2,      9X'TAPE:    '3A2,5X'SEQUENCE:  '3A2)
      IF(ID(301).LT.0) THEN
        NPLANE=ID(301)+65536
      ELSE
        NPLANE=ID(301)
      END IF
      PRINT 106, ID(62),ID(96),NPLANE,ID(63),ID(97),ID(106),
     X                ID(65),ID(98),ID(67)
  106 FORMAT(/'  DATA CHARACTERISTICS...'
     X/'   COMPUTER:   ',2X,A2,5X'RECS/FIELD:  'I4,5X'PTS/FIELD:  'I6
     X/'   BITS/DATUM: ',I4,   5X'RECS/PLANE:  'I4,5X'NO. PLANES: 'I6
     X/'   BLOCK SIZE: ',I4,   5X'RECS/VOLUME: 'I4,5X'BAD DATA:   'I6)
      IF(ISTERM()) IQ=IPAUSE(0)
      N=ID(175)
      PRINT 107, N
  107 FORMAT(/'  FIELDS PRESENT: ',I2,' ...'
     X       /4X,'NO.',3X,'NAME',7X,'SCALE FACTOR')
      K2=175
      DO 15 I=1,N
      K1=K2+1
      K2=K2+5
      PRINT 108, I,(ID(K), K=K1,K2)
  108 FORMAT(4X,I3,3X,4A2,5X,I5)
   15 CONTINUE
      IF(N.GT.8.AND.ISTERM()) IQ=IPAUSE(0)
      N=ID(302)
      PRINT 109, N,ID(303)
  109 FORMAT(/'  LANDMARKS PRESENT: ',I2,5X,I3,' RADAR (S) ...'
     X   /4X,'NO.',3X,'NAME',6X,'X (KM)',4X,'Y (KM)',4X,'Z (KM)')
      K=306
      DO 20 I=1,N
      R1=ID(K+3)*SF
      R2=ID(K+4)*SF
      R3=ID(K+5)*0.001
      PRINT 110, I,ID(K),ID(K+1),ID(K+2), R1,R2,R3
  110 FORMAT(4X,I3,3X,3A2,2F10.2,F10.3)
      K=K+6
   20 CONTINUE
      IF(ID(303).NE.1) GO TO 21
C
C        WRITE OUT RADAR SPECS IF SINGLE RADAR
C
      R1=ID(304)*SF
      R2=ID(305)*SF
      PRINT 116, R1,R2
  116 FORMAT('  NYQUIST VELOCITY:',F8.2,7X,'RADAR CONSTANT:',F8.2)
   21 CONTINUE
      R1=ID(35)*SF
      R2=ID(38)*SF
      PRINT 111, ID(33),ID(34),R1,ID(36),ID(37),R2
  111 FORMAT(/'  ORIGIN  LATITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC'
     X       /9X,      'LONGITUDE:',I8,' DEG',I6,' MIN',F9.2,' SEC')
      IF(ISTERM()) IQ=IPAUSE(0)
   22 CONTINUE
C
C --  WRITE OUT CARTESIAN COORDINATE SPECS  --
C
      PRINT 112
  112 FORMAT(/'  CARTESIAN COORDINATE SYSTEM SPECIFICATIONS...'
     X/3X'AXIS'5X'MINIMUM (KM)'5X'MAXIMUM (KM)'6X'DELTA (KM)'4X,
     X'NO. OF PTS.')
      K=160
C        CALCULATE KILOMETERS FROM METERS FOR Z-AXIS
      CKM=1.0
      DO 25 I=1,3
      IF(I.EQ.3) CKM=0.1
      R1=ID(K)*SF*CKM
      R2=ID(K+1)*SF*CKM
      R3=ID(K+3)*0.001
      PRINT 114, ITAX(I),R1,R2,R3,ID(K+2)
  114 FORMAT(5X,A1,6X,F10.2,7X,F10.2,8X,F8.2,7X,I5)
      K=K+5
   25 CONTINUE
      L1=ID(164)
      L2=ID(169)
      L3=ID(174)
      R1=ID(40)*CF
      R2=ID(41)*SF
      R3=ID(42)*SF
      PRINT 115, ITAX(L1),ITAX(L2),ITAX(L3),R1,R2,R3
  115 FORMAT(/3X,'AXIS ORDER IS   ',3A3
     X    /3X,'ANGLE OF THE X-AXIS RELATIVE TO NORTH: ',F9.2,4X,'DEG.',
     X    /3X,'(X,Y)  AXIS ARE SPECIFIED RELATIVE TO:  (',
     X                F7.2,',',F7.2,')')
      IF(ID(173).LT.0) THEN
         PRINT 117
  117    FORMAT(3X,'ALL FIELDS ARE ASSUMED TO BE AT THE SURFACE.')
      END IF
      IF(ISTERM()) IQ=IPAUSE(0)
      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION IPAUSE(N)
C
C        PROMTS THE USER FOR A CARRIGE RETURN- USED FOR FREEZING THE 
C               TERMINAL DISPLAY.
C

      PRINT 101
  101 FORMAT('(HIT RETURN TO CONTINUE)')
      READ (*, 201) IRESP
  201 FORMAT(A1)
      IPAUSE=0
      IF(IRESP.EQ.'Q') IPAUSE=1
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE IPKDAT(IPACK,I1,I2,I3)
C
C     CONSTRUCTS A 6-DIGIT INTEGER FROM 3 (I*2) COMPONENTS
C
      INTEGER*4 IPACK,IHUN
      INTEGER*2 I1,I2,I3
      DATA IHUN/100/
C     
      IPACK = ( ( (I1 * IHUN) + I2) * IHUN) + I3
      RETURN
      END

c------------------------------------------------------------------------------

      LOGICAL FUNCTION ISTERM()
C
C     RETURNS: TRUE  if SYS$OUTPUT is a terminal device
C              FALSE   otherwise.
C              Requires NCAR library function CHECK_DEV_CLASS
C                                                            
C
        ISTERM = .TRUE.

      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE IUNDAT(IPACK,I1,I2,I3)
C
C     BREAKS DOWN A 6-DIGIT INTEGER INTO 3 (I*2) COMPONENTS
C
      INTEGER*4 IPACK,IHUN
      INTEGER*2 I1,I2,I3
      DATA IHUN/100/
C
      I1=IPACK/10000
      I2=(IPACK/IHUN)-(I1*IHUN)
      I3=IPACK-(I1*100+I2)*IHUN 
      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION LOCFLD(IFLD,LTAB,N1,N2,LOOK)
C
C        LOCATES MATCHUPS BETWEEN I*2 QUANTITIES
C           IFLD- ITEM BEING SEARCHED FOR
C           LTAB- SEARCH TABLE
C             N1- NUMBER OF WORDS/ITEM
C             N2- NUMBER OF ITEMS IN THE TABLE
C           LOOK- NUMBER OF WORDS DEEP TO SEARCH FOR THE ITEM
C
      INTEGER*2 IFLD(LOOK),LTAB(N1,N2)

      DO 10 M=1,N2
      DO 5 L=1,LOOK
         IF(IFLD(L).NE.LTAB(L,M)) GO TO 10
    5 CONTINUE
      K=M
      GO TO 20
   10 CONTINUE
      K=0
   20 CONTINUE
      LOCFLD=K
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
      PRINT 101, NTYP(ITYP)
  101 FORMAT(/,A6,' FILE NAME? ',$)
      READ (*, 201) INP
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
      PRINT 102, NTYP(ITYP),INP
  102 FORMAT(' ',A6,' FILE NAME= ',A/'OK? ')
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
      RETURN
      END

c------------------------------------------------------------------------------

      LOGICAL FUNCTION NO(N)
      DATA IBY,ISY/'Y','y'/
      READ (*, 101) IYN
  101 FORMAT(A1)
      
      NO=.FALSE.
      IF(IYN.NE.IBY.AND.IYN.NE.ISY) NO=.TRUE.
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE PLACED(IOUT,ID,NID,LEV,IFLD,IBUF,RBUF,N1,N2,
     X                  IFIXAX,BAD,NST)
C
C        WRITES A SINGLE FIELD (IN A PLANE) TO DISK
C              -INVERSE FUNCTION OF FETCHD
C               IOUT- OUTPUT UNIT
C                 ID- FILE ID HEADER
C                NID- DIMENSION OF ID
C                LEV- LEVEL (PLANE) NUMBER
C               IFLD- FIELD NUMBER
C               IBUF- I/O BUFFER
C               RBUF- CONTENTS OF FIELD FOR THIS PLANE
C                 N1- FIRST DIMENSION OF RBUF
C                 N2- SECOND    "     "   "
C             IFIXAX- FIXED AXIS (1,2 OR 3)
C                BAD- BAD DATA FLAG IN RBUF
C                NST- STATUS OF OPERATION (0-OK, 1-ERROR)
C         
      DIMENSION IBUF(1),RBUF(1),ID(NID)
      INTEGER*2 ID,IBUF
C

      IBAD=ID(67)
      MULT=ID(175+IFLD*5)
      NRC=ID(96)
      NBLK=ID(65)
      IF(ID(301).LT.0) THEN
        NPLANE=ID(301)+65536
      ELSE
        NPLANE=ID(301)
      END IF
      NSIZ=MAX0(NPLANE,NID)
C
C        IFIXAX=3 IS THE ONLY ONE ALLOWED AS OF 5/1/82
C
   10 CONTINUE
      LOCR=ID(400+IFLD)+(LEV-1)*NRC
      K=1
      KBAD=0
      DO 15 I=1,NPLANE
         IBUF(I)=IBAD
         IF(RBUF(I).EQ.BAD) GO TO 15
         ICHECK=NINT(RBUF(I)*MULT)
         IF(IABS(ICHECK).GE.32768) GO TO 14
            IBUF(I)=ICHECK
            GO TO 15
   14    KBAD=KBAD+1
   15 CONTINUE
C        
C        PRINT OUT MESSAGE IF OUT OF BOUNDS VALUES HAVE BEEN DETECTED AND
C             RESET TO IBAD.
C
      IF(KBAD.NE.0) THEN
         I1=171+IFLD*5
         I2=I1+3
         PRINT 101, KBAD,(ID(I),I=I1,I2),LEV
         IF(IPSWIT.NE.0) PRINT 101, KBAD,(ID(I),I=I1,I2),LEV
  101    FORMAT(' +++  ',I5,' VALUES OUT OF RANGE IN FIELD: ',4A2,
     X          ' AT LEVEL=',I3,'  +++')
      END IF
      DO 20 I=1,NRC
         LSIZ=MIN0(NBLK,NSIZ-(I-1)*NBLK)
         CALL WRRAND(IOUT,LOCR,IBUF(K),LSIZ,NST)
         IF(NST.NE.0) GO TO 701
         LOCR=LOCR+1
         K=K+LSIZ
   20 CONTINUE
      NST=0
      RETURN
  701 CONTINUE
C
C        ERROR EXIT
C
      PRINT 801, IFLD,LEV
  801 FORMAT(' +++  ERROR WRITING FIELD  ',I2,'  AT  LEVEL ',I3,'  +++')
      NST=1
      RETURN
      END                         

c------------------------------------------------------------------------------ 
      SUBROUTINE RITER(RBUF,NX,NY,BAD,IWIND,PLWIND,SCLDSP,
     X                 NDIG,NCWORD,ZLEV,NAMF) 
C 
C        PRODUCES A DIGITAL DISPLAY OF A 2-DIMENSIONAL FIELD 
C        +++++ ALL PARAMETERS SUPPLIED BY THE USER +++++ 
C              LISTING IS SENT TO SYS$OUTPUT 
C                      
C       RBUF- 2-D ARRAY CONTAINING THE PLANE OF DATA TO BE LISTED OUT 
C         NX- FIRST DIMENSION OF RBUF (# PTS ALONG X) 
C         NY-  2ND  DIMENSION OF RBUF (# PTS ALONG X) 
C        BAD- VALUE USED TO INDICATE MISSING DATA 
C      IWIND- ARRAY OF BEGINNING ENDING AXES INDICES FOR DISPLAY 
C             (1,1) 1ST INDEX X  (1,2) 1ST INDEX Y  (1,3) 1ST INDEX Z 
C             (2,1) END INDEX X  (2,2) END INDEX Y  (2,3) END INDEX Z 
C     PLWIND- VALUES IN WORLD COORDINATES CORRESPONDING TO IWIND INDICES 
C     SCLDSP- DISPLAY SCALING FACTOR (E.G... TO RETAIN IST DECIMAL PLACE 
C                                            OF DISPLAYED VALUES USE 10.)
C       NDIG- MAXIMUM NUMBER OF DIGITS TO DISPLAY; IF THIS VALUE IS
C             EXCEEDED ONLY THE SIGN OF THE NUMBER WILL APPEAR.
C     NCWORD- AXES ORDER: (1,2,3) <==> (X,Y,Z)
C       ZLEV- VALUE IN WORLD COORDINATES OF THE FIXED PLANE OF DATA
C       NAMF- 8-CHAR INTEGER ARRAY NAME OF THE FIELD TO BE LISTED OUT
C
C
      DIMENSION LABAXS(3,3),SCLAXS(3,3)
      DIMENSION RBUF(NX,NY),IWIND(2,3),PLWIND(2,3),NCWORD(3)
      INTEGER*2 NAMF(4), LAX(3)
      CHARACTER*1 IBL,IDOT,ICOL,IPL,IEQ,IMIN
      CHARACTER LABI*130, LINE*130, IFMT*10
      LOGICAL ISTERM
C                   
C     SET DEFAULT AXES UNITS TO (KM,KM,KM)
C
      DATA LABAXS/'KM','KM','KM','NM','NM','K-FT','MI','MI','K-FT'/
      DATA SCLAXS/1.0,1.0,1.0, 0.5399568035,0.5399568035,3.280833,
     X                         0.6213688756,0.6213688756,3.280833/
      DATA IUNAXS/1/
C
      DATA IDOT/'.'/, ICOL/':'/, IPL/'+'/, IEQ/'='/
      DATA IBL/' '/,  IMIN/'-'/
      DATA LAX/'X', 'Y', 'Z'/
      DATA NTAB/5/
C
      IFIXAX=NCWORD(3)
      II=NCWORD(1)
      I1=IWIND(1,II)
      I2=IWIND(2,II)
      IJ=NCWORD(2)
      J1=IWIND(1,IJ)
      J2=IWIND(2,IJ)
      IF(ISTERM()) THEN
        NI=75
        NJ=20
      ELSE
        NI=125
        NJ=1024
      END IF
C
C        INITIALIZE FOR THE ENTIRE DISPLAY
C
      MAXAC=NI/NDIG
      IPDIV=10**NDIG
      INDIV=IPDIV/10

      WRITE (IFMT, 201) NDIG

  201 FORMAT(6X,'(I',I1,')')
      LOOP=0
      ILAST=I1-1
   10 CONTINUE
C
C        GENERATE NEXT SECTION OF THE DISPLAY
C
      IFRST=ILAST+1
      ILAST=ILAST+MAXAC
      IF(ILAST.GT.I2) ILAST=I2
      NGO=ILAST-IFRST+1
      LASCOL=NTAB+NGO*NDIG
      L=J2
      LOOP=LOOP+1
   15 CONTINUE
C
C        GENERATE NEXT PAGE OF DISPLAY
C
      PRINT 105,      I1,I2,PLWIND(1,II),PLWIND(2,II),LABAXS(II,IUNAXS),
     X                J1,J2,PLWIND(1,IJ),PLWIND(2,IJ),LABAXS(IJ,IUNAXS)
  105 FORMAT(//10X,'DISPLAY RANGES FROM (',I3,' TO ',I3,')   (',F6.1,
     X         ' TO ',F6.1,1X,A4,') ALONG I'/21X,'AND FROM (',I3,
     X         ' TO ',I3,')   (',F6.1,' TO ',F6.1,1X,A4,') ALONG J')
      PRINT 100,      NAMF,SCLDSP,LAX(IFIXAX),ZLEV,
     X                LABAXS(IFIXAX,IUNAXS),LOOP
  100 FORMAT(1X,4A2,' ( X ',F6.1,')',35X,A2,'=',F6.1,' ',A4,
     X         3X,'P.',I2)
      KNT=2
   20 CONTINUE
C
C        GENERATE NEXT LINE OF DISPLAY
C
      LINE=IBL
      IF(MOD(L,5).EQ.0) THEN

      WRITE (LINE(1:5), 101) L
  101   FORMAT(1X,I3,'+')

      ELSE
        LINE(5:5)=ICOL
      END IF
      IPOS=IFRST
      DO 25 I=1,NGO
      LOC=NTAB + I*NDIG
      IF(RBUF(IPOS,L).EQ.BAD) THEN
        LINE(LOC:LOC)=IDOT
      ELSE
        NUM=RBUF(IPOS,L)*SCLDSP+0.5
        IF(RBUF(IPOS,L).LT.0.0) NUM=NUM-1
        IF(NUM.GE.0) THEN
          ITEST=NUM/IPDIV
          IF(ITEST.NE.0) THEN
            LINE(LOC:LOC)=IPL
          ELSE
            WRITE (LINE(LOC-NDIG+1:LOC), IFMT) NUM
          END IF
        ELSE
          ITEST=NUM/INDIV
          IF(ITEST.NE.0) THEN
            LINE(LOC:LOC)=IMIN
          ELSE
            WRITE (LINE(LOC-NDIG+1:LOC), IFMT) NUM
          END IF
        END IF
      END IF
      IPOS=IPOS+1
   25 CONTINUE
      PRINT 102,      LINE(1:LASCOL)
  102 FORMAT(A)
      L=L-1
      IF(L.LT.J1) GO TO 30
      KNT=KNT+1
      IF(KNT.LT.NJ) GO TO 20
   30 CONTINUE
C
C        GENERATE AXIS LABEL AT BOTTOM OF PAGE
C
      LABI=IBL
      LINE=IBL
      IPOS=IFRST
      DO 35 I=1,NGO
      LOC=NTAB + I*NDIG
      IF(MOD(IPOS,5).NE.0) THEN
        LINE(LOC:LOC)=IEQ
      ELSE
        LINE(LOC:LOC)=IPL
        WRITE (LABI(LOC-2:LOC), 103) IPOS
  103   FORMAT(I3)
      END IF
      IPOS=IPOS+1
   35 CONTINUE
      PRINT 102,      LINE(1:LASCOL)
      PRINT 102,      LABI(1:LASCOL)
C
C        PAUSE IF WRITING TO TERMINAL
C
      IF(ISTERM()) THEN
         IQ=IPAUSE(0)
         IF(IQ.EQ.1) RETURN
      END IF
C
      IF(L.GE.J1) GO TO 15
      IF(ILAST.LT.I2) GO TO 10
      RETURN
      END

c--------------------------------------------------------------------------

      SUBROUTINE SWAP(IBUF,N16)
C
C        SWAP THE DATA FOR UNIX VERSION
C          CHARACTER DATA DOES NOT NEED TO BE SWAPPED
C
      BYTE IBUF(2,N16), ISAV
c      DO 10 I=1,N16
c      ISAV=IBUF(1,I)
c      IBUF(1,I)=IBUF(2,I)
c      IBUF(2,I)=ISAV
c   10 CONTINUE
      RETURN
      END

c------------------------------------------------------------------------------

      FUNCTION TTINP(N)
C
C        PERMITS THE INPUT OF NUMERICAL VALUES WITH ERROR RECOVERY
C

    1 CONTINUE
      READ (*,101,ERR=90) VAL 
  101 FORMAT(F10.0) 
      TTINP=VAL 
      RETURN 
C 
C       ERROR DETECTED ON INPUT
C
   90 CONTINUE
      CALL ERRSNS(IERR)
      PRINT 200
  200 FORMAT(' +++  INPUT CONVERSION ERROR  +++')
      PRINT 201
  201 FORMAT('RE-TYPE THE NUMBER: ')
      GO TO 1
      END

c------------------------------------------------------------------------------

      SUBROUTINE WRRAND(IT,NREC,IBUF,NWDS,NST)
C 
C  WRITES DATA ACCORDING TO UNIT AND RECORD NUMBER
C 
      INTEGER*2 IBUF(NWDS)
C
C        WRITE A RECORD
C
      WRITE(IT,REC=NREC,ERR=401)  IBUF
      NST=0
      RETURN
  401 NST=2
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE RDRAND(IT,NREC,IBUF,NWDS,NST)
C
C  READS DATA ACCORDING TO UNIT AND RECORD NUMBER
C
      INTEGER*2 IBUF(NWDS)
C
       READ(IT,REC=NREC,ERR=401) (IBUf(i), i=1,nwds)
    
      NST=0
      RETURN
  401 NST=2
      write (6, 999) nrec, nwds
999	format ('READ error rec ', i7, ' nw ', i6)

      RETURN
      END

c------------------------------------------------------------------------------

      LOGICAL FUNCTION YES(N)
 
      DATA IBY,ISY/'Y','y'/
      READ (*, 101) IYN
  101 FORMAT(A1)
      YES=.FALSE.
      IF(IYN.EQ.IBY.OR.IYN.EQ.ISY) YES=.TRUE.
      RETURN
      END

c------------------------------------------------------------------------------

      SUBROUTINE ERRSNS(IERR)

      INTEGER IERR
      RETURN
      END

c------------------------------------------------------------------------------
