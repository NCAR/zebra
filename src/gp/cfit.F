C		Copyright (C) 1987,88,89,90,91 by UCAR
C	University Corporation for Atmospheric Research
C		   All rights reserved
C
C No part of this work covered by the copyrights herein may be reproduced
C or used in any form or by any means -- graphic, electronic, or mechanical,
C including photocopying, recording, taping, or information storage and
C retrieval systems -- without permission of the copyright owner.
C 
C This software and any accompanying written materials are provided "as is"
C without warranty of any kind.  UCAR expressly disclaims all warranties of
C any kind, either express or implied, including but not limited to the
C implied warranties of merchantibility and fitness for a particular purpose.
C UCAR does not indemnify any infringement of copyright, patent, or trademark
C through use or modification of this software.  UCAR does not provide 
C maintenance or updates for its software.
C
       SUBROUTINE COMPX(XIN,INDEX)
C ROUTINE TO COMPUTE THE ARRAY XIN WHICH IS THE INPUT TO THE
C CURVE-FIT ROUTINE CURFIT.

# include "rgrid.h"

       REAL XIN(21,MAXSTN)
C
C INDEX OF THE LAST TERM OF THE POLYNOMIAL TO BE FIT PLUS 1.
       INDEX=0
       IF(NSTNS.GT.1) INDEX=3
       IF(NSTNS.GT.4) INDEX=6
       IF(NSTNS.GT.8) INDEX=10
       IF(NSTNS.GT.13) INDEX=15
       IF(NSTNS.GT.19) INDEX=21
       IX=0
       DO 100 I=1,NP
       IF(.NOT.DELETED(I)) THEN
         IX=IX+1
         XX=9.*(X(I)-XMIN)/(XMAX-XMIN)+1.
         YY=9.*(Y(I)-YMIN)/(YMAX-YMIN)+1.
         XIN(1,IX)=XX
         XIN(2,IX)=YY
         IF(INDEX.GT.3) THEN
           XIN(3,IX)=XX*YY
           XIN(4,IX)=XX*XX
           XIN(5,IX)=YY*YY
           IF(INDEX.GT.6) THEN
             XIN(6,IX)=XIN(4,IX)*YY
             XIN(7,IX)=XIN(5,IX)*XX
             XIN(8,IX)=XIN(4,IX)*XX
             XIN(9,IX)=XIN(5,IX)*YY
             IF(INDEX.GT.10) THEN
               XIN(10,IX)=XIN(9,IX)*XX
               XIN(11,IX)=XIN(4,IX)*XIN(5,IX)
               XIN(12,IX)=XIN(8,IX)*YY
               XIN(13,IX)=XIN(4,IX)*XIN(4,IX)
               XIN(14,IX)=XIN(5,IX)*XIN(5,IX)
               IF(INDEX.GT.15) THEN
                 XIN(15,IX)=XIN(14,IX)*XX
                 XIN(16,IX)=XIN(4,IX)*XIN(9,IX)
                 XIN(17,IX)=XIN(8,IX)*XIN(5,IX)
                 XIN(18,IX)=XIN(13,IX)*YY
                 XIN(19,IX)=XIN(13,IX)*XX
                 XIN(20,IX)=XIN(14,IX)*YY
               ENDIF
             ENDIF
           ENDIF
         ENDIF
       ENDIF
  100  CONTINUE
       RETURN
       END




       SUBROUTINE POLYFIT(PAM,XIN,INDEX,B)

# include "rgrid.h"

       REAL PAM(MAXSTN),XIN(21,MAXSTN),B(21),W(MAXSTN)
C
       DO 10 I=1,21
  10   B(I)=0.
       IX=0
       DO 100 I=1,NP
       IF(.NOT.DELETED(I)) THEN
         IX=IX+1
         XIN(INDEX,IX)=PAM(I)
       ENDIF
  100  CONTINUE
C
       W(1)=0.
       CALL CURFIT(INDEX,NSTNS,XIN,W,.01,.01,B)
       RETURN
       END




       SUBROUTINE EVAL(GRID,B,INDEX,MODE)
C EVALUATE THE POLYNOMIAL AT THE POINTS OF A RECTANGULAR GRID.

# include "rgrid.h"

       REAL GRID(NX,NY),B(21)
C
       DO 100 J=1,NY
       XX=X0
       YY=Y0+(J-1)*DY
       DO 100 I=1,NX
       IF(JW(I,J).GE.0) THEN
         CALL CFITVAL(XX,YY,B,INDEX,GRID(I,J))
       ELSE
         IF(MODE.NE.0) GRID(I,J)=FLAG
       ENDIF
       XX=XX+DX
  100  CONTINUE
       RETURN
       END




       SUBROUTINE CFITVAL(XX,YY,B,INDEX,VALUE)

# include "rgrid.h"

       REAL B(21),BX(20)
C
       X1=9.*(XX-XMIN)/(XMAX-XMIN)+1.
       Y1=9.*(YY-YMIN)/(YMAX-YMIN)+1.
       BX(1)=X1
       BX(2)=Y1
       IF(INDEX.GT.3) THEN
         BX(3)=X1*Y1
         BX(4)=X1*X1
         BX(5)=Y1*Y1
         IF(INDEX.GT.6) THEN
           BX(6)=BX(4)*Y1
           BX(7)=BX(5)*X1
           BX(8)=BX(4)*X1
           BX(9)=BX(5)*Y1
           IF(INDEX.GT.10) THEN
             BX(10)=BX(9)*X1
             BX(11)=BX(4)*BX(5)
             BX(12)=BX(8)*Y1
             BX(13)=BX(4)*BX(4)
             BX(14)=BX(5)*BX(5)
             IF(INDEX.GT.15) THEN
               BX(15)=BX(14)*X1
               BX(16)=BX(4)*BX(9)
               BX(17)=BX(5)*BX(8)
               BX(18)=BX(13)*Y1
               BX(19)=BX(13)*X1
               BX(20)=BX(14)*Y1
             ENDIF
           ENDIF
         ENDIF
       ENDIF
       VALUE=B(INDEX)
       DO 90 K=1,INDEX-1
       VALUE=VALUE+B(K)*BX(K)
  90   CONTINUE
       RETURN
       END




       SUBROUTINE CURFIT(N,M,XIN,W,F1,F2,BB)

# include "rgrid.h"

       DOUBLE PRECISION VAR(21),XB(21),S(21,21),SIG(21),R(21,21)
       DOUBLE PRECISION VMAX,VMIN,SY,F2X,F1X,B0,B(21),A(21,21),SB(21)
       REAL XIN(21,MAXSTN),BB(21),W(MAXSTN)
C
       IF(W(1).LE.1.E-6) THEN
         DO 90 I=1,M
 90      W(I)=1.
       ENDIF
       TW=0.
       DO 97 I=1,M
  97   TW=TW+W(I)
       DO 100 I=1,N
       VAR(I)=0.
       DO 99 L=1,M
  99   VAR(I)=VAR(I)+W(L)*XIN(I,L)
       DO 100 J=1,N
       A(I,J)=0.
       DO 100 K=1,M
  100  A(I,J)=A(I,J)+W(K)*XIN(I,K)*XIN(J,K)
       DO 120 I=1,N
       XB(I)=VAR(I)/DBLE(TW)
       DO 110 J=1,N
  110  S(I,J)=(DBLE(TW)*A(I,J)-VAR(I)*VAR(J))/DBLE(TW)
       R(I,I)=1.
  120  SIG(I)=SQRT(S(I,I))
       NM1=N-1
       DO 130 I=1,NM1
       IP1=I+1
       DO 130 J=IP1,N
       IF(ABS(SIG(I)*SIG(J)).GE..0001) GO TO 400
       R(I,J)=0.
       GO TO 130
  400  R(I,J)=S(I,J)/(SIG(I)*SIG(J))
  130  R(J,I)=R(I,J)
       DO 150 J=1,N
       DO 150 I=1,N
  150  A(I,J)=R(I,J)
       PHI=TW-1.
       ITER=0
  7    DO 140 I=1,NM1
       SB(I)=0.
  140  B(I)=0.
       VMIN=1.D+35
       VMAX=0.
       NMIN=0
       NMAX=0
       SY=SIG(N)*SQRT(A(N,N)/PHI)
       I=1
  4    IF(A(I,I).LE.1.D-6) GO TO 3
       VAR(I)=A(I,N)*A(N,I)/A(I,I)
       IF(VAR(I)) 1,3,160
  160  IF(VAR(I).LE.VMAX) GO TO 3
  2    VMAX=VAR(I)
       NMAX=I
       GO TO 3
  1    B(I)=A(I,N)*SIG(N)/SIG(I)
       SB(I)=SY*SQRT(A(I,I))/SIG(I)
       IF(ABS(VAR(I)).GE.ABS(VMIN)) GO TO 3
       VMIN=VAR(I)
       NMIN=I
  3    IF(I.GE.NM1) GO TO 170
       I=I+1
       GO TO 4
  170  B0=XB(N)
       DO 180 J=1,NM1
  180  B0=B0-B(J)*XB(J)
       F2X=ABS(VMIN)*PHI/A(N,N)
       IF(F2X.GE.F2) GO TO 6
       K=NMIN
       PHI=PHI+1.
       GO TO 5
  6    F1X=VMAX*(PHI-1.)/(A(N,N)-VMAX)
       IF(F1X.LE.F1) GO TO 22
  8    K=NMAX
       PHI=PHI-1.
  5    DO 4237 J=1,N
       DO 4237 I=1,N
 4237  R(I,J)=A(I,J)
       DO 190 J=1,N
       DO 190 I=1,N
       IF(I.EQ.K) GO TO 11
       IF(J.EQ.K) GO TO 20
  10   A(I,J)=R(I,J)-R(I,K)*R(K,J)/R(K,K)
       GO TO 190
  11   IF(J.EQ.K) GO TO 30
  40   A(K,J)=R(K,J)/R(K,K)
       GO TO 190
  30   A(K,K)=1./R(K,K)
       GO TO 190
  20   A(I,K)=-1.*R(I,K)/R(K,K)
  190  CONTINUE
       ITER=ITER+1
       IF(ITER.LT.(2*N)) GO TO 7
  22   B(N)=B0
       DO 23 I=1,N-1
  23   BB(I)=B(I)
       BB(N)=B0
       IPRINT=0
       IF(IPRINT.EQ.0) RETURN
       SB0=SQRT(SY**2/PHI)
C       WRITE(LUOUT,2000) SY,F1,F2
C       WRITE(LUOUT,5000) B0,SB0,(I,B(I),I,SB(I),I=1,NM1)
C       WRITE(LUOUT,5002)
       ERRMAX=0.
       ERRM=0.
       SSAB=0.
       SSDUE=0.
       DO 210 I=1,M
       YP=B0
       DO 200 J=1,NM1
  200  YP=YP+B(J)*XIN(J,I)
       DEV=YP-XIN(N,I)
        DEVABS=ABS(DEV)
       ERRMAX=MAX(ERRMAX,DEVABS)
       ERRM=ERRM+DEVABS
       SSAB=SSAB+DEV*DEV
       SSDUE=SSDUE+(YP-XB(N))**2
       IF(ABS(XIN(N,I)).GE..0001) GO TO 3000
       PE=1.E+6
       GO TO 210
 3000  PE=100.*DEV/XIN(N,I)
C       WRITE(LUOUT,5001) XIN(N,I),YP,DEV,PE
  210  CONTINUE
       ERRM=ERRM/FLOAT(M)
       WRITE(LUOUT,123) ERRM,ERRMAX
  123  FORMAT(' ERRMEAN,ERRMAX',2F10.4)
       SSTOT=SSAB+SSDUE
       IF(SSTOT.GE..0001) GO TO 900
       RSQUAR=0.
       GO TO 901
  900  RSQUAR=SSDUE/SSTOT
  901  IDOF=1
       DO 300 I=1,NM1
       IF(ABS(B(I)).LE.1.E-25) GO TO 300
       IDOF=IDOF+1
  300  CONTINUE
       JDOF=M-IDOF
       RMSDUE=SSDUE/IDOF
       RMSAB=SSAB/JDOF
       STERR=SQRT(RMSAB)
C       WRITE(LUOUT,4001)
 4001  FORMAT(1H0,5X,'SOURCE OF VARIATION',7X,'DEGREE OF',
     *7X,'SUM OF',9X,'MEAN'/32X,'FREEDOM',9X,'SQUARES',
     *7X,'SQUARE')
C       WRITE(LUOUT,4002) IDOF,SSDUE,RMSDUE
 4002  FORMAT(1H0,'DUE TO REGRESSION',12X,I6,F17.5,F14.5)
C       WRITE(LUOUT,4003) JDOF,SSAB,RMSAB
 4003  FORMAT(1H ,'DEVIATION ABOUT REGRESSION',I6,F17.5,F14.5)
C       WRITE(LUOUT,4006) M,SSTOT
 4006  FORMAT(8X,'TOTAL',19X,I6,F17.5)
       WRITE(LUOUT,4008) RSQUAR,STERR
 4008  FORMAT(1H0,'MULTIPLE CORRELATION COEFFICIENTS',8X,
     *'STANDARD ERROR OF ESTIMATE'/10X,F10.5,29X,F10.5)
 2000  FORMAT(1H0,'SIGMA=',E20.8,5X,'F1(ENTER)=',F6.3,5X,
     *'F2(REMOVE)=',F6.3)
 5000  FORMAT(1H0,2X,'INTERCEPT=',E20.8,10X,'SB(N)=',E20.8/
     *(5X,'B(',I3,')=',E20.8,10X,'SB(',I3,')=',E20.8))
 5001  FORMAT(1H0,3E15.7,2X,F10.1)
 5002  FORMAT(1H0,'ACTUAL VALUE',5X,'PREDICTED VALUE',
     *2X,'DIFFERENCE',5X,'PER CENT ERROR')
       RETURN
       END




       SUBROUTINE IERROR(ARRAY1,ARRAY2,INDEX)
C COMPUTE APPROXIMATION ERRORS AT PAM STATIONS
C INDEX = 1 IF EXCLUDE ALL PAM STATIONS THAT ARE NEAR GRID POINTS
C OUTSIDE THE AREA COVERED BY PAM STATIONS
C INDEX = 0 IF INCLUDE ALL PAM STATIONS

# include "rgrid.h"

       REAL ARRAY1(NX,NY),ARRAY2(MAXSTN)
C
       RINTRP(B,DELTA,F1,F2)=F1+(B/DELTA)*(F2-F1)
C
       AVERR=0.
       ERRMAX=0.
       AVEFLD=0.
       IX=0
       DO 100 I=1,NP
       IF(.NOT.DELETED(I)) THEN
         IF(X(I).GE.(XMIN+DX).AND.X(I).LE.XMAX.AND.Y(I).GE.(YMIN+DY)
     *.AND.Y(I).LE.YMAX) THEN
           I1=INT((X(I)-XMIN)/DX)
           J1=INT((Y(I)-YMIN)/DY)
           IF(I1.LE.0.OR.J1.LE.0.OR.I1.GT.NX.OR.J1.GT.NY) GO TO 100
           IF((JW(I1,J1).LE.0.OR.JW(I1+1,J1).LE.0.OR.
     *JW(I1,J1+1).LE.0.OR.JW(I1+1,J1+1).LE.0).AND.
     *INDEX.EQ.0) GO TO 100
           XX=X(I)-(REAL(I1-1))*DX-X0
           YY=Y(I)-(REAL(J1-1))*DY-Y0
           F0=RINTRP(XX,DX,ARRAY1(I1,J1),ARRAY1(I1,J1+1))
           F1=RINTRP(XX,DX,ARRAY1(I1+1,J1),ARRAY1(I1+1,J1+1))
           F2=RINTRP(YY,DY,F0,F1)
           ERRABS=ABS(F2-ARRAY2(I))
           ERRMAX=MAX(ERRMAX,ERRABS)
           AVERR=AVERR+ERRABS
           AVEFLD=AVEFLD+ABS(ARRAY2(I))
           IX=IX+1
         ENDIF
       ENDIF
  100  CONTINUE
       AVERR=AVERR/REAL(IX)
       AVEFLD=AVEFLD/REAL(IX)
       WRITE(LUOUT,110) AVERR,ERRMAX,AVEFLD,IX
  110  FORMAT(' AVERR,ERRMAX,AVEFLD,IX',3F10.4,I6)
       RETURN
       END




       SUBROUTINE CFITD(XIN,INDEX,MODE,UU,VV,ZZ,SCR1,SCR2)
C ROUTINE TO COMPUTE FIELDS THAT REQUIRE DIFFERENTIATION USING
C THE POLYNOMIAL CURVE-FIT.

# include "rgrid.h"

       REAL SCR1(NX,NY),SCR2(NX,NY)
       REAL UU(NX,NY),VV(NX,NY),ZZ(NX,NY)
       REAL XIN(21,MAXSTN),B1(21),B2(21),B3(21)
       DATA PI/3.1415927/
C
       IF(IPAR.EQ.56.OR.IPAR.EQ.77) THEN
C DIVERGENCE OR STRETCHING DEFORMATION.
         CALL POLYFIT(U,XIN,INDEX,B1)
         CALL EVALDX(SCR1,B1,INDEX,MODE)
         CALL POLYFIT(V,XIN,INDEX,B2)
         CALL EVALDY(SCR2,B2,INDEX,MODE)
C DIVERGENCE.
         IF(IPAR.EQ.56) THEN
           DO 10 J=1,NY
           DO 10 I=1,NX
  10       ZZ(I,J)=SCR1(I,J)+SCR2(I,J)
         ENDIF
C STRETCHING DEFORMATION.
         IF(IPAR.EQ.77) THEN
           DO 50 J=1,NY
           DO 50 I=1,NX
  50       ZZ(I,J)=SCR1(I,J)-SCR2(I,J)
         ENDIF
       ENDIF
C
       IF(IPAR.EQ.57.OR.IPAR.EQ.78) THEN
C VORTICITY OR SHEARING DEFORMATION.
         CALL POLYFIT(U,XIN,INDEX,B1)
         CALL EVALDY(SCR1,B1,INDEX,MODE)
         CALL POLYFIT(V,XIN,INDEX,B2)
         CALL EVALDX(SCR2,B2,INDEX,MODE)
C VORTICITY.
         IF(IPAR.EQ.57) THEN
           DO 20 J=1,NY
           DO 20 I=1,NX
  20       ZZ(I,J)=SCR2(I,J)-SCR1(I,J)
         ENDIF
C SHEARING DEFORMATION.
         IF(IPAR.EQ.78) THEN
           DO 60 J=1,NY
           DO 60 I=1,NX
 60        ZZ(I,J)=SCR2(I,J)+SCR1(I,J)
         ENDIF
       ENDIF
C
       IF(IPAR.EQ.58) THEN
C ENERGY FLUX CONVERGENCE.
         CALL POLYFIT(U,XIN,INDEX,B1)
         CALL EVALDX(UU,B1,INDEX,MODE)
         CALL POLYFIT(V,XIN,INDEX,B2)
         CALL EVALDY(VV,B2,INDEX,MODE)
         CALL POLYFIT(Z,XIN,INDEX,B3)
         CALL EVAL(ZZ,B3,INDEX,MODE)
         DO 30 J=1,NY
         DO 30 I=1,NX
  30     ZZ(I,J)=ZZ(I,J)*(UU(I,J)+VV(I,J))
         CALL EVALDX(SCR1,B3,INDEX,MODE)
         CALL EVALDY(SCR2,B3,INDEX,MODE)
         CALL EVAL(UU,B1,INDEX,MODE)
         CALL EVAL(VV,B2,INDEX,MODE)
         DO 40 J=1,NY
         DO 40 I=1,NX
  40     ZZ(I,J)=ZZ(I,J)+UU(I,J)*SCR1(I,J)+VV(I,J)*SCR2(I,J)
       ENDIF
C
       IF(IPAR.EQ.88.OR.IPAR.EQ.999) THEN
C TOTAL DEFORMATION
         CALL POLYFIT(U,XIN,INDEX,B1)
         CALL POLYFIT(V,XIN,INDEX,B2)
C DU/DX
         CALL EVALDX(UU,B1,INDEX,MODE)
C DV/DX
         CALL EVALDX(VV,B2,INDEX,MODE)
C DU/DY
         CALL EVALDY(SCR1,B1,INDEX,MODE)
C DV/DY
         CALL EVALDY(SCR2,B2,INDEX,MODE)
         DO 70 J=1,NY
         DO 70 I=1,NX
         SHDEF=(UU(I,J)-SCR2(I,J))
         STDEF=(SCR1(I,J)+VV(I,J))
         IF(IPAR.EQ.88) THEN
C TOTAL DEFORMATION
           ZZ(I,J)=SQRT(SHDEF*SHDEF+STDEF*STDEF)
         ELSE
C AXIS OF DILATATION IN RADIANS.
           IF(SHDEF.GT.0.) THEN
             IF(SHDEF.LT.1.E-10) THEN
               ZZ(I,J)=PI/4.
             ELSE
               ZZ(I,J)=P1/4.-ATAN(STDEF/SHDEF)
             ENDIF
           ELSE
             IF(SHDEF.GT.(-1.E-10)) THEN
               ZZ(I,J)=3.*PI/4.
             ELSE
               ZZ(I,J)=3.*PI/4.-ATAN(STDEF/SHDEF)
             ENDIF
           ENDIF
C AXIS OF DILATATION IN DEGREES.
           ZZ(I,J)=ZZ(I,J)*180./PI
         ENDIF
  70     CONTINUE
       ENDIF
c
c Vorticity stretching (added 4/17/89 cb)
c
	if (ipar .eq. 143) then
c
c	Get the du/dx terms into array scr1
c
		call polyfit (u, xin, index, b1)
		call evaldx (scr1, b1, index, mode)
c
c	Get the dv/dy terms into array scr2
c
		call polyfit (v, xin, index, b2)
		call evaldy (scr2, b2, index, mode)
c
c	zz = du/dx + dv/dy (divergence)
c
		do 80, j = 1, ny
			do 90, i = 1, nx
				zz(i,j) = scr1(i,j) + scr2(i,j)
 90			continue
 80		continue
c
c	Get the du/dy terms into array scr2 and the dv/dx terms
c	into array scr1
c
		call evaldy (scr2, b1, index, mode)
		call evaldx (scr1, b2, index, mode)
c
c	zz = -divergence * (dv/dx - du/dy)
c	   = -divergence * vorticity		(vorticity stretching)
c
		do 100, j = 1, ny
			do 110, i = 1, nx
				zz(i,j) = -zz(i,j) * 
     .					(scr1(i,j) - scr2(i,j))
 110			continue
 100		continue
	endif
C
C COMPUTE U_WIND AND V_WIND AT GRID POINTS IF WIND VECTORS ARE OVERLAYED.
       IF(IPAROV.EQ.60.AND.IPAR.NE.58) THEN
         CALL EVAL(UU,B1,INDEX,MODE)
         CALL EVAL(VV,B2,INDEX,MODE)
       ENDIF
C
       RETURN
       END




       SUBROUTINE CFITDX(XX,YY,B,INDEX,VALUE)
C ROUTINE TO CALCULATE DERIVATIVE W.R.T. X OF THE POLYNOMIAL WHOSE
C COEFFICIENTS ARE B(*) AT (XX,YY).

# include "rgrid.h"

       REAL B(21),BX(20)
C
       X1=9.*(XX-XMIN)/(XMAX-XMIN)+1.
       Y1=9.*(YY-YMIN)/(YMAX-YMIN)+1.
       BX(1)=1.
       BX(2)=0.
       IF(INDEX.GT.3) THEN
         BX(3)=Y1
         BX(4)=2.*X1
         BX(5)=0.
         IF(INDEX.GT.6) THEN
           BX(6)=2.*X1*Y1
           BX(7)=Y1*Y1
           BX(8)=3.*X1*X1
           BX(9)=0.
           IF(INDEX.GT.10) THEN
             BX(10)=BX(7)*Y1
             BX(11)=2.*X1*BX(7)
             BX(12)=BX(8)*Y1
             BX(13)=4.*X1*X1*X1
             BX(14)=0.
             IF(INDEX.GT.15) THEN
               BX(15)=BX(7)*BX(7)
               BX(16)=2.*X1*BX(10)
               BX(17)=BX(8)*BX(7)
               BX(18)=BX(13)*Y1
               BX(19)=5.*X1**4
               BX(20)=0.
             ENDIF
           ENDIF
         ENDIF
       ENDIF
       VALUE=0.
       DO 10 I=1,INDEX-1
  10   VALUE=VALUE+B(I)*BX(I)
       VALUE=9.*VALUE/(XMAX-XMIN)
       RETURN
       END




       SUBROUTINE CFITDY(XX,YY,B,INDEX,VALUE)
C ROUTINE TO CALCULATE DERIVATIVE W.R.T. Y AT (XX,YY) OF POLYNOMIAL
C WHOSE COEFFICIENTS ARE B(*).

# include "rgrid.h"

       REAL B(21),BX(20)
C
       X1=9.*(XX-XMIN)/(XMAX-XMIN)+1.
       Y1=9.*(YY-YMIN)/(YMAX-YMIN)+1.
       BX(1)=0.
       BX(2)=1.
       IF(INDEX.GT.3) THEN
         BX(3)=X1
         BX(4)=0.
         BX(5)=2.*Y1
         IF(INDEX.GT.6) THEN
           BX(6)=X1*X1
           BX(7)=2.*Y1*X1
           BX(8)=0.
           BX(9)=3.*Y1*Y1
           IF(INDEX.GT.10) THEN
             BX(10)=BX(9)*X1
             BX(11)=BX(6)*BX(5)
             BX(12)=BX(6)*X1
             BX(13)=0.
             BX(14)=4.*Y1*Y1*Y1
             IF(INDEX.GT.15) THEN
               BX(15)=BX(14)*X1
               BX(16)=BX(9)*BX(6)
               BX(17)=BX(5)*BX(12)
               BX(18)=BX(6)*BX(6)
               BX(19)=0.
               BX(20)=5.*Y1**4
             ENDIF
           ENDIF
         ENDIF
       ENDIF
       VALUE=0.
       DO 10 I=1,INDEX-1
  10   VALUE=VALUE+B(I)*BX(I)
       VALUE=9.*VALUE/(YMAX-YMIN)
       RETURN
       END




       SUBROUTINE EVALDX(GRID,B,INDEX,MODE)
C COMPUTE DERIVATIVE W.R.T. X AT ALL GRID POINTS.

# include "rgrid.h"

       REAL GRID(NX,NY),B(21)
C
       DO 100 J=1,NY
       XX=X0
       YY=Y0+(J-1)*DY
       DO 100 I=1,NX
       IF(JW(I,J).GE.0) THEN
         CALL CFITDX(XX,YY,B,INDEX,GRID(I,J))
       ELSE
         IF(MODE.NE.0) GRID(I,J)=FLAG
       ENDIF
       XX=XX+DX
  100  CONTINUE
       RETURN
       END




       SUBROUTINE EVALDY(GRID,B,INDEX,MODE)
C COMPUTE DERIVATIVE W.R.T. Y AT ALL GRID POINTS.

# include "rgrid.h"

       REAL GRID(NX,NY),B(21)
C
       DO 100 J=1,NY
       XX=X0
       YY=Y0+(J-1)*DY
       DO 100 I=1,NX
       IF(JW(I,J).GE.0) THEN
         CALL CFITDY(XX,YY,B,INDEX,GRID(I,J))
       ELSE
         IF(MODE.NE.0) GRID(I,J)=FLAG
       ENDIF
       XX=XX+DX
  100  CONTINUE
       RETURN
       END
