      SUBROUTINE BINTS(Z,ISIZE,JSIZE,XSTA,YSTA,DATA,DZ,DZR,NSTA,
     X                 IP,R,RMX,NQD,NLFLT,BAD)
C 
c $Id: bints.f,v 1.1 1994-06-22 19:29:55 corbet Exp $
c
C Implementation of the Barnes objective analysis technique:
C
C          Barnes,S.L., 1964: A technique for maximizing details in
C             numerical weather map analysis. J. Appl. Meteor.,
C             3, 396-409.       
C
C         ------ ,1973: Mesoscale objective map analysis using weighted
C             time-series observations. NOAA Tech. Memo. ERL NSSL-62,
C             60 pp. [ NTIS COM-73-10781 ].
C
C Leise Telescoped Filter:
C
C         Leise, J.A., 1981: A multidimensional scale-telescoped
C             filter and data extension package. NOAA Tech. Memo.
C             ERL/WPL-82, Wave Propogation Lab, Boulder, 20pp.
C
C History:
C    1980 -- Programmed by Jim Heimbach, Montana State Univ.
C    1981 -- Converted to the NCAR CRAY BY Tom Engle, CRAY/NCAR
C    1982 -- Enhanced by Carl Mohr, NCAR/CSD for use
C            with CCOPE data as documented in:
C
C         Mohr,C.G., L.J. Miller, R.L. Vaughan and H.W. Frank, 1986:
C             The merger of mesoscale datasets into a common Cartesian
C             format for effiecient and systematic analyses.
C             J. Atmos. and Ocean. Tech., Vol. 3, No. 1, 
C             (March, 1986), 144-161.
C
C    1989 -- Cleaned up for general usage by Carl Mohr (NCAR/ATD)
C            In this version 2 features have been disabled...
C               1- Non-spatial differential weighting (such as
C                  utilized for off-time or noisy reports) is
C                  not implemented -- it's a red herring anyway.
C               2- In a similar vein, adaptive adjustment of the
C                  radius of influence is also disabled. Sure,
C                  it preserves the scales of the original when
C                  reports are non-uniformly spaced. The resultant
C                  gridded field is also a noisy mess requiring post-
C                  filtering. 
C
C    Questions???
C                  Carl Mohr 
C                  NCAR/ATD/RAP
C                  P.O. Box 3000
C                  Boulder, CO 80307-3000
C                  (303) 497-8968
C                  E-mail: mohr@ncar.ucar.edu
C       
C
C       An implementation of the Barnes Objective Analysis Technique is 
C    employed for remapping the irregularly spaced input values to the uniform
C    2-D Cartesian grid. At each destination grid location all input values,
C    no matter how distant, are included in the calculation of the estimate.
C    The weight W assigned to any datum at distance D from the destination 
C    Cartesian coordinate location is given by the following gaussian
C    function:
C
C       W = EXP (ln(0.1) * D**2 / R**2 )
C
C    where R is the distance (in kilometers) where the gaussian weighting 
C    function will be equal to 1/10. Every input value
C    is multiplied by it's calculated weight, all weighted values are 
C    accumulated and a final normalized estimate is assigned to the grid
C    location in question. R controls the response of the gaussian 
C    weighting function. Satisfactory results are usually attained with
C    R set approximately equal to the maximum spacing of the input values.
C
C       An iterative correction technique for reducing the Root Mean
C    Square (RMS) difference between the original input values and the nearest
C    Cartesian estimates may also be invoked. If IP is set greater than 1, 
C    the residual between each original datum and the
C    Cartesian field estimate is computed immediately following the first
C    pass (described above) of the objective analysis procedure. Using the
C    set of residuals as input, the objective analysis procedure is invoked
C    and the resultant field of gridded residuals is used to adjust the
C    current grid of Cartesian estimates. This operation is repeated until
C    IP iterations have been performed or if the gross residual starts to
C    increase. Experience with this feature suggests a value for IP of 
C    2 to 4.
C
C       The capability for 2-D filtering of the remapped Cartesian estimates
C    using Leise's Multi-dimensional Telescoped Filter (see references) is also
C    available. Each iteration of the filter will reduce the scales retained
C    by a factor of two. 2-D filtering of the Cartesian space estimates should
C    NOT be necessary if all preceeding computational steps have been properly
C    parameterized.
C
C       A final option for throwing out "unbounded" gridded estimates beyond
C    a specified distance from original input reports is also available.
C    This is done using two criteria:
C          1) Gridded estimate is first tested to see if it is "bounded";
C             the user specifies the minimum acceptable number of quadrants
C             in which original input reports must be present with respect
C             to the (X,Y) grid location. 
C          2) If the gridded estimate IS bounded, it is retained. If
C             it is NOT bounded, then an additional check is performed to 
C             see if it is within a user-specified distance of an input
C             report. If it fails that test, the gridded estimate is
C             replaced with the missing data flag.
C
C       A "backdoor" also exists for relocating each input report to 
C    the nearest grid location in the output array. See the description
C    of the variable called IP if you wish to invoke this option.
C
C
C    Restrictions:
C
C    -- All spatial variables must be normalized to the
C       index space of the output grid by the calling program.
C    -- Grid spacing along the X and Y axes must be equal.
C    -- Missing data will be represented by the variable BAD
C       (on both input and output).
C    
C    
C    Variables:
C
C      Z- (OUTPUT)  Two dimensional array of gridded estimates;
C                   dimensioned (ISIZE x JSIZE) 
C  ISIZE- ( INPUT)  X-dimension of Z array 
C  JSIZE- ( INPUT)  Y-dimension of Z array 
C   XSTA- ( INPUT)  One dimensional array of input report X-locations
C                   -- normalized to INDEX space (in GRID units);
C                   dimensioned to NSTA
C   YSTA- ( INPUT)  One dimensional array of input report Y-locations
C                   -- normalized to INDEX space (in GRID units);
C                   dimensioned to NSTA
C   DATA- ( INPUT)  One dimensional array of input report values;
C                   dimensioned to NSTA
C     DZ- (SCRATCH) One dimensional array; dimensioned to NSTA
C    DZR- (SCRATCH) One dimensional array; dimensioned to (ISIZE x JSIZE)
C   NSTA- ( INPUT)  Dimension of 1-D arrays described above;
C                   corresponds to the number of irregularly spaced
C                   input reports to be remapped to the Z grid.
C     IP- ( INPUT)  Number of passes. The first pass of the remapping generates
C                   an estimate at each grid location using the distance 
C                   weighting procedure described above. If IP is greater
C                   than 1, the residual between each original data value and
C                   the nearest Cartesian field estimate is computed 
C                   immediately after the first pass of the objective analysis.
C                   Using the residuals as input, the analysis is performed 
C                   again and adjustments are made to the current grid of
C                   Cartesian estimates. This iterative improvement procedure
C                   is performed IP-1 times. Suggest a value from 2 to 4.
C                   --- Special backdoor for choosing closest point
C                       instead of the objective analysis procedure:
C                          If IP is less than or equal to 0; each input
C                          report will be mapped to the nearest grid 
C                          location in the output array. All other
C                          objective analysis parameterization will be
C                          ignored. 
C      R- ( INPUT)  Radius of "influence". Distance in GRID units
C                   at which the exponential weighting function = 0.1
C                   Weights are assigned to each input datum 
C                   according to it's distance from the destination Cartesian
C                   location in the 2-D grid. The final estimate at the 
C                   Cartesian location is computed by multiplying each input
C                   datum by the weight, summing the results and normalizing
C                   by the sum of all the weights. The weight W assigned to any
C                   datum at distance D from the destination Cartesian grid
C                   location is given by:
C                       W = EXP ( ln(0.1) * D**2 / R**2 )
C                   and is in the range 0 to 1. Suggest a value approximately
C                   equal to the coarsest spacing of the input reports.
C    RMX- ( INPUT)  Maximum distance in GRID units to extrapolate estimates. 
C                   Unbounded grid locations (as determined by NQD - below)
C                   beyond this distance from an input report will be set
C                   to the missing data flag just prior to procedure exit.
C    NQD- ( INPUT)  Gridded array locations with original input reports
C                   contained in fewer than NQD quadrants around the location
C                   will be identified as unbounded and subject to decimation
C                   based upon the value of RMX - above. Suggest a value
C                   of 3 or 4 if you wish to utilize this feature.
C  NLFLT- ( INPUT)  Number of 2-D Leise filtering steps to perform on
C                   the gridded estimates. Each step reduces the retained
C                   scale sizes by a factor of 2. Suggest NOT using this
C                   feature; increase the value of R instead to produce
C                   a smoother result. 
C    BAD- ( INPUT)  Missing data flag. Missing data in the input report
C                   array (DATA) should be represented by this value.
C                   Missing data in the 2-D output array (Z) as determined 
C                   by decimation criteria will be set to this as well.
C                    
C
      DIMENSION Z(ISIZE,JSIZE),XSTA(1),YSTA(1),DATA(1),DZ(1),IQUAD(4)
      DIMENSION DZR(ISIZE,JSIZE),RQUAD(4)
C
C        Transfer function is designed to perform gaussian weighting
C           with 0.1 assigned at radius R (2.3025 is -ln(0.1) ).
C           CHNG can be used to alter the response as follows:
C                    CHNG.LT.1 (Faster rolloff) or
C                    CHNG.GT.1 (Slower rolloff) .
C         
      DATA CHNG/1.0/
      DATA BIG/1.0E+10/
C
C...Use average station value as first guess -- hardwired.
C
      DATA IG/1/
C
C...Set RDSPDX to R -- disables the adaptive radius scheme.
C             
      RDSPDX = R
c cc - 2/28/92
c ... activate adaptive adjustment of the radius of influence. 
c     It preserves the scales of the original when reports are 
c     non-uniformly spaced.  (The resultant gridded field is also 
c     a noisy mess requiring post-filtering.)
c
c      RDSPDX = R+4.
C
      RFAC = ALOG(0.1)/CHNG
      RMS  = BIG
C   
C...Check if closest point option is enabled (IP.LE.0)
C
      IF(IP.LE.0) THEN
         CALL CONFLD(Z,ISIZE*JSIZE,BAD)
         CALL CONFLD(DZR,ISIZE*JSIZE,BIG)
         DO 30 M=1,NSTA
            IF(DATA(M).EQ.BAD) GO TO 30
            I=NINT(XSTA(M))
            J=NINT(YSTA(M))
            IF(I.LT.1.OR.I.GT.ISIZE) GO TO 30
            IF(J.LT.1.OR.J.GT.JSIZE) GO TO 30
            R2= (XSTA(M)-I)**2 + (YSTA(M)-J)**2
            IF(R2.GT.DZR(I,J)) GO TO 30
            DZR(I,J)=R2
            Z(I,J)=DATA(M)
   30    CONTINUE
         GO TO 90
      END IF
C
C...Proceed with regular objective analysis
C
C
C...Initialize radius array to the primary radius
C
      RT2=R*2.0
      RREG=1./(R*R)
      CALL CONFLD(DZR,ISIZE*JSIZE,RREG)
C
      IF(RDSPDX.GT.R) THEN
C
C...Adaptive radius technique -- determine the radius at each grid location
C
         DO 50 J=1,JSIZE
         DO 50 I=1,ISIZE
C
         Y=J
         X=I
C
C
C......Compute a radius depending upon the data density
C
            DO 40 L=1,4
               RQUAD(L)=BIG
   40       CONTINUE
C
            DO 45 M=1,NSTA
               IF(DATA(M).EQ.BAD) GO TO 45
               XOF=XSTA(M)-X
               YOF=YSTA(M)-Y
               IND=SIGN(1.0,XOF)+SIGN(0.5,YOF)+3.0
               RDN=SQRT(XOF*XOF+YOF*YOF)
               RQUAD(IND)=AMIN1(RDN,RQUAD(IND))
   45       CONTINUE
            RDN= -1.0
            DO 46 L=1,4
C
c.........Select the max distance to any quadrant (may be less than 4)
C
               IF(RQUAD(L).EQ.BIG) GO TO 46
               RDN=AMAX1(RDN,RQUAD(L))
   46       CONTINUE
            RDN=AMIN1(RDN,RDSPDX)
            RDN=AMAX1(RDN,R)
C
         DZR(I,J)=1./(RDN*RDN)
C
   50    CONTINUE
C
      END IF
C
C...Guess at initial field -- IG=1, use average station value
C                             IG=2, provided by calling routine in Z array
C...Hardwired to 1 (7/89 -- cgm)
C
      GO TO (1,2),IG
    1 CONTINUE
      DO 3 I=1,ISIZE
      DO 3 J=1,JSIZE
         Z(I,J)=0.0
    3 CONTINUE
C
C...Perform IP passes -- main loop of analysis 
C
    2 CONTINUE
C
      DO 4 IPASS=1,IP
         OLDRMS=RMS
         RMS=0.0
         NRMS=0
C
C...If average of data is used as first guess and currently on first
C      pass, use all data points.
C
         IF(IPASS.GT.1.OR.IG.GT.1) GO TO 6
C
         DO 10 ISTA=1,NSTA
            DZ(ISTA)=BAD
            IF(DATA(ISTA).EQ.BAD) GO TO 10
            DZ(ISTA)=DATA(ISTA)
            RMS=RMS+(DATA(ISTA)**2)
            NRMS=NRMS+1
   10    CONTINUE
C
         GO TO 11
C
C...Find correction factors.
C
    6 CONTINUE
C
         DO 5 ISTA=1,NSTA
            DZ(ISTA)=BAD
            IF(DATA(ISTA).EQ.BAD) GO TO 5
            I=XSTA(ISTA)
            J=YSTA(ISTA)
C
C...If station is outside of the specified grid, can't generate a 
C      corrector estimate for it.
C
            IF(I.LE.0.OR.J.LE.0) GO TO 5
            IF(I.GE.ISIZE.OR.J.GE.JSIZE) GO TO 5
C
C......Linear interpolation to estimate station location values
C
            W1=XSTA(ISTA)-I
            W2=YSTA(ISTA)-J
            W3=1.-W1
            W4=1.-W2
            DZ(ISTA)=DATA(ISTA)-( (Z( I , J )*W3+Z(I+1, J )*W1)*W4 +
     X                            (Z( I ,J+1)*W3+Z(I+1,J+1)*W1)*W2  )
            RMS=RMS+(DZ(ISTA)**2)
            NRMS=NRMS+1
    5    CONTINUE
C
   11    CONTINUE
C
C...Check to verify continued convergence
C
         IF(NRMS.LE.0) THEN
            PRINT 129
  129       FORMAT(/' INSUFFICIENT DATA FOR CORRECTOR ITERATION.'/)
            GO TO 65
         END IF
C
         RMS=SQRT(RMS/NRMS)
         IF(OLDRMS.GE.RMS) GO TO 12
         PRINT 130, IPASS,IP
130      FORMAT(/' RMS RESIDUAL INCREASED BEFORE SPECIFIED NUMBER OF',
     X           ' PASSES COMPLETED.',
     X   /' PASS NUMBER = ',I2,'. TOTAL NUMBER SPECIFIED = ',I2,'.',
     X   /' ITERATION STOPPED AT THIS POINT.'/)
         GO TO 65     
C
C...Still converging. On a grid point by grid point basis, compute
C         weights, calculate net corrections and apply them.
C         The weights are recalculated on each pass to save space.
C
   12    CONTINUE
C
C...Initiate the objective analysis procedure for this pass.
C
         DO 7 I=1,ISIZE
         DO 7 J=1,JSIZE
            SUMW=0.0
            SUM=0.0
            DO 8 ISTA=1,NSTA
               IF (XSTA(ISTA).NE.BAD.AND.YSTA(ISTA).NE.BAD) THEN
                  R2 = ( (XSTA(ISTA)-I)**2 + (YSTA(ISTA)-J)**2 )
               ELSE
                  R2 = BIG
               END IF
C
C...Check for underflow.  If no stations are within decent range,
C         this scheme will give a simple arithmetic average.
C
               W=AMAX1(R2*RFAC*DZR(I,J),-20.0)
               IF (DZ(ISTA).NE.BAD) THEN
                  W = EXP(W)
               ELSE
                  W = 0.0
               END IF
C
               SUM=SUM+(W*DZ(ISTA))
               SUMW=SUMW+W
    8       CONTINUE
C
            Z(I,J)=Z(I,J)+(SUM/SUMW)
C
    7    CONTINUE
C
    4 CONTINUE
C
   65 CONTINUE
C
c write the grided data to a file
c
c      do 141,js=1,jsize
c       do 141 is=1,isize
c            write(23,'(2i4,f9.3)') is,js,z(is,js)
c  41    format(' i,j,z ',2i5,f9.4)
c 141  continue

C
C..Analysis completed- check for filtering/decimation options.
C
         IF(NLFLT.GT.0) THEN
C
C......Leise filtering of the output field.
C
            CALL T5FLTR(Z,ISIZE,JSIZE,1,NLFLT)
         END IF
C
         IF(RMX.LE.0.0.AND.NQD.LE.0) GO TO 90
C
C......Decimation of distant unbounded grid locations.
C
         RMXSQ=RMX*RMX
         IF(RMX.LE.0.0) RMXSQ=BIG
         DO 80 J=1,JSIZE
         DO 75 I=1,ISIZE
            DO 66 L=1,4
               IQUAD(L)=0
   66       CONTINUE
            KNTR=0
            DO 70 ISTA=1,NSTA
               IF(DATA(ISTA).NE.BAD) THEN
                  XOF=XSTA(ISTA)-I
                  YOF=YSTA(ISTA)-J
                  IND=SIGN(1.0,XOF)+SIGN(0.5,YOF)+3.0
                  IQUAD(IND)=IQUAD(IND)+1
                  R2=XOF*XOF+YOF*YOF
                  IF(R2.LE.RMXSQ)
     X               KNTR=KNTR+1
               END IF
   70       CONTINUE
            IF(NQD.GT.0) THEN
               KNTQD=0
               DO 71 L=1,4
                  IF(IQUAD(L).GT.0) THEN
                     KNTQD=KNTQD+1
                  END IF
   71          CONTINUE
               IF(KNTQD.GE.NQD) GO TO 75
            END IF
            IF(KNTR.GT.0) GO TO 75
            Z(I,J)=BAD
   75    CONTINUE
   80    CONTINUE
C
   90 CONTINUE
C
      RETURN
      END
      SUBROUTINE CONFLD(RBUF,NPLANE,CON)
C
C        FILLS AN ARRAY WITH A CONSTANT VALUE- CON
C
      DIMENSION RBUF(1)
      DO 10 I=1,NPLANE
         RBUF(I)=CON
   10 CONTINUE
      RETURN
      END
      SUBROUTINE T5FLTR(Y,N1,N2,N3,NSTEP)
C                                               JIM LEISE 8/80
C    *****************************************************************
C    HELLO,
C    I AM A MULTIDIMENSIONAL LOW-PASS FILTER WHICH NEEDS NO EXTRA
C    ARRAY SPACE.  THUS, THE FILTERED ANSWER IS RETURNED IN THE SAME
C    ARRAY Y(N1,N2,N3) THAT THE DATA IS INPUT.  THE CENTRAL FILTER
C    IS A LINEAR 5-PT FILTER AND THE BOUNDARY FILTER IS COMPUTED
C    USING A MIRROR EXTENSION.  THUS, THE TOTAL FILTER IS LINEAR.
C
C          ********** NSTEP CONTROL FOR 1-DIM **********
C        STEP RESTRICTION:  5*2**(NSTEP-1) .LE. MAX(N1,N2,N3)
C         PASSBAND .LE. 2**(NSTEP+2)  POINTS/CYCLE
C         STOPBAND .GE. 2**(NSTEP)  POINTS/CYCLE.
C
C          ********** MULTIDIMENSIONAL USE **********
C    PARAMETER CONTROL FOR THE THREE DIMENSIONS CAN BE REALIZED
C    VIA COMMON/FLTRPL/ WHERE NS CORRESPONDS TO NSTEP.  IF THIS
C    COMMON IS NOT USED, THE VALUES OF NS ARE DEFAULTED TO NSTEP
C    -I.E. NSTEP IS USED IN PLACE OF ANY ZEROS.
C    ******************************************************************
C
         DIMENSION Y(1),KORD(5),NET(5),NNS(3)
         COMMON/FLTRPL/NS(3)
C    INITIALIZATION OF NS FOR CSD APPLICATIONS (4/15/82)
         DATA NS/0,0,0/
C
C    INITIALIZE THE 3-D ARITHMETIC.
         NDIM=1
         IF(N2.GT.1)NDIM=2
         IF(N3.GT.1)NDIM=3
         KORD(1)=MAX0(1,N1)
         KORD(2)=MAX0(1,N2)
         KORD(3)=MAX0(1,N3)
         KORD(4)=KORD(1)
         KORD(5)=KORD(2)
         NET(1)=1
         NET(2)=KORD(1)
         NET(3)=KORD(1)*KORD(2)
         NET(4)=NET(1)
         NET(5)=NET(2)
C
C    DEFAULT PARAMETER TRANSFER.
         MPYRMD=0
         DO 10 N=1,NDIM
         NNS(N)=NS(N)
         IF(NS(N).EQ.0)NNS(N)=NSTEP
         IF(KORD(N).LT.5)NNS(N)=0
 10      MPYRMD=MAX0(MPYRMD,NNS(N)+NNS(N)-1)
         IF(MPYRMD.LE.0)RETURN
         MSTEP=(MPYRMD+1)/2
C
C    ***** START THE MAIN LOOP *****
         K1=1
         DO 50 MAIN=1,MPYRMD
         DO 40 N=1,NDIM
C    SAMPLING CHECKS.
         IF(10*K1.GT.KORD(N))NNS(N)=MIN0(NNS(N),MAIN)
         IF((MAIN.GE.NNS(N)).AND.(MPYRMD-MAIN.GE.NNS(N)))GO TO 40
C
C    THE 3-D ARITHMETIC.
         M1=K1*NET(N)
         M2=M1+M1
         M3=M2+M1
         ISTOP=KORD(N+1)
         JSTOP=KORD(N+2)
         DO 30 I=1,ISTOP
         DO 30 J=1,JSTOP
         KSTRT=1+(I-1)*NET(N+1)+(J-1)*NET(N+2)
         KSTOP=KSTRT+(KORD(N)-1)*NET(N)
         KN=KSTRT-NET(N)
         DO 30 K=1,K1
         KN=KN+NET(N)
         LN=KN+((KSTOP-KN)/M1)*M1
C
C    FILTER THE ENDS USING A MIRROR EXTENSION.
         YKN=.875*Y(KN)+.1875*Y(KN+M1)-.0625*Y(KN+M2)
         YLN=.875*Y(LN)+.1875*Y(LN-M1)-.0625*Y(LN-M2)
         YKN1=.1875*Y(KN)+.625*Y(KN+M1)+.25*Y(KN+M2)-.0625*Y(KN+M3)
         YLN1=.1875*Y(LN)+.625*Y(LN-M1)+.25*Y(LN-M2)-.0625*Y(LN-M3)
C
C    DO THE CENTRAL 5-PT FILTER.
         YM2=Y(KN)
         YM1=Y(KN+M1)
         MSTRT=KN+M2
         MSTOP=LN-M2
C
         DO 20 M=MSTRT,MSTOP,M1
         YSAVE=Y(M)
         Y(M)=.625*Y(M)+.25*(YM1+Y(M+M1))-.0625*(YM2+Y(M+M2))
         YM2=YM1
 20      YM1=YSAVE
C
         Y(KN+M1)=YKN1
         Y(LN-M1)=YLN1
         Y(KN)=YKN
         Y(LN)=YLN
C
 30      CONTINUE
 40      CONTINUE
C    UPDATE THE SAMPLING INCREMENT.
         K1=K1+K1
         IF(MAIN.GE.MSTEP)K1=K1/4
 50      CONTINUE
C
         RETURN
      END
