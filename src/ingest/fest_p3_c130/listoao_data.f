Ftn7x,l
      Program Listoao_Data

C  Program to make a listing of an OAO Standard Tape

      DIMENSION BUF(106),ID(21),LUS(2)

      DATA ID /4,5,18,17,19,20,37,38,6,7,31,25,15,16,8,9,63,64,13,65,14/
      DATA LU/1/, LPP/55/,NLINE/0/,NPAGE/0/

      WRITE (1,10)
   10 FORMAT ('RFC Standard tape list'/'Enter Stime,Etime Print rate Tap
     1e lu Printer lu'/'hhmmss hhmmss rrr T PP')   
      READ (1,11) IHS,IMS,ISS,IHE,IME,ISE,RATE,LUT,LUP
   11 FORMAT (3I2,1X,3I2,1X,F3.0,1X,I1,1X,I2) 
      LUS(1)=LUT
      LUS(2)=LUP
      CALL LURQ(1,LUS,2)
      STIME=TIMZ(IHS,IMS,ISS) 
      ETIME=TIMZ(IHE,IME,ISE) 

      Call Io9 (4,Lut,0,0,Istat)    ! rewind the tape drive

    1 CALL RDRST(BUF,ISTAT,LUT)

      IF (ISTAT .LT. 0) GO TO 999 
      IH=BUF(3)/10000.0 
      IM=(BUF(3)-FLOAT(IH)*10000.0)/100.0 
      IS=BUF(3)-FLOAT(IH)*10000.0-FLOAT(IM)*100.0 
      IF (IH .GT. 24 .OR. IM .GT. 60 .OR. IS .GT. 60) GO TO 1 
      TIME=TIMZ(IH,IM,IS) 
      CALL CHKTM(TIME)
      CALL CTME(TIME,IH,IM,IS)
      IF (TIME .LT. STIME) GO TO 1
      IF (TIME .GT. ETIME) GO TO 888
      IF (AMOD(TIME,RATE) .NE. 0.0) GO TO 1 
      BUF(3)=FLOAT(IH)*10000.0 + FLOAT(IM)*100.0 + FLOAT(IS)
      IF (NLINE .GT. 0) GO TO 3 
      NPAGE=NPAGE + 1 
      If (Lup .eq. 12) Write (Lup,'(1H)')
      WRITE (LUP,20) BUF(1),BUF(2),BUF(11),BUF(62),NPAGE
   20 FORMAT(1H1,10X,'FLIGHT #=',F6.0,' AIRCRAFT=',
     1 F2.0,2X,'TEMP PROBE #',F1.0,2X,'INE #',F1.0,50X,'PAGE=',I4// 
     12X,4HTIME,4X,3HLAT,5X,4HLONG,4X,4HHEAD,2X,3HTRK,4X,3HPIT,2X,4HROLL
     2,2X,2HGS,4X,3HTAS,3X,2HRA,3X,2HPA,3X,2HRH,2X,5HPRESS,2X,2HWD,4X 
     3,2HWS,3X,2HJW,4X,2HVW,2X,5HTEMP1,1X,5HTEMP2,2X,4HR.SD,2X,2HTD,3X, 
     34HR.DN/10X,3Hdeg, 
     4 5X,3Hdeg,5X,3Hdeg,3X,3Hdeg,4X,3Hdeg,2X,3Hdeg,3X,3Hm/s,3X,3Hm/s,  
     54X,1Hm,4X,1Hm,4X,1H%,3X,2Hmb,4X,3Hdeg,3X,3Hm/s,1X,4Hg/m3,3X,3Hm/s,
     6 4X,1HC,5X,1HC,5X,1HC,4X,1HC,5X,1HC)
    3 WRITE (LUP,40) IH,IM,IS,(BUF(ID(I)),I=1,21) 
   40 FORMAT (1X,3I2.2,F8.3,F9.3,6F6.1,1X,F4.0,1X,F4.0,1X,F4.0, 
     1  F6.1,F6.1,2F5.1,6F6.1)
      NLINE=NLINE + 1 
      IF (MOD(NLINE,LPP) .EQ. 0) NLINE=0
      GO TO 1 
  999 IF (ISTAT .EQ. -1 .OR. ISTAT .EQ. -3) GO TO 1 
      WRITE (1,60) ISTAT
   60 FORMAT ('ISTAT=',I3,' PROGRAM ABORTING')
      STOP 2
  888 WRITE (LUP,12)
      If (Lup .eq. 12) Write (Lup,'(1H)')
   12 FORMAT (1H1)
      Call Exit
      END 
