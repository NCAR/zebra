FTN7X,L
      PROGRAM COPYOAO_DATA

c  Program to make an ASCII formatted tape from the OAO flight level
c  standard tape

      DIMENSION BUF(106),IBUF(70),LUS(2),NBUF(70,30)
      Integer*4 nrecs

c  Kq is the number of seconds of data to put into 1 physical tape
c  record

      Kq = 5
      WRITE (1,'("Enter input,output tape lus:_")')
      READ (1,*) LUR,LUW

      LUS(1)=LUR
      LUS(2)=LUW
      CALL LURQ(1,LUS,2)

      WRITE (1,'("Enter Start time, End time"/"XXXXXX XXXXXX")')
      READ (1,'(3i2,1x,3i2)') IHS,IMS,ISS,IHE,IME,ISE 

      STIME=TIMZ(IHS,IMS,ISS) 
      ETIME=TIMZ(IHE,IME,ISE) 

      NRC=0 
      NRECS=0.0

    1 CALL RDRST(BUF,ISTA,LUR)
      IF (ISTA .EQ. -1 .or. ista .eq. -6) GO TO 1
      IF (ISTA .LE. 0) GO TO 999

      DATE=BUF(1)
      TIM =BUF(3) 
      TIME=TMCON(TIM )  
      Call Chktm(time)

      IF (TIME .LT. STIME) GO TO 1
      IF (TIME .GT. ETIME) GO TO 999

      if (amod(time,60.0) .eq. 0.0) write (1,'(3f8.0)') date,tim,time

      PLAT=BUF(4) 
      PLON=BUF(5) 
      TEMP1=BUF(63) 
      TEMP2=BUF(64) 
      CO2=BUF(13) 
      DEWP=BUF(12)
      VW=BUF(9)
      AJW=BUF(8)
      WD=BUF(15)
      WS=BUF(16)
      PR=BUF(25)
      RA=BUF(6) 
      PA=BUF(7) 
      TK=BUF(17)
      HD=BUF(18)
      SST=BUF(14) 
      PITCH=BUF(19) 
      ROLL=BUF(20)
      ATCAN=BUF(21) 
      SLIP=BUF(22)
      GS=BUF(37)
      TAS=BUF(38) 

      Call Ctme(Time,Ih,Im,Is)
      CALL CODE 
      WRITE (IBUF,11) DATE,Ih,Im,Is,PLAT,PLON,TEMP1,TEMP2,DEWP,CO2,VW,
     1 AJW,WD,WS,PR,RA,PA,TK,HD,SST,PITCH,ROLL,ATCAN,SLIP,GS,TAS
  11  FORMAT (F6.0,3i2.2,F7.3,F8.3,8F5.1,F7.1,2F6.0,9F6.1)

      NRC=NRC+1 

      DO I=1,70
         NBUF(I,NRC)=IBUF(I) 
      End Do

      NRECS=NRECS+1   

      IF (NRC .LT. Kq) GO TO 1

      NRC=0 
      CALL IO9(1,LUW,NBUF,Kq*70,ISTAT)    
      IF (ISTAT .NE. Kq*70) GO TO 888   

      GO TO 1 

 999  If (Nrc .gt. 0 .and. Nrc .lt. Kq) Then
         Do j=nrc+1,kq
            Do i=1,70
               Nbuf(i,j) = 2H           ! fill in with blanks
            End Do
         End Do

         CALL IO9(1,LUW,NBUF,Kq*70,ISTAT)    
      End If

      CALL IO9(5,LUW,NBUF,0,ISTAT)   ! Write EOF on output tape
      CALL IO9(4,LUR,NBUF,0,ISTAT)   ! Rewind input tape

      WRITE (1,200) NRECS,ISTA,TIM 
  200 FORMAT ('Number of records written:',I8,1X, 
     1 'STATUS=',I4,1X,'TIME:',F8.0)
      Call Exit

  888 WRITE (1,201) LUW,ISTAT 
  201 FORMAT ('Abnormal error on output tape',I5,1X,I5) 
      Call Exit

      END 
