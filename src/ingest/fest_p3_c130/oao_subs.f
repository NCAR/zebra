c                           <921029.1239>
      Subroutine Io9 (Ifunc,Lun,Ibuf,Nwords,Stat),Tape I/O & Positioning
  
c  The device drivers for the tape units should be either dd*23 or
c     dd*24 (there is just one case of the later: see how LuDD24
c     is defined).
  
c Entry conditions
C
C  Ifunc=0 - READ BINARY
C       =1 - Write BINARY
C       =2 - BACKSPACE 1 RECORD
C       =3 - SKIP FOREWARD 1 RECORD
C       =4 - REWIND
C       =5 - Write AN End OF FILE MARK
C       =6 - SKIP FOREWARD 1 FILE
C       =7 - BACKSPACE 1 FILE
C       =8 - REWIND and take off line
C       =9 - Change density (7980 drive only)
C       =10- Get Status
c  Lun: Mag tape Lu
c  Ibuf: integer*2 buffer which is only used when Reading or Writing.
c  Nwords: integer*2 = number of words in Ibuf to be Read or Written;
c     not used for the other functions.
c  Stat: if > 0, then this is the number of words actually Read/Written.
c     Consequently it will be <= Nwords.
c         if = 0, then for Read/Write functions, no words were transferred.
c         if = 0 for the other functions, then this means that the operation
c             was successfully accomplished.
c         if < 0, then this is an error/status code.  This code is based on
c             what is returned in the A register (from sub ABreg) and/or from
c             the parameters returned by sub Rmpar following the Exec call.
c             It is possible that multiple status-items can occur; however
c             only one code can be reported. For example, the user trys to
c             write 5000 words from Ibuf.  If the end-of-tape is sensed while
c             writing the 5000 words, then even if the full 5000 are written
c             stat will return -3 meaning End-of-tape.
c             The value of Stat is primarily taken from Istat, the lowest 6
c             bits in the first status word returned by RMPAR.  The third
c             status word from RMPAR as well as the A-register value also
c             contribute to the value of Stat.   Soft=recoverable errors
c             are ignored, i.e., these data transfers are considered good.
  
C  Correlation of Istat to the status/error codes returned in Stat:
  
 
C  Istat                    Stat
C  -----                    -----
C    0 - No Error            ---
C    1 - Illegal Request     -7
C    2 - Not Ready           -5
C    3 - Time Out            -6
C    4 - End of Tape         -3
C        End of File         -2
C    5 - Transmission/
C        Parity Error        -1  (will cause skip forward one record if
C                                 Reading or will cause a 3.5 gap if Writing)
C    6 - Write Protected     -4
C   63 - Driver Request Restart (driver 23b)
C                            -8  If more than IrepsMax requests
C
C   >6 - Undetermined Error  -9
  
C  A subroutine is provided (Tape_Error) which will report these
C    errors to a designated Lu (note: Stat = -8 is tied to the value in
C    IrepsMax in this subroutine.
      Parameter (IrepsMax = 5) ! max "driver request restarts" permitted
      Parameter (LUDD24 = 9) ! lu using DD*24
  
      Dimension Ibuf(*)
  
      Integer Param(5), Stat
  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  
C  Legal function?
  
      If (Ifunc .lt. 0 .or. Ifunc .gt. 10) Go To 999
  
      Ireps = 0 ! number of times action has been attempted.
  
c  Not a Read or Write request?
 
      If (Ifunc .ge. 2) Go To 1
  
C  Do the Read or Write
  
      Icode = Ifunc + 1
  
  100 Call Exec (Icode,064100B+Lun,Ibuf,Nwords)
!       064100B: Non-buff,User-error handler,stream,binary
  
C  Find out if there were any errors
  
      Call Abreg (Ia,Ib)
      Call Rmpar (Param)
      Ireps = Ireps + 1
      IDev_type = Ibits (Ia,8,6) ! device type, 23b for 7970, 24b for 7980
  
      Istat = Iand(Param(1),77B)
  
      If ((Istat .eq. 63) .and.(Idev_Type .eq. 23b)) Then
          If (Ireps .le. IrepsMax) Go To 100! DD*23 driver request restart
          Stat=-8 ! -8 for too many driver request restarts
          Return
      End If
      Stat  = Iand(Param(2),77777B)   ! # of words actually transferred
      Irunaway = Ibits(Param(3),3,1)  ! is the tape a runaway?
      Ieot = Ior(Ibits(Param(3),13,1),Ibits(Ia,5,1))
      Ieof = Ior(Ibits(Param(3),15,1),Ibits(Ia,7,1))
  
c  Parity Error on read or write
c  Two possibilities: error on read or write
  
      If (Istat .eq. 5) Then  ! transmission error/parity error
         If (Icode .eq. 1) Then         ! A read request
            Call Exec (3,060300b+Lun)   ! Skip over the bad record
         Else                           ! A write request
            Call Exec (3,061200b+Lun)   ! Write a 3.5 inch gap
         End If
  
         Call Abreg (Ia,Ib)
         Ieot = Ibits(Ia,5,1)
      End If
  
      Call Convert_Errs (Istat, Stat)
  
c  EOF
  
      If (Istat .eq. 0 .and. Stat .eq. 0) Stat = -2
      If (Ieof .eq. 1) Stat = -2
  
c EOT caused by run away tape
  
      If (Irunaway .ne. 0) Stat = -3
      If (Ieot .eq. 1) Stat = -3
 
      Return
  
c  Define the control code
  
    1 Ifun = Ifunc - 1
  
      Go To (2,3,4,5,6,7,8,9,10) Ifun
  
    2 Icode = Lun + 064200b         ! Backspace 1 record
      Go To 11
 
    3 Icode = Lun + 064300b         ! Forward space 1 record
      Go To 11
  
    4 Icode = Lun + 064400b         ! Rewind
      Go To 11
  
    5 Icode = Lun + 060100b         ! Write EOF mark
      Go To 11
  
    6 Icode = Lun + 065300b         ! Forward space 1 file
      Go To 11
  
    7 Icode = Lun + 065400b         ! Backspace 1 file
      Go To 11
  
    8 Icode = Lun + 064500b         ! Rewind and go off line
      Go To 11
  
    9 Icode = Lun + 065500b         ! Change density
      Iden = 1600
      If (Nwords .eq. 6250) Iden = 6250
      Call Exec (3,Icode,Iden)
      Call Rmpar (Param)
      Istat = Iand(Param(1),77B)
      Call Convert_Errs (Istat, Stat)
      Return
  
   10 Call Exec (13,Lun,Stat)
      Return
  
   11 Call Exec (3,Icode)
      Call Rmpar (Param)
      Istat = Iand(Param(1),77B)
      Ireps = Ireps + 1
      If ((Istat .eq. 63) .and.(LuDD24 .ne. Lun)
     1  .and. (Ireps .le. IrepsMax) ) Go To 11 ! DD*23 driver request restart
      Ieot = Ibits (Param(3),13,1)
      Ieof = Ibits (Param(3),15,1)
      Irunaway = Ibits(Param(3),3,1)
      Call Convert_Errs (Istat, Stat)
      If (Ieof .eq. 1) Stat = -2
      If (Ieot .eq. 1) Stat = -3
      If (Irunaway .ne. 0) Stat = -3
      Return
  
  999 Stat = -7
  
      Return
      End
      Subroutine Rdrst (Buff,Istat,Mtu),Read OAO standard tapes
  
C Routine to read OAO standard tapes and Return 1 sec of data
  
c  The tape consists of several types of records:
c   Type 1: Header Record
c   Type 2: Calibration record, used to convert the "raw" voltages
c           (type 4 records) to "scientific units" (type 5)
c   Type 3: Data scaling record for science units (where to place the
c           decimal point)
c   Type 4: Original 1 s data (voltages from various sensors)
c   Type 5: Processed record, i.e., useful observations
c   Type 6: Trailer record
  
      Integer T1(9), T2(9), Dp(9), Ap(9), Dap(9), Pc1(9), Pc2(9),
     #        Av1(9), Av2(9), Vs1(9), Vs2(9)
  
      Dimension Buff(106), Ibuf(2500), Type4(11,10)
      Dimension Divisor(106), Type2(202), Jbuf(404)
  
      Equivalence (Swtchs,Ibuf(8)), (Flags,Ibuf(10))
      Equivalence (Type2,Jbuf)
      Equivalence (Type,Ibuf(1))
  
      Data Rec /0.0/, Icnt /999/
 
c  There are 10 logical records per physical tape records
  
    9 Icnt = Icnt + 1
  
C IF Icnt .gt. 10 NEED TO READ A NEW TAPE RECORD
  
C  CHECK FOR TYPE 2,4 RECORDS
C  THIS IS NEEDED TO GET THE ORIGINAL DATA
C  THE TYPE 4 RECORDS ARE READ FIRST AND HELD IN THE Buffer Type4
C  SO THE TIMES WILL CORRESPOND.
 
      If (Icnt .gt. 10 ) Then
         Call Io9 (0,Mtu,Ibuf,2500,Istat)    ! Read a record
         If (Istat .lt. 0) Return
         Icnt = 1
  
c  What type of record is this?
  
         If (Ibuf(1) .eq. 2 .and. Istat .eq. 1332) Go To 200 ! old type2
         If (Type .eq. 2.0) Go To 210 ! new type2
         If (Ibuf(1) .eq. 3) Go To 300       ! type 3 record
         If (Ibuf(1) .eq. 4 .and. Istat .lt. 1600) Go To 400 ! old type 4
         If (Ibuf(1) .eq. 4 .and. Istat .gt. 2000) Go To 410 ! new type 4
  
c  If a type 1 record, then save the data and aircraft number
  
         If (Ibuf(1) .eq. 1) Then
            Ymd = Float(Ibuf(4))*1.E4 + Float(Ibuf(5))*100.0
     #           + Float(Ibuf(6))
            Iyear = Ibuf(4)
            Iacnum = Ibuf(3)
            Icnt = 999
            Go To 9
         End If
  
c  Type = 6; Trailer record
  
         If (Ibuf(1) .eq. 6) Then
            Istat = -2
            Return
         End If
  
      End If
  
c  Not a type 5 record?
  
      If (Ibuf(1) .ne. 5) Then
         Icnt = 999
         Go To 9
      End If
 
C PLACE DATE IN THE FIRST WORD, A/C ID IN THE SECOND, TIME IN THE THIRD.
 
      Isn = (Icnt-1) * 106
      Buff(1) = Ymd
  
C A/C NUMBER
  
      Buff(2) = Float(Iacnum)
  
C TIME
  
      Buff(3) = 10000.0 * Float(Ibuf(3+Isn)) + Float(100*Ibuf(4+Isn)) +
     #          Float(Ibuf(5+Isn))
  
C Latitude
  
      Xlatd = Float(Ibuf(Isn+12)) / Divisor(12)
      Xlatm = Float(Ibuf(Isn+13)) / Divisor(13)
      Buff(4) = Xlatd + Xlatm/60.0
  
C Longitude
  
      Xlond = Float(Ibuf(Isn+14)) / Divisor(14)
      Xlonm = Float(Ibuf(Isn+15)) / Divisor(15)
      If (Xlond .lt. 0 .and. Xlonm .gt. 0) Xlonm = -Xlonm
      Buff(5) = Xlond + Xlonm/60.0
  
C Radar altitude
  
      Buff(6) = Float(Ibuf(Isn+16)) / Divisor(16)
  
C Pressure altitude
  
      Buff(7) = Float(Ibuf(Isn+41)) / Divisor(41)
  
C JW liquid water
  
      Buff(8) = Float(Ibuf(Isn+31)) / Divisor(31)
  
C Vertical wind
  
      Buff(9) = Float(Ibuf(Isn+57)) / Divisor(57)
  
C Rosemount temperature
  
      Buff(10) = Float(Ibuf(Isn+18)) / Divisor(18)
  
C FLAG FOR WHICH TEMPERATURE PROBE USED
  
      Buff(11) = Ibuf(Isn+76)
 
C Dew point
  
      Buff(12) = Float(Ibuf(Isn+19)) / Divisor(19)
  
C CO 2 RADIOMETER
  
      Buff(13) = Float(Ibuf(Isn+21)) / Divisor(21)
  
C RADIOMETER - DN
  
      Buff(14) = Float(Ibuf(Isn+20)) / Divisor(20)
  
C WIND DIRECTION
  
      Buff(15) = Float(Ibuf(Isn+59)) / Divisor(59)
  
C WIND SPEED
  
      Buff(16) = Float(Ibuf(Isn+58)) / Divisor(58)
  
C TRACK
  
      Buff(17) = Float(Ibuf(Isn+25)) / Divisor(25)
  
C HEADING
  
      Buff(18) = Float(Ibuf(Isn+26)) / Divisor(26)
  
C PITCH
  
      Buff(19) = Float(Ibuf(Isn+27)) / Divisor(27)
  
C ROLL
  
      Buff(20) = Float(Ibuf(Isn+28)) / Divisor(28)
  
C ATTACK ANGLE
  
      Buff(21) = Float(Ibuf(Isn+29)) / Divisor(29)
  
C SLIP ANGLE
  
      Buff(22) = Float(Ibuf(Isn+30)) / Divisor(30)
  
C DYNAMIC PRESSURE
  
      Buff(23) = Float(Ibuf(Isn+32)) / Divisor(32)
  
c  Vacant
  
      Buff(24) = -999.0
  
C PRESSURE
  
      Buff(25) = Float(Ibuf(Isn+17)) / Divisor(17)
  
C SWITCHES
  
      Buff(26) = Swtchs
  
C Flags
  
      Buff(27) = Flags
 
C East - West velocity of the tail
 
      Buff(28) = Float(Ibuf(Isn+36)) / Divisor(36)
  
C North - South velocity of tail
  
      Buff(29) = Float(Ibuf(Isn+37)) / Divisor(37)
  
C Vertical velocity of the tail relative to the INE
  
      Buff(30) = Float(Ibuf(Isn+38)) / Divisor(38)
 
C REL. HUMIDITY
  
      Buff(31) = Float(Ibuf(Isn+45)) / Divisor(45)
  
C VIRTUAL TEMP.
  
      Buff(32) = Float(Ibuf(Isn+46)) / Divisor(46)
  
C MIXING RATIO
  
      Buff(33) = Float(Ibuf(Isn+62)) / Divisor(62)
  
C VAPOUR PRESSURE
  
      Buff(34) = Float(Ibuf(Isn+61)) / Divisor(61)
  
C POTENTIAL TEMPERATURE
  
      Buff(35) = Float(Ibuf(Isn+63)) / Divisor(63)
  
C EQUIV. POT. TEMP. (THETA-E)
  
      Buff(36) = Float(Ibuf(Isn+64)) / Divisor(64)
  
C Ground speed
  
      Buff(37) = Float(Ibuf(Isn+22)) / Divisor(22)
  
C True air speed
  
      Buff(38) = Float(Ibuf(Isn+23)) / Divisor(23)
  
C GEOPOTENTIAL HT.
  
      Buff(39) = Float(Ibuf(Isn+40)) / Divisor(40)
  
C D-VALUE
  
      Buff(40) = Float(Ibuf(Isn+42)) / Divisor(42)
  
C STANDARD HT
  
      Buff(41) = Float(Ibuf(Isn+43)) / Divisor(43)
  
C SFC PRESSURE
  
      Buff(42) = Float(Ibuf(Isn+44)) / Divisor(44)
  
C RATIO SPECIFIC HEAT
  
      Buff(43) = Float(Ibuf(Isn+48)) / Divisor(48)
  
C MACH NUMBER
  
      Buff(44) = Float(Ibuf(Isn+49)) / Divisor(49)
  
C DRIFT ANGLE
  
      Buff(45) = Float(Ibuf(Isn+50)) / Divisor(50)
  
C VERT AIRSPEED
  
      Buff(46) = Float(Ibuf(Isn+47)) / Divisor(47)
  
C E-W GROUND SPEED
  
      Buff(47) = Float(Ibuf(Isn+51)) / Divisor(51)
  
C N-S GROUND SPEED
  
      Buff(48) = Float(Ibuf(Isn+52)) / Divisor(52)
  
C E-W TAS
  
      Buff(49) = Float(Ibuf(Isn+53)) / Divisor(53)
  
C N-S TAS
  
      Buff(50) = Float(Ibuf(Isn+54)) / Divisor(54)
  
C E-W WIND SPEED
  
      Buff(51) = Float(Ibuf(Isn+55)) / Divisor(55)
  
C N-S WIND SPEED
  
      Buff(52) = Float(Ibuf(Isn+56)) / Divisor(56)
  
C VS INTEGRATED VERTICAL ACCELERATION (M/SEC)-AC vertical velocity
  
      Buff(53) = Float(Ibuf(Isn+24)) / Divisor(24)
  
c  Vacant
  
      Buff(54) = -999.0
  
C E-W AVE WIND
  
      Buff(55) = Float(Ibuf(Isn+65)) / Divisor(65)
  
C N-S AVE WIND
  
      Buff(56) = Float(Ibuf(Isn+66)) / Divisor(66)
  
C AVE WIND SPEED
  
      Buff(57) = Float(Ibuf(Isn+67)) / Divisor(67)
  
C AVE WIND DIR.
  
      Buff(58) = Float(Ibuf(Isn+68)) / Divisor(68)
  
c  Vacant
  
      Buff(59) = -999.0
  
C Vacant
  
      Buff(60) = -999.0
  
C Vacant
  
      Buff(61) = -999.0
  
C FLAG FOR WHICH INE USED 0-ONE 1-INE1 2-INE2
  
      Buff(62) = Ibuf(Isn+75)
  
C INDICATED TOTAL TEMPERATURE 1
  
      Te = (Buff(25)/(Buff(25)+Buff(23))) ** ((Buff(43)-1.0)/Buff(43))
      Temp1 = Type4(3,Icnt) + 273.16
      Buff(63) = Temp1 * Te - 273.16
  
C INDICATED TOTAL TEMPERATURE 2
  
      Temp2 = Type4(4,Icnt) + 273.16
      Buff(64) = Temp2 * Te - 273.16
  
C DEW POINT (ORIGINAL)
  
      Buff(65) = Type4(5,Icnt)
  
      If (Buff(65) .lt. 0.0) Then            ! Frost point 
         Ee = 6.1078 * Exp((22.4716*Buff(65))/(272.722+Buff(65)))
         Buff(65) = 243.17 * ((Alog(Ee)-1.8096)/(19.4594-Alog(Ee)))
      End If
  
C INE1 PITCH
  
      Buff(66) = Type4(1,Icnt)
  
C INE2 PITCH
  
      Buff(67) = Type4(2,Icnt)
  
C ALPHA PRESSURE
  
      Buff(68) = Type4(6,Icnt)
  
C DYN ALPHA PRESSURE
  
      Buff(69) = Type4(7,Icnt)
  
C Hurricane Parameters . . . . . . .
  
C STORM LAT.
  
      Buff(70) = Float(Ibuf(Isn+78))+Float(Ibuf(Isn+79)) / 600.0
  
C STORM LONGITUDE
  
      Buff(71) = Float(Ibuf(Isn+80))
      TMP = Float(Ibuf(Isn+81)) / 600.0
      If (Buff(71) .lt. 0. .and. TMP .gt. 0.) TMP= -TMP
      Buff(71) = Buff(71) + TMP
  
C VIRTUAL LATITUDE
  
      Buff(72) = Float(Ibuf(Isn+82)) /10.0
  
C VIRTUAL LONGITUDE
  
      Buff(73) = Float(Ibuf(Isn+83))/10.0
  
C STORM DISTANCE
  
      Buff(74) = Float(Ibuf(Isn+84))/10.0
  
C STORM SPEED
  
      Buff(75) = Float(Ibuf(Isn+85)) / 10.0
  
C STORM TRACK
  
      Buff(76) = Float(Ibuf(Isn+86)) / 10.0
  
C RADIAL WIND
  
      Buff(77) = Float(Ibuf(Isn+87)) / 10.0
  
C TANGENTIAL WIND
  
      Buff(78) = Float(Ibuf(Isn+88)) / 10.0
  
C REL. RADIAL WIND
  
      Buff(79) = Float(Ibuf(Isn+89)) / 10.0
  
C REL. TANGENTIAL WIND
  
      Buff(80) = Float(Ibuf(Isn+90)) / 10.0
  
C REL. WIND SPEED
  
      Buff(81) = Float(Ibuf(Isn+91)) / 10.0
  
C REL. WIND DIRECTION
  
      Buff(82) = Float(Ibuf(Isn+92)) / 10.0
  
C REF. PRESSURE
  
      Buff(83) = Float(Ibuf(Isn+93))
  
C REFERENCE HEIGHT
  
      Buff(84) = Float(Ibuf(Isn+94))
  
C REFERENCE TEMP.
  
      Buff(85) = Float(Ibuf(Isn+95)) / 10.0
  
C Adjusted Temperature
  
      Buff(86) = Float(Ibuf(Isn+96)) / 10.0
  
C ADJUSTED DEW POINT
  
      Buff(87) = Float(Ibuf(Isn+97)) / 10.0
  
C ADJUSTED D - VALUE
  
      Buff(88) = Float(Ibuf(Isn+98)) / 10.0
  
C ADJUSTED EQUIVALENT POTENTIAL TEMP.
  
      Buff(89) = Float(Ibuf(Isn+99)) / 10.0
  
C ICING RATE
  
      Buff(90) = Float(Ibuf(Isn+78))
  
C FORMVAR FRAME COUNT
  
      Buff(91) = Float(Ibuf(Isn+79))
  
C FORMVAR EVENT COUNT
  
      Buff(92) = Float(Ibuf(Isn+80))
  
C FORMVAR SPEED
  
      Buff(93) = Float(Ibuf(Isn+81))
  
C INE1 VERTICAL ACCELERATION AV1
  
      If (Iyear .ge. 87) Then
         Buff(94) = Float(Ibuf(Isn+69)) / Divisor(69)
      Else
         Buff(94) = Type4(8,Icnt)
      End If
  
C INE2 VERTICAL ACCELERATION AV2
  
      If (Iyear .ge. 87) Then
         Buff(95) = Float(Ibuf(Isn+70)) / Divisor(70)
      Else
         Buff(95) = Type4(9,Icnt)
      End If
  
C INE1 AIRCRAFT VERTICAL SPEED VS1
  
      Buff(96) = Type4(10,Icnt)
  
C INE2 AIRCRAFT VERTICAL SPEED VS2
  
      Buff(97) = Type4(11,Icnt)
 
c  98 - 106 are vacant
  
      Do i = 98, 106
         Buff(i) = -999.0
      End Do
  
      Return
  
c  Unpack the type 2 records (scaling parameters for type 4 records)
c  This is the old (pre-1987) format
  
  200 DO i = 3, Ibuf(2), 10
 
c  1st Rosemount temperature
  
         If (Ibuf(i) .eq. 61) Then
            Do j = 1, 9
               T1(j) = Ibuf(i+j)
            End Do
         End If
 
c  2nd Rosemount temperature
  
         If (Ibuf(i) .eq. 62) Then
            Do j = 1, 9
               T2(j) = Ibuf(i+j)
            End Do
         End If
  
c  Dewpoint temperature
  
         If (Ibuf(i) .eq. 63) Then
            Do J = 1, 9
               Dp(j) = Ibuf(i+j)
            End Do
         End If
  
c  INE2 Pitch angle
  
         If (Ibuf(i) .eq. 46) Then
            Do j = 1, 9
               Pc2(j) = Ibuf(i+j)
            End Do
         End If
  
c  INE1 pitch angle
  
         If (Ibuf(i) .eq. 36) Then
            Do j = 1, 9
               Pc1(j) = Ibuf(i+j)
            End Do
         End If
  
c  Attack pressure
  
         If (Ibuf(i) .eq. 64) Then
            Do j = 1, 9
               Ap(J) = Ibuf(i+j)
            End Do
         End If
  
c  Differential attack pressure
  
         If (Ibuf(i) .eq. 65) Then
            Do j = 1, 9
               Dap(j) = Ibuf(i+j)
            End Do
         End If
  
c  INE1 vertical acceleration
  
         If (Ibuf(i) .eq. 75) Then
            Do j = 1, 9
               Av1(j) = Ibuf(i+j)
            End Do
         End If
  
c  INE2 vertical acceleration
  
         If (Ibuf(i) .eq. 76) Then
            Do j = 1, 9
               Av2(j) = Ibuf(i+j)
            End Do
         End If
  
c  INE1 vertical airspeed
  
         If (Ibuf(i) .eq. 127) Then
            Do j = 1, 9
               Vs1(j) = Ibuf(i+j)
            End Do
         End If
  
c  INE2 vertical airspeed
  
         If (Ibuf(i) .eq. 128) Then
            Do j = 1, 9
               Vs2(j) = Ibuf(i+j)
            End Do
         End If
  
      End Do
  
      Icnt = 999
      Go To 9
  
c--------------------------------------------------------------
c  Upack the type 2 (calibration) records for the new format
c  post 1987
c--------------------------------------------------------------
  
  210 Do i = 1, Istat
         Jbuf(i) = Ibuf(i)
      End Do
  
      Icnt = 999
      Go To 9
  
c  Unpack the type 3 (scaling parameters for type 5 records) records
  
  300 Do i = 3, Ibuf(2), 2
         j = Ibuf(i)
         Divisor(j) = Float(Ibuf(i+1))
      End Do
  
      Icnt = 999
      Go To 9
  
c  Unpack the type 4 records (original data)
c  This is the pre-1987 format
  
  400 Do i = 1, 10
         I4n = (I-1) * Ibuf(2)
         Type4(1,i) = Eqt(Pc1,Ibuf(I4n+36))        ! INE1 Pitch
         Type4(2,i) = Eqt(Pc2,Ibuf(I4N+46))        ! INE2 Pitch
         Type4(3,i) = Eqt(T1,Ibuf(I4N+61))         ! Rosemount #1
         Type4(4,i) = Eqt(T2,Ibuf(I4N+62))         ! Rosemount #2
         Type4(5,i) = Eqt(Dp,Ibuf(I4N+63))         ! Dew Point
         Type4(6,i) = Eqt(Ap,Ibuf(I4N+64))         ! Attack pressure
         Type4(7,i) = Eqt(Dap,Ibuf(I4N+65))        ! Diff. Attack press
         Type4(8,i) = Eqt(Av1,Ibuf(I4N+75))        ! Vert accel #1
         Type4(9,i) = Eqt(Av2,Ibuf(I4N+76))        ! Vert accel #2
         Type4(10,i) = Eqt(Vs1,Ibuf(I4N+127))      ! Vert airspeed #1
         Type4(11,i) = Eqt(Vs2,Ibuf(I4N+128))      ! Vert airspeed #2
      End Do
  
      Icnt = 999
      Go To 9
  
c  Unpack 10 seconds worth of type 4 data (New format)
  
 410  Call Type4_New (Type2,Type4,Ibuf)
  
      Icnt = 999
      Go To 9
  
      End
  
      Function TMCON(TIME),Time: hhmmss->seconds
  
C Routine to convert time from hhmmss. format to seconds.
  
      IH=TIME/10000.0
      IM=(TIME-Float(IH)*10000.0)/100.0
      IS=TIME-Float(IH)*10000.0-Float(IM)*100.0
      TMCON=TIMZ(IH,IM,IS)
      Return
      End
  
      Function TIMZ(IH,IM,IS),Time: IH,IM,IS->seconds
  
C Function to convert time from 3 word (IH,IM,IS) to seconds.
  
      TIMZ=Float(IH)*3600.0 + Float(IM)*60.0 + Float(IS)
      Return
      End
  
      Subroutine Ctme (T,Ih,Im,Is),Time: seconds->hhmmss
  
c  Routine to convert time from seconds to hour,minutes,seconds
  
      Ih = T/3600.0                  ! Hours
      Im = (T-Float(Ih)*3600.0)/60.0 ! Minutes
      Is = T-Float(Ih)*3600.0-Float(Im)*60.0
      Return
  
      End
  
      Subroutine CHKTM(T),24 to 48 hour clock
  
C   Routine to check for time flips over midnight
C   this will add 24 hours if a flip occurs
  
      DATA OCN/86400.0/,TMJMP /0.0/, TH /0.0/
      IF (T .lt. 0.0) Return
      IF (T-TH .lt. -86390.0) Go To 1
      TH=T
      T=T + TMJMP
      Return
    1 TMJMP=OCN
      TH=T
      T=T+TMJMP
      Return
      End
  
      Subroutine Convert_Errs (Istat,Stat)
  
      Integer Stat, Istat
  
c  EOT
  
      If (Istat .eq. 4) Stat = -3
  
c  Parity Error
  
      If (Istat .eq. 5) Stat = -1
 
c  No Write ring
  
      If (Istat .eq. 6) Stat = -4
  
c  Unit not ready
  
      If (Istat .eq. 2) Stat = -5
  
c  Unit timed out
  
      If (Istat .eq. 3) Stat = -6
  
c Too many driver restart requests
  
      If (Istat .eq.63)  Then
          Stat = -8
          Return
      End If
  
c  strange error (undetermined)
  
      If (Istat .gt. 6) Stat = -9
  
      Return
      End
 
      Subroutine Type4_New (Type2,Type4,Ibuf)
  
c  This routing converts the raw P-3 flight level data to engineering
c  units
c       Type4 will contain 10 seconds worth of data.
c       There are 11 parameters which are converted.
c                1 INE1 Pitch
c                2 INE2 Pitch
c                3 Rosemount #1
c                4 Rosemount #2
c                5 Dew Point
c                6 Attack pressure
c                7 Diff. Attack press
c                8 Vert accel #1
c                9 Vert accel #2
c               10 Vert airspeed #1
c               11 Vert airspeed #2
  
      Dimension Type2(202), Type4(11,10), Ibuf(222,10), Ireg(2)
  
      Equivalence (Reg,Ireg)
  
      Length = Type2(2)
  
      Do i = 1, 10
         Ireg(1) = Ibuf(73,i)
         Ireg(2) = Ibuf(74,i)
         Type4(1,i) = Pack(Reg) * 720.0
  
         Ireg(1) = Ibuf(93,i)
         Ireg(2) = Ibuf(94,i)
         Type4(2,i) = Pack(Reg) * 720.0
  
         Type4(8,i) = Float(Ibuf(156,i)) * 0.000305176 * 4.90308
         Type4(9,i) = Float(Ibuf(157,i)) * 0.000305176 * 4.90308
  
         Ireg(1) = Ibuf(67,i)
         Ireg(2) = Ibuf(68,i)
         Type4(10,i) = Pack(Reg) * 2107.17
 
         Ireg(1) = Ibuf(87,i)
         Ireg(2) = Ibuf(88,i)
         Type4(11,i) = Pack(Reg) * 2107.17
  
         Do j = 3, Length, 5
  
c   Rosemount temperature #1 (143)
  
            If (Type2(j) .eq. 143.0) Then
               A3 = Type2(j+1)
               A2 = Type2(j+2)
               A1 = Type2(j+3)
               A0 = Type2(j+4)
               Volts = Float(Ibuf(143,i))
               Volts2 = Volts * Volts
               Volts3 = Volts2 * Volts
               Type4(3,i) = A3*Volts3 + A2*Volts2 + A1*Volts + A0
            End If
  
c   Rosemount temperature #2 (144)
  
            If (Type2(j) .eq. 144.0) Then
               A3 = Type2(j+1)
               A2 = Type2(j+2)
               A1 = Type2(j+3)
               A0 = Type2(j+4)
               Volts = Float(Ibuf(144,i))
               Volts2 = Volts * Volts
               Volts3 = Volts2 * Volts
               Type4(4,i) = A3*Volts3 + A2*Volts2 + A1*Volts + A0
            End If
  
c  Dew Point (145)
  
            If (Type2(j) .eq. 145.0) Then
               A3 = Type2(j+1)
               A2 = Type2(j+2)
               A1 = Type2(j+3)
               A0 = Type2(j+4)
               Volts = Float(Ibuf(145,i))
               Volts2 = Volts * Volts
               Volts3 = Volts2 * Volts
               Type4(5,i) = A3*Volts3 + A2*Volts2 + A1*Volts + A0
            End If
  
c  Attack Pressure (146)
  
            If (Type2(j) .eq. 146.0) Then
               A3 = Type2(j+1)
               A2 = Type2(j+2)
               A1 = Type2(j+3)
               A0 = Type2(j+4)
               Volts = Float(Ibuf(146,i))
               Volts2 = Volts * Volts
               Volts3 = Volts2 * Volts
               Type4(6,i) = A3*Volts3 + A2*Volts2 + A1*Volts + A0
            End If
  
c  Differential attack pressure (147)
  
            If (Type2(j) .eq. 147.0) Then
               A3 = Type2(j+1)
               A2 = Type2(j+2)
               A1 = Type2(j+3)
               A0 = Type2(j+4)
               Volts = Float(Ibuf(147,i))
               Volts2 = Volts * Volts
               Volts3 = Volts2 * Volts
               Type4(7,i) = A3*Volts3 + A2*Volts2 + A1*Volts + A0
            End If
         End Do
  
c---------------------------------------------------------------
c  Dynamic calibrations
c---------------------------------------------------------------
  
         Do j = 3, Length, 5
  
            If (Type2(j) .eq. -143.0) Type4(3,i) = Type4(3,i) * 
     #        Type2(j+2) + Type2(j+4)
            If (Type2(j) .eq. -144.0) Type4(4,i) = Type4(4,i) * 
     #        Type2(j+2) + Type2(j+4)
         End Do
      End Do
  
      Return
      End
  
      Function Eqt(Icons,Ival),Convert OAO standard type 4 data
  
      Dimension Icons(9)
  
      X = Ival
  
      Eqt=(Float(Icons(1))*10000.0+Float(Icons(2)))*10.0**Icons(3)*X*X
     1  + (Float(Icons(4))*10000.0+Float(Icons(5)))*10.0**Icons(6)*X
     2  + (Float(Icons(7))*10000.0+Float(Icons(8)))*10.0**Icons(9)
  
      Return
      End
  
