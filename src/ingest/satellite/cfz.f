       CHARACTER*12 FUNCTION CFZ(L)
C *** McIDAS Revision History ***
C 1 CFZ.FOR 27-Feb-90,21:47:28,`SSEC' PC-McIDAS ver 5.00
C 2 CFZ.FOR 25-Sep-90,7:35:58,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
       IMPLICIT CHARACTER*12 (C)
       WRITE(C,1)L
 1     FORMAT(4X,Z8)
       CFZ=C
       RETURN
       END
