       CHARACTER*4 FUNCTION CLIT(L)
C *** McIDAS Revision History ***
C 1 CLIT.FOR 27-Feb-90,8:52:06,`SSEC' PC-McIDAS ver 5.00
C 2 CLIT.FOR 24-Sep-90,18:19:28,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
       CHARACTER*4 C
       CALL MOVWC(L,C)
       CLIT=C
       RETURN
       END
