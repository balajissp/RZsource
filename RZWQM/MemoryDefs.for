C       MODULE MemoryDefs
      ! Many subroutines in the project use static variables using 
      ! SAVE option. This module modifies it to help resume execution 
      ! from any arbitrary state. 
      ! All storage elements have `mem_` prefix
      ! Programmer: Balaji S. Pokuri
      ! 
      SUBROUTINE MAQUE_Memory(CODE, ADIW, JSPLT, SPLT1, FIRST7, NYR, 
     +  IRPL, NYRC, NYRP, ADIWMONTH, totadiw, curradiw)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MAXHOR=12,MXAPP=200,MXCHEM=15,MXSPEC=10,
     +          MXTG=500,MXPEST=3)
      CHARACTER (len=3)  CODE
      INTEGER IRPL, NYR, NYRC, MEM_IRPL, MEM_NYR, MEM_NYRC
      DIMENSION ADIW(MXAPP),MEM_ADIW(MXAPP),NYRP(MXPEST)
     + ,MEM_NYRP(MXPEST),ADIWMONTH(12,200),MEM_ADIWMONTH(12,200),
     + totadiw(20),MEM_totadiw(20)
      LOGICAL SPLT1, MEM_SPLT1, FIRST7, MEM_FIRST7
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          ADIW = MEM_ADIW
          JSPLT = MEM_JSPLT
          SPLT1 = MEM_SPLT1
          FIRST7 = MEM_FIRST7
          NYR = MEM_NYR
          IRPL = MEM_IRPL
          NYRC = MEM_NYRC
          NYRP = MEM_NYRP
          ADIWMONTH =  MEM_ADIWMONTH
          totadiw = MEM_TOTADIW
          curradiw = MEM_curradiw
        CASE('PUT')
          MEM_ADIW = ADIW
          MEM_JSPLT = JSPLT
          MEM_SPLT1 = SPLT1
          MEM_FIRST7 = FIRST7
          MEM_NYR = NYR
          MEM_IRPL = IRPL
          MEM_NYRC = NYRC
          MEM_NYRP = NYRP
          MEM_ADIWMONTH =  ADIWMONTH
          MEM_totadiw = TOTADIW
          MEM_curradiw = curradiw
      END SELECT
      
      RETURN
      END SUBROUTINE MAQUE_Memory
      
C       END MODULE