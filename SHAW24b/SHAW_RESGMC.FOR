C***********************************************************************
C
      SUBROUTINE RESGMC(NR,HUM,GMCDT,TRDT)
C
C     THIS SUBROUTINE INITIALIZES THE GRAV. MOISTURE CONTENT OF THE
C     RESIDUE BY DOING THE INVERSE OR RESHUM
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (UGAS=8.3143D0, RESMA=-53.72D0, RESMB=1.32D0,
     +           NODRES=10, G=9.8D0)
      DOUBLE PRECISION GMCDT(NODRES),TRDT(NODRES)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
C****    USE HUMDITITY-TENSION-MOISTURE CONTENT RELATIONS
      DO 10 I = 1, NR
C rma/gf
        if (hum .ge. 0.999d0) then 
          GMCDT(I) = (LOG(0.999d0)/((RESMA*0.018D0*G)/(UGAS*
     +             (TRDT(I)+273.16D0))))**(-1/RESMB)  
         else
          GMCDT(I) = (LOG(HUM)/((RESMA*0.018D0*G)/(UGAS*
     +             (TRDT(I)+273.16D0))))**(-1/RESMB)  
        endif
10    CONTINUE
      IF (PPLASTIC.GT.0.0D0) GMCDT(1)=0.0D0
C
      RETURN
      END
