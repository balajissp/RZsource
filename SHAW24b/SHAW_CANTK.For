C***********************************************************************
C
      SUBROUTINE CANTK(NC,CON,TC,ZC,ZMSUB,ZHSUB,ZERSUB,STABLE,WINDC)
C
C     THIS SUBROUTINE COMPUTES THE EDDY CONDUCTANCE COEFFICIENT THROUGH
C     THE CANOPY
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C 9/18/98 - CORRECTED
C     PARAMETER (nodcan=11, CA=1.25D0 ,VONKRM=0.4D0 ,TKA=0.025D0, 
      PARAMETER (nodcan=11, CA=1006.D0 ,VONKRM=0.4D0 ,TKA=0.025D0, 
     + rhoa=1.25d0)
C
      DOUBLE PRECISION CON(NODCAN),ZC(NODCAN),TC(NODCAN),WINDC(NODCAN)
C
C       RICHRD=STABLE
      IF (STABLE.GT. 1.0d0) STABLE= 1.0
      IF (STABLE.LT. -2.d0) STABLE= -2.d0
C       stabl = richrd
C
      DO 10 I=1,NC-1
         dudz = (windc(i)-windc(i+1))/(ZC(I+1)-ZC(I))
         IF (STABLE .GE. 0.D0) THEN
C           STABLE CONDITIONS
            PHIW = 1.d0 + 5.0d0*STABL
            PHIH = 1.d0 + 5.0d0*STABL
           ELSE         
C           UNSTABLE CONDITIONS
            PHIW = 1.d0/(1.d0 - 16.d0*STABL)**0.25d0
            PHIH = 1.d0/SQRT(1.d0 - 16.d0*STABL)
         END IF           
         TKCAN=RHOA*CA*dudz*(VONKRM*(ZC(NC+1)-ZERSUB))**2/(PHIW*PHIH)
         IF (TKCAN .LT. TKA) TKCAN=TKA
         CON(I) = TKCAN/(ZC(I+1)-ZC(I))
   10 CONTINUE
c
c     Get conductance for surface
      TMPAIR = TC(NC)
      TMPSFC = TC(NC+1)
      HEIGHT = ZC(NC+1)-ZC(NC)
C
      ITER1=0
      HFLUX=0.d0
      PSIH=0.0d0
      PSIM=0.0d0
      ZMLOG=LOG((HEIGHT+ZMSUB-ZERSUB)/ZMSUB)
      ZHLOG=LOG((HEIGHT+ZHSUB-ZERSUB)/ZHSUB)
      USTAR=WINDC(NC)*VONKRM/(ZMLOG + PSIM)
      CALL STAB(ITER1,HEIGHT,HFLUX,PSIH,RH,TMPSFC,TMPAIR,ZHSUB,ZERSUB,
     +                ZMLOG,ZHLOG,WINDC(NC),USTAR,STABLE,0,0.0d0)
      CON(NC) = RHOA*CA/RH

      RETURN
      END
C
