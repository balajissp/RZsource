C***********************************************************************
C
      SUBROUTINE SWRSNO(NSP,SWSNOW,DIRECT,DIFFUS,ALBSNO,
     +                  RHOSP,DZSP,SUNSLP)
C
C     THIS SUBROUTINE COMPUTES THE AMOUNT OF OF SOLAR RADIATION
C     ABSORBED BY EACH LAYER WITHIN THE SNOWPACK
C
C***********************************************************************
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODSNO=100)
C
      DOUBLE PRECISION SWSNOW(NODSNO),RHOSP(NODSNO),DZSP(NODSNO)
C
      PARAMETER (RHOL=1000.D0, G1=0.16D0, G2=0.0D0, G3=110.0D0,
     +  EXTSP=1.2D0)
C
C     ASSUME 58% OF SOLAR RADIATION IS IN VISIBLE SPECTRAL 
      data vis / 0.58D0/

C
C     COMPUTE THE TOTAL ABSORBED FOR THE ENTIRE SNOWPACK
cxxx  TOTAL = (DIRECT + DIFFUS)*(1. - ALBSNO)
      SPGRAV = RHOSP(1)/RHOL
      GRAIN = G1 + G2*SPGRAV*SPGRAV + G3*SPGRAV**4
c     include influence of sun elevation angle
      if (sunslp .gt. 0.0) then
         dzv = (1.375D-3*sqrt(grain*1000.D0))*(1.D0-sin(sunslp))
         dzir= (2.0D-3*sqrt(grain*1000.D0) + 0.1D0)*(1.D0-sin(sunslp))
        else
C        SUN IS NOT ABOVE THE HORIZON
         DZV = 0.0D0
         DZIR = 0.0D0
      END IF
      albdir = albsno + dzv*vis + dzir*(1.D0-vis)
      TOTAL = DIRECT*(1.D0-albdir) + DIFFUS*(1.D0 - ALBSNO)
C
C
C     CALCULATE EXTINCTION COEFF. AND RADIATION ABSORBED BY EACH LAYER
      DO 10 I= 1,NSP
         SPGRAV = RHOSP(I)/RHOL
         GRAIN = G1 + G2*SPGRAV*SPGRAV + G3*SPGRAV**4.d0
C        EXC = EXTINCTION COEFFICIENT --> CONVERT FROM 1/CM TO 1/M
         EXC = EXTSP*SPGRAV*SQRT(1.0d0/GRAIN)*100.d0
         PERCAB = 1.0d0 - EXP(-EXC*DZSP(I))
         SWSNOW(I) = TOTAL*PERCAB
         TOTAL = TOTAL - SWSNOW(I)
   10 CONTINUE
C
C     DEFINE THE DIFFUSE RADIATION OUT THE BOTTOM OF THE SNOWPACK
C     (THIS WILL BE ABSORBED BY EITHER THE RESIDUE OR THE SOIL.)
      DIFFUS = TOTAL
      DIRECT = 0.0d0
      RETURN
      END
C
