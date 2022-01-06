C
      SUBROUTINE SNOWHT(NSP,CSP,TSP,RHOSP)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBOUTINE CALCULATES THE VOLUMETRIC SPECIFIC HEAT FOR EACH
C     RESIDUE NODE, INCLUDING LATENT HEAT EFFECTS.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CSP       I/O   ave vol heat cap of the snow for time step (J/kg-K)
C       I          L    canopy, snow, residue or soil layer
C       LS         P    latent heat of sublimation
C       NSP        I    num of layers in snowpack
C       RHOI       P    density of ice   (kg/m^3)
C       RHOSP      I    density of ice fract of snowpack  (kg/m^3)
C       S          L    slope of sat vapor density curve  (kg/m^3-K)
C       TSP        I    temp of canapy layer at time t
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       VSLOPE
C
C       CALLED FROM:
C
C       PROGRAMMER:
C
C       VERSION:
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (NODSNO=100)
C
      DOUBLE PRECISION CSP(NODSNO),TSP(NODSNO),RHOSP(NODSNO), LS
      PARAMETER (LS=2.835D6,RHOI=920.0D0)
C
      DO 10 I=1,NSP
C        CALCULATE THE SPECIFIC HEAT OF THE SNOW
         SPHEAT = 92.96D0 + 7.37D0*(TSP(I)+273.16D0)
C        INCLUDE THE LATENT HEAT TERM
         CALL VSLOPE (S,DUMMY,TSP(I))
C        CALCULATE THE VOLUMETRIC SPECIFIC HEAT INCLUDING LATENT HEAT
         CSP(I) = RHOSP(I)*SPHEAT + (1.0D0-RHOSP(I)/RHOI)*LS*S
   10 CONTINUE
      RETURN
      END
