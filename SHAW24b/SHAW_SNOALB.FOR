C
      SUBROUTINE SNOALB(NSP,ALBSNO,ALBNXT,RHOSP,ZSP)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE COMPUTES THE ALBEDO THE SNOW SURFACE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       ALBNXT     I    albedo of material beneath canopy or snow
C       ALBSOI     I    albedo of the soil surface
C       EXTSP      P    coeff for extinction coeff of snowpack
C       G1         P    coeff for grain size of snow
C       G2         P    coeff for grain size of snow
C       G3         P    coeff for grain size of snow
C       NSP        I    num of layers in snowpack
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOSP      I    density of ice fract of snowpack  (kg/m^3)
C       SNOCOF     L    coeff for albedo of the snowpack
C       SNOEXP     L    exponent for the albedo of the snowpack
C       ZSP        I    depth of node i in the snow from the snowpack top
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       NONE
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
      DOUBLE PRECISION RHOSP(NODSNO),ZSP(NODSNO)
      PARAMETER (RHOL=1.0D3,G1=1.6D-1,G2=0.0D0,G3=110.0D0,
     +  EXTSP=1.77D0)
C
C      SNOCOF=0.0D0
C      SNOEXP=1.0D0
C
C     DETERMINE THE ALBEDO OF THE SNOW
      SPGRAV = RHOSP(1)/RHOL
      GRAIN = G1 + G2*SPGRAV*SPGRAV + G3*SPGRAV**4.0D0
      ALBSNO = 1.0D0 - 0.206D0*EXTSP*SQRT(GRAIN)
      IF (ALBSNO .LT. 0.35D0) ALBSNO = 0.35D0
C
      IF (ZSP(NSP+1) .LE. 0.04D0) THEN
C        SNOWPACK IS LESS THAN 4.0 CM -- ALBEDO IS AFFECTED BY
C        UNDERLYING MATERIAL
         ABSN = 1.0D0 - ALBNXT
         W = 2.0D0*(1.0D0 - ALBSNO)/(1.0D0 + ALBSNO)
C        EXC = EXTINCTION COEFFICIENT --> CONVERT FROM 1/CM TO 1/M
         EXC = EXTSP*SPGRAV*SQRT(1.0D0/GRAIN)*1.0D2
         Y = EXP(-EXC*ZSP(NSP+1))*(ABSN + W*(ABSN/2.0D0-1.0D0))
     +    / (W*(ABSN/2.0D0-1.0D0)*COSH(EXC*ZSP(NSP+1))-ABSN *
     +    SINH(EXC*ZSP(NSP+1)))
         ALBSNO = (1.0D0 - W*(1.0D0-Y)/2.0D0)/(1.0D0 +
     +    W*(1.0D0-Y)/2.0D0)
      END IF
C
CCCC  IF (ZSP(NSP+1) .LE. SNOCOF) THEN
C        SNOCOF IS MINIMUM DEPTH OF SNOW FOR COMPLETE GROUND COVER --
C        SNOW IS NOT COMPLETELY COVERED WITH SNOW
CCCC     ALBSNO = ALBNXT + (ALBSNO-ALBNXT)*(ZSP(NSP+1)/SNOCOF)**SNOEXP
CCCC  END IF
      RETURN
      END
C
