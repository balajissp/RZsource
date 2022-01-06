C
      SUBROUTINE SNOWTK(NSP,TK,RHOSP)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE THERMAL CONDUCTIVITY OF THE
C     SNOWPACK LAYERS.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       I          L    canopy, snow, residue or soil layer
C       NSP        I    num of layers in snowpack
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOSP      I    density of ice fract of snowpack  (kg/m^3)
C       TK        I/O   ave thermal cond over the time step
C       TKSPA      P    coeff for thermal cond of snow
C       TKSPB      P    coeff for thermal cond of snow
C       TKSPEX     P    exponent for thermal cond of snow
C
C       COMMENTS:
C     THE EQUATION USED IS OF THE FORM:
C
C           K = A + B*(RHOSP/RHOL)**C
C
C     WHERE:    A = TKSPA      B = TKSPB     C = TKSPEX
C
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
      DOUBLE PRECISION TK(NODSNO),RHOSP(NODSNO)
      PARAMETER (RHOL=1.0D3,TKSPA=2.1D-2,TKSPB=2.51D0,TKSPEX=2.0D0)
C
      DO 10 I=1,NSP
         TK(I) = TKSPA + TKSPB*(RHOSP(I)/RHOL)**TKSPEX
   10 CONTINUE
      RETURN
      END
