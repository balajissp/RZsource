c rma\gf  substituted G. Flerchinger routine
      SUBROUTINE SOILHT(NS,CS,VLC,VIC,TS,MAT,CONC,RHOB,om,NSALT)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE SPECIFIC HEAT THE SOIL LAYERS
C     -- THE LATENT HEAT OF VAPORIZATION IS INCLUDED.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CA         P    spec heat of air  (J/kg-K)
C       CI         P    spec heat cap of ice  (J/kg-K)
C       CL         P    spec heat cap of liquid water  (J/kg-K)
C       CM         P    spec heat cap of mineral fract of soil (J/kg-K)
C       CONC       I    conc of j-th solute in soil soln at time t (eq/kq)
C       CS        I/O   ave heat cap of the soil over time step  (J/kg-K)
C       CV         P    spec heat of water vapor  (J/kg-K)
C       G          P    acceleration of gravity
C       I          L    canopy, snow, residue or soil layer
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       LV         P    latent heat of vaporization
C       MAT        I    matric potential  (m)
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       OM         I    organic matter fraction in soil layer
C       RHOA       P    density of air  (kg/m^3)
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOI       P    density of ice   (kg/m^3)
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOM       P    density of the soil (particle density) kg/m^3
C       S          L    slope of sat vapor density curve  (kg/m^3-K)
C       TOTPOT     L    total water potential
C       TS         I    temp of soil layer at time t
C       UGAS       P    universal gas constant
C       VIC        I    vol ice content
C       VLC        I    vol liquid content
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
      PARAMETER (MXNOD=300, MXCHEM=15)
C
c rma\gf
      DOUBLE PRECISION CS(MXNOD),VLC(MXNOD),VIC(MXNOD),TS(MXNOD),
     +  MAT(MXNOD),CONC(MXCHEM,MXNOD),RHOB(MXNOD),om(MXNOD)
      DOUBLE PRECISION LV
C
c rma\gf
      PARAMETER(LV=2.5D6,G=9.81D0,UGAS=8.3143D0,
     +        RHOL=1.0D3,RHOI=9.2D2,RHOM=2.65D3,RHOA=1.25D0,RHOOM=1.3D3,
     +        CL=4.2D3,CM=9.0D2,CI=2.1D3,CA=1.006D3,COM=1.92D3)
C
      DO 20 I=1,NS
c kej
c         VAC = sat(I) - VLC(I) - VIC(I)*rhoi/rhol
cc         VAC = 1.0D0 - RHOB(I)/RHOM - VLC(I) - VIC(I)
c         IF (VAC .LT. 0.0D0) VAC=0.0D0
c         CS(I)=RHOB(I)*CM + VLC(I)*RHOL*CL + VIC(I)*RHOI*CI +VAC*RHOA*CA
c rma\gf
C        CORRECTION OF MINERAL FRACTION FOR ORGANIC MATTER
         XSM = 1.0 - OM(I)
C
         VAC = 1. -XSM*RHOB(I)/RHOM -OM(I)*RHOB(I)/RHOOM -VLC(I) -VIC(I)
         IF (VAC .LT. 0.0) VAC=0.0
         CS(I)=XSM*RHOB(I)*CM + OM(I)*RHOB(I)*COM + VLC(I)*RHOL*CL
     >          + VIC(I)*RHOI*CI + VAC*RHOA*CA
C
C
C****    INCLUDE THE LATENT HEAT OF VAPORIZATION IN THE HEAT CAPACITY
C
C        ..DETERMINE HUMIDITY OF LAYER FROM TOTAL WATER POTENTIAL
         TLCONC=0.0D0
         DO 10 J=1,NSALT
            TLCONC= TLCONC+CONC(J,I)
   10    CONTINUE
         TOTPOT=MAT(I) - TLCONC*UGAS*TS(I)/G
         HUMID=EXP(0.018D0*G/UGAS/(TS(I)+273.16D0)*TOTPOT)
C        OBTAIN SLOPE OF VAPOR DENSITY CURVE
         CALL VSLOPE (S,RHOV,TS(I))
         CS(I)=CS(I)+VAC*LV*HUMID*S
   20 CONTINUE
      RETURN
      END
