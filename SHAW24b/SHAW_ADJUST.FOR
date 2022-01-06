C
      SUBROUTINE ADJUST(I,NS,VICDT,VLCDT,MATDT,CONCDT,TSDT,SALTDT,
     +            ICESDT,CS,SOILHP,NDXN2H,RHOB,NSALT,SALTKQ,sat,Hmin)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE ADJUSTS THE SOIL LAYER TO ACCOUNT FOR THE LATENT
C     HEAT OF FUSION ON THE FIRST ITERATION THAT THE LAYER CROSSES THE
C     FREEZING POINT.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CONCDT    I/O   conc of j-th solute in soil soln at time t+dt (eq/kg)
C       CS         I    ave heat cap of the soil over time step  (J/kg-K)
C       DLDT       L    derivitive of soil liquid content with respect to temp
C       G          P    acceleration of gravity
C       I          I    canopy, snow, residue or soil layer
C       ICESDT     I    1 if soil layer contains both ice and water at t+dt
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       MATDT     I/O   matric potential at t+dt  (m)
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOI       P    density of ice   (kg/m^3)
C       RHOL       P    density of liquid water  (kg/m^3)
C       SALTDT     I    amount of j-th solute in i-th soil node at t+dt
C       SALTKQ     I    adsorption char of solute j for node i  (kg/kg)
C       TOTPOT     L    total water potential
C       TSDT      I/O   temp of soil layer at time t + dt
C       UGAS       P    universal gas constant
C       VICDT     I/O   vol ice content at t+dt
C       VLCDT     I/O   vol liquid content at t+dt
C
C
C       COMMENTS:
C
C
C       MASS STORAGE FILES:
C
C
C       EXTERNAL REFERENCES:
C                       FROZEN
C                       FSLOPE
C                       WCH
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
      PARAMETER (MXNOD=300, MAXHOR=12, MXCHEM=15)
C
      DOUBLE PRECISION VICDT(MXNOD),VLCDT(MXNOD),MATDT(MXNOD),
     +  CONCDT(MXCHEM,MXNOD), TSDT(MXNOD),SALTDT(MXCHEM,MXNOD),
     +    CS(MXNOD),SOILHP(13,MAXHOR),RHOB(MXNOD),SALTKQ(MXCHEM,MXNOD),
     +    sat(mxnod)
      DOUBLE PRECISION LF
      INTEGER ICESDT(MXNOD), NDXN2H(NS)

      PARAMETER(LF=3.35D5,G=9.81D0,UGAS=8.3143D0,RHOL=1.0D3,RHOI=9.2D2)
C
C**** INITIALLY ASSUME THAT ALL WATER IS LIQUID
      VLCDT(I)=VLCDT(I) + (RHOI/RHOL)*VICDT(I)
CCCC  CALL MATVL1 (I,MATDT(I),VLCDT(I),DUMMY,B,ENTRY,SAT)
      JH=NDXN2H(I)
      HTMP = -(SOILHP(1,JH)+5.0D0)
C LMA UPDATE WCH
      MATDT(I) = WCH(HTMP,VLCDT(I),SOILHP(1,JH),JH,0)/1.0D2
      DO 5 J=1,NSALT
         CONCDT(J,I)=SALTDT(J,I)/(SALTKQ(J,I) + VLCDT(I)*RHOL/RHOB(I))
    5 CONTINUE
      VICDT(I)=0.0D0
C
C**** CALCULATE THE FREEZING POINT TEMPERATURE ACCORDING TO TOTAL WATER
C     POTENTIAL AT THE END OF THE TIME STEP
      TLCONC=0.0D0
      DO 10 J=1,NSALT
         TLCONC= TLCONC+CONCDT(J,I)
   10 CONTINUE
      TOTPOT=MATDT(I) - TLCONC*UGAS*273.16D0/G
      TMPFRZ=273.16D0*TOTPOT/(LF/G-TOTPOT)
      TMPF=TMPFRZ+273.16D0
C
C**** CALCULATE THE ENERGY AVAILABLE TO FREEZE WATER
      ENERGY=CS(I)*(TMPFRZ-TSDT(I))
C
C**** CALCULATE THE EFFECTIVE HEAT CAPACITY INCLUDING LATENT HEAT TERM
      CALL FSLOPE (I,NS,DLDT,DUMMY,TMPF,MATDT,CONCDT,VLCDT,
     +             SOILHP,NDXN2H,RHOB,NSALT,SALTKQ)
      EFFCS=CS(I)+RHOL*LF*DLDT
C
C**** CALCULATE THE TEMPERATURE AT THE END OF THE TIME STEP
      TSDT(I)=TMPFRZ - ENERGY/EFFCS
C
C**** CALL SUBROUTINE FROZEN TO DETERMINE THE LIQUID, ICE, AND SOLUTES
      CALL FROZEN(I,NS,VLCDT,VICDT,MATDT,CONCDT,TSDT,SALTDT,ICESDT,
     +             SOILHP,NDXN2H,RHOB,NSALT,SALTKQ,SAT,Hmin)
      RETURN
      END
