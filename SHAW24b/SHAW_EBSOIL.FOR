C
c rma\gf
      SUBROUTINE EBSOIL(N,NS,TS,TSDT,MAT,MATDT,CONC,CONCDT,VLC,VLCDT,
     +                VIC,VICDT,ICES,ICESDT,QSL,QSV,SS,ITER,WT,WDT,
     +                DT,A1,B1,C1,D1,CS,SOILHP,NDXN2H,RHOB,ZS,
     +                NSALT,SALTKQ,sand,silt,clay,om,gflux)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE COEFFICIENTS FOR THE SOIL IN THE
C     NEWTON-RAPHSON PROCEDURE OF THE ENERGY BALANCE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       A1        I/O   first term in Jacobian  matrix for energy balance
C       B1        I/O   diagonal term in Jacobian matrix for energy balance
C       C1        I/O   third term in Jacobian  matrix for energy balance
C       CL         P    spec heat cap of liquid water  (J/kg-K)
C       CON        L    cond term between nodes I and I+1  K/(Z(I+1)-Z(I))
C       CONC       I    conc of j-th solute in soil soln at time t (eq/kq)
C       CONCDT     I    conc of j-th solute in soil soln at time t+dt (eq/kg)
C       CS         I    ave heat cap of the soil over time step  (J/kg-K)
C       CSDT       L    vol heat cap of the soil at time t+dt  (J/kg-K)
C       CST        L    vol heat cap of the soil at time t  (J/kg-K)
C       CV         P    spec heat of water vapor  (J/kg-K)
C       D1        I/O   residual term in Newton-Raphson iteration for
C                       energy balance
C       DLDM       L    derivative of liquid content with respect to
C                       matric potential(m^-1)
C       DLDT       L    derivitive of soil liquid content with respect to temp
C       DLDTDT     L    second derivitive of soil liquid content with temp
C       DUMMY      L    dummy variable passed but not needed
C       G          P    acceleration of gravity
C       I          L    canopy, snow, residue or soil layer
C       ICES       I    1 if soil layer contains both ice and water at t
C       ICESDT     I    1 if soil layer contains both ice and water at t+dt
C       ITER       I    current num of iter for energy and water balance
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       LV         P    latent heat of vaporization
C       MAT        I    matric potential  (m)
C       MATDT      I    matric potential at t+dt  (m)
C       N          I    node num starting from surface material
C       NS         I    num of soil layers
C       NSALT      I    num of different types of solutes to be simulated
C       QSL        I    ave liquid flux for time step between soil nodes
C       QSV        I    vapor flux between soil node i and i+1
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOI       P    density of ice   (kg/m^3)
C       RHOL       P    density of liquid water  (kg/m^3)
C       SALTKQ     I    adsorption char of solute j for node i  (kg/kg)
C       SAT        I    sat WC
C       SS         I    heat souce for soil layer
C       T          L    temp
C       TDT        L    temp at end of time step
C       TK         L    ave thermal cond over the time step
C       TKDT       L    thermal cond at the end of the time step
C       TKM        I    thermal cond of mineral
C       TKT        L    thermal cond at the beginning of the time step
C       TKWA       I    weighting factor for thermal cond of air fract
C       TKWI       I    weighting factor for thermal cond of ice fract
C       TKWL       I    weighting factor for thermal cond of liquid fract
C       TKWM       I    weighting factor for thermal cond of mineral fract
C       TS         I    temp of soil layer at time t
C       TSDT       I    temp of soil layer at time t + dt
C       UGAS       P    universal gas constant
C       VIC        I    vol ice content
C       VICDT      I    vol ice content at t+dt
C       VLC        I    vol liquid content
C       VLCDT      I    vol liquid content at t+dt
C       VLCLMT     I    vol WC of soil below which thermal cond
C                       is linear interpolated to that of dry soil
C       WDT        I    weighting factor for end of time step values
C       WT         I    weightin factor for beginning of time step values
C       ZS         I    depth of node i in the soil from the soil surface
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       CONDUC
C                       FSLOPE
C                       SOILHT
C                       SOILTK
C                       SPMOIS
C                       WEIGHT
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
      PARAMETER (MXNOD=300, MAXHOR=12, MXCHEM=15, NODRES=10, nodcan=11,
     +  NODSNO=100, NODTOT=MXNOD+NODCAN+NODRES+NODSNO)
C
c rma\gf
      DOUBLE PRECISION TS(MXNOD),TSDT(MXNOD),MAT(MXNOD),MATDT(MXNOD),
     +  CONC(MXCHEM,MXNOD),CONCDT(MXCHEM,MXNOD),VLC(MXNOD),VLCDT(MXNOD),
     +  VIC(MXNOD),VICDT(MXNOD),QSL(MXNOD),
     +  QSV(MXNOD),SS(MXNOD),TK(MXNOD),TKT(MXNOD),TKDT(MXNOD),
     +  CON(MXNOD),CST(MXNOD),CSDT(MXNOD),
     +  A1(NODTOT),B1(NODTOT),C1(NODTOT),
     +  D1(NODTOT),CS(MXNOD),RHOB(MXNOD),epslon(mxnod),
     +  ZS(MXNOD),SOILHP(13,MAXHOR),SALTKQ(MXCHEM,MXNOD),
     +  sand(mxnod),silt(mxnod),clay(mxnod),om(mxnod),delta(mxnod)
      DOUBLE PRECISION LF,LV
      INTEGER ICES(MXNOD),ICESDT(MXNOD),NDXN2H(mxnod)
C
      PARAMETER(LF=3.35D5,G=9.81D0,UGAS=8.3143D0,RHOL=1.0D3,RHOI=9.2D2)
      PARAMETER(LV=2.5D6,CL=4.2D3,CV=1.86D3)
      DO 4 I=1,NS-1
C        SET UP LIQUID FLUX COEFFICIENTS SO THAT HEAT IS CARRIED IN
C        DIRECTION OF MOISTURE MOVEMENT.
c
         IF (QSL(I) .GT. 0.0d0) THEN
            DELTA(I)=1.0d0
            EPSLON(I)=0.0d0
          ELSE
            DELTA(I)=0.0d0
            EPSLON(I)=-1.0d0
         END IF
    4 CONTINUE
C
C**** CALCULATE THERMAL CONDUCTIVITY
c rma\gf
      IF (ITER .EQ. 1) CALL SOILTK (NS,TKT,VLC,VIC,RHOB,
     +                              sand,silt,clay,om)
      CALL SOILTK (NS,TKDT,VLCDT,VICDT,RHOB,sand,silt,clay,om)
C
C**** OBTAIN AVERAGE CONDUCTIVITY OVER THE TIME STEP
      CALL WEIGHT (NS,TK,TKT,TKDT,WT,WDT)
C
C**** CALCULATE CONDUCTANCE TERMS BETWEEN NODES
      CALL CONDUC (NS,ZS,TK,CON)
C
C**** CALCULATE THE EFFECTIVE SPECIFIC HEAT AT EACH NODE
c rma\gf
      IF (ITER .EQ. 1) CALL SOILHT (NS,CST,VLC,VIC,TS,MAT,CONC,
     +                              RHOB,om,NSALT)
      CALL SOILHT (NS,CSDT,VLCDT,VICDT,TSDT,MATDT,CONCDT,RHOB,
     +             om,NSALT)
C
C**** OBTAIN AVERAGE SPECIFIC HEAT OVER THE TIME STEP
      CALL WEIGHT (NS,CS,CST,CSDT,WT,WDT)
C
C
C**** DETERMINE THE COEFFICIENTS FOR THE SURFACE LAYER
C
      A1(N+1) = WDT*(CON(1) + DELTA(1)*(RHOL*CL*QSL(1) + CV*QSV(1)))
      B1(N) = B1(N)- WDT*(CON(1) + EPSLON(1)*(RHOL*CL*QSL(1)+CV*QSV(1)))
     >             - (ZS(2)-ZS(1))*CSDT(1)/(2.0D0*DT)
      C1(N) = WDT*(CON(1) + EPSLON(1)*(RHOL*CL*QSL(1)+CV*QSV(1)))
      D1(N) = D1(N)-(CON(1) + EPSLON(1)*(RHOL*CL*QSL(1)+CV*QSV(1)))
     >           *(WT*(TS(1)-TS(2))+ WDT*(TSDT(1)-TSDT(2))) 
     >        - LV*QSV(1) + SS(1) - (ZS(2)-ZS(1))/(2.0D0*DT)
     >           *(CS(1)*(TSDT(1)-TS(1)) - RHOI*LF*(VICDT(1)-VIC(1)))
C
C     COMPUTE SURFACE GROUND HEAT FLUX FOR ENERGY OUTPUT
      GFLUX = (CON(1) + EPSLON(1)*(RHOL*CL*QSL(1)+CV*QSV(1)))
     >           *(WT*(TS(1)-TS(2))+ WDT*(TSDT(1)-TSDT(2))) 
     >        + LV*QSV(1) + (ZS(2)-ZS(1))/(2.D0*DT)
     >           *(CS(1)*(TSDT(1)-TS(1)) - RHOI*LF*(VICDT(1)-VIC(1)))
C
C**** CHECK IF ICE IS PRESENT AT THE END OF THE TIME STEP
      IF (ICESDT(1) .NE. 1) GO TO 10
C
C**** ICE IS PRESENT IN LAYER - - ADJUST COEFFICIENTS FOR LATENT HEAT
C**** TRANSFER AND THE SLOPE OF THE WATER CONTENT-TEMPERATURE CURVE
      IF (ICES(1) .EQ. 1) THEN
C        ICE IS PRESENT FOR THE ENTIRE TIME STEP
         T=273.16D0+TS(1)
        ELSE
C        ICE IS PRESENT ONLY AT THE END OF THE TIME STEP - DETERMINE
C        THE TEMPERATURE AT WHICH THE SOIL WILL BEGIN TO FREEZE
         TLCONC=0.0D0
         DO 5 K=1,NSALT
            TLCONC=TLCONC + CONC(K,1)
    5    CONTINUE
         TMPFRZ=273.16D0*LF/G/(LF/G-MAT(1)+TLCONC*UGAS*(TS(1)+273.17)/G)
         T=TMPFRZ
      END IF
      TDT=273.16D0+TSDT(1)
C     DETERMINE SLOPE OF LIQUID CONTENT-TEMPERATURE CURVE
      CALL FSLOPE (1,NS,DLDTDT,DUMMY,TDT,MATDT,CONCDT,VLCDT,SOILHP,
     +             NDXN2H,RHOB,NSALT,SALTKQ)
      CALL FSLOPE (1,NS,DLDT,DUMMY,T,MAT,CONC,VLC,SOILHP,NDXN2H,
     +             RHOB,NSALT,SALTKQ)
C     ENTER MATVLC TO DETERMINE SLOPE OF LIQUID-MATRIC POTENTIAL CURV
CCCC  CALL MATVL3 (1,MATDT(1),VLCDT(1),DLDM,B,ENTRY)
      JH=NDXN2H(1)
C LMA UPDATE SPMOIS
      DLDM=SPMOIS(MATDT(1)*1.0D2,SOILHP(1,JH),JH)*1.0D2
      B1(N)=B1(N) - (ZS(2)-ZS(1))/(2.0D0*DT)*0.5D0*RHOL*LF*(DLDT+DLDTDT)
C
C**** DETERMINE THE MATRIX COEFFICIENTS FOR THE REST OF THE PROFILE
   10 DO 20 I=N+1,N+NS-2
         J=I-N+1
         A1(I+1)=WDT*(CON(J) + DELTA(J)*(RHOL*CL*QSL(J) +CV*QSV(J)))
         B1(I)= -WDT*(CON(J-1)+CON(J)
     >         +(DELTA(J-1)*(RHOL*CL*QSL(J-1)+CV*(QSV(J-1)))
     >         + EPSLON(J)*(RHOL*CL*QSL(J)+CV*QSV(J))))
     >         - (ZS(J+1)-ZS(J-1))*CSDT(J)/(2.0D0*DT)
         C1(I)=WDT*(CON(J) + EPSLON(J)*(RHOL*CL*QSL(J)+CV*QSV(J)))
         D1(I)=(CON(J-1) + DELTA(J-1)*(RHOL*CL*QSL(J-1) + CV*QSV(J-1)))
     >            *(WT*(TS(J-1)-TS(J)) + WDT*(TSDT(J-1)-TSDT(J)))
     >        -(CON(J) + EPSLON(J)*(RHOL*CL*QSL(J) + CV*QSV(J)))
     >            *(WT*(TS(J)-TS(J+1)) + WDT*(TSDT(J)-TSDT(J+1)))
     >        -LV*(QSV(J)-QSV(J-1)) + SS(J)
     >        -(ZS(J+1)-ZS(J-1))/(2.0D0*DT)
     >            *(CS(J)*(TSDT(J)-TS(J)) - RHOI*LF*(VICDT(J)-VIC(J)))
C
C****    CHECK IF ICE IS PRESENT AT THE END OF THE TIME STEP
         IF (ICESDT(J) .NE. 1) GO TO 20
C
C**** ICE IS PRESENT IN LAYER - - ADJUST COEFFICIENTS FOR LATENT HEAT
C**** TRANSFER AND THE SLOPE OF THE WATER CONTENT-TEMPERATURE CURVE
         IF (ICES(J) .EQ. 1) THEN
C           ICE IS PRESENT FOR THE ENTIRE TIME STEP
            T=273.16D0+TS(J)
          ELSE
C           ICE IS PRESENT ONLY AT THE END OF THE TIME STEP - DETERMINE
C           THE TEMPERATURE AT WHICH THE SOIL WILL BEGIN TO FREEZE
            TLCONC=0.0D0
            DO 15 K=1,NSALT
               TLCONC=TLCONC + CONC(K,J)
   15       CONTINUE
            TMPFRZ=273.16D0*LF/G/
     +            (LF/G-MAT(J)+TLCONC*UGAS*(TS(J)+273.16d0)/G)
            T=TMPFRZ
         END IF
         TDT=273.16D0+TSDT(J)
C        DETERMINE SLOPE OF LIQUID CONTENT-TEMPERATURE CURVE
         CALL FSLOPE (J,NS,DLDTDT,DUMMY,TDT,MATDT,CONCDT,VLCDT,
     +                SOILHP,NDXN2H,RHOB,NSALT,SALTKQ)
         CALL FSLOPE (J,NS,DLDT,DUMMY,T,MAT,CONC,VLC,SOILHP,NDXN2H,
     +                RHOB,NSALT,SALTKQ)
C        ENTER MATVLC FOR SLOPE OF LIQUID-MATRIC POTENTIAL CURVE
CCCC     CALL MATVL3 (J,MATDT(J),VLCDT(J),DLDM,B,ENTRY)
         JH=NDXN2H(J)
C LMA UPDATE SPMOIS
         DLDM=SPMOIS(MATDT(J)*1.0D2,SOILHP(1,JH),JH)*1.0D2
         B1(I)=B1(I) - (ZS(J+1)-ZS(J-1))/(2.0D0*DT)
     +                *0.5D0*RHOL*LF*(DLDT+DLDTDT)
C     >       - WDT*RHOL*LF*(CONH(J-1)+CONH(J))*DLDTDT/DLDM  !THIS IS USED RECENTLY DEPENDING ON CONH
   20 CONTINUE
      N=N+NS-2
      RETURN
      END
