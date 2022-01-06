C
      SUBROUTINE EBRES(ISHUTL,N,NR,NSP,TR,TRDT,GMC,GMCDT,VAPR,VAPRDT,
     +  SR,EVAPR,EVAPK,ITER,WT,WDT,DT,WINDR,RHOR,ZR,RESCOF,GMCMAX,RHOSP,
     +  evapmx,presur,A1,B1,C1,D1,ISTRESS,RESTKB)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBOUTINE CALCULATES THE NEWTON-RAPHSON COEFFICIENTS FOR THE
C     ENERGY BALANCE OF THE RESIDUE LAYERS.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       A1        I/O   first term in Jacobian  matrix for energy balance
C       B1        I/O   diagonal term in Jacobian matrix for energy balance
C       C1        I/O   third term in Jacobian  matrix for energy balance
C       CON        L    cond term between nodes I and I+1  K/(Z(I+1)-Z(I))
C       CRES       L    vol spec heat of residue plus absorbed water
C       CRESDT     L    vol spec heat of residue plus absorbed water at t+dt
C       CREST      L    vol spec heat of residue plus absorbed water at t
C       D1        I/O   residual term in Newton-Raphson iteration for
C                       energy balance
C       GMC        I    grav water content of i-th residue node at time t
C       GMCDT      I    grav water content of i-th residue node at t+dt
C       I          L    canopy, snow, residue or soil layer
C       ITER       I    current num of iter for energy and water balance
C       J          L    specifies solute species in soil or plant species
C       LV         P    latent heat of vaporization
C       N          I    node num starting from surface material
C       NR         I    num of matted residue layers
C       NSP        I    num of layers in snowpack
C       RHOR       I    density of residue in matted layer  (kg/m^3)
C       RHOSP      I    density of ice fract of snowpack  (kg/m^3)
C       SR         I    source term in energy balance for i-th residue node
C       TKRES      L    effective heat transfer coeff for residue (W/m^2-K)
C       TR         I    temp of residue layer at time t
C       TRDT       I    temp of residue layer at time t + dt
C       WDT        I    weighting factor for end of time step values
C       WINDR      I    windspeed at the top of the residue
C       WT         I    weightin factor for beginning of time step values
C       ZR         I    depth of node i in residue from the residue top
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       CONDUC
C                       RESHT
C                       RESTK
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
      PARAMETER (MXNOD=300, NODRES=10, NODSNO=100, 
     +  NODTOT=MXNOD+NODRES+NODSNO)
C
      DOUBLE PRECISION TR(NODRES),TRDT(NODRES),GMC(NODRES),
     +  GMCDT(NODRES),VAPR(NODRES),VAPRDT(NODRES),SR(NODRES),
     +  CON(NODRES),CRES(NODRES),CREST(NODRES),
     +  CRESDT(NODRES),TKRES(NODRES),A1(NODTOT),B1(NODTOT),C1(NODTOT),
     +  D1(NODTOT),RHOR(NODRES),ZR(NODRES),RESCOF(NODRES),RHOSP(NODSNO),
     +  WINDR(NODRES),EVAPR(NODRES),EVAPK(NODRES),gmcmax(nodres),
     +  convec(nodres),qvr(nodres),vapcon(nodres),conv(nodres),
     +  RESTKB(NODRES)
      DOUBLE PRECISION LV,RHOL
      PARAMETER (LV=2.5D6,RHOL=1.0D3)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
C**** DETERMINE THE EVAPORATION FROM RESIDUE ELEMENTS AT EACH NODE
C      CALL RESVAP (NR,EVAPR,EVAPK,TR,TRDT,GMC,GMCDT,VAPR,VAPRDT,
C     +  WT,WDT,DT,RHOR,ZR,RESCOF,GMCMAX)
      IF (ISHUTL .EQ. 0) THEN 
!****    DETERMINE THE EVAPORATION FROM RESIDUE ELEMENTS AT EACH NODE
         CALL RESVAP(NR,EVAPR,EVAPK,TR,TRDT,GMC,GMCDT,VAPR,VAPRDT,
     &            WT,WDT,DT,RHOR,ZR,RESCOF,GMCMAX,ISTRESS)
      ELSE
!        RESIDUE EVAPORATION NOT CONSIDERED UNLESS ISHUTL = 0
         DO I=1,NR
            EVAPR(I)=0.0D0
            EVAPK(I)=0.0d0
         END DO
      END IF
C
C**** DETERMINE THE HEAT TRANSFER COEFFICIENT FOR EACH NODE
      CALL RESTK (NR,NSP,TKRES,CONVEC,TR,TRDT,GMC,GMCDT,WT,WDT,WINDR,
     +            RHOR,RHOSP,RESTKB,ZR)
      NR1=NR+1
      TKRES(NR1)=TKRES(NR)
      CONVEC(NR1) = CONVEC(NR)
C
C**** DETERMINE THE CONDUCTANCE TERM FOR HEAT TRANSPORT BETWEEN NODES
c possible site

      CALL CONDUC (NR1,ZR,TKRES,CON)
C
      IF (ISHUTL .EQ. 0) THEN 
C**** DETERMINE THE VAPOR TRANSPORT FROM THE THERMAL CONVECTION
         CALL RESVK (NR1,NSP,TR,TRDT,CONVEC,VAPCON,WT,WDT,PRESUR)
         VAPCON(NR1)=VAPCON(NR)
C
C**** DETERMINE THE CONDUCTANCE TERM FOR CONVECTIVE VAPOR TRANSPORT
         CALL CONDUC (NR1,ZR,VAPCON,CONV)
C
C**** DETERMINE THE VAPOR FLUX BETWEEN RESIDUE NODES
         CALL QVRES (NR,QVR,CONV,VAPR,VAPRDT,WT,WDT)
c gnf check if soil can supply water for evaporation
c         if (-qvr(nr).gt.evapmx*rhol) then
             if (istress.lt.3) then     !ISTRESS=3
             qvr(nr)=-evapmx*rhol
             conv(nr)=0.0
             else
c            Limit flux to residue since it is contolled by evapmx 
c             qvr(nr)=qvr(nr)*(1.0d0-pplastic)
C             conv(nr)=conv(nr)    !*(1.0d0-pplastic)
             endif
c         end if
      ELSE
!        VAPOR TRANSER THROUGH RESIDUE NOT CONSIDERED UNLESS ISHUTL = 0
         DO I=1,NR
            QVR(I)=0.0
            CONV(I)=0.0           
         END DO
      END IF
C
C**** DETERMINE THE VOLUMETRIC HEAT CAPACITY OF EACH NODE
      IF (ITER .EQ. 1) CALL RESHT (NR,CREST,GMC,RHOR,PPLASTIC)
      CALL RESHT (NR,CRESDT,GMCDT,RHOR,PPLASTIC)
C
C**** CALCULATE THE AVERAGE HEAT CAPACITY OVER THE TIME STEP
C
      CALL WEIGHT (NR,CRES,CREST,CRESDT,WT,WDT)
C
C
C**** DETERMINE THE MATRIX COEFFICIENTS FOR THE TOP LAYER
C
      A1(N+1)=A1(N+1) + WDT*CON(1)
      C1(N)=C1(N) + WDT*CON(1)
      IF (NR.GT.1) THEN
         B1(N)=B1(N) - WDT*CON(1) - (ZR(2)-ZR(1))/(2.0D0*DT)*CRES(1)
         D1(N)=D1(N) - CON(1)*(WT*(TR(1)-TR(2))+WDT*(TRDT(1)-TRDT(2)))
     +               + LV*EVAPR(1) + SR(1)
     +               - (ZR(2)-ZR(1))/(2.0D0*DT)*CRES(1)*(TRDT(1)-TR(1))
        ELSE
         B1(N)=B1(N) - WDT*CON(1) - (ZR(2)-ZR(1))/DT*CRES(1)
         D1(N)=D1(N) - CON(1)*(WT*(TR(1)-TR(2))+WDT*(TRDT(1)-TRDT(2)))
     +               + LV*EVAPR(1) + SR(1)
     +               - (ZR(2)-ZR(1))/DT*CRES(1)*(TRDT(1)-TR(1))
      END IF
C
C**** DETERMINE THE COEFFICIENTS FOR THE REST OF THE LAYERS
      DO 10 I=N+1,N+NR-1
         J=I-N+1
         A1(I+1)=A1(I+1) + WDT*CON(J)
         C1(I)=C1(I) + WDT*CON(J)
         IF (J .NE. NR) THEN
            B1(I)=B1(I) - WDT*(CON(J-1)+CON(J))
     +                  - (ZR(J+1)-ZR(J-1))/(2.0D0*DT)*CRES(J)
            D1(I)=CON(J-1)*(WDT*(TRDT(J-1)-TRDT(J))+WT*(TR(J-1)-TR(J)))
     +        -CON(J)*(WDT*(TRDT(J)-TRDT(J+1)) + WT*(TR(J)-TR(J+1)))
     +        +LV*EVAPR(J) + SR(J)
     +        -(ZR(J+1)-ZR(J-1))/(2.0D0*DT)*CRES(J)*(TRDT(J)-TR(J))
          ELSE
            B1(I)=B1(I) - WDT*(CON(J-1)+CON(J))
     +                - (ZR(J+1)-ZR(J)+(ZR(J)-ZR(J-1))/2.0D0)/DT*CRES(J)
            D1(I)=CON(J-1)*(WDT*(TRDT(J-1)-TRDT(J))+WT*(TR(J-1)-TR(J)))
     +       -CON(J)*(WDT*(TRDT(J)-TRDT(J+1)) + WT*(TR(J)-TR(J+1)))
     +       +LV*EVAPR(J)+SR(J)-(ZR(J+1)-ZR(J)+(ZR(J)-ZR(J-1))/2.0D0)
     +       /DT*CRES(J)*(TRDT(J)-TR(J))
         END IF
   10 CONTINUE
C
C**** DETERMINE THE COEFFICIENTS FOR THE SOIL SURFACE
C
      N=N+NR
      B1(N)=B1(N) - WDT*CON(NR)
      D1(N)=CON(NR)*(WDT*(TRDT(NR)-TRDT(NR+1))+WT*(TR(NR)-TR(NR+1)))
     >      + LV*QVR(NR) 
      RETURN
      END
