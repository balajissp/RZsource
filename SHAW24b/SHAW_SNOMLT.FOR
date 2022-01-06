      SUBROUTINE SNOMLT(NSP,ICESPT,DZSP,RHOSP,TSPDT,DLWDT,SCOUT,DT,
     +  STORSN,store)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE DETERMINES THE SNOW COVER OUTFLOW DURING EACH STEP
C     BASED ON THE CHANGE IN LIQUID-WATER DURING THE PERIOD AND
C     THE PHYSICAL CHARTERISTICS OF THE SNOW COVER.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CI         P    spec heat cap of ice  (J/kg-K)
C       CLAG1      L    coeff for lag and attenuation of snowpack water
C       CLAG2      L    coeff for lag and attenuation of snowpack water
C       CLAG3      L    coeff for lag and attenuation of snowpack water
C       CLAG4      L    coeff for lag and attenuation of snowpack water
C       CTHICK     P    coeff that allows snow layer thickness to increase
C       DLWDT     I/O   liquid water depth for i-th snow pack node at
C                       end of time step
C       DZSP       I    thickness of i-th snowpack node
C       I          L    canopy, snow, residue or soil layer
C       ICESPT    I/O   1 if snow layer contains both ice and water at t+dt
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       NLAG       L    num of lags for snowcover outflow
C       NSP        I    num of layers in snowpack
C       PLWDEN     L    snow density above which is its min water holding cap
C       PLWHC      L    min liquid water holding cap of snow
C       PLWMAX     L    max liquid water holding cap of snow
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOSP      I    density of ice fract of snowpack  (kg/m^3)
C       SCOUT     I/O   liquid water outflow from snowpack for the hour
C       STORE      L    excess water in snowpack currently being attenuated
C       THICK      P    desired thickness of snowpack layers
C       TSPDT     I/O   temp of snowpack layer at time t + dt
C       WLAG       L    depth of water for i-th lag of snowpack outflow
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
c
      DOUBLE PRECISION DZSP(NODSNO),RHOSP(NODSNO),TSPDT(NODSNO),
     +  DLWDT(NODSNO),WLAG(11)
      DOUBLE PRECISION LF
c
      INTEGER ICESPT(NODSNO)
      PARAMETER(LF=3.35D5,RHOL=1.0D3,CI=2.1D3)
      SAVE IFIRST,CLAG1,CLAG2,CLAG3,CLAG4,PLWMAX,PLWDEN,PLWHC,WLAG,
     +     IHSV
C
      DATA IFIRST/0/
      DATA  CLAG1,CLAG2 ,CLAG3 ,CLAG4 ,PLWMAX, PLWDEN , PLWHC
     +    /10.0D0, 1.0D0, 5.0D0, 4.5D2, 0.1D0, 200.0D0, 0.03D0/
      DATA IHSV/1/
C
      IF(IFIRST .EQ. 0) THEN
C        INITIALIZE VARIABLES.
C        1. STORE IS THE AMOUNT OF LIQUID-WATER(THAT HAS ALREADY BEEN
C           LAGGED) THAT IS IN STORAGE IN THE SNOW-COVER.
C        2. WLAG() IS THE AMOUNT OF LIQUID-WATER IN THE PROCESS OF
C           BEING LAGGED.  NLAG IS THE NUMBER OF ARRAY ELEMENTS USED.
         STORE=0.0D0
         JDT=int(CLAG1+0.01D0)
         NLAG=JDT+2
         IF (NLAG.GT.11) NLAG=11
         DO 5 J=1,NLAG
            WLAG(J)=0.0D0
    5    CONTINUE
         AMT2LAG = 0.0D0
         IFIRST=1
      END IF
C
      IF (NSP .LE. 0) THEN
C        ..SNOW COVER HAS JUST DISAPPEARED
         DO 10 J=1,NLAG
            SCOUT=SCOUT+WLAG(J)
            WLAG(J)=0.0D0
   10    CONTINUE
         SCOUT=SCOUT+STORE
         STORE=0.0D0
      ELSE
C
C       DETERMINE THE EXCESS LIQUID-WATER (IN EXCESS OF LIQUID-WATER
C       HOLDING CAPACITY) GENERATED DURING THIS TIME PERIOD.
C
        EXCESS=0.0D0
C
C       ..EXCESS WATER IN BOTTOM LAYER IS NOT TO BE ROUTED.
        IF (NSP .GT. 1) THEN
          BOTTOM=0.0D0
          IF (ICESPT(NSP) .EQ. 1) THEN
             WEL=RHOSP(NSP)*DZSP(NSP)/RHOL
             PLW=(PLWMAX-PLWHC)*(PLWDEN-RHOSP(NSP))/PLWDEN+PLWHC
             IF (RHOSP(NSP) .GE. PLWDEN) PLW=PLWHC
             WMAX=PLW*WEL
             IF (DLWDT(NSP) .GT. WMAX) BOTTOM= DLWDT(NSP) - WMAX
          END IF
C
          WE=0.0D0
          TDEPTH=0.0D0
        ENDIF
        DO 20 I=1,NSP
           WEL=RHOSP(I)*DZSP(I)/RHOL
           IF (ICESPT(I) .EQ. 0 .AND. EXCESS .NE. 0.0D0) THEN
C             ..LAYER IS BELOW ZERO DEGREES CELSIUS.
              FREEZE = -1.0D0*TSPDT(I)*CI*WEL/LF
              IF (EXCESS .LE. FREEZE) THEN
C                ..EXCESS IS ALL FROZEN IN THIS LAYER.
                 TSPDT(I)=TSPDT(I)+(EXCESS*LF*RHOL)/(CI*WEL*RHOL)
                 WEL=WEL+EXCESS
                 EXCESS=0.0D0
                 RHOSP(I)=RHOL*WEL/DZSP(I)
                 GO TO 15
              ELSE
C                ..EXCESS EXCEEDS REFREEZE.
                 TSPDT(I)= 0.0D0
                 WEL=WEL+FREEZE
                 RHOSP(I)=RHOL*WEL/DZSP(I)
                 EXCESS=EXCESS-FREEZE
                 ICESPT(I)=1
              END IF
           END IF
           IF ((ICESPT(I) .EQ. 0 .AND. EXCESS .NE. 0.0D0) .OR.
     1         (ICESPT(I) .EQ. 1)) THEN
             PLW=(PLWMAX-PLWHC)*(PLWDEN-RHOSP(I))/PLWDEN+PLWHC
             IF (RHOSP(I) .GE. PLWDEN) PLW=PLWHC
             WMAX= PLW*WEL
             W= DLWDT(I) + EXCESS
             IF (W .LE. WMAX) THEN
C               ..LIQUID-WATER HOLDING CAPACITY IS NOT SATISFIED.
                DLWDT(I)=W
                EXCESS=0.0D0
             ELSE
C               ..LIQUID-WATER HOLDING CAPACITY IS EXCEEDED.
                DLWDT(I)=WMAX
                EXCESS=W-WMAX
             END IF
           END IF
   15      WE=WE+WEL
           TDEPTH=TDEPTH+DZSP(I)
   20   CONTINUE
C      
      CSTORSN = 0.0D0
      DO 23 I = 1,NSP
        CSTORSN = CSTORSN + RHOSP(I) * DZSP(I) / RHOL + DLWDT(I)
 23   CONTINUE
C      
        IF (NSP .EQ. 1) THEN
C          ..WATER NOT LAGGED IF ONLY ONE NODE IN THE SNOWPACK
           SCOUT = EXCESS
           DO 50 J=1,NLAG
              SCOUT=SCOUT+WLAG(J)
              WLAG(J)=0.0D0
   50      CONTINUE
           SCOUT=SCOUT+STORE
           STORE=0.0D0
        ELSE IF (ICESPT(NSP) .EQ. 0) THEN
C       
C        DO NOT ALLOW ANY WATER LAGGED OR STORED TO LEAVE SNOWPACK
C          (EXCESS MUST BE EQUAL 0.0D0)  IS IT??
C       
           EXCESS=EXCESS-BOTTOM
           OUTHR=0.0D0
           SCOUT=0.0D0
        ELSE
C       
CKEJ - CHECK SUM OF WLAG
             WSUM1 = 0.0D0
             DO 100 J=1,NLAG
                WSUM1 = WSUM1 + WLAG(J)
  100        CONTINUE
C
          EXCESS=EXCESS-BOTTOM
          OUTHR=0.0D0
ckej
c          scout=excess
C       
C         ROUTE EXCESS WATER THROUGH THE SNOW COVER.
C         EMPIRICAL LAG AND ATTENUATION EQUATIONS - ONE HOUR TIME STEP USED.
       
C??          EXCESS = EXCESS/DT*3600.0D0
C          IDT = INT(DT/3600.0D0+0.01D0)
C         IDT = MAX(IDT,1)
          IDT = 1
C??
C          EXCESS = EXCESS/DBLE(IDT)
          DENSE = WE/TDEPTH
          SCOUT = 0.0D0
C
          DO 35 IHR = 1,IDT
C            ..LAG-FUNCTION OF DEPTH,DENSITY,AND EXCESS WATER.
             IF (EXCESS .GE. 1.0D-5) THEN
                NI = INT(((EXCESS*1.0D4)**0.3D0)+0.5D0)
                IF (NI .LT. 1) NI=1
                FN = DBLE(NI)
                FLMAX = CLAG1*(1.0D0-EXP(-0.25D0*TDEPTH/DENSE))
                DO 25 J = 1,NI
                   FJ = DBLE(J)
                   FLAG = FLMAX/(CLAG2*EXCESS*1.0D2*(FJ-0.5D0)/FN+1.0D0)
                   K = INT(FLAG+1.0D0)
                   POR = DBLE(K)-FLAG
                   WINC = 1.0D0/FN
                   WLAG(K) = WLAG(K)+EXCESS*WINC*POR
                   WLAG(K+1) = WLAG(K+1)+EXCESS*WINC*(1.0D0-POR)
   25           CONTINUE
             ELSE
                WLAG(1)=WLAG(1)+EXCESS
             END IF
CKEJ - CHECK SUM OF WLAG
             WSUM2 = 0.0D0
             DO 101 J=1,NLAG
                WSUM2 = WSUM2 + WLAG(J)
  101        CONTINUE
             chkst1 = store + wlag(1)
             ostr = store
C
C           ..FIND PORTION OF WLAG TO LET MELT THIS TIME STEP
C            IF (INT(HOUR + 1.0D0) .NE. IHSV) THEN
C              AMT2LAG = WLAG(1)
C              IMV = 1
C            ELSE
              AMT2LAG = WLAG(1)*DT/3600.0D0
              IF (WLAG(1) .LE. 1.0D-5) THEN
                AMT2LAG = WLAG(1)
                WLAG(1) = 0.0D0
                IMV = 1
              ELSE
                WLAG(1) = WLAG(1) - AMT2LAG
                IMV = 0
              ENDIF
C            ENDIF
C       
C          ATTENUATION-FUNCTION OF DENSITY AND PREVIOUS OUTFLOW.
             IF ((STORE+AMT2LAG) .NE. 0.0D0) THEN
                R=1.0D0/(CLAG3*EXP(-CLAG4*AMT2LAG*DENSE/TDEPTH)+1.0D0)
                OUTHR=(STORE+AMT2LAG)*R
                STORE=STORE+AMT2LAG-OUTHR
                SCOUT=SCOUT+OUTHR
                IF(STORE .LE. 1.0D-5) THEN
                   OUTHR=OUTHR+STORE
                   SCOUT=SCOUT+STORE
                   STORE=0.0D0
                END IF
             chkst2 = scout + store + wlag(1)
             if (abs(chkst1 - chkst2).gt.1.0d-12) then
               write(9,*)'store check'
               write(9,*)chkst1,chkst2,store,scout,wlag(1),chkst1-chkst2
             endif
             END IF
             IF (IMV.EQ.1) THEN
               NI=NLAG-1
               DO 30 J=1,NI
                  WLAG(J)=WLAG(J+1)
   30          CONTINUE
               WLAG(NLAG)=0.0D0
             ENDIF
   35     CONTINUE
C       
          SCOUT=SCOUT+BOTTOM
CKEJ - CHECK SUM OF WLAG
             WSUM3 = 0.0D0
             DO 102 J=1,NLAG
                WSUM3 = WSUM3 + WLAG(J)
  102        CONTINUE
CKEJ - DO CHECK OF WSUMS
          IF (ABS(WSUM1 + EXCESS - WSUM2).GT.1.0D-12) THEN
            WRITE(9,*) 'SUM1 - SUM2'
            WRITE(9,*) EXCESS,WSUM1,WSUM2,WSUM1 + EXCESS - WSUM2
            STOP
          ELSE IF (ABS(WSUM2+ostr - WSUM3 - SCOUT - STORE + bottom)
     1     .GT.1.0D-12) THEN
            WRITE(9,*) 'SUM2 - SUM3'
            WRITE(9,*) WSUM2,ostr,WSUM3,SCOUT,STORE,bottom,
     1       WSUM2 +ostr- WSUM3 - SCOUT +bottom - STORE
            STOP
          ELSE IF (ABS(WSUM1 + EXCESS+ostr - WSUM3 - SCOUT 
     1    - STORE + bottom) .gt. 1.0D-12) THEN
            WRITE(9,*) 'SUM1 - SUM3'
            WRITE(9,*) EXCESS,ostr,WSUM1,WSUM3,SCOUT,STORE,bottom,
     1       WSUM1 + EXCESS +ostr- WSUM3 - SCOUT +bottom- STORE
            STOP
          ENDIF

        END IF
      END IF
C      
C       ==============================
C       DO A SIMPLE SNOW WATER BALANCE
C       ==============================
C      
      STORSN = 0.0D0
      DO 40 I = 1,NSP
        STORSN = STORSN + RHOSP(I) * DZSP(I) / RHOL + DLWDT(I)
 40   CONTINUE
      DO 45 I =1, NLAG
        STORSN = STORSN + WLAG(I)
 45   CONTINUE
      STORSN = STORSN + STORE
C      IHSV = INT(HOUR + 1.0D0)
C
      RETURN
      END
