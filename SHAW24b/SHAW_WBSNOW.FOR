      SUBROUTINE WBSNOW(NSP,ICESPT,TSPDT,DLW,DLWDT,RAIN,TRAIN,SCOUT,
     +                   DT,TOPSNO,TQVSP,RHOSP,DZSP,ZSP,STORSN)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE PERFORMS A WATER BALANCE OF THE SNOWPACK BY
C     ADSUSTING THE DENSITY FOR VAPOR FLUX AND ANY MELT WHICH OCCURRED
C     OVER THE TIME STEP.  CHECKS ARE MADE TO SEE IF ANY LAYERS HAVE
C     DISAPPEARED DUE TO MELT, OR IF ANY LAYERS ARE OUTSIDE THE
C     ACCEPTABLE THICKNESS RANGE.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       ABOVE      L    long-wave rad entering node from above
C       CI         P    spec heat cap of ice  (J/kg-K)
C       CL         P    spec heat cap of liquid water  (J/kg-K)
C       CTHICK     P    coeff that allows snow layer thickness to increase
C       DLW        I    depth of liquid water for i-th snow pack node
C       DLWDT     I/O   liquid water depth for i-th snow pack node at
C                       end of time step
C       DZSP      I/O   thickness of i-th snowpack node
C       I          L    canopy, snow, residue or soil layer
C       ICESPT    I/O   1 if snow layer contains both ice and water at t+dt
C       J          L    specifies solute species in soil or plant species
C       LF         P    latent heat of fusion
C       NSP       I/O   num of layers in snowpack
C       RAIN       I    precip or depth of rain entering
C       RHOL       P    density of liquid water  (kg/m^3)
C       RHOSP     I/O   density of ice fract of snowpack  (kg/m^3)
C       SCOUT     I/O   liquid water outflow from snowpack for the hour
C       THICK      P    desired thickness of snowpack layers
C       TOPSNO     I    sum of vapor entering the top of the snowpack
C       TQVSP      I    sum of vapor flux in snow pack over the hour
C       TRAIN      I    rain temp
C       TSPDT     I/O   temp of snowpack layer at time t + dt
C       UGAS       P    universal gas constant
C       ZSP       I/O   depth of node i in the snow from the snowpack top
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       META
C                       SNOMLT
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
      DOUBLE PRECISION TSPDT(NODSNO),DLW(NODSNO),DLWDT(NODSNO), LF,
     +  RHOSP(NODSNO),DZSP(NODSNO),ZSP(NODSNO),TQVSP(NODSNO)
      INTEGER ICESPT(NODSNO)
      PARAMETER(LF=3.35D5,RHOL=1.0D3,CI=2.1D3,CL=4.2D3)
      PARAMETER (THICK=2.5D-2, CTHICK=5.0D-2)
      double precision store
C
      SCOUT = 0.0
C
C
C     ADJUST THE DENSITY FOR VAPOR FLUX
C     (THICKNESS OF LAYER IS ADJUSTED FOR SURFACE LAYER)
C
      CHANGE = (TOPSNO - TQVSP(1))/RHOSP(1)
      DZSP(1) = DZSP(1) + CHANGE
      IF (DZSP(1).LE.0.0 .AND. NSP.EQ.1) THEN
C        SNOWPACK IS LOST TO SUBLIMATION -- ADJUST TOPSNO AND EVAP1 FOR
C        THE WATER BALANCE TO CLOSE AND RETURN TO CALLING SUBROUTINE
         TOPSNO=TOPSNO - DZSP(1)*RHOSP(1)         
         EVAP1=EVAP1 - DZSP(1)*RHOSP(1)/RHOL
         NSP=0
         RETURN
      END IF
      DO 10 I=2,NSP
         CHANGE = (TQVSP(I-1) - TQVSP(I))/DZSP(I)
         RHOSP(I) = RHOSP(I) + CHANGE
   10 CONTINUE
C
C     ADJUST LAYERS FOR MELT AND RAINFALL
C        ABOVE = THE RAIN OR THE MELTWATER FROM ABOVE LAYERS
C        EXCESS= THIS IS THE MELTWATER THAT WOULD HAVE BEEN PRODUCED
C                BY ABOVE LAYERS HAD THERE BEEN ENOUGH ICE IN THE LAYER
C                FOR ALL THE ENERGY ABSORBED.  (FOR THE FIRST LAYER,
C                THIS TERM INCLUDES THE ENERGY TRANSFERRED BY RAIN.)
      NL=NSP
      ABOVE= RAIN
      EXCESS= RAIN*(CL*TRAIN+LF)/LF
C
      DO 20 I=1,NSP
      IF (ICESPT(I) .EQ. 0) THEN
C
C        TEMPERATURE IS UNKNOWN--SOME EXCESS LIQUID-WATER FROM
C        THE ABOVE LAYER MUST BE FROZEN.
C
         IF (EXCESS .NE. 0.0D0) THEN
C           COMPUTE AMOUNT TO BE FROZE IN ORDER TO RAISE THE TEMPERATURE
C           TO ZERO DEGREES CELSIUS. (USE SPECIFIC HEAT OF ICE, CI)
            WEL=RHOSP(I)*DZSP(I)/RHOL
            FREEZE = -1.0D0*TSPDT(I)*WEL*CI/LF
            IF (EXCESS .GT. FREEZE) THEN
C              EXCESS EXCEEDS REFREEZE.
               TSPDT(I)=0.0D0
               WEL=WEL+FREEZE
               RHOSP(I)=RHOL*WEL/DZSP(I)
               EXCESS=EXCESS-FREEZE
               ABOVE=ABOVE-FREEZE
               ICESPT(I) = 1
             ELSE
C              EXCESS IS ALL FROZEN IN THIS LAYER.
               TSPDT(I)=TSPDT(I) +(EXCESS*LF*RHOL)/(CI*RHOSP(I)*DZSP(I))
               WEL=WEL+EXCESS
               RHOSP(I)=RHOL*WEL/DZSP(I)
               ABOVE = ABOVE - EXCESS
               EXCESS=0.0D0
            END IF
         END IF
      END IF
C
      DLWDT(I)= DLWDT(I) + EXCESS
      CHANGE= DLWDT(I) - DLW(I) - ABOVE
C
      IF (ABS(CHANGE).LT.1.0D-7) THEN
C        THIS IS BEYOND PRECISION OF COMPUTER -- ASSUME ZERO
         ABOVE=0.0
         EXCESS=0.0
       ELSE
         IF (CHANGE .LE. 0.0D0) THEN
C        SOME LIQUID-WATER IS FROZEN. ADD TO ICE CONTENT OF LAYER.
         WEL=RHOSP(I)*DZSP(I)/RHOL
         WEL=WEL-CHANGE
         RHOSP(I)=RHOL*WEL/DZSP(I)
            ABOVE=0.0
            EXCESS=0.0
       ELSE
C        MELT HAS OCCURRED,SUBTRACT FROM ICE CONTENT
         WEL=RHOSP(I)*DZSP(I)/RHOL
C        IF MELT EXCEEDS ICE CONTENT OF LAYER--LAYER IS GONE.
         IF (CHANGE .GE. WEL) THEN
C           LAYER IS GONE
C           LIQUID-WATER IS ADDED TO THE LAYER BELOW.(EXCESS).THE ICE
C           CONTENT PLUS WT OF THE LAYER CANNOT BE TAKEN FROM THE NEXT
C           LAYER,BUT IS STILL ADDED TO THE LIQUID-WATER CONTENT OF
C           THE NEXT LAYER (ABOVE).
            NL=NL-1
            ABOVE= ABOVE + WEL + DLW(I)
            EXCESS=DLWDT(I)
            DZSP(I)=0.0D0
          ELSE
C           LAYER REMAINS
            WEL=WEL-CHANGE
            DZSP(I)=RHOL*WEL/RHOSP(I)
               ABOVE=0.0
               EXCESS=0.0
            END IF
         END IF
      END IF
C
   20 CONTINUE
C
C***********************************************************************
C     CHECK TO SEE IF ENTIRE SNOW COVER IS GONE.
      IF (NL .LE. 0) THEN
         NSP = 0
         WE = 0.0D0
         TDEPTH = 0.0D0
         SCOUT = ABOVE
         CALL SNOMLT (NSP,ICESPT,DZSP,RHOSP,TSPDT,DLWDT,SCOUT,DT,STORSN
     +         ,store)
         RETURN
      END IF
C
C     ELIMINATE LAYERS WHICH ARE GONE.
      IF (NL .NE. NSP) THEN
         DO 30 I=1,NSP
   23       IF (DZSP(I) .GT. 0.0D0) GO TO 30
C           LAYER GONE. MOVE OTHER LAYERS UP.
            NEXT=I+1
            DO 25 J=NEXT,NSP
               L=J-1
               DZSP(L)=DZSP(J)
               RHOSP(L)=RHOSP(J)
               TSPDT(L)=TSPDT(J)
               TQVSP(L)=TQVSP(J)
               DLWDT(L)=DLWDT(J)
               ICESPT(L)=ICESPT(J)
   25       CONTINUE
            NSP = NSP - 1
            IF (NSP .EQ. NL) GO TO 35
            GO TO 23
   30    CONTINUE
   35    DLWDT(NSP)= DLWDT(NSP) + ABOVE
      END IF
C
C***********************************************************************
C     CHECK THICKNESS OF EACH LAYER. IF NOT WITHIN SPECIFIED LIMITS,
C     DIVIDE (IF TOO LARGE) OR ADD TO AN ADJACENT LAYER (IF TOO SMALL).
      TDEPTH=0.0D0
      I=1
      IF (NSP .GT. 1) THEN
        IKE = NSP
        DO 70 IK = 1,IKE
C
          ZZ=TDEPTH+DZSP(I)*0.5D0
C         COMPUTED DESIRED THICKNESS FOR THE LAYER.
          DZ1=THICK
          IF (ZZ .GT. 0.30D0)  DZ1=CTHICK*(ZZ-0.30D0)+THICK
C
C         ..CHECK ACTUAL THICKNESS AGAINST DESIRED THICKNESS.
C
          IF (DZSP(I) .GT. 1.55D0*DZ1) THEN
C***********************************************************************
C          THICKNESS IS GREATER THAN SPECIFIED LIMTS.
C          SUB-DIVIDE LAYER,THUS CREATING A NEW LAYER.
C          PROPERTIES ARE THE SAME FOR BOTH LAYERS.
C          MOVE OTHER LAYERS DOWN.
C
            NEXT=I+1
            IF (NEXT .LE. NSP) THEN
               DO 55 J=NEXT,NSP
                  L=NSP-J+NEXT
                  LL=L+1
                  DZSP(LL)=DZSP(L)
                  RHOSP(LL)=RHOSP(L)
                  TSPDT(LL)=TSPDT(L)
                  TQVSP(LL)=TQVSP(L)
                  DLWDT(LL)=DLWDT(L)
                  ICESPT(LL)=ICESPT(L)
   55          CONTINUE
            END IF
            TDEPTH=TDEPTH+DZSP(I)
            DZSP(NEXT)=DZSP(I)*0.5D0
            RHOSP(NEXT)=RHOSP(I)
            TSPDT(NEXT)=TSPDT(I)
            TQVSP(NEXT)=TQVSP(I)
            DLWDT(NEXT)=DLWDT(I)*0.5D0
            ICESPT(NEXT)=ICESPT(I)
            DZSP(I)=DZSP(I)*0.5D0
            DLWDT(I)=DLWDT(I)*0.5D0
            TQVSP(I)=0.0D0
            I=I+1
            NSP=NSP+1
C
          ELSE IF (DZSP(I) .LT. 0.55D0*DZ1 .AND. NSP.GT.1) THEN
C***********************************************************************
C           THICKNESS IS SMALLER THAN SPECIFIED LIMITS.
C           ADD TO THE SMALLEST ADJACENT LAYER, THUS
C           LOSING A LAYER. PROPERIES OF THE NEW LAYER
C           ARE THE WEIGHTED AVERAGE OF THE TWO FORMER
C           LAYERS. MOVE OTHER LAYERS UP.
            IF (I .EQ. 1  .OR.  I .EQ. NSP) THEN
              IF (I .EQ. 1) THEN
                NL=2
              ELSE
                NL=NSP-1
              END IF
            ELSE
              NL=I-1
              IF (DZSP(I+1) .LT. DZSP(I-1)) NL=I+1
            END IF
C
C           ..NL IS THE SMALLEST ADJACENT LAYER - ADD LAYER I TO LAYER NL
            WEI=RHOSP(I)*DZSP(I)/RHOL
            WENL=RHOSP(NL)*DZSP(NL)/RHOL
            WEL=WEI+WENL
            DZSP(NL)=DZSP(NL)+DZSP(I)
            RHOSP(NL)=RHOL*WEL/DZSP(NL)
            DLWDT(NL)= DLWDT(NL) + DLWDT(I)
            TSPDT(NL)=(WENL*TSPDT(NL)+WEI*TSPDT(I))/WEL
            IF (NL .LT. I) TQVSP(NL) = TQVSP(I)
C
            IF (ICESPT(I) .NE. ICESPT(NL)) THEN
C              UNKNOWNS ARE DIFFERENT. COMPUTE THE UNKNOWN FOR NEW LAYER.
               FREEZE = -1.0D0*(TSPDT(NL)*CI*RHOSP(NL)*DZSP(NL))/
     +                  (LF*RHOL)
               IF (DLWDT(NL) .LE. FREEZE) THEN
C                 ..TEMPERATURE IS UNKNOWN
                  TSPDT(NL)=TSPDT(NL) + (DLWDT(NL)*LF*RHOL)
     +                                  /(CI*RHOSP(NL)*DZSP(NL))
                  WEL=WEL+DLWDT(NL)
                  RHOSP(NL)=RHOL*WEL/DZSP(NL)
                  ICESPT(NL)=0
               ELSE
C                 ..LIQUID-WATER IS UNKNOWN.
                  DLWDT(NL)=DLWDT(NL)-FREEZE
                  WEL=WEL+FREEZE
                  RHOSP(NL)=RHOL*WEL/DZSP(NL)
                  ICESPT(NL)=1
               END IF
            END IF
C
            IF (ICESPT(NL) .EQ. 1) TSPDT(NL)=0.0D0
            IF (ICESPT(NL) .EQ. 0) DLWDT(NL)=0.0D0
C           ..MOVE OTHER LAYERS UP.
            IF (NL .LT. I) TDEPTH=TDEPTH+DZSP(I)
            NEXT=I+1
C           
            IF (NEXT .LE. NSP) THEN
               DO 65 J=NEXT,NSP
                  L=J-1
                  DZSP(L)=DZSP(J)
                  RHOSP(L)=RHOSP(J)
                  TSPDT(L)=TSPDT(J)
                  TQVSP(L)=TQVSP(J)
                  DLWDT(L)=DLWDT(J)
                  ICESPT(L)=ICESPT(J)
   65          CONTINUE
            END IF
C           
            I = I-1
            NSP = NSP-1
C
          ELSE 
C***********************************************************************
C           THICKNESS IS WITHIN SPECIFIED LIMITS.
            TDEPTH=TDEPTH+DZSP(I)
          ENDIF
C
          I=I+1
   70   CONTINUE
      END IF
C
C***********************************************************************
C     ADJUST THE DENSITY FOR METAMORPHISM AND COMPUTE ANY SNOMELT
      CALL META (NSP,TSPDT,RHOSP,DZSP,DLWDT,DT)
      CALL SNOMLT (NSP,ICESPT,DZSP,RHOSP,TSPDT,DLWDT,SCOUT,DT,STORSN,
     +    store)
C
C***********************************************************************
C     CALCULATE THE DEPTH FOR EACH NODE
      ZSP(1) = 0.0D0
      TDEPTH = DZSP(1)
      WE = DZSP(1)*RHOSP(1)/RHOL + DLWDT(1)
      DO 85 I=2,NSP
         ZSP(I)= TDEPTH + DZSP(I)/2.0D0
         TDEPTH= TDEPTH + DZSP(I)
         WE = WE + DZSP(I)*RHOSP(I)/RHOL + DLWDT(I)
   85 CONTINUE
      ZSP(NSP+1) = TDEPTH
C
      RETURN
      END
C
