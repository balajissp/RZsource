C $Id: Rzrich.for,v 1.1 2002/08/28 00:00:48 rojas Exp $
C
      SUBROUTINE CHKBC(EVAP1,GH,H,HOLD,IHBC,NN,DELT,IREBOT,BOTHED,
     +    BOTFLX,ITER,ISTRT,MAXITR,Hmin,nsp)
C
C=======================================================================
C
C       PURPOSE:  CHECKS BOUNDRY CONDITIONS FOR WATER PROFILE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BOTFLX     I
C       BOTHED     I
C       DELT  I/O INCREMENTAL TIME STEP [HR]
C       EVAP   I  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       GH        I/O 2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2)
C                 BOUNDARY CONDITION VALUES
C       H     I/O NODAL SOIL WATER PRESSURE HEADS [CM]
C       HMAX   P
C       HMIN   P
C       HOLD   I  NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       I      L  INDEX VARIABLE
C       IHBC  I/O 2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2) B.C.
C                 TYPES: 1 (HEAD - DIRICHLET) OR
C                      2 (FLUX - NEUMANN - NORMAL GRAD)
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       ISTRT I/O
C       ITER  I/O ITERATION COUNTER
C       MAXITR     I
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       OHBC   L
C       STRTMA     P
C
C       CALLED FROM:
C
C       PROGRAMMER: KENNETH W. ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(HMAX=0.0D0)  !HMIN=-35000.0D0,
      INTEGER IHBC(2),OHBC,STRTMAX
      PARAMETER(STRTMAX=10)
      DIMENSION H(NN),HOLD(NN),GH(2)
      DATA OHBC /2/
C
C ... CHECK & SET SURFACE BOUNDARY CONDITION TYPE & VALUE
C
      if (nsp.gt.0) then
	evap=0.0d0
	else
	evap=evap1
	endif
      IF(H(1).GT.(HMIN+1.0D-5).AND.H(1).LE.HMAX) THEN
c       IF (H(1).GT.(HMIN+1.0D-5) .AND. H(1).LE.HMAX .and.
c     +  iter.le.maxitr) THEN
C       ...EVAPORATIVE FLUX B.C.
        IHBC(1)=2
        GH(1)=EVAP
        OHBC=2
      ELSEIF(ITER.GT.MAXITR.OR.H(1).GE.HMAX) THEN
C       ...DIVERGING - CHECK CONDITIONS TO DETERMINE WHAT TO DO
C       
        IF(ISTRT.LE.STRTMAX) THEN
C         ...RESET TIME STEP AND BEGIN ITERATIONS AGAIN
          IHBC(1)=2
          if (H(1).GE.HMAX) then  
            GH(1)=EVAP/(2.0d0*(istrt+1))  !done by Liwang Ma to reduce evaporation by half, 5-15-2012
          else
            GH(1)=EVAP  !done by Liwang Ma to reduce evaporation by half, 5-15-2012
          endif
          OHBC=2
          DELT=DELT*2.0D-1
          ITER=0     !MAXITR/2   why not reset to zero?
          ISTRT=ISTRT+1
          DO 10 I=1,NN
            H(I)=HOLD(I)
   10     CONTINUE
        ELSEIF (OHBC.NE.1) THEN   !why do we need to reset BC here?
C         ...RESET TOP B.C. TO CONSTANT HEAD AND CONTINUE
          IHBC(1)=1
          GH(1)=HMIN
          OHBC=1
          H(1)=GH(1)
        ELSE
C         ...GIVE IT UP AND GO ON TO THE NEXT TIME STEP
          ITER=MAXITR+1
        ENDIF
      ELSE
C       ...SET TOP B.C. TO CONSTANT HEAD
        IHBC(1)=1
        GH(1)=HMIN
        OHBC=1
        H(1)=GH(1)
      ENDIF
C     
C     ... CHECK & SET BOTTOM BOUNDARY CONDITION TYPE & VALUE
C     
      IF(IREBOT.EQ.1) THEN
C       ..CONSTANT HEAD BTWN BOTTOM NODE AND PSEUDO-NODE
        IHBC(2)=1
        GH(2)=BOTHED
      ELSEIF(IREBOT.EQ.2) THEN
C       ..UNIT GRADIENT BTWN BOTTOM NODE AND PSEUDO-NODE
        IHBC(2)=2
      ELSEIF(IREBOT.EQ.3) THEN
C       ..FLUX BTWN BOTTOM NODE AND PSEUDO-NODE
        IHBC(2)=3
        GH(2)=BOTFLX
      ENDIF
C     
      RETURN
      END
C
      SUBROUTINE CNHEAD(NN,DELT,NDXN2H,HOLD,HITR,H,THETA,TL,HKBAR,DELZ,
     +    QS,SOILHP,GH,IHBC,EVAP,IREBOT,BOTHED,BOTFLX,ALPH,CNVG,ISTRT,
     +    MAXITR,H2OT1,QT,IMAGIC,RICH_EPS,MAXCYCLE_RICH,pori,qsn,
     +    ISHUTL,ishaw,EVAPS,Hmin,nsp,QSSI,istress)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A      L  CONSTANT FOR NON-UNIFORM MIXING []
C       ALPH   I  VARIABLE DETERMINING FDM METHOD USED
C       ALPH2  L
C       B      L  CONSTANT FOR NON-UNIFORM MIXING []
C       BOTFLX     I
C       BOTHED     I
C       CITR   L
C       CNVG  I/O
C       CSM        L
C       CVCRT  L
C       D      L  SOLAR DECLINATION [R]
C       DA         L
C       DBAL   L
C       DBMAX  L
C       DELT  I/O INCREMENTAL TIME STEP [HR]
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       DFLX   L
C       DSNK   L
C       DTH        L
C       DTSAV  L
C       E      L  ECCENTRICITY OF EARTH'S ORBIT [RAD]
C       ENN1   L
C       EPS        P  EPSILON - CONVERGENCE CRITERION
C       EVAP   I  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       F      L  COMPUTED FUNCTIONAL VALUE
C       FLXI   L
C       FLXO   L
C       FNN1   L
C       GH         I  2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2)
C                 BOUNDARY CONDITION VALUES
C       H     I/O NODAL SOIL WATER PRESSURE HEADS [CM]
C       HBTI   L
C       HBTP   L
C       HITR   I
C       HKBAR I/O LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HKBI   L
C       HKBP   L
C       HMIN   P
C       HOLD   I  NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       HSAV   L
C       I      L  INDEX VARIABLE
C       IHBC   I  2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2) B.C.
C                 TYPES: 1 (HEAD - DIRICHLET) OR
C                      2 (FLUX - NEUMANN - NORMAL GRAD)
C       II         L
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       ISTRT  I
C       ITER   L  ITERATION COUNTER
C       J      L  INDEX VARIABLE
C       JH         L  HORIZON INDEX
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXITR     I
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODC     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NN1        L
C       QS        I/O NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       U      L  IONIC STRENGTH [MOLES/L]
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 CHKBC
C                 POINTK
C                 SPMOIS
C                 WC
C
C       CALLED FROM:
C
C       PROGRAMMER:  LAJ AHUJA, KAREN JOHNSEN, AND KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C-----------------------------------------------------------------------
C
      PARAMETER(MXNOD=300,MAXHOR=12)
C
      PARAMETER(MXNODC=MXNOD+2)  !HMIN=-35000.0D0,
      PARAMETER(HMAX=0.0D0,HMAXWT=3000.0D0)
C
      DIMENSION HOLD(NN),H(NN),THETA(NN),TL(NN),HKBAR(NN),DELZ(NN),
     +    QS(NN),NDXN2H(NN),SOILHP(13,MAXHOR),GH(2),IHBC(2),QT(NN),
     +    pori(nn),qssi(nn)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
      DIMENSION A(MXNODC),B(MXNODC),C(MXNODC),D(MXNODC),HITR(MXNODC),
     +    CITR(MXNODC),CSM(MXNODC),HSAV(MXNODC),E(MXNODC),F(MXNODC)
      LOGICAL CNVG
C
      if (cnvg) icnvg = 0
      ISTRT=0
   10 ITER=0
      NN1=NN-1
C     
C     ..SAVE INITIAL GUESS
c      ISAT = 0   !GNF, 9-2-2009, WHY DID HE DO THAT???  
       HBMIN=0.0D0
       HBMAX=0.0D0
C       
      DO 20 I=1,NN
        JH=NDXN2H(I)
        HSAV(I)=HITR(I)
        HBMIN=MIN(HBMIN,SOILHP(1,JH))
        HBMAX=MAX(HBMAX,SOILHP(1,JH))
c           if (ishaw.eq.1) then
c!           CHECK FOR SATURATED CONDITIONS    GNF 5/15/06
c            IF (HITR(I) .GE.-SOILHP(1,NDXN2H(I))) THEN
c!               ALPH = 1.D0
c!               ALPH2 = 1.0D0 - ALPH
c!              CHECK IF SATURATED NODES SURROUNDED BY ZERO K
c               IF (I .LE. 2) THEN
c                  ISAT=I
c               ELSE
c                  IF (ISAT .EQ. 0) THEN
c!                    CHECK FOR ZERO CONDUCTIVITY
c                     JH = NDXN2H(I-1)
c                     HKBI = POINTK(HITR(I-1),SOILHP(1,JH),JH,PORI(I-1))
c                     JH = NDXN2H(I)
c                     HKBP = POINTK(HITR(I),SOILHP(1,JH),JH,PORI(I))
c                     IF (HKBI*HKBP .EQ. 0.0) ISAT = I
c                  END IF
c               END IF
c            ELSE
c               IF (ISAT .GT. 0) THEN
c!                 CHECK FOR ZERO CONDUCTIVITY
c                  JH = NDXN2H(I-1)
c                  HKBI = POINTK(HITR(I-1),SOILHP(1,JH),JH,PORI(I-1))
c                  JH = NDXN2H(I)
c                  HKBP = POINTK(HITR(I),SOILHP(1,JH),JH,PORI(I))
c                  IF (HKBI*HKBP .EQ. 0.0) THEN
c!                    SATURATED NODES SURROUNDED BY ZERO K
c!                    SET TOP SATURATED HEAD TO JUST BELOW AIR ENTRY
c                     HITR(ISAT)=-SOILHP(1,NDXN2H(ISAT))
c                     DO JJ=ISAT+1,I-1
c                        IF (JJ .EQ. 2) THEN
c                           HITR(JJ)=-SOILHP(1,NDXN2H(JJ))
c                        ELSE
c                           HITR(JJ) = HITR(JJ-1)+DELZ(JJ)
c                        END IF
c                     END DO
c                  END IF
c               END IF
c               ISAT = 0
c            END IF
c            endif
   20 CONTINUE
      DTSAV=DELT
C     
C     ..GET THE NEW TRIDIAGONAL COEFFICIENTS
   30 CONTINUE
      ALPH2=ABS(1.D0-ALPH)
      JH=NDXN2H(1)
      HBTI=(ALPH2*HOLD(1)+ALPH*HITR(1))
      HBTP=(ALPH2*HOLD(2)+ALPH*HITR(2))
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      HKBI=POINTK(HBTI,SOILHP(1,JH),JH,pori(1))
      HKBP=POINTK(HBTP,SOILHP(1,JH),JH,pori(2))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      HKBAR(1)=DSQRT(HKBI*HKBP)
      IF(IHBC(1).EQ.1) THEN
C       ..CONSTANT HEAD AT TOP
        E(1)=0.0D0
        F(1)=GH(1)
      ELSE
C       ..FLUX AT TOP
        E(1)=1.0D0
c        IF (HKBAR(1) .NE.0.0D0) THEN   !GNF'S IDEA
           F(1)=GH(1)*DELZ(1)/(HKBAR(1)*ALPH)+(HOLD(2)-HOLD(1))*
     +        (ALPH2/ALPH)-DELZ(1)/ALPH
c        ELSE
c           F(1)=0.0D0
c        ENDIF
      ENDIF
C     
C     IF (DELT.EQ.0.0D0) GOTO 55
      DO 40 I=2,NN1
        U=TL(I)/DELT
        JH=NDXN2H(I)
        HBTP=(ALPH2*HOLD(I+1)+ALPH*HITR(I+1))
        CITR(I)=SPMOIS(HITR(I),SOILHP(1,JH),JH)
        CSM(I)=WC(HITR(I),SOILHP(1,JH),JH,0)
        HKBI=HKBP
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
        HKBP=POINTK(HBTP,SOILHP(1,NDXN2H(I+1)),NDXN2H(I+1),pori(i+1))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        HKBAR(I)=DSQRT(HKBI*HKBP)
        C(I)=ALPH*HKBAR(I-1)/DELZ(I-1)
        B(I)=ALPH*HKBAR(I-1)/DELZ(I-1)+ALPH*HKBAR(I)/DELZ(I)+U*CITR(I)
        A(I)=ALPH*HKBAR(I)/DELZ(I)
        DA=U*(CITR(I)*HITR(I)-(CSM(I)-THETA(I)))
        D(I)=ALPH2*HKBAR(I)*HOLD(I+1)/DELZ(I)-ALPH2*(HKBAR(I)/DELZ(I)+
     +      HKBAR(I-1)/DELZ(I-1))*HOLD(I)+ALPH2*HKBAR(I-1)*HOLD(I-1)/
     +      DELZ(I-1)+(HKBAR(I-1)-HKBAR(I))-TL(I)*QS(I)+DA-TL(I)*QT(I)+
     +      TL(I)*qssi(I)
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
c	  IF (I.EQ.2) D(I) = D(I) + QSN / DELT  !why did Joe do that?
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        E(I)=A(I)/(B(I)-C(I)*E(I-1))
        IF(B(I)-C(I)*E(I-1).EQ.0.0) THEN
          F(I)=0.0
          PRINT*,'SOLUTION INNER DIVISION BY ZERO -- RESETTING'
        ELSE
          F(I)=(D(I)+C(I)*F(I-1))/(B(I)-C(I)*E(I-1))
        ENDIF
   40 CONTINUE
C     
      IF(IHBC(2).EQ.1) THEN
C       ..CONSTANT HEAD AT BOTTOM
        H(NN)=(2.D0*GH(2)-F(NN1))/(1.0D0+E(NN1))
      ELSEIF(IHBC(2).EQ.2) THEN
C       ..UNIT GRADIENT CONDITION AT BOTTOM
        H(NN)=F(NN1)/(1.0D0-E(NN1))
      ELSE
C       ..FLUX AT BOTTOM
        ENN1=1.0D0
        FNN1=GH(2)*DELZ(NN1)/(HKBAR(NN1)*ALPH)+(HOLD(NN)-HOLD(NN1))*(
     +      ALPH2/ALPH)-DELZ(NN1)/ALPH
        H(NN)=(F(NN1)-FNN1)/(ENN1-E(NN1))
      ENDIF
C     
C     ..BOUND H VALUES
      H(NN)=MAX(HMIN,H(NN))
      IF(NN.GT.INT(H2OT1)-10) THEN
        HMX=HMAXWT
      ELSE
        HMX=HMAX
      ENDIF
      H(NN)=MIN(HMX,H(NN))
C     
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!	CHECKS PRESSUREHED TO SEE IF IT COULD REACH HMIN THROUGH ITERATION
!	IF  IT CAN, THEN THE SINK TERMS FOR THE "PROBLEM" NODES ARE SUMMED
!	AND  DISTRIBUTED THROUGH THE "NON-PROBLEM" NODES WHERE THERE ARE
!	ROOTS, PER GNF, 9-2-2009
c            IF (ISHUTL.EQ.0.and.ishaw.eq.1) THEN
c               HMAX1 = HMIN
c               SUMQS = 0.0
c               ICOUNT = 0
c               DO I = 1,NN
c                  IF (H(I).LT.(0.8*HMIN)) THEN
c                     SUMQS = SUMQS + QS(I)*TL(I)
c                     QS(I) = 0.0
c                  END IF
c                  IF (RDF(I).GT.0.D0.AND.H(I).GT.(0.8*HMIN)) 
c     +                        ICOUNT = ICOUNT + 1
c!     IF(H(I).GT.HMAX1) IJOE=I
c               END DO
c!
c               IF (ICOUNT.GT.0) QST = SUMQS/ICOUNT
c               DO I = 1,NN
c                  IF (RDF(I).GT.0.0D0.AND.H(I).GT.(0.8*HMIN)) 
c     +                      QS(I) = QS(I) + QST/TL(I)
c               END DO
c            END IF
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
C     ..SOLVE THE TRIDIAGONAL MATRIX
      DO 50 I=NN1,1,-1
        H(I)=H(I+1)*E(I)+F(I)
C       
C       ..BOUND H VALUES
        H(I)=MAX(HMIN,H(I))
C       IF (I.GT.INT(H2OT1)-10) THEN
C       HMX = HMAXWT
C       ELSE
C       HMX = HMAX
C       ENDIF
C       H(I) = MIN(HMX,H(I))
C       
C       ..SET SINK TO 0 IF WE ARE AT MINIMUM HEAD
        IF(H(I).EQ.HMIN) THEN
          QS(I)=0.0D0
          QT(I)=0.0D0
        ENDIF
   50 CONTINUE
C     
C     ..CHECK FOR CONVERGENCE
      DO 70 I=1,NN
        CVCRT=ABS(H(I)-HITR(I))
        IF(CVCRT.GE.RICH_EPS) THEN
C         
C         ...CALCULATE FOR TRUE SURFACE NODE
          DELH=ALPH*(H(1)-H(2))+ALPH2*(HOLD(1)-HOLD(2))
          if (istress.ge.3) then
            evapbc=-evaps  !max(-evaps,evap)
            else
          AEVAP=HKBAR(1)*(DELH/DELZ(1))+HKBAR(1)
          IF(ABS(AEVAP).LE.1.0D-14) AEVAP=0.0D0
c          EVAPBC=AEVAP
          EVAPBC=EVAP
          endif
c          else
c          AEVAP=EVAPS
c          EVAPBC=EVAPs
c          endif
C         
C         ..CHECK BOUNDRY CONDITIONS
          ITER=ITER+1
          CALL CHKBC(EVAPBC,GH,H,HOLD,IHBC,NN,DELT,IREBOT,BOTHED,BOTFLX,
     +        ITER,ISTRT,MAXITR,Hmin,nsp)
c          evap=evapbc
c          ISAT = 0
          DO 60 II=1,NN
            HITR(II)=H(II)
c           if (ishaw.eq.1) then
c!                    CHECK FOR SATURATED CONDITIONS    GNF 4/18/06
c                     IF (HITR(II) >=-SOILHP(1,NDXN2H(II))) THEN
c!                       ALPH = 1.D0
c!                       ALPH2 = 1.0D0 - ALPH
cC                       CHECK IF SATURATED NODES SURROUNDED BY ZERO K
c                        IF (II <= 2) THEN
c                           ISAT=II
c                        ELSE
c                           IF (ISAT==0 .AND. HKBAR(II-1)==0.0) ISAT = II
c                        END IF
c                     ELSE
c                        IF (ISAT > 0) THEN
c                           IF (HKBAR(II-1)==0.0) THEN
C                             SATURATED NODES SURROUNDED BY ZERO K
C                             SET TOP SATURATED HEAD TO JUST BELOW AIR ENTRY
c                              HITR(ISAT)=-SOILHP(1,NDXN2H(ISAT))
c                              DO JJ=ISAT+1,II-1
c                                 IF (JJ == 2) THEN
c                                    HITR(JJ)=-SOILHP(1,NDXN2H(JJ))
c                                 ELSE
c                                    HITR(JJ) = HITR(JJ-1)+DELZ(JJ)
c                                 END IF
c                              END DO
c                           END IF
c                        END IF
c                        ISAT = 0
c                     END IF
c                     endif
   60     CONTINUE
          IF(ITER.LE.MAXITR) GOTO 30
        ENDIF
   70 CONTINUE
C     
C     ..DO CELL BY CELL MASS BALANCE
       HBMIN=ABS(SOILHP(1,1))
       HBMAX=ABS(SOILHP(1,1))
C       
       DBMAX=0.D0
      DO 80 I=2,NN1
        JH=NDXN2H(I)
        FLXI=-HKBAR(I-1)*(ALPH*(H(I)-H(I-1))+ALPH2*(HOLD(I)-HOLD(I-1)))/
     +      DELZ(I-1)+HKBAR(I-1)
        FLXO=-HKBAR(I)*(ALPH*(H(I+1)-H(I))+ALPH2*(HOLD(I+1)-HOLD(I)))/
     +      DELZ(I)+HKBAR(I)
        DFLX=(FLXI-FLXO)*DELT
        DTH=(WC(H(I),SOILHP(1,JH),JH,0)-THETA(I))*TL(I)
        DSNK=QS(I)*TL(I)*DELT+QT(I)*TL(I)*DELT-qssi(I)*TL(I)*delt
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
c         IF (I.EQ.2) DSNK = DSNK - QSN
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        DBAL=DFLX-DTH-DSNK
        DBMAX=MAX(ABS(DBAL),DBMAX)
        IF(ABS(DBAL).GT.RICH_EPS.AND.ITER.LE.MAXITR) THEN
C       WRITE(9,*)'MASS BALANCE PROBLEM IN CNHEAD-GET HELP',DBAL,ITER
C       PRINT*,'MASS BALANCE PROBLEM IN CNHEAD-GET HELP'
C       STOP
        ENDIF
        HBMIN=MIN(HBMIN,ABS(SOILHP(1,JH)))
        HBMAX=MAX(HBMAX,ABS(SOILHP(1,JH)))
   80 CONTINUE
C     
C     ..CHECK IF SOLUTION DID NOT CONVERGE
      IF(ITER.GT.MAXITR) THEN
        IF(ISTRT.LE.MAXCYCLE_RICH) THEN
C         
C         .. RESET WITH A MUCH SMALLER TIME STEP AND TRY AGAIN
          ISTRT=ISTRT+1
          DO 90 I=1,NN1
            HITR(I)=HOLD(I)
   90     CONTINUE
c          DELT=DELT*1.0D-1
          DELT=DELT*5.0D-1
C         
C         .. TEST TO SEE IF INCREASE ITERATIONS ALSO
          MAXITR=MAXITR+300
          WRITE(9,1000) ISTRT,MAXITR,DELT,DBMAX
          GOTO 10
        ENDIF
C
        CNVG=.FALSE.
        IF(DBMAX.GT.2.0D-1) THEN
C         
C         ..SOLUTION IS TOO FAR OFF - USE "FAKE" SOLN. TO PREVENT
C         BAD #'S
          PRINT*,' no solution'
c  Liwang Ma 6-14-04
	    icnvg = icnvg + 1
	    if (icnvg.gt.200) stop  !return  'per Ryan suggestion to stop and then PEST
       IF ((ICNVG.GT.100).AND.((HBMIN.LE.0.0D0).OR.(HBMAX.GT.100.D0))) 
     +             STOP   !TO CATCH NO CONVERGENCE DUE TO ABNORMAL HB, BUBBLING PRESSURE.
C         fudge = RICH_EPS
          FUDGE=0.0D0
          H(1)=HSAV(1)-FUDGE
          HBTI=(ALPH2*HOLD(1)+ALPH*H(1))
          HBTP=(ALPH2*HOLD(2)+ALPH*H(2))
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
          HKBI=POINTK(HBTI,SOILHP(1,NDXN2H(1)),NDXN2H(1),pori(1))
          HKBP=POINTK(HBTP,SOILHP(1,NDXN2H(2)),NDXN2H(2),pori(2))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
          HKBAR(1)=DSQRT(HKBI*HKBP)
          DO 100 J=2,NN1
            H(J)=HSAV(J)-FUDGE
            HKBI=HKBP
            HBTP=(ALPH2*HOLD(J+1)+ALPH*H(J+1))
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
            HKBP=POINTK(HBTP,SOILHP(1,NDXN2H(J+1)),NDXN2H(J+1),
     +      pori(j+1))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
            HKBAR(J)=DSQRT(HKBI*HKBP)
  100     CONTINUE
          H(NN)=HSAV(NN)-FUDGE
          DELT=DTSAV
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
c	    qsn=0.0
cceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        ENDIF
C     DELT = 0.01
      ENDIF
C     
C     ..build new starting point for heads if actual evap is greater than
C     potential, which happens sometimes when switching from flux to constant
C     head bc.
C     ..CALCULATE aevap FOR TRUE SURFACE NODE
C     DELH = ALPH*(H(1) - H(2)) + ALPH2*(HOLD(1) - HOLD(2))
C     aevap = HKBAR(1) * (DELH / DELZ(1)) + HKBAR(1)
C     IF (ABS(aevap) .LE. 1.0D-14) aevap = 0.0D0
C     jaev = int(aevap * 1.0d4)
C     jev = int(evap * 1.0d4)
C     if(once .and. jaev.lt.jev) then
C     print*,' cnhead',jaev,jev,ITER,delt
C     once = .false.
C     do 85 i=1,nn
C     hitr(i) = hold(i) * 1.05D0
C     85   continue
C     if (hold(1) .eq. hmin) hitr(3)=hitr(4)
C     delt = dtsav
C     if (delt.lt.0.1D0) delt = delt * 10.0D0
C     CALL CHKBC (EVAP,GH,Hitr,HOLD,IHBC,NN,DELT,IREBOT,BOTHED,BOTFLX,
C     +    ITER,ISTRT,MAXITR,aevap)
C     goto 9999
C     endif
C     
      IF(IMAGIC.EQ.-1) WRITE(9,*) 
     +    ' NUMBER OF ITERATIONS FOR THIS TIME STEP',ITER
      RETURN
 1000 FORMAT(' NO CONVERGENCE -- RESETTING -- ',I4,I8,2G12.3)
      END
C
      SUBROUTINE HYDPAR(SOILHP,NDXN2H,NN,H,THETA,MAXHOR,AEF,ZN,H2OTAB,
     +pori,Hmin)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE SEQUENCES THROUGH THE NUMERICAL NODES FOR
C             THE PURPOSE OF SOLVING FOR SUCTION (H) FROM A GIVEN
C             THETA; CALCULATES WATER CONTENT (THETA)
C
C       REF:  R.E. SMITH:  ARS, FORT COLLINS CO
C           LAJ AHUJA:  ARS, DURANT OK
C
C       VARIABLE DEFINITIONS:
C       variable  I/O description
C       --------  --- -----------
C       AEF        I  FIELD SATURATION FRACTION [0..1]
C       H     I/O NODAL SOIL WATER PRESSURE HEADS [CM]
C       HMIN   P
C       I      L  INDEX VARIABLE
C       JH         L  HORIZON INDEX
C       MAXHOR     I  MAXIMUM NUMBER OF SOIL HORIZONS
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       T9         L
C       THETA I/O VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C          -WC: CALCULATE WATER CONTENT @ A SINGLE NODE
C          -WCH: CALCULATES THE HEAD FROM A SUPPLIED THETA VALUE
C
C       CALLED FROM:
C          -RICHRD: EXECUTIVE ROUTINE FOR SOLUTION OF RICHARD'S EQU.
C          -PHYSCL: EXECUTIVE ROUTINE FOR SOLUTION OF PHYSICAL PROCESS.
C
C       PROGRAMMER:  Ken Rojas
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(mxnod=300) !HMIN=-35000.0D0,
      INTEGER NDXN2H(NN)
      LOGICAL FIRST
      DIMENSION H(NN),SOILHP(13,MAXHOR),THETA(NN),ZN(NN),H2OTAB(2),
     +pori(mxnod)
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C      IE = MIN(INT(H2OTAB(1))+1,NN)
      IE=MIN(INT(H2OTAB(1)),NN)
C     
C     ..ABOVE WATER TABLE
      DO 10 I=1,IE
C       
C       ... RELATE NUMERICAL LAYER TO A SOIL HORIZON
        JH=NDXN2H(I)
C       
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak      
c        T9=SOILHP(6,JH)*AEF
        T9=pori(i)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        THETA(I)=MIN(THETA(I),T9)
        H(I)=WCH(H(I),THETA(I),SOILHP(1,JH),JH,0)
C       
C       ... CALC HYDRAULIC PROPERTIES
        H(I)=MAX(H(I),HMIN)
        THETA(I)=WC(H(I),SOILHP(1,JH),JH,0)
   10 CONTINUE
C     
C     ..WITHIN WATER TABLE
C     NOTE: AEF=1.0 FOR WATER TABLE CONDITIONS
      IF(FIRST.AND.IE.LT.NN) THEN
C       
C       ...SET INITIAL CONDITIONS
C       
C       DO 20 I=1,NN (SET INITIAL CONDITIONS TO EQUILIBRIUM)
        DO 20 I=IE+1,NN
          JH=NDXN2H(I)
          H(I)=ZN(I)-H2OTAB(2)
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak
c          THETA(I)=SOILHP(6,JH)*AEF
          THETA(I)=pori(i)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

   20   CONTINUE
        FIRST=.FALSE.
      ELSE
        DO 30 I=IE+1,NN
          JH=NDXN2H(I)
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak
c          THETA(I)=SOILHP(6,JH)*AEF
          THETA(I)=pori(i)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
   30   CONTINUE
      ENDIF
      RETURN
      END
C
      SUBROUTINE NODFLX(DELZ,GH,H,HOLD,HKBAR,IHBC,NN,Q,ALPH)
C
C======================================================================
C
C       PURPOSE: CALCULATE WATER FLUX AT EACH NODE AND NODAL INTERFACE
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ALPH   I  VARIABLE DETERMINING FDM METHOD USED
C       ALPH2  L
C       DELH   L  HEAD FORWARD DIFFERENCE [MM]
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       DK         L
C       DZ         L
C       GH         I  2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2)
C                 BOUNDARY CONDITION VALUES
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HKBAR  I  LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HOLD   I  NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       I      L  INDEX VARIABLE
C       IHBC   I  2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2) B.C.
C                 TYPES: 1 (HEAD - DIRICHLET) OR
C                      2 (FLUX - NEUMANN - NORMAL GRAD)
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       Q     I/O NODAL MOISTURE FLUXES [CM/HR]
C
C       CALLED FROM: REDIST, NODFLX
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
C  ...DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DELZ(NN),H(NN),HOLD(NN),HKBAR(NN),Q(NN),GH(2),IHBC(2)
C
      ALPH2=ABS(1.D0-ALPH)
C     
C     ...CALCULATE SURFACE TRANSIENT NODE FLUX
      IF(IHBC(1).EQ.2) THEN
        Q(1)=GH(1)
      ELSE
        DELH=ALPH*(H(1)-H(2))+ALPH2*(HOLD(1)-HOLD(2))
        Q(1)=HKBAR(1)*(DELH/DELZ(1))+HKBAR(1)
      ENDIF
      IF(ABS(Q(1)).LE.1.0D-14) Q(1)=0.0D0
C     
C     ...CALCULATE FOR TRUE SURFACE NODE
      DELH=ALPH*(H(1)-H(2))+ALPH2*(HOLD(1)-HOLD(2))
      Q(2)=HKBAR(1)*(DELH/DELZ(1))+HKBAR(1)
      IF(ABS(Q(2)).LE.1.0D-14) Q(2)=0.0D0
C     
C     ...CALCULATE INTERIOR NODE FLUXES
      DO 10 I=3,NN
        DELH=ALPH*(H(I-1)-H(I))+ALPH2*(HOLD(I-1)-HOLD(I))
        DZ=DELZ(I-1)
        DK=HKBAR(I-1)
        Q(I)=DK*(DELH/DZ)+DK
        IF(ABS(Q(I)).LE.1.0D-14) Q(I)=0.0D0
   10 CONTINUE
C     
      RETURN
      END
C
      FUNCTION POINTK(Head,HYDP,J,pori)
C
C======================================================================
C
C       PURPOSE: CALCULATE POINT HYDRAULIC CONDUCTIVITY
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HYDP   I  MODIFIED BROOKS-COREY PARAMETERS
C                 1: HB      - BUBBLING PRESSURE THETA(H) [CM]
C                 2: LAMDA - PORE SIZE DISTRIBUTION INDEX
C                 3: EPS   - EXPONENT FOR K(H) CURVE
C                 4: KSAT  - SAT HYDRAULIC CONDUCTIVTY [CM/HR]
C                 5: WR      - RESIDUAL WATER CONTENT
C                 6: WS      - SATURATION WATER CONTENT
C                 7: WFC   - VOL FIELD CAPACITY (1/3 BAR) WC
C                 8: WFC   - VOL FIELD CAPACITY (1/10 BAR) WC
C                 9: WWP   - VOL WILTING POINT (15 BAR) WC
C                 10:HB      - BUBBLING PRESSURE K(H) CURVE [CM]
C                 11:C2      - SECOND INTRCEPT ON K(H) CURVE
C                 12:N1      - FIRST EXPONENT FOR K(H) CURVE
C                 13:A1      - CONSTANT FOR O(H) CURVE
C       POINTK     O  HYDRAULIC CONDUCTIVITY [CM/HR]
C       TH         L
C
C       CALLED FROM:
C
C       PROGRAMMER: KEN ROJAS AND LAJ AHUJA
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf
c     jak
      PARAMETER(MAXHOR=12,porimin=0.13d0,mxnod=300)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      DOUBLE PRECISION HYDP(13),C22(MAXHOR),SN22(MAXHOR)
      INTEGER IRST
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
      SAVE IRST, C22, SN22
      DATA IRST /0/
C
      IF(J.GT.IRST) THEN
        C22(J)=HYDP(11)
        SN22(J)=HYDP(3)
        IRST=J
      ENDIF
C     
      TH=Head
      IF(TH.GE.0.0D0) THEN
        POINTK=HYDP(4)
C     
      ELSE
        IF(TH.GE.-HYDP(10)) THEN
          POINTK=HYDP(4)*(-TH)**(-HYDP(12))
        ELSE
          IF(TH.LE.(-HYDP(10)*10.0D0)) THEN
            POINTK=C22(J)*(-TH)**(-SN22(J))
          ELSE
            POINTK=HYDP(11)*(-TH)**(-HYDP(3))
          ENDIF
        ENDIF
      ENDIF
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf
c     jak
c      IF (PORI .LE. PORIMIN) then 
c	             POINTK = 0.0D0
      IF(POINTK.EQ.0.0D0) THEN
        PRINT*,"POINTK = 0"
c      else    !by GNF to limit pointK
c       FRACT=(PORI-PORIMIN)/(HYDP(6)*AEF-PORIMIN)   
c       POINTK=FRACT*POINTK
      ENDIF
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C  
      RETURN
      END
C
C
      SUBROUTINE REDIST(DELT,DELZ,H,NDXN2H,NN,QF,QS,SOILHP,TL,HKBAR,
     +    THETA,START,EVAP,AEVAP,BD,DTSEEP,TSEEP,DTCHEM,SLKS,CC,IREBOT,
     +    BOTHED,BOTFLX,AEF,IBRKTH,BRKH2O,BKCHEM,ALPH,H2OTAB,TLT,ITBL,
     +    CNVG,DTDCHEM,DTLCHEM,DAYTIM,IMAGIC,CAPZONE,ZN,T,SOLTP1,QT,
     +    DTDNIT,RICH_EPS,MAXITER_RICH,MAXCYCLE_RICH,QTD,QTL,ICHEM,
     +    pori,qsn,freund,deltice,ishaw,EVAPS,thetaold,thetai,Hmin,nsp,
     +    qssi,istress)
C
C======================================================================
C
C       PURPOSE:  THIS IS THE MAIN DRIVER ROUTINE FOR THE REDISTRIBUTION
C             SECTION OF RZWQM.  WATER IS MOVED ACCORDING TO RICHRD'S
C             EQUATION AND THEN CHEMICALS ARE ROUTED THROUGH THE
C             PROFILE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ACCPO4     L
C       AEF        I  FIELD SATURATION FRACTION [0..1]
C       AEVAP  I  POTENTIAL SOIL EVAPORATION (CM/HR)
C       ALPH   L  VARIABLE DETERMINING FDM METHOD USED
C       AWCP   L
C       BKCHEM    I/O FLUX OF SELECTED CHEMICALS AT BRK. THR. NODE
C                 CONTAINS NO3-N, PEST#1-3 [UG/CM^2]
C       BOTFLX     I
C       BOTHED     I
C       BRKH2O    I/O FLUX OF WATER AT BRK. THR. NODE [CM/DAY]
C       CC         I  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C                 CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C                 1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C                 5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C                 9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C                 12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CNVG   I
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       DTCHEM     I  TOTAL AMOUNT OF CHEMICAL LOST TO SEEPAGE
C                 CORRESPONDS TO CC ORDER [UG/CM^2]
C       DTSEEP     I
C       EVAP   I  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       FLUX   L
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HKBAR  I  LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HOLD   L  NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       I      L  INDEX VARIABLE
C       IBRKTH     I
C       ICHEM  L  INDICATOR FLAG SIGNALLING IF PHOSPHROUS CHEMISTRY IS USED
C                 (0) - DO NOT USE CHEMISTRY MODEL, USE DEFAULT VALUES
C                 (1) - USE CHEMISTRY MODEL
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNOD2     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       QF         I  LAYER INTERFACIAL MOIST FLUXES [CM/HR]
C       QS         I  NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       BD         I    SOIL BULK DENSITY [G/CM^3]
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       START  I
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TSEEP  I
C
C       COMMENTS:
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   ARRAY DIMENSION VALUES
      PARAMETER(MXNOD=300,MAXHOR=12,MXPEST=3,MXCHEM=15)
      PARAMETER(MXNOD2=MXNOD+2,rhoi=9.2d2,rhol=1.0d3)
C
      INTEGER NDXN2H(NN),ICHEM
      LOGICAL START,CNVG
      DIMENSION DELZ(NN),H(NN),SOILHP(13,MAXHOR),HKBAR(NN),TLT(NN),
     +    QF(NN),QS(NN),THETA(NN),TL(NN),CC(MXNOD,MXCHEM),H2OTAB(2),
     +    BD(MXNOD),SLKS(MXNOD,MXPEST,2),DTCHEM(MXCHEM),AWCP(MXNOD),
     +    HOLD(MXNOD2),BKCHEM(MXCHEM),DTDCHEM(MXCHEM),DTLCHEM(MXCHEM),
     +    ZN(NN),SOLTP1(MAXHOR,5),T(NN),QT(NN),QTD(MXNOD),QTL(MXNOD),
     +    pori(nn),freund(mxpest),deltice(mxnod),thetaold(mxnod),
     +    thetai(nn),qssi(nn)
      common/maxbal/IDmxH2O,IMmxH2O,IYYYmxH2O,IDmxN,IMmxN,IYYYmxN,
     +              IDmxC,IMmxC,IYYYmxC,IDmxP,IMmxP,IYYYmxP,
     +              DTMASSmxH2O,BalmxN,BalmxC,BalmxP,IWCINIT1
     +            ,ireset1
      SAVE HOLD,AWCP
      DATA ACCPO4 /0.0D0/
c      IF(START.or.iwcinit1.eq.1) THEN
C
C   ..SAVE AVERAGE WATER CONTENT IF AFTER RAIN STORM
      IF(START.or.iwcinit1.eq.1.or.ireset1.eq.1) THEN
        IF (ISHAW.EQ.0) THEN
        DO 10 I=1,NN
          AWCP(I)=THETA(I)*TL(I)
   10   CONTINUE
        ELSE if (ishaw.eq.1) then
!        CHECK IF HEADS HAVE CHANGED SINCE LAST TIME (PERHAPS
!        DUE TO ICE FORMATION IN SHAWHT)  !PER GNF, 9-2-2009
         IF(HOLD(2) .NE. H(1)) THEN 
            HOLD(1)=H(1)-DELZ(1)
         END IF
C      ENDIF
C
C      if (ishaw.eq.1) then
      tawcp=0.0d0
	tawc=0.0d0
        do i=1,nn
c	    if (deltice(i).gt.0.0d0) then
c            awcp(i)=(theta(i)+deltice(i)*rhoi/rhol)*tl(i)
            awcp(i)=theta(i)*tl(i)
            tawcp=tawcp+awcp(i)  !-deltice(i)*rhoi/rhol*tl(i)
	      tawc=tawc+theta(i)*tl(i)  !+deltice(i)*rhoi/rhol*tl(i)
c	endif
        enddo
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak

	hold(1)=h(1)
	hold(nn)=h(nn)
      endif
      ENDIF
       do i=1,nn
        if (ishaw.eq.1) awcp(i)=theta(i)*tl(i)
       enddo
C     ...REDISTRIBUTE WATER USING RICHARDS EQUATION
      CALL RICHRD(DELT,DELZ,H,HOLD,NDXN2H,NN,QF,QS,HKBAR,SOILHP,TL,
     +    THETA,START,EVAP,AEVAP,IREBOT,BOTHED,BOTFLX,ALPH,CNVG,H2OTAB,
     +    DAYTIM,ITBL,QT,IMAGIC,RICH_EPS,MAXITER_RICH,MAXCYCLE_RICH,
     +    pori,qsn,ISHAW,EVAPS,Hmin,nsp,qssi,istress)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C     
C     ... ACCUMULATE BREAKTHROUGH MASS
      IF(IBRKTH.LT.NN) THEN
        FLUX=QF(IBRKTH)*DELT
        BRKH2O=BRKH2O+FLUX
        IF(FLUX.LT.0.0D0) THEN
          BKCHEM(9)=BKCHEM(9)+FLUX*CC(IBRKTH+1,9)
          BKCHEM(13)=BKCHEM(13)+FLUX*CC(IBRKTH+1,13)
          BKCHEM(14)=BKCHEM(14)+FLUX*CC(IBRKTH+1,14)
          BKCHEM(15)=BKCHEM(15)+FLUX*CC(IBRKTH+1,15)
          ACCPO4=ACCPO4+FLUX*CC(IBRKTH+1,11)
        ELSE
          BKCHEM(9)=BKCHEM(9)+FLUX*CC(IBRKTH,9)
          BKCHEM(13)=BKCHEM(13)+FLUX*CC(IBRKTH,13)
          BKCHEM(14)=BKCHEM(14)+FLUX*CC(IBRKTH,14)
          BKCHEM(15)=BKCHEM(15)+FLUX*CC(IBRKTH,15)
          ACCPO4=ACCPO4+FLUX*CC(IBRKTH,11)
        ENDIF
      ELSE
        FLUX=QF(IBRKTH)*DELT
        BKCHEM(9)=BKCHEM(9)+FLUX*CC(IBRKTH,9)
        BKCHEM(13)=BKCHEM(13)+FLUX*CC(IBRKTH,13)
        BKCHEM(14)=BKCHEM(14)+FLUX*CC(IBRKTH,14)
        BKCHEM(15)=BKCHEM(15)+FLUX*CC(IBRKTH,15)
        ACCPO4=ACCPO4+FLUX*CC(IBRKTH,11)
      ENDIF
C     
C     ... REDISTRIBUTE CHEMICALS THROUGH THE SOIL PROFILE
      CALL RETRAN(DELT,NN,QF,BD,CC,THETA,SLKS,TL,DTCHEM,AWCP,DTDCHEM,
     +    DTLCHEM,IMAGIC,QTD,QTL,DTDNIT,ICHEM,ISHAW,DELTICE)
C     
      CALL DRAIN(NN,NDXN2H,THETA,TL,SOILHP,DTCHEM,TSEEP,DTSEEP,CC,SLKS,
     +    BD,AEF,IBRKTH,BRKH2O,BKCHEM,T,SOLTP1,FREUND,pori)
C     
C     ...     UPDATE HEADS WITH NEW WATER CONTENTS
      CALL WCHEAD(THETA,H,SOILHP,NN,NDXN2H,MAXHOR)
C     
C     ..FIND WATER TABLE
      IF(ITBL.EQ.1) CALL WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QF(NN),DELT,
     +    TLT,ZN,IREBOT,BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,THETA)
C     
      DO 20 I=2,(NN+1)
        IF (ISHAW.EQ.0) THEN
        AWCP(I-1)=THETA(I-1)*TL(I-1)
        ELSE
        AWCP(I-1)=THETA(I-1)*TL(I-1)
c         awcp(i-1)=(theta(i-1)+deltice(i-1)*rhoi/rhol)*tl(i-1)
        ENDIF
        HOLD(I)=H(I-1)
   20 CONTINUE
C     
      RETURN
      END
C
      SUBROUTINE REKNTC(DELT,IP,NN,SLKS,BD,THETA,CONC,FREUND)
C
C======================================================================
C
C       PURPOSE: CALCULATES KINETIC ADSORPTION/RELEASE OF A PESTICIDE
C    FROM SOIL TO SOLUTION PHASE.  THE ADSORBED PESTICIDE IS ASSUMED TO
C    EXIST IN TWO FORMS - ONE THAT IS IN INSTANTANEOUS EQUILIBRIUM WITH
C    SOLUTION AND THE SECOND THAT EQUILIBRIATES OVER A DAY.  WE DEAL
C    HERE WITH KINETIC RELEASE OF THE SECOND FORM FOR A SINGLE CHEMICAL.
C
C       REF:  DR. L. AHUJA, ARS DURANT, OK
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ALK        L
C       BDH        I  BULK DENSITY FOR HORIZON [G/CM^3]
C       BDK        L
C       CONC  I/O CHEM. CONC. IN SOIL WATER OF EACH SOIL NODE
C                 SAME AS CC [MG/L]
C       CONCX2    I/O PEST. CONC. INVOLVED IN KINETIC PROCESSES [UG/G-S]
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DELX2  L
C       EK2        I  EQUIL.CONST. FOR ADSORPTION [CM3 H2O/G SOIL]
C       IP         I  INDEX FOR PESTICIDE NUMBER
C       J      L  INDEX VARIABLE
C       JJ         L
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       RK2        I  KINETIC CONST FOR RELEASE OF PEST FROM
C                 ABSORBTION SITE
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       T1         L  STARTING TIME [DAYS]
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXPEST=3)
C
      COMMON /KNTCS/ CONCX2(MXNOD,MXPEST),EK2(MXNOD,MXPEST),
     +    RK2(MXPEST),RDIF(MXPEST),FEK2(MXPEST),IEK2(MXPEST)
C
      DIMENSION CONC(NN),THETA(NN),BD(MXNOD),SLKS(MXNOD,MXPEST,2)
	DIMENSION FREUND(MXPEST)
C
C     ..DETERMINE ADSORPTION/DESORPTION
      DO 10 J=1,NN
        BDK=BD(J)
c Liwang Ma, 7-3-2006
            ALK=FSLKS(SLKS(J,IP,1),CONC(J),FREUND(IP),theta(j),bd(j))
c
        T1=THETA(J)+BDK*ALK
        CONCX2(J,IP)=MAX(CONCX2(J,IP),0.0D0)
        CONCJ=CONC(J)
C       
C       ..GROSS DISPERSION FROM WITHIN MESOPORES
        DELX2=DELT*RK2(IP)*(EK2(J,IP)*CONC(J)-CONCX2(J,IP))
        CONC(J)=CONC(J)-DELX2*BDK/T1
        IF(CONC(J).LT.0.0D0) THEN
          CONC(J)=0.0D0
          DELX2=CONCJ*T1/BDK
        ENDIF
        CONCX2(J,IP)=CONCX2(J,IP)+DELX2
   10 CONTINUE
C     
      RETURN
      END
C
      SUBROUTINE RETRAN(DELT,NN,QF,BD,CC,THETA,SLKS,TL,DTCHEM,AWCP,
     +    DTDCHEM,DTLCHEM,IMAGIC,QTD,QTL,DTDNIT,ICHEM,ISHAW,DELTICE)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE INITIALIZES PERTINENT LOCAL DATA ITEMS
C              AND SEQUENCES THROUGH ALL THE CHEMICALS FOR TRANSPORT
C              DURING THE REDISTRIBUTION PHASE.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AWC        L
C       AWCP   I
C       CC         I  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C                 CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C                 1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C                 5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C                 9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C                 12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CHMD   L  TOTAL AMT. OF CHEM. LOST THROUGH TILE DRAIN
C       CHML   L  TOTAL AMT. OF CHEM. LOST THROUGH LATERAL SEEPAGE
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DTCHEM    I/O TOTAL AMOUNT OF CHEMICAL LOST TO SEEPAGE
C                 CORRESPONDS TO CC ORDER [UG/CM^2]
C       FLUX   L
C       I      L  INDEX VARIABLE
C       ICHEM  L  INDICATOR FLAG SIGNALLING IF PHOSPHORUSEQUILIBRIUM CHEMISTRY IS USED
C                 (0) - DO NOT USE CHEMISTRY MODEL, USE DEFAULT VALUES
C                 (1) - USE CHEMISTRY MODEL
C       IP         L  INDEX FOR PESTICIDE NUMBER
C       J      L  INDEX VARIABLE
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       IPACTV     I  =1 WHEN PESTICIDE IS ACTIVE
C       QF         I  LAYER INTERFACIAL MOIST FLUXES [CM/HR]
C       BD         I    SOIL BULK DENSITY [G/CM^3]
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       TAWC   L
C       TAWCP  L
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TMASS  L
C       WMASS  L
C       WPMASS     L
C
C       EXTERNAL REFERENCES:
C                 REKNTC
C                 REROUT
C
C       CALLED FROM:
C          -REDIST: SEQUENCING ROUTINE FOR THE REDISTRIBUTION PHASE
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION: 2.0
C
C======================================================================
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   ARRAY DIMENSION VALUES
      PARAMETER(MXNOD=300,MXCHEM=15,MXPEST=3,RHOL=1.0D3,RHOI=9.2D2)
C
C     PESTICIDE COMMON VARIABLES
C
      COMMON /RZPEST/ HALFF(MXPEST),HALFFP(MXPEST),HALFFB(MXPEST),
     +    HALFR(MXPEST),HALFRP(MXPEST),HALFRB(MXPEST),HALFSSA(MXPEST),
     +    HALFSSV(MXPEST),HALFSSP(MXPEST),HALFSSB(MXPEST),
     +    HALFSA(MXPEST),HALFSN(MXPEST),HALFSB(MXPEST),HALFBD(MXPEST),
     +    XKH(MXPEST),XKOC(MXPEST),XMW(MXPEST),THETAR(MXPEST),
     +    TMPR(MXPEST),EA(MXPEST),EN(MXPEST),WALKERB(MXPEST),
     +    VMAX(MXPEST),DYIELD(4,MXPEST),FREUND(MXPEST),XPKA(MXPEST),
     +    XPKB(MXPEST),REFPH(MXPEST),CATKOC(MXPEST),ANKOC(MXPEST),
     +    PCOEFF(MXPEST),PPOWER(MXPEST),RCOEFF(MXPEST),RPOWER(MXPEST),
     +    XKOW(MXPEST),PBIND(MXPEST),IPCODE(4,MXPEST),IPANC(MXPEST),
     +    IPDEP(MXPEST),IPACTV(MXPEST),NPEST
C
      DIMENSION BD(MXNOD),CC(MXNOD,MXCHEM),QF(MXNOD),THETA(MXNOD),
     +    TL(MXNOD),SLKS(MXNOD,MXPEST,2),DTCHEM(MXCHEM),
     +    QTD(MXNOD),QTL(MXNOD),DTDCHEM(MXCHEM),DTLCHEM(MXCHEM)

      DIMENSION AWC(MXNOD),AWCP(MXNOD),FLUX(MXNOD),TAWC(MXNOD),
     +    TAWCP(MXNOD),AWD(MXNOD),AWL(MXNOD),DELTICE(MXNOD)
C
C     ..FIND AVERAGE WATER CONTENT AT THE END OF THIS TIME STEP
      DO 10 I=1,NN
        AWL(I)=QTL(I)*TL(I)*DELT
        AWD(I)=QTD(I)*TL(I)*DELT
        AWC(I)=THETA(I)*TL(I)
        FLUX(I)=QF(I)*DELT
   10 CONTINUE
C     
C     ..ROUTE THE CHEMICALS ONE AT A TIME
      DO 30 J=1,MXCHEM
        IP=J-12
        IF(IP.GT.0) THEN
          IF(IPACTV(IP).NE.1) GOTO 30
        ENDIF
        IF((J.GT.8.AND.J.NE.11).OR.ICHEM.EQ.1) THEN   !ICHEM IS REPURPOSED TO PHOSPHORUS, LIWANG MA, 5-28-2020
C        IF(J.GT.8.AND.J.NE.11) THEN
          CHMD=0.0D0
          CHML=0.0D0
          DO 20 I=1,NN
            IF(J.NE.10) CHMD=CHMD+CC(I,J)*AWD(I)
            IF(J.NE.10) CHML=CHML+CC(I,J)*AWL(I)
            IF(IP.GT.0) THEN
C LIWANG MA, 7-3-2006 (is theta correct here??)
              TAWC(i)=AWC(i)+FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +           awc(i),bd(i))*BD(I)*TL(I)
              TAWCP(i)=AWCP(i)+FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +           awcp(i),bd(i))*BD(I)*TL(I)
c              TAWC(I)=AWC(I)+CTERM
c              TAWCP(I)=AWCP(I)+CTERM
            ENDIF
            IF(J.EQ.10) CC(I,J)=CC(I,J)*AWCP(I)/AWC(I)
   20     CONTINUE
C         
C         ..ACCOUNT FOR CHEMICAL GOING OUT OF OR COMING IN FROM DRAIN
          IF(J.EQ.9) DTDNIT=CHMD
          IF(J.EQ.9) DTLNIT=CHML
          IF (J.NE.10) THEN
          DTDCHEM(J)=DTDCHEM(J)+CHMD
          DTLCHEM(J)=DTLCHEM(J)+CHML
          DTCHEM(J)=DTCHEM(J)+max(0.0d0,FLUX(NN)*CC(NN,J))

          ENDIF
          IF(IP.GT.0) THEN
            CALL REROUT(TAWC,TAWCP,FLUX,NN,CC(1,J),J,AWD,AWL)
            CALL REKNTC(DELT,IP,NN,SLKS,BD,THETA,CC(1,J),FREUND)
          ELSEIF (J.NE.10) THEN
            CALL REROUT(AWC,AWCP,FLUX,NN,CC(1,J),J,AWD,AWL)
          ENDIF
        ENDIF
   30 CONTINUE
C     
C     ..LOAD THE VALUES FOR AVERAGE WATER CONTENT INTO AWCP FOR NEXT TIME
      DO 40 I=1,NN
        AWCP(I)=AWC(I)     !+(deltice(i)*rhoi/rhol)*tl(i)
   40 CONTINUE
C     
      RETURN
      END
C
      SUBROUTINE REROUT(AWC,AWCP,FLUX,NN,CONC,IC,AWD,AWL)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE COMPLETE THE ACTUAL TRANSPORT OF
C             A CHEMICAL FROM ONE LAYER TO THE NEXT.
C
C       REF: L. AHUJA, USDA-ARS, DURANT OK.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AWC        I
C       AWCP   I
C       CONC  I/O CHEM. CONC. IN SOIL WATER OF EACH SOIL NODE
C                 SAME AS CC [MG/L]
C       CONCJ  L
C       CONCJ1     L
C       CONJM1     L
C       FLUX   I
C       IC     I  CURRENT CHEMICAL INDEX
C       J      L  INDEX VARIABLE
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C
C
C       CALLED FROM: RETRAN
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION: 2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CONC(NN),FLUX(NN),AWC(NN),AWCP(NN),AWD(NN),AWL(NN)
C
C  --START THE TRANSPORT FOR ONE CHEMICAL
C
      CONCJ=CONC(NN)
C     
      DO 10 J=NN,1,-1
        CONCJ1=CONCJ
        CONCJ=CONC(J)
        IF(CONCJ.LT.0.0D0) THEN
          WRITE(9,*) ' STARTING REROUT WITH NEG CONC// CHEM #',IC
        ENDIF
C       
        BASEWAT=AWCP(J)-AWD(J)-AWL(J)
        IF(J.NE.1) THEN
          CONJM1=CONC(J-1)
          IF(FLUX(J).GT.0.0D0.AND.FLUX(J-1).GT.0.0D0) THEN
C           
C           ..+FLUX IN BOTH BOUNDRY'S
C            CONC(J)=(CONC(J)*(AWCP(J)-FLUX(J)-AWD(J))+CONJM1*FLUX(J-1))/
C     +          AWC(J)
            CONC(J)=(CONC(J)*(BASEWAT-FLUX(J))+CONJM1*FLUX(J-1))/
     +          AWC(J)
C         
          ELSEIF(FLUX(J).GT.0.0D0.AND.FLUX(J-1).LE.0.0D0) THEN
C           
C           ..+FLUX IN LOWER BOUNDRY ONLY
C            CONC(J)=(CONC(J)*(AWCP(J)-FLUX(J)-AWD(J))-ABS(FLUX(J-1))*
C     +          CONCJ)/AWC(J)
            CONC(J)=(CONC(J)*(BASEWAT-FLUX(J))-ABS(FLUX(J-1))*
     +          CONCJ)/AWC(J)
C         
          ELSEIF(FLUX(J).LE.0.0D0.AND.FLUX(J-1).LE.0.0D0) THEN
C           
C           ..NO +FLUX IN EITHER BOUNDRY
C            CONC(J)=(CONC(J)*(AWCP(J)-AWD(J))-CONCJ*ABS(FLUX(J-1))+
C     +          ABS(FLUX(J))*CONCJ1)/AWC(J)
           if (j.eq.NN) then
            CONC(J)=(CONC(J)*(BASEWAT)-CONCJ*ABS(FLUX(J-1))+
     +          ABS(FLUX(J))*0.0d0)/AWC(J)
           else
            CONC(J)=(CONC(J)*(BASEWAT)-CONCJ*ABS(FLUX(J-1))+
     +          ABS(FLUX(J))*CONCJ1)/AWC(J)
           endif
C         
          ELSE
C           
C           ..+FLUX IN UPPER BOUNDRY ONLY
C            CONC(J)=(CONC(J)*(AWCP(J)-AWD(J))+ABS(FLUX(J))*CONCJ1+
C     +          FLUX(J-1)*CONJM1)/AWC(J)
            if (j.eq.nn) then
            CONC(J)=(CONC(J)*(BASEWAT)+ABS(FLUX(J))*0.0d0+
     +          FLUX(J-1)*CONJM1)/AWC(J)
            else
            CONC(J)=(CONC(J)*(BASEWAT)+ABS(FLUX(J))*CONCJ1+
     +          FLUX(J-1)*CONJM1)/AWC(J)
            endif
C         
          ENDIF
        ELSE
C         ..DETERMINE CONCENTRATION FOR EVAPORATIVE BOUNDRY
          IF(FLUX(1).GT.0.0D0) THEN
C            CONC(J)=(CONC(J)*(AWCP(J)-FLUX(J)-AWD(J)))/AWC(J)
            CONC(J)=(CONC(J)*(BASEWAT-FLUX(J)))/AWC(J)
          ELSE
C            CONC(J)=(CONC(J)*(AWCP(J)-AWD(J))+CONCJ1*ABS(FLUX(J)))/
C     +          AWC(J)
            CONC(J)=(CONC(J)*(BASEWAT)+CONCJ1*ABS(FLUX(J)))/AWC(J)
          ENDIF
        ENDIF
C       
        IF(CONC(J).LT.0.0) THEN
          IF(ABS(CONC(J)).LE.1.0D-15) THEN
            CONC(J)=0.0D0
            GOTO 10
          ENDIF
          IF(J.NE.1) THEN
            WRITE(9,1000) J,FLUX(J-1),FLUX(J),CONC(J),CONCJ,AWC(J),
     +          AWCP(J)
          ELSE
            WRITE(9,1100) J,FLUX(J),CONC(J),CONCJ,AWC(J),AWCP(J)
          ENDIF
          CONC(J)=0.0D0
          PRINT*,'  CONCENTRATION WENT NEGATIVE, LAYER => ',J,IC
        ENDIF
   10 CONTINUE
C
      RETURN
 1000 FORMAT(///' LAYER IN QUESTION',I5,/' FLUX IN AT LAYER =====>',G12
     +    .3,/' FLUX LEAVING =====>',G12.3,/
     +    ' CONC AT LAYER (NEW) =====>',G12.3,/
     +    ' CONC AT LAYER (OLD) ----->',G12.3,/' WATER CONTENT',G12.6,/
     +    ' WATER CONTENT PREV.',G12.6)
 1100 FORMAT(///' LAYER IN QUESTION',I5,/' FLUX LEAVING =====>',G12.3,/
     +    ' CONC AT LAYER (NEW) =====>',G12.3,/
     +    ' CONC AT LAYER (OLD) ----->',G12.3,/' WATER CONTENT',G12.6,/
     +    ' WATER CONTENT PREV.',G12.6)
      END
C
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      SUBROUTINE RICHRD(DELT,DELZ,H,HOLD,NDXN2H,NN,QF,QS,HKBAR,SOILHP,
     +    TL,THETA,START,EVAP,AEVAP,IREBOT,BOTHED,BOTFLX,ALPH,CNVG,
     +    H2OTAB,DAYTIM,ITBL,QT,IMAGIC,RICH_EPS,MAXITER_RICH,
     +    MAXCYCLE_RICH,pori,qsn,ISHAW,EVAPS,Hmin,nsp,qssi,istress)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C======================================================================
C
C       PURPOSE: SOLVE RICHARDS EQUATION BY FINITE DIFFERENCES
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AEVAP  I  POTENTIAL SOIL EVAPORATION (CM/HR)
C       ALPH   I  VARIABLE DETERMINING FDM METHOD USED
C       BOTFLX     I
C       BOTHED    I/O
C       CNVG   I
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       EVAP   I  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       FIRST  L  TRUE IF FIRST TIME THROUGH ROUTINE
C       GH         L  2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2)
C                 BOUNDARY CONDITION VALUES
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HITR   L
C       HKBAR  I  LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HMIN   P
C       HOLD  I/O NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       IHBC   L  2-LONG VECTOR OF UPPER (EL 1) & LOWER (EL 2) B.C.
C                 TYPES: 1 (HEAD - DIRICHLET) OR
C                      2 (FLUX - NEUMANN - NORMAL GRAD)
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       ISTRT  L
C       ITER   L  ITERATION COUNTER
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXITR     P
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODC     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NDXHOR     L
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NNC        L
C       QF         I  LAYER INTERFACIAL MOIST FLUXES [CM/HR]
C       QS         I  NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       START  I
C       TDELZ  L
C       TH         L
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       THKBAR     L
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TQF        L  TOTAL AND AVERAGE WATER FLUX OVER STIME [CM/HR]
C       TQS        L  TOTAL AND AVERAGE ROOT UPTAKE OVER STIME [CM/HR]
C       TTHETA     L
C       TTL        L
C       DELT      TIME INCREMENT [HR]
C       DELZ   I  V [NN]: NODAL DEPTH FORWARD DIFFERENCES
C                 DELZ(I) = Z(I+1)-Z(I)
C       GH
C       H     I/O V: [NN]: NODAL HEADS [CM]
C       HK        I/O V: [NN]: NODAL HYDRAULIC CONDUCTIV'S [CM/HR]
C       HKBAR     V:       LAYER INTERFACE "    [CM/HR]
C       HOLD   I  V:       NODAL HEADS FROM PREVIOUS TIME STEP
C       SOILHP     I  A: [8XNHOR]: ARRAY OF HYDRAULIC PARAMETERS,
C                 PARAMETERS BY ROW, SOIL HORIZON BY COLUMN
C       NDXN2H     I  V: [NN]: INDEX VECTOR RELATING NUMERICAL NODE
C                 TO CONTAINING SOIL HORIZON:
C                 NODE I IS IN HORIZON NDXN2H(I)
C       IHBC
C       NN         I  NO. INTERIOR NUMERICAL NODES - DOES NOT INCLUDE
C                 SURFACE NODE 0.  THUS, NN+1 TOTAL NODES
C       Q      O  V [NN]  NODAL MOISTURE FLUXES [CM/HR]
C       QF         O          ;AYER INTERFACIAL MOIST FLUXES [CM/HR]
C       QS         I  V [NN]  NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       TL         I  V [NN] NUMERICAL LAYER THICKNESSES [CM]
C       THETA  O  V [NN] NODAL MOISTURE CONTENTS
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 CHKBC
C                 CNHEAD
C                 NODFLX
C                 REDGRD
C                 TRUGRD
C                 WCNODS
C
C       CALLED FROM:
C         -REDIST: REDISTRIBUTION OF SOIL WATER EXEC ROUTINE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C=======================================================================
C
C   ARRAY DIMENSION VALUES
C
C-----------------------------------------------------------------------
C
      PARAMETER(MXNOD=300,MAXHOR=12)
C
      PARAMETER(MXNODC=MXNOD+2)
c      PARAMETER(HMIN=-35000.0D0)
      INTEGER NDXN2H(NN),IHBC(2),MAXITR
      LOGICAL START,FIRST,CNVG
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      DIMENSION DELZ(NN),GH(2),H(NN),HKBAR(NN),SOILHP(13,MAXHOR),
     +    QF(NN),QS(NN),THETA(NN),TL(NN),HITR(MXNODC),TTL(MXNODC),
     +    TDELZ(MXNODC),HOLD(MXNODC),TH(MXNODC),THKBAR(MXNODC),
     +    NDXHOR(MXNODC),TTHETA(MXNODC),TQF(MXNODC),TQS(MXNODC),
     +    H2OTAB(2),QT(NN),TQT(MXNODC),pori(nn),tpori(mxnodc),qssi(nn),
     +    tqssi(mxnodc)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      SAVE TTL,TDELZ,NNC,NDXHOR,FIRST
      DATA FIRST /.TRUE./
C
      IF(FIRST) THEN
        BOTHED=H(NN)
        FIRST=.FALSE.
      ENDIF
      MAXITR=MAXITER_RICH
      ITER=0
      ISTRT=0
      IF(ITBL.EQ.1) THEN
        H2OT1=MIN(H2OTAB(1),DBLE(NN))
      ELSE
        H2OT1=H2OTAB(1)
      ENDIF
C     
C     ..SET UP REDISTRIBUTION GRID
      CALL REDGRD(NN,NNC,NDXN2H,DELZ,TL,H,THETA,QS,IREBOT,BOTHED,BOTFLX,
     +    SOILHP,TDELZ,TTL,NDXHOR,HOLD,HITR,TTHETA,TQS,START,EVAP,DELT,
     +    QT,TQT,pori,tpori,Hmin,qssi,tqssi)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C     
C     ..AT START OF DAY, ALWAYS TRY EVAP CONDiTION 1ST. SET THIS UP.
      IF(DAYTIM.LE.0.0D0) THEN
        HITR(1)=MAX(HITR(1),HMIN+2.0D-5)
        ALPH=1.0D0
      ELSEIF(ITBL.NE.1) THEN
        ALPH=0.5D0
      ENDIF
C     
      IF (Istress.GE.3) THEN
         EVAPBC=-evaps  !max(-EVAPS,evap)
      ELSE
         EVAPBC=EVAP
      ENDIF
C     ..CHECK BOUNDRY CONDITIONS
      CALL CHKBC(EVAPBC,GH,HITR,HOLD,IHBC,NNC,DELT,IREBOT,BOTHED,BOTFLX,
     +    ITER,ISTRT,MAXITR,Hmin,nsp)
c      evap=evapbc
C     
C--------------------SOLVE RICHARD'S EQUATION-------------------------
C     
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
C     ... IMPLICIT (CRANK NICHOLSON SOLUTION)
      CALL CNHEAD(NNC,DELT,NDXHOR,HOLD,HITR,TH,TTHETA,TTL,THKBAR,TDELZ,
     +   TQS,SOILHP,GH,IHBC,EVAP,IREBOT,BOTHED,BOTFLX,ALPH,CNVG,ISTRT,
     +    MAXITR,H2OT1,TQT,IMAGIC,RICH_EPS,MAXCYCLE_RICH,tpori,qsn,
     +    ISHUTL,ishaw,EVAPS,Hmin,nsp,tqssi,istress)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C     
C     .. CALCULATE WATER FLUXES IN SOIL PROFILE
      CALL NODFLX(TDELZ,GH,TH,HOLD,THKBAR,IHBC,NNC,TQF,ALPH)
C     
C     .. UPDATE THETA VALUES
      CALL WCNODS(TH,SOILHP,NDXHOR,NNC,TTHETA,MAXHOR)
C     
C     ..SAVE PSUEDO-NODE VALUES
      HOLD(1)=TH(1)
      HOLD(NNC)=TH(NNC)
      HOLD(NN+2)=TH(NNC)
C     
C     .. TRANSFER HEAD VALUES BACK INTO TRUE GRID
      CALL TRUGRD(NN,H,THETA,QF,HKBAR,QS,TQS,TH,TTHETA,THKBAR,TQF,AEVAP,
     +    QT,TQT,Qssi,tqssi)
C     
c      if (ishaw.eq.1) AEVAP=EVAPS
      RETURN
      END
C
      SUBROUTINE SINK(PET,HK,DELZ,NN,H,RDF,SRT,TL,ET,HROOT,PUP,FIRST,
     +                Hmin)
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A      L  CONSTANT FOR NON-UNIFORM MIXING []
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       DEPTH  L  DEPTH (INCHES) TO BOTTOM OF SOIL LAYER [CM]
C       ENDL   L
C       ENDR   L
C       EPS        P  EPSILON - CONVERGENCE CRITERION
C       EPSADJ     L
C       ET        I/O
C       FIRST  I  TRUE IF FIRST TIME THROUGH ROUTINE
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HK         I  NODAL HYDRAULIC CONDUCTIV'S [CM/HR]
C       HMIN   P
C       HMINHF     P
C       HROOT I/O
C       I      L  INDEX VARIABLE
C       ITR        L
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       OLDHR  L
C       PET        I  POTENTIAL TRANSPIRATION [CM/DAY]
C       PUP       I/O
C       RDF        I  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       SRT       I/O
C       STEP   L
C       TA         L  TEMPERATURE AT AV. SAT VP (C)
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TSRT   L
C       U1         L
C       U2         L
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300)
      PARAMETER(EPS=1.0D-5)  !,HMIN=-35000.D0,HMINHF=HMIN/2.0D0) 
      PARAMETER(HMAX=0.0D0)
CC
      LOGICAL FIRST
      DIMENSION H(NN),RDF(NN),SRT(NN),TL(NN),DELZ(NN),HK(NN)
      DIMENSION U1(MXNOD),U2(MXNOD),TSRT(MXNOD)
      SAVE OLDHR
c      DATA OLDHR /HMINHF/
      DATA OLDHR /-7500.0D0/  !use half of 15 bar
C
      Hminhf=Hmin/2.0d0
      HROOT=OLDHR
      EPSADJ=1.0D-1
      IF(FIRST) HROOT=HMINHF
      ITR=0
      A=0.0D0
      ENDL=2.0D0*HROOT
      ENDR=HMAX
      STEP=ABS(ENDL-ENDR)*0.5D0
C
      DEPTH=0.5D0
      TA=0.0D0
      PUP=0.0D0
      DO 10 I=1,NN
        TSRT(I)=0.0D0
        U1(I)=0.0D0
        U2(I)=0.0D0
        IF(H(I).LE.0.0D0) THEN
          if (i.gt.1) DEPTH=DEPTH+DELZ(I-1)
          U1(I)=1.05D0*DEPTH-H(I)
          U2(I)=HK(I)*RDF(I)
          TSRT(I)=-(HMIN+U1(I))*U2(I)
          TSRT(I)=MAX(TSRT(I),0.0D0)
          TA=TA+TSRT(I)
          PUP=PUP+MAX(-(HMIN+U1(I))*U2(I),0.0D0)
        ENDIF
   10 CONTINUE
C     
   20 A=0.0D0
      DO 30 I=1,NN
        SRT(I)=0.0D0
        SRT(I)=-(HROOT+U1(I))*U2(I)
        SRT(I)=MAX(SRT(I),0.0D0)
        A=A+SRT(I)
   30 CONTINUE
C     
C     WRITE(*,191)ICOUNT,ITR,HROOT,STEP,A,PET
C     WRITE(88,191)ITR,HROOT,STEP,A,PET
C     
C     ..SAVE THE POTENTIAL UPTAKE FOR STRESS CALC'S
C     IF (ITR .EQ. 0) PUP = A
      ITR=ITR+1
C     
      IF(STEP.LE.EPS*EPSADJ) THEN
        HROOT=HMINHF
        ENDL=2.0D0*HROOT
        STEP=ABS(ENDL-ENDR)*0.5D0
        EPSADJ=EPSADJ*1.0D-1
        GOTO 20
      ENDIF
C     
      IF(TA.LT.(-PET+EPS)) THEN
        PUP=TA
        HROOT=HMIN
        DO 40 I=1,NN
          SRT(I)=TSRT(I)
   40   CONTINUE
        GOTO 50
      ENDIF
C     
      IF(A.GT.(-PET+EPS)) THEN
        STEP=STEP*0.5D0
        ENDL=HROOT
        HROOT=ENDL+STEP
        GOTO 20
      ELSEIF(A.LT.(-PET-EPS)) THEN
        STEP=STEP*0.5D0
        HROOT=ENDL+STEP
        HROOT=MAX(HROOT,HMIN)
        GOTO 20
      ENDIF
C     
   50 ET=0.0D0
      DO 60 I=1,NN
        IF(SRT(I).NE.0.0D0) THEN
          ET=ET+SRT(I)
          SRT(I)=ABS(SRT(I)/TL(I))
          SRT(I)=MAX(SRT(I),1.0D-60)
C       WRITE(88,191)-I,SRT(I),RDF(I),H(I),ET
        ENDIF
   60 CONTINUE
      OLDHR=HROOT
C     
C     191  FORMAT(I4,4G14.6)
C
      RETURN
      END
      FUNCTION BCBETA(HYDP1,HYDP2,HYDP5,HYDP6,HYDP13)
C
C======================================================================
C
C       PURPOSE: CALCULATE BETA TERM FOR MODIFIED BROOKS-COREY MODEL.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BCBETA    O  BETA TERM OF BROOK-COREY EQUATIONS
C       HYDP      I  MODIFIED BROOKS-COREY PARAMETERS
C                     1: HB      - BUBBLING PRESSURE THETA(H) [CM]
C                     2: LAMDA   - PORE SIZE DISTRIBUTION INDEX
C                     5: WR      - RESIDUAL WATER CONTENT
C                     6: WS      - SATURATION WATER CONTENT
C                     13:A1      - CONSTANT FOR THETA(H) CURVE
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION:   3-2004
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      BCBETA=(HYDP6-HYDP5-HYDP13*HYDP1)*HYDP1**HYDP2

      RETURN
      END

      FUNCTION SPMOIS(H,HYDP,I)
C
C======================================================================
C
C       PURPOSE: CALCULATE SPECIFIC MOISTURE BY MODIFIED BROOKS-COREY
C              MODEL.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       B      L  CONSTANT FOR NON-UNIFORM MIXING []
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HYDP   I  MODIFIED BROOKS-COREY PARAMETERS
C                 1: HB      - BUBBLING PRESSURE THETA(H) [CM]
C                 2: LAMDA - PORE SIZE DISTRIBUTION INDEX
C                 3: EPS   - EXPONENT FOR K(H) CURVE
C                 4: KSAT  - SAT HYDRAULIC CONDUCTIVTY [CM/HR]
C                 5: WR      - RESIDUAL WATER CONTENT
C                 6: WS      - SATURATION WATER CONTENT
C                 7: WFC   - VOL FIELD CAPACITY (1/3 BAR) WC
C                 8: WFC   - VOL FIELD CAPACITY (1/10 BAR) WC
C                 9: WWP   - VOL WILTING POINT (15 BAR) WC
C                 10:HB      - BUBBLING PRESSURE K(H) CURVE [CM]
C                 11:C2      - SECOND INTRCEPT ON K(H) CURVE
C                 12:N1      - FIRST EXPONENT FOR K(H) CURVE
C                 13:A1      - CONSTANT FOR O(H) CURVE
C       SPMOIS     O  SPECIFIC MOISTURE [1/MM]
C       TH         L
C
C       CALLED FROM:
C
C       PROGRAMMER: KEN ROJAS AND LAJ AHUJA
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MAXHOR=12)
      DIMENSION HYDP(13),TRHYDP(13,MAXHOR)
      INTEGER IRST
      SAVE TRHYDP,IRST
      DATA IRST /0/
C
      IF(I.GT.IRST) THEN
        DO 10 J=1,13
          TRHYDP(J,I)=HYDP(J)
   10   CONTINUE
        IRST=I
      ENDIF
      TH=H
      IF(TH.GE.0.0D0) THEN
C       
C       ..SATURATED SOIL
        SPMOIS=0.0D0
      ELSE
        IF(TH.GT.-HYDP(1)) THEN
C         ..ON THE SATURATED PORTION OF THE CURVE
          SPMOIS=HYDP(13)
        ELSEIF(TH.LT.-10D0*HYDP(1)) THEN
C         ..ON DRY PORTION (ORIG PORTION) OF THE CURVE
          B=BCBETA(TRHYDP(1,I),TRHYDP(2,I),TRHYDP(5,I),TRHYDP(6,I),
     +        TRHYDP(13,I))
          SPMOIS=B*TRHYDP(2,I)*(-TH)**(-TRHYDP(2,I)-1.0D0)
        ELSE
C         ..ON THE ADJUSTED PORTION OF THE CURVE
C         B2 = (TRHYDP(6,I)-TRHYDP(5,I)-TRHYDP(13,I)*TRHYDP(1,I))
C         +       * TRHYDP(1,I)**TRHYDP(2,I)
          B=BCBETA(HYDP(1),HYDP(2),HYDP(5),HYDP(6),HYDP(13))
          SPMOIS=B*HYDP(2)*(-TH)**(-HYDP(2)-1.0D0)
        ENDIF
      ENDIF
C     
      RETURN
      END
C
      FUNCTION WC(H,HYDP,I,ISTAT)
C
C======================================================================
C
C       PURPOSE: CALCULATE WATER CONTENT BY MODIFIED BROOKS-COREY MODEL
C              IF TILLAGE OCCURES THEN THE CURVE FOLOWS THE RELATIONSHIP
C              DESCRIBED BY AHUJA ET AL. (1997)
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AXINIT     L  LAMDA VALUE OF RECONSILIDATED SOIL
C       B      L  CONSTANT FOR NON-UNIFORM MIXING []
C       FRST   L  BOOLEAN FLAG FOR FIRST TIME THROUGH
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HYDP   I  MODIFIED BROOKS-COREY PARAMETERS
C                 1: HB      - BUBBLING PRESSURE THETA(H) [CM]
C                 2: LAMDA - PORE SIZE DISTRIBUTION INDEX
C                 3: EPS   - EXPONENT FOR K(H) CURVE
C                 4: KSAT  - SAT HYDRAULIC CONDUCTIVTY [CM/HR]
C                 5: WR      - RESIDUAL WATER CONTENT
C                 6: WS      - SATURATION WATER CONTENT
C                 7: WFC   - VOL FIELD CAPACITY (1/3 BAR) WC
C                 8: WFC   - VOL FIELD CAPACITY (1/10 BAR) WC
C                 9: WWP   - VOL WILTING POINT (15 BAR) WC
C                 10:HB      - BUBBLING PRESSURE K(H) CURVE [CM]
C                 11:C2      - SECOND INTRCEPT ON K(H) CURVE
C                 12:N1      - FIRST EXPONENT FOR K(H) CURVE
C                 13:A1      - CONSTANT FOR O(H) CURVE
C       I      I  HORIZON INDEX
C       TH         L
C       WC         O  MOISTURE CONTENT AT EACH 1 CM INCREMENT [0..1]
C
C       CALLED FROM:
C
C       PROGRAMMER: KEN ROJAS AND LAJ AHUJA
C
C       VERSION:   98
C
C-----------------------------------------------------------------------
C
C  .. DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXHOR=12)
      DIMENSION HYDP(13),TRHYDP(13,MAXHOR)
      SAVE TRHYDP
C
      IF(ISTAT.EQ.-1) THEN
        DO 10 J=1,13
          TRHYDP(J,I)=HYDP(J)
   10   CONTINUE
      ENDIF
      TH=H
      IF(TH.GE.0.0D0) THEN
C       
C       ..SATURATED SOIL SET TO SAT WAT CONTENT
        WC=HYDP(6)
      ELSE
        IF(TH.GT.-HYDP(1)) THEN
C         ..ON THE SATURATED PORTION OF THE CURVE
          WC=HYDP(6)+HYDP(13)*TH
        ELSEIF(TH.LT.-10D0*HYDP(1)) THEN
C         ..ON DRY PORTION (ORIG PORTION) OF THE CURVE
          B=BCBETA(TRHYDP(1,I),TRHYDP(2,I),TRHYDP(5,I),TRHYDP(6,I),
     +        TRHYDP(13,I))
          WC=B*(-TH)**(-TRHYDP(2,I))+TRHYDP(5,I)
        ELSE
C         ..ON THE ADJUSTED PORTION OF THE CURVE
          B=BCBETA(HYDP(1),HYDP(2),HYDP(5),HYDP(6),HYDP(13))
          WC=B*(-TH)**(-HYDP(2))+HYDP(5)
        ENDIF
      ENDIF
C     
      RETURN
      END
C
      FUNCTION WCH(H,THETA,HYDP,I,ISTAT)
C
C======================================================================
C
C       PURPOSE: CALCULATE PRESSURE HEAD BY MODIFIED BROOKS-COREY MODEL
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       B      L  CONSTANT FOR NON-UNIFORM MIXING []
C       P      L  LATITUDE [R]
C       HYDP   I  MODIFIED BROOKS-COREY PARAMETERS
C                 1: HB      - BUBBLING PRESSURE THETA(H) [CM]
C                 2: LAMDA - PORE SIZE DISTRIBUTION INDEX
C                 3: EPS   - EXPONENT FOR K(H) CURVE
C                 4: KSAT  - SAT HYDRAULIC CONDUCTIVTY [CM/HR]
C                 5: WR      - RESIDUAL WATER CONTENT
C                 6: WS      - SATURATION WATER CONTENT
C                 7: WFC   - VOL FIELD CAPACITY (1/3 BAR) WC
C                 8: WFC   - VOL FIELD CAPACITY (1/10 BAR) WC
C                 9: WWP   - VOL WILTING POINT (15 BAR) WC
C                 10:HB      - BUBBLING PRESSURE K(H) CURVE [CM]
C                 11:C2      - SECOND INTRCEPT ON K(H) CURVE
C                 12:N1      - FIRST EXPONENT FOR K(H) CURVE
C                 13:A1      - CONSTANT FOR O(H) CURVE
C       I      I  HORIZON INDEX
C       ISTAT  I  FLAG TO INDICATE REINITILAIZATION
C       THET   L
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       WCH        O  POINT SUCTION HEAD VALUE [CM]
C       WCH        O  POINT HEAD VALUE [MM]
C
C       CALLED FROM:
C          -WCHEAD: SEQUENCING ROUTINE FOR SOLUTION OF SUCTION HEAD
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION:   98
C
C-----------------------------------------------------------------------
C
C  .. DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      DIMENSION HYDP(13)
      PARAMETER(MAXHOR=12)
      DIMENSION HYDP(13),TRHYDP(13,MAXHOR),WC10S2(MAXHOR)
      SAVE TRHYDP,WC10S2
C
      IF(ISTAT.EQ.-1) THEN
        DO 10 J=1,13
          TRHYDP(J,I)=HYDP(J)
   10   CONTINUE
C       ..DETERMINE THETA VALUE AT 10S2
        B=BCBETA(HYDP(1),HYDP(2),HYDP(5),HYDP(6),HYDP(13))
        WC10S2(I)=B*(10.0D0*HYDP(1))**(-HYDP(2))+HYDP(5)
      ENDIF
C     
C     ..CHECK LIMITS ON THETA'S FOR MAXIMUM AND MINIMUM BASED ON WHAT
C     WAS PROVIDED BY THE USER INPUT FILE
      TH=H
      THET=THETA
c      THET=MAX(HYDP(9),THET)  !to account for lower than 15 bar Liwang 2017
C     
      IF(THET-(HYDP(6)-HYDP(13)*HYDP(1)).GE.-1.0D-10) THEN
        IF(HYDP(13).NE.0.D0.AND.TH.LE.0.D0) THEN
          WCH=(HYDP(6)-THET)/HYDP(13)*(-1.0D0)
        ELSEIF(TH.GE.-HYDP(1)) THEN
          WCH=TH
        ELSE
          WCH=-HYDP(1)
        ENDIF
      ELSE
        IF(THET.LE.WC10S2(I)) THEN
C         ..ON DRY PORTION (ORIG PORTION) OF THE CURVE
          B=BCBETA(TRHYDP(1,I),TRHYDP(2,I),TRHYDP(5,I),TRHYDP(6,I),
     +        TRHYDP(13,I))
          P=-1.0D0/TRHYDP(2,I)
          WCH=((THET-TRHYDP(5,I))/B)**P*(-1.0D0)
C       ELSE
        ELSEIF(THET.LT.HYDP(6)) THEN
C         ..ON THE ADJUSTED PORTION OF THE CURVE
          B=BCBETA(HYDP(1),HYDP(2),HYDP(5),HYDP(6),HYDP(13))
          P=-1.0D0/HYDP(2)
          WCH=((THET-HYDP(5))/B)**P*(-1.0D0)
C       ENDIF
        ELSE
          WCH=TH
        ENDIF
      ENDIF
      IF(ABS(WCH).LE.1.D-12) WCH=0.D0
C     
      RETURN
      END
C
      SUBROUTINE WCHEAD(THETA,H,SOILHP,NN,NDXN2H,MAXHOR)
C
C======================================================================
C
C       PURPOSE:  CALCULATE A HEAD VALUE FROM A THETA.
C
C       REF:  LAJ AHUJA, USDA-ARS, DURANT, OK
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       H     I/O NODAL SOIL WATER PRESSURE HEADS [CM]
C       I      L  INDEX VARIABLE
C       JH         L  HORIZON INDEX
C       MAXHOR     I  MAXIMUM NUMBER OF SOIL HORIZONS
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C
C       EXTERNAL REFERENCES:
C                 WCH
C
C       CALLED FROM:
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2.0
C
C======================================================================
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION H(NN),SOILHP(13,MAXHOR),THETA(NN),NDXN2H(NN)
      INTEGER I,JH
C
C  .. LOOP OVER SOIL NODES
      DO 10 I=1,NN
        JH=NDXN2H(I)
        H(I)=WCH(H(I),THETA(I),SOILHP(1,JH),JH,0)
C     
C     
C     .. TEMPORARY FOR CHECKING
C     TH = WC(H(I),SOILHP(1,JH),JH,0)
C     IF ((TH-THETA(I)) .GT. 1.0D-4) THEN
C     PRINT*,' HEADS DIDNT MATCH'
C     WRITE(9,100) I,(TH-THETA(I)),THETA(I),TH,H(I)
C100  FORMAT(1X,'LAYER: ',I3,'  DIFFERENCE',F15.5,
C     +  /' ORIGINAL THETA VALUE',F15.6,/' NEW THETA VALUE',F15.6,
C     +  /' HEAD VALUE',F15.8)
C     ENDIF
C
   10 CONTINUE
C     
      RETURN
      END
C
      SUBROUTINE WCNODS(H,SOILHP,NDXN2H,NN,THETA,MAXHOR)
C
C======================================================================
C
C       PURPOSE: CALCULATE WATER CONTENT IN SOIL PROFILE FROM HEAD
C              PROFILE
C
C       REF:  LAJ AHUJA, USDA-ARS, DURANT, OK
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       I      L  INDEX VARIABLE
C       JH         L  HORIZON INDEX
C       MAXHOR     I  MAXIMUM NUMBER OF SOIL HORIZONS
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       THETA I/O VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C
C       COMMENTS: SUITABLE FOR ANY CONSISTENT SET OF UNITS, NO
C          ADJUSTMENT NECESSARY.
C
C       EXTERNAL REFERENCES:
C                 WC
C                 C
C          - WC: CALCULATE WATER CONTENT @ A SINGLE NODE
C
C       CALLED FROM:
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION:   2.0
C
C-----------------------------------------------------------------------
C
C  .. DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION H(NN),SOILHP(13,MAXHOR),THETA(NN),NDXN2H(NN)
C
C  .. LOOP OVER SOIL NODES
      DO 10 I=1,NN
        JH=NDXN2H(I)
        THETA(I)=WC(H(I),SOILHP(1,JH),JH,0)
C     
C     .. TEMPORARY FOR CHECKING
C     TH = WCH(H(I),THETA(I),SOILHP(1,JH),JH,0)
C     IF ((TH-H(I)) .GT. 1.0D-9) THEN
C     PRINT*,' THETA DIDNT MATCH'
C     WRITE(9,100) I,(TH-H(I)),H(I),TH,THETA(I)
C100  FORMAT(1X,'LAYER: ',I3,'  DIFFERENCE',F15.5,
C     +  /' ORIGINAL HEAD VALUE',F15.2,/' NEW HEAD VALUE',F15.2,
C     +  /' THETA VALUE',F15.8)
C     ENDIF
C
   10 CONTINUE
C     
      RETURN
      END
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      SUBROUTINE REDGRD(NN,NNC,NDXN2H,DELZ,TL,H,THETA,QS,IREBOT,BOTHED,
     +    BOTFLX,SOILHP,TDELZ,TTL,NDXHOR,HOLD,HITR,TTHETA,TQS,START,
     +    EVAP,DELT,QT,TQT,pori,tpori,Hmin,qssi,tqssi)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C=======================================================================
C
C       PURPOSE:  CREATES THE REDISTRIBUTION GRID DEPENDING UPON THE
C             SLOPE OF THE DEPTH VS. HEAD CURVE. IF THERE IS A
C             STEEP WETTING FRONT, IT WILL CREATE A FINE GRID
C             AT THE FRONT TO AID CONVERGING.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BOTFLX     I
C       BOTHED     I
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       EVAP   I  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HITR   I
C       HKBAR  L  LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HMIN   P
C       HOLD   I  NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODC     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NDXHOR     I
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NNC       I/O
C       QS         I  NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       START  I
C       TDELZ  I
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TQS        I  TOTAL AND AVERAGE ROOT UPTAKE OVER STIME [CM/HR]
C       TTHETA     I
C       TTL        I
C
C
C       CALLED FROM: RICHRD
C
C       PROGRAMMER: KAREN JOHNSEN
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12)
C
      PARAMETER(MXNODC=MXNOD+2)  !,HMIN=-35000.D0)
C
      INTEGER NDXN2H(NN),NDXHOR(MXNODC)
C
      LOGICAL START
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      DIMENSION DELZ(NN),H(NN),SOILHP(13,MAXHOR),QS(NN),THETA(NN),
     +    TL(NN),HITR(MXNODC),TTL(MXNODC),TDELZ(MXNODC),HOLD(MXNODC),
     +    TTHETA(MXNODC),TQS(MXNODC),HKBAR(MXNODC),QT(NN),TQT(MXNODC),
     +    pori(nn),tpori(mxnodc),qssi(nn),tqssi(mxnodc)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C     ..SET UP GRID WHEN NOT IN WATER TABLE CONDITIONS, OR A REFINED
C       GRID ISN'T NECESSARY
C
      IF(START) THEN
C       
C       .. SET UP REDISTRIBUTION GRID USING CURRENT NUMERICAL GRID
C       
C       ADD PSUEDO-SURFACE AND -BOTTOM NODES TO THE TRUE GRID,
C       AND GET INITIAL CONDITIONS AND OLD H VALUES
        NNC=NN+2
C       
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
        CALL INITCOND(NN,NNC,MAXHOR,THETA,H,QS,SOILHP,DELZ,TL,NDXN2H,
     +      HOLD,HMIN,HITR,TTHETA,TQS,TDELZ,TTL,NDXHOR,HKBAR,START,DELT,
     +      EVAP,IREBOT,BOTFLX,BOTHED,QT,TQT,pori,tpori,qssi,tqssi)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C     
      ELSE
C       
C       .. NORMAL REDISTRIBUTION GRID HAS ALREADY BEEN SET UP;
C       SET UP NEW INITIAL GUESS, SINK TERMS, THETA;
C       STORE SOLUTION FROM PREVIOUS TIME STEP
C       
C       
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
        CALL INITCOND(NN,NNC,MAXHOR,THETA,H,QS,SOILHP,DELZ,TL,NDXN2H,
     +      HOLD,HMIN,HITR,TTHETA,TQS,TDELZ,TTL,NDXHOR,HKBAR,START,DELT,
     +      EVAP,IREBOT,BOTFLX,BOTHED,QT,TQT,pori,tpori,qssi,tqssi)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C     
      ENDIF
C     
      RETURN
      END
C
      SUBROUTINE TRUGRD(NN,H,THETA,QF,HKBAR,QS,TQS,TH,TTHETA,THKBAR,TQF,
     +    AEVAP,QT,TQT,qssi,tqssi)
C=======================================================================
C
C       PURPOSE:  RETURNS REDISTRIBUTION VALUES TO TRUE NUMERICAL
C             GRID
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AEVAP     ACTUAL EVAPORATION
C       H     I/O NODAL SOIL WATER PRESSURE HEADS [CM]
C       HKBAR I/O LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       I      L  INDEX VARIABLE
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODC     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       QF        I/O LAYER INTERFACIAL MOIST FLUXES [CM/HR]
C       QS        I/O NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       TH         I
C       THETA I/O VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       THKBAR     I
C       TQF        I  TOTAL AND AVERAGE WATER FLUX OVER STIME [CM/HR]
C       TQS        I  TOTAL AND AVERAGE ROOT UPTAKE OVER STIME [CM/HR]
C       TTHETA     I
C       NN            NUMBER OF NUMERICAL NODES
C       NND           NUMBER OF NODES FOR REFINED REDISTRIBUTION GRID
C       H         ARRAY OF HEAD VALUES AT EACH NODE - NUM. GRID
C       THETA     ARRAY OF WATER CONTENTS AT EACH NODE - NUM. GRID
C       QF            ARRAY OF WATER FLUXES - NUM. GRID
C       HKBAR     ARRAY OF HYDRAULIC CONDUCTIVITIES - NUM. GRID
C       TH            ARRAY OF HEAD VALUES AT EACH NODE -  REDIST. GRID
C       TTHETA        ARRAY OF WATER CONTENTS AT EACH NODE - REDIST. GRID
C       THKBAR        ARRAY OF HYDRAULIC CONDUCTIVITIES - REDIST. GRID
C       TQF           ARRAY OF WATER FLUXES - REDIST. GRID
C
C       CALLED FROM:
C
C       PROGRAMMER: KAREN JOHNSEN
C
C       VERSION:   2
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300)
C
      PARAMETER(MXNODC=MXNOD+2)
C
      DIMENSION H(NN),HKBAR(NN),QF(NN),THETA(NN),QS(NN),TQS(MXNODC),
     +    TH(MXNODC),THKBAR(MXNODC),TTHETA(MXNODC),TQF(MXNODC),QT(NN),
     +    TQT(MXNODC),qssi(nn),tqssi(mxnodc)
C
C
      AEVAP=TQF(2)
C     
C     ..SIMPLE CASE: JUST TAKE CARE OF PSEUDO-NODES
      DO 10 I=1,NN
        H(I)=TH(I+1)
        QF(I)=TQF(I+2)
        QS(I)=TQS(I+1)
        QSsi(I)=TQSsi(I+1)
        QT(I)=TQT(I+1)
        THETA(I)=TTHETA(I+1)
        HKBAR(I)=THKBAR(I+1)
   10 CONTINUE
C     
C     
      RETURN
      END
C
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      SUBROUTINE INITCOND(NN,NNC,MAXHOR,THETA,H,QS,SOILHP,DELZ,TL,
     +    NDXN2H,HOLD,HMIN,HITR,TTHETA,TQS,TDELZ,TTL,NDXHOR,HKBAR,START,
     +    DELT,EVAP,IREBOT,BOTFLX,BOTHED,QT,TQT,pori,tpori,qssi,tqssi)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
C======================================================================
C
C       PURPOSE:  SETS UP THE INITIAL GUESS FOR THE RICHARDS' EQN.
C             SOLVER, AND ADDS PSEUDO NODES FOR THE BOUNDARY
C             CONDITIONS
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BOTFLX     I
C       BOTHED     I
C       DELT  I/O INCREMENTAL TIME STEP [HR]
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       EVAP   I  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       FLX        L
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       HITR  I/O
C       HKBAR I/O LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HKBI   L
C       HKBP   L
C       HMIN   I
C       HOLD  I/O NODAL HEADS FROM PREVIOUS TIME STEP [CM]
C       I      L  INDEX VARIABLE
C       IH         L
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       JH         L  HORIZON INDEX
C       MAXHOR     I  MAXIMUM NUMBER OF SOIL HORIZONS
C       NDXHOR    I/O
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NNC        I
C       QS         I  NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       RESTRT     L
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       START  I
C       TDELZ I/O
C       THET   L
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TMPK   L
C       TQS       I/O TOTAL AND AVERAGE ROOT UPTAKE OVER STIME [CM/HR]
C       TTHETA    I/O
C       TTL       I/O
C       U      L  IONIC STRENGTH [MOLES/L]
C
C       CALLED FROM:  REDGRD
C
C       PROGRAMMER: KAREN JOHNSEN
C
C       VERSION:   2
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INTEGER NDXN2H(NN),NDXHOR(NNC)
C
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      DIMENSION THETA(NN),H(NN),SOILHP(13,MAXHOR),DELZ(NN),TL(NN),
     +    QS(NN),HOLD(NNC),HITR(NNC),TTHETA(NNC),TQS(NNC),TDELZ(NNC),
     +    TTL(NNC),HKBAR(NNC),QT(NN),TQT(NNC),pori(nn),tpori(nnc),
     +    qssi(nn),tqssi(nnc)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
      LOGICAL START,RESTRT
C
      RESTRT=.FALSE.
C
C     
C     ..SET UP SPACE INCREMENTS IN NNC GRID
      DO 10 I=2,(NNC-1)
        TDELZ(I)=DELZ(I-1)
        TTL(I)=TL(I-1)
        NDXHOR(I)=NDXN2H(I-1)
   10 CONTINUE
      TDELZ(1)=DELZ(1)
      TTL(1)=TL(1)
      NDXHOR(1)=NDXN2H(1)
      TDELZ(NNC-1)=2.D0*TL(NN)-DELZ(NN-1)
      TTL(NNC)=TL(NN)
      NDXHOR(NNC)=NDXN2H(NN)
C     
C     ..SET UP OLD VALUES OF H AND SINK TERM
      IF(HOLD(1).GT.HMIN+1.0D-5.OR.START) THEN
        IH=NDXHOR(1)
        IF(START) THEN
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf
c     jak
          TMPK=POINTK(H(1),SOILHP(1,IH),IH,pori(1))
          FLX=MIN(TMPK,EVAP)
c           if (tmpk.gt.0.0) then
          HOLD(1)=H(1)-TDELZ(1)*(TMPK-FLX)/TMPK
c             else
c              HOLD(1) = H(1)
c           end if
        ENDIF
        HOLD(1)=MAX(HOLD(1),HMIN)
        TTHETA(1)=WC(HOLD(1),SOILHP(1,IH),IH,0)
	  tpori(1)=pori(1)
      ELSE
        HOLD(1)=MAX(HOLD(1),HMIN)
        TTHETA(1)=THETA(1)
	  tpori(1)=pori(1)
      ENDIF
      TQS(1)=0.0D0
      TQSSI(1)=0.0d0
      TQT(1)=0.0D0
C     
      DO 20 I=2,(NNC-1)
        HOLD(I)=H(I-1)
        TQS(I)=QS(I-1)
        TQSSI(I)=QSSI(I-1)
        TQT(I)=QT(I-1)
        TTHETA(I)=THETA(I-1)
	  tpori(i)=pori(i-1)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
   20 CONTINUE
C     
      IH=NDXHOR(NNC)
      IF(START) THEN
        IF(IREBOT.EQ.1) THEN
          HOLD(NNC)=2.D0*BOTHED-HOLD(NNC-1)
        ELSEIF(IREBOT.EQ.3) THEN
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf
c     jak
          TMPK=POINTK(HOLD(NNC-1),SOILHP(1,IH),IH,pori(nn))
c           if (tmpk.gt.0.0) then
          HOLD(NNC)=TDELZ(NNC-1)*(TMPK-BOTFLX)/TMPK+HOLD(NNC-1)
c        ELSE
c              HOLD(NNC)=HOLD(NNC-1)
c           end if
        ELSE
          HOLD(NNC)=H(NN)
        ENDIF
      ENDIF
      HOLD(NNC)=MAX(HOLD(NNC),HMIN)
      TQS(NNC)=0.0D0
      TQSSI(NNC)=0.0D0
      TQT(NNC)=0.0D0
      TTHETA(NNC)=WC(HOLD(NNC),SOILHP(1,IH),IH,0)
	tpori(nnc)=pori(nn)
C     
C     ..SET UP INITIAL GUESS
      TTDELT=DELT
   30 HKBI=POINTK(HOLD(1),SOILHP(1,NDXHOR(1)),NDXHOR(1),pori(1))
      HKBP=POINTK(HOLD(2),SOILHP(1,NDXHOR(2)),NDXHOR(2),pori(2))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      HKBAR(1)=DSQRT(HKBI*HKBP)
      DO 40 I=2,NNC-1
        IH=NDXHOR(I)
        JH=NDXHOR(I+1)
        U=TTDELT/TTL(I)
        HKBI=HKBP
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
        HKBP=POINTK(HOLD(I+1),SOILHP(1,JH),JH,tpori(i+1))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        HKBAR(I)=DSQRT(HKBI*HKBP)
        THET=U*(-HKBAR(I-1)*(HOLD(I)-HOLD(I-1))/TDELZ(I-1)+HKBAR(I)*(
     +      HOLD(I+1)-HOLD(I))/TDELZ(I)+HKBAR(I-1)-HKBAR(I))+TTHETA(I)
        IF(THET.LT.SOILHP(9,IH).AND..NOT.RESTRT) THEN
          TTDELT=TTDELT*0.5D0
          RESTRT=.TRUE.
          GOTO 30
        ENDIF
        THET=MAX(THET,SOILHP(9,IH))
        THET=MIN(THET,SOILHP(6,IH))
        HITR(I)=WCH(HOLD(I),THET,SOILHP(1,IH),IH,0)
        HITR(I)=MAX(HITR(I),HMIN)
!           DO NOT LET ESTIMATE OVERSHOOT POTENTIAL OF LAYER BELOW   GNF 4/10/06 ask GNF why? 9-2-2009
c  commnented out temporarily unless there is problems in convergence. 
c            IF (HOLD(I) > HOLD(I+1)) HITR(I)=MAX(HITR(I),HOLD(I+1))
   40 CONTINUE
      HITR(1)=MIN(HOLD(1),HITR(2))
      HITR(1)=MAX(HMIN,HITR(1))
      IF(HITR(NNC-1).GT.0.0D0) THEN
        HITR(NNC)=MAX(HOLD(NNC),HITR(NNC-1))
      ELSE
        HITR(NNC)=MIN(HOLD(NNC),HITR(NNC-1))
      ENDIF
      HITR(NNC)=MAX(HMIN,HITR(NNC))
C     
      RETURN
      END
C
C
      SUBROUTINE WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QNN,DELT,TLT,ZN,IREBOT,
     +    BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,THETA)
C
C======================================================================
C
C       PURPOSE:  CHECKS FOR WATER TABLE WITHIN PROFILE AND SETS UP
C             VARIABLES FOR THIS CONDITION
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       NN            NUMBER OF NUMERICAL NODES
C       NDXN2H        INDEX ARRAY TO GO FROM NUMERICAL TO HORIZON GRID
C       MAXHOR        MAXIMUM NUMBER OF HORIZONS
C       SOILHP        ARRAY OF SOIL HYDRAULIC PROPERTIES
C       H         ARRAY OF HEAD VALUES AT EACH NODE - NUM. GRID
C       TLT           ARRAY OF DEPTHS TO BOTTOM OF EACH LAYER - NUM. GRID
C       IREBOT        FLAG FOR BOTTOM BOUNDARY CONDITION
C       BOTHED        VALUE OF BOTTOM HEAD FOR CONSTANT HEAD BBC
C       BOTFLX        VALUE OF LEAKAGE RATE FOR CONSTANT FLUX BBC
C       CAPZONE       DEPTH TO TOP OF CAPILLARY ZONE
C       H2OTAB        ARRAY CONTAINING NODE # AND DEPTH OF WATER TABLE
C       ALPH      VARIABLE INDICATING FDM TO BE USED
C       AEF           AIR ENTRY FACTOR
C       PREDEP        PREVIOUS DEPTH OF WATER TABLE
C       CURDEP        CURRENT DEPTH OF WATER TABLE
C
C       CALLED FROM:
C
C       PROGRAMMER: KAREN JOHNSEN
C
C       VERSION:   1
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      PARAMETER(MXNOD=300)
      PARAMETER(MXTSTP=5000)
C
      PARAMETER(FLXMX=5.D0)
C
      DIMENSION H(NN),TLT(NN),H2OTAB(2),NDXN2H(NN),SOILHP(13,MAXHOR),
     +    ZN(NN),THETA(NN)
C
      LOGICAL TBLCND
      SAVE TBLCND
C
      DATA TBLCND /.FALSE./
C
C   ..INITIALIZE WATER TABLE
      TBLCND=.TRUE.
      PREDEP=H2OTAB(2)
      H2OTAB(1)=MXNOD*3
      H2OTAB(2)=MXTSTP*3
C     ..FORM OF FINITE DIFFERENCE METHOD TO BE USED
C     0.5D0 -->  CRANK-NICOLSEN
C     1.D0 --> FULLY IMPLICIT
C     0.D0 --> FULLY EXPLICIT
      ALPH=0.5D0
C     
C     ------------------------
C     original code 8.23.02
C     ------------------------
CC    
CC    ..FIND WATER TABLE, IF ANY
C     NSAT = 0
C     DO 20 I=1,NN
CC    
CC    ..FIND FIRST UNSAT. LAYER ABOVE SAT. LAYERS
C     IF (H(I).LT.0.D0 .AND. H(NN).GE.0.D0) THEN
C     IREBOT = 3
C     BOTFLX = MAX(BOTFLX,0.0D0)
C     BOTFLX = MIN(BOTFLX,FLXMX)
C     ALPH = 1.0D0
C     TBLCND = .TRUE.
C     AEF = 1.0D0
C     DO 10 J = I,NN
C     JH = NDXN2H(J)
C     IF (H(J).GE.0.D0) THEN
CC    H2OTAB(1) = DBLE(J-1)
CC    ..SET WATER TABLE AT BOTTOM OF PREVIOUS NUMERICAL LAYER
CC    H2OTAB(2) = TLT(J-1)
CC    ..DO LINEAR INTERP. BTWN THIS NODE AND PREVIOUS TO
CC    ESTIMATE WATER TABLE DEPTH
C     H2OTAB(2) = ZN(J) - (ZN(J)-ZN(J-1))/(H(J)-H(J-1))
C     1                   * H(J)
CC    
CC    ..FIND CAPILLARY ZONE
C     H2OTAB(1) = DBLE(J)
CC    CAPZONE = TLT(J-1) + 1.0d0
C     DO 5 K = J-1,1,-1
C     KH = NDXN2H(K)
C     IF (H2OTAB(2) .LT. TLT(K)) THEN
C     H2OTAB(1) = DBLE(K)
C     ENDIF
C     IF (ABS(THETA(K)-SOILHP(6,KH)) .GT. 1.D-12) THEN
CC    CAPZONE = TLT(K) + 1.0D0
C     GOTO 999
C     ENDIF
C5    CONTINUE
C     GOTO 999
C     ENDIF
C     10     CONTINUE
CC    
CC    .. COUNT SAT. LAYERS
C     ELSE IF (H(I).GE.0.D0) THEN
C     NSAT = NSAT+1
C     ENDIF
C     20 CONTINUE
C     
C     ------------------------
C     
C     ..FIND WATER TABLE, IF ANY
C     changed: 8.23.2002 by ken
C     ..nsat is the number of saturate layers
      NSAT=0
      TAG=0.0d0
C     ..search from the bottom up
      DO 10 I=NN,1,-1
C       
C       ..FIND FIRST UNSAT. LAYER ABOVE SAT. LAYERS
        IF(H(I).LT.0.D0.AND.H(NN).GE.0.D0) THEN
          IREBOT=3
          BOTFLX=MAX(BOTFLX,0.0D0)
          BOTFLX=MIN(BOTFLX,FLXMX)
          ALPH=1.0D0
          AEF=1.0D0
          TBLCND=.FALSE.
C         .. determined that wt exist, now SET it.
          J=I+1
          TAG=dble(NN)
          H2OTAB(2)=ZN(J)-(ZN(J)-ZN(J-1))/(H(J)-H(J-1))*H(J)
          DO 15 JJ = 1,NN
            IF (INT(H2OTAB(2)).LE.TLT(1)) TAG=1.0d0
            IF (INT(H2OTAB(2)).GT.TLT(JJ)) TAG=dble(JJ)
15        CONTINUE
          H2OTAB(1)=MIN(TAG+1.0d0,DBLE(NN))
          GOTO 20
        ELSEIF(H(I).GE.0.D0) THEN
C         .. COUNT number of SAT. LAYERS
          NSAT=NSAT+1
        ENDIF
   10 CONTINUE
C     
C     ..CHECK FOR FULLY SATURATED PROFILE
      IF(NSAT.EQ.NN) THEN
        H2OTAB(1)=1.D0
        H2OTAB(2)=0.D0
C       CAPZONE = 0.0D0
        IREBOT=3
        BOTFLX=MAX(BOTFLX,0.0D0)
        BOTFLX=MIN(BOTFLX,FLXMX)
        ALPH=1.0D0
        AEF=1.0D0
        TBLCND=.FALSE.
C     
C     ..CHECK TO SEE IF WE HAD WATER TABLE CONDITIONS WHICH MAY
C     HAVE LEFT
      ELSEIF(TBLCND) THEN
C       
C       ..IF SO, THEN THE TABLE HAS LEFT THE PROFILE - TRACK
C       LOCATION AND SET BOTTOM BC
        JH=NDXN2H(NN)
        if (abs(SOILHP(6,JH)-SOILHP(8,JH)).le.1.0d-12) then   !Liwang Ma to prevent denominator becoming zero, 4-14-2014
        CURDEP=-((QNN-BOTFLX)*DELT)/(SOILHP(6,JH)-SOILHP(7,JH))+PREDEP
        else
        CURDEP=-((QNN-BOTFLX)*DELT)/(SOILHP(6,JH)-SOILHP(8,JH))+PREDEP
        endif
        IF(CURDEP-TLT(NN).GT.50.D0) THEN
          IREBOT=2
        ELSEIF(CURDEP-TLT(NN).GE.0.D0) THEN
          IREBOT=1
          BOTHED=CURDEP-TLT(NN)
        ELSE
          IREBOT=3
          BOTFLX=MAX(BOTFLX,0.0D0)
          BOTFLX=MIN(BOTFLX,FLXMX)
          H2OTAB(1)=DBLE(NN)
          CURDEP=TLT(NN)
          ALPH=1.0D0
        ENDIF
        H2OTAB(2)=CURDEP
C     CAPZONE = CURDEP-1.0D0
      ENDIF
   20 IF(H2OTAB(2).EQ.0.0D0) THEN
        CAPZONE=H2OTAB(2)
      ELSE
        CAPZONE=H2OTAB(2)-1.0D0
      ENDIF
C     print*,'capzone: ',capzone,H2OTAB(2)
C     
      RETURN
      END
