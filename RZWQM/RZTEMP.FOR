C $Id: RZTEMP.FOR,v 1.1 2002/08/28 00:00:48 rojas Exp $
C
      SUBROUTINE CHKCON(CONRUL,CONVRG,EPS,NN,X,XNEW)
C
C======================================================================
C
C       PURPOSE: CHECK CONVERGENCE OF A VECTOR SOLUTION
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CONRUL     I  TYPE OF CONVERGENCE CRITERION ('ABS ','REL ')
C       CONVRG     O  LOGICAL CONVERGENCE FLAG (TRUE--CONVERGENCE,
C                 FALSE--NO CONVERGENCE)
C       EPS        I  ERROR CONVERGENCE CRITERION
C       NN         I  LENGTH OF SOLUTION VECTOR
C       ERR           CALCULATED RELATIVE ERROR
C       X     I/O I: PREVIOUS SOLUTION VECTOR
C                 O: REPLACED WITH XNEW
C       XNEW   I  NEW SOLUTION VECTOR
C
C       COMMENTS:
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES HEBSON
C
C       VERSION: 1
C
C======================================================================
C
C
C     ..DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER CONRUL*4
      LOGICAL CONVRG
      INTEGER I,NN
      DIMENSION X(NN),XNEW(NN)
C
      CONVRG=.TRUE.
C     
C     ..REPEAT FOR ALL VECTOR ELEMENTS
C     
      DO 10 I=1,NN
        ERROR=ABS(XNEW(I)-X(I))
        IF(CONRUL.EQ.'ABS '.AND.ERROR.GT.EPS) CONVRG=.FALSE.
        IF(CONRUL.EQ.'REL '.AND.X(I).NE.0.0) THEN
          IF(ERROR/ABS(X(I)).GT.EPS) CONVRG=.FALSE.
        ENDIF
        X(I)=XNEW(I)
   10 CONTINUE
C     
      RETURN
      END
C
      SUBROUTINE FACEC(CSHD,CSH,NDXN2H,NN,QF,QS,QSS,T,THETA,MAXHOR)
C
C======================================================================
C
C       PURPOSE: CALCULATE INTERFACE COEFFICIENTS (VOL HEAT CAPACITY
C              AND CONVECTIVE VELOCITY) FOR HEAT EQUATION NUM SOLN
C
C       REF: DEVRIES, THERMAL PROPERTIES OF SOILS, CH 7:210-213, IN
C          PHYSICS OF PLANT ENVIRONMENT (VAN WIJK), WILEY, 1963.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CSH        O  V: EFFECTIVE SOIL LAYER VOLUMETRIC HEAT
C                   CAPACITIES [J/MM^3/C]
C       CSOLD     EFFECTIVE SOIL LAYER VOL HEAT CAPACITY
C                   [J/MM^3/C] @ PREVIOUS TIME STEP
C       CSHD   I  V: SOIL HORIZON DRY VOLUMETRIC HEAT CAPACITIES
C                   [J/MM^3/C]
C       CW            VOLUMETRIC HEAT CAPACITY OF WATER [J/MM^3/C]
C       NDXN2H     I  INDEX VECTOR RELATING SOIL HORIZONS TO
C                 CONSTITUENT NUMERICAL LAYERS E.G. LAYER I IS
C                 IN HORIZON # NDXN2H(I)
C       JH            HORIZON INDEX
C       NN        I   NUMBER OF NODES
C       QF         I/O    I: V: INTERFACIAL MOISTURE FLUX [MM/HR]
C                 O   SCALED BY (CW/CS) RATIO
C       QS        I   V: PLANT WATER UPTAKE [MM^3/HR/MM^3]
C       QSS       O   V: QS SCALED BY (CW/CS) RATIO
C       T      I/O    I: V: TEMP SOLN FROM PREVIOUS TIME STEP
C                 O:  SCALED BY RATIO (CSOLD/CS)
C       THETA I   VECTOR OF NODAL MOISTURE CONTENTS
C
C       COMMENTS:
C          - SUITABLE FOR ANY CONSISTENT SET OF UNITS;
C          - CSHD SHOULD PROBABLY BE RE-CALCULATED PERIODICALLY AS A
C          FUNCTION OF SOIL OM (SEE WIERENGA ET AL, SSSA, 33:P.355,
C          1969 FOR FUNCTION)
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION:   1.0
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C ... DECLARE VARIABLES
      PARAMETER(CW=4.184D-3)
      DIMENSION CSHD(MAXHOR),CSH(NN),QF(NN),QS(NN),QSS(NN),THETA(NN),
     +    NDXN2H(NN),T(NN)
C
C ... LOOP OVER NUMERICAL LAYERS
      DO 10 I=1,NN
C       
C       ... SAVE LAYER HEAT CAPACITY FROM PREVIOUS TIME STEP
        CSOLD=CSH(I)
C       
C       ... NUMERICAL LAYER HORIZON INDEX
        JH=NDXN2H(I)
C       
C       ... NEW EFFECTIVE LAYER HEAT CAPACITY
        CSH(I)=CSHD(JH)+CW*THETA(I)
C       
C       ... SCALED INTERFACE FLUX
        RATIO=CW/CSH(I)
        QF(I)=RATIO*QF(I)
C       
C       ... SCALED PLANT WATER UPTAKE
        QSS(I)=RATIO*QS(I)
C       
C       ... SCALED TEMP @ PREVIOUS TIME STEP
        T(I)=CSOLD*T(I)/CSH(I)
   10 CONTINUE
C     
      RETURN
      END
C
C
C
      FUNCTION FNDELT(SLOPE)
C
C======================================================================
C
C       PURPOSE: TO FIND AN APPROPRIATE TIME STEP FOR THE HEAT MODEL
C              USING ARBITRARY CUTOFF SLOPE VALUES
C
C       REF: DICE
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       DELTMD     P  median time step
C       DELTMN     P  minimum time step
C       DELTMX     P  maximum time step
C       EPS1   P  mininum slope for mediam time step
C       EPS2   P  minimum slope for minimum time step
C       SLOPE  I  maximum slope of temperature profile
C
C       COMMENTS: THE CHOICE OF TIME STEP IS MADE BY ASSIGNING SMALLER
C             TIME STEPS TO LARGER SLOPES AND VICA-VERSA.
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:  SANDY MAPLE
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C  ..SET MAXIMUM AND MINIMUM SLOPE VALUES
      PARAMETER(EPS1=2.0D0,EPS2=0.1D0)
C
C  ..SET MAXIMUM, MEDIUM AND MINIMUM TIME STEPS FOR HEAT MODEL
C      PARAMETER(DELTMX=6.0D0,DELTMD=1.0D0,DELTMN=0.1D0)
C
      DELTMX=6.0D0
      DELTMD=1.0D0
      DELTMN=0.1D0
      IF(SLOPE.GE.EPS1) THEN
        FNDELT=DELTMN
      ELSEIF(SLOPE.GE.EPS2) THEN
        FNDELT=DELTMD
      ELSE
        FNDELT=DELTMX
      ENDIF
      RETURN
C     
      END
C
      FUNCTION FNDSLP(TM,T,DELZ,NN)
C
C======================================================================
C
C       PURPOSE: THE PURPOSE OF FNDSLP IS TO FIND THE MAXIMUM SLOPE OF
C              THE TEMPERATURE PROFILE INCLUDING THE BOUNDARY
C              CONDITION, TM
C
C       REF: "THIS LIFT SERVES MOST DIFFICULT AND  EXPERT ONLY TERRAIN"
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       DELZ   I  V: nodal elev forward differences [mm]
C       FNDSLP     O
C       IQ         L
C       NN         I  number of nodes
C       SLOPE  L  maximum slope of temperature profile
C       SLPMAX     L
C       T     I/O V: nodal temperatures [C]
C       TM         I  upper boundary condition of temperature profile
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:  STUPENDOUS MAN
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION T(NN),DELZ(NN)
C
      SLPMAX=ABS((T(1)-TM)/DELZ(1))
      DO 10 IQ=2,NN-1
        SLOPE=ABS((T(IQ)-T(IQ-1))/DELZ(IQ-1))
        SLPMAX=MAX(SLOPE,SLPMAX)
   10 CONTINUE
C     
      FNDSLP=SLPMAX
C     
      RETURN
      END
C
      SUBROUTINE HEATFX(CSH,SOLTP2,T,DELT,DELZ,NDXN2H,NN,ATMP,QF,QS,
     +    THETA,TL,SOLTP1,TM,TBOTM)
C
C======================================================================
C
C       PURPOSE: THE PURPOSE OF THE HEAT SUBROUTINE IS TO CALL THE
C              HEAT EQUATION SUBROUTINE AT THE APPROPRIATE TIME,
C              USING VARIABLE TIME STEPS TO MAXIMIZE PERFORMANCE
C              AND SPEED
C
C       REF: STUPENDOUS MAN FROM "TIMING IN THE KNOWN UNIVERSE"
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ATMP   I  atmospheric pressure [KPA]
C       CSH        I  V: nodal (wet) soil heat capacities [J/mm^3/C]
C                 convergence criterion ('ABS ','REL ')
C       DELT   I  incremental time step
C       DELZ   I  V: nodal elev forward differences [mm]
C       DTIME  L  time of day
C                 equilbrium ie. during initialization
C       IGOR   L  simple counter
C       MAXHOR     P  maximum number of soil horizons
C       MAXSCT     P  max number of soil constituents per horizon
C       MXNOD  P  max number of numerical nodes
C       NDXN2H     I  index between numerical and horizon vectors
C       NHOR   I  number of soil horizons (types)
C       NN         I  number of nodes
C       QF         I  V: moisture flux @ interfaces [mm/hr]
C       QS         I  V: plant water uptake [mm^3/hr/mm^3]
C       SLOPE  L  maximum slope of temperature profile
C       SOLTP1     I  (MAXHOR x 5) array of heat model parameters,
C       SOLTP2     I  (MAXSCT x MAXHOR x 3) array of soil horizon
C       STIME  L  time since last invocation of heat model
C       T      I  V: nodal temperatures [C]
C       TDELT  L  time step used in heat model
C       THETA  I  V: nodal moisture contents
C       TIME   I  time in hours, used in determining boundary conditions
C       TL         I  V: layer thicknesses [mm]
C       TM         I  upper boundary condition of temperature profile
C       TQF        L  total and average water flux over STIME
C       TQS        L  total and average root uptake over STIME
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 FNDELT
C                 FNDSLP
C                 HEATEQ
C
C       CALLED FROM:
C
C       PROGRAMMER:  SANDY MAPLE
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C
      PARAMETER(MXNOD=300,MAXHOR=12,MAXSCT=11)
C
      LOGICAL EQLIBR
      INTEGER NDXN2H(NN)
      DIMENSION CSH(NN),DELZ(NN),THETA(NN),SOLTP2(MAXSCT,MAXHOR,3),
     +    QF(NN),T(NN),TL(NN),QS(NN),SOLTP1(MAXHOR,5),TQF(MXNOD),
     +    TQS(MXNOD)
C
C  ..SAVE TIME OF DAY AND TIME SINCE LAST CALL TIME COUNTERS
      SAVE EQLIBR,DTIME,STIME,TQS,TQF
      DATA EQLIBR /.TRUE./,DTIME /0.0D0/,STIME /0.0D0/,TQS /MXNOD*0.0D0
     +    /,TQF /MXNOD*0.0D0/
C
C  ..RESET SLOPE OF TEMPERATURE PROFILE AND TIME STEP FOR
C    HEAT EQUATION AT BEGINNING OF DAY
      IF(DTIME.LE.0.0D0) THEN
        SLOPE=FNDSLP(TM,T,DELZ,NN)
        TDELT=FNDELT(SLOPE)
      ENDIF
C     
C     ..INCREMENT TIME OF DAY AND SINCE LAST CALL TIME COUNTERS
      DTIME=DTIME+DELT
      STIME=STIME+DELT
C     WRITE(32,*) 'DTIME: ',DTIME
C     WRITE(32,*) 'DELT: ',DELT
C     WRITE(32,*) 'STIME: ',STIME
C     
C     ..ACCUMULATE QF AND QS VALUES
      DO 10 IGOR=1,NN
        TQF(IGOR)=TQF(IGOR)+QF(IGOR)*DELT
        TQS(IGOR)=TQS(IGOR)+QS(IGOR)*DELT
   10 CONTINUE
C     
C     ..CHECK FOR HEAT EQUATION RUN CONDITION
   20 IF(EQLIBR.OR.STIME.GE.TDELT.OR.(DTIME.GE.24.0.AND.STIME.GT.0.0)) 
     +    THEN
C       WRITE(32,*) 'HQ TDELT: ',TDELT
C       
C       ..FIND AVERAGE QF AND QS FOR TIME SINCE LAST CALL
        DO 30 IGOR=1,NN
          TQF(IGOR)=TQF(IGOR)/STIME
          TQS(IGOR)=TQS(IGOR)/STIME
   30   CONTINUE
C       
C       ..ENTER HEAT EQUATION ROUTINES FOR SINGLE TIME STEP
        CALL HEATEQ(CSH,SOLTP2,T,TDELT,DELZ,NDXN2H,NN,ATMP,TQF,TQS,
     +      THETA,TL,SOLTP1,TM,EQLIBR,TBOTM)
C       
C       ..RESET TIME SINCE LAST CALL AND ACCUMULATIONS OF QF AND QS
        STIME=STIME-TDELT
        DO 40 IGOR=1,NN
          TQF(IGOR)=QF(IGOR)*STIME
          TQS(IGOR)=QS(IGOR)*STIME
   40   CONTINUE
C       
C       ..FIND MAXIMUM SLOPE OF TEMPERATURE PROFILE
        SLOPE=FNDSLP(TM,T,DELZ,NN)
C       
C       ..FIND NEW TIME STEP FOR HEAT EQUATION
        TDELT=FNDELT(SLOPE)
        GOTO 20
      ENDIF
C     
C     ..CHECK FOR END OF DAY RESET
      IF(DTIME.GE.24.0) DTIME=0.0D0
C     
C     ..RETURN TO CALLING ROUTINES
      RETURN
      END
C
      SUBROUTINE HEATCO(TRICO,THK,CS,T,TL,DELT,QF,QSS,DELZ,NN,GT,ITBC)
C
C======================================================================
C
C       PURPOSE: CALCULATE TRIDIAGONAL COEFFICIENTS FOR SOLUTION OF
C              CONSERVATIVE FORM OF ADVECTIVE-DIFFUSIVE HEAT EQUATION
C              WITH FIRST ORDER SINK/SOURCE TERM. TEMP, NORMAL TEMP
C              GRADIENT, OR HEAT FLUX B.C.'S ALLOWED.
C
C       REF: PATANKAR, NUMERICAL HEAT TRANSFER & FLUID FLOW,
C          HEMISPHERE PUBL., 1980;
C          ALONSO, C.V., SWAM POND MODEL, USDA-ARS, FT COLLINS, CO.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       TRICO  O  (NN X 4) M: TRIDIAGON SYSTEM COEFFICIENTS
C                  COL 1: V: SUBDIAGONAL COEFFICIENTS
C                  COL 2:    " DIAGONAL      "
C                  COL 3:    " SUPERDIAGONAL    "
C                  COL 4: R.H.S. VECTOR
C       CS         I  V: NODAL VOLUMETRIC HEAT CAPACITIES [J/MM^3/C]
C       CW            VOLUMETRIC HEAT CAPACITY OF WATER [J/MM^3/C]
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DELZ   I  NODAL ELEVATION FORWARD DIFFERENCES [MM]
C       GPECM     TRAILING INTERFACE GRID PECLET NUMBER
C       GPECP     LEADING  INTERFACE GRID PECLET NUMBER
C       GT         I  2-LONG VECTOR OF B.C. VALUES (UPPER & LOWER)
C       ITBC   I  2-LONG I*4 VECTOR OF B.C. TYPES (1-TEMP,
C                  2-NORM GRAD, OR 3-FLUX; EL 1 - @ Z=ZMIN,
C                  EL 2 - @ Z=ZMAX)
C       NN         I  NO. SOIL NODES
C       PECM      TRAILING INTERFACE PECLET NUMBER
C       PECP      LEADING  INTERFACE PECLET NUMBER
C       QF         I  V: MOISTURE FLUX @ INTERFACES [MM/HR] SCALED BY
C                  RATIO (CW/CS(I)). ACTS LIKE ADVECTIVE VELOCITY
C                  IN TRANSPORT EQN.
C       QSS        I  V: NODAL PLANT WATER UPTAKE SCALED BY RATIO
C                  (CW/CS(I))  [MM^3/HR/MM^3]. ACTS LIKE FIRST
C                  ORDER DECAY FACTOR IN TRANSPORT EQN. (POS FOR
C                  DECAY (SINK); NEG FOR PRODUCTION (SOURCE)).
C       R         LAYER NUMERICAL VELOCITY [MM/HR]
C       T      I  V: NODAL TEMP'S @ PREV TIME STEP [C] SCALED BY
C                  RATIO (CS|J-1 / CS|J), J = CURRENT TIME STEP
C       THK        I  V: INTERFACE THERMAL CONDUCTIVITIES [J/MM/HR/C]
C       TL         I  V: NUMERICAL LAYER THICKNESSES [MM]
C
C       COMMENTS:
C        1) BND TEMP'S ASSIGNED/CALC'ED IN CALLING ROUTINE;
C        2) THIS ROUTINE SUITABLE FOR ANY CONSISTENT SET OF UNITS.
C        3) THIS ROUTINE CAN BE USED TO SOLVE THE A-D TRANSPORT EQN WITH
C         1-ST ORDER DECAY. THEN APPLY FOLLOWING ANALOGIES:
C           CS    - RETARDATION FACTOR
C           THK - DIFFUSIVITY/DISPERSIVITY
C           QSS - 1-ST ORDER DECAY FACTOR
C           QF    - ADVECTIVE VELOCITY
C           T - CONCENTRATION @ PREV TIME STEP, SCALED BY RATIO
C               (CS|J-1 / CS|J), CS ==> RETARDATION. SEE ABOVE.
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER ITBC(2,2)
      DIMENSION TRICO(NN,4),T(NN),THK(NN),CS(NN),TL(NN),QF(NN),QSS(NN),
     +    DELZ(NN),GT(2)
C
C ... STATEMENT FUNCTION FOR FUNCTION P/(EXP(P)-1)
      EXPTRM(P)=MAX(0.0D0,(1.0D0-0.1D0*P)**5.0D0)
C     
C-----------------------------------------------------------------------
C     
C     ... CALCULATE COEFFICIENTS FOR INTERIOR NODES
C     
      DO 10 I=2,NN-1
C       
C       ... INTERFACE THERMAL DIFFUSIVITIES
        DM=THK(I-1)/CS(I)
        DP=THK(I)/CS(I)
C       
C       ... NUMERICAL LAYER VELOCITY
        R=TL(I)/DELT
C       
C       ... INTERFACE "GRID" PECLET NUMBERS
        GPECM=R*DELZ(I-1)/DM
        GPECP=R*DELZ(I)/DP
C       
C       ... INTERFACE "FLOW" PECLET NUMBERS
        PECM=QF(I-1)*DELZ(I-1)/DM
        PECP=QF(I)*DELZ(I)/DP
C       
C       ... TRIDIAGONAL COEFFICIENTS
        IF(PECM.LT.10.0D0) THEN
          TRICO(I,1)=-QF(I-1)/R-EXPTRM(PECM)/GPECM
        ELSE
          TRICO(I,1)=-QF(I-1)/R
        ENDIF
        IF(PECP.LT.10.0D0) THEN
          TRICO(I,3)=-EXPTRM(PECP)/GPECP
        ELSE
          TRICO(I,3)=0.0D0
        ENDIF
        TRICO(I,2)=1.0D0-TRICO(I,1)-TRICO(I,3)+(QF(I)-QF(I-1))/R+
     +      QSS(I)*DELT
        TRICO(I,4)=T(I)
C     
   10 CONTINUE
C     
C     ... ADJUST NODE 1, NN-1 COEF'S FOR UPPER & LOWER B.C.'S
C     NOTE: BND TEMPS CORRESPONDING TO SPECIFIED B.C.'S ARE ASSIGNED/
C     CALCULATED IN CALLING ROUTINE
C     
C     ... B.C.'S @ UPPER BND  - ADJUST NODE 2 COEFF'S
      IF(ITBC(1,1).EQ.1) THEN
C       
C       ... DIRICHLET (TEMP) B.C.
        TRICO(2,4)=T(2)-TRICO(2,1)*GT(1)
C     
      ELSE
C       ... NEUMANN OR ROBBINS B.C.
        R=TL(1)/DELT
        DP=THK(1)/CS(1)
        GPECP=R*DELZ(1)/DP
        PECP=QF(1)*DELZ(1)/DP
C       
        TRICO(1,3)=-EXPTRM(PECP)/GPECP
C
        IF(ITBC(1,1).EQ.2) THEN
C         
C         ... TYPE 2 = NEUMANN (NORMAL GRADIENT) B.C.
C         ... NOTE: 2ND QF SHOULD ACTUALLY BE Q @ SURFACE NODE
          TRICO(1,2)=1.0D0-TRICO(1,3)+(QF(1)-QF(1))/R+QSS(1)*DELT
          TRICO(1,4)=T(1)-DP*GT(1)/R
C       
        ELSEIF(ITBC(1,1).EQ.3) THEN
C         
C         ... TYPE 3 = ROBBINS (FLUX) B.C.
          TRICO(1,2)=1.0D0-TRICO(1,3)+QF(1)/R+QSS(1)*DELT
          TRICO(1,4)=T(1)+GT(1)/R
        ENDIF
      ENDIF
C     
C     ... B.C.'S @ LOWER BND  - ADJUST NODE (NN-1) COEFF'S
      IF(ITBC(2,1).EQ.1) THEN
C       
C       ... DIRICHLET (TEMP) B.C.
        TRICO(NN-1,4)=T(NN-1)-TRICO(NN-1,3)*GT(2)
C     
      ELSE
C       
C       ... NEUMANN OR ROBBINS B.C.
        R=TL(NN)/DELT
        DM=THK(NN-1)/CS(NN)
        GPECM=R*DELZ(NN-1)/DM
        PECM=QF(NN-1)*DELZ(NN-1)/DM
        TRICO(NN,1)=-QF(NN-1)/R-EXPTRM(PECM)/GPECM
C       
        IF(ITBC(2,1).EQ.2) THEN
C         
C         ... NEUMANN TYPE 2 (NORMAL GRADIENT) B.C.
          TRICO(NN,2)=1.0D0-TRICO(NN,1)+(QF(NN)-QF(NN-1))/R+QSS(NN)*DELT
          TRICO(NN,4)=T(NN-1)+GT(2)*THK(NN)/CS(NN)
C       
        ELSEIF(ITBC(2,1).EQ.3) THEN
C         
C         ... TYPE 3 - ROBBINS (FLUX) B.C.
C         WRITE (*,1000)
C         STOP
          TRICO(NN,2)=1.0D0-TRICO(NN,1)-QF(NN-1)/R+QSS(NN)*DELT
          TRICO(NN,4)=T(NN)-GT(2)/R
        ENDIF
      ENDIF
C     
C     
      RETURN
C 1000 FORMAT(' !!!!!'/' FLUX B.C. @ NOT CURRENTLY SUPPORTED @ LOWER ',
C     +    'BND IN SOIL TEMP MODEL.'/' EXECUTION STOPS IN SUBR HEATCO.'/
C     +    '!!!!!')
      END
C
C
C
      SUBROUTINE HEATEQ(CSH,SOLTP2,T,DELT,DELZ,NDXN2H,NN,ATMP,QF,QS,
     +    THETA,TL,SOLTP1,TM,EQLIBR,TBOTM)
C
C======================================================================
C
C       PURPOSE: SOLVE HEAT EQUATION ITERATIVELY FOR SOIL TEMPERATURE
C              PROFILE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CSH        I  V: NODAL (WET) SOIL HEAT CAPACITIES [J/MM^3/C]
C       CONRUL        CONVERGENCE CRITERION ('ABS ','REL ')
C       DELT   I  INCREMENTAL TIME STEP
C       DELZ   I  V: NODAL ELEV FORWARD DIFFERENCES [MM]
C       EPS           RELATIVE ERROR CONVERGENCE CRITERION
C       SOLTP2     I  (MAXSCT X MAXHOR X 3) ARRAY OF SOIL HORIZON
C                 CONSTITUENT PROPERTIES, CONSTIT'S BY ROW,
C                 HORIZON BY COL, PROPERTY BY PLANE
C                  (;;1): CONSTITUENT VOLUME FRACTIONS
C                  (;;2): CONSTITUENT HEAT CONDUCT [J/MM/HR/C],
C                  (;;3):      PARTICLE SHAPE FACTORS GA
C       NDXN2H     I  V: INDICES RELATING SOIL HOR TO CONSTITUENT
C                 NUMERICAL LAYERS (E.G. LAYER 1 IS IN HORIZON
C                 NDXN2H(1)
C       MAXSCT     I  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       NN         I  NUMBER OF NODES
C       NHOR   I  NUMBER OF SOIL HORIZONS (TYPES)
C       ATMP   I  ATMOSPHERIC PRESSURE
C       QF         I  V: MOISTURE FLUX @ INTERFACES [MM/HR]
C       QS         I  V: PLANT WATER UPTAKE [MM^3/HR/MM^3]
C       T     I/O V: NODAL TEMPERATURES [C]
C       THETA  I  V: NODAL MOISTURE CONTENTS
C       TL         I  V: LAYER THICKNESSES [MM]
C       WRKNUM     I  (0:NN X 9) WORK ARRAY
C                  COL 1: SUBDIAG ELEM'S IN TRIDIAG SYSTEM
C                      2: DIAG     "
C                      3: SUPER    "
C                      4: R.H.S.   "
C                      5: WORKING SPACE FOR TRIDIAG SOL'N
C                      6:    "
C                      7: TEMPORARY NEW TEMPERATURE SOL'N [C]
C                      8: INTERFACIAL SOIL THERMAL CONDUCTIVITIES
C                       [J/MM/HR/C]
C                      9: SCALED PLAT WATER UPTAKES
C       SOLTP1     I  (MAXHOR X 5) ARRAY OF HEAT MODEL PARAMETERS,
C                 HORIZONS BY ROW, PARAMETERS BY COL
C                  COL 1: SAT MOISTURE CONTENT
C                  COL 2: FIELD CAPACITY
C                  COL 3: TEXTURE CLASS (1-COARSE, 2-MED, 3-FINE)
C                  COL 4: # CONSTITUENTS FOR THERMAL PROPERTY CALC
C                  COL 5: DRY VOL HEAT CAPACITY  [J/MM^3/C]
C       EQLIBR      L  .TRUE. IF HEATEQ IS CALLED TO EQUILIBRIATE THE
C                  ENTIRE PROFILE
C       COMMENTS:
C
C       MASS STORAGE FILES: NONE
C
C       EXTERNAL REFERENCES:
C          - CHKCON
C          - FACEC
C          - INTEMP
C          - TPROF
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FEXIST
C
C   ARRAY DIMENSION VALUES
      PARAMETER(MXNOD=300,MAXHOR=12,MAXSCT=11)
C
C=======================================================================
C
C     WORK SPACE ARRAYS
C
C-----------------------------------------------------------------------
C
      COMMON /WKSPAC/ WRKNUM(MXNOD,9)
C
C
      CHARACTER CONRUL*4
      LOGICAL CONVRG,EQLIBR
      INTEGER NN,NDXN2H(NN),ITBC(2,2),MAXIT
      PARAMETER(CW=4.184D-3,CONRUL='REL ',EPS=1.0D-4)
      DIMENSION CSH(NN),DELZ(NN),THETA(NN),SOLTP2(MAXSCT,MAXHOR,3),
     +    QF(NN),T(NN),TL(NN),GT(2),QS(NN),SOLTP1(MAXHOR,5),TQF(MXNOD),
     +    TQS(MXNOD)
      DIMENSION TTL(MXNOD),TDELZ(MXNOD)
C
C-----------------------------------------------------------------------
C
      TTLT=0.0D0
C     --- TEMP VARIATION FOR SURFACE B.C.
      DO 10 II=1,NN
        TTL(II)=TL(II)*10.0D0
        TDELZ(II)=DELZ(II)*10.0D0
        TTLT=TTLT+TL(II)
   10 CONTINUE
C     ... CALCULATE INTERFACIAL COEFFICIENTS, SCALE MOISTURE FLUXES
C     ..    & PLANT WATER UPTAKES
   20 IF(EQLIBR) THEN
        DO 30 IQ=1,NN
          TQF(IQ)=0.0D0
          TQS(IQ)=0.0D0
   30   CONTINUE
        DO 40 I=1,NN
          JH=NDXN2H(I)
          CSH(I)=SOLTP1(JH,5)+CW*THETA(I)
   40   CONTINUE
        MAXIT=100
      ELSE
C       
C       ... INITIALIZE NEW TEMPERATURE SOLUTIONS
C       
        DO 50 II=1,NN
          TQF(II)=QF(II)*10.0D0
          TQS(II)=QS(II)*10.0D0
   50   CONTINUE
        MAXIT=1
      ENDIF
c from SHAW
C FIND BOTTOM TEMPEATURE FOR USING THE DIRECLET CONDITION
cc	  INQUIRE (FILE = 'LOWERB.DAT', EXIST = FEXIST)
cc        IF (.NOT.FEXIST) THEN
cc      DAMPNG=SQRT(2.D0*WRKNUM(NN,8)/CSH(NN)/1.99238D-07)
cc      AA=(-0.00082+0.00983957*DAMPNG/(TL(NN)))
cc     >    *(TTLT/DAMPNG)**(-0.381266)
cc      IF (AA .LT. 0.0) AA=0.0
C     ADJUST FOR ACTUAL TIME STEP
cc      AA=AA*DELT/86400.d0
cc      TBOTM=(1.-AA)*TBOTM + AA*T(NN-1)
cc      ENDIF
C      PRINT*, TBOTM1
c from SHAW
c
      CALL TBC(GT,ITBC,NN,TM,TBOTM)
C     
      CALL VMOVE(T,WRKNUM(1,7),NN)
C     
      CALL FACEC(SOLTP1(1,5),CSH,NDXN2H,NN,TQF,TQS,WRKNUM(1,9),T,THETA,
     +    MAXHOR)
C     
C     ... ITERATE FOR SOLUTION
C     
      DO 60 I=1,MAXIT
C
        CALL TPROF(WRKNUM,CSH,SOLTP2(1,1,2),DELT,TDELZ,SOLTP2(1,1,3),
     +      NDXN2H,MAXSCT,NN,SOLTP1(1,4),ATMP,TQF,WRKNUM(1,9),
     +      SOLTP1(1,3),T,THETA,WRKNUM(1,8),TTL,WRKNUM(1,7),
     +      SOLTP2(1,1,1),SOLTP1(1,2),SOLTP1(1,1),GT,ITBC,MAXHOR)
C
C       ... CHECK FOR SOLUTION
        CALL CHKCON(CONRUL,CONVRG,EPS,NN,T,WRKNUM(1,7))
C
        IF(CONVRG) GOTO 70
   60 CONTINUE
C     
C     ... SAVE NEW TEMPERATURE PROFILE
C40   WRITE (9,*) 'ITERATIONS: ',I - 1
   70 IF(EQLIBR) THEN
        EQLIBR=.FALSE.
        GOTO 20
      ENDIF
C
      CALL VMOVE(WRKNUM(1,7),T,NN)
C
      RETURN
      END
C
C
C
      SUBROUTINE TBC(GT,ITBC,NN,TM,TBOTM)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE SETS UP THE BOUNDRY CONDITIONS FOR THE
C              TEMPERATURE MODEL.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       GT        I/O 2-LONG VECTOR OF B.C. VALUES (UPPER & LOWER)
C       ITBC  I/O 2-LONG I*4 VECTOR OF B.C. TYPES (1-TEMP,
C                 2-NORM GRAD, OR 3-FLUX; EL 1 - @ Z=ZMIN,
C                 EL 2 - @ Z=ZMAX)
C       NN         I  NUMBER OF NODES
C       TM         L
C       TMAX   I
C       TMIN   I
C
C
C       COMMENTS:  IN THIS FORM ONLY THE (TEMP) BC IS USED, AND ALSO
C              CHANGES NEED TO BE MADE TO ALLOW FOR SHORTENING THE
C              NUMBER OF COMPUTATIONAL LAYERS.
C
C
C       MASS STORAGE FILES:
C
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C
C       CALLED FROM:
C              HEATEQ
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION: 1.0
C
C======================================================================
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION GT(2),ITBC(2,2)
      logical FEXIST
C
C
C    ..SET SURFACE BOUNDRY CONDITION
      ITBC(1,1)=1
      ITBC(1,2)=0
      GT(1)=TM
C     
	  INQUIRE (FILE = 'LOWERB.DAT', EXIST = FEXIST)
        IF (FEXIST) THEN
C     ..SET LOWER BOUNDRY CONDITION from given measured soil temperature
      ITBC(2,1)=1
      ITBC(2,2)=NN
      GT(2)= TBOTM
      else
C     ..SET LOWER BOUNDRY CONDITION based on Neumann type II boundary
      ITBC(2,1)=2
      ITBC(2,2)=NN
      GT(2)=0.0D0     
      endif
C     
C     
      RETURN
      END
C
      SUBROUTINE THCOND(CLAM,GA,IDXHOR,MAXSCT,NN,XNSC,PATM,STEXT,T,THK,
     +    TL,XF,XFC,XSAT,XW,MAXHOR)
C
C======================================================================
C
C       PURPOSE: CALCULATE INTERFACE THERMAL DIFFUSIVITIES
C
C       REF: PATANKAR, S.V. NUMERICAL HEAT TRANSFER AND FLUID FLOW,
C          HEMISPHERE, NEW YORK, PP 44-45, 1980.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CLAM   I  M: SOIL CONSTITUENT THERMAL CONDUCTIVITIES,
C                 CONSTITUENTS BY ROW, HORIZONS BY COL
C       GA         I  M: TYPICAL SOIL CONSTIT PARTICLE SHAPE FACTOR,
C                 CONSTITUENTS BY ROW, HORIZONS BY COL
C       IDXHOR     I  V: INDICES RELATING SOIL HORIZONS TO
C                 CONSTITUENT NUMERICAL LAYERS (E.G. LAYER I IS
C                 IN HORIZON IDXHOR(I))
C       JH            SOIL HORIZON INDEX
C       MAXSCT     I  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       NN         I  NUMBER OF NODES
C       XNSC   I  V: NO. OF MINERAL/ORGANIC CONSTITUENTS
C                    (AS DOUBLE PRECISION)
C                 IN SOIL HORIZONS
C       PATM   I  ATM PRESSURE CORRECTED FOR ELEVATION [KPA]
C       STEXT  I  V: SOIL HORIZON TEXTURES
C       T      I  V: NODAL TEMPERATURES
C       THK        O  V: LAYER INTERFACE THERMAL CONDUCTIVITIES
C       TL         I  V: LAYER THICKNESSES
C       XF         I  M: SOIL HORIZON CONSTITUENT BULK VOLUME
C                 FRACTIONS (HORIZONS BY COLUMN)
C       XFC        I  V: SOIL HORIZON FIELD CAPACITIES
C       XSAT   I  V: SOIL HORIZON SATURATION MOISTURE CONTENTS
C       XW         I  V: NODAL MOISTURE CONTENTS
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES: TPROF
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IDXHOR(NN)
      DIMENSION CLAM(MAXSCT,MAXHOR),GA(MAXSCT,MAXHOR),T(NN),THK(NN),
     +    TL(NN),XF(MAXSCT,MAXHOR),XSAT(MAXHOR),XFC(MAXHOR),XW(NN),
     +    XNSC(MAXHOR),STEXT(MAXHOR)
      CHARACTER CTEXT*4,CTEXTV(3)*4
      DATA CTEXTV /'CORS','MED ','FINE'/
C
C-----------------------------------------------------------------------
C
C ... CALCULATE NODE 1 THERMAL CONDUCTIVITY  THK(1)
C
      JH=IDXHOR(1)
      NSC=NINT(XNSC(JH))
C      CTEXT=CTEXTV(NINT(STEXT(JH)))
      THK(1)=THERMK(GA(1,JH),STEXT(JH),T(1),NSC,XF(1,JH),CLAM(1,JH),
     +    XW(1),XSAT(JH),XFC(JH),PATM)
C     
C     ... TEMPORARY ADJUSTMENT TO LAYER 1 THICKNESS
      TL(1)=TL(1)+TL(1)
C     
C     ... CALCULATE NODAL CONDUCTIVITIES
C     
      DO 10 I=2,NN
        JH=IDXHOR(I)
        NSC=NINT(XNSC(JH))
C        CTEXT=CTEXTV(NINT(STEXT(JH)))
C       
C       ... CALCULATE NODE(I) (POINT) THERMAL CONDUCTIVITIES
C       
        THK(I)=THERMK(GA(1,JH),STEXT(JH),T(I),NSC,XF(1,JH),CLAM(1,JH),
     +      XW(I),XSAT(JH),XFC(JH),PATM)
C       
C       .. ESTIMATE CONDUCTIVITY @ INTERFACE BET NODES (I-1),I:
C       .. AS IN PATANKAR PP 44-45
C       
        THK(I-1)=THK(I-1)*THK(I)*(TL(I-1)+TL(I))/(THK(I)*TL(I-1)+
     +      THK(I-1)*TL(I))
C     
   10 CONTINUE
C     
C     ... RESTORE LAYER 0 THICKNESS
      TL(1)=TL(1)/2.0D0
C     
      RETURN
      END
C
C
C
C
      FUNCTION THERMK(GA,STEXT1,T,NSC,XF,LAM,XW,XSAT,XFC,PATM)
C
C======================================================================
C
C       PURPOSE: CALCULATE SOIL THERMAL CONDUCTIVITY
C
C       REF: DE VRIES, D.A., THERMAL PROPERTIES OF SOILS, IN PHYSICS
C          OF PLANT ENV (VAN WIJK, ED.), NORTH HOLLAND, 1963.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       D         DIFFUSIVITY OF WATER VAPOR IN AIR
C       DD            SLOPE OF VAPOR PRESSURE-TEMP CURVE [KPA/C]
C       GA         I  V: SHAPE FACTORS FOR SOIL CONSTITUENTS
C       KI            TEMP GRADIENT RATIO OF I-TH CONSTITUENT
C       KPAIR     THERMAL CONDUCTIVITY OF PORE AIR [J/MM/HR/C]
C       LAM        I  V: THERMAL CONDUCTIVITIES OF MINERAL/
C                 ORGANIC SOIL CONSTITUENTS  [J/MM/HR/C]
C       LAM0      THERM CONDUCT OF CONTINUOUS MEDIUM [J/MM/HR/C]
C       LAMAIR           "    "   DRY AIR
C       LAMDRY           "    "   DRY MEDIUM
C       LAMM         "       "   MEDIUM
C       LAMV         "       "    PORE AIR
C       LAMVS        "       "   SATURATED PORE AIR
C       LAMW         "       "   WATER [J/MM/HR/C]
C       LE            LATENT HEAT OF VAPORIZATION [MJ/KG]
C       LPAIR     THERMAL CONDUCTIVITY
C       NSC           NUMBER MIN/ORG SOIL CONSTITUENTS (NOT INCLUDING
C                 WATER AND AIR)
C       PATM   I  ATM PRESSURE CORRECTED FOR ELEV [KPA]
C       PS            SAT VAPOR PRESSURE
C       RW            MOLECULAR GAS CONSANT FOR WATER VAPOR [J/G/C]
C       STEXT     SOIL TEXTURE ('C0RS', 'MED', 'FINE')
C       T      I  TEMPERATURE [C]
C       THERMK     O  THERMAL CONDUCTIVITY OF MOIST SOIL MEDIUM
C                 [J/MM/HR/C]
C       THORP  I  V: SOIL HORIZON PROPERTIES (IN ORDER XSAT, XFC,
C                  NSC (AS DOUBLE PRECISION), SOIL TEXTURE INDEX (DOUBLE P
C                  1.D0 - CORS, 2.D0 - FINE)
C       XW         I  MOISTURE CONTENT
C       X0            CONTINUOUS MEDIUM CONTENT
C       XA            AIR CONTENT
C       XF         I  VECTOR BULK VOLUMETRIC FRACTIONS OF MIN/ORG
C                 SOIL CONSTITUENTS
C       XFC           FIELD CAPACITY
C       XSAT      SATURATION MOISTURE CONTENT
C       XWL           LOWER MOISTURE CONTENT LIMIT FOR CONTINUOUS
C                 WATER MEDIUM
C
C       COMMENTS:
C          - THIS PROGRAM ASSUMES METRIC UNITS, CONDUCTIVITIES IN
C          [J/MM/HR/C], PRESSURE IN [KPA], ENERGY IN [J]. MOST
C          REFERENCES USE TRADITIONAL UNITS  OF [MCAL/CM/S/C].
C          CONVERSION FACTOR: .6636 X [J/MM/HR/C] = [MCAL/CM/S/C]
C          - ALL MINERAL SOIL CONSTIT'S SHOULD COME BEFORE THE ORGANIC
C          MATTER CONTITUENT IN THE CONSITUENT PROPERTY VECTORS
C          - DISPERSED WATER PARTICLES ARE ASSIGNED THE SHAPE FACTOR
C          OF THE FIRST SOIL CONSTITUENT
C
C       EXTERNAL REFERENCES:
C          - DELTA
C          - SATVP
C          - VAPDIF
C          - XLHVAP
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION: 1
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      CHARACTER STEXT*4
      INTEGER NSC
      DOUBLE PRECISION LAM,KI,KPAIR,LAMW,LAMAIR,LAMV,LAMVS,KWK,K0,LAM0,
     +    LAMM,RW,LE,LPAIR,LAMDRY,LAMXWL,STEXT1
      DIMENSION XF(NSC),LAM(NSC),GA(NSC)
      PARAMETER(K0=1.D0,C1=8.71962D-2,C2=2.593746D-4,LAMW=2.13886D0,RW=
     +    .4615D0)
C
C ... FUNCTION STATEMENT FOR TEMP GRADIENT RATIO
      FNK(RR,GG)=(2.0D0/(1.0D0+RR*GG)+1.0D0/(1.0D0+RR*(1.0D0-2.0D0*GG)))
     +    /3.0D0
C     
C     ... STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      SATVP(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))
C     
C     ... STATEMENT FUNCTION LATENT HEAT OF VAPORIZATION
      XLHVAP(T)=2.501D0-2.361D-3*T
C     
C     ... SLOPE OF THE SATURATION VAPOR CURVE (KPA/C)
      DELTA(T)=4098.0D0*SATVP(T)/(T+237.3D0)**2.0D0
C     
C     .. CALCULATE VAPOR DIFFUSIVITY
      VAPDIF(T,P)=(2340.5D0/P)*((T+273.0D0)/273.0D0)**2.3D0
C-----------------------------------------------------------------------
C     
C     CALCULATE THERMAL COND FOR FULLY SAT OR FULLY DRY SOIL
C     
C-----------------------------------------------------------------------
C     
C     ... DETERMINE LOWER MOISTURE THRESHOLD ON BASIS OF SOIL TEXTURE
       XWL=STEXT1
C      IF(STEXT.EQ.'FINE') THEN
C        XWL=0.09D0
C      ELSEIF(STEXT(1:3).EQ.'MED') THEN
C        XWL=0.06D0
C      ELSEIF(STEXT.EQ.'CORS') THEN
C        XWL=0.03D0
C      ENDIF
C     
C     ... CALCULATE DRY AIR THERMAL CONDUCTIVITY [J/MM/HR]
C     ... BASED ON TABULATED DATA IN BRUNT
      LAMAIR=C1+C2*T
	LAMM=LAMAIR
C     
C     .. SET X0 (CONTINUOUS MED) & XA VALUES
      X0=XSAT
      XA=XSAT-XW
C     
      IF(XW.EQ.XSAT.OR.XW.LT.XWL) THEN
C       
C       .. SET PARAMETERS OF CONTINUOUS MEDIUM
        IF(XW.EQ.XSAT) THEN
C         
C         .. CONTINUOUS WATER
          LAM0=LAMW
          C=1.0D0
C       
        ELSEIF(XW.LT.XWL) THEN
C         
C         .. CONTINUOUS AIR
          LAM0=LAMAIR
          C=1.25D0
        ENDIF
C       
C       .. CALCULATE EFFECTIVE MEDIUM CONDUCTIVITY
        LAMM=X0*K0*LAM0
        DENOM=X0*K0
C       
        DO 10 I=1,NSC
          R=LAM(I)/LAM0-1.D0
          KI=FNK(R,GA(I))
C         
          LAMM=LAMM+XF(I)*KI*LAM(I)
          DENOM=DENOM+XF(I)*KI
   10   CONTINUE
C       
        LAMM=C*(LAMM/DENOM)
        IF(XW.LT.XWL) LAMDRY=LAMM
      ENDIF
      THERMK=LAMM
C     
C     .. FINISHED FOR FULLY SATURATED OR FULLY DRY SOIL - RETURN
      IF(XW.EQ.XSAT.OR.XA.EQ.XSAT) RETURN
C     
C-----------------------------------------------------------------------
C     
C     CALCULATE THERMAL CONDUCTIVITY FOR MOIST SOIL
C     
C-----------------------------------------------------------------------
C     
      IF((0.D0.LT.XW).AND.(XW.LT.XSAT)) THEN
C       
C       .. FOR MOIST SOIL, CASE 1:  WATER IS CONTINUOUS BUT CALCULATIONS
C       .. ARE MODIFIED TO ACCOUNT FOR WATER VAPOR IN PORE AIR (ASSUMED
C       .. VAPOR SATURATED). AIR IS A DISPERSED SPECIES, INDEXED AFTER
C       .. SOIL MINERAL/ORGANIC CONSTITUENTS.
C       
C       .. SET PARAMETERS FOR CONTINUOUS WATER & DISPERSED AIR
        X0=XW
        LAM0=LAMW
C       
C       .. CALC TEMP GRADIENT RATIOS FOR DISPERSED MIN/ORG CONST.
        LAMM=X0*K0*LAM0
        DENOM=X0*K0
C       
        DO 20 I=1,NSC
          R=LAM(I)/LAM0-1.0D0
          KI=FNK(R,GA(I))
C         
          LAMM=LAMM+XF(I)*KI*LAM(I)
          DENOM=DENOM+XF(I)*KI
   20   CONTINUE
C       
C       .. CALC APPARENT THERMAL COND OF VAPOR-SAT PORE AIR
C       .. CALC COND DUE TO VAPOR
C       .. CONVERSIONS: 1.E-6 - M^2/MM^2; 3600 - SEC/HR;
C       ..          1.E3 - N/M^2 / KPA
        PS=SATVP(T)
        D=VAPDIF(T,PATM)*(1.D-6)*(3600.D0)
C       ..LATENT HEAT FOR VAPORIZATION [MJ/KG] CONVERT TO [J/G]
        LE=XLHVAP(T)*1.0D3
        DD=DELTA(T)*(1.0D3)
C       
C       .. CONVERSIONS: 1.E-3 - M/MM
        LAMVS=(LE*D*PATM*DD)/(RW*(T+273.D0)*(PATM-PS))
        LAMVS=LAMVS*(1.0D-3)
C       
C       .. APPARENT THERMAL COND
C       
        LPAIR=LAMAIR+LAMVS
C       
C       .. CALC TEMP GRADIENT RATIO OF PORE AIR
C       .. FOR WATER PARTICLES DISPERSED IN VAPOR-SAT PORE AIR,
C       .. WATER PART'S SAME SHAPE AS SOIL PART'S
C       
        R=LAMW/LPAIR-1.0D0
        KWK=FNK(R,GA(1))
C       
C       .. INVERT KWK FOR AIR DISPERSED IN WATER S.T. EFFECTIVE
C       .. MEDIUM COND IS SAME
C       
        KWK=1.0D0/KWK
C       
C       .. CALC EFFECTIVE AIR PART SHAPE FACTOR WHEN SOIL IS FULLY DRY
        R=LPAIR/LAMW-1.0D0
        B=(KWK*(R-1.D0)+1.D0)/(-2.0D0*KWK*R)
        C=(3.D0*KWK*(R+1.D0)-2.D0*R-3.D0)/(-6.D0*KWK*R*R)
        RADCAL=B*B-4.0D0*C
        IF(RADCAL.GE.0) THEN
          ROOT1=(-B-SQRT(RADCAL))/2.D0
          ROOT2=(-B+SQRT(RADCAL))/2.D0
          IF(ROOT1.GT.0.D0.AND.(2.D0*ROOT1).LT.1.D0) THEN
            GDRY1=ROOT1
          ELSEIF(ROOT2.GT.0.D00.AND.(2.D0*ROOT2).LT.1.D0) THEN
            GDRY1=ROOT2
          ENDIF
        ELSE
C         ..THE RESULT FOR QUADRATIC CALCULATION IS UNREASONABLE
C         ASSUME SATURATED SOIL
          GDRY1=.333D0
        ENDIF
C       .. INTERPOLATE FOR EFFECTIVE AIR PARTICLE SHAPE @ PREVAILING
C       .. MOIS CONTENT XW; SPHERICAL SHAPE (GA=.333) ASSUMED FOR AIR
C       .. WHEN SOIL IS SATURATED (XW=XSAT,XA=0.0)
C       
        GAEFF=.333D0-(XA/XSAT)*(.333D0-GDRY1)
C       
C       .. CALC TEMP GRADIENT RATIO FOR DISPERSED AIR PARTICLES
        R=LPAIR/LAM0-1.D0
        KPAIR=FNK(R,GAEFF)
C       
C-----------------------------------------------------------------------
C       
C       CALCULATE THERMAL COND FOR MOIST SOIL, 0 < XW < XFC
C       
C-----------------------------------------------------------------------
C       
C       .. MOIST SOIL, CASE 2: SET UP CALC'S FOR 0 < XW < XFC.  WATER
C       .. IS CONTINUOUS MEDIUM BUT PORE AIR IS NOT VAPOR-SATURATED.
C       
        IF(0.D0.LT.XW.AND.XW.LT.XFC) THEN
C         
C         .. CALC APPARENT COND OF PORE AIR
          IF(XW.LT.XWL) THEN
            XX=XWL
          ELSE
            XX=XW
          ENDIF
          X0=XX
C         
          LAMV=(XX/XFC)*(LPAIR-LAMAIR)
          LPAIR=LAMAIR+LAMV
C         
C         .. CALC TEMP GRADIENT RATIO FOR WATER PART'S (SHAPED AS SOIL
C         .. PART'S) DISPERSED IN DRY AIR
          R=LAMW/LAMAIR-1.D0
          KWK=FNK(R,GA(1))
C         
C         .. INVERT FOR K FOR AIR DISPERSED IN WATER, S.T. MEDIUM COND
C         .. IS SAME
          KWK=1.D0/KWK
C         
C         .. CALC AIR PART SHAPE FOR ???
          R=LAMAIR/LAMW-1.D0
          B=(KWK*(R-1.D0)+1.D0)/(-2.D0*KWK*R)
          C=(3.D0*KWK*(R+1.D0)-2.D0*R-3.D0)/(-6.D0*KWK*R*R)
          RADCAL=B*B-4.D0*C
          IF(RADCAL.GE.0) THEN
            ROOT1=(-B-SQRT(RADCAL))/2.D0
            ROOT2=(-B+SQRT(RADCAL))/2.D0
            IF(ROOT1.GT.0.D0.AND.(2.D0*ROOT1).LT.1.D0) THEN
              GDRY2=ROOT1
            ELSEIF(ROOT2.GT.0.D0.AND.(2.D0*ROOT2).LT.1.D0) THEN
              GDRY2=ROOT2
            ENDIF
          ELSE
C           .. RESULT FOR QUADRATIC SOLUTION IS UNREASONABLE ASSUME THEN
C           GDRY1 = GRY2
            GDRY2=GDRY1
          ENDIF
C         
C         .. INTERPOLATE FOR AIR PARTICLE SHAPE
          GAEFF=GDRY1+(XX/XFC)*(GDRY1-GDRY2)
C         
C         .. CALC TEMP GRADIENT RATIO
          R=LPAIR/LAM0-1.D0
          KPAIR=FNK(R,GAEFF)
C       
        ENDIF
C       
C-----------------------------------------------------------------------
C       
C       CALCULATE EFFECTIVE MEDIUM COND FOR MOIST SOIL, XWL < XW < XSAT
C       
C-----------------------------------------------------------------------
C       
        LAMM=LAMM+XA*LPAIR*KPAIR
        DENOM=DENOM+XA*KPAIR
C       
        LAMM=LAMM/DENOM
C       
C-----------------------------------------------------------------------
C       
C       CALCULATE EFFECTIVE MEDIUM COND FOR NEARLY DRY SOIL, 0 < XW < XWL
C       
C-----------------------------------------------------------------------
C       
C       .. MOIST SOIL, CASE 3: SOIL NEARLY DRY, WATER NOT CONTINUOUS;
C       .. INTERPOLATE BET DRY SOIL LAMM & LAMM @ XW = XWL
C       
        IF(XW.LT.XWL) THEN
          LAMXWL=LAMM
          LAMM=LAMDRY+(XW/XWL)*(LAMXWL-LAMDRY)
        ENDIF
C     
      ENDIF
      THERMK=LAMM
C     
C-----------------------------------------------------------------------
C     
      RETURN
      END
C
C
C
      SUBROUTINE TPROF(TRICO,CSH,CLAM,DELT,DELZ,GA,IDXHOR,MAXSCT,NN,
     +    XNSC,PATM,QF,QSS,STEXT,T,THETA,THK,TL,TNEW,XF,XFC,XSAT,GT,
     +    ITBC,MAXHOR)
C
C======================================================================
C
C       PURPOSE: SOLVE ADVECTIVE-DIFFUSIVE HEAT EQUATION FOR SOIL
C              TEMPERATURE PROFILE AT A SINGLE ITERATION
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CSH        I  SOIL LAYER VOLUMETRIC HEAT CAPACITIES [J/MM^3/C]
C       CLAM   I  ARRAY OF CONSTITUENT THERMAL CONDUCTIVITIES FOR
C                 SOIL HORIZONS (HORIZONS BY COLUMNS) [J/MM/HR/C]
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DELZ   I  V: NODAL ELEV FORWARD DIFFERENCES [MM]
C       EREL   I  ITERATIVE SOLN ERROR CONVERGENCE CRITERION
C       GA         I  A: TYPICAL PARTICLE SHAPE FACTOR, CONSTITUENTS
C                 BY ROW, HORIZON BY COL
C       GT         I  V: 2-LONG, UPPER & LOWER B.C.: TEMP [C], NORMAL
C                    GRADIENT [C/MM], OR HEAT FLUX [J/MM^2/HR]
C       IDXHOR     I  INDEX VECTOR RELATING SOIL HOR TO CONSTITUENT
C                 NUMERICAL LAYERS (E.G. LAYER 1 IS IN HORIZON
C                 IDXHOR(1)
C       IS         I  INDEX OF FIRST EQUATION IN COEF VECTORS
C       IE         I    "    LAST "     "     "
C       ITBC   I  V: 2-LONG, I*4 UPPER & LOWER B.C. TYPES:
C                    1-TEMP, 2-NORM GRAD, OR 3-FLUX; EL 1 @ ZMIN,
C                    EL 2 @ ZMAX
C       MAXSCT     I  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       NN         I  NUMBER OF NODES
C       XNSC   I  V: NUMBER OF CONSTITUENTS PER HORIZON
C                    (AS DOUBLE PRECISION)
C       PATM   I  ATMOSPHERIC PRESSURE [KPA]
C       QF         I  V: SCALED MOISTURE FLUX @ INTERFACES [MM/HR]
C       QSS        I  V: SCALED PLANT WATER UPTAKE [MM^3/HR/MM^3]
C                  (ACTS LIKE FIRST ORDER DECAY FACTOR ON TEMP)
C       STEXT  I  V: SOIL HOR TEXTURES (1.D0-COARSE, 2.D0-MEDIUM,
C                   3.D0-FINE)
C       T      I  V: NODAL TEMP SOLN FROM PREVIOUS ITERATION [C]
C       THETA  I  V: SOIL NODE MOISTURE CONTENTS
C       THK        I  V: INTERFACE THERMAL CONDUCTIVITIES [J/MM/HR/C]
C       TL         I  V: LAYER THICKNESSES [MM]
C       TNEW   O  V: NEW ITERATION NODAL TEMP SOLN [C]
C       TRICO  I  A: (NN X 6) WORK ARRAY FOR TRIDIAG SYSTEM SOLN
C                  COL 1: VECTOR OF SUBDIAGONAL COEFFICIENTS
C                  COL 2:    " DIAGONAL      "
C                  COL 3:    " SUPERDIAGONAL    "
C                  COL 4: R.H.S. VECTOR
C                  COL 5,6: INTERMEDIATE WORKING VECTORS
C       XF         I  A: MATRIX OF SOIL HORIZON CONSTITUENT BULK
C                    VOLUME FRACTIONS (HORIZONS BY COLUMN)
C       XFC        I  V: SOIL HORIZON FIELD CAPACITIES
C       XSAT   I  V: SOIL HORIZON SATURATION MOISTURE CONTENTS
C
C       COMMENTS:
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IDXHOR(NN),ITBC(2,2)
      DIMENSION CSH(NN),CLAM(MAXSCT,MAXHOR),DELZ(NN),GA(MAXSCT,MAXHOR),
     +    T(NN),THETA(NN),THK(NN),TL(NN),TNEW(NN),TRICO(NN,6),QF(NN),
     +    XF(MAXSCT,MAXHOR),XFC(MAXHOR),XSAT(MAXHOR),GT(2),QSS(NN),
     +    XNSC(MAXHOR),STEXT(MAXHOR)
C
C-----------------------------------------------------------------------
C
C ... CALCULATE INTERFACIAL THERMAL CONDUCTIVITIES
C
      CALL THCOND(CLAM,GA,IDXHOR,MAXSCT,NN,XNSC,PATM,STEXT,T,THK,TL,XF,
     +    XFC,XSAT,THETA,MAXHOR)
C     
C     ... CALCULATE HEAT EQUATION TRIDIAGONAL COEFFICIENTS
C     
      CALL HEATCO(TRICO,THK,CSH,T,TL,DELT,QF,QSS,DELZ,NN,GT,ITBC)
C     
C     ... SOLVE TRIDIAGONAL SYSTEM
C     
C     ... CHECK BOUNDARIES HERE TO REDUCE COMP BURDEN
C     
      IF(ITBC(1,1).EQ.1) THEN
        IS=2
      ELSE
        IS=1
      ENDIF
      IF(ITBC(2,1).EQ.1) THEN
        IE=NN-1
      ELSE
        IE=NN
      ENDIF
      CALL TRIDAG(TRICO,TNEW,IS,IE,1,NN)
C     
C-----------------------------------------------------------------------
C     
C     INSERT DIRICHLET (TEMPERATURE) BOUNDARY CONDITIONS
C     
C-----------------------------------------------------------------------
C     
C     ... SURFACE (NODE 1) BND
C     .. TEMPERATURE (DIRICHLET - TYPE 1) B.C.
      IF(ITBC(1,1).EQ.1) TNEW(1)=GT(1)
C     
C     ... LOWER (NODE NN) BND
C     .. TEMPERATURE (DIRICHLET - TYPE 1) B.C.
      IF(ITBC(2,1).EQ.1) TNEW(NN)=GT(2)
C     
C     ...
      RETURN
      END
C
C
C
C
      SUBROUTINE TRIDAG(TRICO,X,IS,IE,I0,IN)
C
C======================================================================
C
C       PURPOSE: SOLVE TRIDIAGONAL SYSTEM OF EQUATIONS BY TRIDIAGONAL
C              (THOMAS) ALGORITHM
C
C       REF: CARNAHAN ET AL, APPL NUM METH, PP 441-446, WILEY, NY, 1969.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       TRICO  I  (I0:IN , 6) MATRIX OF TRIDIAGONAL COEFF,
C                 R.H.S., & WORKING SPACE VECTORS
C                  COL 1: VECTOR OF SUBDIAGONAL COEFFICIENTS
C                  COL 2:   "    DIAGONAL     "
C                  COL 3:   "    SUPERDIAGONAL    "
C                  COL 4: R.H.S VECTOR
C                  COL 5,6: INTERMEDIATE WORKING VECTORS
C       X      O  SOLUTION VECTOR
C       IS         I  INDEX OF FIRST EQUATION IN COEF VECTORS
C       IE         I    "    LAST "     "      "
C       I0         I  STARTING ROW INDEX OF TRICO AS DIMENSIONED IN
C                 CALLING ROUTINE
C       IN         I  ENDING ROW INDEX OF TRICO AS DIMENSIONED IN
C                 CALLING ROUTINE
C
C       COMMENTS:
C          - THE EQUATIONS ARE NUMBERED FROM IS THRU IE;
C          - BEAR IN MIND FORTRAN MATRIX STORAGE: TRICO SHOULD BE
C          DIMENSIONED THE SAME WAY IN TRIDAG & THE ROUTINE WHERE
C          TRICO IS CALCULATED
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION: 1
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER I0,IS,IE,IN
      DIMENSION TRICO(I0:IN,6),X(I0:IN)
C
C-----------------------------------------------------------------------
C
C ... COMPUTE INTERMEDIATE VECTORS
C
      TRICO(IS,5)=TRICO(IS,2)
      TRICO(IS,6)=TRICO(IS,4)/TRICO(IS,5)
C     
      DO 10 I=IS+1,IE
        TRICO(I,5)=TRICO(I,2)-TRICO(I,1)*TRICO(I-1,3)/TRICO(I-1,5)
        TRICO(I,6)=(TRICO(I,4)-TRICO(I,1)*TRICO(I-1,6))/TRICO(I,5)
   10 CONTINUE
C     
C     ... COMPUTE FINAL SOLUTION VECTOR X - LOOP BACKWARDS
      X(IE)=TRICO(IE,6)
      DO 20 I=IE-1,IS,-1
        X(I)=TRICO(I,6)-TRICO(I,3)*X(I+1)/TRICO(I,5)
   20 CONTINUE
C     
C     ... FINISHED - RETURN
      RETURN
      END
C
C
C
      SUBROUTINE VMOVE(X,Y,NN)
C
C======================================================================
C
C       PURPOSE: MOVE CONTENTS OF VECTOR X INTO VECTOR Y
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       NN         I  NUMBER OF NODES
C       X      I  SOURCE VECTOR
C       Y      O  TARGET VECTOR
C
C       COMMENTS:
C
C       MASS STORAGE FILES: NONE
C
C       EXTERNAL REFERENCES: NONE
C
C       CALLED FROM:
C
C       PROGRAMMER: CHARLES S. HEBSON
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
C ... DECLARE VARIABLES
C
      INTEGER NN
      DOUBLE PRECISION X(NN),Y(NN)
      INTEGER I
C
C ... MOVE VECTOR CONTENTS, X ==> Y
C
      DO 10 I=1,NN
        Y(I)=X(I)
   10 CONTINUE
C     
      RETURN
      END
c rma\gf  substituted G. Flerchinger routine. This routine is now in SHAWHT.FOR
      SUBROUTINE SOILTK_SHAW(NS,TK,VLC,VIC,RHOB,sand,silt,clay,om)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE THERMAL CONDUCTIVITY OF THE SOIL
C     LAYERS USING DE VRIES'S METHOD.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CLAY       I    clay fraction in soil layer
c rma
C       GAair      P    DeVries' ellipsoid shape factor (ga) for air
C       GAC        P    DeVries' ellipsoid shape factor (ga) for clay
C       GAI        P    DeVries' ellipsoid shape factor (ga) for ice
C       GASI       P    DeVries' ellipsoid shape factor (ga) for silt
C       GAOM       P    DeVries' ellipsoid shape factor (ga) for org mtr
C       GASA       P    DeVries' ellipsoid shape factor (ga) for sand
C       I          L    canopy, snow, residue or soil layer
C       NS         I    num of soil layers
C       OM         I    organic matter fraction in soil layer
C       RHOB       I    bulk density of soil  (kg/m^3)
C       RHOM       P    density of the soil (particle density) kg/m^3
C       SAND       I    sand fraction in soil layer
C       SILT       I    silt fraction in soil layer
C       TK        I/O   ave thermal cond over the time step
C       TKA        P    thermal cond of air
C       TKI        P    thermal cond of ice
C       TKL        P    thermal cond of liquid
C       TKCL       P    thermal cond of clay fraction
C       TKSA       P    thermal cond of sand fraction
C       TKSI       P    thermal cond of silt fraction
C       TKOM       P    thermal cond of organic matter
C       VIC        I    vol ice content
C       VLC        I    vol liquid content
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
      PARAMETER (MXNOD=300)
      DOUBLE PRECISION TK(MXNOD),VLC(MXNOD),VIC(MXNOD),RHOB(MXNOD),
     +  SAND(MXNOD),SILT(MXNOD),CLAY(MXNOD),OM(MXNOD)
C
      PARAMETER (RHOM=2650.0D0,RHOA=1.25D0,RHOOM=1300.D0,
     +           TKL=0.57D0,TKI=2.2D0,TKA=0.025D0,
     +           TKSA=8.8d0,TKSI=2.92d0,TKCL=2.92d0,TKOM=0.25d0)
c rma
C     WEIGHTING FACTOR FOR ICE IS NOW SET TO 1.0 TO CORRECT PROBLEMS 
C     POINTED OUT BY KENNEDY & SHARRATT, SOIL SCI. 163(8):636-645.
c     PARAMETER (GAI=0.33D0,GAOM=0.5D0,GASA=0.144D0,GASI=0.144D0,
      PARAMETER (GAOM=0.5D0,GASA=0.144D0,GASI=0.144D0,
     +           GAC=0.125D0,gaair_lim=0.035d0)
C
C     DEFINE FUNCTION FOR CALCULATING DEVRIES WEIGHTING FACTORS
      DeVWF(TK0,TK1,GA) = 2.0/3.0/(1.0+(TK1/TK0-1.0)*GA)
     >                   + 1.0/3.0/(1.0+(TK1/TK0-1.0)*(1.0-2.0*GA))
C
      DATA IFIRST/ 0/
C
      IF (IFIRST .EQ. 0) THEN
C        FIRST TIME INTO SUBROUTINE -- CALCULATE WEIGHTING FACTORS FOR
C        SOIL COMPONENTS WHEN SOIL IS COMPLETELY DRY AND WHEN WET
         WFAIRD = 1.0
         WFSAD = DeVWF(TKA,TKSA,GASA)
         WFSID = DeVWF(TKA,TKSI,GASI)
         WFCLD = DeVWF(TKA,TKCL,GAC)
         WFOMD = DeVWF(TKA,TKOM,GAOM)
         WFICED = 1.0d0
C
C        SET THERMAL CONDUCTIVITY OF MOIST AIR EQUAL TO THAT OF DRY AIR
C        (VAPOR TRANSFER IS CALCULATED SEPARATELY AND INCLUDED IN HEAT
C        TRANSFER THROUGH SOIL; THEREFORE IT SHOULD NOT BE COMPENSATED
C        FOR IN COMPUTATION FOR THERMAL CONDUCTIVITY AS IS USUALLY DONE
C        WHEN USING DEVRIES METHOD.)
         TKMA = TKA
         WFL = 1.0
         WFSA = DeVWF(TKL,TKSA,GASA)
         WFSI = DeVWF(TKL,TKSI,GASI)
         WFCL = DeVWF(TKL,TKCL,GAC)
         WFOM = DeVWF(TKL,TKOM,GAOM)
         WFICE = 1.0d0
      END IF
C
      DO 10 I=1,NS
C       CORRECTION OF TEXTURE FOR ORGANIC MATTER
        XSM = 1.0 - OM(I)
        XSAND=XSM*SAND(I)
        XSILT=XSM*SILT(I)
        XCLAY=XSM*CLAY(I)
C
C       CONVERT WEIGHT FRACTION TO VOLUMETRIC FRACTION
        VSAND = XSAND*RHOB(I)/RHOM
        VSILT = XSILT*RHOB(I)/RHOM
        VCLAY = XCLAY*RHOB(I)/RHOM
        VOM = OM(I)*RHOB(I)/RHOOM
        PORO = 1.-VSAND-VSILT-VCLAY-VOM
        VAC = PORO-VIC(I)-VLC(I)
        IF (VAC .LT. 0.0) VAC=0.0
C
C       DETERMINE LIMIT OF DEVRIES METHOD FROM TEXTURE
C       SAND ==> 0.05;  LOAM ==> 0.10;  CLAY ==> 0.15
        VLMT = 0.10 + 0.2*CLAY(I) - 0.1*SAND(I)
        IF (VLMT .LT. 0.05) VLMT = 0.05
        IF (VLMT .GT. 0.15) VLMT = 0.15
C
        IF (VLC(I) .GT. VLMT) THEN
C         MOIST SOIL -- USE DEVRIES METHOD DIRECTLY
          GAAIR = 0.035+0.298*(VLC(I)-VLMT)/(PORO-VLMT)
          WFAIR= DeVWF(TKL,TKMA,GAAIR)
          TK(I)=(WFSA*VSAND*TKSA + WFL*VLC(I)*TKL + WFICE*VIC(I)*TKI
     >    + WFAIR*VAC*TKMA + WFSI*VSILT*TKSI + WFCL*VCLAY*TKCL
     >    + WFOM*VOM*TKOM)  /  (WFSA*VSAND + WFL*VLC(I) + WFICE*VIC(I)
     >    + WFAIR*VAC + WFSI*VSILT + WFCL*VCLAY + WFOM*VOM)
        ELSE
C         INTERPOLATE THERMAL CONDUCTIVITY BETWEEN WATER CONTENT AT 0
C         AND LIMIT OF DEVRIES METHOD
          TK0=1.25*(WFSAD*VSAND*TKSA + WFICED*VIC(I)*TKI 
     >    + WFAIRD*(VAC+VLC(I))*TKA
     >    + WFSID*VSILT*TKSI + WFCLD*VCLAY*TKCL + WFOMD*VOM*TKOM)
     >    /(WFSAD*VSAND + WFICED*VIC(I) + WFAIRD*(VAC+VLC(I))
     >    + WFSID*VSILT + WFCLD*VCLAY + WFOMD*VOM)
C
          WFAIR= DeVWF(TKL,TKMA,gaair_lim)
          TKLMT =(WFSA*VSAND*TKSA + WFL*VLMT*TKL + WFICE*VIC(I)*TKI
     >    + WFAIR*VAC*TKMA + WFSI*VSILT*TKSI + WFCL*VCLAY*TKCL
     >    + WFOM*VOM*TKOM) / (WFSA*VSAND + WFL*VLMT + WFICE*VIC(I)
     >    + WFAIR*VAC + WFSI*VSILT + WFCL*VCLAY + WFOM*VOM)
C
          TK(I) = TK0 + (TKLMT-TK0)*VLC(I)/VLMT
        ENDIF
   10 CONTINUE
      RETURN
      END
c
C original code, as modified by K  Johnsen
c      DOUBLE PRECISION TK(MXNOD),VLC(MXNOD),VIC(MXNOD),RHOB(MXNOD),
c     +       SAT(MXNOD)
c      PARAMETER (RHOM=2650.0D0,TKL=0.57D0,TKI=2.2D0,TKA=0.025D0,
c     +          RHOL=1.0D3,RHOI=9.2D2)
C
c      DO 10 I=1,NS
c       VMC=RHOB(I)/RHOM
cckej
c       VAC=sat(i)-VIC(I)*rhoi/rhol-VLC(I)
cc       VAC=1.0D0-VMC-VIC(I)-VLC(I)
c       IF (VAC .LT. 0.0D0) VAC=0.0D0
cC
cC      IF WATER CONTENT IS TOO LOW, DEVRIES METHOD IS NO LONGER VALID
c       IF (VLC(I) .LT. VLCLMT  .AND.  VIC(I) .LE. 0.0D0) THEN
cC         INTERPOLATE BETWEEN THE VALUE FOR TK AT THE WATER CONTENT
cC         LIMIT AND TK CALCULATED FOR DRY SOIL BASED ON CAMPBELL, 1985
c          TKLMT=(TKWM*VMC*TKM+TKWL*VLCLMT*TKL+TKWA*
c     +          (1.0D0-VMC-VLCLMT)*TKA)
c     +         /(TKWM*VMC+TKWL*VLCLMT+TKWA*(1.0D0-VMC-VLCLMT))
c          TKDRY=0.03D0 + 0.7D0*VMC*VMC
c          TK(I)=TKLMT - (VLCLMT-VLC(I))*(TKLMT-TKDRY)/VLCLMT
c       ELSE
cC      MOISTURE CONTENT IS HIGH ENOUGH SUCH THAT WATER CAN BE CONSIDERED
cC      TO BE THE CONTINUOUS MEDIUM
c       TK(I)=(TKWM*VMC*TKM+TKWL*VLC(I)*TKL+TKWI*VIC(I)*TKI+TKWA*VAC*TKA)
c     +      /(TKWM*VMC+TKWL*VLC(I)+TKWI*VIC(I)+TKWA*VAC)
c       END IF
c   10 CONTINUE
c      RETURN
c      END
C
