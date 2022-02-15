C $Id: Rzday.for,v 1.1 2002/08/28 00:00:48 rojas Exp $
      SUBROUTINE ADJDT(DAYTIM,DELT,TS0NXT,NN,Q,FIRST4,ihflag,THETAI
     +        ,ishaw)
C
C======================================================================
C
C       PURPOSE: ADJUST TIME INCREMENT FOR NUMERICAL SOLUTION OF
C            RICHARDS EQUATION DURING INTERSTORM PERIODS.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ASAVE  L
C       DAYTIM     I  TIME OF DAY [0..24 HRS]
C       DELT  I/O INCREMENTAL TIME STEP [HR]
C       DELTAT     L
C       DTMAX  L  MAXIMUM TIME INCREMENT IN DARCY (STARTING VALUE).
C             INPUT DATA [HR].
C       FIRST  I  TRUE IF FIRST TIME THROUGH ROUTINE
C       I    L    INDEX VARIABLE
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       PDTMAX     P
C       PDTMIN     P
C       Q    I    NODAL MOISTURE FLUXES [CM/HR]
C       TFIRST     L
C       TS0NXT     I  START TIME OF NEXT STORM THIS DAY [HR]
C       DAYTIM     I  TIME OF DAY [HR]
C       DELT   O  ADJUSTED TIME INCREMENT [HR]
C       TS0NXT     I  START TIME OF NEXT STORM THIS DAY [HR]
C
C       COMMENTS:
C
C       CALLED FROM: DAILY
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300)
      PARAMETER(PDTMIN=1.0D-4,PDTMAX=1.0D-1)
      LOGICAL FIRST4,TFIRST, IHFLAG     !LIWANG MA, RZ-SHAW
      DIMENSION Q(MXNOD),ASAVE(MXNOD),thetai(mxnod)
      SAVE ASAVE,TFIRST,DELTSV   !LIWANG MA, RZ-SHAW
C
      IF(FIRST4) THEN
        TFIRST=.TRUE.
        DELT=PDTMIN
C
      ELSEIF(TFIRST) THEN
        DO 10 I=1,NN
          ASAVE(I)=ABS(Q(I))*DELT*1.05D0
   10   CONTINUE
        DELT=PDTMIN
        TFIRST=.FALSE.
      ELSE
        DTMAX=1.0D-60
        DELTAT=1.0D-60
        DO 20 I=1,NN
          IF(Q(I).NE.0.D0) DELTAT=ASAVE(I)/ABS(Q(I))
          DTMAX=MAX(DTMAX,DELTAT)
          ASAVE(I)=ABS(Q(I))*DELT*1.05D0
   20   CONTINUE
        DELT=MIN(DTMAX,1.5D0*DELT)
        endif
        if (ishaw.eq.1) then
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf, LIWANG MA, RZ-SHAW
c     jak
c	 if (ihflag) delt = deltsv
C 
!        LIMIT TIME STEP IF THAWED LAYER OVERTOP FROZEN LAYERS:  GNF 4/18/06
         IFREEZE = 0
         DO I=NN,1,-1
            IF (THETAI(I) .gt. 0.0D0) THEN
!              FOUND FROZEN LAYER
               IF (IFREEZE .eq. 0) IFREEZE = 1
            ELSE
!              THAWED LAYER -- SET FLAG IF FROZEN LAYER EXISTS
               IF (IFREEZE .eq. 1) IFREEZE = 2
            END IF
         END DO
         IF (IFREEZE .eq. 2) DELT = MIN(0.10D0,DELT)
c      ENDIF
C
c      END IF
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     rma LIWANG MA, RZ-SHAW
c     jak
C     establish hourly execution time
c     ihflag to set/prepare for hourly output
c      ihflag = .false.
      rhour=daytim-aint(daytim)
      if (rhour+delt .gt. 1.0d0) then
c     gnf
          deltsv = delt
          delt=1.0d0-rhour
c          ihflag = .true.
      else
c          ihflag = .false.
      endif
      if (delt .gt. 0.9999d0 .or. (daytim+delt) .ge. 24.d0) then
c         ihflag = .true.
      endif
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      endif
C     ..LIMIT TIME STEP
      DELT=MIN(DELT,PDTMAX)
      DELT=MAX(DELT,PDTMIN)
      IF(DAYTIM+DELT.GT.TS0NXT) DELT=TS0NXT-DAYTIM
C
      RETURN
      END
C
      SUBROUTINE CHSPAN(DUR,MXSTEP,NSTEPS,SPAN,STORM,TS0,STMSEG,NS,ND)
C
C======================================================================
C
C       PURPOSE: CHECK IF A STORM EVENT SPANS 2 OR MORE DAYS. IF SO,
C            STORM DATA IS ADJUSTED SO THAT TIME INCREMENTS
C            DO NOT SPAN DAYS.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       DUR        I  STORM DURATION [HR]
C       I1         L  --SUBSCRIPT OF FIRST TABLE VALUE.
C       I2         L  --SUBSCRIPT OF SECOND TABLE VALUE.
C       IC         L
C       ISPAN  L  INDICATOR FOR SPANNING STORM
C       K    L    VON KARMAN CONSTANT ( =.41)
C       MXSTEP     I  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       ND        I/O
C       NS        I/O
C       NSTEPS     I  NUMBER TIME INCREMENTS (STEPS) IN STORM
C       SPAN  I/O LOGICAL: T - STORM SPANS 2 OR MORE DAYS;
C                  F - STORM CONTAINED WITHIN A DAY
C       STMSEG    I/O
C       STORM  I  COL 1: INCREMENTAL BREAKPOINT TIMES
C             ("DELTA T"'S) [HR]; INCREMENTAL BREAKPOINT
C             RAINFALL DEPTHS [CM]
C       TPROP  L
C       TS0        I  STORM STARTING CLOCK TIME [HR], 0<TS0<24
C       TSE        I  STORM CLOCK ENDING TIME [HR], 0<TSE<24
C       TT         L
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C             NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION STORM(MXSTEP,2),STMSEG(MXSTEP,2,2)
      LOGICAL SPAN
C
C ... CHECK FOR SPANNING DAYS
C
      ISPAN=INT((TS0+DUR)/24.0D0)
      IF(TS0+DUR.EQ.24.0D0) ISPAN=0
      NS=ISPAN+1
      IC=0
C
C     ... ADJUST STORM DATA SO TIME INCREMENTS DO NOT SPAN DAYS -
C     ...  WORK BACKWARDS.
C
      IF(ISPAN.GT.0) THEN
C
        SPAN=.TRUE.
        TT=TS0
        DO 40 K=1,NSTEPS
          TT=TT+STORM(K,1)
          IF(TT.GT.24.0D0) THEN
            DO 10 I1=1,K-1
              STMSEG(I1,1,NS-1)=STORM(I1,1)
              STMSEG(I1,2,NS-1)=STORM(I1,2)
   10       CONTINUE
C
            IF((TT-STORM(K,1))/24.0D0.EQ.1.0D0) THEN
C
C             ...HAVE AN EVEN BREAK IN TIME, EVERYTHING IS EASY NOW
              IC=0
              DO 20 I2=K,NSTEPS
                IC=IC+1
                STMSEG(IC,1,NS)=STORM(I2,1)
                STMSEG(IC,2,NS)=STORM(I2,2)
   20         CONTINUE
C
            ELSE
C
C             ...INTERPOLATE FOR UNEVEN BREAKS IN TIME
              STMSEG(1,1,NS)=TT-24.0D0
              STMSEG(K,1,NS-1)=STORM(K,1)-STMSEG(1,1,NS)
              TPROP=STMSEG(1,1,NS)/STORM(K,1)
              STMSEG(1,2,NS)=STORM(K,2)*TPROP
              STMSEG(K,2,NS-1)=STORM(K,2)*(1.0D0-TPROP)
C
              IC=1
              DO 30 I2=K+1,NSTEPS
                IC=IC+1
                STMSEG(IC,1,NS)=STORM(I2,1)
                STMSEG(IC,2,NS)=STORM(I2,2)
   30         CONTINUE
            ENDIF
            GOTO 60
          ENDIF
C
   40   CONTINUE
C
      ELSE
C
        SPAN=.FALSE.
        DO 50 I1=1,NSTEPS
          STMSEG(I1,1,NS)=STORM(I1,1)
          STMSEG(I1,2,NS)=STORM(I1,2)
   50   CONTINUE
C
      ENDIF
C
   60 CONTINUE
      NS=I1
      ND=IC
C
      RETURN
      END
      SUBROUTINE PHYSCL(CC,TMIN,TMAX,EPAN,RTS,U,ATMP,INP4,JDAY,PTRANS,
     +    LAI,SLKS,TUP,COPLNT,CORES,FT,FTR,RDF,TTRFD,IMAGIC,NSC,AIRR,TQ,
     +    IBRKTH,UPNIT,EWP,IRRTYP,MDAY,IPL,RH,IYYY,NOSTAR,XNU,ALPH,
     +    H2OTAB,RPOOL,ICNVG,BIGCNV,HEIGHT,CAPZONE,BD,FPW,INXPL,EFFLUX,
     +    IRTYPE,OMSEA,TLAI,SNOWPK,IRLOC,METMOD,IPR,SDEAD_HEIGHT,
     +    SDEAD,SDCN,HYDGRAD,ICHEM,jeday,tdew,npcpoint,nuse,
     +    IHOURLY,ISHAW,IPENFLUX,hrt,hrts,hru,hrh,fracom,clouds,
     +    iwzone,co2r,rth,hrth,wsi,plwidth,RSDIA,
     +    smelt_SHAW,TBOTM,Hmin,DAYRAIN,istress0,subirr,SSURFT,IPDEPTH,
     +    jbday,trat,ipet,hrzpar,CKCC,EMITR0,fracon,TLCSHAW,
     +    TLEAFU,TLEAFL,CS,CR,CCL)
      USE VARIABLE
C
C======================================================================
C
C       PURPOSE: SIMULATE PHYSICAL PROCESSES @ A SINGLE TIME STEP
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ACCPO4     L
C       ACTEVP     L
C       ACTTRN     L
C       AEF        I  FIELD SATURATION FRACTION [0..1]
C       AEVAP  L  POTENTIAL SOIL EVAPORATION (CM/HR)
C       AIRR  I/O AMOUNT OF IRRIGATION WATER TO APPLY
C       ASPECT     I  ASPECT ANGLE CLOCKWISE FROM NORTH [Radius]
C       ATMP   I  ATMOSPHERIC PRESSURE CORRECTED FOR ELEV [KPA]
C       ATRANS     L  ACTUAL TRANSPIRATION VALUE FOR EACH LAYER.[CM/DAY]
C       BASE   I  BASAL AREA OF A PLANT [M^2]
C       BKCHEM     L  FLUX OF SELECTED CHEMICALS AT BRK. THR. NODE
C             CONTAINS NO3-N, PEST#1-3 [UG/CM^2]
C       BRKH2O     L  FLUX OF WATER AT BRK. THR. NODE [CM/DAY]
C       CANIRR     L
C       CC         I  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C             CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C             1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C             5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C             9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C             12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CDNCI  I  WATER SEEPAGE OUT BOTTOM OF PROFILE [CM]
C       CII        L  INFILTRATION AMT. AT CURRENT TIME STEP [CM]
C       CLAT   I  LATERAL HYDROLIC CONDUCTIVITY [CM/HR]
C       CMPS   L  CUMM. WATER SEEP OUT BOTTOM OF PROFILE FROM
C             MACROPORES [CM]
C       CNVG   L
C       CONCX2     I  PEST. CONC. INVOLVED IN KINETIC PROCESSES [UG/G-S]
C       COPLNT     I  INITIAL AMT OF PESTICIDE ON THE FOLIAGE [UG/CM^2]
C       COR        L  CORRECTION FACTOR FOR ROUNDING ERROR
C       CORES  I  INIT. AMT. CHEM. AVAIL. ON RESIDUE FOR DISSIPATION
C       CSH        I  SOIL HEAT CAPACITIES (WET-DRY) [J/MM^3/C]
C       CUMCLM     I  CUM CHEMICAL FLOW IN MP VS. TIME [UG/CM^2]
C       CUMMF  I  CUM MACROPORE WATER FLOW [CM]
C       CUTTER     L
C       DAYTIM     L  TIME OF DAY [0..24 HRS]
C       DELOLD     L
C       DELSTO     L
C       DELT   L  INCREMENTAL TIME STEP [HR]
C       DELTA  L
C       DELZ   I  NODAL DEPTH FORWARD DIFFERENCES [CM]
C       DFLOW  I  WATER FLOW FROM DEADEND MACROPORES TO SOIL
C             SPACIALLY DISTRUB [CM]
C       DTCHEM     L  TOTAL AMOUNT OF CHEMICAL LOST TO SEEPAGE
C             CORRESPONDS TO CC ORDER [UG/CM^2]
C       DTEVAP     L
C       DTMASS     L
C       DTOLD  L
C       DTSEEP     L
C       DTSINK     L
C       DUR        L  STORM DURATION [HR]
C       ELEV   I  LOCATION ELEVATION [M]
C       EPAN   I  DAILY PAN EVAPORATION [CM]
C       EVAP   L  AVE EVAP RATE OVER PHOTOPERIOD [CM/HR] (<=0)
C       EWP       I/O
C       FERTIR    I/O AMT. OF FERTILIZER IN IRRIGATION WATER [MG/L]
C       FIRST  L  TRUE IF FIRST TIME THROUGH ROUTINE
C       FLOW   I  CUMCUL. WATER FLOW FROM CONT. MACROPORES TO SOIL
C             IN EACH LAYER. [CM]
C       FMP        L  CUMMULATIVE FLOW FROM MACROPORES [CM]
C       FT         I
C       FTR        I
C       H    I    NODAL SOIL WATER PRESSURE HEADS [CM]
C       HKBAR  I  LAYER INTERFACE HYDRAULIC COND [CM/HR]
C       HROOT  L
C       I    L    INDEX VARIABLE
C       IBRKTH     I
C       ICRUST    I/O FLAG FOR PRESENCE OF SURFACE CRUST [0=NO, 1=YES]
C       ICHEM  L  INDICATOR FLAG SIGNALLING IF EQULIBRIUM CHEMISTRY IS USED
C                 (0) - DO NOT USE CHEMISTRY MODEL, USE DEFAULT VALUES
C                 (1) - USE CHEMISTRY MODEL
C       IPHOS  L  INDICATOR FLAG SIGNALLING IF PHOSPHROUS IS USED
C                 (0) - DO NOT USE PHOSPHROUS MODEL, USE DEFAULT VALUES
C                 (1) - USE PHOSPHORUS MODEL
C       ID        I/O INDEX FOR DEPTH OF WETTING FRONT
C       IHOUR  L
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       INP4   I  POINTER TO BRKPNT.DAT DATAFILE
C       IP         L  INDEX FOR PESTICIDE NUMBER
C       IPL        I  PLANT CURRENTLY MODELING [1..MXSPEC]
C       IPSTM  P
C       IPSTO  P
C       IREBOT     I  BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       ISEG   L
C       ITBL   I  FLAG FOR PRESENCE OF WATER TABLE [0=NO, 1=YES]
C       ITYPE  L  INDICATOR FOR SOIL TYPE
C       IYYY   I  --YEAR (4 DIGITS).
C       J    L    INDEX VARIABLE
C       JDAY   I  JULIAN DAY   [1..366]
C       JJ         L
C       JSTDAY     L
C       K   I/O VON KARMAN CONSTANT ( =.41)
C       LAI        L  LEAF AREA INDEX (LIVE LEAVES ONLY)
C       TLAI   L  LEAF AREA INDEX (LIVE AND DEAD LEAVES)
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MDAY   I  TOTAL DAY FROM START OF SIMULATION
C       MINUT  L
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NBPR   L
C       ND         L
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C               IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NHOR   I  NUMBER OF SOIL HORIZONS
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NOSTAR     I
C       NS         L
C       NSC        I  CURRENT OUTPUT SCENARIO NUMBER
C       NSPAN  L
C       OBTNIT     L
C       OLDSTR     L
C       PCLAY  L  SEDIMENT FRACTION CLAY CONTENT [0..1]
C       PDEN   L
C       PES        L  POTENTIAL EVAP FROM SUBSTRATE [CM/DAY]
C       PESTIR    I/O AMT. OF PESTICIDES IN IRRIGATION WATER [MG/L]
C       PET        L  POTENTIAL TRANSPIRATION [CM/DAY]
C       PP         L  PHOTOPERIOD [HR]
C       PSAND  L  SEDIMENT SIZE FRACTION SAND PORTION [0..1]
C       PSILT  L  SEDIMENT SIZE FRACTION SILT PORTION [0..1]
C       PTRANS    I/O TOTAL POTENTIAL TRANSPIRATION (DIRECT + UNUSED
C             SOIL EVAPORATION).[CM/DAY]
C       PUP        L
C       PWRTS  I  NUMBER OF PLANT WITH ACTIVE ROOTS [#/HA]
C       QF         I  LAYER INTERFACIAL MOIST FLUXES [CM/HR]
C       QS         L  NODAL WATER UPTAKE BY PLANTS [CM/HR]
C       RDF        I  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       RFDD  I/O TOTAL RAINFALL DEPTH [CM]
C       RH         I  RELATIVE HUMIDITY [0..100]
C       BD         L  SOIL BULK DENSITY [G/CM^3]
C       RNDR   L  RANDOM ROUGHNESS [CM]
C       ROI        L  TOTAL SURFACE RUNOFF [CM]
C       RRATE  I
C       RTS        I  TOTAL S-W RADIATION INCOMING [MJ/M^2/DAY]
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       SLOPE  I  SLOPE OF FIELD [RAD]
C       SMELT  L  SNOWMELT FOR THE DAY (CM)
C       SNODON     L  FLAG FORCING ONE PASS THROUGH THE SNOMELT ROUTINE
C       SNOWPK    I/O PACK WATER EQUILIVALENT THAT IS PASSED IN FOR INITIAL
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C               (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C               (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C               (3):   EPS   - EXPONENT FOR K(H) CURVE
C               (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C               (5):   WR    - RESIDUAL WATER CONTENT
C               (6):   WS    - SATURATION WATER CONTENT
C               (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C               (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C               (9):   WWP   - WILTING POINT (15 BAR) WC
C               (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C               (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C               (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C               (13):  A1    - CONSTANT FOR O(H) CURVE
C       SOILPP     I
C       SOLTP1    I/O ARRAY OF HEAT MODEL PARAMETERS,
C              1: SAT MOISTURE CONTENT [0..1]
C              2: FIELD CAPACITY [0..1]
C              3: TEXTURE CLASS (1-COARSE, 2-MED, 3-FINE)
C              4: # CONSTITUENTS FOR THERMAL PROPERTY CALC
C              5: DRY VOL HEAT CAPACITY  [J/MM^3/C]
C       SOLTP2     I  (MAXSCT X MAXHOR X 3) ARRAY OF SOIL HORIZON
C             CONSTITUENT PROPERTIES, CONSTIT'S BY ROW,
C             HORIZON BY COL, PROPERTY BY PLANE
C             1: CONSTITUENT VOLUME FRACTIONS
C             2: CONSTITUENT HEAT CONDUCT [J/MM/HR/C],
C             3: PARTICLE SHAPE FACTORS GA
C       SPAN   L  LOGICAL: T - STORM SPANS 2 OR MORE DAYS;
C       SSEEP  L
C       START  L  FLAG FOR PHYSCL ROUTINE INITIALIZATION
C       SSTART     L  FLAG FOR SNOW ROUTINE INITIALIZATION
C       STMSEG     L
C       STORM  L  COL 1: INCREMENTAL BREAKPOINT TIMES
C             ("DELTA T"'S) [HR]; INCREMENTAL BREAKPOINT
C             RAINFALL DEPTHS [CM]
C       T    I    SOIL TEMPERATURE IN NUMERICAL CONFIGURATION [C]
C       TACTVE     L
C       TAIRR I/O NITROGEN IN IRRIGATION WATER
C       TARAIN    I/O NITROGEN IN RAIN WATER
C       TBDH   L
C       TCII   L
C       TDAY   L
C       TDPLNT     L
C       TEVAP  L
C       TFMASS     L
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TLRO  I/O LOSS OF NITROGEN DUE TO RUNOFF [KG-N/HA]
C       TLSEP I/O LOSS OF NITROGEN DUE TO SEEPAGE [KG-N/HA]
C       TLDRN I/O LOSS OF NITROGEN DUE TO TILE DRAINAGE [KG-N/HA]
C       TLLAT I/O LOSS OF NITROGEN DUE TO LATERAL FLOW [KG-N/HA]
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       TM         L  AVERAGE DAILY AIR TEMPERATURE [C]
C       TMASS  L
C       TMAX   I  MAXIMUM AIR TEMPERATURE [C]
C       TMIN   I  MINIMUM AIR TEMPERATURE [C]
C       TMIRR  L
C       TNITUP    I/O TOTAL NITROGEN UPTAKE FOR WHOLE PROFILE [G/PLANT]
C       TNUP   L
C       TOOTH  L    PROFILE WATER STORAGE BEFORE RAINFALL (CM)
C       TPASS  L
C       TQ        I/O
C       TRFDD  L  ACCUMULATED TOTAL RAINFALL DEPTH [CM]
C       TRNTIM     L
C       TROI   L
C       TRTS   L  TOTAL ROOTS FOR PROFILE (SUM OF RDF)
C       TS0        L  STORM STARTING CLOCK TIME [HR], 0<TS0<24
C       TSE        L  STORM CLOCK ENDING TIME [HR], 0<TSE<24
C       TSEEP  L
C       TSTART     L
C       TTCII  L
C       TTRFD  L  DAILY TOTAL RAINFALL DEPTH [CM]
C       TTMASS     L
C       TTRO   L
C       TTUP   L
C       TUP       I/O WATER UPTAKE / LAYER [CM].
C       IRRTYP     I  --TYPE OF IRRIGATION, IE. SPRINKLER, FLOOD,
C             FURROW, DRIP, or SUBSURFACE.
C       U    I    IONIC STRENGTH [MOLES/L]
C       UPNIT I/O UPTAKE OF NITROGEN FOR EACH LAYER [G/PLANT/LAYER]
C       UPPASS     L  PASSIVE UPTAKE OF NITROGEN [KG-N/HA]
C       UPACT  L  ACTIVE UPTAKE OF NITROGEN [KG-N/HA]
C       UPTOT  L  TOTAL NITROGEN UPTAKE [KG-N/HA]
C       UPDMD  L  TOTAL NITROGEN DEMAND BY PLANTS [KG-N/HA]
C       WRES   L  MOISTURE CONTENT OF RESIDUE [0..1]
C       WRKNUM     I  WORK ARRAY INTERMEDIATARY VALUES OF:
C               1: SUBDIAG ELEM'S IN TRIDIAG SYSTEM
C               2: DIAG   "
C               3: SUPER   "
C               4: R.H.S.   "
C               5: WORKING SPACE FOR TRIDIAG SOL'N
C               6:        "
C               7: TEMPORARY NEW TEMPERATURE SOL'N [C]
C               8: INTERFACIAL SOIL THERMAL COND [J/MM/HR/C]
C               9: SCALED PLAT WATER UPTAKES
C       XLAT   I  LATITUDE OF FIELD
C       XNU        I
C       ZN         I  DEPTH TO NUMERICAL NODES [CM]
C       ZRFDD  L
C
C       COMMENTS:
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C             ADJDT
C             CHSPAN
C             EVNTRO
C             HEATFX
C             MANOUT
C             MASSBL
C             POTEVP
C             RECON
C             REDIST
C             SGATE
C             SINK
C             SOILPR
C             STMINP
C             UPTAKE
C             VGATE
C             WCHEAD
C
C       CALLED FROM:
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION:   3.0
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MAXBP=50,MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,
     +    MXCHEM=15,MXPEST=3,MXSPEC=10,MXAPP=200,MXANA=135,MXTG=500)
      PARAMETER(MXTSTP=5000,MHR=24)
      PARAMETER(CW=4.184D-3,RHOL=1.0D3,CL=4.2D3,CM=9.0D2,RHOI=9.2D2)
C     Liwang Ma
      real ptransr,atransr,tltr(mxnod),qsr(mxnod),wtdep,trwup,
     +     RZrwu(MXNOD),RZtrwup,oldstress1,oldstress2,TRATIO,avg_hroot
      character crop*2
C     ..SHAW PARAMETERS
      PARAMETER (NODCAN=11, NODRES=10, NODSNO=100,MXSPNOD=MXSPEC*NODCAN)
      parameter (crr = 0.12d0,mxspnd=mxspec*mxnod)  !hmin=-35000.0d0,
C-GH Liwang MA
C      COMMON /CROPH2O/ ACTEVP,PET,ACTTRN,PER,PES,TOTPUP,PTRANSDAY
      COMMON /CROPH2O/ ACTEVP,PET,ACTTRN,PER,PES,TOTPUP,PTRANSDAY
     &    ,RET_day_T,RET_day_S,ipet1
C     &     ,TTRFD, SMELT,TTRO,DTSEEP,TTIRR
c      common/stress/istress !,oldstress1,oldstress2
      common /wuptake/qsr,wtdep,trwup,RZrwu,RZtrwup,istress,avg_hroot
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
c      dimension pup(mxnod)
C-GH Liwang MA
c     Liwang Ma
      
      common /Gleams_hydsol/gleams_clay,gleams_silt,gleams_por,   !chd  GLEAMS
     +        gleams_om1,gleams_sand,gleams_BulkDn,gleams_Dacre, 
     +        gleams_CHS,gleams_ei,gleams_exrain,thirtyRR,  
     +        IBYEAR,IEYEAR
      common/po4log/ErosionActive,UseEnrichment
      logical erosionactive,UseEnrichment
      common/gleams/rzrainfall,rzirrigation,rzrunoff
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
      COMMON /HEAT/ CSH(MXNOD),T(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3)
C
      COMMON /WKSPAC/ WRKNUM(MXNOD,9)
C
      COMMON /KNTCS/ CONCX2(MXNOD,MXPEST),EK2(MXNOD,MXPEST),
     +    RK2(MXPEST),RDIF(MXPEST),FEK2(MXPEST),IEK2(MXPEST)
C
      COMMON /IPINF/ CRUSTK,OCRUST,ICRUST,INFLPD,NHZ,NSLT,POND,RR,
     +    DHB(MAXHOR),NSL(MAXHOR),WI(Mxnod),DSL(MXNODT),VWC(MXNODT),
     +    IUGFLW
C
      COMMON /OPINF/ RO,SWF(MXnod),TR(MXTSTP),CI(MXTSTP),CRFD(MAXBP),
     +    CRFT(MAXBP),RFI(MAXBP),RFDD,RFD
C
      COMMON /OPCHEM/ THSMS(MXNOD,MXCHEM),CUPRO,CUPCHM(MXCHEM),CDNCI,
     +    CDNCHM(MXCHEM),CMESO(MXNODT,MXCHEM),CMICR(MXNODT,MXCHEM),
     +    CRO(MXTSTP,MXCHEM)
C
      COMMON /IRRIG/ DOIMAX(MXAPP),FDOI(MXAPP),VDOI(MXAPP,MXAPP),
     +    XKU(MXAPP,20),XMAD(MXAPP,20),RRATE(MXAPP),amxirr,
     +    totaliw,DOIMONTH(12,200),TOTALMONTH,airrdepth,amirr,
     +    RRDUR(MXAPP),IIPL(MXAPP),ISPT(MXAPP),ITOI(MXAPP),
     +    JDIRRB(MXAPP),JDIRRE(MXAPP),NAPPI,NDAIRR(MXAPP),NKU(MXAPP),
     +    NMAD(MXAPP),JDOI(MXAPP,MXAPP),JKU(MXAPP,20),JMAD(MXAPP,20),
     +    IDOI(MXAPP),id_depth,MONTHIRR
C
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
      DOUBLE PRECISION MCPOR,LV
      INTEGER YESMAC
      parameter (LV=2.5D6)
C
      COMMON /INFILC/ RAC(11),RRC(11),FERTIR(4),PESTIR(MXPEST),FMNRIR
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
      COMMON /NINFO/ TSOTN0,TSOTN,TOTA,TOTL,TAMIN,TANIT,TADRT(2),
     +    TARAIN(2),TAIRR(3),TAFERT(3),TARES(3),TLIMM,TLDEN,TLVOL,
     +    TLRO(3),TLSEP(3),TLDRN(3),TLLAT(3),TAMANR,TMANH4,tamanr_bal
     +    ,TLN2O,TLNXO,TLNITN2O,TLNITNXO,TLGHGADS,TLGHGADSden
C
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
C
C
      COMMON /THGIN/ THGDEP(MXTG),THGCUR,ITHG(MXTG),IWTHG(MXTG),
     +    ITHGOFF(MXTG,2),ITHGDUR(MXTG),NUMTHG,IDP
C
      COMMON /PINFO/ TSOTP0(MXPEST),TAPAP(MXPEST),TLPRS(MXPEST),
     +    TLPCN(MXPEST),TLPSL(MXPEST),TLPRO(MXPEST),TLPLC(MXPEST),
     +    TDPPLNT(MXPEST),TLPDT(MXPEST),TLPDTD(MXPEST),
     +    TLPDTL(MXPEST),TLPDMA(MXPEST)
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

      COMMON /RESID/ RM,RESAGE,CRES,HR,RCN,NADLY,NSDLY,NAREC,NSREC,WORM,
     +               SAI,HSTUBL,WSTUBL
C Liwang Ma
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
      common/sumet/sumAET,sumCropET,sumPET,sumPT,sumAT,sumrain,deltheta
C     ... LOCALLY DECLARED VARIABLES
      DIMENSION hrt(mhr),hrts(mhr),hru(mhr),hrh(mhr),HEPAN(MHR),
     +         HREVAP(MHR),HRPTRANS(MHR),hrth(mhr)
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
C     ... LOCALLY DECLARED VARIABLES
      PARAMETER (IPSTO = MAXBP*2,IPSTM = IPSTO*2, THRSNO = 0.0D0)
c	PARAMETER (RHOL=1.0D3,RHOI=9.2D2)
      PARAMETER (Wsolar=4.9212D0, PPCNST = 12.0D0/3.141592654D0)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      CHARACTER PERIOD*5,TIME*40,IFMT*70
      LOGICAL SPAN,START,FIRST5,CANIRR,CNVG,SNODON,SSTART,ihflag,pfirst
      DOUBLE PRECISION LAI,TLAI,store
      DOUBLE PRECISION METMOD(8,12)
      DIMENSION STORM(MAXBP,2),BD(MXNOD),RDF(mxnod),
     +    SLKS(MXNOD,MXPEST,2),qsdummy(mxnod),wsi(mxspec),
     +    CC(MXNOD,MXCHEM),PP(2),QS(MXNOD),STMSEG(MAXBP,2,2),TQ(MXNOD),
     +    OLDSTR(MXNOD),TUP(MXNOD),DTCHEM(MXCHEM),UPNIT(MXNOD),
     +    TTUP(MXNOD),COPLNT(MXPEST),CORES(MXPEST),TNUP(MXNOD),
     +    BKCHEM(MXCHEM),XNU(MXNOD,25),H2OTAB(2),DTDCHEM(MXCHEM),
     +    DTLCHEM(MXCHEM),RPOOL(MXNOD,2),TQFL(MXNOD),TQNO3(MXNOD),
     +    FPW(MXNOD),QT(MXNOD),UDRN(MXNOD),INXPL(MXSPEC),EFFLUX(MXSPEC),
     +    IRTYPE(MXSPEC),OMSEA(MXANA),IRRTYP(MXAPP),TNO3UP(MXNOD),
     +    PESTUP(MXNOD,MXPEST),ULAT(MXNOD),QTD(MXNOD),QTL(MXNOD),
     +    FRACOM(MXNOD),xcc(mxnod,mxchem),dchar(mxspec,nodcan),
     +    SCUPCH(MXCHEM),QSSI(mxnod),qs1(mxnod),FRACON(MXNOD)
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     rma/gnf
c     jak
C     ..SHAW VARIABLES DIMENSIONED

      DIMENSION ZC(NODCAN), ZSP(NODSNO), ZS(MXNOD),
     +     ZR(NODRES),  ICESDT(MXNOD),
     +     RHOSP(NODSNO), DZSP(NODSNO), TMID(MXNOD),
     +     PLANTZ(MXSPEC), PLANTW(MXSPEC),
     +     ITRN(MXSPEC), IEVAP(MXSPEC), RSTOM0(MXSPEC), GMCDT(NODRES),
     +        sand(mxnod),silt(mxnod),
     +     CLAY(MXNOD), SAT(MXNOD), CANALB(MXSPEC),
     +     QSL(MXNOD), RHOB(MXNOD), TRDT(NODRES), TSPDT(NODSNO),
     +     DLWDT(NODSNO), ICESPT(NODSNO), RHOR(NODRES),
     +        swres(nodres),swcan(mxspec,nodcan),
     +     swsnow(nodsno), evapr(nodres), VAPRDT(NODRES), 
     +     TCDT(NODCAN), TLCDT(MXSPEC,NODCAN), VAPCDT(NODCAN),
     +     WCANDT(NODCAN), TOTLAI(MXSPEC), ROOTDN(MXSPEC,MXNOD),
     +     TRANSP(MXSPEC), TPOT(MXSPEC), ROOTXT(MXNOD), 
     +     STRMLT(2),WINDC(NODCAN),
     +     wthour(mhr),xtract(mxnod),ssurft(mhr),
     +     XANGLE(MXSPEC),TCCRIT(MXSPEC),PLEAF0(MXSPEC),
     +     RSTEXP(MXSPEC),RLEAF0(MXSPEC),RROOT0(MXSPEC)


      DOUBLE PRECISION lwcan(mxspec,nodcan),matdt(mxnod),
     +     thetaold(mxnod),lwsnow,lwres(NODRES),lwsoil,thetaoldi(mxnod)
     +     ,dsnow,subl,totmelt
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C       SAVE PERIOD,SPAN,DELT,START,NBPR,COR,TTRO,STMSEG,ND,FIRST5,JSTDAY,
C      +    TSTART,OLDSTR,TRFDD,TCII,TROI,NBPRI,CANIRR,HROOT,RNDR,SNP,
C      +    SSTART,PKTEMP,hrootdummy
C       SAVE IDIMIYOLD,wthour,totalsub
      data pfirst/.true./
C
      DATA PERIOD /'STORM'/,SPAN /.FALSE./,ISEG /1/,COR /1.0D-6/,START /
     +    .TRUE./,DELT /1.0D-4/,FIRST5 /.TRUE./,CMPS /0.0D0/,TROI /0.0D0
     +    /,JSTDAY /0/,TRFDD /0.0D0/,TCII /0.0D0/,STORM /IPSTO*0.0D0/,
     +    NSPAN /0/,STMSEG /IPSTM*0.0D0/,CANIRR /.FALSE./,DTCHEM /MXCHEM
     +    *0.0D0/,TSTART /0.0D0/,TSE /0.0D0/,RNDR /0.0D0/,TMASS /0.0D0/,
     +    DUR /0.0D0/,NBPRI /0/,ID /1/,SNP /0.0D0/,SSTART /.TRUE./,
     +    PKTEMP /0.0D0/,xtract/mxnod*0.0d0/ !make only the first soil layer, OK?  Liwang Ma 
	 ! RM - ngfl already declared, rzrunoff must be declared later.
      CHARACTER CTEXT*4,CTEXTV(3)*4
      DATA CTEXTV /'CORS','MED ','FINE'/,SCUPCH /MXCHEM*0.0D0/
	  ! RM Moved the follow 3 lines up, used in the next data block.
c	data needed to calculate hrm,zr,etc. if ishutl.ne.1
      parameter(mxres=3)
      DOUBLE PRECISION RHORS(MXRES),RDIA(MXRES),deltice(mxnod),rcshr(24)
     &                ,rr30(30),rchhr(24),hrzpar(24)
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     rma
c     jak
C     ..SHAW DATA INITIALIZATIONS
c rma removed ishutl initialization, read in from rzwqm.dat
      DATA NC/0/,NSP/0/,NR/0/,ZC/NODCAN*0.0D0/,ZSP/NODSNO*0.0D0/,
     +     ZR/NODRES*0.0D0/, ICESDT/MXNOD*0.0D0/,
     +     RHOSP/NODSNO*0.0D0/, DZSP/NODSNO*0.0D0/, ZS/MXNOD*0.0D0/,
     +     PLANTZ/MXSPEC*0.0D0/, PLANTW/MXSPEC*0.0D0/,
     +     ITRN/MXSPEC*0/, IEVAP/MXSPEC*0/, RSTOM0/MXSPEC*0.0D0/,
     +     RAINSP/0.0D0/, NPLANT/0/,                !CLOUDS/0.0D0/,
     +     SPMELT/0.0D0/, UHGHT/1.93/, GMCDT/NODRES*0.0D0/,
     +     EVAPS/0.0D0/, TRDT/NODRES*0.0D0/, RHOR/NODRES*0.0D0/,
     +     TSPDT/NODSNO*0.0D0/, DLWDT/NODSNO*0.0D0/,
     +     ICESPT/NODSNO*0.0D0/, NGfl/4/, FROM/0.0D0/,
     +     VAPRDT/NODRES*0.0D0/,TCDT/NODCAN*0.0D0/,TLCDT/MXSPNOD*0.0D0/,
     +     VAPCDT/NODCAN*0.0D0/, WCANDT/NODCAN*0.0D0/,
     +     TOTLAI/MXSPEC*0.0D0/, ROOTDN/MXSPND*0.0D0/,
     +     TRANSP/MXSPEC*0.0D0/, ROOTXT/MXNOD*0.0D0/,
     +     STEMAI/0.0D0/, ZSTUBL/0.0D0/, STUBLW/0.0D0/,
     +     HRM/0.0D0/, NNSP/0/, TRAIN/0.0D0/,
     +     RDIA/1.0D0, 0.5D0, 0.25D0/,istart/0/,iend/0/,
     +     XANGLE/MXSPEC*0.0D0/,TCCRIT/MXSPEC*7.0D0/,PLEAF0/MXSPEC*-100/
     +    ,RSTEXP/MXSPEC*5.0D0/,RLEAF0/MXSPEC*9.0D-5/,
     +     RROOT0/MXSPEC*4.5D-5/
      PARAMETER (R2D=180.0D0/3.141592654D0)

      data hroot/0.0d0/,dtqsn/0.0d0/,shawe/0.0d0/,shawet/0.0d0/,
     +     shawpot/0.0d0/,fdepth/0.0d0/,tdepth/0.0d0/,shawt1/0.0d0/


      DATA RHORS/0.15d0,0.17d0,0.18d0/

!-----------MEMORY-----------------------------------------------------
      CALL PHYSCL_Memory('GET', PERIOD, SPAN, DELT, START, NBPR, COR, 
     +    TTRO, STMSEG, ND, FIRST5, JSTDAY, TSTART, OLDSTR, TRFDD,
     +    TCII, TROI, NBPRI, CANIRR, HROOT, RNDR, SNP,
     +    SSTART, PKTEMP, hrootdummy,
     +    IDIMIYOLD,wthour,totalsub)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    Pass weather variables
C    ISTRESS=0, USE DSSAT WATER STRESS, ET FROM DSSAT
C    ISTRESS=1, USE NIMAH-HANKS T FOR UPTAKE AND WATER STRESS 
C    ISTRESS=2, USE NIMAH-HANKS (T+AE)/PET
C    ISTRESS=3, USE NIMAH-HANKS/SHAW PET
C    ISTRESS=4, USE DSSAT Potential Water uptake/SHAW PET
C-----------------------------------------------------------------------
C
      ipet1=ipet
      rzrunoff=0.0d0 
      IF(START) THEN
        istress=istress0
        DTOLD=0.0D0
C        IDP=1
        DO 10 I=1,NN
          JJ=NDXN2H(I)
          BD(I)=SOILPP(3,JJ)
          OLDSTR(I)=THETA(I)*TL(I)
          DTOLD=DTOLD+OLDSTR(I)
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf
c     jak     
c     initialize water flux to zero first time thru for SHAW routines
      if (ishaw.eq.1)   qf(i)=0.0d0
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
c         IF(DRDEP.GT.TLT(I)) IDP=I
   10   CONTINUE
c        IDP=MIN(NN,IDP+1)
        START=.FALSE.
        SNP=SNOWPK
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        if (ishaw.eq.1) then
c     rma
c     jak
        CALL READ_SNOW(FRACIN)           !TO USE SNOW DENSITY IN .SNO FILE
C       ..INITIALIZE SOME SHAW VARIABLES AT START OF RUN
        sumdt = 0.0d0
        slwcan =0.0d0
        slwsno =0.0d0
        slwres =0.0d0
        slwsoi =0.0d0
        sswcan =0.0d0
        sswsno =0.0d0
        sswres =0.0d0
        sswsoi =0.0d0
        shsum  =0.0d0
        sgsum  =0.0d0
        svLEsum=0.0d0

        QSN = 0.0D0
        ZS(1) = 0.0D0
        sand(1) = soilpp(5,1)
        silt(1) = soilpp(6,1)
        CLAY(1)  = SOILPP(7,1)
        PORI(1) = SOILHP(6,1)*aef
        DO 12 I = 2,NN
          IH = NDXN2H(I)
          ZS(I) = ZN(I)*0.01D0
c     rma\gf
              sand(i) = soilpp(5,ih)
              silt(i) = soilpp(6,ih)
              CLAY(I)  = SOILPP(7,IH)
          PORI(I) = SOILHP(6,IH)*aef
12      CONTINUE
C	  PRINT HEADER FOR TEMP AND MOISTURE FILE 
c        WRITE (84,1184) (ZS(J)*100, J=1,NN)
c        WRITE (86,1184) (ZS(J)*100, J=1,NN)
1184    FORMAT ('  DY HR  YR ',100(F6.2,1x))

C
C   Note: Need to improve initializion of zm and zh as function of
C     bare soil and/or surface residue characteristics; also dynamic
C     parameter as standing/flat residues degrade and change architecture
C
        SROUGH = 0.03D0
        ZM = 0.123D0*SROUGH
        ZH = 0.2D0*ZM
        DO 15 I=1,MXSPEC
          CANALB(I) = AC
15      CONTINUE
C

C     ..INITIALIZE NR & TRDT FOR CASE WHEN WE START WITH A SNOW STORM
        IF (RM .GT. 0.0D0 .AND. ISHAW.EQ.1) THEN
          if (pplastic.gt.0.0d0) then 
          NR = 2
          else
          NR = 1
          endif
          TRDT(1) = (TMIN+TMAX) * 0.5D0
        ENDIF
          endif
      ENDIF
C
C     .. RESET THE SNOW ROUTINE INITIALIZATION FLAG IF MIDDLE OF SUMMUER
      IF(JDAY.EQ.200.and.xlat.gt.0.0d0) SSTART=.TRUE.
      IF(JDAY.EQ.10.and.xlat.lt.0.0d0) SSTART=.TRUE.
C
C     ... RESET BREAKTHROUGH CURVES AND INITIALIZE VARIABLES
C
      TIME='             '
      TDAY=DBLE(MDAY)
      TM=(TMIN+TMAX)*0.5D0
      SMELT=0.0D0
      if (ishaw.eq.1) then
	   smelt=smelt_SHAW
         tmstm=0.0d0
         SMELT_SHAW=0.0D0
      endif
      SNRO=0.0D0
      SNODON=.TRUE.
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     gnf
c     jak
      if (ishaw.eq.1) then  
      ISOD = 1
      SUMSHAW = 0.0D0
      NTM = 0
      STRMLT(1) = 0.0D0
      STRMLT(2) = 0.0D0
      DTPEVP = 0.0D0
      DTPTRN = 0.0D0
      AQSN = 0.0D0
      DTQSN = 0.0D0
      ZRFDD = 0.0D0
      CDNCI = 0.0D0
      CII = 0.0D0
      DRSEEP = 0.0D0
      TPUP = 0.0D0
      ROI = 0.0D0
      endif
      TRTS = 0.0D0
      CNVG=.TRUE.
      XMAC=0.0D0
      TMSTM=0.0D0
      RFDACC=0.0D0
      DAYTIM=0.0D0
      DAYRAIN=0.0D0
      TDPLNT=0.0D0
      TPASS=0.0D0
      TACTVE=0.0D0
      RFDD=0.0D0
      BRKH2O=0.0D0
      ACCPO4=0.0D0
      ACTEVP=0.0D0
      TRNTIM=0.0D0
      ACTTRN=0.0D0
      totalsub=0.0d0
      ACTTRNdum=0.0D0
      ATRANS=0.0D0
      DTEVAP=0.0D0
      DTSEEP=0.0D0
      TSEEP=0.0D0
      DTDSEEP=0.0D0
      Dsubirr=0.0D0
      DTLAT=0.0D0
      DSEEP=0.0D0
      DTSINK=0.0D0
      FMP=0.0D0
      TTRO=0.0D0
      TTRFD=0.0D0
      TTCII=0.0D0
      TNITUP=0.0D0
      TDELTA=0.0D0
      WRES=0.0D0
      TTAIRR=0.0D0
      TARAIN(1)=0.0D0
      TARAIN(2)=0.0D0
C-GH Liwang Ma
        TOTPUP = 0.0D0
        totpet = 0.0d0
        totevp = 0.0d0
        totpethr = 0.0d0
        totevphr = 0.0d0
        avg_hroot=0.0   !real to be used by DSSAT
C-GH  Liwang Ma
      DO 20 I=1,3
        TAIRR(I)=0.0D0
        TLRO(I)=0.0D0
   20 CONTINUE
      DO 30 I=1,MXCHEM
        BKCHEM(I)=0.0D0
        DTDCHEM(I)=0.0D0
        DTLCHEM(I)=0.0D0
        DTCHEM(I)=0.0D0
        SCUPCH(I)=0.0D0
   30 CONTINUE
      TMASS=0.0D0
C     .. added delold calc's 8.23.2002 by ken
      DELOLD=0.0D0
      DO 50 I=1,NN
       if (ishaw.eq.1) then
        DELOLD=DELOLD+(THETA(I)+thetai(i)*rhoi/rhol)*TL(I)
       else
        DELOLD=DELOLD+THETA(I)*TL(I)
       endif
        FPW(I)=0.0D0
        UPNIT(I)=0.0D0
        QS(I)=0.0D0
        QSSI(I)=0.0D0
        QT(I)=0.0D0
        QTD(I)=0.0D0
        QTL(I)=0.0D0
        TQ(I)=0.0D0
        TTUP(I)=0.0D0
        TNO3UP(I)=0.0D0
        TUP(I)=0.0D0
        TQFL(I)=0.0D0
        TQNO3(I)=0.0D0
        TMASS=TMASS+CC(I,9)*TL(I)*THETA(I)
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak
        TRTS = TRTS + RDF(I)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
        DO 40 J=1,NPEST
          PESTUP(I,J)=0.0D0
   40   CONTINUE
   50 CONTINUE
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


c     rma
c     jak
C     ..SHAW DAILY INITIALIZATIONS, This following section is commented out as suggested by GNF (calculated after call INPDAY)
      if (ISHAW.eq.1) then
c      if (ihourly .eq. 1) then
c        idaytim = min(int(daytim)+1,24)
c        ta = hrt(idaytim)
c        hum = hrh(idaytim)*0.01D0
c      else
c        HUM = RH * 0.01D0
c        TA = 0.5D0 * (TMAX+TMIN+(TMAX-TMIN)*COS(0.2618D0*(-15.0d0)))
c      endif
c
        IF (IPL .EQ. 0) THEN
          IR = 1
        ELSE
          IR = IRTYPE(IPL)
        ENDIF

      NPLANT = 0
      IF (TRTS.NE.0.0D0) THEN                 !IT ONLY SIMULATES ONE CROP, EITHER LIVING PLANT OR STANDING DEAD
C                                             !NEEDS TO MODIFY NPLANT=2 TO SIMULATE BOTH THE SAME TIME WITH
C                                             !J=1 FOR LIVING PLANT AND J=2 FOR STANDING STUBBLE 
         NPLANT = 1
C         DO 55 I = 1,NPLANT
C
C           PLANTZ = HEIGHT OF CANOPY ABOVE RESIDUE (M)
C           PLANTW = DRY BIOMASS OF PLANT (KG/M^2), SET TO ZERO UNTIL
C                  WE DETERMINE HOW TO HANDLE STANDING DEAD IN SHAW
            PLANTZ(1) = MAX(0.0D0, HEIGHT*0.01D0)  ! - HRM)
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak
c	sets minimum on plantz;  w/o have trouble in VSLOPE
      if(plantz(1).lt.1e-05) plantz(1)=0.0
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
          if (plwidth.eq.0.0d0) plwidth=0.05d0  !for corn, Liwang Ma
          do j=1,nodcan
            dchar(1,j)=plwidth
          enddo
            PLANTW(1) = OMSEA(41)/1.0D4
            TOTLAI(1) = LAI
            IF (LAI.GT.0.0D0) THEN
               ITRN(1)  = 1
               XANGLE(1)=RXANGLE(IPL)
             ELSE
               ITRN(1)  = 0
               XANGLE(1)=0.0D0
            ENDIF
            IEVAP(1)  = 1
            RSTOM0(1) = RST(IPL)
C            XANGLE(1) = RXANGLE(IPL)
            TCCRIT(1) = RTCCRIT(IPL)
            PLEAF0(1) = RPLEAF0(IPL)
            RSTEXP(1) = RRSTEXP(IPL)
            RLEAF0(1) = 1.0D0/RRLEAF0(IPL)  !CONVERTED TO CONDUCTANCE
            RROOT0(1) = RLEAF0(1)/2.0D0     !ASSUME TO BE HALF OF LEAVE CONDUCTANCE
            IF (IRTYPE(IPL).EQ.1) THEN
                RESTKB=8.5D0
            ELSE
                RESTKB=4.0D0
            ENDIF
C
            DO 52 J = 1,NN
               ROOTDN(1,J)= RDF(J)
52          CONTINUE
C55       CONTINUE
         ENDIF
c        CHECK IF STANDING STUBBLE EXISTS
         IF (STEMAI .GT. 0.0D0) THEN
         NPLANT=NPLANT+1
         ievap(NPLANT)=0
         PLANTZ(NPLANT) = MAX(0.0D0, HSTUBL*1.0d-2 - HRM)
         if (rsdia.eq.0.0d0) rsdia=rdia(ipr)
          do j=1,nodcan
            dchar(NPLANT,J)=RSDIA
          enddo
C        CONVERT KG/HA TO KG/M2
         PLANTW(NPLANT)=STUBLW/1.D04
         TOTLAI(NPLANT)=STEMAI
c         RSTOM0(2) = RST(IPL)
         ITRN(NPLANT)=0
         XANGLE(NPLANT)=0.0D0
         END IF
!	HAVE TO SET ZR AND RHOR FOR RZTEMP ROUTINE  !IT WAS LATER AND MOVED HERE by GNF
      IF (RM .GT. 0.0D0 .AND.ISHUTL .NE. 1) THEN       !JCAII GNF  4/7/06
!     IF (RM>0.0) THEN
         if (pplastic.gt.0.0d0) then
         NR = 2
         else
         NR = 1
         endif
c
         ZR(1) = 0.0D0
         TRM = RM*1.0D-3
         IF (CRES .LE. 0.0D0) CRES = 1.320D0
         CS = EXP(-CRES*1.270D-2*TRM/(RDIA(IPR)*RHORS(IPR)))
         RHORB = 0.20D0*RHORS(IPR)
         HR = TRM*1.0D-2/((1.0D0-CS)*RHORB)
!        CONVERT HR FROM CM TO M
         HRM = HR/100.0d0
         DO I = 1,NR
            ZR(I+1) = HRM*(1.0D0-CS)      ! Do not include fraction of cover in computing residue thickness for SHAW
            RHOR(I) = RHORB*1.0D3
         END DO
      ENDIF
c add plastic layer  Liwang 2017
         IF(PPLASTIC.GT.0.0D0) THEN
            RHOR(1)=1.2D3                  !PLASTIC DENSITY, LIWANG 2017
            ZR(2)=1.0D-3                   !1MM THICKNESS OF PLASTIC IS ASSUMED  LIWANG 2017
            ZR(3)=ZR(2)+HRM*(1.0D0-CS)     ! Do not include fraction of cover in computing residue thickness for SHAW
         ENDIF
!
      IF (RM<=1.0D-06) THEN                     !JCAII GNF  4/7/06
         CS = 1.0D0
         HRM = 0.D0
         if (pplastic.gt.0.0d0) then
         NR = 1
         else
         NR = 0
         endif
      END IF
      END IF
C
C     ..MAXIMUM SHORT WAVE RADIATION
C
      ORTS = RTS
C
c      read (333,*) jjday,soitmp1
c      if (jjday.eq.jday) then
c	soitmp=soitmp1
c      else
c	backspace (333)
c	endif
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
c      CALL MAXSW(ASPECT,JDAY,XLAT,rchd,RN,rch,SLOPE,pp,RCS,TSSH,d,rcshr)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
c      IF (RTS .EQ. 0.0D0) THEN
c        RTS = SWSUN(FSUN,RCS)
c      ELSE
c        RTS = ORTS
c      ENDIF

C
C     ..INITIALIZE HOURLY TO DAILY, AND CORRECT IN HRMET LATER IF NEEDED
c rma
c         HRTS = RTS
c         HRCS = RCS
C
c rma/gf  reference rts to radiation at atmospheric boundary
c code derived from maxsw sub-routine
C======================================================================
C     CALCULATE POTENTIAL RAD ON HORIZONTAL SURFACE, at edge of atmosphere
C======================================================================
C
C        SUNRISE(tSRh), SUNSET(tSSh), IN RADIANS
C          NOTE: NOON = 0[R]
C
c         TSRH = -TSSH
C
C        ..CALCULATE COMBINATION TERMS.
C
c         C1H = DSIN(D) * DSIN(xlat)
c         C2H = DCOS(D) * DCOS(xlat)
c         endif
c
C=======================================================================
C
C     .. ADJUST SOLAR CONSTANT FOR ORBIT ECCENTRICITY.
C
        rtstot=0.0d0
          do i=1,mhr
              rtstot=rtstot+hrts(i)*3.6D3/1.D6
c              SSURFT(i)=-273.0d0  !initialize soil surface temperature to absolute zero
          enddo
      CALL MAXSW(ASPECT,JDAY,XLAT,RCHD,RN,RCH,S,PP,RCS,RCSHR,RCHHR)
      if (Rtstot.eq.0.0d0) then
      IF(IHOURLY.eq.0) THEN
         RTS=SWSUN(FSUN,RCS)
         RTH=SWSUN(FSUN,RCH)
      else IF(IHOURLY.eq.1) THEN
c        RTS=SWSUN(FSUN,RCS)
c        RTH=SWSUN(FSUN,RCH)
          RTS=0.0d0
          RTH=0.0d0
        do i=1,mhr
        RTS=RTS+SWSUN(FSUN,RCSHR(i))
        RTH=RTH+SWSUN(FSUN,RCHHR(i))
        HRTS(i)=SWSUN(FSUN,RCSHR(i))*1.d6/3.6d3
        HRTH(i)=SWSUN(FSUN,RCHHR(i))*1.d6/3.6d3
        rtstot=rtstot+hrts(i)*3.6D3/1.D6
        enddo
       ENDIF
      endif
c
       Tavg=(Tmax+Tmin)/2.0d0
       TRAT = dble(TRATIO(CROP,REAL(CO2R),REAL(Tavg),REAL(U),
     +       REAL(TLAI)))
c       trat=1.0d0     !by pass the CO2 effects on stomatal resistence, Liwang Ma
C     ... CALC POTENTIAL EVAP FOR DAY (NO CORRECTION HERE FOR RAIN)
       if (ihourly.eq.1) then
        TOTALPET=0.0D0
        TOTALEVP=0.0D0
        TPET=0.0D0
        TPES=0.0D0
        TPER=0.0D0
        Ret_day_T=0.0d0
        Ret_day_S=0.0d0
        if (rtstot.eq.0.0d0) then
            rts=0.0d0
            rth=0.0d0
        endif
c
          DO I=1,MHR
             Tmax1 = HRT(I)
             Tmin1 = HRT(I)
             RH1 = HRH(I)
             U1 = HRU(I)*3.6D0*24.0D0  !convert to km/day
             RTS1 = HRTS(I)*3.6D3/1.D6   !convert to MJ/M2/HR
             RTH1 = HRTH(I)*3.6D3/1.D6   !convert to MJ/M2/HR
             tair = hrt(I)
             RESAGE=RESAGE+1.0d0/24.d0
c      CALL POTEVP(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,RNDR,RTS1,
c     +    SLOPE,THETA(1),TMIN1,TMAX1,U1,WRES,LAI,XLAT,PP,ICRUST,RH1,
c     +    SOILHP(7,1),SOILHP(9,1),IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,
c     +    TLAI,IPR,SDEAD_HEIGHT,SDEAD,IHOURLY,tdew,i,iwzone,hrts,co2r,
c     +    trat,RTH,HRTH)
        PRINT *, "CALLING REF_ET"
        Call REF_ET(I,hrt(I),hrt(I),hrts(I),hrh(I),hru(I),elev,xlat*R2D,
     +       Xlong*R2D,jday,ETr_hr_T,ETr_hr_S,trat,xw,ihourly,PA)
           CALL CDATE(JDAY,ID,IM,IYYY)
           WRITE (UNIT=89,FMT=780) IYYY,IM,ID,JDAY,I-1,HRT(I),HRU(I),
     +    HRTS(I),HRH(I),CO2R,HRZPAR(I),ETr_HR_T/10.0d0,ETr_HR_S/10.0d0
        Ret_day_T=Ret_day_T+ETr_HR_T/10.0d0
        Ret_day_S=Ret_day_S+ETr_HR_S/10.0d0
             if (ipet.eq.0) then
      CALL POTEVPHR(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,RNDR,
     +   RTS1,SLOPE,THETA(1),TMIN1,TMAX1,U1,WRES,LAI,XLAT,PP,ICRUST,RH1,
     +   SOILHP(7,1),SOILHP(9,1),IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,
     +   TLAI,IPR,SDEAD_HEIGHT,SDEAD,tl,cshslab,thermkavg,delt,pfirst,
     +   h,ipenflux,aevap,tair,t(1),t(ngfl),zn(ngfl),daytim,clouds,
     +   gflux,ishaw,iyyy,soilhp(6,1),.false.,ihourly,tmsoil,tmresidue,
     +   hkbar,nn,rdf,qs,atrans,hroot,pup,trts,pltslv,stemai,zstubl,
     +   stublw,pa,xlh,rhorb,hrm,cr,ar,unew,nsp,nr,rhosp,zsp,orts,delz,
     +   cor,i,iwzone,hrts,co2r,trat,rth1,hrth,0,tmcanopy,rtstot)
      else if (ipet.eq.1) then
        ETC=max(0.0d0,ETr_hr_T*CKCC/10.0d0)
      PRINT *, "CALLING C_R_S_Cover"
      Call C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
      else if (ipet.eq.2) then
        ETC=max(0.0d0,ETr_hr_S*CKCC/10.0d0)
      PRINT *, "CALLING C_R_S_Cover"
      Call C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
      endif
c   Environmental Hydrology, Third Edition, 2016
c   By Andy D. Ward, Stanley W. Trimble, Suzette R. Burckhard, John G. Lyon
c   use short reference ET if height <0.12 m and use tall reference ET when height > 0.12m. 
c   for most agricultural crops, use tall reference ET
c   page 150, Table 4.9
c     .. DETERMINE EVAPORATIVE FLUX FOR REDISTRIBUTION EQUATION
C     SOIL EVAPORATION [CM/DAY]
C      HRPTRANS(I)=PET
c      PET=PET*TRAT
      PES=PES  !*(1.0d0-pplastic)
      PER=PER  !*(1.0d0-pplastic)
      HRPTRANS(I)=-PET
      HREVAP(I)=-(PES+PER)
      TOTALPET=TOTALPET+HRPTRANS(I)
	TOTALEVP=TOTALEVP+HREVAP(I)
      TPET=TPET+PET
      TPES=TPES+PES
      TPER=TPER+PER
        if (rtstot.eq.0.0d0) then
           rts=rts+hrts(i)*3.6D3/1.D6
           rth=rth+hrth(i)*3.6D3/1.D6
        endif
          enddo
      PET=TPET
      PES=TPES
      PER=TPER
        else
c      CALL POTEVP(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,RNDR,RTS,
c     +    SLOPE,THETA(1),TMIN,TMAX,U,WRES,LAI,XLAT,PP,ICRUST,RH,
c     +    SOILHP(7,1),SOILHP(9,1),IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,
c     +    TLAI,IPR,SDEAD_HEIGHT,SDEAD,IHOURLY,tdew,1,iwzone,hrts,co2r,
c     +    trat,RTH,HRTH)
        RESAGE=RESAGE+1.0d0
        PRINT *, "CALLING REF_ET"
        Call REF_ET(I,Tmin,Tmax,RTS,rh,U*1.0d3/(24.0d0*3.6d3),elev,
     +     xlat*R2D,Xlong*R2D,jday,ETr_hr_T,ETr_hr_S,trat,xw,ihourly,PA)
        Ret_day_T=ETr_HR_T/10.0d0
        Ret_day_S=ETr_HR_S/10.0d0
        if (ipet.eq.0) then
      CALL POTEVPHR(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,RNDR,
     +   RTS,SLOPE,THETA(1),TMIN,TMAX,U,WRES,LAI,XLAT,PP,ICRUST,RH,
     +   SOILHP(7,1),SOILHP(9,1),IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,
     +   TLAI,IPR,SDEAD_HEIGHT,SDEAD,tl,cshslab,thermkavg,delt,pfirst,
     +   h,ipenflux,aevap,tavg,t(1),t(ngfl),zn(ngfl),daytim,clouds,
     +   gflux,ishaw,iyyy,soilhp(6,1),.false.,ihourly,tmsoil,tmresidue,
     +   hkbar,nn,rdf,qs,atrans,hroot,pup,trts,pltslv,stemai,zstubl,
     +   stublw,pa,xlh,rhorb,hrm,cr,ar,unew,nsp,nr,rhosp,zsp,orts,delz,
     +   cor,1,iwzone,hrts,co2r,trat,rth,hrth,0,tmcanopy,rts)
      else if (ipet.eq.1) then
        ETC=max(0.0d0,ETr_hr_T*CKCC/10.0d0)
      PRINT *, "CALLING C_R_S_Cover"
      Call C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
      else if (ipet.eq.2) then
        ETC=max(0.0d0,ETr_hr_S*CKCC/10.0d0)
      PRINT *, "CALLING C_R_S_Cover"
      Call C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
        Endif
C     .. DETERMINE EVAPORATIVE FLUX FOR REDISTRIBUTION EQUATION
C     SOIL EVAPORATION [CM/DAY]
c      PET=PET*TRAT
      PES=PES  !*(1.0d0-pplastic)
      PER=PER  !*(1.0d0-pplastic)
      DO I=1,MHR
C      HRPTRANS(I)=PET
      HRPTRANS(I)=-PET/24.0D0
      HREVAP(I)=-(PES+PER)/24.0D0
	ENDDO
      TPET=PET
      TPES=PES
      TPER=PER
       endif
        CALL CDATE(JDAY,ID,IM,IYYY)
        WRITE (UNIT=98,FMT=777) IYYY,IM,ID,JDAY,TMIN,TMAX,U,RTS,
     +         RH,CO2R,RZPAR,ret_day_t,ret_day_s
c      read (333,*) jjday,ETSHAW
c      if (jjday.eq.jday) then
c	do i=1,mhr
c	hrevap(i)=etshaw/24.0d0
c      HRPTRANS(I)=0.0D0
c	enddo
c	pes=etshaw
c	tpes=pes
c	per=0.0d0
c	tper=per
c	pet=0.0d0
c	tpet=pet
c      else
c	backspace (333)
c	endif
c this is to limit DSSAT water uptake by today's PET, 10-1-2009
      IF (PET .GT. 0.0d0. and. dble(trwup).ne.0.0d0) THEN
        IF (PET .LE. dble(TRWUP)) THEN
          WUF = PET/dble(TRWUP)
        ELSE
          WUF = 1.0d0
        ENDIF
c      write (666,*) ep, trwu, wuf
        DO L = 1,nn
            JH = NDXN2H(L)
          IF (theta(L) .GT. soilhp(9,jh)) THEN
               qsr(L) = qsr(l)*real(WUF)
cc               qsr(l) = rwu(l)/24.0/SOILPROP % dlayr(l)
          else IF (theta(L) .LT. soilhp(9,jh)) THEN
               qsr(L)=0.0
          ENDIF
        END DO
      ENDIF
      PETPLANT=PET
c   test Penman-Monteith equation (from HERMERS Model)
c      if (ipet.eq.2)
c      Wind1=U/24.0d0/3.6d0
c      Call ET_PM(DOY,RTS,Wind1,Tmax,Tmin,TAVG,CLOUDS,PES,PET)
c       PER=0.0d0
c       endif
c   
c  test Priestley-Taylor equation
c===========================
C     ..GET SOIL ALBEDO WEIGHTED BY MOISTURE CONTENT

c       AS=ALBSWS(A0,AW,WC13,WC15,THETA,ICRUST,RR)
c
c      call petpt(a0,rts,tmax,tmin,lai,eo)
c       pes = PE(EO,LAI)/10.d0
c	 pet = max((eo/10.0d0-pes),0.0d0)
c	 per = 0.0d0
c===========================
C     .. KNOCK DOWN STANDING DEAD FROM WEATHER
      CALL SDEAD_KNOCKDOWN(SDEAD,SDCN,RM,RCN)
C
       if (ishaw.eq.1 .or. ipenflux.eq.1) then
      TPET=0.0D0
      TPES=0.0D0
      TPER=0.0D0
      TOTALPET=0.0D0
      TOTALEVP=0.0D0
      SHAWET=0.0d0
      SHAWE=0.0d0
      SHAWT1=0.0d0
      SHAWT2=0.0d0
      SHAWPOT=0.0d0
      SHAWEThr=0.0d0
      SHAWEhr=0.0d0
      SHAWThr=0.0d0
      SHAWPOThr=0.0d0
            totaethr=0.0d0
            totathr=0.0d0
            totaehr=0.0d0
   
        endif
C
C     ... ADVANCE THROUGH TIME WITHIN DAY
   60 CONTINUE
C
          IDAYTIM = INT(MIN(DAYTIM+1.0D0,24.0D0))
          PTRANS=HRPTRANS(IDAYTIM)
          EVAP=HREVAP(IDAYTIM)
C     ..SET THE BEGINNING OF THE STORM TO LAST READ IN EVENT
      IF(JDAY.EQ.JSTDAY) THEN
        TS0=TSTART
      ELSE
        TS0=24.0D0
      ENDIF
C
      IF (IRLOC.GT.0) then
          irrtype = irrtyp(irloc)
          if (irrtype.eq.5) airr=airr-subirr
      else
          irrtype = 0
      endif
C
      IF((PERIOD.EQ.'STORM'.OR.PERIOD.EQ.'IRR'.OR.PERIOD.EQ.'SNOAC')
     +    .AND..NOT.SPAN) THEN
C
C       . . .   R E A D    N E W S T O R M I N F O   . . .
C       ==================================================
C       ... INITIALIZE SURFACE AND SOIL PROPERTIES AFTER MOST RECENT STORM
        IF(PERIOD.EQ.'STORM') THEN
          CALL RECON(FTR,TMSTM,STORM,SOILPP,MAXBP,NBPR,THETA,CC,SLKS,
     +        CONCX2,XNU,RPOOL,FREUND)
        ELSEIF(PERIOD.EQ.'IRR') THEN
          ISEG=1
          CALL RECON(FTR,TMSTM,STMSEG(1,1,ISEG),SOILPP,MAXBP,NBPR,THETA,
     +        CC,SLKS,CONCX2,XNU,RPOOL,FREUND)
        ENDIF
        IF(PERIOD.NE.'SNOAC') THEN
C
C         ..DO ALL THIS STUFF IF STORM OR IRR WATER APPLICATION
C
C         ... UPDATE BULK DENSITY USED FOR TRANSPORT
          DO 70 I=1,NN
            JJ=NDXN2H(I)
            BD(I)=SOILPP(3,JJ)
   70     CONTINUE
C
C         ..UPDATE SOIL HYDRAULIC PROPERTIES
          DO 80 J=1,NHOR
            PCLAY=SOILPP(7,J)*100.0D0
            PSILT=SOILPP(6,J)*100.0D0
            PSAND=SOILPP(5,J)*100.0D0
            TBDH=SOILPP(3,J)
            ITYPE=INT(SOILPP(1,J))
            PDEN=SOILPP(2,J)
            CALL SOILPR(J,SOILHP,TBDH,PCLAY,PSAND,PSILT,ITYPE,PDEN,0,
     +                  HWP)
            SOLTP1(J,1)=SOILHP(6,J)
            SOLTP1(J,2)=SOILHP(7,J)
            SOILPP(4,J)=SOILHP(6,J)
   80     CONTINUE
C adjust pori after reconsolidation
        DO 13 I = 1,NN
          IH = NDXN2H(I)
          PORI(I) = SOILHP(6,IH)*aef
13      CONTINUE
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
C         ..DETERMINE ICE DEPENDENT EFFECTIVE POROSITY
          if (ishaw.eq.1)
     +    CALL NWPRSTY(1,THETA,THETAI,SOILHP,NN,NDXN2H,MAXHOR,PORI,AEF)
C
C         ..DRAIN EXCESS THETA DUE TO NEW RECONSOLIDATED PROPERTIES
          CALL DRAIN(NN,NDXN2H,THETA,TL,SOILHP,DTCHEM,TSEEP,DTSEEP,CC,
     +        SLKS,BD,AEF,IBRKTH,BRKH2O,BKCHEM,T,SOLTP1,FREUND,pori)
C
C         ...   UPDATE HEADS WITH NEW WATER CONTENTS
          DO 90 I=1,NDXT2N(ID)
            JJ=NDXN2H(I)
            IF(H(I).GT.-SOILHP(1,JJ)) THEN
              H(I)=MIN(H(I),-SOILHP(1,JJ))
            ENDIF
   90     CONTINUE
          CALL WCHEAD(THETA,H,SOILHP,NN,NDXN2H,MAXHOR)
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
C     KEJ 
c     FOR TESTING ONLY; THETAI AND THETA_SAT HAVEN'T CHANGED, SO
C      PORI SHOULDN'T CHANGE
C         ..DETERMINE ICE DEPENDENT EFFECTIVE POROSITY
c          CALL NWPRSTY(5,THETA,THETAI,SOILHP,NN,NDXN2H,MAXHOR,PORI,AEF)  
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
       
C         ..FIND WATER TABLE
          IF(ITBL.EQ.1) CALL WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QF(NN),
     +        DELT,TLT,ZN,IREBOT,BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,
     +        THETA)
C
C         ..FIND FRACTION WATER FILLED PORE SPACE
          DO 100 I=1,NN
            J=NDXN2H(I)
            IF(YESMAC.EQ.1) XMAC=MCPOR(J)
            XFPW=THETA(I)/(1.0D0-SOILPP(3,J)/SOILPP(2,J)+XMAC)*1.0D2
            FPW(I)=FPW(I)+XFPW*TMSTM
  100     CONTINUE
        ENDIF
C
        IF(PERIOD.EQ.'STORM'.OR.PERIOD.EQ.'SNOAC') THEN
          RFDNEW=0.0
C
C         ... SET UP THE SEGMENTED STORM ARRAY FOR NEXT STORM
          DO 130 I=1,MAXBP
            DO 120 J=1,2
              STORM(I,J)=0.0D0
              DO 110 K=1,2
                STMSEG(I,J,K)=0.0D0
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
C
C         ... RETRIEVE DATA FOR NEXT STORM
          CALL STMINP(DUR,MAXBP,NBPR,STORM,TSTART,TSE,INP4,JSTDAY,JDAY,
     +        IYYY,IMAGIC,RFDNEW,METMOD)
          NBPRI=NBPR
        ENDIF
C
C       ... SET PERIOD TYPE (STORM JUST ENDED)
        PERIOD='INTER'
C
c      ELSEIF(((NSP.gt.0.0d0.or.SNP.GT.0.0D0.OR.TM.LE.0.0D0).
c     +   AND.SNODON).AND..NOT.SPAN) THEN
      ELSEIF(((SNP.GT.0.0D0.OR.TM.LE.0.0D0).AND.SNODON).AND..NOT.SPAN)
     +    THEN
C
C       . . . S N O W . . .
C       ===================
C       ..ENTER HERE IF THERE IS A SNOPACK, OR TEMPERATURE IS FREEZING AND
C       WE WANT THE SNOW ROUTINE TO FIRE
C
        IF(JDAY.EQ.JSTDAY) THEN
C
C         ..ACCUMULATE PRECIP FOR THE WHOLE DAY
          DAYRAIN=DAYRAIN+RFDNEW
          RFDACC=RFDACC+RFDNEW
          PERIOD='SNOAC'
        ELSEIF(SNP.GT.0.0D0.OR.RFDACC.GT.0.0D0) THEN
C
C         ..TAKE CARE OF SNOWPACK AND NEW SNOW
            if (ishaw.eq.0) then
            IF(SNP.GT.0.0D0.OR.RFDACC.GT.0.0D0) THEN
            CALL SNOWCOMP_PRMS(SSTART,JDAY,RTS,RCS,PTRANS,TMIN,TMAX,AS,
     +          ESN,RFDACC,SNP,SMELT,FSNC,PKTEMP,FRACIN,SNOWPK)
C
C           ..NEED TO PARTITION EVAP AND R/O
            AIRR=SMELT*FRACIN
            SNRO=SMELT-AIRR
            EVAP=(1.0D0-FSNC)*EVAP
            SSTART=.FALSE.
            endif
            else
c use SHAW snow routine
        IDAYTIM = INT(MIN(DAYTIM+1.0D0,24.0D0))
        tair = hrt(IDAYTIM)
C         ..FIND TOTAL DEPTH OF SNOW IN M
          SNOW = RFDACC * 1.0D-2
C         ..CHECK FOR TYPE OF SNOWPACK INTERACTION
          IF (NSP.GT.0 .AND. TAir.GT.THRSNO) THEN
            IFMT='(1X,''===> RAIN ON SNOW AT '',I3,'':'',I2.2,1X,A2,/)'
C
C           ..RAIN ON SNOWPACK (M)
            RAINSP = SNOW
C           ..SAVE TEMPERATURE OF RAIN FOR SHAWHT
            TRAIN = TAir
          ELSE
          IFMT='(1X,''===> SNOWFALL EVENT AT '',I3,'':'',I2.2,1X,A2,/)'
C
C           ..ACCUMULATE SNOWPACK
            CALL NWSNOW (NSP,ICESPT,TSPDT,DLWDT,TAir,SNOW,
     +        RHOSP,DZSP,ZSP,TRDT,T,NR)
C
c            smelt = strmlt(2)
C           ..SET FLAG TO DO SHAW ET
          ENDIF
C
            SSTART=.FALSE.
c end of SHAW stuff
c		  endif
          ENDIF
          PERIOD='INTER'
          SNODON=.FALSE.
          OMSEA(92)=SNP
        ELSE
c          SMELT=0.0D0
          SNRO=0.0D0
          SNODON=.FALSE.
        ENDIF
C
      ELSEIF(((DAYTIM.GE.TS0.AND.DAYTIM.LT.(TSE-COR).AND.JDAY.EQ.
     +   JSTDAY).OR.SPAN).OR.(AIRR.GT.0.0D0)) THEN
C
C       . . .  R A I N . . . AND  . . . I R R I G A T I O N   . . .
C       ===========================================================
C       .. CHECK FOR IRRIGATION EVENT OR SPANNING RAINFALL EVENT
        IF(AIRR.GT.0.0D0.AND.(.NOT.SPAN)) THEN
C
C         ..CHECK IF STORM AND IRRIGATION EVENT START AT THE SAME TIME
          IF(AIRR.GT.0.0D0.AND.(JDAY.EQ.JSTDAY.AND.TS0.EQ.0.0D0)) THEN
C
C           ..PAD THE STORM EVENT BY 1 MINUTE TO ALLOW BOTH TO OCCUR
            TS0=TS0+1
          ENDIF
          SPAN=.FALSE.
          PERIOD='IRR  '
          IF(SMELT.GT.0.0D0.or.smelt_shaw.gt.0.0d0) THEN
            IFMT='(1X,"===> SNOWMELT INFILTRATION AT ",I3,":",    I2.2,1
     +X,A2)'
          ELSE
            IFMT='(1X,"===> IRRIGATION EVENT AT ",I3,":",I2.2,    1X,A2)
     +'
C
            IF (IRLOC.GT.0) THEN
C             .. IRLOC WOULD BE 0 IF AUTOMATIC IRRIGATION IS USED
              IF(IRRTYP(IRLOC).EQ.1) THEN
C
C               .. SPRINKLER IRRIGATION; SETUP PSEUDO-STORM DATA
                ISEG=1
                NBPR=4
                NSPAN=4
C LIWANG MA, DEC 8, 2006. TO PREVENT RRATE BEING 0
                IF (RRATE(IRLOC).EQ.0.0D0) RRATE(IRLOC)=1.0d0
                RRATE_C = RRATE(IRLOC)
C END OF MODIFICATION
                TMIRR=AIRR/RRATE(IRLOC)
                DO 140 I=1,NBPR
                  STMSEG(I,1,ISEG)=TMIRR*0.25D0
                  STMSEG(I,2,ISEG)=AIRR*0.25D0
  140           CONTINUE
                IF(TMIRR.GT.24.0D0) THEN
                  PRINT*
                  PRINT*,'  <<< E R R O R  >>>'
                  PRINT*
                  PRINT*,'IT WOULD TAKE MORE THAN 24 HOURS TO APPLY'
                  PRINT*,'THE AMOUNT OF IRRIGATION WATER YOU SPECIFIED'
                  PRINT*,'AT THE CURRENT APPLICATION RATE.'
                  PRINT*
                  PRINT*,'AMOUNT =',AIRR,'  RATE =',RRATE(IRLOC)
                  CANIRR=.TRUE.
                ENDIF
              ELSEIF(IRRTYP(IRLOC).EQ.4) THEN
C
C         ..FIND average Ksat of the top 30 cm  Liwang Ma, 12-18-2008
          TERM2=0.0D0
          DO 402 J=1,30
             JJ=IT2H(j)
c            IF(Jj.EQ.1) THEN
              TERM2=TERM2+1.0d0/soilhp(4,JJ)   !1 cm increment for the infiltration grade.
c            ELSE
c              TERM2=TERM2+(DHB(J)-DHB(J-1))/soilhp(4,JJ)
c            ENDIF
  402     CONTINUE
          SK_avg = 30.0d0/term2/2  ! per Laj request to use 30 cm average Ksat/2.0
C               .. DRIP IRRIGATION; SETUP FOR CONSTANT RAINFALL @ KSAT RATE
                ISEG=1
                NBPR=4
                NSPAN=4
c                TMIRR=AIRR/SOILHP(4,1)
                TMIRR=AIRR/SK_avg
                DO 150 I=1,NBPR
                  STMSEG(I,1,ISEG)=TMIRR*0.25D0
                  STMSEG(I,2,ISEG)=AIRR*0.25D0
  150           CONTINUE
C
              ELSE
C         ... SET UP THE SEGMENTED STORM ARRAY FOR NEXT STORM
          DO I=1,MAXBP
            DO J=1,2
              STORM(I,J)=0.0D0
              DO K=1,2
                STMSEG(I,J,K)=0.0D0
          enddo
            enddo
              enddo
              ENDIF
            ENDIF
          ENDIF
        ELSE
C
C         ... INITIALIZE STORM
          PERIOD='STORM'
          IFMT='(1X,"===> RAINFALL EVENT AT ",I3,":",I2.2,1X,A2)'
C
C         ... CHECK IF NEXT STORM SPANS NEXT DAY & ADJUST
          IF(.NOT.SPAN) THEN
C
C           ..ONLY GO IN HERE IF FIRST TIME THRU AFTER READ
            NBPR=NBPRI
            CALL CHSPAN(DUR,MAXBP,NBPR,SPAN,STORM,TS0,STMSEG,NS,ND)
            ISEG=1
            NSPAN=NS
            DAYRAIN=DAYRAIN+RFDNEW
          ELSE
C
C           ..GO THRU HERE ONLY ON A SPAN STORM
            IF(AIRR.GT.0.0D0) THEN
              CALL MANOUT(-1,0,JDAY,0,IYYY)
              PRINT*,'===> IRRIGATION APPLICATION CANCELED (SPANNING)'
              CANIRR=.TRUE.
            ENDIF
            PRINT*,'===> CONTINUING PREVIOUS DAYS STORM'
            PRINT*
            ISEG=2
            SPAN=.FALSE.
            NSPAN=ND
          ENDIF
        ENDIF
        IF(CANIRR) THEN
          AIRR=0.0D0
          DO 160 I=1,4
            FERTIR(I)=0.0D0
  160     CONTINUE
          DO 170 I=1,MXPEST
            PESTIR(I)=0.0D0
  170     CONTINUE
        ENDIF
        CANIRR=.FALSE.
C
C       .. PRINT TIME OF STORM
        IHOUR=INT(MOD(DAYTIM,12.0D0))
        MINUT=INT((DAYTIM-INT(DAYTIM))*60.0D0)
        IF(IHOUR.EQ.0) IHOUR=12
        IF(INT(DAYTIM/12.0D0).LT.1) THEN
          WRITE(*,FMT=IFMT) IHOUR,MINUT,'AM'
        ELSE
          WRITE(*,FMT=IFMT) IHOUR,MINUT,'PM'
        ENDIF
C
C       ..FIND STORM DURATION
        TMSTM=0.0D0
        DO 180 I=1,NSPAN
          TMSTM=TMSTM+STMSEG(I,1,ISEG)
  180   CONTINUE
        IF(AIRR.GT.0.0D0) THEN
          TMSTM=0.0D0
          TTAIRR=AIRR
        ENDIF
C
C       ... CALCULATE FLUX DUE TO SUBSURFACE DRAINAGE
        SUBDR=0.0D0
        IF(IDRAIN.EQ.1) CALL TILEFLO(TLT,HORTHK,THGCUR,DRSPAC,DRRAD,
     +      H2OTAB,SUBDR,NN,NHOR,CLAT,IDP,UDRN)
C
C       ..IF THERE IS A SNOWPACK SET TEMPERATURE SURFACE BOUNDRY CONDITION = 0
c           if (ihourly.eq.1) then  !use hourly air temperature for surface temp all the time, 6-12-2014
                TMX=hrt(idaytim)
c           else
c                TMX=TM       !check this if Penflux is used.
c           endif
        IF(SNP.GT.0.0D0) THEN  !assume zero temperature only snow depth greater than an inch
          TMX=TMX+(0.85D0+31.13D0*SNP/100.0D0)  !FROM GE AND GONG, 2010 VOL 115, D08107, DIO:10.1029/2009JD012798, J GEOPHYSICAL RESEARCH
        ENDIF
C
C       ..FIND POTENTIAL TOTAL OUTFLOW FOR STORM
C       TOUTFLO = (SUBDR+BOTFLX)*TMSTM
C
C       ..FIND INITIAL WATER CONTENT
        TOOTH=0.0D0
        DO 190 I=1,NN
          if (ishaw.eq.1) then
          TOOTH=TOOTH+(THETA(I) + THETAI(I)*RHOI/RHOL) * TL(I)
          else
          TOOTH=TOOTH+THETA(I)*TL(I)
          endif
  190   CONTINUE
C
c      if (airr.gt.0.0d0.and.smelt_SHAW.eq.airr) then
c         STMSEG(1,1,1)=2.0D0  !make it a two hour rain event for snowmelt
c         STMSEG(1,2,1)=AIRR
c      endif
C       .. GO THROUGH INFILTRATION ROUTINE
        CALL EVNTRO(CC,NN,NHOR,ID,FTR,SLKS,STMSEG(1,1,ISEG),
     +      STMSEG(1,2,ISEG),COPLNT,CORES,NBPR,ZRFDD,JDAY,CII,ROI,AIRR,
     +      FMP,DTCHEM,T,SOLTP1,TMX,CMPS,IMAGIC,INT(TLT(IBRKTH)),BRKH2O,
     +      BKCHEM,H2OTAB,SUBDR,DRSEEP,DTDCHEM,TLT,HORTHK,CAPZONE,SMELT,
     +      IDP,OMSEA,IYYY,THGCUR,pori,SCUPCH,Hmin,pplastic)
C
        CUTTER=0.0D0
        TMASS=0.0D0
        DO 200 I=1,NN
          TMASS=TMASS+CC(I,9)*TL(I)*THETA(I)
          if (ishaw.eq.1) then
          CUTTER=CUTTER+(THETA(I) + THETAI(I)*RHOI/RHOL) * TL(I)
          else
          CUTTER=CUTTER+THETA(I)*TL(I)
          endif
  200   CONTINUE
        TTMASS=TOOTH+ZRFDD-ROI-CUTTER-CDNCI-DRSEEP
        IF((IMAGIC.LE.-1.AND.IMAGIC.GE.-3).OR.IMAGIC.EQ.-10) WRITE(9,
     +      1000) JDAY,TOOTH,ZRFDD,ROI,DRSEEP,CDNCI,CUTTER,TTMASS
C
C       ..UPDATE HEADS WITH NEW WATER CONTENTS
        CALL WCHEAD(THETA,H,SOILHP,NN,NDXN2H,MAXHOR)
C
C       ..FIND WATER TABLE
        IF(ITBL.EQ.1) CALL WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QF(NN),DELT,
     +      TLT,ZN,IREBOT,BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,THETA)
C
c        DAYTIM=DAYTIM+TMSTM   !as suggested by Zhiming Qi. We may be able to do hourly redistribution instead of end of infiltration 
        DELOLD=CUTTER
        AIRR=0.0D0
        FIRST5=.TRUE.
        DTSEEP=DTSEEP+CDNCI
        DTDSEEP=DTDSEEP+DRSEEP
        TTRFD=TTRFD+ZRFDD+SNRO
        TRFDD=TRFDD+ZRFDD+SNRO
        TTRO=TTRO+ROI+SNRO
        TROI=TROI+ROI+SNRO
        RZrunoff=RZrunoff+ROI
        TCII=TCII+CII
        TTCII=TTCII+CII
        IF(DAYTIM.GE.(24.0D0-COR)) THEN
          DAYTIM=24.0D0
          CALL MASSBL(MDAY,IMAGIC,TL,THETA,NN,DAYTIM,DTOLD,DTEVAP,
     +        DTSINK,DTSEEP,DTDSEEP,DTLAT,TTRFD,TTRO,FMP,TTCII,DTCHEM,
     +        CMPS,CNVG,DTDCHEM,DTLCHEM,ICNVG,BIGCNV,OMSEA,TTAIRR,SMELT,
     +        SNRO,JDAY,IYYY,jeday,DTQSN,THETAI,ishaw,SCUPCH,dsubirr,
     +        jbday)
        ENDIF
CC ----- chd -------------
      If (ErosionActive.and.nspan.gt.0) then                                            !sab\
C       GLEAMS
c        RzIrrigation = AIRR_C  !sab presumed to be in cm for this day-used in Gleams
c        RzRainfall = trfdd_C    !sab 5/03 - these link to gleams - in cm, must be in inches
c        RzRunoff   = troi_C     !sab conversion takes place in GLEAMSDailyErosion in Rz-Erosion
c        gleams_ei = 100 * gleams_exrain * gleams_ei
        !DayMeanTemp = T(1)            !sab
c        Call DailyErosion (SOLOSS,ENRICH,iyyy*1000+jday,           !sab arg=yyyyddd=sdate
c     &                          T(1),trfdd_C,                     ! trfdd_c includes rain and irrigation
c     &                          RzRunoff)
        totaltime=0.0d0
        totalrain=0.0d0
        thirtyRR=0.0d0
        do i=1,30
            RR30(i)=0.d0
        enddo
c
        do i=1,nspan-1
        totaltime=totaltime+stmseg(i,1,iseg)
        totalrain=totalrain+stmseg(i,2,iseg)
        enddo
        if (totaltime.le.0.5d0) then
            thirtyRR=(totalrain/2.54d0)/max(totaltime,0.5d0)
        else
        istart=1
        iend=0
        do i=1,nspan-1   !nspan is # of breakpoints, nspan-1 is # of storm segments
            iend=iend+int(stmseg(i,1,iseg)*60.d0)
            RRI=(stmseg(i,2,iseg)/2.54d0)/stmseg(i,1,iseg)
            do j=istart,iend
            thirtyRR=max(thirtyRR,AVEMOV(RR30,30,RRI,J))
            enddo
            istart=istart+int(stmseg(i,1,iseg)*60.d0)
        enddo
        endif
      end if
c
      ELSE
C
C       . . .   R E D I S T R I B U T I O N   . . .
C       ===========================================
C       ... BETWEEN STORMS, ADJUST DELT
        CALL ADJDT(DAYTIM,DELT,TS0,NN,QF,FIRST5,ihflag,THETAI,ishaw)
C
C       ...       FIND SINK FOR PLANT TRANSPIRATION
        TRTS=0.0D0
        QS(1)=0.0D0
        PUP=0.0D0
c-GH Liwang Ma        PUP=0.0D0
        DO 210 I=1,NN
          TRTS=TRTS+RDF(I)
          QS(I)=0.0D0
          QSSI(I)=0.0D0
          QT(I)=0.0D0
          QTD(I)=0.0D0
          QTL(I)=0.0D0
          QSdummy(I)=0.0D0
  210   CONTINUE
        OBTNIT=CC(NN,9)
        DTDNIT=0.0D0
C LIWANG MA, RZ-PENFLUX, JUNE 2008, CALCULATE HOURLY PET
      IF (IPENFLUX.EQ.1) THEN
C
          cshslab = 0.0d0
          thermkavg = 0.0d0
        DO I=1,ngfl   !USE FIRST 4 LAYERS.
C       
C       ... NUMERICAL LAYER HORIZON INDEX
        JH=NDXN2H(I)
        NSC=NINT(SOLTP1(JH,4))
c        CTEXT=CTEXTV(NINT(SOLTP1(JH,3)))
C       ... NEW EFFECTIVE LAYER HEAT CAPACITY
c        CSHNOD=(SOLTP1(JH,5)+CW*THETA(I))   ! in J/mm3/C
         CSHNOD=soilpp(3,jh)*1.0D3*CM + Theta(i)*RHOL*CL   !use SHAW routine
C  SOLTP2 IS USED TO SET GA       
              call soiltk(1,thknod,theta(i),0.0D0,soilpp(3,jh)*1.0D3,
     +      soilpp(5,jh),soilpp(6,jh),soilpp(7,jh),fracom(i))   !use SHAW routine
c        THKNOD=THERMK(SOLTP2(1,JH,3),CTEXT,T(I),NSC,SOLTP2(1,JH,1),
c     +      SOLTP2(1,JH,2),THETA(I),SOLTP1(JH,1),SOLTP1(JH,2),ATMP)
            cshslab = cshslab + cshnod/ngfl
            thermkavg = thermkavg + thknod/ngfl
         ENDDO
      endif
         if (ishaw.eq.1.or.ipenflux.eq.1) then
	  if ((idaytim.ne.int(daytim+1.0d0+delt).and.idaytim.ne.1).or.
     +     (idaytim.eq.1.and.daytim.eq.0.0d0)) then  !go to hourly ET once an hour
c        IDAYTIM = INT(MIN(DAYTIM+1.0D0,24.0D0))
        tair = hrt(IDAYTIM)
        Tmax1 = Tair
        Tmin1 = Tair
        Uhr = hru(idaytim)*3.6D0*24.0D0  !convert to km/day
        RH1 = hrh(idaytim)
	  hrts1=hrts(idaytim)*3.6D3/1.D6
	  rth1=hrth(idaytim)*3.6D3/1.D6
        hum = hrh(idaytim)*0.01D0  !from percent to decimal
C
c      if (ihflag) write (88,188) jday,idaytim,iyyy,tair,rh,uhr,hrts1
188    format (i3,i4,1x,i5,5(f8.3,2x))
      if (ipet.eq.0) then
      CALL POTEVPHR(ASPECT,CS,ELEV,EPAN,FT,FTR,JDAY,PET,PER,PES,RNDR,
     + HRTS1,SLOPE,THETA(1),TMIN1,TMAX1,Uhr,WRES,LAI,XLAT,PP,ICRUST,RH1,
     +    SOILHP(7,1),SOILHP(9,1),IPL,HEIGHT,AS,ESN,RCS,PKTEMP,IRTYPE,
     +    TLAI,IPR,SDEAD_HEIGHT,SDEAD,tl,cshslab,thermkavg,delt,pfirst,     !need to understand first5 and pfirst, liwang ma, 6-23-2010
     +    h,ipenflux,aevap,tair,t(1),t(ngfl),zn(ngfl),daytim,clouds,
     +    gflux,ishaw,iyyy,soilhp(6,1),ihflag,ihourly,tmsoil,tmresidue,
     +    hkbar,nn,rdf,qs,atrans,hroot,pup,trts,pltslv,stemai,zstubl,
     +    stublw,pa,xlh,rhorb,hrm,cr,ar,unew,nsp,nr,rhosp,zsp,orts,delz,
     +    cor,idaytim,iwzone,hrts,co2r,trat,rth1,hrth,1,tmcanopy,rtstot)
      else if (ipet.eq.1) then
      PRINT *, "CALLING REF_ET"
      Call REF_ET(I,hrt(Idaytim),hrt(Idaytim),hrts(Idaytim),
     +       hrh(Idaytim),hru(Idaytim),elev,xlat*R2D,
     +       Xlong*R2D,jday,ETr_hr_T,ETr_hr_S,trat,xw,ihourly,PA)
        ETC=max(0.0d0,ETr_hr_T*CKCC/10.0d0)
      PRINT *, "CALLING C_R_S_Cover"
      Call C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
      else if (ipet.eq.2) then
      PRINT *, "CALLING REF_ET"
      Call REF_ET(I,hrt(Idaytim),hrt(Idaytim),hrts(Idaytim),
     +       hrh(Idaytim),hru(Idaytim),elev,xlat*R2D,
     +       Xlong*R2D,jday,ETr_hr_T,ETr_hr_S,trat,xw,ihourly,PA)
        ETC=max(0.0d0,ETr_hr_S*CKCC/10.0d0)
      PRINT *, "CALLING C_R_S_Cover"
      Call C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
      endif  

C     .. DETERMINE EVAPORATIVE FLUX FOR REDISTRIBUTION EQUATION
C     SOIL EVAPORATION [CM/DAY]
c      PTRANS=PET
      PES=PES  !*(1.0d0-pplastic)
      PER=PER  !*(1.0d0-pplastic)
      HRPTRANS(Idaytim)=-PET
      HREVAP(Idaytim)=-(PES+PER)
      TOTALPET=TOTALPET+HRPTRANS(Idaytim)
      TOTALEVP=TOTALEVP+HREVAP(Idaytim)
      TPET=TPET+PET
      TPES=TPES+PES
      TPER=TPER+PER
      PTRANS=-PET
      EVAP=-(PES+PER)       !use hourly ET when penflux is used
c	hrptrans(idaytim) = ptrans
c	hrevap(idaytim) = evap
      ENDIF
       endif
C total water in the root zone
      tpw=0.0d0
      do i=1,nn
            JJ=NDXN2H(I)
            if (rdf(i).gt.0.0d0) tpw=tpw+(theta(i)-SOILHP(9,JJ))*tl(i)
      enddo

c RZ-ma Liwang Ma
c      if (ihourly.eq.1) then
c        IF(PTRANS.NE.0.0D0.AND.TRTS.NE.0.0D0) THEN
        IF(PTRANS.NE.0.0D0.AND.TRTS.NE.0.0D0.and.petplant.ne.0.0d0) THEN
c  from generic plant growth module
         if ((inxpl(ipl).lt.2000). or .(inxpl(ipl).ge.8000)
     +       .or.(nuse.eq.1.and.npcpoint.gt.0)) then
          if ((ishaw .eq. 1).and.((istress.eq.3).or.(istress.eq.4)))then  !from SHAW
c            actualET=(evaps+transp/rhol)*100.d0   !from SHAW
            HROOT=H(1)
          CALL SINK(PTRANS,HKBAR,DELZ,NN,H,RDF,QS1,TL,ATRANS2,HROOT,PUP,
     +        FIRST5,HWP)
	      do i=1,nn
c            qs(i)=xtract(i)*100.0d0/tl(i)*3600.0d0/rhol   !in m/s
            qs(i)=rootxt(i)/tl(i)   !in cm/hr
	      end do
	    else    !if ((istress.eq.1).or.(istress.eq.2)) then
            HROOT=H(1)
          CALL SINK(PTRANS,HKBAR,DELZ,NN,H,RDF,QS,TL,ATRANS,HROOT,PUP,
     +        FIRST5,HWP)
c          else if (istress.eq.0) then
c              stop 'cannot use DSSAT ET'
          endif
      else   !from DSSAT crop growth models
          if ((ishaw .eq. 1).and.((istress.eq.3).or.(istress.eq.4)))then  !from SHAW
c            actualET=(evaps+transp/rhol)*100.d0   !from SHAW
            HROOT=H(1)
          CALL SINK(PTRANS,HKBAR,DELZ,NN,H,RDF,QS1,TL,ATRANS2,HROOT,PUP,
     +        FIRST5,HWP)
	      do i=1,nn
c            qs(i)=xtract(i)*100.0d0/tl(i)*3600.0d0/rhol   !in m/s
            qs(i)=rootxt(i)/tl(i)   !in cm/hr
	      end do
	    else
c      ATRANS1=0.0D0  !limit uptake to today's ptrans as per Laj suggestion
      do 221 i = 1, nn
C        IF ((ISTRESS.EQ.1).or.(ISTRESS.EQ.2).or.(istress.eq.4)) THEN
        IF ((ISTRESS.EQ.1).or.(ISTRESS.EQ.2)) THEN
            HROOT=H(1)
          CALL SINK(PTRANS,HKBAR,DELZ,NN,H,RDF,QS,TL,ATRANS,HROOT,PUP,
     +        FIRST5,HWP)
         ELSE if (istress.eq.0.OR.ISTRESS.EQ.4) then
              qs(i) = dble(qsr(i))*24.0d0*abs(ptrans/petplant)  !when shaw is used, pet is not there yet, so use pet without shaw
         ENDIF
c              ATRANS1=ATRANS1+QS(I)*TL(I)
221         continue
c            IF (ABS(ATRANS1).GT.ABS(PTRANS)) THEN
c               OVER=ABS(ATRANS1)-ABS(PTRANS)
c               DO I=1,NN
c               QS(I)=QS(I)*(1.0d0-OVER/ABS(ATRANS1))
c               ENDDO
c	         endif
CC        HROOT=H(1)
CC          CALL SINK(PTRANS,HKBAR,DELZ,NN,H,RDF,QSdummy,TL,ATRANSdummy,
CC     +        HROOT,PUPdummy,FIRST5,Hmin)
      endif
	 endif
c RZ-ma  Liwang Ma
c        endif
        ENDIF
c        if (ishaw.eq.1) then
c         do i=1,nn
c           xtract(i)=qs(i)/100.0d0*tl(i)/3600.0d0*rhol   !in m/s
c         enddo
c        endif
        OBTNIT=CC(NN,9)
        DTDNIT=0.0D0
C
C       ..FIND WATER TABLE
        IF(ITBL.EQ.1) CALL WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QF(NN),DELT,
     +      TLT,ZN,IREBOT,BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,THETA)
C
C       .. CALCULATE FLUX DUE TO SUBSURFACE DRAINAGE
        SUBDR=0.0D0
        IF(IDRAIN.EQ.1) CALL TILEFLO(TLT,HORTHK,THGCUR,DRSPAC,DRRAD,
     +      H2OTAB,SUBDR,NN,NHOR,CLAT,IDP,UDRN)
C
C       .. DETERMINE LATERAL TRANSLOCATED FLOW
        QLAT=0.0D0
        CALL LATFLO(H2OTAB,HORTHK,TLT,CLAT,NDXN2H,DRDEP,IDP,NHOR,NN,
     +     ULAT,QLAT,HYDGRAD,soilhp)
C
C       ..DETERMINE DISTRIBUTED SINK
          DO 220 I=1,NN
            QTD(I)=QTD(I)+UDRN(I)/TL(I)
            QTL(I)=QTL(I)+ULAT(I)/TL(I)
            QT(I)=QTD(I)+QTL(I)
  220     CONTINUE
C
c  subsurface irrigation, added on Nov. 27, 2013, Liwang Ma
c
            if (irrtype.eq.5) then
                 INDP=ndxt2n(int(airrdepth))
                 if (rrate(irloc).le.0.0d0) rrate(irloc)=0.5d0
                if (subirr.gt.dsubirr) then
                    rsubirr=rrate(irloc)  !min(RRATE(IRLOC),max(0.0d0,subirr-max(dsubirr-rrate(irloc)))
                else
                    rsubirr=0.0d0
                endif
c
                totalsub=dsubirr+Rsubirr*delt
c                totalsub=dsubirr+RRATE(IRLOC)*delt
               if (totalsub.le.subirr) then
c                qssi(indp)=rrate(irloc)/tl(indp)*1.0d0/3.0d0
                qssi(indp)=rsubirr/tl(indp)  !*1.0d0/3.0d0
c                qssi(indp+1)=rsubirr/tl(indp+1)*1.0d0/3.0d0
c                qssi(indp-1)=rsubirr/tl(indp-1)*1.0d0/3.0d0
c                TTRFD=TTRFD+RRATE(IRLOC)*delt
c                TTIRR=TTIRR+RRATE(IRLOC)*delt
c                TTAIRR=totalsub
               else if(abs(subirr-dsubirr).lt.abs(rsubirr*delt))then
                rsubirr=abs(subirr-dsubirr)/delt
                qssi(indp)=rsubirr/tl(indp)  !*1.0d0/3.0d0
c                qssi(indp+1)=rsubirr/tl(indp+1)*1.0d0/3.0d0
c                qssi(indp-1)=rsubirr/tl(indp-1)*1.0d0/3.0d0
               else
c                qssi(indp-1)=0.0d0
                qssi(indp)=0.0d0
c                qssi(indp+1)=0.0d0
               endif
            endif
c            
c           do i=1,NN
c              deltice(i)=thetai(i)-thetaoldi(i)
c           enddo
C       ...       INTER-EVENT PERIOD: REDISTRIBUTE MOISTURE
        CALL REDIST(DELT,DELZ,H,NDXN2H,NN,QF,QS,SOILHP,TL,HKBAR,THETA,
     +     FIRST5,EVAP,AEVAP,BD,DTSEEP,TSEEP,DTCHEM,SLKS,CC,IREBOT,
     +     BOTHED,BOTFLX,AEF,IBRKTH,BRKH2O,BKCHEM,ALPH,H2OTAB,TLT,ITBL,
     +     CNVG,DTDCHEM,DTLCHEM,DAYTIM,IMAGIC,CAPZONE,ZN,T,SOLTP1,QT,
     +     DTDNIT,RICH_EPS,MAXITER_RICH,MAXCYCLE_RICH,QTD,QTL,ICHEM,
     +     pori,qsn,freund,deltice,ishaw,evaps,thetaold,thetai,Hmin,nsp,
     +     qssi,istress)
C
C       ...       INTER-EVENT PERIOD: CALC SOIL TEMP
        if(ishaw.ne.1.and.ipenflux.ne.1) then
c           if (ihourly.eq.1) then !use hourly temperature all the time, 6-12-2014
                TMX=hrt(idaytim)
c           else
c                TMX=TM       !check this if Penflux is used.
c           endif
        IF(SNP.GT.0.0D0) THEN
          TMX=TMX+(0.85D0+31.13D0*SNP/100.0D0)  !FROM GE AND GONG, 2010 VOL 115, D08107, DIO:10.1029/2009JD012798, J GEOPHYSICAL RESEARCH
        ENDIF
        CALL HEATFX(CSH,SOLTP2,T,DELT,DELZ,NDXN2H,NN,ATMP,QF,QS,THETA,
     +      TL,SOLTP1,TMX,TBOTM)
        DAYTIM=DAYTIM+DELT   !why Joe increase early?  2-21-2009
        FIRST5=.FALSE.
         else if (ipenflux.eq.1) then
         TMX = tmsoil  !use T from Penflux? Liwang Ma
c         t(1)=tmsoil
c
c          if (mod(daytim,1.0d0).lt.1d-5) then
c	     write (101,132) jday, int(daytim),iyyy,tmx,t(1)
c132   format (i4,i4,i8,30(f7.2,1x,f7.2,1x))
c          endif
        CALL HEATFX(CSH,SOLTP2,T,DELT,DELZ,NDXN2H,NN,ATMP,QF,QS,THETA,
     +      TL,SOLTP1,TMX,TBOTM)
        FIRST5=.FALSE.
c          if (mod(daytim,1.0d0).lt.1d-5) then
c	     write (101,132) jday, int(daytim),iyyy,t(1)
c          endif
        DAYTIM=DAYTIM+DELT   !why Joe increase early?  2-21-2009
C
         else if (ishaw.eq.1) then
C       ..GET ACTUAL SNOW MELT FOR CURRENT TIME STEP
        AQSN = QSN
        QSN = 0.0D0
C     KEJ
c     FOR TESTING ONLY; THETAI AND THETA_SAT HAVEN'T CHANGED, SO
C      PORI SHOULDN'T CHANGE
C         ..DETERMINE ICE DEPENDENT EFFECTIVE POROSITY

c          CALL NWPRSTY(3,THETA,THETAI,SOILHP,NN,NDXN2H,MAXHOR,PORI,AEF)
cc
c     Note: Convert RZWQM variables to SHAWHT terms
c        if ishutl = 0, calculates energy balance; otherwise, partitions
c        gflux to multiple soil and residue sinks, computes soil and residue
c        T profile, and ice content
c
C
C       ..SET UP VARIABLES FOR SHAW
C
c rma
C       ..CONVERT MJ/m2_hr to J/mw_s
        WCONV = 1.0D6/3.6D3
        DT = DELT * 3600.0D0
c        WIND = UNEW/86.4D0  !from km/day to m/s
c        IF (IHOURLY .EQ. 1) THEN  !when SHAW was used, it always has hourly input
          WIND = UNEW
c          GFLUXS = GFLUX*WCONV
c          SUNHOR = HRTS1*WCONV
          SUNHOR = HRTh(idaytim)
c        ELSE
c          WIND = UNEW/86.4D0
c          GFLUXS = GFLUX*WCONV/24.0D0
c          SUNHOR = RTh*WCONV/24.0D0
c        ENDIF

        PRESUR = PA*1.0D3
        DO 227 I = 1,NN
          IH = NDXN2H(I)
          MATDT(I) = H(I)/100.0D0
          QSL(I)   = QF(I)/3.6D5
          RHOB(I)  = SOILPP(3,IH)*1.0D3
          SAT(I)   = SOILHP(6,IH)
227     CONTINUE

csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak  MOVED TO EARLY SECTIOIN 9-2-2009
c	have to set zr and rhor for SHAW Temperature routine
C        IF (RM .GT. 0) THEN
C          NR = 1
C          ZR(1) = 0.0D0
C          TRM=RM*1.0D-3
C          IF(CRES.LE.0.0D0) THEN
C          CRES=1.320D0
C          ENDIF
C          CS=EXP(-CRES*1.270D-2*TRM/(RDIA(IPR)*RHORS(IPR)))
C		RHORB=0.20D0*RHORS(IPR)
C          HR=TRM*1.0D-2/((1.0D0-CS)*RHORB)
c	convert hr from cm to m
C		hrm=hr/100.0d0
C          DO 228 I = 1,NR
C            ZR(I+1) = HRM
C            RHOR(I) = RHORB*1.0D3
C228       CONTINUE
C        ENDIF
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C
c gnf 5/2/97
        ishutl=0   !use ET from SHAW, Liwang Ma, only transpiration from SHAW now. evaporation is from RZWQM
      IF (ISHUTL.EQ.0) THEN
C        ESTIMATE MAXIMUM SOIL EVAPORATION
c         JH= NDXN2H(1)
c         HMINet=-15000.d0
c         HKBI= POINTK(H(1),SOILHP(1,JH),jh,PORI(1))
c         HKBP= POINTK(HMINet,SOILHP(1,JH),jh,PORI(2))
c         HKAVG= DSQRT(HKBI*HKBP)
c         DELH = HMINet-H(1)
c         EVAPMX= HKAVG * (DELH / DELZ(1)) + HKAVG
c        CONVERT TO M/S AND CHANGE SIGN (POSITIVE EVAPORATION)
c         EVAPMX = -EVAPMX/3.6D5
         if (istress.lt.3) EVAPMX = -AEVAP/3.6D5  !per gnf suggestion, 6-25-2010, possible problem in the winter due to snow routine differences
c rma
c           esmin = max(esmin2,aevap*xlh*1.d7/3.6d3)
c           evapmx = -aevap*xlh*1.d7/3.6d3  !use actual evapotranspiration from RZWQM
 	DO J=1,NPLANT
 	 IF (ITRN(J).EQ.1) THEN
 	    IF (SUNHOR .LT. 10.d0) IEVAP(J)=0
 		IF (SUNHOR .GE. 10.d0) then
		    IEVAP(J)=1
          IF (TAVG.LE.TCCRIT(J)) IEVAP(J)=0
	    endif
 	 END IF
 	END DO

        ELSE
         EVAPMX = 0.0
      END IF
C       .. CALL SHAW HEAT ROUTINES
c rma
c      if (ihflag) write (88,388) jday,idytim,iyyy,tair,hum,wind,sunhor
388   format (i3,i4,i5,5(f8.3,2x))
C rma\gf
           do i=1,NN
              thetaold(i) = theta(i)   !added by Liwang Ma, need to check
              thetaoldi(i) = thetai(i)   !added by Liwang Ma, need to check
            do j=1,mxchem
	          xcc(i,j)=cc(i,j)   !it seems to me that SHAWHT did not adjust CC after ice correctly. Liwang Ma, 9-16-2009
            enddo
           enddo
c      write (101,*) t(1),t(2)
        Uhr1 = uhr/(3.6D0*24.0D0)  !convert back to m/s from km/day per Gerald's request
C NEED TO WORRY ABOUT PLANT WIDTH. IT IS HARD CODED TO 0.02 M FOR NOW
c        if (nsp.gt.0.0d0) then
         SWE = 0.0
         DO I=1,NSP
            SWE=SWE + RHOSP(I)*DZSP(I)/RHOL + DLWDT(I)
         enddo
         SWEDT = SWEDT + STORE
c        endif
      ATRANS1=0.0D0  !limit uptake to today's ptrans as per Laj suggestion
         DO I=1,NN
              ATRANS1=ATRANS1+ABS(QS(I)*TL(I)*RHOL/100.d0/3600.d0)
         ENDDO
c
      CALL GOSHAW(NC,NSP,NR,NN,MXCHEM,DT,UHGHT,TAir,HUM,Uhr1,SUNHOR,
     + GFLUXS,SPMELT,EVAPS,PRESUR,SOILHP,NDXN2H,ZS,T,CC,MATDT,THETA,
     + THETAI,QSL,RHOB,SAT,sand, silt,CLAY,fracom,AS,ICESDT,ZR,TRDT,
     + GMCDT,RHOR,TSPDT,DLWDT,ICESPT,RHOSP,DZSP,ZSP,ZH,ZM,VAPRDT,CR,
     + PLANTZ,PLANTW,TCDT,TLCDT,VAPCDT,WCANDT,ZC,TOTLAI,ITRN,IEVAP,
     + RSTOM0,ROOTDN,TRANSP,TPOT,ROOTXT,XMW,SLKS,RAINSP,IDAYTIM,XLAT,
     + SLOPE,ASPECT,elev,AR,CANALB,NPLANT,evapr,ISHUTL,TSSH,D,CLOUDS,
     + NNSP,TRAIN,jday,tswcan,tswsno,tswres,tswsoi,tlwcan,tlwsno,tlwres,
     + tlwsoi,swcan,swsnow,swres,swsoil,lwcan,lwsnow,lwres,lwsoil,
     + hsum,hflux,gsum,vLEsum,ATRANS1,evapmx,dchar,xtract,tevaps,evap1,
     + store,iyyy,fdepth,tdepth,tbotm,Hmin,ISTRESS,XANGLE,
     + PLEAF0,RSTEXP,RLEAF0,RROOT0,RESTKB,WINDC,EMITR0,TLCSHAW,
     + TLEAFU,TLEAFL)
C
        sumdt = sumdt  + dt
        slwcan =slwcan + tlwcan 
        slwsno =slwsno + tlwsno 
        slwres =slwres + tlwres 
        slwsoi =slwsoi + tlwsoi 
        sswcan =sswcan + tswcan 
        sswsno =sswsno + tswsno 
        sswres =sswres + tswres 
        sswsoi =sswsoi + tswsoi 
        shsum  =shsum  + hsum  
        sgsum  =sgsum  + gsum  
        svLEsum=svLEsum+ vLEsum
          GFLUX = GFLUXS/WCONV
c        if (nsp.gt.0.0d0) then
         SWEDT = 0.0
         DO I=1,NSP
            SWEDT = SWEDT + RHOSP(I)*DZSP(I)/RHOL + DLWDT(I)
         enddo
C        WATER IN PROCESS OF BEING LAGGED THROUGH THE SNOWPACK
         SWEDT = SWEDT + STORE
c         DO I=1,11
c            SWEDT = SWEDT + WLAG(I)
c         enddo
C     CALCULATE CHANGE IN WATER CONTENT OF SNOWPACK IN CM
      totmelt=totmelt+spmelt
      DSNOW = DSNOW + (SWEDT - SWE)*100.d0
      subl=dsnow+totmelt
c       endif
c
       shawe=shawe+evaps*delt
       shawehr=shawehr+evaps*delt
c       shawpot=shawpot+tpot(1)/10.0d0   !*delt/10.0d0
       shawpot=shawpot+tpot(1)*delt      !*delt/10.0d0
       shawpothr=shawpothr+tpot(1)*delt  !*delt/10.0d0
       do i=1,nn
       shawt1=shawt1+rootxt(i)*delt   !*delt
       shawthr=shawthr+rootxt(i)*delt   !*delt
c       shawt2=shawt2+xtract(i)*100.0d0*delt*3600.d0/rhol   !*delt
       enddo
       shawet=shawet-evap1*delt
       shawethr=shawethr-evap1*delt
       if (istress.ge.3) then
           evap=-evaps
           ptrans=-tpot(1)
       endif
c
C       IF (EVAPS.NE.0.0D0) THEN
c       PRINT *, DAYTIM,EVAPS
C       ENDIF
c         adjust concentration after ice change, Liwang Ma, 12-24-2009
          call ADCONICE(SOILPP,THETAold,theta,
     +           XCC,SLKS,CONCX2,XNU,FREUND)
c      write (101,*) t(1),t(2),t(3)
C       ...   UPDATE HEADS WITH NEW WATER CONTENTS
           do i=1,NN
              deltice(i)=thetaoldi(i)-thetai(i)
            do j=1,mxchem
	          cc(i,j)=xcc(i,j)
            enddo
           enddo
        CALL WCHEAD (THETA,H,SOILHP,NN,NDXN2H,MAXHOR)
C
C       ..DETERMINE ICE DEPENDENT EFFECTIVE POROSITY
c        if (ishaw.eq.1) then
          CALL NWPRSTY(4,THETA,THETAI,SOILHP,NN,NDXN2H,MAXHOR,PORI,AEF)
c        endif
cc
C       ...   INCREMENT DAILY TIME COUNTERS
C
        IF (SPMELT .GT. 0.0D0 .or. (DAYTIM+DELT).GE. (24.0D0-COR)) THEN
c        IF (SPMELT.GT.0.0D0.or.(abs((DAYTIM+DELT)-24.0D0).lt.cor))THEN
c         ..COLLECT SNOW MELT TO BE INFILTRATED AT END OF DAY.
c           TIME WILL BE UPDATED AFTER INFILTRATION OR HERE IF
c           THERE IS NO MELT TO BE DONE.
c         if (spmelt.gt.0.0d0) then
          CALL STMMLT(SPMELT,STRMLT,DELT,DAYTIM,cor)
c            smelt=strmlt(2)
c            AIRR=SMELT
            if ((DAYTIM+DELT).GE. (24.0D0-COR)) then
c            if (abs((DAYTIM+DELT)-24.0D0).lt.cor) then
                smelt_SHAW=strmlt(2)
            else
                smelt_SHAW=0.0d0
            endif
c
            strmlt(2)=0.0d0
c            strmlt(1)=0.0d0
c         endif
c          IF (STRMLT(2) .EQ. 0.0D0) DAYTIM = DAYTIM + DELT !should time advance here?
        ELSE
          STRMLT(1) = 0.0D0
          STRMLT(2) = 0.0D0
c          DAYTIM = DAYTIM + DELT  !should time advance here?
        ENDIF
        IF(AIRR.GT.0.0D0) THEN
          TMSTM=0.0D0
          TTAIRR=AIRR
        ENDIF
        SPMELT = 0.0D0
        FIRST5 = .FALSE.
        DAYTIM = DAYTIM + DELT  !should time advance here?
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
         endif    !end of SHAW option
C       ...       UPTAKE NITROGEN FOR PLANT GROWTH MODEL
        IF(PTRANS.NE.0.0D0.AND.TRTS.NE.0.0D0) THEN
          DO 230 I=1,NN
            TUP(I)=0.0D0
            TUP(I)=ABS(QS(I)*DELT*TL(I))
  230     CONTINUE
c Changed by Liwang Ma 8/13/2003
         if ((inxpl(ipl).lt.2000).or.
     &      ((inxpl(ipl).ge.8000).AND.(inxpl(ipl).LT.9700))) then
          CALL UPTAKE(NN,RDF,CC(1,9),CC(1,10),TUP,THETA,TL,UPNIT,DELT,
     +        IPL,TDPLNT,TPASS,TACTVE,INXPL,EFFLUX,TNO3UP)
         endif
C
C         ..ACCOUNT FOR PESTICIDE UPTAKE BY ROOTS
          DO 240 IP=1,NPEST
            CALL RTUPTK(CC(1,IP+12),PESTUP(1,IP),TDPPLNT(IP),RDF,THETA,
     +          TL,TUP,PWRTS,XKOW(IP),NN,BD,SLKS,IP,FREUND)
  240     CONTINUE
        ENDIF
C
C       ..GET DATA READY FOR OUTPUT
        ATRANS=0.0D0
        Asubirr=0.0d0
        ATRANSdummy=0.0D0
        ATILE=0.0D0
        ALAT=0.0D0
        DO 250 I=1,NN
          TQ(I)=TQ(I)+WRKNUM(I,8)*DELT
          TUP(I)=TUP(I)+ABS(QS(I)*DELT*TL(I))
          TTUP(I)=TTUP(I)+ABS(QS(I)*DELT*TL(I))
          TNUP(I)=UPNIT(I)*PWRTS*1.0D-3
          TQFL(I)=TQFL(I)+QF(I)*DELT
          TQNO3(I)=TQNO3(I)+CC(I,9)*QF(I)*DELT
C-GH  Liwang Ma
c          TOTPUP(I) = TOTPUP(I) + ABS(PUP(I)*DELT)
C-GH  Liwang Ma
          RZrwu(I)=REAL(TUP(I))
          ATRANS=ATRANS+ABS(QS(I)*TL(I))
          Asubirr=Asubirr+abs(QSsi(I)*TL(I))
          ATRANSdummy=ATRANSdummy+ABS(QSdummy(I)*TL(I))
          ATILE=ATILE+ABS(QTD(I)*TL(I))
          ALAT=ALAT+ABS(QTL(I)*TL(I))
C
C         ..FIND FRACTION WATER FILLED PORE SPACE INSTANTANEOUS
          J=NDXN2H(I)
          IF(YESMAC.EQ.1) XMAC=MCPOR(J)
          XFPW=THETA(I)/(1.0D0-SOILPP(3,J)/SOILPP(2,J)+XMAC)*1.0D2
          FPW(I)=FPW(I)+XFPW*DELT
  250   CONTINUE
C
C       ...       ACCUMULATE TOTALS FOR DAILY MASS BALANCE
        TRNTIM=TRNTIM+DELT
        totpet = totpet + ptrans*delt
        totevp = totevp + evap*delt
        totpethr = totpethr + ptrans*delt
        totevphr = totevphr + evap*delt
        totAEhr=totAEhr+ABS(AEVAP*DELT)
        totathr=totathr+ABS(ATRANS*DELT)
        ACTEVP=ACTEVP+ABS(AEVAP*DELT)
        ACTTRN=ACTTRN+ABS(ATRANS*DELT)
        ACTTRNdum=ACTTRNdum+ABS(ATRANSdummy*DELT)
        TOTPUP = TOTPUP + ABS(PUP*DELT)
        DTSEEP=DTSEEP+QF(NN)*DELT    !deep seepage
        DTDSEEP=DTDSEEP+ATILE*DELT   !tile flow
        Dsubirr=Dsubirr+Asubirr*DELT !subsurface irrigation
        DTLAT=DTLAT+ALAT*DELT        !leteral flow
        DTEVAP=DTEVAP+ABS(AEVAP*DELT)
        DTSINK=DTSINK+ABS(ATRANS*DELT)
        Avg_hroot=avg_hroot+real((hroot*delt)/24.0d0)
C
            if (irrtype.eq.5) then
c                 INDP=ndxt2n(int(airrdepth))
c                totalsub=totalsub+RRATE(IRLOC)*delt
c               if (totalsub.le.airr) then
c                tlsub=rrate(irloc)/(tl(indp-1)+tl(indp)+tl(indp+1))
c                qssi(indp-1)=tlsub
c                qssi(indp)=tlsub
c                qssi(indp+1)=tlsub
                TTRFD=TTRFD+Asubirr*DELT
                TTIRR=TTIRR+Asubirr*DELT
                TTAIRR=dsubirr
c                print*, totalsub
c            else
c                qssi(indp-1)=0.0d0
c                qssi(indp)=0.0d0
c                qssi(indp+1)=0.0d0
c               endif
            endif
c         if (abs(daytim-IDAYTIM).lt.DELT/2.0d0) THEN
c         SSURFT(idaytim)=T(IPDEPTH)
c         ENDIF
C
        IF(IMAGIC.EQ.-1) THEN
          TFMASS=0.0D0
          DELSTO=0.0D0
          TTNUP=0.0D0
          SSEEP=TSEEP
          DSEEP=ATILE*DELT
          DLAT=ALAT*DELT
          TSEEP=QF(NN)*DELT
          TEVAP=ABS(AEVAP*DELT)
          DO 260 I=1,NN
            TTNUP=TTNUP+TNO3UP(I)*TL(I)*THETA(I)
            TNO3UP(I)=0.0D0
            TFMASS=TFMASS+CC(I,9)*TL(I)*THETA(I)
           if (ishaw.eq.1) then
             DELSTO = DELSTO + TL(I) * (THETA(I) + THETAI(I)*RHOI/RHOL)
           else
             DELSTO=DELSTO+TL(I)*THETA(I)
           endif
  260     CONTINUE
          DELTA=TMASS-TFMASS-OBTNIT*TSEEP-DTDNIT-TTNUP
          TDELTA=TDELTA+DELTA
          WRITE(9,*) ' TDELTA ==>',TDELTA
          DTMASS=DELOLD-TEVAP-ABS(ATRANS*DELT)-
     +      TSEEP-DLAT-DSEEP-SSEEP-DELSTO+abs(asubirr*delt)
C
C         MASSBALANCE TRAP
C         IF (ABS(DTMASS) .GT. 1.0D-4) THEN
C         PRINT *, 'HIT MASSBALANCE TRAP'
C         ENDIF
          WRITE(9,1100) DAYTIM,DELT,H2OTAB(2),TMASS,OBTNIT*TSEEP,DTDNIT,
     +        TTNUP,TFMASS,DELTA,DELOLD,ABS(EVAP*DELT),TEVAP,ATRANS,
     +        HKBAR(NN-1)*DELT,TSEEP,DSEEP,SSEEP,DELSTO,DTMASS
          DELOLD=DELSTO
          TMASS=TFMASS
          TSEEP=0.0D0
        ENDIF
      if (ishaw.eq.1) then
c     rma/gnf 
c          get snow depth in centimeters
           if (nsp.gt.0) then
             snodep = zsp(nsp+1)*100.
             omsea(92) = zsp(nsp+1)*100.
            else
             snodep = 0.0d0
             omsea(92) = 0.0d0
           endif
c
c     write energy balance output
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c	jak
c        if (imagic.eq.-2.and.mod(daytim,1.0d0).lt.1d-5.and.ihflag) then
c	with the above if statement, imagic=-8 won't run
ccc      This statement did not always work; try using IF statement that follows
ccc      if (mod(daytim,1.0d0).lt.1.0e-5) then
c         if (abs(daytim-nint(daytim)).lt.DELT/4.0D0) then
         if (abs(daytim-nint(daytim)).lt.1.0e-5) then
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
            delsto = 0.d0
          DO 262 I = 1, NN
           if (ishaw.eq.1) then
            DELSTO = DELSTO + TL(I) * (THETA(I) + THETAI(I)*RHOI/RHOL)
           else
            DELSTO = DELSTO + TL(I) * THETA(I)
           endif
  262     CONTINUE
c
c          get snow depth in centimeters
           if (nsp.gt.0) then
             snodep = zsp(nsp+1)*100.
            else
             snodep = 0.0d0
           endif
c
          idytim=nint(daytim)
          if (ishutl .eq. 0) then
c             pass and write SHAWET energy balance terms
c             use SHAWET energy balance terms for energy-limiting ET
C             Note: convert units from cm/hr to kJ/m2 s
              transp1w = (transp(1)*xlh*1.d7)/3.6
	        evapsw = (evaps*xlh*1.d7)/3.6
              evaprw = (evapr(1)*xlh*1.d7)/3.6
c rma 4/21/97
              aevapw = (-1.d0*aevap*xlh*1.d7)/3.6
              efluxw = evapsw+evaprw+transp1w
c             correct these to get average flux from GOSHAW
c             instead of the last time step from GOSHAW
c             rlnsubw = tlwcan + tlwres + tlwsoi + tlwsno
c             rsnsubw = tswcan + tswres + tswsoi + tswsno
              rlnsubw = slwcan + slwres + slwsoi + slwsno
              rsnsubw = sswcan + sswres + sswsoi + sswsno
              rnsubw = rsnsubw + rlnsubw
c             gflw = -(rsnsubw + rlnsubw + hsum + vLEsum)
              gflw = -(rsnsubw + rlnsubw + shsum + svLEsum)
c             G = G*3.6D3/1.D6
c output statements need to be adapted for this configuration
c rma 1/28/97; gnf 4/6/97
c             pesw = pes*xlh*1.0d7/3.6d3
c             enbal = rnw-(-hfluxw+efluxw+gflw)
c rma 4/21/97
C           PRINT OUT ENERGY FLUXES IN kJ ADJUSTED TO HOURLY BASIS
C           IF TIME STEP IS LESS THAN 1 HOUR.
c           write (87,*) ' efluxw = ', efluxw
c	      write (87,*) transp(1),evaps,evapr(1),evaps*2500.
c            write (87,111) jday,idytim,iyyy,tswcan*3.6/DT,tswsno*3.6/DT,
c     +	     tswres*3.6/DT,tswsoi*3.6/DT,rsnsubw*3.6/DT,tlwcan*3.6/DT,
c     +	     tlwsno*3.6/DT,tlwres*3.6/DT,tlwsoi*3.6/DT,rlnsubw*3.6/DT,
c     +         -hsum*3.6/DT,vLEsum*3.6/DT,gflw*3.6/DT


c	write(87,111)jday,idytim,iyyy,
cxxxx       enbal0=rsnsubw/dt+rlnsubw/dt+hum/dt+vLEsum/dt+gflw/dt
c            enbal0=rsnsubw/dt+rlnsubw/dt+hsum/dt+vLEsum/dt+gsum/dt
            enbal0=rsnsubw+rlnsubw+shsum+svLEsum-sgsum
c            write (87,112) jday,idytim,iyyy,tlwcan/DT,tlwsno/DT,
c     +	     tlwres/DT,tlwsoi/DT,tswcan/DT,tswsno/DT,tswres/DT,
c     +         tswsoi/DT,rlnsubw/DT+rsnsubw/DT,
c     +         -hsum/DT,-vLEsum/DT,gflw/DT,enbal0
	    
c           write (87,112) jday,idytim,iyyy,slwcan/DT,slwsno/DT,
c    +	     slwres/DT,slwsoi/DT,sswcan/DT,sswsno/DT,sswres/DT,
c    +         sswsoi/DT,rlnsubw/DT+rsnsubw/DT,
c    +         -shsum/DT,-svLEsum/DT,gflw/DT,enbal0
c     adjust LEsum and hsum when SHAWE and AE are different, commented out for now
            if (istress.ge.3) then
            adjLEsum=LV*((TOTAEHR-SHAWEHR)+(totathr-shawthr))*RHOL/1.d2
            else
            adjLEsum=0.0d0
            endif
            SVLESUM=SVLESUM-ADJLESUM
            SHSUM=SHSUM+ADJLESUM
C END OF ADJUSTMENT
            write (87,112) jday,idytim,iyyy,slwcan/sumdt,slwsno/sumdt,
     +	     slwres/sumdt,slwsoi/sumdt,sswcan/sumdt,sswsno/sumdt,
     +         sswres/sumdt,sswsoi/sumdt,rlnsubw/sumdt+rsnsubw/sumdt,
     +         -shsum/sumdt,-svLEsum/sumdt,sgsum/sumdt,enbal0/sumdt
cxxx +         -shsum/sumdt,-svLEsum/sumdt,gflw/sumdt,enbal0/sumdt
            sumdt = 0.0d0
            slwcan =0.0d0
            slwsno =0.0d0
            slwres =0.0d0
            slwsoi =0.0d0
            sswcan =0.0d0
            sswsno =0.0d0
            sswres =0.0d0
            sswsoi =0.0d0
            shsum  =0.0d0
            sgsum  =0.0d0
            svLEsum=0.0d0
c           write (86,121) jday,idytim,iyyy,tair,trdt(1),t(1),
c     +        (t(i),i=1,24),t(nn)
          IF (NR.EQ.1.AND.pplastic.eq.0.0d0) THEN
             TPLASTIC=0.0D0
             TRESIDUE=TRDT(1)
          ELSE IF (NR.EQ.1.AND.PPLASTIC.GT.0.0D0) THEN
             TPLASTIC=TRDT(1)
             TRESIDUE=0.0D0
          ELSE IF (NR.EQ.2) THEN    !ONLY THERE IS PLASTIC COVER
             TPLASTIC=TRDT(1)
             TRESIDUE=TRDT(2)
          ELSE
             TPLASTIC=0.0D0
             TRESIDUE=0.0D0
          ENDIF
          write (86,121) jday,idytim,iyyy,tair,TPLASTIC,TRESIDUE,
     +       tcdt(1),(t(i),i=1,NN)
c rma
c gnf
C           write (84,131) jday,idytim,iyyy,
C    +        ((theta(i)+THETAI(I)*RHOI/RHOL),i=1,24),snodep,
C    +        (THETAI(I),i=1,24)
            write (84,131) jday,idytim,iyyy,
     +        ((theta(i)+THETAI(I)*RHOI/RHOL),i=1,NN)
            write (184,131) jday,idytim,iyyy,
     +        (theta(i),i=1,NN)
            write (185,131) jday,idytim,iyyy,
     +        (THETAI(I)*RHOI/RHOL,i=1,NN)
            write (186,133) jday,idytim,iyyy,fdepth,tdepth,snodep
            write (187,133) jday,idytim,iyyy,shawehr,shawthr,shawethr,
     &                  shawpothr, abs(totpethr),abs(totevphr),snodep,
     &                   abs(totaehr),abs(totathr)
            if (nc.gt.0) then
            write (284,133) jday,idytim,iyyy,(WINDC(i),i=1,NC)
            write (285,133) jday,idytim,iyyy,(TCDT(i),i=1,NC)
            write (286,133) jday,idytim,iyyy,(VAPCDT(i),i=1,NC)
            write (287,133) jday,idytim,iyyy,(LWCAN(1,i),i=1,NC)
            write (288,133) jday,idytim,iyyy,(SWCAN(1,i),i=1,NC)
            write (289,133) jday,idytim,iyyy,(TLCDT(1,i),i=1,NC)
            endif
            shawehr=0.0d0
            shawthr=0.0d0
            shawethr=0.0d0
            shawpothr=0.0d0
            totpethr=0.0d0
            totevphr=0.0d0
            totaethr=0.0d0
            totathr=0.0d0
            totaehr=0.0d0
c
c          elseif (ishutl .lt. 4) then
c            if (ishutl .eq. 1) tmpen2 = tair
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
c            write (86,121) jday,idytim,iyyy,tair,trdt(1),(t(i),i=1,NN)
c            write (84,131) jday,idytim,iyyy,
c     +        ((theta(i)+THETAI(I)*RHOI/RHOL),i=1,NN)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

c            write (86,121) jday,idytim,iyyy,ta,tmpen2,tmpen1,
c     +        (t(i),i=1,10),t(nn),ishutl,nr,hrm,trdt(1),gflux

c            write (84,131) jday,idytim,iyyy,gmcdt(1),
c     +        ((theta(i)+THETAI(I)*RHOI/RHOL),i=1,10),snodep,
c     +        (THETAI(I),i=1,10)
CCCC +        ((theta(i)+THETAI(I)*RHOI/RHOL),i=1,8),delsto
          endif
c         print out flux between nodes in mm/hr
c          write (89,132) jday,idytim,iyyy,-evaps*10,(qf(i)*10.d0,i=1,24)

         endif
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
         endif
C
        IF(IMAGIC.EQ.-3) THEN
C         IF (IMAGIC.EQ.-1) THEN
C         ---CODE FOR 3-D PLOT DATA ---
C         TDELT = TDELT + DELT
C         WRITE (99,1200) (TDELT,ZN(I),H(I),THETA(I),HKBAR(I),QF(I),
C         WRITE (9,1200) (DAYTIM,tlt(I),H(I),THETA(I),HKBAR(I),QF(I),
C         +    QF(I),CC(I,9),I = 1,NN)
          WRITE(99,1200) ((MDAY+DAYTIM*1.0D-2)*1E6,TLT(I),H(I),THETA(I),
     +        HKBAR(I),QF(I),TTUP(I),CC(I,9),I=1,NN)
        ENDIF
C
        if ((mod(daytim,1.0d0).lt.delt).and.((ipenflux.eq.1).or.
     +       ((ihourly.eq.1).and.(ishaw.ne.1)))) then
            write (86,121) jday,nint(daytim),iyyy,tair,TMPLASTIC,
     +          tmresidue,tmcanopy,(t(i),i=1,NN)
            write (84,131) jday,nint(daytim),iyyy,
     +        ((theta(i)+THETAI(I)*RHOI/RHOL),i=1,NN)
            write (184,131) jday,nint(daytim),iyyy,
     +        (theta(i),i=1,NN)
            write (185,131) jday,nint(daytim),iyyy,
     +        (THETAI(I)*RHOI/RHOL,i=1,NN)
            write (186,131) jday,nint(daytim),iyyy,fdepth,tdepth,snodep
c            write (86,121) jday,idaytim,iyyy,(t(i),i=1,NN)
c            write (84,131) jday,idaytim,iyyy,(theta(i),i=1,NN)
            write (187,133) jday,nint(daytim),iyyy,shawehr,shawthr,
     &           shawethr,shawpothr, abs(totpethr),abs(totevphr),snodep,
     &                   abs(totaehr),abs(totathr)
            shawehr=0.0d0
            shawthr=0.0d0
            shawethr=0.0d0
            shawpothr=0.0d0
            totpethr=0.0d0
            totevphr=0.0d0
            totaethr=0.0d0
            totathr=0.0d0
            totaehr=0.0d0
        endif
121   FORMAT (i3,2x,i2,2x,i4,1x,300(F6.1,1x))
131   FORMAT (i3,2x,i2,2x,i4,1x,300(F6.3,1x))
133   FORMAT (i3,2x,i2,2x,i4,1x,15(F14.6,1x))
C       INCREMENT DAILY TIME COUNTER
c        DAYTIM=DAYTIM+DELT   !why Joe increase early?  2-21-2009
C
        IF(DAYTIM.GE.(24.0D0-COR)) then
            DAYTIM=24.0D0
        endif
        CALL MASSBL(MDAY,IMAGIC,TL,THETA,NN,DAYTIM,DTOLD,DTEVAP,DTSINK,
     +      DTSEEP,DTDSEEP,DTLAT,TTRFD,TTRO,FMP,TTCII,DTCHEM,CMPS,CNVG,
     +      DTDCHEM,DTLCHEM,ICNVG,BIGCNV,OMSEA,TTAIRR,SMELT,SNRO,JDAY,
     +      IYYY,jeday,DTQSN,THETAI,ishaw,SCUPCH,dsubirr,jbday)
C
      ENDIF
C
c         if (abs(daytim-IDAYTIM).lt.DELT/2.0d0) THEN
         if (abs(daytim-IDAYTIM).lt.1.0e-05) THEN
c         if (abs(daytim-nint(DAYTIM)).lt.1.0e-5) THEN
         SSURFT(idaytim)=T(IPDEPTH)
         ENDIF
C     .. DISPLAY ROW OF STARS FOR USER'S PIECE-OF-MIND
c      IF(NOSTAR.EQ.0) THEN
c        IP=INT(DAYTIM)+1
c        DO 270 I=1,IP
c          TIME(I:I)='*'
c  270   CONTINUE
c        WRITE(*,1300) TIME
c      ENDIF
C
      IF (.NOT.CNVG) THEN 
         CALL CDATE(JDAY,ID,IM,IYYY)
         CALL DAYJACK(0,0,0,1)
          IDIMIY=IYYY*10000+IM*100+ID
          IF (IDIMIYOLD.NE.IDIMIY) THEN
            WRITE (666,FMT=667) ID, IM, IYYY, jday !Liwang Ma
            IDIMIYOLD = IDIMIY
          ENDIF
      ENDIF
667     FORMAT (I4,I4,I6,' (DOY ',i3,')',15x,
     + ' ===>NON-CONV RICHARDS EQUATION')
C     ... CHECK FOR END OF DAY
      IF(DAYTIM.LT.24.0D0-COR.OR.(PERIOD.EQ.'STORM'.AND.(.NOT.SPAN)))
     +    GOTO 60
C
C     ..DETERMINE WATER STRESS FOR PLANT GROWTH
C     --MINIMUM STATEMENT IS NEEDED BECAUSE ATRANS IS AN APPROXIMATION
      IF(PTRANS.NE.0.0D0.AND.TRNTIM.NE.0.0D0) THEN
c        EWP=MIN((ACTTRN/(ABS(PTRANS)*TRNTIM)),1.0D0)
       if (wsi(ipl).gt.0.0d0) then
        EWP=MIN((ACTTRN/ABS(totpet))**wsi(ipl),1.0D0)
       else
        EWP=MIN(ACTTRN/ABS(totpet),1.0D0)
       endif
      ELSE
        EWP=1.0D0
      ENDIF
C RESET ACCUMULATED DAILY ET FOR DSSAT TO USE
      PET=TPET
      PES=TPES
      PER=TPER
c      ewp=1.0d0
c      do i=1,mhr
c	    wthour(i)=hrptrans(i)/pet
c      enddo
C
       write (555,fmt=543) mday,shawe,shawt1,shawet,shawpot, abs(totpet)
     &     ,omsea(6),omsea(7),shawe-omsea(6),abs(totevp)
543   format (i5,15(f10.6,5x))
CZ-MA Liwang Ma, in DSSAT, RZtrwup can be greater than potential transpiration. So, the Nimah=Hanks equation is used when 
c  actual T = potential T when SHAW ET is used.
C      PTRANSDAY = ABS(PTRANS)*TRNTIM
      PTRANSDAY = ABS(totpet)
C      if ((istress.eq.1).or.(istress.eq.2).or.(istress.eq.4)) then
      if ((istress.eq.1).or.(istress.eq.2)) then
      RZtrwup=REAL(min(TOTPUP,tpw))
      else if (istress.eq.3) then
      RZtrwup=REAL(max(min(TOTPUP,tpw),shawt1))
c          if (shawt1.lt.shawpot) then
c              RZtrwup=REAL(min(shawt1,tpw))
c          else if (totpup.gt.shawt1) then
c              RZtrwup=REAL(min(TOTPUP,tpw))
c          else
c              RZtrwup=REAL(min(shawt1,tpw))
c          endif
c
      PET=shawpot
      PES=shawe   !(shawet-shawpot)   !*(1.0d0-pplastic), should not needed because vflux has taken into accout of pplastic in ATSTAB.FOR
      PER=0.0d0
c      RZtrwup=REAL(min(shawpot,tpw))
      else if (istress.eq.4) then
      PET=shawpot
      PES=shawe   !(shawet-shawpot)   !*(1.0d0-pplastic), should not needed because vflux has taken into accout of pplastic in ATSTAB.FOR
      PER=0.0d0
      endif
c      oldstress1 = 1.0d0-ewp
c      write (666,*) ptransday
CZ-MA   Liwang Ma
C     ..FIND FRACTION WATER FILLED PORE SPACE DAILY
        avg_H=0.0d0
        avg_SW=0.0d0
        avg_Hrdf=0.0d0
        avg_SWrdf=0.0d0
        tlroot=0.0d0
        tlrootrdf=0.0d0
      DO 280 I=1,NN
        IF(TRNTIM.GT.0.0D0) THEN
          FPW(I)=FPW(I)/TRNTIM
        ELSE
          FPW(I)=FPW(I)/24.0D0
        ENDIF
        FPW(I)=MIN(FPW(I),100.0D0)
         JH = NDXN2H(I)
         if (rdf(i).gt.0.0d0) then
         Avg_H=Avg_H+(H(I)*TL(I))                      !tlt(nn)   !*rdf(i))/tlt(nn)
         Avg_SW=Avg_SW+((theta(I)-soilhp(9,jh))*TL(I)) !*rdf(i))   !/tlt(nn)
         Avg_Hrdf=Avg_Hrdf+(H(I)*TL(I)*rdf(i))
         Avg_SWrdf=Avg_SWrdf+((theta(I)-soilhp(9,jh))*TL(I)*rdf(i))   !/tlt(nn)
         tlroot=tlroot+tl(i)
         tlrootrdf=tlrootrdf+tl(i)*rdf(i)
         endif
  280 CONTINUE
         if (tlroot.gt.0.0d0) avg_H=avg_H/tlroot
         if (tlroot.gt.0.0d0) avg_Hrdf=avg_Hrdf/tlrootrdf
         if (tlroot.gt.0.0d0) avg_swrdf=avg_swrdf/tlrootrdf*tlroot
C
C     ..UPDATE FOR NITROGEN UPTAKE
      IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
        UPPASS=TPASS*PWRTS*1.0D-3
        UPACT=TACTVE*PWRTS*1.0D-3
        UPTOT=TNITUP*PWRTS*1.0D-3
        UPDMD=DMDNIT*PWRTS*1.0D-3
        UPPLUX=UPLUX*PWRTS*1.0D-3
        IF(UPTOT.GT.0.0D0) THEN
          UPRAT=UPACT/UPTOT
        ELSE
          UPRAT=0.0D0
        ENDIF
c added by Liwang Ma, July 12, 2005
      CALL CDATE(JDAY,ID,IM,IYYY)
c
        WRITE(72,1400) JDAY,UPPASS,UPACT,UPTOT,UPRAT,UPDMD,UPPLUX
C     +                 ,im,id,iyyy
      ENDIF
C
C     ..SAVE SOME NITROGEN MASS BALANCE VARIABLES UG/CM^2 ==> KG/HA
        TLSEP(1)=DTCHEM(9)*0.1D0
        TLDRN(1)=DTDCHEM(9)*0.1D0
        TLLAT(1)=DTLCHEM(9)*0.1D0
        TLSEP(2)=DTCHEM(10)*0.1D0
        TLDRN(2)=DTDCHEM(10)*0.1D0
        TLLAT(2)=DTLCHEM(10)*0.1D0
        TLSEP(3)=DTCHEM(12)*0.1D0
        TLDRN(3)=DTDCHEM(12)*0.1D0
        TLLAT(3)=DTLCHEM(12)*0.1D0
C
C     ..SAVE SOME PESICIDE MASS BALANCE VARIABLES UG/CM^2 ==> KG/HA
      DO 290 IP=1,NPEST
        TLPLC(IP)=(DTCHEM(IP+12)+DTDCHEM(IP+12)+DTLCHEM(IP+12))*0.1D0
        TLPDT(IP)=DTCHEM(IP+12)*0.1D0
        TLPDTD(IP)=DTDCHEM(IP+12)*0.1D0
        TLPDTL(IP)=DTLCHEM(IP+12)*0.1D0
        TLPDMA(IP)=SCUPCH(IP+12)*0.1D0
  290 CONTINUE
C
C     ..SEND INFORMATION TO OUTPUT SECTION
      IF(NSC.GE.1) THEN
        CALL SGATE(TDAY,1,TRFDD)
        CALL SGATE(TDAY,2,TCII)
        CALL SGATE(TDAY,3,CC(IBRKTH,9))
        CALL SGATE(TDAY,5,ACTEVP)
        CALL SGATE(TDAY,6,ACTTRN)
        CALL SGATE(TDAY,8,CC(IBRKTH,13))
        CALL SGATE(TDAY,9,CC(IBRKTH,14))
        CALL SGATE(TDAY,10,CC(IBRKTH,15))
        DO 300 I=1,MXPEST
          CALL SGATE(TDAY,75+I,BKCHEM(12+I))
          CALL SGATE(TDAY,91+I,DTDCHEM(12+I))
  300   CONTINUE
        CALL SGATE(TDAY,13,(1.0D0-FT))
        CALL SGATE(TDAY,22,(ACTEVP+ACTTRN))
        CALL SGATE(TDAY,35,TROI)
        CALL SGATE(TDAY,36,TROI)
        if (ihourly.eq.0) then
        CALL SGATE(TDAY,39,-EVAP*24.0D0)
        CALL SGATE(TDAY,40,-PTRANS*24.0D0)
        else if (ihourly.eq.1) then
        CALL SGATE(TDAY,39,-totEVP)
        CALL SGATE(TDAY,40,-totpet)
        endif
        CALL SGATE(TDAY,42,DTSEEP)
        CALL SGATE(TDAY,44,DTCHEM(9))
C        CALL SGATE(TDAY,46,ACCPO4)   !PO4 IS NOT SIMULATED SO FAR
        CALL SGATE(TDAY,47,BRKH2O)
        CALL SGATE(TDAY,48,T(IBRKTH))
        CALL SGATE(TDAY,75,BKCHEM(9))
        if (ihourly.eq.0) then
        CALL SGATE(TDAY,88,(-EVAP*24.0D0)+(-PTRANS*24.0D0))
        else if (ihourly.eq.1) then
        CALL SGATE(TDAY,88,(-totEVP)+(-totpet))
        endif
        CALL SGATE(TDAY,89,H2OTAB(2))
        CALL SGATE(TDAY,90,DTDSEEP)
        CALL SGATE(TDAY,91,DTDCHEM(9))
        CALL VGATE(TDAY,10,TQNO3)
        CALL VGATE(TDAY,39,TTUP)
        CALL VGATE(TDAY,40,TQFL)
        CALL VGATE(TDAY,36,TNUP)
      ENDIF
      
      DO i = 1,Nnode
        m12flow(i) = TQFL(i)      ! DEBASIS
      END DO
      
      m11flow =  FMP
C
C     ..SETUP MSEA SPECIFIC OUTPUT
      OMSEA(3)=DAYRAIN
      OMSEA(61)=H2OTAB(2)
      OMSEA(74)=FDEP(1)
      OMSEA(76)=DTLAT
      if (ihourly.eq.0) then
c      OMSEA(9)=(-EVAP*24.0D0)+(-PTRANS*24.0D0)
      OMSEA(8)=(-EVAP*24.0D0)
      OMSEA(9)=(-PTRANS*24.0D0)
      else if (ihourly.eq.1) then
c      OMSEA(9)=(-totpet)+(-totevp)
      OMSEA(8)=(-totevp)
      OMSEA(9)=(-totpet)
      endif
      sumPT=omsea(9)
      sumPET=omsea(8)+omsea(9)
      sumrain=dayrain
      OMSEA(30)=(DTCHEM(9)+DTCHEM(10)+DTCHEM(12))*0.1D0
      OMSEA(31)=(DTDCHEM(9)+DTDCHEM(10)+DTDCHEM(12))*0.1D0
      OMSEA(75)=(DTLCHEM(9)+DTLCHEM(10)+DTLCHEM(12))*0.1D0
      DO 310 IP=1,MXPEST
        OMSEA((8+IP)*5+3)=DTDCHEM(12+IP)*0.1D0
        OMSEA((8+IP)*5+4)=DTCHEM(12+IP)*0.1D0
  310 CONTINUE
        OMSEA(116)=DTLCHEM(12+1)*0.1D0
        OMSEA(117)=DTLCHEM(12+2)*0.1D0
        OMSEA(118)=DTLCHEM(12+3)*0.1D0
C
      OMSEA(81) = MAX(0.0D0,RET_DAY_T)
      OMSEA(82) = MAX(0.0D0,RET_DAY_S)
c      IF ((RET_DAY_T).GT.0.0D0.and.OMSEA(9).GT.0.0D0) THEN
      OMSEA(83) = (OMSEA(8)+OMSEA(9))  !/RET_DAY_T
      OMSEA(84) = (OMSEA(6)+OMSEA(7))  !/RET_DAY_T
c	ELSE
c      OMSEA(86)=0.0D0
c      OMSEA(88)=0.0D0
c	ENDIF
c       IF ((RET_DAY_S).GT.0.0D0.AND.OMSEA(9).GT.0.0D0) THEN
c      OMSEA(87) = (OMSEA(8)+OMSEA(9))/RET_DAY_S
c      OMSEA(89) = (OMSEA(6)+OMSEA(7))/RET_DAY_S
c	ELSE
c      OMSEA(87)=0.0D0
c      OMSEA(89)=0.0D0
c	ENDIF
c
c       write (*,*) totmelt,subl,dsnow
      wtdep = real(h2otab(2))
      OMSEA(94)=FDEPTH
      OMSEA(95)=TDEPTH
      OMSEA(114)=1.0d0-CS
C     ... FINISHED
C     print out stress related water balance stuff
      write (unit=777,fmt=707) iyyy,jday,pet,acttrn,AVG_hroot,
     +       avg_H,avg_sw,trwup,acttrndum,RET_day_T,SumCropET,
     +       avg_Hrdf,avg_swrdf
  707 format (i8,i6,12(f15.4,2x))

      !-----------MEMORY-----------------------------------------------------
      CALL PHYSCL_Memory('PUT', PERIOD, SPAN, DELT, START, NBPR, COR, 
     +    TTRO, STMSEG, ND, FIRST5, JSTDAY, TSTART, OLDSTR, TRFDD,
     +    TCII, TROI, NBPRI, CANIRR, HROOT, RNDR, SNP,
     +    SSTART, PKTEMP, hrootdummy,
     +    IDIMIYOLD,wthour,totalsub)

      RETURN
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
111		format (i3,2x,i2,2x,i4,9(4x,f5.0),9x,f5.0,14x,f5.0,13x,f5.0,
     *            10x,f5.0)
112		format (i3,2x,i2,2x,i4,9(3x,f6.0),28x,f6.0,30x,f6.0,9x,f6.0,
     *            9x,f6.0)
c121   FORMAT (1X,2I3,I5,100(F6.1,1x))
c131   FORMAT (1X,2I3,I5,100(F6.3,1x))
132   format (1x,i3,i4,i5,25(f6.2))
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
 1000 FORMAT(/1X,24('#'),/' MASS BALANCE SUMMARY FOR STORM: ',I4,/1X,24(
     +    '#'),/' STORAGE BEFORE STORM (CM)',T50,F15.4,/' INPUT:',/
     +    '        PRECIPITATION (CM)',T50,G15.4,/' OUTPUT:',/4X,
     +    'TOTAL SURFACE RUNOFF (CM)',T50,G15.4,/4X,
     +    'TOTAL SEEPAGE OUT DRAIN (CM)',T50,G15.4,/4X,
     +    'TOTAL SEEPAGE OUT BOTTOM (CM)',T50,G15.4,/
     +    ' STORAGE AFTER STORM (CM)',T50,F15.4,/T50,15('-'),/
     +    ' MASS BALANCE:',T50,G15.4)
 1100 FORMAT(/1X,34('*'),/' INST MASS BALANCE SUMMARY FOR TIME:',F8.4,5
     +    X,'DELT = ',F10.8,4X,'WT DEPTH: ',F6.1/1X,34('*'),
     +    /' BEGINNING MASS NO3-N ==>',
     +    T45,F25.14,/'        NO3-N LOST TO SEEPAGE ==>',T40,F25.14,/
     +    '        NO3-N LOST TO DRAINAGE ==>',T40,F25.14,/
     +    '        NO3-N LOST TO UPTAKE ==>',T40,F25.14,/
     +    ' ENDING MASS NO3-N ==>',T45,F25.14,/T45,15('-'),/
     +    ' CHEMICAL MASS BALANCE  ===',T45,F25.14,//
     +    ' STORAGE FROM PREVIOUS TIME (CM)',T50,F25.14,//' OUTPUT:',/
     +    '        (POTENTIAL EVAPORATION) (CM)',T50,F25.14,/
     +    '        EVAPORATION FROM SOIL SURFACE (CM)',T50,F25.14,/
     +    '        PLANT UPTAKE FOR TRANSPIRATION (CM)',T50,F25.14,/
     +    '        (POTENTIAL DEEP SEEPAGE) (CM)',T50,F25.14,/
     +    '        DEEP SEEPAGE OUT OF PROFILE (CM)',T50,F25.14,/
     +    '        SEEPAGE OUT OF DRAIN (CM)',T50,F25.14,/
     +    '        EXCESS DRAINAGE (CM)',T50,F25.14,//
     +    ' STORAGE AT END OF CURRENT DAY (CM)',T50,F25.14,/T50,15('-'),
     +    /' MASS BALANCE AT END OF TIME STEP:',T50,F25.14)
C     +    ' STORAGE FROM PREVIOUS TIME (CM)',T50,F15.4,//' OUTPUT:',/
C     +    '    (POTENTIAL EVAPORATION) (CM)',T50,G15.4,/
C     +    '    EVAPORATION FROM SOIL SURFACE (CM)',T50,G15.4,/
C     +    '    PLANT UPTAKE FOR TRANSPIRATION (CM)',T50,G15.4,/
C     +    '    (POTENTIAL DEEP SEEPAGE) (CM)',T50,G25.14,/
C     +    '    DEEP SEEPAGE OUT OF PROFILE (CM)',T50,G25.14,/
C     +    '    SEEPAGE OUT OF DRAIN (CM)',T50,G25.14,/
C     +    '    EXCESS DRAINAGE (CM)',T50,G25.14,//
C     +    ' STORAGE AT END OF CURRENT DAY (CM)',T50,F25.14,/T50,15('-'),
C     +    /' MASS BALANCE AT END OF TIME STEP:',T50,G15.4)
C
C     1200 FORMAT (1X,F10.5,2X,F5.0,2X,F10.2,5X,F5.4,5X,F9.3,3G14.3)
 1200 FORMAT(1X,F11.1,2X,F6.1,2X,F10.2,5X,F5.4,5X,G9.3,3G14.3)
 1300 FORMAT('+',A40)
 1400 FORMAT(I5,6(2x,F9.4),3x,i2,'/',i2.2,'/',i4)
 777  FORMAT (I6,I4,I4,I6,7(F10.3),2(F10.5))
 780  FORMAT (I6,I4,I4,I6,I4,6(F10.3),2(F10.5))
      END
C
      SUBROUTINE MASSBL(MDAY,IMAGIC,TL,THETA,NN,DAYTIM,DTOLD,DTEVAP,
     +    DTSINK,DTSEEP,DTDSEEP,DTLAT,TRFDD,TROI,FMP,CII,DTCHEM,CMPS,
     +    CNVG,DTDCHEM,DTLCHEM,ICNVG,BIGCNV,OMSEA,AIRR,SMELT,SNRO,JDAY,
     +    IYYY,jeday,DTQSN,THETAI,ishaw,SCUPCH,dsubirr,jbday)
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
C       CII        I  INFILTRATION AMT. AT CURRENT TIME STEP [CM]
C       CMPS   I  CUMM. WATER SEEP OUT BOTTOM OF PROFILE FROM
C             MACROPORES [CM]
C       CNVG   I
C       DAYTIM     I  TIME OF DAY [0..24 HRS]
C       DTCHEM     I  TOTAL AMOUNT OF CHEMICAL LOST TO SEEPAGE
C             CORRESPONDS TO CC ORDER [UG/CM^2]
C       DTEVAP     I
C       DTMASS     L
C       DTOLD I/O
C       DTSEEP     I
C       DTSINK     I
C       DTSTOR     L
C       FL         L
C       FMP        I  CUMMULATIVE FLOW FROM MACROPORES [CM]
C       I    L    INDEX VARIABLE
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       MDAY   I
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       TCHEM  L
C       TDTMAS     L
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TRFDD  I  TOTAL RAINFALL DEPTH [CM]
C       TROI   I
C       TTCII  L
C       TTET   L
C       TTFMP  L
C       TTRFDD     L
C       TTROI  L
C       TTSEEP     L
C
C       COMMENTS:
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
      PARAMETER(MXCHEM=15,MXANA=135,MXNOD=300)
c     jak
      PARAMETER (RHOL=1.0D3,RHOI=9.2D2)
      DIMENSION THETA(MXNOD),TL(MXNOD),DTCHEM(MXCHEM),TCHEM(MXCHEM),
     +    DTDCHEM(MXCHEM),DTLCHEM(MXCHEM),OMSEA(MXANA),THETAI(MXNOD),
     +    SCUPCH(MXCHEM)
      LOGICAL FL,CNVG
      SAVE TDTMAS,TTFMP,TTCII,TTRFDD,TTROI,TTET,TTSEEP,TTDSEEP,TCHEM,FL,
     +    TTIRR,TTEVAP,TTRANS,TTLAT
      SAVE YTRFDD,YTROI,YTET,YTSEEP,YTDSEEP,
     +    YTIRR,YTEVAP,YTRANS,YTLAT,Ydtadj,Ydeltheta,YTORG
      common/maxbal/IDmxH2O,IMmxH2O,IYYYmxH2O,IDmxN,IMmxN,IYYYmxN,
     +              IDmxC,IMmxC,IYYYmxC,IDmxP,IMmxP,IYYYmxP,
     +              DTMASSmxH2O,BalmxN,BalmxC,BalmxP,IWCINIT1
     +            ,ireset1
C-RZ Liwang Ma
      SAVE TTE,TTT,dtadj,tdtadj,tdeltheta
      DATA TTE, TTT /0.0D0, 0.0D0/,TTQSN/0.0D0/
C-RZ Liwang Ma
      DATA TDTMAS,TTFMP,TTCII,TTRFDD,TTROI,TTET,TTSEEP,TTDSEEP,TTIRR,
     +    TCHEM /0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     +    MXCHEM*0.0D0/,TTEVAP,TTRANS,TTLAT/0.0D0,0.0D0,0.0D0/
      DATA FL /.TRUE./
      data tdtadj/0.0d0/,tdeltheta/0.0d0/
C-GH   Liwang Ma
      COMMON/WATER_C/TRFDD_C,AIRR_C,SMELT_C,TROI_C,DTSEEP_C,TTRFDD_C,
     +               TTIRR_C
      common/sumet/sumAET,sumCropET,sumPET,sumPT,sumAT,sumrain,deltheta
      common/gleams/rzrainfall,rzirrigation,rzrunoff
C-GH    Liwang Ma
C
C  ... CALCULATE DAILY MASS BALANCE
      DTSTOR=0.0D0
      DTICE=0.0D0
      if (iwcinit1.eq.1) then
       dtadj=dtadj+dtold-dtorg
       IF (dtold.NE.dtorg) THEN
        CALL CDATE(JDAY,ID,IM,IYYY)
        write(unit=666,fmt=672) JDAY,ID,IM,IYYY,dtadj
        ENDIF
       dtold=dtorg
c       iwcinit1=0   !reset later
      else
       dtadj=0.0d0
      endif
      DO 10 I=1,NN
        if (ishaw.eq.1) then
        DTSTOR=DTSTOR+ TL(I) * (theta(i)+thetai(I)*rhoi/rhol)
        DTICE=DTICE+ TL(I) * (thetai(I)*rhoi/rhol)
        else
        DTSTOR=DTSTOR+TL(I)*THETA(I)
        endif
   10 CONTINUE
C INITIATE VARIABLES FOR YEARLY WATER BALANCE
      CALL CDATE(JDAY,ID,IM,IYYY)
      IF ((JDAY.EQ.JBDAY).OR.((IM.EQ.1).AND.(ID.EQ.1))) THEN
      YTRFDD=0.0D0
      YTROI=0.0D0
      YTET=0.0D0
      YTSEEP=0.0D0
      YTDSEEP=0.0D0
      YTIRR=0.0D0
      YTEVAP=0.0D0
      YTRANS=0.0D0
      YTLAT=0.0D0
      Ydtadj=-dtadj
      Ydeltheta=0.0D0
      YTORG=DTOLD
      ENDIF
      
      DTMASS=DTOLD+TRFDD-TROI-DTEVAP-DTSINK-DTSEEP-DTDSEEP-DTLAT-DTSTOR
     +       +deltheta   !+dsubirr
c      IF (DTMASS.LT.1.0D-10) DTMASS=0.0D0
c-RZ Liwang Ma
c      IF(((IMAGIC.LE.-1.AND.IMAGIC.GE.-2).OR.IMAGIC.EQ.-10).AND.DAYTIM
c     +    .GE.24.0D0) THEN
      IF (DAYTIM .GE.24.0D0) THEN
        CALL CDATE(JDAY,ID,IM,IYYY)
	  if (abs(dtmass).ge.abs(DTMASSmxH2O)) then
            DTMASSmxH2O=dtmass
            IDmxH2O=id
		  IMmxH2O=im
		  IYYYmxH2O=iyyy
        endif
        WRITE(9,1800) JDAY,ID,IM,IYYY,DTOLD,TRFDD,TROI,DTEVAP,DTSINK,
     +      DTSEEP,DTDSEEP,DTLAT,DTSTOR,DTMASS
c        IF (ABS(DTMASS/DTSTOR).GT.1.0D-6) THEN
        IF (ABS(DTMASS).GT.1.0D-2) THEN
	    CALL DAYJACK(0,0,0,2)  !Liwang Ma
          WRITE (666,FMT=667) ID, IM, IYYY, jday, DTMASS !Liwang Ma
667     FORMAT (I4,I4,I6,' (DOY ',i3,')',
     +G15.6,' ===>H2O BALANCE PROBLEM')
        ENDIF
        IF (ABS(DTMASS).GT.1.0D-6) THEN
c          CALL DAYJACK(0,0,0,2)
          IF (CNVG) THEN
            WRITE(9,1700)
          ELSE
            WRITE(9,1400)
            ICNVG=ICNVG+1
            BIGCNV=MAX(BIGCNV,ABS(DTMASS))
          ENDIF
        ENDIF
        TDTMAS=TDTMAS+DTMASS
        TTFMP=TTFMP+FMP
        TTCII=TTCII+CII
c        TTRFDD = TTRFDD + TRFDD   !take out irrigation from TRFDD
        TTRFDD = TTRFDD + TRFDD - MAX(AIRR-SMELT,0.0D0)
        TTIRR=TTIRR+MAX(AIRR-SMELT,0.0D0)
        TTROI=TTROI+TROI
        TTET=TTET+DTEVAP+DTSINK
        TTEVAP=TTEVAP+DTEVAP
        TTRANS=TTRANS+DTSINK
        TTLAT=TTLAT+DTLAT
        TTSEEP=TTSEEP+DTSEEP
        TTDSEEP=TTDSEEP+DTDSEEP
        TDTADJ=TDTADJ+DTADJ
        tdeltheta=tdeltheta+deltheta
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        YTRFDD = YTRFDD + TRFDD - MAX(AIRR-SMELT,0.0D0)
        YTIRR=YTIRR+MAX(AIRR-SMELT,0.0D0)
        YTROI=YTROI+TROI
        YTET=YTET+DTEVAP+DTSINK
        YTEVAP=YTEVAP+DTEVAP
        YTRANS=YTRANS+DTSINK
        YTLAT=YTLAT+DTLAT
        YTSEEP=YTSEEP+DTSEEP
        YTDSEEP=YTDSEEP+DTDSEEP
        YDTADJ=YDTADJ+DTADJ  ! not needed for yearly water balance
        Ydeltheta=Ydeltheta+deltheta
c	jak
        TTQSN = TTQSN + DTQSN  !snow melt?
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
C-GH
        TRFDD_C  = TRFDD
        AIRR_C   = AIRR
        SMELT_C  = SMELT
        TROI_C   = TROI
        DTSEEP_C = DTSEEP + DTLAT + DTDSEEP  !INCLUDE TILE DRAIN, LATERAL FLOW AND DEEP DRAIANGE
        TTRFDD_C = TTRFDD
        TTIRR_C  = TTIRR_C+MAX(AIRR-SMELT,0.0D0)
        DUR_C = DUR
C-GH
        IF(FL) THEN
          WRITE(78,1300)
          WRITE(77,1200)  IYYY
          WRITE(77,1000) MDAY,DTOLD
          DTORG=DTOLD
          FL=.FALSE.
        ENDIF
C
C       ..WRITE STUFF OUT TO ACCWAT.OUT
        DELTSTOR=DTORG-DTSTOR+TTRFDD+TTIRR-TTROI-TTET-TTSEEP-TTDSEEP
     +           -tdtadj-ttlat+tdeltheta
        WRITE(77,1000) MDAY,DTSTOR,TTRFDD,TTIRR,TTROI,TTET,TTSEEP,
     +      TTDSEEP,TTFMP,TTCII,CMPS,DELTSTOR
        DTOLD=DTSTOR
        DO 20 I=1,MXCHEM
          TCHEM(I)=TCHEM(I)+DTCHEM(I)+DTDCHEM(I)+DTLCHEM(I)
   20   CONTINUE
C
C       ..WRITE STUFF OUT TO CLEACH.OUT
        WRITE(78,1100) MDAY,(TCHEM(I),I=1,MXCHEM)
      ELSEIF(IMAGIC.LT.0.AND.DAYTIM.GE.24.0D0) THEN
C
C       /////////////////////////////////////////////
C       WRITE OUT MASSBALANCE STUFF FOR DEFAULT TIMES
C       /////////////////////////////////////////////
        CALL CDATE(JDAY,ID,IM,IYYY)
        IF(ABS(DTMASS).GT.1.0D-6) THEN
          WRITE(9,1500) JDAY,ID,IM,IYYY,DTMASS
        ELSE
          WRITE(9,1600) JDAY,ID,IM,IYYY,DTMASS
        ENDIF
        IF(.NOT.CNVG) THEN
          ICNVG=ICNVG+1
c          IF(ABS(DTMASS).GT.1.0D-6) ICNVG=ICNVG+1
          BIGCNV=MAX(BIGCNV,ABS(DTMASS))
        ENDIF
        DTOLD=DTSTOR
      ENDIF
C
      IF(DAYTIM.GE.24.0D0) THEN
        OMSEA(2)=DTSTOR
        OMSEA(91)=DTICE
C       OMSEA(3) = TRFDD - AIRR + SMELT
        OMSEA(4)=MAX(AIRR-SMELT,0.0D0)
        OMSEA(5)=TRFDD-TROI
        OMSEA(6)=DTEVAP
        OMSEA(7)=DTSINK
c        OMSEA(8)=DTEVAP+DTSINK  !use this slot for potential evapo
        sumAT=omsea(7)     
        sumAET=omsea(6)+omsea(7)     
        OMSEA(10)=DTSEEP
        OMSEA(11)=DTDSEEP
c corrected by Liwang Ma, March 7, 2005
c        OMSEA(12)=TROI+SNRO
        OMSEA(12)=TROI
        OMSEA(105)=SMELT
C YEARLY WATER BALANCE
C
        IF (MDAY.EQ.JEDAY) THEN
        YDTMASS=YTORG+YTRFDD+YTIRR-YTROI-YTEVAP-YTRANS-YTSEEP
     +         -YTDSEEP-YTLAT-DTSTOR-Ydtadj+Ydeltheta
        WRITE(9,1902) IYYY,YTORG,YTRFDD,YTIRR,YTROI,YTEVAP,YTRANS,
     +      YTSEEP,YTDSEEP,YTLAT,DTSTOR,YDTMASS
C
        TDTMASS=DTORG+TTRFDD+TTIRR-TTROI-TTEVAP-TTRANS-TTSEEP
     +         -TTDSEEP-TTLAT-DTSTOR-tdtadj+tdeltheta
        WRITE(9,1900) DTORG,TTRFDD,TTIRR,TTROI,TTEVAP,TTRANS,
     +      TTSEEP,TTDSEEP,TTLAT,DTSTOR,TDTMASS
        WRITE(71,1900) DTORG,TTRFDD,TTIRR,TTROI,TTEVAP,TTRANS,
     +      TTSEEP,TTDSEEP,TTLAT,DTSTOR,TDTMASS
        WRITE(666,1901) TDTMASS
        ELSE IF ((IM.EQ.12).AND.(ID.EQ.31)) THEN
        YDTMASS=YTORG+YTRFDD+YTIRR-YTROI-YTEVAP-YTRANS-YTSEEP
     +         -YTDSEEP-YTLAT-DTSTOR-Ydtadj+Ydeltheta
        WRITE(9,1902) IYYY,YTORG,YTRFDD,YTIRR,YTROI,YTEVAP,YTRANS,
     +      YTSEEP,YTDSEEP,YTLAT,DTSTOR,YDTMASS
        ENDIF
      ENDIF
C
c        RzIrrigation = AIRR_C  !sab presumed to be in cm for this day-used in Gleams
c        RzRainfall = trfdd_C    !sab 5/03 - these link to gleams - in cm, must be in inches
c        RzRunoff   = troi_C     !sab conversion takes place in GLEAMSDailyErosion in Rz-Erosion
      RzIrrigation = AIRR    !MAX(AIRR - SMELT,0.0D0)  !sab presumed to be in cm for this day-used in Gleams
c      RzRainfall = TRFDD    !trfdd    !sab 5/03 - these link to gleams - in cm, must be in inches
c      RzRunoff   = troi     !sab conversion takes place in DailyErosion in Rz-Erosion
C
      RETURN
 1000 FORMAT(I5,10F9.3,2X,G11.4)
 1100 FORMAT(I6,20E13.6)
 1200 FORMAT('DAY:  day of the year',
     +     /'STOR:  Total soil water storage in the soil profile (cm)',
     +     /'RAIN:  Cumulative rainfall (cm)'
     +     /'IRR:   Cumulative irrigation water (cm)'
     +     /'RUNOFF: Cumulative runoff water (cm)'
     +     /'AET:    Cumulative evapotranspiration (cm)'
     +     /'SEEPAGE:  Cumulative seepage out of the soil profile (cm)',
     +     /'DRAIN:    Cumulative water loss to tile drain (cm)',
     +     /'MACRO:     Cumulative macropore water flow (cm)'
     +     /'INFIL: Cumulative water infiltrated at the surface (cm)',
     +     /'INFILSEEP:  Cumulative seepage out of macropores (cm)',
     +     /'DELTASTOR:      daily mass balance (cm)',//9x,I4,
     +     /2X,'DAY',T9,'STOR',T18,'RAIN',T28,'IRR',T36,'RUNOFF',T46,
     +    'AET',T53,'SEEPAGE',T64,'DRAIN',T73,'MACRO',T81,'INFIL',T88,
     +    'INFILSEEP',T99,'DELTASTOR')
 1300 FORMAT(1X,'DAY ',T13,'H',T26,'CA',T39,'NA',T52,'MG',T65,'CL',T78,
     +  'HCO3',T91,'SO4',T104,'AL',T115,'NO3-N',T128,'NH4-N',T143,'CO3',
     +    T154,'UREA-N',T167,'PEST #1',T180,'PEST #2',T193,'PEST #3'/
     +   97('='),'ug/cm2',97('='))
 1400 FORMAT(1X,'NOTE... RICHARDS EQUATION DIDNT CONVERGE - MASS',
     +    ' BALANCE MAY BE OFF')
 1500 FORMAT(I6,2X,'==>',I3,'/',I2,'/',I4,5X,'BALANCE ==>',G15.4,' ***')
 1600 FORMAT(I6,2X,'==>',I3,'/',I2,'/',I4,5X,'BALANCE ==>',G15.4)
 1700 FORMAT(1X,'NOTE... RICHARDS EQUATION DID CONVERGE BUT MASS',
     +    ' BALANCE IS OFF')
  672 FORMAT(1X,' WATER GAIN DUE TO RESETTING ON DAY: ',I6,2X,
     +    '==>',I3,'/',I2,'/',I4,5X,F15.4/)
 1800 FORMAT(/1X,34('*'),/' DAILY MASS BALANCE SUMMARY FOR DAY: ',I6,2X,
     +    '==>',I3,'/',I2,'/',I4,/1X,34('*'),/
     +    ' STORAGE FROM PREVIOUS DAY (CM)',T50,F15.4,//' INPUT:',/
     +    '    PRECIPITATION (CM)',T50,G15.4,/' OUTPUT:',/4X,
     +    'TOTAL SURFACE RUNOFF (CM)',T50,G15.4,/4X,
     +    'EVAPORATION FROM SOIL SURFACE (CM)',T50,G15.4,/4X,
     +    'PLANT UPTAKE FOR TRANSPIRATION (CM)',T50,G15.4,/4X,
     +    'DEEP SEEPAGE OUT OF PROFILE (CM)',T50,G15.4,/4X,
     +    'SEEPAGE OUT OF DRAIN (CM)',T50,G15.4,/4X,
     +    'LATERAL FLOW LOSS (CM)',T50,G15.4,//
     +    ' STORAGE AT END OF CURRENT DAY (CM)',T50,F15.4,/T50,15('-'),/
     +    ' MASS BALANCE AT END OF DAY:',T50,G15.4)
 1900 FORMAT(//1X,44('*'),
     +    /' WATER BALANCE SUMMARY FOR SIMULATION PERIOD' 
     +       /1X,44('*'),/
     + ' STORAGE AT BEGINNING OF SIMULATION(CM)',T50,F15.4,//' INPUT:',/
     + '    PRECIPITATION (CM)',T50,G15.4,/4X,
     + 'IRRIGATION (CM)',T50,G15.4,/' OUTPUT:',/4X,
     + 'TOTAL SURFACE RUNOFF (CM)',T50,G15.4,/4X,
     + 'EVAPORATION FROM SOIL SURFACE (CM)',T50,G15.4,/4X,
     + 'PLANT UPTAKE FOR TRANSPIRATION (CM)',T50,G15.4,/4X,
     + 'DEEP SEEPAGE OUT OF PROFILE (CM)',T50,G15.4,/4X,
     + 'SEEPAGE OUT OF DRAIN (CM)',T50,G15.4,/4X,
     + 'LATERAL FLOW LOSS (CM)',T50,G15.4,//
     + ' STORAGE AT ENDS OF SIMULATION (CM)',T50,F15.4,/T50,15('-'),/
     + ' WATER BALANCE AT END OF SIMULATION:',T50,G15.4/)
 1901 format (/'  WATER       BALANCE AT END OF SIMULATION:',T60,G15.6)
 1902 FORMAT(//1X,44('*'),
     +    /' WATER BALANCE SUMMARY FOR YEAR  ',I4, 
     +       /1X,44('*'),/
     + ' STORAGE AT BEGINNING OF YEAR (CM)',T50,F15.4,//' INPUT:',/
     + '    PRECIPITATION (CM)',T50,G15.4,/4X,
     + 'IRRIGATION (CM)',T50,G15.4,/' OUTPUT:',/4X,
     + 'TOTAL SURFACE RUNOFF (CM)',T50,G15.4,/4X,
     + 'EVAPORATION FROM SOIL SURFACE (CM)',T50,G15.4,/4X,
     + 'PLANT UPTAKE FOR TRANSPIRATION (CM)',T50,G15.4,/4X,
     + 'DEEP SEEPAGE OUT OF PROFILE (CM)',T50,G15.4,/4X,
     + 'SEEPAGE OUT OF DRAIN (CM)',T50,G15.4,/4X,
     + 'LATERAL FLOW LOSS (CM)',T50,G15.4,//
     + ' STORAGE AT ENDS OF YEAR (CM)',T50,F15.4,/T50,15('-'),/
     + ' WATER BALANCE AT END OF YEAR:',T50,G15.4/)
      END
C
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
      SUBROUTINE NWPRSTY(iq,THETA,THETAI,SOILHP,NN,NDXN2H,MAXHOR,PORI,
     1 AEF)
C
C======================================================================
C
C       PURPOSE: FIND NEW MAXIMUM PORE SPACE AVAILABLE TO BE FILLED
C                WITH LIQUID DUE TO CHANGEDS EITHER IN THETA_SAT
C                OR IN THE ICE CONTENT OF THE LAYER
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C
C       CALLED FROM: PHYSCL
C
C       PROGRAMMER:  KAREN JOHNSEN
C
C       VERSION:   1
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (RHOL=1.0D3,RHOI=9.2D2,MXNOD=300)
      DIMENSION NDXN2H(MXNOD),PORI(MXNOD),SOILHP(13,MAXHOR),
     1   THETAI(MXNOD),THETA(MXNOD)

      DO 10 I = 1,NN
        IH = NDXN2H(I)
        PORI(I) = SOILHP(6,IH)*AEF-thetai(i)*rhoi/rhol
        if ((thetai(i)*rhoi/rhol + theta(i) - soilhp(6,ih))
     1         .gt. 1.0d-3 .and. iq.ne.1) then
          write (*,20) i,soilhp(6,ih),theta(i),thetai(i)  !,pori(i)
          write(9,*) i,iq,aef*soilhp(6,ih),theta(i),thetai(i),
     1               thetai(i)*rhoi/rhol,pori(i),
     2               theta(i)+thetai(i)*rhoi/rhol
c         stop '  Ice formation is wrong, check NWPRSTY Subroutine'
          print*, '  Ice formation is wrong, check NWPRSTY Subroutine'
        endif
10    CONTINUE
20    format ('soil layer= ',i3,'  porosity= ',f10.7,'  liquid= ',f10.7,
     +      '  ice= ',f10.7)
      RETURN
      END
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      SUBROUTINE RECON(BARE,TR,STRM,SOILPP,MAXPTS,NBPR,THETA,CC,SLKS,
     +    CONCX2,XNU,RPOOL,FREUND)
C
C======================================================================
C
C       PURPOSE: ADJUST SURFACE AND SOIL PROPERTIES AFTER A RAIN STORM
C            EVENT
C
C       REF:  AFTER NTRM
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BARE   I  FRACTION BARE SURFACE (NOT COVERED BY RES.) [0..1]
C       CC        I/O TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C             CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C             1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C             5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C             9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C             12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CONCX2    I/O PEST. CONC. INVOLVED IN KINETIC PROCESSES [UG/G-S]
C       EINC   L
C       FDEP  I/O FRACTION OF TOTAL MACROPORES THAT ARE DEADEND [0..1]
C       I    L    INDEX VARIABLE
C       IH         L
C       IN         L  ENDING ROW INDEX OF TRICO AS DIMENSIONED IN
C       IP         L  INDEX FOR PESTICIDE NUMBER
C       J    L    INDEX VARIABLE
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXPTS     I
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MCPOR  I  TOTAL MACROPOROSITY OF HORIZON [CM3/CM3]
C       MICP   I  TOTAL MICROPOROSITY OF HORIZON [CM3/CM3]
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NBPR   I
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C               IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       OBD        I
C       OFDP   I
C       PBD        L
C       RAINP  L  TOTAL RAINFALL AMT [CM]
C       RANRAT     L  RAINFALL INTENSITY [CM/HR]
C       RBD        L
C       SF         P  SOIL SURFACE ENERGY SEALING FACTOR
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       SOILPP     I
C       STRM   I
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TOTEN  L  RAINFALL ENERGY [J/CM^2] (RAINFALL ENERGY IS THE
C             FRACTION OF THE ENERGY NORMAL TO THE SOIL
C             PER UNIT AREA OF SURFACE,  NON RESIDUE, OR CANOPY
C             PROTECTED SOIL)
C       TR         I  CUMMULATIVE TIME OF RAINFALL [HR]
C       XNU       I/O
C       YESMAC     I  INDICATS PRESENCE OF MACROPORES =1 YES, =0 NOT
C
C       COMMENTS:
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C             REDUCE
C             SETTL
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
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MXCHEM=15,MXPEST=3,
     +   MXAPP=200)
      PARAMETER(SF=1.0D0)
C
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
      DOUBLE PRECISION MCPOR
      INTEGER YESMAC
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      DIMENSION STRM(MAXPTS,2),SOILPP(8,MAXHOR),PBD(2),THETA(MXNOD),
     +    CC(MXNOD,MXCHEM),CONCX2(MXNOD,MXPEST),SLKS(MXNOD,MXPEST,2),
     +    XNU(MXNOD,25),RPOOL(MXNOD,2),FREUND(MXPEST)
C
C
      IF(OBD(1).GT.SOILPP(3,1).AND.NBPR.GT.0) THEN
C
C       ... CALCULATE STORM RAINFALL ENERGY
        TOTEN=0.0D0
        RAINP=0.0D0
        DO 10 I=1,NBPR
          IF(STRM(I,1).NE.0.0D0) THEN
            RANRAT=STRM(I,2)/STRM(I,1)
            CALL REDUCE(RANRAT,STRM(I,1),SF,BARE,TR,EINC)
            TOTEN=TOTEN+EINC
            RAINP=RAINP+STRM(I,2)
          ENDIF
   10   CONTINUE
C
C       ... ADJUST BULK DENSITY, POROSITY AND D.E.PORES IN TILLAGE ZONE
        DO 20 IH=1,2
          PBD(IH)=SOILPP(3,IH)
          IF(OBD(IH).GT.SOILPP(3,IH)) CALL
     +        SETTL(SOILPP(3,IH),SF,TOTEN,RAINP,OBD(IH),SOILPP(2,IH))
          RBD=SOILPP(3,IH)/PBD(IH)
          IF(OFDP(IH).GE.0.0D0) FDEP(IH)=MIN(FDEP(IH)*RBD,OFDP(IH))
   20   CONTINUE
C
C       .. RECALCULATE PEST. CONCENTRATIONS
        DO 50 I=1,NN
          IH=NDXN2H(I)
          TP=SOILPP(3,IH)*TL(I)/1.0D6
          IF(IH.LE.2) then
             if (OBD(IH).GT.pbd(ih)) THEN   !corrected by Liwang Ma, 5-14-2009
C
C           ..ADJUST PESTICIDE MASS-BASED CONCENTRATIONS
            DO 30 IP=1,MXPEST
              J=IP+12
C LIWNAG MA, 7-3-2006
              CC(I,J)=CC(I,J)*(THETA(I)+FSLKS(SLKS(I,IP,1),
     +             CC(I,J),FREUND(IP),theta(i),pbd(ih))*PBD(IH))/(
     +            THETA(I)+FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +            theta(i),soilpp(3,ih))
     +            *SOILPP(3,IH))
              CONCX2(I,IP)=CONCX2(I,IP)*PBD(IH)/SOILPP(3,IH)
   30       CONTINUE
C
C           ..ADJUST NUTRIENT MASS-BASED CONCENTRATIONS
            DO 40 IN=1,20
              IF(IN.LT.10.OR.IN.GT.12) XNU(I,IN)=XNU(I,IN)*PBD(IH)/   
     +            SOILPP(3,IH)
              IF(IN.EQ.1.OR.IN.EQ.2) RPOOL(I,IN)=XNU(I,IN)*TP
   40       CONTINUE
             ENDIF
             endif
   50   CONTINUE
C
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE REDUCE(RANRAT,TIMEI,SF,BARE,TR,ENRED)
C=======================================================================
C
C       PURPOSE: IS A FUNCTION TO COMPUTE THE TOTAL ENERGY AND THE
C         EFFECTIVE SEALING ENERGY OF RAINFALL.  RAINFALL ENERGY IS
C         FROM WISCHMEIER MODIFIED TO METRIC UNITS BY VAN DOREN AND
C         ALLMARAS, 1978.  SEALING ENERGY (REDUCE) IS ENERGY DIRECTLY
C         REACHING, AND NORMAL TO, THE SOIL SURFACE (PER UNIT AREA OF
C         THE SURFACE).      TOTAL ENERGY IS REDUCED BY BARE (FRACTION BARE
C         SURFACE) (VAN DOREN AND ALLMARAS, 1978), AND BY THE SURFACE
C         CONFIGURATION FACTOR (SF - LINDEN UNPUBLISHED, 1979), AND BY
C         THE CANOPY TRANSMISSION FRACTION (TR) TO OBTAIN SEALING
C         ENERGY.  SF REDUCES THE ENERGY FOR SEALING BECAUSE RAINDROP
C         ENERGY IS SPREAD OVER MORE AREA AND IS ALSO STRIKING AT
C         ANGLE ON ROUGH SURFACES (SF = COSINE MEAN ANGLE OF INCLI-
C         NATION/SURFACE AREA PER UNIT HORIZONTAL AREA).
C
C       REF:  LINDEN, IN NTRM ...
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BARE   I  FRACTION BARE SURFACE (NOT COVERED BY RES.) [0..1]
C       ENERGY     L  TOTAL RAINFALL ENERGY FOR THE EVENT (UP TO
C             THE TIME TIMEI) [J/CM^2]
C       ENRED I/O REDUCED ENERGY AVAILABLE FOR SEALING DURING
C             EVENT [J/CM^2]
C       RANRAT     I  RAINFALL INTENSITY [CM/HR]
C       SF         I  SOIL SURFACE ENERGY SEALING FACTOR
C       TIMEI  I  DURATION OF RAINFALL [HR]
C       TR         I  CUMMULATIVE TIME OF RAINFALL [HR]
C              NATION EFFECTS) (SEE SUBROUTINE SURFAC)
C
C       COMMENTS:
C          - EXTRACTED FROM NTRM WITH MINOR MODIFICATION;
C
C       CALLED FROM:
C
C       PROGRAMMER:  CHARLES S. HEBSON
C
C       VERSION:    2.0
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C ... CALC ENERGIES IN [J/CM^2]
      IF(RANRAT.LE.0.0D0) THEN
        ENERGY=0.0218D0
      ELSE
        ENERGY=(0.0218D0+0.0092D0*LOG10(RANRAT))*RANRAT*TIMEI
      ENDIF
      ENRED=ENERGY*SF*BARE*TR
C
      RETURN
      END
C
      SUBROUTINE SETTL(BDX,SF,TOTEN,RAINP,BDCOR,RHOP)
C=======================================================================
C
C       PURPOSE: ROUTINE FOR THE SETTLING OF THE SOIL DUE TO
C            RAINFALL AND SOIL WETTING BASED ON DATA FROM
C            R. BURWELL VIA PERSONAL COMMUNICATION.
C
C       REF:  D.R. LINDEN, IN NTRM ...
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       BDCOR  I  MAXIMUM (STABLE) BULK DENSITY [G/CM^3]
C       BDX       I/O BULK DENSITY AFFECTED BY SETTLING [G/CM^3]
C       FRAC   L
C       FRAC1  L
C       FRAC2  L
C       POR        L  POROSITY [0..1]
C       PORCOR     L  MAXIMUM (STABLE) POROSITY [0..1]
C       PORI   L  INITIAL POROSITY [0..1]
C       RAINP  I  TOTAL RAINFALL AMT [CM]
C       RHOP   I  SOIL PARTICLE DENSITY [G/CM^3]
C       SF         I  SOIL SURFACE ENERGY SEALING FACTOR
C       TOTEN  I  RAINFALL ENERGY [J/CM^2] (RAINFALL ENERGY IS THE
C             FRACTION OF THE ENERGY NORMAL TO THE SOIL
C             PER UNIT AREA OF SURFACE,  NON RESIDUE, OR CANOPY
C             PROTECTED SOIL)
C
C       COMMENTS:
C          - EXTRACTED FROM NTRM WITH MINOR MODIFICATION;
C          - PROGRAMMED BY D.R. LINDEN, JULY, 1981;
C
C       CALLED FROM:  RECON
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2.0
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PORCOR=1.0D0-(BDCOR/RHOP)
      PORI=1.0D0-(BDX/RHOP)
C     FRAC1      = EXP(-0.015D0*RAINP)
      FRAC1=EXP(-0.25D0*RAINP)
C     FRAC2      = 1.0D0 - EXP( -0.193D0*SF*TOTEN)
      FRAC2=1.0D0-EXP(-0.01D0*SF*TOTEN)
      FRAC=FRAC1-FRAC2
      IF(FRAC.LT.0.0D0) FRAC=0.0D0
      POR=PORI-(PORI-PORCOR)*(1.0D0-FRAC)
      IF(POR.LT.PORCOR) POR=PORCOR
C
      BDX=(1.0D0-POR)*RHOP
C
      RETURN
      END
C
C
      SUBROUTINE STMINP(DUR,MAXBP,NBPR,STORM,TS0,TSE,INP4,JSTDAY,JDAY,
     +    IYYY,IMAGIC,RFDNEW,METMOD)
C
C======================================================================
C
C       PURPOSE:  READ BREAKPOINT RAINFALL DATA FOR A SINGLE EVENT
C           FROM A DATA FILE IN OPUS FORMAT
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       DISAMT     L
C       DISTIM     L
C       DUR       I/O STORM DURATION [HR]
C       I    L    INDEX VARIABLE
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       INP4   I  POINTER TO BRKPNT.DAT DATAFILE
C       IPASS  L
C       IYYY   I  --YEAR (4 DIGITS).
C       IYR        L  --TEMPORARY STORAGE OF CURRENT YEAR [1900-1999]
C       JDAY   I  JULIAN DAY   [1..366]
C       JSTDAY    I/O
C       MAXBP  I  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       NBP        L  NUMBER OF BREAK POINTS
C       NBPR  I/O
C       STORM I/O COL 1: INCREMENTAL BREAKPOINT TIMES
C             ("DELTA T"'S) [HR]; INCREMENTAL BREAKPOINT
C             RAINFALL DEPTHS [CM]
C       TS0       I/O STORM STARTING CLOCK TIME [HR], 0<TS0<24
C       TSE       I/O STORM CLOCK ENDING TIME [HR], 0<TSE<24
C
C       COMMENTS:
C          - THIS ASSUMES BREAKPOINT DATA IS IN OPUS FORMAT
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C             ECHO
C
C       CALLED FROM: DAILY
C
C       PROGRAMMER:   CHARLES S. HEBSON
C
C       VERSION:   2.15
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION METMOD(8,12)
      DIMENSION STORM(MAXBP,2)
      LOGICAL IPASS,DISTIM,DISAMT
      SAVE IPASS,JOSTDAY,ETIME,JOSTIY
      DATA IPASS /.TRUE./,JOSTDAY /0/,ETIME /0.0D0/,JOSTIY /0/
C
C     ... FIRST TIME THROUGH ROUTINE - READ HEADER INFO
      IF(IPASS) THEN
C       OPEN (INP4,FILE='BRKPNT.DAT',STATUS='UNKNOWN')
        CALL ECHO(INP4)
        READ(INP4,*) IYR
      ENDIF
C
C     ... READ CONTROL INFO FOR A BREAKPOINT EVENT
   10 READ(INP4,*,END=30) IYR,JSTDAY,NBP,ITYR,RFDNEW
C
C     ... CHECK THAT NO. BREAKPOINTS DOESN'T EXCEED SIZE OF EVENT
C     ... DATA STORAGE ARRAY
      NBPR=NBP
      IF(NBP.GT.MAXBP) THEN
        PRINT*,' >>> FATAL ERROR READING BRKPNT.DAT <<<'
        PRINT*,'DAY ',JSTDAY,' YEAR',IYR
        PRINT*,'NUMBER OF BREAK POINT PAIRS EXCEEDS MAXIMUM BOUND',MAXBP
        PRINT*
        STOP
      ELSEIF(NBP.EQ.0.0D0) THEN
        PRINT*,' >>> FATAL ERROR READING BRKPNT.DAT <<<'
        PRINT*,'DAY ',JSTDAY,' YEAR',IYR
        PRINT*,'NUMBER OF BREAK POINT PAIRS EQUALS ZERO',NBP
        PRINT*
        STOP
      ENDIF
C
C     ... READ BREAKPOINT DATA
      READ(INP4,*,END=30) (STORM(I,2),STORM(I,1),I=1,NBPR)
C     ..FIND THE CORRECT DAY
        IF (RFDNEW.LT.1.0D-2) GOTO 10
C
      IF(IMAGIC.NE.2) THEN
        IF(IYR.LT.IYYY.AND.IPASS) GOTO 10
        IF((IYR.EQ.IYYY.AND.JSTDAY.LT.JDAY).AND.IPASS) GOTO 10
        IF (RFDNEW.LT.1.0D-2.AND.IPASS) GOTO 10
      ELSE
C
C       .. TEST DATA TO SEE IF IT'S CORRECT
        CALL CDATE(JSTDAY,ID,IM,IYR)
c        WRITE(*,1400) JSTDAY,ID,IM,IYR
        IF(STORM(1,1).GT.1440.0D0) WRITE(*,1000) JSTDAY,IYR
        IF(STORM(NBPR,1).GT.2888.0D0) WRITE(*,1100) JSTDAY,IYR
        IF(STORM(1,2).GT.0.0D0) WRITE(*,1200) JSTDAY,IYR
        IF(JSTDAY.EQ.JOSTDAY.AND.STORM(1,1).LT.ETIME) WRITE(*,1600)
     +      JSTDAY,IYR
        IF(JSTDAY.EQ.JOSTDAY+1.AND.ETIME.GE.1440.D0.AND.STORM(1,1).LE.
     +      ETIME-1440.0D0) WRITE(*,1600) JSTDAY,IYR
        IF(JSTDAY.LT.JOSTDAY.AND.IYR.EQ.JOSTIY) WRITE(*,1700) JSTDAY,IYR
        JOSTDAY=JSTDAY
        JOSTIY=IYR
        ETIME=STORM(NBPR,1)
      ENDIF
      IPASS=.FALSE.
c
C     ... SAVE STARTING & ENDING TIMES (AS [HR])
      TS0=STORM(1,1)/60.0D0
      TSE=STORM(NBPR,1)/60.0D0
      DUR=TSE-TS0
C check for rainfall irregularity
      CALL CDATE(JSTDAY,ID,IM,IYR)
      DO 21 I=1,NBPR-1
        if ((STORM(I+1,1)-STORM(I,1)).le.0.0d0 .and.
     +     (STORM(I+1,2)-STORM(I,2)).gt.0.0d0) then
c            STORM(I+1,1)=STORM(I,1)+10.0d0    !correct when there is rainfall accumulation but no time interval.
        write (*,1401) id,im,iyr,jstday
        stop
        endif
        if ((STORM(I+1,2)-STORM(I,2))*2.54d0/
     +       ((STORM(I+1,1)-STORM(I,1))/60.d0).gt.1.0d0
     +      .and.(STORM(I+1,2)-STORM(I,2)).lt.0.01d0) then
        write (*,1402) id,im,iyr,jstday
        endif
        if ((STORM(I+1,2)-STORM(I,2))*2.54d0/
     +       ((STORM(I+1,1)-STORM(I,1))/60.d0).lt.1.0d-2) then
        write (*,1402) id,im,iyr,jstday
        endif
c the following is added back on to correct irregular rainfall event by adjusting the time only. 
c        if ((STORM(I+1,1)-STORM(I,1)).lt.5.0d0 .and.
c     +     (STORM(I+1,2)-STORM(I,2)).lt.0.05d0) then
c        write (*,1403) id,im,iyr,jstday
c	    STORM(I+1,1)=STORM(I,1)+10.0d0   !limited to minimum 10 min rainfall event.
c          do j=i+2,nbpr
c           STORM(J,1)=STORM(J,1)+10.0d0
c          enddo
c        endif
c        if ((STORM(I+1,1)-STORM(I,1)).ge.60.0d0 .and.
c     +     (STORM(I+1,2)-STORM(I,2)).le.0.02d0) then
c        write (*,1404) id,im,iyr,jstday
c         ABC=STORM(I+1,1)-STORM(I,1)
c	    STORM(i+1,1)=STORM(i,1)+30.0d0   !half the duration
c        DO J=I+2,NBPR
c          STORM(J,1)=STORM(J,1)-(ABC-30.D0)
c        ENDDO
c        endif
c end of rainfall adjustements
   21 CONTINUE
C     ... CONVERT DATA TO INCREMENTAL TIMES AND DEPTHS; ALSO, CONVERT
C     ... UNITS: [MIN]==>[HR]  [IN]==>[CM]
      DISTIM=.FALSE.
      DISAMT=.FALSE.
      CALL CDATE(JDAY,ID,IM,IYYY)
      DO 20 I=1,NBPR-1
        STORM(I,1)=(STORM(I+1,1)-STORM(I,1))/60.0D0
        STORM(I,2)=(STORM(I+1,2)-STORM(I,2))*2.54D0
C
C       .. APPLY METEOROLOGY MODIFIERS
        STORM(I,2)=STORM(I,2)*METMOD(7,IM)*1.0D-2
C
        IF(STORM(I,1).LE.0.0D0) DISTIM=.TRUE.
        IF(STORM(I,2).LE.0.0D0) DISAMT=.TRUE.
   20 CONTINUE
      RFDNEW=STORM(NBPR,2)*2.54D0*METMOD(7,IM)*1.0D-2
C
C
C     .. DO DATA VERIFICATION IF NEEDED
      IF(IMAGIC.EQ.2) THEN
        IF(DISTIM) WRITE(*,1300) JSTDAY,IYR
        IF(DISAMT) WRITE(*,1500) JSTDAY,IYR
        GOTO 10
      ENDIF
C
C     ... DECREMENT EVENT COUNTER SINCE TIMES ARE NOW IN INCREMENTS
      NBPR=NBPR-1
      GOTO 40
C
   30 PRINT*
      PRINT*,' >>>> END OF BREAK-POINT DATA ENCOUNTERED'
      PRINT*
      IF(IMAGIC.EQ.2) THEN
        PRINT*,'   BRKPNT.DAT VERIFICATION COMPLETED'
        PRINT*,'     >>> PROGRAM TERMINATED <<<'
        STOP
      ENDIF
C
      TS0=0.0D0
      TSE=0.0D0
      DUR=0.0D0
      IYR=0
      JSTDAY=0
   40 RETURN
C     1000 FORMAT (A72)
 1000 FORMAT(2I6,' STORM START TIME > 1440 MINUTES ')
 1100 FORMAT(2I6,' STORM ENDING TIME > 2880 MINUTES ')
 1200 FORMAT(2I6,' INITIAL RAINFALL AMT IS > 0.0')
 1300 FORMAT(2I6,' TIME INCREMENTS ARE NOT INCREASING')
 1400 FORMAT(' TESTING ===>',I6,2X,'**',I3,'/',I2,'/',I4)
 1401 FORMAT(3x,'zero rainfall interval on',3x,I3,'/',I2,'/',I4,
     &      5x,'DOY',2x,i3)
 1402 FORMAT(3x,'rainfall intensity or amount is too low on',
     &       3x,I3,'/',I2,'/',I4,5x,'DOY',2x,i3)
 1403 FORMAT(3x,'** rainfall interval is too short, set to 10 min ** ',
     &       3x,I3,'/',I2,'/',I4,5x,'DOY',2x,i3)
 1404 FORMAT(3x,'** rainfall interval is too long, set to 30 min ** ',
     &       3x,I3,'/',I2,'/',I4,5x,'DOY',2x,i3)
 1500 FORMAT(2I6,' RAINFALL AMTS ARE NOT INCREASING  ','<',30('='),
     & 'ERROR')
 1600 FORMAT(2I6,' TIME OF PREVIOUS STORM OVERLAPS CURRENT STORM  ',
     &  '<',20('='),'ERROR')
 1700 FORMAT(2I6,' STORM DATES ARE NOT SEQUENTIAL  ','<',30('='),
     & 'ERROR')
      END
C
      SUBROUTINE DRAIN(NN,NDXN2H,THETA,TL,SOILHP,DTCHEM,TSEEP,DTSEEP,CC,
     +    SLKS,BD,AEF,IBRKTH,BRKH2O,BKCHEM,T,SOLTP1,FREUND,pori)
C======================================================================
C
C       PURPOSE:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ABSB   L
C       AEF        I  FIELD SATURATION FRACTION [0..1]
C       BKCHEM    I/O FLUX OF SELECTED CHEMICALS AT BRK. THR. NODE
C             CONTAINS NO3-N, PEST#1-3 [UG/CM^2]
C       BRKH2O    I/O FLUX OF WATER AT BRK. THR. NODE [CM/DAY]
C       CC        I/O TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C             CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C             1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C             5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C             9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C             12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       DEF        L
C       DMASS  L
C       DTCHEM    I/O TOTAL AMOUNT OF CHEMICAL LOST TO SEEPAGE
C             CORRESPONDS TO CC ORDER [UG/CM^2]
C       DTSEEP    I/O ACCUMULATED SEEPAGE OUT THE BOTTOM (DAILY)
C       DWATER     L  THE AMOUNT OF WATER TRANSFERRED DOWN ONE LAYER
C       I    L    INDEX VARIABLE
C       IBRKTH     I
C       J    L    INDEX VARIABLE
C       JH         L  HORIZON INDEX
C       JN         L  INDEX VARIABLE
C       K    L    VON KARMAN CONSTANT ( =.41)
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C               IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       BD         I  SOIL BULK DENSITY [G/CM^3]
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C               (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C               (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C               (3):   EPS   - EXPONENT FOR K(H) CURVE
C               (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C               (5):   WR    - RESIDUAL WATER CONTENT
C               (6):   WS    - SATURATION WATER CONTENT
C               (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C               (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C               (9):   WWP   - WILTING POINT (15 BAR) WC
C               (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C               (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C               (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C               (13):  A1    - CONSTANT FOR O(H) CURVE
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TSEEP I/O TIME STEP SEEPAGE OUT OF HORIZON
C
C       COMMENTS:
C
C       CALLED FROM: DAILY
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:   2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12,MXCHEM=15,MXPEST=3)
      PARAMETER(CW=4.184D-3)
C
      DIMENSION THETA(MXNOD),TL(MXNOD),NDXN2H(MXNOD),DTCHEM(MXCHEM),
     +    SOILHP(13,MAXHOR),CC(MXNOD,MXCHEM),BD(MXNOD),T(MXNOD),
     +    SLKS(MXNOD,MXPEST,2),BKCHEM(MXCHEM),SOLTP1(MAXHOR,5),
     +    FREUND(MXPEST),pori(mxnod)
C
      TSEEP=0.0D0
      DO 30 I=1,NN
        JH=NDXN2H(I)
        IF (THETA(I)-PORI(I) .GT. 1.0D-12) THEN
c        IF(THETA(I)-SOILHP(6,JH)*AEF.GT.1.0D-12) THEN
          DO 20 J=I,NN
            JN=NDXN2H(J)
            DEF = THETA(J) - PORI(J)
c            DEF=THETA(J)-SOILHP(6,JN)*AEF
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
            IF(DEF.GT.0.0D0) THEN
              DO 10 K=1,MXCHEM
                DMASS=DEF*TL(J)*CC(J,K)
                IF(J.NE.NN) THEN
                  IF(K.GE.13) THEN
C LIWANG MA, 7-3-2006
                    ABSB=FSLKS(SLKS(J+1,K-12,1),CC(J,K),FREUND(K-12),
     +                   theta(j+1),bd(j+1))*BD(J+1)
                  ELSE
                    ABSB=0.0D0
                  ENDIF
                  DWATER=THETA(J+1)*TL(J+1)+DEF*TL(J)
                  OCONC=CC(J+1,K)
                  CC(J+1,K)=(OCONC*(THETA(J+1)+ABSB)*TL(J+1)+DMASS)/(
     +                DWATER+TL(J+1)*ABSB)
                  IF(CC(J+1,K).LT.0.0D0) THEN
                    WRITE(9,1000) J+1,OCONC,DMASS
                  ENDIF
                  IF(J.EQ.IBRKTH) THEN
                    BRKH2O=BRKH2O+DEF*TL(J)
                    IF(K.EQ.9) THEN
                      BKCHEM(9)=BKCHEM(9)+DMASS
                    ELSEIF(K.GE.13) THEN
                      BKCHEM(K)=BKCHEM(K)+DMASS
                    ENDIF
                  ENDIF
                ELSE
                  DTCHEM(K)=DTCHEM(K)+DMASS
                  IF(K.EQ.1) THEN
                    DTSEEP=DTSEEP+DEF*TL(NN)
                    TSEEP=DEF*TL(NN)
                  ENDIF
                  IF(J.EQ.IBRKTH) THEN
                    BRKH2O=BRKH2O+DEF*TL(NN)
                    IF(K.EQ.9) THEN
                      BKCHEM(9)=BKCHEM(9)+DMASS
                    ELSEIF(K.GE.13) THEN
                      BKCHEM(K)=BKCHEM(K)+DMASS
                    ENDIF
                  ENDIF
                ENDIF
   10         CONTINUE
C
C             ..UPDATE TEMPERATURES
              IF(J.LT.NN) THEN
                JH=NDXN2H(J+1)
                CURT=(THETA(J+1)*CW+SOLTP1(JH,5))*TL(J+1)*1.0D3
                FLIT=DEF*TL(J)*CW*1.0D3
                DENT=((THETA(J+1)*TL(J+1)+DEF*TL(J))*CW+SOLTP1(JH,5)*
     +              TL(J+1))*1.0D3
                T(J+1)=(T(J+1)*CURT+T(J)*FLIT)/DENT
              ENDIF
C
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     kej
c              THETA(J)=SOILHP(6,JN)*AEF
              THETA(J) = PORI(J)
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
              IF(J.LT.NN) THETA(J+1)=DWATER/TL(J+1)
            ENDIF
   20     CONTINUE
          GOTO 40
        ENDIF
   30 CONTINUE
C
   40 RETURN
C     1000 FORMAT (' SEEPAGE OUT OF PROFILE ADJUSTMENT',F15.6,4X,F15.6)
 1000 FORMAT(///' LAYER IN QUESTION',I5,/' ORIGINAL MASS',G12.3,/
     +    ' MASS INTO LAYER =====>',G12.3,/' NEW MASS',G12.3)
      END
C
      SUBROUTINE TILEFLO(TLT,HORTHK,DRDEP,DRSPAC,DRRAD,H2OTAB,SUBDR,NN,
     +    NHOR,CLAT,IDP,UDRN)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE SOLVES HOOGHOUDT'S EQN. TO FIND THE
C           CURRENT FLUX OUT THE TILE DRAIN BASED ON THE
C           LATERAL KSATS WITHIN THE WATER TABLE, DISTANCES
C           BETWEEN AND RADIUS OF DRAINS, AND DEPTH TO THE
C           WATER TABLE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CLAT       I  LATERAL HYDROLIC CONDUCTIVITY [CM/HR]
C       DRDEP      I  DEPTH TO DRAIN [CM]
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       DRSPAC     I  DISTANCE BETWEEN DRAINS [CM]
C       DRRAD      I  RADIUS OF DRAINS [CM]
C       HORTHK     I  DEPTH OF SOIL HORIZONS [CM]
C       H2OTAB     I  ARRAY CONTAINING NODE # AND DEPTH OF WATER TABLE
C       SUBDR      O  FLUX OUT DRAIN BASED ON CURRENT CONDITIONS [CM/HR]
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       UDRN       O  FLUX OUT OF DRAIN BY LAYER (CM/HR)
C
C       COMMENTS:
C
C       CALLED FROM:
C
C       PROGRAMMER:  KAREN JOHNSEN AND KEN ROJAS
C
C       VERSION:   1
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MAXHOR=12)
      DOUBLE PRECISION NUM
      DIMENSION H2OTAB(2),HORTHK(MAXHOR),TLT(MXNOD),D(MAXHOR),
     +    CLAT(MAXHOR),UDRN(MXNOD),TWTL(MXNOD)
C
      IF(DRDEP.GT.TLT(NN)) DRDEP=0.0D0
      DM=DRDEP-H2OTAB(2)
      DO 10 I=1,NN
        UDRN(I)=0.0D0
   10 CONTINUE
C
      IF(DM.LE.0.D0) THEN
C
C       ..WATER TABLE IS BELOW DRAIN, SO NO DRAINAGE
        SUBDR=0.D0
      ELSE
C
C       ..CALCULATE EFFECTIVE DEPTH OF DRAIN
        PI=2.0D0*ASIN(1.0D0)
        DD=HORTHK(NHOR)-DRDEP
C        DD=TLT(NN)-DRDEP
        RAT=DD/DRSPAC
        CON=3.55D0-1.6D0*RAT+2.D0*RAT*RAT
        IF(RAT.LT.0.3D0) THEN
          EFFDEP=DD/(1+RAT*(8.0D0/PI*LOG(DD/DRRAD)-CON))
        ELSE
          EFFDEP=DRSPAC*PI/(8.0D0*(LOG(DRSPAC/DRRAD)-1.15D0))
        ENDIF
        EFFDEP=MAX(EFFDEP,0.0D0)
C
C       ..CALCULATE EFFECTIVE CONDUCTIVITY
        HORDEP=0.D0
        NUM=0.D0
        DEN=0.D0
        DO 20 I=1,NHOR
          D(I)=0.D0
          IF(I.NE.1) THEN
            HORDEP=HORTHK(I)-HORTHK(I-1)
          ELSE
            HORDEP=HORTHK(I)
          ENDIF
          IF(H2OTAB(2).LT.HORTHK(I)) THEN
            D(I)=MIN(HORTHK(I)-H2OTAB(2),HORDEP)
          ENDIF
          NUM=NUM+D(I)*CLAT(I)
          DEN=DEN+D(I)
   20   CONTINUE
        EFFK=NUM/DEN
C
C       ..CALCULATE DRAINAGE
        SUBDR=(8.0D0*EFFK*EFFDEP*DM+4.D0*EFFK*DM*DM)/(DRSPAC*DRSPAC)
C
C       ..CALCULATE DRAINAGE PER UNIT DEPTH IN SAT ZONE
C       ABOVE DRAIN (IDP in place of NN)
        TLSAT=TLT(IDP)-H2OTAB(2)
c        CHK=0.0D0
C       ..WEIGHTED
        IL=0
        WDEN=0
        DO 40 I=INT(H2OTAB(1)),IDP
          IL=IL+1
          IF(I.GT.1) THEN
            UPR=MAX(TLT(I-1),H2OTAB(2))
          ELSE
            UPR=MAX(0.0D0,H2OTAB(2))
          ENDIF
          TWTL(IL)=TLT(I)-UPR
          TWTL(IL)=MAX(TWTL(IL),0.0D0)
          WDEN=WDEN+IL*TWTL(IL)
   40   CONTINUE
       WT1 = TLSAT / WDEN
        IL=0
        DO 50 I=INT(H2OTAB(1)),IDP
          IL=IL+1
         WT = IL * WT1
C         WT=1.0D0
          UDRN(I)=SUBDR*WT*TWTL(IL)/TLSAT
c          CHK=CHK+UDRN(I)
   50   CONTINUE
C     IF (ABS(CHK - SUBDR) .GE. 1.D-12) THEN
C     PRINT*,'SOMETHING IS WRONG IN TILE FLOW CALCULATION'
C     STOP
C     ENDIF
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE UPTAKE(NN,RDF,SNIT,ANIT,TUP,THETA,TL,UPNIT,DELT,IPL,
     +    TDPLNT,TPASS,TACTVE,INXPL,EFFLUX,TNO3UP)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE TAKES UP NITROGEN INTO THE PLANT AT THE
C            TRANSPIRATION RATE. CHEMICAL IS TAKEN UP UNTIL THE
C            THE NITROGEN DEMAND IS SATISFIED.
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       variable  I/O description
C       --------  --- -----------
C       ACTIVE     L
C       AHLDG  I
C       ANIT   I  NH4-H IN LAYERS (MG/L)
C       AVAIL  L
C       CNST   I
C       DELT   I  INCREMENTAL TIME STEP [HR]
C       DMDNIT     I  NITROGEN DEMAND FROM PLANT (G-N/PLANT)
C       EFFLUX     I  LUXURIOUS N-UPTAKE EFFICIENCY FACTOR
C       GIWAT  I
C       G2UG   P  CONVERSION FACTOR FOR GRAM ==> MICROGRAM
C       I    L    INDEX VARIABLE
C       IPL        I  PLANT CURRENTLY MODELING [1..MXSPEC]
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       OVER   L  FLAG USED FOR MASS UNDERRUN
C       PFACT  L  NUMBER OF PLANTS / CM^2
C       PWRTS  I  NUMBER OF PLANT WITH ACTIVE ROOTS [#/HA]
C       RDF        I  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       SNIT   I  NO3-N IN SOIL LAYERS [MG/L]
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TIMEN  L
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TNITUP    I/O TOTAL NITROGEN UPTAKE FOR WHOLE PROFILE [G/PLANT]
C       TUP        I  WATER UPTAKE / LAYER [CM].
C       UPNIT I/O UPTAKE OF NITROGEN FOR EACH LAYER [G/PLANT/LAYER]
C       UPMAX  P  MAXIMUM DAILY UPTAKE OF NITROGEN [KG/HA]
C
C       COMMENTS:
C          -- SNIT COMES IN AS MG/L
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:  JON HANSON AND KEN "FORTRAN GOD" ROJAS
C
C       VERSION:  2.0
C
C======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXSPEC=10,mxpest=3,maxhor=12)
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
      LOGICAL LUX
      PARAMETER(G2UG=1.0D6,UPMAX=8.0D0)
      DIMENSION UPNIT(MXNOD),THETA(MXNOD),SNIT(MXNOD),RDF(MXNOD),
     +    TUP(MXNOD),TL(MXNOD),TIMEN(MXNOD),TPRM(MXNOD),
     +    PASS1(MXNOD),ACTIVE(MXNOD),ANIT(MXNOD),
     +    INXPL(MXSPEC),EFFLUX(MXSPEC),TNO3UP(MXNOD)
      character SOLTYP*10,NMPEST*30,PLNAME*30
      COMMON /NAMES/ NMPEST(MXPEST),SOLTYP(MAXHOR),PLNAME(MXSPEC)
C
C      TNITUP = DMDNIT
C
C     ..DETERMINE IF WE'RE IN A LUXURY UPTAKE CONDITION. (ONLY CORN)
      IF((    INDEX(PLNAME(IPL),'CORN').NE.0.OR.
     +        INDEX(PLNAME(IPL),'corn').NE.0.or.
     +        INDEX(PLNAME(IPL),'MAIZE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'maize').NE.0.or. 
     +        INDEX(PLNAME(IPL),'Corn').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'Maize').NE.0)    !.or.(INXPL(IPL).LE.100)) 
     +    .AND.GS.GE.1.0D0.AND.DMDNIT.LE.0.D0) THEN
        LUX=.TRUE.
        DMDNIT=UPMAX
        EFFAC=EFFLUX(IPL)
      ELSE
        LUX=.FALSE.
        UPLUX=0.0D0
        EFFAC=1.0D0
      ENDIF
C
C
      TTPASS=0.0D0
	TTACTIVE=0.0D0
      TTIMEN=0.0D0
      IF((TNITUP+1.0D-10).LT.DMDNIT.AND.DMDNIT.GT.0.0D0.AND.PWRTS.NE.
     +    0.0D0.AND.TDPLNT.LE.UPMAX) THEN
C
C       ..CALCULATE NITROGEN UPTAKE FROM EACH LAYER
        PFACT=1.0D8/PWRTS
        DO 10 I=1,NN
          TIMEN(I)=0.0D0
          PASS1(I)=0.0D0
          ACTIVE(I)=0.0D0
          ROOTN=0.0D0
          IF((SNIT(I).GT.0.0D0.OR.ANIT(I).GT.0.0D0).AND.RDF(I).NE.0.0D0)
     +        THEN
C
C           .. AMOUNT OF NITROGEN AVAILABLE TO EACH PLANT (UG/PLANT)
            SPRM=SNIT(I)*THETA(I)*TL(I)*PFACT
            APRM=ANIT(I)*THETA(I)*TL(I)*PFACT
            TPRM(I)=SPRM+APRM
C
C           ..DETERMINE AMOUNT OF PASSIVE N UPTAKE
            IF(TUP(I).GT.0.0D0) THEN
C
C             .. AMOUNT OF WATER ENTERING EACH PLANT (ML/PLANT)
              VPRM=TUP(I)*PFACT
C
C             ..WORK IN LAYER ONLY IF IT HAS LIVE ROOTS
              IF(RDF(I).GT.0.0D0.AND.TPRM(I).GT.0.0D0) THEN
C
C               ..CALC PASSIVE NITROGEN UPTAKE (UG/PLANT)
                TMPASS=VPRM*(SNIT(I)+ANIT(I))
                PASS1(I)=MIN(TMPASS,TPRM(I))*EFFAC
              ELSE
                PASS1(I)=0.0D0
              ENDIF
              IF(PASS1(I).LT.TPRM(I)) THEN
                AVAIL=TPRM(I)-PASS1(I)
              ELSE
                AVAIL=0.0D0
              ENDIF
            ELSE
              PASS1(I)=0.0D0
              AVAIL=TPRM(I)
            ENDIF
C           ..DETERMINE ACTIVE UPTAKE
            IF(AVAIL.GT.0.0D0.AND..NOT.LUX) THEN
              TCNUP2=CNUP2(IPL)*THETA(I)*TL(I)*PFACT
              ACTIVE(I)=CNUP1(IPL)*AVAIL*DELT/(TCNUP2+AVAIL)/24.0D0
            ELSE
              ACTIVE(I)=0.0D0
            ENDIF
            ACTIVE(I)=MIN(ACTIVE(I),AVAIL)*EFFAC
C
C           ..DON'T TAKE MORE NITROGEN THAN IS AVAILABLE
            ROOTN=PASS1(I)+ACTIVE(I)
            ROOTN=MAX(ROOTN,0.0D0)
            ROOTN=MIN(ROOTN,TPRM(I))
          ELSE
            ROOTN=0.0D0
          ENDIF
          TIMEN(I)=ROOTN
C
C         ..SAVE AMT UPTAKEN AND CONVERT FROM (UG/PLANT) ==> (G/PLANT)
C LIWANG MA, 7-1=2006
          TTPASS=TTPASS+PASS1(I)/G2UG
          TTACTIVE=TTACTIVE+ACTIVE(I)/G2UG
          TTIMEN=TTIMEN+TIMEN(I)/G2UG
   10   CONTINUE
C
C       ..CHECK FOR TOO MUCH PASSIVE NITROGEN UPTAKE
C        IF((TNITUP+TTPASS).GT.DMDNIT) THEN
C          OVER=(TNITUP+TTPASS)-DMDNIT
        IF(TTPASS.GT.DMDNIT) THEN
          OVER=TTPASS-DMDNIT
          FACT=1.0D0-OVER/TTPASS
          DO 21 I=1,NN
            PASS1(I)=PASS1(I)*FACT
            ACTIVE(I)=0.0D0
            TIMEN(I)=PASS1(I)
   21     CONTINUE
        ENDIF

C       ..CHECK FOR TOO MUCH ACTIVE NITROGEN UPTAKE
C        IF((TNITUP+TTIMEN).GT.DMDNIT) THEN
C          OVER=(TNITUP+TTIMEN-TTPASS)-DMDNIT
        IF(TTIMEN.GT.DMDNIT) THEN
          OVER=(TTIMEN-TTPASS)-DMDNIT
          FACT=1.0D0-OVER/(TTIMEN-TTPASS)
          DO 20 I=1,NN
            ACTIVE(I)=ACTIVE(I)*FACT
            TIMEN(I)=PASS1(I)+ACTIVE(I)
   20     CONTINUE
        ENDIF
        TNITUP=0.0D0
        TPASS=0.0D0
        TACTVE=0.0D0
        TTIMEN=0.0D0
C
C       .. EXTRACT CHEMICALS FROM NITROGEN POOLS
        DO 30 I=1,NN
          IF(TIMEN(I).GT.0.0D0.AND.TPRM(I).GT.0.0D0) THEN
            PROPL=1.0D0-(TIMEN(I)/TPRM(I))
            ANIT(I)=ANIT(I)*PROPL
            TNO3UP(I)=SNIT(I)*(1.0D0-PROPL)
            SNIT(I)=SNIT(I)*PROPL
            UPNIT(I)=UPNIT(I)+TIMEN(I)/G2UG
          ENDIF
c move the following line here, Liwnang Ma, 7-1-2006
          TNITUP=TNITUP+UPNIT(I)
          TPASS=TPASS+PASS1(I)/G2UG
          TACTVE=TACTVE+ACTIVE(I)/G2UG
          TTIMEN=TTIMEN+TIMEN(I)/G2UG
   30   CONTINUE
        TDPLNT=TNITUP*PWRTS*1.0D-3
      ENDIF
      IF(LUX) THEN
        UPLUX=TNITUP
        DMDNIT=0.0D0
        TNITUP=0.0D0
      ENDIF
C
      RETURN
      END
      SUBROUTINE RTUPTK(PEST,PESTUP,TDPPLNT,RDF,THETA,TL,TUP,PWRTS,XKOW,
     +    NN,BD,SLKS,IP,FREUND)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE TAKES UP PESTICIDES INTO THE PLANT AT THE
C            TRANSPIRATION RATE. CHEMICAL IS TAKEN UP IN THE PASSIVE
C         .      UPTAKE STREAM (TRANSPIRATION STREAM)
C
C       REF:  CHAPTER 7, RZWQM: MODELING MANAGEMENT EFFECTS ON WATER
C         QUALITY AND CROP PRODUCTION, 2000.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       G2UG   P  CONVERSION FACTOR FOR GRAM ==> MICROGRAM
C       I    L    INDEX VARIABLE
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       PEST  I/O PESTICIDE CONCENTRATION BY LAYER (MG/L)
C       PFACT  L  NUMBER OF PLANTS / CM^2
C       PWRTS  I  NUMBER OF PLANT WITH ACTIVE ROOTS [#/HA]
C       RDF        I  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       TDPPLNT   I/O LOSS OF PESTICIDE FROM PLANT UPTAKE (KG/HA)
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       PESTUP    I/O PESTICIDE UPTAKE / LAYER [G/LAYER].
C       TUP        I  WATER UPTAKE / LAYER [CM].
C
C       COMMENTS:
C          -- PEST (PESTICIDES) COMES IN AS MG/L
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:  DON WAUCHOPE AND KEN "FORTRAN GOD" ROJAS
C
C       VERSION:  98.0
C
C======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXPEST=3)
C
      PARAMETER(G2UG=1.0D6)
      DIMENSION THETA(MXNOD),RDF(MXNOD),TUP(MXNOD),PESTUP(MXNOD),
     +    PEST(MXNOD),TL(MXNOD),
     +    PASS1(MXNOD),TPRM(MXNOD),FREUND(MXPEST)
C
      DIMENSION BD(MXNOD),SLKS(MXNOD,MXPEST,2)
C
      IF(PWRTS.NE.0.0D0.AND.XKOW.GT.0.0D0) THEN
C
C       ..CALCULATE PESTICIDE UPTAKE FROM EACH LAYER
        PFACT=1.0D8/PWRTS
        DO 10 I=1,NN
          PASS1(I)=0.0D0
          IF((PEST(I).GT.0.0D0).AND.RDF(I).NE.0.0D0) THEN
C
C           .. AMOUNT OF PESTICIDE AVAILABLE TO EACH PLANT (UG/PLANT)
            TPRM(I)=PEST(I)*THETA(I)*TL(I)*PFACT
C
C           ..DETERMINE AMOUNT OF PASSIVE PESTICIDE UPTAKE
            IF(TUP(I).GT.0.0D0) THEN
C
C             .. AMOUNT OF WATER ENTERING EACH PLANT (ML/PLANT)
              VPRM=TUP(I)*PFACT
C
C             ..WORK IN LAYER ONLY IF IT HAS LIVE ROOTS
              IF(RDF(I).GT.0.0D0.AND.TPRM(I).GT.0.0D0) THEN
C
C               .. DETERMINE PEST OCTANOL-WATER PARTITIONING COEFF
                XKTSCF=0.784D0*10.0D0**(-(LOG10(XKOW)-1.78D0)**2.0D0/
     +              2.44D0)
C
C               ..CALC PASSIVE NITROGEN UPTAKE (UG/PLANT)
                TMPASS=VPRM*PEST(I)*XKTSCF
                PASS1(I)=MIN(TMPASS,TPRM(I))
              ENDIF
            ENDIF
          ENDIF
   10   CONTINUE
C
C       .. EXTRACT CHEMICALS FROM PESTICIDE POOLS
        TPUP=0.0D0
        DO 20 I=1,NN
          IF(PASS1(I).GT.0.0D0.AND.TPRM(I).GT.0.0D0) THEN
            PROPL=1.0D0-(PASS1(I)/TPRM(I))
            PROPL=MIN(PROPL,1.0D0)
C LIWANG MA, 7-3-2006
            CONV=(THETA(I)+FSLKS(SLKS(I,IP,1),PEST(I),FREUND(IP),
     +            theta(i),bd(i))
     +           *BD(I))*TL(I)*1.0D-1
            PESTLAY=(1.0-PROPL)*PEST(I)*CONV
            PESTUP(I)=PESTUP(I)+PESTLAY
            PEST(I)=PEST(I)*PROPL
          ENDIF
          TPUP=TPUP+PESTUP(I)
   20   CONTINUE
C
C       .. DETERMINE LOSS OR PESTICIDE FROM PLANT UPTAKE (KG/HA)
        TDPPLNT=TPUP
      ELSE
        TDPPLNT=0.0D0
      ENDIF
C
      RETURN
      END
      SUBROUTINE SDEAD_KNOCKDOWN(SDEAD,SDCN,RM,RCN)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE CONVERTS STANDING DEAD RESIDUE INTO FLAT
C         (ON THE GROUND) RESIDUE.  ASSUME AN EXPONTENTIAL DISTRIBUTION
C         OF THE KNOCKDOWN BASED ON MASS.
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  1.3.2004.213
C
C======================================================================
C
      DOUBLE PRECISION SDEAD,SDCN,RM,RCN,CNEW,DSDEAD
C
C     ..CONVERT STANDING RESIDUE TO FLAT (ON THE GROUND) RESIDUE
      DSDEAD=SDEAD*(1.0D0-EXP(-0.01))
      SDEAD=SDEAD-DSDEAD
C
C     ..STICK THE NEW KNOCKDOWN RESIDUE INTO THE RESIDUE POOL
      RCN=CNEW(RM,DSDEAD,RCN,SDCN,0)
C     RCN = MAX(MIN(RCN,CN(2)),CN(1))
      RM=RM+DSDEAD

      RETURN
      END
      SUBROUTINE LATFLO(H2OTAB,HORTHK,TLT,CLAT,NDXN2H,DRDEP,IDP,NHOR,NN,
     +ULAT,QLAT,HYDGRAD,soilhp)
C
C======================================================================
C
C       PURPOSE: THIS ROUTINE DETERMINES THE LATERAL FLOW FROM THE
C           SATURATED LAYERS BELOW THE TILE DRAIN DEPTH
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CLAT       I  LATERAL HYDRAULIC CONDUCTIVITY [CM/HR]
C       DRDEP      I  DEPTH TO DRAIN [CM]
C       EFFK       L  EFFECTIVE LATERAL CONDUCTIVITY --
C                     THICKNESS DISTRIBUTED K [CM/HR]
C       HEADDEPTH  L  DEPTH OF HEAD DRIVING THE LATERAL FLOW PROCESS, WILL
C                     MAXIMUM OF DRAIN DEPTH OR WATERTABLE DEPTH WHICHEVER
C                     IS DEEPER. [CM]
C       H2OTAB     I  ARRAY CONTAINING NODE # AND DEPTH OF WATER TABLE
C       HORTHK     I  DEPTH OF SOIL HORIZONS [CM]
C       HYDGRAD    I  HYDRAULIC GRADIENT - DH/DL POTENTIAL RATE AT WHICH
C                     WATER FLOWS LATERALLY.
C       ITOPNDX    L  NODE INDEX FOR HEADDEPTH
C       ITOPHOR    L  HORIZON INDEX FOR HEADDEPTH
C       MXNOD      P  MAX NUMBER OF NUMERICAL NODES
C       QLAT       O  LATERAL FLUX OUT OF SATURATED LAYERS [CM/HR]
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C     ` TWTL       L  THICKNESS DISTRIBUTED WEIGHTING FACTOR
C       TOP        L  DOMINATE DEPTH USED TO DETERMINE HEADDEPTH [CM]
C       ULAT       O  LAYER DISTRIBUTED LATERAL FLUX [CM/HR/LAYER]
C       WT1        L  TOTAL WEIGHTING APPLIED
C
C
C       COMMENTS:
C
C       CALLED FROM:  PHYSCL
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  1.3.2004.506
C
C======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MAXHOR=12)
      DIMENSION H2OTAB(2),HORTHK(MAXHOR),TLT(MXNOD),CLAT(MAXHOR),
     +  ULAT(MXNOD),TWTL(MXNOD),NDXN2H(MXNOD),soilhp(13,maxhor)

      TOP=MAX(H2OTAB(2),DRDEP)
      HEADDEPTH=MAX(HORTHK(NHOR)-TOP,0.0D0)
      IF (HEADDEPTH.GT.0.0D0 .and. HYDGRAD.gt.0.0d0) THEN
        ITOPNDX=MAX(INT(H2OTAB(1)),IDP+1)
C
C       ..CALCULATE EFFECTIVE LATERAL CONDUCTIVITY -- ONLY LAYERS FROM
C         WATERTABLE DEPTH TO BOTTOM OF PROFILE
        TNUM=0.D0
        DEN=0.D0
        EFFK=0.D0
        ITOPHOR=NDXN2H(ITOPNDX)
        DO 10 I=ITOPHOR,NHOR
          D=0.D0
          IF(I.NE.1) THEN
            HORDEP=HORTHK(I)-HORTHK(I-1)
          ELSE
            HORDEP=HORTHK(I)
          ENDIF
          IF(H2OTAB(2).LT.HORTHK(I)) D=MIN(HORTHK(I)-H2OTAB(2),HORDEP)
          IF(CLAT(I).GT.0.0) THEN  !ONLY USE LAYERS WITH CLAT
            TNUM=TNUM+D*CLAT(I)
            DEN=DEN+D
          ENDIF
   10   CONTINUE
        IF (DEN.NE.0.0) EFFK=TNUM/DEN
C
C       ..DETERMINE THE LATERAL FLOW
        QLAT = EFFK*HEADDEPTH*HYDGRAD
C
C       ..DISTRIBUTE LATERAL FLOW PER UNIT DEPTH
C       ..DETERMINE WEIGHTS
        IL=0
        WDEN=0
        DO 20 I=ITOPNDX, NN
          K=NDXN2H(I)
          IL=IL+1
          TWTL(IL)=0.0
          IF(I.GT.1) THEN
            UPR=MAX(TLT(I-1),H2OTAB(2))
          ELSE
            UPR=MAX(0.0D0,H2OTAB(2))
          ENDIF
          IF (CLAT(K).GT.0.0) THEN !ONLY USE LAYERS WITH CLAT
            TWTL(IL)=TLT(I)-UPR
            TWTL(IL)=MAX(TWTL(IL),0.0D0)
c            WDEN=WDEN+IL*TWTL(IL)
            WDEN=WDEN+TWTL(IL)*soilhp(4,k)
          ENDIF
   20   CONTINUE
        WT1 = HEADDEPTH / WDEN
        IL=0
C        WT=1.0
C        CHK=0.0
        DO 30 I=ITOPNDX, NN
          K=NDXN2H(I)
          IL=IL+1
          WT = IL * WT1
          ULAT(I)=QLAT*TWTL(IL)*soilhp(4,k)/WDEN    !evenly distribute flux among layers, only weighted for thickness.
c          ULAT(I)=QLAT*WT*TWTL(IL)/HEADDEPTH
C          CHK=CHK+ULAT(I)
   30   CONTINUE
C        PRINT*,QLAT,HEADDEPTH
C        IF (ABS(CHK - QLAT) .GE. 1.D-12)
C     +   PRINT*,'SOMETHING IS WRONG IN LAT FLOW CALCULATION',CHK-QLAT
      ELSE
C       ..NO FLOW POSSIBLE ZERO OUT STUFF
        QLAT=0.0D0
        DO 40 I=1,NN
          ULAT(I)=0.0D0
   40   CONTINUE
      ENDIF
      RETURN
      END

ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
      SUBROUTINE ADCONICE(SOILPP,THETAold,thetanew,
     +           CC,SLKS,CONCX2,XNU,FREUND)
C
C======================================================================
C
C       PURPOSE: ADJUST Chemical concentration due to ice change
C
C       REF:  AFTER NTRM
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       CC        I/O TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C             CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C             1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C             5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C             9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C             12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CONCX2    I/O PEST. CONC. INVOLVED IN KINETIC PROCESSES [UG/G-S]
C       EINC   L
C       FDEP  I/O FRACTION OF TOTAL MACROPORES THAT ARE DEADEND [0..1]
C       I    L    INDEX VARIABLE
C       IH         L
C       IN         L  ENDING ROW INDEX OF TRICO AS DIMENSIONED IN
C       IP         L  INDEX FOR PESTICIDE NUMBER
C       J    L    INDEX VARIABLE
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXPTS     I
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MCPOR  I  TOTAL MACROPOROSITY OF HORIZON [CM3/CM3]
C       MICP   I  TOTAL MICROPOROSITY OF HORIZON [CM3/CM3]
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NBPR   I
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C               IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       OBD        I
C       OFDP   I
C       PBD        L
C       RAINP  L  TOTAL RAINFALL AMT [CM]
C       RANRAT     L  RAINFALL INTENSITY [CM/HR]
C       RBD        L
C       SF         P  SOIL SURFACE ENERGY SEALING FACTOR
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       SOILPP     I
C       STRM   I
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TOTEN  L  RAINFALL ENERGY [J/CM^2] (RAINFALL ENERGY IS THE
C             FRACTION OF THE ENERGY NORMAL TO THE SOIL
C             PER UNIT AREA OF SURFACE,  NON RESIDUE, OR CANOPY
C             PROTECTED SOIL)
C       TR         I  CUMMULATIVE TIME OF RAINFALL [HR]
C       XNU       I/O
C       YESMAC     I  INDICATS PRESENCE OF MACROPORES =1 YES, =0 NOT
C
C       COMMENTS:
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C             REDUCE
C             SETTL
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
C       CC         I  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C             CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C             1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C             5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C             9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C             12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MXCHEM=15,MXPEST=3,
     +   MXAPP=200)
      PARAMETER(SF=1.0D0)
C
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
      DOUBLE PRECISION MCPOR
      INTEGER YESMAC
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      DIMENSION SOILPP(8,MAXHOR),PBD(maxhor),THETAold(MXNOD),
     +    CC(MXNOD,MXCHEM),CONCX2(MXNOD,MXPEST),SLKS(MXNOD,MXPEST,2),
     +    XNU(MXNOD,25),RPOOL(MXNOD,2),FREUND(MXPEST),THETAnew(MXNOD)
C
C
C       .. RECALCULATE PEST. CONCENTRATIONS
        DO 50 I=1,NN
          IH=NDXN2H(I)
C           ..ADJUST PESTICIDE MASS-BASED CONCENTRATIONS
            DO 30 IP=1,MXPEST
              J=IP+12
C LIWNAG MA, 7-3-2006
              CC(I,J)=CC(I,J)*(THETAold(I)+FSLKS(SLKS(I,IP,1),
     +             CC(I,J),FREUND(IP),thetaold(i),soilpp(3,ih))
     +             *soilpp(3,ih))/(
     +            THETAnew(I)+FSLKS(SLKS(I,IP,1),CC(I,J),FREUND(IP),
     +            thetanew(i),soilpp(3,ih))
     +            *SOILPP(3,IH))
   30       CONTINUE
C
C           ..ADJUST NUTRIENT Water-BASED CONCENTRATIONS (12=NH4, 11=NO3, 10=urea)
            DO 40 IN=10,12
              XNU(I,IN)=XNU(I,IN)*thetaold(I)/thetanew(i)
   40       CONTINUE
c            do ic=1,12   !assume no adsorption of the chemicals
              CC(I,9)=CC(I,9)*thetaold(I)/thetanew(i)
              CC(I,10)=CC(I,10)*thetaold(I)/thetanew(i)
              CC(I,12)=CC(I,12)*thetaold(I)/thetanew(i)
c            enddo
   50   CONTINUE
C
      RETURN
      END
C
      Subroutine C_R_S_Cover(CCL,CR,CS,CO,RM,LAI,TLAI,CRES,IPR,
     +          ETC,PET,PES,PER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXRES=3)
C
C     .. INITIALIZE CROP DEPENDENT RESIDUE VALUES
      DOUBLE PRECISION RHORS(MXRES),RDIA(MXRES)
      SAVE RHORS,RDIA
      DATA RHORS/0.15,0.17,0.18/,
     +     RDIA /1.0,0.5,0.25/
C     ===::> R E S I D U E
C
C     ..CALCULATE RESIDUE CHARACTERISTICS
      IF(RM.GT.1.0D-6 .AND. IPR.NE.0) THEN
C
C       ..CONVERT FROM KG/HA ==> T/HA
        TRM=RM*1.0D-3
        IF(CRES.LE.0.0D0) THEN
          CRES=1.320D0
        ENDIF
C
C       ... REVISED ALGORITHM TO ESTIMATE FRACTION OF SOIL COVERED
C       RHORB = RESIDUE BULK DENSITY
C
c        NDXR=IRTYPE(IPR)
        CS=EXP(-CRES*1.270D-2*TRM/(RDIA(IPR)*RHORS(IPR)))
        else
        CS=1.0d0
        endif
        CR=1.0d0-CS
      IF(TLAI.NE.0.0D0) THEN
        CO=DEXP(-0.594D0*TLAI)
      ELSE
        CO=1.0D0
      ENDIF
      CCL=1.0D0-CO
      PET=ETC*CCL
      PER=ETC*CO*CR
      PES=ETC*CO*CS
      end        
      