C=======================================================================
C  ML_GROSUB, Subroutine
C
C  Growth subroutine for Pearl Millet
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/07/1993 PWW Header revision and minor changes         
C  06/  /1994 WTB Made to correspond to Millet 2.1 (7/18/91)
C  06/21/1994 JTR/BDB Updated PCARB calculation          
C  07/31/2002 WDB Converted to modular format               
C. 07/31/2002 WDB Added pest damage, SLPF, satfac           
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C-----------------------------------------------------------------------
C  Called : ML_CERES
C
C  Calls  : ML_NFACT ML_NUPTAK
C-----------------------------------------------------------------------

      SUBROUTINE ML_GROSUB (
     & AGEFAC, BIOMAS, CARBO, CNSD1,CNSD2, CO2X, CO2Y, 
     & CO2, CUMDTT, CUMPH, DLAYR,DM, DTT,  
     & GPP, GRAINN, GROLF, GRORT, GROSTM, ISTAGE, 
     & ISWNIT, ISWWAT, LAI, LEAFNO, LFWT, LL, LWMIN, NDEF3, 
     & NFAC, NLAYR, NH4,NSTRES, NO3, P3, P4, P5, PAF, PANWT, 
     & PDWI, PGC, PGRORT, PHINT, PLA, PLAN, PLAG, PLAO, PLATO, 
     & PLTPOP, PTF, RANC, RCNP, RLV,ROOTN, ROWSPC, RTWT, 
     & SAT,SEEDRV, SENLA, SHF, SLAN, SLW, SRAD, 
     & STMWT, STOVN, STOVWT, SW, SUMDTT,  
     & SWFAC, TANC, TBASE, TCNP,TEMF, TEMPM, TILN, 
     & TMAX, TMFAC1, TMIN, TMNC, TRNU,TSIZE, TURFAC,
     & XN,XSTAGE, EOP, TRWUP, RWUEP1,DYNAMIC,UNO3,UNH4,BD,
     & PRFTC,PORMIN,PARSR,RUE,SLPF,SATFAC, RESERVE,
     & ASMDOT,WLIDOT,WSIDOT,WRIDOT,PPLTD,SWIDOT,ISWDIS,G1,
     & VANC,VMNC,TLAG1, SENESCE, MPLA, TPLA, MPLAG, TPLAG)

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

!     ResidueType defined in ModuleDefs.for
      TYPE (ResidueType) SENESCE 

C----------------------------------------------------------------
C      VARIABLES THAT WERE IN COMMON BLOCKS
C----------------------------------------------------------------
      REAL AGEFAC
      REAL ASMDOT
      REAL BD(20)
      REAL BIOMAS
      REAL CARBO
      REAL CNSD1
      REAL CNSD2
      REAL CO2X(10)    
      REAL CO2Y(10)
      REAL CO2 
      REAL CUMDTT
      REAL CUMPH
      REAL CURV
      REAL DM
      REAL DTT
      INTEGER DYNAMIC
      REAL EOP
      REAL EP1
      REAL GPP
      REAL GRAINN
      REAL GROLF
      REAL GRORT
      REAL GROSTM
      INTEGER ISTAGE
      CHARACTER ISWDIS*1
      CHARACTER ISWNIT*1
      CHARACTER ISWWAT*1
      REAL LAI
      INTEGER LEAFNO
      REAL LFWT
      REAL LWMIN
      REAL NDEF3
      REAL NFAC
      REAL NSTRES
      REAL P3
      REAL P4
      REAL P5
      REAL PAF
      REAL PANWT
      REAL PDWI
      REAL PGC
      REAL PGRORT
      REAL PHINT
      REAL PLA
      REAL PLAN
      REAL PLAG
      REAL PLAO
      REAL PLATO
      REAL PLTPOP
      REAL PRFTC(4)
      REAL PTF
      REAL RANC
      REAL RCNP
      REAL ROOTN
      REAL ROWSPC
      REAL RTWT
      REAL RWUEP1
      REAL SEEDRV
      REAL SENLA
      REAL SLA 
      REAL SLAN
      REAL SLW
      REAL SRAD
      REAL STMWT
      REAL STOVN
      REAL STOVWT
      REAL SUMDTT
      REAL SWFAC
      REAL TANC
      REAL TBASE
      REAL TCNP
      REAL TEMF
      REAL TEMPM
      REAL TILN
      REAL TMAX
      REAL TMFAC1(8)
      REAL TMIN
      REAL TMNC
      REAL TRWUP
      REAL TSIZE
      REAL TURFAC
      REAL UNO3(20)
      REAL UNH4(20)
      REAL XN
      REAL XSTAGE
C      PEST DAMAGE
      REAL WLIDOT
      REAL LAIDOT
      REAL WSIDOT
      REAL WRIDOT
      REAL PPLTD
      REAL SWIDOT


C-----------------------------------------------------------------
C   Local Variables
C-----------------------------------------------------------------

      INTEGER   I
      REAL      NSINK,NPOOL1,NPOOL2,NPOOL,NSDR
      REAL      TT,PCARB,PRFT,TTMP,PC
      REAL      GRF,RGFILL,GROPAN
      REAL      RMNC,XNF,TNLAB,RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS
      REAL      TABEX,PCO2


C--------------------------------------------------------------------
C     Variables required in SG_NFAC but not used in SG_GROSUB
C--------------------------------------------------------------------

      REAL DLAYR(20)
      INTEGER L
      REAL LIFAC
      REAL LL(20)
      REAL NH4(20)
      INTEGER NLAYR
      REAL NO3(20)
      REAL PAR
      REAL PARSR
      REAL SLPF
      REAL PORMIN
      REAL RLV(20)
      REAL RUE
      REAL SAT(20)
      REAL SATFAC
      REAL SHF(20)
      REAL SUMEX
      REAL SUMRL
      REAL SW(20)
      REAL SWEXF
      REAL TRNU
      REAL TSS(20)

c--------------------------------------------------------------------
c        Variables for Millet
C--------------------------------------------------------------------
      REAL TOPSINK,TOPMAIN,MGROLF,MGROSTM,TCARBO
      REAL TGROLF,TGROSTM
      REAL MLAG2,MPLAG,MLAG1,TPLAG,SENF,RESERVE
      REAL G1,STMGF,VANC,VMNC,MPLA,TPLA,MLFWT,TLFWT
      REAL MSTMWT,TSTMWT,TLAG1
      REAL  RZrwu(300),RZtrwup,AVG_HROOT,WTDEP,qsr(300),TRWUP1
      DOUBLE PRECISION alaireset, WSI(10),heightset
      INTEGER ISTRESS, iresetlai, iresetht1
       common /wuptake/qsr,wtdep,trwup1,RZrwu,RZtrwup,istress,avg_hroot
      common /wsi/ wsi, alaireset, heightset, iresetlai, iresetht1
      TYPE (PLANTVARTYPE)   PLANTVAR
C--------------------------------------------------------------------
C              DYNAMIC = RUNINIT OR SEASINIT
C--------------------------------------------------------------------
       TRWUP1=TRWUP
C RESET LAI 12-16-2018
       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) THEN
          LAI=REAL(ALAIRESET)
C          XLAI = LAI                !Leaf area index, m2/m2
C          XHLAI = LAI   !Used in WATBAL
          PLA = LAI/PLTPOP*10000.+SENLA
          LFWT = PLA*0.0040
       ENDIF
       
       IF(DYNAMIC.EQ.RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
         XSTAGE = 0.0
         AGEFAC = 0.0
         BIOMAS = 0.0
         CARBO = 0.0
         DM = 0.0
         GPP = 0.0
         GRAINN = 0.0
         GROLF = 0.0
         GRORT = 0.0
         GROSTM = 0.0
         LAI = 0.0
         LEAFNO = 0
         LFWT = 0.0
         LWMIN = 0.0
         PAF = 0.0
         PANWT = 0.0
         PDWI = 0.0
         PGC = 0.0
         PGRORT = 0.0
         PLA = 0.0
         PLAN = 0.0
         PLAG = 0.0
         PLAO = 0.0
         PLATO = 0.0
         PTF = 0.0
         RLV = 0.0
         ROOTN = 0.0
         RTWT = 0.0
         SENLA = 0.0
         SLA = 0.0
         SLAN = 0.0
         SLW = 0.0
         STMWT = 0.0
         STOVN = 0.0
         STOVWT = 0.0
         TILN = 1.0
         TRNU = 0.0
         TSIZE = 0.0
         XN = 0.0
         MPLA = 0.0
         TPLA = 0.0
         MLFWT = 0.0
         TLFWT = 0.0
         MSTMWT = 0.0
         TSTMWT = 0.0
         MGROSTM = 0.0
         MSTMWT = 0.0
         MGROLF = 0.0
         MLFWT = 0.0
         STMWT = 0.0
         PANWT = 0.0
         LFWT = 0.0
         BIOMAS = 0.0
         MLAG1 = 0.0
         MLAG2 = 0.0

         SENESCE % ResWt  = 0.0
         SENESCE % ResLig = 0.0
         SENESCE % ResE   = 0.0


C-----------------------------------------------------------------------  
C-----------------------------------------------------------------------
C
C                     DYNAMIC = RATE
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.RATE) THEN

          !daily senescence
          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0


C--------------------------------------------------------------------
C                   DYNAMIC = INTEGRATE
C--------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

      !----------------------------------------------------------
      !   Compute Nitrogen Stress Factors
      !----------------------------------------------------------

      IF (ISWNIT .NE. 'N') THEN
         CALL ML_NFACT(
     &   AGEFAC, CNSD1, CNSD2, ISTAGE, NDEF3, NFAC,
     &   NSTRES, RANC, RCNP, TANC, TCNP, TMNC, XSTAGE)
      ENDIF

      !-------------------------------------------------------------
      !      Compute Water Stress Factors       
      ! ------------------------------------------------------------
      SWFAC  = 1.0
      TURFAC = 1.0

      IF(ISWWAT.NE.'N') THEN
         IF (EOP .GT. 0.0) THEN
            EP1 = EOP * 0.1

            IF (TRWUP / EP1 .LT. RWUEP1) THEN
               TURFAC = (1./RWUEP1) * TRWUP / EP1
            ENDIF

            IF (EP1 .GE. TRWUP) THEN
               SWFAC = TRWUP / EP1
            ENDIF
C          IF (ISTRESS.EQ.4) CALL HrootStress(TURFAC,SWFAC,EOP)

         ENDIF
      ENDIF

      TURFAC = REAL(INT(TURFAC*1000))/1000

      !-------------------------------------------------------------
      !      Compute Water Saturation Factors       
      ! ------------------------------------------------------------
      SATFAC = 0.0    
      SUMEX = 0.0
      SUMRL = 0.0

      DO L = 1,NLAYR

         !------------------------------------------------------------
         !PORMIN = Minimum pore space required for supplying oxygen to 
         !         roots for optimum growth and function    
         !TSS(L) = Number of days soil layer L has been saturated 
         !         above PORMIN
         !------------------------------------------------------------
         IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
            TSS(L) = 0.
         ELSE
            TSS(L) = TSS(L) + 1.
         ENDIF

         !------------------------------------------------------------
         ! Delay of 2 days after soil layer is saturated before root
         ! water uptake is affected
         !------------------------------------------------------------

         IF (TSS(L) .GT. 2.) THEN
            SWEXF = (SAT(L)-SW(L))/PORMIN
            SWEXF = MAX(SWEXF,0.0)
         ELSE
            SWEXF = 1.0
         ENDIF

         SWEXF = MIN(SWEXF,1.0)
         SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
         SUMRL  = SUMRL + DLAYR(L)*RLV(L)
      ENDDO

      IF (SUMRL .GT. 0.0) THEN
         SATFAC = SUMEX/SUMRL
      ELSE
         SATFAC = 0.0
      ENDIF

      SATFAC = AMAX1(SATFAC,0.0)
      SATFAC = AMIN1(SATFAC,1.0)


      !--------------------------------------------------------------
      !          Initialize Some Variables
      !--------------------------------------------------------------
      TOPSINK = 0.0
      TOPMAIN = 0.0
      MGROLF  = 0.0
      MGROSTM = 0.0
      TGROLF  = 0.0
      TGROSTM = 0.0
      GROPAN  = 0.0
      GRORT   = 0.0
      TCARBO  = 0.0

      !-------------------------------------------------------------
      !           Daily Photosynthesis Rate
      !-------------------------------------------------------------
                
      PAR = SRAD*PARSR        !PAR local variable
      LIFAC =1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1
      PCO2  = TABEX (CO2Y,CO2X,CO2,10)
      PCARB = RUE * PAR/PLTPOP * (1.0 - EXP(-LIFAC * LAI))
      PCARB = PCARB*PCO2      !chp added

      TEMPM = (TMAX + TMIN)*0.5   !Mean air temperature, C

      TT    = 0.25*TMIN+0.75*TMAX 
      PRFT = CURV('LIN',PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4),TT)
      PRFT  = AMAX1 (PRFT,0.0)
      PRFT = MIN(PRFT,1.0)


C      Saturated soil water content reduces RWU and causes damage
C      through SWFAC. Do not need to also cause reduction through SATFAC
C WDB 10/20/03      CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES,(1-SATFAC))*SLPF
      CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES)*SLPF
      !Reduce CARBO for assimilate pest damage
      CARBO = CARBO - ASMDOT
      CARBO = MAX(CARBO,0.0)


      !---------------------------------------------------------------
      !      Compute temperature effect on leaf expansion
      !---------------------------------------------------------------
      TEMF  = 1.0

      DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
      END DO


      IF (TMIN .LT. 14.0 .OR. TMAX .GT. 32.0) THEN
         IF (TMAX .LT. TBASE) THEN
            TEMF = 0.0
         ENDIF
         IF (TEMF .NE. 0.0) THEN
            TEMF = 0.0
            DO I = 1, 8
               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
               IF (TTMP .GT. 14.0 .AND. TTMP .LE. 32.0) THEN
                   TEMF = TEMF + 1.0/8.0
               ENDIF
               IF (TTMP .GE.  8.0 .AND. TTMP .LT. 14.0) THEN
                   TEMF = TEMF + 0.021*(TTMP-8.0)
               ENDIF
               IF (TTMP .GT. 32.0 .AND. TTMP .LT. 42.0) THEN
                   TEMF = TEMF + 0.0125*(42.0-TTMP)
               ENDIF
            END DO
         ENDIF
      ENDIF


      !--------------------------------------------------------------
      !       Compute leaf characteristics for different stages
      !--------------------------------------------------------------

      DTT = AMAX1 (DTT,0.0)
      IF (ISTAGE .LE. 3) THEN
         PC    = 1.0
         IF (CUMPH .LT. 5.0) THEN
            PC = 0.66 + 0.068 * CUMPH
         ENDIF
         CUMPH  = CUMPH + DTT / (PHINT * PC)
         XN     = CUMPH
         LEAFNO = XN
      ENDIF

      !--------------------------------------------------------------
      !     ISTAGE = 2
      !--------------------------------------------------------------

      IF (ISTAGE .LE. 2) THEN
          MLAG2   = G1 * (18.0*EXP(0.00787*CUMDTT)-18.4)
          MPLAG   = (MLAG2-MLAG1) * AMIN1(TURFAC,TEMF,AGEFAC)
          MGROLF  = MPLAG  * 0.004
          MGROSTM = MGROLF * 0.20
          IF (CUMDTT .LE. 125.0) THEN
             IF(SEEDRV .GT. 0.0) THEN
                CARBO = SEEDRV + CARBO
                IF (MGROLF .LT. CARBO) THEN
                   GRORT = MGROLF
                ENDIF
                SEEDRV = CARBO - MGROLF - GRORT
                IF (SEEDRV .LT. 0.0) THEN
                   GRORT  = 0.25*CARBO
                   MGROLF = CARBO-GRORT
                   SEEDRV = 0.0
                ENDIF
              ELSE
                GRORT = CARBO - MGROSTM - MGROLF
             END IF
             IF (GRORT .LT. 0.25*CARBO) THEN
                GRORT   = CARBO*0.25
                MGROLF  = 0.65*CARBO
                MGROSTM = CARBO-GRORT-MGROLF
                MPLAG   = MGROLF/0.0040
             END IF
           ELSEIF (CUMDTT .GT. 125.0) THEN
             CALL ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &       G1,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &       CARBO,SUMDTT,DTT,TEMF, ISTAGE)
          END IF

          TOPSINK = MGROSTM + MGROLF + TGROLF + TGROSTM
          TCARBO  = TGROLF  + TGROSTM

          IF (TOPSINK .GT. 0.80*CARBO) THEN
             GRORT   = CARBO   * 0.20
             GRF     = CARBO   * 0.80/TOPSINK
             MGROLF  = MGROLF  * GRF
             MGROSTM = MGROSTM * GRF
             MPLAG   = MGROLF  / 0.0040
             TGROLF  = TGROLF  * GRF
             TGROSTM = TGROSTM * GRF
             TPLAG   = TGROLF  / 0.0040
           ELSE
             GRORT   = CARBO - TOPSINK
          ENDIF

      !--------------------------------------------------------------
      !     ISTAGE = 3
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 3) THEN
          MLAG2  = G1 * (18.0*EXP(0.00787*CUMDTT)-18.4)
          MPLAG  = (MLAG2-MLAG1)* AMIN1(TURFAC,TEMF,AGEFAC)
          MGROLF = MPLAG*0.0045
          GRORT  = CARBO*0.15
          IF (SUMDTT .GT. P3-75.0) THEN              ! P3-75
             MPLAG  = MPLAG*(1.0-((SUMDTT-P3+75.0)/125.0))
             MGROLF = MPLAG*0.0045
          END IF
          MGROSTM = MGROLF*(0.20+1.0*SUMDTT/P3)
          TOPMAIN = MGROSTM + MGROLF
          TCARBO  = CARBO   - MGROLF - MGROSTM - GRORT
C-WALTER  GRF     = 0.45*AMIN1(AGEFAC,TEMF,SWFAC)
          GRF     = 0.45*AMIN1(AGEFAC,TEMF,TURFAC)
          IF (TCARBO .LT. GRF*CARBO) THEN
             TCARBO  = GRF*CARBO
             MGROLF  = (CARBO-GRORT-TCARBO)*(MGROLF/TOPMAIN)
             MGROSTM =  CARBO-GRORT-TCARBO-MGROLF
             MPLAG   = MGROLF/0.0045
          END IF
          IF (TCARBO .GT. 0.0) THEN
              CALL ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &        G1,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &        CARBO,SUMDTT,DTT,TEMF, ISTAGE)
          ENDIF

      !--------------------------------------------------------------
      !     ISTAGE = 4
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 4) THEN
C-WALTER  MGROSTM = 0.095*DTT*AMIN1(TEMF,AGEFAC,SWFAC)   ! 0.15
          MGROSTM = 0.095*DTT*AMIN1(TEMF,AGEFAC,TURFAC)  ! 0.15
          GRORT   = CARBO*(0.25-0.15*AMIN1(SWFAC,NSTRES))
          TCARBO  = CARBO-MGROSTM-GRORT
C-WALTER  GRF     = 0.50*AMIN1(AGEFAC,TEMF,SWFAC)
          GRF     = 0.50*AMIN1(AGEFAC,TEMF,TURFAC)
          IF (TCARBO .LT. CARBO*GRF) THEN
             TCARBO  = CARBO*GRF
             MGROSTM = CARBO-GRORT-TCARBO
          ENDIF
          IF (TCARBO .GT. 0.0) THEN
              CALL ML_TILLSUB (DYNAMIC,TURFAC,AGEFAC,
     &        G1,CUMDTT,TPLAG,TLAG1,TGROLF,TGROSTM,TCARBO,
     &        CARBO,SUMDTT,DTT,TEMF, ISTAGE)
          ENDIF
      !--------------------------------------------------------------
      !     ISTAGE = 5
      !--------------------------------------------------------------
       ELSEIF (ISTAGE .EQ. 5) THEN
          SENF   = 0.85-0.5*AMIN1(AGEFAC,TURFAC)
          CARBO  = CARBO*(1.-SENF*(SUMDTT/P5))   !0.40
          RGFILL = 1.0
          IF (TEMPM .LT. 22.) THEN
             RGFILL = (TEMPM-7.0)/15.0
          ENDIF
          RGFILL = AMAX1 (RGFILL,0.0)
          GROLF  = 0.0
          IF (SUMDTT .LE. P5) THEN
C-WALTER     GROPAN = PGC*(1.0+0.0040*SUMDTT)*AMIN1(SWFAC,AGEFAC)   !.0036
             GROPAN = PGC*(1.0+0.0040*SUMDTT)*AMIN1(TURFAC,AGEFAC) 
             IF (GROPAN .GT. 1.75*PGC) THEN
                GROPAN = 1.75*PGC
             ENDIF
             GROPAN = GROPAN*RGFILL
             GRORT  = CARBO*(0.3-0.28*AMIN1(SWFAC,NSTRES))
             GROSTM = CARBO - GRORT - GROPAN
             IF (GROSTM .LT. 0.0) THEN
                IF (RESERVE .LE. 0.0) THEN
                   GROPAN  = CARBO - GRORT
                   GROSTM  = 0.0
                 ELSE
                   RESERVE = RESERVE+GROSTM
                   GROLF   = GROSTM*0.3
                   GROSTM  = GROSTM*0.7
                ENDIF
              ELSE
                STMGF   = AMIN1((0.5+0.003*SUMDTT),1.0)
                RESERVE = RESERVE+GROSTM*STMGF
             ENDIF
           ELSE
             GROPAN = 0.72-0.006*(SUMDTT-P5)         ! .0067
             GROSTM = CARBO - GROPAN
             IF (GROSTM .LT. 0.0) THEN
                GROSTM = 0.0
                GROPAN = CARBO
             ENDIF
          END IF
          !
          ! Used by N Submodel
          !
          IF (ISWNIT .NE. 'N') THEN
             !
             ! Adjusted from wheat model to millet grain size
             !
             NSINK = GROPAN*TCNP*AGEFAC
             IF (NSINK.NE.0.0) THEN
                RMNC   = RCNP*0.75
                VANC   = STOVN/STOVWT
                NPOOL1 = STOVWT*(VANC-VMNC)
                NPOOL2 = RTWT*(RANC-RMNC)
                NPOOL2 = AMAX1 (NPOOL2,0.0)
                NPOOL1 = AMAX1 (NPOOL1,0.0)
                XNF    = 0.1+0.2*NFAC
                TNLAB  = XNF*NPOOL1
                RNLAB  = XNF*NPOOL2
                NPOOL  = TNLAB+RNLAB
                NSDR   = NPOOL/NSINK
                IF (NSDR .LT. 1.0) THEN
                   NSINK = NSINK*NSDR
                ENDIF

                IF (NSINK .GT. TNLAB) THEN
                   VANC  = VMNC
                   STOVN = STOVN-TNLAB
                   RNOUT = NSINK-TNLAB
                   TNLAB = 0.0
                   ROOTN = ROOTN-RNOUT
                   RANC  = ROOTN/RTWT
                 ELSE
                   STOVN = STOVN-NSINK
                   VANC  = STOVN/STOVWT
                ENDIF
             ENDIF
             GRAINN = GRAINN+NSINK
          ENDIF
      !--------------------------------------------------------------
      !     ISTAGE = 6
      !--------------------------------------------------------------

       ELSEIF (ISTAGE .EQ. 6) THEN
          RETURN
      ENDIF   !End if ISTAGE loop



C--------------------------------------------------------------
C                   APPLY PEST DAMAGE
C--------------------------------------------------------------

      IF(ISWDIS.EQ.'Y') THEN
      ! Leaf Damage
          IF((LFWT+STMWT).GT.0.0)
     $      STOVN=STOVN - STOVN*(WLIDOT/PLTPOP)/(LFWT+STMWT)
          IF (PLTPOP.GT.0.0.AND.LFWT.GT.0.0)
     &      LAIDOT = WLIDOT*(PLA-SENLA)/(LFWT*PLTPOP)  !cm2/plant/day
          IF(PLTPOP.GT.0.0)
     &      LFWT = LFWT - WLIDOT/PLTPOP
          MPLA = MPLA - LAIDOT
          LAI = LAI - LAIDOT*PLTPOP/10000

      ! Stem Damage
          IF(PLTPOP.GT.0.0)
     &       STMWT = STMWT - WSIDOT/PLTPOP
          IF(PLTPOP.GT.0.0.AND.(LFWT+STMWT).GT.0.0)
     &       STOVN=STOVN - STOVN*(WSIDOT/PLTPOP)/(LFWT+STMWT)

      ! Root Weight
          IF(PLTPOP.GT.0.0) THEN
            ROOTN = ROOTN - ROOTN*(WRIDOT/PLTPOP)/RTWT
            RTWT = RTWT - WRIDOT/PLTPOP
          ENDIF

      ! Grain Weight and Number
          IF (PANWT .GT. 0.AND.PLTPOP.GT.0) THEN
            GRAINN = GRAINN - GRAINN*(SWIDOT/PLTPOP)/PANWT
            GPP = GPP - GPP*(SWIDOT/PLTPOP)/PANWT
          ENDIF

          IF(PLTPOP.GT.0.0) THEN 
            PANWT = PANWT - SWIDOT/PLTPOP
          ENDIF

      ! Population
          IF(PPLTD.GT.0) THEN
             PLTPOP = PLTPOP - PLTPOP * PPLTD/100
             LAI = LAI - LAI*(PPLTD/100)
           ENDIF
      ENDIF

           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            PLANTVAR%PLTPOP  = PLTPOP
           CALL GETPUT_PLANTVAR('PUT',PLANTVAR)

C--------------------------------------------------------------------
C      The following code is executed every day regardless of ISTAGE
C--------------------------------------------------------------------
      IF (ISTAGE .NE. 6) THEN
         MPLA   = MPLA   + MPLAG
         TPLA   = TPLA   + TPLAG
         PLA    = MPLA   + TPLA
         MLFWT  = MLFWT  + MGROLF
         TLFWT  = TLFWT  + TGROLF
         MSTMWT = MSTMWT + MGROSTM
         TSTMWT = TSTMWT + TGROSTM
         IF (ISTAGE .NE. 5) THEN
            GROSTM = MGROSTM + TGROSTM
            STMWT  = MSTMWT  + TSTMWT
            GROLF  = MGROLF  + TGROLF
            LFWT   = MLFWT   + TLFWT
          ELSE
            STMWT  = STMWT   + GROSTM
            PANWT  = PANWT   + GROPAN
            LFWT   = LFWT    + GROLF
         ENDIF
         BIOMAS = (LFWT+STMWT+PANWT)*PLTPOP
         DM     = BIOMAS*10.0
         STOVWT = LFWT+STMWT
         IF (CARBO .EQ. 0.0)  THEN
            CARBO = 0.001
         ENDIF
         PDWI   = PCARB*(1.0-GRORT/CARBO)
         PGRORT = PCARB-PDWI
         PTF    = 1.0-GRORT/CARBO
         RTWT   = RTWT+0.5*GRORT-0.005*RTWT

         IF (ISTAGE .LT. 3) SLAN = 0.0
         IF (ISTAGE .EQ. 3) SLAN = PLA*(0.05*(SUMDTT/P3)**2)
         IF (ISTAGE .EQ. 4) SLAN = PLA*(0.05+0.75*((SUMDTT/(P4+P5))**2))
         IF (ISTAGE .EQ. 5) THEN
            SLAN = PLA*(0.05+0.75*((SUMDTT+P4)/(P4+P5))**2)
            IF (SUMDTT .GT. P5) THEN
               SLAN = PLA*(0.8+0.00049*(SUMDTT-P5))
            ENDIF
         ENDIF

         IF (CARBO .EQ. 0.0) THEN
            CARBO = 0.001
         ENDIF
         SLFW   = 0.95+0.05*SWFAC
         SLFN   = 0.95+0.05*AGEFAC
         SLFC   = 1.0
         IF (LAI .GT. 4.0) THEN
            SLFC = 1.0-0.008*(LAI-4.0)
         ENDIF
         SLFT   = 1.0
         IF (TEMPM .LE. 6.0) SLFT = 1.0-(6.0-TEMPM)/6.0
         IF (TMIN  .LE. 0.0) SLFT = 0.0
         SLFT   = AMAX1 (SLFT,0.0)
         PLAS   = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN))
         SENLA  = SENLA+PLAS
         SENLA  = AMAX1 (SENLA,SLAN)
         SENLA  = AMIN1 (SENLA,PLA)

!        Specific leaf area (cm2/g)
         IF (LFWT .GT. 0.) THEN
           SLA = LAI*10000/LFWT  
         ELSE    
           SLA = 0
         ENDIF

         LAI    = (PLA-SENLA)*PLTPOP*0.0001
         MLAG1  = MLAG2
C       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) LAI = REAL(ALAIRESET)

      ENDIF




 
      !--------------------------------------------------------------
      !       Compute Nitrogen Uptake
      !--------------------------------------------------------------

      IF (ISWNIT .NE. 'N') THEN
      CALL ML_NUPTAK(
     %    RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %    RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %    XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %    SHF,PTF,BD, SENESCE)
      ENDIF


C--------------------------------------------------------------------
C     END OF DYNAMIC IF-BLOCK
C--------------------------------------------------------------------
      ENDIF

      RETURN
      END SUBROUTINE ML_GROSUB

C                         DEFINITIONS
C
C  NSINK  : Demand for N associated with grain filling (g/plant/day)
C  NPOOL  : Total plant N available for translocation to grain (g/plant)
C  NPOOL1 : Tops N available for translocation to grain (g/plant)
C  NPOOL2 : Root N available for translocation to grain (g/plant)
C  NSDR   : Plant N supply/demand ratio used to modify grain N content
C  I      : Loop counter
C  PAR    : Daily photosynthetically active radiation, calculated as half
C           the solar radiation - MJ/square metre
C  TT     :
C  PCARB  : Daily amount of carbon fixed - g
C  PRFT   : Photosynthetic reduction factor for low and high temperatures
C  TTMP   :
C  PC     :
C  TI     : Fraction of a phyllochron interval which occurred as a fraction
C           of today's daily thermal time
C  XTN    :
C  A      : Zero to unity factor for relative nitrification rate (unitless)
C  RTR    :
C  TC1    :
C  TC2    :
C  PLATN  :
C  PLAGT  :
C  GRF    :
C  TLG    :
C  FLG    :
C  GGG    :
C  RGFILL : Rate of grain fill - mg/day
C  WSTR   :
C  FSTR   :
C  GROPAN : Daily growth of the pannicle - g
C  RGNFIL :
C  RMNC   : Root minimum nitrogen concentration (g N/g root dry weight)
C  XNF    :
C  TNLAB  :
C  RNLAB  :
C  RNOUT  :
C  SLFW   :
C  SLFN   :
C  SLFC   :
C  SLFT   :
C  PLAS   : The rate of senescence of leaf area on one plant - sq. cm/day
C=======================================================================
!PAR - photosynthetically active radiation, MJ/m2/d
!PARSR - conversion from solar radiation to PAR
!LIFAC - light interception factor used to reduce photosynthesis under
!        wide rowspacing or low population (0-1.0)
!RUE - radiation use efficiency (from ecotype file), g CH2O/MJ PAR
!PCARB       !Potential dry matter production under optimum water, nitrogen and temperature, g/plant
!ROWSPC - Row spacing, cm
!LAI - leaf area index, m2 leaf/m2 ground
!ASMDOT - daily assimilate damage due to pests, g/plant/day
!CARBO - daily plant growth rate, g/plant/day
!TEMPM - average daily temperature, C
!TT - weighted average daily temperature for computing temperatue effect on photosynthesis
!
