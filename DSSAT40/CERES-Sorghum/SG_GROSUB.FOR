C=======================================================================
C  SG_GROSUB, Subroutine
C
C  Determines sorghum growth.
C-----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/07/1993 PWW Header revision and minor changes  
C  02/07/1993 PWW Added switch common block          
C  06/21/1991 JTR/BDB Updated PCARB calculation      
C  07/31/2002 WDB Converted to modular format        
C. 07/31/2002 WDB Added pest damage, SLPF, satfac    
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,I,PAR,TT,PCARB,PRFT,TTMP,PC,TI,
C           XTN,A,RTR,TC1,TC2,PLATN,PLAGT,GRF,TLG,FLG,GGG,RGFILL,WSTR,FSTR,
C           GROPAN,RGNFIL,RMNC,XNF,TNLAB,RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SG_CERES
C
C  Calls  : SG_NFACT SG_NUPTAK
C-----------------------------------------------------------------------

      SUBROUTINE SG_GROSUB (
     & AGEFAC, BIOMAS, CARBO, CNSD1,CNSD2, CO2X, CO2Y, 
     & CO2, CSD2, CUMDTT, CUMPH, DLAYR,DM, DTT,  
     & GPP, GRAINN, GROLF, GRORT, GROSTM, ICSDUR, ISTAGE, 
     & ISWNIT, ISWWAT, LAI, LAT, LEAFNO, LFWT, LL, LWMIN, NDEF3, 
     & NFAC, NLAYR, NH4,NSTRES, NO3, P1, P3, P4, P5, PAF, PANWT, 
     & PDWI, PGC, PGRORT, PHINT, PLA, PLAN, PLAG, PLAO, PLATO, 
     & PLAY, PLTPOP, PTF, RANC, RCNP, RLV,ROOTN, ROWSPC, RTWT, 
     & SAT,SEEDRV, SENLA, SHF, SLAN, SLW, SRAD, 
     & STMWT, STOVN, STOVWT, SW, SWMAX, SWMIN, SUMDTT, SUMRTR, 
     & SWFAC, TANC, TBASE, TCNP,TEMF, TEMPM, TDUR, TILN, 
     & TMAX, TMFAC1, TMIN, TMNC, TRNU,TSIZE, TURFAC,
     & XN,XSTAGE, EOP, TRWUP, RWUEP1,DYNAMIC,UNO3,UNH4,BD,
     & PRFTC,RGFIL,PORMIN,PARSR,RUE,SLPF,SATFAC,FSLFW,FSLFN,
     & ASMDOT,WLIDOT,WSIDOT,WRIDOT,PPLTD,SWIDOT,ISWDIS, SENESCE,
     & control)

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

!     ResidueType defined in ModuleDefs.for
      TYPE (ControlType) CONTROL
      TYPE (ResidueType) SENESCE 
      TYPE (PLANTVARTYPE)   PLANTVAR

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
      REAL CSD2
      REAL CUMDTT
      REAL CUMPH
      REAL CURV
      REAL DM
      REAL DTT
      INTEGER DYNAMIC
      REAL EOP
      REAL EP1
      REAL FSLFW
      REAL FSLFN
      REAL GPP
      REAL GRAINN
      REAL GROLF
      REAL GRORT
      REAL GROSTM
      INTEGER ICSDUR
      INTEGER ISTAGE
      CHARACTER ISWDIS*1
      CHARACTER ISWNIT*1
      CHARACTER ISWWAT*1
      REAL LAI
      REAL LAT
      INTEGER LEAFNO
      REAL LFWT
      REAL LWMIN
      REAL NDEF3
      REAL NFAC
      REAL NSTRES
      REAL P1
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
      REAL PLAY
      REAL PLTPOP
      REAL PRFTC(4)
      REAL PTF
      REAL RANC
      REAL RCNP
      REAL RGFIL(4)
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
      REAL SWMAX
      REAL SWMIN
      REAL SUMDTT
      REAL SUMRTR
      REAL SWFAC
      REAL TANC
      REAL TBASE
      REAL TCNP
      REAL TEMF
      REAL TEMPM
      REAL TDUR
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
      REAL      NSINK,NPOOL1,NPOOL2,NPOOL,NSDR,GCS
      REAL      TT,PCARB,PRFT,TTMP,PC,TI,XTN,A,RTR,TC1,TC2,PLATN,TCON
      REAL      PLAGT,GRF,TLG,FLG,GGG,RGFILL,WSTR,FSTR,GROPAN,RGNFIL
      REAL      RMNC,XNF,TNLAB,RNLAB,RNOUT,SLFW,SLFN,SLFC,SLFT,PLAS
      REAL      TABEX,PCO2

!      SAVE      GCS
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
      REAL  RZrwu(300),RZtrwup,AVG_HROOT,WTDEP,qsr(300),TRWUP1
      REAL AREAWT
      DATA AREAWT/0.0078/
      DOUBLE PRECISION alaireset, WSI(10),heightset
      INTEGER ISTRESS, iresetlai, iresetht1
       common /wuptake/qsr,wtdep,trwup1,RZrwu,RZtrwup,istress,avg_hroot
      common /wsi/ wsi, alaireset, heightset, iresetlai, iresetht1

       TRWUP1=TRWUP
C--------------------------------------------------------------------
C              DYNAMIC = RUNINIT OR SEASINIT
C--------------------------------------------------------------------
C RESET LAI 12-16-2018
       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) THEN
          LAI=REAL(ALAIRESET)
c          XLAI = LAI                !Leaf area index, m2/m2
c          XHLAI = LAI   !Used in WATBAL
          PLA = LAI/PLTPOP*10000.+SENLA
C          LFWT = PLA*0.0078
          AREAWT = LFWT/PLA
       ELSE
          AREAWT = 0.0078
       ENDIF
        
      IF(DYNAMIC .EQ. RUNINIT) THEN
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
         SLAN = 0.0
         SLW = 0.0
         STMWT = 0.0
         STOVN = 0.0
         STOVWT = 0.0
         TILN = 1.0
         TRNU = 0.0
         TSIZE = 0.0
         XN = 0.0

      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN



C-----------------------------------------------------------------------  
C-----------------------------------------------------------------------
C
C                     DYNAMIC = RATE
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.RATE) THEN

!        SENCLN = 0.0  !daily senescence
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
         CALL SG_NFACT(
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

      IF ((ISTAGE .GT. 3) .OR.
     &    (ISTAGE .EQ. 3  .AND. SUMDTT .GT. P3-3.0*PHINT)) THEN
          GO TO 10
      ENDIF

         PC    = 1.0
         IF (CUMPH .LT. 5.0) THEN
          PC = 0.66+0.068*CUMPH
         ENDIF
      TI     = DTT/(PHINT*PC)
      CUMPH  = CUMPH+DTT/(PHINT*PC)
         XN     = CUMPH !+ 1.0  maize has +1, Liwang
      LEAFNO = INT(XN)

      IF (ISTAGE .EQ. 3) THEN
         SLW = 0.0055+0.003*(SUMDTT/P3)
      ENDIF

      GCS    = 0.33
      XTN    = -10.34*EXP(-PLAY*CUMPH)
      A      = 6000.0

      IF (SUMDTT .GE. 120.0 .AND. ISTAGE .LE. 2) THEN
         IF (DTT .LE. 0.0) THEN
            DTT = 1.0
                ENDIF
         SUMRTR = SUMRTR + SRAD * 23.9/DTT
         TDUR   = TDUR   + 1.0
         RTR    = SUMRTR / TDUR
         IF (RTR .LT. 27) THEN
            TILN = 1.0
              ELSE
            TC1  = 1.0/25.0*(RTR-27.0)
            TC2  = 6.25E-5*(40.0-PLTPOP*TILN)**3
            TILN = TILN+TI*AMIN1(TC1,TC2)*TURFAC
            IF (TILN*PLTPOP .GT. 40.0) THEN
               TILN = 40.0/PLTPOP
            ENDIF
            TILN = AMAX1 (TILN,1.0)
             END IF
         IF (LAT .GT. -16.0 .AND. LAT .LE. 0.0) THEN
            TILN = 1.0
         ENDIF
             END IF


C--------------------------------------------------------------------
C                     ISTAGE = 1
C--------------------------------------------------------------------

   10 IF (ISTAGE .EQ. 1) THEN
         PLAN = A*EXP(XTN)
         PLAG = (PLAN-PLAO)*AMIN1(TURFAC,TEMF,AGEFAC)
         PLAO = PLAN

         IF (TILN .GT. 1.0 .AND. CUMPH .GT. 5.0) THEN
            PLATN = (0.79-0.049*(5.0-TILN)**2)*((CUMPH-5.0)**3)*0.5
            PLAGT = (PLATN-PLATO)*AMIN1(TURFAC,TEMF,AGEFAC)
            PLATO = PLATN
            PLAG  = PLAG + PLAGT
          END IF

         GROLF  = PLAG  * 0.0038
         GRORT  = CARBO - GROLF

         IF (GRORT .LE. 0.25*CARBO) THEN
            GRORT  = CARBO*0.25
            SEEDRV = SEEDRV + CARBO - GROLF - GRORT
            IF (SEEDRV .LE. 0.0) THEN
               SEEDRV = 0.0
               GROLF  = CARBO*0.7500
               PLAG   = GROLF/0.0038
            ENDIF
          ENDIF

         PLA    = PLA  + PLAG
         LFWT   = LFWT + GROLF
      ENDIF
C--------------------------------------------------------------------
C                      ISTAGE = 2
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 2) THEN
         PLAN   = A*EXP(XTN)
         PLAG   = (PLAN-PLAO)*AMIN1(TURFAC,TEMF,AGEFAC)
         PLAO   = PLAN

         IF (TILN .GT. 1.0) THEN
            PLATN = (0.79-0.049*(5.0-TILN)**2)*((CUMPH-5.0)**3)*0.5
            PLAGT = (PLATN-PLATO)*AMIN1(TURFAC,TEMF,AGEFAC)
            PLATO = PLATN
            PLAG  = PLAG + PLAGT
         END IF

         CARBO  = CARBO + SEEDRV
         SEEDRV = 0.0
         GROLF  = PLAG*0.0053
         GROSTM = GROLF*0.1

         IF (GROLF+GROSTM .GT. CARBO*0.75) THEN
            GRF    = CARBO*0.75/(GROLF+GROSTM)
            GROLF  = GROLF*GRF
            GROSTM = GROSTM*GRF
            PLAG   = GROLF/0.0053
         END IF

         PLA   = PLA   + PLAG
         GRORT = CARBO - GROLF-GROSTM
         LFWT  = LFWT  + GROLF
         STMWT = STMWT + GROSTM
      ENDIF
C--------------------------------------------------------------------
C                     ISTAGE = 3
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 3) THEN
         PLAN  = A*EXP(XTN)
         PLAG  = (PLAN-PLAO)*AMIN1(TURFAC,TEMF,AGEFAC)
         PLAO  = PLAN
         TLG   = PHINT*5.0

         IF (SUMDTT .GE. P3-TLG) THEN
            PLAG = PLAG*(0.7+0.2*((P3-SUMDTT)/TLG)**2)
         ENDIF

         IF (TILN .GT. 1.0) THEN
            PLATN = (0.79-0.049*(5.0-TILN)**2)*((CUMPH-5.0)**3)*0.5
            PLAGT = (PLATN-PLATO)*AMIN1(TURFAC,TEMF,AGEFAC)
            PLATO = PLATN
            PLAG  = PLAG + PLAGT
            PLAG  = AMAX1 (PLAG,0.0)
C
C            Allow PLAG to decline if tiller dies I.E. PLAGT < 0
C
         END IF

         GROLF = PLAG*AREAWT
         FLG   = 5.0*PHINT

         IF (SUMDTT .LE. P3-FLG) THEN
            GROSTM = GROLF*(0.1+0.8*(SUMDTT/P3)**2)
         ELSE
            GROSTM = 0.048*DTT*TILN*AMIN1(TURFAC,TEMF)*49.0/PHINT
         END IF

         TDUR  = TDUR + 1.0
         GRORT = CARBO - GROLF - GROSTM

         IF (GRORT .LT. CARBO*0.25) THEN
            GRORT  = CARBO  * 0.25
            GRF    = CARBO  * 0.75/(GROLF+GROSTM)
            GROLF  = GROLF  * GRF
            GROSTM = GROSTM * GRF
            PLAG   = GROLF  / AREAWT
            SUMRTR = SUMRTR + GRF
            TCON   = SUMRTR / TDUR
         ELSE
            SUMRTR = SUMRTR + 1.0
            TCON   = SUMRTR / TDUR
         END IF

         TILN  = TILN*(0.4+0.6*TCON)
         TILN  = AMAX1 (TILN,1.0)
         LFWT  = LFWT  + GROLF
         STMWT = STMWT + GROSTM
         PLA   = PLA   + PLAG
         if (CONTROL % crop .eq. 'SG') then
         SLAN  = 1.0+200.0*(SUMDTT/P3)**2  !original for sorghum
         else if ((CONTROL % crop .eq. 'MO')
     &          .or.(control%crop .eq. 'MX')) then
         SLAN  = 1.0+50.0*(SUMDTT/P3)**2   !modified for proso and fox Millet  Sasi
         endif
      ENDIF
C--------------------------------------------------------------------
C                    ISTAGE = 4
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 4) THEN
         GROSTM = 0.07*DTT*(1.0+(TILN-1.0)*TSIZE)*AMIN1(TURFAC,TEMF)*
     &             49.0/PHINT
C
C         GROSTM coeff changed from 0.07 to 0.10
C         TSIZE is relative size of tillers compared to main culm.
C         TSIZE is a function of PLTPOP
C
         GRORT  = CARBO - GROSTM
         GROLF  = 0.0

         IF (GRORT .LT. 0.08*CARBO) THEN
            GRORT  = CARBO*0.08
            GROSTM = CARBO*0.92
          ENDIF

         GGG    = (CUMDTT-P1-100.0-P3)/(P4+P5)
         SLAN   = PLA*(0.07+GCS*GGG)
         STMWT  = STMWT + GROSTM
      ENDIF
C--------------------------------------------------------------------
C                      ISTAGE = 5
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 5) THEN
         IF (PLTPOP .EQ. 0.01) RETURN

         IF (CARBO .EQ. 0.0 .AND. SUMDTT .LT. P5) THEN
            SUMDTT = P5
         ENDIF

         CARBO  = CARBO*(1.0-0.35*(SUMDTT/P5))
         SLAN   = PLA*(0.07+GCS*(CUMDTT-P1-100.0-P3)/(P4+P5))
         RGFILL = 1.0
         RGFILL = CURV('LIN',RGFIL(1),RGFIL(2),RGFIL(3),
     $                     RGFIL(4),TEMPM)
          RGFILL = AMAX1 (RGFILL,0.0)
         RGFILL = MIN(RGFILL,1.0)
         PAF    = 1.0
         PAF    = 1.0-(SUMDTT/P5)*0.85
C
C         IF (SUMDTT .GT. 0.75*P5) PAF = 1.0 - (SUMDTT-P5*0.75)*2.5/P5
C
         WSTR   = 1.0
         FSTR   = 1.0

         IF (ISWWAT .EQ. 'Y') THEN
            WSTR = 1.0 - CSD2 /ICSDUR
         ENDIF

         IF (ISWNIT .EQ. 'Y') THEN
            FSTR = 1.0 - CNSD2/ICSDUR
         ENDIF

         GROPAN = RGFILL*PGC*PAF*(1.0+(TILN-1.)*TSIZE)*AMIN1(FSTR,WSTR)
         GRORT  = 0.0
         GROSTM = CARBO - GROPAN

         IF ((STMWT+GROSTM) .GE. SWMIN) THEN
            IF (GROSTM .GT. 0.0) THEN
               GRORT = GROSTM*0.5
               STMWT = STMWT + GROSTM*0.5
            ELSE
               STMWT = STMWT + GROSTM
            ENDIF
         ELSE
            IF (LFWT .GT. LWMIN) THEN
               STMWT = STMWT+LFWT*0.0050
               LFWT  = LFWT*0.9950
               IF (STMWT+GROSTM .LT. SWMIN) THEN
                  GROPAN = CARBO + (STMWT-SWMIN)
                  STMWT  = SWMIN
               ENDIF
            ELSE
               GROPAN = CARBO + (STMWT-SWMIN)
               STMWT  = SWMIN
            ENDIF
         ENDIF

         IF (ISWNIT .NE. 'N') THEN
C            Code inserted from wheat model
            RGNFIL = 4.82966-3.2488*DTT+0.2503*(TMAX-TMIN)+4.3067*TEMPM
            RGNFIL = RGNFIL*0.28
C           Adjusted from wheat model to sorghum grain size
            NSINK  = RGNFIL*GPP*1.E-6
            NSINK  = NSINK*NDEF3

            IF (NSINK .NE. 0.0) THEN
               RMNC   = RCNP*0.75
               TANC   = STOVN/STOVWT
               NPOOL1 = STOVWT*(TANC-TMNC)
               NPOOL2 = RTWT*(RANC-RMNC)
               NPOOL2 = AMAX1 (NPOOL2,0.0)
               NPOOL1 = AMAX1 (NPOOL1,0.0)
               XNF    = AMIN1((0.18+0.2*NFAC),0.30)
               TNLAB  = XNF*NPOOL1
               RNLAB  = XNF*NPOOL2
               NPOOL  = TNLAB+RNLAB

               NSDR   = NPOOL/NSINK
               IF (NSDR .LT. 1.0) THEN
                  NSINK  = NSINK *NSDR
                  GROPAN = GROPAN*NSDR
               ENDIF

               IF (NSINK .GT. TNLAB) THEN
                  IF (TNLAB .GT. STOVWT*TMNC) THEN
                     TNLAB = STOVWT*TMNC
                  ENDIF
                  STOVN = STOVN - TNLAB
                  RNOUT = NSINK - TNLAB
                  TNLAB = 0.0
                  IF (RNOUT .GT. RTWT*RMNC) THEN
                     RNOUT = RTWT*RMNC
                  ENDIF
                  ROOTN = ROOTN - RNOUT
                  RANC  = ROOTN / RTWT
               ELSE
                  STOVN = STOVN - NSINK
               ENDIF
            ENDIF

            GRAINN = GRAINN + NSINK
            TANC   = STOVN  / STOVWT
         ENDIF

         IF (STMWT .GT. SWMAX) THEN
            GROPAN = GROPAN + (STMWT-SWMAX)*0.8
            GRORT  = GRORT  + (STMWT-SWMAX)*0.2
            STMWT  = SWMAX
         ENDIF

         GROSTM = AMAX1 (GROSTM,0.0)
         PANWT  = PANWT + GROPAN
      ENDIF
C--------------------------------------------------------------------
C                          ISTAGE = 6
C--------------------------------------------------------------------
      IF (ISTAGE .EQ. 6) THEN
         RETURN
      ENDIF

C--------------------------------------------------------------------
C  The following code is executed each day regardless of growth stage
C--------------------------------------------------------------------
      IF (CARBO .EQ. 0.0)  THEN
         CARBO = 0.001
      ENDIF

      PDWI   = PCARB*(1.0-GRORT/CARBO)
      PGRORT = PCARB*GRORT/CARBO

      !------------------------------------------------------------
      !        Compute Leaf Senescence Factors
      !------------------------------------------------------------

!     Senescence due to water
      SLFW   = (1-FSLFW) + FSLFW*SWFAC

!     Senescence due to nitrogen
      SLFN   = (1-FSLFN) + FSLFN*NSTRES

!     Senescence due to light competition
      SLFC   = 1.00   
      IF (LAI .GT. 4.0) THEN
         SLFC = 1.0-0.008*(LAI-4.0)
      ENDIF

      ! Senescence due to temperature
      SLFT   = 1.0
      IF (TEMPM .LE. 6.0) THEN
         SLFT   = 1.0-(6.0-TEMPM)/6.0
      ENDIF
      IF (TMIN .LE. 0.0) THEN
         SLFT = 0.0
      ENDIF
      SLFT   = AMAX1 (SLFT,0.0)
   
C--------------------------------------------------------------
C                   APPLY PEST DAMAGE
C--------------------------------------------------------------

      IF(ISWDIS.EQ.'Y') THEN
!        Leaf Damage
          IF((LFWT+STMWT).GT.0.0)
     $      STOVN=STOVN - STOVN*(WLIDOT/PLTPOP)/(LFWT+STMWT)
          IF (PLTPOP.GT.0.0.AND.LFWT.GT.0.0)
     &      LAIDOT = WLIDOT*(PLA-SENLA)/(LFWT*PLTPOP)  !cm2/plant/day
          IF(PLTPOP.GT.0.0)
     &      LFWT = LFWT - WLIDOT/PLTPOP
   
          PLA = PLA - LAIDOT
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
            GPP = GPP - GPP*(SWIDOT/PLTPOP)/PANWT
            GRAINN = GRAINN - GRAINN*(SWIDOT/PLTPOP)/PANWT
          ENDIF

          IF(PLTPOP.GT.0.0) THEN 
            PANWT = PANWT - SWIDOT/PLTPOP
C           EARWT = EARWT - SWIDOT/PLTPOP
          ENDIF
       
      ! Population
          IF(PPLTD.GT.0) THEN
             PLTPOP = PLTPOP - PLTPOP * PPLTD/100
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            PLANTVAR%PLTPOP  = PLTPOP
           CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
C            EARS = EARS - EARS*PPLTD/100
             LAI = LAI - LAI*(PPLTD/100)
          ENDIF
      ENDIF
   
      !--------------------------------------------------------------
      !    Update State Variables
      !--------------------------------------------------------------
      PLAS   = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN))
      SENLA  = SENLA + PLAS
      SENLA  = AMAX1 (SENLA,SLAN)
      SENLA  = AMIN1 (SENLA, PLA)
      LAI    = (PLA-SENLA)*PLTPOP*0.0001
C       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) LAI = REAL(ALAIRESET)

!     Specific leaf area (cm2/g)
      IF (LFWT .GT. 0.) THEN
        SLA = LAI*10000/LFWT  
      ELSE    
        SLA = 0
      ENDIF

      RTWT   = RTWT + 0.5*GRORT-0.005*RTWT
      BIOMAS = (LFWT+STMWT+PANWT)*PLTPOP
      DM     = BIOMAS*10.0
      STOVWT = LFWT + STMWT
      PTF    = (LFWT+STMWT+PANWT) / (RTWT+LFWT+STMWT+PANWT)

C     Note: senesced leaf area (SENLA) is subtracted from leaf area
C           but is not subtracted from leaf weight in CERES v3.5.
C           the following code needs to be turned on to correct this.
C      LFWT = LFWT - (SENLA/PLA)*LFWT

      !--------------------------------------------------------------
      !       Compute Nitrogen Uptake
      !--------------------------------------------------------------

      IF (ISWNIT .NE. 'N') THEN
      CALL SG_NUPTAK(
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
      END

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
