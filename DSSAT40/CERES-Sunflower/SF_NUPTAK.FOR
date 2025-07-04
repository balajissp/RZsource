C======================================================================
C  SF_NUPTAK, Subroutine
C
C  Determines N uptake
C----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/08/1993 PWW Header revision and minor changes    
C  06/01/1994 WTB Modified 
C  06/28/1994 JR/BDB Changed water content dependent factor 
C  03/29/2001 WBD Converted to modular format      
C  12/01/2001 WDB Major revision for 2002 release   
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
!  03/02/2005 CHP/JIL Add check for negative RNH4U & RNO3U.
!  01/31/2006 CHP/JIL Fixed computation of RNLOSS to match root loss
!                     calculation in SF_GROSUB (from 5% to .5%)
C----------------------------------------------------------------------
C  Called : SF_GROSUB 
C
C  Calls  : None
C----------------------------------------------------------------------
      SUBROUTINE SF_NUPTAK(STMWT,GLFWT,HEADWT,SLFWT,
     %    RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %    RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %    XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %    SHF,PTF,BD, SENESCE,XSTEMN,XNGLF,XHEADN,XNSLF,XLEAFN,
     %    EMBN,PDWIL,PDWIS,PDWIH,XLANC,XSANC,XHANC,XLCNP,XSCNP,XHCNP,
     %    GROEMB,GROPER,PERWT,EMBWT,NFAC,JPEPE,PERN,ISTAGE,TURFAC,TEMPM)

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE
C----------------------------------------------------------------------
C                         Define Variables
C----------------------------------------------------------------------

      REAL        ANDEM              
      REAL        BD(20) 
      REAL        DLAYR(20)   
      REAL        DNG         
      REAL        DROOTN      
      REAL        DSTOVN      
      REAL        FAC(20)     
      REAL        FACTOR      
      REAL        FNH4        
      REAL        FNO3        
      INTEGER     L           
      INTEGER     L1          
      REAL        LL(20)      
      REAL        NDEM        
      REAL        NH4(20)     
      INTEGER     NLAYR       
      REAL        NO3(20)     
      REAL        NUF         
      REAL        PDWI        
      REAL        PGRORT      
      REAL        PLTPOP      
      REAL        PTF         
      REAL        RANC        
      REAL        RCNP        
      REAL        RFAC        
      REAL        RLV(20)     
      REAL        RNDEM       
      REAL        RNH4U(20)   
      REAL        RNLOSS      
      REAL        RNO3U(20)   
      REAL        ROOTN       
      REAL        RTWT
      REAL        SHF(20)     
      REAL        SMDFR       
      REAL        SNH4(20)    
      REAL        SNO3(20)    
      REAL        STOVN       
      REAL        STOVWT      
      REAL        SAT(20)     
      REAL        SW(20)      
      REAL        TANC        
      REAL        TCNP        
      REAL        TNDEM       
      REAL        TRLV        
      REAL        TRNLOS      
      REAL        TRNU        
      REAL        UNH4(20)    
      REAL        UNO3(20)    
      REAL        XMIN        
      REAL        XNDEM       
      REAL        XSTAGE 
      REAL        ENP,PNP,GROPER,PERWT,GROEMB,EMRWT,NFAC,EMBWT,HEADWT
      REAL        XHEADN,XSTEMN,STMWT,XSANC,XLANC,GLFWT,XNGLF,XHANC
      REAL        SLFWT,XLCNP,XHCNP,TCNP2,XSCNP,PTF2,ANO3,ANH4,DNGP,DNGE
      REAL        PDWIL,TOTWT2,DNGL,PDWIS,DNGS,PDWIH,DNGH,DNSL,DNSS,DNSH
      REAL        XLNDEM,XSNDEM,XHNDEM,XPNDEM,XENDEM,DLEAFN,DSTEMN
      REAL        DHEADN,DEMBN,DPERN,TNUP,PTFL,PTFS,PTFH,PERN
      REAL        PTFE,PTFP,FON(20),XLEAFN,XNSLF,XANC,APTNUP,EMBN
      REAL        TEMPM,TMFAC1,TTMP,GRNWT,RTDEP,HRLT,PHOTOP,ZSIND,P3P
      REAL        SFAC,TFAC,TURFAC
      INTEGER     ISTAGE,JPEPE
      CHARACTER   ISWNIT*1
      TYPE (ResidueType) SENESCE


C----------------------------------------------------------------------
C     Initialize variables
C----------------------------------------------------------------------
      TRNLOS = 0.0
C      IF (RTWT.GT.0.0) RANC   = ROOTN / RTWT
C      IF (STOVWT.GT.0.0) TANC   = STOVN / STOVWT
      TRLV   = 0.0
      DROOTN = 0.0
      DSTOVN = 0.0
      TRNU   = 0.0
      NUF    = 0.0
      XNDEM = 0.0
      XMIN = 0.0
      TNDEM = 0.0
      RNLOSS = 0.0
      RNDEM = 0.0
      RFAC = 0.0
      NDEM = 0.0
      FNO3 = 0.0
      FNH4 = 0.0
      FACTOR = 0.0
      DNG = 0.0
      ANDEM = 0.0
      L1 = 0
      UNH4 = 0.0
      UNO3 = 0.0

C-----------------------------------------------------------------------
C     Initialize variables
C-----------------------------------------------------------------------
      SFAC   = 1.125 - 0.1250*TURFAC                      !
      TFAC   = 0.690 + 0.0125*TEMPM                       !
      ENP = (0.0225 + 0.0200*NFAC)*AMAX1(SFAC,TFAC)       ! SUN Emryo N conc.
      PNP = (0.0050 + 0.0100*NFAC)*AMAX1(SFAC,TFAC)       ! SUN Pericarp N conc.

C      ENP    = (0.0225 + 0.0200*NFAC)                ! SUN Emryo N conc.
C      PNP    = (0.0050 + 0.0100*NFAC)                ! SUN Pericarp N conc.
      TRNLOS = 0.0

      IF (ISTAGE .LT. 4) THEN
         GROPER = 0.0
         PERWT  = 0.0
      ENDIF
      IF (ISTAGE .LT. 5) THEN
         GROEMB = 0.0
         EMBWT  = 0.0
      ENDIF
      !
      ! Reset head and stem N amounts
      !
      IF (JPEPE .EQ. 1) THEN
         XHEADN = 0.0420 * HEADWT
         XSTEMN = XSTEMN - XHEADN
         JPEPE  = 0
      ENDIF
       IF (RTWT.GT.0.0) RANC   = ROOTN  / RTWT
       IF (STMWT.GT.0.0) XSANC  = XSTEMN / STMWT                        ! Stem
C     XLANC  = XLEAFN / LFWT                         ! Leaf
      IF (GLFWT.GT.0.0) XLANC  = XNGLF  / GLFWT                        ! Green leaves
      IF (HEADWT .GT. 0.0) THEN
         XHANC = XHEADN / HEADWT
      ENDIF
      TOTWT2 = STMWT + GLFWT + HEADWT + PERWT + EMBWT
      IF (STOVWT.GT.0.0) TANC=STOVN/STOVWT                     !(XSTEMN + XNGLF + XHEADN)/(STOVWT - SLFWT)
      TCNP2  = (XLCNP*GLFWT + XSCNP*STMWT + XHCNP*HEADWT)
      IF (TOTWT2.GT.0.0) THEN
      TCNP2=(TCNP2 + EMBWT*0.0425 + PERWT*0.015)/TOTWT2
      PTF2   = TOTWT2/(TOTWT2 + RTWT)
      ENDIF

      TRLV   = 0.0
      DROOTN = 0.0
      DSTOVN = 0.0
      TRNU   = 0.0
      NUF    = 0.0

      DO L = 1, NLAYR
         RNO3U(L) = 0.0
         RNH4U(L) = 0.0
         TRLV     = TRLV    + RLV(L) * DLAYR(L)
         FAC(L) = 10.0/(BD(L)*DLAYR(L))
         SNO3(L)   = NO3(L) / FAC(L)
         SNH4(L)   = NH4(L) / FAC(L)
      END DO

C-----------------------------------------------------------------------
C     Calculate N demand (DNG=g N/PLT) for new growth (PDWI=g/plt)
C-----------------------------------------------------------------------

      IF (PDWI .EQ. 0.0) THEN
         PDWI = 1.0                                  ! Minimum pot. growth
      ENDIF
      !
      ! Demand of N for new growth of the plant parts
      !
      DNGP   = GROPER * PNP
      DNGE   = GROEMB * ENP
      DNGL   = PDWIL  * XLCNP
      DNGS   = PDWIS  * XSCNP
      DNGH   = PDWIH  * XHCNP
      DNSL   = GLFWT  * (XLCNP - XLANC)
      DNSS   = STMWT  * (XSCNP - XSANC)
      DNSH   = HEADWT * (XHCNP - XHANC)
      !
      ! N demand for new growth
      !
      DNG    = DNGL + DNGS + DNGH + DNGE + DNGP      
      !
      ! Total N demand for the plant parts
      !
      XLNDEM = DNSL + DNGL
      XSNDEM = DNSS + DNGS
      XHNDEM = DNSH + DNGH
      IF (PERWT .GT. 0.0) THEN
C        XPNDEM = PERWT*(0.015-PERN/PERWT)+DNGP
         XPNDEM = DNGP
       ELSE
         IF (GROPER .GT. 0.0) THEN
            XPNDEM = DNGP
          ELSE
            XPNDEM = 0.0
         ENDIF
      ENDIF
      IF (EMBWT .GT. 0.0) THEN
C        XENDEM = EMBWT *(0.0425-EMBN/EMBWT)+DNGE
         XENDEM = DNGE
       ELSE
         IF (GROEMB .GT. 0.0) THEN
            XENDEM = DNGE
          ELSE
            XENDEM = 0.0
         ENDIF
      ENDIF
C     IF (XPNDEM.LT.0.)XPNDEM=0.
C     IF(XENDEM.LT.0.)XENDEM=0.
C     IF(XLNDEM.LT.0.)XLNDEM=0.
C     IF(XSNDEM.LT.0.)XSNDEM=0.
C     IF(XHNDEM.LT.0.)XHNDEM=0.

C     DNG = PDWI*TCNP

C-----------------------------------------------------------------------
C     Calculate total N demand (NDEM) of tops (TNDEM) and roots (RNDEM),
C     all expressed in g N/plt Convert total N demand to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      TNDEM = XLNDEM + XSNDEM + XHNDEM
      RNDEM = RTWT * (RCNP-RANC) + PGRORT*RCNP       ! Roots N demand
C     IF (RNDEM .LT. 0.)RNDEM=0.
C
      NDEM  = TNDEM + RNDEM
      IF (NDEM .EQ. 0.0) THEN
         NDEM = 0.001
      ENDIF

C-----------------------------------------------------------------------
C     Convert total N demand from g N/plt to kg N/ha (ANDEM)
C-----------------------------------------------------------------------

      ANDEM  = NDEM   * PLTPOP*10.0

C-----------------------------------------------------------------------
C     Calculate potential N supply in soil layers with roots (TRNU)
C-----------------------------------------------------------------------

      DROOTN = 0.0
      DSTOVN = 0.0
      DLEAFN = 0.0
      DSTEMN = 0.0
      DHEADN = 0.0
      DEMBN  = 0.0
      DPERN  = 0.0
      TRNU   = 0.0
      TNUP   = 0.0
      NUF    = 0.0

      DO L = 1, NLAYR
         IF (RLV(L) .NE. 0.0) THEN
            L1 = L
            !
            ! The following code was written by JTR to simplify the code for
            ! the generic model and to make the functions more like the water
            ! uptake functions.  Done on June 28, 1994.
            !
            ! New water content dependent factor for uptake and new uptake
            ! methods that work like the soil water uptake  -- JTR 6/94
            !
            SMDFR    = 1.5-6.0*((SW(L)-LL(L))/(SAT(L)-LL(L))-0.5)**2
            SMDFR    = AMAX1 (SMDFR,0.0)
            SMDFR    = AMIN1 (SMDFR,1.0)
            RFAC     = 1.0-EXP(-8.0*RLV(L))
            FNH4     = SHF(L)*0.075
            FNO3     = SHF(L)*0.075
            RNH4U(L) = RFAC*SMDFR*FNH4*(NH4(L)-0.5)*DLAYR(L)
            RNO3U(L) = RFAC*SMDFR*FNO3*NO3(L)*DLAYR(L)
            RNH4U(L) = MAX(RNH4U(L),0.0)
            RNO3U(L) = MAX(RNO3U(L),0.0) 
            TRNU     = TRNU + RNO3U(L) + RNH4U(L)
         ENDIF
      END DO

C-----------------------------------------------------------------------
C     Calculate factor (NUF) to reduce N uptake to level of demand
C-----------------------------------------------------------------------

      IF (ANDEM .LE. 0.0) THEN
         TRNU  = 0.0
         NUF   = 0.0
       ELSE
         ANDEM = AMIN1 (ANDEM,TRNU)
         IF (TRNU .EQ. 0.0) RETURN
         NUF   = ANDEM/TRNU
         TRNU  = 0.0
      ENDIF

C-------------------------------------------------------------------------
C     Calculate N uptake in soil layers with roots based on demand (kg/ha)
C-------------------------------------------------------------------------

      DO L = 1, L1
         UNO3(L) = RNO3U(L)*NUF
         UNH4(L) = RNH4U(L)*NUF
         XMIN    = 0.25/FAC(L)
         UNO3(L) = MIN (UNO3(L),SNO3(L) - XMIN)
         UNO3(L) = MAX(UNO3(L),0.0)  
         XMIN    = 0.5/FAC(L)
         UNH4(L) = MIN (UNH4(L),SNH4(L) - XMIN)
         UNH4(L) = MAX(UNH4(L),0.0) 
         TRNU    = TRNU + UNO3(L) + UNH4(L)   !kg[N]/ha
      END DO

      IF (PLTPOP .GT. 0.0) THEN
        TRNU = TRNU/(PLTPOP*10.0)               !g[N]/plant
      ELSE
        TRNU = 0.0
      ENDIF

C-----------------------------------------------------------------------
C     Update stover and root N
C-----------------------------------------------------------------------

      IF (NDEM .GT. TRNU) THEN
          XNDEM  = TRNU
          FACTOR = XNDEM / NDEM
          NDEM   = XNDEM
          !
          ! Reduce demand to potential N supply
          !
          XLNDEM = XLNDEM * FACTOR
          XSNDEM = XSNDEM * FACTOR
          XHNDEM = XHNDEM * FACTOR
          XENDEM = XENDEM * FACTOR
          XPNDEM = XPNDEM * FACTOR
          TNDEM  = TNDEM  * FACTOR
          RNDEM  = RNDEM  * FACTOR
      ENDIF
      !
      ! Fractions of organs over the whole plant weight
      !
      PTFL = PTF  * GLFWT  / STOVWT
      PTFS = PTF  * STMWT  / STOVWT
      PTFH = PTF  * HEADWT / STOVWT
      PTFE = PTF2 * EMBWT  / TOTWT2
      PTFP = PTF2 * PERWT  / TOTWT2
C
C     Increment in N amount per organ
C
C     DLEAFN = XLNDEM/NDEM*TRNU - PTFL*TRNLOS/(PLTPOP*10.0)
C     DSTEMN = XSNDEM/NDEM*TRNU - PTFS*TRNLOS/(PLTPOP*10.0)
C     DHEADN = XHNDEM/NDEM*TRNU - PTFH*TRNLOS/(PLTPOP*10.0)
c     DEMBN  = XENDEM/NDEM*TRNU
c     DPERN  = XPNDEM/NDEM*TRNU
C          
      IF (NDEM .LE. 0.0 .OR. TRNU .LE. 0.0) THEN
         DSTOVN = 0.0
         DROOTN = 0.0
       ELSE
         !
         ! Calculate root exudation losses (if any)
         !
         DO L = 1, L1
            RNLOSS = 0.0
            IF (TANC .GT. TCNP) THEN
               RNLOSS = RANC*RTWT*0.05*PLTPOP*RLV(L)/TRLV
            ENDIF
            TRNLOS  = TRNLOS + RNLOSS
            FON(L)  = FON(L) + RNLOSS
         END DO
         !
         ! Increment in N amount per organ
         !
         DLEAFN = XLNDEM/NDEM*TRNU - PTFL*TRNLOS/(PLTPOP*10.0)
         DSTEMN = XSNDEM/NDEM*TRNU - PTFS*TRNLOS/(PLTPOP*10.0)
         DHEADN = XHNDEM/NDEM*TRNU - PTFH*TRNLOS/(PLTPOP*10.0)
         DSTOVN = DLEAFN + DSTEMN + DHEADN
         DROOTN = RNDEM /NDEM*TRNU - (1.0-PTF)*TRNLOS/(PLTPOP*10.0)
      ENDIF
C
C     Cumulative N amount per organ
C
      XLEAFN = XLEAFN + DLEAFN
      XNGLF  = XNGLF  + DLEAFN
      XNSLF  = XLEAFN - XNGLF
      XSTEMN = XSTEMN + DSTEMN
      XHEADN = XHEADN + DHEADN
c     EMBN   = EMBN   + DEMBN
c     PERN   = PERN   + DPERN

C
      STOVN  = XNGLF + XSTEMN + XHEADN + XNSLF
C     TANC   = (STOVN-XNSLF)/(STOVWT-SLFWT)
      ROOTN  = ROOTN + DROOTN
      RANC   = ROOTN / (RTWT-0.01*RTWT)
      
      RETURN
      END SUBROUTINE SF_NUPTAK


C--------------------------------------------------------------------------------------------------
C                         Define Variables
C--------------------------------------------------------------------------------------------------

! ANDEM       !Crop N demand (kg N/ha)
! ANO3        !Total extractable nitrate N in soil profile (kg N/ha)
! ANH4        !Total extractable ammonium N in soil profile (kg N/ha)
! DLAYR(L)    Soil thickness in layer L (cm)
! DNG         !N demand of potential new growth of tops (g N/plant)
! DROOTN      !Daily change in plant root nitrogen content (g N/plant)
! DSTOVN      !Daily change in plant stover nitrogen content (g N/plant)
! FAC(20)     !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! FACTOR      !Ratio of root N uptake to plant N demand
! FNH4        !Unitless soil ammonium supply index
! FNO3        !Unitless soil nitrate supply index
! FON(20)     !Fresh organic nitrogen in soil layer L due to root senescence, kg N/ha
! L           !Index counter
! L1          !Lowest soil layer with roots
! LL(20)      !Lower limit of plant extractable water for soil layer L, cm3/cm3
! NDEM        !Plant nitrogen demand, g/plant
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       !Number of soil layer
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil)
! NUF         !Plant N supply to demand used to modify N uptake
! PDWI        !Potential increment in new shoot growth, g/plant
! PGRORT      !Potential increment in new root growth, g/plant
! PLTPOP      !Plant population, plants/m2
! PTF         !Ratio of above ground biomass to total biomass
! RANC        !Root actual N concentration, g N/g root
! RCNP        !Root critical nitrogen concentration, g N/g root dry weight
! RFAC        !Interim variable describing the effects of root length density on potential N uptake from a layer
! RLV(20)     !Root length density for soil layer L, cm root/cm2 soil
! RNDEM       !Plant root demand for nitrogen (g/plant)
! RNH4U(20)   !Potential ammonia uptake from layer L, kg N/ha
! RNLOSS      !Loss of N from the plant via root exudation in one layer (g N/m2)
! RNO3U(20)   !Potential nitrate uptake from layer L, kg N/ha
! ROOTN       !Root nitrogen content, g N/plant
! RTWT        !Root weight, g/plant
! SHF(20)     !Relative root distribution in soil layer L (0-1)
! SMDFR       !Soil moisture deficit factor affecting N uptake
! SNH4(20)    !Ammonium nitrogen in soil layer L, kg N/ha
! SNO3(20)    !Nitrate content in soil layer L, kg N/ha
! STOVN       !Nitrogen content in stover, g N/plant
! STOVWT      !Stover weight (Stem + leaf), g/plant
! SAT(20)     !Saturated water holding capacity for soil layer L, cm3/cm3
! SW(20)      !Soil water content in layer L, cm3 water/cm3 soil
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry weight
! TNDEM       !Plant tops demand for nitrogen (g N/plant)
! TRLV        !Total root length density, cm root/cm2 soil
! TRNLOS      !Total plant N lost by root exudation (g N/m2)
! TRNU        !Total potential root nitrogen uptake, kg N/ha
! UNH4(20)    !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)    !Plant uptake of nitrate from a layer (kg N/ha/day)
! XMIN        !
! XNDEM       !Temporary variable
! XSTAGE      !Non-integer growth stage indicator

