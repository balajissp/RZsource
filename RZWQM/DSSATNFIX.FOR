C=======================================================================
C  NFIX, Subroutine, J.W. Jones, G. Hoogenboom, and K.J. Boote
C-----------------------------------------------------------------------
C  Calculates nitrogen fixation
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/09/1989 JWJ Written.
C  01/03/1993 WTB Modified.
C  07/14/1995 JWH Modified to reserve some C for nodule growth.
C  09/26/1995 KJB Modified to put temperature, water deficit, flooding,
C                 and age effects, not on RGR or SNA, but on growth or
C                 N-fix per unit land area.  Helped greatly for water
C                 deficit effects.  Also, C not used for N-fixation
C                 because of water deficit, is not allowed to be used
C                 for growth of nodules, except a small amount.
C  10/07/1995 KJB Modified to exclude evaporating (layer 1) zone from
C                 the nodule zone (DNOD).  Cancelled 6/25/97 KJB
C  01/19/1996 KJB Add a eigtht day memory of soil water deficiet (SWMEM8)
C                 on specific nodule activity.
C  06/25/1997 KJB Modified to use TURFAC to affect nodule growth and NFIX
C                 rather than SWFACT (frac avail. soil water in nodule
C                 zone.  Poor experience with it and recent lit indicates
C                 that nodules respond more to plant water status than to
C                 soil water around nodules.  Also, now we can use more
C                 shallow nodule zone (starting at top to 30 cm) to
C                 compute soil temperature effect and flooding effect.
C  02/02/1998 GH  Deleted CUSTOV, not used
C  07/07/1998 CHP Modified for modular format
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/26/2001 GH  Correct DAS
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------
C  Called from:  PLANT
C  Calls:        FIND, ERROR, IGNORE
C=======================================================================

      SUBROUTINE DSSATNFIX(PLTPOP,SAT, ST, SW, SWEWP,RTL,NN,firstnfix)  
C-----------------------------------------------------------------------
      IMPLICIT NONE
c      SAVE
      logical firstnfix
      CHARACTER*3 TYPFXT,TYPNGT,TYPFXD,TYPFXW,TYPFXA
C      CHARACTER*6 ERRKEY
C      PARAMETER (ERRKEY = 'NFIX')

      CHARACTER*80 C80

      INTEGER NN

      INTEGER I, II, NR7, J, YRDOY, YRSIM, DAS
C      INTEGER DYNAMIC, TIMDIF
      INTEGER SDWNOD

      double precision NDTHMX, NFIXN, NODRGM, NDTH, NODGR
      double precision NODRGR, NFXAGE
      double precision SWFACT, FLDACT, CNFACT, FRCNM
      double precision ACSTF, ACSTG, DSW, FLDSUM, FLAYR, TNFIX, TNGRO
      double precision CLEFT, SNACT, RNDTH, CTONOD, DWNODI, DWNOD, CNOD
      double precision CUSFIX, DNOD, SNACTM, EFNFIX, EFINOC, CNODCR
      double precision AGRNOD, RFIXN, DXR57,CNODGR
      double precision CURV
      double precision EPORS, PLTPOP, DWNODA, WTNFX, PRONOD, CNODMN
      double precision SWMEM8, PCSFIX, CNOFIX, PNFIXN,TURFAC
      real FNFXT(4), FNNGT(4), FNFXD(4), FNFXW(4), FNFXA(4)
      double precision SWMEM(9),SAT(NN),ST(NN),SWEWP,SW(NN),RTL(NN)
C      double precision DLAYR(NL), SAT(NL), SW(NL), ST(NL)
      double precision LAYERFRAC(300), SENNOD(300), DSWP
      double precision CTONODS,WNDOT,WTNOO,NNOFF,WTNNO,WTNNAG
!       double precision DUL(NL), LL(NL)
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      double precision PLIPNO,PLIGNO,POANO,PMINNO,PCARNO,RLIP,RLIG,ROA,
     &     FRCNOD,TTFIX,RCH2O,RMIN,PCH2O
      data CTONOD /0.0/
      COMMON/NFIX1/CNOD,DWNOD,DWNODA,NDTH,NFIXN,NODGR,WTNFX,
     +  SENNOD,DNOD,CTONOD,CTONODS,WNDOT,WTNOO,NNOFF,WTNNO,PRONOD
C N-FIXATION PARAMETERS FROM SBGRO040.SPE
      PLIPNO=0.05d0
      PLIGNO=0.07d0
      POANO=0.05d0
      PMINNO=0.05d0
      PCARNO=0.48d0
      RLIP=3.106d0
      RLIG=2.174d0
      ROA=0.929d0
      RMIN=0.05d0
      PCH2O=1.13d0
      FRCNOD=0.05d0
      RFIXN=2.830d0
      PRONOD=0.30d0
C NOT SURE PARAMETERS
      SNACTM=0.045d0
      NODRGM=0.170d0
      DWNODI=0.014d0
      TTFIX=0.0d0
      NDTHMX=0.07d0
      CNODCR=0.05d0
      FNNGT(1)=7.00
      FNNGT(2)=22.0
      FNNGT(3)=35.0
      FNNGT(4)=44.0
      FNFXT(1)=5.00
      FNFXT(2)=20.0
      FNFXT(3)=35.0
      FNFXT(4)=44.0
      FNFXD(1)=0.00
      FNFXD(2)=0.85
      FNFXD(3)=1.0
      FNFXD(4)=10.0
      FNFXW(1)=-0.02
      FNFXW(2)=0.001
      FNFXW(3)=1.00
      FNFXW(4)=2.0
      FNFXA(1)=0.00
      FNFXA(2)=0.10
      FNFXA(3)=1.0
      FNFXA(4)=0.00
      EFINOC = 1.0d0
      EFNFIX = 1.0d0
      TYPNGT='LIN'
      TYPFXT='LIN'
      TYPFXD='LIN'
      TYPFXW='LIN'
      TYPFXA='INL'
C     
!***********************************************************************
      AGRNOD =  PLIPNO*RLIP + PLIGNO*RLIG + POANO*ROA
     &         + PMINNO*RMIN + PCARNO*RCH2O
C-----------------------------------------------------------------------
C     Compute CH2O cost per g of tissue, excluding cost for protein (AGRVG)
C     and total CH2O cost per g of veg tissue (AGRVG2)
C-----------------------------------------------------------------------
C      AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
C      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI)*RPROAV
C-----------------------------------------------------------------------
C    Compute N Demand for New Tissue, including reproductive and vegetative
C    NDMVEG=TTN1/10.0
C    NDMNEW=DMDNIT/10.0
C-----------------------------------------------------------------------
C      NDMVEG = (CDMVEG/AGRVG2) * (FRLF*FNINL+FRSTM*FNINS+
C     &   FRRT*FNINR)
C      NDMNEW = NDMREP + NDMVEG
C-----------------------------------------------------------------------
C     Allow some of today's PG to be used for N fixation, depending
C     on N uptake and mining, and on demand for N.
C     NAVLV = N available for veg growth from uptake and mining
C     CAVVEG = C available for veg growth
C     NDMVEG = N required for veg growth if all PGAVL is used as computed
C     CNDFX = carbon needed to fix N needed but not supplied by uptake or mining
C     PROVEG = average protein composition of growing tissue today
C     CTONOD = C to allocate to nodules to fix N needed for Rep and Veg growth
c     NDMREP = (TTN2-TTN1)/10.0
C     NAVL = (TNITUP + ALFTN)/10.0
C     FROM CH2O TO C  12/30
C     C/N RATIO 0.4
C     KG N TO KG CH2O /0.16  (0.4*12/30)
C     PGAVL=PNCA/(12/30)/10*PWRTS/1.0D-3  SUMCL()?
C     CDMREP = PPROP+TPVS
C-----------------------------------------------------------------------
c      CTONODR = MAX(0.0, (NDMREP-NAVL)*RFIXN/0.16)
C      CTONODR = MIN(CTONODR,PGAVL)
C      CTONODR = MAX(0.0, ((DMDNIT-ALFTN-TNITUP)/10.0)*RFIXN/0.16)
C      CTONODR = MIN(CTONODR,PGAVL)
C      CTONOD = 0.0
C      CAVVEG = MAX(0.,(PGAVL - CDMREP))
C      NAVLV = MAX(0.,(NAVL-NDMREP))
C      CNDFX = MAX(0.,(RFIXN/0.16)*(NDMVEG-NAVLV))
C      IF(CAVVEG .GT. 0.0 .AND. CNDFX .GT. 0.0) THEN
C         PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM
C         CTONOD = CAVVEG - (CAVVEG +
C     &            (NAVLV*RFIXN/0.16))*AGRVG/(AGRVG+PROVEG*RFIXN)
C-----------------------------------------------------------------------
C     Reserve for nodule growth an amount of C equivalent to a fixed
C     fraction (FRCNOD) of C allocated to root growth.  JWH 7/11/95
c     dmdrt is in kg C/ha from rzplnt.for
c     cavveg is C available for vegetative tissue growth (g [ch2o]/m2/d]
C-----------------------------------------------------------------------
C      IF (DAS .LT. NR2) THEN   !ask Ken Boote
c        CNODMN = CAVVEG * FRRT * FRCNOD
        CNODMN = CTONOD * FRCNOD
C      ELSE
C        CNODMN = 0.0
C      END IF
C      CTONOD = MIN(CNODMN + MAX(0.0, CTONOD), CAVVEG) + CTONODR
C-----------------------------------------------------------------------
C       If ISWSYM = U, then N-FIXATION is assumed to occur at a rate
C       that carbon will allow, and nodules are not grown explicitely
C-----------------------------------------------------------------------
c      IF ((ISWNIT .EQ. 'Y') .AND. (ISWSYM .EQ. 'U') .OR.
c     &   (ISWNIT .NE. 'Y')) THEN
c        NFIXN = MAX(0.0,NDMREP + NDMVEG - NAVL)
c        CNOD = RFIXN * NFIXN/0.16
c      ENDIF
C-----------------------------------------------------------------------
C    Accumulate NAVL for growth, reduce PGAVL by cost to fix N
C-----------------------------------------------------------------------
C      IF (PGAVL .GT. CNOD) THEN
C         PGAVL = PGAVL - CNOD
C      ELSE
C         PGAVL = 0.0
C      ENDIF
C      NAVL = NAVL + NFIXN
C-----------------------------------------------------------------------
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
C      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C      CNOD   = 0.0
C      DWNOD  = 0.0    
C      DWNODA = 0.0  
C      NDTH   = 0.0    
C      NFIXN  = 0.0    
C      NODGR  = 0.0    
C      WTNFX  = 0.0    
C      SDWNOD = 0 
C      SENNOD = 0.0 

C      DNOD   = 30.0   !why set up to 50 later?

C***********************************************************************
C***********************************************************************
C     DAILY RATE/INTEGRATION
C***********************************************************************
C      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C      DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
C-----------------------------------------------------------------------
C   Set initial nodule mass to DWNODI as read from crop species file
C-----------------------------------------------------------------------
      IF (firstnfix) THEN
         DWNOD  = DWNODI * PLTPOP
         firstnfix=.false.
         DWNODA = DWNODI * PLTPOP
         WTNFX  = DWNODA * 0.16d0 * PRONOD
         DO J = 1,8
           SWMEM(J) = 1.0d0
         ENDDO
      ENDIF

!-----------------------------------------------------------------------
C   Initialize soil water and temperature factors (top DNOD cm of soil)
C-----------------------------------------------------------------------
      SWFACT = 1.0d0
      FLDACT = 1.0d0
      ACSTF  = 0.0d0
      ACSTG  = 0.0d0
      DSW    = 0.0d0
      FLDSUM = 0.0d0
C-----------------------------------------------------------------------
C     Calculate carbon allocated per unit of nodule biomass:
C     CNODCR = C requirement for nodule respiration (g C/g nodule/d)
C-----------------------------------------------------------------------
      CNFACT = 1.d0
      IF (DWNOD .GT. 1E-4) THEN
        FRCNM = CTONOD/DWNOD
        IF (FRCNM .LT. CNODCR) CNFACT = FRCNM / CNODCR
      ENDIF
C-----------------------------------------------------------------------
C   Calculate soil water and temperature factors for each layer to DNOD
C-----------------------------------------------------------------------
      LAYERFRAC = 0.0d0
      DSWP = 0.0d0
      DNOD = 50.0d0
      DO I = 1,NN
         FLAYR = 1.0d0
         DSW = DSW + RTL(I)
         IF (DSW .GT. DNOD) FLAYR = (DNOD-(DSW-RTL(I)))/RTL(I)

         ACSTF = ACSTF + RTL(I) * FLAYR *
     &     dble(CURV(TYPFXT,FNFXT(1),FNFXT(2),FNFXT(3),
     &         FNFXT(4),real(ST(I))))

         ACSTG = ACSTG + RTL(I) * FLAYR *
     &     real(CURV(TYPNGT,FNNGT(1),FNNGT(2),FNNGT(3),
     &     FNNGT(4),real(ST(I))))

         EPORS = MAX(SAT(I) - SW(I), 0.0d0)
         FLDSUM = FLDSUM + RTL(I) * FLAYR *
     &     real(CURV(TYPFXW,FNFXW(1),FNFXW(2),FNFXW(3),
     &      FNFXW(4),real(EPORS)))

         IF (I .EQ. 1) THEN
           LAYERFRAC(1) = DSW / DNOD
         ELSE
           LAYERFRAC(I) = (DSW - DSWP)*FLAYR / DNOD
         ENDIF
         DSWP = DSW
         IF ( FLAYR .LT. 1.0d0 ) GOTO 400

      ENDDO
C-----------------------------------------------------------------------
C   Constraints due to soil water and T and average nodule age:
C   TNFIX : soil T effect on N2 fixation
C   TNGRO : soil T effect on nodule growth
C   SWFACT: soil water deficit effect on N2 fixation and nodule growth
C   FLDACT: soil water flooding effect on N2 fixation and nodule growth
C   NFXAGE: average nodule age effect on nodule growth
C-----------------------------------------------------------------------
  400 TNFIX  = ACSTF / DNOD
      TNGRO  = ACSTG / DNOD
      FLDACT = FLDSUM / DNOD

      SWFACT = dble(CURV(TYPFXD,FNFXD(1),FNFXD(2),FNFXD(3),
     +        FNFXD(4),real(SWEWP)))
      NFXAGE = dble(CURV(TYPFXA,FNFXA(1),FNFXA(2),FNFXA(3),
     +        FNFXA(4),0.0))
C-----------------------------------------------------------------------
C DETERMINE MEMORY OF PREVIOUS EIGHT DAYS OF SOIL WATER DEFICITS
C-----------------------------------------------------------------------
      DO J= 8,2,-1
          SWMEM(J) = SWMEM(J-1)
      ENDDO
      SWMEM(1) = SWFACT

      SWMEM8 = 0.0d0
      DO J = 1,8
          SWMEM8 = SWMEM8 + SWMEM(J)
      ENDDO
      SWMEM8 = SWMEM8/8.0d0
C-----------------------------------------------------------------------
C     Reserve CNODMN for nodule growth.  JWH 7/9/95
C-----------------------------------------------------------------------
      CLEFT = CTONOD - CNODMN
C-----------------------------------------------------------------------
C    Compute Specific Nodule Activity taking into account the maximum
C       activity of the nodules (SNACTM), and strain effects only.
C    9/27/95 moved temp, water deficit, and soil water flooding effects
C    below to the primary rate.  We are not getting proper stress effects.
C-----------------------------------------------------------------------
      SNACT  = SNACTM  * EFNFIX
C-----------------------------------------------------------------------
C       Compute nodule death rate as function of SW deficit, SW flooding,
!                               and carbon deficit (chp)
C-----------------------------------------------------------------------
      RNDTH = NDTHMX * MAX((1.d0-FLDACT),(1.d0-SWFACT),(1.d0-CNFACT))
      NDTH = MIN(1.0d0,RNDTH) * DWNOD               !g/m2
      DO I = 1, NN
        SENNOD(I) = NDTH * LAYERFRAC(I) * 10.d0     !kg/ha
      ENDDO
C-----------------------------------------------------------------------
C    Compute N-Fixation
C
C-----------------------------------------------------------------------
c      IF (DAS .LT. NR7) THEN
         PNFIXN = MIN((CLEFT * 0.16d0 / RFIXN), (DWNOD * SNACT)) * TNFIX
         NFIXN = PNFIXN * MIN(SWFACT, SWMEM8, FLDACT)
c      ELSE
c         PNFIXN = 0.0
c         NFIXN = 0.0
c      ENDIF
C-----------------------------------------------------------------------
C    Compute C Used for N-Fixation
C-----------------------------------------------------------------------
      PCSFIX = (PNFIXN / 0.16d0) * RFIXN
      CUSFIX = (NFIXN  / 0.16d0) * RFIXN
      CNOFIX = PCSFIX - CUSFIX
C-----------------------------------------------------------------------
C     Compute C Left to Grow New Nodule Mass
C     Includes minimum reserved for nodule growth (CNODMN) plus any C
C     left after N fixation.  JWH 7/11/95
C-----------------------------------------------------------------------
      CLEFT = MAX(0.0d0,CLEFT - CUSFIX- 0.9d0*CNOFIX) + CNODMN
C      CTONODS=CTONODS+CLEFT
C-----------------------------------------------------------------------
C    Compute Potential Growth of Nodules (Demand)
C    EFNFIX = strain efficiency
C    EFINOC = inoculation effectiveness (or rhizobium density factor)
C-----------------------------------------------------------------------
c      IF (DAS .LT. NR7) THEN
         NODRGR = NODRGM  * EFNFIX * EFINOC
c      ELSE
c         NODRGR = 0.0
c      ENDIF
C-----------------------------------------------------------------------
C    Compute Nodule Growth, Limiting by Either Supply or Demand for C
C-----------------------------------------------------------------------
      NODGR = MIN(CLEFT/AGRNOD,DWNOD*NODRGR)
     &        * TNGRO * MIN(SWFACT,FLDACT) * NFXAGE
      CNODGR = NODGR * AGRNOD
C-----------------------------------------------------------------------
C    Compute C used in N-Fixation and Nodule Growth (Including
C    Respiration Costs) Today
C-----------------------------------------------------------------------
      CNOD = CUSFIX + CNODGR
      CTONODS=CTONODS + (CTONOD-CNOD) + CNODMN
!***********************************************************************
C-----------------------------------------------------------------------
C     Net nodule growth rate
C-----------------------------------------------------------------------
      WNDOT  = NODGR - NDTH            
      DWNOD  = DWNOD  + WNDOT
      WTNOO = WTNOO + NDTH
      NNOFF  = NDTH * 0.16d0 * PRONOD
      WTNNO  = WTNNO  + NNOFF
C-----------------------------------------------------------------------
C    Nodules
C-----------------------------------------------------------------------
         DWNODA = DWNODA + NODGR
         WTNFX = WTNFX + NFIXN + (NODGR * 0.16d0 * PRONOD)
         WTNNAG = DWNODA * 0.16d0 * PRONOD
!***********************************************************************
!     NEED TO DISTRIBUTE NDTH, WTNOO, NNOFF, WTNNO, WNDOT, DWNOD BASED ON RDF
!     ALSO NEED TO ADJUST PLANT N CONCENTRATION IF NFIXN IS REDUCED DUE TO STRESS
!***********************************************************************
C      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE NFIX
!=======================================================================
! ACSTF    Weighted average soil temperature effect on N fixation (cm)
! ACSTG    Weighted average soil temperature effect on nodule growth (cm)
! AGRNOD   CH2O requirement for nodule growth (g[CH2O] / g[nodule])
! CLEFT    C left to grow new nodule mass (g[CH2O] / m2 / d)
! CNFACT   Ratio of C needed from nodules for vegetative and reproductive 
!            growth to C needed for nodule respiration (stress factor) 
! CNOD     C used in N-Fixation and nodule growth (including respiration 
!            costs) today (g[CH2O] / m2 / d)
! CNODCR   C requirement for nodule respiration (g[C] / g[nodule] / d)
! CNODGR   C used in nodule growth today (g[CH2O] / m2 / d)
! CNODMN   Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
! CNOFIX   C used for N fixation (g[CH2O] / m2 / d)
! CTONOD   C to allocate to nodules to fix N needed for reproductive and 
!            vegetative growth (g[CH2O] / m2 / d)
! CURV     Function subroutine 
! CUSFIX   C used in N-Fixation today (g[CH2O] / m2 / d)
! DAS      Days after start of simulation (days)
! DLAYR(L) Soil thickness in layer L (cm)
! DNOD     Depth of nodule zone (cm)
! DSW      Accumulated soil depth (cm)
! DWNOD    Current nodule mass (g[nodule] / m2)
! DWNODA   Cumulative nodule growth (g[nodule] / m2)
! DWNODI   Initial nodule mass per plant (g[nodule] / plant)
! DXR57    Relative time between first seed (NR5) and physiological 
!            maturity (NR7) 
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or FINAL 
! EFINOC   Inoculation effectiveness (or rhizobium density factor) 
! EFNFIX   Strain efficiency 
! EPORS    Soil water content above saturation (cm3/cm3)
! ERR      Error code for file operation 
! ERRKEY   Subroutine name for error file 
! FILECC   Path plus filename for species file (*.spe) 
! FILEIO   Filename for INP file (e.g., IBSNAT35.INP) 
! FLAYR    Fraction of layer within nodule zone 
! FLDACT   Soil water flooding effect on N2 fixation and nodule growth 
!            (0-1) 
! FLDSUM   Weighted average flooding conditions effect on N fixation and 
!            nodule growth and nodule death rate (cm)
! FNFXA(I) Critical points in development from first seed to physiological 
!            maturity for reducing N fixation due to canopy physiological age 
! FNFXD(I) Effects of soil water stress factor (TURFAC) on N fixation 
! FNFXT(I) Critical temperature points for function to reduce N fixation 
!            rate when temperature is not optimal (°C)
! FNFXW(I) Critical soil volumetric water contents for reducing N fixation 
!            when the soil is too wet (flooded) (cm3/cm3)
! FNNGT(I) Critical temperature points for function to reduce Nodule growth 
!            rates when temperature is not optimal (°C)
! FRCNM    C required for reproductive and vegetative growth per nodule 
!            mass (g[CH2O] / g[nodule] / d)
! LUNCRP   Logical unit number for FILEC (*.spe file) 
! LUNIO    Logical unit number for FILEIO 
! NDTH     Nodule death rate (g[nodule] / m2 / d)
! NDTHMX   Maximum relative death rate of nodules under flooded or dry 
!            conditions (g[nodule] / g[nodule] / d)
! NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
! NFXAGE   Average nodule age effect on nodule growth (0-1) 
! NL       maximum number of soil layers = 20 
! NLAYR    Number of soil layers 
! NODGR    New nodule growth (g[nod] / m2 / d)
! NODRGM   Maximum nodule relative growth rate (g[nodule] / g[nodule] / d)
! NODRGR   Effective nodule relative growth rate
!            (g[nodule] / g[nodule] / d)
! NR7      Day when 50% of plants first have yellowing or maturing pods
!            (days)
! PCSFIX   Potential C used in N-Fixation today (g[CH2O] / m2 / d)
! PLTPOP   Plant population (# plants / m2)
! PNFIXN   Potential amount of N fixation today (g[N] / m2 / d)
! PRONOD   Protein composition in nodules (g[protein] / g[nodule])
! RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
! RNDTH    Nodule death rate (g[nodule] / g[nodule] / d)
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SDWNOD   Denotes first entrance to NFIX subroutine 
! SNACT    Specific nodule activity (g [nodule])
! SNACTM   Maximum activity of the nodules (g [nodule])
! ST(L)    Soil temperature in soil layer L (°C)
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! SWFACT   Soil water deficit effect on N2 fixation and nodule growth (0-1)
!            
! SWMEM(I) Values of last 8 days of soil water deficit factor (SWFACT) 
! SWMEM8   Average of last 8 days of soil water deficit factor 
! TNFIX    Soil temperature effect on N2 fixation (0-1) 
! TNGRO    Soil temperature effect on nodule growth (0-1) 
! TURFAC   Water stress factor for expansion (0 - 1) 
! TYPFXA   Type of function for canopy age effects on N fixation 
! TYPFXD   Type of function for dry soil effects on N fixation 
! TYPFXT   Type of function for temperature effects on N fixation rates 
! TYPFXW   Type of function for wet soil effects on N fixation 
! TYPNGT   Type of function for temperature effects on Nodule growth rates 
! WTNFX    Cumulative weight of N fixed (g[N] / m2)
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!       END SUBROUTINE NFIX
!=======================================================================

