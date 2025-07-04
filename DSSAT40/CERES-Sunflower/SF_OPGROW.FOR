C======================================================================
C  SF_OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C----------------------------------------------------------------------
C  Generates output file for daily growth variables
C     This routine is used for maize, sorghum and millet.
C----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written
C  09/21/1998 CHP Split off from OPDAY.FOR file
C  05/11/1999 GH  Incorporated in CROPGRO
C  12/18/2001 WDB Revised for modular CERES
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
C----------------------------------------------------------------------
C  Called by: MAIZE, SG_CERES, ML_CERES
C  Calls:     None
!======================================================================
      SUBROUTINE SF_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

!----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE
!----------------------------------------------------------------------
      INTEGER NOUTDG
      INTEGER DYNAMIC

      CHARACTER*1  RNMODE
      CHARACTER*12 OUTG

      INTEGER TIMDIF
      INTEGER DAP, DAS, DOY, I, RSTAGE, RUN
      INTEGER MDATE, YEAR, YRDOY, YRPLT, YRSIM

      REAL VSTAGE, XLAI, STMWTO, SDWT, WTLF, TOPWT, RTWT, PODWT, SEEDNO
      REAL SLA, PCNL, TURFAC, CANHT, CANWH, HI, SHELPC, SATFAC
      REAL SDSIZE, PODNO, RTDEP, NSTRES, SWFAC, HIP
      REAL PLTPOP, PODWTD
      REAL WTLO, WTSO, WTCO
      REAL RLV(NL)
      REAL CUMSENSURF, CUMSENSOIL     !cumul. senes. soil and surface

      CHARACTER*1     IDETG
      CHARACTER*30    FILEIO
      CHARACTER*6, PARAMETER :: ERRKEY = 'SF_OPG'
      INTEGER         ERRNUM, FROP, NLAYR, L
      LOGICAL         FEXIST, FIRST

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) SENESCE

      IDETG   = ISWITCH % IDETG
      IF (IDETG .EQ. 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

C-----------------------------------------------------------------------
C                                 DYNAMIC = RUNINIT
C-----------------------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT) THEN
          !-------------------------------------------------------------
          !  Determine name of Plant Output File
          !-------------------------------------------------------------
          OUTG  = 'PlantGro.OUT'
          CALL GETLUN('OUTG',  NOUTDG)

!**********************************************************************
!     Seasonal initialization - run once per season
!**********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
          !-------------------------------------------------------------
          !   Open PlantGro.OUT as new or append
          !-------------------------------------------------------------
          INQUIRE (FILE = OUTG, EXIST = FEXIST)
          IF (FEXIST) THEN
            OPEN (UNIT=NOUTDG, FILE=OUTG, STATUS='OLD',
     &        IOSTAT=ERRNUM, POSITION='APPEND')
            FIRST = .FALSE.  
          ELSE
            OPEN (UNIT=NOUTDG, FILE=OUTG, STATUS='NEW',
     &        IOSTAT = ERRNUM)
              WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
            FIRST = .TRUE.  
          ENDIF

          !---------------------------------------------------------
          ! Generate variable heading for GROWTH.OUT
          !---------------------------------------------------------
          CALL HEADER(SEASINIT, FILEIO, NOUTDG, RUN)

          WRITE (NOUTDG,2201)
 2201     FORMAT('@YEAR DOY   DAS   DAP',
     &   '   L#SD   GSTD   LAID   LWAD   SWAD   GWAD',
     &   '   RWAD   CWAD   G#AD   GWGD   HIAD   PWAD',
     &   '   P#AD   WSPD   WSGD   NSTD   EWSD   LN%D',
     &   '   SH%D   HIPD   PWDD   PWTD     SLAD   CHTD',
     &   '   CWID   RDPD   RL1D   RL2D   RL3D', 
     &   '   RL4D   RL5D   RL6D   RL7D   RL8D   RL9D',
     &   '   RL10   CDAD   LDAD   SDAD   SNW0C   SNW1C') 

        CUMSENSURF = 0.0
        CUMSENSOIL = 0.0

!-----------------------------------------------------------------------
!                         DYNAMIC = OUTPUT
!-----------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
        IF (YRDOY .GE. YRPLT) THEN
          PODWTD = 0
          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))
          DAS = MAX(0, TIMDIF(YRSIM, YRDOY))

!         Calculate cumulative senesence
          CUMSENSURF = CUMSENSURF + SENESCE % ResWt(0) 
          DO L = 1, NLAYR
            CUMSENSOIL = CUMSENSOIL + SENESCE % ResWt(L)
          ENDDO

          !-------------------------------------------------------------
          !  Write output based on user specified frequency 
          !-------------------------------------------------------------
          IF ((MOD(DAS,FROP) .EQ. 0)    !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

            CALL YR_DOY(YRDOY, YEAR, DOY)
            
            WRITE(NOUTDG,400)YEAR, DOY, DAS, DAP,VSTAGE,RSTAGE,XLAI,
     &        NINT(WTLF*10.),NINT(STMWTO*10.),NINT(SDWT*10.),
     &        NINT(RTWT*10.*PLTPOP),NINT(TOPWT*10.),NINT(SEEDNO),SDSIZE,
     &        HI,NINT(PODWT*10.),NINT(PODNO),(1.-SWFAC),(1.-TURFAC),
     &        (1.-NSTRES),SATFAC,PCNL,SHELPC,HIP,NINT(PODWTD*10.),
     &        NINT((PODWTD+PODWT)*10.),SLA,CANHT,CANWH,
     &        (RTDEP/100.),(RLV(I),I=1,10),
     &        NINT(WTCO*10.),NINT(WTLO*10.),NINT(WTSO*10.),
     &         NINT(CUMSENSURF), NINT(CUMSENSOIL)

 400          FORMAT (1X,I4,1X,I3.3,2(1X,I5),1X,F6.1,1X,I6,1X,F6.2,
     &        6(1X,I6),1X,F6.1,1X,F6.3,2(1X,I6),4(1X,F6.3),3(1X,F6.2),
     &        2(1X,I6),1X,F8.1,2(1X,F6.2),11(1X,F6.2),
     &        3(1X,I6), 2I8)
          ENDIF
        ENDIF 


!-----------------------------------------------------------------------
!                 DYNAMIC = FINAL
!-----------------------------------------------------------------------
C     Simulation Summary File
C-------------------------------------------------------------------
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
        !Close daily output files.
        CLOSE (NOUTDG)

      ENDIF

!***********************************************************************
      RETURN
      END SUBROUTINE SF_OPGROW
!=======================================================================


!=======================================================================
!       Variable definitions for OPGROW
!-----------------------------------------------------------------------
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CROP    Crop identification code 
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR,
!           OUTPUT, or FINAL 
! ENAME   Experiment description 
! EXPER   Experiment code (prefix of input files) 
! HI      Ratio of seed weight (SDWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[seed] / g[tops])
! HIP     Ratio of pod weight (PODWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[pods] / g[tops])
! MODEL   Name of CROPGRO executable file 
! NL      maximum number of soil layers = 20 
! NOUTDG  Unit number for growth output file 
! RUN    Report number for sequenced or multi-season runs 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! OUTG    Growth output file name (typically 'GROWTH.OUT') 
! PCNL    Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PODNO   Total number of pods (#/m2)
! PODWT   Dry mass of seeds plus shells, including C and N
!           (g[pods] / m2[ground])
! PODWTD  Mass of detached pods (g[pods] / m2[ground])
! RLV(L)  Root length density for soil layer L ((cm root / cm3 soil))
! RSTAGE  Number of RSTAGES which have occurred. 
! RTDEP   Root depth (cm)
! RTWT    Dry mass of root tissue, including C and N (g[root] / plant)
! SATFAC  Root length weighted soil water excess stress factor ( 0 = no 
!           stress; 1 = saturated stress ) 
! SDSIZE  Average mass of seeds (mg / seed)
! SDWT    Dry mass of seed tissue, including C and N (g[seed] / m2[ground])
! SEEDNO  Total number of seeds (#/m2)
! SHELLW  Shell mass (g[shell] / m2)
! SHELPC  Percentage of pod mass that is seeds (g[seed]/g[pods] * 100%)
! SLA     Specific leaf area (cm2[leaf] / m2[ground])
! STMWTO   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SWFAC   Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!           0.0=max stress 
! TITLET  Description of treatment for this simulation 
! TOPWT   Total weight of above-ground portion of crop, including pods
!           (g[tissue] / m2)
! TRTNO   Treatment number 
! TURFAC  Water stress factor for expansion (0 - 1) 
! VSTAGE  Number of nodes on main stem of plant 
! WTCO    Cumulative losses of plant tissue (g[tissue] / m2)
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! WTLO    Cumulative leaf losses (g[leaf] / m2)
! WTSO    Cumulative stem losses (g[stem] / m2)
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! YRDOY   Current day of simulation (YYDDD)
! YRPLT   Planting date (YYDDD)
!***********************************************************************
!       END SUBROUTINE SF_OPGROW
!=======================================================================

