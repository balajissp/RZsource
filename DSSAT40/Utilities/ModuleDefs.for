!=======================================================================
C  MODULE ModuleDefs
C  11/01/2001 CHP Written
C  06/15/2002 CHP Added flood-related data constructs 
C  03/12/2003 CHP Added residue data construct
C  05/08/2003 CHP Added version information
C  09/03/2004 CHP Modified GetPut_Control routine to store entire
C                   CONTROL variable. 
C             CHP Added GETPUT_ISWITCH routine to store ISWITCH.
C             CHP Added TRTNO to CONTROL variable.
!  06/14/2005 CHP Added FILEX to CONTROL variable.
!  10/24/2005 CHP Put weather variables in constructed variable. 
!             CHP Added PlantStres environmental stress variable
!=======================================================================

      MODULE ModuleDefs
!     Contains defintion of derived data types and constants which are 
!     used throughout the model.
      SAVE
!=======================================================================
!     Global CSM Version Number
      TYPE VersionType
        INTEGER :: Major = 4
        INTEGER :: Minor = 0  
        INTEGER :: Sub   = 2
        INTEGER :: Build = 0  
      END TYPE VersionType
      TYPE (VersionType) Version

!     Version history:
!       4.0.2.0 chp 08/11/2005 Release
!       4.0.2.1 chp 08/02/2005 Pre-release
!       4.0.2.0 chp 01/14/2005 
!       4.0.1.1 chp 12/16/2004 Minor updates, Hawaii
!       4.0.1.0 chp 01/28/2004 Release Version 
!       4.0.1.1 chp 01/07/2004
!       4.0.1.0 chp 10/14/2003
!       4.0.6   chp 08/29/2003
!       4.0.5   chp 08/12/2003
!       4.0.4   chp 07/22/2003
!       4.0.3   chp 06/27/2003
!       4.0.2   chp 06/04/2003
!       4.0.1   chp 05/08/2003

!=======================================================================

!     Global constants
      INTEGER, PARAMETER :: 
     &    NL       = 20,  !Maximum number of soil layers 
     &    TS       = 24,  !Number of hourly time steps per day
     &    NAPPL    = 250, !Maximum number of applications or operations
     &    NCOHORTS = 300, !Maximum number of cohorts
     &    NELEM    = 1,   !Number of elements modeled (currently only N)

         !Dynamic variable values
     &    RUNINIT  = 1, 
     &    INIT     = 2,  !Will take the place of RUNINIT & SEASINIT
                         !     (not fully implemented)
     &    SEASINIT = 2, 
     &    RATE     = 3,
     &    EMERG    = 3,  !Used for some plant processes.  
     &    INTEGR   = 4,  
     &    OUTPUT   = 5,  
     &    FINAL    = 6

!=======================================================================

!     Data construct for control variables
      TYPE ControlType
        CHARACTER (len=1)  MESIC, RNMODE
        CHARACTER (len=2)  CROP
        CHARACTER (len=12)  MODEL
        CHARACTER (len=12) FILEX
        CHARACTER (len=30) FILEIO
        INTEGER      DAS, DYNAMIC, FROP, LUNIO, MULTI, REPNO, RUN
        INTEGER      TRTNO, YRDOY, YRSIM, NYRS, YRDIF, ROTNUM
!  MODIFIED BY LIWANG MA, RZWQM-DSSAT
        CHARACTER (len=12) FILEC, FILEE, FILES,FILEA,FILET
        CHARACTER (len=255) PATHCR,PATHER,PATHSR
        CHARACTER (LEN=8) EXPER
        CHARACTER (LEN=60) ENAME
        CHARACTER (LEN=80) TITLET
        INTEGER YRPLT, IEMRG
!  END OF MODIFICATION
      END TYPE ControlType

!=======================================================================
!     Data construct for control switches
      TYPE SwitchType
        CHARACTER (len=1) IDETC, IDETD, IDETG, IDETL, IDETN, IDETO
        CHARACTER (len=1) IDETR, IDETS, IDETW
        CHARACTER (len=1) IHARI, IPLTI, IIRRI
        CHARACTER (len=1) ISWCHE, ISWDIS, ISWNIT, ISWSYM, ISWTIL, ISWWAT
        CHARACTER (len=1) MEEVP, MEPHO, MESOM
        CHARACTER (len=1) IFERI, IRESI, FMOPT
        INTEGER NSWI
      END TYPE SwitchType

!Other switches and methods used by model:
! ISWPHO, ISWPOT, MELI, MEINF, MEHYD, NSWI, IOX, IDETP, IDETR- not used
! IDETH - only used in MgmtOps
! MEWTH - only used in WEATHR

!=======================================================================
!     Data construct for weather variables
      TYPE WeatherType
!       Weather station information
        REAL REFHT, WINDHT, XLAT

!       Daily weather data.
        REAL CLOUDS, CO2, DAYL, PAR, RAIN, SNDN, SNUP, SRAD, 
     &    TAMP, TA, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,      
     &    TMAX, TMIN, TWILEN, WINDSP
            
!       Hourly weather data
        REAL
     &    AZZON(TS), BETA(TS), FRDIFP(TS), FRDIFR(TS),
     &    PARHR(TS), RADHR(TS), RHUMHR(TS),
     &    TAIRHR(TS), TGRO(TS), WINDHR(TS)
      END TYPE WeatherType

!=======================================================================
!     Data construct for soil variables
      TYPE SoilType
        INTEGER NLAYR
        REAL CN, DMOD, SALB, SLPF, SWCON, U, SLNF
        REAL, DIMENSION(NL) :: ADCOEF, BD, CEC, CLAY, DLAYR, DS
        REAL, DIMENSION(NL) :: DUL, LL, OC, PH, PHKCL, SAND
        REAL, DIMENSION(NL) :: SAT, SILT, STONES, SWCN, TOTN, WR
      !These variables could be made available if needed elsewhere.
      !  They are currently read by SOILDYN module.
      !  CHARACTER*5 SLTXS
      !  CHARACTER*11 SLSOUR
      !  CHARACTER*50 SLDESC, TAXON
      END TYPE SoilType

!=======================================================================
!   Data construct for residue (harvest residue, senesced matter, etc.)
      TYPE ResidueType
        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha (E=N, P, S,...)
      END TYPE ResidueType

!=======================================================================
!     Data construct for oxidation layer
      TYPE OxLayerType
        INTEGER IBP
        REAL    OXU, OXH4, OXN3   
        REAL    OXLT, OXMIN4, OXMIN3
        REAL    DLTOXU, DLTOXH4, DLTOXN3
        REAL    ALGACT
        LOGICAL DAILY, UNINCO
      END TYPE OxLayerType

!======================================================================
!     Plant stresses for environmental stress summary
      Type PlStresType
        INTEGER NSTAGES   !# of stages (max 5)
        CHARACTER(len=23) StageName(5)
        REAL TURFAC, SWFAC, NSTRES, AGEFAC
        LOGICAL ACTIVE(5)
      End Type plstresType
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
!=======================================================================

!     Data construct for PLANT variables
      TYPE PLANTVARType
      CHARACTER (LEN=6)  VARNO,ECONO          
      CHARACTER (LEN=16) VRNAME
      CHARACTER (LEN=1) PLME
      INTEGER HDATE(3)
      REAL HPC, HBPC, HRPC                  
!        REAL  P1,P2,P5,G2,G3,PHINT
	REAL          PLTPOP,ROWSPC,SDEPTH,AZIR,SDWTPL,SDAGE,SPRLAP
      REAL          P1,P1V,P1D,P2,P2O,P2R,P3,P4,P5,G1,G2,G3,G4
      REAL          PHINT,PD,TC,O1,GPROT,LT50H
      REAL          CSDVAR,PHTHRS(20),SDPDVR,SLAVAR,LFMAX,XFRUIT,WTPSD
      REAL          SFDUR,PODUR,PPSEN,PH2T5,SIZELF
      REAL          RATPOT,PI1,PI2,DTTPI
      REAL          PCINT,PCGRD
      REAL          GCOEFF(15)
      REAL          SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ
      REAL          EFINOC, EFNFIX
	REAL          RESAMT,ATEMP  !BE SURE UNIT BETWEEN ICRES, RESAMT, RM IN RZWQM
      END TYPE PLANTVARType

!=======================================================================
!======================================================================
      CONTAINS

      SUBROUTINE GETPUT_CONTROL(CODE, CONTROL_ARG)
!     Transfers CONTROL variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (ControlType) CONTROL_ARG, CONTROL_SAVE

      SELECT CASE(CODE)
        CASE('GET','get')
          CONTROL_ARG = CONTROL_SAVE
        CASE('PUT','put')
          CONTROL_SAVE = CONTROL_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_CONTROL

!======================================================================

      SUBROUTINE GETPUT_ISWITCH(CODE, ISWITCH_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (SwitchType) ISWITCH_ARG, ISWITCH_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          ISWITCH_ARG = ISWITCH_SAVE
        CASE('PUT')
          ISWITCH_SAVE = ISWITCH_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_ISWITCH

!======================================================================
!======================================================================
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      SUBROUTINE GETPUT_PLANTVAR(CODE, PLANTVAR_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (PLANTVARType) PLANTVAR_ARG, PLANTVAR_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          PLANTVAR_ARG = PLANTVAR_SAVE
        CASE('PUT')
          PLANTVAR_SAVE = PLANTVAR_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_PLANTVAR
!======================================================================
      SUBROUTINE GETPUT_PlantStres(CODE, PlantStres_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (PlStresType) Plantstres_ARG, PlantStres_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          PlantStres_ARG = PlantStres_SAVE
        CASE('PUT')
          PlantStres_SAVE = PlantStres_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_PlantStres

!======================================================================
      SUBROUTINE GETPUT_SOILPROP(CODE, SOILPROP_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (SOILType) SOILPROP_ARG, SOILPROP_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          SOILPROP_ARG = SOILPROP_SAVE
        CASE('PUT')
          SOILPROP_SAVE = SOILPROP_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_SOILPROP

!======================================================================
      SUBROUTINE GETPUT_HARVRES(CODE, HARVRES_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (ResidueType) HARVRES_ARG, HARVRES_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          HARVRES_ARG = HARVRES_SAVE
        CASE('PUT')
          HARVRES_SAVE = HARVRES_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_HARVRES

!======================================================================
      SUBROUTINE GETPUT_WEATHER(CODE, WEATHER_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (WEATHERType) WEATHER_ARG, WEATHER_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          WEATHER_ARG = WEATHER_SAVE
        CASE('PUT')
          WEATHER_SAVE = WEATHER_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_WEATHER
!======================================================================

      END MODULE ModuleDefs


!======================================================================
!     Paddy Managment routines.
!======================================================================
      MODULE FloodModule
!=======================================================================
!     Data construct for flood data. 
      Type FloodWatType
        !From IRRIG
        LOGICAL BUNDED        
        INTEGER NBUND         
        REAL ABUND            
        REAL PUDPERC, PERC    

        !From Paddy_Mgmt
        INTEGER YRDRY, YRWET  
        REAL FLOOD, FRUNOFF   
        REAL TOTBUNDRO        
        LOGICAL PUDDLED       

        REAL CEF, EF          !From SPAM
        REAL INFILT, RUNOFF   !From WATBAL
      End Type FloodWatType

      Type FloodNType
        INTEGER NDAT
        REAL    FLDH4C, FLDN3C                !Flood N concentrations
        REAL    FLDU, FLDN3, FLDH4            !Flood N mass (kg/ha)
        REAL    FRNH4U, FRNO3U                !Flood N uptake (kg/ha)
        REAL    DLTFUREA, DLTFNH4, DLTFNO3    !Flood N flux (kg/ha)
      End Type FloodNType

      END MODULE FloodModule
!======================================================================
!=======================================================================
!  MODULE ModuleData
!  01/22/2008 CHP Written
!=======================================================================

      MODULE ModuleData
!     Data storage and retrieval module.
!     Defines data structures that hold information that can be 
!       stored or accessed by query.  

!     A call to the GET routine will return the value of variable 
!       requested.  The procedure is "overloaded", i.e., the procedure 
!       executed will depend on the type of variable(s) in the argument 
!       list.  A request for a "real" data type will invoke the GET_Real
!       procedure, for example.  

!     Similarly, a call to the PUT routine will store the data sent.
!       It is also an overloaded procedure including several different
!       types of data which can be stored.

!     The SAVE_data variable is used to store all information.

!     To add a variable for storage and retrieval: 
!     1.  Add the variable to one of the Type constructs based on the 
!         module that "owns" the variable, for example SPAMType, Planttype 
!         or MgmtType.
!     2.  For a real data type, add a line of code in both the GET_Real and
!         PUT_Real subroutines.  
!     3.  For an integer data type, add a line of code in both the 
!         GET_Integer and PUT_Integer subroutines.  
!     4.  All routines accessing GET or PUT procedures must include a 
!         "USE ModuleData" statement.
!     5.  A call to the PUT routine must be used to store data prior to
!         a call to the GET routine to retrive the data.

      USE ModuleDefs
      SAVE

!======================================================================
!     Data transferred from hourly energy balance 
      Type SPAMType
        REAL AGEFAC, PG                   !photosynthese
        REAL CEF, CEM, CEO, CEP, CES, CET !Cumulative ET - mm
        REAL  EF,  EM,  EO,  EP,  ES,  ET !Daily ET - mm/d
        REAL  EOP, EVAP                   !Daily mm/d
        REAL, DIMENSION(NL) :: UH2O       !Root water uptake
        !ASCE reference ET with FAO-56 dual crop coefficient (KRT)
        REAL REFET, SKC, KCBMIN, KCBMAX, KCB, KE, KC
      End Type SPAMType


!     Data transferred from management routine 
      Type MgmtType
        REAL DEPIR, EFFIRR, FERNIT, IRRAMT, TOTIR, TOTEFFIRR
        REAL V_AVWAT(20)    ! Create vectors to save growth stage based irrigation
        REAL V_IMDEP(20)
        REAL V_ITHRL(20)
        REAL V_ITHRU(20)
        INTEGER V_IRON(20)
        REAL V_IRAMT(20)
        REAL V_IREFF(20)
        INTEGER V_IFREQ(20)
        INTEGER GSIRRIG
        CHARACTER*5 V_IRONC(20)
      End Type MgmtType

!     Data transferred from Soil water routine
      Type WatType
        REAL DRAIN, RUNOFF, SNOW
      End Type WatType

!     Data transferred from Soil Inorganic Nitrogen routine
      Type NiType
        REAL TNOXD, TLeachD    !, TN2OD     ! added N2O PG
      End Type NiType

!     Data transferred from Organic C routines
      Type OrgCType
        REAL TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2
        REAL TOMINSOM3, TNIMBSOM
        REAL MULCHMASS
      End Type OrgCType

!     Data from weather
      Type WeathType
        Character*8 WSTAT
      End Type WeathType

      TYPE PDLABETATYPE
        REAL PDLA
        REAL BETALS
      END TYPE

!     Data which can be transferred between modules
      Type TransferType
        Type (ControlType) CONTROL
        Type (SwitchType)  ISWITCH
C        Type (OutputType)  OUTPUT
C        Type (PlantType)   PLANT
        Type (MgmtType)    MGMT
        Type (NiType)      NITR
        Type (OrgCType)    ORGC
        Type (SoilType)    SOILPROP
        Type (SPAMType)    SPAM
        Type (WatType)     WATER
        Type (WeathType)   WEATHER
        TYPE (PDLABETATYPE) PDLABETA
      End Type TransferType

!     The variable SAVE_data contains all of the components to be 
!     stored and retrieved.
      Type (TransferType) SAVE_data

!======================================================================
!     GET and PUT routines are differentiated by argument type.  All of 
!       these procedures can be accessed with a CALL GET(...)
      INTERFACE GET
         MODULE PROCEDURE GET_Control
     &                  , GET_ISWITCH 
C     &                  , GET_Output 
     &                  , GET_SOILPROP
!     &                  , GET_Weather
     &                  , GET_Real 
     &                  , GET_Real_Array_NL
     &                  , GET_Integer
     &                  , GET_Char
      END INTERFACE

      INTERFACE PUT
         MODULE PROCEDURE PUT_Control
     &                  , PUT_ISWITCH 
C     &                  , PUT_Output 
     &                  , PUT_SOILPROP
!     &                  , PUT_Weather
     &                  , PUT_Real 
     &                  , PUT_Real_Array_NL
     &                  , PUT_Integer
     &                  , PUT_Char
      END INTERFACE

      CONTAINS

!----------------------------------------------------------------------
      Subroutine Get_CONTROL (CONTROL_arg)
!     Retrieves CONTROL variable
      IMPLICIT NONE
      Type (ControlType) Control_arg
      Control_arg = SAVE_data % Control
      Return
      End Subroutine Get_CONTROL

!----------------------------------------------------------------------
      Subroutine Put_CONTROL (CONTROL_arg)
!     Stores CONTROL variable
      IMPLICIT NONE
      Type (ControlType) Control_arg
      SAVE_data % Control = Control_arg
      Return
      End Subroutine Put_CONTROL

!----------------------------------------------------------------------
      Subroutine Get_ISWITCH (ISWITCH_arg)
!     Retrieves ISWITCH variable
      IMPLICIT NONE
      Type (SwitchType) ISWITCH_arg
      ISWITCH_arg = SAVE_data % ISWITCH
      Return
      End Subroutine Get_ISWITCH

!----------------------------------------------------------------------
      Subroutine Put_ISWITCH (ISWITCH_arg)
!     Stores ISWITCH variable
      IMPLICIT NONE
      Type (SwitchType) ISWITCH_arg
      SAVE_data % ISWITCH = ISWITCH_arg
      Return
      End Subroutine Put_ISWITCH

!----------------------------------------------------------------------
C      SUBROUTINE GET_OUTPUT(OUTPUT_ARG)
!     Retrieves OUTPUT variable as needed
C      IMPLICIT NONE
C      TYPE (OutputType) OUTPUT_ARG
C      OUTPUT_ARG = SAVE_data % OUTPUT
C      RETURN
C      END SUBROUTINE GET_OUTPUT

!----------------------------------------------------------------------
C      SUBROUTINE PUT_OUTPUT(OUTPUT_ARG)
!     Stores OUTPUT variable 
C      IMPLICIT NONE
C      TYPE (OutputType) OUTPUT_ARG
C      SAVE_data % OUTPUT = OUTPUT_ARG
C      RETURN
C      END SUBROUTINE PUT_OUTPUT

!----------------------------------------------------------------------
      SUBROUTINE GET_SOILPROP(SOIL_ARG)
!     Retrieves SOILPROP variable as needed
      IMPLICIT NONE
      TYPE (SoilType) SOIL_ARG
      SOIL_ARG = SAVE_data % SOILPROP
      RETURN
      END SUBROUTINE GET_SOILPROP

!----------------------------------------------------------------------
      SUBROUTINE PUT_SOILPROP(SOIL_ARG)
!     Stores SOILPROP variable 
      IMPLICIT NONE
      TYPE (SoilType) SOIL_ARG
      SAVE_data % SOILPROP = SOIL_ARG
      RETURN
      END SUBROUTINE PUT_SOILPROP

!!----------------------------------------------------------------------
!      SUBROUTINE GET_WEATHER(WEATHER_ARG)
!!     Retrieves WEATHER variable as needed
!      IMPLICIT NONE
!      TYPE (WeathType) WEATHER_ARG
!      WEATHER_ARG = SAVE_data % WEATHER
!      RETURN
!      END SUBROUTINE GET_WEATHER
!
!!----------------------------------------------------------------------
!      SUBROUTINE PUT_WEATHER(WEATHER_ARG)
!!     Stores WEATHER variable 
!      IMPLICIT NONE
!      TYPE (WeathType) WEATHER_ARG
!      SAVE_data % WEATHER = WEATHER_ARG
!      RETURN
!      END SUBROUTINE PUT_WEATHER

!----------------------------------------------------------------------
      Subroutine GET_Real(ModuleName, VarName, Value)
!     Retrieves real variable from SAVE_data.  Variable must be
!         included as a component of SAVE_data. 
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Real Value
      Logical ERR

      Value = 0.0
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('AGEFAC'); Value = SAVE_data % SPAM % AGEFAC
        Case ('PG');     Value = SAVE_data % SPAM % PG
        Case ('CEF');    Value = SAVE_data % SPAM % CEF
        Case ('CEM');    Value = SAVE_data % SPAM % CEM
        Case ('CEO');    Value = SAVE_data % SPAM % CEO
        Case ('CEP');    Value = SAVE_data % SPAM % CEP
        Case ('CES');    Value = SAVE_data % SPAM % CES
        Case ('CET');    Value = SAVE_data % SPAM % CET
        Case ('EF');     Value = SAVE_data % SPAM % EF
        Case ('EM');     Value = SAVE_data % SPAM % EM
        Case ('EO');     Value = SAVE_data % SPAM % EO
        Case ('EP');     Value = SAVE_data % SPAM % EP
        Case ('ES');     Value = SAVE_data % SPAM % ES
        Case ('ET');     Value = SAVE_data % SPAM % ET
        Case ('EOP');    Value = SAVE_data % SPAM % EOP
        Case ('EVAP');   Value = SAVE_data % SPAM % EVAP
        Case ('REFET');  Value = SAVE_data % SPAM % REFET
        Case ('SKC');    Value = SAVE_data % SPAM % SKC
        Case ('KCBMIN'); Value = SAVE_data % SPAM % KCBMIN
        Case ('KCBMAX'); Value = SAVE_data % SPAM % KCBMAX
        Case ('KCB');    Value = SAVE_data % SPAM % KCB
        Case ('KE');     Value = SAVE_data % SPAM % KE
        Case ('KC');     Value = SAVE_data % SPAM % KC
        Case DEFAULT; ERR = .TRUE.
        END SELECT

C       Case ('PLANT')
C         SELECT CASE (VarName)
C         Case ('BIOMAS'); Value = SAVE_data % PLANT % BIOMAS
C         Case ('CANHT') ; Value = SAVE_data % PLANT % CANHT
C         Case ('CANWH') ; Value = SAVE_data % PLANT % CANWH
C         Case ('DXR57') ; Value = SAVE_data % PLANT % DXR57
C         Case ('EXCESS'); Value = SAVE_data % PLANT % EXCESS
C         Case ('PLTPOP'); Value = SAVE_data % PLANT % PLTPOP
C         Case ('RNITP') ; Value = SAVE_data % PLANT % RNITP
C         Case ('SLAAD') ; Value = SAVE_data % PLANT % SLAAD
C         Case ('XPOD')  ; Value = SAVE_data % PLANT % XPOD
C         Case DEFAULT; ERR = .TRUE.
C         END SELECT

      Case ('MGMT')
        SELECT CASE (VarName)
        Case ('EFFIRR'); Value = SAVE_data % MGMT % EFFIRR
        Case ('TOTIR');  Value = SAVE_data % MGMT % TOTIR
        Case ('TOTEFFIRR');Value=SAVE_data % MGMT % TOTEFFIRR
        Case ('DEPIR');  Value = SAVE_data % MGMT % DEPIR
        Case ('IRRAMT'); Value = SAVE_data % MGMT % IRRAMT
        Case ('FERNIT'); Value = SAVE_data % MGMT % FERNIT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); Value = SAVE_data % WATER % DRAIN
        Case ('RUNOFF');Value = SAVE_data % WATER % RUNOFF
        Case ('SNOW');  Value = SAVE_data % WATER % SNOW
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('NITR')
        SELECT CASE (VarName)
        Case ('TNOXD'); Value = SAVE_data % NITR % TNOXD
       Case ('TLCHD'); Value = SAVE_data % NITR % TLeachD
!       Case ('TN2OD'); Value = SAVE_data % NITR % TN2OD
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('ORGC')
        SELECT CASE (VarName)
        Case ('MULCHMASS');Value = SAVE_data % ORGC % MULCHMASS
        Case ('TOMINFOM'); Value = SAVE_data % ORGC % TOMINFOM
        Case ('TOMINSOM'); Value = SAVE_data % ORGC % TOMINSOM
        Case ('TOMINSOM1');Value = SAVE_data % ORGC % TOMINSOM1
        Case ('TOMINSOM2');Value = SAVE_data % ORGC % TOMINSOM2
        Case ('TOMINSOM3');Value = SAVE_data % ORGC % TOMINSOM3
        Case ('TNIMBSOM'); Value = SAVE_data % ORGC % TNIMBSOM
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('SOIL')
        SELECT CASE (VarName)
        Case ('TOMINFOM'); Value = SAVE_data % ORGC % TOMINFOM
        Case ('TOMINSOM'); Value = SAVE_data % ORGC % TOMINSOM
        Case ('TOMINSOM1');Value = SAVE_data % ORGC % TOMINSOM1
        Case ('TOMINSOM2');Value = SAVE_data % ORGC % TOMINSOM2
        Case ('TOMINSOM3');Value = SAVE_data % ORGC % TOMINSOM3
        Case ('TNIMBSOM'); Value = SAVE_data % ORGC % TNIMBSOM
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      CASE ('PDLABETA')
        SELECT CASE(VarName)
        CASE('PDLA'); Value = SAVE_data % PDLABETA % PDLA
        CASE('BETA'); Value = SAVE_data % PDLABETA % BETALS
        CASE DEFAULT; ERR = .TRUE.
        END SELECT
            
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, " in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_REAL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Real

!----------------------------------------------------------------------
      SUBROUTINE PUT_Real(ModuleName, VarName, Value)
!     Stores real variable SAVE_data.  
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Real Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('AGEFAC'); SAVE_data % SPAM % AGEFAC = Value
        Case ('PG');     SAVE_data % SPAM % PG     = Value
        Case ('CEF');    SAVE_data % SPAM % CEF    = Value
        Case ('CEM');    SAVE_data % SPAM % CEM    = Value
        Case ('CEO');    SAVE_data % SPAM % CEO    = Value
        Case ('CEP');    SAVE_data % SPAM % CEP    = Value
        Case ('CES');    SAVE_data % SPAM % CES    = Value
        Case ('CET');    SAVE_data % SPAM % CET    = Value
        Case ('EF');     SAVE_data % SPAM % EF     = Value
        Case ('EM');     SAVE_data % SPAM % EM     = Value
        Case ('EO');     SAVE_data % SPAM % EO     = Value
        Case ('EP');     SAVE_data % SPAM % EP     = Value
        Case ('ES');     SAVE_data % SPAM % ES     = Value
        Case ('ET');     SAVE_data % SPAM % ET     = Value
        Case ('EOP');    SAVE_data % SPAM % EOP    = Value
        Case ('EVAP');   SAVE_data % SPAM % EVAP   = Value
        Case ('REFET');  SAVE_data % SPAM % REFET  = Value
        Case ('SKC');    SAVE_data % SPAM % SKC    = Value
        Case ('KCBMIN'); SAVE_data % SPAM % KCBMIN = Value
        Case ('KCBMAX'); SAVE_data % SPAM % KCBMAX = Value
        Case ('KCB');    SAVE_data % SPAM % KCB    = Value
        Case ('KE');     SAVE_data % SPAM % KE     = Value
        Case ('KC');     SAVE_data % SPAM % KC     = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

C       Case ('PLANT')
C         SELECT CASE (VarName)
C         Case ('BIOMAS'); SAVE_data % PLANT % BIOMAS = Value
C         Case ('CANHT');  SAVE_data % PLANT % CANHT  = Value
C         Case ('CANWH');  SAVE_data % PLANT % CANWH  = Value
C         Case ('DXR57');  SAVE_data % PLANT % DXR57  = Value
C         Case ('EXCESS'); SAVE_data % PLANT % EXCESS = Value
C         Case ('PLTPOP'); SAVE_data % PLANT % PLTPOP = Value
C         Case ('RNITP');  SAVE_data % PLANT % RNITP  = Value
C         Case ('SLAAD');  SAVE_data % PLANT % SLAAD  = Value
C         Case ('XPOD');   SAVE_data % PLANT % XPOD   = Value
C         Case DEFAULT; ERR = .TRUE.
C         END SELECT

      Case ('MGMT')
        SELECT CASE (VarName)
        Case ('EFFIRR'); SAVE_data % MGMT % EFFIRR = Value
        Case ('TOTIR');  SAVE_data % MGMT % TOTIR  = Value
        Case ('TOTEFFIRR');SAVE_data%MGMT % TOTEFFIRR=Value
        Case ('DEPIR');  SAVE_data % MGMT % DEPIR  = Value
        Case ('IRRAMT'); SAVE_data % MGMT % IRRAMT = Value
        Case ('FERNIT'); SAVE_data % MGMT % FERNIT = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); SAVE_data % WATER % DRAIN  = Value
        Case ('RUNOFF');SAVE_data % WATER % RUNOFF = Value
        Case ('SNOW');  SAVE_data % WATER % SNOW   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('NITR')
        SELECT CASE (VarName)
        Case ('TNOXD'); SAVE_data % NITR % TNOXD = Value
        Case ('TLCHD'); SAVE_data % NITR % TLeachD = Value
!       Case ('TN2OD'); SAVE_data % NITR % TN2OD = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('ORGC')
        SELECT CASE (VarName)
        Case ('MULCHMASS');SAVE_data % ORGC % MULCHMASS = Value
        Case ('TOMINFOM'); SAVE_data % ORGC % TOMINFOM  = Value
        Case ('TOMINSOM'); SAVE_data % ORGC % TOMINSOM  = Value
        Case ('TOMINSOM1');SAVE_data % ORGC % TOMINSOM1 = Value
        Case ('TOMINSOM2');SAVE_data % ORGC % TOMINSOM2 = Value
        Case ('TOMINSOM3');SAVE_data % ORGC % TOMINSOM3 = Value
        Case ('TNIMBSOM'); SAVE_data % ORGC % TNIMBSOM  = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      CASE ('PDLABETA')
        SELECT CASE(VarName)
        CASE('PDLA'); SAVE_data % PDLABETA % PDLA = Value
        CASE('BETA'); SAVE_data % PDLABETA % BETALS = Value
        CASE DEFAULT; ERR = .TRUE.
        END SELECT
            
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_REAL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Real

!----------------------------------------------------------------------
      SUBROUTINE GET_Real_Array_NL(ModuleName, VarName, Value)
!     Retrieves array of dimension(NL) 
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      REAL, DIMENSION(NL) :: Value
      Logical ERR

      Value = 0.0
      ERR = .FALSE.

      SELECT CASE (ModuleName)

      CASE ('SPAM')
        SELECT CASE (VarName)
          CASE ('UH2O'); ; Value = SAVE_data % SPAM % UH2O
          CASE DEFAULT; ERR = .TRUE.
        END SELECT

        CASE DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_Real_Array_NL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Real_Array_NL

!----------------------------------------------------------------------
      SUBROUTINE PUT_Real_Array_NL(ModuleName, VarName, Value)
!     Stores array of dimension NL
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      REAL, DIMENSION(NL) :: Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('UH2O'); SAVE_data % SPAM % UH2O = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Real_Array_NL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Real_Array_NL

!----------------------------------------------------------------------
      Subroutine GET_Integer(ModuleName, VarName, Value)
!     Retrieves Integer variable as needed
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78  MSG(2)
      Integer Value
      Logical ERR

      Value = 0
      ERR = .FALSE.

      SELECT CASE (ModuleName)
C       Case ('PLANT')
C         SELECT CASE (VarName)
C         Case ('NR5');  Value = SAVE_data % PLANT % NR5
C         Case ('iSTAGE');  Value = SAVE_data % PLANT % iSTAGE
C         Case ('iSTGDOY'); Value = SAVE_data % PLANT % iSTGDOY
C         Case DEFAULT; ERR = .TRUE.
C         END SELECT

      Case Default; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_INTEGER',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Integer

!----------------------------------------------------------------------
      SUBROUTINE PUT_Integer(ModuleName, VarName, Value)
!     Stores Integer variable
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Integer Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
C       Case ('PLANT')
C         SELECT CASE (VarName)
C         Case ('NR5');  SAVE_data % PLANT % NR5  = Value
C         Case ('iSTAGE');  SAVE_data % PLANT % iSTAGE  = Value
C         Case ('iSTGDOY'); SAVE_data % PLANT % iSTGDOY = Value
C         Case DEFAULT; ERR = .TRUE.
C         END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Integer',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Integer

!----------------------------------------------------------------------
      Subroutine GET_Char(ModuleName, VarName, Value)
!     Retrieves Integer variable as needed
      IMPLICIT NONE
      Character*(*) ModuleName, VarName, Value
      Character*78  MSG(2)
      Logical ERR

      Value = ' '
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WSTA');  Value = SAVE_data % WEATHER % WSTAT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

C       Case ('PLANT')
C         SELECT CASE (VarName)
C         Case ('iSTNAME');  Value = SAVE_data % PLANT % iSTNAME
C         Case DEFAULT; ERR = .TRUE.
C         END SELECT

      Case Default; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_INTEGER',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Char

!----------------------------------------------------------------------
      SUBROUTINE PUT_Char(ModuleName, VarName, Value)
!     Stores Character variable
      IMPLICIT NONE
      Character*(*) ModuleName, VarName, Value
      Character*78 MSG(2)
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WSTA');  SAVE_data % WEATHER % WSTAT  = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

C       Case ('PLANT')
C         SELECT CASE (VarName)
C         Case ('iSTNAME');  SAVE_data % PLANT % iSTNAME = Value
C         Case DEFAULT; ERR = .TRUE.
C         END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Integer',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Char

!======================================================================
      END MODULE ModuleData
!======================================================================
