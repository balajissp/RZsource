C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  CROPGRO Weather Module consists of the following files:
C     WEATHR.FOR  - Main routine for weather module
C     HMET.FOR    - Generates hourly meteorological values from daily data
C     IPWTH.FOR   - Reads daily weather data from FILEW
C     SOLAR.FOR   - Computes day length (DAYLEN) and solar parameters (SOLAR)
C     WGEN.FOR    - Generates daily weather data
C     WTHMOD.FOR  - Modification of daily data based on user-supplied
C                     parameters
C
C=======================================================================
C  WEATHR, Subroutine, N.B. Pickering
C  Weather handling routine: input, weather modification and
C  generation of hourly values.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/1991 NBP Written
C  10/23/1993 NBP Separated DAYLEN function and created DECL to compute
C               declination from DAYL.  Used for sens. analysis.
C  04/04/1996 GH  Added RHUM
C  07/14/1996 GH  Initialize variables for WGEN
C  07/01/1999 CHP Modified for modular format
C  09/02/1999 GH  Incorporated into CROPGRO
C  10/01/2000 NBP Moved growth temperature here from ETPG
C  06/06/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
C  06/02/2005 GH  Fixed call to WTHMOD in Seasinit section
!  10/24/2005 CHP Put weather variables in constructed variable. 
C-----------------------------------------------------------------------
C  Called by: Main
c  Calls:     DAYLEN, ERROR, HMET, IPWTH, SOLAR, WGEN, WTHMDB, WTHMOD
C=======================================================================

      SUBROUTINE WEATHR (CONTROL, ISWITCH, WEATHER)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  MEWTH, RNMODE
      CHARACTER*6  ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*78 MESSAGE(10)
      CHARACTER*255 PATHWT

      INTEGER DOY, MULTI, NEV, RUN, YEAR, YRDOY, YRSIM, YYDDD
      INTEGER RSEED1, RSEED(4), REPNO
      INTEGER DYNAMIC

      REAL
     &  CCO2, CLOUDS, CO2, CO2BAS, DAYL, DEC, ISINB, PAR, 
     &  PI, RAD, RAIN, REFHT, RHUM, S0N, SNDN, SNUP, SRAD, 
     &  TA, TAMP, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &  TMAX, TMIN, TWILEN, WINDHT, WINDSP, XELEV, XLAT, XLONG

      REAL
     &  AZZON(TS), BETA(TS), FRDIFP(TS), FRDIFR(TS), 
     &  PARHR(TS), RADHR(TS), RHUMHR(TS), 
     &  TAIRHR(TS), TGRO(TS), WINDHR(TS)

      PARAMETER (CO2BAS = 330.0)
      PARAMETER (ERRKEY = 'WEATHR')
      PARAMETER (PI=3.14159, RAD=2.0*PI/365.0)

      INTERFACE 
        SUBROUTINE OPSTRESS(C, I, E, P, W)
          USE ModuleDefs
          TYPE (ControlType), Intent(IN)           :: C
          CHARACTER*1,        Intent(IN), Optional :: I
          REAL,               Intent(IN), Optional :: E
          TYPE (PlStresType), Intent(IN), Optional :: P
          TYPE (WeatherType), Intent(IN), Optional :: W
        END SUBROUTINE OPSTRESS
      END INTERFACE

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (WeatherType) WEATHER

      DYNAMIC = CONTROL % DYNAMIC 
      MULTI   = CONTROL % MULTI   
      RUN     = CONTROL % RUN    
      RNMODE  = CONTROL % RNMODE  
      REPNO   = CONTROL % REPNO  
      YRDOY   = CONTROL % YRDOY   
      YRSIM   = CONTROL % YRSIM   

!***********************************************************************
!     Weather station data
      REFHT  =WEATHER %  REFHT
      WINDHT =WEATHER %  WINDHT 
      XLAT   =WEATHER %  XLAT  
      PAR    =WEATHER % PAR   
      RAIN   =WEATHER % RAIN  
      TGROAV =WEATHER %  TGROAV
      TA     =WEATHER % TA    
      SRAD   =WEATHER %  SRAD  
      TMAX   =WEATHER % TMAX  
      TMIN   =WEATHER % TMIN  
      WINDSP =WEATHER %  WINDSP
      CO2    =WEATHER %  CO2   
      TDEW   =WEATHER %  TDEW  


!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
c      CALL IPWTH(CONTROL,
c     &    CCO2, FILEW, MEWTH, PAR, PATHWT,                !Output
c     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
c     &    TAMP, TAV, TDEW, TMAX, TMIN, WINDHT,            !Output
c     &    WINDSP, XELEV, XLAT, XLONG,                     !Output
c     &    RUNINIT)
c
c      CALL WTHMOD(RUNINIT,
c     &    CONTROL, FILEW, XLAT, YYDDD,                    !Input
c     &    DAYL, PAR, RAIN, SRAD, TDEW,                    !Input/Output
c     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
c     &    CO2, DEC, NEV, SNUP, SNDN)                      !Output

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
        YYDDD = YRSIM
        CALL YR_DOY(YYDDD, YEAR, DOY)
!-----------------------------------------------------------------------
c        IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
c          CALL IPWTH(CONTROL,
c     &      CCO2, FILEW, MEWTH, PAR, PATHWT,              !Output
c     &      RAIN, REFHT, RHUM, RSEED1, SRAD,              !Output
c     &      TAMP, TAV, TDEW, TMAX, TMIN, WINDHT,          !Output
c     &      WINDSP, XELEV, XLAT, XLONG,                   !Output
c     &      SEASINIT)

c        ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
C       Set default values FOR REFHT AND WINDHT
c          REFHT  = 1.5
c          WINDHT = 2.0
c          CCO2   = 0.0
c          SRAD = -99.0
c          TMAX = -99.0
c          TMIN = -99.0
c          RAIN = -99.0
c          PAR  = -99.0
c          RHUM = -99.0
c          CALL WGEN (SEASINIT,
c     &      FILEW, MEWTH, MULTI, RUN, PATHWT, REPNO,      !Input
c     &      RNMODE, RSEED1, YRDOY, YRSIM,                 !Input
c     &      PAR, RAIN, RSEED, SRAD, TAMP, TAV, TDEW,      !Output
c     &      TMAX, TMIN, WINDSP, XLAT, XLONG)              !Output
c        ELSE
c          CALL ERROR(ERRKEY,1,' ',0)
c        ENDIF

c        IF (CCO2 .LE. 0.) THEN
c          CO2 = CO2BAS
c        ELSE
c          CO2 = CCO2
c        ENDIF
      
      IF (INDEX('QFN',RNMODE) .LE. 0 .OR. 
     &            (RUN .EQ. 1 .AND. REPNO .EQ. 1)) THEN
C       Substitute default values if TAV or TAMP are missing.  Write a
C         message to the WARNING.OUT file.
!       10/27/2005 CHP The checks for TAV and TAMP were being done in the 
!         STEMP routine, overriding this check. STEMP used .LE. instead 
!         of .LT. and the results were very different for some experiments 
!         which do not supply TAV and TAMP (UFMA8301.PNX, CLMO8501.SBX, 
!         NCCL8801.SBX, GALN0201.COX, others).
!         So -- leave LE for now.
          IF (TAV  .LE. 0.0) THEN       
!         IF (TAV  .LT. 0.0) THEN       
            TAV = 20.0
            WRITE(MESSAGE(1), 100)
            WRITE(MESSAGE(2), 120) TAV
            WRITE(MESSAGE(3), 130)
            CALL WARNING (3, ERRKEY, MESSAGE)
          ENDIF
          IF (TAMP .LE. 0.0) THEN
!         IF (TAMP .LT. 0.0) THEN
            TAMP = 5.0
            WRITE(MESSAGE(1), 110)
            WRITE(MESSAGE(2), 120) TAMP
            WRITE(MESSAGE(3), 130)
            CALL WARNING (3, ERRKEY, MESSAGE)
          ENDIF
        ENDIF

  100 FORMAT
     &   ('Value of TAV, average annual soil temperature, is missing.')
  110 FORMAT('Value of TAMP, amplitude of soil temperature function,',
     &            ' is missing.')
  120 FORMAT('A default value of', F5.1, '�C is being used for this',
     &            ' simulation,')
  130 FORMAT('which may produce undesirable results.')

C     Calculate day length, sunrise and sunset.
      CALL DAYLEN(
     &    DOY, XLAT,                                      !Input
     &    DAYL, DEC, SNDN, SNUP)                          !Output

C     Adjust daily weather data, if weather modification requested.
C     Effective DEC calculated if DAYL is changed.
c      IF (NEV .GT. 0) THEN
c        CALL WTHMOD(SEASINIT, 
c     &    CONTROL, FILEW, XLAT, YYDDD,                    !Input
c     &    DAYL, PAR, RAIN, SRAD, TDEW,                    !Input/Output
c     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
c     &    CO2, DEC, NEV, SNUP, SNDN)                      !Output
c      ENDIF

C     Calculate daily solar parameters.
      CALL SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

C     Calculate hourly weather data.
      CALL HMET(
     &    CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,           !Input
     &    SNDN, SNUP, S0N, SRAD, TDEW, TMAX,              !Input
     &    TMIN, WINDHT, WINDSP, XLAT,                     !Input
     &    AZZON, BETA, FRDIFP, FRDIFR, PARHR, RADHR,      !Output
     &    RHUMHR, TAIRHR, TAVG, TDAY, TGRO, TGROAV,       !Output
     &    TGRODY,WINDHR)                                  !Output

C     Adjust wind speed from reference height to 2m height.
c      WINDSP = WINDSP * (2.0 / WINDHT) ** 2.0

C     Compute daily normal temperature.
c      TA = TAV - SIGN(1.0,XLAT) * TAMP * COS((DOY-20.0)*RAD)

      CALL OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP)                       !Daily values

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS - Read or generate daily weather data
!         (also run for initialization to get first day of weather data
!         for use by soil nitrogen and soil temperature initialization
!         routines.)
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
      YYDDD = YRDOY
      CALL YR_DOY(YYDDD, YEAR, DOY)
!-----------------------------------------------------------------------
C     Read new weather record.
c      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
c        CALL IPWTH(CONTROL,
c     &    CCO2, FILEW, MEWTH, PAR, PATHWT,                !Output
c     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
c     &    TAMP, TAV, TDEW, TMAX, TMIN, WINDHT,            !Output
c     &    WINDSP, XELEV, XLAT, XLONG,                     !Output
c     &    RATE)

c      ELSE IF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
c        SRAD = -99.0
c        TMAX = -99.0
c        TMIN = -99.0
c        RAIN = -99.0
c        PAR  = -99.0

c        CALL WGEN (RATE,
c     &    FILEW, MEWTH, MULTI, RUN, PATHWT, REPNO,        !Input
c     &    RNMODE, RSEED1, YRDOY, YRSIM,                   !Input
c     &    PAR, RAIN, RSEED, SRAD, TAMP, TAV, TDEW,        !Output
c     &    TMAX, TMIN, WINDSP, XLAT, XLONG)                !Output
c      ELSE
c        CALL ERROR(ERRKEY,1,' ',0)
c      ENDIF

C     Calculate day length, sunrise and sunset.
      CALL DAYLEN(
     &    DOY, XLAT,                                      !Input
     &    DAYL, DEC, SNDN, SNUP)                          !Output

C     Calculate twilight to twilight daylength for 
C        rice and maize routines.
      CALL TWILIGHT(DOY, XLAT, TWILEN) 

C     Adjust daily weather data, if weather modification requested.
C     Effective DEC calculated if DAYL is changed.
c      IF (NEV .GT. 0) THEN
c        CALL WTHMOD(RATE, 
c     &    CONTROL, FILEW, XLAT, YYDDD,                    !Input
c     &    DAYL, PAR, RAIN, SRAD, TDEW,                    !Input/Output
c     &    TMAX, TMIN, TWILEN, WINDSP,                     !Input/Output
c     &    CO2, DEC, NEV, SNUP, SNDN)                      !Output
c      ENDIF

C     Calculate daily solar parameters.
      CALL SOLAR(
     &    DAYL, DEC, SRAD, XLAT,                          !Input
     &    CLOUDS, ISINB, S0N)                             !Output

C     Calculate hourly weather data.
      CALL HMET(
     &    CLOUDS, DAYL, DEC, ISINB, PAR, REFHT,           !Input
     &    SNDN, SNUP, S0N, SRAD, TDEW, TMAX,              !Input
     &    TMIN, WINDHT, WINDSP, XLAT,                     !Input
     &    AZZON, BETA, FRDIFP, FRDIFR, PARHR, RADHR,      !Output
     &    RHUMHR, TAIRHR, TAVG, TDAY, TGRO, TGROAV,       !Output
     &    TGRODY,WINDHR)                                  !Output

      !Adjust wind speed from reference height to 2m height.
c      WINDSP = WINDSP * (2.0 / WINDHT) ** 2.0

C     Compute daily normal temperature.
c      TA = TAV - SIGN(1.0,XLAT) * TAMP * COS((DOY-20.0)*RAD)

      CALL OPSTRESS(CONTROL, W=WEATHER)

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      CALL OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP)                       !Daily values

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
c      IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
c        CALL IPWTH(CONTROL,
c     &    CCO2, FILEW, MEWTH, PAR, PATHWT,                !Output
c     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
c     &    TAMP, TAV, TDEW, TMAX, TMIN, WINDHT,            !Output
c     &    WINDSP, XELEV, XLAT, XLONG,                     !Output
c     &    FINAL)
c      ENDIF

      CALL OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP)                       !Daily values

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!     Daily data
      WEATHER % CLOUDS = CLOUDS
      WEATHER % DAYL   = DAYL  
      WEATHER % SNDN   = SNDN  
      WEATHER % SNUP   = SNUP  
      WEATHER % TAMP   = TAMP  
      WEATHER % TAV    = TAV   
      WEATHER % TAVG   = TAVG  
      WEATHER % TDAY   = TDAY  
      WEATHER % TGRODY = TGRODY
      WEATHER % TWILEN = TWILEN
!     Hourly data
      WEATHER % AZZON  = AZZON 
      WEATHER % BETA   = BETA  
      WEATHER % FRDIFP = FRDIFP
      WEATHER % FRDIFR = FRDIFR
      WEATHER % PARHR  = PARHR 
      WEATHER % RADHR  = RADHR 
      WEATHER % RHUMHR = RHUMHR
      WEATHER % TAIRHR = TAIRHR
      WEATHER % TGRO   = TGRO  
      WEATHER % WINDHR = WINDHR

      RETURN
      END SUBROUTINE WEATHR

!***********************************************************************
!***********************************************************************
! WEATHR Variables
!-----------------------------------------------------------------------
! AZZON(TS)  Hourly solar azimuth (+/- from South) (deg.)
! BETA(TS)   Hourly solar elevation (+/- from horizontal) (deg.)
! CCO2       Atmospheric CO2 concentration read from input file (ppm)
! CLOUDS     Relative cloudiness factor (0-1) 
! CO2        Atmospheric carbon dioxide concentration (ppm)
! CO2BAS     Carbon dioxide base level from which adjustments are made
!              (ppm)
! DAYL       Day length on day of simulation (from sunrise to sunset) (hr)
! DEC        Solar declination or (90o - solar elevation at noon). 
!              Amplitude = +/- 23.45. Min = Dec. 21, Max = Jun 21/22 (deg.)
! DOY        Current day of simulation (d)
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or FINAL 
! ERRKEY     Subroutine name for error file 
! FILEW      Weather data file 
! FRDIFP(TS) Hourly fraction diffuse photon flux density after correcting 
!              for circumsolar radiation (Spitters, 1986) 
! FRDIFR(TS) Hourly fraction diffuse solar radiation after correcting for 
!              circumsolar radiation (Spitters, 1986) 
! ISINB      Integral in Spitter's Equation 6 
! MEWTH      Switch for method of obtaining weather data-- 'G' or 'M'- read 
!              weather data file 'S'- read SIMMETEO inputs and generate 
!              weather data 'W'- read WGEN inputs and generate weather data 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NEV        Number of environmental modification records 
! RUN       Report number for sequenced or multi-season runs 
! PAR        Daily photosynthetically active radiation or photon flux 
!              density (moles[quanta]/m2-d)
! PARHR(TS)  hourly PAR (J / m2 - s)
! PATHWT     Directory path for weather file 
! RADHR(TS)  Total hourly solar radiation (J/m2-s)
! RAIN       Precipitation depth for current day (mm)
! REFHT      Reference height for wind speed (m)
! RHUM       Relative humidity (%)
! RHUMHR(TS) Relative humidity hourly value (%)
! RSEED(4)   Random number generator seeds 
! RSEED1     Random number generator seed- user input 
! S0N        Normal extraterrestrial radiation (set equal to average solar 
!              constant; elliptical orbit ignored) (J/m2-s)
! SNDN       Time of sunset (hr)
! SNUP       Time of sunrise (hr)
! SRAD       Solar radiation (MJ/m2-d)
! TAIRHR(TS) Hourly air temperature (in some routines called TGRO) (�C)
! TAMP       Amplitude of temperature function used to calculate soil 
!              temperatures (�C)
! TAV        Average annual soil temperature, used with TAMP to calculate 
!              soil temperature. (�C)
! TAVG       Average daily temperature (�C)
! TDAY       Average temperature during daylight hours (�C)
! TDEW       Dewpoint temperature (�C)
! TGRO(I)    Hourly air temperature (�C)
! TGROAV     Average daily canopy temperature (�C)
! TGRODY     Average temperature during daylight hours (�C)
! TMAX       Maximum daily temperature (�C)
! TMIN       Minimum daily temperature (�C)
! TS         Number of intermediate time steps (=24) 
! WINDHR(TS) Hourly wind speed (m/s)
! WINDHT     Reference height for wind speed (m)
! WINDSP     Wind speed (km/d)
! XELEV      Field elevation (not used) (m)
! XLAT       Latitude (deg.)
! XLONG      Longitude (deg.)
! YEAR       Year of current date of simulation 
! YR_DOY     Function subroutoine converts date in YYDDD format to integer 
!              year (YY) and day (DDD). 
! YRDOY      Current day of simulation (YYDDD)
! YRSIM      Start of simulation date (YYDDD)
! YYDDD      Current date for weather module 
!***********************************************************************
!***********************************************************************


