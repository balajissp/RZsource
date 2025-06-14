C=======================================================================
C  OpWeath, Subroutine, C.H.Porter from weather portions of OPDAY
C  Generates output for daily weather data.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  02/08/2002 CHP Written
C  06/06/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called from:   WEATHR
C  Calls:         None
C=======================================================================
      SUBROUTINE OpWeath(CONTROL, ISWITCH, 
     &    CLOUDS, CO2, DAYL, PAR, RAIN, SRAD,         !Daily values
     &    TAVG, TDAY, TDEW, TGROAV, TGRODY, TMAX,     !Daily values
     &    TMIN, TWILEN, WINDSP)                       !Daily values

!     Daily values:
!     SRAD,TMAX,TMIN,RAIN,TDEW,WINDSP,PAR,RHUM

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETG, RNMODE
      CHARACTER*6,  PARAMETER :: ERRKEY = 'OPWTHR'
      CHARACTER*11, PARAMETER :: OUTWTH = 'Weather.OUT'
      CHARACTER*30 FILEIO

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, LUN

      INTEGER RUN, YEAR, YRDOY, YRSIM, REPNO

      REAL
     &  CLOUDS, CO2, DAYL, PAR, RAIN, SRAD, 
     &  TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &  TMAX, TMIN, TWILEN, WINDSP

      LOGICAL FEXIST

C-----------------------------------------------------------------------
C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
C-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
C-----------------------------------------------------------------------
C     The variable "ISWITCH" is of type "SwitchType".
C-----------------------------------------------------------------------
      TYPE (SwitchType) ISWITCH

      IDETG   = ISWITCH % IDETG
      IF (IDETG .NE. 'Y') RETURN

      DAS     = CONTROL % DAS 
      DYNAMIC = CONTROL % DYNAMIC 
      FILEIO  = CONTROL % FILEIO  
      FROP    = CONTROL % FROP  
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY   
      YRSIM   = CONTROL % YRSIM   

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
        CALL GETLUN('OUTWTH', LUN)
        INQUIRE (FILE = OUTWTH, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = OUTWTH, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUN,'("*WEATHER MODULE DAILY OUTPUT FILE")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, FILEIO, LUN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, FILEIO, LUN, RUN)
          ENDIF

C-----------------------------------------------------------------------
C     Variable heading for Weather.OUT
C-----------------------------------------------------------------------
          WRITE (LUN,120)
  120     FORMAT('@YEAR DOY   DAS',
     &    '   PRED   DYLD   TWLD   SRAD   PARD   CLDD   TMXD   TMND',
     &    '   TAVD   TDYD   TDWD   TGAD   TGRD   WDSD   CO2D')
        ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output - OUTPUT and FINAL calls
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
C       Generate output for file Weather.OUT
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. FINAL  .AND. MOD(DAS,FROP) .NE. 0)) THEN 
      
          CALL YR_DOY(YRDOY, YEAR, DOY)
          !Daily printout
          WRITE (LUN,300) YEAR, DOY, DAS, 
     &        RAIN, DAYL, TWILEN, SRAD, PAR, CLOUDS, 
     &        TMAX, TMIN, TAVG, TDAY, TDEW, TGROAV, TGRODY,
     &        WINDSP, NINT(CO2)
  300     FORMAT(1X,I4,1X,I3.3,1X,I5,
     &        5(1X,F6.1),1X,F6.2,
     &        8(1X,F6.1),1X,I6)
        ENDIF

        IF (DYNAMIC .EQ. FINAL) THEN
          !Close daily output files.
          CLOSE (LUN)
        ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpWeath
!***********************************************************************
