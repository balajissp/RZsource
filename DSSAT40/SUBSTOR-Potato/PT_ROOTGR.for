C=======================================================================
C  PT_ROOTGR, Subroutine
C
C  Determines root growth
C-----------------------------------------------------------------------
C  Revision history
C
C  Written
C  09/  /1988 EA & BB Modified by E. Alocilja & B. Baer 
C  04/  /1989 TJ Modified by T. Jou
C  02/08/1989 PWW Header revision and minor changes 
C  02/08/1993 PWW Added switch block, etc. 
C  08/23/2001 CHP Modified for modular format
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : RLDF,RNFAC,RLNEW,RLVF,SWDF,TRLDF,RNLF,L,L1
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : WATBAL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  RLDF() : A root length density factor for soil layer L used to calculate
C           new root growth distribution - unitless
C  RNFAC  : Zero to unity factor describing mineral N availability effect on
C           root growth in Layer L
C  RLNEW  : New root length to be added to the total root system length -
C           cm.  root per sq. cm. ground
C  RLVF   :
C  SWDF   : Soil water deficit factor for Layer L used to calculate root
C           growth and water uptake - unitless value between 0 and 1
C  TRLDF  : An intermediate calculation used to calculate distribution of
C           new root growth in soil
C  RNLF   : Intermediate factor used to calculate distribution of new root
C           growth in the soil - unitless value between 0 and 1
C  L,L1   : Loop counter
C=======================================================================

      SUBROUTINE PT_ROOTGR (DYNAMIC, 
     &    DLAYR, DS, DTT, DUL, FILEIO, GRORT, ISWNIT,     !Input
     &    LL, NH4, NLAYR, NO3, PLTPOP, SHF, SW, SWFAC,    !Input
     &    CUMDEP, RLV, RTDEP)                             !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE

      LOGICAL FIRST
      CHARACTER*1   ISWNIT
      CHARACTER*30 FILEIO

      INTEGER DYNAMIC, L, L1, NLAYR

      REAL CUMDEP, DEP, DEPMAX, DTT, GRORT, PLTPOP
      REAL RLINIT, RLNEW, RLWR, RNFAC, RNLF, RTDEP, RTDEPI
      REAL SDEPTH, SWDF, SWFAC, TRLDF, TRLV

      REAL, DIMENSION(NL) :: DLAYR, DS, DUL, ESW, LL 
      REAL, DIMENSION(NL) :: NH4, NO3, RLDF, RLV, SHF, SW

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CALL PT_IPROOT(FILEIO,                    !Input
     &               RLWR, SDEPTH)              !Output

!********* TEMPORARY CHP *********************************
!     RLWR Sensitivity
!      SELECT CASE(RUN)
!        CASE(1); RLWR = 0.50
!        CASE(2); RLWR = 0.75
!        CASE(3); RLWR = 2.5
!        CASE(4); RLWR = 5.0
!        CASE(5); RLWR = 7.5
!        CASE(6); RLWR = 10.0
!      END SELECT
!*********************************************************

      FIRST = .TRUE.

      DO L = 1, NL
         RLV(L) = 0.0
      END DO

      DEPMAX = DS(NLAYR)
      CUMDEP = 0.0
      RTDEP  = 0.0

!***********************************************************************
!***********************************************************************
!     Daily rate calculations
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     Initial root distribution:  
      IF (FIRST) THEN

!********* TEMPORARY CHP *********************************
!     RLWR Sensitivity
!     Write to Overview.out file - can't do it when value is 
!       set because file is not open yet.
!      CALL GETLUN('OUTO',L)   !Get unit # for Overview.out
!      WRITE(L,*) ' Sensitivity analysis. RLWR = ',RLWR  
!*********************************************************

        !RTDEPI = SDEPTH  
        RTDEPI = MIN(20.0,DS(NLAYR))     !CHP per JWJ                 
        FIRST  = .FALSE.

C-------------------------------------------------------------------------
!       CHP 5/29/03 - Added this section based on CROPGRO initialization
!           at emergence. 
C       INITIALIZE ROOT DEPTH AT EMERGENCE
C       DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
C       RTDEPTI (ROOT DEPTH AT EMERGENCE)
C-------------------------------------------------------------------------
        CUMDEP = 0.

        DO L = 1,NLAYR
          DEP = MIN(RTDEPI - CUMDEP, DLAYR(L))
   !       RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * DEP / ( RTDEP *
   !    &       10000 )
          RLINIT = GRORT * RLWR * PLTPOP
          CUMDEP = CUMDEP + DEP
          RLV(L) = RLINIT / DLAYR(L)
          IF (CUMDEP .GE. RTDEPI) EXIT
        ENDDO

        RTDEP = RTDEPI

!***********************************************************************
      ELSE
!     Daily root growth and distribution

C       RLNEW  = GRORT * PLANTS * 0.75
!       RLNEW  = GRORT * RLWR * PLANTS    ! Read in from species file
        RLNEW  = GRORT * RLWR * PLTPOP  !CHP    
        TRLDF  = 0.0
        CUMDEP = 0.0
        RNFAC  = 1.0

        DO L = 1, NLAYR
           L1     = L
           ESW(L) = DUL(L) - LL(L)
           !   FAC(L) = 10. / (BD(L) * DLAYR(L))
           !   SNO3(L) = NO3(L) / FAC(L)
           !   SNH4(L) = NH4(L) / FAC(L)
           CUMDEP = CUMDEP + DLAYR(L)
           SWDF   = 1.0
           IF (SW(L)-LL(L) .LT. 0.25*ESW(L)) THEN
              SWDF = 4.0*(SW(L)-LL(L))/ESW(L)
           ENDIF
           SWDF = AMAX1 (SWDF,0.0)
           IF (ISWNIT .NE. 'N') THEN
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * TOTIN)
!             RNFAC = 1.0 - (1.17 * EXP(-0.15 * (SNH4(L) + SNO3(L))))
              RNFAC = 1.0 - (1.17 * EXP(-0.15 * (NH4(L) + NO3(L))))
              RNFAC = AMAX1 (RNFAC,0.01)
           ENDIF
           RLDF(L) = AMIN1(SWDF,RNFAC)*SHF(L)*DLAYR(L)
           IF (CUMDEP .LT. RTDEP) THEN
              TRLDF   = TRLDF + RLDF(L)
            ELSE
              RTDEP   = RTDEP + DTT*1.3*AMIN1((SWFAC*2.0),SWDF)
              RTDEP   = AMIN1 (RTDEP,DEPMAX)
              RLDF(L) = RLDF(L)*(1.0-(CUMDEP-RTDEP)/DLAYR(L))
              TRLDF   = TRLDF + RLDF(L)
              EXIT
           END IF
        END DO

!-------------------------------------------------------------------------
        IF (TRLDF .GE. RLNEW*0.00001) THEN
           RNLF = RLNEW/TRLDF
           DO L = 1, L1
              RLV(L) = RLV(L)+RLDF(L)*RNLF/DLAYR(L)-0.005*RLV(L)
              RLV(L) = AMAX1 (RLV(L),0.0)
              RLV(L) = AMIN1 (RLV(L),5.0)
           END DO
        END IF

        TRLV = 0.0
        DO L = 1, NLAYR
          TRLV = TRLV + RLV(L) * DLAYR(L)
        ENDDO

      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE PT_ROOTGR
C=======================================================================


C=======================================================================
C  PT_IPROOT, Subroutine
C
C  Input data for potato root module
C-----------------------------------------------------------------------
C  Revision history
C
C  08/23/2001 CHP Written
C  10/25/2002 CHP Modified read format for Y2K
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------

      SUBROUTINE PT_IPROOT(FILEIO,                    !Input
     &                     RLWR, SDEPTH)              !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.

      IMPLICIT NONE

      INTEGER LUNIO, LUNCRP
      CHARACTER*1, PARAMETER :: BLANK = ' '
      CHARACTER*6, PARAMETER :: ERRKEY = 'ROOTGR'

      CHARACTER*6   SECTION
      CHARACTER*12  FILEC
      CHARACTER*30  FILEIO
      CHARACTER*255  PATHCR
      CHARACTER*255  FILECC
      CHARACTER*180 CHAR

      INTEGER ERR, FOUND, ISECT, LINC, LNUM, PATHL

      REAL RLWR, SDEPTH
C LIWANG MA, RZWQM-DSSAT
      TYPE (ControlType) CONTROL
      TYPE (PLANTVARType) PLANTVAR
C END OF MODIFICATION
!-----------------------------------------------------------------------
!     Read data from FILEIO for use in ROOTGR module
C  MODIFED BY LIWANG MA, RZWQM-DSSAT
          CALL GETPUT_CONTROL('GET',CONTROL)
          FILEC = CONTROL % FILES
c          FILEE = CONTROL % FILEE
c          FILES = CONTROL % FILES
c          PATHSR = CONTROL % PATHSR
          PATHCR = CONTROL % PATHSR
c          PATHER = CONTROL % PATHER

          CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          SDEPTH = PLANTVAR% SDEPTH

c      CALL GETLUN('FILEIO', LUNIO)
c      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
c      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

c      READ(LUNIO,'(6(/),15X,A12,1X,A80)', IOSTAT=ERR) FILEC, PATHCR
c      LNUM = 7
c      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------
c      SECTION = '*PLANT'
c      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
c      IF (FOUND .EQ. 0) THEN
c        CALL ERROR(SECTION, 42, FILEIO, LNUM)
c      ELSE
c        READ (LUNIO,'(55X,F5.1)', IOSTAT=ERR) SDEPTH ; LNUM = LNUM + 1
c        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
c      ENDIF
c
c      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Read Crop Parameters from FILEC
C-----------------------------------------------------------------------
      LNUM   = 0
      PATHL  = INDEX (PATHCR,BLANK)
      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
         FILECC = PATHCR(1:(PATHL-1)) // FILEC
      ENDIF
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,0)

      !DO WHILE (.NOT. EOF (LUNCRP))
      DO
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        IF (ISECT .EQ. 0) THEN
          CALL ERROR(ERRKEY,33,FILECC,LNUM)
          EXIT ! RM EXIT IF 0 or 2, (0=eof, 2 = end of sec)
        END IF 
        !READ (LUNCRP,'(A180)',IOSTAT=ERR) CHAR
        !IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
        IF (CHAR(10:13) .EQ. 'RLWR') THEN
          READ (CHAR,'(14X,F6.0)',IOSTAT=ERR) RLWR
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEC,LNUM)
          EXIT
        ENDIF
      END DO

      CLOSE (LUNCRP)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE PT_IPROOT
C=======================================================================
