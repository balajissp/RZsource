C=======================================================================
C  IPPHENOL Subroutine created from:
C  INPHEN and IPECO Subroutines (and portions of IPIBS, INPLNT, IPCROP)
C  Reads and initializes phenological parameters.  (Called once per
C             simulation)
C----------------------------------------------------------------------
C  REVISION HISTORY
C  03/31/1991 JWW Separated old INPHEN into INPHEN, INVEG, INCOMP
C  04/01/1991 GH  Adapted for CROPGRO
C  08/01/1997 CHP Modified to modularize Phenology sections
C  07/30/1998 CHP Changed name to IPPHENOL - input section of PHENOL module
C  01/12/1999 GH  Incorporated into CROPGRO
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------
C  Called by: PHENOL
C  Calls    : ERROR, FIND, IGNORE
C=======================================================================

      SUBROUTINE IPPHENOL(CONTROL,
     &    ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, CROP,    !Output
     &    CTMP, DLTYP, EVMODC, ISIMI, NPRIOR, NSENP,      !Output
     &    OPTBI, PHTHRS, PLME, SDAGE, SDEPTH, SLOBI,      !Output
     &    THVAR, TRIFOL, TSELC, TB, TO1, TO2, TM, WSENP)  !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
!-----------------------------------------------------------------------
      CHARACTER*1   ISIMI, PLME, BLANK
      CHARACTER*2   CROP
      CHARACTER*3   CTMP(20), DLTYP(20)
      CHARACTER*6   SECTION, ECOTYP, ECONO, ERRKEY
      CHARACTER*12  FILEC, FILEE
      CHARACTER*16  ECONAM
      CHARACTER*30  FILEIO
      CHARACTER*255  CHAR, PATHCR, PATHEC
      CHARACTER*255  FILECC, FILEGC
      CHARACTER*255 C255

      INTEGER LUNIO, NPHS
      INTEGER LUNCRP, LUNECO, ISECT, PATHL
      INTEGER I, J, K
      INTEGER IVRGRP, IVRTEM, ERR, LINC, LNUM, FOUND
      INTEGER NPRIOR(20), TSELC(20), IPATH

      PARAMETER (BLANK = ' ')
      PARAMETER (ERRKEY = 'IPPHEN')
      PARAMETER (NPHS = 13)

      REAL ATEMP, CLDVAR, CLDVRR, CSDVAR, CSDVRR, EVMODC
      REAL OPTBI
      REAL PPSEN, PH2T5, R1PPO, PM06, PM09
      REAL SDEPTH, SDAGE, SLOBI, THVAR, TRIFOL
      REAL TB(5), TO1(5), TO2(5), TM(5)
      REAL WSENP(20), NSENP(20)
      REAL PHTHRS(20), PSENP(20)

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      TYPE (PLANTVARType) PLANTVAR
          CALL GETPUT_CONTROL('GET',CONTROL)
          FILEC = CONTROL % FILES
          FILEE = CONTROL % FILEE
C          FILES = CONTROL % FILES
          PATHCR = CONTROL % PATHSR
C          PATHCR = CONTROL % PATHCR
          PATHEC = CONTROL % PATHER
          CROP = CONTROL % CROP

          CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          ECONO = PLANTVAR% ECONO
          CSDVAR = PLANTVAR% CSDVAR
          PPSEN = PLANTVAR% PPSEN
          PH2T5 = PLANTVAR% PH2T5
          PHTHRS(6) = PLANTVAR% PHTHRS(6)
          PHTHRS(8) = PLANTVAR%  PHTHRS(8)
          PHTHRS(10) = PLANTVAR% PHTHRS(10)
          PHTHRS(13) = PLANTVAR% PHTHRS(13)
          SDEPTH = PLANTVAR% SDEPTH
          ISIMI = 'S'               !start simulation on specific date
          PLME = PLANTVAR% PLME
          SDAGE = PLANTVAR% SDAGE   ! ask for values liwang ma, rzwqm-dssat
          ATEMP = PLANTVAR% ATEMP   ! ask for values liwang ma, rzwqm-dssat
C END OF MODIFICATION
!     Transfer values from constructed data types into local variables.
C      FILEIO  = CONTROL % FILEIO
C      LUNIO   = CONTROL % LUNIO
C
!-----------------------------------------------------------------------
!     Read in values from temporary file, which were previously input
!       in Subroutine IPIBS.
!-----------------------------------------------------------------------
C      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
C      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
C
C      READ (LUNIO,100,IOSTAT=ERR) FILEC, PATHCR; LNUM = 7
C  100 FORMAT(//////,15X,A12,1X,A80)
C      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
C      READ (LUNIO,105,IOSTAT=ERR) FILEE, PATHEC; LNUM = LNUM + 1
C  105 FORMAT(15X,A12,1X,A80)
C      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
!-----------------------------------------------------------------------
!     Subroutine FIND finds appropriate SECTION in a file by
!     searching for the specified 6-character string at beginning
!     of each line.
!-----------------------------------------------------------------------
C      SECTION = '*SIMUL'
C      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
C      READ(LUNIO,'(31X,A1)',IOSTAT=ERR) ISIMI; LNUM = LNUM + 1
C      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
!-----------------------------------------------------------------------
!     Find and read Cultivar Section
!-----------------------------------------------------------------------
C      SECTION = '*CULTI'
C      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
C      READ(LUNIO,'(3X,A2)',IOSTAT=ERR) CROP; LNUM = LNUM + 1
C      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
!-----------------------------------------------------------------------
C      IF (CROP .NE. 'FA') THEN
!-----------------------------------------------------------------------
!     Find and Read Planting Details Section
!-----------------------------------------------------------------------
C        SECTION = '*PLANT'
C        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
C        READ(LUNIO,140,IOSTAT=ERR) PLME, SDEPTH, SDAGE, ATEMP
C  140   FORMAT(35X,A1,19X,F5.1,6X,2(1X,F5.0))
C        LNUM = LNUM + 1
C        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

!-----------------------------------------------------------------------
!     Find and read Cultivar Section
!-----------------------------------------------------------------------
C        SECTION = '*CULTI'
C        CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
C        READ(LUNIO,165,IOSTAT=ERR) ECONO, CSDVAR, PPSEN, PH2T5, 
C     &              PHTHRS(6), PHTHRS(8), PHTHRS(10), PHTHRS(13)
C  165   FORMAT(24X,A6,7F6.0)
C        LNUM = LNUM + 1
C        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C      ENDIF
C
C      CLOSE (LUNIO)

!-----------------------------------------------------------------------
!     Open FILEC
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
C LIWANG MA        PATHL  = INDEX(PATHCR,BLANK)
        PATHL  = IPATH(PATHCR)
        IF (PATHL .LE. 1) THEN
          FILECC = FILEC
        ELSE
          FILECC = PATHCR(1:(PATHL-1)) // FILEC
        ENDIF

        CALL GETLUN('FILEC', LUNCRP)
        OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)
        LNUM = 0
!-----------------------------------------------------------------------
!     Find Leaf Growth Parameters from FILEC and read EVMODC value
!-----------------------------------------------------------------------
        SECTION = '!*LEAF'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,'(24X,F6.1)',IOSTAT=ERR) EVMODC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
!-----------------------------------------------------------------------
!     Find Phenology Section in FILEC and read
!-----------------------------------------------------------------------
        SECTION = '!*PHEN'
        CALL FIND(LUNCRP, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILECC,LNUM)
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,250,IOSTAT=ERR) TB(1), TO1(1), TO2(1), TM(1)
  250   FORMAT(13F6.1)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,250,IOSTAT=ERR) TB(2), TO1(2), TO2(2), TM(2)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
        READ(CHAR,250,IOSTAT=ERR) TB(3), TO1(3), TO2(3), TM(3)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
  
        DO I = 1,NPHS
          CALL IGNORE(LUNCRP,LNUM,ISECT,CHAR)
          READ(CHAR,270,IOSTAT=ERR) J, NPRIOR(J), DLTYP(J), CTMP(J),
     &        TSELC(J), WSENP(J), NSENP(J), PSENP(J)  !PSENP not used
  270     FORMAT(I3,I3,2(2X,A3),1X,I2,3(1X,F5.2))
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        ENDDO

        CLOSE (LUNCRP)

C-----------------------------------------------------------------------
C     Open FILEE
C-----------------------------------------------------------------------
        LNUM = 0
C LIWANG MA        PATHL  = INDEX(PATHEC,BLANK)
        PATHL  = IPATH(PATHEC)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHEC(1:(PATHL-1)) // FILEE
        ENDIF

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,0)
        ECOTYP = '      '
        LNUM = 0
  
        DO WHILE (ECOTYP .NE. ECONO)
          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
          IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &          C255(1:1) .NE. '*') THEN
            READ (C255,3100,IOSTAT=ERR) ECOTYP, ECONAM, IVRGRP, 
     &          IVRTEM, THVAR, (PHTHRS(K), K=1,4), PM06, PM09,
     &          (PHTHRS(K),K=11,12), TRIFOL, R1PPO, OPTBI, SLOBI
 3100       FORMAT (A6, 1X, A16, 1X, 2(1X,I2), 7(1X,F5.0), 6X, 
     &          3(1X,F5.0), 5(6X), 3(1X,F5.0))
            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEGC,LNUM)
            IF (ECOTYP .EQ. ECONO) THEN
              EXIT
            ENDIF

          ELSE IF (ISECT .EQ. 0) THEN
            IF (ECONO .EQ. 'DFAULT') CALL ERROR(ERRKEY,35,FILEGC,LNUM)
            ECONO = 'DFAULT'
            REWIND(LUNECO)
            LNUM = 0
          ENDIF
        ENDDO

        CLOSE (LUNECO)

        PHTHRS(5) = MAX(0.,PH2T5 - PHTHRS(3) - PHTHRS(4))
        PHTHRS(7) = PHTHRS(6) + MAX(0.,(PHTHRS(8) - PHTHRS(6))* PM06)
        PHTHRS(9) = MAX(0.,PHTHRS(10) * PM09)
  
        IF (PPSEN .GE. 0.0) THEN
          CLDVAR = CSDVAR + (1.-THVAR)/MAX(PPSEN,0.000001)
        ELSE IF (PPSEN .LT. 0.0) THEN
          CLDVAR = CSDVAR + (1.-THVAR)/MIN(PPSEN,-0.000001)
        ENDIF
  
        CSDVRR = CSDVAR - R1PPO
        CLDVRR = CLDVAR - R1PPO

      ENDIF

      RETURN
      END    !SUBROUTINE IPPHENOL

!-----------------------------------------------------------------------
!     IPPHENOL LOCAL VARIABLES:  (Other variables defined in PHENOL)
!-----------------------------------------------------------------------
! ATEMP     Temperature of transplant environment (�C)
! CHAR      Contains the contents of last record read 
! CLDVAR    Critical daylength above which development rate remains at min 
!             value (prior to flowering) (hours)
! CLDVRR    Critical daylength above which development rate remains at min 
!             value (after flowering) (hours)
! CROP      Crop identification code 
! CSDVAR    Critical daylength above which development rate decreases 
!             (prior to flowering) (hours)
! CSDVRR    Critical daylength above which development rate decreases 
!             (after flowering) (hours)
! CTMP(I)   Type of curve used for temp. function for phase I: LIN=linear, 
!             QDR=quadratic, SIN=sine function 
! DLTYP(I)  Type of curve used for daylength function for phase I:  NON=no 
!             photoperiod sensitivity, INL=inverse linear 
! ECONAM    Ecotype name 
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! ECOTYP    Ecotype code for this simulation 
! ERR       Error code for file operation 
! ERRKEY    Subroutine name for error file 
! EVMODC    Modifier of rate of vegetative node appearance for the first 
!             few nodes, primarily used for peanut 
! FILEC     Filename for SPE file (e.g., SBGRO980.SPE) 
! FILECC    Path plus filename for species file (*.spe) 
! FILEE     Filename for ECO file (e.g., SBGRO980.ECO) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! ISECT     Data record code (0 - End of file encountered, 1 - Found a good 
!             line to read, 2 - End of Section in file encountered, denoted 
!             by * in column 1 
! ISIMI     Start of simulation code:     E = On reported emergence day, I 
!             = When initial conditions measured, P = On reported planting 
!             date, S = On specified date 
! ISWWAT    Water simulation control switch (Y or N) 
! IVRGRP    Maturity classification (not used) 
! IVRTEM    Temperature classification (1=warm adapted, 2=cool adapted) 
!             (not used) 
! LNUM      Current line number of input file 
! LUNCRP    Logical unit number for FILEC (*.spe file) 
! LUNECO    Logical unit number for FILEE (*.eco file) 
! LUNIO     Logical unit number for FILEIO 
! NL        Maximum number of soil layers = 20 
! NPHS      Number of plant phases 
! NPRIOR(I) The phase of growth at which phase I accumulator can start 
! NSENP(I)  Sensitivity of phase I to Nitrogen stress. Varies from -1 
!             (slows dev) to +1 (hastens dev) 
! OPTBI     Temperature below which growth rate is slowed from emergence to 
!             flowering (�C)
! PATHCR    Pathname for SPE file or FILEE. 
! PATHEC    Pathname for FILEC 
! PATHL     Number of characters in path name (path plus filename for 
!             FILEC) 
! PH2T5     Time from end of juvenile phase to first flowering under 
!             optimal conditions (photothermal days)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur  (thermal or photothermal days)
! PLME      Planting method; T = transplant, S = seed, P = pre-germinated 
!             seed, N = nursery 
! PM06      Proportion of time between first peg and first seed for first 
!             pod.  If = 0, 1st peg and 1st pod occur simultaneously 
! PM09      Proportion of time between 1st seed and physiological maturity 
! PPSEN     Sensitivity to photoperiod; Slope of the relative rate of 
!             development for day lengths above CSDVAR (1/hr)
! PSENP     Sensitivity of phase I to phosphorus stress (not yet used) 
! R1PPO     Increased (+) or decreased (-) sensitivity to photoperiod 
!             expressed as change in critical daylength (hours)
! SDAGE     Transplant age (days)
! SDEPTH    Planting depth (cm)
! SECTION   Section name in input file 
! SLOBI     Sensitivity of growth rate to minimum temperatures from 
!             emergence to flowering 
!            TO2, and  TM Coefficients which define daily temperature distr
! THVAR     Minimum relative rate of reproductive development under long 
!             days and optimal temperature 
! TRIFOL    Rate of appearance on leaves on mainstem. Maximum rate of 
!             V-stage formation (leaves per thermal day)
! TSELC(I)  Number of temperature curve to be used for phase I development 
!             rate: 1=veg, 2=early rep, 3=late rep 
! WSENP(I)  Sensitivity of phase I to water stress, varying from -1 (slows 
!             dev) to 1 (hastens dev) 
!-----------------------------------------------------------------------
!     END SUBROUTINE IPPHENOL
!-----------------------------------------------------------------------
