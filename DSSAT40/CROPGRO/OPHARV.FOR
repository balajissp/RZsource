C=======================================================================
C  OPHARV, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  03/03/2002 GH  Modified logic for reading fileA
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking and changed call to READA
!  12/16/2004 KJB, LAH, CHP Change BWAH to BWAM, compute as 
!                   TOPWT - SDWT, instead of STMWT
!  02/03/2005 CHP Change HWAM to mean dry weight at harvest maturity 
!                   (not physiological maturity).
!  02/04/2005 CHP Added PODWT to SUMVALS array for output to Summary.out
!  08/11/2005 CHP/GH BWAH = TOPWT - PODWT (was TOPWT - SDWT)
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C=======================================================================

      SUBROUTINE OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, CANHT, CANNAA, CANWAA, CROP, LAIMX,     !Input
     &    HARVFRAC, MDATE, NSTRES, PCLSD, PCNSD, PODNO,   !Input
     &    PODWT, SDRATE, SDWT, SEEDNO, STGDOY, SWFAC,     !Input
     &    TOPWT, TURFAC, VSTAGE, WTNCAN, WTNFX, WTNSD,    !Input
     &    WTNST, WTNUP, XLAI, YRNR1, YRNR3, YRNR5, 
     &    YRNR7, YRPLT,                                   !Input
     &    SDWTAH,RNAD,RWAD)                                         !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  RNMODE,IDETO,IPLTI, PLME
      CHARACTER*2  CROP
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPHARV'
      CHARACTER*10 STNAME(20)
      CHARACTER*12 FILEA
      CHARACTER*30 FILEIO

      INTEGER ACOUNT, DFLR,DFPD,DFSD,DHRV,DNR8,DMAT,DNR1,DNR3,DNR5,DNR7
      INTEGER DYNAMIC, ERRNUM, FOUND
      INTEGER IFLR, IFPD, IFSD, IHRV, IMAT, ISENS
      INTEGER LINC, LNUM, LUNIO, RUN, TIMDIF, TRTNO, YIELD
      INTEGER YRNR1,YRNR3,YRNR5,YRNR7,MDATE,YRDOY, YRPLT,YRSIM
      INTEGER STGDOY(20)

      REAL BIOMAS, BWAH, CANHT, CANNAA, CANWAA, HI, HWAH, HWAM
      REAL LAIMX, PCLSD, PCNSD, PODWT, PODNO, PSDWT, PSPP
      REAL SDRATE, SDWT, SDWTAH, SEEDNO
      REAL THRES, TOPWT, VSTAGE, RNAD, RWAD
      REAL WTNCAN, WTNFX, WTNSD, WTNST, WTNUP, XLAI
      REAL, DIMENSION(3) :: HARVFRAC

!     Arrays which contain data for printing in SUMMARY.OUT file
!       (OPSUM subroutine)
      INTEGER, PARAMETER :: SUMNUM = 17
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!     Arrays which contain Simulated and Measured data for printing
!       in OVERVIEW.OUT and EVALUATE.OUT files (OPVIEW subroutine)
      CHARACTER*4 OLAB(40), OLAP(40)  !OLAP modified for dap
      CHARACTER*6  X(40)
      CHARACTER*8 Simulated(40), Measured(40)
      CHARACTER*35 DESCRIP(40)

!     Variables added for environmental and stress factors output
      REAL AGEFAC, NSTRES, SWFAC, TURFAC
      TYPE (PlStresType) PlantStres

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (PLANTVARType) PLANTVAR
      CALL GETPUT_PLANTVAR('GET',PLANTVAR)

!     Transfer values from constructed data types into local variables.
      DYNAMIC= CONTROL % DYNAMIC
      FILEIO = CONTROL % FILEIO
      LUNIO  = CONTROL % LUNIO
      RUN    = CONTROL % RUN
      RNMODE = CONTROL % RNMODE
      YRDOY  = CONTROL % YRDOY
      YRSIM  = CONTROL % YRSIM
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
      FILEA = CONTROL % FILEA
	TRTNO = CONTROL % TRTNO
      PLME = PLANTVAR% PLME
      ISENS = 0
C END OF MODIFICATION

      IDETO = ISWITCH % IDETO
      IPLTI = ISWITCH % IPLTI

      ACOUNT = 26  !Number of possible FILEA headers for this crop

!CHP 12/16/2004 Need to be able to read FILEA headers of either
!     'BWAM' or 'BWAH' and interpret data as 'BWAM'

!     Define headings for observed data file (FILEA)
      DATA OLAB / !Pred.          Obs.   Definition
                  !------------   -----  -----------
     & 'ADAT', ! 1 DNR1           DFLR   Anthesis date
     & 'PD1T', ! 2 DNR3           DFPD   First Pod        
     & 'PDFT', ! 3 DNR5           DFSD   First Seed       
     & 'MDAT', ! 4 DNR7           DMAT   Physiological Maturity
     & 'HWAM', ! 5 NINT(SDWT*10)  XGWT   Seed Yield (kg/ha;dry)
     & 'PWAM', ! 6 NINT(PODWT*10) XPDW   Pod Yield (kg/ha;dry) 
     & 'H#AM', ! 7 NINT(SEEDNO)   XNOGR  Seed Number (Seed/m2)
     & 'HWUM', ! 8 PSDWT          XGWU   Weight Per Seed (g;dry)
     & 'H#UM', ! 9 PSPP           XNOGU  Seeds/Pod
     & 'CWAM', !10 NINT(TOPWT*10) XCWT   Biomass (kg/ha) at Harvest Mat.

!     & 'BWAH', !11 (STMWT)*10    XSWT   Stem weight (kg/ha) at Mat.
! KJB, LAH, CHP 12/16/2004  change BWAH to BWAM
!     & 'BWAM', !11 (TOPWT-SDWT)*10 XSWT  Tops - seed (kg/ha) at Mat.
! CHP/GH 08/11/2005 Change BWAM = TOPWT - PODWT
     & 'BWAM', !11 (TOPWT-PODWT)*10 XSWT Tops - seed (kg/ha) at Mat.

     & 'LAIX', !12 LAIMX          XLAM   Maximum LAI (m2/m2)
     & 'HIAM', !13 HI             XHIN   Harvest Index (kg/kg)
     & 'THAM', !14 THRES          XTHR   Shelling Percentage (%)
     & 'GNAM', !15 NINT(WTNSD*10) XNGR   Seed N (kg N/ha)
     & 'CNAM', !16 NINT(WTNCAN*10)XNTP   Biomass N (kg N/ha)
     & 'SNAM', !17 NINT(WTNST*10) XNST   Stalk N (kg N/ha)
     & 'GN%M', !18 PCNSD          XNPS   Seed N (%)
     & 'CWAA', !19 NINT(CANWAA*10)XCWAA  Biomass (kg/ha) at Anthesis
     & 'CNAA', !20                XCNAA 
     & 'L#SM', !21 VSTAGE         XLFNO  Final Leaf Number (Main Stem)
     & 'GL%M', !22 PCLSD          XLPS   Seed Lipid (%)
     & 'CHTA', !23 CANHT          XCNHT  Canopy Height (m)
     & 'R8AT', !24 DNR8           DHRV   Harvest Maturity (dap)
     & 'RNAD', !25 RNAD root N kg/ha
     & 'RWAD', !26 RWAD ROOT BIOMASS KG/HA
     & 14*'    '/

!     GWAH    !Grain weight at harvest (kg/ha)
!     CWAH    !Canopy weight at harvest (kg/ha)
!     FWAH    !Fruit weight at harvest (kg/ha)

!  9/14/2004 GH, JWW, CHP
!  Problem with PDFT header and corresponding variables DNR5 and DFSD.
!  The simulated variable, DNR5, corresponds to first seed.
!  The observed variable, DFSD, also seems to correspond to first seed.
!  But the label in DATA.CDE for header PDFT is full pod (which 
!     corresponds to R4 stage, not R5). 
!  The apparent fix is to change all PDFT headers in FILEA to R5AT.
!  The OLAB array element 3 would have to be changed also to R5AT.

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
C MODIFIED BY LIWANG MA, RZWQM-DSSAT
!     Read FILEIO
C      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
C      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
C
C      READ (LUNIO,'(55X,I5)',IOSTAT=ERRNUM) ISENS; LNUM = 1   
C      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
C      READ (LUNIO,'(3(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA
C      LNUM = LNUM + 4
C      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
C  
C      SECTION = '*TREAT'
C      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C      IF (FOUND .EQ. 0) THEN
C        CALL ERROR(SECTION, 42, FILEIO, LNUM)
C      ELSE
C        READ(LUNIO, '(I3)',IOSTAT=ERRNUM) TRTNO ; LNUM = LNUM + 1
C        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
C      ENDIF
C
!     Find and Read Planting Details Section
C      SECTION = '*PLANT'
C      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C      IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO,LNUM)
C      READ(LUNIO,'(35X,A1)',IOSTAT=ERRNUM) PLME ; LNUM = LNUM + 1
C      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
C
C      CLOSE (LUNIO)
C END OF MODIFICATION

!     Assign names to stages based on crop.
      CALL STNAMES(CROP, PLME, STNAME)
      
!     Assign descriptions to Measured and Simulated data 
!         from DATA.CDE.
      CALL GETDESC(ACOUNT, OLAB, DESCRIP)
      OLAP = OLAB

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT)

C***********************************************************************
C***********************************************************************
C     SEASONAL INITIALIZATION
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      Simulated = ' '
      Measured  = ' '
      YIELD  = 0
      BIOMAS = 0.0
      
!     Establish #, names of stages for environmental & stress summary
      PlantStres % ACTIVE = .FALSE.
      PlantStres % StageName = '                       '
      SELECT CASE (CROP)
      CASE ('BN','CH','CO','CP','CT','FB','PE',
     &      'PN','PP','PR','SB','TM','VB','CA','SU')
        PlantStres % NSTAGES = 4
        PlantStres % StageName(1)  = 'Emergence -First Flower'
        PlantStres % StageName(2)  = 'First Flower-First Seed'
        PlantStres % StageName(3)  = 'First Seed - Phys. Mat.'
        PlantStres % StageName(4)  = 'Emergence  - Phys. Mat.'

      CASE ('CB')
        PlantStres % NSTAGES = 1
        PlantStres % StageName(1)  = 'Sowing  - Harvest      '

      CASE ('BR','C3','C4','G0','G1','G2',
     &      'G3','G4','G5','G6','G7','G8','G9')
        PlantStres % NSTAGES = 1
        PlantStres % StageName(1)  = 'Emergence  - Phys. Mat.'

      CASE DEFAULT
        PlantStres % NSTAGES = 0
      END SELECT

      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      BIOMAS = TOPWT*10.

      PlantStres % TURFAC  = TURFAC
      PlantStres % SWFAC   = SWFAC 
      PlantStres % NSTRES  = NSTRES
      PlantStres % AGEFAC  = AGEFAC
      PlantStres % ACTIVE  = .FALSE.

!     Set ACTIVE variable to indicate that current phase is active
      SELECT CASE (CROP)
      CASE ('BN','CH','CO','CP','CT','FB','PE',
     &      'PN','PP','PR','SB','TM','VB','CA','SU')
        IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(5)) THEN
          PlantStres % ACTIVE(1) = .TRUE.
        END IF

        IF (YRDOY > STGDOY(5) .AND. YRDOY <= STGDOY(8)) THEN
          PlantStres % ACTIVE(2) = .TRUE.
        END IF

        IF (YRDOY > STGDOY(8) .AND. YRDOY <= STGDOY(10)) THEN
          PlantStres % ACTIVE(3) = .TRUE.
        END IF

        IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(10)) THEN
          PlantStres % ACTIVE(4) = .TRUE.
        ENDIF

      CASE ('CB')
        IF (YRDOY > STGDOY(15) .AND. YRDOY <= STGDOY(16)) THEN
          PlantStres % ACTIVE(1) = .TRUE.
        ENDIF

      CASE ('BR','C3','C4','G0','G1','G2',
     &      'G3','G4','G5','G6','G7','G8','G9')
        IF (YRDOY > STGDOY(1) .AND. YRDOY <= STGDOY(16)) THEN
          PlantStres % ACTIVE(1) = .TRUE.
        ENDIF
      END SELECT

!     Send data to Overview.out data on days where stages occur
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
!     Compute values to be sent to Overview, Summary and Evaluate files.
      IF (SEEDNO .GT. 1E-4) THEN
         PSDWT = SDWT/SEEDNO
      ELSE
         PSDWT = 0.0
      ENDIF

      IF (PODNO .GT. 1E-4) THEN
         PSPP = SEEDNO/PODNO
      ELSE
         PSPP  = 0.
      ENDIF

      IF (PODWT .GT. 0.1) THEN
         THRES = SDWT*100./PODWT
      ELSE
         THRES = 0.0
      ENDIF
      THRES = MIN(THRES,99.99)

      IF (TOPWT .GT. 1E-4 .AND. SDWT .GE. 1E-4) THEN
         HI = SDWT/TOPWT
      ELSE
         HI = 0.
      ENDIF

      IF (CROP .EQ. 'FA') YRPLT = -99

!-----------------------------------------------------------------------
!     Read Measured (measured) data from FILEA
!-----------------------------------------------------------------------
      IF(IDETO .EQ. 'Y' .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
        CALL READA(FILEA, OLAB, TRTNO, YRSIM, X)

!     Convert from YRDOY format to DAP.  Change descriptions to match.
        CALL READA_Dates(X(1), YRSIM, IFLR)
        IF (IFLR .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFLR = TIMDIF(YRPLT,IFLR)
        ELSE
          DFLR  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(1)) 
        OLAP(1) = 'ADAP'
        CALL GetDesc(1,OLAP(1), DESCRIP(1))

        CALL READA_Dates(X(2), YRSIM, IFPD)
        IF (IFPD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFPD = TIMDIF(YRPLT,IFPD)
        ELSE
          DFPD  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(2)) 
        OLAP(2) = 'PD1P'
        CALL GetDesc(1,OLAP(2), DESCRIP(2))

        CALL READA_Dates(X(3), YRSIM, IFSD)
        IF (IFSD .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DFSD = TIMDIF(YRPLT,IFSD)
        ELSE
          DFSD  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(3))
        OLAP(3) = 'PDFP'
        CALL GetDesc(1,OLAP(3), DESCRIP(3))

        CALL READA_Dates(X(4), YRSIM, IMAT)
        IF (IMAT .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DMAT = TIMDIF(YRPLT,IMAT)
        ELSE
          DMAT  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(4)) 
        OLAP(4) = 'MDAP'
        CALL GetDesc(1,OLAP(4), DESCRIP(4))

        CALL READA_Dates(X(24), YRSIM, IHRV)
        IF (IHRV .GT. 0 .AND. IPLTI .EQ. 'R' .AND. ISENS .EQ. 0) THEN
          DHRV = TIMDIF(YRPLT,IHRV)
        ELSE
          DHRV  = -99
        ENDIF
        !CALL CHANGE_DESC(DESCRIP(24)) 
        OLAP(24) = 'R8AP'
        CALL GetDesc(1,OLAP(24), DESCRIP(24))

        DNR1 = TIMDIF(YRPLT,YRNR1)
        IF (DNR1 .LE. 0) THEN
          DNR1 = -99
          YRNR1 = -99
        ENDIF

        DNR3 = TIMDIF(YRPLT,YRNR3)
        IF (DNR3 .LE. 0) THEN
          DNR3 = -99
          YRNR3 = -99
        ENDIF

        DNR5 = TIMDIF(YRPLT,YRNR5)
        IF (DNR5 .LE. 0) THEN
          DNR5 = -99
          YRNR5 = -99
        ENDIF

        DNR7 = TIMDIF(YRPLT,YRNR7)
        IF (DNR7 .LE. 0) THEN
          DNR7 = -99
          YRNR7 = -99
        ENDIF

        DNR8 = TIMDIF(YRPLT,MDATE)
        IF (DNR8 .LE. 0) THEN
          DNR8 = -99
          MDATE = -99
        ENDIF

!-----------------------------------------------------------------------
!     Store Simulated and Measured data for this season.
      WRITE(Simulated(1),' (I8)') DNR1;  WRITE(Measured(1),'(I8)') DFLR
      WRITE(Simulated(2),' (I8)') DNR3;  WRITE(Measured(2),'(I8)') DFPD
      WRITE(Simulated(3),' (I8)') DNR5;  WRITE(Measured(3),'(I8)') DFSD
      WRITE(Simulated(4),' (I8)') DNR7;  WRITE(Measured(4),'(I8)') DMAT
      WRITE(Simulated(5),' (I8)') NINT(SDWT*10);  
                                         WRITE(Measured(5),'(A8)') X(5)
      WRITE(Simulated(6),' (I8)') NINT(PODWT*10); 
                                         WRITE(Measured(6),'(A8)') X(6)
      WRITE(Simulated(7),' (I8)') NINT(SEEDNO);   
                                         WRITE(Measured(7),'(A8)') X(7)
      WRITE(Simulated(8),'(F8.4)')PSDWT; WRITE(Measured(8),'(A8)') X(8)
      WRITE(Simulated(9),'(F8.2)')PSPP;  WRITE(Measured(9),'(A8)') X(9)
      WRITE(Simulated(10),'(I8)') NINT(TOPWT*10); 
                                         WRITE(Measured(10),'(A8)')X(10)
!      WRITE(Simulated(11),'(I8)') NINT(STMWT*10); 
!                                         WRITE(Measured(11),'(A8)')X(11)
! KJB, LAH, CHP 12/16/2004  change BWAH to BWAM
      WRITE(Simulated(11),'(I8)') NINT(TOPWT-SDWT)*10; 
                                         WRITE(Measured(11),'(A8)')X(11)
      WRITE(Simulated(12),'(F8.2)')LAIMX;WRITE(Measured(12),'(A8)')X(12)
      WRITE(Simulated(13),'(F8.3)')HI;   WRITE(Measured(13),'(A8)')X(13)
      WRITE(Simulated(14),'(F8.2)')THRES;WRITE(Measured(14),'(A8)')X(14)
      WRITE(Simulated(15),'(I8)') NINT(WTNSD*10); 
                                         WRITE(Measured(15),'(A8)')X(15)
      WRITE(Simulated(16),'(I8)') NINT(WTNCAN*10);
                                         WRITE(Measured(16),'(A8)')X(16)
      WRITE(Simulated(17),'(I8)') NINT(WTNST*10); 
                                         WRITE(Measured(17),'(A8)')X(17)
      WRITE(Simulated(18),'(F8.2)')PCNSD;WRITE(Measured(18),'(A8)')X(18)
      WRITE(Simulated(19),'(I8)') NINT(CANWAA*10);
                                         WRITE(Measured(19),'(A8)')X(19)
      WRITE(Simulated(20),'(I8)') NINT(CANNAA*10);
                                         WRITE(Measured(20),'(A8)')X(20)
      WRITE(Simulated(21),'(F8.2)')VSTAGE
                                         WRITE(Measured(21),'(A8)')X(21)
      WRITE(Simulated(22),'(F8.2)')PCLSD;WRITE(Measured(22),'(A8)')X(22)
      WRITE(Simulated(23),'(F8.2)')CANHT;WRITE(Measured(23),'(A8)')X(23)
      WRITE(Simulated(24),'(I8)')  DNR8; WRITE(Measured(24),'(I8)')DHRV
      WRITE(Simulated(25),'(F8.2)')RNAD; WRITE(Measured(25),'(A8)')X(24)
      WRITE(Simulated(26),'(F8.2)')RWAD; WRITE(Measured(26),'(A8)')X(25)
     
      ENDIF  

!-----------------------------------------------------------------------
!     Send data to OPSUM for SUMMARY.OUT file.
!-----------------------------------------------------------------------
!     Compute values to be sent to OPSUM for SUMMARY.OUT file.

C     Actual yield harvested (default is 100 %)
      SDWTAH = SDWT * HARVFRAC(1)
!      SDWTAH = SDWT * HPC(1)/100.
!     Let OPSUM multiply by HPC - not available here.
!      SDWTAH = SDWT

C     Actual byproduct harvested (default is 0 %)
C     Byproduct not harvested is incorporated
!      BWAH   = (TOPWT - SDWT)
!      BWAH   = (TOPWT - SDWT) * HBPC(1)/100.
!08/11/2005 GH/CHP
!      BWAH   = (TOPWT - SDWT) * HARVFRAC(2)
!     By-product, for use in Summary.OUT is the by-product harvested 
!     for its economic value -- generally the stalks.
      BWAH   = (TOPWT - PODWT) * HARVFRAC(2)

      IF (SEEDNO .GT. 1E-4) THEN
         PSDWT = SDWT/SEEDNO
      ELSE
         PSDWT = 0.0
      ENDIF

      IF (PODNO .GT. 1E-4) THEN
         PSPP = SEEDNO/PODNO
      ELSE
         PSPP  = 0.
      ENDIF

      IF ((CROP .EQ. 'TM') .OR. (CROP .EQ. 'PR')) THEN
        HWAM = PODWT * 10.
        HWAH = PODWT * 10.
      ELSE
!chp 2/3/05 per GH        HWAM = SDWTAM * 10.
        HWAM = SDWT * 10.
        HWAH = SDWTAH * 10.
      END IF

!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.
        LABEL(1)  = 'ADAT'; VALUE(1)  = FLOAT(YRNR1)
        LABEL(2)  = 'MDAT'; VALUE(2)  = FLOAT(YRNR7)
        LABEL(3)  = 'DWAP'; VALUE(3)  = SDRATE
        LABEL(4)  = 'CWAM'; VALUE(4)  = TOPWT*10.
        LABEL(5)  = 'HWAM'; VALUE(5)  = HWAM
        LABEL(6)  = 'HWAH'; VALUE(6)  = HWAH
        LABEL(7)  = 'BWAH'; VALUE(7)  = BWAH
        LABEL(8)  = 'HWUM'; VALUE(8)  = PSDWT     !*1000.
        LABEL(9)  = 'H#AM'; VALUE(9)  = SEEDNO
        LABEL(10) = 'H#UM'; VALUE(10) = PSPP
        LABEL(11) = 'NFXM'; VALUE(11) = WTNFX*10.
        LABEL(12) = 'NUCM'; VALUE(12) = WTNUP*10.
        LABEL(13) = 'CNAM'; VALUE(13) = WTNCAN*10.
        LABEL(14) = 'GNAM'; VALUE(14) = WTNSD*10.
        LABEL(15) = 'PWAM'; VALUE(15) = PODWT * 10.
        LABEL(16) = 'LAIX'; VALUE(16) = LAIMX
        LABEL(17) = 'HIAM'; VALUE(17) = HI

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

!-----------------------------------------------------------------------
!     Call Overview.out routine
      BIOMAS = TOPWT*10.
!      YIELD  = NINT(HWAM)
      YIELD  = NINT(HWAH)     !12/10/2003
      
      CALL OPVIEW(CONTROL, 
     &    BIOMAS, ACOUNT, DESCRIP, IDETO, VSTAGE, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT)

!-----------------------------------------------------------------------
      !Send Measured and Simulated datat to OPSUM
      IF(IDETO .EQ. 'Y' .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) THEN
        CALL EvaluateDat (ACOUNT, Measured, Simulated, DESCRIP, OLAP) 
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPHARV
C=======================================================================



C=======================================================================
C  STNAMES, Subroutine C.H.Porter
C  Assigns STNAME for various CROPGRO crops.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2001 CHP Written
C=======================================================================

      SUBROUTINE STNAMES(CROP, PLME, STNAME)

C-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*1  PLME
      CHARACTER*2  CROP 
      CHARACTER*10 STNAME(20)
      INTEGER I

C-----------------------------------------------------------------------
C     Define names of reproductive phases
C-----------------------------------------------------------------------
      DO I = 1,20
         STNAME(I) = '          '
      ENDDO

      SELECT CASE (CROP)
      CASE ('BN')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('CB')
        STNAME( 1) = 'Emergence '
        STNAME( 2) = 'Unifoliate'
        STNAME( 3) = 'End Juven.'
        STNAME( 4) = '          '
        STNAME( 5) = '          '
        STNAME( 6) = 'First Head'
        STNAME( 7) = 'Full Head '
        STNAME( 8) = '          '
        STNAME( 9) = '          '
        STNAME(10) = 'Phys. Mat '
        STNAME(11) = 'Harv. Mat '
        STNAME(12) = 'End Msnode'
        STNAME(13) = 'End Leaf  '
        STNAME(14) = 'Start Sim '
        STNAME(15) = 'Sowing    '
        STNAME(16) = 'Harvest   '

      CASE ('C3','C4','G0','G1','G2','G3','G4','G5','G6','G7','G8','BR')
         STNAME( 1) = 'Emergence '
         STNAME( 2) = 'First Leaf'
         STNAME( 3) = 'End Juven.'
         STNAME( 4) = 'Flower Ind'
         STNAME( 5) = 'Flowering '
         STNAME(10) = 'Phys. Mat '
         STNAME(11) = 'Harv. Mat '
         STNAME(12) = 'End Msnode'
         STNAME(13) = 'End Leaf  '
         STNAME(14) = 'Start Sim '
         STNAME(15) = 'Sowing    '
         STNAME(16) = 'Harvest   '

      CASE ('CH')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('CO') 
        STNAME( 1) = 'Emergence '
        STNAME( 2) = 'First Leaf'
        STNAME( 3) = 'End Juven.'
        STNAME( 4) = 'Flower Ind'
        STNAME( 5) = 'Flowering '
        STNAME( 6) = 'Boll > 6mm'
        STNAME( 7) = 'End Flower'
        STNAME( 8) = 'First Seed'
        STNAME( 9) = 'Bolls>.5sz'
        STNAME(10) = 'Cracked Bl'
        STNAME(11) = '90%Open Bl'
        STNAME(12) = 'End Msnode'
        STNAME(13) = 'End Leaf  '
        STNAME(14) = 'Start Sim '
        STNAME(15) = 'Sowing    '
        STNAME(16) = 'Harvest   '                

      CASE ('CP')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('CT')
        STNAME( 1) = 'Emergence '
        STNAME( 2) = 'Unifoliate'
        STNAME( 3) = 'End Juven.'
        STNAME( 4) = 'Flower Ind'
        STNAME( 5) = 'Flowering '
        STNAME( 6) = 'First Pod '
        STNAME( 7) = 'Full Pod  '
        STNAME( 8) = 'First Seed'
        STNAME( 9) = 'End Pod   '
        STNAME(10) = 'Phys. Mat '
        STNAME(11) = 'Harv. Mat '
        STNAME(12) = 'End Msnode'
        STNAME(13) = 'End Leaf  '
        STNAME(14) = 'Start Sim '
        STNAME(15) = 'Sowing    '
        STNAME(16) = 'Harvest   '       

      CASE ('FA')
        STNAME(14)= 'Start Sim '
        STNAME(16)= 'End Sim   '

      CASE ('FB','CA')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('PE')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('PN')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Peg '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('PP')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('PR')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'Frst Fruit'
        STNAME(7) = 'Frst Fruit'
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Fruit '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('SB')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('TM')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'Frst Fruit'
        STNAME(7) = 'Frst Fruit'
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Fruit '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      CASE ('VB')
        STNAME(1) = 'Emergence '
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'First Flwr'
        STNAME(6) = 'First Pod '
        STNAME(7) = 'First Pod '
        STNAME(8) = 'First Seed'
        STNAME(9) = 'End Pod   '
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '
!KJB - 5/4/2017 adding stages for sunflower and safflower (maybe CROPGRO wheat later)
      CASE ('SU')
!     For stage-dependant irrigation - send GSTAGE back to irrig routine
        STNAME(1) = 'Emergence '    !; GSTAGE(1) = "GS001"
        STNAME(2) = 'Unifoliate'
        STNAME(3) = 'End Juven.'
        STNAME(4) = 'Flower Ind'
        STNAME(5) = 'Star burst'
        STNAME(6) = '1st thalam'
        STNAME(7) = '1st thalam'
        STNAME(8) = '1st Flw/Sd'
        STNAME(9) = 'End Sd-add'
        STNAME(10)= 'Phys. Mat '
        STNAME(11)= 'Harv. Mat '
        STNAME(12)= 'End Msnode'
        STNAME(13)= 'End Leaf  '
        STNAME(14)= 'Start Sim '
        STNAME(15)= 'Sowing    '
        STNAME(16)= 'Harvest   '

      
      END SELECT

      IF (PLME .EQ. 'T') THEN
        STNAME(15) = 'Transplant'
      ENDIF

      RETURN
      END SUBROUTINE STNAMES
C=======================================================================

