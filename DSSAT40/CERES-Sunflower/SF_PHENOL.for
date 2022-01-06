C======================================================================
C  PHENOL, Subroutine
C
C  Determines Phenological Stage and Growing Degree Days
C----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      2-7-93
C  3. Added switch block, code cleanup            P.W.W.      2-7-93
C  4. Modified TT calculations to reduce line #'s P.W.W.      2-7-93
C  5. Modified for MILLET model                   W.T.B.      MAY 94
C  6. Converted to modular format                 W.D.B       3-29-01
C  7. Major restructuring for 2002 release        W.D.B       12-01-01
C
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C  
C----------------------------------------------------------------------
      SUBROUTINE SF_PHENOL(DYNAMIC,ISWWAT,FILEIO,NOUTDO,IDETO,    !C
     &    CUMDEP,DAYL,DLAYR,IDURP,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,  !I
     &    SI1,SI3,SNOW, SRAD,SUMP,SW,TMAX,TMIN, TWILEN,           !I
     &    XN,YRDOY,YRSIM,DOY,POTHEADWT,SWFAC,TURFAC,P9,              !I
     &    CUMDTT,DTT,EARS,GPP,ISDATE,ISTAGE,MDATE,STGDOY,SUMDTT, !O
     &    XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP,P3,XPEPE,PERN,SENCODE,
     &    SENTIME,SENRATE,XLANC,P3P,RI1,XNGLF,XSTEMN,STOVN,XANC,TOTNUP,
     &    XLEAFN,EMBN,GROEMB,GROPER,POTGROPER,GRFACTOR,HWMAX,HWMIN,
     &    LAI,PLA,GPLA,LFWT,GLFWT,RTWT,STMWT,STOVWT,BIOMAS,PERWT,EMBWT,
     &    TMNC,ROOTN,HEADWT,SLAN1,SLAN2,SLOPEPE,OIL,PPP,GROHEAD,GRNWTE,
     &    SLFWT,SPLA,XLN,XCUMPH,SGRO,APLA,XSANC,XHANC,XNSLF,GPSM,PTF,
     &    SENLA,GRNWT,SWMAX,SWMIN,GROSTM,GROLF,PLAMX)

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE
C----------------------------------------------------------------------
C                             Define Variables
C----------------------------------------------------------------------
      INTEGER         DYNAMIC         

      REAL            ABSTRES         
      REAL            ACOEF           
      REAL            BARFAC 
      CHARACTER*1     BLANK         
      REAL            C1              
      REAL            CUMDEP          
      REAL            CUMDTT          
      REAL            DAYL            
      REAL            DEC             
      REAL            DGET
      REAL            DJTI
      REAL            DLAYR(NL)       
      REAL            DLV             
      REAL            DOPT                      
      REAL            DSGT
      REAL            DSGFT
      REAL            DTT             
      REAL            DUMMY           
      REAL            EARS            
      CHARACTER*6     ECONO           
      INTEGER         ERR             
      CHARACTER*6     ERRKEY          
      PARAMETER       (ERRKEY='SFPHEN')
      INTEGER         ERRNUM
      CHARACTER*12    FILEC     
      CHARACTER*12    FILES
      CHARACTER*12    FILEE     
      CHARACTER*255    FILEGC
      CHARACTER*30    FILEIO         
      INTEGER         FOUND          
      REAL            G2             
      REAL            G3             
      REAL            GDDE
      REAL            GPP            
      INTEGER         I              
      CHARACTER*1     IDETO          
      INTEGER         IDURP          
      INTEGER         ISTAGE         
      CHARACTER*1     ISWWAT         
      REAL            KCAN
      REAL            KEP
      INTEGER         LEAFNO         
      INTEGER         L              
      INTEGER         L0             
      INTEGER         LINC           
      REAL            LL(NL)         
      INTEGER         LNUM           
      INTEGER         LUNIO          
      INTEGER         MDATE          
      INTEGER         NDAS           
      INTEGER         NLAYR          
      INTEGER         NOUTDO         
      REAL            P1             
      REAL            P2             
      REAL            P2O            
      REAL            P3             
      REAL            P5             
      REAL            P9             
      CHARACTER*255    PATHCR 
      CHARACTER*255    PATHSR
      CHARACTER*255    PATHER        
      REAL            PDTT
      REAL            PHINT          
      REAL            PLTPOP         
      REAL            PSKER          
      REAL            RATEIN         
      REAL            ROPT           
      REAL            ROWSPC         
      REAL            RUE
      REAL            SDEPTH         
      CHARACTER*6     SECTION        
      REAL            S1             
      REAL            SI1(6)         
      REAL            SI3(6)         
      REAL            SIND           
      REAL            SNDN           
      REAL            SNOW           
      REAL            SNUP           
      REAL            SRAD           
      INTEGER         STGDOY(20)     
      REAL            SUMDTT         
      REAL            SUMP           
      REAL            SW(NL)         
      REAL            SWCG
      REAL            SWSD           
      REAL            TBASE          
      REAL            TDSOIL         
      REAL            TEMPCN         
                                     
      REAL            TEMPCR         
      REAL            TEMPCX         
      REAL            TH             
      REAL            TLNO           
      REAL            TMAX           
      REAL            TMIN           
      REAL            TMSOIL         
      REAL            TNSOIL         
      REAL            TOPT           
      REAL            TWILEN         
      CHARACTER*6     VARNO          
      CHARACTER*16    VRNAME                  
      REAL            XN             
      REAL            XNTI           
      REAL            XS  
      REAL            O1
      REAL            XSTAGE, tdsoil1,tnsoil1,tmsoil1         
      INTEGER         YRDOY          
      INTEGER         YREMRG         
      INTEGER         YRSIM
      INTEGER         ISDATE         

      INTEGER PATHL,IPATH
      PARAMETER (BLANK = ' ')
      INTEGER LUNECO,DOY,ICSDUR

      CHARACTER*6 ECOTYP
      INTEGER ISECT
      CHARACTER*255 C255
      CHARACTER*16  ECONAM
      INTEGER LUNCRP
      CHARACTER*255 FILECC
      CHARACTER*80 C80
      CHARACTER*78 MESSAGE(10)
      CHARACTER*1 ISWNIT
      REAL TANC,XANC,STOVN,APTNUP,TEMPM,TTMP,TMFAC1(8),GRNWT,RTDEP,HRLT
      REAL PHOTOP,ZSIND,P3P,PLAMX,APLA,LAI,ABIOMS,BIOMAS,RTWT,RM,RI1
      REAL PPP,PLA,HEADWT,ZZZ,POTHEADWT,GRFACTOR,PERWT,ALF,ALF1
      REAL PERWTE,EMBWTE,EMBWT,GRNWTE,GRAINN,GRAINN1,GRAINNE,PERN
      REAL EMBN,GPSM,YIELD,SKERWT,PGRNWT,STOVER,XGNP,GNUP,XPTN,TOTNUP
      REAL CSD1,SI2(6),CSD2,CNSD1,CNSD2,SI4(6)
      REAL POTGROPER,PS,MAXLAI,XPEPE,SGRO(12),SENRATE,SENTIME,SENCODE
      REAL GPLA,SLFWT,XNGLF,XLEAFN,SPLA,XNSLF,GLFWT,SWMIN,SUNDTT,SWMAX
      REAL STMWT,PTF,XRAT,YRAT,HWMAX,OIL,SLOPEPE,AGEFAC,HWMIN,NSTRES
      REAL STOVWT,LFWT,GROSTM,GROLF,GROEMB,GROPER,GROHEAD,SLAN1,CUMPHTOT
      REAL SLAN2,SLAN,SLAI,GRORT,SENLA,VMNC,VANC,TMNC,NDEF3,CUMPH
      REAL XCUMPH,XLN,RLV(20),RANC,XSTEMN,XHANC,XLANC,ROOTN,XSANC
      REAL PLAG,SWFAC,TURFAC
      INTEGER ICOLD
      COMMON/SOILTEMP/TDSOIL,TMSOIL, TNSOIL
C LIWANG MA, RZWQM-DSSAT
      TYPE (ControlType) CONTROL
      TYPE (PLANTVARType) PLANTVAR
C END OF MODIFICATION
C----------------------------------------------------------------------
C         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
C ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN
          CUMDTT = 0.0
          SUMDTT = 0.0
          DTT = 0.0
          GPP = 0.0
          ISTAGE = 7
          XSTAGE = 0.1
          MDATE      = -99
          DUMMY = 0
          DO I=1,20
              STGDOY(I) = 9999999      
          ENDDO
          STGDOY(14) = YRSIM
          YREMRG = 9999999

          !-------------------------------------------------------
          !     Read input file name (ie. DSSAT40.INP) and path
          !-------------------------------------------------------
C  MODIFED BY LIWANG MA, RZWQM-DSSAT
          CALL GETPUT_CONTROL('GET',CONTROL)
          FILEC = CONTROL % FILEC
          FILEE = CONTROL % FILEE
          FILES = CONTROL % FILES
          PATHSR = CONTROL % PATHSR
          PATHCR = CONTROL % PATHCR
          PATHER = CONTROL % PATHER

          CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          VARNO = PLANTVAR% VARNO
          VRNAME = PLANTVAR% VRNAME
          ECONO = PLANTVAR% ECONO
          P1 = PLANTVAR% P1
          P2 = PLANTVAR% P2
          P5 = PLANTVAR% P5
          G2 = PLANTVAR% G2
          G3 = PLANTVAR% G3
          O1 = PLANTVAR% O1
	    PLTPOP = PLANTVAR% PLTPOP
          ROWSPC = PLANTVAR% ROWSPC
          SDEPTH = PLANTVAR% SDEPTH

C          CALL GETLUN('FILEIO', LUNIO)
C          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
C
C          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
C   50     FORMAT(//////,15X,A12,1X,A80)
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
C          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C   51     FORMAT(15X,A12,1X,A80)
C
C          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
          !------------------------------------------------------
          !   Read Planting Details Section
          !------------------------------------------------------
C          SECTION = '*PLANT'
C          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C          IF (FOUND .EQ. 0) THEN
C            CALL ERROR(SECTION, 42, FILEIO, LNUM)
C          ELSE
C            READ(LUNIO,60,IOSTAT=ERR) PLTPOP,ROWSPC,SDEPTH
C            LNUM = LNUM + 1
C 60         FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
C            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C          ENDIF
C     -----------------------------------------------------------------
C             Read crop cultivar coefficients
C     -----------------------------------------------------------------
C          SECTION = '*CULTI'
C          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C          IF (FOUND .EQ. 0) THEN
C            CALL ERROR(SECTION, 42, FILEIO, LNUM)
C          ELSE
C            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
C     %                   P1,P2,P5,G2,G3,PHINT ; LNUM = LNUM + 1 
C            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
!CHP 1800        FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))    
C1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
C          ENDIF
C          CLOSE(LUNIO)
C END OF MODIFICAITON
C     -----------------------------------------------------------------
C              Read Species Coefficients
C     -----------------------------------------------------------------
      PATHL  = LEN_TRIM(PATHSR)

      IF (PATHL .LE. 1) THEN
         FILECC = FILES
       ELSE
C-KEN         FILEGG = PATHGE(1:(PATHL-1)) // FILEG
         FILECC = PATHSR(1:(PATHL)) // FILES
      ENDIF

c      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      !----------------------------------------------------------------
      !       Find and Read TEMPERATURE Section
      !----------------------------------------------------------------

      SECTION = '*SEED '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F7.3)',IOSTAT=ERR) DSGT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F7.3)',IOSTAT=ERR) DGET
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F7.3)',IOSTAT=ERR) SWCG
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE(LUNCRP)
 

C-----------------------------------------------------------------------
C     Open Ecotype File FILEE
C-----------------------------------------------------------------------
        LNUM = 0
C LIWANG MA        PATHL  = INDEX(PATHER,BLANK)
        PATHL  = IPATH(PATHER)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHER(1:(PATHL-1)) // FILEE
        ENDIF

C-----------------------------------------------------------------------
C    Read Ecotype Parameter File
C-----------------------------------------------------------------------
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
        ECOTYP = '      '
        LNUM = 0
        DO WHILE (ECOTYP .NE. ECONO)
  
          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
          IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &          C255(1:1) .NE. '*') THEN
             READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &            ROPT,P2O,DJTI,GDDE,DSGFT,RUE, KCAN
3100         FORMAT (A6,1X,A16,1X,9(1X,F5.1))
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
          ELSEIF (ISECT .EQ. 0) THEN
            CALL ERROR(ERRKEY,7,FILEE,LNUM)
          ENDIF
        ENDDO

        CLOSE (LUNECO)
        
        KEP = KCAN/(1-0.07)*(1-0.25)
      CNSD1  = 0.0
      CNSD2  = 0.0
      CSD1   = 0.0
      CSD2   = 0.0
      ICSDUR = 1

      ISDATE = 0
      TNSOIL = 0.0
      TMSOIL = 0.0
      TH = 00.0
      TEMPCX = 0.
      TEMPCR = 0.0
      TDSOIL = 0.0
      SWSD = 0.0
      SNUP = 0.0
      SNDN = 0.0
      S1 = 0.0
      RATEIN = 0.0
      PSKER = 0.0
      PDTT = 0.0
      P9 = 0.0
      P3 = 0.0
      NDAS = 0.0
      L0 = 0.0
      L = 0
      DLV = 0.0
      DEC = 0.0
      C1 = 0.0
      ACOEF = 0.0
      DOPT = 0.0



C----------------------------------------------------------------------
C         DYNAMIC = RATE OR INTEGRATE
C ---------------------------------------------------------------------

      ELSE    

C         -------------------------------------------------------------
C             Compute Crown Temperature under snow pack.
C             Used in COLD.FOR
C         -------------------------------------------------------------
          ! TEMPCN = crown temperature when snow is present and 
          !   TMIN < 0. This function computes crown temperature 
          !   as higher than TMIN, C.
          ! TEMPCX = crown temp. for max. development rate, C
          ! SNOW  = Snow depth, mm
          ! XS    = temporary snow depth variable, mm

      XANC   = TANC*100.0
      APTNUP = STOVN*10.0*PLTPOP
C         -------------------------------------------------------------
C             Compute Crown Temperature under snow pack.
C             Used in COLD.FOR
C         -------------------------------------------------------------
          ! TEMPCN = crown temperature when snow is present and 
          !   TMIN < 0. This function computes crown temperature 
          !   as higher than TMIN, C.
          ! TEMPCX = crown temp. for max. development rate, C
          ! SNOW  = Snow depth, mm
          ! XS    = temporary snow depth variable, mm

          TEMPCN = TMIN
          TEMPCX = TMAX
          XS     = SNOW
          XS     = AMIN1 (XS,15.0)
          !------------------------------------------------------------
          ! Calculate crown temperature based on temperature and
          ! snow cover. Crown temperature is higher than TAVG due
          ! to energy balance of snow pack.
          !------------------------------------------------------------
          IF (TMIN .LT. 0.0) THEN
              TEMPCN = 2.0 + TMIN*(0.4+0.0018*(XS-15.0)**2)
          ENDIF
          IF (TMAX .LT. 0.0) THEN
              TEMPCX = 2.0 + TMAX*(0.4+0.0018*(XS-15.0)**2)
          ENDIF
          TEMPCR = (TEMPCX + TEMPCN)/2.0
  
  
          !------------------------------------------------------------
          ! Compute thermal time based on new method developed by J.T.R
          ! at CYMMIT, 5/5/98.  TBASE, TOPT, and ROPT are read in 
          ! from the species file.
          !------------------------------------------------------------
          
          !   DOPT, Devlopment optimum temperature, is set to TOPT 
          !   during vegetative growth and to ROPT after anthesis
          
          DOPT = TOPT
          IF ((ISTAGE .GT. 3) .AND. (ISTAGE .LE. 6)) THEN
              DOPT = ROPT
          ENDIF

          !   Check basic temperature ranges and calculate DTT for
          !   development based on PC with JTR

          IF (TMAX .LT. TBASE) THEN
              DTT = 0.0
          ELSEIF (TMIN .GT. DOPT) THEN
          !   !
       !This statement replaces DTT = TOPT .. GoL and LAH, CIMMYT, 1999
          !   !
              DTT = DOPT - TBASE
          !   !
          !Now, modify TEMPCN, TEMPCX based on soil conditions or snow
          !   ! If wheat and barley is before terminal spiklett stage
          !   ! Or if corn and sorghum are before 10 leaves
          !   !
          ELSEIF (LEAFNO.LE.10) THEN  
          !Check for snow  (should following be GT.0 or GT.15 ?).  
          !   !Based on snow cover, calculate DTT for the day
          !   !
              IF (XS .GT. 0.0) THEN
          !       !
          !       ! Snow on the ground
          !       !
                  DTT    = (TEMPCN + TEMPCX)/2.0 - TBASE
              ELSE
          !       !
          !       ! No snow, compute soil temperature
          !       !
                  ACOEF  = 0.01061 * SRAD + 0.5902
                  TDSOIL = ACOEF * TMAX + (1.0 - ACOEF) * TMIN  !use RZWQM simulated soil temperature
                  TNSOIL = 0.36354 * TMAX + 0.63646 * TMIN      !use RZWQM simulated soil temperature
                  IF (TDSOIL .LT. TBASE) THEN
                      DTT = 0.0
                  ELSE
                      IF (TNSOIL .LT. TBASE) THEN
                          TNSOIL = TBASE
                      ENDIF
                      IF (TDSOIL .GT. DOPT) THEN
                          TDSOIL = DOPT
                      ENDIF
                      !Import DAYL from WEATHR module. chp 5-6-02
                      !CALL DAYLEN (DOY,XLAT,DAYL,DEC,SNDN,SNUP)
               TMSOIL = TDSOIL * (DAYL/24.) + TNSOIL * ((24.-DAYL)/24.)            !use RZWQM simulated soil temperature         
C                      write (100,'(9f6.2,3x)') tdsoil,tnsoil,tmsoil,
c     &                     tdsoil1,tnsoil1,tmsoil1  !,srad,tmax,tmin
                      IF (TMSOIL .LT. TBASE) THEN
                          DTT = (TBASE+TDSOIL)/2.0 - TBASE
                      ELSE
                          DTT = (TNSOIL+TDSOIL)/2.0 - TBASE
                      ENDIF
          !           !
          !           ! Statement added ... GoL and LAH, CIMMYT, 1999
          !           !
                      DTT = AMIN1 (DTT,DOPT-TBASE)
                  ENDIF
              ENDIF
          !
          ! Now, compute DTT for when Tmax or Tmin out of range
          !
          ELSEIF (TMIN .LT. TBASE .OR. TMAX .GT. DOPT) THEN
              DTT = 0.0
              DO I = 1, 24
                  TH = (TMAX+TMIN)/2. + (TMAX-TMIN)/2. * SIN(3.14/12.*I)
                  IF (TH .LT. TBASE) THEN
                      TH = TBASE
                  ENDIF
                  IF (TH .GT. DOPT) THEN
                      TH = DOPT
                  ENDIF
                  DTT = DTT + (TH-TBASE)/24.0
              END DO
          ELSE
              DTT = (TMAX+TMIN)/2.0 - TBASE
          ENDIF

          DTT   = AMAX1 (DTT,0.0)
          SUMDTT  = SUMDTT  + DTT 
          CUMDTT = CUMDTT + DTT
      
      
      TEMPM  = (TMAX + TMIN)/2.0
cc      DTT    = TEMPM - TBASE

cc     IF (TMIN .LE. TBASE .OR. TMAX .GE. 28.0) THEN
cc         IF (TMAX .LT. TBASE) THEN
cc            DTT = 0.0
cc         ENDIF
cc         IF (DTT .NE. 0.0) THEN
cc            DTT = 0.0
cc            DO I = 1, 8
c         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
c               TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
c               IF (TTMP .GT. TBASE .AND. TTMP .LE. 28.0) THEN
c                  DTT = DTT + (TTMP-TBASE)/8.0
c               ENDIF
c               IF (TTMP .GT. 28.0 .AND. TTMP .LT. 40.0) THEN
c                  DTT = DTT + (28.0-TBASE)*(1.-.007*(TTMP-28.0)**2.)/8.0
c              ENDIF
c            END DO
c         ENDIF
c      ENDIF

cc      DTT   = AMAX1 (DTT,0.0)
cc      SUMDTT  = SUMDTT  + DTT
cc      CUMDTT  = CUMDTT  + DTT
cc      CSD1   = CSD1 + 1.0 - SWFAC
cc      CSD2   = CSD2 + 1.0 - TURFAC
cc      CNSD1 = CNSD1 + 1.0-NSTRES
cc      CNSD2 = CNSD2 + 1.0-AGEFAC
cc      ICSDUR = ICSDUR + 1

      !  ีอออออออออออออออออออออออออออออออออออออธ
      !  ณ7 - Sowing date                      ณ
      !  ณ8 - Germination                      ณ
      !  ณ9 - Emergence                        ณ
      !  ณ1 - End juvenile                     ณ
      !  ณ2 - Pannicle initiation              ณ
      !  ณ3 - End leaf growth                  ณ
      !  ณ4 - End pannicle growth              ณ
      !  ณ5 - Grain fill                       ณ
      !  ณ6 - Maturity                         ณ
      !  ิอออออออออออออออออออออออออออออออออออออพ
C   COMMON TO ALL ISTAGES
          CNSD1=0.0
          CNSD2=0.0
          CSD1=0.0
          CSD2=0.0
          ICSDUR=1
          
          IF (ISTAGE .EQ. 7) THEN
          !
          ! Stage 7 >> Determine sowing date <<
          !
          STGDOY(ISTAGE) = YRDOY
          NDAS           = 0
          ISTAGE        = 8
          SUMDTT   = 0.0
          RTDEP = SDEPTH
          IF (ISWWAT .EQ. 'N') RETURN
          CUMDEP         = 0.0

          DO L = 1, NLAYR
             CUMDEP = CUMDEP + DLAYR(L)
             IF (SDEPTH .LT. CUMDEP) GOTO 100
          END DO
100       CONTINUE
          L0 = L
          RETURN
          ELSEIF (ISTAGE .EQ. 8) THEN
          !
          ! Stage 8 >> Determine germination date <<
          !
          IF (ISWWAT .NE. 'N') THEN
             IF (SW(L0) .LE. LL(L0)) THEN
                SWSD = (SW(L0)-LL(L0))*0.65 + (SW(L0+1)-LL(L0+1))*0.35
                NDAS = NDAS + 1

                IF (NDAS .GE. DSGT) THEN
                   ISTAGE = 6
                   PLTPOP = 0.0
                   GPP    = 1.0
                   GRNWT  = 0.0
                   WRITE (*,3600)
                   IF (IDETO .EQ. 'Y') THEN
                      WRITE (NOUTDO,3600)
                   ENDIF
                   MDATE = YRDOY
                   RETURN
                 ELSEIF (SWSD .LT. SWCG) THEN
                  RETURN
                ENDIF
             ENDIF
          ENDIF

          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 9
          P9     = 66.0 + 11.9*SDEPTH                ! SUN
          CUMDTT = 0.0
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3  = 1.0
          SUMDTT = 0.0
          TBASE  = 4.0                               ! SUN
          RETURN
          ELSEIF (ISTAGE .EQ. 9) THEN
          !
          ! Stage 9 >> Determine seedling emergence date <<
          !
C          NDAS = NDAS + 1   !DO WE NEED THIS? LIWANG MA
          RTDEP  = RTDEP + 0.15*DTT
          IF (SUMDTT .LT. P9) RETURN
          IF (P9 .GT. DGET) THEN
             ISTAGE = 6
             PLTPOP = 0.00
             GPP    = 1.0
             GRNWT  = 0.0
             WRITE (*,1399)
             IF (IDETO .EQ. 'Y') THEN
                WRITE (NOUTDO,1399)
             ENDIF
             MDATE = YRDOY
             RETURN
          ENDIF
          STGDOY(ISTAGE) = YRDOY
          YREMRG = STGDOY(9)
          ISTAGE   = 1
          P3       = 400.0
          SUMDTT   = SUMDTT - P9
          PLA      = 3.0
          GPLA     = 3.0
          PLAG     = 0.0
          LAI      = PLTPOP*PLA*0.0001
          LFWT     = 0.03
          GLFWT    = 0.03
          SLFWT    = 0.0
          RTWT     = 0.02
          STMWT    = 0.01
          HEADWT   = 0.0
          PERWT    = 0.0
          EMBWT    = 0.0
          STOVWT   = 0.04
          BIOMAS   = STOVWT
          GROSTM   = 0.0
          GROLF    = 0.0
          GROHEAD  = 0.0
          GROPER   = 0.0
          GROEMB   = 0.0
          GRNWT    = 0.0
          GRNWTE   = 0.0
          OIL      = 0.0
          PPP      = 0.0
          SENLA    = 0.0
          SLAN     = 0.0
          SLAI     = 0.0
          SLAN1    = 0.0
          SLAN2    = 0.0
          GRORT    = 0.0
          !
          ! First estimate of final leaf number
          !
          TLNO     = IFIX (2.0+(P1+30.0+30.0*P2)/14.0)
          CUMPHTOT = 0
          CUMPH    = 0
          LEAFNO   = 1
          XLN      = 1.0
          XCUMPH   = 0.0
          ICOLD    = 0
          TBASE    = 4.0                             ! SUN
          CUMDEP   = 0.0
          RETURN
          ELSEIF (ISTAGE .EQ. 1) THEN
          !
          ! Stage 1 >> Determine end of juvenile stage <<
          !
C          NDAS   = NDAS + 1   !NDAS - number of days after sowing, DO WE NEED THIS? LIWANG MA
          XSTAGE = SUMDTT/P1
          IF (SUMDTT .LT. P1) RETURN
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 2
          SIND   = 0.0
          ELSEIF (ISTAGE .EQ. 2) THEN
          !
C              NDAS   = NDAS + 1       DO WE NEED THIS? LIWANG MA
          ! Stage 2 >> Determine date of panicle initiation <<
          !
          !HRLT   = PHOTOP (DOY,S1,C1)
          !HRLT   = AMIN1  (HRLT,15.0)
              IF (TWILEN .GT. P2O) THEN
                RATEIN = 1.0/(DJTI+P2*(TWILEN-P2O))  
              ELSE
                RATEIN = 1.0 / DJTI
              ENDIF
c          RATEIN = 1.0/(3.0 + P2 * (15.0 - TWILEN))    ! SUN
          SIND   = SIND + RATEIN   ! MAX(0.0,RATEIN)   !for some reason RATEIN becomes negative? should it be? Liwang
          ZSIND  = AMIN1 (SIND,1.0)
          XSTAGE = 1.0 + 0.5 * ZSIND
          IF (SIND .LT. 1.0) RETURN
          !HRLT = PHOTOP (DOY,S1,C1)
          P3P  = 2.0*P1
          !
          ! P3P will define the start of anthesis
          !
          STGDOY(ISTAGE) = YRDOY
          ISTAGE  = 3
          XNTI   = SUMDTT/43.0
          SENCODE = 0.0
          SENTIME = 0.0
          SENRATE = 0.0
          DO I = 1, 12
            SGRO(I) = 0.0
          END DO
          GPLA    = PLA
          SPLA    = 0.0
          SLFWT   = 0.0
          XNSLF   = 0.0
          GLFWT   = LFWT
          XNGLF   = XLEAFN
          HEADWT  = 0.0                              ! SUN, assum.
          TLNO    = IFIX(SUMDTT/14.0+2.0)            ! SUN
          P3      = 625.0                            ! SUN
          SUMDTT  = 0.0
          SUMP    = 0.0                              ! SUN, compute before anthesis
          IDURP   = 0                                ! SUN, idem
          ELSEIF (ISTAGE .EQ. 3) THEN
          !
C              NDAS   = NDAS + 1       DO WE NEED THIS? LIWANG MA     
          ! Stage 3 >> Determine end of leaf growth <<
          !
          XSTAGE = 1.5 + 3.0 * SUMDTT / P3P 
          IF (SUMDTT .LT. P3P) RETURN
          ISDATE = YRDOY
          MAXLAI = PLAMX*PLTPOP/10000.0
          APLA   = LAI*10000.0/PLTPOP
          ABIOMS = BIOMAS
          PSKER  = SUMP / IDURP
          PSKER  = AMAX1 (PSKER,0.1)                 ! Set PSKER to min of 0.1
          !
          ! Include calculations to correct RUE after anthesis
          ! mantainance respiration
          !
          RM  = (BIOMAS+RTWT*PLTPOP)*0.008*0.729
          RI1 = 1.0 + RM/PSKER/PLTPOP       
C         IF (PSKER .GT. 5.0) THEN
C            PPP = 430.0 + G2 * PSKER / 14.5         ! Pericarp number, HV
C          ELSE
C            PPP = PSKER * (430.0+G2*5.0/14.5) / 5.0
C         ENDIF
          !
          ! Pericarp number calculated according to Soriano
          ! FV - 12/12/97
          !
          PPP = 1250.0 + (PLA-750.0)*750.0/1500.0
          PPP = AMIN1 (PPP,G2)

          ZZZ       = AMIN1 (1.0,HEADWT/POTHEADWT)
          GRFACTOR  = 0.6 + 0.4 * ZZZ
          GRFACTOR  = AMIN1 (GRFACTOR,1.0)
          PERWT     = PPP * 0.002                     ! Pericarp starts with 2 mg
          HEADWT    = HEADWT - PERWT
          ALF       = 0.22
          ALF1      = (ALF*G3/24.*(P5-170.)-2.*(1.-ALF))/270./(1.-ALF)
          POTGROPER = 24.0*ALF1*PPP/1000.0
          STGDOY(ISTAGE) = YRDOY
          ISTAGE  = 4
          SUMP    = 0.0
          IDURP   = 0
          SWMIN   = 0.99 * STMWT
          SWMAX   = 1.30 * STMWT
          PTF     = 1.0
          SUMDTT  = SUMDTT - P3P                       ! SUN
          XRAT    = XNGLF / GLFWT
          YRAT    = (0.009-0.0875*XRAT)/0.9125
          SLOPEPE = LAI*10E3/PLTPOP/(XNGLF-YRAT*GLFWT) ! SUN, HV
          XPEPE   = 0.50 * (XNGLF - YRAT*GLFWT)
C
          ELSEIF (ISTAGE .EQ. 4) THEN
C              NDAS = NDAS + 1  DO WE NEED THIS? LIWANG MA
          !
          ! Stage 4 >> Determine end of panicle growth <<
          !
          XSTAGE = 4.5 + 5.5 * SUMDTT / (P5 * 0.95)
          IF (SUMDTT .LT. DSGFT) RETURN
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 5
          OIL    = GRNWT * 0.025
          GPP    = 0.0
          HWMAX  = 2.20 * HEADWT
          HWMIN  = 1.00 * HEADWT
          VANC   = TANC
          VMNC   = TMNC
          EARS  = PLTPOP
      ELSEIF (ISTAGE .EQ. 5) THEN
          !
C          NDAS = NDAS + 1 DO WE NEED THIS? LIWANG MA
          ! Stage 5 >> Determine end of effective filling period for maize <<
          !
          XSTAGE = 4.5 + 5.5 * SUMDTT/P5
          IF (SUMDTT .GE. 230.0 .AND. GPP .EQ. 0.0) THEN
             !
             ! Include calculations to correct RUE after anthesis
             ! mantainence respiration
             !
             PS    = RM/(RI1-1.0)
             RM    = (BIOMAS+RTWT*PLTPOP)*0.008*0.729
             RI1   = 1.0 + RM/PS
C             PSKER = SUMP / IDURP                !NOT NEEDED, LIWANG MA
C            IF (PSKER .GT. 5.0) THEN
C               GPP = 430.0 + G2 * PSKER / 14.5      ! Embryo number, HV
C             ELSE
C               GPP = PSKER * (430.0+G2*5.0/14.5)/5.0
C            ENDIF
             !
             ! Grain number calculated according to Soriano
             ! FV - 12/12/97
             !
             GPP = 500.0 + PLA*750.0/6000.0
             GPP = AMIN1 (GPP,PPP)
             IF (GPP .GT. 0.0) THEN
                PERWTE  = (PPP - GPP) * PERWT / PPP
                EMBWTE  = (PPP - GPP) * EMBWT / PPP
                GRNWTE  = PERWTE + EMBWTE
                IF (ISWNIT .EQ. 'Y') THEN
                   GRAINN1 = GRAINN * (1.0 - GRNWTE / GRNWT)
                   GRAINNE = GRAINN - GRAINN1
                   PERN    = PERN   * GRAINN1 / GRAINN
                   EMBN    = EMBN   * GRAINN1 / GRAINN
                   GRAINN  = GRAINN1
                ENDIF
                GRNWT   = GRNWT  - GRNWTE
                PERWT   = PERWT  - PERWTE
                EMBWT   = EMBWT  - EMBWTE
                GPSM    = GPP    * PLTPOP
              ELSE
                PERWTE  = PERWT
                EMBWTE  = EMBWT
                GRNWTE  = PERWTE + EMBWTE
                GRAINN1 = 0.0
                GRAINNE = GRAINN
                EMBN    = 0.0
                GRNWT   = 0.0
                EMBWT   = 0.0
                GPSM    = 0.0
             ENDIF
          ENDIF
          IF (SUMDTT .LT. (P5 * 0.95)) RETURN
          STGDOY(ISTAGE) = YRDOY
          ISTAGE = 6
          ELSEIF (ISTAGE .EQ. 6) THEN
          !
          ! Stage 6 >> Determine physiological maturity <<
          !
          IF (DTT .LT. 2.0) THEN
             SUMDTT = P5
          ENDIF
          IF (SUMDTT .LT. P5)  RETURN

          STGDOY (ISTAGE) = YRDOY
          MDATE = YRDOY
          YIELD = GRNWT * 10.0 * PLTPOP

          IF (PLTPOP .NE. 0.0) THEN
             IF (GPP .GT. 0.0) THEN
                SKERWT = GRNWT / GPP
                GPP = 1.0
             ENDIF
             PGRNWT = SKERWT * 1000.0
             GPSM   = GPP    * PLTPOP
             YIELD  = YIELD  / 0.91                  ! SUN, 9% moisture
             STOVER = BIOMAS * 10.0 - YIELD
             IF (ISWNIT .EQ. 'N') THEN
                XGNP = 0.0
                GNUP = 0.0
              ELSE
                IF (GRNWT .GT. 0.0) THEN
                   XGNP = (GRAINN / GRNWT) * 100.0
                   XPTN = XGNP   * 6.25
                   GNUP = GRAINN * PLTPOP  *  10.0
                ENDIF
                TOTNUP = GNUP + APTNUP

                SI1(ISTAGE) = 0.0
                SI2(ISTAGE) = 0.0
                SI3(ISTAGE) = 0.0
                SI4(ISTAGE) = 0.0
             ENDIF
          ENDIF
          ISTAGE = 10
          CUMDTT = 0.0
          DTT = 0.0
      END IF

cc      IF (ISWWAT .NE. 'N'.AND.ISTAGE.GT.0) THEN
C         SI1(ISTAGE) = CSD1  / ICSDUR
C         SI2(ISTAGE) = CSD2  / ICSDUR
C         SI3(ISTAGE) = CNSD1 / ICSDUR
C         SI4(ISTAGE) = CNSD2 / ICSDUR
cc      ENDIF
      IF (ISWNIT .NE. 'N') THEN
c         RANC   = 0.0466
         RANC   = 0.0220  !to reduce root N
c         XLANC  = 0.0620
c         XSANC  = 0.0444
c         XHANC  = 0.0420
         XLANC  = 0.0420
         XSANC  = 0.0244
         XHANC  = 0.0220
         TANC   = 0.025
c         TANC   = 0.0576
         XLEAFN = XLANC  * LFWT
         XNGLF  = XLEAFN
         XNSLF  = 0.0000
         XSTEMN = XSANC  * STMWT
         ROOTN  = RANC   * RTWT
         STOVN  = STOVWT * TANC
C        GRAINN = 0.0000
      ENDIF

         RETURN
      ENDIF

C      ENDIF
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

1399  FORMAT (10X,'Seed ran out of metabolite due to deep planting')
3500  FORMAT ('Crop failure because of lack of germination ',
     &           'within 15 days of sowing')
3600  FORMAT (1X,'Crop failure because of lack of germination ',
     1           'within 40 days of sowing')

      END SUBROUTINE SF_PHENOL

! DYNAMIC    Modular control
! ABSTRES    Maximum of water stress stage 1 and 3
! ACOEF      Coefficient
! BARFAC     Factor to reduce ears/plant under high populations (barrenn
! C1         Used to comptue daylength (computed in maize.for)
! CUMDEP     Cumulative depth of soil, cm
! CUMDTT     Cumulative daily thermal time after germination, C
! DAYL       Daylength, hours
! DEC        Used to compute daylength
! DGET       Threshold defined as growing degree days between germination and emergence.
!            If this threshold is exceeded, crop failure ocurrs.
! DJTI       Minimum days from end of juvenile stage to tassel initiation if the cultivar
!            is not photoperiod sensitive, DJTI
! DLAYR(L)   Soil thickness in layer L (cm)
! DLV        Used to compute daylength
! DOPT       Development optimum temperature
! DOY        Julian day of year
! DSGFT      GDD from silking to effective grain filling period, C
! DSGT       Maximum number of days from sowing to germination before crop failure occurs.
! DTT        Growing degree days today, C
! DUMMY      Temporary variable
! EARS       Ears per m2, computed here and used in grosub.
! ECONO      Ecotype number for the variety (not really used in maize ye
! ERR        Determines if error in reading file (0=ok, 1=error)
! ERRKEY     Variable containing routine where error occurred
! (ERRKEY='SF_PHENL')
! FILEC      Filename of .SPE or species file
! FILEIO     Filename containing model inputs (IBSNAT35.INP)
! FOUND      Indicates if a section in a file is found
! G2         Potential kernel number, kernels/plant
! G3         Potential kernel growth rate mg/kernel/day
! GDDE       Growing degree days per cm seed depth required for emergence, GDD/cm
! GPP        Grain number per plant, grains/plant
! I          Loop counter
! IDETO      Screen output switch (Y/N)
! IDURP      Duration of ISTAGE 4, calendar days
! ISTAGE     Growth stage
! ISWWAT     Water balance switch (Y/N)
! LEAFNO     Number of oldest leaf per plant (same as XN)
! L          Loop counter
! L0         Temporary soil layer number
! LINC       Indicates if a line is a good line
! LL(NL)     Soil water lower limit, cm3/cm3
! LNUM       Line number in an input file
! LUNIO      Logical input number for model input file
! LUNIO      Assign value to LUNIO for local use.
! MDATE      Year and day of year of maturity
! NDAS       Number of days after sowing
! NLAYR      Number of soil layers
! NOUTDO     Output file number
! P1         GDD from seedling emergence to end of juvenile phase, C
! P2         Photoperiod sensitivity coefficient, 1/hr
! P2O        Minimum daylength below which daylength does not affect dev
! P3         Cumulative GDD required to complete ISTAGE 3, C
! P5         GDD from silking to physiological maturity, C
! P9         Growing degree days from germination to emergence, C
! PATHCR     Pathname of species file
! DTT
! PHINT      Phyllochron interval. Number of GDD required for new leaf e
! PLTPOP     Plant population, no./m2
! PSKER      Average rate of photosynthesis during ISTAGE 4
! RATEIN     Rate of floral induction
! ROPT       Second optimum temperature for development from species fil
! ROWSPC     Row spacing in cm read in from input file
! SDEPTH     Sowing depth, cm
! SECTION    Temporary variable used to identify section in a file
! S1         Used to compute daylength (computed in maize.for)
! SI1(6)     Water stress during a growth stage used for output
! SI3(6)     Water stress during a growth stage used for output
! SIND       Summed photoperiod induction rate
! SNDN       Sun down
! SNOW       Snow, mm
! SNUP       Sun up
! SRAD       Daily solar radiation, MJ/m2/day
! STGDOY(20) Year and day of year that a growth stage occurred on
! SUMDTT     Sum of GDD for a given stage, C
! SUMP       Cumulative plant growth during ISTAGE 4, g/plant
! SW(NL)     Soil water content in layer, cm3/cm3
! SWCG       Minimum soil water available required for germination to occur, cm3/cm3
! SWSD       Modified soil water content for computing emergence
! TBASE      Base temperature for development from ecotype file, C
! TDSOIL     Weighted average soil temperature, C
! TEMPCN     Crown temperature when snow is present and TMIN < 0. This f
!            computes crown temperature as higher than TMIN, C.
! TEMPCR     Crown temperature, C
! TEMPCX     Crown temperature for maximum development rate, C
! TH         Intermedate variable for computing GDD today, C
! TLNO       Total leaf numbers that will eventually develop
! TMAX       Daily maximum temperature, C
! TMIN       Daily minimum temperature, C
! TMSOIL     Weighted average soil temperature, C
! TNSOIL     Weighted average soil temperture, C
! TOPT       Optimum temperature for development from species file, C
! TWILEN     Twilight definition of daylength
! VARNO      Variety identification number
! VRNAME     Variety name
! WTHADJ(2,8)Note, used here, but not passed into maize.for from cropgro
! WMODB*1    Note, used here, but not passed into maize.for from cropgro
! XLAT       Latitude
! XN         Number of oldest expanding leaf
! XNTI       Number of leaves at tassel initiation (used in grosub)
! XS         Temporary snow depth variable
! XSTAGE     Non-integer growth stage indicator
! YRDOY      Year and day of year
! YREMRG     Year and day of year of emergence (passed back to water bal
! YRSIM      Year and day of year of first day of simulation

