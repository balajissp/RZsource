C======================================================================
C  SF_GROSUB, Subroutine
C
C  Maize growth routine
C----------------------------------------------------------------------
C  Revision history
C
C                 Written
C  02/07/1993 PWW Header revision and minor changes   
C  02/07/1993 PWW Switch block added, etc                     
C  06/21/1994 JTR & BDB Updated PCARB calculation                       
C  03/29/2001 WDB Converted to modular version               
C  12/01/2000 WDB Major restructuring for 2002 release      
C  04/15/2002 WDB Added pest damage and eco and species file 
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C  12/16/2004 CHP Changed conversion for grain weight from PLTPOP to
C                   ears to account for barrenness factor.
C  02/04/2005 CHP Changed conversion for podwt to account for
C                   barrenness factor.
!  05/11/2005 CHP Calculate N loss in senesced leaves for N balance.
!  08/11/2005 JIL Minor revisions to eliminate carbon imbalance
!                 Do not allow increase in leaf number during 
!                   expansion of final leaf.
!  10/04/2005 JIL Set MDATE upon failure due to cold stress or water 
!                   stress.
C----------------------------------------------------------------------
C
C  Called : MAIZE
C
C  Calls  : NFACTO NUPTAK
C----------------------------------------------------------------------

      SUBROUTINE SF_GROSUB (NOUTDO,ISWNIT,ISWWAT,IDETO,DYNAMIC,
     &  STGDOY,YRDOY,DOY,PLTPOP,PHINT,ROWSPC,PORMIN,
     &  P5,G3,SUMDTT,DTT,CUMPH,ISTAGE,
     &  ICSDUR,SWFAC,TURFAC,
     &  LFWT,LAI,GRNWT,XN,TMIN,TMAX,SRAD,CO2,GPSM,
     &  GPP,GRORT,PTF,BIOMAS,PLA,SENLA,RTWT,EARWT,
     &  TLNO,CARBO,STMWT,XNTI,PLAG,EARS,
     &  SUMP,LEAFNO,IDURP,DM,
     &  NSTRES,AGEFAC,GRAINN,ROOTN,STOVN,PDWI,STOVWT,
     &  PGRORT,RCNP,VMNC,GNP,
     &  TANC,TRNU,NLAYR,RLV,NO3,NH4,
     &  UNO3,UNH4,XSTAGE,SW,DUL,LL,SAT,DLAYR,SHF, !CNSD1,CNSD2,
     &  YIELD,SKERWT,CANNAA,CANWAA,XHLAI,EOP,TRWUP,RWUEP1,
     &  WTNCAN,WTNSD,WTNVEG,PCNVEG,WTNUP,
     &  WTNLF,WTNST,MDATE, 
     &  APTNUP, GNUP, MAXLAI, STOVER, TOTNUP, XGNP,XLAI,
     &  SHELPC,SDSIZE,HI,HIP,TOPWT,SLA,STMWTO,RTWO,PODWT,
     &  SDWT,SEEDNO,PODNO,WTLF,RTWTO,PCNL,PCNSD,PCNST,PCNRT,
     &  CANHT,CANWH,VSTAGE,RSTAGE,PCNGRN,SATFAC,   
     $  AREALF,WLIDOT,WSIDOT,WRIDOT,SWIDOT,PPLTD,ASMDOT,FILEIO,
     &  RLWR,SDSZ,RWUMX,BD,RUE,SENESCE,SLPF,P3,XPEPE,PERN,SENCODE,
     &  SENTIME,SENRATE,POTHEADWT,CUMDTT,P9,XLANC,P3P,RI1,
     &  XNGLF,XSTEMN,XANC,XLEAFN,EMBN,POTGROPER,GRFACTOR,
     &  HWMAX,HWMIN,GLFWT,GPLA,PERWT,EMBWT,TMNC,TCNP,HEADWT,SLAN1,SLAN2,
     &  SLOPEPE,OIL,PPP,GROHEAD,GRNWTE,SLFWT,SPLA,XLN,XCUMPH,SGRO,APLA,
     &  XSANC,XHANC,XNSLF,SWMAX,SWMIN,GROSTM,GROLF,PLAMX,yrplt)

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE
C----------------------------------------------------------------------
C                         Variable Declaration
C----------------------------------------------------------------------

      REAL        AGEFAC            
      REAL        APTNUP      
      REAL        AREALF
      REAL        ASMDOT
      REAL        BD(20) 
      REAL        BIOMAS 
      CHARACTER*80 C80
      REAL        CANHT     
      REAL        CANNAA      
      REAL        CANWAA 
      REAL        CANWH     
      REAL        CARBO       
      REAL        CARBOT       
      INTEGER     CMAT
!      REAL        CNSD1       
!      REAL        CNSD2        
      REAL        CUMPH       
      REAL        CO2X(10)    
      REAL        CO2Y(10)    
      REAL        CO2         
!      REAL        CSD1        
!      REAL        CSD2        
      REAL        CURV
      REAL        DLAYR(20)   
      REAL        DM          
      INTEGER     DOY         
      REAL        DTT         
      REAL        DUMMY           
      INTEGER     DYNAMIC     
      REAL        EARS        
      REAL        EARWT       
      CHARACTER*6     ECONO 
      INTEGER     EMAT        
      REAL        EOP
      REAL        EP1
      REAL        pp1,pp2
      INTEGER         ERR 
      CHARACTER*6     ERRKEY          
      PARAMETER       (ERRKEY='SF_GRO')   
      CHARACTER*30    FILEIO 
      CHARACTER*12    FILEC     
      CHARACTER*255    FILECC
      CHARACTER*12    FILES
      CHARACTER*12    FILEE 
      INTEGER         FOUND  
      REAL        FSLFW
      REAL        FSLFN  
      REAL        G2    
      REAL        G3          
      REAL        GNP         
      REAL        GNUP        
      REAL        GPP         
      REAL        GPSM        
      REAL        GRAINN      
      REAL        GRF         
      REAL        GRNWT       
      REAL        GROEAR      
      REAL        GROGRN      
      REAL        GROLF       
      REAL        GRORT       
      REAL        GROSTM      
      REAL        HI
      REAL        HIP
      INTEGER     I           
      INTEGER     ICOLD       
      INTEGER     ICSDUR      
      CHARACTER   IDETO*1     
      INTEGER     IDURP       
      INTEGER     ISECT
      INTEGER     ISTAGE    
      CHARACTER*1 ISWNIT
      CHARACTER*1 ISWWAT      
      INTEGER     L
      REAL        LAI         
      REAL        LAIDOT
      INTEGER     LEAFNO 
      REAL        LEAFNOE 
      REAL        CumLeafSenes    !today's cumul. leaf senescence
      REAL        CumLeafSenesY   !yesterday's cumul. leaf senescence
      REAL        CumLfNSenes     !cumul. N loss in senesced leaves
      INTEGER     LINC  
      REAL        LFWT        
      REAL        LFWTE
      REAL        LIFAC
      REAL        LL(20)
      REAL        DUL(20),TAW,DTAW
      INTEGER     LNUM          
      INTEGER     LUNCRP            
      INTEGER     LUNIO 
      INTEGER     MDATE
      REAL        MAXLAI
      REAL        NDEF3       
      REAL        NFAC        
      REAL        NH4(20)     
      INTEGER     NLAYR       
      REAL        NO3(20)     
      INTEGER     NOUTDO      
      REAL        NPOOL       
      REAL        NPOOL1      
      REAL        NPOOL2      
      REAL        NSDR        
      REAL        NSINK       
      REAL        NSTRES      
      INTEGER     NWSD 
      REAL        P1             
      REAL        P2    
      REAL        P3         
      REAL        P5          
      REAL        PAR         
      REAL        PARSR
      CHARACTER*255    PATHCR 
      CHARACTER*255    PATHSR
      CHARACTER*255    PATHER 
      REAL        PC          
      REAL        PCARB
      REAL        PCNGRN
      REAL        PCNL 
      REAL        PCNRT     
      REAL        PCNSD
      REAL        PCNVEG      
      REAL        PCNST
      REAL        PCO2        
      REAL        PDWI        
      REAL        PRFTC(4)
      REAL        SLPF
      REAL        PGRORT      
      REAL        PHINT       
      REAL        PLA    
      REAL        PLAE     
      REAL        PLAG        
      REAL        PLAS        
      REAL        PLTPOP      
      REAL        PODNO
      REAL        PODWT
      REAL        PORMIN
      REAL        PPLTD 
      REAL        PRFT        
      REAL        PTF         
      REAL        RANC        
      REAL        RANCE
      REAL        RCNP        
      REAL        RGFILL 
      REAL        RGFIL(4)     
      REAL        RLV(20)     
      REAL        RLWR
      REAL        RMNC        
      REAL        RNLAB
      REAL        RNOUT
      REAL        ROOTN 
      REAL        RSGR
      REAL        RSGRT
      INTEGER     RSTAGE      
      REAL        RTWO
      REAL        RTWT
      REAL        RTWTE
      REAL        RTWTO        
      REAL        RUE         
      REAL        RWUEP1
      REAL        ROWSPC      
      REAL        RWUMX
      REAL        SAT(20)     
      REAL        SATFAC      
      REAL        SDSIZE
      REAL        SDSZ
      REAL        SDWT
      CHARACTER*6     SECTION 
      REAL        SEEDNO
      REAL        SEEDRV      
      REAL        SEEDRVE
      REAL        SENLA   !, SENLAY    
      REAL        SFAC        
      REAL        SHF(20)     
      REAL        SHELPC
      REAL        SI1(6)      
      REAL        SI2(6)      
      REAL        SI3(6)      
      REAL        SI4(6)      
      REAL        SKERWT      
      REAL        SLAN        
      REAL        SLA
      REAL        SLFC        
      REAL        SLFN        
      REAL        SLFT        
      REAL        SLFW        
      REAL        SRAD        
      INTEGER     STGDOY(20) 
      REAL        Stg2CLS 
      REAL        STMWTO
      REAL        STMWT   
      REAL        STMWTE    
      REAL        STOVER      
      REAL        STOVN       
      REAL        STOVWT      
      REAL        SUMDTT      
      REAL        SUMEX
      REAL        SUMP   
      REAL        SUMRL     
      REAL        SW(20) 
      REAL        SWEXF     
      REAL        SWFAC       
      REAL        SWIDOT  
      REAL        SWMAX       
      REAL        SWMIN       
      REAL        TABEX       
      REAL        TANC 
      REAL        TANCE
      REAL        TAVGD       
      REAL        TCNP        
      REAL        TEMPM       
      REAL        TFAC        
      REAL        TI         
      REAL        TLNO        
      REAL        TMAX        
      REAL        TMNC        
      REAL        TNLAB
      REAL        TMIN        
      REAL        TOPWT
      REAL        TOTNUP      
      REAL        TRNU    
      REAL        TRWUP
      REAL        TSS(20)    
      REAL        TURFAC      
      REAL        UNH4(20)    
      REAL        UNO3(20)    
      REAL        VMNC        
      REAL        VANC  
      CHARACTER*6     VARNO          
      CHARACTER*16    VRNAME  
      REAL        VSTAGE      
      REAL        WLIDOT
      REAL        WRIDOT
      REAL        WSIDOT 
      REAL        WTLF
      REAL        WTNCAN      
      REAL        WTNLF       
      REAL        WTNSD       
      REAL        WTNST       
      REAL        WTNUP       
      REAL        WTNVEG      
      REAL        XANC        
      REAL        XLAI
      REAL        XHLAI      
      REAL        XLFWT       
      REAL        XGNP        
      REAL        XN          
      REAL        XNF      
      REAL        XNTI        
      REAL        XSTAGE           
      REAL        YIELD       
      REAL        YIELDB
      REAL        O1,TTE,PHY1,PHY2,QN,QD,K2,K1,C1P,RI,GF1,C1,C2,RM,RI1
      REAL        RFR,PHY,XHY,CUMDDT,P9,CUMPHTOT,XLN,SLAMAX,SLAMIN
      REAL        INCPLA,MAXGROLF,MINGROLF,V1,FRCARB,GLFWT,GPLA
      REAL        MAXGROSTM,HEADWT,P3P,POTHEADWT,CUMDTT,XCUMPH
      REAL        SLAX,TMFAC1(8),TTMP,ELO,ELOFT,GROHEAD,SENTIME,SENCODE
      REAL        SGRO(12),WWWW,SENRATE,DSLAN1,DSLANW,SLAN1,SLAY,SPLA
      REAL        XNGLF,XRAT,YRAT,SDN,SLFWT,XNSLF,XLCNP,XXX,YYY
      REAL        XSTEMN,XLEAFN,GROPER,POTGROPER,EXCESS,XLAY,ZZZ
      REAL        APLA,SLOPEPE,SLAN2,WLAN2,PERWT,SUMDT8,SLAN22,PEPE
      REAL        GRFACTOR,GROEMB,PPP,HWMAX,CDEMAND,CDGR,FCP,GRNWTE
      REAL        DSLAN2,FCP2,CPOOL1,HWMIN,CPOOL2,CPOOL,FPOOL1,FPOOL2
      REAL        FACPOOL,EMBWT,OIL,OILPERC,PDWIL,PDWIS,PDWIH,PLAMX
      REAL        XSMNC,XPEPE,XLANC,XLMNC,XHMNC,EMBN,PERN,XHEADN
      REAL        XSANC,XHANC,XSCNP,XHCNP,DSGT,DGET
      INTEGER     YRDOY, PATHL,JPEPE,NGRO,yrplt    
      REAL  RZrwu(300),RZtrwup,AVG_HROOT,WTDEP,qsr(300),TRWUP1
      DOUBLE PRECISION alaireset, WSI(10),heightset
      INTEGER ISTRESS, iresetlai, iresetht1
       common /wuptake/qsr,wtdep,trwup1,RZrwu,RZtrwup,istress,avg_hroot
      common /wsi/ wsi, alaireset, heightset, iresetlai, iresetht1

!     Added to send messages to WARNING.OUT
      CHARACTER*78 MESSAGE(10)

      TYPE (ResidueType) SENESCE   
C LIWANG MA, RZWQM-DSSAT
      TYPE (ControlType) CONTROL
      TYPE (PLANTVARType) PLANTVAR
C END OF MODIFICATION

C----------------------------------------------------------------------
C                     DYNAMIC = RUNINIT
C----------------------------------------------------------------------
       TRWUP1=TRWUP
C RESET LAI 12-16-2018
       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) THEN
          LAI=REAL(ALAIRESET)
          XLAI = LAI                !Leaf area index, m2/m2
          XHLAI = LAI   !Used in WATBAL
          PLA = LAI/PLTPOP*10000.+SENLA
          LFWT = (PLA/267.0)**1.25
       ENDIF
        
          
      IF(DYNAMIC.EQ.RUNINIT.OR.DYNAMIC.EQ.SEASINIT) THEN

C   MODIFIED BY LIWANG MA, RZWQM-DSSAT
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
c          PHINT = PLANTVAR% PHINT
	    PLTPOP = PLANTVAR% PLTPOP
          ROWSPC = PLANTVAR% ROWSPC
C          SDEPTH = PLANTVAR% SDEPTH
          O1 = PLANTVAR% O1  !O1    !sunflower oil content, assume to be 65%

C          !-------------------------------------------------------
C          !     Read input file name (ie. DSSAT40.INP) and path
C          !-------------------------------------------------------
C          CALL GETLUN('FILEIO', LUNIO)
C          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
C          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
C   50     FORMAT(//////,15X,A12,1X,A80)
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
C          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1   
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C
C          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1 
C          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C   51     FORMAT(15X,A12,1X,A80)
C          !------------------------------------------------------
C          !   Read Planting Details Section
C          !------------------------------------------------------
C          SECTION = '*PLANT'
C          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C          IF (FOUND .EQ. 0) THEN
C            CALL ERROR(SECTION, 42, FILEIO, LNUM)
C          ELSE
C            READ(LUNIO,60,IOSTAT=ERR) PLTPOP,ROWSPC ; LNUM = LNUM + 1
C 60         FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
C            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C          ENDIF
C     -----------------------------------------------------------------
C             Read crop cultivar coefficients
C     -----------------------------------------------------------------
C          SECTION = '*CULTI'
C          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
C          IF (FOUND .EQ. 0) THEN
C              CALL ERROR(SECTION, 42, FILEIO, LNUM)
C          ELSE
C            READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
C     %                 P1,P2,P5,G2,G3,PHINT  
!CHP 1800        FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))    
C1800        FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
C            IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
C          ENDIF
C
C        CLOSE(LUNIO)

C         ************************************************************
C         ************************************************************
C 
C                 READ SPECIES FILE
C         ************************************************************
C         ************************************************************
      PATHL  = LEN_TRIM(PATHSR)

      IF (PATHL .LE. 1) THEN
         FILECC = FILES
       ELSE
C-KEN         FILEGG = PATHGE(1:(PATHL-1)) // FILEG
         FILECC = PATHSR(1:(PATHL)) // FILES
      ENDIF


C      FILECC =  TRIM(PATHSR) // FILES
      CALL GETLUN('FILEC', LUNCRP)
      OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
      IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

      !----------------------------------------------------------------
      !       Find and Read TEMPERATURE Section
      !----------------------------------------------------------------

      SECTION = '*TEMPE'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR)
     &       PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR) RGFIL(1),
     &     RGFIL(2),RGFIL(3),RGFIL(4)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      REWIND(LUNCRP)
      !---------------------------------------------------------------
      !         Find and Read PHOTOSYNTHESIS section
      !---------------------------------------------------------------
      SECTION = '*PHOTO'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) PARSR ! RM removed extra paren in format.
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,10(1X,F5.0))',IOSTAT=ERR) CO2X(1),CO2X(2),
     &   CO2X(3), CO2X(4),CO2X(5), CO2X(6),CO2X(7),CO2X(8),
     &   CO2X(9),CO2X(10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(7X,10(1X,F5.2))',IOSTAT=ERR) CO2Y(1),CO2Y(2),
     &   CO2Y(3), CO2Y(4),CO2Y(5), CO2Y(6),CO2Y(7),CO2Y(8),
     &   CO2Y(9),CO2Y(10)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Stress Response
      !----------------------------------------------------------------
      SECTION = '*STRES'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFW
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) FSLFN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Seed Growth Parameters
      !----------------------------------------------------------------
      SECTION = '*SEED '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.4)',IOSTAT=ERR) SDSZ
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) RSGR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) RSGRT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(8X,F6.3)',IOSTAT=ERR) CARBOT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
        
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F7.3)',IOSTAT=ERR) DSGT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F7.3)',IOSTAT=ERR) DGET
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF    
      REWIND(LUNCRP)
      !----------------------------------------------------------------
      !        Find and Read Emergence Initial Conditions
      !----------------------------------------------------------------
      SECTION = '*EMERG'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) STMWTE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RTWTE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) LFWTE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) SEEDRVE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) LEAFNOE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) PLAE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

      ENDIF
      REWIND(LUNCRP)

      !----------------------------------------------------------------
      !        Find and Read Plant Nitrogen Parameters
      !----------------------------------------------------------------
      SECTION = '*NITRO'
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.4)',IOSTAT=ERR) TMNC
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.3)',IOSTAT=ERR) TANCE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.4)',IOSTAT=ERR) RCNP
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F8.3)',IOSTAT=ERR) RANCE
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF
      REWIND(LUNCRP)

      !----------------------------------------------------------------
      !        Find and Read Root parameters
      !----------------------------------------------------------------
      SECTION = '*ROOT '
      CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILECC, LNUM)
      ELSE
        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) PORMIN
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RWUMX
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RLWR
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

        CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
        READ(C80,'(9X,F6.3)',IOSTAT=ERR) RWUEP1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
      ENDIF

      CLOSE (LUNCRP)

          APTNUP = 0.0
          AREALF = 0.0
          ASMDOT = 0.0  
          BIOMAS = 0.0
          CANHT  = 0                
          CANNAA = 0.0
          CANWAA = 0.0
          CANWH  = 0  
          CARBO  = 0.0
!          CSD1   = 0.0
!          CSD2   = 0.0
          CumLeafSenes = 0.0
          CumLeafSenesY = 0.0
          CumLfNSenes = 0.0
          CUMPH  = 0.0
          DM     = 0.0
          DUMMY  = 0.0
          EARS   = 0.0 
          EARWT  = 0.0
          EMAT   = 0
          EP1    = 0.0
          GRAINN = 0.0
          GRF    = 0.0
          GNP = 0.0
          GNUP   = 0.0
          GRNWT  = 0.0
          GROEAR = 0.0
          GROGRN = 0.0
          GPSM   = 0.0
          GROLF  = 0.0
          GRORT  = 0.0
          GROSTM = 0.0
          HI     = 0.0
          HIP    = 0.0
          ICOLD  = 0
          ICSDUR = 0
          IDURP  = 0
          LAI    = 0.0
          LAIDOT = 0.0
          LEAFNO = 0
          LFWT   = 0.0
          LIFAC  = 0.0
          MAXLAI = 0.0
          NPOOL  = 0.0
          NPOOL1 = 0.0
          NPOOL2 = 0.0
          NSDR   = 0.0
          NSINK  = 0.0
          PCNVEG = 0.0
          PCO2   = 0.0
          PCNGRN = 0.0
          PCNL   = 0.0
          PCNRT  = 0.0
          PCNSD  = 0.0
          PCNST  = 0.0
          PDWI   = 0.0
          PLA = 0.0
          PLAG   = 0.0
          PLAS = 0.0
          PODNO  = 0.0
          PODWT  = 0.0
          PRFT   = 0.0
          PPLTD  = 0.0  
          PTF    = 0.0
          RANC   = 0.0
          RMNC   = 0.0
          RNLAB  = 0.0
          ROOTN  = 0.0
          RSTAGE = 0
          RTWTO  = 0.0
          RTWO = 0.0
          RTWT   = 0.0
          SATFAC = 0.0
          SDSIZE = 0.0
          SDWT = 0.0
          SEEDNO = 0.0
          SEEDRV = 0.0
          SENLA  = 0.0
          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0

          SHELPC = 0.0
          SLA    = 0.0
          SKERWT = 0.0
          SLAN = 0.0
          SLFC   = 0.0  
          SLFN   = 0.0
          SLFT   = 0.0
          SLFW   = 0.0
          DO I = 1, 6
              SI1(I) = 0.0
              SI2(I) = 0.0
              SI3(I) = 0.0
              SI4(I) = 0.0
          END DO
          STMWT  = 0.0
          STMWTO = 0.0
          STOVER = 0.0
          STOVN  = 0.0
          STOVWT = 0.
          SUMP   = 0.0
          SUMRL  = 0.0
          SUMEX  = 0.0
          SWEXF  = 0.0
          SWFAC  = 1.0
          SWIDOT = 0.0 
          SWMAX = 0.0
          SWMIN = 0.0
          TANC   = TANCE
          TAVGD  = 0.0
          TCNP   = 0.0
          TEMPM  = 0.0
          TFAC   = 0.0
          TI     = 0.0
          TLNO   = 0.0
          TNLAB  = 0.0
          TOPWT  = 0.0
          TOTNUP = 0.0
          TRNU   = 0.0
          DO L=1,NLAYR
              TSS(L) = 0.0
              RLV(L) = 0.0
          ENDDO
          TURFAC = 1.0
          VANC   = 0.0
          VMNC   = 0.0 
          VSTAGE = 0.0
          WLIDOT = 0.0 
          WRIDOT = 0.0  
          WSIDOT = 0.0  
          WTLF   = 0.0
          WTNCAN = 0.0
          WTNLF  = 0.0
          WTNSD  = 0.0
          WTNST  = 0.0
          WTNUP  = 0.0
          WTNVEG = 0.0
          XANC   = 0.0
          XGNP   = 0.0
          XHLAI  = 0.0
          XLAI   = 0.0
          XLFWT  = 0.0
          XN = 0.0
          XNF    = 0.0
          XNTI   = 0.0
          YIELD  = 0.0
          YIELDB = 0.0
          GLFWT = 0.0
          HEADWT = 0.0
          EMBWT = 0.0
          PERWT = 0.0
          XLCNP = 0.0
          XSMNC = 0.0
          XLANC = 0.0
          XLMNC = 0.0
          XHMNC = 0.0 
          XSCNP = 0.0
          XHCNP = 0.0
          XSTEMN = 0.0
          XNGLF = 0.0
          XHEADN = 0.0
          XLEAFN = 0.0
          EMBN = 0.0
          PDWIL = 0.0
          XLANC = 0.0
          XSANC = 0.0
          XHANC = 0.0
          XLCNP = 0.0
          XSCNP = 0.0
          XHCNP = 0.0
          GROEMB = 0.0
          PERN = 0.0
          GROEMB =0.0
          GROPER = 0.0
          GROHEAD = 0.0
          GROSTM = 0.0
          SLFWT = 0.0
          PGRORT = 0.0
          NFAC = 1.0
          GPP = 0.0
          PPP = 0.0
          ISTAGE = 7
          XSTAGE = 7.0

          IF (ISWNIT .NE. 'N') THEN
             CALL SF_NFACTO(DYNAMIC,GLFWT,STMWT,HEADWT,
     &       ISTAGE,TANC,TCNP,TMNC,STOVWT,SLFWT,XLCNP,
     %       AGEFAC,NDEF3,NFAC,NSTRES,XSMNC,XLANC,XLMNC,XHMNC,RMNC
     &      ,xstage,XSCNP,RCNP,XHCNP)         
          ELSE
              AGEFAC = 1.0
              NSTRES = 1.0
              NDEF3 = 1.0
              NFAC = 1.0
          ENDIF


          CALL SF_NUPTAK(STMWT,GLFWT,HEADWT,SLFWT,
     %      RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %      RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %      XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %      SHF,PTF,BD, SENESCE,XSTEMN,XNGLF,XHEADN,XNSLF,XLEAFN,
     &      EMBN,PDWIL,PDWIS,PDWIH,XLANC,XSANC,XHANC,XLCNP,XSCNP,XHCNP,
     &      GROEMB,GROPER,PERWT,EMBWT,NFAC,JPEPE,PERN,ISTAGE,TURFAC,
     &      TEMPM)

C-----------------------------------------------------------------------  
C-----------------------------------------------------------------------
C
C                     DYNAMIC = RATE
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
!      ELSEIF(DYNAMIC.EQ.RATE) THEN

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C                     DYNAMIC = INTEGR
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.INTEGR) THEN   
   
          SENESCE % ResWt  = 0.0
          SENESCE % ResLig = 0.0
          SENESCE % ResE   = 0.0

!         Keep yesterdays cumulative senescence 
          CumLeafSenesY = CumLeafSenes

!      Need to set these at the beg. of each new growth stage. From phasei
!      Initialize variables at the beginning of a new phenological stage
!      This code was moved from phenol or phasei in Generic CERES v 3.9


!     CHP TEMP
!      IF (YRDOY .GT. 1982179) THEN
!        PAUSE
!      ENDIF

          IF (YRDOY == STGDOY(1) .OR. YRDOY == STGDOY(2) .OR. 
     &        YRDOY == STGDOY(5) .OR. YRDOY == STGDOY(6) .OR. 
     &        YRDOY == STGDOY(7) .OR. YRDOY == STGDOY(8)) THEN 
            ICSDUR = 1
          ENDIF

          !IF(YRDOY.EQ.STGDOY(1)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(2)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF


          IF(YRDOY.EQ.STGDOY(3)) THEN
!              EARWT  = 0.167*STMWT      !only for maize
!              STMWT  = STMWT - EARWT      
              SWMIN  = STMWT*0.85         
              SUMP   = 0.0
              !CNSD1  = 0.0
              !CNSD2  = 0.0
              !CPSD1  = 0.0
              !CPSD2  = 0.0
              !CSD1   = 0.0
              !CSD2   = 0.0
              IDURP  = 0
              ICSDUR = 1
              CANNAA = STOVN*PLTPOP
              CANWAA = BIOMAS
          ENDIF

          IF(YRDOY.EQ.STGDOY(4)) THEN
              SWMAX  = STMWT          
              VANC   = TANC       
              VMNC   = TMNC       
              EMAT   = 0
              !CNSD1  = 0.0
              !CNSD2  = 0.0
              !CPSD1  = 0.0
              !CPSD2  = 0.0
              !CSD1   = 0.0
              !CSD2   = 0.0
              ICSDUR = 1
          ENDIF

          !IF(YRDOY.EQ.STGDOY(5)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(6)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(7)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          !IF(YRDOY.EQ.STGDOY(8)) THEN
          !    CNSD1  = 0.0
          !    CNSD2  = 0.0
          !    CPSD1  = 0.0
          !    CPSD2  = 0.0
          !    CSD1   = 0.0
          !    CSD2   = 0.0
          !    ICSDUR = 1
          !ENDIF

          IF(YRDOY.EQ.STGDOY(9)) THEN
              !Emergence
              STMWT   = STMWTE          
              RTWT    = RTWTE        
              LFWT    = LFWTE         
              STOVWT  = LFWT+STMWT+HEADWT     
              SEEDRV  = SEEDRVE
              BIOMAS = STOVWT
              LEAFNO = INT(LEAFNOE)       
              PLA     = PLAE
              SENLA  = 0.0
              LAI    = PLTPOP*PLA*0.0001 
              CUMPH   = 0.514
              IF (ISWNIT .NE. 'N') THEN
                  GRAINN = 0.000
                  TANC = TANCE
                  ROOTN = RANC   * RTWT
                  STOVN = STOVWT * TANC
              ENDIF
          ENDIF


          !-------------------------------------------------------------
          !           Begin Rate and Integration Calculations
          !-------------------------------------------------------------
      
          TEMPM = (TMAX + TMIN)*0.5   !Mean air temperature, C

          !-------------------------------------------------------------
          !           Compute critical nitrogen contents
          !-------------------------------------------------------------

          TCNP = EXP (1.52-0.160*XSTAGE)/100.0  
C SPE          TMNC = 0.0045                        
c          TMNC = TMNCO    !Where is TMNCO given value?
          IF (XSTAGE .LT. 4.0) THEN
              TMNC =  (1.25-0.200*XSTAGE)/100.0 
          ENDIF

          !-------------------------------------------------------------
          !        Compute Nitrogen Stress Factors 
          !-------------------------------------------------------------
          IF (ISWNIT .NE. 'N' .AND. ISTAGE .LT. 7) THEN
              CALL SF_NFACTO(DYNAMIC,GLFWT,STMWT,HEADWT,                 !Control
     %        ISTAGE,TANC,TCNP,TMNC,STOVWT,SLFWT,XLCNP,                  !Inputs
     %        AGEFAC,NDEF3,NFAC,NSTRES,XSMNC,XLANC,XLMNC,XHMNC,RMNC,
     &        xstage,XSCNP,RCNP,XHCNP)               !Outputs
          ELSE
              AGEFAC = 1.0
              NSTRES = 1.0
              NDEF3 = 1.0
              NFAC = 1.0
          ENDIF

          !-------------------------------------------------------------
          !      Compute Water Stress Factors       
          ! ------------------------------------------------------------
          SWFAC  = 1.0
          TURFAC = 1.0
           pp1=0.40
           pp2=0.55
          IF(ISWWAT.NE.'N') THEN
             TAW=0.0
             DTAW=0.0
             DO I=1,NL
               IF (RLV(I).GT.0.0) THEN
                TAW=TAW+(DUL(I)-LL(I))*DLAYR(I)
                DTAW=DTAW+(DUL(I)-SW(I))*DLAYR(I)
               ENDIF
             ENDDO
             IF (EOP .GT. 0.0) THEN
                EP1 = EOP * 0.1
                IF (TRWUP / EP1 .LT. RWUEP1) THEN
                   TURFAC = (1./RWUEP1) * TRWUP / EP1
                ENDIF
                IF (EP1 .GE. TRWUP) THEN
                  SWFAC = TRWUP / EP1
                ENDIF
C          IF (ISTRESS.EQ.4) CALL HrootStress(TURFAC,SWFAC,EOP)
cc             IF (DTAW.GT.pp1*TAW) THEN
cc                TURFAC=(TAW-DTAW)/(TAW*(1.0-pp1))
cc             ENDIF
cc             IF (DTAW.GT.pp2*TAW) THEN
cc                SWFAC=(TAW-DTAW)/(TAW*(1.0-pp2))
cc             ENDIF
             ENDIF
          ENDIF
          TURFAC = REAL(INT(TURFAC*1000))/1000


          !-------------------------------------------------------------
          !      Compute Water Saturation Factors       
          ! ------------------------------------------------------------

          SATFAC = 0.0    
          SUMEX = 0.0
          SUMRL = 0.0
      
          DO L = 1,NLAYR

          !------------------------------------------------------------
          !PORMIN = Minimum pore space required for supplying oxygen to 
          !         roots for optimum growth and function    
          !TSS(L) = Number of days soil layer L has been saturated 
          !         above PORMIN
          !------------------------------------------------------------
              IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
                  TSS(L) = 0.
              ELSE
                  TSS(L) = TSS(L) + 1.
              ENDIF
          !------------------------------------------------------------
          ! Delay of 2 days after soil layer is saturated before root
          ! water uptake is affected
          !------------------------------------------------------------
              IF (TSS(L) .GT. 2.) THEN
                  SWEXF = (SAT(L)-SW(L))/PORMIN
                  SWEXF = MAX(SWEXF,0.0)
              ELSE
                  SWEXF = 1.0
              ENDIF

              SWEXF = MIN(SWEXF,1.0)
              SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
              SUMRL  = SUMRL + DLAYR(L)*RLV(L)
          ENDDO

          IF (SUMRL .GT. 0.0) THEN
              SATFAC = SUMEX/SUMRL
          ELSE
              SATFAC = 0.0
          ENDIF
          SATFAC = AMAX1(SATFAC,0.0)
          SATFAC = AMIN1(SATFAC,1.0)

          !-------------------------------------------------------------
          !                Daily Photosynthesis Rate
          ! ------------------------------------------------------------
          !Compare with weather module: PAR = SRAD * 2.0  chp
cc          PAR = SRAD*PARSR    !PAR local variable
cc  
cc          LIFAC  = 1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PLTPOP)**0.1 
cc          PCO2  = TABEX (CO2Y,CO2X,CO2,10)
cc          PCARB = RUE*PAR/PLTPOP * (1.0 - EXP(-LIFAC * LAI)) * PCO2
C-SPE     PRFT= AMIN1(1.25 - 0.0035*((0.25*TMIN+0.75*TMAX)-25.0)**2,1.0) 
          TAVGD = 0.25*TMIN+0.75*TMAX
cc  the following is from OILCROP-SUN
      TTE  = 350.0
      PHY1 =  39.0
      PHY2 =  24.0

c      IF (ISWNIT .EQ. 'Y') THEN
c         CALL NFACTO
c      ENDIF

      PAR = 0.5*SRAD
      QN  = 1.0 - EXP(-0.86*LAI)
      QD  = 2.0*QN/(1.0 + QN)

      IF (LAI .GT. 0.001) THEN
         K2 = -ALOG(1.0-QD)/LAI
       ELSE
         K2 = 3.0
      ENDIF
      K2 = AMIN1 (K2,3.0)

      IF (ISTAGE .GT. 4) THEN
         K2 = 1.0
      ENDIF
      K1  = 1.4 + 1.8*(1.0-EXP(-0.5*PLA*PLTPOP/10000.0))
      K1  = AMIN1 (K1,3.0)
      C1P = K1
      RI  = PAR*(1.0-EXP(-K2*LAI))
      !
      ! RUE changes when oil starts to accumulate
      !
      IF (ISTAGE .GT. 3) THEN
         IF (GPP .EQ. 0.0) THEN
            GF1 = 0.8 - SUMDTT/140.0*0.2
            GF1 = AMAX1 (GF1,0.6)
            C1  = 0.8
            C2  = GF1
          ELSE
            GF1 = 0.6 - (SUMDTT-230.0)/140.0*0.2
            GF1 = AMAX1 (GF1,0.4)
            C1  = 0.8
            C2  = GF1
         ENDIF
      ENDIF

      IF (ISTAGE .LT. 4) THEN
         PCARB = K1 * RI/PLTPOP
       ELSE
         IF (RI .GT. 0) THEN
            PCARB = (RI*RI1*C2/C1*K1-RM)/PLTPOP
          ELSE
            PCARB = 0.0
         ENDIF
      ENDIF

      PCARB = AMAX1 (PCARB,0.0)
      !
      ! Effect of temperature on CARBO
      !
c      ELO  = 0.25 * TMIN + 0.75 * TMAX
c      PRFT = 1.0
c      IF (ELO .LT. 4.0 .OR. ELO .GT. 45.0) THEN
c         PRFT = 0.0
c       ELSE
c         IF (ELO .LE. 17.0) THEN
c            PRFT = (ELO - 4.0)/13.0
c          ELSE
c            IF (ELO .LE. 31.0) THEN
c               PRFT = 1.0
c             ELSE
c               PRFT = (45.0 - ELO)/14.0
c            ENDIF
c         ENDIF
c      ENDIF  
c      PRFT = AMAX1 (PRFT,0.0)

      RFR  = 1.2*EXP(-0.5*K2*LAI)
      IF (RFR .GT. 0.5) THEN
         RFR = 1.0
       ELSE
         RFR = 1.0/0.5*RFR
      ENDIF
      !
      ! Calculate Photosynthetic Response to CO2
      !
      PCO2  = TABEX (CO2Y,CO2X,CO2,10)
      PCARB = PCARB*PCO2
cc end from OILCROP-SUN

!**************************************************************************
!**************************************************************************
!     CHP inserted old code for comparison of methods:
!**************************************************************************
!     DSSAT V3.5 CODE:
!      PRFT  = 1.0 - 0.0025*((0.25*TMIN+0.75*TMAX)-26.0)**2

!**************************************************************************
!     DSSAT V3.5 CODE modified:
      !
      ! CIMMYT 99-- JTR, US
      !
!      PRFT  = AMIN1(1.25 - 0.0035*((0.25*TMIN+0.75*TMAX)-25.0)**2,1.0)

!**************************************************************************
!     Use DSSAT V4.0 CODE:
          PRFT = CURV('LIN',PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4),TAVGD)
          PRFT  = AMAX1 (PRFT,0.0)
          PRFT = MIN(PRFT,1.0)

!**************************************************************************
      !
!**************************************************************************


C** WDB   Water logging reduces root water uptake and creates water stress
C** WDB   through SWFAC. Thus, we do not need to also create a reduction
C** WDB   using SATFAC
C** WDB 10/20/03 CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES,(1.0-SATFAC))*SLPF
          CARBO = PCARB*AMIN1 (PRFT,SWFAC,NSTRES)*SLPF
          !Reduce CARBO for assimilate pest damage
          CARBO = CARBO - ASMDOT
          CARBO = MAX(CARBO,0.0)
       WRITE (6666,'(I10,5X,8(F10.6,6X))') YRDOY,PCARB,CARBO,PRFT,SWFAC,
     &      NSTRES,PCO2,SLPF,TAVGD
cc          IF (ISTAGE .EQ. 1 .OR. ISTAGE .EQ. 2 .OR. ISTAGE .EQ. 3) THEN
              !Allow first 5 leaves to expand faster than Phyllochron 
              !interval PHINT by reducing PC
cc              PC     = 1.0
cc             IF (CUMPH .LT. 5.0) THEN
cc                  PC  = 0.66+0.068*CUMPH      
cc                 !Sets PC to 0.7 for leaf 1 and 1.0 for leaf 5
cc              ENDIF
cc
cc              TI     = DTT/(PHINT*PC)
             !CUMPH  = CUMPH + DTT/(PHINT*PC)
cc              CUMPH  = CUMPH + TI
cc              XN     = CUMPH + 1.0
cc              LEAFNO = INT(XN)
cc          ENDIF
      ! Calculate total number of leaves appeared ....  OILCROP-SUN
      !
      IF (LEAFNO .LT. 6) THEN
         PHY = PHY1
       ELSE
         PHY = PHY2
      ENDIF
      IF (CUMPH .LT. 6) THEN
         XHY = PHY1
       ELSE
         XHY = PHY2
      ENDIF

      CUMPHTOT = INT ((CUMDTT-P9)/PHY)
      XLN      = XLN + DTT/PHY

      IF (XLN .GT. TLNO) THEN
         LEAFNO = TLNO
       ELSE
         LEAFNO = INT(XLN)
      ENDIF
      !
      ! Calculate number of expanded leaves  .... SUN
      !
      IF (CUMPH .LT. TLNO) THEN
         IF ((CUMDTT-P9) .LT. (TTE+PHY1)) THEN
            CUMPH  = 0
          ELSE
            XCUMPH = XCUMPH + DTT/XHY
            CUMPH  = INT(XCUMPH)
         ENDIF
       ELSE
         CUMPH = TLNO
      ENDIF

      XN = CUMPH + 1.0
      SLAX   = 1.0  / SQRT (2.778E-5-2.007E-7*LFWT)
      SLAMAX = 1.40 * SLAX
      SLAMIN = 1.00 * SLAX
cc      SELECT CASE (ISTAGE)
cc        CASE (1)
cc          CALL STAGE1 (TURFAC)
          !-------------------------------------------------------------
          !   ISTAGE = 1 (Emergence to End of Juvenile Stage)
          !-------------------------------------------------------------
          IF (ISTAGE .EQ. 1) THEN
       IF (LEAFNO .GE. 1) THEN
         PLAG = RFR*INCPLA (LEAFNO,CUMPH,TEMPM,V1,TLNO)
       ELSE
         PLAG = 2.0
      ENDIF

      IF (TURFAC .LT. 1.0) THEN
         SLAMAX = SLAMIN
      ENDIF

      MAXGROLF = PLAG / SLAMIN
      MINGROLF = PLAG / SLAMAX
      !
      ! More than 0.18 carbo to root
      !
      FRCARB   = 0.57 * CARBO

      IF (MAXGROLF .LE. FRCARB) THEN
         GROLF = MAXGROLF
         PLAG  = MAXGROLF * SLAMIN
       ELSE
         IF (MINGROLF .GT. FRCARB) THEN
            GROLF = FRCARB
            PLAG  = GROLF * SLAMAX
          ELSE
            GROLF = FRCARB
         ENDIF
      ENDIF

      GROLF  = GROLF * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
      PLAG   = PLAG  * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
      GROSTM = 0.245 * CARBO * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
      GRORT  = CARBO - GROLF - GROSTM
      PLA    = PLA   + PLAG
      LFWT   = LFWT  + GROLF
      GLFWT  = LFWT
      GPLA   = PLA
      STMWT  = STMWT + GROSTM
      
              IF (GROLF .GT. 0.0) THEN
                SLAN = SUMDTT*PLA/10000.0   
               !SLAN - normal leaf senescence since emergence, cm2/plant
              ENDIF
              LFWT   = LFWT - SLAN/600.0      ! Subtract respiration
!             CHP - 5/11/2005 why use 600 here?  Why not convert with SLA?

!             5/11/2005 CHP Added cumulative leaf senescence
              CumLeafSenes = SLAN / 600. * PLTPOP * 10.
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

          !-------------------------------------------------------------
          !         ISTAGE = 2 (End of Juvenile to Tassel Initiation)
          !-------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 2) THEN

      PLAG = RFR*INCPLA (LEAFNO,CUMPH,TEMPM,V1,TLNO)

      IF (TURFAC .LT. 1.0) THEN
         SLAMAX = SLAMIN
      ENDIF

      MAXGROLF = PLAG / SLAMIN
      MINGROLF = PLAG / SLAMAX
      !
      ! More than 0.18 carbo to root
      !
      FRCARB = 0.57 * CARBO
      IF (MAXGROLF .LE. FRCARB) THEN
         GROLF = MAXGROLF
         PLAG  = MAXGROLF * SLAMIN
       ELSE
         IF (MINGROLF .GT. FRCARB) THEN
            GROLF = FRCARB
            PLAG  = GROLF * SLAMAX
          ELSE
            GROLF = FRCARB
         ENDIF
      ENDIF

      GROLF  = GROLF * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))   !by Liwang Ma
      PLAG   = PLAG  * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))   !by Liwang Ma
      GROSTM = 0.245 * CARBO * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
      GRORT  = CARBO - GROLF - GROSTM
      STMWT  = STMWT + GROSTM
      PLA    = PLA   + PLAG
      GPLA   = PLA
      LFWT   = LFWT  + GROLF
      GLFWT  = LFWT

!             5/11/2005 CHP Added cumulative leaf senescence
              CumLeafSenes = SLAN / 600. * PLTPOP * 10.
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

!             Stage 3 accumulates leaf senescence from 0
!             Save stage 2 value for true accumulation
              Stg2CLS = CumLeafSenes

!          -------------------------------------------------------------------
!             ISTAGE=3 (Tassel Initiation to End of Leaf Growth and Silking)
!          -------------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 3) THEN

      PLAG = RFR*INCPLA (LEAFNO,CUMPH,TEMPM,V1,TLNO)

      IF (TURFAC .LT. 1.0) THEN
         SLAMAX = SLAMIN
      ENDIF

      MAXGROLF = PLAG / SLAMIN
      MINGROLF = PLAG / SLAMAX

      IF (SUMDTT .LT. DGET) THEN
         !
         ! More than 0.18 carbo to root
         !
         FRCARB = 0.57 * CARBO
         IF (MAXGROLF .LE. FRCARB) THEN
            GROLF = MAXGROLF
            PLAG  = MAXGROLF * SLAMIN
          ELSE
            IF (MINGROLF .GT. FRCARB) THEN
               GROLF = FRCARB
               PLAG  = GROLF * SLAMAX
             ELSE
               GROLF = FRCARB
            ENDIF
         ENDIF

         GROLF  = GROLF * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))   !Liwang Ma
         PLAG   = PLAG  * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))   !Liwang Ma
         GROSTM = 0.245 * CARBO * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
         GRORT  = CARBO - GROLF - GROSTM
         STMWT  = STMWT + GROSTM
         PLA    = PLA   + PLAG
         GPLA   = PLA
         LFWT   = LFWT  + GROLF
         GLFWT  = LFWT    
       ELSE
         MAXGROSTM = 0.605 * CARBO
         IF (SUMDTT .GT. (P3P-180.0) .AND. HEADWT .EQ. 0.0) THEN
            HEADWT    = 0.05  * STMWT
            POTHEADWT = 22.1
            STMWT     = STMWT - HEADWT
            JPEPE     = 1
         ENDIF

         RGFILL = 0.0

         DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
            TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
            ELO  = TTMP
            ELOFT = CURV('LIN',RGFIL(1),RGFIL(2),RGFIL(3),RGFIL(4),TTMP)
c            IF (ELO .LT. 4.0 .OR. ELO .GT. 45.0) THEN
c               ELOFT = 0.0
c             ELSE
c               IF (ELO .LE. 17.0) THEN
c                  ELOFT = (ELO - 4.0)/13.0
c                ELSE
c                  IF (ELO .LE. 31.0) THEN
c                     ELOFT = 1.0
c                   ELSE
c                     ELOFT = (45.0 - ELO)/14.0
c                  ENDIF
c               ENDIF
c            ENDIF
            RGFILL = RGFILL + ELOFT/8.0
         END DO

         IF (HEADWT .GT. 0.0) THEN
            GROHEAD   = 1.71 * RGFILL
          ELSE
            GROHEAD   = 0.0
            MAXGROSTM = RGFILL * MAXGROSTM
         ENDIF

         FRCARB = 0.90 * CARBO - MAXGROSTM - GROHEAD

         IF (FRCARB .GT. 0.295*CARBO) THEN
            FRCARB = 0.295 * CARBO
         ENDIF

         IF (FRCARB .LE. 0.0) THEN
            FRCARB    = 0.0
            MAXGROSTM = 0.90 * CARBO - GROHEAD
            MAXGROSTM = AMAX1 (MAXGROSTM,0.0)
            GROLF     = 0.0
            PLAG      = 0.0
          ELSE
            IF (MAXGROLF .LE. FRCARB) THEN
               GROLF = MAXGROLF
               PLAG  = MAXGROLF * SLAMIN
             ELSE
               IF (MINGROLF .GT. FRCARB) THEN
                  GROLF = FRCARB
                  PLAG  = GROLF * SLAMAX
                ELSE
                  GROLF = FRCARB
               ENDIF
            ENDIF
         ENDIF

         GROSTM  = MAXGROSTM
         PLAG    = PLAG    * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))   !Liwang Ma
         GROLF   = GROLF   * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
         GROHEAD = GROHEAD * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
         GROSTM  = GROSTM  * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
         GRORT   = CARBO - GROLF - GROSTM - GROHEAD
         !
         ! Cambios en el calculo de la senescencia
         !
         IF (SENTIME .EQ. 13.0) THEN
            SENCODE = 1.0
         ENDIF
         IF (LAI .GT. 1.2 .AND. SENCODE .EQ. 0.0.AND.SENTIME.LT.12) THEN  ! ADDED SENTIME.LT.12
            SENTIME    = SENTIME + 1.0
            NGRO       = IFIX(SENTIME)
            SGRO(NGRO) = PLAG
         ENDIF

         IF (SENCODE .EQ. 1.0) THEN
            WWWW     = -0.0182 + 0.4147*SGRO(1)*PLTPOP/10000.0
            DO I = 1, 11
               SGRO(I) = SGRO(I+1)
            END DO
            SGRO(12) = PLAG
            SENRATE  = WWWW
          ELSE
            SENRATE  = 0.0
         ENDIF

         IF (SENRATE .GT. 0.0) THEN
            DSLAN1 = SENRATE*10000.0/PLTPOP
          ELSE
            DSLAN1 = 0.0
         ENDIF
         !
         ! Calculo de senescencia March/91
         !
         IF (CUMPH .GT. (TLNO-5) .AND. TURFAC .LT. 0.8) THEN
            DSLANW = 0.03 * GPLA
          ELSE
            DSLANW = 0.0
         ENDIF

         DSLAN1 = AMAX1 (DSLAN1,DSLANW)
         SLAN1  = SLAN1 + DSLAN1
         SLAN   = SLAN1
         SLAY   = GPLA  / GLFWT
         PLA    = PLA   + PLAG
         GPLA   = GPLA  - DSLAN1 + PLAG
         SPLA   = SPLA  + DSLAN1
         XRAT   = XNGLF / GLFWT
         YRAT   = (0.009 - 0.0875 * XRAT)/0.9125
         SDN    = DSLAN1/SLAY*(XRAT - YRAT)
         SLFWT  = SLFWT + DSLAN1/SLAY - SDN * 6.25
         XNSLF  = XNSLF + DSLAN1/SLAY*YRAT
         GLFWT  = GLFWT + GROLF + SDN * 6.25 - DSLAN1/SLAY
         XNGLF  = XNGLF - DSLAN1/SLAY*YRAT
         XXX    = GLFWT * XLCNP
         IF (XNGLF .GT. XXX) THEN
            YYY    = XNGLF  - XXX
            XNGLF  = XXX
            GLFWT  = GLFWT  - YYY * 6.25
            LFWT   = LFWT   - YYY * 6.25
            STMWT  = STMWT  + YYY * 6.25
            XSTEMN = XSTEMN + YYY
            XLEAFN = XLEAFN - YYY
         ENDIF
         LFWT   = LFWT   + GROLF
         STMWT  = STMWT  + GROSTM
         HEADWT = HEADWT + GROHEAD

         IF (SUMDTT .GT. (P3P-130.0)) THEN
            SUMP  = SUMP  + CARBO
            IDURP = IDURP + 1
         ENDIF
      ENDIF


!             5/11/2005 CHP Added cumulative leaf senescence
!             7/22/2005 CHP Need to add leaf senescence from Stage 2 for
!                           total accumulation.
              CumLeafSenes = SLAN / 600. * PLTPOP * 10. + Stg2CLS
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

!      --------------------------------------------------------------------
!         ISTAGE = 4 (Silking to beginning of effective grain filling period)
!      --------------------------------------------------------------------

          ELSEIF (ISTAGE .EQ. 4) THEN
          
      IF (CARBO .NE. 0.0) THEN
         RGFILL = 0.0
         DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
            TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
            ELO  = TTMP
            ELOFT = CURV('LIN',RGFIL(1),RGFIL(2),RGFIL(3),RGFIL(4),TTMP)
c            IF (ELO .LT. 4.0 .OR. ELO .GT. 45.0) THEN
c               ELOFT = 0.0
c             ELSE
c               IF (ELO .LE. 17.0) THEN
c                  ELOFT = (ELO - 4.0)/13.0
c                ELSE
c                  IF (ELO .LE. 31.0) THEN
c                     ELOFT = 1.0
c                   ELSE
c                     ELOFT = (45.0 - ELO)/14.0
c                  ENDIF
c               ENDIF
c            ENDIF
            RGFILL = RGFILL + ELOFT/8.0
         END DO
         GROPER = POTGROPER * RGFILL                 ! SUN, Hall
      ENDIF

      GROHEAD = 1.71 * RGFILL                        ! SUN, Hall

      IF (GROPER .GT. CARBO) THEN
         GROPER  = CARBO
         GROHEAD = 0.0
       ELSE
         IF ((GROPER + GROHEAD) .GT. CARBO) THEN
            GROHEAD = CARBO - GROPER
         ENDIF
      ENDIF   

      EXCESS = CARBO - GROPER - GROHEAD
      EXCESS = AMAX1 (EXCESS,0.0)

      IF (STMWT .LT. SWMAX) THEN
         GROSTM = EXCESS
         GRORT  = 0.0
       ELSE
         GRORT  = EXCESS
         GROSTM = 0.0
      ENDIF 

      GROHEAD = GROHEAD * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))   !Liwang Ma
      GROPER  = GROPER  * (0.7 + 0.3*SWFAC)
      GROSTM  = GROSTM  * AMIN1 (AGEFAC,TURFAC,(1.-SATFAC))
      GRORT   = CARBO - GROHEAD - GROPER - GROSTM

      IF (ISWNIT .EQ. 'Y') THEN
         CALL NGRAIN (NFAC,GPP,G3,GROEMB,O1,P5,OIL,GROPER,RANC,
     &       RMNC,XNGLF,GLFWT,XSTEMN,STMWT,HEADWT,STOVN,STOVWT,
     &       XLANC,XLMNC,XPEPE,XSMNC,XHMNC,RNOUT,SUMDTT,
     &       ROOTN,XLEAFN,VANC,PERN,EMBN,GRAINN,EMBWT,PERWT,NSINK,
     &       XSANC,XHANC,XRAT,YRAT,XHEADN,XNSLF,TURFAC,TEMPM,rtwt)
         !
         ! Calculate senescence due to N demand
         !
         IF (GPLA .GT. 0.1) THEN
            XLAY = XNGLF/GLFWT
            YRAT = (0.009 - 0.0875 * XLAY)/0.9125
            ZZZ  = APLA - SLOPEPE * (XNGLF - YRAT*GLFWT)
            IF (ZZZ .GT. SLAN2) THEN
               DSLAN2 = ZZZ - SLAN2
             ELSE
               DSLAN2 = 0.0
            ENDIF
          ELSE
            DSLAN2 = 0.0
         ENDIF
         !
         ! Threshold for starting leaf senescence due to water stress
         ! changed from 0.8 to 0.2 - FV, 12/12/97
         !
         IF (TURFAC .LT. 0.2) THEN
            DSLANW = 0.06 * GPLA
          ELSE
            DSLANW = 0.0
         ENDIF

         DSLAN2 = AMAX1 (DSLAN2,DSLANW)
         SLAN2  = SLAN2  + DSLAN2
         SLAY   = GPLA   / GLFWT
         SLAN   = SLAN1  + SLAN2
         WLAN2  = DSLAN2 / SLAY
         LFWT   = LFWT   - WLAN2 * (XLAY-YRAT)*6.25
         GLFWT  = GLFWT  - WLAN2
         SLFWT  = SLFWT  + WLAN2 * (1.0-6.25*(XLAY-YRAT))
         GPLA   = GPLA   - DSLAN2
         SPLA   = SPLA   + DSLAN2
         HEADWT = HEADWT + GROHEAD
         STMWT  = STMWT  + WLAN2 * (XLAY-YRAT)*6.25+GROSTM
         PERWT  = PERWT  + GROPER
         EARWT  = EARWT  + GROPER
         GRNWT  = PERWT
         SUMP   = SUMP   + CARBO
         IDURP  = IDURP  + 1
      ENDIF
!             5/11/2005 CHP Added cumulative leaf senescence
              CumLeafSenes = SLAN / 600. * PLTPOP * 10. + Stg2CLS
!                kg/ha     =  g/plant * plants/m2 * (kg/ha)/(g/m2)

          !-------------------------------------------------------------
          !   ISTAGE = 5 Effective Grain Filling Period
          !-------------------------------------------------------------

c          ELSEIF (ISTAGE .EQ. 5) THEN
          ELSEIF (PLTPOP.NE.0.01) THEN

      IF (ISWNIT .EQ. 'Y') THEN
         IF (GLFWT .GT. 0.1) THEN
            XLAY = XNGLF / GLFWT
            YRAT = (0.009 - 0.0875 * XLAY)/0.9125
            ZZZ  = APLA - SLOPEPE * (XNGLF - YRAT*GLFWT)
            IF (ZZZ .GT. SLAN2 .AND. ZZZ .LE. APLA) THEN
               DSLAN2 = ZZZ - SLAN2                  ! Increment in senesc. leaf
             ELSE
               DSLAN2 = 0.0
            ENDIF
         ENDIF
       ELSE
         !
         ! Convert thermal time to base of 8.5 C
         !
         ! If N is not calculated, calculate leaf senescence according to
         ! Sadras and Hall (1988), FCR 18:185-196.  - FV, 12/12/97
         !
         SUMDT8 = 0.75*SUMDTT
         IF (SUMDT8 .GT. 200.0) THEN
            !
            ! Senescence=f(TT) - FV 12/12/97
            !
            SLAN22 = APLA*(1.614-685./SUMDT8+73870./SUMDT8**2)
            DSLAN2 = SLAN22-SLAN2
          ELSE
            DSLAN2 = 0.0
         ENDIF
      ENDIF
      !
      ! Threshold for starting leaf senescence due to water stress
      ! changed from 0.8 to 0.2 - FV, 12/12/97
      !
      IF (TURFAC .LT. 0.2) THEN
         DSLANW = 0.06 * GPLA
       ELSE
         DSLANW = 0.0
      ENDIF

      DSLAN2 = AMAX1 (DSLAN2,DSLANW)
      SLAN2  = SLAN2 + DSLAN2
      SLAN   = SLAN1 + SLAN2

      IF (CARBO .NE. 0.0) THEN
         RGFILL = 0.0
         DO I = 1, 8
         TMFAC1(I) = 0.931 + 0.114*I-0.0703*I**2+0.0053*I**3
            TTMP = TMIN + TMFAC1(I)*(TMAX-TMIN)
            ELO  = TTMP
            ELOFT = CURV('LIN',RGFIL(1),RGFIL(2),RGFIL(3),RGFIL(4),TTMP)
c            IF (ELO .LT. 4.0 .OR. ELO .GT. 45.0) THEN
c               ELOFT = 0.0
c             ELSE
c               IF (ELO .LE. 17.0) THEN
c                  ELOFT = (ELO - 4.0)/13.0
c                ELSE
c                  IF (ELO .LE. 31.0) THEN
c                     ELOFT = 1.0
c                   ELSE
c                     ELOFT = (45.0 - ELO)/14.0
c                  ENDIF
c               ENDIF
c            ENDIF
            RGFILL = RGFILL + ELOFT/8.0
         END DO

         IF (SUMDTT .LE. 270.0) THEN
            GROPER = POTGROPER * RGFILL *(0.70 + 0.30*SWFAC)
          ELSE
            GROPER = 0.0
         ENDIF
         !
         ! Growth per embryo
         !
         PEPE = RGFILL * G3 * 0.001 * (0.70 + 0.30 * SWFAC) 
         PEPE = PEPE * GRFACTOR
         IF (GPP .GT. 0.0) THEN
            GROEMB = GPP * PEPE
          ELSE
            GROEMB = PPP * PEPE
         ENDIF

         IF (HEADWT .LT. HWMAX) THEN
            GROHEAD = 1.71 * RGFILL
          ELSE
            GROHEAD = 0.0
         ENDIF
      ENDIF

      CDEMAND = GROEMB + GROPER + GROHEAD
      CDGR    = GROPER + GROEMB

      IF (CDGR .GT. 0.1) THEN
         IF (CARBO .GT. CDEMAND ) THEN
            IF (STMWT .LT. SWMAX) THEN
               IF ((SWMAX-STMWT) .GT. (CARBO - CDEMAND)) THEN
                  GROSTM = CARBO - CDEMAND
                ELSE
                  GROSTM = SWMAX - STMWT
                  GRORT  = CARBO - CDEMAND - GROSTM
               ENDIF
             ELSE
               GRORT  = CARBO - CDEMAND
               GROSTM = 0.0
            ENDIF
          ELSE
           GRORT  = 0.0
           GROSTM = 0.0
           IF (CARBO .GT. CDGR) THEN
              GROHEAD = CARBO - CDGR
            ELSE
              GROHEAD = 0.0
C             CPOOL   = (STMWT - SWMIN) + (HEADWT - HWMIN)
              FCP     = (STMWT - SWMIN) / (SWMAX - SWMIN)
              IF (FCP .GT. 0.3) THEN
                 FCP2 = 1.0
               ELSE
                 FCP2 = 0.0
              ENDIF
              CPOOL1 = 0.79*FCP2
              FCP    = (HEADWT-HWMIN)/(HWMAX-HWMIN)
              IF (FCP .GT. 0.3) THEN
                 FCP2 = 1.0
               ELSE
                 FCP2 = 0.0
              ENDIF
              CPOOL2 = 0.42*FCP2
              CPOOL  = CPOOL1 + CPOOL2
              IF (CPOOL .GT. ((CDGR-CARBO)/0.44)) THEN
                 FPOOL1 = CPOOL1 / CPOOL
                 FPOOL2 = CPOOL2 / CPOOL
                 STMWT  = STMWT  - (GROEMB+GROPER-CARBO)/0.44*FPOOL1
                 HEADWT = HEADWT - (GROEMB+GROPER-CARBO)/0.44*FPOOL2
               ELSE
                 IF (CDGR .GT. 0.1) THEN
                    FACPOOL = (CPOOL*0.44+CARBO)/CDGR
                  ELSE
                    FACPOOL = 0.0
                 ENDIF
                 GROEMB = GROEMB * FACPOOL
                 GROPER = GROPER * FACPOOL
                 STMWT  = STMWT  - CPOOL1
                 HEADWT = HEADWT - CPOOL2
              ENDIF   
           ENDIF
         ENDIF

c        GROPER = GROPER*(.7+.3*SWDF1)
         GRORT  = CARBO - GROEMB - GROPER - GROHEAD - GROSTM

         IF (ISWNIT .EQ. 'Y') THEN
            CALL NGRAIN (NFAC,GPP,G3,GROEMB,O1,P5,OIL,GROPER,RANC,
     &       RMNC,XNGLF,GLFWT,XSTEMN,STMWT,HEADWT,STOVN,STOVWT,
     &       XLANC,XLMNC,XPEPE,XSMNC,XHMNC,RNOUT,SUMDTT,
     &       ROOTN,XLEAFN,VANC,PERN,EMBN,GRAINN,EMBWT,PERWT,NSINK,
     &       XSANC,XHANC,XRAT,YRAT,XHEADN,XNSLF,TURFAC,TEMPM,rtwt)
         ENDIF

         EMBWT  = EMBWT  + GROEMB
         PERWT  = PERWT  + GROPER
         EARWT  = EARWT  + GROPER
         GRNWT  = PERWT  + EMBWT
         HEADWT = HEADWT + GROHEAD
         SLAY   = GPLA   / GLFWT
         XLAY   = XNGLF  / GLFWT
         SLAN   = SLAN1  + SLAN2
         IF (SLAY .GT. 0.0) THEN
            WLAN2 = DSLAN2/SLAY
          ELSE
            WLAN2 = 0.0
         ENDIF
         LFWT  = LFWT  - WLAN2 * (XLAY-YRAT)*6.25
         GLFWT = GLFWT - WLAN2
         SLFWT = SLFWT + WLAN2 * (1.0-6.25*(XLAY-YRAT))
         GPLA  = GPLA  - DSLAN2
         SPLA  = SPLA  + DSLAN2
         STMWT = STMWT + WLAN2*(XLAY-YRAT)*6.25 + GROSTM

         IF (GRNWT .GT. 0.1) THEN
            OILPERC = OIL / GRNWT * 100.0
          ELSE
            OILPERC = 0.0
         ENDIF

         SUMP  = SUMP  + CARBO
         IDURP = IDURP + 1
      ENDIF

      !----------------------------------------------------------------
      !ISTAGE = 6 
      !(End effective grain filling period to physiological maturity)
      !----------------------------------------------------------------
C          ELSEIF (ISTAGE .EQ. 6) THEN
C              RETURN
          ENDIF           !corresponding to main ELSEIF ISTAGE Loop
          
cc begining of OILCROP-SUN
      CARBO = AMAX1 (CARBO,0.001)
      !
      ! Calculate potential growth of plant parts
      !
      PDWI   = CARBO - GRORT
      PDWIL  = GROLF
      PDWIS  = GROSTM
      PDWIH  = GROHEAD
      PGRORT = GRORT
      SLFN   = 0.95 + 0.05*AGEFAC
      SLFT   = 1.0
      IF (TMIN .GT. -3.0) THEN
         ICOLD = 0
       ELSE
         ICOLD = ICOLD + 1
      ENDIF
      SLFT   = AMAX1 (SLFT,0.0)
      PLAS   = 0.0
      SENLA  = SENLA + PLAS
      SENLA  = AMAX1 (SENLA,SLAN)
      SENLA  = AMIN1 (SENLA,PLA)
      LAI    = (PLA-SENLA)*PLTPOP*0.0001
C       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) LAI = REAL(ALAIRESET)
      PLAMX  = AMAX1 (PLAMX,PLA-SENLA)

      IF (((LEAFNO .GT. 3) .AND. (LAI .LE. 0.0) .AND. (ISTAGE .LE. 4))
     &                      .OR. (ICOLD .GE. 7)) THEN
         WRITE (*,2800) yrdoy-yrplt
         IF (IDETO .EQ. 'Y') THEN
            WRITE (NOUTDO,2800) yrdoy-yrplt
         ENDIF
         ISTAGE = 6
      ENDIF

      IF (ISTAGE .LT. 4) THEN
         RTWT = RTWT + GRORT
       ELSE
         RTWT = RTWT + GRORT - 0.005*RTWT
      ENDIF

      BIOMAS = (LFWT + STMWT + HEADWT + GRNWT + GRNWTE)*PLTPOP
      DM     = BIOMAS*10.0
      STOVWT = LFWT + STMWT + HEADWT
      PTF    = STOVWT/(RTWT + STOVWT)

cc      IF (ISWNIT. EQ. 'Y') THEN
cc         CALL NUPTAK (ISTAGE,PLTPOP)
cc      ENDIF
cc end of OILCROP-SUN
c----------------------------------------------------------------------
c   The following code is executed each day regardless of ISTAGE value
c----------------------------------------------------------------------


          !------------------------------------------------------------
          !               Compute Leaf Senescence Factors
          !------------------------------------------------------------

!         Senescence due to water stress
          SLFW   = (1-FSLFW) + FSLFW*SWFAC  

!         Senescence due to nitrogen stress
          SLFN   = (1-FSLFN) + FSLFN*AGEFAC 

!         Senescence due to light competition
          SLFC   = 1.00        
          IF (LAI .GT. 4.0) THEN
              SLFC = 1.0 - 0.008*(LAI-4.0)
          ENDIF

!         Senescence due to temperature
          SLFT   = 1.0
          IF (TMIN.LE.6.0) THEN
              SLFT  = AMAX1 (0.0, 1.0 - 0.01 * (TMIN-6.0)**2)
          ENDIF
          SLFT  = AMAX1 (SLFT,0.0)

          PLAS  = (PLA-SENLA)*(1.0-AMIN1(SLFW,SLFC,SLFT,SLFN)) 
!         Daily rate of leaf senescence
          SENLA = SENLA + PLAS
          SENLA = AMAX1 (SENLA,SLAN)
          SENLA = AMIN1 (SENLA,PLA)         
          LAI   = (PLA-SENLA)*PLTPOP*0.0001
C       IF (IRESETLAI.EQ.1.AND.ALAIRESET.GT.0.D0) LAI = REAL(ALAIRESET)

          !------------------------------------------------------------
          !       CROP GROWTH FAILURE DUE TO COLD TEMPERATURES
          !  This is probably a holdover from wheat, and can be deleted
          !------------------------------------------------------------

C ** JIL Better use a parameter instead of 6.0 (read in Ecotype file?)
          IF (TMIN .LE. 6.0) THEN 
              ICOLD = ICOLD + 1
          ELSE
              ICOLD = 0
          ENDIF

          IF (LEAFNO .GT. 4 .AND. LAI .LE. 0.0 .AND. ISTAGE .LE. 4
     &                  .AND. ICOLD .GT. 6) THEN
              WRITE(MESSAGE(1),2800) yrdoy-yrplt
              CALL WARNING(1,ERRKEY, MESSAGE)
              WRITE (*,2800) yrdoy-yrplt
              IF (IDETO .EQ. 'Y') THEN
                  WRITE (NOUTDO,2800) yrdoy-yrplt
              ENDIF
              ISTAGE = 6
              MDATE = YRDOY
          ELSE
              IF (ICOLD .GE. 15) THEN
                  WRITE(MESSAGE(1),2800) yrdoy-yrplt
                  CALL WARNING(1,ERRKEY, MESSAGE)
                  WRITE (*,2800) yrdoy-yrplt
                  IF (IDETO .EQ. 'Y') THEN
                      WRITE (NOUTDO,2800) yrdoy-yrplt
                  ENDIF
                  ISTAGE = 6
                  MDATE = YRDOY
              ENDIF
          ENDIF

          !------------------------------------------------------------
          !CROP GROWTH FAILURE DUE TO SEVERE WATER STRESS  
          !End growth and module run if water stress days 
          !exceeds 10 days before Effective grain Filling Period if 
          !LAI <= 0.0
          !------------------------------------------------------------

          IF (SWFAC .GT. 0.1) THEN
              NWSD = 0                                                  
          ELSE
              NWSD = NWSD + 1
          ENDIF

          IF (LAI .LE. 0.1 .AND. ISTAGE .LT. 4
     &                 .AND. NWSD .GT. 10) THEN
              WRITE(MESSAGE(1),2801) yrdoy-yrplt
              CALL WARNING(1,ERRKEY, MESSAGE)
              WRITE (*,2801) yrdoy-yrplt
              IF (IDETO .EQ. 'Y') THEN
                  WRITE (NOUTDO,2801) yrdoy-yrplt
              ENDIF   
              ISTAGE = 6           
              MDATE = YRDOY
          ENDIF

C--------------------------------------------------------------
C  APPLY PEST DAMAGE
C--------------------------------------------------------------

      ! Leaf Damage
          IF((LFWT+STMWT).GT.0.0)
     $      STOVN=STOVN - STOVN*(WLIDOT/PLTPOP)/(LFWT+STMWT)
          IF (PLTPOP.GT.0.0.AND.LFWT.GT.0.0)
     &      LAIDOT = WLIDOT*(PLA-SENLA)/(LFWT*PLTPOP)  !cm2/plant/day
          IF(PLTPOP.GT.0.0)
     &      LFWT = LFWT - WLIDOT/PLTPOP
           
          PLA = PLA - LAIDOT
          LAI = LAI - LAIDOT*PLTPOP/10000

      ! Stem Damage
          IF(PLTPOP.GT.0.0)
     &       STMWT = STMWT - WSIDOT/PLTPOP
          IF(PLTPOP.GT.0.0.AND.(LFWT+STMWT).GT.0.0)
     &       STOVN=STOVN - STOVN*(WSIDOT/PLTPOP)/(LFWT+STMWT)

      ! Root Weight
          IF(PLTPOP.GT.0.0 .AND. RTWT .GT. 0.) THEN
            ROOTN = ROOTN - ROOTN*(WRIDOT/PLTPOP)/RTWT
            RTWT = RTWT - WRIDOT/PLTPOP
          ENDIF

      ! Grain Weight and Number
          IF (GRNWT .GT. 0.AND.PLTPOP.GT.0) THEN
            GRAINN = GRAINN - GRAINN*(SWIDOT/PLTPOP)/GRNWT
            GPP = GPP - GPP*(SWIDOT/PLTPOP)/GRNWT
          ENDIF

          IF(PLTPOP.GT.0.0) THEN 
            GRNWT = GRNWT - SWIDOT/PLTPOP
            EARWT = EARWT - SWIDOT/PLTPOP
          ENDIF
c          write (100,*) grnwt, swidot, pltpop
      ! Population
          IF(PPLTD.GT.0) THEN
             PLTPOP = PLTPOP - PLTPOP * PPLTD/100
             EARS = EARS - EARS*PPLTD/100
             LAI = LAI - LAI*(PPLTD/100)
           ENDIF

          !------------------------------------------------------------
          !               COMPUTE NITROGEN UPTAKE
          !------------------------------------------------------------

          IF (CARBO .LE. 0.0) THEN
              CARBO = 0.001
          ENDIF

          IF (ISWNIT .NE. 'N') THEN 
              IF(CARBO.GT.0.0) THEN  
                PDWI   = PCARB*(1.0-GRORT/CARBO)
                PGRORT = PCARB*GRORT/CARBO
              ENDIF

              CALL SF_NUPTAK(STMWT,GLFWT,HEADWT,SLFWT,
     %        RANC, ROOTN,RTWT,TANC,STOVN,STOVWT,TRNU,NLAYR,
     %        RLV,NO3,NH4,PDWI,TCNP,UNO3,UNH4,
     %        XSTAGE,RCNP,PGRORT,PLTPOP,SW,LL,SAT,DLAYR,
     %        SHF,PTF,BD, SENESCE,XSTEMN,XNGLF,XHEADN,XNSLF,XLEAFN,
     %        EMBN,PDWIL,PDWIS,PDWIH,XLANC,XSANC,XHANC,XLCNP,XSCNP,
     %        XHCNP,GROEMB,GROPER,PERWT,EMBWT,NFAC,JPEPE,PERN,ISTAGE,
     %        TURFAC,TEMPM)
          ENDIF


          !------------------------------------------------------------
          !                      STATE VARIABLES
          !------------------------------------------------------------

          RTWT   = RTWT  + 0.5*GRORT-0.005*RTWT
          LFWT = MAX(0.0,LFWT)
          PLA = MAX(0.0,PLA)
          LAI = MAX(0.0,LAI)
          STMWT = MAX(0.0,STMWT)
          STOVN = MAX(0.0,STOVN)
          GPP = MAX(0.0,GPP)
          GRNWT = MAX(0.0,GRNWT)
          EARWT = MAX(0.0,EARWT)
          PLTPOP = MAX(0.0,PLTPOP)
          EARS = MAX(0.0,EARS)
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
            PLANTVAR%PLTPOP  = PLTPOP
           CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
         if (grort.lt.0.0) then
         continue
         endif
          RTWT = MAX(0.0,RTWT)
          
          BIOMAS = (LFWT + STMWT + HEADWT + GRNWT + GRNWTE)*PLTPOP
          DM     = BIOMAS*10.0
          STOVWT = LFWT + STMWT + HEADWT

c          BIOMAS = (LFWT + STMWT + EARWT)*PLTPOP
c          DM     = BIOMAS*10.0
          STOVWT = LFWT  + STMWT + HEADWT
          IF (RTWT + LFWT + STMWT + HEADWT + GRNWT + GRNWTE.GT.0.0) THEN
            PTF = (LFWT + STMWT + HEADWT + GRNWT + GRNWTE)/
     &            (RTWT + LFWT + STMWT + HEADWT + GRNWT + GRNWTE)
          ELSE
            PTF = 0.0
          ENDIF

          VSTAGE = REAL (LEAFNO)        !V-stage
          IF (LEAFNO .GT. 0.0) THEN
              RSTAGE = ISTAGE           !R-stage
          ELSE
              RSTAGE = 0
          ENDIF

          TOPWT = BIOMAS            !Total above ground biomass, g/m2
          WTLF = LFWT*PLTPOP        !Leaf weight, g/m2
          XLAI = LAI                !Leaf area index, m2/m2
          XHLAI = LAI   !Used in WATBAL


          IF(WTLF.GT.0) THEN
              SLA = LAI*10000/WTLF  !Specific leaf area, cm2/g
              DUMMY = PLA/LFWT
          ELSE    
              SLA = 0
          ENDIF

          STMWTO = STMWT * PLTPOP   !Stem weight, g/m2
          RTWTO = RTWT * PLTPOP     !Root weight, g/m2

!     02/06/2005 CHP, GH changed to account for barrenness factor
!     Reduce PODWT by grain weight per plant for barren ears
!OLD          PODWT = EARWT*PLTPOP      !Pod weight, g/m2
c     Liwang Ma commented out the IF condition so that PODWT will not be negative.
c      IF (PLTPOP .GE. EARS) THEN                       !Barrenness
c        PODWT = HEADWT*PLTPOP - GRNWT*(PLTPOP-EARS) !Pod weight, g/m2
c      ELSE                                           !Prolificacy
        PODWT = HEADWT*EARS
c      ENDIF
      if (podwt.lt.0.0) then
      continue
      endif
      
!          PODWTD = 0.0              !Detached pod weight

!     12/16/2004 CHP, GH changed to account for barrenness factor
!          SDWT = GRNWT*PLTPOP       !Seed weight, g/m2
          SDWT = GRNWT*EARS         !Seed weight, g/m2
          GPSM = GPP * EARS         !Seed no/m2, but units don't match?
          SEEDNO=GPSM               !Seed no./m2
          IF(GPP.GT.0) THEN
              PODNO = SEEDNO/GPP    !Pod (ear) No/m2
          ELSE
              PODNO = 0
          ENDIF
          
          IF (PODWT .GT. 0.1) THEN
              SHELPC = SDWT*100./PODWT
          ELSE
              SHELPC = 0.0
          ENDIF

          SHELPC = MIN(SHELPC,99.99)

          IF (SEEDNO .GT. 0.0) THEN
              SDSIZE = SDWT/SEEDNO*1000
          ELSE
              SDSIZE = 0.0
          ENDIF

          IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
              HI = SDWT/TOPWT
          ELSE
              HI = 0.
          ENDIF

          IF (TOPWT .GT. 0. .AND. PODWT .GE. 0.) THEN
              HIP = PODWT/TOPWT
          ELSE
              HIP = 0.
          ENDIF

!------> OLD NUPTAK CALL

          !------------------------------------------------------------
          ! Compute Plant Nitrogen Variables for Output
          !------------------------------------------------------------

          WTNCAN = (STOVN+GRAINN)*PLTPOP !Nitrogen in canopy (KG N/ha)
          WTNSD = GRAINN*PLTPOP  !Nitrogen in grain (g/m2)
          IF ((LFWT+STMWT) .GT. 0.0) THEN
              WTNLF = STOVN * (LFWT/STOVWT) * PLTPOP 
                                  !Nitrogen in leaf, g/m2
              WTNST = STOVN * (STMWT/(LFWT+STMWT)) * PLTPOP 
                                  !Nitrogen in stem, g/m2
          ELSE
              WTNLF = 0.0 
              WTNST = 0.0
          ENDIF
      
          WTNVEG=WTNLF+WTNST     !Nitrogen in vegetative tissue (g/m2)


          !------------------------------------------------------------
          !Plant Nitrogen Concentration Variables
          !------------------------------------------------------------

          IF(LFWT.GT.0.AND.PLTPOP.GT.0) THEN
              PCNL = WTNLF/(LFWT*PLTPOP)*100  !Percent N in leaf tissue
          ELSE
              PCNL = 0
          ENDIF

          IF(SDWT.GT.0.AND.PLTPOP.GT.0) THEN
              PCNSD = WTNSD/(GRNWT*PLTPOP) *100
          ELSE
              PCNSD = 0.0
          ENDIF

          IF (GRNWT .GT. 0.0.AND.PLTPOP.GT.0) THEN
             PCNGRN = WTNSD/(GRNWT*PLTPOP)*100
          ELSE
             PCNGRN = 0.0
          ENDIF

          IF( (WTLF+STMWT).GT.0.AND.PLTPOP.GT.0) THEN
              PCNVEG = (WTNLF+WTNST)/((LFWT+STMWT)*PLTPOP)*100  
          ELSE
              PCNVEG = 0.0
          ENDIF

          IF(STMWT.GT.0.0.AND.PLTPOP.GT.0.0) THEN
              PCNST = WTNST/(STMWT*PLTPOP)*100 
          ELSE                            !Concentration of N in stems
              PCNST = 0.0
          ENDIF

          IF(RTWT.GT.0.0) THEN
              PCNRT = ROOTN/RTWT*100  !Concentration of N in roots
          ELSE
              PCNRT = 0.0
          ENDIF

          !------------------------------------------------------------
          ! COMPUTE AVERAGE STRESS FACTORS DURING EACH GROWTH STAGE
          !------------------------------------------------------------
          !CHP 9/5/04 - these aren't currently being used anywhere.
          !CSD1 = CSD1 + 1.0 - SWFAC
          !CSD2 = CSD2 + 1.0 - TURFAC
          !CNSD1 = CNSD1 + 1.0 - NSTRES
          !CNSD2 = CNSD2 + 1.0 - AGEFAC
          !CPSD1 = CPSD1 + 1.0 - PSTRES1
          !CPSD2 = CPSD2 + 1.0 - PSTRES2
          ICSDUR = ICSDUR + 1

          !IF (ISWWAT .NE. 'N' .AND. ISTAGE .GT. 0) THEN
          !    IF(ICSDUR.GT.0) THEN
          !      SI1(ISTAGE) = CSD1  / ICSDUR
          !      SI2(ISTAGE) = CSD2  / ICSDUR
          !      SI3(ISTAGE) = CNSD1 / ICSDUR
          !      SI4(ISTAGE) = CNSD2 / ICSDUR
          !      SI5(ISTAGE) = CPSD1 / ICSDUR
          !      SI6(ISTAGE) = CPSD2 / ICSDUR
          !    ENDIF
          !ENDIF

          !------------------------------------------------------------
          ! Compute Variables Needed for Next Day's Pest Damage
          !------------------------------------------------------------

          AREALF = LAI *10000   !cm2/m2

!         Compute N lost in leaf senescence
!         N in senesced leaves - calculate based on today's N content
          CumLfNSenes = CumLfNSenes + 
     &               (CumLeafSenes - CumLeafSenesY) * STOVN / STOVWT

!!         TEMP CHP
!          IF (CumLeafSenes > 1.E-5) THEN
!            WNRat = CumLfNSenes/CumLeafSenes
!          ELSE
!            WNRat = 0.0
!          ENDIF
!          write(2000,'(2I7,7F10.3)') YRDOY, ISTAGE, WTLF, LFWT, GROLF, 
!     &    SLAN/600., CumLeafSenes, CumLfNSenes, WNRat
!!         TEMP CHP

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C                     DYNAMIC = OUTPUT
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN

          WTNUP = WTNUP + TRNU*PLTPOP  
              !Total N uptake, g/m2  Moved to SF_roots.for
          YIELD = GRNWT*10.0*EARS
          IF (GPP.GT. 0) THEN
            SKERWT = GRNWT/GPP
          ELSE
            SKERWT = 0.0
          ENDIF
          GPSM = GPP*EARS
          APTNUP = STOVN*10.0*PLTPOP
          TOTNUP = APTNUP

          IF (ISWNIT .EQ. 'N') THEN
              XGNP = 0.0
              GNUP = 0.0
          ELSE
              IF (GRNWT .GT. 0.0) THEN
                  XGNP = (GRAINN/GRNWT)*100.0
                  GNUP = GRAINN*EARS*10.0
              ENDIF
          ENDIF

          IF(ISTAGE.EQ.6) TOTNUP      = GNUP + APTNUP  
              !Need to track the logic for this...

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C                     DYNAMIC = FINAL
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.FINAL) THEN

        STOVER  = BIOMAS*10.0-YIELD
        YIELDB  = YIELD/62.8         
            ! Yield in bu/ac at 0% moisture content
        XANC   = TANC*100.0

        IF (ISWNIT .EQ. 'N') THEN
          XGNP = 0.0
          GNUP = 0.0
        ELSE
          IF (GRNWT .GT. 0.0) THEN
            XGNP = (GRAINN/GRNWT)*100.0
            GNUP = GRAINN*EARS*10.0
          ENDIF
        ENDIF

!       Senesced leaves do not fall to the ground and so are added to
!         surface litter only at harvest.
        SENESCE % ResWt(0)  = CumLeafSenes
!       Lignin - assume 7% - may want to put this in species file eventually
        SENESCE % ResLig(0) = 0.070 * CumLeafSenes
!       N in senesced leaves
        SENESCE % ResE(0,1) = CumLfNSenes

      ENDIF       !Endif for DYNAMIC LOOP

      RETURN

C----------------------------------------------------------------------
C     Format Strings
C----------------------------------------------------------------------

 2700 FORMAT (2X,'Crop mature on JD',I4,' due to slowed grain filling')
 2800 FORMAT (2X,'Crop growth failure due to cold stress on DAP---',i4)
 2801 FORMAT (2X,'Crop growth failure due to water stress on DAP---',i4)


      END SUBROUTINE SF_GROSUB

C=======================================================================
C  INCPLA, Function
C
C  Function to calculate daily plant leaf area growth
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                    F. Villalobos y A.J. Hall, July, 1989
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      4-8-96
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  :
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : GROSUB
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  None   :
C=======================================================================

      REAL FUNCTION INCPLA (LEAFNO,CUMPH,TEMPM,V1,TLNO)

      IMPLICIT NONE

      REAL     TTE,TEMPM,PLAG,A,B,K,N,LER,MAXLA,Z,V1,TFAC
      REAL     TLNOI,ZLNOI,YLNOI,X,XI,TLNO,CUMPH
      INTEGER  LEAFNO,I

      A    =   1.08
      B    =   5.23
      K    =   7.76
      N    =   1.13
      TTE  = 350.00
      PLAG =   0.00

      IF (TEMPM .LT. 4.0 .OR. TEMPM .GT. 40.0) THEN
         TFAC = 0.0
       ELSE
         IF (TEMPM .LT. 24.0) THEN
            TFAC = (TEMPM - 4.0)/20.0
          ELSE
            TFAC = 1.0 - (TEMPM - 24.0)/16.0
        ENDIF
      ENDIF

      TLNOI = IFIX (1.54 + 0.61*TLNO)
      ZLNOI = REAL (TLNOI)
      YLNOI = 150 + (ZLNOI - 6.0) *74.0

      DO I = CUMPH + 1, LEAFNO
         Z  = REAL(I)
         X  = Z/TLNO
         XI = (Z-1)/TLNO
         IF (Z .LE. 6.0) THEN
            MAXLA = Z * 25.0
          ELSE
            IF (Z .LE. ZLNOI) THEN
               MAXLA = 150.0 + (Z - 6.0) * 74.0
             ELSE
               MAXLA = YLNOI - 117.0 * (Z - ZLNOI)
            ENDIF
        ENDIF
        IF (MAXLA .LT. 0.0) THEN
           MAXLA = 1.0
        ENDIF
C       MAXLA = V1*A*((1+EXP(B-K*X))**(-1/N)-(1+EXP(B-K*XI))**(-1/N))
        LER   = MAXLA * TFAC / 18.0
        PLAG  = PLAG + LER
      END DO

      INCPLA = PLAG

      RETURN
      END
C=======================================================================
C  NGRAIN, Subroutine
C
C  Grain N
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                       F. Villalobos and A.J. Hall, July/89
C  2  Modified by
C  3. Header revision and minor changes               P.W.W.      4-8-96
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : STAGE5 STAGE4
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  None   :
C=======================================================================

      SUBROUTINE NGRAIN (NFAC,GPP,G3,GROEMB,O1,P5,OIL,GROPER,RANC,
     &       RMNC,XNGLF,GLFWT,XSTEMN,STMWT,HEADWT,STOVN,STOVWT,
     &       XLANC,XLMNC,XPEPE,XSMNC,XHMNC,RNOUT,SUMDTT,
     &       ROOTN,XLEAFN,VANC,PERN,EMBN,GRAINN,EMBWT,PERWT,NSINK,
     &       XSANC,XHANC,XRAT,YRAT,XHEADN,XNSLF,TURFAC,TEMPM,rtwt)

      IMPLICIT  NONE

c      INCLUDE  'GEN2.BLK'
c      INCLUDE  'GEN4.BLK'
c      INCLUDE  'NTRC1.BLK'

      REAL      NSINK,NSINK1,NSINK2,NPOOL1,NPOOL2,NPOOL,NSDR
      REAL      NPL1L,NPL1S,NPL1H,NPL2L,NPL2R
      REAL      PR,PO,OILINC,OILFRAC,FSINK1,FSINK2,XRAT,YRAT,RNOUT
      REAL      RONL,FACLN,FACSN,FACHN
      REAL      ENP,NFAC,PNP,SUMDTT,GROEMB,GPP,G3,O1,P5,OIL,EMBWT,PERWT
      REAL      GROPER,RMNC,GLFWT,XNGLF,XLANC,XSANC,STMWT,XSTEMN,XHEADN
      REAL      XHANC,HEADWT,VANC,STOVN,STOVWT,RANC,XLMNC,XPEPE,XSMNC
      REAL      XHMNC,RTWT,ROOTN,XLEAFN,XNSLF,PERN,EMBN,GRAINN
      REAL      TFAC,TEMPM,SFAC,TURFAC
      !
      ! Grain n allowed to vary between .02 and .038.
      ! High temp., low soil water, and high n increase grain n
      !
      SFAC   = 1.125 - 0.1250*TURFAC                      !
      TFAC   = 0.690 + 0.0125*TEMPM                       !
      ENP = (0.0225 + 0.0200*NFAC)*AMAX1(SFAC,TFAC)       ! SUN Emryo N conc.
      PNP = (0.0050 + 0.0100*NFAC)*AMAX1(SFAC,TFAC)       ! SUN Pericarp N conc.

      IF (GROEMB .GT. 0.0 .AND. SUMDTT .GT. 230.0) THEN
         PR = 1000.0*GROEMB/GPP/G3
         IF (PR .LE. 0.7) THEN
            PO = O1
          ELSE
C           PO = (1.0-(PR-0.7)*0.3/0.3)*O1
            PO = EXP(-1.4*(PR-0.8))*O1
         ENDIF
         OILINC  = PO/100.0 * GROEMB *(P5-170.0)/(P5-230.0)
         OIL     = OIL + OILINC
       ELSE
         OILFRAC = 0.0
      ENDIF

      NSINK = 0.1
      !
      ! Modified N conc in kernel to 4.25%
      !
      IF (EMBWT .GT. 0.0) THEN
C         NSINK1 = EMBWT*(0.0425-EMBN/EMBWT)
          NSINK1 = GROEMB*ENP
       ELSE
          NSINK1 = 0.0
      ENDIF

      IF (PERWT .GT. 0.0) THEN
C         NSINK2 = PERWT*(0.015-PERN/PERWT)
C         NSINK2 = PERWT*(PNP-PERN/PERWT)
          NSINK2 = GROPER*PNP
       ELSE
          NSINK2 = 0.0
      ENDIF
      NSINK1 = AMAX1 (NSINK1,0.0)
      NSINK2 = AMAX1 (NSINK2,0.0)
      NSINK  = NSINK1 + NSINK2

      IF (NSINK .NE. 0.0) THEN
         FSINK1 = NSINK1 / NSINK
         FSINK2 = NSINK2 / NSINK
         RANC   = AMAX1 (RANC,RMNC)                  ! Roots actual N conc.
C        XLANC  = XLEAFN / LFWT                      ! Leaf N conc.
         XLANC  = XNGLF  / GLFWT
         XSANC  = XSTEMN / STMWT                     ! Stem N conc.
         XHANC  = XHEADN / HEADWT                    ! Head N conc.
         VANC   = STOVN  / STOVWT                    ! Stover actual N conc.
         NPL1L  = GLFWT  * (XLANC - XLMNC)           ! Total N in leaf pool
         NPL1L  = AMAX1 (NPL1L,0.0)
         IF (NPL1L .GT. XPEPE) THEN
            NPL1L = NPL1L - XPEPE
            NPL2L = XPEPE
          ELSE
            NPL2L = NPL1L
            NPL1L = 0
         ENDIF
         NPL1S  = STMWT  * (XSANC - XSMNC)           ! Total N in stem pool
         NPL1S  = AMAX1 (NPL1S,0.0)
         NPL1H  = HEADWT * (XHANC - XHMNC)           ! Total N in head pool
         NPL1H  = AMAX1 (NPL1H,0.0)
         NPOOL1 = NPL1L  + NPL1S + NPL1H             ! Total N in stover
         NPL2R  = RTWT   * (RANC  - RMNC)
         NPL2R  = AMAX1 (NPL2R,0.0)
         NPOOL2 = NPL2R  + NPL2L                     ! Total N in roots pool
         NPOOL  = NPOOL1 + NPOOL2
         NSDR   = NPOOL  / NSINK                     ! Nitrogen supply/demand ratio

         IF (NSDR .LT. 1.0) THEN
            NSINK = NSINK*NSDR
         ENDIF

         XRAT = XNGLF / GLFWT
         YRAT = (0.009 - 0.0875*XRAT)/0.9125

         IF (NSINK .GT. NPOOL1 ) THEN
            XSTEMN = XSTEMN - NPL1S
            XHEADN = XHEADN - NPL1H
            STOVN  = STOVN  - NPOOL1
            RNOUT  = NSINK  - NPOOL1
            ROOTN  = ROOTN  - RNOUT*NPL2R/NPOOL2
            RANC   = ROOTN  / RTWT
            RONL   = RNOUT*NPL2L/NPOOL2 + NPL1L
            XLEAFN = XLEAFN - RONL
            XNGLF  = XNGLF  - RONL - RONL*YRAT/(XNGLF/GLFWT-YRAT)
            XNSLF  = XLEAFN - XNGLF
          ELSE
C-GH 9/7/1998
            IF (NPOOL1 .GT. 0.0) THEN
              FACLN  = NPL1L  / NPOOL1
              FACSN  = NPL1S  / NPOOL1
              FACHN  = NPL1H  / NPOOL1
            ELSE
              FACLN  = 0.0
              FACSN  = 0.0
              FACHN  = 0.0
            ENDIF
C-GH 9/7/1998
            XLEAFN = XLEAFN - FACLN * NSINK
            XNGLF  = XNGLF  - FACLN * NSINK *(1+YRAT/(XNGLF/GLFWT-YRAT))
            XNSLF  = XLEAFN - XNGLF
            XSTEMN = XSTEMN - FACSN * NSINK
            XHEADN = XHEADN - FACHN * NSINK
            STOVN  = STOVN  - NSINK
            VANC   = STOVN  / STOVWT
         ENDIF

         PERN = PERN + NSINK * FSINK2
         EMBN = EMBN + NSINK * FSINK1
      ENDIF

      GRAINN = EMBN + PERN

      RETURN
      END

C----------------------------------------------------------------------
C                         DEFINITIONS
C----------------------------------------------------------------------

! AGEFAC      !Nitrogen stress factor affecting expansion (0-1)
! ANO3        !Total extractable nitrate N in soil profile (kg N/ha)
! ANH4        !Total extractable ammonium N in soil profile (kg N/ha)
! APTNUP      !Nitrogen in stover (above ground biomass) kg N/ha
! AREALF      !Leaf area index, cm2 leaf/m2 ground
! ASMDOT      !Reduction in photosynthesis due to pests, %/day
! BIOMAS      !Above ground biomass, g/m2
! CANHT       !Canopy height, cm (not used)
! CANNAA      !Stover N at anthesis, g N/M2
! CANWAA      !Canopy weight at anthesis, g/m2
! CANWA       !Canopy width, cm (not used)
! CARBO       !Daily biomass production, g/plant/day
! CARBOT      !Consecutive days where CARBO is less than .001 before plant matures due to stress
! CMAT        !Counts consecutive days that CARBO is less than 0.001    
! CNSD1       !Cumulative nitrogen deficit factor on photosynthesis in 
!              each stage used to compute avg. stress during stage
! CNSD2       !Cumulative nitrogen deficit on growth in each stage used
!              to compute avg. stress during stage
! CUMPH       !Cumulative phyllochron intervals or fully expanded leaves
! CO2X(10)    !CO2 effect on photosynthesis, X axis is CO2 level, ppm
! CO2Y(10)    !CO2 effect on photosynthesis, Y axis is relative effect
! CO2         !Atmospheric CO2, ppm
! CSD1        !Cumulative water stress on photosynthesis during growth stage
! CSD2        !Cumulative water stress on growth during growth stage
! CURV        !Function to interpolate a value from a series of curves
! DLAYR(L)    Soil thickness in layer L (cm)
! DM          !Total above ground biomass, kg/ha
! DOY         !Day of year
! DTT         !Growing degree days today, C
! DYNAMIC     !Main control variable to tell each module which section of code to run
! EARS        !Ears per m2
! EARWT       !Ear weight, g/ear
! ECONO       !Ecotype number for the variety
! EMAT        !Flag used in grosub to determine if relative grain fill is below 0.1 for 2 consecutive days
! EOP         !Potential plant transpiration, mm/d
! EP1         !Potential plant transpiration, cm/d
! FAC(20)     !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! FINAL       !Program control variable to execute code to complet model run (value=6)
! FSLFW       !Fraction of leaf area senesced due to 100% water stress, 1/day
! FSLFN       !Fraction of leaf area senesced due to 100% nitrogen stress, 1/day
! G3          !Potential kernel growth rate mg/kernel/day
! GNP         !Nitrogen concentration in new grain growth, gN/g dry matter
! GNUP        !Total grain N uptake, kg N/ha
! GPP         !Grain number per plant, grains/plant
! GPSM        !Grain numbers, grains/m2
! GRAINN      !Grain nitrogen content, g N/plant
! GRF         !Fraction of today's carbon allocated to above ground biomass
! GRNWT       !Grain weight, g/plant
! GROEAR      !Daily growth of the ear - g
! GROGRN      !Daily growth of the grain - g
! GROLF       !Leaf growth rate, g/plant/day
! GRORT       !Root growth rate, g/plant/day
! GROSTM      !Stem growth rate, g/plant/day
! HI          Ratio of seed weight (SDWT) to weight of above-ground portion of 
!             plant (TOPWT) (g[seed] / g[tops])
! HIP         Ratio of pod weight (PODWT) to weight of above-ground portion of 
!             plant (TOPWT) (g[pods] / g[tops])
! I           !Counter
! ICOLD       !Cumulative number of days when TMIN is less than 6 C
! ICSDUR      !Calendar day accumulator for each growth stage   
! IDETO*1     !Screen output switch, (Y/N)
! IDURP       !Duration of ISTAGE 4, calendar days
! INTEGR      !Program control variable to execute code to integrate daily rate variables (value=4)
! ISTAGE      !Growth stage (integer)
! ISWNIT*1    !Nitrogen balance switch (Y/N)
! ISWWAT      !Soil water balance on/off switch (Y for yes, N for no)
! L           !Index counter
! LAI         !Leaf area index, m2/m2
! LAIDOT      !Leaf area consumed by pests, cm2 leaf area/m2/day
! LEAFNO      !Number of oldest leaf per plant (same as XN)
! LEAFNOE     !Number of oldest leaf at emergence
! LFWT        !Leaf weight, g/plant
! LFWTE       !Leaf weight at emergence, g/plant
! LIFAC       !Effect of row spacing and plant population on light interception
! LL(20)      !Lower limit of plant extractable water for soil layer L, cm3/cm3
! MAXLAI      !Maximum leaf area index, m2/m2
! NDEF3       !Nitrogen stress factor affecting grains per plant (0-1)
! NFAC        !Nitrogen stress factor based on actual and critical nitrogen content in vegetative tisue
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil
! NLAYR       !Number of soil layer
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil)
! NOUTDO      !File number
! NPOOL       !Total plant N available for translocation to grain (g/plant)
! NPOOL1      !Tops N available for translocation to grain (g/plant)
! NPOOL2      !Root N available for translocation to grain (g/plant)
! NSDR        !Plant N supply/demand ratio used to modify grain N content
! NSINK       !Demand for N associated with grain filling (g/plant/day)
! NSTRES      !Nitrogen stress factor affecting growth (0-1)
! NWSD        !No. of consecutive days with severe water stress
! OUTPUT      !Program control variable to output state and rate variables to output file (value=5)
! P5          !GDD from silking to physiological maturity, C
! PAR         !Daily photosynthetically active radiation, calculated as half
! PARSR       !Conversion of solar radiation to PAR, (MJ PAR/m2/d)/(MJ SRAD/m2/d)
! PC          !Used to compute fraction of phyllochron interval occurring today
! PCARB       !Potential dry matter production under optimum water, nitrogen and temperature, g/plant
! PCNGRN      !Percent nitrogen in grain, %
! PCNL        !Percent nitrogen in leaves, %
! PCNRT       !Percent nitrogen in roots, %
! PCNST       !Percent nitrogen in stems, %
! PCNVEG      !Percent nitrogen in vegetative tissue (leaf and stem), kg N/ha
! PCO2        !Effect of CO2 on plant growth rate
! PDWI        !Potential increment in new shoot growth, g/plant
! SLPF      !Relative reduction in growth due to poor fertility (0-1.0)
! SLPF      !Relative reduction in growth due to poor soil fertility (0-1.0) that is
                   !not related to nitrogen.
! PGRORT      !Potential increment in new root growth, g/plant
! PHINT       !Phyllochron interval. Number of GDD required for new leaf emergence, C.
! PLA         !Plant leaf area, cm2/plant
! PLAE        !Plant leaf area at emergence, cm2/plant
! PLAG        !Leaf area growth, cm2/plant
! PLAS        !The rate of senescence of leaf area on one plant - sq. cm/day
! PLTPOP      !Plant population, plants/m2
! PORMIN      !Minimum pore space volume required for supplying oxygen to roots for optimum
!             ! growth and function (0-100%) 
! PPLTD       ! Percent of plants destroyed today, %/m2/d
! PRFTC       !Array containing base, optimum and maximum temperature for function reducing photosynthesis due to temperature.
! PRFT        !Photosynthetic reduction factor for low and high temperatures
! PTF         !Ratio of above ground biomass to total biomass
! RANC        !Root actual N concentration, g N/g root
! RATE        !Program control variable to execute code to compute daily rate variables (value=3)
! RANCE       !Root nitrogen concentration at emergence, g N/g root dry weight
! RCNP        !Root critical nitrogen concentration, g N/g root dry weight
! RGFIL       !Array containing base, optimum and maximum temperature to compute RGFILL function
! RGFILL      !Rate of grain fill - mg/day
! RLV(20)     !Root length density for soil layer L, cm root/cm2 soil
! RLWR        !Root length weight ratio
! RMNC        !Root minimum nitrogen concentration (g N/g root dry weight)
! RNLAB
! RNOUT
! ROOTN       !Root nitrogen content, g N/plant
! RSGR        !Relative seed growth rate below which early maturity may occur after RSGRT days
! RSGRT       !Number of consecutive days where minimum relative seed growth rate (RSGR)is not
!              achieved. This results in early maturity due to slow seed growth. days
! RSTAGE      !ISTAGE as a real number for output
! RTWT        !Root weight, g/plant
! RTWTE       !Root weight at emergence, g/plant
! RTWTO       !Root weight, g/m2
! RUE         !Radiation use efficiency, g CH2O/MJ Par        
! ROWSPC      !Row spacing, cm
! RWUEP1      !Factor to modify water stress for cell expansion (in species file), mm/day
! RWUMX       ! Max root water uptake
! SAT(20)     !Saturated water holding capacity for soil layer L, cm3/cm3
! SATFAC      !Watterlogging stress factor (0-1.0)
! SDSIZE      !average mass of seeds (mg / seed)
! SDWT        !Seed weight, g/m2
! SEEDRV      !Carbohydrate reserve in seed, g/plant
! SEEDRVE     !Carbohydrate reserve in seed at emergence, g/plant
! SENLA       !Normal leaf senescence today, cm2/plant
! SFAC        !Drought stress factor for grain nitrogen concentration
! SHF(20)     !Relative root distribution in soil layer L (0-1)
! SHELPC      !Shelling percent
! SI1(6)      !Water stress during a growth stage used for output
! SI2(6)      !Water stress during a growth stage used for output
! SI3(6)      !Nitrogen stress during a growth stage used for output
! SI4(6)      !Nitrogen stress during a growth stage used for output
! SKERWT      !Weight per kernel, g/kernel
! SLA         !Specific leaf area, cm2/g
! SLAN        !Normal leaf senescence since emergence, cm2/plant
! SLFC        !Leaf senescence factor due to competition for light(0-1)
! SLFN        !Leaf senescence factor due to nitrogen stress (0-1)
! SLFT        !Leaf senescence factor due to temperature (0-1)
! SLFW        !Leaf senescence factor due to water sterss (0-1)
! SNH4(20)    !Ammonium nitrogen in soil layer L, kg N/ha
! SNO3(20)    !Nitrate content in soil layer L, kg N/ha
! SRAD        !Daily solar radiation, MJ/M2/day
! STGDOY(20)  !Year and day of year that a growth stage occurred on
! STMWT       !Stem weight, g/plant
! STMWTE      !Stem weight at emergence, g/plant
! STOVER      !Stover weight (leaf+stem), kg/ha
! STOVN       !Nitrogen content in stover, g N/plant
! STOVWT      !Stover weight (Stem + leaf), g/plant
! SUMDTT      !Sum of GDD for a given stage, C
! SUMEX       !Sum over all layers of water excess factor times depth
!             ! times root length density
! SUMP        !Cumulative plant growth during ISTAGE 4, g/plant
! SUMRL       !Sum of root length density (integral over depth)
! SW(20)      !Soil water content in layer L, cm3 water/cm3 soil
! SWEXF       !Soil water excess factor (0-1.0)
! SWFAC       !Soil water stress effect on growth (0-1), 1 is no stress, 0 is full    
! SWIDOT      !Seed loss due to pests, g seed/m2/day
! SWMAX       !Maximum stem weight (set in phasei) (move to grosub)
! SWMIN       !Minimum value stem weight can reach durign linear grain filling g/plant
! TABEX       !Table lookup function
! TANCE       !Nitrogen content in above ground biomass at emergence, g N/g dry weight
! TANC        !Nitrogen content in above ground biomass, g N/g dry weight
! TAVGD       !Average temperature during daylight hours, C
! TCNP        !Critical nitrogen concentration in tops, g N/g dry weight
! TEMPM       !Mean daily temperature, C
! TFAC        !Temperature stress factor for grain nitrogen concentration
! TI          !Fraction of a phyllochron interval which occurred as a fraction of today's daily thermal time
! TLNO        !Total number of leaves that the plant produces
! TMAX        !Maximum daily temperture, C
! TMNC        !Plant top minimum N concentration g N/g dry matter
! TNLAB       !Total potential daily plant water uptake, cm/d
! TMIN        !Minimum daily temperature
! TOTNUP      !Total shoot N uptake at maturity, kg N/ha
! TRNU        !Total potential root nitrogen uptake, kg N/ha
! TRWUP
! TSS(L)      !Total days soil layer L has been saturated in excess of PORMIN
! TURFAC      !Soil water stress effect on expansion (0-1), 1 is no stress, 0 is full stress
! UNH4(20)    !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)    !Plant uptake of nitrate from a layer (kg N/ha/day)
! VMNC        !Plant vegetative minimum nitrogen concentration, g N/g plant
! VANC        !Plant vegetative actual N concentration, g N/g plant
! WLIDOT      !Leaf loss due to pests, g/m2/day
! VSTAGE      !Vegetative growth stage (real number) used for output
! WRIDOT      !Root loss due to pests, g root/m2/day
! WSIDOT      !Stem loss due to pests, g stem/m2/day


! WTNCAN      !Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha
! WTNLF       !Weight of nitrogen in leaf tissue, kg N/ha
! WTNSD       !Weight of nitrogen in seed, g[N] / m2[ground]
! WTNST       !Weight of nitrogen in stem tissue, kg N/ha
! WTNUP       !Total N uptake, g/m2
! WTNVEG      !Weight of nitrogen in vegetative tissue, kg N/ha
! XANC        !Nitrogen concentration in above ground biomass %
! XHLAI       !Healthy leaf area index used to compute transpiration in water balance routine, m2/m2
! XLFWT       !New leaf weight today, g/plant
! XGNP        !Nitrogen content of grain, %
! XN          !Number of oldest expanding leaf
! XNF         !Modified nitrogen factor based on critical N concentration in vegetative biomass
! XNTI        !Number of leaves at tassel initiation
! XSTAGE      !Non-integer growth stage indicator
! YIELD       !Yield in kg/ha at 0% moisture content
! YIELDB      !Yield, bu/ac at 0% moisture content
! YRDOY       !Year and day of year


