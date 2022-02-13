C
C $Id: Rzmain.for,v 1.1 2002/08/27 23:59:42 rojas Exp $
C
      PROGRAM RZWQM2
      USE VARIABLE                                                       DEBASIS
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AEF        I  FIELD SATURATION FRACTION [0..1]
C       AIRR   L  AMOUNT OF IRRIGATION WATER TO APPLY
C       ANHYD  L  ANHYDROUS-NH3 POOL, USED TO STORE NH4 DURING THE
C                 APPLICATION DELAY PERIOD [MG/L]
C       ATMP   L  ATMOSPHERIC PRESSURE CORRECTED FOR ELEV [KPA]
C       BASE   L  BASAL AREA OF A PLANT [M^2]
C       BD         L  BULK DENSITY FOR CURRENT HORIZON [G/CM^3]
C       CC         L  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C                 CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C                 1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C                 5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C                 9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C                 12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CDSOIL     L  AMT OF CHEMICAL LEFT AFTER DISS IN THE SOIL PROFILE
C       CONCX2     I  PEST. CONC. INVOLVED IN KINETIC PROCESSES [UG/G-S]
C       COBIND    I/O CONCENTRATION OF IRREVERSIBLY BOUND POOL (UG/CC)
C       COPLNT     L  AMT OF PESTICIDE ON THE FOLIAGE [UG/CM^2]
C       CORES  L  AMT. CHEM. AVAIL. ON RESIDUE FOR DISSIPATION
C       CPSOL  L
C       DEGD   L  DEGREE DAYS FOR CURRENT DAY REFERENCE TEMP=0 [C].
C       EFFLUX     L  PLANT NITROGEN N-UPTAKE EFFICIENCY FACTOR
C       EK2        I  EQUIL.CONST. FOR ADSORPTION [CM3 H2O/G SOIL]
C       EPAN   L  DAILY PAN EVAPORATION [CM]
C       EWP        L
C       FIXN   L
C       FPW        L  FRACTION WATER FILLED PORE SPACE [0..100]
C       FRACOM     L  FACTION OF ORGANIC MATTER IN LAYERS [0..1]
C       FT         L
C       FTR        L
C       H      I  NODAL SOIL WATER PRESSURE HEADS [CM]
C       I      L  INDEX VARIABLE
C       IBRKTH     L
C       ICHEM  L  INDICATOR FLAG SIGNALLING IF EQUILIBRIUM CHEMISTRY IS USED
C                 (0) - DO NOT USE CHEMISTRY MODEL, USE DEFAULT VALUES
C                 (1) - USE CHEMISTRY MODEL
C       ICHEM  L  INDICATOR FLAG SIGNALLING IF PHOSPHROUS IS SIMULATED
C                 (0) - DO NOT USE PHOSPHORUS MODEL, USE DEFAULT VALUES
C                 (1) - USE PHOSPHORUS MODEL
C       IEVNT I/O LOGICAL CONTAINING TRUE WHEN AN IRRIGATION EVENT
C                 HAS TAKEN PLACE IN A DAY
C       IMAGIC     L  INDICATOR OF DEBUG OUTPUT
C       INP1   L  POINTER TO CNTRL.DAT DATAFILE
C       INP2   L  POINTER TO RZWQM.DAT DATAFILE
C       INP3   L  POINTER TO DAYMET.DAT DATAFILE
C       INP4   L  POINTER TO BRKPNT.DAT DATAFILE
C       INP5   L  POINTER TO RZINIT.DAT DATAFILE
C       INP6   L  POINTER TO PLGEN.DAT DATAFILE
C       IP         L  INDEX FOR PESTICIDE NUMBER
C       IPL        L  PLANT CURRENTLY MODELING [1..MXSPEC]
C       IPM        L
C       IYYY   L  --YEAR (4 DIGITS).
C       JBDAY  L
C       JDAY   L  JULIAN DAY    [1..366]
C       JEDAY  L
C       JGROW  L  --NUMBER OF DAYS SINCE PLANTING.
C                 AFTER HARVEST JGROW= -999.
C       JGS        L  [1..2];1:JULIAN DAY PLANT EMERGES
C                 2:JULIAN DAY PLANT IS RIPE
C       JN         L  INDEX VARIABLE
C       JTRM   L
C       LAI        L  LEAF AREA INDEX (LIVE LEAVES ONLY)
C       TLAI   L  LEAF AREA INDEX (LIVE AND DEAD LEAVES)
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MCPOR  I  TOTAL MACROPOROSITY OF HORIZON [CM3/CM3]
C       MDAY   L
C       MICP   I  TOTAL MICROPOROSITY OF HORIZON [CM3/CM3]
C       MX2        P
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXC        P
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXN        P
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXP        P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NHORZ I/O ORIGINAL NUMBER OF USER SPECIFIED HORIZONS
C       NITNHB     I  FLAG CONTROLLING NITRIFICATION SHUTDOWN DUE TO
C                 MANAGEMENT PRACTICE. (0=OFF, 1=ON)
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NOSTAR     L
C       NPEST  I  NUMBER OF USER DEFINED PESTICIDES
C       NSC        L  CURRENT OUTPUT SCENARIO NUMBER
C       ODMDN  L  SAVE DMDNIT USED FOR NITROGEN UPTAKE FROM TODAY
C       PACTIV     I  TRUE WHEN PESTICIDE IS ACTIVE
C       PLTSLV     L  NUMBER OF LIVE PLANTS [#/HA]
C       PPCO2  L  USER PROVIDED PARTIAL PRESSURE OF CO2 [ATM]
C       PTRANS     L  TOTAL POTENTIAL TRANSPIRATION (DIRECT + UNUSED
C                 SOIL EVAPORATION).[CM/DAY]
C       QEMPTY     I
C       QF         I  LAYER INTERFACIAL MOIST FLUXES [CM/HR]
C       RDF        L  FRACTIONAL DISTRIBUTION OF ROOTS BY LAYER [0..1]
C       RFDD   L  TOTAL RAINFALL DEPTH [CM]
C       RH         L  RELATIVE HUMIDITY [0..100]
C       RM         I  MASS OF RESIDUE (KG/HA)
C       ROP        L  TEMPORARY FOR PARTICLE DENSITY (G/CM^3)
C       RPOOL  L  RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       RTS        L  TOTAL S-W RADIATION INCOMING [MJ/M^2/DAY]
C       SATH   L
C       SDEAD  L  STANDING DEAD MATERIAL [KG/HA]
C       SDCN   L  STANDING DEAD C:N RATIO
C       SEVNT I/O LOGICAL CONTAINING TRUE WHEN AN SURFACE
C                 APPLICATION EVENT HAS TAKEN PLACE IN A DAY
C       SLKS   L  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       SOILPP     I  SOIL PHYSICAL PROPERTIES MATRIX
C                   (1):   T CODE- TEXTURE CODE (=0, USE USER HYD)
C                   (2):   RHOP  - PARTICLE DENSITY (G/CC)
C                   (3):   RHOB  - BULK DENSITY (G/CC)
C                   (4):   PHI   - POROSITY
C                   (5):   FSAND - FRACTION SAND PARTICLES
C                   (6):   FSILT - FRACTION SILT PARTICLES
C                   (7):   FCLAY - FRACTION CLAY PARTICLES
C                   (8):   FOM   - NOT USED
C       SOLTP1     I  ARRAY OF HEAT MODEL PARAMETERS,
C                  1: SAT MOISTURE CONTENT [0..1]
C                  2: FIELD CAPACITY [0..1]
C                  3: TEXTURE CLASS (1-COARSE, 2-MED, 3-FINE)
C                  4: # CONSTITUENTS FOR THERMAL PROPERTY CALC
C                  5: DRY VOL HEAT CAPACITY  [J/MM^3/C]
C       SPH        L
C       T      I  SOIL TEMPERATURE IN NUMERICAL CONFIGURATION [C]
C       TDAY   L
C       TEVNT I/O LOGICAL CONTAINING TRUE WHEN AN EVENT
C                 INVOLVING TILLAGE HAS TAKEN PLACE IN A DAY
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TH3        I  1/3 BAR VOLUMETRIC WATER CONTENT [CC/CC]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TLPLNT     L  TOTAL DAILY UPTAKE OF NITROGEN [KG-N/HA]
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       TMAX   L  MAXIMUM AIR TEMPERATURE [C]
C       TMIN   L  MINIMUM AIR TEMPERATURE [C]
C       TNITUP     I  TOTAL NITROGEN UPTAKE FOR WHOLE PROFILE [G/PLANT]
C       TPL        L
C       TQ         L
C       TRS        L
C       TSL        L
C       TSL1   L
C       TSL2   L
C       TSL2C  L
C       TSL3   L
C       TSLX   L
C       TSPLNT     L
C       TUP        L  WATER UPTAKE / LAYER [CM].
C       TWL        L
C       IRRTYP     L  --TYPE OF IRRIGATION, IE. SPRINKLER, FLOOD,
C                 FURROW, DRIP, or SUBSURFACE.
C       U      L  IONIC STRENGTH [MOLES/L]
C       UPNIT  L  UPTAKE OF NITROGEN FOR EACH LAYER [G/PLANT/LAYER]
C       W      L  HOURLY SOLAR CONSTANT = 5.016 [MJ/M^2/HR]
C       XMP        L
C       XNU        L
C       XPH        L
C       YESMAC     I  INDICATS PRESENCE OF MACROPORES =1 YES, =0 NOT
C       ZN         I  DEPTH TO NUMERICAL NODES [CM]
C
C
C       COMMENTS:
C
C
C       MASS STORAGE FILES:
C
C
C       EXTERNAL REFERENCES:
C                 BLKOUT
C                 BLOCK1
C                 CHEM
C                 CLOSER
C                 HYDPAR
C                 INIT
C                 INPDAY
C                 INPUT
C                 JDATE
C                 KDSTAR
C                 MAFERT
C                 MAIRR
C                 MAPEST
C                 MAPLNT
C                 MATILL
C                 NITBAL
C                 NUTRI
C                 OUTIN2
C                 PEMAIN
C                 PHYSCL
C                 SGATE
C                 TILADJ
C                 VGATE
C
C
C       CALLED FROM: MAIN PROGRAM MODULE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C=======================================================================
C
C   ARRAY DIMENSION VALUES
C
C-----------------------------------------------------------------------
C
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,MXCHEM=15,
     +    MXPEST=3,MXAPP=200,MXSPEC=10,MXANA=135,MHR=24)
      PARAMETER(MXTN=39,MXTNC=27,MXTP=19)
      PARAMETER(MXTSTP=5000,rhoi=9.2d2,rhol=1.0d3)
      PARAMETER (R2D=180.0D0/3.141592654D0,FCR=0.4d0)
C
      PARAMETER(MX2=MXNOD*2,MXC=MXNOD*MXCHEM,MXN=MXNOD*25,MXP=MXNOD*
     +    MXPEST,MAPA=MXAPP*4,MXTPP=MXPEST*MXTP)
C
C     ..HELP THE LINKER SEE THE BLOCK DATAS
      EXTERNAL BLOCK1,BLKOUT
C
C     VARIABLES PERTAINING TO SOIL HEAT TRANSPORT
      COMMON /HEAT/ CSH(MXNOD),T(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3)
C
C     VARIABLES PERTAINING TO SYSTEM DISCRETIZATION
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
C
      COMMON /PLNTG/ PLDEN(MXAPP),PLTDAY(MXAPP,2),ROWSP(MXAPP),
     +    PLDEPTH(MXAPP),sdwtplr(mxapp),sdager(mxapp),sprlapr(mxapp),
     +    PLNTSW(MXAPP),PLNTWD(MXAPP),LAYNDX(MXAPP),NPGS,NPR(MXAPP),
     +    NGPL(MXAPP),iptype(mxapp),iemrg(mxapp)
C
      DOUBLE PRECISION MCPOR
      INTEGER YESMAC,IHOUR, Pstr(MXSPEC)
C
c      common/ErosionLoss/CumulErodedSolute(6),enrich            !sab 1=po4, 2..4 = Pest#1..#3, 5=NH4/NO3
      common/gleams/rzrainfall,rzirrigation,rzrunoff
      common /Gleams_hydsol/gleams_clay,gleams_silt,gleams_por,   !chd  GLEAMS
     +        gleams_om1,gleams_sand,gleams_BulkDn,gleams_Dacre, 
     +        gleams_CHS,gleams_ei,gleams_exrain,thirtyRR,  
     +        IBYEAR,IEYEAR 
      common/po4log/ErosionActive,UseEnrichment
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
      COMMON /RZPEST/ HALFF(MXPEST),HALFFP(MXPEST),HALFFB(MXPEST),
     +    HALFR(MXPEST),HALFRP(MXPEST),HALFRB(MXPEST),HALFSSA(MXPEST),
     +    HALFSSV(MXPEST),HALFSSP(MXPEST),HALFSSB(MXPEST),
     +    HALFSA(MXPEST),HALFSN(MXPEST),HALFSB(MXPEST),HALFBD(MXPEST),
     +    XKH(MXPEST),XKOC(MXPEST),XMW(MXPEST),THETAR(MXPEST),
     +    TMPR(MXPEST),EA(MXPEST),EN(MXPEST),WALKERB(MXPEST),
     +    VMAX(MXPEST),DYIELD(4,MXPEST),FREUND(MXPEST),XPKA(MXPEST),
     +    XPKB(MXPEST),REFPH(MXPEST),CATKOC(MXPEST),ANKOC(MXPEST),
     +    PCOEFF(MXPEST),PPOWER(MXPEST),RCOEFF(MXPEST),RPOWER(MXPEST),
     +    XKOW(MXPEST),PBIND(MXPEST),IPCODE(4,MXPEST),IPANC(MXPEST),
     +    IPDEP(MXPEST),IPACTV(MXPEST),NPEST
C
      common/maxbal/IDmxH2O,IMmxH2O,IYYYmxH2O,IDmxN,IMmxN,IYYYmxN,
     +              IDmxC,IMmxC,IYYYmxC,IDmxP,IMmxP,IYYYmxP,
     +              DTMASSmxH2O,BalmxN,BalmxC,BalmxP,IWCINIT1
     +            ,ireset1
C
      COMMON /KNTCS/ CONCX2(MXNOD,MXPEST),EK2(MXNOD,MXPEST),
     +    RK2(MXPEST),RDIF(MXPEST),FEK2(MXPEST),IEK2(MXPEST)
C
      COMMON /RESID/ RM,RESAGE,CRES,HR,RCN,NADLY,NSDLY,NAREC,NSREC,WORM,
     +               SAI,HSTUBL,WSTUBL
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
      COMMON /NUTPAR/ R14,R23,R34,R43,R45,R53,R15,R25,EFFMAX,EFFNIT,
     +    EFFDEN,DENC,RPOP(3),ADCAY,ANIT,ADENIT,AUREA,ADEATH(3),CN(9),
     +    ADCAYV(5),XKP(3),AMETH,FRN2O_NIT,FRN2O_DEN,RNO_N2O,RAINNO,
     +    O2LIM,EFFMETH,ISBUG
C
      COMMON /PINFO/ TSOTP0(MXPEST),TAPAP(MXPEST),TLPRS(MXPEST),
     +    TLPCN(MXPEST),TLPSL(MXPEST),TLPRO(MXPEST),TLPLC(MXPEST),
     +    TDPPLNT(MXPEST),TLPDT(MXPEST),TLPDTD(MXPEST),
     +    TLPDTL(MXPEST),TLPDMA(MXPEST)
c
      COMMON /SC1/ ISTAB,IVTAB,ISPLT,IVPLT
C
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
      common/sumet/sumAET,sumCropET,sumPET,sumPT,sumAT,sumrain,deltheta
C
      DIMENSION CC(MXNOD,MXCHEM),BD(MXNOD),JGS(2),RPOOL(MXNOD,2),
     +    ROP(MXNOD),RDF(MXNOD),XPH(MXNOD),H2OTAB(2),
     +    SLKS(MXNOD,MXPEST,2),WTCC(MXNOD,MXCHEM),CORES(MXPEST),
     +    UPNIT(MXNOD),ANHYD(MXNOD),COPLNT(MXPEST),FRACOM(MXNOD),
     +    FPW(MXNOD),XNU(MXNOD,25),QNO3(MXNOD),XMP(MXNOD),
     +    CDSOIL(MXNOD),TQ(MXNOD),CPSOL(MXNOD),INXPL(MXSPEC),
     +    TUP(MXNOD),U(MXNOD),PPCO2(MXNOD),SPH(MXNOD),SATH(MXNOD),
     +    TSL2C(MXNOD,MXPEST),IQUE(4,MXAPP),EFFLUX(MXSPEC),
     +    IRTYPE(MXSPEC),OMSEA(MXANA),NUTEQ(13),IRRTYP(MXAPP),
     +    TH3(MXNOD),POR(MXNOD),COBIND(MXNOD,MXPEST),NYRP(MXPEST),
     +    TSOTPDAY(MXPEST),TSOTP00(MXPEST),TSOTP0YR(MXPEST)
     +    ,wsi(mxspec),erodedmass(6),totaladd(100),ssurft(mhr)
     +    ,FRACON(MXNOD), FRACOM1(MXNOD)
c     +    ,tempNo3(mxnod),tempnh4(mxnod)
C
      REAL DAYL,DEC,SNDN,SNUP,TWILEN,S0N,CLOUDS_DSSAT,WINDHR(MHR),
     &     RADHR(MHR),PARHR(MHR),RHUMHR(MHR),TAIRHR(MHR),AZZON(MHR), 
     &     BETA(MHR),FRDIFP(MHR),FRDIFR(MHR),TGRO(MHR),
     &     TGRODY,TAVG,TMDAY,TGROAV,isinb
C
      DIMENSION hrt(mhr),hrts(mhr),hru(mhr),hrh(mhr),HEPAN(MHR)
     +          ,hrzpar(mhr),HRTH(MHR),NPCDAY(500),NPCMON(500),
     +          NPCYEAR(500),PCHT(500),PLAI(500),PROOT(500),
     +          NPCJDAY(500),PWITH(500),PBIOMASS(500),PSTHT(500),
     +          PSTDEAD(500),PSTDIA(500),PFLAT(500),PSTAI(500)
      common/hourweather/ hrt,hrts,hru,hrh,HRZPAR,HRTH
      common /wsi/ wsi, alaireset, heightset, iresetlai, iresetht1
      LOGICAL CHLRFL,Erosionactive,UseEnrichment,FEXIST
      DOUBLE PRECISION LAI,TLAI,TN(MXTN),TNC(MXTNC),TPEST(MXTP,MXPEST)
      DOUBLE PRECISION METMOD(8,12), OHORTHK(MAXHOR),thxnu(9)
      INTEGER IPR
       character*3 mon(12)
       character*10 airrtype(5)
       integer DATE_TIME(8)
      character iswpar*1, scenname*255,string*255
C Liwang Ma
c      character*20 deldayjack/'del dayjack.out'/
      dimension ta(2),EXPD(1700,maxhor),IDAY(1700),IMONTH(1700),
     +         IYEAR(1700),dep(1700,maxhor),jdayexp(1700),
     +         mhorz(1700),ndxn2e(1700,mxnod),theta_old(mxnod),
     +         ilwater(1700,maxhor),JdayLAI(500),ExpLAI(500),
     +         JdayHT(500),ExpHT(500),idaylai(500),iiyear(500)
     +        ,idayht(500),IYEARCO2(500),CO2PPM(500)
       DATA MON /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &         ,'Aug','Sep','Oct','Nov','Dec'/
       data airrtype/'Sprinkler','Flood','Furrow','Drip','Subsurface'/
      DATA INP1,INP2,INP3,INP4,INP5,INP6,INP7 /10,11,12,13,14,15,18/
c Liwang Ma
      DATA JGS /366,366/,JGROW /0/,TDAY /0.0D0/,FT /1.0D0/,FTR /1.0D0/,
     +    CC /MXC*0.0D0/,BD /MXNOD*0.0D0/,RPOOL /MX2*0.0D0/,ANHYD /MXNOD
     +    *0.0D0/,ROP /MXNOD*0.0D0/,RDF /MXNOD*0.0D0/,LAI /0.0D0/,XPH /
     +    MXNOD*0.0D0/,ID /0/,IM /0/,CORES /MXPEST*0.0D0/,COBIND /MXP*
     +    0.0D0/,COPLNT /MXPEST*0.0D0/,FRACOM /MXNOD*0.0D0/,ALPH /0.5D0
     +    /,XNU /MXN*0.0D0/,QNO3 /MXNOD*0.0D0/,XMP /MXNOD*0.0D0/,CDSOIL
     +    /MXNOD*0.0D0/,TQ /MXNOD*0.0D0/,NITNHB /0/,TUP /MXNOD*0.0D0/,
     +    CPSOL /MXNOD*0.0D0/,IPL /0/,IPM /1/,UPNIT /MXNOD*0.0D0/,EWP /
     +    1.0D0/,U /MXNOD*0.0D0/,PPCO2 /MXNOD*0.0D0/,H2OTAB /99.D0,
     +    999.D0/,IQUE /MAPA*-1/,FIXN /0.0D0/,TSPLNT /0.0D0/,ODMDN /
     +    0.0D0/,SDEAD /0.0D0/,TSL2C /MXP*0.0D0/,CHLRFL /.FALSE./,OMSEA
     +    /MXANA*0.0D0/,HYDGRAD/0.0D0/,iswpar/'N'/,iplp/0/,tapmun/0.0d0/
      DATA ICNVG /0/,BIGCNV /0.0D0/,DEGD/0.0D0/,SDCN /40.0D0/,DELT /
     +    0.0D0/,FPW /MXNOD*59.0D0/,TLAI/0.0D0/,TN/MXTN*0.0D0/,JNUM/0/,
     +    NYR /0/,TH3 /MXNOD*0.0D0/,POR /MXNOD*0.0D0/,TNC/MXTNC*0.0D0/,
     +    snowpk/0.0d0/,SDEAD_HEIGHT/0.0D0/,HEIGHT/0.0D0/,IPR/1/,
     +    NYRC/0/,TPEST/MXTPP*0.0D0/,NYRP/MXPEST*0/,NPCJDAY/500*0/,
     +    NPCPOINT/0/,nuse/0/,erodedmass/6*0.0d0/,IPDEPTH/1/,
     +    ssurft/24*-273.0d0/,dayrain/0.0d0/,smelt_shaw/0.0d0/,
     +    tmday/0.0d0/,enrich/1.0d0/,trat/1.0d0/,fracon/mxnod*0.0d0/,
     +    TNSORB/0.0D0/ ! RM - Commonblock variables cannot be init in data statement.
C     
C
C      ==============================
C      INTRODUCE THE MODEL
C      ==============================
       Erosionactive = .false. ! RM -init commonblock here.
       UseEnrichment = .false. ! RM -init commonblock here.
       CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
      WRITE(*,1000)
	WRITE(*,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
C GET AVERAGE AIR TEMPEARTURE AS SOIL AVERAGE TEMP FOR THE SHAW MODEL 
c      CALL AVERAGES(INP3,TAAVG)
C
C=====================================
C     OPEN ALL THE INPUT FILES
C=====================================
C
      CALL OPNINP(INP1,INP2,INP3,INP4,INP5,INP6,INP7,JBDAY,JEDAY,IYYY,
     +    METMOD,JULBDAY,JULEDAY,IYB,IYE,iweather,scenname)
      CALL CDATE(JBDAY,ID,IM,IYYY)
      OPEN(64,FILE='DAYJACK.OUT',STATUS='UNKNOWN')
      WRITE(64,'(I8,3I6)') 1,ID,IM,IYYY
      CLOSE(64)
C===============================
C     READ IN ALL THE INPUT VALUES
C===============================
C
c      Hmin=-35000.0D0          !use specified minimum water suction for plant uptake and water movement.
      CALL INPUT(IRRTYP,INP1,INP2,NSC,TWL,INXPL,EFFLUX,IRTYPE,IBRKTH,
     +    ICHEM,IMAGIC,NOSTAR,NUTEQ,INP6,FTR,IPR,hydgrad,IHOURLY,ISHAW,
     +    IPENFLUX,iwzone,wsi,Hmin,iyb,istress,CO2A,JULBDAY,JULEDAY,niw,
     +    ipsw_depth,ipet,CKinit,CKmax,emitr0,pstr)

      sdead_height = hstubl   !set inital residue dead to given value 11-30-08
      sdead = wstubl          ! Wstubl is in kg/ha already
c      erosionactive=.true.    !sab
c      useenrichment=.false.   !sab
      PRINT*,' ====>  INITIAL VALUES READ IN '
C    for Gleams_ei calculation
c * * * * Adjusting rainfall energy for low intensities at high (North  ei
c         and South latitudes, greater than 45 degrees.  The adjustment ei
c         is based on Seppo Rekolainen work in Finland, except that a   ei
c         constant reduction factor is used instead of changing the     ei
c         coefficient and exponent in the equation.  wgk                ei
      if (xlat .ge. 0.0d0) then                                         ei
         cxlat = xlat*180.d0/3.1415d0                                   ei
      else                                                              ei
         cxlat = - xlat*180.d0/3.1415d0                                 ei
      endif                                                             ei
      if (cxlat .gt. 45.0d0) then                                       ei
         if (cxlat .gt. 65.0d0) then                                    ei
            fac_e = 0.20d0                                              ei
         else                                                           ei
            fac_e =(0.1462d0*cxlat)-(0.001693d0*cxlat*cxlat)-2.149d0    ei
         endif                                                          ei
      else                                                              ei
         fac_e = 1.0d0                                                  ei
      endif                                                             ei
      IF (CO2A.EQ.0.0D0) THEN
        ITOTYEAR=0
        INQUIRE (FILE = 'yearco2.dat', EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT=233, FILE='yearco2.dat',STATUS='UNKNOWN')
          DO WHILE (.NOT. EOF(233))
          READ (233,*) IYEARCO2(ITOTYEAR+1),CO2PPM(ITOTYEAR+1)
          IF (CO2PPM(ITOTYEAR+1).LT.280.D0) PRINT*, 'CO2 TOO LOW'
          IF (CO2PPM(ITOTYEAR+1).GT.1000.D0) PRINT*, 'CO2 TOO HIGH'
          ITOTYEAR=ITOTYEAR+1
        ENDDO
        IF (IYB.LT.IYEARCO2(1).OR.IYE.GT.IYEARCO2(ITOTYEAR)) STOP 
     &  'CORRECT CO2 CONCENTRATION IN YEARCO2.DAT FILE'
        ENDIF
        CLOSE(233)
      ENDIF
c      if (ishaw.eq.1.or.ipenflux.eq.1) then
	  INQUIRE (FILE = 'canopy.dat', EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT=222, FILE='canopy.dat',STATUS='UNKNOWN')
          CALL ECHO(222)
          READ (222,*) NPCPOINT,NUSE
          DO I=1,NPCPOINT
              READ (222,*) NPCDAY(I),NPCMON(I),NPCYEAR(I),PCHT(I)
     +             ,PWITH(I),PBIOMASS(I),PLAI(I),PROOT(I),PSTHT(I)
     +             ,PSTDIA(I),PSTDEAD(I),PSTAI(I),PFLAT(I)
              NPCJDAY(I)=JDATE(NPCDAY(I),NPCMON(I),NPCYEAR(I))
          pcht(i)=pcht(i)*100.d0
          pwith(i)=pwith(i)/100.d0
          proot(i)=proot(i)*100.d0
          pbiomass(i)=pbiomass(i)*1.0d4
          pstdead(i)=pstdead(i)*1.0d4
          pstdia(i)=pstdia(i)/100.d0
          pflat(i)=pflat(i)*1.0d4
          pstht(i)=pstht(i)*100.d0
          ipl=1
              DO J=IYB,NPCYEAR(I)-1
                NPCJDAY(I)=NPCJDAY(I)+JDATE(31,12,J)
              ENDDO
          ENDDO
          CLOSE (UNIT=222)
           else
            NPCPOINT=0
            NUSE=0
	  ENDIF
c	endif
C======================================================
C     INITIALIZE THE OUTPUT FILE SET FOR INITIAL DATA
C======================================================
C
      DO 10 I=1,NN
        JN=NDXN2H(I)
        BD(I)=SOILPP(3,JN)
        ROP(I)=SOILPP(2,JN)
        XMP(I)=MCPOR(JN)
        QNO3(I)=CC(I,9)
        PORI(I) = SOILHP(6,JN)*aef
   10 CONTINUE
      H2OTAB(1)=MXNOD*3
      H2OTAB(2)=MXTSTP*3
C
C======================================================
C     INITIALIZE ANY OF THE GLOBAL VARIABLES
C======================================================
C
      CALL INIT(INP5,JBDAY,CC,XNU,COPLNT,CORES,PPCO2,RPOOL,BD,RM,FRACOM,
     +    RPOP,iwcinit,FRACON,CN)
c      if (erosionactive) then
c      Call AccessAuxillaryParameters(Fracom(1))        !sab must follow the call to INIT
C     Set t soil variables  !chd      GLEAMS
c      IF (IPHOS.EQ.1) THEN  

C        Get SOM for each layer from nodes
          DO i=1, NN
          fracom1(i) = fracom(i)
         END DO
         CALL dbleMATCH (NN, TLT, fracom1, Nsoil, HORTHK)      !Zhiming trying to make org C dynamics for P modeling; dbleMatch in LMATCH.FOR
                                      
          DO i=1, Nsoil                       ! Zhiming - soil C is now from RZ model not a fixed value in RZ.int
          orgm(i) = fracom1(i)
          END DO
          
      CALL OPENPFILE       ! DEBASIS
      CALL Initializepvar  ! DEBASIS
      CALL WRITEHEADER     ! DEBASIS 
c      ENDIF      
      gleams_clay = max(0.01d0,soilpp(7,1))
      gleams_silt = max(0.01d0,soilpp(6,1))
      gleams_sand = max(0.01d0,soilpp(5,1))
      gleams_bulkdn = soilpp(3,1)
      gleams_por = soilpp(4,1)
      gleams_om1 = fracom(1)
      gleams_Dacre = AREA      ! one would think that 'acre' means acre, but it just means area, only used in RZ-Erosion.for::GLEAMSCalculatePeakRunoff
      gleams_CHS = tan(SLOPE)
cgleamstest        gleams_clay=0.13d0
cgleamstest        gleams_sand=0.66d0
cgleamstest        gleams_silt=0.21d0
c      gleams_EROOUT = 5
c      gleams_metflg = 1
C     Set GLEAMS field variables
c      Do i = 1,6
c       CumulErodedSolute(i) = 0.0D0                     !sab intialize cumul loss of sorbing solutes
c      enddo
c      endif
	  INQUIRE (FILE = 'GLEAMS.DAT', EXIST = FEXIST)
        IF (FEXIST.and.IMAGIC.LE.0) THEN
         OPEN(65,FILE='GLEAMS.DAT',STATUS='UNKNOWN') 
         Call Read_Gleams_Erosion_Input(65,ErosionActive)  !sab
         close (65)
         PRINT*,' ====>  GLEAMS Erosion Is Active'
        ENDIF
      PRINT*,' ====>  INITIAL PROFILES READ IN '
C
      DO I=1,NHOR
          OHORTHK(I)=HORTHK(I)
      ENDDO
C
C     ... ADJUST HORIZONS FOR TILLAGE IMPLIMENTS
      CALL TILADJ(NSC,NHORZ,Hmin)
C
C adjust pori after tillage
        DO 15 I = 1,NN
          IH = NDXN2H(I)
          PORI(I) = SOILHP(6,IH)*aef
15      CONTINUE
C     ..OPEN DEBUG FILES
c      OPEN(UNIT=333,FILE='ETSHAW.DAT',STATUS='UNKNOWN',ERR=20)
      OPEN(UNIT=98,FILE='WEATHERD.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (98,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      WRITE (UNIT=98,FMT=778) 
      OPEN(UNIT=89,FILE='WEATHERH.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (89,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      WRITE (UNIT=89,FMT=779) 
      OPEN(UNIT=109,FILE='DSSATWTH.WTH',STATUS='UNKNOWN',ERR=20)
C	  WRITE (109,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
C     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      WRITE (UNIT=109,FMT=789) XLAT*R2D,XLONG*R2D,INT(ELEV),XW,XW
c      
      OPEN(UNIT=70,FILE='MANAGE.OUT',STATUS='UNKNOWN',ERR=20)
	WRITE (70,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      OPEN(UNIT=9,FILE='MBLWAT.OUT',STATUS='UNKNOWN',ERR=20)
	WRITE (9,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
        OPEN(UNIT=74,FILE='MBLNIT.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (74,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=75,FILE='MBLCARBON.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (75,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=72,FILE='NUPTAK.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (72,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(72,IYYY)
        OPEN(UNIT=73,FILE='NUTRI.OUT',STATUS='UNKNOWN',ERR=20,RECL=300)
c	WRITE (73,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(73,IYYY)
      ENDIF
c changed by Liwang Ma to print out Accwat.out at any time, April 19, 2001
c     IF ((IMAGIC.LE.-1 .AND. IMAGIC.GE.-2).OR.IMAGIC.EQ.-10) THEN
      IF (IMAGIC.LE.-1) THEN
        OPEN(UNIT=77,FILE='ACCWAT.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (77,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(77,IYYY)
        OPEN(UNIT=78,FILE='CLEACH.OUT',STATUS='UNKNOWN',ERR=20,RECL=255)
	  WRITE (78,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(78,IYYY)
        OPEN(UNIT=79,FILE='MACRO.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (79,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=179,FILE='MACRO-P.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (179,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=279,FILE='MACRO-PC.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (279,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      ENDIF
      IF(IMAGIC.EQ.-3) THEN
        OPEN(UNIT=99,FILE='PROFILE.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (99,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(99,IYYY)
      ENDIF
      IF(IMAGIC.EQ.-7.OR.IMAGIC.EQ.-8) THEN
        OPEN(UNIT=90,FILE='PLANT0.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (90,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(90,IYYY)
        OPEN(UNIT=91,FILE='PLANT1.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (91,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(91,IYYY)
        OPEN(UNIT=92,FILE='PLANT2.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (92,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(92,IYYY)
        OPEN(UNIT=93,FILE='PLANT3.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (93,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(93,IYYY)
        OPEN(UNIT=94,FILE='PLANT4.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (94,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(94,IYYY)
        OPEN(UNIT=95,FILE='PLANT5.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (95,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(95,IYYY)
        OPEN(UNIT=96,FILE='PLPROD.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (96,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(96,IYYY)
        OPEN(UNIT=97,FILE='PLANT.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (97,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(97,IYYY)
      ENDIF
      IF(npest.gt.0) THEN
        OPEN(UNIT=50,FILE='PESTP.OUT',STATUS='UNKNOWN',ERR=20)  !contain all three pesticides
c	  WRITE (50,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
C       NAME = 'MBLP1_'+NMPEST[1]
        OPEN(UNIT=60,FILE='MBLP1.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (60,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=61,FILE='MBLP2.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (61,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=62,FILE='MBLP3.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (62,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=80,FILE='PEST1.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (80,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(80,IYYY)
        OPEN(UNIT=81,FILE='PEST2.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (81,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(81,IYYY)
        OPEN(UNIT=82,FILE='PEST3.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (82,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        CALL PRNTYR(82,IYYY)
      ENDIF
c tested by Liwang Ma
      IF ((IMAGIC .EQ. -11) .OR. (IMAGIC .EQ. -7) .OR.
     +   (IMAGIC .EQ. -8)) then
c      IF (IMAGIC .EQ. -11) THEN
        OPEN(UNIT=76,FILE='AVG6IN.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (76,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      ENDIF
        OPEN(UNIT=100,FILE='test.out',STATUS='UNKNOWN',ERR=20)
	  WRITE (100,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=101,FILE='Phenology.out',STATUS='UNKNOWN',ERR=20)
	  WRITE (101,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=299,FILE='RESIDUES.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (299,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=299,fmt=1189)
C
      if(ipenflux.eq.1.or.(ishaw.eq.1).or.ihourly.eq.1) then
        OPEN(UNIT=87,FILE='ebalance.out',STATUS='UNKNOWN',ERR=20)
c        OPEN(UNIT=88,FILE='weather.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=86,FILE='SHAWtemp.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=84,FILE='SHAWtotw.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=184,FILE='SHAWliqw.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=185,FILE='SHAWicew.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=186,FILE='SHAWdepth.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=187,FILE='SHAWEThr.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=284,FILE='SHAWCanWind.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=285,FILE='SHAWCanTemp.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=286,FILE='SHAWCanMoist.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=287,FILE='SHAWCanLWR.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=288,FILE='SHAWCanSWR.out',STATUS='UNKNOWN',ERR=20)
        OPEN(UNIT=289,FILE='SHAWLeafTemp.out',STATUS='UNKNOWN',ERR=20)
	  WRITE (87,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=87,fmt=287)
c	  WRITE (88,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
c	  write (unit=88,fmt=288)
	  WRITE (86,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=86,fmt=1186) (ZN(J), J=1,NN)
	  WRITE (84,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=84,fmt=1184) (ZN(J), J=1,NN)
	  WRITE (184,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=184,fmt=1184) (ZN(J), J=1,NN)
	  WRITE (185,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=185,fmt=1184) (ZN(J), J=1,NN)
	  WRITE (186,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=186,fmt=1187)
	  WRITE (187,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        write (187,fmt=889)
	  WRITE (284,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=284,fmt=1190) (J, J=1,11)
	  WRITE (285,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=285,fmt=1190) (J, J=1,11)
	  WRITE (286,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
	  write (unit=286,fmt=1190) (J, J=1,11)
	  WRITE (287,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        write (287,fmt=1190) (J, J=1,11)
	  WRITE (288,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        write (288,fmt=1190) (J, J=1,11)
	  WRITE (289,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        write (289,fmt=1190) (J, J=1,11)
      endif
        OPEN(UNIT=555,FILE='SHAWET.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (555,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        write (555,fmt=888)
        OPEN(UNIT=666,FILE='MASSBAL.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (666,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        OPEN(UNIT=6666,FILE='PHOTOSYN.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (6666,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
        WRITE (6666,'(a136)') 'YEAR-DOY   POTENTIAL_PHOTO  ACTUAL_PHOTO 
     &       TEMP_STRESS    WATER_STRESS     N_STRESS      CO2_FACTOR  
     &   FERTILITY_FACTOR   AVG_TEMP'
       WRITE (6666,'(11X,A30)') '=========== G/PLANT/DAY ============='
        OPEN(UNIT=777,FILE='STRESS.OUT',STATUS='UNKNOWN',ERR=20)
	  WRITE (777,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
      write (unit=777,fmt=717) 
	  INQUIRE (FILE = 'LOWERB.DAT', EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN(UNIT=188,FILE='LOWERB.DAT',STATUS='UNKNOWN',ERR=20)
        CALL ECHO(188)
        ENDIF
	  INQUIRE (FILE = 'EXPDATA.DAT', EXIST = FEXIST)
        IF (FEXIST) THEN
        OPEN (UNIT=189,FILE='EXPDATA.DAT',STATUS='OLD',err=23)
C  read the new addition to expdata.dat for horizon.out depth
      read (189,1123,err=27) string
      read (189,1123,err=27) string
          IF ((string(5:15).EQ.'Horizon.out') .or.
     +       (string(5:15).EQ.'Horizon.OUT') .or.
     +       (string(5:15).EQ.'Horizon.Out') .or.
     +       (string(5:15).EQ.'HORIZON.out') .or.
     +       (string(5:15).EQ.'HORIZON.OUT') .or.
     +       (string(5:15).EQ.'HORIZON.Out')) then
              read (189,1123,err=27) string
              read (189,1123,err=27) string
          else
             backspace (189)
             backspace (189)
             backspace (189)
             backspace (189)
          endif
        CALL ECHO(189)
        read (189,*) idexp
C
        IF (IDEXP.EQ.1) THEN
        CALL ECHO(189)
C
        read (189,fmt=1123) string
        k=inumb(string)
        backspace (189)
	 if (k.eq.1) then
        READ (UNIT=189,FMT=*,err=27) NEDATA  !, ITYPESWC  !,iwater
        itypeswc=0
        iwater=0
c       print*,'TAB is used in expdata.dat for 1st line soil moisture'
	 else if (k.eq.2) then
        READ (UNIT=189,FMT=*,err=27) NEDATA, ITYPESWC  !,iwater
        iwater=0
       else if (k.eq.3) then
        READ (UNIT=189,FMT=*,err=27) NEDATA, ITYPESWC, iwater
       endif
        read (189,fmt=1123) string
        kki=inumb(string)
        backspace (189)
        if (iwater.eq.1) then
        DO 35 K=1,NEDATA
           MHORZ(K) = 1
           I = 1
          if (kki.eq.5) then
           READ (UNIT=189,FMT=*,err=27) IDAY(K),IMONTH(K),
     +        IYEAR(K),DEP(K,1),EXPD(K,1)
          else if (kki.eq.6) then
           READ (UNIT=189,FMT=*,err=27) IDAY(K),IMONTH(K),
     +        IYEAR(K),DEP(K,1),EXPD(K,1),ilwater(K,1)
          endif
           jDAYexp(k) = JDATE(IDay(k),IMonth(k),IYear(K))
           DEPTHTRK = DEP(K,1)
255        IF (K.EQ.NEDATA) THEN
             READ (UNIT=189,FMT=1123,err=27) string
             IF (string(1:1) .EQ. '=') THEN
              GOTO 35
             ELSE
             BACKSPACE (UNIT=189)
             ENDIF
           ENDIF
              I = I + 1
              if (kki.eq.5) then
              READ (UNIT=189,FMT=*,err=27) IDUM1,IDUM2,IDUM3,
     +             DEP(K,I),EXPD(K,I)
              else if (kki.eq.6) then
              READ (UNIT=189,FMT=*,err=27) IDUM1,IDUM2,IDUM3,
     +             DEP(K,I),EXPD(K,I),ilwater(K,I)
              endif
           IF (DEP(K,I).GT.DEPTHTRK) THEN
              DEPTHTRK = DEP(K,I)
              MHORZ(K) = MHORZ(K) + 1
              GOTO 255
           ELSE
              BACKSPACE (UNIT=189)
              GOTO 35
           ENDIF
C  20      CONTINUE
 35     CONTINUE
       do k=1,nedata 
       JN=1
      DO 21 I=1,mhorz(k)
        DO J=JN,NN
          IF(dep(k,I).GE.TLT(J)) THEN
            NDXN2E(k,J)=I
          ELSE
            JN=J
            GOTO 21
          ENDIF
        enddo
   21 CONTINUE
       enddo
        ENDIF
        endif
        DO WHILE(.TRUE.)
        read (189,1123,err=27) string
c      IF (string(1:38).EQ.'=   Sampling DATE (DD-MM-YYYY), Height') then
c              read (189,1123,err=27) string
c              read (189,*) idexpHT
c        do i=1, idexpHT
c           READ (UNIT=189,FMT=*,err=27) IIDAY,IIMONTH,IIYEAR,ExpHT(i)
c           jDAYHT(i) = JDATE(IIDay,IIMonth,IIYear)
c        enddo
c        endif
        IF (string(1:20).EQ.'=YIELD,BIOMASS,PLANT') then
        read (189,1123,err=27) string
        read (189,*,err=27) i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12
        IF (I8.EQ.0.AND.I10.EQ.0) GOTO 145
        IF (I8.GT.0) THEN
        DO WHILE(.TRUE.)
              read (189,1123,err=27) string
        IF (string(1:36).EQ.'=   Sampling DATE (DD-MM-YYYY), Heig') then
              read (189,1123,err=27) string
              read (189,1123,err=27) string
              kki=inumb(string)
              backspace(189)
              if (kki.eq.1) then
              read (189,*) idexpht
              else
              read (189,*) idexpht,iresetht
              endif
       if (iresetht.eq.1) then  !reset LAI and Height
        do i=1, idexpht
           READ (UNIT=189,FMT=*,err=27) IIDAY,IIMONTH,IIYEAR(i),
     &        Expht(i),idayht(i)
           jDAYht(i) = JulDATE(IIDay,IIMonth,IIYear(i))
        enddo
       endif
        goto 144
        endif
        end do
        ENDIF
C
144     CONTINUE
        IF (I10.GT.0) THEN
        DO WHILE(.TRUE.)
              read (189,1123,err=27) string
        IF (string(1:33).EQ.'= Sampling DATE (DD-MM-YYYY), LAI') then
              read (189,1123,err=27) string
              read (189,1123,err=27) string
              kki=inumb(string)
              backspace(189)
              if (kki.eq.1) then
              read (189,*) idexplai
              else
              read (189,*) idexplai,iresetlai
              endif
       if (iresetlai.eq.1) then  !reset LAI and Height
        do i=1, idexplai
           READ (UNIT=189,FMT=*,err=27) IIDAY,IIMONTH,IIYEAR(i),
     &        ExpLAI(i),idaylai(i)
           jDAYLAI(i) = JulDATE(IIDay,IIMonth,IIYear(i))
        enddo
       endif
        goto 145
        endif
        end do
        else
        goto 145
        endif
        endif
        end do
145      continue        
        close (189)       
        endif
        ! chdTODO open sediment output file and write header
        ! year, month, day, surface_h2o, runoff_vol, excess_rain, avg_soil_loss, [p1_frac, p1_soil_loss, p1_soilconc (cm^3/cm^3)] for p1-p5
c      IF (ErosionActive) THEN
          OPEN(UNIT=654,FILE='SEDIMENT.OUT',STATUS='UNKNOWN',ERR=20)
          WRITE (654,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
          WRITE(654,99654)'date_yyyyddd   source     Rain+Irr_h2o_cm'//
     &    '   runoff_cm         Peak_RO_cm/hr '//
     &    '   avg_soil_loss_kg/ha  ' //
     &    '   avg_conc_kg/m^3  ' //
     &    '   p.002_frac   p.002_soil_loss_kg      p.002_conc_Kg/m^3' //
     &    '   p.010_frac   p.010_soil_loss_kg      p.010_conc_Kg/m^3' //
     &    '   p.030_frac   p.030_soil_loss_kg      p.030_conc_Kg/m^3' //
     &    '   p.300_frac   p.300_soil_loss_kg      p.300_conc_Kg/m^3' //
     &    '   p.200_frac   p.200_soil_loss_kg      p.200_conc_Kg/m^3' 
99654     FORMAT(A)
c      ENDIF
        
c     for ishutl>0    
c        OPEN(UNIT=88,FILE='weather.OUT',STATUS='UNKNOWN',ERR=20)
c	  WRITE (88,103) Mon(DATE_TIME(2)), DATE_TIME(3), 
c     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
1186    FORMAT ('  DY HR  YR     Tair   Tplas  Tres   Tcan ',
     &         300(F6.2,1x))
1190    FORMAT ('Units: Wind in m/s, Temperature in oC, Air Mositure in 
     &kg/m3, and Radiation in w/m2 (negative means leaving the canopy)'/
     & '                          Canopy Layers are defined per 0.5 LAI'
     &/,180('=')/ '  DY HR  YR',15(i14,1x))
1187    FORMAT ('  DY HR  YR         Frozen         ThawDep        
     &SnowDEP')
1189  FORMAT (' JDY   YR   DEPTH   RES_1(Surf)     C/N_R1    RES_2(SD)  
     &     C/N_R2       HUMUS_1     C/N_1       HUMUS_2     C/N_2     
     &   HUMUS_3    C/N_3     POP_1      C/N_P1       POP_2       C/N_P2
     &      POP_3      C/N_P3'/,98('='),' KG CARBON/HA  ',98("="))
1184    FORMAT ('  DY HR  YR   ',300(F6.2,1x))
  287 FORMAT (/33X,' SUMMARY OF ENERGY TRANSFER AT THE SURFACE',///,22X,
     *'LONG WAVE RADIATION',16X,'SHORT WAVE RADIATION',9X,
     *'TOTAL RAD',9x,'SENSIBLE HEAT FLUX',21X,
     *'LATENT HEAT FLUX',10X,'SOIL HEAT FLUX',2X,'ENERGY BALANCE'/,
     *15X,33('-'),2X,34('-'),2x, 9('-'),2x,34('-'),2x,34('-'),2x,
     *14('-'),2x,14('-')/,
     *' DAY HR  YR    CANOPY     SNOW   RESIDUE   SOIL ',
     *'  CANOPY    SNOW    RESIDUE    SOIL  ',
     *'            SOIL    RESIDUE   CANOPY   TOTAL  ',
     *'  SOIL    RESIDUE   CANOPY   TOTAL  ',
     *'      TOTAL          CLOSURE',/,12X,17('     W/M2'),1x,
     *1('        W/M2            W/M2'))
288     format ('day',1x,'time',1x,'year',2x,'TA',8x,
     +          'hum',8x,'wind',5x,'sunh')
888   format (9x,'SHAW E',9x, 'SHAW T',8x,'SHAW ET',8x,'SHAW PT',
     +       8x,'RZ PT',12x,'AE',12x,'AT',11x,'SHAW(E-AE)',8x,'RZ PE')
889   format (22x,'SHAW E',9x, 'SHAW T',8x,'SHAW ET',8x,'SHAW PT',
     +       8x,'S-W PT', 8x,'S-W PE', 8x,'Snow Depth',8x,'Act E',
     +       8x,'Act T')
c 777  FORMAT (I6,I4,I4,I6,10(F10.3))
c 778  FORMAT ('  YEAR ',' MON ',' DAY ',' DOY  ',T25,'TMIN',T35,'TMAX',
c     +        T45,'RAD',T55,'WIND',T65,'REL H',T75,'CO2',T87,'PAR')      
C-----------------------------
      GOTO 30
   20 STOP'DEBUG FILE NOT OPENED'
C
C=======================================
C     INITIALIZE NECESSARY SOIL PROPERTIES
C=======================================
C
       IF(iwcinit.EQ.1 .or.iwcinit.EQ.2) then
         nuteq(1)=0
         nuteq(2)=0
       endif
C     ..UPDATE SYSTEM STATES IF NEEDED
   30 IF(NUTEQ(1).EQ.1) CALL STATIN(XNU,NN,TL,BD,RPOOL,SOILHP,SOILPP,CC,
     +    CONCX2,THETA,T,RM,SDEAD,RCN,SDCN,NUTEQ,NDXN2H,H,AEF,SNOWPK,
     +    FRACOM,RPOP,FTR,FPW,SOLTP1,SOLTP2,FRACON,CN)  !,THETAI,PORI)
      IF(NUTEQ(13).EQ.1) CALL PLSTATREAD()  ! load plant state
C
C======================================================
C     INITIALIZE THE OUTPUT FILE SET
C======================================================
C
      DO 40 I=1,NN
        JN=NDXN2H(I)
        BD(I)=SOILPP(3,JN)
        ROP(I)=SOILPP(2,JN)
        XMP(I)=MCPOR(JN)
        QNO3(I)=CC(I,9)
   40 CONTINUE
      TDAY=DBLE(JBDAY-1)
      CALL VGATE(TDAY,1,TLT)
      CALL VGATE(TDAY,39,TUP)
      DO 50 IP=1,3
        CALL VGATE(TDAY,25+IP,CDSOIL)
   50 CONTINUE
      CALL VGATE(TDAY,20,BD)
      CALL VGATE(TDAY,21,XMP)
      CALL VGATE(TDAY,29,CC(1,13))
      CALL VGATE(TDAY,30,CC(1,14))
      CALL VGATE(TDAY,31,CC(1,15))
      CALL VGATE(TDAY,40,QF)
      CALL VGATE(TDAY,41,TQ)
      CALL VGATE(TDAY,2,THETA)
      CALL VGATE(TDAY,42,CC(1,12))
      CALL VGATE(TDAY,43,H)
      CALL VGATE(TDAY,44,CC(1,2))
      CALL VGATE(TDAY,45,CC(1,3))
      CALL VGATE(TDAY,46,CC(1,4))
      CALL VGATE(TDAY,47,CC(1,5))
      CALL VGATE(TDAY,48,CC(1,6))
      CALL VGATE(TDAY,49,CC(1,7))
      CALL VGATE(TDAY,50,CC(1,8))
      CALL VGATE(TDAY,52,FRACOM*0.58D0)
      CALL VGATE(TDAY,59,FRACON)
c     SEND ICE CONTENT AND POROSITY OUT  DO WE NEED TO CHANGE THE INTERFACE?  LIWANG MA
      CALL VGATE(TDAY,53,THETAI)
      CALL VGATE(TDAY,54,PORI)   
C  
      CALL VGATE(TDAY,3,T)
      CALL VGATE(TDAY,10,QNO3)
      CALL VGATE(TDAY,-1,QNO3)
      CALL SGATE(TDAY,-1,QNO3(1))
      CALL OUTIN2(NSC,IBRKTH,ICHEM,IMAGIC,NOSTAR,NUTEQ,INP1,IYYY,IPHOS)
      CALL VGATE(TDAY,-2,QNO3)
      CALL SGATE(TDAY,-2,QNO3(1))
      PRINT*,' ====>  OUTPUT SECTION INITIALIZED'
C
C     ..UPDATE HYDRAULIC PROPERTIES
      BOTHED=0.D0
      IF(ITBL.EQ.1) CALL WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QF(NN),DELT,
     +    TLT,ZN,IREBOT,BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,THETA)
C
      CALL HYDPAR(SOILHP,NDXN2H,NN,H,THETA,MAXHOR,AEF,ZN,H2OTAB,pori,
     +            Hmin)
        TSNO30=0.0d0
        TSNH40=0.0d0
      DO 60 I=1,NN
        JN=NDXN2H(I)
        BD(I)=SOILPP(3,JN)
        ROP(I)=SOILPP(2,JN)
        XMP(I)=MCPOR(JN)
        FPW(I)=1.0D2*THETA(I)/(1.0D0-SOILPP(3,JN)/SOILPP(2,JN))
        TH3(I)=SOILHP(7,JN)
        TSNO30=TSNO30+CC(I,9)*TL(I)*THETA(I)*0.1D0
        TSNH40=TSNH40+CC(I,10)*TL(I)*THETA(I)*0.1D0
   60 CONTINUE
C
C     ..INITIALIZE CHEMISTRY SYSTEM   
C   commented out equilibrium chemstry by Liwang Ma, 12-25-2013
      IF(ICHEM.EQ.1) THEN
        PRINT*,' ====>    INITIALIZING INORGANIC CHEMISTRY'
        CALL CHEM(NN,U,CC,BD,PPCO2,THETA,0.0D0,TLT)
        TSNO31=0.0D0
        TSNH41=0.0D0
        do i=1,nn
        TSNO31=TSNO31+CC(I,9)*TL(I)*THETA(I)*0.1D0
        TSNH41=TSNH41+CC(I,10)*TL(I)*THETA(I)*0.1D0
        enddo
        TNSORB=TSNH40+TSNO30-TSNH41-TSNO31
      ELSE
        DO 70 I=1,NN
          U(I)=0.01D0
C
C         ..SAVE PH SINCE NOT USING SOIL CHEMISTRY, THIS WILL BE CONST
          SPH(I)=-LOG10(CC(I,1))
   70   CONTINUE
      ENDIF
C
C     .. SET WIND TO BREEZE FOR FIRST TIME THROUGH
      W=100.0D0
C
C     ..INITIALIZE NUTRIENT CYCLING
      PRINT*,' ====>  INITIALIZING NUTRIENT CHEMISTRY'
      PRINT*
      PRINT*
      IF(NUTEQ(1).EQ.0) CALL NUTRI(NN,U,BD,PPCO2,T,THETA,XNU,CC,
     +    JBDAY-1,0,FRACOM,TL,RPOOL,XPH,W,TLT,IMAGIC,SDEAD,SDCN,DEGD,
     +    NITNHB,ANHYD,FPW,NUTEQ,TH3,ROP,snowpk,DAYRAIN,FRACON)
      DO 90 I=1,NN
        DO 80 J=9,10
          WTCC(I,J)=CC(I,J)*THETA(I)/BD(I)
   80   CONTINUE
   90 CONTINUE
      CALL VGATE(TDAY,16,XPH)
      CALL VGATE(TDAY,4,XNU(1,1))
      CALL VGATE(TDAY,5,XNU(1,2))
      CALL VGATE(TDAY,6,XNU(1,3))
      CALL VGATE(TDAY,7,XNU(1,4))
      CALL VGATE(TDAY,8,XNU(1,5))
      CALL VGATE(TDAY,9,WTCC(1,9))
      CALL VGATE(TDAY,11,WTCC(1,10))
      CALL VGATE(TDAY,17,XNU(1,7))
      CALL VGATE(TDAY,18,XNU(1,9))
      CALL VGATE(TDAY,19,XNU(1,8))
      CALL VGATE(TDAY,52,FRACOM*0.58D0)
      CALL VGATE(TDAY,59,FRACON)
      JDAY=JBDAY-1
      JTRM=JDATE(31,12,IYYY)
C
C output residue information
          ijk=0
          do ij=1,nhorz
          thxnu(1)=0.0d0
          thxnu(2)=0.0d0
          thxnu(3)=0.0d0
          thxnu(4)=0.0d0
          thxnu(5)=0.0d0
          thxnu(7)=0.0d0
          thxnu(8)=0.0d0
          thxnu(9)=0.0d0
              if (ij.eq.1) then
              write (299,199) jday,iyyy,0,rm*FCR,rcn,sdead*FCR,sdcn
              endif
          do i=ijk+1,nn
              if (tlt(i).le.ohorthk(ij)) then
              do j=1,5
              thxnu(j)=thxnu(j)+xnu(i,j)*TL(I)*BD(I)*0.1D0
              enddo
              Thxnu(7)=Thxnu(7)+XNU(I,7)/RPOP(1)*TL(I)*BD(I)*0.1D0
              Thxnu(8)=Thxnu(8)+XNU(I,8)/RPOP(2)*TL(I)*BD(I)*0.1D0
              Thxnu(9)=Thxnu(9)+XNU(I,9)/RPOP(3)*TL(I)*BD(I)*0.1D0
              ijk=i
              endif
c          write (299,199) jday,iyyy,tlt(i),((xnu(i,j)*conv1,cn(j)),j=1,5)
          enddo
        write (299,199)jday,iyyy,ohorthk(ij),((thxnu(j),cn(j)),j=1,5),
     &     ((thxnu(jk),cn(jk)),jk=7,9)
           ResdueC0 = rm*FCR                 !DEBASIS
           ResdueC1(ij) = thxnu(1)
           ResdueC2(ij) = thxnu(2)
           HumusC1(ij) = thxnu(3)
           HumusC2(ij) = thxnu(4)
           HumusC3(ij) = thxnu(5)
           ResdueP0 = rm*FCR/CP0
           ResdueP1(ij) = thxnu(1)/CPini(1)
           ResdueP2(ij) = thxnu(2)/CPini(2)
           HumusP1(ij) = thxnu(3)/CPini(3)
           HumusP2(ij) = thxnu(4)/CPini(4)
           HumusP3(ij) = thxnu(5)/CPini(5)       !DEBASIS
        ENDDO
C     ..INITIALIZE NITROGEN MASS BALANCE ROUTINE
      CALL NITBAL(NN,BD,THETA,XNU,JDAY,TL,RPOOL,CC,TNITUP,TSPLNT,IMAGIC,
     +    JDAY,ANHYD,IYYY,OMSEA,NYR,TN,JNUM,FIXN,TSOTN0yr,tsotn00,
     +    tsotnday,tapmun,erodedmass,tsotn00_in,TNSORB)
C
C     ..INITIALIZE CARBON MASS BALANCE ROUTINE
      CALL CARBAL(NN,BD,THETA,XNU,JDAY,TL,RPOOL,CC,TNITUP,TSPLNT,IMAGIC,
     +    JDAY,IYYY,OMSEA,NYRC,TNC,JNUMC,RM,TSOTCDAY,TSOTC0YR,TSOTC00
     +   ,erodedmass)
C     ..UPDATE KDSTAR FOR PESTICIDE PROCESSES
      CALL KDSTAR(SLKS,CC,NN,FRACOM,THETA,BD)
C
C     ..INITIALIZE PESTICIDE MASS BALANCE ROUTINE
C
C     IF (IMAGIC .EQ. -9) THEN
      DO 100 IP=1,NPEST
        CALL PESBAL(NN,JDAY,IP,BD,THETA,TL,CC,CORES,COPLNT,SLKS,CONCX2,
     +      TSL2C,IYYY,JDAY,OMSEA,IMAGIC,COBIND,FREUND,
     +      TSOTP0YR,TSOTP00,TSOTPDAY,TPEST,NYRP,erodedmass)
  100 CONTINUE
C     ENDIF
c Liwang Ma, to write out initial status for resetting 12-21-2007
      IF(iwcinit.EQ.1.or.iwcinit.EQ.2) CALL STATOT(XNU,NN,BD,SOILHP,
     +    SOILPP,CC,CONCX2,THETA,T,RM,SDEAD,RCN,SDCN,H,SNOWPK,FTR,
     +    SOLTP1,SOLTP2)  !,THETAI,PORI)
c
C     =========================================================
C     ==   S T A R T   D A I L Y   C A L C U L A T I O N S   ==
C     =========================================================
      TBOTM=T(NN)
c write down initial condition in the ANA file
      OMSEA(1)=DBLE(IYYY)+DBLE(JDAY)*1.0D-3
      DTSTOR=0.0D0
      DO I=1,NN
        if (ishaw.eq.1) then
        DTSTOR=DTSTOR+ TL(I) * (theta(i)+thetai(I)*rhoi/rhol)
        DTICE=DTICE+ TL(I) * (thetai(I)*rhoi/rhol)
        else
        DTSTOR=DTSTOR+TL(I)*THETA(I)
        endif
      enddo
      OMSEA(2)=DTSTOR
      CALL OUTMSEA(OMSEA)
C END OF INIITAL STATUS OUTPUT
C
      DO 200 MDAY=JBDAY,JEDAY
	  INQUIRE (FILE = 'LOWERB.DAT', EXIST = FEXIST)
        IF (FEXIST) THEN
28      READ (188,*) IID,IIM,IIY,DUMMT
                iidate=JDATE(IID,IIM,IIY)
              DO J=IYB,iiy-1
                iidate=iidate+JDATE(31,12,j)
              ENDDO
        IF (MDAY.EQ.iidate) THEN
           TBOTM=DUMMT
	     GOTO 18
        ELSE IF (MDAY.GT.iidate) THEN
           GOTO 28
        ELSE IF (MDAY.LT.iidate) THEN
           TBOTM=TBOTM+(DUMMT-TBOTM)/(iidate-MDAY)
           backspace (188)
        ENDIF
        ENDIF
C           
18        JDAY=JDAY+1
c this is to take input to SHAW
       IF (NPCPOINT.GT.0.and.nuse.eq.1) THEN
	    DO 111 J=1,NPCPOINT-1
             IF (MDAY.GE.NPCJDAY(J).AND.MDAY.LE.NPCJDAY(J+1)) THEN
                HEIGHT=PCHT(J)+(PCHT(J+1)-PCHT(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !plant height IN CM
                RTDEP=PROOT(J)+(PROOT(J+1)-PROOT(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !plant ROOTING DEPTH IN CM
                LAI=PLAI(J)+(PLAI(J+1)-PLAI(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   ! LAI
                TLAI=LAI                                     ! total LAI
                RM=PFLAT(J)+(PFLAT(J+1)-PFLAT(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !FLAT RESIDE IN KG/HA
                OMSEA(41)=PBIOMASS(J)+(PBIOMASS(J+1)-PBIOMASS(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !living biomass IN KG/HA
                WSTUBL=PSTDEAD(J)+(PSTDEAD(J+1)-PSTDEAD(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !standing RESIDE IN KG/HA, SAME AS WSTUBL
                HSTUBL=PSTHT(J)+(PSTHT(J+1)-PSTHT(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !standing RESIDE HEIGHT IN CM
                plwidth=Pwith(J)+(Pwith(J+1)-Pwith(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !PLANT LEAF WIDTH IN METERS
                RSDIA=PSTDIA(J)+(PSTDIA(J+1)-PSTDIA(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !STANDING RESIDUE DIAMETER IN METERS
                SAI=PSTAI(J)+(PSTAI(J+1)-PSTAI(J))/
     +         (NPCJDAY(J+1)-NPCJDAY(J))*(MDAY-NPCJDAY(J))   !STEM AREA INDEX, SIMILAR TO LAI
c               RTDEP=PROOT(J)*1.0d-2                         !ROOTING DEPTH, WHY CANNOT I USE THE ONE ABOVE? VSLOPE.FOR GAVE ME PROBLEM, LIWANG
               sdead=wstubl
               sdead_height=hstubl
               omsea(43)=LAI
               omsea(62)=height
               omsea(63)=RTDEP   !*100.D0
c              READ (222,*) NPCDAY(I),NPCMON(I),NPCYEAR(I),PCHT(I)
c     +             ,PLAI(I),PWITH(I),PBIOMASS(I),PSTDEAD(I),PSTDIA(I),
c     +              PFLAT(I),PROOT(I)
C       .. FIGURE TOTAL DEPTH (CM) THAT THE ROOTS EXTEND (FROM QUICK PLANT)
        CALL SHAW_RTDIST (NN,zn*1.0D-2,RTDEP*1.0d-2,RDF)
c  the followng was from Quickplant
cc        RTDEP=PROOT(J)
cc        IBOT=1
cc        ROOTBOT=0.0D0
cc        ISDP=3
cc        DO I=1,NN
cc          IF(TLT(I).LE.RTDEP) THEN
cc            IBOT=I
cc            ROOTBOT=TLT(I)
cc          ENDIF
cc        ENDDO       
C       FIND AREA UP TO THE PLANTING DEPTH
cc        DL=ROOTBOT*TLT(ISDP)
cc        RDF(1)=TLT(1)*TLT(1)/DL/TL(1)
cc        Y2=RDF(1)*TL(1)
cc        DO I=2,ISDP
cc          Y1=TLT(I)*TLT(I)/DL
cc          RDF(I)=(Y1-Y2)/TL(I)
cc          Y2=Y1
cc        ENDDO       
C       .. BELOW THE PLANTING DEPTH
cc        DDL=ROOTBOT*(ROOTBOT-TLT(ISDP))
cc        DO I=ISDP+1,IBOT
cc          Y1=1.0D0-(ROOTBOT-TLT(I))**2.0D0/DDL
cc          RDF(I)=(Y1-Y2)/TL(I)
cc          Y2=Y1
cc        ENDDO       
C       .. NORMALIZE THE ROOT DISTRIBUTION == 1
cc        TRDF=0.0D0
cc        DO I=1,IBOT
cc          TRDF=TRDF+RDF(I)
cc        ENDDO
cc        DO I=1,IBOT
cc          RDF(I)=RDF(I)/TRDF
cc        ENDDO
            ENDIF
  111   CONTINUE 
c       ELSE
c	   OMSEA(41)=PLANTBIOMASS
	   ENDIF      
c   end of plant inputs to SHAW
        If (ErosionActive) then
            CALL ResetGleamsOrganicFraction(FRACOM(1)) !sab
        Temperature_at_day_start = T(1)                !sab
        endif
        IF(JDAY.GT.JTRM) THEN
          JDAY=1
          IYYY=IYYY+1
          JTRM=JDATE(31,12,IYYY)
        ENDIF
        CALL CDATE(JDAY,ID,IM,IYYY)
        WRITE(*,1100) ID,IM,IYYY,JDAY,MDAY-JBDAY+1
C reset initial condition each year. Liwang Ma, 12-26-2007
      if ((iwcinit.eq.2). and. (jday.eq.jbday).and.(iyyy.ne.iyb)) then
c      if ((iwcinit.eq.2). and. (jday.eq.jbday)) then
         iwcinit1=1
         DO K=3,12
            NUTEQ(K) = 1
         ENDDO
         TOTHETA1=0.0D0
         DO I=1,NN
          totheta1=totheta1+theta(i)*tl(i)
         ENDDO
         CALL STATIN(XNU,NN,TL,BD,RPOOL,SOILHP,SOILPP,CC,
     +    CONCX2,THETA,T,RM,SDEAD,RCN,SDCN,NUTEQ,NDXN2H,H,AEF,SNOWPK,
     +    FRACOM,RPOP,FTR,FPW,SOLTP1,SOLTP2,FRACON,CN)  !,THETAI,PORI)
      PRINT*,' =======>  RESET INITIAL SOIL PROFILES at begining of run'
         TOTHETA2=0.0D0
         DO I=1,NN
          totheta2=totheta2+theta(i)*tl(i)
         ENDDO
          OMSEA(115)=totheta2-totheta1
          
      else
          iwcinit1=0
      endif
161        CALL DAYJACK(ID,IM,IYYY,0)
C
        TDAY=DBLE(MDAY)
        IF(ITBL.NE.1) ALPH=0.5D0
        IF(NSC.GE.1) CALL VGATE(TDAY,1,TLT)
C
C       ..FIGURE MSEA DATE CODE
        DDAY=DBLE(IYYY)+DBLE(JDAY)*1.0D-3
        OMSEA(1)=DDAY
C
        IF (Iweather.EQ.0) THEN
C       ..INPUT DAILY METEOROLOGY
        CALL INPDAY(TMIN,TMAX,RTH,W,EPAN,ATMP,RH,INP3,JDAY,IYYY,IMAGIC,
     +      METMOD,RZPAR,CO2R,iswpar,CO2A)
c        if (rzpar.eq.0.0d0) rzpar=rts*2.0d0  !convert from MJ/m2/d to mole/m2/d
C        WRITE (UNIT=98,FMT=777) IYYY,IM,ID,JDAY,TMIN,TMAX,RTS,
C     +         W,RH,CO2R,RZPAR
c estimate hourly weather using DSSAT approaches, LIWANG MA, RZ-PENFLUX, JUNE 2008
            CALL DAYLEN (JDAY, REAL(XLAT*R2D), DAYL, DEC, SNDN, SNUP) 
            CALL TWILIGHT (JDAY, REAL(XLAT*R2D), TWILEN)
            CALL SOLAR(DAYL, DEC, REAL(RTH), REAL(XLAT*R2D),     !Input
     &           CLOUDS_DSSAT, ISINB, S0N)                             !Output
            CALL HMET(CLOUDS_DSSAT, DAYL, DEC, ISINB, REAL(RZPAR),
     &    REAL(XW),SNDN, SNUP, S0N, REAL(RTH),           !Input
     &    real(TMIN), REAL(TMAX),REAL(TMIN),             !Input
     &    REAL(XW), REAL(W), REAL(XLAT*R2D),             !Input
     &    AZZON, BETA, FRDIFP, FRDIFR,
     &    PARHR, RADHR,                                   !Output
     &    RHUMHR, TAIRHR, TAVG, TMDAY,
     &    TGRO, TGROAV,                                   !Output
     &    TGRODY,WINDHR)                                  !Output
C        Ret_day_T=0.0d0
C        Ret_day_S=0.0d0
          DO I=1,MHR
             HRT(I) = DBLE(TAIRHR(I))
             HRH(I) = DBLE(RHUMHR(I))
             HRU(I) = DBLE(WINDHR(I))
             HRTH(I) = DBLE(RADHR(I))
             HRZPAR(I) = DBLE(PARHR(I))
C        CALL REF_ET(I,hrt(I),hrts(I),hrh(I),hru(I),elev,xlat*R2D,
C     +       Xlong*R2D,jday,ETr_hr_T,ETr_hr_S)
C        WRITE (UNIT=89,FMT=780) IYYY,IM,ID,JDAY,I,HRT(I),HRTS(I),
C     +       HRU(I),HRH(I),CO2R,HRZPAR(I),ETr_HR_T/10.0d0,ETr_HR_S/10.0d0
C        Ret_day_T=Ret_day_T+ETr_HR_T/10.0d0
C        Ret_day_S=Ret_day_S+ETr_HR_S/10.0d0
          ENDDO
C
        ELSEIF (Iweather.EQ.1) THEN
C       ..INPUT HOURLY METEOROLOGY
        CALL INPHOUR(TMIN,TMAX,RTH,W,EPAN,ATMP,RH,INP3,JDAY,IYYY,IMAGIC,
     +      METMOD,RZPAR,CO2R,iswpar,HRT,HRH,HRU,HRTH,HEPAN,hrzpar,CO2A)
            CALL DAYLEN (JDAY, REAL(XLAT*R2D), DAYL, DEC, SNDN, SNUP) 
            CALL TWILIGHT (JDAY, REAL(XLAT*R2D), TWILEN)
            CALL SOLAR(DAYL, DEC, REAL(RTH), REAL(XLAT*R2D),     !Input
     &           CLOUDS_DSSAT, ISINB, S0N)                             !Output
        ENDIF
        IF (CO2A.EQ.0.0D0) THEN
            DO I=1,ITOTYEAR
            IF (IYYY.EQ.IYEARCO2(I)) CO2R=CO2PPM(I)
            ENDDO
        ENDIF
C for climate change CO2 for soybean simulation in Railegh NC. 
c      81=year in future - current year in simulation. In our example, 2002 is 2083 in the future. 
c      CO2R=dexp(-201.947+0.2019*(IYYY+81)-4.9006d-5*(IYYY+81)**2)  !RCP 2.6 from 2083-2099
c      CO2R=dexp(-173.9853+0.1717*(IYYY+81)-4.0883d-5*(IYYY+81)**2)  !RCP 4.5 from 2083-2099
c      CO2R=dexp(56.7571-0.0555*(IYYY+81)+1.5022d-5*(IYYY+81)**2)  !RCP 6.0 from 2083-2099
c      CO2R=dexp(94.71-0.0959*(IYYY+81)+2.575d-5*(IYYY+81)**2)  !RCP 8.5 from 2083-2099
c  use SHAW to partition measured solar radiation to direct and diffuse for each hour to take accout aspect and slope effects
c  But it needs to be checked that RTS and HRTS are not corrected for slope and aspect again, especially when SHAW is used. GNF 12-4-2009
      RTS=0.0D0
C      NHRPDT=1
      CALL CLOUDY (CLOUDS,XLAT,DECLIN,HAFDAY,hrth,Jday,1)   !this is from SHAW
      do i=1,24
          SUNHOR = HRTH(i)
          IHOUR=I
          DTT=3600.D0
      CALL SOLAR_SHAW (DIRECT,DIFFUS,SUNSLP,ALTITU,SUNHOR,HAFDAY,
     >            DECLIN,IHOUR,DTT,XLAT,SLOPE,ASPECT)
c      CALL SOLAR_SHAW_new(DIRECT,DIFFUS,SUNSLP,ALTITU,SUNHOR,
c     >                  I,XLAT,Slope,ASPECT,jday,1)
       HRTS(I)=(DIRECT+DIFFUS)
       RTS=RTS+HRTS(I)*3.6d3/1.d6
        if (hrzpar(i).le.0.0d0) hrzpar(i)=hrts(i)*2.0d0  !the unit here is umol/m2/s from w/m2.
      enddo
        if (rzpar.eq.0.0d0) rzpar=rts*2.0d0  !from MJ/m2/d to mole/m2/d
c this is for daily calculation, but not working well use hourly as recommended by G. N. Flerchinger
c      CALL SOLAR_SHAW(DIRECT,DIFFUS,SUNSLP,ALTITU,rts/3.6d3*1.d6/24.0d0,
c     >                  24,XLAT,Slope,ASPECT,jday,24)
c      rts11=(DIRECT+DIFFUS)*3.6d3/1.d6*24.0d0
C
C   CACULATE REFERENCE ET FOR TOM TROUT
cc        if (ihourly.eq.1) then
cc        Ret_day_T=0.0d0
cc        Ret_day_S=0.0d0
cc        tdew=tmin
cc        DO I=1,24
cc        CALL REF_ET(I,hrt(I),hrt(I),hrts(I),hrh(I),hru(I),elev,xlat*R2D,
cc     +       Xlong*R2D,jday,ETr_hr_T,ETr_hr_S,trat,xw,ihourly)
cc        WRITE (UNIT=89,FMT=780) IYYY,IM,ID,JDAY,I-1,HRT(I),HRU(I),
cc     +    HRTS(I),HRH(I),CO2R,HRZPAR(I),ETr_HR_T/10.0d0,ETr_HR_S/10.0d0
cc        Ret_day_T=Ret_day_T+ETr_HR_T/10.0d0
cc        Ret_day_S=Ret_day_S+ETr_HR_S/10.0d0
cc	  ENDDO
cc        else
cc        CALL REF_ET(I,Tmin,Tmax,RTH,rh,w*1.0d3/(24.0d0*3.6d3),elev,
cc     +       xlat*R2D,Xlong*R2D,jday,ETr_hr_T,ETr_hr_S,trat,xw,ihourly)
cc        Ret_day_T=ETr_HR_T/10.0d0
cc        Ret_day_S=ETr_HR_S/10.0d0
cc        endif               
C
cc        WRITE (UNIT=98,FMT=777) IYYY,IM,ID,JDAY,TMIN,TMAX,W,RTS,
cc     +         RH,CO2R,RZPAR,ret_day_t,ret_day_s
        OMSEA(85)=TMIN
        OMSEA(86)=TMAX
        OMSEA(87)=(TMIN+TMAX)/2
c        OMSEA(88)=RTS
        OMSEA(89)=RH
        OMSEA(90)=W
C       ..UPDATE KD FOR PESTICIDE PROCESSES
        CALL KDSTAR(SLKS,CC,NN,FRACOM,THETA,BD)
C
C       ..MODIFY MANAGEMENT QUE
        CALL MAQUE(JDAY,THETA,NDXH2N,NDXN2H,SOILHP,TWL,JGS,IQUE,AIRR,NN,
     +      FRACOM,TLT,BD,CC,CHLRFL,INXPL,FT,IYYY,IRLOC,JGROW,RDF,IPL,
     +      pori,tapmun,iyb,irrtyp,airrtype,niw,ipsw_depth,TLCSHAW,
     +      TLEAFU,TLEAFL,HRT(14))
C
      if (irloc.gt.0) then
      if (irrtyp(irloc).eq.5) then
          subirr=airr
      else
          subirr=0.0d0
      endif
       endif

        if (smelt_SHAW.gt.0.0d0) airr=airr+smelt_SHAW  !add left-over snow melt from SHAW
c
C reset initial condition at planting. Liwang Ma, 10-19-2007
c      if ((iplp.eq.0).and.(ipl.eq.1).and.(iwcinit.eq.1)) then
      if ((iwcinit.eq.1)) then
        DO K=1,MXAPP
          IF(IQUE(1,K).EQ.18) THEN
            IPMr=IQUE(2,K)
            IPLr=NPR(IPMr)
          ENDIF
        enddo
      if ((iplp.eq.0).and.(iplr.ne.0).and.(jday.ne.jbday)) then
         iwcinit1=1
         DO K=3,12
            NUTEQ(K) = 1
         ENDDO
         TOTHETA1=0.0D0
         DO I=1,NN
          totheta1=totheta1+theta(i)*tl(i)
         ENDDO
         CALL STATIN(XNU,NN,TL,BD,RPOOL,SOILHP,SOILPP,CC,
     +    CONCX2,THETA,T,RM,SDEAD,RCN,SDCN,NUTEQ,NDXN2H,H,AEF,SNOWPK,
     +    FRACOM,RPOP,FTR,FPW,SOLTP1,SOLTP2,FRACON,CN)  !,THETAI,PORI)
      PRINT*,' ==============>  RESET INITIAL SOIL PROFILES at planting'
         TOTHETA2=0.0D0
         DO I=1,NN
          totheta2=totheta2+theta(i)*tl(i)
         ENDDO
          OMSEA(115)=totheta2-totheta1
          
      else
          iwcinit1=0
      endif
        iplp=iplr
	  iplr=0
      endif
C       ..MANAGE FERTILIZER APPLICATIONS
        CALL MAFERT(CC,THETA,AIRR,JDAY,NN,IQUE,NDXT2N,NITNHB,ANHYD,
     +             IYYY,subirr,cr,cs,ccl)
C
C       ..MANAGE PESTICIDE APPLICATIONS
        CALL MAPEST(FT,FTR,CC,THETA,JDAY,COPLNT,CORES,BD,SLKS,H,IQUE,
     +      AIRR,IYYY)
C
C       ..MANAGE MANURE APPLICATIONS
        CALL MANURE(CC,THETA,AIRR,JDAY,NN,TL,TLT,IQUE,RPOOL,RM,RCN,CN,
     +      IYYY,ANHYD,xnu,NDXN2H,subirr,cr,cs,ccl)
C
C       ..MANAGE TILLAGE OPERATIONS
        CALL MATILL(JDAY,SOILPP,RPOOL,SDEAD,SDCN,XNU,CC,THETA,SLKS,T,
     +      CONCX2,SOLTP1,SOILHP,IQUE,RM,RCN,EK2,CORES,BD,IYYY,FRACOM,
     +      FREUND,H,HWP,FRACON)
C adjust pori after tillage
        DO 12 I = 1,NN
          IH = NDXN2H(I)
          PORI(I) = SOILHP(6,IH)*aef
12      CONTINUE
C This WATBL was added to adjust water table if water table is in the tillage zone. Liwang Ma, 6-4-2009
      IF(ITBL.EQ.1) CALL WATBL(NN,NDXN2H,MAXHOR,SOILHP,H,QF(NN),DELT,
     +    TLT,ZN,IREBOT,BOTHED,BOTFLX,H2OTAB,ALPH,AEF,CAPZONE,THETA)
C       .. UPDATE HEAD AND THETA VALUES
        CALL HYDPAR(SOILHP,NDXN2H,NN,H,THETA,MAXHOR,AEF,ZN,H2OTAB,pori,
     +              Hmin)
C
C       ..UPDATE KD FOR PESTICIDE PROCESSES
        CALL KDSTAR(SLKS,CC,NN,FRACOM,THETA,BD)
C
c             KSS=1.0d0
             TAW=0.0
             DTAW=0.0
             DO I=1,NN
               JJ=NDXN2H(I)
               IF (RDF(I).GT.0.0) THEN
                TAW=TAW+(SOILHP(7,JJ)-SOILHP(9,JJ))*TL(I)
                DTAW=DTAW+(SOILHP(7,JJ)-THETA(I))*TL(I)
               ENDIF
             ENDDO
             IF (DTAW.GT.0.6D0*TAW) THEN
                KSS=(TAW-DTAW)/(TAW*(1.0-0.6D0))
             else
                KSS=1.0d0
             ENDIF
C the following is used for calculating crop ET using reference ET * crop coefficient
cc        if (ipl.eq.1) then
C calculate potential crop ET using crop coefficient approach
cc       Cover=1.0d0-exp(-0.594d0*LAI)   !from Gerald Flerchinger
c        CO=DEXP(-0.594D0*TLAI)   !this is used in RZWQM originally.
       Cover=1.0d0-exp(-0.594d0*LAI)   !from Gerald Flerchinger
       if (cover.lt.0.83d0) then    !LAI<3
cc       CKCC=0.12d0+1.05*cover    !from Tom Trout for Corn
       CKCC=CKinit+(CKmax-CKinit)/0.83*cover    !from Tom Trout for Corn
       else
cc       CKCC=0.96d0
       CKCC=CKmax
       endif
cc       CropET=Ret_day_T*CKCC  !*KSS
cc       sumCropET=CropET
cc        else
cc       CropET=0.0d0
cc       sumCropET=0.0d0
cc        endif         
C
C       ..CALCULATE PHYSICAL PROCESSES
        CALL PHYSCL(CC,TMIN,TMAX,EPAN,RTS,W,ATMP,INP4,JDAY,PTRANS,LAI,
     +      SLKS,TUP,COPLNT,CORES,FT,FTR,RDF,RFDD,IMAGIC,NSC,AIRR,TQ,
     +      IBRKTH,UPNIT,EWP,IRRTYP,MDAY,IPL,RH,IYYY,NOSTAR,XNU,ALPH,
     +      H2OTAB,RPOOL,ICNVG,BIGCNV,HEIGHT,CAPZONE,BD,FPW,INXPL,
     +      EFFLUX,IRTYPE,OMSEA,TLAI,SNOWPK,IRLOC,METMOD,IPR,
     +      SDEAD_HEIGHT,SDEAD,SDCN,hydgrad,ICHEM,jeday,tdew,npcpoint,
     +      nuse,IHOURLY,ISHAW,IPENFLUX,hrt,hrts,hru,hrh,fracom,clouds,
c     +      IHOURLY,ISHAW,IPENFLUX,hrt,hrts,hru,hrh,fracom,dble(clouds),
     +      iwzone,co2r,rth,hrth,wsi,plwidth,RSDIA,
     +      smelt_SHAW,TBOTM,Hmin,DAYRAIN,istress,subirr,SSURFT,IPDEPTH,
     +      jbday,trat,ipet,hrzpar,CKCC,EMITR0,fracon,TLCSHAW,
     +      TLEAFU,TLEAFL,CS,CR,CCL)
C
        OMSEA(88)=RTS        !move here so that estimated RTS can be ouput
        OMSEA(130)=HRT(14)   !Air temperature at 2:00 pm
        OMSEA(131)=TLCSHAW   !Current leaf temperature at 2:00 pm
        OMSEA(132)=TLEAFU    !Leaf temperature at non-transpiring at 2:00pm
        OMSEA(133)=TLEAFL    !Leaf temperature at full transpiring at 2:00 pm
        IF (((TLEAFU-HRT(14))-(TLEAFL-HRT(14))).NE.0.0D0) THEN
        OMSEA(134)=((TLCSHAW-HRT(14))-(TLEAFL-HRT(14)))/
     &             ((TLEAFU-HRT(14))-(TLEAFL-HRT(14)))     !Crop Water Stress Index at 2:00 pm
       ELSE
        OMSEA(134)=-1.0D0                                  !IS THIS CORRECT? SHOULD IT BE 0 OR 1?
       ENDIF
        
        DO 110 I=1,NN
          JN=NDXN2H(I)
c          SATH(I)=SOILHP(4,JN)
          SATH(I)=POINTK(H(I),SOILHP(1,JN),JN,pori(I))
          TH3(I)=SOILHP(7,JN)
          ROP(I)=SOILPP(2,JN)
          BD(I)=SOILPP(3,JN)
          POR(I)=SOILPP(4,JN)
          XMP(I)=MCPOR(JN)
  110   CONTINUE
C
C       ..DETERMINE IRR-REVERSIBLE BOUND POOL OF PESTICIDES
        CALL PEBIND(BD,CC,COBIND,NN,TL,SLKS,THETA)
C
C       ..FIND THE DEGRADATION OF THE ACTIVE PESTICIDES
        DO 140 IP=1,NPEST
          TPL=COPLNT(IP)
          TRS=CORES(IP)
          TSL=0.0D0
          DO 120 I=1,NN
C LIWANG MA, 7-3-2006
            TSL=TSL+CC(I,IP+12)/10.0D0*(THETA(I)*TL(I)+
     +          FSLKS(SLKS(I,IP,1),CC(I,IP+12),FREUND(IP),
     +          theta(i),bd(i))*
     +          BD(I)*TL(I))+CONCX2(I,IP)*BD(I)*TL(I)/10.0D0+
     +          COBIND(I,IP)*BD(I)*TL(I)/10.0D0
  120     CONTINUE
C
          IF(IPACTV(IP).EQ.1) THEN
            CALL PEMAIN(IP,COPLNT,CORES,CC,NN,T,THETA,TH3,ZN,BD,FPW,
     +          SLKS,TL)
          ENDIF
C
C         ..PREPARE STANDARD OUTPUT FROM PESTICIDE MODEL
          TSL1=0.0D0
          DO 130 I=1,NN
C LIWANG MA, 7-3-2006
            TSL1=TSL1+CC(I,IP+12)/10.0D0*(THETA(I)*TL(I)+
     +          FSLKS(SLKS(I,IP,1),CC(I,IP+12),FREUND(IP),
     +          theta(i),bd(i))*
     +          BD(I)*TL(I))+CONCX2(I,IP)*BD(I)*TL(I)/10.0D0+
     +          COBIND(I,IP)*BD(I)*TL(I)/10.0D0
            CDSOIL(I)=CC(I,IP+12)*FSLKS(SLKS(I,IP,1),CC(I,IP+12),
     +                FREUND(IP),theta(i),bd(i))
            CPSOL(I)=CC(I,IP+12)
  130     CONTINUE
          PSUR=(CC(1,IP+12)*(THETA(1)+
     +         FSLKS(SLKS(1,IP,1),CC(1,IP+12),FREUND(IP),
     +         theta(1),bd(1))
     +         *BD(1))+CONCX2(1,IP)*
     +        BD(1))/10.0D0+COBIND(1,IP)*BD(1)/10.0D0
C
C         ..DETERMINE AMOUNTS LOST TO DEGRADATION
          TLPRS(IP)=(TRS-CORES(IP))*1.0D-1
          TLPCN(IP)=(TPL-COPLNT(IP))*1.0D-1
          TLPSL(IP)=TSL-TSL1
          IF(NSC.GE.1) THEN
            CALL VGATE(TDAY,25+IP,CDSOIL)
            CALL VGATE(TDAY,28+IP,CPSOL)
            CALL SGATE(TDAY,25+IP,(CORES(IP)+COPLNT(IP))*0.1D0)
            CALL SGATE(TDAY,28+IP,PSUR)
            CALL SGATE(TDAY,31+IP,TSL1)
          ENDIF
  140   CONTINUE
C reset soil moisture if requested by users
        if (iwater.eq.1) then
           do k=1,nedata
            if ((iyear(k).eq.iyyy).and.(jdayexp(k).eq.jday)) then   !reset at end of day, not begining of day.
         totheta1=0.0d0
         totheta2=0.0d0
                DO I=1,NN
             theta_old(i)=theta(i)
                    IK=NDXn2h(I)
                    IH=NDXN2E(K,I)
        if ((expd(k,ih).ge.0.0d0).and.(ilwater(k,ih).gt.0)) then
            if (itypeswc.eq.1) then
             if (soilhp(9,ik).le.expd(k,IH)*bd(i))  then  !soilpp(3,ih)
              THETA(I)=expd(k,IH)*bd(i) 
              else
              THETA(I)=soilhp(9,ik) 
              endif
            else
             if (soilhp(9,ik).le.expd(k,IH)) then
              THETA(I)=expd(k,IH)
              else
              THETA(I)=soilhp(9,ik) 
              endif
            endif
         H(I)=WCH(H(I),theta(I),SOILHP(1,IK),IK,0)
         theta(I)=WC(H(I),SOILHP(1,IK),IK,0)
      endif
        totheta1=totheta1+theta_old(i)*tl(i)
        totheta2=totheta2+theta(i)*tl(i)
              enddo
       print*,'       ==> Reset soil water content from exp measurement'
          deltheta=totheta2-totheta1
          totalAdd(iyyy-iyb+1)=totalAdd(iyyy-iyb+1)+deltheta
c          CALL WCHEAD(THETA,H,SOILHP,NN,NDXN2H,MAXHOR)
          OMSEA(115)=DELTHETA
          CALL ADCONICE(SOILPP,THETA_old,theta,
     +           CC,SLKS,CONCX2,XNU,FREUND)
         ireset1=1
        goto 234
        else
            deltheta=0.0d0
            ireset1=0
            endif
           enddo
        else
            deltheta=0.0d0
            ireset1=0
        endif
c end of reset soil moisture
234    continue
c        
C
        IF(NSC.GE.1) THEN
          CALL SGATE(TDAY,43,TQ(NN))
          CALL SGATE(TDAY,82,1.0D0-FTR)
          CALL VGATE(TDAY,1,TLT)
          CALL VGATE(TDAY,20,BD)
          CALL VGATE(TDAY,21,XMP)
          CALL VGATE(TDAY,41,TQ)
          CALL VGATE(TDAY,2,THETA)
          CALL VGATE(TDAY,43,H)
          CALL VGATE(TDAY,3,T)
          CALL VGATE(TDAY,37,SATH)
          CALL VGATE(TDAY,38,U)
        ENDIF
C
        IF(NSC.GE.1) THEN
          CALL VGATE(TDAY,16,XPH)
          CALL VGATE(TDAY,4,XNU(1,1))
          CALL VGATE(TDAY,5,XNU(1,2))
          CALL VGATE(TDAY,6,XNU(1,3))
          CALL VGATE(TDAY,7,XNU(1,4))
          CALL VGATE(TDAY,8,XNU(1,5))
          CALL VGATE(TDAY,17,XNU(1,7))
          CALL VGATE(TDAY,18,XNU(1,9))
          CALL VGATE(TDAY,19,XNU(1,8))
          DO 170 I=1,NN
            DO 160 J=1,MXCHEM-3
              WTCC(I,J)=CC(I,J)*THETA(I)/BD(I)
  160       CONTINUE
  170     CONTINUE
          CALL VGATE(TDAY,9,WTCC(1,9))
          CALL VGATE(TDAY,11,WTCC(1,10))
          CALL VGATE(TDAY,42,WTCC(1,12))
          CALL VGATE(TDAY,44,WTCC(1,2))
          CALL VGATE(TDAY,45,WTCC(1,3))
          CALL VGATE(TDAY,46,WTCC(1,4))
          CALL VGATE(TDAY,47,WTCC(1,5))
          CALL VGATE(TDAY,48,WTCC(1,6))
          CALL VGATE(TDAY,49,WTCC(1,7))
          CALL VGATE(TDAY,50,WTCC(1,8))
          CALL VGATE(TDAY,52,FRACOM*0.58D0)
          CALL VGATE(TDAY,59,FRACON)
        ENDIF
C
c        do i=1,nn
c        tempno3(i)=cc(i,9)
c        tempnh4(i)=cc(i,10)
c        enddo
C       ..UPDATE CHEMISTRY SYSTEM
C commented out by Liwang Ma, 12-25-2013
        IF(ICHEM.EQ.1) THEN
        TSNO30=0.0D0
        TSNH40=0.0D0
        do i=1,nn
        TSNO30=TSNO30+CC(I,9)*TL(I)*THETA(I)*0.1D0
        TSNH40=TSNH40+CC(I,10)*TL(I)*THETA(I)*0.1D0
        enddo
          PRINT*,'==> UPDATING INORGANIC CHEMISTRY'
          CALL CHEM(NN,U,CC,BD,PPCO2,THETA,RFDD,TLT)
        TSNO31=0.0D0
        TSNH41=0.0D0
        do i=1,nn
        TSNO31=TSNO31+CC(I,9)*TL(I)*THETA(I)*0.1D0
        TSNH41=TSNH41+CC(I,10)*TL(I)*THETA(I)*0.1D0
        enddo
        TNSORB=TSNH40+TSNO30-TSNH41-TSNO31
C        do i=1,nn
C        tempno3(i)=cc(i,9)
C        tempnh4(i)=cc(i,10)
C        enddo
        ELSE
          DO 150 I=1,NN
            U(I)=0.01D0
            CC(I,1)=10.0D0**(-SPH(I))
            XPH(I)=SPH(I)
  150     CONTINUE
        ENDIF
C
C       ..DETERMINE DEGREE DAY FOR CURRENT DAY
        DEGD=MAX((TMAX+TMIN)*0.5D0,0.0D0)
C
C       ..UPDATE NUTRIENT CYCLING
        PRINT*,'==> UPDATING NUTRIENT CHEMISTRY'
        CALL NUTRI(NN,U,BD,PPCO2,T,THETA,XNU,CC,JDAY,NSC,FRACOM,TL,
     +      RPOOL,XPH,W,TLT,IMAGIC,SDEAD,SDCN,DEGD,NITNHB,ANHYD,FPW,
     +      NUTEQ,TH3,ROP,snowpk,DAYRAIN,FRACON)
C
C       ..OUTPUT OF 6" AVERAGE OF WATER, NO3-N AND PESTICIDES
c tested by Liwang Ma
      IF ((IMAGIC .EQ. -11) .OR. (IMAGIC .EQ. -7) .OR.
     +   (IMAGIC .EQ. -8)) then
c      IF (IMAGIC .EQ. -11) THEN
          CALL SIXAVG(NN,NPEST,JDAY,TL,TLT,THETA,SLKS,BD,CONCX2,CC,
     +         FREUND)
        ENDIF
C
c        IF(NSC.GE.1) THEN
c          CALL VGATE(TDAY,16,XPH)
c          CALL VGATE(TDAY,4,XNU(1,1))
c          CALL VGATE(TDAY,5,XNU(1,2))
c          CALL VGATE(TDAY,6,XNU(1,3))
c          CALL VGATE(TDAY,7,XNU(1,4))
c          CALL VGATE(TDAY,8,XNU(1,5))
c          CALL VGATE(TDAY,17,XNU(1,7))
c          CALL VGATE(TDAY,18,XNU(1,9))
c          CALL VGATE(TDAY,19,XNU(1,8))
c          DO 170 I=1,NN
c            DO 160 J=1,MXCHEM-3
c              WTCC(I,J)=CC(I,J)*THETA(I)/BD(I)
c  160       CONTINUE
c  170     CONTINUE
c          CALL VGATE(TDAY,9,WTCC(1,9))
c          CALL VGATE(TDAY,11,WTCC(1,10))
c          CALL VGATE(TDAY,42,WTCC(1,12))
c          CALL VGATE(TDAY,44,WTCC(1,2))
c          CALL VGATE(TDAY,45,WTCC(1,3))
c          CALL VGATE(TDAY,46,WTCC(1,4))
c          CALL VGATE(TDAY,47,WTCC(1,5))
c          CALL VGATE(TDAY,48,WTCC(1,6))
c          CALL VGATE(TDAY,49,WTCC(1,7))
c          CALL VGATE(TDAY,50,WTCC(1,8))
c          CALL VGATE(TDAY,52,FRACOM*0.58D0)
c        ENDIF
C
C
c      CALL CDATE(JDAY,ID,IM,IYYY)
      JULDAY=JULDATE(ID,IM,IYYY)
C       ..GROW CROPS
        IF (NPCPOINT.EQ.0.or.nuse.eq.0) THEN    !WHEN SHAW IS USED AND WITH INPUT PALNT LAI AND HEIGHT, THIS IS PASSED
      if ((iresetlai.eq.1).and.(LAI.gt.0.0d0)) then
      do k=1,idexplai-1
      if ((julday.ge.jdaylai(k)).and.(julday.le.jdaylai(k+1))) then
                LAI=explai(k)+(expLAI(k+1)-expLAI(k))/
     +         (jdaylai(k+1)-jdaylai(k))*(julday-jdaylai(k))   
          TLAI=LAI
c          omsea(43)=LAI
          ALAIreset=LAI
          GOTO 195
       else
          ALAIreset=0.0d0
       endif
       enddo
          endif
c
195     CONTINUE
        CALL MAPLNT(RFDD,JDAY,IYYY,TMAX,TMIN,RTS,TPP,W,HEIGHT,LAI,T,
     +      SOILPP,NDXN2H,RDF,JGS,NSC,PLTSLV,RPOOL,UPNIT,EWP,SDEAD,
     +      TLPLNT,XNIT,BASE,FIXN,IPL,IPM,JGROW,IMAGIC,ODMDN,CN,RM,
     +      RESAGE,RCN,SDCN,CORES,COPLNT,NPEST,CHLRFL,TDAY,IQUE,INXPL,
     +      IRTYPE,OMSEA,TLAI,IPR,SDEAD_HEIGHT,AIRR,CC,RZPAR,JBDAY,IYB,
     +      CO2R,ndxt2n,iswpar,dble(tmday),iweather,SSURFT,IPDEPTH,TUP,
     +      dble(DAYL))
     
        IF (ipl .gt. 0) THEN
            IF(Pstr(ipl) == 0) THEN   !zhiming RZP crop P stress
               Psts = .False.
            ELSE
             Psts = .True.
            ENDIF 
        ELSE
               Psts = .False.
        ENDIF
            
c         PLANTBIOMASS=OMSEA(41)
          RTDEP=OMSEA(63)
          if ((iresetht.eq.1).and.(Height.gt.0.0d0)) then
          do k=1,idexpht-1
      if ((julday.ge.jdayht(k)).and.(julday.le.jdayht(k+1))) then
                Height=expht(k)+(expht(k+1)-expht(k))/
     +         (jdayht(k+1)-jdayht(k))*(julday-jdayht(k))
          HEIGHTSET=HEIGHT
          omsea(62)=Height
          iresetht1=1
          goto 196
          else
          iresetht1=0
          endif
          enddo
         ENDIF
196     continue
         endif
                 
C       ..CLEAN UP THIS DAY'S EVENTS FROM QUE
        CALL MEQUE(IQUE)
C
c        If (.not.UseEnrichment) enrich = 1.0d0                           !sab if inactive, set it to 1 (non-enrichment)
c        CALL ApplyErosion_n_Report(Tday,SoLoss,Enrich,CC,SLKS,           !sab
c     &                 CONCX2,COBIND,BD,TL,THETA,XNU,CN,erodedmass)     !sab
c
      If (ErosionActive.and.rzrunoff.gt.1.0d-10) then  !sab
       RzRainfall=DAYRAIN
       DayMeanTemp = (Temperature_at_day_start + T(1))*0.5d0          !sab
      if (iweather.eq.0) then
      gleams_ei=7.87d0*((RzRainFall+RzIrrigation)/2.54d0)**1.51d0*fac_e !the ei for daily rainfall. otherwise use hourly rainfall ei calcualted in RZTEST.FOR
      else
      gleams_ei=gleams_ei*thirtyRR/100.d0
      endif
cc       gleams_exrain=gleams_exrain/43200d0
cgleamstest        gleams_exrain=0.56d0  
cgleamstest        gleams_ei=21.88d0
cgleamstest        CALL DailyErosion (SOLOSS,ENRICH,iyyy*1000+jday,29.50d0,     !sab arg=yyyyddd=sdate
cgleamstest     &                          5.0d0,0.5d0)        !sab
        CALL DailyErosion (SOLOSS,ENRICH,iyyy*1000+jday,DayMeanTemp,     !sab arg=yyyyddd=sdate
     &                          RzRainFall+RzIrrigation,RzRunoff)        !sab
        If (.not.UseEnrichment) enrich = 1.0d0                           !sab if inactive, set it to 1 (non-enrichment)
        CALL ApplyErosion_n_Report(Tday,SoLoss,Enrich,CC,SLKS,           !sab
     &         freund,CONCX2,COBIND,BD,TL,THETA,XNU,CN,erodedmass,ii)    !sab
        do ij=1,ii
        RPOOL(ij,1)=XNU(ij,1)/(1.0D6/(TL(ij)*BD(ij)))
        RPOOL(ij,2)=XNU(ij,2)/(1.0D6/(TL(ij)*BD(ij)))
        enddo
        OMSEA(107)=ERODEDMASS(1)
        OMSEA(108)=ERODEDMASS(2)
        OMSEA(109)=ERODEDMASS(3)
        OMSEA(110)=ERODEDMASS(4)
        OMSEA(111)=ERODEDMASS(5)
        OMSEA(112)=ERODEDMASS(6)
        OMSEA(113)=SOLOSS
cgleamstest        stop
        endif                                                              !sab
C ADD NODULE TO FAST HUMUS POOL 1
C       DO I=1,NN
C           XNU(I,3)=XNU(I,3)+NDTH*RDF(I)/C2BM*10.D0/TL(I)/BD(I)/0.1D0
C       ENDDO
C
C       ..DO DAILY NITROGEN BALANCE
        CALL NITBAL(NN,BD,THETA,XNU,JDAY,TL,RPOOL,CC,TLPLNT,TSPLNT,
     +      IMAGIC,MDAY,ANHYD,IYYY,OMSEA,NYR,TN,JNUM,FIXN,TSOTN0yr,
     +      tsotn00,tsotnday,tapmun,erodedmass,tsotn00_in,TNSORB)
C       ..DO DAILY CARBON BALANCE
        CALL CARBAL(NN,BD,THETA,XNU,JDAY,TL,RPOOL,CC,TLPLNT,TSPLNT,
     +      IMAGIC,MDAY,IYYY,OMSEA,NYRC,TNC,JNUMC,RM,TSOTCDAY,TSOTC0YR,
     +      TSOTC00,erodedmass)
C
C       ..DO DAILY PESTICIDE BALANCE
C       IF (IMAGIC .EQ. -9 .OR. IMAGIC.EQ.-12) THEN
        DO 180 IP=1,NPEST
          CALL PESBAL(NN,JDAY,IP,BD,THETA,TL,CC,CORES,COPLNT,SLKS,
     +         CONCX2,TSL2C,IYYY,MDAY,OMSEA,IMAGIC,COBIND,FREUND,
     +         TSOTP0YR,TSOTP00,TSOTPDAY,TPEST,NYRP,erodedmass)
  180   CONTINUE
        IF(IMAGIC.EQ.-9) THEN
          DO 190 I=1,NN
            WRITE(50,1200) TDAY,TLT(I),(TSL2C(I,IP),IP=1,MXPEST)
  190     CONTINUE
        ENDIF
C       ENDIF
C
C       ..DUMP MSEA OUTPUT VARIABLES IS NECESSARY
        OMSEA(72)=RM
        OMSEA(73)=SDEAD
        
        
        Mint = Tmin                          !DEBASIS
        Maxt = Tmax
        Rain = OMSEA(3)
        Runoff = OMSEA(12)
        Tempt = (Tmin+Tmax)*0.5D0
        Dgwt = OMSEA(61)*0.01D0
        Tiledrain = OMSEA(11)*0.01D0
        Sedi = OMSEA(113)*Areap*0.001D0
        PlntP = 0.0
        
        IF(OMSEA(13) >0.0 .AND. OMSEA(40)>0.0 .AND.OMSEA(64)>0.0) THEN 
         Potbiomass = (OMSEA(41)+OMSEA(42))/
     +                MIN(OMSEA(13),OMSEA(40),OMSEA(64))
         Potavgbiomass = OMSEA(41)/
     +                MIN(OMSEA(13),OMSEA(40),OMSEA(64))
        
         Potblgbiomass = OMSEA(42)/
     +                MIN(OMSEA(13),OMSEA(40),OMSEA(64))
         Potyeild = OMSEA(44)/
     +                MIN(OMSEA(13),OMSEA(40),OMSEA(64))
        ELSE          
         Potbiomass =  OMSEA(41)+OMSEA(42)
         Potavgbiomass = OMSEA(41)
         Potblgbiomass = OMSEA(42)
         Potyeild = OMSEA(44)
        ENDIF
      
        
        Avgrbiom = OMSEA(41)*Pstress(Slnc)
        Blgrbiom = OMSEA(42)*Pstress(Slnc)
        Yeild = OMSEA(44)*Pstress(Slnc)
        Actbiomass = OMSEA(41)*Pstress(Slnc)
     +                            + OMSEA(42)*Pstress(Slnc)
     
        Dlypotbiomsincrs =
     +                      MAX(Potbiomass-Dummy1,0.0)
        Dummy1 = Potbiomass  
        
        
        OMSEA(41) =  Avgrbiom
        OMSEA(42) =  Blgrbiom
        OMSEA(44) = Yeild
        
        Rootd = OMSEA(63)*0.01D0
        LAIP =  OMSEA(43)
        Plnth = OMSEA(62)*0.01D0
        Latflow = OMSEA(76)*0.01D0
        Deepsepg = OMSEA(10)*0.01D0
        
        
        Do i = 1, Nsoil
            Soilno3(i) = 0.0D0
            mmm(i) = 0.0D0
        END DO   
        
        DO i = 1, Nnode
          DO j= 1, Nsoil
           IF(Dnode(i)<=Dsoil(j)) THEN
            Soilno3(j)=Soilno3(j)+ WTCC(i,9)
            mmm(j) = mmm(j) + 1
            EXIT
           END IF        
          END DO
        END DO
        
        DO i=1, Nsoil
            Soilno3(i) = 
     +       (Tsoil(i)*Bdsoil(i)*Soilno3(i)*0.01)/ mmm(i)
        END DO
      
        Do i = 1, Nsoil
            Soiltemp(i) = 0.0D0
            mmm(i) = 0.0D0
        END DO
        
        DO i = 1, Nnode
          DO j= 1, Nsoil
           IF(Dnode(i)<=Dsoil(j)) THEN
            Soiltemp(j)= Soiltemp(j)+ T(i)
            mmm(j) = mmm(j) + 1
            EXIT
           END IF        
          END DO
        END DO
        
        DO i=1, Nsoil
            Soiltemp(i) = Soiltemp(i)/mmm(i)
        END DO
        
        DO i = 1, Nnode
         Soilwaternode(i) = Theta(i)
        END DO
        
        DO i = 1, Nsoil
            Soilwater(i) = 0.0D0
            mmm(i) = 0.0D0
        END DO
        
        DO i = 1, Nnode
          DO j= 1, Nsoil
           IF(Dnode(i)<=Dsoil(j)) THEN
            Soilwater(j)= Soilwater(j)+ Theta(i)
            mmm(j) = mmm(j) + 1
            EXIT
           END IF        
          END DO
        END DO
        
        DO i=1,Nsoil
            Soilwater(i) = Soilwater(i)/mmm(i)
        END DO
       
        DO i = 1,Nnode
            
            Matflow(i) = m12flow(i)
        END DO
        
        Macflow = m11flow*0.01D0
        
   !     WRITE(117,'(3I6,F10.5)') ID,IM,IYYY, Macflow(ID,IM,IYYY)
        
        IF(ID == Sday .AND. IM == Smon .AND. IYYY == Syear) THEN
            Flag = .True.
            PrevYear = Syear
        END IF                                !DEBASIS
        
         Tempu1 = ResdueP0 
         Tempu2 = ResdueC0
C output residue information
          ijk=0
          do ij=1,nhorz
          thxnu(1)=0.0d0
          thxnu(2)=0.0d0
          thxnu(3)=0.0d0
          thxnu(4)=0.0d0
          thxnu(5)=0.0d0
          thxnu(7)=0.0d0
          thxnu(8)=0.0d0
          thxnu(9)=0.0d0
              if (ij.eq.1) then
              write (299,199) jday,iyyy,0,rm*FCR,rcn,sdead*FCR,sdcn
              endif
          do i=ijk+1,nn
              if (tlt(i).le.ohorthk(ij)) then
              do j=1,5
              thxnu(j)=thxnu(j)+xnu(i,j)*TL(I)*BD(I)*0.1D0
              enddo
              Thxnu(7)=Thxnu(7)+XNU(I,7)/RPOP(1)*TL(I)*BD(I)*0.1D0
              Thxnu(8)=Thxnu(8)+XNU(I,8)/RPOP(2)*TL(I)*BD(I)*0.1D0
              Thxnu(9)=Thxnu(9)+XNU(I,9)/RPOP(3)*TL(I)*BD(I)*0.1D0
              ijk=i
              endif
c          write (299,199) jday,iyyy,tlt(i),((xnu(i,j)*conv1,cn(j)),j=1,5)
          enddo
        write (299,199) jday,iyyy,ohorthk(ij),((thxnu(j),cn(j)),j=1,5),
     &     ((thxnu(jk),cn(jk)),jk=7,9)
           ResdueC0 = rm*FCR
           ResdueC1(ij) = thxnu(1)           !DEBASIS
           ResdueC2(ij) = thxnu(2)
           HumusC1(ij) = thxnu(3)
           HumusC2(ij) = thxnu(4)
           HumusC3(ij) = thxnu(5)
           if (tempu2.gt.0.0d0) then
           ResdueP0 = Tempu1*ResdueC0/Tempu2
           else
           ResdueP0 = 0.0d0
           endif
           ResdueP1(ij) = thxnu(1)/CPini(1)
           ResdueP2(ij) = thxnu(2)/CPini(2)
           HumusP1(ij) = thxnu(3)/CPini(3)
           HumusP2(ij) = thxnu(4)/CPini(4)
           HumusP3(ij) = thxnu(5)/CPini(5)       !DEBASIS
           ENDDO
           
      IF (IPHOS.EQ.1) THEN
           CALL WRITELOG(ID,IM,IYYY)
           CALL TOTALP(ID,IM,IYYY,IniSoilP)
           CALL FERTILIZER(ID,IM,IYYY)
           CALL FERTILIZERFATE(ID,IM,IYYY)
           CALL MANUREP(ID,IM,IYYY)
           CALL MANUREFATE(ID,IM,IYYY) 
           CALL TILLAGE(ID,IM,IYYY)
           CALL PLANTPUPTAKE(ID,IM,IYYY,OMSEA(45))      
           CALL PFLUX(ID,IM,IYYY)
           CALL LABPLOSS(ID,IM,IYYY)      
           CALL DRPLOSSRUNOFF(ID,IM,IYYY)   
           CALL PPLOSSRUNOFF(ID,IM,IYYY)
           CALL DRPLOSSTILE(ID,IM,IYYY)
           CALL PPLOSSTILE(ID,IM,IYYY)
           CALL UPDATEPOOL(ID,IM,IYYY)
           CALL TOTALP(ID,IM,IYYY,FinalSoilP)
           CALL PBALANCE(ID,IM,IYYY)
           CALL WRITEOUTPUT(ID,IM,IYYY)
        
        DO i =1,Slnf
            Dummy2(i)=Fertpsorp(i)
        END DO                                ! DEBASIS
                    
       OMSEA(119) = Drplossrnf*1000.0
       OMSEA(120) = PPlossrnf*1000.0
       OMSEA(121) = Drplosstdrain*1000.0 
       OMSEA(122) = PPlosstdrain*1000.0
       OMSEA(123) = ADDPLANTP 
       OMSEA(124) = Drplosslatflow*1000.0
       OMSEA(125) = PPlosslatflow*1000.0
       OMSEA(126) = Drplossdesp*1000.0
       OMSEA(127) = PPlossdesp*1000.0
       OMSEA(128) = Pstress(Slnc)
       OMSEA(129) = PlntP
       else
       Pstress(Slnc) = 1.0d0
       ENDIF
      CALL OUTMSEA(OMSEA)   
        
C
C       ..DUMP REMAINING DATA IN VGATE AND SGATE
        IF(NSC.GE.1) THEN
          CALL SGATE(TDAY,95,RM)
          CALL SGATE(TDAY,96,RCN)
          CALL SGATE(TDAY,97,SDEAD)
          CALL SGATE(TDAY,98,SDCN)
          CALL SGATE(TDAY,103,Drplossrnf*1000.0)
          CALL SGATE(TDAY,104,PPlossrnf*1000.0)
          CALL SGATE(TDAY,105,Drplosstdrain*1000.0 )
          CALL SGATE(TDAY,106,PPlosstdrain*1000.0)
          CALL SGATE(TDAY,107,ADDPLANTP)    
          CALL VGATE(TDAY,-1,ZN)
          CALL SGATE(TDAY,-1,0.0D0)
        ENDIF
        
199       format (i5,i5,2x,f5.1,2x,20(2x,f10.3))
c zero out the erosion mass for next day
       do i=1,6
           erodedmass(i)=0.0d0
       enddo
           RzRainFall=0.0d0
           RzIrrigation=0.0d0
           RzRunoff=0.0d0
           soloss=0.0d0
c
  200 CONTINUE
      IF(ICNVG.GT.0) THEN
        WRITE(9,*) 'NUMBER OF TIMES RICHARDS" EQN. DID NOT CONVERGE: ',
     +      ICNVG
        WRITE(9,*) 'LARGEST MASS BALANCE ERROR: ',BIGCNV
      ENDIF
C
C     .. FINISH OFF THE NITBAL WRITES
      IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
        CALL CARBYR(NYRC,IYYY+1,JNUMC,TNC,0,TSOTC0YR,TSOTCDAY)
        CALL CARBYR(NYRC,IYYY,JNUMC,TNC,1,TSOTC00,TSOTCDAY)
        CALL NITBYR(NYR,IYYY+1,JNUM,TN,0,TSOTN0yr,TSOTNday)
        CALL NITBYR(NYR,IYYY,JNUM,TN,1,TSOTN00,TSOTNday)
      DO IP=1,NPEST
        CALL PESTBYR(NYRP,IYYY+1,JNUMC,TPEST,0,TSOTP0YR,TSOTPDAY,IP)
        CALL PESTBYR(NYRP,IYYY,JNUMC,TPEST,1,TSOTP00,TSOTPDAY,IP)
      ENDDO
      ENDIF
C
C     ..CLOSE ALL OUTPUT FILES
          CLOSE (654)
      CALL CLOSER(2)
      CLOSE(UNIT=70)
      IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
        CLOSE(UNIT=9)
        CLOSE(UNIT=74)
        CLOSE(UNIT=75)
        CLOSE(UNIT=72)
        CLOSE(UNIT=73)
      ENDIF
c changed by Liwang Ma
c      IF ((IMAGIC.LE.-1 .AND. IMAGIC.GE.-2).OR.IMAGIC.EQ.-10) THEN
      IF (IMAGIC.LE.-1) THEN
        CLOSE(UNIT=77)
        CLOSE(UNIT=78)
        CLOSE(UNIT=79)
      ENDIF
      IF(IMAGIC.EQ.-3) THEN
        CLOSE(UNIT=99)
      ENDIF
      IF(IMAGIC.EQ.-7.OR.IMAGIC.EQ.-8) THEN
        CLOSE(UNIT=90)
        CLOSE(UNIT=91)
        CLOSE(UNIT=92)
        CLOSE(UNIT=93)
        CLOSE(UNIT=94)
        CLOSE(UNIT=95)
        CLOSE(UNIT=96)
        CLOSE(UNIT=97)
      ENDIF
      IF(IMAGIC.EQ.-9) THEN
        CLOSE(UNIT=50)
        CLOSE(UNIT=60)
        CLOSE(UNIT=61)
        CLOSE(UNIT=62)
        CLOSE(UNIT=80)
        CLOSE(UNIT=81)
        CLOSE(UNIT=82)
      ENDIF
c tested by Liwang Ma
      IF ((IMAGIC .EQ. -11) .OR. (IMAGIC .EQ. -7) .OR.
     +   (IMAGIC .EQ. -8)) then
c      IF (IMAGIC .EQ. -11) THEN
        CLOSE(UNIT=76)
      ENDIF
C EDITED BY LIWANG MA
c      IF(IMAGIC.EQ.-12) THEN
        CLOSE (UNIT=83)
	  close (unit=84)
	  close (unit=86)
	  close (unit=87)
c	  close (unit=88)
c      ENDIF
C
C     ..PRINTOUT RZINIT.DAT SPECIFIC INFORMATION FOR WATER, TEMPERATURE,
C     AND NUTRIENT INITIALIZATION.  PARTICULARILY USEFUL FOR INITIALIZATION
C     OF LONGTERM RUNS.
      CALL NEWINIT(NHOR,NDXH2N,TL,HORTHK,THETA,T,XNU,RPOOL,CC,SOILPP,
     +    NHORZ,H)
      IF(NUTEQ(2).EQ.1) CALL STATOT(XNU,NN,BD,SOILHP,SOILPP,CC,CONCX2,
     +    THETA,T,RM,SDEAD,RCN,SDCN,H,SNOWPK,FTR,SOLTP1,SOLTP2)
      IF(NUTEQ(13).EQ.1) CALL PLSTATWRITE()
c     +    THETAI,PORI)
C
C Edited by Liwang Ma 1/14/08 1.3 release, use inital BD for comparison. 4/21/2008
      DO I=1,NN
        JN=NDXN2H(I)
        BD(I)=SOILPP(3,JN)
      enddo
c       itime=etime(ta)
c       print*,'total time used in seconds',itime
C Edited by Liwang Ma
C Edited by Liwang Ma
       write (unit=666, fmt=667) IDmxH2O,IMmxH2O,IYYYmxH2O,
     +        JDATE(IDmxH2O,IMmxH2O,IYYYmxH2O),DTMASSmxH2O
       write (unit=666, fmt=669) IDmxC,IMmxC,IYYYmxC,
     +        JDATE(IDmxC,IMmxC,IYYYmxC),BalmxC
       write (unit=666, fmt=668) IDmxN,IMmxN,IYYYmxN,
     +        JDATE(IDmxN,IMmxN,IYYYmxN),BalmxN
       IF (NPEST.GT.0) 
     + write (unit=666, fmt=670) IDmxP,IMmxP,IYYYmxP,
     +        JDATE(IDmxP,IMmxP,IYYYmxP),BalmxP
       write (unit=666,fmt=671) (totalAdd(i),i=1,iyyy-iyb+1)
       close (inp5)
	 close (666)
       close (98)
       CLOSE (777)
       CLOSE (555)
       CLOSE (188)
       close (6666)
       close(101) ! RM added close to the Phenology file (it is opened in COMP2EXP)
c       status=system(deldayjack)
c         CALL DAYJACK(0,0,0,0)
c
c      CALL Horizon(NN,JBDAY,JULBDAY,JULEDAY,TLT,DRSPAC,BD,Ohorthk,NHORZ)
      CALL COMP2EXP(NN,JBDAY,JULBDAY,JULEDAY,TLT,DRSPAC,BD,scenname,
     &      Ohorthk,NHORZ)
c
      STOP'NORMAL TERMINATION'
1123    FORMAT(A255)
667     FORMAT (/I4,I4,I6,' (DOY ',i3,')',
     +'  ======>Worst daily H2O BALANCE',t60,G15.6)
668     FORMAT (I4,I4,I6,' (DOY ',i3,')',
     +'  ======>Worst daily N BALANCE',t60,G15.6)
669     FORMAT (I4,I4,I6,' (DOY ',i3,')',
     +'  ======>Worst daily C BALANCE',t60,G15.6)
670     FORMAT (I4,I4,I6,' (DOY ',i3,')',
     +'  ======>Worst daily Pest BALANCE',t60,G15.6)
671     format (//'Annual Water Added due to Reset Soil Moisture (cm)'/,
     +   50(G15.6))
103   FORMAT ("*RZWQM2 OUTPUTS Version 4.2 Intel Fortran (2.22.2021)",
     &         15X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2//)
c     +    '"Let Us Not Make the Perfect the Enemy of the Essential"',
c     +    '  ---President Obama, Feb 4, 2009'//)
1000  FORMAT(T23,'UNITED STATES DEPARTMENT OF AGRICULTURE',/T28,
     +    'AGRICULTURAL RESEARCH SERVICE',//T28,
     +    'ROOT ZONE WATER QUALITY MODEL',/T36,
     +    'VERSION 4.2 Intel Fortran',
     +    /T36,'Feb 22, 2021',/T10)
c     +    'ANY SUFFICIENTLY ADVANCED TECHNOLOGY IS INDISTINGUISHABLE',
c     +    ' FROM MAGIC.'/T45,'- ARTHUR C. CLARKE',///)
 1100 FORMAT(/35('-'),I3,'/',I2,'/',I4,' --- (DOY) ',I3,' --- ',
     &       I6,' DAYS'/)
 1200 FORMAT(F10.1,5(G15.6E3,1x))
 777  FORMAT (I6,I4,I4,I6,7(F10.3),2(F10.5))
778   FORMAT ('  YEAR ',' MON ',' DAY ',' DOY  ',T25,'TMIN',T35,'TMAX',
     +        T45,'WIND',T55,'RAD',T65,'REL H',T75,'CO2',T87,'PAR',T94,
     +        'RefET_T',T104,'RefET_S')      
 779  FORMAT ('  YEAR ',' MON ',' DAY ',' DOY  ','HOUR ',
     +        T30,'THOUR',T40,'WINDH',T50,'RADH',T60,'HUM H',
     +        T70,'CO2',T80,'PARH',T88,'RefET_T',T98,'RefET_S')      
789   FORMAT ('*WEATHER DATA : YOUR LOCATION'//,
     +'@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT'/,
     + 9X,F6.3,1X,F8.3,1X,I5,15x,F3.1,3X,F3.1/
     +'@DATE  SRAD  TMAX  TMIN  RAIN  RHUM  WIND')

 780  FORMAT (I6,I4,I4,I6,I4,6(F10.3),2(F10.5))
 717  Format ('    Year   DOY         Pot_ET        Actual_ET                  
     &H_root           H_soil           Avg_Avail_SW     Pot_RWUP
     &         N-H_WUP          Ref_ET           Crop_ET
     &         H_soilrdf        Avg_SWrdf')
23      stop 'observed data are not available'
27      stop 'check your expdata.dat file'
      END
C
      BLOCK DATA BLOCK1
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 NONE
C
C       CALLED FROM:
C
C       PROGRAMMER:
C
C       VERSION:  2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C=======================================================================
C
C   ARRAY DIMENSION VALUES
C
C-----------------------------------------------------------------------
C
      PARAMETER(MAXBP=50,MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,
     +    MXPEST=3,MXAPP=200,MXSPEC=10,MXCHEM=15)
      PARAMETER(MXTSTP=5000)
C
      PARAMETER(LX2=MAXHOR*13,LX3=MAXHOR*8,LX4=MAXHOR*5,LX5=MXAPP*2,LX6=
     +    MAXHOR*MAXSCT*3,MXPE=MXNOD*MXPEST,LX7=MXNOD*3,LST=5*MXSPEC,LDV
     +    =6*MXSPEC,MXAPP3=MXAPP*3,MXAPP20=MXAPP*20,MXAPP00=MXAPP*200)
      PARAMETER(MXHCH=MXNOD*MXCHEM,MXNCH=MXNODT*MXCHEM,MXTCH=MXTSTP*
     +    MXCHEM)
C
C=======================================================================
C
C     VARIABLES PERTAINING TO HYDROLOGY
C
C-----------------------------------------------------------------------
C
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    HEAD(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
C
      COMMON /IRRIG/ DOIMAX(MXAPP),FDOI(MXAPP),VDOI(MXAPP,MXAPP),
     +    XKU(MXAPP,20),XMAD(MXAPP,20),RRATE(MXAPP),amxirr,
     +    totaliw,DOIMONTH(12,200),TOTALMONTH,airrdepth,amirr,
     +    RRDUR(MXAPP),IIPL(MXAPP),ISPT(MXAPP),ITOI(MXAPP),
     +    JDIRRB(MXAPP),JDIRRE(MXAPP),NAPPI,NDAIRR(MXAPP),NKU(MXAPP),
     +    NMAD(MXAPP),JDOI(MXAPP,MXAPP),JKU(MXAPP,20),JMAD(MXAPP,20),
     +    IDOI(MXAPP),id_depth,MONTHIRR
C
      DOUBLE PRECISION AHLDG,ALA,ALNAVL,ALNGER,ALNLW,ALPHA,ALX,BETA,
     +    BGSEED,CAA,CANK,CAX,CONVLA,CVLBIO,DEVRAT,DROTOL,DTSEED,GITEMP,
     +    GIWAT,GMN,GRMRAT,GSR,HFMAX,LWM,LWS,PCLDG,PGDVRT,PGNTGT,PLALFA,
     +    PLBIO,PLDIAM,PLHGHT,PLTMNT,PLTMXT,PLTOPT,PMAX,PMNNIT,PRNLW,
     +    PTLAMX,R20,RATLS,RATRSX,RATRSN,RDR,RDX,RQ10,RTNLW,SDAMAX,
     +    SDDVRT,SDSVRT,SDTMGM,SDWMAX,SFREEZ,SLA1,SLA2,SLA3,SLA4,STEND,
     +    STNAVL,STNGER,STNLW,TBS,TOP,TOT4WT,WCG,WCP,WDLDG,EFFN,CNST,
     +    PRB,GSGDD,PMAXN,PNRED
      INTEGER IPLTYP,NPL,VRNLZ,GDDFLG
      COMMON /PLNTIN/ AHLDG(MXSPEC),ALA(MXSPEC),ALNAVL(MXSPEC),
     +    ALNGER(MXSPEC),ALNLW(MXSPEC),ALPHA(MXSPEC),ALX(MXSPEC),
     +    BETA(MXSPEC),BGSEED(MXSPEC),CAA(MXSPEC),CANK(MXSPEC),
     +    CAX(MXSPEC),CONVLA(MXSPEC),CVLBIO(MXSPEC),DEVRAT(2:7,MXSPEC),
     +    DROTOL(MXSPEC),EFFN(MXSPEC),DTSEED(MXSPEC),GITEMP(MXSPEC),
     +    GIWAT(MXSPEC),GMN(MXSPEC),GRMRAT(MXSPEC),GSR(MXSPEC),
     +    HFMAX(MXSPEC),PCLDG(MXSPEC),PGDVRT(MXSPEC),PGNTGT(MXSPEC),
     +    PLALFA(MXSPEC),PLBIO(MXSPEC),PLDIAM(MXSPEC),PLHGHT(MXSPEC),
     +    PLTMNT(MXSPEC),PLTMXT(MXSPEC),PLTOPT(MXSPEC),PMAX(MXSPEC),
     +    PMAXN(MXSPEC),PNRED(MXSPEC),PMNNIT(MXSPEC),PRNLW(MXSPEC),
     +    PTLAMX(MXSPEC),R20(MXSPEC),RATLS(MXSPEC),RATRSX(MXSPEC),
     +    RATRSN(MXSPEC),RDR(MXSPEC),RDX(MXSPEC),RQ10(MXSPEC),
     +    RTNLW(MXSPEC),SDAMAX(MXSPEC),SDDVRT(MXSPEC),SDSVRT(MXSPEC),
     +    SDTMGM(MXSPEC),SDWMAX(MXSPEC),SFREEZ(MXSPEC),SLA1(MXSPEC),
     +    SLA2(MXSPEC),SLA3(MXSPEC),SLA4(MXSPEC),STEND(2:6,MXSPEC),
     +    STNAVL(MXSPEC),STNGER(MXSPEC),STNLW(MXSPEC),TBS(MXSPEC),
     +    TOP(MXSPEC),TOT4WT(MXSPEC),WCG(MXSPEC),WCP(MXSPEC),
     +    WDLDG(MXSPEC),CNST(MXSPEC),PRB(7,7,MXSPEC),GSGDD(2:6,MXSPEC),
     +    GDDFLG(MXSPEC),VRNLZ(MXSPEC),SUFNDX(MXSPEC),IPLTYP(MXSPEC),
     +    LWM(MXSPEC),LWS(MXSPEC),NPL,INFIXN(MXSPEC)
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
      COMMON /HRVST/ HSTAGE(MXAPP),HPERC(MXAPP),HRVEFF(MXAPP),YIELD(3),
     +    STUBHT(MXAPP),IHTYPE(MXAPP),IHDAY(MXAPP,3),IHARV(MXAPP),
     +    IHCL(MXAPP)
C
C  ...COMMON SPACE PERTAINING TO POTENTIAL EVAPORATION
C
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
C
      COMMON /RESID/ RM,RESAGE,CRES,HR,RCN,NADLY,NSDLY,NAREC,NSREC,WORM,
     +               SAI,HSTUBL,WSTUBL
C
      COMMON /NUTPAR/ R14,R23,R34,R43,R45,R53,R15,R25,EFFMAX,EFFNIT,
     +    EFFDEN,DENC,RPOP(3),ADCAY,ANIT,ADENIT,AUREA,ADEATH(3),CN(9),
     +    ADCAYV(5),XKP(3),AMETH,FRN2O_NIT,FRN2O_DEN,RNO_N2O,RAINNO,
     +    O2LIM,EFFMETH,ISBUG
C
C
      COMMON /SCCONC/ AL,ALOH,ALSO4,CA,CASO4,H,HCO3,SO4,XMG,XMGSO4,XNA,
     +    XNASO4,XNAORG,XNH4,ORG,CO3,ALORG,ALI,ALOHI,ALSO4I,CAI,CO3I,HI,
     +    HCO3I,ORGI,SO4I,XMGI,XNAI,XNH4I,ALIS,ALOHIS,ALSOIS,CAIS,CO3IS,
     +    HIS,HCO3IS,ORGIS,SO4IS,XMGIS,XNAIS,XNASIS,XNH4IS,ALORIS
C
      COMMON /SCHMIN/ ECA(MXNOD),ENA(MXNOD),EMG(MXNOD),ENH4(MXNOD),
     +    EAL(MXNOD),SCL(MXNOD,3),CEC(MXNOD)
C
      LOGICAL SCL
C
C=======================================================================
C
C     VARIABLES PERTAINING TO SOIL PARAMETERS & PROPERTIES
C
C-----------------------------------------------------------------------
C
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
C
C=======================================================================
C
C     VARIABLES PERTAINING TO SYSTEM DISCRETIZATION
C
C-----------------------------------------------------------------------
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
      COMMON /INFILC/ RAC(11),RRC(11),FERTIR(4),PESTIR(MXPEST),FMNRIR
C
      COMMON /OPINF/ RO,SWF(MXnod),TR(MXTSTP),CI(MXTSTP),CRFD(MAXBP),
     +    CRFT(MAXBP),RFI(MAXBP),RFDD,RFD
      COMMON /OPCHEM/ THSMS(MXNOD,MXCHEM),CUPRO,CUPCHM(MXCHEM),CDNCI,
     +    CDNCHM(MXCHEM),CMESO(MXNODT,MXCHEM),CMICR(MXNODT,MXCHEM),
     +    CRO(MXTSTP,MXCHEM)
      COMMON /OPMACF/ CDBET(MXNODT,MXCHEM),CFLOW(MXNODT,MXCHEM),
     +    CDFLOW(MXNODT,MXCHEM),CUMCLM(MXTSTP,MXCHEM),
     +    CUMCLR(MXTSTP,MXCHEM),CUMMF(MXTSTP),CUMRO(MXTSTP),
     +    DBET(MXNODT),DFLOW(MXNODT),FLOW(MXNODT),TDFLOW(MXNODT),
     +    TFLOW(MXNODT)
C
C   ...COMMON SPACE FOR INFILTRATION AND EVENT-TRANSPORTATION
C
C ----------------------------------------------------------------------
      COMMON /IPCHEM/ A,ALH(MXNOD,MXCHEM),B,BDH(MAXHOR),DEG(MAXHOR),
     +    BDHR(MAXHOR),MICP(MAXHOR),NMICP
C ----------------------------------------------------------------------
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
C ----------------------------------------------------------------------
      COMMON /IPINF/ CRUSTK,OCRUST,ICRUST,INFLPD,NHZ,NSLT,POND,RR,
     +    DHB(MAXHOR),NSL(MAXHOR),WI(MXnod),DSL(MXNODT),VWC(MXNODT),
     +    IUGFLW
C ----------------------------------------------------------------------
      DOUBLE PRECISION MICP,MCPOR,NPOR
      INTEGER YESMAC
      COMMON /INTMCF/ FLXMAX(MAXHOR),DB(MAXHOR),NPOR(MAXHOR)
C
C=======================================================================
C
C     VARIABLES PERTAINING TO SOIL HEAT TRANSPORT
C
C-----------------------------------------------------------------------
C
      COMMON /HEAT/ CSH(MXNOD),T(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3)
C
C
C     PESTICIDE COMMON VARIABLES
C
      COMMON /RZPEST/ HALFF(MXPEST),HALFFP(MXPEST),HALFFB(MXPEST),
     +    HALFR(MXPEST),HALFRP(MXPEST),HALFRB(MXPEST),HALFSSA(MXPEST),
     +    HALFSSV(MXPEST),HALFSSP(MXPEST),HALFSSB(MXPEST),
     +    HALFSA(MXPEST),HALFSN(MXPEST),HALFSB(MXPEST),HALFBD(MXPEST),
     +    XKH(MXPEST),XKOC(MXPEST),XMW(MXPEST),THETAR(MXPEST),
     +    TMPR(MXPEST),EA(MXPEST),EN(MXPEST),WALKERB(MXPEST),
     +    VMAX(MXPEST),DYIELD(4,MXPEST),FREUND(MXPEST),XPKA(MXPEST),
     +    XPKB(MXPEST),REFPH(MXPEST),CATKOC(MXPEST),ANKOC(MXPEST),
     +    PCOEFF(MXPEST),PPOWER(MXPEST),RCOEFF(MXPEST),RPOWER(MXPEST),
     +    XKOW(MXPEST),PBIND(MXPEST),IPCODE(4,MXPEST),IPANC(MXPEST),
     +    IPDEP(MXPEST),IPACTV(MXPEST),NPEST
C
      COMMON /MMNUR/ AMNH4(MXAPP),AMOW(MXAPP),AMBED(MXAPP),
     +    CNBED(MXAPP),AMCN(15),CNMANR(MXAPP),FCMANR(MXAPP),
     +    IWMANR(MXAPP),IMOFF(MXAPP,2),IMMETH(MXAPP),IMTYPE(MXAPP),
     +    NAPPM,IMPL(MXAPP),IMANDECOMP(MXAPP)
C
      COMMON /MFERT/ FNH4IN(MXAPP),FNO3IN(MXAPP),FUREIN(MXAPP),
     +    SPLTST(MXAPP),SPLTAP(MXAPP),IFOFF(MXAPP,2),IFMETH(MXAPP),
     +    IWFERT(MXAPP),NAPPF,IBMPAP(MXAPP),IBMPMD(MXAPP),
     +    IDSPLT(MXAPP),IFPL(MXAPP)
C
      COMMON /MPEST/ APP(MXAPP),PDR(MXAPP),IAPP(MXAPP),IPOFF(MXAPP,2),
     +    IWPEST(MXAPP),IPMETH(MXAPP),NAPP,IPPL(MXAPP)
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      COMMON /KNTCS/ CONCX2(MXNOD,MXPEST),EK2(MXNOD,MXPEST),
     +    RK2(MXPEST),RDIF(MXPEST),FEK2(MXPEST),IEK2(MXPEST)
C
C     TILLAGE INTENSITY INITIALIZATION
C     1ST IS CORN
C     2ND IS SOYBEANS
      DATA TILINT /0.93D0,0.25D0,0.45D0,0.25D0,0.50D0,0.55D0,0.40D0,
     +    0.20D0,0.20D0,0.30D0,0.10D0,0.75D0,0.10D0,0.20D0,0.55D0,
     +    0.10D0,0.08D0,0.15D0,0.20D0,0.40D0,0.15D0,0.15D0,0.25D0,
     +    0.25D0,0.15D0,0.50D0,0.15D0,0.6D0,0.5D0,0.0D0,0.96D0,0.45D0,
     +    0.65D0,0.35D0,0.65D0,0.7D0,0.5D0,0.25D0,0.25D0,0.45D0,0.15D0,
     +    0.80D0,0.15D0,0.3D0,0.7D0,0.1D0,0.11D0,0.18D0,0.30D0,0.5D0,
     +    0.15D0,0.15D0,0.3D0,0.3D0,0.2D0,0.55D0,0.2D0,0.75D0,0.7D0,
     +    0.0D0/
C
C     MEAN TILLAGE DEPTH INITIALIZATION PER IMPLIMENT
      DATA TDEPTH /15.0D0,13.0D0,13.0D0,10.0D0,10.0D0,10.0D0,10.0D0,
     +    15.0D0,3.0D0,5.0D0,3.0D0,15.0D0,8.0D0,35.0D0,8.0D0,15*0.0D0/
      DATA OFDP,OBD /4*0.0D0/
C
      DATA PSTATE /9*0.0D0/,CLSIZE /8*0.0D0/,PWRTS /0.0D0/,DMDNIT /0.0D0
     +    /,TNITUP /0.0D0/
C
      DATA IPACTV /MXPEST*0/,SCL /LX7*.FALSE./
C
C--------------INITIALIZATION OF SCALARS-----------------------------
C
      DATA A /0.0D0/,A0 /0.0D0/,AC /0.0D0/,ADCAY /0.0D0/,ADENIT /0.0D0/,
     +    ANIT /0.0D0/,XLONG /0.0D0/,ASPECT /0.0D0/,AUREA /0.0D0/,B /
     +    0.0D0/,CRUSTK /0.0D0/,OCRUST /0.0D0/,DENC /0.0D0/,EFFDEN /
     +    0.0D0/,EFFMAX /0.0D0/,EFFNIT /0.0D0/,ELEV /0.0D0/,FSUN /0.0D0/,
     +    ICRUST /0/,NAPPF /0/,NAPPI /0/,NAPPM /0/,INFLPD /0/,NAPP /0/,
     +    NHOR /0/,NHZ /0/,NMICP /0/,NN /0/,NNT /0/,NPEST /0/,NSLT /0/,
     +    PI /0.0D0/,ISBUG/0/,area/0.0d0/,EFFMETH/0.0D0/
      DATA POND /0.0D0/,R14 /0.0D0/,R23 /0.0D0/,R34 /0.0D0/,R43 /0.0D0/,
     +    R45 /0.0D0/,R53 /0.0D0/,RR /0.0D0/,SLOPE /0.0D0/,TWOPI /0.0D0
     +    /,XLAT /0.0D0/,YESMAC /0/,R15/0.0d0/,R25/0.0d0/
      DATA CUPRO /0.0D0/,CDNCI /0.0D0/,FLXMAX /MAXHOR*0.0D0/,RFD /0.0D0
     +    /,RFDD /0.0D0/,RO /0.0D0/,RM /0.0D0/,RESAGE /0.0D0/,CRES /
     +    0.0D0/,HR /0.0D0/,RCN /0.0D0/,NADLY /0/,NSDLY /0/,NAREC /0/,
     +    NSREC /0/,WORM /0.0D0/
      DATA ALOH /0.0D0/,ALSO4 /0.0D0/,CA /0.0D0/,CASO4 /0.0D0/,H /0.0D0
     +    /,HCO3 /0.0D0/,SO4 /0.0D0/,XMG /0.0D0/,XMGSO4 /0.0D0/,XNA /
     +    0.0D0/,XNASO4 /0.0D0/,XNAORG /0.0D0/,XNH4 /0.0D0/,ORG /0.0D0/,
     +    CO3 /0.0D0/,ALORG /0.0D0/,ALI /0.0D0/,ALOHI /0.0D0/,ALSO4I /
     +    0.0D0/,CAI /0.0D0/,CO3I /0.0D0/,HI /0.0D0/,HCO3I /0.0D0/,ORGI
     +    /0.0D0/,SO4I /0.0D0/,XMGI /0.0D0/,XNAI /0.0D0/,XNH4I /0.0D0/,
     +    ALIS /0.0D0/,ALOHIS /0.0D0/,ALSOIS /0.0D0/,CAIS /0.0D0/,CO3IS
     +    /0.0D0/,HIS /0.0D0/,HCO3IS /0.0D0/,ORGIS /0.0D0/,SO4IS /0.0D0
     +    /,XMGIS /0.0D0/,XNAIS /0.0D0/,XNASIS /0.0D0/,XNH4IS /0.0D0/,
     +    ALORIS /0.0D0/
C
C--------------INITIALIZATION OF VECTORS-----------------------------
      DATA ADCAYV /5*0.0D0/,ADEATH /3*0.0D0/,AGSZ /MAXHOR*0.0D0/,ALH /
     +    MXHCH*0.0D0/,APP /MXAPP*0.0D0/,BDH /MAXHOR*0.0D0/,CN /9*0.0D0
     +    /,CSH /MXNOD*0.0D0/,DEG /MAXHOR*0.0D0/,DELZ /MXNOD*0.0D0/,DHB
     +    /MAXHOR*0.0D0/,DOIMAX /MXAPP*0.0D0/,DSL /MXNODT*0.0D0/
      DATA FDEP /MAXHOR*0.0D0/,FDOI /MXAPP*0.0D0/,HEAD /MXNOD*0.0D0/,
     +    HKBAR /MXNOD*0.0D0/,HORTHK /MAXHOR*0.0D0/,IAPP /MXAPP*0/,IDOI
     +    /MXAPP*0/,IIPL /MXAPP*0/,ISPT /MXAPP*0/,ITOI /MXAPP*0/,IWPEST
     +    /MXAPP*0/,JDOI /MXAPP00*0/,JDIRRB /MXAPP*0/,JDIRRE /MXAPP*0/,
     +    JKU /MXAPP20*0/,JMAD /MXAPP20*0/
      DATA MCPOR /MAXHOR*0.0D0/,IPMETH /MXAPP*0/,MICP /MAXHOR*0.0D0/,
     +    NDAIRR /MXAPP*0/,NDXH2N /MAXHOR*0/,NDXN2H /MXNOD*0/,NDXN2T /
     +    MXNOD*0/,NDXT2N /MXNODT*0/,NKU /MXAPP*0/,NMAD /MXAPP*0/,NSL /
     +    MAXHOR*0/,PL /MAXHOR*0.0D0/,IPOFF /LX5*0.0D0/,QF /MXNOD*0.0D0/
      DATA RAC /11*0.0D0/,RCOEFF /MXPEST*0.0D0/,RP /MAXHOR*0.0D0/,RPOP /
     +    3*0.0D0/,RPOWER /MXPEST*0.0D0/,RRATE /MXAPP*0.0D0/,RRC /11*
     +    0.0D0/,RRDUR /MXAPP*0.0D0/,SOILHP /LX2*0.0D0/,SOILPP /LX3*
     +    0.0D0/,SOLTP1 /LX4*0.0D0/,SOLTP2 /LX6*0.0D0/,T /MXNOD*0.0D0/,
     +    THETA /MXNOD*0.0D0/,TL /MXNOD*0.0D0/,TLT /MXNOD*0.0D0/,PCOEFF
     +    /MXPEST*0.0D0/
      DATA PPOWER /MXPEST*0.0D0/,VDOI /MXAPP00*0.0D0/,VWC /MXNODT*0.0D0
     +    /,WI /MXnod*0.0D0/,WP /MAXHOR*0.0D0/,XPKA /MXPEST*0.0D0/,XPKB
     +    /MXPEST*0.0D0/,XKH /MXPEST*0.0D0/,XKOC /MXPEST*0.0D0/,XKP /3*
     +    0.0D0/,XKU /MXAPP20*0.0D0/,XMAD /MXAPP20*0.0D0/,XMW /MXPEST*
     +    0.0D0/
      DATA ZN /MXNOD*0.0D0/
      DATA CONCX2 /MXPE*0.0D0/,IEK2 /MXPEST*0.0D0/,EK2 /MXPE*0.0D0/,RK2
     +    /MXPEST*0.0D0/,RDIF /MXPEST*0.0D0/
      DATA CDBET /MXNCH*0.0D0/,CDFLOW /MXNCH*0.0D0/,CFLOW /MXNCH*0.0D0/,
     +    CI /MXTSTP*0.0D0/,CMESO /MXNCH*0.0D0/,CMICR /MXNCH*0.0D0/,CRFD
     +    /MAXBP*0.0D0/,CRFT /MAXBP*0.0D0/,CRO /MXTCH*0.0D0/,CUMCLM /
     +    MXTCH*0.0D0/,CUMCLR /MXTCH*0.0D0/,CUMMF /MXTSTP*0.0D0/,CUMRO /
     +    MXTSTP*0.0D0/,DB /MAXHOR*0.0D0/,DBET /MXNODT*0.0D0/,DFLOW /
     +    MXNODT*0.0D0/,FLOW /MXNODT*0.0D0/,NPOR /MAXHOR*0.0D0/,RFI /
     +    MAXBP*0.0D0/,SWF /MXnod*0.0D0/,TDFLOW /MXNODT*0.0D0/,TFLOW /
     +    MXNODT*0.0D0/,CUPCHM /MXCHEM*0.0D0/,CDNCHM /MXCHEM*0.0D0/,
     +    THSMS /MXHCH*0.0D0/,TR /MXTSTP*0.0D0/
      DATA IFOFF /LX5*0/,IFMETH /MXAPP*0/,IWFERT /MXAPP*0/,FNH4IN /MXAPP
     +    *0.0D0/,FNO3IN /MXAPP*0.0D0/,FUREIN /MXAPP*0.0D0/
      DATA ECA /MXNOD*0.0D0/,ENA /MXNOD*0.0D0/,EMG /MXNOD*0.0D0/,ENH4 /
     +    MXNOD*0.0D0/,EAL /MXNOD*0.0D0/,CEC /MXNOD*0.0D0/
C
      DATA AHLDG /MXSPEC*0.0D0/,ALA /MXSPEC*0.0D0/,ALNAVL /MXSPEC*0.0D0
     +    /,ALNGER /MXSPEC*0.0D0/,ALNLW /MXSPEC*0.0D0/,ALPHA /MXSPEC*
     +    0.0D0/,ALX /MXSPEC*0.0D0/,BETA /MXSPEC*0.0D0/,BGSEED /MXSPEC*
     +    0.0D0/,CAA /MXSPEC*0.0D0/,CANK /MXSPEC*0.0D0/,CAX /MXSPEC*
     +    0.0D0/,CNUP1 /MXSPEC*0.0D0/,CNUP2 /MXSPEC*0.0D0/,CONVLA /
     +    MXSPEC*0.0D0/,CVLBIO /MXSPEC*0.0D0/,DEVRAT /LDV*0.0D0/,DROTOL
     +    /MXSPEC*0.0D0/,DTSEED /MXSPEC*0.0D0/,GITEMP /MXSPEC*0.0D0/,
     +    GIWAT /MXSPEC*0.0D0/,GMN /MXSPEC*0.0D0/,GRMRAT /MXSPEC*0.0D0/,
     +    GSR /MXSPEC*0.0D0/,HFMAX /MXSPEC*0.0D0/,LWM /MXSPEC*0.0D0/,LWS
     +    /MXSPEC*0.0D0/,EFFN /MXSPEC*0.0D0/,PCLDG /MXSPEC*0.0D0/,PGDVRT
     +    /MXSPEC*0.0D0/,PGNTGT /MXSPEC*0.0D0/,PLALFA /MXSPEC*0.0D0/,
     +    PLBIO /MXSPEC*0.0D0/,PLDIAM /MXSPEC*0.0D0/,PLHGHT /MXSPEC*
     +    0.0D0/,PLTMNT /MXSPEC*0.0D0/
C
      DATA PLTMXT /MXSPEC*0.0D0/,PLTOPT /MXSPEC*0.0D0/,PMAX /MXSPEC*
     +    0.0D0/,PMNNIT /MXSPEC*0.0D0/,PRNLW /MXSPEC*0.0D0/,PTLAMX /
     +    MXSPEC*0.0D0/,R20 /MXSPEC*0.0D0/,RATLS /MXSPEC*0.0D0/,RATRSX /
     +    MXSPEC*0.0D0/,RDR /MXSPEC*0.0D0/,RDX /MXSPEC*0.0D0/,RQ10 /
     +    MXSPEC*0.0D0/,RTNLW /MXSPEC*0.0D0/,RATRSN /MXSPEC*0.0D0/,
     +    SDAMAX /MXSPEC*0.0D0/,SDDVRT /MXSPEC*0.0D0/,SDSVRT /MXSPEC*
     +    0.0D0/,SDTMGM /MXSPEC*0.0D0/,SDWMAX /MXSPEC*0.0D0/,SFREEZ /
     +    MXSPEC*0.0D0/,SLA1 /MXSPEC*0.0D0/,SLA2 /MXSPEC*0.0D0/,SLA3 /
     +    MXSPEC*0.0D0/,SLA4 /MXSPEC*0.0D0/,STEND /LST*0.0D0/,STNAVL /
     +    MXSPEC*0.0D0/,STNGER /MXSPEC*0.0D0/,STNLW /MXSPEC*0.0D0/,TBS /
     +    MXSPEC*0.0D0/,TOP /MXSPEC*0.0D0/,TOT4WT /MXSPEC*0.0D0/,WCG /
     +    MXSPEC*0.0D0/,WCP /MXSPEC*0.0D0/,WDLDG /MXSPEC*0.0D0/
      DATA IPLTYP /MXSPEC*0/
C
      DATA IHTYPE /MXAPP*0/,HSTAGE /MXAPP*0.0D0/,HPERC /MXAPP*0.0D0/,
     +    IHDAY /MXAPP3*0/,HRVEFF /MXAPP*0.0D0/,YIELD /3*0.0D0/,IHARV /
     +    MXAPP*0/,STUBHT /MXAPP*0.0D0/,IHCL /MXAPP*0/,AMNH4 /MXAPP*
     +    0.0D0/,AMOW /MXAPP*0.0D0/,AMBED /MXAPP*0.0D0/,CNBED /MXAPP*
     +    0.0D0/,IWMANR /MXAPP*0/,IMOFF /LX5*0/,IMMETH /MXAPP*0/,IMTYPE
     +    /MXAPP*0/,AMCN /19.0D0,20.0D0,18.0D0,16.0D0,18.0D0,14.0D0,
     +    11.0D0,11.0D0,12.0D0,9.0D0,16.0D0,12.0D0,24.0D0,24.0D0,0.0D0/
C
      END
C
      BLOCK DATA BLKOUT
C
C======================================================================
C
C       PURPOSE:  THIS BLOCK INITIALIZES ALL THE OUTPUT FILES WITH
C             THE APPROPIATE SCNRIO LABELS AND VARIABLES.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ASCASS     L   2D ARRAY OF ALL SCNRIO'S  SEQUENCE LISTS
C       AVECSS     L   2D ARRAY OF ALL SCNRIO'S VECTOR SEQUENCE LISTS
C       CNTRL  I   CONTROL FILE UNIT NUMBER
C       MXNOD  P   MAXIMUM LENGTH OF VECTORS
C       NUMSCA     P   NUMBER OF SCALAR VARIABLES
C       SCATAB     I   VECTOR OF SCALAR TABULAR OUTPUT FILE UNIT NUMBERS
C       USCASS     I   VECTOR OF USER SELECTED SCALAR SEQUENCE LIST
C       UVECSS     I   VECTOR OF USER SELECTED VECTOR SEQUENCE LIST
C       VECPLT     I   VECTOR PLOT FILE UNIT NUMBER
C       VECSSL     I   VECTOR OF CURRENT SCNRIO'S VECTOR SEQUENCE LIST
C       VECTAB     I   VECTOR OF VECTOR TABULAR OUTPUT FILE UNIT NUMBERS
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN
C
C       PROGRAMMER:  SANDY MAPLE
C
C       VERSION:   3.0
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C    SET BOUNDARIES FOR DATA ARRAYS
C
      PARAMETER(NUMSCN=7,NUMSCA=107,NUMVEC=59,IHZNUM=21,MAXSCA=41,MAXVEC
     +    =23)
C
      INTEGER SCASSL(MAXSCA),USSSL(MAXSCA)
      INTEGER VECSSL(MAXVEC+1),UVSSL(MAXVEC+1)
      INTEGER ASSSL(NUMSCN+1,MAXSCA),AVSSL(NUMSCN+1,MAXVEC)
      INTEGER SC,CNTRL,SCAPLT,VECPLT,SCATAB(6),VECTAB(6)
      CHARACTER SCNRIO(NUMSCN+1)*30,SCALAR(NUMSCA)*30,VECTOR(NUMVEC)*30
      CHARACTER HZLABS(IHZNUM)*30
C
C    TO REDUCE ARGUMENT PASSING TO SUBROUTINES ALL VARIABLE SEQUENCE
C    LISTS, FILE UNIT NUMBERS, LABELS AND THE CURRENT SCNRIO NUMBER
C    ARE COMMON UNDER THE FOLLOWING BLOCKS
C
      COMMON /SSLIST/ SCASSL,USSSL,VECSSL,UVSSL,ASSSL,AVSSL
      COMMON /FUNIT/ SC,CNTRL,SCAPLT,VECPLT,SCATAB,VECTAB,ISCA,IVEC
      COMMON /LABLS/ SCNRIO,SCALAR,VECTOR,HZLABS
      COMMON /SC1/ ISTAB,IVTAB,ISPLT,IVPLT
C
C
C    ..INITIALIZATION OF THE SCNRIO LABEL LIST
C
      DATA(SCNRIO(I),I=1,NUMSCN+1) /'HYDROLOGY         ',
     +    'NITROGEN          ','PESTICIDE         ',
     +    'PLANT GROWTH      ','MANAGEMENT        ',
     +    'GENERAL           ','USER              ',
     +    'INPUT VARIABLES     '/
C
C    ..INITIALIZATION OF THE SCALAR VARIABLE LABELS
C
      DATA(SCALAR(I),I=1,19) /'ACCUMULATED PRECIPITATION (CM)',
     +    'ACCUMULATED INFILTRATION (CM) ','NO3 BREAK THROUGH (MG/L) ',
     +    'TOTAL EROSION (KG/HA)','ACTUAL EVAPORATION (CM)  ',
     +    'ACTUAL TRANSPIRATION (CM) ','NH4-N ERODED     ',   !FROM NO3-N ERODED TO NH4 ERODED
     +    'PEST #1 BREAK THROUGH (MG/L) ',
     +    'PEST #2 BREAK THROUGH (MG/L) ',
     +    'PEST #3 BREAK THROUGH (MG/L) ','LEAF AREA INDEX   ',
     +    'PLANT HEIGHT (CM)   ','PLANT AREA COVER (%)  ',
     +    'DEPTH OF ROOTS (CM)  ','TOTAL NO3-N IN PROFILE (KG/HA)',
     +    'WATER STRESS     ','TEMPERATURE STRESS  ',
     +    'NUTRIENT STRESS   ','MINERALIZATION (KG/HA)  '/
C
      DATA(SCALAR(I),I=20,38) /'VOLATILIZATION (KG/HA)  ',
     +    'NITRIFICATION (KG/HA)  ','ACTUAL EVAPOTRANSPIRATION (CM)',
     +    'PEST#1 ERODED (KG/HA)  ',
     +    'PEST#2 ERODED (KG/HA)  ',
     +    'PEST#3 ERODED (KG/HA)  ',
     +    'PEST#1 ON PLANTS/MULCH(KG/HA) ',
     +    'PEST#2 ON PLANTS/MULCH(KG/HA) ',
     +    'PEST#3 ON PLANTS/MULCH(KG/HA) ',
     +    'PEST#1 ON SOIL SURFCE (KG/HA) ',
     +    'PEST#2 ON SOIL SURFCE (KG/HA) ',
     +    'PEST#3 ON SOIL SURFCE (KG/HA) ',
     +    'PEST#1 IN SOIL PROFLE (KG/HA) ',
     +    'PEST#2 IN SOIL PROFLE (KG/HA) ',
     +    'PEST#3 IN SOIL PROFLE (KG/HA) ','NET RUNOFF (CM)   ',
     +    'TOTAL RUNOFF (CM)   ','RUNON (CM)        ',
     +    'RUNON EROSION    '/
C
      DATA(SCALAR(I),I=39,56) /'POTENTIAL EVAPORATION (CM) ',
     +    'POTENTIAL TRANSPIRATION (CM) ','TOTAL PLANT STRESS  ',
     +    'WATER FLUX INTO GW (CM/DAY) ',
     +    'HEAT FLUX INTO GW(MJ/CM^2/DAY)',
     +    'NO3 FLUX INTO GW (UG/CM^2/DAY)','ORGANIC N ERODED (KG/HA) ',  !FROM PO4 TO ORGANIC N #45
     +    'ORGANIC C ERODED (KG/HA) ','H2O BRK THROUGH CURVE (CM/DAY)',  !FROM PO4 TO ORGANIC C #46
     +    'TEMP BREAK THROUGH CURVE (C) ','PLANT GROWTH STAGE  ',
     +    'NUMBER OF LIVE PLANTS  ','DORMANT SEEDS    ',
     +    'GERMINATING PLANTS  ','EMERGENT PLANTS   ',
     +    'JUVENILE PLANTS   ','VEGETATIVE PLANTS   ',
     +    'FLOWERING PLANTS   '/
C
      DATA(SCALAR(I),I=57,75) /'HARVESTABLE PLANTS  ',
     +    'LEAF BIOMASS (G)   ','STEM BIOMASS (G)   ',
     +    'PROPAGULE BIOMASS (G)  ','ROOT BIOMASS (G)   ',
     +    'SEED BIOMASS (G)   ','STANDING DEAD BIOMASS (G) ',
     +    'LITTER BIOMASS (G)  ','DEAD ROOT BIOMASS (G)  ',
     +    'LEAF NITROGEN (G-N)  ','STEM NITROGEN (G-N)  ',
     +    'PROPAGULE NITROGEN (G-N) ','ROOT NITROGEN (G-N)  ',
     +    'SEED NITROGEN (G-N)  ','STANDING DEAD NITROGEN (G-N) ',
     +    'LITTER NITROGEN (G-N)  ','DEAD ROOT NITROGEN (G-N) ',
     +    'TOTAL USABLE NITROGEN (KG/HA) ',
     +    'NO3 FLX AT BRKTH NODE(UG/CM^2)'/
C
      DATA(SCALAR(I),I=76,88) /'PEST1 FLX AT BKTHNODE(UG/CM^2)',
     +    'PEST2 FLX AT BKTHNODE(UG/CM^2)',
     +    'PEST3 FLX AT BKTHNODE(UG/CM^2)',
     +    'TOT ABOVE GRD BIOMASS (KG/HA) ',
     +    'TOT ABOVE GRD DEAD BM (KG/HA) ',
     +    'TOTAL ROOT BIOMASS (KG/HA) ','RESIDUE COVER (%)   ',
     +    'PEST1 MASS LOST TO RO(UG/CM^2)',
     +    'PEST2 MASS LOST TO RO(UG/CM^2)',
     +    'PEST3 MASS LOST TO RO(UG/CM^2)',
     +    'NO3-N MASS LOST TO RO (KG/HA) ',
     +    'NH4-N MASS LOST TO RO (KG/HA) ','POTENTIAL EVPOTRANSP. (CM) '
     +    /
C
      DATA(SCALAR(I),I=89,NUMSCA) /'DEPTH OF WATER TABLE (CM) ',
     +    'WATER OUT TILE DRAIN (CM/DAY) ',
     +    'NO3-N MASS OUT DRAIN (UG/CM^2)',
     +    'PEST1 MASS OUT DRAIN (UG/CM^2)',
     +    'PEST2 MASS OUT DRAIN (UG/CM^2)',
     +    'PEST3 MASS OUT DRAIN (UG/CM^2)',
     +    'SURFACE MULCH MASS (KG/HA) ','C:N RATIO OF SURFACE MULCH ',
     +    'STANDING DEAD MASS (KG/HA) ','C:N RATIO OF STANDING DEAD ',
     +    'N2O PRODUCTION (KG N/HA)     ','NxO PRODUCTIOIN (KG N/HA)',
     +    'CH4 PRODUCTION (KG N/HA)','CO2 PRODUCTION (KG C/HA)',
     +    'DRP RUNOFF (GM P/HA)','PP RUNOFF (GM P/HA) ',
     +    'DRP TILE (GM P/HA)','PP TILE (GM P/HA)',
     +    'PLANT P UPTAKE (GM P/HA)' /
C
C    ..INITIALIZATION OF THE VECTOR VARIABLE LABELS
C
      DATA(VECTOR(I),I=1,19) /'DEPTH (CM)        ',
     +    'SOIL WATER CONTENT (VOL) ','TEMPERATURE (C)   ',
     +    'SLOW RESIDUE POOL (UG-C/G) ','FAST RESIDUE POOL (UG-C/G) ',
     +    'FAST HUMUS POOL (UG-C/G) ','INTERMEDIATE HUMUS POOL(UGC/G)',
     +    'SLOW HUMUS POOL (UG-C/G) ','NO3-N CONC. (UG/G)  ',
     +    'FLUX OF NO3 (UG/CM^2)  ','NH4-N CONC. (UG/G)  ',
     +    'MINERALIZED NO3 (KG/HA)  ','DENITRIFICATION (KG/HA)  ',
     +    'NITRIFICATION (KG/HA)  ','IMMOBILE NO3 (KG/HA)  ',
     +    'SOIL PH          ','AEROBIC HETEROTROPHS (#ORG/G) ',
     +    'ANAEROBIC H-TROPHS (#ORG/G) ','AUTOTROPHS (#ORG/G)  '/
C
      DATA(VECTOR(I),I=20,38) /'SOIL BULK DENSITY (G/CM^3) ',
     +    'MACROPORE PERCENT   ','PO4-P CONC. (UG/G)  ',
     +    'FLUX OF SOLUBLE PO4-P  ','MINERAL PO4-P    ',
     +    'IMMOBILE PO4-P    ','PEST#1 CONC. ABSORB. (UG/G) ',
     +    'PEST#2 CONC. ABSORB. (UG/G) ','PEST#3 CONC. ABSORB. (UG/G) ',
     +    'PEST#1 CONC. IN SOL. (MG/L) ','PEST#2 CONC. IN SOL. (MG/L) ',
     +    'PEST#3 CONC. IN SOL. (MG/L) ','PEST#1 TOTAL MASS (KG/HA) ',
     +    'PEST#2 TOTAL MASS (KG/HA) ','PEST#3 TOTAL MASS (KG/HA) ',
     +    'ROOT NITROGEN (G-N)  ','NITROGEN UPTAKE (KG/HA)  ',
     +    'SAT HYD COND  (CM/HR)  ','IONIC STRENGTH (MOLES/L) '/
C
      DATA(VECTOR(I),I=39,NUMVEC) /'PLANT WATER UPTAKE (CM)  ',
     +    'MOISTURE FLUX (CM/DAY)  ','HEAT FLUX (MJ/CM^2)  ',
     +    'UREA-N (UG/G)    ','PRESSURE HEAD (CM)  ',
     +    'CALCIUM (UG/G)    ','SODIUM (UG/G)    ',
     +    'MAGNESIUM (UG/G)   ','CHLORINE (UG/G)   ',
     +    'HCO3 (UG/G)       ','SO4 (UG/G)        ',
     +    'ALUMINUM (UG/G)   ','ROOT MASS (G)    ',
     +    'FRACTION ORGANIC CARBON  ','SOIL ICE CONTENT    ',  !ICE IS SET TO #53
     +    'POROSITY       ','N2O PRODUCTION(KG N/HA)',
     +    'NxO PRODUCTION(KG N/HA)','CH4 PRODUCTION (KG N/HA)',
     +    'CO2 PRODUCTION (KG C/HA)','FRACTION ORGANIC NITROGEN'/
C
C
C    ..SCALAR SEQUENCE LISTS FOR ALL SCNRIOS (EACH SCNRIO IS PLACED
C      VERTICALLY)
C
C------------------------------------------------
C
      DATA(ASSSL(1,J),J=1,MAXSCA) /1,2,3,5,6,35,39,40,46,47,48,88,89,90,
     +    27*0/
C
      DATA(ASSSL(2,J),J=1,MAXSCA) /1,2,3,7,13,15,18,19,20,21,35,44,46,
     +    74,75,86,87,91,23*0/
C
      DATA(ASSSL(3,J),J=1,MAXSCA) /1,2,5,6,8,9,10,11,13,14,18,26,27,28,
     +    29,30,31,32,33,34,35,39,40,76,77,78,83,84,85,92,93,94,9*0/
C
      DATA(ASSSL(4,J),J=1,MAXSCA) /1,2,6,11,12,13,14,16,17,18,35,40,49,
     +    50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
     +    70,71,72,73,74,79,80,81/
C
      DATA(ASSSL(5,J),J=1,MAXSCA) /1,2,5,6,35,39,40,50,82,95,96,97,98,28
     +    *0/
C
      DATA(ASSSL(6,J),J=1,MAXSCA) /1,2,11,14,22,35,41,42,43,49,50,58,59,
     +    61,62,66,67,69,74,79,88,20*0/
C
      DATA(ASSSL(7,J),J=1,MAXSCA) /MAXSCA*0/
C
      DATA(ASSSL(8,J),J=1,MAXSCA) /1,2,4,5,6,7,35,34*0/
C
C
C    ..VECTOR SEQUENCE LISTS FOR ALL SCNRIOS (EACH SCNRIO IS PLACED
C      VERTICALLY)
C
C
C SCNRIO #: 1   2   3 4   5   6 7   8   9
C-----------------------------------------------
C
      DATA(AVSSL(1,J),J=1,MAXVEC) /2,20,35,36,37,38,39,40,41,54,13*0/
C
      DATA(AVSSL(2,J),J=1,MAXVEC) /2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
     +    17,18,19,20,21,42,52,55/
C
      DATA(AVSSL(3,J),J=1,MAXVEC) /26,27,28,29,30,31,32,33,34,52,13*0/
C
      DATA(AVSSL(4,J),J=1,MAXVEC) /2,3,4,5,6,7,8,9,11,16,17,18,19,29,30,
     +    31,51,52,53,4*0/
C
      DATA(AVSSL(5,J),J=1,MAXVEC) /2,3,16,20,21,51,52,54,15*0/
C
      DATA(AVSSL(6,J),J=1,MAXVEC) /2,3,9,11,16,20,21,29,30,31,32,33,34,
     +    51,52,8*0/
C
      DATA(AVSSL(7,J),J=1,MAXVEC) /MAXVEC*0/
C
      DATA(AVSSL(8,J),J=1,MAXVEC) /4,5,6,7,8,9,11,17,18,19,52,12*0/
C
C
C    ..INITIALIZATION OF HZLABS
C
      DATA(HZLABS(I),I=1,IHZNUM) /'TEXTURE CODE     :',
     +    'PARTICLE DENSITY   :','BULK DENSITY     :',
     +    'POROSITY         :','% SAND           :',
     +    '% SILT           :','% CLAY           :','% OM         :',
     +    'S2-BUB PRES TH(H)  :','A2-PORE DISTN INDEX:',
     +    'N2-SLOPE K(H) CURVE:','C1-SAT HYD COND    :',
     +    'WR- THETA-R      :','WS- THETA-S      :',
     +    'FC13- THETA-1/3 BAR:','FC10- THETA-1/10BAR:',
     +    'FC15- THETA-15  BAR:','S1-BUB PRES K(H)   :',
     +    'C2-ITCPT UNSAT K(H):','N1-SLOPE K(H) CURVE:',
     +    'A1-CONS TH(H) CURVE:'/
C
C    .. INITIALIZATION OF REMAINING COMMON VARIABLES
C
      DATA(SCASSL(I),I=1,MAXSCA) /MAXSCA*0/
      DATA(USSSL(I),I=1,MAXSCA) /MAXSCA*0/
      DATA(VECSSL(I),I=1,(MAXVEC+1)) /MAXVEC*0,0/
      DATA(UVSSL(I),I=1,(MAXVEC+1)) /MAXVEC*0,0/
      DATA SC,CNTRL,SCAPLT,VECPLT /0,0,0,0/
      DATA SCATAB,VECTAB /6*0,6*0/
      DATA ISTAB,IVTAB,ISPLT,IVPLT /0,0,0,0/
C
      END
C
      SUBROUTINE INIT(INP5,JBDAY,CONC,XNU,COPLNT,CORES,PCO2,RPOOL,BD,RM,
     +    FRACOM,RPOP,iwcinit,FRACON,CN)
      USE VARIABLE
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AEF       I/O FIELD SATURATION FRACTION [0..1]
C       BD         I  BULK DENSITY FOR CURRENT HORIZON [G/CM^3]
C       CEC       I/O CATION EXCHANGE CAPACITY [MEQ/100 GRAMS SOIL]
C       CONC  I/O CHEM. CONC. IN SOIL WATER OF EACH SOIL NODE
C                 SAME AS CC [MG/L]
C       COPLNT    I/O INITIAL AMT OF PESTICIDE ON THE FOLIAGE [UG/CM^2]
C       CORES I/O INIT. AMT. CHEM. AVAIL. ON RESIDUE FOR DISSIPATION
C       EAL       I/O FRACTION EXCHANGABLE IONS OF ALUMINUM [0..1]
C       ECA       I/O FRACTION EXCHANGABLE IONS OF CALCIUM [0..1]
C       EMG       I/O FRACTION EXCHANGABLE IONS OF MAGNESIUM [0..1]
C       ENA       I/O FRACTION EXCHANGABLE IONS OF SODIUM [0..1]
C       ENH4  I/O FRACTION EXCHANGABLE IONS OF AMMONIA [0..1]
C       FACT   L
C       FRACOM    I/O FRACTION ORGANIC MATTER IN LAYERS [.]
C       FIRST  L  TRUE IF FIRST TIME THROUGH ROUTINE
C       H     I/O NODAL SOIL WATER PRESSURE HEADS [CM]
C       HFRCOM     L  FRACTION ORGANIC MATTER BY HORIZONS [.]
C       I      L  INDEX VARIABLE
C       IDATE  L
C       IDB        L  --DAY OF BEGINNING OF RUN.
C       IDE        L  --DAY OF END OF RUN.
C       IH         L
C       IMB        L  --MONTH OF BEGINNING OF RUN.
C       IME        L  --MONTH OF END OF RUN.
C       INP3   I  POINTER TO DAYMET.DAT DATAFILE
C       INP5   I  POINTER TO RZINIT.DAT DATAFILE
C       IWTYPE     L
C       J      L  INDEX VARIABLE
C       JBDAY I/O
C       JEDAY I/O
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NDXN2H     I  INDEX FOR NUMERICAL LAYERS TO HORIZONS,
C                   IE. WHICH HORIZON IS NUMERICAL LAYER IN.
C       NHOR   I  NUMBER OF SOIL HORIZONS
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NPEST  I  NUMBER OF USER DEFINED PESTICIDES
C       PACT   L
C       PACTIV    I/O TRUE WHEN PESTICIDE IS ACTIVE
C       PCO2  I/O PARTIAL PRESSURE OF CO2 [ATM]
C       PH         L  PH [STANDARD UNITS] HYDROGEN ION ACTIVITY
C       RPOOL I/O RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       SCL       I/O
C       SOILHP     I  MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       T     I/O SOIL TEMPERATURE IN NUMERICAL CONFIGURATION [C]
C       T9         L
C       TAL        L
C       TCA        L
C       TCEC   L
C       TCO2   L
C       TCONC  L  CHEMICAL CONCENTRATIONS OF SINGLE CHEMICAL
C       TDAY   L
C       TEM        L
C       THEAD  L
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TMG        L
C       TNA        L
C       TNH4   L
C       TPEST  L  DAY COUNTER FOR PESTICIDE; STARTING WITH THE
C                 DAY OF APPLICATION [DAYS]
C       TPH        L
C       TSCL   L
C       TXNU   L
C       WAT        L
C       XNU       I/O
C       XTRCT  L  SOIL CHEMISTRY ADJUSTMENT FACTOR FOR INITIAL CHEM
C                 CONCENTRATIONS, WEIGHT H2O / WEIGHT SOIL.
C
C
C       COMMENTS:
C
C
C       MASS STORAGE FILES:
C
C
C       EXTERNAL REFERENCES:
C                 ECHO
C                 JDATE
C                 SGATE
C                 VGATE
C                 WCH
C
C
C       CALLED FROM:  MAIN
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ARRAY DIMENSION VALUES
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,MXCHEM=15,
     +    MXPEST=3,CARBCV=1.0D0/0.58D0)
C
C     VARIABLES PERTAINING TO SYSTEM DISCRETIZATION
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
C     VARIABLES PERTAINING TO HYDROLOGY
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
      COMMON /RZPEST/ HALFF(MXPEST),HALFFP(MXPEST),HALFFB(MXPEST),
     +    HALFR(MXPEST),HALFRP(MXPEST),HALFRB(MXPEST),HALFSSA(MXPEST),
     +    HALFSSV(MXPEST),HALFSSP(MXPEST),HALFSSB(MXPEST),
     +    HALFSA(MXPEST),HALFSN(MXPEST),HALFSB(MXPEST),HALFBD(MXPEST),
     +    XKH(MXPEST),XKOC(MXPEST),XMW(MXPEST),THETAR(MXPEST),
     +    TMPR(MXPEST),EA(MXPEST),EN(MXPEST),WALKERB(MXPEST),
     +    VMAX(MXPEST),DYIELD(4,MXPEST),FREUND(MXPEST),XPKA(MXPEST),
     +    XPKB(MXPEST),REFPH(MXPEST),CATKOC(MXPEST),ANKOC(MXPEST),
     +    PCOEFF(MXPEST),PPOWER(MXPEST),RCOEFF(MXPEST),RPOWER(MXPEST),
     +    XKOW(MXPEST),PBIND(MXPEST),IPCODE(4,MXPEST),IPANC(MXPEST),
     +    IPDEP(MXPEST),IPACTV(MXPEST),NPEST
C
C     VARIABLES PERTAINING TO SOIL HEAT TRANSPORT
      COMMON /HEAT/ CSH(MXNOD),T(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3)
C
      COMMON /SCHMIN/ ECA(MXNOD),ENA(MXNOD),EMG(MXNOD),ENH4(MXNOD),
     +    EAL(MXNOD),SCL(MXNOD,3),CEC(MXNOD)
C
      LOGICAL SCL
C
      DIMENSION CONC(MXNOD,MXCHEM),XNU(MXNOD,25),WAT(MAXHOR),
     +    TEM(MAXHOR),TPH(MAXHOR),TSCL(MAXHOR,3),TCO2(MAXHOR),
     +    TCEC(MAXHOR),TCA(MAXHOR),TNA(MAXHOR),TMG(MAXHOR),
     +    TNH4(MAXHOR),TAL(MAXHOR),TCONC(MAXHOR,MXCHEM),THEAD(MAXHOR),
     +    COPLNT(MXPEST),CORES(MXPEST),TXNU(MAXHOR,14),PACT(MXPEST),
     +    TPEST(MAXHOR,MXPEST),PH(MXNOD),PCO2(MXNOD),RPOOL(MXNOD,2),
     +    BD(MXNOD),TWAT(MAXHOR),XTRCT(MAXHOR),HFRCOM(MAXHOR),
     +    FRACOM(MXNOD),RPOP(3),FRACON(MXNOD),CN(9),HFRACON(MAXHOR)
      LOGICAL TSCL
      integer k
      CHARACTER string*255
C
      DATA PACT /MXPEST*0.0D0/,XTRCT /MAXHOR*1.0D0/,HFRCOM /MAXHOR*0.0D0
     +    /
C
C
C======================================================================
C==   R E A D   I N   S T A T E   V A R I A B L E S      ==
C======================================================================
C
C ... INITIAL WATER AND TEMPERATURE STATE
      CALL ECHO(INP5)
      READ(INP5,*) IWTYPE,IWCINIT
      READ(INP5,*) (WAT(I),TEM(I),I=1,NHOR)
C
C     ... INITIAL SOIL CHEMISTRY STATE
      CALL ECHO(INP5)
      DO 10 I=1,NHOR
        READ(INP5,*) TPH(I),(TSCL(I,J),J=1,3),TCO2(I),TCEC(I),TCA(I),
     +      TNA(I),TMG(I),TNH4(I),TAL(I),XTRCT(I)
        READ(INP5,*) (TCONC(I,J),J=2,9)
        TCONC(I,1)=10.0D0**(-TPH(I))
   10 CONTINUE
C
C     ... INITIAL NUTRIENT STATE
      CALL ECHO(INP5)
        read (inp5,1110) string
        k=inumb(string)
        backspace (inp5)
 1110 FORMAT(A255)

        DO 20 I=1,NHOR
      IF (K.EQ. 14) THEN      
      READ(INP5,*) (TXNU(I,J),J=1,14)
      Labp(i)=0.0D0
      Stabop(i)=0.0D0
      Frsop(i)=0.0D0      
      ELSE IF (K.EQ. 17) THEN
      READ(INP5,*) (TXNU(I,J),J=1,14),Labp(i),Stabop(i),Frsop(i)
      ELSE
      READ(INP5,*) (TXNU(I,J),J=1,14),Labp(i),Stabop(i),Frsop(i), 
     + RatioSIPtoAIP(i)
      ENDIF

   20 CONTINUE
      
      DO i=1,Nsoil
          Crpres(i)= ((TXNU(i,1)+TXNU(i,2))*Tsoil(i)*PDsoil(i))/
     +         (100.00*0.56)
      END DO
C
C     ... INITIAL PESTICIDE STATE
      CALL ECHO(INP5)
      DO 30 I=1,NPEST
        READ(INP5,*) IDUM,COPLNT(I),CORES(I)
C
C       ..CHECK FOR THE PRESENCE OF MULCH OR CANOPY FOR CHEMICAL TO BE ON
        IF(RM.LE.0.0D0) CORES(I)=0.0D0
        COPLNT(I)=0.0D0
   30 CONTINUE
      DO 40 I=1,NHOR
        READ(INP5,*) (TPEST(I,J),J=1,NPEST)
   40 CONTINUE
C
      REWIND(INP5)  !liwang Ma, 10-19-2007
c      CLOSE(UNIT=INP5)
C
      DO 70 I=1,NHOR
        IF(IWTYPE.NE.0) THEN
C
C         ..THETA INPUT
          IF(WAT(I).LT.SOILHP(6,I)) THEN
            TWAT(I)=MIN(WAT(I),SOILHP(6,I)*AEF)
            THEAD(I)=0.D0
C
C           ,, MUST CALL BOTH TO INITIALIZE LOCAL ARRAYS
            THEAD(I)=WCH(THEAD(I),TWAT(I),SOILHP(1,I),I,-1)
            TWAT(I)=WC(THEAD(I),SOILHP(1,I),I,-1)
          ELSE
            TWAT(I)=WAT(I)
            THEAD(I)=0.D0
          ENDIF
        ELSE
C
C         ..HEAD INPUT
          THEAD(I)=WAT(I)
          TWAT(I)=WC(THEAD(I),SOILHP(1,I),I,-1)
          TWAT(I)=MIN(TWAT(I),SOILHP(6,I)*AEF)
          THEAD(I)=WCH(THEAD(I),TWAT(I),SOILHP(1,I),I,-1)
        ENDIF
C
C       ..GET CHEMISTRY CONVERSION FACTOR UG/G ==> MG/L
        CONV=SOILPP(3,I)/TWAT(I)
C
C       ..CONVERT CHEMISTRY SYSTEM
        DO 50 J=2,MXCHEM
          TCONC(I,J)=TCONC(I,J)*CONV
   50   CONTINUE
        TCONC(I,11)=TCONC(I,9)
        TCONC(I,9)=TXNU(I,11)*CONV
        TCONC(I,10)=TXNU(I,12)*CONV
        TCONC(I,12)=TXNU(I,10)*CONV
        TXNU(I,11)=TCONC(I,9)
        TXNU(I,12)=TCONC(I,10)
        TXNU(I,10)=TCONC(I,12)
C
C       ..TOTAL HUMUS AND CONVERT FROM G-CARBON ==> G-OM
        TOTWT=(TXNU(I,1)+TXNU(I,2)+TXNU(I,3)+TXNU(I,4)+TXNU(I,5))*CARBCV
        TOTWT=TOTWT+(TXNU(I,7)/RPOP(1)+TXNU(I,8)/RPOP(2)+TXNU(I,9)/
     +      RPOP(3))*CARBCV
        WHUMUS=TOTWT*HORTHK(I)*SOILPP(3,I)/1.0D6
        WTSOIL=HORTHK(I)*SOILPP(3,I)
C
C       ..UPDATE FRACTION OF ORGANIC MATTER IN SOIL
        HFRCOM(I)=WHUMUS/WTSOIL
        TOTON=TXNU(I,1)/CN(1)+TXNU(I,2)/CN(2)+TXNU(I,3)/CN(3)+
     &        TXNU(I,4)/CN(4)+TXNU(I,5)/CN(5)+TXNU(I,7)/RPOP(1)/CN(7)+
     &        TXNU(I,8)/RPOP(2)/CN(8)+TXNU(I,9)/RPOP(3)/CN(9)
        HFRACON(I)=TOTON*HORTHK(I)*SOILPP(3,I)/1.0D6/WTSOIL
        DO 60 IP=1,NPEST
          TKD=0.58D0*HFRCOM(I)*XKOC(IP)
          TCONC(I,IP+12)=TPEST(I,IP)/(TWAT(I)/SOILPP(3,I)+TKD)
          PACT(IP)=PACT(IP)+TPEST(I,IP)+COPLNT(IP)+CORES(IP)
          IF(PACT(IP).GT.0.0D0) IPACTV(IP)=1
   60   CONTINUE
   70 CONTINUE
C
      DO 100 I=1,NN
        IH=NDXN2H(I)
        H(I)=THEAD(IH)
        THETA(I)=TWAT(IH)
        T(I)=TEM(IH)
        PH(I)=TPH(IH)
        SCL(I,1)=TSCL(IH,1)
        SCL(I,2)=TSCL(IH,2)
        SCL(I,3)=TSCL(IH,3)
        IF(TCO2(IH).EQ.0.0D0) THEN
C         .. SET PARTIAL PRESSURE CO2 TO ATMOSHPERIC VALUE
          PCO2(I)=0.00033D0   !change to 370 ppm Liwang Ma
        ELSE
          PCO2(I)=TCO2(IH)
        ENDIF
        CEC(I)=TCEC(IH)
        ECA(I)=TCA(IH)
        ENA(I)=TNA(IH)
        EMG(I)=TMG(IH)
        ENH4(I)=TNH4(IH)
        EAL(I)=TAL(IH)
        DO 80 J=1,MXCHEM
          CONC(I,J)=TCONC(IH,J)
   80   CONTINUE
        DO 90 J=1,13
          XNU(I,J)=TXNU(IH,J)
   90   CONTINUE
        XNU(I,13)=TXNU(IH,14)
        FRACOM(I)=HFRCOM(IH)
        FRACON(I)=HFRACON(IH)
        if (fracom(i).gt.1.0d0) stop 'organic fraction is above 100%'
C
C       ..GET RESIDUE MASS AND CONVERT FROM UG-C/G-SOIL ==> G-C/CM**2
        FACT=1.0D6/(TL(I)*BD(I))
        RPOOL(I,1)=XNU(I,1)/FACT
        RPOOL(I,2)=XNU(I,2)/FACT
C
  100 CONTINUE
C
      CALL VGATE(DBLE(JBDAY-1),-2,PH)
      CALL SGATE(DBLE(JBDAY-1),-2,PH(1))
      CALL VGATE(DBLE(JBDAY-1),16,PH)
      DO 110 I=1,5
        CALL VGATE(DBLE(JBDAY-1),I+3,XNU(1,I))
  110 CONTINUE
      CALL VGATE(TDAY,9,CONC(1,9))
      CALL VGATE(TDAY,11,CONC(1,10))
      CALL VGATE(DBLE(JBDAY-1),17,XNU(1,7))
      CALL VGATE(DBLE(JBDAY-1),18,XNU(1,9))
      CALL VGATE(DBLE(JBDAY-1),19,XNU(1,8))
C
      RETURN
c 1000 FORMAT(A80)
      END
C
      SUBROUTINE INPDAY(TMIN,TMAX,RTS,U,EPAN,ATMP,RH,INP3,JDAY,IYYY,
     +    IMAGIC,METMOD,RZPAR,CO2R,iswpar,CO2A)
C
C======================================================================
C
C       PURPOSE:  READ IN DAILY METEOROLOGY FOR MODEL
C
C       REF:  RELATIVE HUMIDITY EQU.  CLARENCE RICHARDSON, ARS, TX
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ATMP  I/O ATMOSPHERIC PRESSURE CORRECTED FOR ELEV [KPA]
C       DEW        L
C       EPAN  I/O DAILY PAN EVAPORATION [CM]
C       IDUM   L
C       IDYR   L
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       INP3   I  POINTER TO DAYMET.DAT DATAFILE
C       IYYY   I  --YEAR
C       JDAY   I  JULIAN DAY    [1..366]
C       RH        I/O RELATIVE HUMIDITY [0..100]
C       RTS       I/O TOTAL S-W RADIATION INCOMING [MJ/M^2/DAY]
C       SATVP  I
C       SMAX   L
C       TMAX  I/O MAXIMUM AIR TEMPERATURE [C]
C       TMIN  I/O MINIMUM AIR TEMPERATURE [C]
C       TRHN   P
C       TRHX   P
C       TRN        P
C       TRX        P
C       TTN        P
C       TTX        P
C       TUN        P
C       TUX        P
C       U     I/O WIND SPEED [KM/DAY]
C       UBREEZ     P
C
C
C       COMMENTS:  WILL ESTIMATE WIND RUN AS A BREEZE (20 KM/DAY) IF
C              VALUE ENTERED IS = 0.0
C              RELATIVE HUMIDITY WILL BE ESTIMATED FROM MIN AND
C              MAX TEMPERATURE IF NOT GIVEN
C
C
C       MASS STORAGE FILES:
C
C
C       EXTERNAL REFERENCES:
C                 C
C
C
C       CALLED FROM:  MAIN
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION METMOD(8,12)
      character iswpar*1,string*255
      PARAMETER(TTN=-50.0D0,TTX=50.0D0,TUN=45.0D0,TUX=4700.0D0,TRN=
     +    0.0D0,TRX=45.0D0,TRHN=0.0D0,TRHX=100.0D0,UBREEZ=100.0D0)

C
C ... STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      SATVP(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))

c      read (inp3,1900) string
c        k=inumb(string)
c      backspace (inp3)
      IF(IMAGIC.NE.1) THEN
10      if (iswpar.eq.'N' .or. iswpar.eq.'n') then
        READ(INP3,*,END=30) IDUM,IDYR,TMIN,TMAX,U,RTS,EPAN,RH
c          RZPAR=RTS*2.0D0          !UNIT CHANGE MADE IT DOUBLLE
          RZPAR=0.0D0          !UNIT CHANGE MADE IT DOUBLLE
      else if (iswpar.eq.'Y' .or. iswpar.eq.'y') then
        READ(INP3,*,END=30) IDUM,IDYR,TMIN,TMAX,U,RTS,EPAN,RH,RZPAR
      endif
c          CO2R=330.0D0  !change to 370 ppm
c          CO2R=380.0D0  !change to 370 ppm
        IF(IDUM.NE.JDAY) GOTO 10
        IF(IDYR.NE.IYYY) GOTO 10
        U=MAX(U,UBREEZ)
        ATMP=101.32D0
        IF(RH.EQ.0.0D0) THEN
          DEW=SATVP(TMIN)
          SMAX=SATVP(TMAX)
          RH=DEW/((DEW+SMAX)*0.5D0)*1.0D2
        ENDIF
C
C       .. APPLY METEOROLOGY MODIFIERS
        CALL CDATE(JDAY,ID,IM,IYYY)
        TMIN=TMIN+METMOD(1,IM)
        TMAX=TMAX+METMOD(2,IM)
        U=U*METMOD(3,IM)*1.0D-2
        RTS=RTS*METMOD(4,IM)*1.0D-2
        EPAN=EPAN*METMOD(5,IM)*1.0D-2
        RH=RH*METMOD(6,IM)*1.0D-2
        CO2R=CO2A*METMOD(8,IM)*1.0D-2
C
c output irregularity in weather data
        IF(TMIN.GT.TMAX) then
             WRITE(*,1000) IDUM,IDYR,TMIN,TMAX
             stop
        endif
        IF(TMIN.LT.TTN) WRITE(*,1100) IDUM,IDYR,TMIN,TTN
        IF(TMAX.GT.TTX) WRITE(*,1200) IDUM,IDYR,TMAX,TTX
        IF(U.LT.TUN.AND.U.NE.0.0D0) WRITE(*,1300) IDUM,IDYR,U,TUN
        IF(U.GT.TUX) WRITE(*,1400) IDUM,IDYR,U,TUX
        IF(RTS.LT.TRN) WRITE(*,1500) IDUM,IDYR,RTS,TRN
        IF(RTS.GT.TRX) WRITE(*,1600) IDUM,IDYR,RTS,TRX
        IF(RH.LT.TRHN) WRITE(*,1700) IDUM,IDYR,RH,TRHN
        IF(RH.GT.TRHX) WRITE(*,1800) IDUM,IDYR,RH,TRHX
C       ..BOUND THE INPUT DATA
        TMIN=MAX(TMIN,TTN)
        TMAX=MIN(TMAX,TTX)
        U=MAX(U,TUN)
        U=MIN(U,TUX)
        RTS=MAX(RTS,TRN)
        RTS=MIN(RTS,TRX)
        RH=MAX(RH,TRHN)
        RH=MIN(RH,TRHX)
c
      ELSE
C
C       ..CHECK DATA FILE INTEGRITY
   20   READ(INP3,*,END=30) IDUM,IDYR,TMIN,TMAX,U,RTS,EPAN,RH
        IF(TMIN.GT.TMAX) WRITE(*,1000) IDUM,IDYR,TMIN,TMAX
        IF(TMIN.LT.TTN) WRITE(*,1100) IDUM,IDYR,TMIN,TTN
        IF(TMAX.GT.TTX) WRITE(*,1200) IDUM,IDYR,TMAX,TTX
        IF(U.LT.TUN.AND.U.NE.0.0D0) WRITE(*,1300) IDUM,IDYR,U,TUN
        IF(U.GT.TUX) WRITE(*,1400) IDUM,IDYR,U,TUX
        IF(RTS.LT.TRN) WRITE(*,1500) IDUM,IDYR,RTS,TRN
        IF(RTS.GT.TRX) WRITE(*,1600) IDUM,IDYR,RTS,TRX
        IF(RH.LT.TRHN) WRITE(*,1700) IDUM,IDYR,RH,TRHN
        IF(RH.GT.TRHX) WRITE(*,1800) IDUM,IDYR,RH,TRHX
        GOTO 20
      ENDIF
C
      RETURN
   30 PRINT*,'>>>> END OF FILE REACHED IN DAYMET.DAT <<<<'
      IF(IMAGIC.EQ.1) PRINT*,'  DAYMET.DAT VERIFICATION COMPLETED'
      PRINT*,'     PROGRAM TERMINATED'
      STOP
 1000 FORMAT(I4,I5,' MIN AIR TEMP',F5.1,'> MAX AIR TEMP',F5.1)
 1100 FORMAT(I4,I5,' MIN AIR TEMP',F5.1,' < MIN BOUND',F5.1)
 1200 FORMAT(I4,I5,' MAX AIR TEMP',F5.1,' > MAX BOUND',F5.1)
 1300 FORMAT(I4,I5,' WIND SPEED',F5.1,' < MIN BOUND',F5.1)
 1400 FORMAT(I4,I5,' WIND SPEED',F5.1,' > MAX BOUND',F5.1)
 1500 FORMAT(I4,I5,' SHORTWAVE RADIATION',F5.1,' < MIN BOUND',F5.1)
 1600 FORMAT(I4,I5,' SHORTWAVE RADIATION',F5.1,' > MAX BOUND',F5.1)
 1700 FORMAT(I4,I5,' REL HUMIDITY',F5.1,' < MIN BOUND',F5.1)
 1800 FORMAT(I4,I5,' REL HUMIDITY',F5.1,' > MAX BOUND',F5.1)
c 1900 format('A123')
      END
C
      SUBROUTINE INPHOUR(TMIN,TMAX,RTS,U,EPAN,ATMP,RH,INP3,JDAY,IYYY,
     +    IMAGIC,METMOD,RZPAR,CO2R,iswpar,HRT,HRH,HRU,HRTS,HEPAN,hrzpar
     +    ,CO2A)
C
C======================================================================
C
C       PURPOSE:  READ IN HOURLY METEOROLOGY FOR MODEL
C
C       REF:  RELATIVE HUMIDITY EQU.  CLARENCE RICHARDSON, ARS, TX
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ATMP  I/O ATMOSPHERIC PRESSURE CORRECTED FOR ELEV [KPA]
C       DEW        L
C       EPAN  I/O DAILY PAN EVAPORATION [CM]
c       hrt       i/o   bounded array of hourly ambient temperature [C]
c       hrts_si   i/o   bounded array of hourly horizontal solar radiation [W/m2]
c       hru       i/o   bounded array of hourly reference wind speed [m/s]
c       hrh       i/o   bounded array of hourly reference relative humidity [%]
C       IDUM   L
C       IDYR   L
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       INP3   I  POINTER TO DAYMET.DAT DATAFILE
C       IYYY   I  --YEAR
C       JDAY   I  JULIAN DAY    [1..366]
C       RH        I/O RELATIVE HUMIDITY [0..100]
C       RTS       I/O TOTAL S-W RADIATION INCOMING [MJ/M^2/DAY]
C       SATVP  I
C       SMAX   L
C       TMAX  I/O MAXIMUM AIR TEMPERATURE [C]
C       TMIN  I/O MINIMUM AIR TEMPERATURE [C]
C       TRHN   P
C       TRHX   P
C       TRN        P
C       TRX        P
C       TTN        P
C       TTX        P
C       TUN        P
C       TUX        P
C       U     I/O WIND SPEED [KM/DAY]
C       UBREEZ     P
C
C
C       COMMENTS:  WILL ESTIMATE WIND RUN AS A BREEZE (20 KM/DAY) IF
C              VALUE ENTERED IS = 0.0
C              RELATIVE HUMIDITY WILL BE ESTIMATED FROM MIN AND
C              MAX TEMPERATURE IF NOT GIVEN
C
C
C       MASS STORAGE FILES:
C
C
C       EXTERNAL REFERENCES:
C                 C
C
C
C       CALLED FROM:  MAIN
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION METMOD(8,12)
      character iswpar*1
      PARAMETER(TTN=-50.0D0,TTX=50.0D0,TUN=45.0D0,TUX=4700.0D0,TRN=
     +    0.0D0,TRX=45.0D0,TRHN=0.0D0,TRHX=100.0D0,UBREEZ=100.0D0,
     +    MHR=24)
      dimension hrt(mhr),hrts(mhr),hru(mhr),hrh(mhr),HRZPAR(MHR),
     +          HEPAN(MHR)

C
C ... STATEMENT FUNCTION FOR SATURATED VAPOR PRESSURE CURVE
      SATVP(T)=EXP((16.78D0*T-116.9D0)/(T+237.3D0))

c      IF(IMAGIC.NE.1) THEN
10      if (iswpar.eq.'N' .or. iswpar.eq.'n') then
C        READ(INP3,*,END=30) IDUM,IDYR,TMIN,TMAX,U,RTS,EPAN,RH
         DO I=1,24
         READ (INP3,*) IDUM,IDYR,ihr,hrt(i),HRU(I),hrts(i),
     +              HEPAN(I),hrh(i)
c         HRZPAR(I)=2.0*hrts(i)
         HRZPAR(I)=0.0d0
         ENDDO
      else if (iswpar.eq.'Y' .or. iswpar.eq.'y') then
C        READ(INP3,*,END=30) IDUM,IDYR,TMIN,TMAX,U,RTS,EPAN,RH,RZPAR
         DO I=1,24
         READ (INP3,*) IDUM,IDYR,ihr,hrt(i),HRU(I),hrts(i),HEPAN(I),
     +                hrh(i),HRZPAR(I)
         ENDDO
      endif
c        CO2R=330.0D0   !change to 370ppm
c        CO2R=380.0D0   !change to 370ppm
        IF(IDUM.NE.JDAY) GOTO 10
        IF(IDYR.NE.IYYY) GOTO 10
c accumulate daily solar radiation, windrun, humidity
        RTS = 0.0D0
        RZPAR = 0.0D0
        RH_SUM = 0.0D0
        U = 0.0D0
        TMAX = -50.0D0
        TMIN = 50.0D0
        EPAN=0.0D0
	  rhmax=0.0d0
	  rhmin=100.d0
        avg_VP=0.0d0
        avg_T=0.0d0
C
C       .. APPLY METEOROLOGY MODIFIERS, SHOULD IT APPLY TO HOURLY?
        CALL CDATE(JDAY,ID,IM,IYYY)
        do i=1,24
        hrT(i)=hrt(i)+(METMOD(1,IM)+METMOD(2,IM))/2.0d0
C        TMAX=TMAX+METMOD(2,IM)
        hrU(i)=hrU(i)*METMOD(3,IM)*1.0D-2
        hRTS(i)=hRTS(i)*METMOD(4,IM)*1.0D-2
        hEPAN(i)=hEPAN(i)*METMOD(5,IM)*1.0D-2
        hRH(i)=hRH(i)*METMOD(6,IM)*1.0D-2
        enddo
        CO2R=CO2A*METMOD(8,IM)*1.0D-2
C
        DO I=1,24
        hrts(i)=max(hrts(i),0.0d0)
        if (hru(i).le.0.0d0) then
         hru(i)=max(hru(i),1.157d0)  !100*1000/24/60/60=1.157 m/s.
        else
         hru(i)=max(hru(i),0.5d0)  !check with Gerald Flerchinger.
        endif
c        if (hrzpar(i).le.0.0d0) hrzpar(i)=hrts(i)*2.0d0
        rts = rts + max(0.d0,hrts(i)*3.6d3/1.d6)  !*0.001*60)
        RZPAR = RZPAR + max(0.d0,HRZPAR(i)*3.6d3/1.d6) !0.001*60)
        EPAN=EPAN+HEPAN(I)
        if (hrh(i).lt.1.0d0) then
        hrh(i)= hrh(i)*100.d0
        endif
        rh_sum = rh_sum + hrh(i)
        rhmax = max(0.0d0,hrh(i),rhmax)
        rhmin = min(100.d0,hrh(i),rhmin)
        u = u + hru(i)*3.6d0
c find Tmax,Tmin
        tmax = max(tmax,hrt(i))
        tmin = min(tmin,hrt(i))
        avg_VP=avg_VP+satvp(hrt(i))*hrh(i)/24.0d0
        avg_T=avg_T+hrt(i)/24.0d0
        ENDDO
c
        RH=avg_VP/satvp(avg_T)
c compute relative humidity if data not present
c
        Tdew=tmin
        if (rh_sum .lt. 0.001d0) then
          do i = 1,24
            DEW=SATVP(Tdew)
            S_Tair = SATVP(hrt(i))
            hrh(i) = (DEW / S_Tair) * 1.0D2
C         ..BOUND THE INPUT DATA
            hrh(i) = MAX(hrh(i),TRHN)
            hrh(i) = MIN(hrh(i),TRHX)
            rhmax = max(0.0d0,hrh(i),rhmax)
            rhmin = min(100.d0,hrh(i),rhmin)
          ENDDO
        ENDIF
c        RH = RH_SUM/24.0D0
c this is from Richard Allen FAO No. 56, eq. 17= eq. 19 as stamp proven by Gerald Flerchinger.
c        RH = (satvp(tmin)*RHmax + satvp(tmax)*rhmin)*0.5d0/
c     +       ((satvp(tmin)+satvp(tmax))*0.5d0)
        
        U=MAX(U,UBREEZ)
        ATMP=101.32D0
        IF(RH.EQ.0.0D0) THEN
          DEW=SATVP(Tdew)
          SMAX=SATVP(TMAX)
          RH=DEW/((DEW+SMAX)*0.5D0)*1.0D2
        ENDIF
C
C       ..BOUND THE INPUT DATA
        TMIN=MAX(TMIN,TTN)
        TMAX=MIN(TMAX,TTX)
        U=MAX(U,TUN)
        U=MIN(U,TUX)
        RTS=MAX(RTS,TRN)
        RTS=MIN(RTS,TRX)
        RH=MAX(RH,TRHN)
        RH=MIN(RH,TRHX)
c      ELSE
C
C       ..CHECK DATA FILE INTEGRITY
c   20   READ(INP3,*,END=30) IDUM,IDYR,TMIN,TMAX,U,RTS,EPAN,RH
        IF(TMIN.GT.TMAX) WRITE(*,1000) IDUM,IDYR,TMIN,TMAX
        IF(TMIN.LT.TTN) WRITE(*,1100) IDUM,IDYR,TMIN,TTN
        IF(TMAX.GT.TTX) WRITE(*,1200) IDUM,IDYR,TMAX,TTX
        IF(U.LT.TUN.AND.U.NE.0.0D0) WRITE(*,1300) IDUM,IDYR,U,TUN
        IF(U.GT.TUX) WRITE(*,1400) IDUM,IDYR,U,TUX
        IF(RTS.LT.TRN) WRITE(*,1500) IDUM,IDYR,RTS,TRN
        IF(RTS.GT.TRX) WRITE(*,1600) IDUM,IDYR,RTS,TRX
        IF(RH.LT.TRHN) WRITE(*,1700) IDUM,IDYR,RH,TRHN
        IF(RH.GT.TRHX) WRITE(*,1800) IDUM,IDYR,RH,TRHX
c        GOTO 20
c      ENDIF
C
      RETURN
   30 PRINT*,'>>>> END OF FILE REACHED IN DAYMET.DAT <<<<'
      IF(IMAGIC.EQ.1) PRINT*,'  DAYMET.DAT VERIFICATION COMPLETED'
      PRINT*,'     PROGRAM TERMINATED'
      STOP
 1000 FORMAT(I4,I5,' MIN AIR TEMP',F5.1,'> MAX AIR TEMP',F5.1)
 1100 FORMAT(I4,I5,' MIN AIR TEMP',F5.1,' < MIN BOUND',F5.1)
 1200 FORMAT(I4,I5,' MAX AIR TEMP',F5.1,' > MAX BOUND',F5.1)
 1300 FORMAT(I4,I5,' WIND SPEED',F5.1,' < MIN BOUND',F5.1)
 1400 FORMAT(I4,I5,' WIND SPEED',F5.1,' > MAX BOUND',F5.1)
 1500 FORMAT(I4,I5,' SHORTWAVE RADIATION',F5.1,' < MIN BOUND',F5.1)
 1600 FORMAT(I4,I5,' SHORTWAVE RADIATION',F5.1,' > MAX BOUND',F5.1)
 1700 FORMAT(I4,I5,' REL HUMIDITY',F5.1,' < MIN BOUND',F5.1)
 1800 FORMAT(I4,I5,' REL HUMIDITY',F5.1,' > MAX BOUND',F5.1)
      END
C
      SUBROUTINE INPUT(IRRTYP,INP1,INP2,NSC,TWL,INXPL,EFFLUX,IRTYPE,
     +    IBRKTH,ICHEM,IMAGIC,NOSTAR,NUTEQ,INP6,FTR,IPR,hydgrad,
     +    IHOURLY,ISHAW,IPENFLUX,iwzone,wsi,Hmin,iyb,istress,CO2A,
     +    JULBDAY,JULEDAY,niw,ipsw_depth,ipet,CKinit,CKmax,emitr0,pstr)
C
C======================================================================
C
C       PURPOSE:  THE MAIN PARAMETERIZATION DATAFILE INPUT ROUTINE
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       A     I/O CONSTANT FOR NON-UNIFORM MIXING []
C       A0        I/O ALBEDO OF DRY SOIL
C       AC        I/O ALBEDO OF PLANT CANOPY [0..1]
C       ADCAY I/O
C       ADCAYV    I/O 5-LONG VECTOR OF COEF'S FOR AHRENIUS OM DECAY NS,
C                 FOR OM POOLS  [SEC/DAY]
C       ADEATH    I/O 3-LONG VECTOR OF COEF'S FOR BM DEATH EQNS [SEC/DAY]
C       ADENIT    I/O COEF FOR AHRENIUS EQN FOR DENITRIFICATION, [SEC/DAY]
C       AEF       I/O FIELD SATURATION FRACTION [0..1]
C       AGSZ  I/O AVE. LENGTH OF AGGREGATE IN HORIZON [CM]
C       AHLDG I/O
C       ALA       I/O ALUMINUM SATURATION BELOW WHICH ROOT GROWTH IS
C                 UNAFFECTED (%)
C       ALNAVL    I/O
C       ALNGER    I/O
C       ALNLW I/O
C       ALPHA I/O TEMPORARY STORAGE OF COEFF
C       ALX       I/O ALUMINUM SATURATION ABOVE WHICH ROOT GROWTH IS
C                 NEGLIGIBLE (%)
C       ANIT  I/O COEF FOR AHRENIUS EQN FOR NITRIFICATION, [SEC/DAY]
C       APP       I/O
C       AREA  I/O AREA OF FIELD [HA]
C       ARI       I/O ALBEDO OF FRESH RESIDUE [0..1]
C       ASPECT    I/O ASPECT ANGLE CLOCKWISE FROM NORTH [RADIANS]
C       AUREA I/O COEF FOR AHRENIUS EQN FOR HYDROLYSIS OF UREA[SEC/DAY]
C       AW        I/O ALBEDO WET SOIL [0..1]
C       B     I/O CONSTANT FOR NON-UNIFORM MIXING []
C       BETA  I/O TEMPORARY STORAGE OF COEFF
C       BGSEED    I/O
C       CAA       I/O CALCIUM SATURATION BELOW WHICH ROOT GROWTH IS
C                 REDUCED (%)
C       CANK  I/O
C       CAX       I/O CALCIUM SATURATION BELOW WHICH ROOT GROWTH IS
C                 NEGLIGIBLE (CMOL/KG)
C       CN        I/O 9-LONG VECTOR OF OM & BM C:N RATIOS (IN SAME ORDER
C                 AS IN C VECTOR)
C       CNST  I/O
C       CNUP1 I/O
C       CNUP2 I/O
C       COEPAN    I/O
C       CONVLA    I/O
C       CR        I/O MODIFIER FOR PEST. DISS. DUE TO RESIDUE INTERACTION
C       CRC       I/O COEFF FOR SHUTTLEWORTH EVAP CONSTANTS RESIDUE
C       CRUSTK    I/O CRUST HYRAULIC CONDUCTIVITY [CM/HR]
C       CVLBIO    I/O
C       DELZ  I/O NODAL DEPTH FORWARD DIFFERENCES [CM]
C       DENC  I/O DIMENSIONLESS EFFICIENCY FACTOR:
C                 NIT RATE ==> ANAEROBIC OM DECAY RATE
C       DEVRAT    I/O
C       DHB       I/O DEPTH OF HORIZON BOUNDARIES [CM]
C       DOIMAX    I/O MAXIMUM DEPTH OF IRRIGATION TO BE APPLIED DURING THE
C                 SEASON.  IF DOIMAS = 0.0, THEN THE SEASON'S AMT OF
C                 WATER FOR IRRIGATION IS ASSUMED TO BE UNLIMITED.[CM]
C       DPR       I/O DAUGHTER PRODUCT FORMATION PERCENTAGE [%]
C       DROTOL    I/O
C       DSL       I/O THICKNESS OF INFILTRATION LAYERS [CM]
C       DTSEED    I/O
C       EFFDEN    I/O EFFICIENCY FACTOR FOR DENITRIFICATION (FRACTION FDEN
C                 TAKEN UP BY DENITRIFIERS; REMAINDER -EFFDEN) GOES OFF
C                 AS N2,N2O)
C       EFFLUX    I/O PLANT NITROGEN N-UPTAKE EFFICIENCY FACTOR
C       EFFMAX    I/O BM EFFICIENCY IN CONVERTING DECAYED OM UPTAKE TO
C                 SIMILATED BM (REMAINDER GOES OFF AS RESPIRATION 2)
C       EFFN  I/O
C       EFFNIT I/O
C       EK2       I/O EQUIL.CONST. FOR ADSORPTION [CM3 H2O/G SOIL]
C       ELEV  I/O LOCATION ELEVATION [M]
C       EXTEMP    I/O TEMPERATURE AT WHICH DISS RATE CONSTANT IS MEASURED
C                 OR MODEL DEFAULT TEMPERATURE [C]
C       FDEP  I/O FRACTION OF TOTAL MACROPORES THAT ARE DEADEND [0..1]
C       FDOI  I/O FIXED DEPTH OF IRRIGATION.  A CONSTANT DEPTH OF
C                 WATER TO BE APPLIED WHEN IRRIGATION IS NECESSARY.
C                 CORRESPONDS TO IDOI = 1.[CM]
C       FNH4IN    I/O
C       FNO3IN    I/O
C       FSUN  I/O SUNSHINE FRACTION [0..1]
C       FUREIN    I/O
C       GDDFLG    I/O
C       GITEMP    I/O
C       GIWAT I/O
C       GMN       I/O FRACTION OF NORMAL ROOT GROWTH WHEN PORE SPACE IS
C                 SATURATED [0-1]
C       GRMRAT    I/O
C       GSGDD I/O
C       GSR       I/O GROWTH STAGE WHEN ROOT DEPTH REACHES MAXIMUM [0..1]
C       HALF  I/O PESTICIDE LUMPED HALFLIDE [DAYS]
C       HALFSA    I/O AEROBIC PATHWAY HALF-LIFE FOR PESTICIDE [DAYS]
C       HALFSE    I/O COMPLEXATION PATHWAY HALF-LIFE FOR PESTICIDE [DAYS]
C       HALFSN    I/O PESTICIDE ANAEROBIC DEGRADATION HALF-LIFE [DAYS]
C       HALFSO    I/O PESTICIDE OXIDATION HALF-LIFE [DAYS]
C       HALFSP    I/O PESTICIDE PHOTOLYSIS HALF-LIFE [DAYS]
C       HALFSR    I/O PESTICIDE REDUCTION HALF-LIFE [DAYS]
C       HALFSV    I/O PESTICIDE VOLATILIZATION HALF-LIFE [DAYS]
C       HALFSW    I/O PESTICIDE HYDROLYSIS HALF-LIFE [DAYS]
C       HALFSX    I/O PESTICIDE OTHER DISS HALF-LIFE [DAYS]
C       HC        I/O MAXIMUM HEIGHT OF THE CANOPY [M]
C       HFMAX I/O
C       HORTHK    I/O DEPTH OF SOIL HORIZONS [CM]
C       HPERC I/O
C       HRVEFF    I/O
C       HSTAGE    I/O
C       I      L  INDEX VARIABLE
C       IAPP  I/O
C       ICRUST    I/O FLAG FOR PRESENCE OF SURFACE CRUST [0=NO, 1=YES]
C       ID         L  --DAY OF MONTH [EX 12).
C       IDF        L  --THE FIRST DAY OF IRRIGATION.
C       IDISS I/O FLAG FOR DIFFERENT LOSS ROUTES (MODELS) CALCULATION
C       IDL        L  --THE LAST DAY OF IRRIGATION.
C       IDOI  I/O --USED TO SELECT THE METHOD FOR DETERMINING DEPTH OF
C                 IRRIGATION.  THE LIST IS AS FOLLOWS:
C                 IDOI = 1.  FIXED AMT PER IRRIGATION
C                      2.  VARIABLE AMT PER IRRIGATION
C                      3.  REFILL ROOTZONE TO UPPER LIMIT OF
C                          AVAILABLE WATER
C                      4.  REFILL ROOTZONE TO UPPER LIMIT OF
C                          reference ET
C       IDS        L  --THE DAY OF A SPECIFIED IRRIGATION.
C       IDUM   L
C       IEK2  I/O INDICATES USE OF 2-SITE PESTICIDE ADSORPTION
C       IFMETH    I/O
C       IFOFF I/O
C       IHARV I/O
C       IHCL  I/O
C       IHDAY I/O
C       IHTYPE    I/O
C       IM         L  --MONTH OF YEAR.
C       IMF        L  --THE MONTH IN WHICH THE FIRST IRRIGATION OCCURRED.
C       IML        L
C       IMP       I/O --MONTH OF PLANTING.
C       IMS        L  --THE MONTH OF A SPECIFIED IRRIGATION.
C       INP1   I  POINTER TO CNTRL.DAT DATAFILE
C       INP2   I  POINTER TO RZWQM.DAT DATAFILE
C       IPL        L  PLANT CURRENTLY MODELING [1..MXSPEC]
C       IPLTYP    I/O
C       IPOFF I/O
C       IREBOT    I/O BOTTOM B.C. INDICATOR: 1 (CONSTANT HEAD) OR 2 (UNIT FLUX
C       IRIGDB     L  --BEGINNING DAY OF IRRIGATION.
C       IRIGDE     L  --ENDING DAY OF IRRIGATION.
C       IRIGMB     L  --BEGINNING MONTH OF IRRIGATION.
C       IRIGME     L  --ENDING MONTH OF IRRIGATION.
C       IRIGYB     L  --BEGINNING YEAR OF IRRIGATION.
C       IRIGYE     L  --ENDING YEAR OF IRRIGATION.
C       IRRINT     L  --IN FIXED INTERVAL IRRIGATION, THIS IS THE NUMBER
C                 OF DAYS BETWEEN IRRIGATIONS.
C       ITEXT  L
C       ITL       I/O
C       ITOI  I/O --USED TO DETERMINE WHICH METHOD WILL DETERMINE
C                 WHEN TO IRRIGATE.  THE LIST IS AS FOLLOWS:
C                 ITOI = 1.  FIXED INTERVAL
C                  2.  SPECIFIED DATES FOR THE SEASON
C                  3.  SOIL WATER DEPLETION USING ONLY THE ROOT ZONE
C                  4.  SOIL WATER DEPLETION USING ONLY the reference ET
C       ITYPE  L  INDICATOR FOR SOIL TYPE
C       IWFERT    I/O
C       IWPEST    I/O
C       IYYY   L  --YEAR
C       IYF        L  --THE YEAR OF THE FIRST IRRIGATION.
C       IYL        L  --THE YEAR OF THE LAST IRRIGATION.
C       IYS        L  --THE YEAR OF A SPECIFIED IRRIGATION.
C       IUGFLW     I  TRIGGERS UNIT GRAD FLOW BELOW WETTING FRONT
C       J      L  INDEX VARIABLE
C       JD         L
C       JDAY   L  JULIAN DAY    [1..366]
C       JDIRRB    I/O --JULIAN DATE BEFORE WHICH NO IRRIGATION IS
C                 ALLOWED TO OCCUR.
C       JDIRRE    I/O --JULIAN DATE AFTER WHICH NO IRRIGATION IS
C                 ALLOWED TO OCCUR.
C       JDOFI  L  --JULIAN DATE OF FIRST IRRIGATION (READ IN ONLY).
C       JDOI  I/O --AN ARRAY CONTAINING THE JULIAN DATES OF IRRIGATION.
C       JDOLI  L  --JULIAN DATE OF FINAL IRRIGATION (READ IN ONLY).
C       JKU       I/O --ARRAY OF DAYS SINCE PLANTING CORRESPONDING TO XKU.
C       JM         L  MODIFIED MONTH.
C       JMAD  I/O --ARRAY OF DAYS SINCE PLANTING CORRESPONDING TO XMAD.
C       ITOFF   I/O
C       JY         L  MODIFIED YEAR.
C       LAYNDX    I/O
C       LIRRIG     L
C       LWM       I/O RATIO OF ROOT LENGTH TO WEIGHT IN PLOW LAYER
C                 AT MATURITY [M/G)
C       LWS       I/O NORMAL RATIO OF ROOT LENGTH TO WEIGHT IN
C                 SEEDLING [M/G)
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MCPOR I/O TOTAL MACROPOROSITY OF HORIZON [CM3/CM3]
C       METHOD    I/O
C       MICP  I/O TOTAL MICROPOROSITY OF HORIZON [CM3/CM3]
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NAPP  I/O
C       NAPPF I/O
C       NCLY   L
C       NDAIRR    I/O --MINIMUM NUMBER OF DAYS BETWEEN IRRIGATIONS. (
C                 NOTE:    NOT APPLICABLE FOR ITOI = 1 OR 2. FOR
C                 ITOI = 1 OR 2, SET THE VALUE OF NDAIRR EQUAL T
C                 O ZERO.)
C       NGNPL  L
C       NGPL  I/O
C       NHCMP I/O NUMBER OF HORIZONS CONTAINING MACROPORES
C       NHOR  I/O NUMBER OF SOIL HORIZONS
C       NHZ       I/O NUMBER OF SOIL HORIZONS
C       NKU       I/O --NUMBER OF PAIRS OF JKU AND XKU.
C       NMAD  I/O --NUMBER OF PAIRS OF JMAD AND XMAD.
C       NMICP I/O =1 IF MICROPORSITY VALUES ARE GIVEN, =0 IF NOT
C       NMISC  L
C       NN        I/O NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NNT       I/O NUMBER INTERIOR NODES IN INFILTRATION GRID
C       NOIRR  L  --NUMBER OF IRRIGATIONS WHEN ITOI = 1.
C       NPEST I/O NUMBER OF USER DEFINED PESTICIDES
C       NPGS  I/O
C       NPL       I/O
C       NPR       I/O
C       NSC       I/O CURRENT OUTPUT SCENARIO NUMBER
C       NSL       I/O DEPTH TO BOTTOM OF SOIL HORIZONS [CM]
C       NSLT  I/O PROFILE DEPTH [CM]
C       NUMTL I/O
C       O2        I/O OXYGEN CONTENT OF PEST. DURING ANEROBIC COND. [%]
C       OCRUST    I/O ORIGINAL CRUST HYDRAULIC CONDUCTIVITY [CM/HR]
C       PACTIV     I  TRUE WHEN PESTICIDE IS ACTIVE
C       PCLAY  L  SEDIMENT FRACTION CLAY CONTENT [0..1]
C       PCLDG I/O
C       PDR       I/O
C       PF        I/O MODIFIER FOR DISS. DUE TO PEST. FORMULATION
C       PGDVRT    I/O
C       PGNTGT    I/O
C       PHFBIO     L
C       PL        I/O LENGTH OF VERTICAL MACROPORE CRACKS  [CM]
C       PLALFA    I/O
C       PLBIO I/O
C       PLC       I/O MODIFIER FOR DISS. DUE TO PLANT LEAF CHAR.
C       PLDEN I/O
C       PLDIAM    I/O
C       PLHGHT    I/O
C       PLTDAY    I/O
C       PLTMNT    I/O
C       PLTMXT    I/O
C       PLTOPT    I/O
C       PMAX  I/O
C       PMNNIT    I/O
C       PRB       I/O
C       PRNLW I/O
C       PRODEP     L
C       PSAND  L  SEDIMENT SIZE FRACTION SAND PORTION [0..1]
C       PSILT  L  SEDIMENT SIZE FRACTION SILT PORTION [0..1]
C       PTLAMX    I/O
C       R14       I/O
C       R20       I/O
C       R23       I/O
C       R34       I/O
C       R43       I/O
C       R45       I/O
C       R53       I/O
C       R15       I/O
C       R25       I/O
C       RAC       I/O RAA @ CANOPY [S/M]
C       RAPH   L
C       RATLS I/O
C       RATRSN    I/O
C       RATRSX    I/O
C       RCOEFF    I/O RESIDUE WASH-OFF EQN. COEFFICIENT
C       RDIA  I/O RESIDUE DIAMETER [CM]
C       RDIF  I/O PESTICIDE DIFFUSION RATE [CM^2/HR]
C       RDR       I/O MAXIMUM, NORMAL ROOT DEATH RATE
C                 (PROP. OF ROOT GROWTH)
C       RDX       I/O NORMAL MAXIMUM ROOT SYSTEM DEPTH [M]
C       RESAGE    I/O RESIDUE AGE [DAYS]
C       RHOR  I/O RESIDUE DENSITY [G/CM^3]
C       RK2       I/O KINETIC CONST FOR RELEASE OF PEST FROM
C                 ABSORBTION SITE
C       RM        I/O MASS OF RESIDUE (T/HA)
C       ROWSP I/O
C       RP        I/O RADIUS OF CYLINDRICAL MACROPORES IN HORIZON [CM]
C       RPOP  I/O 3-LONG VECTOR OF BM POPULATION UNIT CONVERSION
C                 CTORS: *RPOP(I): [UG-C/G-SOIL]==>[# ORGS/G-SOIL]
C       RPOWER    I/O RESIDUE WASH-OFF EQN. POWER
C       RQ10  I/O
C       RRATE I/O
C       RRC       I/O IRRIGATION WATER CHEMISTRY [MG/L]
C       RRPH   L
C       RTNLW I/O
C       SCL        I
C       SCOV: SURFACE COVER TYPE FOR P MODEL: 1: BARE; 2= RESIDUE; 3= GRASS/PLANT?
C       SDAMAX    I/O
C       SDDVRT    I/O
C       SDSVRT    I/O
C       SDTMGM    I/O
C       SDWMAX    I/O
C       SFREEZ    I/O
C       SLA1  I/O
C       SLA2  I/O
C       SLA3  I/O
C       SLA4  I/O
C       SLOPE I/O SLOPE OF FIELD [RAD]
C       SOILHP    I/O MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       SOILPP    I/O
C       SOL       I/O WATER SOLUBILITY OF PESTICIDE [UG/L]
C       SOLTP1    I/O ARRAY OF HEAT MODEL PARAMETERS,
C                  1: SAT MOISTURE CONTENT [0..1]
C                  2: FIELD CAPACITY [0..1]
C                  3: 15 bar soil moisture [0-1]   !EXTURE CLASS (1-COARSE, 2-MED, 3-FINE)
C                  3: 15 BAR SOIL MOISTURE   !TEXTURE CLASS (1-COARSE, 2-MED, 3-FINE)
C                  4: # CONSTITUENTS FOR THERMAL PROPERTY CALC
C                  5: DRY VOL HEAT CAPACITY  [J/MM^3/C]
C       SOLTP2    I/O (MAXSCT X MAXHOR X 3) ARRAY OF SOIL HORIZON
C                 CONSTITUENT PROPERTIES, CONSTIT'S BY ROW,
C                 HORIZON BY COL, PROPERTY BY PLANE
C                 1: CONSTITUENT VOLUME FRACTIONS
C                 2: CONSTITUENT HEAT CONDUCT [J/MM/HR/C],
C                 3: PARTICLE SHAPE FACTORS GA
C       SSC       I/O MODIFIER FOR PEST. DISS. DUE TO SOIL SURFACE
C                 CHARACTERISTICS [T-1]
C       STEND I/O
C       STNAVL    I/O
C       STNGER    I/O
C       STNLW I/O
C       STUBHT    I/O
C       TBD        L
C       TBS       I/O BASE TEMPERATURE FOR ROOT GROWTH (C)
C       TL        I/O NUMERICAL LAYER THICKNESSES [CM]
C       TLT       I/O DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       TNDX  I/O
C       TOP       I/O OPTIMUM TEMPERATURE FOR ROOT GROWTH (C)
C       TOT4WT    I/O
C       TWL       I/O
C       IRRTYP    I/O --TYPE OF IRRIGATION, IE. SPRINKLER, FLOOD,
C                 FURROW, DRIP, or SUBSURFACE.
C       PCOEFF    I/O PLANT CANOPY WASH-OFF EQN. COEFFICIENT
C       UDEPTH    I/O
C       PPOWER    I/O PLANT CANOPY WASH-OFF EQN. POWER
C       VDOI  I/O VARYING DEPTH OF IRRIGATION (INCHES).  ENABLES
C                 THE USER TO INPUT VARYING DEPTHS OF IRRIGATION
C                 OVER THE COURSE OF THE SEASON.  SEE JDOI.[CM]
C       VRNLZ I/O
C       WCG       I/O WEIGHTING COEFFICIENT - GEOTROPISM
C       WCP       I/O WEIGHTING COEFFICIENT FOR PLASTICITY
C       WDLDG I/O
C       WP        I/O WIDTH OF RECTANGULAR CRACKS IN HORIZON [CM]
C       XPKA  I/O EQUILIBRIUM CONSTANT FOR ACID DISSOCIATION
C       XPKB  I/O EQUILIBRIUM CONSTANT FOR BASE PROTONATION
C       XKH       I/O HENRY'S LAW CONSTANT FOR A PESTICIDE
C       XKOC  I/O PESTICIDE ADSORPTION CONSTANT CORR FOR SOIL OM
C       XKP       I/O COEF FOR CALC OF ACTIVATION ENERGY
C       XKU       I/O MODIFYING FACTOR USED TO INCREASE OR DECREASE THE
C                 UPPER LIMIT TO WHICH THE SOIL CAN BE RECHARGED.
C                 IF = 1 THEN THE SOIL IS RECHARGED TO FIELD CAPACITY.
C                 IF < 1 THEN THE SOIL IS RECHARGED TO BELOW FIELD
C                      CAPACITY (DEFICIT IRRIGATION)
C                 IF > 1 THEN THE SOIL IS RECHARGED TO ABOVE FIELD
C                      CAPACITY (LEACHING).
C                 XKU MAY BE VARIED OVER THE COURSE OF THE SEASON.
C       XLAT  I/O LATITUDE OF FIELD
C       XMAD  I/O DEC.%--LIMITING VALUE OF MAXIMUM ALLOWABLE SOIL
C                 WATER DEF BEYOND WHICH IRRIGATION IS NECESSARY
C       XMISC  L
C       XMW       I/O MOLECULAR WEIGHT OF PESTICIDE [G/MOLE]
C       XW        I/O MOISTURE CONTENT CONTINUOUS MEDIUM CONTENT [0..1]
C       YESMAC    I/O INDICATS PRESENCE OF MACROPORES =1 YES, =0 NOT
C       ZN        I/O DEPTH TO NUMERICAL NODES [CM]
C
C       COMMENTS:
C
C       MASS STORAGE FILES:
C
C       EXTERNAL REFERENCES:
C                 ECHO
C                 INDEXR
C                 JDATE
C                 OUTIN1
C                 SOILPR
C
C       CALLED FROM:  MAIN
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  3.0
C
C======================================================================
C     
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C   ARRAY DIMENSION VALUES
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,MXPEST=3,
     +    MXAPP=200,MXSPEC=10,MXCHEM=15,MXTG=500)
C
      COMMON /IRRIG/ DOIMAX(MXAPP),FDOI(MXAPP),VDOI(MXAPP,MXAPP),
     +    XKU(MXAPP,20),XMAD(MXAPP,20),RRATE(MXAPP),amxirr,
     +    totaliw,DOIMONTH(12,200),TOTALMONTH,airrdepth,amirr,
     +    RRDUR(MXAPP),IIPL(MXAPP),ISPT(MXAPP),ITOI(MXAPP),
     +    JDIRRB(MXAPP),JDIRRE(MXAPP),NAPPI,NDAIRR(MXAPP),NKU(MXAPP),
     +    NMAD(MXAPP),JDOI(MXAPP,MXAPP),JKU(MXAPP,20),JMAD(MXAPP,20),
     +    IDOI(MXAPP),id_depth,MONTHIRR
c
      DOUBLE PRECISION AHLDG,ALA,ALNAVL,ALNGER,ALNLW,ALPHA,ALX,BETA,
     +    BGSEED,CAA,CANK,CAX,CONVLA,CVLBIO,DEVRAT,DROTOL,DTSEED,GITEMP,
     +    GIWAT,GMN,GRMRAT,GSR,HFMAX,LWM,LWS,PCLDG,PGDVRT,PGNTGT,PLALFA,
     +    PLBIO,PLDIAM,PLHGHT,PLTMNT,PLTMXT,PLTOPT,PMAX,PMNNIT,PRNLW,
     +    PTLAMX,R20,RATLS,RATRSX,RATRSN,RDR,RDX,RQ10,RTNLW,SDAMAX,
     +    SDDVRT,SDSVRT,SDTMGM,SDWMAX,SFREEZ,SLA1,SLA2,SLA3,SLA4,STEND,
     +    STNAVL,STNGER,STNLW,TBS,TOP,TOT4WT,WCG,WCP,WDLDG,EFFN,CNST,
     +    PRB,GSGDD,PMAXN,PNRED,CLBASE(MXSPEC)
      INTEGER IPLTYP,NPL,VRNLZ,GDDFLG
      COMMON /PLNTIN/ AHLDG(MXSPEC),ALA(MXSPEC),ALNAVL(MXSPEC),
     +    ALNGER(MXSPEC),ALNLW(MXSPEC),ALPHA(MXSPEC),ALX(MXSPEC),
     +    BETA(MXSPEC),BGSEED(MXSPEC),CAA(MXSPEC),CANK(MXSPEC),
     +    CAX(MXSPEC),CONVLA(MXSPEC),CVLBIO(MXSPEC),DEVRAT(2:7,MXSPEC),
     +    DROTOL(MXSPEC),EFFN(MXSPEC),DTSEED(MXSPEC),GITEMP(MXSPEC),
     +    GIWAT(MXSPEC),GMN(MXSPEC),GRMRAT(MXSPEC),GSR(MXSPEC),
     +    HFMAX(MXSPEC),PCLDG(MXSPEC),PGDVRT(MXSPEC),PGNTGT(MXSPEC),
     +    PLALFA(MXSPEC),PLBIO(MXSPEC),PLDIAM(MXSPEC),PLHGHT(MXSPEC),
     +    PLTMNT(MXSPEC),PLTMXT(MXSPEC),PLTOPT(MXSPEC),PMAX(MXSPEC),
     +    PMAXN(MXSPEC),PNRED(MXSPEC),PMNNIT(MXSPEC),PRNLW(MXSPEC),
     +    PTLAMX(MXSPEC),R20(MXSPEC),RATLS(MXSPEC),RATRSX(MXSPEC),
     +    RATRSN(MXSPEC),RDR(MXSPEC),RDX(MXSPEC),RQ10(MXSPEC),
     +    RTNLW(MXSPEC),SDAMAX(MXSPEC),SDDVRT(MXSPEC),SDSVRT(MXSPEC),
     +    SDTMGM(MXSPEC),SDWMAX(MXSPEC),SFREEZ(MXSPEC),SLA1(MXSPEC),
     +    SLA2(MXSPEC),SLA3(MXSPEC),SLA4(MXSPEC),STEND(2:6,MXSPEC),
     +    STNAVL(MXSPEC),STNGER(MXSPEC),STNLW(MXSPEC),TBS(MXSPEC),
     +    TOP(MXSPEC),TOT4WT(MXSPEC),WCG(MXSPEC),WCP(MXSPEC),
     +    WDLDG(MXSPEC),CNST(MXSPEC),PRB(7,7,MXSPEC),GSGDD(2:6,MXSPEC),
     +    GDDFLG(MXSPEC),VRNLZ(MXSPEC),SUFNDX(MXSPEC),IPLTYP(MXSPEC),
     +    LWM(MXSPEC),LWS(MXSPEC),NPL,INFIXN(MXSPEC)
C
      COMMON /IPOTEV/ A0,AW,AC,ARI,XW,FSUN,COEPAN,rss,RST(MXSPEC)
     +    ,RXANGLE(MXSPEC),RTCCRIT(MXSPEC),RPLEAF0(MXSPEC),
     +      RRSTEXP(MXSPEC),RRLEAF0(MXSPEC)
      common /shawp/ pplastic,emitc,emitr(10),emitsp,emits,emitf
     &      ,palbedo,ptransm
C
      COMMON /RESID/ RM,RESAGE,CRES,HR,RCN,NADLY,NSDLY,NAREC,NSREC,WORM,
     +               SAI,HSTUBL,WSTUBL
C
      COMMON /NUTPAR/ R14,R23,R34,R43,R45,R53,R15,R25,EFFMAX,EFFNIT,
     +    EFFDEN,DENC,RPOP(3),ADCAY,ANIT,ADENIT,AUREA,ADEATH(3),CN(9),
     +    ADCAYV(5),XKP(3),AMETH,FRN2O_NIT,FRN2O_DEN,RNO_N2O,RAINNO,
     +    O2LIM,EFFMETH,ISBUG
C
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
      COMMON /IPCHEM/ A,ALH(MXNOD,MXCHEM),B,BDH(MAXHOR),DEG(MAXHOR),
     +    BDHR(MAXHOR),MICP(MAXHOR),NMICP
C
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
C
      COMMON /IPINF/ CRUSTK,OCRUST,ICRUST,INFLPD,NHZ,NSLT,POND,RR,
     +    DHB(MAXHOR),NSL(MAXHOR),WI(MXnod),DSL(MXNODT),VWC(MXNODT),
     +    IUGFLW
      DOUBLE PRECISION MICP,MCPOR
      INTEGER YESMAC
C
      COMMON /INFILC/ RAC(11),RRC(11),FERTIR(4),PESTIR(MXPEST),FMNRIR
C
      COMMON /HEAT/ CSH(MXNOD),T(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3)
C
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
      COMMON /RZPEST/ HALFF(MXPEST),HALFFP(MXPEST),HALFFB(MXPEST),
     +    HALFR(MXPEST),HALFRP(MXPEST),HALFRB(MXPEST),HALFSSA(MXPEST),
     +    HALFSSV(MXPEST),HALFSSP(MXPEST),HALFSSB(MXPEST),
     +    HALFSA(MXPEST),HALFSN(MXPEST),HALFSB(MXPEST),HALFBD(MXPEST),
     +    XKH(MXPEST),XKOC(MXPEST),XMW(MXPEST),THETAR(MXPEST),
     +    TMPR(MXPEST),EA(MXPEST),EN(MXPEST),WALKERB(MXPEST),
     +    VMAX(MXPEST),DYIELD(4,MXPEST),FREUND(MXPEST),XPKA(MXPEST),
     +    XPKB(MXPEST),REFPH(MXPEST),CATKOC(MXPEST),ANKOC(MXPEST),
     +    PCOEFF(MXPEST),PPOWER(MXPEST),RCOEFF(MXPEST),RPOWER(MXPEST),
     +    XKOW(MXPEST),PBIND(MXPEST),IPCODE(4,MXPEST),IPANC(MXPEST),
     +    IPDEP(MXPEST),IPACTV(MXPEST),NPEST
C
      COMMON /KNTCS/ CONCX2(MXNOD,MXPEST),EK2(MXNOD,MXPEST),
     +    RK2(MXPEST),RDIF(MXPEST),FEK2(MXPEST),IEK2(MXPEST)
C
      COMMON /MMNUR/ AMNH4(MXAPP),AMOW(MXAPP),AMBED(MXAPP),
     +    CNBED(MXAPP),AMCN(15),CNMANR(MXAPP),FCMANR(MXAPP),
     +    IWMANR(MXAPP),IMOFF(MXAPP,2),IMMETH(MXAPP),IMTYPE(MXAPP),
     +    NAPPM,IMPL(MXAPP),IMANDECOMP(MXAPP)
C
      COMMON /MFERT/ FNH4IN(MXAPP),FNO3IN(MXAPP),FUREIN(MXAPP),
     +    SPLTST(MXAPP),SPLTAP(MXAPP),IFOFF(MXAPP,2),IFMETH(MXAPP),
     +    IWFERT(MXAPP),NAPPF,IBMPAP(MXAPP),IBMPMD(MXAPP),
     +    IDSPLT(MXAPP),IFPL(MXAPP)
C
      COMMON /BMPRC/ YGOAL,OMADJ,USAMDP,UMISCC,ISTATE,ISOYB,BMPIRR,
     +    BMPTIL,FMC,YGSF,SOYYLD
C
      COMMON /MPEST/ APP(MXAPP),PDR(MXAPP),IAPP(MXAPP),IPOFF(MXAPP,2),
     +    IWPEST(MXAPP),IPMETH(MXAPP),NAPP,IPPL(MXAPP)
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP)
C
      COMMON /THGIN/ THGDEP(MXTG),THGCUR,ITHG(MXTG),IWTHG(MXTG),
     +    ITHGOFF(MXTG,2),ITHGDUR(MXTG),NUMTHG,IDP
C
      COMMON /HRVST/ HSTAGE(MXAPP),HPERC(MXAPP),HRVEFF(MXAPP),YIELD(3),
     +    STUBHT(MXAPP),IHTYPE(MXAPP),IHDAY(MXAPP,3),IHARV(MXAPP),
     +    IHCL(MXAPP)
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
      COMMON /PLNTG/ PLDEN(MXAPP),PLTDAY(MXAPP,2),ROWSP(MXAPP),
     +    PLDEPTH(MXAPP),sdwtplr(mxapp),sdager(mxapp),sprlapr(mxapp),
     +    PLNTSW(MXAPP),PLNTWD(MXAPP),LAYNDX(MXAPP),NPGS,NPR(MXAPP),
     +    NGPL(MXAPP),iptype(mxapp),iemrg(mxapp)
C
      CHARACTER SOLTYP*10,NMPEST*30,PLNAME*30,string*255
      COMMON /NAMES/ NMPEST(MXPEST),SOLTYP(MAXHOR),PLNAME(MXSPEC)
C
      LOGICAL NCLY
      DIMENSION XMISC(3,1),INXPL(MXSPEC),EFFLUX(MXSPEC),IRTYPE(MXSPEC),
     +    NUTEQ(13),wsi(mxspec),aim(12)
      DIMENSION IRRTYP(MXAPP)
      CHARACTER(len=1) UPCASE
      DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE :: Bpemr,Bpmat,Bp50mat,
     + BT,Pdp,Humat
      
      INTEGER,DIMENSION (:),ALLOCATABLE:: Countcrop
      INTEGER,DIMENSION (:,:),ALLOCATABLE:: Cropseq
      INTEGER :: Fxdate,pstr(mxspec)
C
      DATA NCLY /.TRUE./  !,DOIMONTH/12*0.0D0/
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C --      R E A D   C O N T R O L D A T A        --
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C     ..READ SCENARIO NUMBER FOR OUTPUT SECTION
      CALL ECHO(INP1)
      READ(INP1,*) NSC
      REWIND(UNIT=INP1)
C
C     ..INITIALIZE OUTPUT SECTION FOR INITIAL CONDITIONS
      CALL OUTIN1(NSC,IBRKTH,ICHEM,IMAGIC,NOSTAR,NUTEQ,INP1,IPHOS)
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      P H Y S I O G R A P H I C   P R O P E R T I E S      --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
	 if (k.eq.6) then
         READ(INP2,*) AREA,ELEV,ASPECT,XLAT,SLOPE,XLONG
         Iwzone=2
         CO2A=330
       else if (k.eq.7) then
         READ(INP2,*) AREA,ELEV,ASPECT,XLAT,SLOPE,XLONG,Iwzone
         CO2A=330
       else if (k.eq.8) then
         READ(INP2,*) AREA,ELEV,ASPECT,XLAT,SLOPE,XLONG,Iwzone,CO2A
           endif
        if (iwzone.eq.0) iwzone=2   !=1 arid <10 in rainfall, =2 semi-arid, 10-20 in rainfall, =3 humid, >20 in rainfall
        Areap = Area   ! debasis
        if (area.eq.0.0d0) Areap=1.0d0
c      IF (XLAT.EQ.0.0D0) XLAT=XLAT1
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      S O I L   P R O P E R T I E S         --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
C     ... SOIL SYSTEM PHYSICAL CONFIGURATION
      CALL ECHO(INP2)
      READ(INP2,*) NHOR,PRODEP
      READ(INP2,*) (HORTHK(I),I=1,NHOR)
      Nsoil = NHOR                       ! DEBASIS
      ALLOCATE(Dsoil(Nsoil),mmm(Nsoil),Tsoil(Nsoil),Bdsoil(Nsoil))
      ALLOCATE(Pdsoil(Nsoil),Ksat(Nsoil),Smc(Nsoil),
     +  Fc(Nsoil),Pwp(Nsoil),Psc(Nsoil),Clayp(Nsoil),
     +  Sandp(Nsoil),Siltp(Nsoil),Orgm(Nsoil),Labp(Nsoil),Actp(Nsoil),
     +  Stabip(Nsoil),Stabop(Nsoil),Frsop(Nsoil),Crpres(Nsoil),
     +  Orgc(Nsoil),
     +  Potpuptake(Nsoil),Actpuptake(Nsoil),Daydesrp(NSoil),
     +  Daysrp(Nsoil),Pflowlabact(Nsoil),Pflowactlab(Nsoil),
     +  Pflowactstbi(Nsoil),Pflowstbiact(Nsoil),Inicrpres(Nsoil),
     +  Pflowresfrso (Nsoil),Pflowfrsolab(Nsoil),Pflowfrsostbo(Nsoil),
     +  Pflowlabfrso(Nsoil),Pflowstbolab(Nsoil),
     +  ResdueP1(Nsoil),ResdueP2(Nsoil),HumusP1(Nsoil),HumusP2(Nsoil),
     +  HumusP3(Nsoil),Pflowr1h2(Nsoil),Pflowr1h1(Nsoil),
     +  Pflowr2h1(Nsoil),Pflowh1h2(Nsoil),Pflowh2h1(Nsoil),
     +  Pflowh2h3(Nsoil),Pflowh3h1(Nsoil),Pflowr1frsop(Nsoil),
     +  Pflowr2frsop(Nsoil),Pflowh1stbop(Nsoil),Pflowh2stbop(Nsoil),
     +  Pflowh3stbop(Nsoil),CP(Nsoil,5),ResdueC1(Nsoil),ResdueC2(Nsoil),
     +  HumusC1(Nsoil),HumusC2(Nsoil),HumusC3(Nsoil),SOroot(Nsoil),
     +  CProot(Nsoil),Blgrbiomly(Nsoil),BlgrPly(Nsoil),
     +  RatioSIPtoAIP(Nsoil))       !Zhiming Stable inorg P to active inorg P
      
      ALLOCATE(Soilno3(Nsoil),Soiltemp(Nsoil),
     +  Soilwater(Nsoil),Soilnfresh(Nsoil))
     
      DO i =1, Nsoil
       Dsoil(i) = HORTHK(i)*0.01D0
      END DO
       Dimvly = Dsoil(Nsoil)
      CALL NODETHICK(Tsoil,Dsoil,Nsoil)                   !DEBASIS
      
      
C
C     ... DISCRETIZATION FOR MOISTURE AND HEAT MODELS
      CALL ECHO(INP2)
      READ(INP2,*) NN
      Nnode = NN                                       !debasis
      ALLOCATE(Dnode(Nnode),Soilwaternode(Nnode),
     +   m12flow(Nnode),Matflow(Nnode),Tnode(Nnode),
     + Labpnode(Nnode),Claynode(Nnode),BDsoilnode(Nnode),
     + Pleachmat(Nnode),Massly(Nnode),Kd(Nnode),Clabpsw(Nnode),
     + Actpnode(Nnode),Stabipnode(Nnode),Stabopnode(Nnode),
     + Frsopnode(Nnode),Pdrn(Nnode,5),ResdueP1node(Nnode),
     + ResdueP2node(Nnode),HumusP1node(Nnode),
     + HumusP2node(Nnode),HumusP3node(Nnode),Pflowr1frsopnode(Nnode),
     + Pflowr2frsopnode(Nnode),
     + Pflowh1stbopnode(Nnode),Pflowh2stbopnode(Nnode),
     + Pflowh3stbopnode(Nnode))
      
      DO 10 I=1,NN
        READ(INP2,*) IDUM,XDUM,DELZ(I)
        Dnode(I) = XDUM*0.01D0  
                                                           !debasis
   10 CONTINUE
       CALL NODETHICK(Tnode,Dnode,Nnode)              !debasis
C
C     ... SOIL HORIZON PHYSICAL PROPERTIES
      CALL ECHO(INP2)
      DO 20 J=1,NHOR
        READ(INP2,'(A10)') SOLTYP(J)
        READ(INP2,*) (SOILPP(I,J),I=1,7)
        BDHR(J)=SOILPP(3,J)
        Pdsoil(j) = SOILPP(2,J)*1000.0D0
        Bdsoil(J) = SOILPP(3,J)*1000.0D0                   !debasis
        Clayp(J) = SOILPP(7,J)*100.0D0
   20 CONTINUE
C
C     ... SOIL HORIZON HYDRAULIC PROPERTIES
      CALL ECHO(INP2)
      DO 30 J=1,NHOR
        READ(INP2,*) IDUM,(SOILHP(I,J),I=1,6)
        READ(INP2,*) (SOILHP(I,J),I=7,13)
        READ(INP2,*) CLAT(J)
c        IF (SOILPP(1,J).NE.0) CLAT(J)=SOILHP(4,J)   !IF MINIMUM DATA ARE USED, SET LATERAL KSAT TO HORIZONAL KSAT, there is no need
        IF (CLAT(J).LE.0) CLAT(J)=SOILHP(4,J)   !SET LATERAL KSAT TO HORIZONAL KSAT
        IF (SOILHP(6,J).LT.SOILHP(7,J).and.SOILHP(6,J).GT.0.0d0) THEN
        PRINT*,'SATURATED WATER CONTENT < 1/3 BAR SWC AT LAYER',J
        STOP
        ENDIF
        IF (SOILHP(6,J).LT.SOILHP(8,J).and.SOILHP(6,J).GT.0.0d0) THEN
        PRINT*,'SATURATED WATER CONTENT < 1/10 BAR SWC AT LAYER',J
        STOP
        ENDIF
   30 CONTINUE
      
      Do i=1, Nsoil
          Fc(i) = SOILHP(7,i)*100.0D0                     !debasis
      END DO
C
C     ... SOIL HORIZON HEAT MODEL PARAMETERS
      CALL ECHO(INP2)
      DO 40 I=1,NHOR
C
C       ...PLACE REQUIRED HYDRAULIC PARAMETERS
        SOLTP1(I,1)=SOILHP(6,I)
        SOLTP1(I,2)=SOILHP(7,I)
C
        READ(INP2,*) ITEXT,SOLTP1(I,5)
c        SOLTP1(I,3)=DBLE(ITEXT)
c        SOLTP1(I,5)=0.002D0*(1.0D0-SOILHP(6,I))   !MODIFIED BY LIWANG MA, 10-10-2009
        SOLTP1(I,5)=0.002D0*SOILPP(3,I)/soilpp(2,i)   !MODIFIED BY LIWANG MA, 5-3-2010
        SOLTP1(I,3)=SOILHP(9,I)   !use 15 bar soil moisture instead of texture code.
        SOLTP1(I,4)=2.0D0  !only sandy, silt+clay, not organic matter
C
C       ..SETUP VOLUME FRACTIONS
        SOLTP2(1,I,1)=1.0D0-SOILPP(5,I)    !-0.03D0   !silt+clay
        SOLTP2(2,I,1)=SOILPP(5,I)   !sand fraction
        SOLTP2(3,I,1)=0.03D0        !assume 3% OM by volume
C
C       ..SETUP HEAT CONDUCTIVITIES FOR ABOVE [J/MM/C/HR]
        SOLTP2(1,I,2)=10.55D0   !silt+clay
        SOLTP2(2,I,2)=30.74D0   !sand
        SOLTP2(3,I,2)=0.9D0
C
C       ..SETUP PARTICLE ELLIPSOID SHAPE FACTORS (GA)
        SOLTP2(1,I,3)=0.125D0
        SOLTP2(2,I,3)=0.125D0
        SOLTP2(3,I,3)=0.5D0
   40 CONTINUE
C
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      M A C R O P O R E AND  I N F I L T R A T I O N      --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
	 if (k.eq.16) then
          READ(INP2,*,ERR=260) ICRUST,CRUSTK,IREBOT,AEF,RICH_EPS,
     +    MAXITER_RICH,MAXCYCLE_RICH,TWL,ITBL,BOTFLX,IDRAIN,DRDEP,
     +    DRSPAC,DRRAD,IUGFLW,hydgrad
          Hmin=-15000.D0
          HFC=-333.D0
          HWP=-15000.D0
       else if (k.eq.17) then
          READ(INP2,*,ERR=260) ICRUST,CRUSTK,IREBOT,AEF,RICH_EPS,
     +    MAXITER_RICH,MAXCYCLE_RICH,TWL,ITBL,BOTFLX,IDRAIN,DRDEP,
     +    DRSPAC,DRRAD,IUGFLW,hydgrad,Hmin
          HFC=-333.D0
          HWP=-15000.D0
       else if (k.eq.19) then
          READ(INP2,*,ERR=260) ICRUST,CRUSTK,IREBOT,AEF,RICH_EPS,
     +    MAXITER_RICH,MAXCYCLE_RICH,TWL,ITBL,BOTFLX,IDRAIN,DRDEP,
     +    DRSPAC,DRRAD,IUGFLW,hydgrad,Hmin,HFC,HWP
       endif
      OCRUST=CRUSTK
      if (MAXITER_RICH.lt.3000) MAXITER_RICH=3000
      if (MAXITER_RICH.gt.10000) MAXITER_RICH=10000
C
C     ..READ INPUT FOR MICROPORES IN SOIL AND RUNOFF
      CALL ECHO(INP2)
      READ(INP2,*) NMICP,A,B
      READ(INP2,*) (MICP(J),J=1,NHOR)
C
C     .. READIN INFORMATION FOR MACROPORES, IF NEEDED
      CALL ECHO(INP2)
      READ(INP2,*) YESMAC,SFCT,XPRESF
      READ(INP2,*) EFFWR,EFFWC
c      IF (YESMAC.GE.1) IMAGIC=-10   ! TO MAKE SURE MACRO.OUT CAN BE PRINTED
      NHCMP=0
      DO 50 J=1,NHOR
        READ(INP2,*) MCPOR(J),RP(J),WP(J),PL(J),AGSZ(J),FDEP(J)
        IF(RP(J).NE.0.0D0.AND.NCLY) THEN
          NHCMP=NHCMP+1
        ELSE
          NCLY=.FALSE.
        ENDIF
   50 CONTINUE
      
      Dtile = DRDEP*0.01D0          ! DEBASis
C
      IF ((YESMAC.GE.1).AND.(RP(1).EQ.0.0D0)) THEN
          RP(1)=0.1D0                    !SET TO 0.1 CM FOR SURFACE LAYER CYLINDER RADIUS
          WRITE (*,*) 'SET TO 0.1 CM FOR SURFACE LAYER CYLINDER RADIUS'
      ENDIF
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      P O T E N T I A L E V A P O R A T I O N       --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
c      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX 
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
       if(k.eq.8) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY
         ishaw=0
         ipenflux=0
         rss=37.0d0
         istress=0
        pplastic=0.0d0
        emitc=1.0d0
        emitr0=1.0d0
        emitsp=1.0d0
        emits=1.0d0
        emitf=1.0d0
        ipet=0
        CKinit=0.12d0
        CKmax=1.0d0
       palbedo=0.0d0
       ptransm=1.0d0
	 else if (k.eq.10) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX
         rss=37.0d0
         istress=0 
        pplastic=0.0d0
        emitc=1.0d0
        emitr0=1.0d0
        emitsp=1.0d0
        emits=1.0d0
        emitf=1.0d0
        ipet=0
        CKinit=0.12d0
        CKmax=1.0d0
       palbedo=0.0d0
       ptransm=1.0d0
       else if (k.eq.11) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX
     +             ,RSS
       istress=0
        pplastic=0.0d0
        emitc=1.0d0
        emitr0=1.0d0
        emitsp=1.0d0
        emits=1.0d0
        emitf=1.0d0
        ipet=0
        CKinit=0.12d0
        CKmax=1.0d0
       palbedo=0.0d0
       ptransm=1.0d0
       else if (k.eq.12) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX
     +             ,RSS,istress
        pplastic=0.0d0
        emitc=1.0d0
        emitr0=1.0d0
        emitsp=1.0d0
        emits=1.0d0
        emitf=1.0d0
        ipet=0
        CKinit=0.12d0
        CKmax=1.0d0
       palbedo=0.0d0
       ptransm=1.0d0
       else if (k.eq.18) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX
     +             ,RSS,istress,pplastic,emitc,emitr0,emitsp,emits,emitf
        ipet=0
        CKinit=0.12d0
        CKmax=1.0d0
       palbedo=0.0d0
       ptransm=1.0d0
       else if (k.eq.21) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX
     +             ,RSS,istress,pplastic,emitc,emitr0,emitsp,emits,emitf
     +             ,ipet,CKinit,CKmax
       palbedo=0.0d0
       ptransm=1.0d0
       else if (k.eq.23) then
      READ(INP2,*) A0,AW,AC,ARI,XW,FSUN,COEPAN,IHOURLY,ISHAW,IPENFLUX
     +             ,RSS,istress,pplastic,emitc,emitr0,emitsp,emits,emitf
     +             ,ipet,CKinit,CKmax,palbedo,ptransm
       endif
c      if ((ishaw.eq.1).and.(hmin.gt.-35000)) then
c          hmin=-35000
c      print*, 'reset Hmin to -35000'
c      endif
      if (ishaw.eq.0) pplastic=0.0d0
      if (pplastic.gt.0.0d0) then
          ishaw=1
          ihourly=1
c           istress=3
      endif
c
      if (istress.ge.3) then
          ishaw=1
          ihourly=1
      endif
C
 1110 FORMAT(A255)
      IF ((ISHAW.EQ.1) .OR. (IPENFLUX.EQ.1)) IHOURLY=1
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      S U R F A C E     R E S I D U E         --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
       if(k.eq.8) then
      READ(INP2,*) RM,RESAGE,CRES,HR,RCN,SAI,HSTUBL,WSTUBL
      CP0=500.D0
       else
      READ(INP2,*) RM,RESAGE,CRES,HR,RCN,SAI,HSTUBL,WSTUBL,CP0
       endif
C
C GNF IF (PPLASTIC.GT.0.0D0.AND.RM.EQ.0.0D0) RM=0.01D0   !MINIMUM RM FOR PLASTIC COVER TO WORK LIWANG 2017
C     ..DETERMINE RESIDUE COVER FOR FIRST DAY ONLY
      IF(CRES.LE.0.0D0) THEN
        CRES=1.320D0
      ENDIF
      FTR=EXP(-CRES*1.270D-2*RM/0.15D0)
C
      CALL ECHO(INP2)
      READ(INP2,*) NADLY,NSDLY,NAREC,NSREC,WORM
       IF ((WORM.GT.0.01D0).or.(worm.lt.0.0d0)) WORM=0.0004D0
C
C     ..CONVERT RESIDUE MASS UNITS MT/HA ---> KG/HA
      RM=RM*1.0D3
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak LIWANG MA, WSTUBL MT/HA ---> KG/HA
	wstubl = wstubl * 1.0D3
ceeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee

C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      S O I L   C H E M I S T R Y         --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) PBASE,EC
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --     R A I N   W A T E R  C H E M I S T R Y       --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) RAPH
      READ(INP2,*) (RAC(J),J=2,11)
      IF(RAPH.NE.0.0D0) RAC(1)=10**(-RAPH)
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --     I R R I G A T I O N  W A T E R  C H E M I S T R Y     --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) RRPH
      READ(INP2,*) (RRC(J),J=2,11)
      IF(RRPH.NE.0.0D0) RRC(1)=10**(-RRPH)
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --     P E S T I C I D E S           --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NPEST
      DO 60 IP=1,NPEST
        IPACTV(IP)=1
        READ(INP2,'(A30)') NMPEST(IP)
        read (INP2,1110,err=27) string
        knumb=inumb(string)
        backspace (inp2)
       if (knumb.eq.7) then
        READ(INP2,*) XMW(IP),XKH(IP),IPANC(IP),IPCODE1,
     +      DYIELD1,XKOW(IP),PBIND(IP)
        if ((ipcode1.gt.0).and.(dyield1.gt.0.0d0)) then
            if (ipcode1.ge.1.and.ipcode1.le.3) then
                ipcode(1,ip)=ipcode1
                dyield(1,ip)=dyield1
            endif
            if (ipcode1.ge.4.and.ipcode1.le.6) then
                ipcode(2,ip)=ipcode1
                dyield(2,ip)=dyield1
            endif
            if (ipcode1.ge.7.and.ipcode1.le.10) then
                ipcode(3,ip)=ipcode1
                dyield(3,ip)=dyield1
            endif
            if (ipcode1.ge.10.and.ipcode1.le.13) then
                ipcode(4,ip)=ipcode1
                dyield(4,ip)=dyield1
            endif
        endif
        else if (knumb.eq.13) then
        READ(INP2,*) XMW(IP),XKH(IP),IPANC(IP),
     +      (IPCODE(ij,IP),DYIELD(ij,IP),ij=1,4),
     +      XKOW(IP),PBIND(IP)
        endif
c
        READ(INP2,*) THETAR(IP),TMPR(IP),EA(IP),EN(IP),WALKERB(IP),
     +      IPDEP(IP),VMAX(IP)
        READ(INP2,*) PCOEFF(IP),PPOWER(IP),RCOEFF(IP),RPOWER(IP)
        READ(INP2,*) XPKA(IP),XPKB(IP),FREUND(IP),XKOC(IP),CATKOC(IP),
     +      ANKOC(IP),REFPH(IP),FEK2(IP),RK2(IP),RDIF(IP)
         if (RK2(IP).GE.1.0D0) then
        RK2(IP)=LOG(2.0D0)/RK2(IP)                     !change from day to 1/day for kinetic desorption 3-5-2009
         endif
         if(freund(ip).ne.1.0d0) freund(ip)=1.0d0
   60 CONTINUE
C
C     ..half lives
      IF(NPEST.GT.0) CALL ECHO(INP2)
      DO 70 IP=1,NPEST
        READ(INP2,*) HALFF(IP),HALFFP(IP),HALFFB(IP),HALFR(IP),
     +      HALFRP(IP),HALFRB(IP),HALFSSA(IP),HALFSSV(IP),HALFSSP(IP),
     +      HALFSSB(IP),HALFSA(IP),HALFSN(IP),HALFSB(IP),HALFBD(IP)
   70 CONTINUE
        DO ID=1,NPEST
            DO ip=1,npest
          IF(IPANC(ID).EQ.IP) THEN
            IF(IPCODE(1,ID).EQ.1.AND.HALFF(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 1 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(1,ID).EQ.2.AND.HALFFP(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 2 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(1,ID).EQ.3.AND.HALFFB(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 3 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(2,ID).EQ.4.AND.HALFR(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 4 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(2,ID).EQ.5.AND.HALFRP(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 5 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(2,ID).EQ.6.AND.HALFRB(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 6 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(3,ID).EQ.7.AND.HALFSSA(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 7 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(3,ID).EQ.8.AND.HALFSSV(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 8 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(3,ID).EQ.9.AND.HALFSSP(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 9 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(3,ID).EQ.10.AND.HALFSSB(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 10 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(4,ID).EQ.10.AND.HALFSSB(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 10 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(4,ID).EQ.11.AND.HALFSA(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 11 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(4,ID).EQ.12.AND.HALFSN(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 12 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            IF(IPCODE(4,ID).EQ.13.AND.HALFSB(IP).LE.0) THEN
                PRINT*,'NO DEGRATION PATHWAY 13 IS SET IN PARENT IP#',IP
                STOP
            ENDIF
            ENDIF
          ENDDO
          ENDDO
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --     P L A N T    G R O W T H          --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
C     --READ IN VALUES FOR PLANT MODEL CONTROL
C
      CALL ECHO(INP2)
      READ(INP2,*) NPL
      ALLOCATE(Bpemr(NPL),Bpmat(NPL),Bp50mat(NPL),
     + BT(NPL),Pdp(NPL),Humat(NPL),Countcrop(NPL))
      
      NGNPL=0
      CALL ECHO(INP2)
      DO 80 IPL=1,NPL
        NGPL(IPL)=0
        Countcrop(IPL) = 0
        READ(INP2,1000) INXPL(IPL),PLNAME(IPL)
        DO I = 1, LEN(PLNAME(IPL))
         PLNAME(IPL)(I:I) = UPCASE(PLNAME(IPL)(I:I))
        END DO
C       CHECK FOR GENERIC PLANTS - OTHERS WILL HAVE SEPARATE INPUT
        IF(INXPL(IPL).NE.9999) THEN
          NGNPL=NGNPL+1
          NGPL(IPL)=NGNPL
        ENDIF
   80 CONTINUE
C
C     --READ USER-SPECIFIC PLANT PARAMETERS; THESE ARE THE ONLY
C     PARAMETERS MOST USERS SHOULD MODIFY.
      CALL ECHO(INP2)
      DO 90 IPL=1,NPL
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
       if(k.eq.11) then
        READ(INP2,*) CNUP1(IPL),ALPHA(IPL),CONVLA(IPL),CLBASE(IPL),
     +      SLA3(IPL),SLA4(IPL),RDX(IPL),RST(IPL),SUFNDX(IPL),
     +      EFFLUX(IPL),wsi(ipl)
        
        RXANGLE(IPL)=0.0d0
        RTCCRIT(IPL)=7.0d0
        RPLEAF0(IPL)=-100.0d0
        RRSTEXP(IPL)=5.0d0
        RRLEAF0(IPL)=1.0d+5
        Bpemr(IPL) = 0.024d0
        Bpmat(IPL) = 0.0008d0
        Bp50mat(IPL) = 0.016d0
C        BT(IPL) = 5.0d0
        Pdp(IPL) = 10.0d0
C        Humat(IPL) = 1800.d0
        Pstr(IPL) = 0
       ELSE IF (K.EQ.16) THEN
        READ(INP2,*) CNUP1(IPL),ALPHA(IPL),CONVLA(IPL),CLBASE(IPL),
     +      SLA3(IPL),SLA4(IPL),RDX(IPL),RST(IPL),SUFNDX(IPL),
     +      EFFLUX(IPL),wsi(ipl),RXANGLE(IPL),RTCCRIT(IPL),RPLEAF0(IPL),
     +    RRSTEXP(IPL),RRLEAF0(IPL)
        Bpemr(IPL) = 0.024d0
        Bpmat(IPL) = 0.0008d0
        Bp50mat(IPL) = 0.016d0
C        BT(IPL) = 5.0d0
        Pdp(IPL) = 10.0d0
C        Humat(IPL) = 1800.d0
        Pstr(ipl) = 0
       ELSE IF (K.EQ.21) THEN
        READ(INP2,*) CNUP1(IPL),ALPHA(IPL),CONVLA(IPL),CLBASE(IPL),
     +      SLA3(IPL),SLA4(IPL),RDX(IPL),RST(IPL),SUFNDX(IPL),
     +      EFFLUX(IPL),wsi(ipl),RXANGLE(IPL),RTCCRIT(IPL),RPLEAF0(IPL),
     +    RRSTEXP(IPL),RRLEAF0(IPL),Bpemr(IPL),Bpmat(IPL),Bp50mat(IPL),
     +    Pdp(IPL),Pstr(ipl)      
C     + BT(IPL),Pdp(IPL),Humat(IPL),Pst      
       ENDIF           
      if (wsi(ipl).lt.0.0d0) then
        if (  INDEX(PLNAME(IPL),'CORN').NE.0.OR.
     +        INDEX(PLNAME(IPL),'corn').NE.0.or.
     +        INDEX(PLNAME(IPL),'MAIZE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'maize').NE.0.or. 
     +        INDEX(PLNAME(IPL),'Corn').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'Maize').NE.0)  !.or.(INXPL(IPL).le. 100))
     +       wsi(ipl)=0.75d0
        if (  INDEX(PLNAME(IPL),'Soybean').NE.0.or.
     +        INDEX(PLNAME(IPL),'SOYBEAN').NE.0.OR.
     +        INDEX(PLNAME(IPL),'soybean').NE.0)
c     +        .or.((INXPL(IPL).gt. 100).and.(inxpl(ipl).lt.200)))
     +       wsi(ipl)=0.5d0
      endif
        CNUP1(IPL)=CNUP1(IPL)*1.0D3
        CONVLA(IPL)=1.0D-3*CONVLA(IPL)*CLBASE(IPL)
        if (RST(IPL).eq.0.0d0) RST(IPL)=100.0d0  !in case users do not supply
        IF (RTCCRIT(IPL).EQ.0.0D0) RTCCRIT(IPL)=7.0d0
        IF (RPLEAF0(IPL).EQ.0.0D0) RPLEAF0(IPL)=-100.0d0
        IF (RRSTEXP(IPL).EQ.0.0D0) RRSTEXP(IPL)=5.0d0
        IF (RRLEAF0(IPL).EQ.0.0D0) RRLEAF0(IPL)=1.0d+5
        IF (Bpemr(IPL).EQ.0.0D0)   Bpemr(IPL)= 0.024d0
        IF (Bpmat(IPL).EQ.0.0D0)   Bpmat(IPL)= 0.0008d0
        IF (Bp50mat(IPL).EQ.0.0D0) Bp50mat(IPL)= 0.016d0
        IF (Pdp(IPL).EQ.0.0D0)     Pdp(IPL)= 10.0d0
        
      
C      IF(Pstr(ipl) == 0) THEN
C          Psts = .False.
C      ELSE
C          Psts = .True.
C      ENDIF
      
   90 CONTINUE
   
C     ====================================================
C     === READ IN INFORMATION FOR GENERIC PLANTS ONLY ====
C     ====================================================
      NGNPL=MAX(NGNPL,1)
      CALL PLREAD(NGNPL,INXPL,INP6,CLBASE)
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --     N U T R I E N T S
C     ----------------------------------------------------------------------
C     Liwang has used the place R53 for R15 because R53 is always zero. 
C     R14  -----> from slow residue pool to intermediate soil humus pool
C     R15  -----> from slow residue pool to slow soil humus pool
C     R23  -----> from fast residue pool to fast soil humus pool pool
C     R34  -----> from fast humus pool to intermediate soil humus pool
C     R45  -----> from intermediate soil humus pool to slow soil humus pool
C     R53  -----> from slow soil humus pool to fast soil humus pool pool (not used)
C     R43  -----> from intermediate soil humus pool to fast soil humus pool pool (not used)
C     R25  -----> from fast residue pool to slow soil humus pool pool 
C     ----------------------------------------------------------------------
      CALL ECHO(INP2)
      READ(INP2,*) R14,R23,R34,R25,R45,R15
      IF ((R14+R15).GT.1.0D0) THEN
      STOP 'R14+R15>1, CHECK NUTRIENT INTERPOOL TRANSFER COEFFICIENTS'
      ENDIF
      IF ((R23+R25).GT.1.0D0) THEN
      STOP 'R23+R25>1, CHECK NUTRIENT INTERPOOL TRANSFER COEFFICIENTS'
      ENDIF
      Coefi14 = R14                         !DEBASIS
      Coefi23 = R23
      Coefi34 = R34
      Coefi25 = R25
      Coefi45 = R45
      Coefi15 = R15

C
      CALL ECHO(INP2)
      READ(INP2,*) O2LIM,EFFMAX,EFFNIT,EFFDEN,DENC,(RPOP(I),I=1,3)
        if (effden.eq.0.1d0) effden=effmax/2.0d0   !changed by Liwang Ma to avoid C balance. Make it half of effmax
        EFFMETH=EFFDEN                 !/2.0D0           !ASSUME METH EMISSION IS NOT AS EFFICIENT AS DENITRIFICATION
        IF (O2LIM.LT.0.6D0) O2LIM=0.6D0
        IF (O2LIM.GT.1.D0) O2LIM=1.0D0
        O2LIM=0.000304D0*DEXP(0.0815D0*O2LIM*1.0D2)
        IF (O2LIM.GT.1.D0) O2LIM=1.0D0
C
      ISBUG=NOSTAR
	IF (ISBUG.EQ.0) THEN
        EFFMAX=0.0D0
        EFFNIT=0.0D0
        EFFDEN=0.0D0
        EFFMETH=0.0D0
      ELSE IF (EFFMAX.NE.0.267D0) THEN
        EFFMAX=0.267D0
      ENDIF
c
      CALL ECHO(INP2)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
       if(k.eq.4) then
      READ(INP2,*) ADCAY,ANIT,ADENIT,AUREA  !,AMETH,DFRN2O,NFRN2O
C       AMETH=2.0D-14
       AMETH=1.0D-15
       FRN2O_NIT=1.6d-3
c       FRN2O_DEN=0.02    !calculated based on Century model
       ELSE
      READ(INP2,*) ADCAY,ANIT,ADENIT,AUREA,AMETH,FRN2O_NIT !,FRN2O_DEN
       ENDIF
C
      CALL ECHO(INP2)
      READ(INP2,*) (ADCAYV(I),I=1,5)
C
      CALL ECHO(INP2)
      READ(INP2,*) (ADEATH(I),I=1,3)
      IF (ADEATH(1).LT.1.D-30.and.ADEATH(1).GT.0.D0) ADEATH(1)=3.44D-11
      IF (ADEATH(2).LT.1.D-30.and.ADEATH(2).GT.0.D0) ADEATH(2)=3.628D-20
      IF (ADEATH(3).LT.1.D-30.and.ADEATH(3).GT.0.D0) ADEATH(3)=4.97D-13
C
      CALL ECHO(INP2)
      READ(INP2,*) (XKP(I),I=1,3)
C
      CALL ECHO(INP2)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
      if (k.eq.9) then      
      READ(INP2,*) (CN(I),I=1,9)
      CPini(1)=500.0d0
      CPini(2)=400.0d0
      CPini(3)=500.0d0
      CPini(4)=500.0d0
      CPini(5)=500.0d0
      Kr=0.01d0
      Kdh=1.20d0
      Kf=0.003d0
      Pextr=0.35d0
      Inidrpmgw=0.22d0
      Inippmgw=0.30d0
      else
      READ(INP2,*) (CN(I),I=1,9), (CPini(i),i=1,5),
     &      Kr,Kdh,Kf,Pextr,Inidrpmgw,Inippmgw
      endif
      DO i = 1,Nsoil
        DO j = 1,5
            CP(i,j) = CPini(j)
        END DO
       END DO
      Inidrpmacgw = Inidrpmgw * 1.0
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --       P L A N T   M A N A G E M E N T        --
C     ----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NPGS
      IF(NPGS.GT.MXAPP) THEN
        PRINT*,' TOO MANY PLANTINGS SPECIFIED'
        STOP
      ENDIF
      Ncrop = NPGS         !DEBASIS
      
       ALLOCATE(Cropname(Ncrop),Cpday(Ncrop),Cpmon(Ncrop),
     + Cpyear(Ncrop),Chday(Ncrop),Chmon(Ncrop),Chyear(Ncrop), 
     + Biopfracemg(Ncrop),Biopfracmat(Ncrop),Basetemp(Ncrop),
     + Pupdist(Ncrop),Phumat(Ncrop),Hu(Ncrop),Biopfrac(Ncrop),
     + Biopfrac50mat(Ncrop),Biopopt(Ncrop),Biopact(Ncrop),
     + Plantpdemand(Ncrop),Crpresleft(Ncrop),Avgrres(Ncrop),
     + Plntr(Ncrop),PlntPstress(Ncrop),Pstress(0:Ncrop))
      
      
      DO 100 J=1,NPGS
C        READ(INP2,*) NPR(J),JD,JM,IYYY,ROWSP(J),LAYNDX(J),PLDEN(J)
        READ(INP2,*) NPR(J),JD,JM,IYYY,ROWSP(J),PLDEPTH(J),PLDEN(J)
     &               ,iptype(j),sdwtplr(j),iemrg(j),sprlapr(j),sdager(j)
        if (iemrg(j).eq.0) iemrg(j)=-99
        if (sprlapr(j).eq.0.0d0) sprlapr(j)=-99.0d0
        if (sdager(j).eq.0.0d0) sdager(j)=-99.0d0
        IF(IYYY.LT.1900) THEN
          PRINT*,'2-DIGIT YEAR IN PLANT MANAGEMENT, CONVERTING 4-DIGIT'
          IYYY=IYYY+1900
        ENDIF
        PLTDAY(J,1)=DBLE(JDATE(JD,JM,IYYY))
        PLTDAY(J,2)=DBLE(IYYY)
        PLDEN(J)=PLDEN(J)*1.0D-3
        Cpday(j) = JD             ! DEBASIS
        Cpmon(j) = JM
        Cpyear(j) = IYYY
        Biopfracemg(j)=Bpemr(NPR(J))
        Biopfracmat(j)= Bpmat(NPR(J))
        Biopfrac50mat(j)= Bp50mat(NPR(J))
C        Basetemp(j)=BT(NPR(J))
        Pupdist(j) =Pdp(NPR(J))
C        Phumat(j) = Humat(NPR(J))
!        PlntPstress(j) = Pstrs(NPR(J))
        Plntr(j)= ROWSP(J)*0.01D0
        
        READ(INP2,*) IHTYPE(J),HSTAGE(J),IHCL(J),HPERC(J),(IHDAY(J,I),I=
     +      1,3)
c     +      1,5)
        Chday(j) = IHDAY(J,1)        !DEBASIS
        Chmon(j) = IHDAY(J,2)
        Chyear(j) = IHDAY(J,3)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
       if(k.eq.3) then
        READ(INP2,*) STUBHT(J),HRVEFF(J),IHARV(J)
        PLNTSW(J)=0.0d0
        PLNTWD(J)=0.0d0
       ELSE IF (K.EQ.5) THEN
        READ(INP2,*) STUBHT(J),HRVEFF(J),IHARV(J),PLNTSW(J),PLNTWD(J)
       ENDIF
C
  100 CONTINUE
      
      DO j=1,Ncrop
          DO i=1,NPL
              IF(NPR(J) == i) THEN
              Countcrop(i) = Countcrop(i)+1
             END IF
          END DO
      END DO
        
      ALLOCATE(Cropseq(NPL,Ncrop))
        DO j =1,Ncrop
            DO i = 1,NPL
                Cropseq(i,j) = 0
              IF(NPR(J) == i) THEN  
                Cropseq(i,j) = j
              END IF
            END DO
        END DO
          
C     ..SET THE FIRST RESIDUE TYPE TO FIRST PLANTING FOR DEFAULT
c      IF(NPGS.GT.0)IPR=NPR(1)
c      IF(NPGS.GT.0) then
        if (cres.eq.2.0d0) IPR=1
        if (cres.eq.2.5d0) IPR=2
        if (cres.eq.4.0d0) IPR=3
c      endif
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      M A N U R E M A N A G E M E N T        --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NAPPM
      IF(NAPPM.GT.MXAPP) THEN
        PRINT*,' TOO MANY MANURE APPLICATIONS SPECIFIED'
        STOP
      ENDIF
      
      Nman = NAPPM
      ALLOCATE(Manmass(MXAPP),Manpmass(MXAPP),Mantpper(MXAPP),
     +  Manweipper(MXAPP),Manweopper(MXAPP),Manwcper(MXAPP),
     +  Mand(MXAPP), Mlefts(MXAPP),Mday1(MXAPP),Mmon(MXAPP),
     +  Manwip(MXAPP),Manwop(MXAPP),Mansop(MXAPP),Mansip(MXAPP),
     +  Mancova(MXAPP),Manmoist(MXAPP),Appdaymanmass(MXAPP),
     +  Mancovadcom(MXAPP),Mansipdcom(MXAPP),Mansopdcom(MXAPP),
     +  Manwopdcom(MXAPP),Manasim(MXAPP),Mancovaasim(MXAPP),
     +  Manwipasim(MXAPP),Manwopasim(MXAPP),Mansipasim(MXAPP),
     +  Mansopasim (MXAPP),Maniprelease(MXAPP),Manoprelease(MXAPP),
     +  Manipcrunoff(MXAPP),Manopcrunoff(MXAPP),Manipmrunoff(MXAPP),
     +  Manopmrunoff(MXAPP),Mandcomr(MXAPP),Manasimr(MXAPP),
     +  Manextrc(MXAPP),PPmanwip(MXAPP),PPmanwop(MXAPP),PPmansop(MXAPP),
     +  PPmansip(MXAPP),PPcmanwop(MXAPP),PPcmanwip(MXAPP),
     +  PPcmansop(MXAPP),PPmmanwip(MXAPP),PPmmanwop(MXAPP),
     +  PPmmansop(MXAPP),Manfc(MXAPP),Myear1(MXAPP),Mtype(MXAPP),
     +  Mandcom(MXAPP),PPcmansip(MXAPP),PPmmansip(MXAPP),
     +  Mantpper1(MXAPP),Manweipper1(MXAPP),Manweopper1(MXAPP),
     +  Mlefts1(MXAPP),AppdayManmass1(MXAPP),Manwcper1(MXAPP),
     +  Manfc1(MXAPP),Mand1(MXAPP),Mtype1(MXAPP),ManSO_WI(MXAPP),
     +  ManSI_WI(MXAPP),ManWO_WI(MXAPP),ManSO_WO(MXAPP),
     +  ManWI_LabP1(MXAPP),ManWI_LabP2(MXAPP),
     +  ManWI_LabP3(MXAPP),ManWO_LabP1(MXAPP),ManWO_LabP2(MXAPP),
     +  ManWO_LabP3(MXAPP),ManSO_LabP1(MXAPP),
     +  ManSO_LabP2(MXAPP),ManSO_LabP3(MXAPP),ManSI_ActP1(MXAPP),
     +  ManSI_ActP2(MXAPP),ManSI_ActP3(MXAPP))
      
      Fxdate = 0
      DO 110 J=1,NAPPM
        READ(INP2,*) IMPL(J),IWMANR(J)
        BACKSPACE(INP2)
        IF(IWMANR(J).NE.5) THEN
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
        if (k.eq.11) then
          READ(INP2,*) IMPL(J),IWMANR(J),IMOFF(J,1),IMMETH(J),
     +        IMTYPE(J),AMNH4(J),AMOW(J),AMBED(J),CNBED(J),CNMANR(J),
     +        FCMANR(J)
              Mantpper1(j)=0.0D0
              Manweipper1(j)=0.0D0
              Manweopper1(j)=0.0D0
              Manwcper1(j)=0.0D0 
              IMANDECOMP(J)=0
        else IF (k.eq.15) then
          READ(INP2,*) IMPL(J),IWMANR(J),IMOFF(J,1),IMMETH(J),
     +        IMTYPE(J),AMNH4(J),AMOW(J),AMBED(J),CNBED(J),CNMANR(J),
     +        FCMANR(J),Mantpper1(j),Manweipper1(j),Manweopper1(j),
     +        Manwcper1(j)
              IMANDECOMP(J)=0
c     +        FCMANR(J),AppdayManmass1(j),Mantpper1(j),Manweipper1(j),
c     +        Manweopper1(j),Manwcper1(j), Manfc1(j),Mand1(j),Mlefts1(j)
               Mtype1(j) = IMTYPE(J)
              AppdayManmass1(J) = AMOW(J)*100.0D0 /
     +                               (100.0D0 - Manwcper1(J)) 
              Mantpper1(j) = (Mantpper1(j) / AppdayManmass1(j))*100.00
              AppdayManmass1(j) = AppdayManmass1(j) * AreaP
        else IF (k.eq.16) then
          READ(INP2,*) IMPL(J),IWMANR(J),IMOFF(J,1),IMMETH(J),
     +        IMTYPE(J),AMNH4(J),AMOW(J),AMBED(J),CNBED(J),CNMANR(J),
     +        FCMANR(J),Mantpper1(j),Manweipper1(j),Manweopper1(j),
     +        Manwcper1(j),IMandecomp(j)
c     +        FCMANR(J),AppdayManmass1(j),Mantpper1(j),Manweipper1(j),
c     +        Manweopper1(j),Manwcper1(j), Manfc1(j),Mand1(j),Mlefts1(j)
               Mtype1(j) = IMTYPE(J)
              AppdayManmass1(J) = AMOW(J)*100.0D0 /
     +                               (100.0D0 - Manwcper1(J)) 
              Mantpper1(j) = (Mantpper1(j) / AppdayManmass1(j))*100.00
              AppdayManmass1(j) = AppdayManmass1(j) * AreaP
        endif
       ELSE 
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
        if (k.eq.13) then
          READ(INP2,*) IMPL(J),IWMANR(J),ID,IM,IYYY,IMMETH(J),
     +        IMTYPE(J),AMNH4(J),AMOW(J),AMBED(J),CNBED(J),CNMANR(J),
     +        FCMANR(J)
              Mantpper1(j)=0.0D0
              Manweipper1(j)=0.0D0
              Manweopper1(j)=0.0D0
              Manwcper1(j)=0.0D0     
              IMANDECOMP(J)=0
        else IF (K.EQ.17) THEN             
          READ(INP2,*) IMPL(J),IWMANR(J),ID,IM,IYYY,IMMETH(J),
     +        IMTYPE(J),AMNH4(J),AMOW(J),AMBED(J),CNBED(J),CNMANR(J),
     +        FCMANR(J),Mantpper1(j),Manweipper1(j),Manweopper1(j),
     +        Manwcper1(j)
              IMANDECOMP(J)=0
              AppdayManmass1(J) = AMOW(J)*100.0D0 /
     +                               (100.0D0 - Manwcper1(J))
              Mantpper1(j) = (Mantpper1(j) / AppdayManmass1(j))*100.00
              AppdayManmass1(j) = AppdayManmass1(j) * AreaP
        else IF (K.EQ.18) THEN             
          READ(INP2,*) IMPL(J),IWMANR(J),ID,IM,IYYY,IMMETH(J),
     +        IMTYPE(J),AMNH4(J),AMOW(J),AMBED(J),CNBED(J),CNMANR(J),
     +        FCMANR(J),Mantpper1(j),Manweipper1(j),Manweopper1(j),
     +        Manwcper1(j),IMANDECOMP(J)
              AppdayManmass1(J) = AMOW(J)*100.0D0 /
     +                               (100.0D0 - Manwcper1(J))
              Mantpper1(j) = (Mantpper1(j) / AppdayManmass1(j))*100.00
              AppdayManmass1(j) = AppdayManmass1(j) * AreaP
          endif
            !Mday1(j)= ID
            !Mmon(j)= IM
            !Myear1(j) = IYYY
            Mtype1(j) = IMTYPE(J)
            Fxdate = Fxdate + 1
            
          IF(IYYY.LT.1900) THEN
            PRINT*,'2-DIGIT YEAR IN MANURE MANAGEMENT, CONVERTING'
            IYYY=IYYY+1900
          ENDIF
          IMOFF(J,1)=JDATE(ID,IM,IYYY)
          IMOFF(J,2)=IYYY
        ENDIF
C
C       ..SAVE USER SPECIFIED C:N RATIO FOR BEDDING
        IF(IMTYPE(J).EQ.15) AMCN(15)=CNBED(J)
  110 CONTINUE
      DO J=1,NAPPM
C  VERY SLOW  100% TO THE SLOW RESIDUE POOL
      IF (IMANDECOMP(J).EQ.1) THEN
         TCARBON1=AMOW(J)*FCMANR(J)
         TNITM=AMOW(J)*FCMANR(J)/CNMANR(J)
         CNMANR(J)=CN(1)
         FCMANR(J)=TNITM*CNMANR(J)/AMOW(J)
         TCARBON2=AMOW(J)*FCMANR(J)
         RCO2=RCO2+(TCARBON1-TCARBON2)
C  SOMEWHAT SLOW  ONLY 25% TO THE FAST RESIDUE POOL
      ELSE IF (IMANDECOMP(J).EQ.2) THEN
         TCARBON1=AMOW(J)*FCMANR(J)
         TNITM=AMOW(J)*FCMANR(J)/CNMANR(J)
         CNMANR(J)=1.0D0/(0.25D0*(1.0D0/CN(2)-1.0D0/CN(1))+1.0D0/CN(1))
         FCMANR(J)=TNITM*CNMANR(J)/AMOW(J)
         TCARBON2=AMOW(J)*FCMANR(J)
         RCO2=RCO2+(TCARBON1-TCARBON2)
C  MEDIUM 50% TO THE FAST RESIDUE POOL
      ELSE IF (IMANDECOMP(J).EQ.3) THEN
         TCARBON1=AMOW(J)*FCMANR(J)
         TNITM=AMOW(J)*FCMANR(J)/CNMANR(J)
         CNMANR(J)=1.0D0/(0.5D0*(1.0D0/CN(2)-1.0D0/CN(1))+1.0D0/CN(1))
         FCMANR(J)=TNITM*CNMANR(J)/AMOW(J)
         TCARBON2=AMOW(J)*FCMANR(J)
         RCO2=RCO2+(TCARBON1-TCARBON2)
C  SOMEWHAT FAST   75% TO THE FAST RESIDUE POOL
      ELSE IF (IMANDECOMP(J).EQ.4) THEN
         TCARBON1=AMOW(J)*FCMANR(J)
         TNITM=AMOW(J)*FCMANR(J)/CNMANR(J)
         CNMANR(J)=1.0D0/(0.75D0*(1.0D0/CN(2)-1.0D0/CN(1))+1.0D0/CN(1))
         FCMANR(J)=TNITM*CNMANR(J)/AMOW(J)
         TCARBON2=AMOW(J)*FCMANR(J)
         RCO2=RCO2+(TCARBON1-TCARBON2)
C  VERY FAST   100% TO THE FAST POOL
      ELSE IF (IMANDECOMP(J).EQ.5) THEN
         TCARBON1=AMOW(J)*FCMANR(J)
         TNITM=AMOW(J)*FCMANR(J)/CNMANR(J)
         CNMANR(J)=CN(2)
         FCMANR(J)=TNITM*CNMANR(J)/AMOW(J)
         TCARBON2=AMOW(J)*FCMANR(J)
         RCO2=RCO2+(TCARBON1-TCARBON2)
         ENDIF
      ENDDO
         
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      F E R T I L I Z E R   M A N A G E M E N T       --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NAPPF
      IF(NAPPF.GT.MXAPP) THEN
        PRINT*,' TOO MANY FERTILIZER APPLICATIONS SPECIFIED'
        STOP
      ENDIF
      
      Nfert = NAPPF
      ALLOCATE(Fday(MXAPP),Fyear(MXAPP),Fmon(MXAPP),Fertp(MXAPP),
     +         Fertd(MXAPP),Flefts(MXAPP),Avfertp(MXAPP),
     +         Daysfert(MXAPP),Resfertp(MXAPP),Fertpfr(MXAPP),
     +         Scov(MXAPP),Fertpsorp(MXAPP),Appdayavfertp(MXAPP),
     +         Appdayresfertp(MXAPP),Dummy2(MXAPP),
     +         Dailyfertpsop(MXAPP),Norain(MXAPP),Fertprelease(MXAPP),
     +         Fertpcrunoff(MXAPP),Fertpmrunoff(MXAPP),PPavfertp(MXAPP),
     +         PPresfertp(MXAPP),PPmavfertp(MXAPP),
     +         PPcavfertp(MXAPP), PPcresfertp(MXAPP),PPmresfertp(MXAPP),
     +         FertP1(MXAPP),Fertd1(MXAPP),Flefts1(MXAPP),Scov1(MXAPP),
     +         FertPLabP1(MXAPP),FertPLabP2(MXAPP))
      
      DO 120 J=1,NAPPF
        READ(INP2,*) IFPL(J),IWFERT(J)
        BACKSPACE(INP2)
        IF(IWFERT(J).NE.5) THEN
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
        if (k.eq.12) then
          READ(INP2,*) IFPL(J),IWFERT(J),IFOFF(J,1),IFMETH(J),
     +        FNO3IN(J),FNH4IN(J),FUREIN(J),IBMPAP(J),IBMPMD(J),
     +        IDSPLT(J),SPLTST(J),SPLTAP(J)
          Fertp1(j)=0.0d0
        else
          READ(INP2,*) IFPL(J),IWFERT(J),IFOFF(J,1),IFMETH(J),
     +        FNO3IN(J),FNH4IN(J),FUREIN(J),IBMPAP(J),IBMPMD(J),
     +        IDSPLT(J),SPLTST(J),SPLTAP(J),Fertp1(j)
c     +       Fertd1(j),Flefts1(j),Scov1(j)
          Fertp1(j) = Fertp1(j) * AreaP
        endif
        ELSE
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
        if (k.eq.14) then
           READ(INP2,*) IFPL(J),IWFERT(J),ID,IM,IYYY,IFMETH(J),
     +        FNO3IN(J),FNH4IN(J),FUREIN(J),IBMPAP(J),IBMPMD(J),
     +        IDSPLT(J),SPLTST(J),SPLTAP(J)
          Fertp1(j)=0.0d0
        else       
          READ(INP2,*) IFPL(J),IWFERT(J),ID,IM,IYYY,IFMETH(J),
     +        FNO3IN(J),FNH4IN(J),FUREIN(J),IBMPAP(J),IBMPMD(J),
     +        IDSPLT(J),SPLTST(J),SPLTAP(J),Fertp1(j)
c     +       Fertd1(j),Flefts1(j),Scov1(j)
           
          Fertp1(j) = Fertp1(j) * AreaP
          endif
          !Fday(j) = ID
          !Fmon(j)= IM
          !Fyear(j) = IYYY 
          
          IF(IYYY.LT.1900) THEN
            PRINT*,'2-DIGIT YEAR IN FERTILIZER MANAGEMENT, CONVERTING'
            IYYY=IYYY+1900
          ENDIF
          IFOFF(J,1)=JDATE(ID,IM,IYYY)
          IFOFF(J,2)=IYYY
        ENDIF
C
C       ..DO ALITTLE ERROR CHECKING FOR BMP OPTIONS
        IF((IBMPAP(J).EQ.4.OR.IBMPAP(J).EQ.5).AND.IBMPMD(J).NE.3) THEN
          IBMPMD(J)=3
        ENDIF
        IF((IBMPAP(J).EQ.1.OR.IBMPAP(J).EQ.3).AND.IBMPMD(J).EQ.3) THEN
          IBMPAP(J)=4
        ENDIF
  120 CONTINUE
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      B M P   M A N A G E M E N T
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) ISTATE,YGOAL,OMADJ,USAMDP,ISOYB,UMISCC,SOYYLD,FMC,
     +    YGSF,BMPIRR,BMPTIL
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      P E S T I C I D E M A N A G E M E N T       --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NAPP
      IF(NAPP.GT.MXAPP) THEN
        PRINT*,' TOO MANY PESTICIDE APPLICATIONS SPECIFIED'
        STOP
      ENDIF
      DO 130 J=1,NAPP
        READ(INP2,*) IPPL(J),IAPP(J),IWPEST(J)
        BACKSPACE(INP2)
        IF(IWPEST(J).NE.5) THEN
          READ(INP2,*) IPPL(J),IAPP(J),IWPEST(J),IPOFF(J,1),IPMETH(J),
     +        APP(J),PDR(J)
        ELSE
          READ(INP2,*) IPPL(J),IAPP(J),IWPEST(J),ID,IM,IYYY,IPMETH(J),
     +        APP(J),PDR(J)
          IF(IYYY.LT.1900) THEN
            PRINT*,'2-DIGIT YEAR IN PESTICIDE MANAGEMENT, CONVERTING'
            IYYY=IYYY+1900
          ENDIF
          IPOFF(J,1)=JDATE(ID,IM,IYYY)
          IPOFF(J,2)=IYYY
        ENDIF
  130 CONTINUE
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      T I L L A G E     M A N A G E M E N T        --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NUMTL
      Ntill = NUMTL
      ALLOCATE(Tday1(MXAPP),Tmon(MXAPP),Tyear(MXAPP),Tillinceffi(MXAPP)
     + ,Tillmixeffi(MXAPP),Tilld(MXAPP),Tillinceffi1(MXAPP),
     + Tillmixeffi1(MXAPP),Tilld1(MXAPP)) 
      
      IF(NUMTL.GE.MXAPP-1) THEN
        PRINT*,' TOO MANY TILLAGE APPLICATIONS SPECIFIED'
        STOP
      ELSE
C
C       ..SETUP LAST IMPLIMENT FOR ANHYDROUS APPLICATOR FOR USE BY BMP
        IMP(MXAPP)=27
        UDEPTH(MXAPP)=BMPTIL
        TNDX(MXAPP)=1.0D0
        ITL(MXAPP)=2
        PManureMix(MXAPP)=0.20D0    ! Zhiming added manure P mixing coeff. by tillag    
      ENDIF
      DO 140 J=1,NUMTL
        READ(INP2,*) ITPL(J),IWTILL(J)
        BACKSPACE(INP2)
        IF(IWTILL(J).NE.5) THEN
         read (inp2,1110) string   ! Zhiming copied for interface tillage
         k=inumb(string)
         backspace (inp2) 
         IF (k .EQ. 7) then
           READ(INP2,*) ITPL(J),IWTILL(J),ITOFF(J,1),IMP(J),UDEPTH(J),
     +        TNDX(J),ITL(J) 
              PManureMix(J)= 0.2D0
          ELSE
           READ(INP2,*) ITPL(J),IWTILL(J),ITOFF(J,1),IMP(J),UDEPTH(J),
     +        TNDX(J),ITL(J), PManureMix(J)    ! Zhiming added manure P mixing coeff. by tillage         
         END IF
        ELSE
        
          read (inp2,1110) string   ! Zhiming copied for interface tillage
          k=inumb(string)
          backspace (inp2) 
          IF (k .EQ. 9) then
          READ(INP2,*) ITPL(J),IWTILL(J),ID,IM,IYYY,IMP(J),UDEPTH(J),
     +        TNDX(J),ITL(J)   ! Zhiming added manure P mixing coeff. by tillage
              PManureMix(J)= 0.2D0
           ELSE 
             READ(INP2,*) ITPL(J),IWTILL(J),ID,IM,IYYY,IMP(J),UDEPTH(J),
     +        TNDX(J),ITL(J), PManureMix(J) 
          END IF
          
          IF(IYYY.LT.1900) THEN
            PRINT*,'2-DIGIT YEAR IN TILLAGE MANAGEMENT, CONVERTING'
            IYYY=IYYY+1900
          ENDIF
          JDAY=JDATE(ID,IM,IYYY)
          ITOFF(J,1)=JDAY
          ITOFF(J,2)=IYYY
        ENDIF
          Tilld1(j) = UDEPTH(J)*0.01
          Tillinceffi1(j) = TNDX(J)*100.0
          Tillmixeffi1(j) = PManureMix(J)*100
          ! was Tillmixeffi1(j) = 100.0 - TNDX(J)*100.00; Zhiming changed to PManureMix(J) requested by Peng Pan 
  140 CONTINUE
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      M I S C E L L A N O U S M A N A G E M E N T      --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
      CALL ECHO(INP2)
      READ(INP2,*) NUMTHG
      IF(NUMTHG.GT.MXTG) THEN
        PRINT*,' TOO MANY HEAD GATE APPLICATIONS SPECIFIED'
        PRINT*,' NUMBER OF APPS ALLOWED ARE: ',MXTG
        STOP
      ELSE
        THGCUR=DRDEP
      ENDIF
      DO 150 J=1,NUMTHG
          READ(INP2,*) ID,IM,IYYY,THGDEP(J)
          JDAY=JDATE(ID,IM,IYYY)
          ITHGOFF(J,1)=JDAY
          ITHGOFF(J,2)=IYYY
  150 CONTINUE
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     --      I R R I G A T I O N           --
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
C     ***    INPUT/CALCULATE IRRIGATION CONTROL DATA
      CALL ECHO(INP2)
        read (inp2,1110) string
        k=inumb(string)
        backspace (inp2)
       if(k.eq.2) then
      READ(INP2,*) NAPPI,id_depth
      monthirr=0
      amirr=0.0d0
      amxirr=100.0d0
      airrdepth=15.0d0
           totaliw=0.0d0
           niw=0
       else if (k.eq.3) then
      READ(INP2,*) NAPPI,id_depth,monthIRR
      amirr=0.0d0
      amxirr=100.0d0
      airrdepth=15.0d0
           totaliw=0.0d0
           niw=0
       else if (k.eq.4) then
      READ(INP2,*) NAPPI,id_depth,monthIRR,amirr
      amxirr=100.0d0
      airrdepth=15.0d0
           totaliw=0.0d0
           niw=0
       else if (k.eq.5) then
      READ(INP2,*) NAPPI,id_depth,monthIRR,amirr,amxirr
      airrdepth=15.0d0
           totaliw=0.0d0
           niw=0
       else if (k.eq.6) then
           totaliw=0.0d0
           niw=0
      READ(INP2,*) NAPPI,id_depth,monthIRR,amirr,amxirr,airrdepth
       else if (k.eq.8) then
      READ(INP2,*) NAPPI,id_depth,monthIRR,amirr,amxirr,airrdepth,
     &             totaliw,niw
       endif
      if (airrdepth.eq.0.0d0) airrdepth=15.0d0
       DO 200 J=1,NAPPI
        READ(INP2,*) IIPL(J),IRRTYP(J),ITOI(J),IDOI(J),IRIGDB,IRIGMB,
     +      IRIGYB,IRIGDE,IRIGME,IRIGYE,NDAIRR(J),DOIMAX(J),RRATE(J)
c        JDIRRB(J)=MIN(JULDATE(IRIGDB,IRIGMB,IRIGYB),JULBDAY)
        JDIRRB(J)=Max(JULDATE(IRIGDB,IRIGMB,IRIGYB),JULBDAY)
        JDIRRE(J)=MIN(JULDATE(IRIGDE,IRIGME,IRIGYE),JULEDAY)
        if (id_depth.le.0) then 
            ipsw_depth=100
        else
            ipsw_depth=id_depth
        endif
C
        IF(IRRTYP(J).NE.0) THEN
C         ----------------------------------------------------
C         ***  WHEN TO IRRIGATE, INPUT FOR 1 OF 3 OPTIONS
C
C=====================================================
C         *** 1) FIXED INTERVAL
C=====================================================
          IF(ITOI(J).EQ.1) THEN
            READ(INP2,*) IRRINT
            JDOI(J,1)=JDIRRB(J)
            JDOLI=JDIRRE(J)
            DO 160,I=2,100
              JDOI(J,I)=JDOI(J,I-1)+IRRINT
              NOIRR=I
              IF(JDOI(J,I).GT.(JDOLI-IRRINT)) GOTO 170
  160       CONTINUE
  170       IF(NOIRR.EQ.100) THEN
              WRITE(*,1100) J
            ENDIF
C
C=====================================================
C         *** 2) SPECIFIED DATES FOR SEASON
C=====================================================
          ELSEIF(ITOI(J).EQ.2) THEN
            READ(INP2,*) NOIRR
            DO 180 I=1,NOIRR
              READ(INP2,*) IDS,IMS,IYS
              JDOI(J,I)=JULDATE(IDS,IMS,IYS)
              IF (JDOI(J,I).LT.JULBDAY) THEN
                  ISPT(J)=ISPT(J)+1
              ENDIF
              IF (I.GT.1) THEN
                IF (JDOI(J,I).LT.JDOI(J,I-1)) THEN
              WRITE(*,1111) IDS,IMS,IYS
              STOP
              ENDIF
              ENDIF
  180       CONTINUE
C
C=====================================================
C         *** 3) SOIL WATER DEPLETION USING ONLY THE ROOT ZONE
C=====================================================
          ELSEIF(ITOI(J).EQ.3) THEN
            READ(INP2,*) NMAD(J),(JMAD(J,I),XMAD(J,I),I=1,NMAD(J))
C
C=====================================================
C         *** 4) ET DEFICIT 
C=====================================================
          ELSEIF(ITOI(J).EQ.4) THEN
            READ(INP2,*) NMAD(J),(JMAD(J,I),XMAD(J,I),I=1,NMAD(J))
c            
C=====================================================
C         *** 5) CANOPY TEMP
C=====================================================
          ELSEIF(ITOI(J).EQ.5) THEN
            READ(INP2,*) NMAD(J),(JMAD(J,I),XMAD(J,I),I=1,NMAD(J))
C
          ELSE
            STOP'INPUT INCORRECT FOR IRRIGATION ==> ITOI'
          ENDIF
C         ----------------------------------------------------
C         ***DEPTH OF IRRIGATION, INPUT FOR 1 OF 3 OPTIONS
C
C=====================================================
C         *** 1) FIXED AMOUNT PER IRRIGATION
C=====================================================
          IF(IDOI(J).EQ.1) THEN
            READ(INP2,*) FDOI(J)
C
C=====================================================
C         *** 2) VARYING AMOUNT PER IRRIGATION
C=====================================================
          ELSEIF(IDOI(J).EQ.2) THEN
            READ(INP2,*) NVDOI
            READ(INP2,*) (VDOI(J,I),I=1,NVDOI)
C           IF NVDOI < NOIRR, LAST VALUE IS USED FOR THE REST OF EVENTS
            DO 190 I=NVDOI+1,NOIRR
              VDOI(J,I)=VDOI(J,NVDOI)
  190       CONTINUE
C
C=====================================================
C         *** 3) FILL ROOTZONE TO LIMIT OF AVAILABLE WATER
C=====================================================
          ELSEIF(IDOI(J).EQ.3) THEN
            READ(INP2,*) NKU(J),(JKU(J,I),XKU(J,I),I=1,NKU(J))
C
C=====================================================
C         *** 4) % ET REFILL
C=====================================================
          ELSEIF(IDOI(J).EQ.4) THEN
            READ(INP2,*) NKU(J),(JKU(J,I),XKU(J,I),I=1,NKU(J))
c
          ELSE
            STOP'INPUT INCORRECT FOR IRRIGATION ==> IDOI'
          ENDIF
        ENDIF
  200 CONTINUE
C
      CALL ECHO(INP2)
        read (inp2,fmt=1110,end=21) string
        k=inumb(string)
        backspace (inp2)
       if((k.GE.1).and.(monthIRR.gt.0)) then
       TOTALMONTH=0.0D0
       READ(INP2,*) NMOYR
        DO I=1,NMOYR
          READ(INP2,*) IYDUM,(AIM(J),J=1,12)
          IF (IYDUM.GE.IYB) THEN
           DO K=1,12
           DOIMONTH(K,IYDUM-IYB+1)=AIM(K)
           TOTALMONTH=TOTALMONTH+AIM(K)
           ENDDO
          endif
        ENDDO
       ENDIF
c
21      CLOSE(UNIT=INP2)
C
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C     ---                              ---
C     ---    ALL THE INPUT IS FINISHED, NOW NEED TO INITIALIZE     ---
C     ---    SOME VARIABLES              ---
C     ---                              ---
C     ----------------------------------------------------------------------
C     ----------------------------------------------------------------------
C
C     ..INITIALIZE RESIDUE COVER TYPE BASED ON CROP TYPE
      DO 210 IPL=1,NPL
        IF(   INDEX(PLNAME(IPL),'CORN').NE.0.OR.
     +        INDEX(PLNAME(IPL),'corn').NE.0.or.
     +        INDEX(PLNAME(IPL),'MAIZE').NE.0.OR.
     +        INDEX(PLNAME(IPL),'maize').NE.0.or. 
     +        INDEX(PLNAME(IPL),'Corn').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'Maize').NE.0)    !.or.(INXPL(IPL).LT.100))
     +      THEN
          IRTYPE(IPL)=1
        ELSEIF(INDEX(PLNAME(IPL),'soybean').NE.0.or.
     +        INDEX(PLNAME(IPL),'SOYBEAN').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'Soybean').NE.0)  !.or.(INXPL(IPL).LT.200))
     +    THEN
          IRTYPE(IPL)=2
        ELSEIF(INDEX(PLNAME(IPL),'WHEAT').NE.0.OR.
     +        INDEX(PLNAME(IPL),'Wheat').NE.0.or.
     +        INDEX(PLNAME(IPL),'wheat').NE.0)    !.or.(INXPL(IPL).LT.400))
     +   THEN
          IRTYPE(IPL)=3
        ELSEIF(INDEX(PLNAME(IPL),'BERMUDAGRASS').NE.0.OR.
     +        INDEX(PLNAME(IPL),'Bermudagrass').NE.0.or.
     +        INDEX(PLNAME(IPL),'bermudagrass').NE.0)    !.or.(INXPL(IPL).LT.400))
     +   THEN
          IRTYPE(IPL)=3
            ELSE IF(INDEX(PLNAME(IPL),'SUNFLOWER').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sunflower').NE.0 ) THEN
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'ALFALFA').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'alfalfa').NE.0 ) THEN
 	          IRTYPE(IPL)=2
            ELSE IF(INDEX(PLNAME(IPL),'COTTON').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'cotton').NE.0 ) THEN
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'SUGARCANE').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sugarcane').NE.0 ) THEN
 	          IRTYPE(IPL)=1
            ELSE IF(INDEX(PLNAME(IPL),'SORGHUM').NE.0 .OR.
     +        INDEX(PLNAME(IPL),'sorghum').NE.0 ) THEN
	          IRTYPE(IPL)=1
        ELSE   !ALL OTHER CROPS
          IRTYPE(IPL)=2
        ENDIF
  210 CONTINUE
C
C     ..SET UP THE REST OF THE NUMERICAL NODE SYSTEM
      ZN(1)=0.5D0
      TL(1)=1.0D0
      TLT(1)=1.0D0
      DO 220 I=2,NN-1
        TL(I)=(DELZ(I-1)+DELZ(I))*0.5D0
        TLT(I)=TLT(I-1)+TL(I)
        ZN(I)=ZN(I-1)+DELZ(I-1)
  220 CONTINUE
      TL(NN)=DELZ(I-1)
      TLT(NN)=TLT(NN-1)+TL(NN)
      ZN(NN)=ZN(NN-1)+DELZ(NN-1)
C
C     ... DISCRETIZATION FOR INFILTRATION MODEL
      IF(PRODEP.NE.TLT(NN)) THEN
        PRINT*," ERROR - HORIZON AND NUMERICAL GRIDS DO NOT AGREE"
        STOP" CHECK HORIZON AND NUMERICAL GRID"
      ENDIF
      NSLT=INT(PRODEP)
      NSLT=MIN(NSLT,3000)
      NNT=NSLT
      NHZ=NHOR
      DO 230 I=1,NHOR
        NSL(I)=INT(HORTHK(I))
        DHB(I)=HORTHK(I)
  230 CONTINUE
      DO 240 I=1,NSLT
        DSL(I)=DBLE(I)
  240 CONTINUE
C
C     .. SET INITIAL TILE DRAINAGE EFFECTIVE DEPTH INDEX
      IDP=1
      DO 245 I=1,NN
        IF(DRDEP.GT.TLT(I)) IDP=I
  245 CONTINUE
      IDP=MIN(NN,IDP+1)

C
C     ... SET UP THE INDEX VECTORS FOR INTERPOLATION PURPOSES
      CALL INDEXR(HORTHK,TLT,DSL,NHOR,NN,NSLT)
C
C     ... INITIALIZE SOIL HYDRAULIC PARAMETERS
      DO 250 J=1,NHOR
        PCLAY=SOILPP(7,J)*100.0D0
        PSILT=SOILPP(6,J)*100.0D0
        PSAND=SOILPP(5,J)*100.0D0
        TBD=SOILPP(3,J)
        SOILPP(4,J)=1.0D0-SOILPP(3,J)/SOILPP(2,J)
        ITYPE=INT(SOILPP(1,J))
        CALL SOILPR(J,SOILHP,TBD,PCLAY,PSAND,PSILT,ITYPE,SOILPP(2,J),0,
     +              HWP)
        SOILPP(4,J)=SOILHP(6,J)
c        IF(CLAT(J).LE.0.0D0) CLAT(J)=SOILHP(4,J)
  250 CONTINUE
C
      RETURN
  260 PRINT*,'RZWQM.DAT CONTROL INFORMATION CONTAINS NEW ',
     +    'VARIABLES - PLEASE CORRECT AND TRY AGAIN'
      STOP
 1000 FORMAT(I4,2X,A30)
 1100 FORMAT('IRRIGATION: NEED TO SPLIT UP THE SEASON LISTED ',I3)
 1111 FORMAT('PLEASE CHECK IRRIGATION DATES, OUT OF ORDER ON OR BEFORE',
     &      2X,I3,'/',I2,'/',I4)
 27    stop 'check your pesticide daughter formation in RZWQM.DAT file'
      END
C
      SUBROUTINE TILADJ(NSC,NHORZ,Hmin)
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       AGSZ  I/O AVE. LENGTH OF AGGREGATE IN HORIZON [CM]
C       DHB       I/O DEPTH OF HORIZON BOUNDARIES [CM]
C       CLAT   I  LATERAL HYDROLIC CONDUCTIVITY [CM/HR]
C       DMAX   L
C       DMEAN  L
C       DMIN   L
C       DSL        I  THICKNESS OF INFILTRATION LAYERS [CM]
C       DSUM   L
C       FDEP  I/O FRACTION OF TOTAL MACROPORES THAT ARE DEADEND [0..1]
C       HORTHK    I/O DEPTH OF SOIL HORIZONS [CM]
C       I      L  INDEX VARIABLE
C       IC         L
C       IH         L
C       IKEN   P
C       IMP        I  --MONTH OF PLANTING.
C       IOFF   L
C       IT         L
C       ITL        I
C       ITLE   L
C       ITYPE  L  INDICATOR FOR SOIL TYPE
C       J      L  INDEX VARIABLE
C       K      L  VON KARMAN CONSTANT ( =.41)
C       KK         L
C       KK2        L
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MCPOR I/O TOTAL MACROPOROSITY OF HORIZON [CM3/CM3]
C       MICP  I/O TOTAL MICROPOROSITY OF HORIZON [CM3/CM3]
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NHCMP I/O NUMBER OF HORIZONS CONTAINING MACROPORES
C       NHOR  I/O NUMBER OF SOIL HORIZONS
C       NHORZ I/O ORIGINAL NUMBER OF USER SPECIFIED HORIZONS
C       NHZ       I/O NUMBER OF SOIL HORIZONS
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       NPRI   L
C       NSL       I/O DEPTH TO BOTTOM OF SOIL HORIZONS [CM]
C       NSLT   I  PROFILE DEPTH [CM]
C       NUMTL  I
C       PCLAY  L  SEDIMENT FRACTION CLAY CONTENT [0..1]
C       PL        I/O LENGTH OF VERTICAL MACROPORE CRACKS  [CM]
C       PSAND  L  SEDIMENT SIZE FRACTION SAND PORTION [0..1]
C       PSILT  L  SEDIMENT SIZE FRACTION SILT PORTION [0..1]
C       RP        I/O RADIUS OF CYLINDRICAL MACROPORES IN HORIZON [CM]
C       SOILHP    I/O MODIFIED BROOKS-COREY PARAMETERS
C                   (1):   HB    - BUBBLING PRESSURE O(H) [CM]
C                   (2):   LAMDA - PORE SIZE DISTRIBUTION INDEX
C                   (3):   EPS   - EXPONENT FOR K(H) CURVE
C                   (4):   KSAT  - SAT HYDRAULIC CONDUCT [CM/HR]
C                   (5):   WR    - RESIDUAL WATER CONTENT
C                   (6):   WS    - SATURATION WATER CONTENT
C                   (7):   WFC   - FIELD CAPACITY (1/3 BAR) WC
C                   (8):   WFC   - FIELD CAPACITY (1/10 BAR) WC
C                   (9):   WWP   - WILTING POINT (15 BAR) WC
C                   (10):  HB    - BUBBLING PRESSURE K(H) CURVE [CM]
C                   (11):  C2    - SECOND INTRCEPT ON K(H) CURVE
C                   (12):  N1    - FIRST EXPONENT FOR K(H) CURVE
C                   (13):  A1    - CONSTANT FOR O(H) CURVE
C       SOILPP    I/O
C       SOLTP1    I/O ARRAY OF HEAT MODEL PARAMETERS,
C                  1: SAT MOISTURE CONTENT [0..1]
C                  2: FIELD CAPACITY [0..1]
C                  3: TEXTURE CLASS (1-COARSE, 2-MED, 3-FINE)
C                  4: # CONSTITUENTS FOR THERMAL PROPERTY CALC
C                  5: DRY VOL HEAT CAPACITY  [J/MM^3/C]
C       SOLTP2    I/O (MAXSCT X MAXHOR X 3) ARRAY OF SOIL HORIZON
C                 CONSTITUENT PROPERTIES, CONSTIT'S BY ROW,
C                 HORIZON BY COL, PROPERTY BY PLANE
C                 1: CONSTITUENT VOLUME FRACTIONS
C                 2: CONSTITUENT HEAT CONDUCT [J/MM/HR/C],
C                 3: PARTICLE SHAPE FACTORS GA
C       TBD        L
C       TLT        I  DEPTH TO BOTTOM OF NUMERICAL LAYERS [CM]
C       TMAX   L  MAXIMUM AIR TEMPERATURE [C]
C       TMOVE  L
C       TT         L
C       UDEPTH     I
C       WP        I/O WIDTH OF RECTANGULAR CRACKS IN HORIZON [CM]
C       YESMAC     I  INDICATS PRESENCE OF MACROPORES =1 YES, =0 NOT
C       ZHOR   L
C
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 HZPARO
C                 INDEXR
C                 SOILPR
C
C       CALLED FROM:
C
C       PROGRAMMER:   KAREN JOHNSEN AND KEN ROJAS
C
C       VERSION:  2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C     ..ARRAY DIMENSION VALUES
      PARAMETER(MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,MXAPP=200,
     +    MXCHEM=15,MXSPEC=10,MXPEST=3)
c      PARAMETER(HMIN=-35000.0D0)
C
C     VARIABLES PERTAINING TO SOIL PARAMETERS & PROPERTIES
      COMMON /SOIL/ SOILPP(8,MAXHOR),pori(mxnod)
C
C     VARIABLES PERTAINING TO SYSTEM DISCRETIZATION
      COMMON /GEOMTR/XLONG,ASPECT,DELZ(MXNOD),ELEV,HORTHK(MAXHOR),SLOPE,
     +    AREA,TL(MXNOD),TLT(MXNOD),XLAT,ZN(MXNOD),NHOR,NN,NNT
C
C     VARIABLES PERTAINING TO SOIL HEAT TRANSPORT
      COMMON /HEAT/ CSH(MXNOD),T(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3)
C
C     VARIABLES PERTAINING TO HYDROLOGY
      COMMON /HYDROL/ AEF,BOTFLX,DRDEP,DRSPAC,DRRAD,CLAT(MAXHOR),
     +    H(MXNOD),HKBAR(MXNOD),QF(MXNOD),SOILHP(13,MAXHOR),HFC,HWP,
     +    THETA(MXNOD),thetai(mxnod),RICH_EPS,IREBOT,ITBL,IDRAIN,
     +    MAXITER_RICH,MAXCYCLE_RICH
C
C   ...COMMON SPACE FOR INFILTRATION AND EVENT-TRANSPORTATION
C
C ----------------------------------------------------------------------
      COMMON /IPCHEM/ A,ALH(MXNOD,MXCHEM),B,BDH(MAXHOR),DEG(MAXHOR),
     +    BDHR(MAXHOR),MICP(MAXHOR),NMICP
C ----------------------------------------------------------------------
      COMMON /IPMACF/ AGSZ(MAXHOR),FDEP(MAXHOR),MCPOR(MAXHOR),PI,
     +    PL(MAXHOR),RP(MAXHOR),SFCT,TWOPI,WP(MAXHOR),NHCMP,YESMAC,
     +    EFFWR,EFFWC,XPRESF
C ----------------------------------------------------------------------
      COMMON /IPINF/ CRUSTK,OCRUST,ICRUST,INFLPD,NHZ,NSLT,POND,RR,
     +    DHB(MAXHOR),NSL(MAXHOR),WI(MXnod),DSL(MXNODT),VWC(MXNODT),
     +    IUGFLW
C ----------------------------------------------------------------------
      DOUBLE PRECISION MICP,MCPOR
      INTEGER YESMAC
C
C     Zhiming added Pmanuremix
      COMMON /TILLIN/ OBD(2),OFDP(2),TDEPTH(30),TILINT(30,2),
     +    TNDX(MXAPP),UDEPTH(MXAPP), PManureMix(MXAPP), IMP(MXAPP),  
     +    ITL(MXAPP), ITOFF(MXAPP,2),IWTILL(MXAPP),NUMTL, ITPL(MXAPP) 
C
      CHARACTER SOLTYP*10,NMPEST*30,PLNAME*30
      COMMON /NAMES/ NMPEST(MXPEST),SOLTYP(MAXHOR),PLNAME(MXSPEC)
C
      COMMON /NDEX/ NDXN2H(MXNOD),NDXH2N(MAXHOR),NDXN2T(MXNOD),
     +    NDXT2N(MXNODT)
C
      PARAMETER(IKEN=8+13+5+3*MAXSCT+8)
      DIMENSION ZHOR(MAXHOR),TMOVE(IKEN)
      CHARACTER TMOVEN(IKEN)*10
C
C      WRITE(*,*) 'AT START IN TILADJ'
C      DO 20 I=1,NHOR
C        WRITE(*,*) ' '
C        WRITE(*,'(A2,I3,A8,F5.0)') 'I=',I,'HORTHK=',HORTHK(I)
C        WRITE(*,'(A7)') 'SOILPP:'
C        WRITE(*,1000) (SOILPP(J,I),J=1,8)
C        WRITE(*,'(A7)') 'SOILHP:'
C        WRITE(*,1000) (SOILHP(J,I),J=1,13)
C        WRITE(*,'(A7)') 'SOLTP1:'
C        WRITE(*,1000) (SOLTP1(I,J),J=1,5)
C        WRITE(*,'(A7)') 'SOLTP2:'
C        DO 10 K=1,MAXSCT
C          WRITE(*,1000) (SOLTP2(K,I,J),J=1,3)
C   10   CONTINUE
C        WRITE(*,'(A20,I3)') 'MICP AND MACP PROPS:',NHCMP
C        WRITE(*,1000) MICP(I),MCPOR(I),RP(I),WP(I),PL(I),AGSZ(I),
C     +      FDEP(I)
C   20 CONTINUE
C
C     ..CHECK TO SEE IF ONLY USING ANHYDROUS APP'S, THEN BYPASS BELOW
      ITLE=1
      DO 30 J=1,NUMTL
        IF(IMP(J).NE.27) ITLE=ITLE+1
   30 CONTINUE
C
C
C     ..START HORIZON MODIFICATION FOR TILLAGE IMPLIMENTS
C
C     ..START DSUM WITH DEFAULT IMPLIMENT (FIELD CULT), EFF DEPTH=10 CM
      DSUM=10.0D0
      DMAX=10.0D0
      NPRI=0
      DO 40 J=1,NUMTL
C
C       ..USE IMPLIMENTS OTHER THAN ANHY. APP
        IF(IMP(J).NE.27) THEN
          DSUM=DSUM+UDEPTH(J)
          DMAX=MAX(DMAX,UDEPTH(J))
          IF(ITL(J).EQ.1) NPRI=NPRI+1
        ENDIF
   40 CONTINUE
C
C
C     ..FIND HORIZON BOUNDRY CLOSEST TO MAX IMPLIMENT DEPTH
C     (MAY BE EITHER ABOVE OR BELOW HORIZON BOUNDRY)
      DMIN=99999.0D0
      NHORZ=NHOR
      DO 50 I=1,NHOR
        ZHOR(I)=HORTHK(I)
        TT=ABS(HORTHK(I)-DMAX)
        IF(TT.LE.DMIN) THEN
          DMIN=TT
          KK=I
        ENDIF
   50 CONTINUE
C
C     ..BEGIN ADJUSTING HORIZON BOUNDRIES IF NECESSARY
      IF(ITLE.NE.1) THEN
        DMEAN=(DSUM-DMAX)/(ITLE-1)
      ELSE
        DMEAN=DSUM
      ENDIF
C
      IC=1
      IF(KK.GT.1.AND.ITLE.EQ.1) THEN
        GOTO 290
      ELSEIF(KK.GT.2) THEN
        DO 60 I=2,NHOR
          HORTHK(I)=HORTHK(I+KK-2)
   60   CONTINUE
        IF(YESMAC.GE.1) NHCMP=NHCMP-KK+2
        NHOR=NHOR-KK+2
        PRINT*,'           ====>  WARNING! <===='
        PRINT*,'USER DEFINED MORE THAN TWO HORIZONS IN THE TILL ZONE'
        WRITE(*,'(A18,I2,A20)') ' TOP ',KK-1,' HORIZONS THROWN OUT'
        PRINT*,' '
      ELSEIF(KK.EQ.1) THEN
        IF(DMEAN.GE.HORTHK(1)) GOTO 290
        IF(DMAX.LT.(0.667D0*HORTHK(1)).AND.NPRI.GE.1) THEN
          IC=2
          DMEAN=DSUM/ITLE
        ENDIF
        DO 70 I=NHOR,1,-1
          HORTHK(I+IC)=HORTHK(I)
   70   CONTINUE
        IF(YESMAC.GE.1) NHCMP=NHCMP+IC
        NHOR=NHOR+IC
      ELSE
C
C       ..NO ADJUSTMENT NECESSARY DEFAULTING TO 2ND HORIZON
        GOTO 290
      ENDIF
C
C     ..FIND NUMERICAL LAYER CLOSEST TO MEAN IMP DEPTH
      DMEAN=MAX(DMEAN,2.0D0)
      TMAX=9999.0D0
      DMIN=9999.0D0
      DO 80 I=1,NN
        TT=ABS(TLT(I)-DMEAN)
        IF(TT.LT.DMIN) THEN
          DMIN=TT
          KK=I
        ENDIF
        IF(ABS(TLT(I)-DMAX).LT.TMAX) THEN
          TMAX=ABS(TLT(I)-DMAX)
          KK2=I
        ENDIF
   80 CONTINUE
C
C     ..CREATE FIRST HORIZON
      IF(IC.EQ.1.AND.TLT(KK).LT.HORTHK(2)) THEN
        HORTHK(1)=TLT(KK)
      ELSEIF(IC.EQ.1.AND.TLT(KK).GE.HORTHK(2)) THEN
        IF(KK.NE.1) THEN
          HORTHK(1)=TLT(KK-1)
        ELSE
          DO 90 I=1,NHOR
            HORTHK(I)=HORTHK(I+IC)
   90     CONTINUE
          IF(YESMAC.GE.1) NHCMP=NHCMP-1
          NHOR=NHOR-1
          GOTO 290
        ENDIF
      ELSEIF(KK.EQ.KK2) THEN
        IF(TLT(KK+1).NE.HORTHK(3)) THEN
          HORTHK(1)=TLT(KK)
          HORTHK(2)=TLT(KK+1)
        ELSE
          HORTHK(1)=TLT(KK-1)
          HORTHK(2)=TLT(KK)
        ENDIF
      ELSE
        HORTHK(1)=TLT(KK)
        HORTHK(2)=TLT(KK2)
      ENDIF
C
C     ..ADJUST PARAMETERS TO REFLECT CHANGES IN HORIZON BOUNDRIES
      IH=0
      DO 150 I=1,NHORZ
        IF(ZHOR(I).LE.HORTHK(2)) THEN
          IH=IH+1
        ENDIF
C
        DO 100 J=1,8
          TMOVE(J)=SOILPP(J,I)
  100   CONTINUE
        IT=8
        DO 110 J=1,13
          TMOVE(J+IT)=SOILHP(J,I)
  110   CONTINUE
        IT=IT+13
        DO 120 J=1,5
          TMOVE(J+IT)=SOLTP1(I,J)
  120   CONTINUE
        IT=IT+5
        DO 140 J=1,3
          DO 130 K=1,MAXSCT
            IOFF=IT+(J-1)*MAXSCT+K
            TMOVE(IOFF)=SOLTP2(K,I,J)
  130     CONTINUE
  140   CONTINUE
        IT=IT+3*MAXSCT
        TMOVE(IT+1)=MICP(I)
        TMOVE(IT+2)=MCPOR(I)
        TMOVE(IT+3)=RP(I)
        TMOVE(IT+4)=WP(I)
        TMOVE(IT+5)=PL(I)
        TMOVE(IT+6)=AGSZ(I)
        TMOVE(IT+7)=FDEP(I)
        TMOVE(IT+8)=CLAT(I)
        TMOVEN(IT)=SOLTYP(I)
        IF(ZHOR(I).GE.HORTHK(2)) GOTO 160
  150 CONTINUE
C
  160 DO 270 I=NHOR,1,-1
C
        IF(I.LE.2) THEN
          DO 170 J=1,8
            SOILPP(J,I)=TMOVE(J)
  170     CONTINUE
          IT=8
          DO 180 J=1,13
            SOILHP(J,I)=TMOVE(J+IT)
  180     CONTINUE
          IT=IT+13
          DO 190 J=1,5
            SOLTP1(I,J)=TMOVE(J+IT)
  190     CONTINUE
          IT=IT+5
          DO 210 J=1,3
            DO 200 K=1,MAXSCT
              IOFF=IT+(J-1)*MAXSCT+K
              SOLTP2(K,I,J)=TMOVE(IOFF)
  200       CONTINUE
  210     CONTINUE
          IT=IT+3*MAXSCT
          MICP(I)=TMOVE(IT+1)
          MCPOR(I)=TMOVE(IT+2)
          RP(I)=TMOVE(IT+3)
          WP(I)=TMOVE(IT+4)
          PL(I)=TMOVE(IT+5)
          AGSZ(I)=TMOVE(IT+6)
          FDEP(I)=TMOVE(IT+7)
          CLAT(I)=TMOVE(IT+8)
          SOLTYP(I)=TMOVEN(IT)
        ELSE
          IT=IH-2
          DO 220 J=1,8
            SOILPP(J,I)=SOILPP(J,I+IT)
  220     CONTINUE
          DO 230 J=1,13
            SOILHP(J,I)=SOILHP(J,I+IT)
  230     CONTINUE
          DO 240 J=1,5
            SOLTP1(I,J)=SOLTP1(I+IT,J)
  240     CONTINUE
          DO 260 J=1,3
            DO 250 K=1,MAXSCT
              SOLTP2(K,I,J)=SOLTP2(K,I+IT,J)
  250       CONTINUE
  260     CONTINUE
          IT=MAX(IT+I,1)
          MICP(I)=MICP(IT)
          MCPOR(I)=MCPOR(IT)
          RP(I)=RP(IT)
          WP(I)=WP(IT)
          PL(I)=PL(IT)
          AGSZ(I)=AGSZ(IT)
          FDEP(I)=FDEP(IT)
          CLAT(I)=CLAT(IT)
          SOLTYP(I)=SOLTYP(IT)
        ENDIF
  270 CONTINUE
C
C     ... INITIALIZE SOIL HYDRAULIC PARAMETERS WITH NEW HORIZONS
      DO 280 J=1,NHOR
        PCLAY=SOILPP(7,J)*100.0D0
        PSILT=SOILPP(6,J)*100.0D0
        PSAND=SOILPP(5,J)*100.0D0
        TBD=SOILPP(3,J)
        BDHR(J)=SOILPP(3,J)
        ITYPE=INT(SOILPP(1,J))
        IF(ITYPE.NE.0) ITYPE=-1
        CALL SOILPR(J,SOILHP,TBD,PCLAY,PSAND,PSILT,ITYPE,SOILPP(2,J),1,
     +              HWP)
        SOILPP(4,J)=SOILHP(6,J)
  280 CONTINUE
C
C     ==============================
C     DO SOME INITIALIZATION
C     ==============================
C
  290 NHZ=NHOR
      DO 300 I=1,NHOR
        NSL(I)=INT(HORTHK(I))
        DHB(I)=HORTHK(I)
  300 CONTINUE
C
C     ...SET UP THE INDEX VECTORS FOR INTERPOLATION PURPOSES
      CALL INDEXR(HORTHK,TLT,DSL,NHOR,NN,NSLT)
C
      DO 310 I=1,2
        OBD(I)=SOILPP(3,I)
        OFDP(I)=FDEP(I)
  310 CONTINUE
      IF(OCRUST.EQ.0.0D0) OCRUST=SOILHP(4,1)/5.0D0
C
C     ..MAKE SURE THE WC AND WCH ARE INITIALIZED PROPERLY, USE -1
      DO 320 I=1,NN
C
C       ...RELATE NUMERICAL LAYER TO A SOIL HORIZON
        JH=NDXN2H(I)
C
C       ...CALC HYDRAULIC PROPERTIES
C       .. USE A -1 FLAG TO CAUSE RE-INITIALIZATION OF THE LOCALLY STORED
C       CURVE VALUES FOR THE TILLAGE ADJUSTMENTS
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     jak
C calculate pori taking into account of ice by liwang ma
        pori(i) = soilhp(6,jh)*aef
        H(I)=MAX(H(I),HMIN)
        THETA(I)=WC(H(I),SOILHP(1,JH),JH,-1)
        T9=SOILHP(6,JH)*AEF
        THETA(I)=MIN(THETA(I),T9)
        H(I)=WCH(H(I),THETA(I),SOILHP(1,JH),JH,-1)
        HK=POINTK(H(I),SOILHP(1,JH),JH,pori(i))
  320 CONTINUE
C
C
C
C999  WRITE(73,*)' '
C     WRITE(73,*)' '
C     WRITE(73,*)'AT END IN TILADJ'
C     DO 76 I=1,NHOR
C     WRITE(73,*)' '
C     WRITE(73,'(A2,I3,A8,F5.0)')'I=',I,'HORTHK=',HORTHK(I)
C     WRITE(73,'(A7)') 'SOILPP:'
C     WRITE(73,101) (SOILPP(J,I), J =1 ,8)
C     WRITE(73,'(A7)') 'SOILHP:'
C     WRITE(73,101) (SOILHP(J,I), J =1 ,13)
C     WRITE(73,'(A7)') 'SOLTP1:'
C     WRITE(73,101) (SOLTP1(I,J), J =1 ,5)
C     WRITE(73,'(A7)') 'SOLTP2:'
C     DO 78 K = 1,MAXSCT
C     78     WRITE(73,101)    (SOLTP2(K,I,J),  J =1 ,3)
C     WRITE(73,'(A20,I3)') 'MICP AND MACP PROPS:',NHCMP
C     WRITE(73,101)MICP(I),MCPOR(I),RP(I),WP(I),PL(I),AGSZ(I),FDEP(I)
C     76   CONTINUE
C
      IF(NSC.GE.1) CALL HZPARO
C
      RETURN
C 1000 FORMAT(1X,13(G9.3,1X))
      END
C
      SUBROUTINE NITBAL(NN,BD,THETA,XNU,JDAY,TL,RPOOL,CC,TLPLNT,TSPLNT,
     +    IMAGIC,MDAY,ANHYD,IYYY,OMSEA,NYR,TN,JNUM,FIXN,TSOTN0yr,
     +    tsotn00,tsotnday,tapmun,erodedmass,tsotn00_in,TNSORB)
C
C======================================================================
C
C       PURPOSE:  TOTAL NITROGEN MASS BALANCE
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ANHYD  L  ANHYDROUS-NH3 POOL, USED TO STORE NH4 DURING THE
C                 APPLICATION DELAY PERIOD [MG/L]
C       BAL        L
C       BD         I  BULK DENSITY FOR CURRENT HORIZON [G/CM^3]
C       CC         I  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C                 CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C                 1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C                 5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C                 9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C                 12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CN         I  9-LONG VECTOR OF OM & BM C:N RATIOS (IN SAME ORDER
C                 AS IN C VECTOR)
C       CONV1  L
C       CONV2  L
C       FACT   L
C       I      L  INDEX VARIABLE
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       JDAY   I  JULIAN DAY    [1..366]
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       RPOOL  I  RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       RPOP   I  3-LONG VECTOR OF BM POPULATION UNIT CONVERSION
C                 CTORS: *RPOP(I): [UG-C/G-SOIL]==>[# ORGS/G-SOIL]
C       START  L
C       TAFERT     I
C       TAIRR  I  NITROGEN IN IRRIGATION WATER
C       TARAIN     I  NITROGEN IN RAIN WATER
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TLDEN  I  LOSS OF NITROGEN DUE TO DENITRIFICATION [KG-N/HA]
C       TLPLNT     I  TOTAL DAILY UPTAKE OF NITROGEN [KG-N/HA]
C       TLRO   I  LOSS OF NITROGEN DUE TO RUNOFF [KG-N/HA]
C       TLSEP  I  LOSS OF NITROGEN DUE TO SEEPAGE [KG-N/HA]
C       TLDRN I/O LOSS OF NITROGEN DUE TO TILE DRAINAGE [KG-N/HA]
C       TLLAT I/O LOSS OF NITROGEN DUE TO LATERAL FLOW [KG-N/HA]
C       TLVOL  I  LOSS OF NITROGEN DUE TO VOLATILIZATION [KG-N/HA]
C       TOTA  I/O TOTAL ADDITION OF NITROGEN
C       TOTF   L
C       TOTI   L
C       TOTL  I/O TOTAL LOSSES OF NITROGEN [KG-N/HA]
C       TOTO   L
C       TOTP   L
C       TOTR   L
C       TSBP   L
C       TSBP1  L
C       TSBP2  L
C       TSBP3  L
C       TSHP   L
C       TSHP1  L
C       TSHP2  L
C       TSHP3  L
C       TSNH4  L
C       TSNO3  L
C       TSOTN I/O TOTAL STORED NITROGEN [KG-N/HA]
C       TSOTN0    I/O TOTAL NITROGEN AT START OF DAY [KG-N/HA]
C       TSPLNT     I
C       TSRP   L
C       TSRP1  L
C       TSRP2  L
C       TSUR   L
C       XNU       I/O
C
C
C       COMMENTS:   CARBCV IS THE CONVERSION FACTOR FOR CARBON TO OM
C
C       EXTERNAL REFERENCES:
C                 SGATE
C
C       CALLED FROM: MAIN
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION: 2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXCHEM=15,MXTN=39,MXANA=135)
C
      COMMON /NUTPAR/ R14,R23,R34,R43,R45,R53,R15,R25,EFFMAX,EFFNIT,
     +    EFFDEN,DENC,RPOP(3),ADCAY,ANIT,ADENIT,AUREA,ADEATH(3),CN(9),
     +    ADCAYV(5),XKP(3),AMETH,FRN2O_NIT,FRN2O_DEN,RNO_N2O,RAINNO,
     +    O2LIM,EFFMETH,ISBUG
C
      COMMON /NINFO/ TSOTN0,TSOTN,TOTA,TOTL,TAMIN,TANIT,TADRT(2),
     +    TARAIN(2),TAIRR(3),TAFERT(3),TARES(3),TLIMM,TLDEN,TLVOL,
     +    TLRO(3),TLSEP(3),TLDRN(3),TLLAT(3),TAMANR,TMANH4,tamanr_bal
     +    ,TLN2O,TLNXO,TLNITN2O,TLNITNXO,TLGHGADS,TLGHGADSden
C
C
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
C
C-MA     Liwang Ma
      COMMON/NITROGEN_C/TSNO3_C,TSNH4_C,TSNH4P_C,TSUR_C,TOTO_C,TOTP_C,
     +       TOTF_C,TSHP_C,RZSOILC(mxnod),RZSOILN(mxnod),
     +       totminr,totimm,totden,totvol,TOTFERT,TOTMANU
	common/rootdepth/ IR
      common/maxbal/IDmxH2O,IMmxH2O,IYYYmxH2O,IDmxN,IMmxN,IYYYmxN,
     +              IDmxC,IMmxC,IYYYmxC,IDmxP,IMmxP,IYYYmxP,
     +              DTMASSmxH2O,BalmxN,BalmxC,BalmxP,IWCINIT1
     +            ,ireset1
C-MA   Liwang Ma
      LOGICAL START
      DIMENSION XNU(MXNOD,25),BD(NN),THETA(NN),TL(NN),RPOOL(MXNOD,2),
     +    CC(MXNOD,MXCHEM),ANHYD(NN),TN(MXTN),OMSEA(MXANA),erodedmass(6)
C
      SAVE START
      DATA START /.TRUE./
C
      TSRP=0.0D0
      totminr=0.0d0 !RM move this through TOTNITNX0 to be initialized here.
      totimm=0.0d0 ! Common block can't be init in a data statement. 
      totden=0.0d0
      totvol=0.0d0
      TOTFERT=0.0d0
      TOTMANU=0.0d0
      TOTNITN2O=0.0d0
      TOTNITNXO=0.0d0
      TSRP1=0.0D0
      TSRP2=0.0D0
      TSHP=0.0D0
      TSHP1=0.0D0
      TSHP2=0.0D0
      TSHP3=0.0D0
      TSBP=0.0D0
      TSBP1=0.0D0
      TSBP2=0.0D0
      TSBP3=0.0D0
      TSUR=0.0D0
      TSNO3=0.0D0
      TSNH4=0.0D0
      TSNH4P=0.0D0
c Liwang Ma, June 24, 2005
      PSUR=0.0D0
      PSNO3=0.0D0
      PSNH4=0.0D0
C
C     ---CYCLE THROUGH ALL THE SOIL LAYERS
      DO 10 I=1,NN
        CONV1=TL(I)*BD(I)*0.1D0
        CONV2=TL(I)*THETA(I)*0.1D0
C
C       ..GET RESIDUE MASS... CONVERT FROM G-C/CM^2 ==> UG-C/G-SOIL
        FACT=1.0D6/(TL(I)*BD(I))
        XNU(I,1)=RPOOL(I,1)*FACT
        XNU(I,2)=RPOOL(I,2)*FACT
C
C       ..SUM UP RESIDUE NITROGEN...  CONVERT FROM UG-N/G-SOIL ==> KG-N/HA
        TSRP=TSRP+(XNU(I,1)/CN(1)+XNU(I,2)/CN(2))*CONV1
        TSRP1=TSRP1+(XNU(I,1)/CN(1))*CONV1
        TSRP2=TSRP2+(XNU(I,2)/CN(2))*CONV1
C
C       ..SUM UP HUMUS NITROGEN... CONVERT FROM UG-N/G-SOIL ==> KG-N/HA
        TSHP=TSHP+(XNU(I,3)/CN(3)+XNU(I,4)/CN(4)+XNU(I,5)/CN(5))*CONV1
        TSHP1=TSHP1+(XNU(I,3)/CN(3))*CONV1
        TSHP2=TSHP2+(XNU(I,4)/CN(4))*CONV1
        TSHP3=TSHP3+(XNU(I,5)/CN(5))*CONV1
CZ-MA Liwang Ma

        RZSOILC(I)=(XNU(I,3)+XNU(I,4)+XNU(I,5))*1.0D-4   !% OC
        RZSOILN(I)=(XNU(I,3)/CN(3)+XNU(I,4)/CN(4)+XNU(I,5)/CN(5))*1.0D-4  !%ON
CZ-MA Liwang Ma
C
C       ..SUM UP BUG NITROGEN...  CONVERT FROM UG-N/G-SOIL ==> KG-N/HA
        TSBP1=TSBP1+(XNU(I,7)/RPOP(1)/CN(7))*CONV1
        TSBP2=TSBP2+(XNU(I,8)/RPOP(2)/CN(8))*CONV1
        TSBP3=TSBP3+(XNU(I,9)/RPOP(3)/CN(9))*CONV1
        TSBP=TSBP+(XNU(I,7)/RPOP(1)/CN(7)+XNU(I,8)/RPOP(2)/CN(8)+
     +      XNU(I,9)/RPOP(3)/CN(9))*CONV1
C       ..SUM UP NO3-N...  CONVERT FROM MG-N/L-P.W. ==> KG-N/HA
        TSNO3=TSNO3+CC(I,9)*CONV2
        if (i.le.ir) PSNO3=PSNO3+CC(I,9)*CONV2
C
C       ..SUM UP NH4-N...  CONVERT FROM MG-N/L-P.W. ==> KG-N/HA
        TSNH4=TSNH4+CC(I,10)*CONV2
        if (i.le.ir) PSNH4=PSNH4+CC(I,10)*CONV2
C
C       ..SUM UP NH4-N IN ANHYDROUS POOL... KG-N/HA
        TSNH4P=TSNH4P+ANHYD(I)
C
C       ..SUM UP UREA-N...  CONVERT FROM MG-N/L-P.W. ==> KG-N/HA
        IF(CC(I,12).LT.1.0D-15) CC(I,12)=0.0D0
        TSUR=TSUR+CC(I,12)*CONV2
        if (i.le.ir) PSUR=PSUR+CC(I,12)*CONV2
C
   10 CONTINUE
      IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
        WRITE(73,'(I6,18F15.6)') MDAY,TSRP1,TSRP2,TSHP1,TSHP2,TSHP3,
     +      TSBP1,TSBP2,TSBP3,TSNO3,TSNH4,TLDEN,TLVOL,TLRO(1),TLSEP(1),
     +      TLPLNT,TAMIN,TLDRN(1),TLLAT(1)
      ENDIF
C
C     ..SUM UP TOTAL MASS IN STORAGE
      TSOTN=TSPLNT+TSRP+TSHP+TSBP+TSNO3+TSNH4+TSNH4P+TSUR
      TSOTN_IN=TSNO3+TSNH4+TSNH4P+TSUR
      IF(START) THEN
C
C       ..SAVE FOR LATER CALCULATIONS IF FIRST TIME THROUGH
        TSOTN0=TSOTN
        TSOTN0_IN=TSOTN_IN
        TSOTN00=TSOTN   !record initial soil N Liwang Ma, 1-22-2009
        TSOTN00_IN=TSOTN_IN   !record initial soil N Liwang Ma, 1-22-2009
        TSOTN0yr=TSOTN   !record initial soil N Liwang Ma, 1-22-2009
        START=.FALSE.
        NYR=IYYY
        JNUM=0
        TOTR=0.0d0
        TOTI=0.0d0
        TOTF=0.0d0
        TOTD=0.0d0
        TOTT=0.0d0
        TOTA=0.0d0
        TOTO=0.0d0
        TOTSEP=0.0D0
        TOTDRN=0.0D0
        TOTLAT=0.0D0
        TOTL=0.0d0
        BAL=0.0d0
      ELSE
C
        if (iwcinit1.eq.1) then
          dtadjN=tsotn0-tsotn00
       IF (tsotn0.NE.tsotn00) THEN
        CALL CDATE(JDAY,ID,IM,IYYY)
        write(unit=666,fmt=673) JDAY,ID,IM,IYYY,tsotn0_in-tsotn00_in
        ENDIF
          tsotn0=tsotn00
        else
          dtadjN=0.0d0
c          iwcinit1=0   !reset at C balance
        endif
C       ..SUM UP ALL THE GAINS
        TOTR=TARAIN(1)+TARAIN(2)
        TOTI=TAIRR(1)+TAIRR(2)+TAIRR(3)
        TOTF=TAFERT(1)+TAFERT(2)+TAFERT(3)
        TOTD=TARES(1)+TARES(2)+TARES(3)
        TOTT=TADRT(1)+TADRT(2)
        TOTA=TOTR+TOTI+TOTF+TOTD+TOTT+TAMANR+FIXN
C
C       ..SUM UP ALL THE LOSSES
        TOTO=TLRO(1)+TLRO(2)+TLRO(3)

c        TOTSEP=TLSEP(1)
        TOTSEP=TLSEP(1)+TLSEP(2)+TLSEP(3)  !Liwang Ma, leaching of NH4 should not be there. need further testing
        TOTDRN=TLDRN(1)+TLDRN(2)+TLDRN(3)
        TOTLAT=TLLAT(1)+TLLAT(2)+TLLAT(3)
        TLEROD=ERODEDMASS(4)+ERODEDMASS(5)
        TOTL=TOTO+TOTSEP+TOTDRN+TOTLAT+TLDEN+TLVOL+TLPLNT+TLEROD
     &      +TLNITN2O+TLNITNXO+TLGHGADS+TNSORB 
C
C       ..CALC THE BALANCE
        BAL=TSOTN+TOTL-TOTA-TSOTN0
c        BALN0=TSOTN-TSOTN00
c        IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
C
          totminr = totminr + tamin
          totimm  = totimm  + tlimm
          totden  = totden  + tlden
          totNITN2  = totNITN2O  + TLNITN2O
          totNITNXO  = totNITNXO  + TLNITNXO
          totvol  = totvol  + tlvol
          TOTFERT = TOTFERT + TOTF + TMANH4  ! ALL INORANGIC N (FERTILIZER AND MANURE)
          TOTMANU = TOTMANU + TAMANR - TMANH4  ! EXCLUE NH4 IN MANURE, ORGANIC N ONLY
C         ..SUM UP FOR SUMMARY CALCULATIONS
          JNUM=JNUM+1
          TN(1)=TN(1)+TOTR
          TN(2)=TN(2)+TOTI
          TN(3)=TN(3)+TOTF
          TN(4)=TN(4)+TAMANR
          TN(5)=TN(5)+TOTD
          TN(6)=TN(6)+TOTT
          TN(7)=TN(7)+TLDEN
          TN(8)=TN(8)+TLVOL
          TN(9)=TN(9)+TOTO
          TN(10)=TN(10)+TOTSEP
          TN(11)=TN(11)+TLPLNT
          TN(12)=TN(12)+TSRP1
          TN(13)=TN(13)+TSRP2
          TN(14)=TN(14)+TSHP1
          TN(15)=TN(15)+TSHP2
          TN(16)=TN(16)+TSHP3
          TN(17)=TN(17)+TSBP1
          TN(18)=TN(18)+TSBP2
          TN(19)=TN(19)+TSBP3
          TN(20)=TN(20)+TSNO3
          TN(21)=TN(21)+TSNH4
          TN(22)=TN(22)+TSNH4P
          TN(23)=TN(23)+TSUR
          TN(24)=TN(24)+TAMIN
          TN(25)=TN(25)+FIXN
          TN(26)=TN(26)+TOTDRN
          TN(27)=TN(27)+TOTLAT
          TN(28)=TN(28)+TLIMM
          TN(29)=TN(29)+tamanr_bal   !FIXN
          TN(30)=TN(30)+TMANH4
          tn(31)=tn(31)+dtadjN
          TN(32)=TN(32)+TLN2O
          TN(33)=TN(33)+TLNXO
          TN(34)=TN(34)+TLEROD
          TN(35)=TN(35)+TLNITN2O
          TN(36)=TN(36)+TLNITNXO
          TN(37)=TN(37)+TLGHGADS
          TN(38)=TN(38)+TLGHGADSden
          TN(39)=TN(39)+TNSORB
          IF(NYR.NE.IYYY) then
            tapmun=tn(29)
		  CALL NITBYR(NYR,IYYY,JNUM,TN,0,TSOTN0yr,TSOTN)
          endif
C
          CALL CDATE(JDAY,ID,IM,IYYY)
          WRITE(74,1000)ID,IM,IYYY,JDAY,TSOTN0,TOTR,TOTI,TOTF,TAMANR,
     +        TOTD,TOTT,FIXN,TOTA
         WRITE(74,1100)TLDEN,TLVOL,TOTO,TLEROD,TLNITN2O+TLNITNXO,TOTSEP,
     +                 TOTDRN,TOTLAT,TLPLNT,TNSORB,TOTL,TAMIN,TLIMM
          WRITE(74,1200)TSPLNT,TSRP,TSRP1,TSRP2,TSHP,TSHP1,TSHP2,TSHP3,
     +        TSBP,TSBP1,TSBP2,TSBP3,TSNO3,TSNH4,TSNH4P,TSUR,TSOTN,BAL
C
c          OMSEA(80) = TSHP  ! TOTAL SOIL ORGANIC N (HUMUS POOLS)
	  if (abs(bal).ge.abs(balmxN)) then
            balmxN=bal
            IDmxN=id
		  IMmxN=im
		  IYYYmxN=iyyy
        endif
          if (abs(bal).gt.1.0d-1) THEN !changed by Liwang Ma
          CALL dayjack(0,0,0,3)!send message that N mass balance was bad
          WRITE (666,FMT=667) ID, IM, IYYY, jday,BAL !Liwang Ma
          if (abs(bal).gt.50.0d0) print*, 'SEVERE MASS BALANCE PROBLEM'
          ENDIF
667   FORMAT (I4,I4,I6,' (DOY ',i3,')',G15.6,
     +' ======>N BALANCE PROBLEM')
c        ENDIF
        TSOTN0=TSOTN
        TSOTN0_in=TSOTN_in
        tsotnday=tsotn
      ENDIF
CZ-MA  Liwang Ma
      TSNO3_C = TSNO3
      TSNH4_C = TSNH4
      TSNH4P_C = TSNH4P
      TSUR_C = TSUR
      TOTO_C = TOTO
      TOTP_C = TN(10)
      TOTF_C = TOTF + TOTR + TOTI
c     TSHP_C = TSHP
      TSHP_C = TSHP1 + TSHP2 + TSHP3 + TSRP1 + TSRP2
CZ-MA   Liwang Ma
      OMSEA(99)=TSNO3+TSNH4+TSUR
      OMSEA(14)=PSNO3+PSNH4+PSUR
      OMSEA(15)=TOTF+TMANH4   !LIWANG MA, 11-25-2014 FOR N MASSBALANCE
      OMSEA(16)=TAMANR_bal    !Liwang Ma, 2-20-2009
      OMSEA(17)=TOTI
      OMSEA(18)=TOTR
      OMSEA(19)=TOTT
      OMSEA(20)=TOTD
      OMSEA(21)=TLPLNT
      OMSEA(26)=TLDEN
      OMSEA(27)=TLVOL
      OMSEA(28)=TAMIN
      OMSEA(29)=TOTO
      OMSEA(32)=TSRP1
      OMSEA(33)=TSRP2
      OMSEA(34)=TSHP1
      OMSEA(35)=TSHP2
      OMSEA(36)=TSHP3
      OMSEA(37)=TSBP1
      OMSEA(38)=TSBP2
      OMSEA(39)=TSBP3
      OMSEA(77)=TLIMM
      OMSEA(97)=TLN2O
      OMSEA(98)=TLNXO
      OMSEA(101)=TLNITN2O
      OMSEA(102)=TLNITNXO
      OMSEA(103)=TLGHGADS
      OMSEA(104)=TLGHGADSden
      OMSEA(135)=TNSORB
      CALL SGATE(DBLE(MDAY),15,TSNO3)
      CALL SGATE(DBLE(MDAY),74,TSNO3+TSNH4+TSUR)
      CALL SGATE(DBLE(MDAY),86,TLRO(1))
      CALL SGATE(DBLE(MDAY),87,TLRO(2))
C
      RETURN
 1000 FORMAT(//,' ---',I3,'/',I2,'/',I4,' ---',I4,' ---',
     +    ' TOTAL NITROGEN MASS BALANCE (KG/HA) ',/
     +    ' TOTAL (N) AT START',T60,G15.6,/' ADDITIONS:',/T4,
     +    'BACKGROUND RAIN WATER',T50,G15.6,/T4,
     +    'BACKGROUND IRRIGATION WATER',T50,G15.6,/T4,'FERTILIZER APP',
     +    T50,G15.6,/T4,'MANURE APP',T50,G15.6,/T4,
     +    'FROM INCORPORATED RESIDUE',T50,G15.6,/T4,'FROM DEAD ROOTS',
     +    T50,G15.6,/T4,'NITROGEN FIXATION',T50,G15.6,/T4,
     +    'TOTAL ADDITIONS',T60,G15.6)
 1100 FORMAT(/' LOSSES:',/T4,'DENITRIFICATION',T50,G15.6,/T4,
     +    'VOLATILIZATION',T50,G15.6,/T4,'RUNOFF',T50,G15.6,/T4,
     +    'ERODED SOIL',T50,G15.6,/T4,
     +    'N2O+NxO LOSS FROM NITRIFICATION',T50,G15.6,/T4,
     +    'SEEPAGE',T50,G15.6,/T4,'TILE DRAINAGE',T50,G15.6,/T4,
     +    'LATERAL FLOW',T50,G15.6,/T4,
     +    'PLANT UPTAKE',T50,G15.6,/T4,
     +    'SORPTION ONTO ION EXCHANGE SITES',T50,G15.6,/T4,
     +    'TOTAL LOSSES',T60,G15.6/,
     +  /' TRANSFORMATIONS:',
     +  /T4,'DAILY MINERALIZATION',T50,G15.6,
     +  /T4,'DAILY IMMOBILIZATION',T50,G15.6)
 1200 FORMAT(/' STORAGE:',/T4,'WHOLE PLANT NITROGEN',T50,G15.6,/T4,
     +    'RESIDUE POOL',T50,G15.6,/T8,'RESIDUE POOL 1',T40,G15.6,/T8,
     +    'RESIDUE POOL 2',T40,G15.6,/T4,'SOIL HUMUS POOL',T50,G15.6,/
     +    T8,'SOIL HUMUS POOL 1',T40,G15.6,/T8,'SOIL HUMUS POOL 2',T40,
     +    G15.6,/T8,'SOIL HUMUS POOL 3',T40,G15.6,/T4,
     +    'MICRO ORGANISM POOL',T50,G15.6,/T8,'MICRO ORGANISM POOL 1',
     +    T40,G15.6,/T8,'MICRO ORGANISM POOL 2',T40,G15.6,/T8,
     +    'MICRO ORGANISM POOL 3',T40,G15.6,/T4,'NO3-N ',T50,G15.6,/T4,
     +    'NH4-N',T50,G15.6,/T4,'NH4-N (ANHYDROUS POOL)',T50,G15.6,/T4,
     +    'UREA-N',T50,G15.6,/' TOTAL (N) AT END',T60,G15.6,/T60,15('-'
     +    ),/' MASS BALANCE AT END OF DAY:',T60,G15.6)
  673 FORMAT(1X,' SOIL INORGANIC N GAIN DUE TO RESETTING ON DAY:',I6,2X,
     +    '==>',I3,'/',I2,'/',I4,5X,F15.4/)
      END
C
      SUBROUTINE CARBAL(NN,BD,THETA,XNU,JDAY,TL,RPOOL,CC,TLPLNT,TSPLNT,
     +    IMAGIC,MDAY,IYYY,OMSEA,NYRC,TNC,JNUMC,RM,TSOTCDAY,TSOTC0YR,
     +    TSOTC00,erodedmass)
C
C======================================================================
C
C       PURPOSE:  TOTAL ORGANIC CARBON MASS BALANCE
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       ANHYD  L  ANHYDROUS-NH3 POOL, USED TO STORE NH4 DURING THE
C                 APPLICATION DELAY PERIOD [MG/L]
C       BAL        L
C       BD         I  BULK DENSITY FOR CURRENT HORIZON [G/CM^3]
C       CC         I  TRANSPORT MATRIX FOR MOBILE CHEMICALS [MG/L]
C                 CONTAINS ONLY THE SOLUBLE FORM OF CHEMICAL:
C                 1-HYDROGEN, 2-CALCIUM, 3-SODIUM, 4-MAGNISIUM,
C                 5-CLORINE, 6-BICARBONATE, 7-SULFATE, 8-ALUMINUM,
C                 9-NITRATE-N, 10-AMMONIUM-N, 11-CARBONATE,
C                 12-UREA-N, 13-PEST #1, 14-PEST #2, 15-PEST #3
C       CN         I  9-LONG VECTOR OF OM & BM C:N RATIOS (IN SAME ORDER
C                 AS IN C VECTOR)
C       CONV1  L
C       CONV2  L
C       FACT   L
C       I      L  INDEX VARIABLE
C       IMAGIC     I  INDICATOR OF DEBUG OUTPUT
C       JDAY   I  JULIAN DAY    [1..366]
C       MAXBP  P  MAXIMUM NUMBER OF BREAK POINTS IN A RAINSTORM
C       MAXHOR     P  MAXIMUM NUMBER OF SOIL HORIZONS
C       MAXSCT     P  MAX NUMBER OF SOIL CONSTITUENTS PER HORIZON
C       MXAPP  P  MAXIMUM NUMBER OF MANAGEMENT APPLICATIONS
C       MXCHEM     P  MAXIMUM NUMBER OF CHEMICALS SIMULATED
C       MXNOD  P  MAX NUMBER OF NUMERICAL NODES
C       MXNODT     P
C       MXPEST     P  MAXIMUM NUMBER OF PESTICIDES
C       MXSPEC     P
C       NN         I  NUMBER INTERIOR NODES IN RICHARD'S EQN SOLUTION
C       RPOOL  I  RESIDUE POOL, BOTH FAST AND SLOW DECOMPOSERS
C                 [0]--SURFACE RESIDUE,  [1] SLOW DECOMP RESIDUE
C                 [1..NN]--MIXED SOIL RES  [2] FAST DECOMP RESIDUE
C                 UNITS = [G-CARBON / CM^2-SOIL]
C       RPOP   I  3-LONG VECTOR OF BM POPULATION UNIT CONVERSION
C                 CTORS: *RPOP(I): [UG-C/G-SOIL]==>[# ORGS/G-SOIL]
C       START  L
C       TAFERT     I
C       TAIRR  I  NITROGEN IN IRRIGATION WATER
C       TARAIN     I  NITROGEN IN RAIN WATER
C       THETA  I  VOLUMETRIC WATER CONTENT [CM^3/CM^3]
C       TL         I  NUMERICAL LAYER THICKNESSES [CM]
C       TLDEN  I  LOSS OF NITROGEN DUE TO DENITRIFICATION [KG-N/HA]
C       TLPLNT     I  TOTAL DAILY UPTAKE OF NITROGEN [KG-N/HA]
C       TLRO   I  LOSS OF NITROGEN DUE TO RUNOFF [KG-N/HA]
C       TLSEP  I  LOSS OF NITROGEN DUE TO SEEPAGE [KG-N/HA]
C       TLDRN I/O LOSS OF NITROGEN DUE TO TILE DRAINAGE [KG-N/HA]
C       TLLAT I/O LOSS OF NITROGEN DUE TO LATERAL FLOW [KG-N/HA]
C       TLVOL  I  LOSS OF NITROGEN DUE TO VOLATILIZATION [KG-N/HA]
C       TOTA  I/O TOTAL ADDITION OF NITROGEN
C       TOTF   L
C       TOTI   L
C       TOTL  I/O TOTAL LOSSES OF NITROGEN [KG-N/HA]
C       TOTO   L
C       TOTP   L
C       TOTR   L
C       TSBP   L
C       TSBP1  L
C       TSBP2  L
C       TSBP3  L
C       TSHP   L
C       TSHP1  L
C       TSHP2  L
C       TSHP3  L
C       TSNH4  L
C       TSNO3  L
C       TSOTC  I/O TOTAL STORED CARBON [KG-N/HA]
C       TSOTC0    I/O TOTAL CARBON AT START OF DAY [KG-N/HA]
C       TSPLNT     I
C       TSRP   L
C       TSRP1  L
C       TSRP2  L
C       TSUR   L
C       XNU       I/O
C
C
C       COMMENTS:   CARBCV IS THE CONVERSION FACTOR FOR CARBON TO OM
C
C       EXTERNAL REFERENCES:
C                 SGATE
C
C       CALLED FROM: MAIN
C
C       PROGRAMMER: LIWANG MA
C
C       VERSION: 2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXCHEM=15,MXTNC=27,MXANA=135,C2BM=2.5D0)
C
      COMMON /NUTPAR/ R14,R23,R34,R43,R45,R53,R15,R25,EFFMAX,EFFNIT,
     +    EFFDEN,DENC,RPOP(3),ADCAY,ANIT,ADENIT,AUREA,ADEATH(3),CN(9),
     +    ADCAYV(5),XKP(3),AMETH,FRN2O_NIT,FRN2O_DEN,RNO_N2O,RAINNO,
     +    O2LIM,EFFMETH,ISBUG
C
      COMMON /CINFO/TAMANC,TADRTC(2),TARESC(3),TOTCO2,TACUREA,YIELDC
     +             ,TAMANC_BAL,RCO2,TCAG,TCBG,TOTCH4,TOTCO2NI
C-MA     Liwang Ma
c      COMMON/NITROGEN_C/TSNO3_C,TSNH4_C,TSNH4P_C,TSUR_C,TOTO_C,TOTP_C,
c     +       TOTF_C,TSHP_C,RZSOILC(mxnod)
	common/rootdepth/ IR
      common/maxbal/IDmxH2O,IMmxH2O,IYYYmxH2O,IDmxN,IMmxN,IYYYmxN,
     +              IDmxC,IMmxC,IYYYmxC,IDmxP,IMmxP,IYYYmxP,
     +              DTMASSmxH2O,BalmxN,BalmxC,BalmxP,IWCINIT1
     +            ,ireset1
C-MA   Liwang Ma
      LOGICAL START
      DIMENSION XNU(MXNOD,25),BD(NN),THETA(NN),TL(NN),RPOOL(MXNOD,2),
     +    CC(MXNOD,MXCHEM),TNC(MXTNC),OMSEA(MXANA),erodedmass(6)
C
      SAVE START
      DATA START /.TRUE./
C
C LIWANG MA FOR C BALANCE
      TSRPC=0.0D0
      TSRPC1=0.0D0
      TSRPC2=0.0D0
      TSHPC=0.0D0
      TSHPC1=0.0D0
      TSHPC2=0.0D0
      TSHPC3=0.0D0
      TSBPC=0.0D0
      TSBPC1=0.0D0
      TSBPC2=0.0D0
      TSBPC3=0.0D0
C
C     ---CYCLE THROUGH ALL THE SOIL LAYERS
      DO 10 I=1,NN
        CONV1=TL(I)*BD(I)*0.1D0
        CONV2=TL(I)*THETA(I)*0.1D0
C
C       ..GET RESIDUE MASS... CONVERT FROM G-C/CM^2 ==> UG-C/G-SOIL
        FACT=1.0D6/(TL(I)*BD(I))
        XNU(I,1)=RPOOL(I,1)*FACT
        XNU(I,2)=RPOOL(I,2)*FACT
C
C       ..SUM UP RESIDUE CARBON...  CONVERT FROM UG-N/G-SOIL ==> KG-N/HA
C LIWANG MA FOR C BALANCE
        TSRPC=TSRPC+(XNU(I,1)+XNU(I,2))*CONV1
        TSRPC1=TSRPC1+(XNU(I,1))*CONV1
        TSRPC2=TSRPC2+(XNU(I,2))*CONV1
C
C       ..SUM UP HUMUS CARBON... CONVERT FROM UG-N/G-SOIL ==> KG-N/HA
C LIWANG MA FOR C BALANCE
        TSHPC=TSHPC+(XNU(I,3)+XNU(I,4)+XNU(I,5))*CONV1
        TSHPC1=TSHPC1+(XNU(I,3))*CONV1
        TSHPC2=TSHPC2+(XNU(I,4))*CONV1
        TSHPC3=TSHPC3+(XNU(I,5))*CONV1
CZ-MA Liwang Ma

C        RZSOILC(I)=(XNU(I,3)+XNU(I,4)+XNU(I,5))*1.0D-4
CZ-MA Liwang Ma
C
C       ..SUM UP BUG CARBON...  CONVERT FROM UG-N/G-SOIL ==> KG-N/HA
C LIWANG MA FOR C BALANCE
        TSBPC1=TSBPC1+(XNU(I,7)/RPOP(1))*CONV1
        TSBPC2=TSBPC2+(XNU(I,8)/RPOP(2))*CONV1
        TSBPC3=TSBPC3+(XNU(I,9)/RPOP(3))*CONV1
        TSBPC=TSBPC+(XNU(I,7)/RPOP(1)+XNU(I,8)/RPOP(2)+
     +      XNU(I,9)/RPOP(3))*CONV1
C
   10 CONTINUE
C
C     ..SUM UP TOTAL MASS IN STORAGE
      TSRESC = RM/2.5D0
c      TSOTC=TSRPC+TSHPC+TSBPC+TSRESC
      TSOTC=TSRPC+TSHPC+TSBPC
      IF(START) THEN
C
C       ..SAVE FOR LATER CALCULATIONS IF FIRST TIME THROUGH
        TSOTC0=TSOTC
        TSOTC00=TSOTC
        TSOTC0YR=TSOTC
        START=.FALSE.
        NYRC=IYYY
        JNUMC=0
        TOTR=0.0d0
        TOTI=0.0d0
        TOTF=0.0d0
        TOTD=0.0d0
        TOTT=0.0d0
        TOTA=0.0d0
        TOTO=0.0d0
        TOTSEP=0.0D0
        TOTDRN=0.0D0
        TOTLAT=0.0D0
        TOTL=0.0d0
        BAL=0.0d0
      ELSE
C
        if (iwcinit1.eq.1) then
          dtadjC=tsotc0-tsotc00
          tsotc0=tsotc00
c          iwcinit1=0
        else
          dtadjC=0.0d0
        endif
C       ..SUM UP ALL THE GAINS
        TOTDC=TARESC(1)+TARESC(2)+TARESC(3)
        TOTTC=TADRTC(1)+TADRTC(2)
        TOTA=TAMANC+TOTTC+TOTDC+TACUREA+TOTCO2NI
C
C       ..SUM UP ALL THE LOSSES
        TOTL=TOTCO2+TOTCH4+ERODEDMASS(6)
C
C       ..CALC THE BALANCE
        BAL=TSOTC+TOTL-TOTA-TSOTC0
C        BALC0=TSOTC-TSOTC00
c        IF(IMAGIC.LT.0.AND.IMAGIC.GT.-12) THEN
C
C         ..SUM UP FOR SUMMARY CALCULATIONS

          JNUMC=JNUMC+1
          TNC(1)=TNC(1)+TAMANC
          TNC(2)=TNC(2)+TADRTC(1)
          TNC(3)=TNC(3)+TADRTC(2)
          TNC(4)=TNC(4)+TARESC(1)
          TNC(5)=TNC(5)+TARESC(2)
          TNC(6)=TNC(6)+TARESC(3)
          TNC(7)=TNC(7)+TOTCO2
          TNC(8)=TNC(8)+TOTDC
          TNC(9)=TNC(9)+TOTTC
          TNC(10)=YIELDC
          TNC(11)=TNC(11)+TACUREA
C          TNC(11)=OMSEA(41)/2.5D0
          TNC(12)=TNC(12)+TSRESC
	    TNC(13)=TNC(13)+TSRPC1
	    TNC(14)=TNC(14)+TSRPC2
	    TNC(15)=TNC(15)+TSHPC1
	    TNC(16)=TNC(16)+TSHPC2
	    TNC(17)=TNC(17)+TSHPC3
	    TNC(18)=TNC(18)+TSBPC1
	    TNC(19)=TNC(19)+TSBPC2
	    TNC(20)=TNC(20)+TSBPC3
	    TNC(21)=TNC(21)+TAMANC_BAL
	    TNC(22)=TNC(22)+RCO2               !CO2 from surface residue
	    TNC(23)=(tcag+tcbg)/c2bm   !CO2 assimilation by plant
          TNC(24)=TNC(24)+dtadjC
          TNC(25)=TNC(25)+TOTCH4
          TNC(26)=TNC(26)+ERODEDMASS(6)
          TNC(27)=TNC(27)+TOTCO2NI
      IF(NYRC.NE.IYYY) THEN
         CALL CARBYR(NYRC,IYYY,JNUMC,TNC,0,TSOTC0YR,TSOTC)
c         RCO2=0.0D0
         YIELDC=0.0D0
      ENDIF
C
          CALL CDATE(JDAY,ID,IM,IYYY)
C LIWANG MA ADD CARBON BALANCE
         WRITE(75,1001)ID,IM,IYYY,JDAY,TSOTC0,TACUREA,TAMANC,
     +        TOTDC,TOTTC,TOTA
         WRITE(75,1101) ERODEDMASS(6),TOTCO2,TOTCH4
         WRITE(75,1201)TSRESC,TSRPC,TSRPC1,TSRPC2,TSHPC,TSHPC1,TSHPC2,
     +        TSHPC3,TSBPC,TSBPC1,TSBPC2,TSBPC3,TSOTC,BAL
C LIWANG MA ADD CARBON BALANCE
C
	  if (abs(bal).ge.abs(balmxC)) then
            balmxC=bal
            IDmxC=id
		  IMmxC=im
		  IYYYmxC=iyyy
        endif
          if (abs(bal).gt.1.0d-1) THEN !changed by Liwang Ma
            CALL dayjack(0,0,0,4)!send message that C mass balance was bad
          WRITE (666,FMT=667) ID, IM, IYYY, jday,BAL !Liwang Ma
          if (abs(bal).gt.50.0d0) print*, 'SEVERE MASS BALANCE PROBLEM'
          ENDIF
667     FORMAT(I4,I4,I6,' (DOY ',i3,')',G15.6,
     +' =========>C BALANCE PROBLEM')
c        ENDIF
        TSOTC0=TSOTC
        TSOTCday=TSOTC
      ENDIF
CZ-MA  Liwang Ma
C      TOTO_C = TOTO
C      TOTP_C = TNC(10)
C      TOTF_C = TOTF + TOTR + TOTI
      TOTCSOIL = TSHPC1 + TSHPC2 + TSHPC3 + TSRPC1 + TSRPC2 +
     +           TSBPC1 + TSBPC1 + TSBPC3
CZ-MA   Liwang Ma
      OMSEA(78)=TOTCO2+RCO2   ! DAILY NET CO2 RELEASE (in kg C/ha/day, Zhiming note)
      OMSEA(79)=TSHPC         ! TOTAL ORGANIC CARBON IN SOIL (HUMUS POOLS)
      OMSEA(80)=TSRPC         ! TOTAL ORGANIC CARBON IN SOIL (Residue POOLS)
      OMSEA(96)=TOTCH4        ! DAILY NET CH4 RELEASE
      RCO2=0.0D0              ! reset residue CO2 release for the day
C
      RETURN
 1001 FORMAT(//,' ---',I3,'/',I2,'/',I4,' ---',I4,' ---',
     +    ' TOTAL CARBON MASS BALANCE (KG/HA) ',/
     +    ' TOTAL (C) AT START',T60,G15.6,/' ADDITIONS:',/T4,
     +    'CARBON FROM UREA APPLICATION',T50,G15.6,/T4,
     +    'MANURE APPLICATION',T50,G15.6,/T4,
     +    'FROM INCORPORATED RESIDUE',T50,G15.6,/T4,'FROM DEAD ROOTS',
     +    T50,G15.6,/T4,
     +    'TOTAL ADDITIONS',T60,G15.6)
 1101 FORMAT(/' MINERALIZATION LOSSES:',/T4,
C     +    'RESIUDE POOL 1',T50,G15.6,/T4,
C     +    'RESIUDE POOL 2',T50,G15.6,/T4,
C     +    'SOIL HUMUS POOL 1',T50,G15.6,/T4,
C     +    'SOIL HUMUS POOL 2',T50,G15.6,/T4,
C     +    'SOIL HUMUS POOL 3',T50,G15.6,/T4,
     +    'ERODED SOIL',T60,G15.6,/T4,
     +    'TOTAL LOSSES AS CO2',T60,G15.6,/T4,
     +    'TOTAL LOSSES AS CH4',T60,G15.6)
 1201 FORMAT(/' STORAGE:',/T4,'SURFACE RESIDUE CARBON',T50,G15.6,/T4,
     +    'RESIDUE POOL',T50,G15.6,/T8,'RESIDUE POOL 1',T40,G15.6,/T8,
     +    'RESIDUE POOL 2',T40,G15.6,/T4,'SOIL HUMUS POOL',T50,G15.6,/
     +    T8,'SOIL HUMUS POOL 1',T40,G15.6,/T8,'SOIL HUMUS POOL 2',T40,
     +    G15.6,/T8,'SOIL HUMUS POOL 3',T40,G15.6,/T4,
     +    'MICRO ORGANISM POOL',T50,G15.6,/T8,'MICRO ORGANISM POOL 1',
     +    T40,G15.6,/T8,'MICRO ORGANISM POOL 2',T40,G15.6,/T8,
     +    'MICRO ORGANISM POOL 3',T40,G15.6,
     +    /' TOTAL (CARBON) AT END',T60,G15.6,/T60,15('-'
     +    ),/' MASS BALANCE AT END OF DAY:',T60,G15.6)
      END
C
      SUBROUTINE NITBYR(NYR,IYYY,JNUM,TN,IFLUSH,TSOTN0,TSOTN)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE PRINTS OUT A YEARLY SUMMARY OF THE NITROGEN
C             MASS BALANCE ROUTINE.  WE ONLY GO INTO THIS ROUTINE WHEN
C             THE CURRENT YEAR IS NOT EQUAL TO THE SAVED YEAR.
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       NYR        I  SAVED CURRENT YEAR
C       IYYY   I  CURRENT YEAR IN MODEL
C       JNUM   I  NUMBER OF DAYS TO AVERAGE OVER
C       TN         I  SAVED TOTALS
C       IFLUSH     I  FLAG: 0-PRINT YEARLY TOTALS
C                     1-PRINT SIMULATION TOTALS
C
C       CALLED FROM: NITBAL
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION: 3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXTN=39,MXTNA=12)
C
      DIMENSION TN(MXTN),TNA(MXTNA),TTN(MXTN)
      SAVE TTN,JJNUM
      DATA TTN /MXTN*0.0D0/,JJNUM /0/
C
C     ..DETERMINE IF THIS IS YEARLY OR SIMULATION TOTALS
      IF(IFLUSH.EQ.0) THEN
C
C       .. AVERAGE THE VALUES
        DO 10 I=1,MXTNA
          TNA(I)=TN(I+11)/DBLE(JNUM)
   10   CONTINUE
        DO 20 I=1,MXTN
          TTN(I)=TTN(I)+TN(I)
   20   CONTINUE
        JJNUM=JJNUM+JNUM
        WRITE(74,1000) IYYY-1
        WRITE(74,1200) (TN(I),I=1,3),TN(30),TN(4)-TN(30),TN(29),
     +         TN(5),tn(6),tn(25),(tn(i),I=7,9),TN(34),TN(35)+TN(36),
     +         TN(37),TN(10),TN(26),TN(27),TN(11),TN(39),TN(24),TN(28)
      WRITE(74,1300) (TNA(I),I=1,MXTNA)
        write (74,1400) TSOTN-TSOTN0+(tn(7)+tn(8)+tn(9)+tn(10)+TN(35)+
     +    TN(37)+TN(36)+tn(11)+tn(26)+tn(27)+TN(34)+tn(39))-(tn(1)+tn(2)
     +    +tn(3)+tn(4)+tn(5)+tn(6)+tn(25))+tn(31),TSOTN0,TSOTN
     +   ,(TN(I),I=1,6), TN(25),TN(7),TN(7)-(TN(32)-TN(35))-
     +      (TN(33)-TN(36))-TN(38),TN(38),
     +      TN(32)-TN(35),TN(33)-TN(36),TN(37),TN(35),
c        (TN(32)+TN(33)-TN(35)),
     +   TN(36),(TN(I),I=8,9),TN(34),TN(10),TN(11),TN(39),TN(26),TN(27)
      TSOTN0=TSOTN
      TSOTN0_IN=TSOTN_IN
      ELSE
        DO 30 I=1,MXTN
          TTN(I)=TTN(I)+TN(I)
   30   CONTINUE
        JJNUM=JJNUM+JNUM
        DO 40 I=1,MXTNA
          IF(JJNUM.NE.0) THEN
            TNA(I)=TTN(I+11)/DBLE(JJNUM)
          ELSE
            TNA(I)=0.0D0
          ENDIF
   40   CONTINUE
        WRITE(71,1100)
        WRITE(71,1200) (TTN(I),I=1,3),TTN(30),TTN(4)-TTN(30),TTN(29),
     +                TTN(5),ttn(6),ttn(25),(ttn(i),I=7,9),TTN(34),
     +   TTN(35)+TTN(36),TTN(37),TTN(10),TTN(26),TTN(27),TTN(11),
     +   TTN(39),TTN(24),TTN(28)
        WRITE(71,1300) (TNA(I),I=1,MXTNA)
        write (71,1400) TSOTN-TSOTN0+(ttn(7)+ttn(8)+ttn(9)+ttn(10)
     +   +TTN(37)+TTN(35)+TTN(36)+ttn(11)+ttn(26)+ttn(27)+ttN(34)+
     +   ttn(39))-(ttn(1)+ttn(2)+ttn(3)+ttn(4)+ttn(5)+ttn(6)+ttn(25))
     +   +ttn(31),TSOTN0,TSOTN
     +     ,(TTN(I),I=1,6), TTN(25),TTN(7),TTN(7)-(TTN(32)-TTN(35))-
     +      (TTN(33)-TTN(36))-TTN(38),TTN(38),
     +      TTN(32)-TTN(35),TTN(33)-TTN(36),TTN(37),TTN(35),
c     +      (TTN(32)+TTN(33)-TTN(35)),
     +    ttn(36),(TTN(I),I=8,9),TTN(34),TTN(10),TTN(11),TTN(39),
     +    TTN(26),TTN(27)
        WRITE(74,1100)
        WRITE(74,1200) (TTN(I),I=1,3),TTN(30),TTN(4)-TTN(30),TTN(29),
     +                TTN(5),ttn(6),ttn(25),(ttn(i),I=7,9),TTN(34),
     +   TTN(35)+TTN(36),TTN(37),TTN(10),TTN(26),TTN(27),TTN(11),
     +   TTN(39),TTN(24),TTN(28)
      WRITE(74,1300) (TNA(I),I=1,MXTNA)
        write (74,1400) TSOTN-TSOTN0+(ttn(7)+ttn(8)+ttn(9)+ttn(10)+
     +     TTN(37)+TTN(35)+TTN(36)+ttn(11)+ttn(26)+ttn(27)+TtN(34)+
     +     ttn(39))-(ttn(1)+ttn(2)+ttn(3)+ttn(4)+ttn(5)+ttn(6)
     +     +ttn(25))+ttn(31),TSOTN0,TSOTN
     +     ,(TTN(I),I=1,6), TTN(25),TTN(7),TTN(7)-(TTN(32)-TTN(35))-
     +      (TTN(33)-TTN(36))-TTN(38),TTN(38),
     +      TTN(32)-TTN(35),TTN(33)-TTN(36),TTN(37),TTN(35),
c     +       (TTN(32)+TTN(33)-TTN(35)),
     +    ttn(36),(TTN(I),I=8,9),TTN(34),TTN(10),TTN(11),TTN(39),
     +    TTN(26),TTN(27)
        write (666,1500) TSOTN-TSOTN0+(ttn(7)+ttn(8)+ttn(9)+ttn(10)+
     +   TTN(37)+TTN(35)+TTN(36)+ttn(11)+ttn(26)+ttn(27)+ttN(34)
     +   +ttn(39))-(ttn(1)+ttn(2)+ttn(3)+ttn(4)+ttn(5)+ttn(6)+
     +   ttn(25))+ttn(31)
      ENDIF
C
C
C     ..ZERO OUT EVERYTHING FOR NEXT YEAR
      DO 50 I=1,MXTN
        TN(I)=0.0D0
   50 CONTINUE
      JNUM=0
      NYR=IYYY
C
      RETURN
 1000 FORMAT(//,' --- YEAR ',I5,'---',
     +    ' YEARLY NITROGEN MASS BALANCE SUMMARY (KG/HA) ')
 1100 FORMAT(//,' --- SIMULATION PERIOD ---',
     +    ' NITROGEN MASS BALANCE SUMMARY (KG/HA) ')
 1200 FORMAT(/' ADDITIONS:',
     +  /T4,'TOTAL BACKGROUND RAIN WATER',T50,G15.6,
     +  /T4,'TOTAL BACKGROUND IRRIGATION WATER',T50,G15.6,
     +  /T4,'TOTAL FERTILIZER APP',T50,G15.6,
     +  /T4,'TOTAL MANURE APP (AS NH4)',T50,G15.6,
     +  /T4,'TOTAL MANURE APP (AS ORGANIC N)',T50,G15.6,
     +      '(N to surface residue:',G15.6,')',
     +  /T4,'TOTAL FROM INCORPORATED RESIDUE',T50,G15.6,
     +  /T4,'TOTAL FROM DEAD ROOTS',T50,G15.6,
     +  /T4,'TOTAL N FIXATION',T50,G15.6,/' LOSSES:',
     +  /T4,'TOTAL DENITRIFICATION',T50,G15.6,
     +  /T4,'TOTAL VOLATILIZATION',T50,G15.6,
     +  /T4,'TOTAL RUNOFF',T50,G15.6,
     +  /T4,'TOTAL ERODED SOIL',T50,G15.6,
     +  /T4,'TOTAL N2O+NXO FROM NITRIFICATION',T50,G15.6,
     +  /T4,'TOTAL N2O+NXO ABSORBED FROM NITRIFICATION',T50,G15.6,
     +  /T4,'TOTAL DEEP SEEPAGE',T50,G15.6,
     +  /T4,'TOTAL TILE DRAINAGE',T50,G15.6,
     +  /T4,'TOTAL LATERAL FLOW',T50,G15.6,
     +  /T4,'TOTAL PLANT UPTAKE',T50,G15.6,
     +  /T4,'TOTAL SORPTION ONTO ION EXCHNAGE SITES',T50,G15.6,
     +  /' TRANSFORMATIONS:',
     +  /T4,'TOTAL MINERALIZATION',T50,G15.6,
     +  /T4,'TOTAL IMMOBILIZATION',T50,G15.6)
 1300 FORMAT(/' STORAGE:',/T4,'AVERAGE RESIDUE POOL 1',T40,G15.6,/T4,
     +  /T4,'AVERAGE RESIDUE POOL 2',T40,G15.6,
     +  /T4,'AVERAGE SOIL HUMUS POOL 1',T40,G15.6,
     +  /T4,'AVERAGE SOIL HUMUS POOL 2',T40,G15.6,
     +  /T4,'AVERAGE SOIL HUMUS POOL 3',T40,G15.6,
     +  /T4,'AVERAGE MICRO ORGANISM POOL 1',T40,G15.6,
     +  /T4,'AVERAGE MICRO ORGANISM POOL 2',T40,G15.6,
     +  /T4,'AVERAGE MICRO ORGANISM POOL 3',T40,G15.6,
     +  /T4,'AVERAGE NO3-N ',T50,G15.6,/T4,'AVERAGE NH4-N',T50,G15.6,
     +  /T4,'AVERAGE NH4-N (ANHYDROUS POOL)',T50,G15.6,
     +  /T4,'AVERAGE UREA-N',T50,G15.6)
1400  format (/T60,15('-'
     +    ),/' NITROGEN BALANCE AT END OF SIMULATION:',T60,G15.6,
     +  /80('*'),
     +  /T4,'INITAL TOTAL SOIL N KG N/HA:',T56,G15.6,
     +  /T4,'FINAL  TOTAL SOIL N KG N/HA:',T56,G15.6,/
     +  /T8,'GAIN FROM RAIN WATER KG N/HA:',T60,G15.6,
     +  /T8,'GAIN FROM IRRIGATION WATER KG N/HA:',T60,G15.6,
     +  /T8,'GAIN FROM FERTLIZER KG N/HA:',T60,G15.6,
     +  /T8,'GAIN FROM MANURE KG N/HA:',T60,G15.6,
     +  /T8,'GAIN FROM SOIL RESIDUE KG N/HA:',T60,G15.6,
     +  /T8,'GAIN FROM DEAD ROOT KG N/HA:',T60,G15.6,
     +  /T8,'GAIN FROM N FIXATION KG N/HA:',T60,G15.6,/
     +  /T8,'LOSS TO DENITRIFICATION KG N/HA:',T60,G15.6,
     +  /T10,'N2  LOSS FROM DENITRIFICATION KG N/HA:',T56,G15.6,
     +  /T10,'N2O LOSS FROM DENITRIFICATION KG N/HA:',T56,G15.6,
     +  /T10,'NxO LOSS FROM DENITRIFICATION KG N/HA:',T56,G15.6,
     +  /T10,'N2O+NxO ABSORBED FROM DENITRIFICATION KG N/HA:',T56,G15.6,
     +  /T8,'N2O LOSS FROM NITRIFICATION KG N/HA:',T60,G15.6,
     +  /T8,'NxO LOSS FROM NITRIFICATION KG N/HA:',T60,G15.6,
     +  /T8,'N2O+NxO ABSORBED FROM NITRIFICATION KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO VOLATILIZATION KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO RUNOFF KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO EROSION KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO SEEPAGE KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO PLANT N UPTAKE KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO ION EXCHANGE SITES KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO TILE DRAINAGE KG N/HA:',T60,G15.6,
     +  /T8,'LOSS TO LATERAL FLOW KG N/HA:',T60,G15.6/80('*'))
 1500 format ('  NITROGEN    BALANCE AT END OF SIMULATION:',T60,G15.6)
      END
C
      SUBROUTINE CARBYR(NYRC,IYYY,JNUMC,TNC,IFLUSH,TSOTC0,TSOTC)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE PRINTS OUT A YEARLY SUMMARY OF THE CARBON
C             MASS BALANCE ROUTINE.  WE ONLY GO INTO THIS ROUTINE WHEN
C             THE CURRENT YEAR IS NOT EQUAL TO THE SAVED YEAR.
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       NYR        I  SAVED CURRENT YEAR
C       IYYY   I  CURRENT YEAR IN MODEL
C       JNUM   I  NUMBER OF DAYS TO AVERAGE OVER
C       TN         I  SAVED TOTALS
C       IFLUSH     I  FLAG: 0-PRINT YEARLY TOTALS
C                     1-PRINT SIMULATION TOTALS
C
C       CALLED FROM: CARBONBAL
C
C       PROGRAMMER: LIWANG MA
C
C       VERSION: 3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXTNC=27,MXTNCA=9)
C
      DIMENSION TNC(MXTNC),TNCA(MXTNCA),TTNC(MXTNC)
      SAVE TTNC,JJNUMC
      DATA TTNC /MXTNC*0.0D0/,JJNUMC /0/
C
C     ..DETERMINE IF THIS IS YEARLY OR SIMULATION TOTALS
      IF(IFLUSH.EQ.0) THEN
C
C       .. AVERAGE THE VALUES
        DO 10 I=1,MXTNCA
          TNCA(I)=TNC(I+11)/DBLE(JNUMC)
   10   CONTINUE
        DO 20 I=1,MXTNC
          TTNC(I)=TTNC(I)+TNC(I)
   20   CONTINUE
        JJNUMC=JJNUMC+JNUMC
        WRITE(75,1000) IYYY-1
        WRITE(75,1200) TNC(1),TNC(21),TNC(8),TNC(9),TNC(11),
     +               TNC(26),TNC(7)+TNC(22),TNC(25)
        WRITE(75,1300) (TNCA(I),I=1,MXTNCA)
      write (75,1400) TSOTC-TSOTC0+tnc(7)+TNC(25)+TNC(26)
     +      -(tnc(1)+tnc(8)+tnc(9)+tnc(11)+TNC(27))
     +       +tnc(24),TSOTC0,TSOTC,TNC(1),TNC(8),TNC(9),TNC(11),TNC(26)
     +        ,tnc(7)+TNC(22),TNC(25),tnc(22),tnc(23)
        TSOTC0=TSOTC
      ELSE
        DO 30 I=1,MXTNC
          TTNC(I)=TTNC(I)+TNC(I)
   30   CONTINUE
        JJNUMC=JJNUMC+JNUMC
        DO 40 I=1,MXTNCA
          IF(JJNUMC.NE.0) THEN
            TNCA(I)=TTNC(I+11)/DBLE(JJNUMC)
          ELSE
            TNCA(I)=0.0D0
          ENDIF
   40   CONTINUE
        WRITE(71,1100)
        WRITE(71,1200) TTNC(1),TTNC(21),TTNC(8),TTNC(9),TTNC(11),
     +               TTNC(26),TTNC(7)+TTNC(22),TTNC(25)
        WRITE(71,1300) (TNCA(I),I=1,MXTNCA)
        write (71,1400) TSOTC-TSOTC0+ttnc(7)+TTNC(25)+TTNC(26)
     +        -(ttnc(1)+ttnc(8)+ttnc(9)+ttnc(11)+TTNC(27))+ttnc(24)
     +       ,TSOTC0,TSOTC,TTNC(1),TTNC(8),TTNC(9),TTNC(11),TTNC(26),
     +        ttnc(7)+TTNC(22),TTNC(25),ttnc(22),ttnc(23)
        WRITE(75,1100)
        WRITE(75,1200) TTNC(1),TTNC(21),TTNC(8),TTNC(9),TTNC(11),
     +                TTNC(26),TTNC(7)+TTNC(22),TTNC(25)
        WRITE(75,1300) (TNCA(I),I=1,MXTNCA)
        write (75,1400) TSOTC-TSOTC0+ttnc(7)+TTNC(25)+TTNC(26)
     +        -(ttnc(1)+ttnc(8)+ttnc(9)+ttnc(11)+TTNC(27))+ttnc(24)
     +       ,TSOTC0,TSOTC,TTNC(1),TTNC(8),TTNC(9),TTNC(11),TTNC(26),
     +       ttnc(7)+TTNC(22),ttnc(25),ttnc(22),ttnc(23)
        write (666,1500) TSOTC-TSOTC0+ttnc(7)+TTNC(25)+ttnc(26)
     +        -(ttnc(1)+ttnc(8)+ttnc(9)+ttnc(11)+TTNC(27))+ttnc(24)
      ENDIF
C
C
C     ..ZERO OUT EVERYTHING FOR NEXT YEAR
      DO 50 I=1,MXTNC
        TNC(I)=0.0D0
   50 CONTINUE
      JNUMC=0
      NYRC=IYYY
C
      RETURN
 1000 FORMAT(//,' --- YEAR ',I5,'---',
     +    ' YEARLY CARBON MASS BALANCE SUMMARY (KG/HA) ')
 1100 FORMAT(//,' --- SIMULATION PERIOD ---',
     +    ' CARBON MASS BALANCE SUMMARY (KG/HA) ')
 1200 FORMAT(/' ADDITIONS:',
     +  /T4,'TOTAL CARBON FROM MANURE APPL',T50,G15.6,
     +      '(C to surface residue:',G15.6,')',
     +  /T4,'TOTAL CARBON FROM RESIDUE',T50,G15.6,
     +  /T4,'TOTAL CARBON FROM DEAD ROOT',T50,G15.6,
     +  /T4,'TOTAL CARBON FROM UREA',T50,G15.6,/' LOSSES:',
     +  /T4,'TOTAL EROSION LOSS',T50,G15.6,
     +  /T4,'TOTAL MINERALIZATION LOSS AS CO2',T50,G15.6,
     +  /T4,'TOTAL MINERALIZATION LOSS AS CH4',T50,G15.6/)
 1300 FORMAT(/' STORAGE:',/T4,'AVERAGE SURFACE RESIDUE',T40,G15.6,/T4,
     +  /T4,'AVERAGE RESIDUE POOL 1',T40,G15.6,
     +  /T4,'AVERAGE RESIDUE POOL 2',T40,G15.6,
     +  /T4,'AVERAGE SOIL HUMUS POOL 1',T40,G15.6,
     +  /T4,'AVERAGE SOIL HUMUS POOL 2',T40,G15.6,
     +  /T4,'AVERAGE SOIL HUMUS POOL 3',T40,G15.6,
     +  /T4,'AVERAGE MICRO ORGANISM POOL 1',T40,G15.6,
     +  /T4,'AVERAGE MICRO ORGANISM POOL 2',T40,G15.6,
     +  /T4,'AVERAGE MICRO ORGANISM POOL 3',T40,G15.6)
1400  format (/T60,15('-'
     +    ),/' CARBON BALANCE AT END OF SIMULATION:',T60,G15.6,
     +  /80('*'),
     +  /T4,'INITAL TOTAL SOIL C KG C/HA:',T56,G15.6,
     +  /T4,'FINAL  TOTAL SOIL C KG C/HA:',T56,G15.6,/
     +  /T8,'GAIN FROM MANURE KG C/HA:',T60,G15.6,
     +  /T8,'GAIN FROM SOIL RESIDUE KG C/HA:',T60,G15.6,
     +  /T8,'GAIN FROM DEAD ROOTS KG C/HA:',T60,G15.6,
     +  /T8,'GAIN FROM UREA KG C/HA:',T60,G15.6,/
     +  /T8,'LOSS TO EROSION KG C/HA:',T60,G15.6,
     +  /T8,'LOSS TO CO2 KG C/HA:',T60,G15.6,
     +  /T8,'LOSS TO CH4 KG C/HA:',T60,G15.6,//
     +  /T8,'CO2 LOSS DUE TO C/N RATIO ADJ OF RES. KG C/HA:',T60,G15.6,
     +  /T8,'CO2 ASSIMILATION BY PLANT  KG C/HA:',T60,G15.6,/80('*'))
 1500 format ('  CARBON      BALANCE AT END OF SIMULATION:',T60,G15.6)
      END
C
      SUBROUTINE ECHO(INP)
C
C======================================================================
C
C       PURPOSE:  USED FOR DATAFILE READ IN PURPOSES.  ANY LINE IN A
C             DATAFILE WHICH HAS AN "=" SIGN IN THE FIRST COLUMN
C             WILL BE SKIPPED.
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       INP       I/O
C
C       EXTERNAL REFERENCES: FILE ASSOCIATED WITH ===> INP
C
C       CALLED FROM: MAIN, INPUT
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION: 2.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER LINE*80
C
   10 READ(UNIT=INP,FMT=1000,END=20) LINE
      IF(LINE(1:1).EQ.'=') GOTO 10
20    BACKSPACE(UNIT=INP) !RM - Moved EOF to backspace so that other READ statements can do EOF handle.
      RETURN
 1000 FORMAT(A80)
      END
C
      SUBROUTINE SIXAVG(NN,NPEST,JDAY,TL,TLT,THETA,SLKS,BD,CONCX2,CC,
     +           FREUND)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE GENERATES AND PRINTS OUT A 6 INCH AVERAGE
C             PROFILE OF THETA, NITRATE, AND PESTICIDES.  THE FILE IS
C             GENERATED IN BORG FORMAT FOR EASY READIN.
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       INP       I/O
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN
C
C       PROGRAMMER: KAREN JOHNSEN
C
C       VERSION: 2.5
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXPEST=3,MXCHEM=15)
      DIMENSION TL(NN),TLT(NN),THETA(NN),BD(NN),SLKS(MXNOD,MXPEST,2),
     +    CONCX2(MXNOD,MXPEST),CC(MXNOD,MXCHEM)
      DIMENSION D6P(MXNOD),TOTP(MXPEST),PVAL(MXPEST),FREUND(MXPEST)
C
C     ..FIND # OF 6" PROFILES
      N6P=INT(TLT(NN)/(6.0D0*2.54D0))+1
      ISAV=0
C
C     ..FIND AVERAGE VALUES
      DO 50 J=1,N6P
        D6P(J)=MIN(J*6.0D0*2.54D0,TLT(NN))
        PBM=0.0D0
        TOTW=0.0D0
        TOTN=0.0D0
        DO 10 IP=1,NPEST
          TOTP(IP)=0.0D0
   10   CONTINUE
        IL1=1
        IF(J.NE.1) IL1=ISAV
        ISAV=0
        THK=0.0D0
        DO 30 I=IL1,NN
          IF(ISAV.EQ.0) THEN
            IF(I.EQ.IL1.AND.J.NE.1) THEN
              THKI=TLT(I)-D6P(J-1)
              IF(IL1.EQ.NN) ISAV=I
            ELSEIF(TLT(I).GE.D6P(J)) THEN
              THKI=D6P(J)-TLT(I-1)
              ISAV=I
            ELSE
              THKI=TL(I)
            ENDIF
            THK=THK+THKI
C           ..SUM PESTICIDE MASS
            DO 20 IP=1,NPEST
C LIWANG MA, 7-3-2006
              TSL1=CC(I,IP+12)*(THETA(I)+
     +          FSLKS(SLKS(I,IP,1),CC(I,IP+12),FREUND(IP),
     +          theta(i),bd(i))*BD(I))+
     +            CONCX2(I,IP)*BD(I)
              IF(TSL1.LT.1.D-12) TSL1=0.0D0
              TOTP(IP)=TOTP(IP)+TSL1*THKI
   20       CONTINUE
C           ..SUM WATER AND N MASS
            TOTW=TOTW+THETA(I)*THKI
            TOTN=TOTN+CC(I,9)*THETA(I)*THKI
            PBM=PBM+BD(I)
          ENDIF
   30   CONTINUE
        PBM=PBM/(ISAV-IL1+1)
C       ..FIND AVERAGE VALUES OVER 6"
        DO 40 IP=1,NPEST
          PVAL(IP)=TOTP(IP)*1.0D3/(THK*PBM)
   40   CONTINUE
        WVAL=TOTW/THK
        CVAL=TOTN/(THK*PBM)
        D6M=D6P(J)/2.0D0
        IF(J.NE.1) D6M=D6P(J-1)+(D6P(J)-D6P(J-1))/2.0D0
        WRITE(76,1000) JDAY,D6M,WVAL,CVAL,(PVAL(IP),IP=1,MXPEST)
   50 CONTINUE
C
      RETURN
 1000 FORMAT(I4,6G15.5)
      END
C
      SUBROUTINE PESBAL(NN,JDAY,IP,BD,THETA,TL,CC,CORES,COPLNT,SLKS,
     +    CONCX2,TSL2C,IYYY,MDAY,OMSEA,IMAGIC,COBIND,FREUND,
     +    TSOTP0YR,TSOTP00,TSOTPDAY,TPEST,NYRP,erodedmass)
C
C======================================================================
C
C       PURPOSE:  TOTAL PESTICIDE MASS BALANCE
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       COBIND    I/O CONCENTRATION OF IRREVERSIBLY BOUND POOL (UG/CC)
C       COPLNT    I/O INITIAL CONCENTRATION ON PLANT (UG/CM^2)
C       CORES I/0 INITIAL CONCENTRATION ON CROP RESIDUE (UG/CM^2)
C       CONV   P  CONVERTS UG/CM^2 ==> KG/HA
C       CONCX2     I  PEST. CONC. INVOLVED IN KINETIC PROCESSES [UG/G-S]
C       SLKS   I  SOIL LAYER KD VALUES; CORRECTED FOR OM [CM^3/G]
C       TSOTP0        TOTAL MASS OF PESTICIDE AT END. (KG/HA)
C       TAPAP     ADDITION OF PESTICIDE (KG/HA)
C       TDPPLNT       LOSS OR PESTICIDE FROM PLANT UPTAKE (KG/HA)
C       TLPRS     PESTICIDE DEGRADATION FROM RESIDUE LAYER (KG/HA)
C       TLPCN     PESTICIDE DEGRADATION FROM CANOPY (KG/HA)
C       TLPSL     PESTICIDE DEGRADATION FROM SOIL PROFILE (KG/HA)
C       TLPRO     LOSS OR PESTICIDE FROM RUNOFF (KG/HA)
C       TLPLC     LOSS OR PESTICIDE FROM LEACHING (KG/HA)
C       TSPRS     STORAGE OF PESTICIDE ON RESIDUE LAYER (KG/HA)
C       TSPCN     STORAGE OF PESTICIDE IN CANOPY (KG/HA)
C       TSPSOL        STORAGE OF PESTICIDE IN SOIL SOLUTION (KG/HA)
C       TSPADS        STORAGE OF PESTICIDE IN SOIL ADSORPED (KG/HA)
C       TSPSKN        STORAGE OF PESTICIDE IN SOIL KINETICS (KG/HA)
C       TSPBND        STORAGE OF PESTICIDE IN BOUND POOL (KG/HA)
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                 SGATE
C
C       CALLED FROM: MAIN
C
C       PROGRAMMER: KEN ROJAS
C
C       VERSION: 3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER(MXNOD=300,MXCHEM=15,MXPEST=3,MAXHOR=12,CONV=1.0D-1,
     +    MXSPEC=10,MXANA=135,MXTP=19)
C
      COMMON /PINFO/ TSOTP0(MXPEST),TAPAP(MXPEST),TLPRS(MXPEST),
     +    TLPCN(MXPEST),TLPSL(MXPEST),TLPRO(MXPEST),TLPLC(MXPEST),
     +    TDPPLNT(MXPEST),TLPDT(MXPEST),TLPDTD(MXPEST),
     +    TLPDTL(MXPEST),TLPDMA(MXPEST)
C
      common/maxbal/IDmxH2O,IMmxH2O,IYYYmxH2O,IDmxN,IMmxN,IYYYmxN,
     +              IDmxC,IMmxC,IYYYmxC,IDmxP,IMmxP,IYYYmxP,
     +              DTMASSmxH2O,BalmxN,BalmxC,BalmxP,IWCINIT1
     +            ,ireset1
      CHARACTER SOLTYP*10,NMPEST*30,PLNAME*30
      COMMON /NAMES/ NMPEST(MXPEST),SOLTYP(MAXHOR),PLNAME(MXSPEC)
C
      LOGICAL START(MXPEST)
      DIMENSION BD(NN),THETA(NN),TL(NN),CC(MXNOD,MXCHEM),CORES(MXPEST),
     +    COPLNT(MXPEST),SLKS(MXNOD,MXPEST,2),CONCX2(MXNOD,MXPEST),
     +    TSL2C(MXNOD,MXPEST),TSL2X(MXNOD),OMSEA(MXANA),NYRP(MXPEST),
     +    COBIND(MXNOD,MXPEST),FREUND(MXPEST),TPEST(MXTP,MXPEST),
     +    TSOTP00(MXPEST),TSOTP0YR(MXPEST),TSOTPDAY(MXPEST),
     +    TSOTP(MXPEST),erodedmass(4),dtadjp(mxpest)
      SAVE START
      DATA START /MXPEST*.TRUE./,bal/0.0d0/
C
      TSPSOL=0.0D0
      TSPADS=0.0D0
      TSPSKN=0.0D0
      TSPBND=0.0D0
C
C     ..DETERMINE MASS ON RESIDUE COVER (KG/HA)
      TSPRS=CORES(IP)*CONV
C
C     ..DETERMINE MASS IN CANOPY (KG/HA)
      TSPCN=COPLNT(IP)*CONV
C
C     ---CYCLE THROUGH ALL THE SOIL LAYERS
      DO 10 I=1,NN
        CONV1=TL(I)*BD(I)*CONV
        CONV2=TL(I)*THETA(I)*CONV
C
C       ..DETERMINE MASS IN SOIL PROFILE SOLUTION (KG/HA)
        TSPSOL=TSPSOL+CC(I,IP+12)*CONV2
C
C       ..DETERMINE MASS IN SOIL PROFILE ADSORBED (KG/HA)
C LIWANG MA, 7-3-2006
        TSPADS=TSPADS+CC(I,IP+12)*FSLKS(SLKS(I,IP,1),
     +         CC(I,IP+12),FREUND(IP),theta(i),bd(i))*CONV1
C
C       ..DETERMINE MASS IN SOIL PROFILE KINETIC POOL (KG/HA)
        TSPSKN=TSPSKN+CONCX2(I,IP)*CONV1
C
C       ..DETERMINE MASS IN BOUND POOL (KG/HA)
        TSPBND=TSPBND+COBIND(I,IP)*CONV1
   10 CONTINUE
C
C     ..SUM UP TOTAL MASS IN STORAGE
      TSOTP(IP)=TSPRS+TSPCN+TSPSOL+TSPADS+TSPSKN+TSPBND
C
      IF(START(IP)) THEN
C
C       ..SAVE FOR LATER CALCULATIONS IF FIRST TIME THROUGH
        NYRP(IP)=IYYY
        TSOTP0(IP)=TSOTP(IP)
        TSOTP00(IP)=TSOTP(IP)
        TSOTP0YR(IP)=TSOTP(IP)
        START(IP)=.FALSE.
      ELSE
C
        if (iwcinit1.eq.1) then
          dtadjP(IP)=tsotp0(IP)-tsotp00(IP)
          tsotp0(IP)=tsotp00(IP)
        else
          dtadjP(IP)=0.0d0
c          iwcinit1=0   !reset at C balance
        endif
C       .. PREPARE DEBUG OUTPUT
C
C       .. THESE ACCUMLATORS ARE IN KG/HA
        TSL1=0.0D0      ! in solution
        TSL2=0.0D0      ! total
        TSL3=0.0D0      ! adsorbed
        TSLX=0.0D0      ! kinetic pool
        TSLB=0.0D0      ! bound pool
        DO 20 I=1,NN
          CONV1=TL(I)*BD(I)*CONV
          CONV2=TL(I)*THETA(I)*CONV
          TSL1=TSL1+CC(I,IP+12)*CONV2
          TSLB=TSLB+COBIND(I,IP)*CONV1
C LIWANG MA, 7-3-2006
          TSL3=TSL3+CC(I,IP+12)*FSLKS(SLKS(I,IP,1),CC(I,IP+12),
     +         FREUND(IP),theta(i),bd(i))*CONV1
          TSLX=TSLX+CONCX2(I,IP)*CONV1
          TSL2X(I)=CC(I,IP+12)/10.0D0*(THETA(I)*TL(I)+
     +        FSLKS(SLKS(I,IP,1),CC(I,IP+12),FREUND(IP),
     +          theta(i),bd(i))*
     +        BD(I)*TL(I))+CONCX2(I,IP)*CONV1+COBIND(I,IP)*CONV1
          TSL2=TSL2+TSL2X(I)
C
C         .. THIS VARIABLE IS IN UG/G
          TSL2C(I,IP)=CC(I,IP+12)*(THETA(I)/BD(I)+
     +         FSLKS(SLKS(I,IP,1),CC(I,IP+12),FREUND(IP),
     +           theta(i),bd(i)))+CONCX2(I,IP)
   20   CONTINUE
C
C       ..SUM UP ALL THE LOSSES
        TOTL=TLPRS(IP)+TLPCN(IP)+TLPSL(IP)+TLPRO(IP)+TLPLC(IP)+
     +      TDPPLNT(IP)+ERODEDMASS(IP)
C Liwang Ma, 3-14-2009
c pesticide gains
        TPEST(1,IP)=TPEST(1,IP)+TAPAP(IP)
c pesticde losses
        TPEST(2,IP)=TPEST(2,IP)+TLPRS(IP)
        TPEST(3,IP)=TPEST(3,IP)+TLPCN(IP)
        TPEST(4,IP)=TPEST(4,IP)+TLPSL(IP)
        TPEST(5,IP)=TPEST(5,IP)+TLPRO(IP)
        TPEST(6,IP)=TPEST(6,IP)+TLPLC(IP)
        TPEST(7,IP)=TPEST(7,IP)+TDPPLNT(IP)
        TPEST(14,IP)=TPEST(14,IP)+TLPDT(IP)
        TPEST(15,IP)=TPEST(15,IP)+TLPDTD(IP)
        TPEST(16,IP)=TPEST(16,IP)+TLPDTL(IP)
        TPEST(17,IP)=TPEST(17,IP)+TLPDMA(IP)
        TPEST(18,IP)=TPEST(18,IP)+ERODEDMASS(IP)
        TPEST(19,IP)=TPEST(19,IP)+dtadjp(IP)
c pesticide storage
        TPEST(8,IP)=TSPRS
        TPEST(9,IP)=TSPCN
        TPEST(10,IP)=TSPSOL
        TPEST(11,IP)=TSPADS
        TPEST(12,IP)=TSPSKN
        TPEST(13,IP)=TSPBND
C
      IF(NYRP(IP).NE.IYYY) 
     +  CALL PESTBYR(NYRP,IYYY,JNUMC,TPEST,0,TSOTP0YR,TSOTP,IP)
C       ..CALC THE BALANCE
        BAL=TSOTP(IP)+TOTL-TAPAP(IP)-TSOTP0(IP)
C
c        IF(npest.gt.0) THEN
          TSL1 = MAX(TSL1, 0.0D0)
          TSL3 = MAX(TSL3, 0.0D0)
          TSLX = MAX(TSLX, 0.0D0)
          TSLB = MAX(TSLB, 0.0D0)
          TSL2 = MAX(TSL2, 0.0D0)
          WRITE(59+IP,*) 'KD(1)==> ',SLKS(1,IP,1)
          IF(ABS(BAL).GT.0.01D0) WRITE(59+IP,1000)
          WRITE(79+IP,1400) MDAY,TSL1,TSL3,TSLX,TSLB,TSL2
          CALL CDATE(JDAY,ID,IM,IYYY)
          WRITE(59+IP,1100) ID,IM,IYYY,JDAY,NMPEST(IP),TSOTP0(IP),
     +        TAPAP(IP),TAPAP(IP)
          WRITE(59+IP,1200) TLPRS(IP),TLPCN(IP),TLPSL(IP),TLPRO(IP),
     +         ERODEDMASS(IP),
     +        TLPLC(IP),TLPDMA(IP),TLPDT(IP)-TLPDMA(IP),TLPDTD(IP),
     +        TLPDTL(IP),TDPPLNT(IP),TOTL
          WRITE(59+IP,1300) TSPRS,TSPCN,TSPSOL,TSPADS,TSPSKN,TSPBND,
     +        TSOTP(IP),BAL
c        ENDIF
        TSOTP0(IP)=TSOTP(IP)
        TSOTPDAY(IP)=TSOTP(IP)
        CALL VGATE(TDAY,31+IP,TSL2X)
      ENDIF
	  if (abs(bal).ge.abs(balmxP)) then
            balmxP=bal
            IDmxP=id
		  IMmxP=im
		  IYYYmxP=iyyy
        endif
          if (abs(bal).gt.1.0d-5) THEN !changed by Liwang Ma
          CALL dayjack(0,0,0,5)!send message that pesticide mass balance was bad
          WRITE (666,FMT=667) ID, IM, IYYY, jday,BAL,ip !Liwang Ma
          if (abs(bal).gt.50.0d0) print*, 'SEVERE MASS BALANCE PROBLEM'
          ENDIF
      OMSEA((8+IP)*5+1)=TAPAP(IP)
      OMSEA((8+IP)*5+2)=TSOTP(IP)
      TLPRS(IP)=0.0D0
      TLPCN(IP)=0.0D0
      TLPSL(IP)=0.0D0
      TLPRO(IP)=0.0D0
      TLPLC(IP)=0.0D0
      TDPPLNT(IP)=0.0D0
      TAPAP(IP)=0.0D0
C
      RETURN
667   FORMAT (I4,I4,I6,' (DOY ',i3,')',G15.6,' Pest #',i3,
     +       ' =============>Pesticide BALANCE PROBLEM')
 1000 FORMAT(5('$'))
 1100 FORMAT(//' ---',I3,'/',I2,'/',I4,' --- ',I4,
     +    ' --- MASS BALANCE (KG/HA) OF ',A30,/
     +    ' TOTAL CHEMICAL AT START',T60,G15.6,/' ADDITIONS:',/T4,
     +    'FROM CHEMICAL APPLICATION',T50,G15.6,/T4,'TOTAL ADDITIONS',
     +    T60,G15.6)
 1200 FORMAT(/' LOSSES:',/T4,'DEGRADATION ON RESIDUE',T50,G15.6,/T4,
     +    'DEGRADATION IN CANOPY',T50,G15.6,/T4,
     +    'DEGRADATION WITHIN SOIL PROFILE',T50,G15.6,/T4,'RUNOFF',T50,
     +    G15.6,/T4,'ERODED SOIL',T50,G15.6,
     +    /T4,'SEEPAGE & DRAINAGE',T50,G15.6,/T10,
     +    'FROM MACROPORES',T35,G15.6,/T10,
     +    'FROM SOIL MATRIX',T35,G15.6,/T10,
     +    'FROM TILE DRAIN FLOW',T35,G15.6,/T10,
     +    'FROM LATERAL FLOW',T35,G15.6,/T4,
     +    'UPTAKEN BY PLANTS',T50,G15.6,/T4,'TOTAL LOSSES',T60,G15.6)
 1300 FORMAT(/' STORAGE:',/T4,'SURFACE RESIDUE LAYER',T50,G15.6,/T4,
     +    'PLANT CANOPY',T50,G15.6,/T4,'SOIL LAYER: SOLUTION POOL',T50,
     +    G15.6,/T4,'SOIL LAYER: ADSORBED POOL',T50,G15.6,/T4,
     +    'SOIL LAYER: KINETIC POOL',T50,G15.6,/T4,
     +    'SOIL LAYER: IRREVERSIBLY BOUND POOL',T50,G15.6,/
     +    ' TOTAL CHEMICAL AT END',T60,G15.6,/T60,15('-'),/
     +    ' MASS BALANCE AT END OF DAY:',T60,G15.6)
 1400 FORMAT(I4,5G15.5)
      END
C
      SUBROUTINE PESTBYR(NYRP,IYYY,JNUM,TPEST,IFLUSH,TSOTP0,TSOTP,IP)
C
C======================================================================
C
C       PURPOSE:  THIS ROUTINE PRINTS OUT A YEARLY SUMMARY OF PEST
C             MASS BALANCE ROUTINE.  WE ONLY GO INTO THIS ROUTINE WHEN
C             THE CURRENT YEAR IS NOT EQUAL TO THE SAVED YEAR.
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       NYR        I  SAVED CURRENT YEAR
C       IYYY   I  CURRENT YEAR IN MODEL
C       JNUM   I  NUMBER OF DAYS TO AVERAGE OVER
C       TN         I  SAVED TOTALS
C       IFLUSH     I  FLAG: 0-PRINT YEARLY TOTALS
C                     1-PRINT SIMULATION TOTALS
C
C       CALLED FROM: PESBAL
C
C       PROGRAMMER: LIWANG MA
C
C       VERSION: 3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXTP=19,MXPEST=3,MXTPP=MXPEST*MXTP)
C
      DIMENSION TPEST(MXTP,MXPEST),TTPEST(MXTP,MXPEST),TSOTP(MXPEST),
     +          TSOTP0(MXPEST),NYRP(MXPEST)
      SAVE TTPEST,JJNUM
      DATA TTPEST /MXTPP*0.0D0/,JJNUM /0/
C
C     ..DETERMINE IF THIS IS YEARLY OR SIMULATION TOTALS
      IF(IFLUSH.EQ.0) THEN
C
C       .. AVERAGE THE VALUES
        DO 20 I=1,7
          TTPEST(I,IP)=TTPEST(I,IP)+TPEST(I,IP)
   20   CONTINUE
        DO 21 I=8,13   !MXTP
          TTPEST(I,IP)=TPEST(I,IP)
   21   CONTINUE
        DO 22 I=14,MXTP
          TTPEST(I,IP)=TTPEST(I,IP)+TPEST(I,IP)
   22   CONTINUE
        JJNUM=JJNUM+JNUM
        TLOSS=TPEST(2,IP)+TPEST(3,IP)+tpest(18,ip)+
     +      TPEST(4,IP)+TPEST(5,IP)+TPEST(6,IP)+TPEST(7,IP)
        TBAL=TSOTP(IP)-TSOTP0(IP)-TPEST(1,IP)+TLOSS+tpest(19,ip)
        WRITE(59+IP,1000) IYYY-1
        WRITE(59+IP,1100) TSOTP0(IP),TPEST(1,IP),TPEST(1,IP)
C        WRITE(59+IP,1200) (TPEST(I,IP),I=2,7),TLOSS
        WRITE(59+IP,1200) (TPEST(I,IP),I=2,5),TPEST(18,IP),
     +         TPEST(6,IP),TPEST(17,IP),
     +         TPEST(14,IP)-TPEST(17,IP),TPEST(15,IP),
     +         TPEST(16,IP),TPEST(7,IP),TLOSS
      WRITE(59+IP,1300) (TPEST(I,IP),I=8,13),TSOTP(IP),TBAL
        TSOTP0(IP)=TSOTP(IP)
      ELSE
        DO 30 I=1,7
          TTPEST(I,IP)=TTPEST(I,IP)+TPEST(I,IP)
   30   CONTINUE
        DO 32 I=14,17
          TTPEST(I,IP)=TTPEST(I,IP)+TPEST(I,IP)
   32   CONTINUE
        JJNUM=JJNUM+JNUM
        WRITE(59+IP,1001)
        TLOSS=TTPEST(2,IP)+TTPEST(3,IP)+ttpest(18,ip)+
     +      TTPEST(4,IP)+TTPEST(5,IP)+TTPEST(6,IP)+TTPEST(7,IP)
        TBAL=TSOTP(IP)-TSOTP0(IP)-TTPEST(1,IP)+TLOSS+ttpest(19,ip)
        WRITE(59+IP,1100) TSOTP0(IP),TTPEST(1,IP),TTPEST(1,IP)
C        WRITE(59+IP,1200) (TTPEST(I,IP),I=2,7),TLOSS
        WRITE(59+IP,1200) (TTPEST(I,IP),I=2,5),TTPEST(18,IP),
     +         TTPEST(6,IP),TTPEST(17,IP),
     +         TTPEST(14,IP)-TTPEST(17,IP),TTPEST(15,IP),
     +         TTPEST(16,IP),TTPEST(7,IP),TLOSS
        WRITE(59+IP,1400) (TTPEST(I,IP),I=8,13),TSOTP(IP),TBAL
        WRITE(666,1500) ip,TBAL
      ENDIF
C
C
C     ..ZERO OUT EVERYTHING FOR NEXT YEAR
      DO 50 I=1,MXTP
        TPEST(I,IP)=0.0D0
   50 CONTINUE
      JNUM=0
      NYRP(IP)=IYYY
C
      RETURN
 1000 FORMAT(//,' --- YEAR ',I5,'---',
     +    ' YEARLY PESTICIDE MASS BALANCE SUMMARY (KG/HA) ')
 1001 FORMAT(//,' --- SIMULATION PERIOD ---',
     +    ' PESTICIDE MASS BALANCE SUMMARY (KG/HA) ')
 1100 FORMAT(//' TOTAL CHEMICAL AT START',T60,G15.6,/' ADDITIONS:',/T4,
     +    'FROM CHEMICAL APPLICATION',T50,G15.6,/T4,'TOTAL ADDITIONS',
     +    T60,G15.6)
 1200 FORMAT(/' LOSSES:',/T4,'DEGRADATION ON RESIDUE',T50,G15.6,/T4,
     +    'DEGRADATION IN CANOPY',T50,G15.6,/T4,
     +    'DEGRADATION WITHIN SOIL PROFILE',T50,G15.6,/T4,'RUNOFF',T50,
     +    G15.6,/T4,'ERODED SOIL',T50,G15.6,
     +     /T4,'SEEPAGE & DRAINAGE',T50,G15.6,/T10,
     +    'FROM MACROPORES',T35,G15.6,/T10,
     +    'FROM SOIL MATRIX',T35,G15.6,/T10,
     +    'FROM TILE DRAIN FLOW',T35,G15.6,/T10,
     +    'FROM LATERAL FLOW',T35,G15.6,/T4,
     +    'UPTAKEN BY PLANTS',T50,G15.6,/T4,'TOTAL LOSSES',T60,G15.6)
 1300 FORMAT(/' STORAGE:',/T4,'SURFACE RESIDUE LAYER',T50,G15.6,/T4,
     +    'PLANT CANOPY',T50,G15.6,/T4,'SOIL LAYER: SOLUTION POOL',T50,
     +    G15.6,/T4,'SOIL LAYER: ADSORBED POOL',T50,G15.6,/T4,
     +    'SOIL LAYER: KINETIC POOL',T50,G15.6,/T4,
     +    'SOIL LAYER: IRREVERSIBLY BOUND POOL',T50,G15.6,/
     +    ' TOTAL CHEMICAL AT END',T60,G15.6,/T60,15('-'),/
     +    ' MASS BALANCE AT END OF YEAR OR SIMULATION:',T60,G15.6)
 1400 FORMAT(/' STORAGE:',/T4,'SURFACE RESIDUE LAYER',T50,G15.6,/T4,
     +    'PLANT CANOPY',T50,G15.6,/T4,'SOIL LAYER: SOLUTION POOL',T50,
     +    G15.6,/T4,'SOIL LAYER: ADSORBED POOL',T50,G15.6,/T4,
     +    'SOIL LAYER: KINETIC POOL',T50,G15.6,/T4,
     +    'SOIL LAYER: IRREVERSIBLY BOUND POOL',T50,G15.6,/
     +    ' TOTAL CHEMICAL AT END',T60,G15.6,/T60,15('-'),/
     +    ' PESTICIDE BALANCE AT END OF SIMULATION:',T60,G15.6)
 1500 format ('  PESTICIDE',i2,' BALANCE AT END OF SIMULATION:',
     +T60,G15.6)
      END
c
      SUBROUTINE PLREAD(NGNPL,INXPL,INP6,CLBASE)
C
C======================================================================
C
C       PURPOSE:  READ IN PLANT PARAMETERS FROM PLGEN.DAT
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: INPUT
C
C       PROGRAMMER: JON HANSON
C
C       VERSION: 3.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXSPEC=10)
C
      COMMON /PLNTDA/ CLSIZE(0:7),PSTATE(9),PWRTS,DMDNIT,TNITUP,GS,
     +    CNUP1(MXSPEC),CNUP2(MXSPEC),UPLUX
C
      INTEGER IPLTYP,NPL,VRNLZ,GDDFLG,NGNPL,NUM,INDX,K
      INTEGER PTR(MXSPEC),INXPL(MXSPEC)
      CHARACTER PLDES*30,STRING*255
      LOGICAL KEEPIT
      DOUBLE PRECISION LWM,LWS,CLBASE(MXSPEC)
C
      COMMON /PLNTIN/ AHLDG(MXSPEC),ALA(MXSPEC),ALNAVL(MXSPEC),
     +    ALNGER(MXSPEC),ALNLW(MXSPEC),ALPHA(MXSPEC),ALX(MXSPEC),
     +    BETA(MXSPEC),BGSEED(MXSPEC),CAA(MXSPEC),CANK(MXSPEC),
     +    CAX(MXSPEC),CONVLA(MXSPEC),CVLBIO(MXSPEC),DEVRAT(2:7,MXSPEC),
     +    DROTOL(MXSPEC),EFFN(MXSPEC),DTSEED(MXSPEC),GITEMP(MXSPEC),
     +    GIWAT(MXSPEC),GMN(MXSPEC),GRMRAT(MXSPEC),GSR(MXSPEC),
     +    HFMAX(MXSPEC),PCLDG(MXSPEC),PGDVRT(MXSPEC),PGNTGT(MXSPEC),
     +    PLALFA(MXSPEC),PLBIO(MXSPEC),PLDIAM(MXSPEC),PLHGHT(MXSPEC),
     +    PLTMNT(MXSPEC),PLTMXT(MXSPEC),PLTOPT(MXSPEC),PMAX(MXSPEC),
     +    PMAXN(MXSPEC),PNRED(MXSPEC),PMNNIT(MXSPEC),PRNLW(MXSPEC),
     +    PTLAMX(MXSPEC),R20(MXSPEC),RATLS(MXSPEC),RATRSX(MXSPEC),
     +    RATRSN(MXSPEC),RDR(MXSPEC),RDX(MXSPEC),RQ10(MXSPEC),
     +    RTNLW(MXSPEC),SDAMAX(MXSPEC),SDDVRT(MXSPEC),SDSVRT(MXSPEC),
     +    SDTMGM(MXSPEC),SDWMAX(MXSPEC),SFREEZ(MXSPEC),SLA1(MXSPEC),
     +    SLA2(MXSPEC),SLA3(MXSPEC),SLA4(MXSPEC),STEND(2:6,MXSPEC),
     +    STNAVL(MXSPEC),STNGER(MXSPEC),STNLW(MXSPEC),TBS(MXSPEC),
     +    TOP(MXSPEC),TOT4WT(MXSPEC),WCG(MXSPEC),WCP(MXSPEC),
     +    WDLDG(MXSPEC),CNST(MXSPEC),PRB(7,7,MXSPEC),GSGDD(2:6,MXSPEC),
     +    GDDFLG(MXSPEC),VRNLZ(MXSPEC),SUFNDX(MXSPEC),IPLTYP(MXSPEC),
     +    LWM(MXSPEC),LWS(MXSPEC),NPL,INFIXN(MXSPEC)
C
      DATA K /0/
C
C     =================================================================
C     == MATCH PLANT INDEX WITH POINTER TO APPROPRIATE SPECIES IN   ===
C     == THE GENERIC PLANT DATA BASE       ===
C     =================================================================
C
C     ...READ NUMBER OF SPECIES IN FILE
      CALL ECHO(INP6)
      READ(INP6,*) NUM
C
C     ...CHECK FOR MATCH BETWEEN DESIRED SPECIES AND THOSE IN DATABASE
C     ...DEFINE POINTER (PTR(I,J))TO POINT TO PROPER RECORDS IN DATABASE
      CALL ECHO(INP6)
      IPTR=0
      DO 20 J=1,NUM
        READ(INP6,1000) INDX,PLDES
        DO 10 I=1,NGNPL
          IF(INDX.EQ.INXPL(I)) THEN
            PTR(I)=J
          ENDIF
      IPTR=IPTR+PTR(I)
   10   CONTINUE
   20   CONTINUE
        if (iptr.eq.0) return
      DO I=1,NGNPL
	   IF (IPTR.EQ.0) PTR(I)=I   !FOR DSSAT TO CALCULATE PLANT HEIGHT
      ENDDO
C
C     ====================================================
C     === READ IN INFORMATION FOR GENERIC PLANTS ONLY ====
C     ====================================================
C
C     ...READ IN VALUES FOR PLANT DIMENSIONS
      CALL ECHO(INP6)
      DO 30 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) PLDIAM(K),PLHGHT(K),PHFBIO,PLBIO(K),TOT4WT(K)
C          PLALFA(K)=-2.0D0*PLHGHT(K)*LOG(0.5D0)/(PHFBIO*PHFBIO)
        if (PHFBIO.gt.100.0d0.and.clbase(k).gt.0.0d0) then
        PLALFA(K)=-2.0D0*PLHGHT(K)*LOG(0.5D0)/(PHFBIO/(CLBASE(K)*1.d-3))
        PLBIO(K)=PLBIO(K)/(CLBASE(K)*1.d-3)
        TOT4WT(K)=TOT4WT(K)/(CLBASE(K)*1.d-3)
        else if(PHFBIO.gt.0.0d0) then
        PLALFA(K)=-2.0D0*PLHGHT(K)*LOG(0.5D0)/PHFBIO
        endif
c
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   30 CONTINUE
C
C     ...READ IN VALUES FOR NITROGEN MANAGEMENT
C     ...WHOLE-PLANT
      CALL ECHO(INP6)
      DO 40 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
        read (inp6,1110) string
        ki=inumb(string)
        backspace (inp6)
        IF (Ki.EQ.7) THEN    
          READ(INP6,*) PMAXN(K),PNRED(K),PMNNIT(K),CNUP2(K),EFFN(K),
     +        IPLTYP(K),VRNLZ(K)
          infixn(k)=0
          ELSE IF (Ki.EQ.8) THEN
          READ(INP6,*) PMAXN(K),PNRED(K),PMNNIT(K),CNUP2(K),EFFN(K),
     +        IPLTYP(K),VRNLZ(K),INFIXN(K)
          ENDIF
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   40 CONTINUE
C
 1110 FORMAT(A255)
C     ...COMPONENTS
      CALL ECHO(INP6)
      DO 50 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) ALNLW(K),ALNAVL(K),STNLW(K),STNAVL(K),RTNLW(K),
     +        PRNLW(K),PGNTGT(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   50 CONTINUE
C
C     ...READ IN VALUES FOR PHOTOSYNTHESIS
      CALL ECHO(INP6)
      DO 60 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) BETA(K),PMAX(K),CANK(K)
C         ...CONVERT PMAX FROM MICROMOLES C/M2/SEC ==> MOLES C/M2/HOUR
          PMAX(K)=PMAX(K)/1.0D6*3600.0D0
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   60 CONTINUE
C
C     ...READ IN VALUES FOR DROUGHT RESISTANCE
      CALL ECHO(INP6)
      DO 70 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) DROTOL(K),HFMAX(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   70 CONTINUE
C
C     ...READ IN VALUES FOR RESPIRATION
      CALL ECHO(INP6)
      DO 80 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) RQ10(K),R20(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   80 CONTINUE
C
C     ...READ IN VALUES FOR TEMPERATURE CONTROL
      CALL ECHO(INP6)
      DO 90 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) PLTMXT(K),PLTMNT(K),PLTOPT(K),CNST(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
   90 CONTINUE
C
C     ...READ IN VALUES FOR PLANT RATIOS
      CALL ECHO(INP6)
      DO 100 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) RATRSX(K),RATRSN(K),RATLS(K),PTLAMX(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  100 CONTINUE
C
C     ...READ IN VALUES FOR MISCELLANEOUS PLANT PROCESSES
      CALL ECHO(INP6)
      DO 110 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) CVLBIO(K),SLA1(K),SLA2(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  110 CONTINUE
C
C     ...READ IN VALUES FOR GERMINATION CONTROL
      CALL ECHO(INP6)
      DO 120 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) GITEMP(K),GIWAT(K),GRMRAT(K),SDTMGM(K),
     +        ALNGER(K),STNGER(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  120 CONTINUE
C
C     ...READ IN VALUES FOR PLANT MORTALITY
      CALL ECHO(INP6)
      DO 130 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) SDAMAX(K),SDWMAX(K),SFREEZ(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  130 CONTINUE
C
C     ...READ IN NONZERO VALUES FOR P MATRIX
C     ...INITIALIZE P MATRIX TO ZERO
      DO 160 IPL=1,NGNPL
        DO 150 J=1,7
          DO 140 I=1,7
            PRB(I,J,IPL)=0.0D0
  140     CONTINUE
  150   CONTINUE
  160 CONTINUE
      CALL ECHO(INP6)
      DO 170 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) PRB(2,2,K),PRB(3,2,K),PRB(3,3,K),PRB(4,3,K),
     +        PRB(4,4,K),PRB(5,4,K),PRB(5,5,K),PRB(6,5,K),PRB(7,6,K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  170 CONTINUE
C
C     ...READ IN VALUES FOR LODGING
      CALL ECHO(INP6)
      DO 180 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) AHLDG(K),PCLDG(K),WDLDG(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  180 CONTINUE
C
C     --READ IN VALUES FOR SEED DYNAMICS
      CALL ECHO(INP6)
      DO 190 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) BGSEED(K),DTSEED(K),SDSVRT(K),PGDVRT(K),
     +        SDDVRT(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  190 CONTINUE
C
C     ...READ IN VALUES FOR GROWING DEGREE DAYS
      CALL ECHO(INP6)
      DO 200 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) GDDFLG(K),(GSGDD(I,K),I=2,6)
          if (gsgdd(2,k).eq.0) gsgdd(2,k)=100.0d0
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  200 CONTINUE
C
C     ...READ IN VALUES FOR GROWTH STAGE THRESHOLDS
      CALL ECHO(INP6)
      DO 210 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) (STEND(I,K),I=2,6)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  210 CONTINUE
C
C     ...READ IN VALUES FOR MINIMUM TIME SPENT IN EACH GROWTH STAGE
      CALL ECHO(INP6)
      DO 230 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) (DEVRAT(I,K),I=2,6)
C
C Liwang Ma, 6-28-2006, use GDD approach
C         ...CALCULATE ACTUAL DEVELOPMENT RATES FOR EACH GROWTH STAGE
         if (GDDFLG(K).eq.0) then
          DEVRAT(2,K)=STEND(2,K)/DEVRAT(2,K)
	   else 
          DEVRAT(2,K)=STEND(2,K)/GSGDD(2,K)
         endif
          DO 220 I=3,6
         if (GDDFLG(K).eq.0) then
            DEVRAT(I,K)=(STEND(I,K)-STEND(I-1,K))/DEVRAT(I,K)
	   else
            DEVRAT(I,K)=(STEND(I,K)-STEND(I-1,K))/GSGDD(I,K)
         endif
  220     CONTINUE
C why use 30 here?
         if (GDDFLG(K).eq.0) then
          DEVRAT(7,K)=(1.0D0-STEND(6,K))/30.0D0
	   else
          DEVRAT(7,K)=(1.0D0-STEND(6,K))/300.0D0
	   endif
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  230 CONTINUE
C
C     ...READ THE ROOT GROWTH GENETIC PARAMETERS
      CALL ECHO(INP6)
      DO 240 J=1,NUM
        IF(KEEPIT(PTR,NGNPL,J,K)) THEN
          READ(INP6,*) GSR(K),LWM(K),LWS(K),WCG(K),TBS(K),TOP(K),
     +        GMN(K),CAA(K),CAX(K),ALA(K),ALX(K),WCP(K),RDR(K)
        ELSE
          READ(INP6,*) DUMMY
        ENDIF
  240 CONTINUE
      CLOSE(INP6)
 1000 FORMAT(I4,1X,A30)
      END
C
      LOGICAL FUNCTION KEEPIT(PTR,NGNPL,J,K)
      PARAMETER(MXSPEC=10)
      INTEGER PTR(MXSPEC),NGNPL,J,K
      DO 10 I=1,NGNPL
        IF(PTR(I).EQ.J) THEN
          KEEPIT=.TRUE.
          K=I
          GOTO 20
        ELSE
          KEEPIT=.FALSE.
          K=NGNPL
        ENDIF
   10 CONTINUE
   20 RETURN
      END
C
      SUBROUTINE NEWINIT(NHOR,NDXH2N,TL,HORTHK,THETA,T,XNU,RPOOL,CC,
     +    SOILPP,NHORZ,H)
C
C======================================================================
C
C       PURPOSE:
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C
C
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN PROGRAM MODULE
C
C       PROGRAMMER:  KAREN JOHNSEN
C
C       VERSION:  3.1
C
C======================================================================
C      
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MXCHEM=15,MAXHOR=12,IUNIT=85)
C
      DIMENSION NDXH2N(MAXHOR),TL(MXNOD),HORTHK(MAXHOR),THETA(MXNOD),
     +    T(MXNOD),XNU(MXNOD,25),RPOOL(MXNOD,2),CC(MXNOD,MXCHEM),
     +    SOILPP(8,MAXHOR),H(MXNOD)
      DIMENSION TWAT(MAXHOR),TEM(MAXHOR),TXNU(MAXHOR,25),TH(MAXHOR)
      LOGICAL  WTABLE ! RM Changed from Logical*1 to Logical (old dec style)
C
C     ..CYCLE THROUGH CURRENT HORIZONS AND AVERAGE
      I=1
      IE=0
      WTABLE=.FALSE.
      DO 40 J=1,NHOR
        IN=I
        IE=IE+NDXH2N(J)
        TWAT(J)=0.0D0
        TEM(J)=0.0D0
        TH(J)=0.0D0
C       ..BEGIN SUM OVER HORIZON
        DO 20 I=IN,IE
          FACT=1.0D6/(TL(I)*SOILPP(3,J))
          DO 10 IC=1,14
            IF(I.EQ.IN) TXNU(J,IC)=0.0D0
            IF(IC.GE.1.AND.IC.LE.2) THEN
              TXNU(J,IC)=TXNU(J,IC)+RPOOL(I,IC)*FACT*TL(I)
            ELSEIF(IC.EQ.10) THEN
              TXNU(J,IC)=TXNU(J,IC)+CC(I,12)*THETA(I)*TL(I)/
     +            SOILPP(3,J)
            ELSEIF(IC.EQ.11) THEN
              TXNU(J,IC)=TXNU(J,IC)+CC(I,9)*THETA(I)*TL(I)/SOILPP(3,J)
            ELSEIF(IC.EQ.12) THEN
              TXNU(J,IC)=TXNU(J,IC)+CC(I,10)*THETA(I)*TL(I)/
     +            SOILPP(3,J)
            ELSE
              TXNU(J,IC)=TXNU(J,IC)+XNU(I,IC)*TL(I)
            ENDIF
   10     CONTINUE
          TH(J)=TH(J)+H(I)*TL(I)
          TWAT(J)=TWAT(J)+THETA(I)*TL(I)
          TEM(J)=TEM(J)+T(I)*TL(I)
   20   CONTINUE
C       ..TAKE AVERAGE
        IF(J.EQ.1) THEN
          HTHK=HORTHK(J)
        ELSE
          HTHK=HORTHK(J)-HORTHK(J-1)
        ENDIF
        TH(J)=TH(J)/HTHK
        TWAT(J)=TWAT(J)/HTHK
        TEM(J)=TEM(J)/HTHK
        TXNU(J,6)=0.0D0
        TXNU(J,14)=0.0D0
        DO 30 IC=1,14
          TXNU(J,IC)=TXNU(J,IC)/HTHK
   30   CONTINUE
        IF(TH(J).GE.0.0) WTABLE=.TRUE.
   40 CONTINUE
C
C     ..CHECK FOR TILLAGE ADJUSTMENT TO HORIZONS AND CORRECT
      NH=NHOR
      IF(NHOR.GT.NHORZ) THEN
        NH=NHORZ
        NDIF=NHOR-NHORZ
        TH(1)=TH(1)*HORTHK(1)
        TWAT(1)=TWAT(1)*HORTHK(1)
        TEM(1)=TEM(1)*HORTHK(1)
        DO 60 J=2,NDIF+1
          HTHK=HORTHK(J)-HORTHK(J-1)
          TH(1)=TH(1)+TH(J)*HTHK
          TWAT(1)=TWAT(1)+TWAT(J)*HTHK
          TEM(1)=TEM(1)+TEM(J)*HTHK
          DO 50 IC=1,14
            IF(J.EQ.2) TXNU(1,IC)=TXNU(1,IC)*HORTHK(1)
            TXNU(1,IC)=TXNU(1,IC)+TXNU(J,IC)*HTHK
   50     CONTINUE
   60   CONTINUE
        HTHK=HORTHK(NDIF+1)
        TH(1)=TH(1)/HTHK
        TWAT(1)=TWAT(1)/HTHK
        TEM(1)=TEM(1)/HTHK
        DO 70 IC=1,14
          TXNU(1,IC)=TXNU(1,IC)/HTHK
   70   CONTINUE
        DO 90 J=2,NHORZ
          TH(J)=TH(J+NDIF)
          TWAT(J)=TWAT(J+NDIF)
          TEM(J)=TEM(J+NDIF)
          DO 80 IC=1,14
            TXNU(J,IC)=TXNU(J+NDIF,IC)
   80     CONTINUE
   90   CONTINUE
      ENDIF
C
C     ..OUTPUT
      OPEN(UNIT=IUNIT,FILE='NEWINT.OUT',STATUS='UNKNOWN')
      IF(WTABLE) THEN
        WRITE(IUNIT,1000) 0
        DO 100 J=1,NH
          WRITE(IUNIT,1100) TH(J),TEM(J)
  100   CONTINUE
      ELSE
        WRITE(IUNIT,1000) 1
        DO 110 J=1,NH
          WRITE(IUNIT,1100) TWAT(J),TEM(J)
  110   CONTINUE
      ENDIF
C
      WRITE(IUNIT,1200)
      DO 120 J=1,NH
        WRITE(IUNIT,1300)(TXNU(J,IC),IC=1,14),Labp(j),Stabop(j),Frsop(j)
  120 CONTINUE
      CLOSE(UNIT=IUNIT)
C
C
      RETURN
 1000 FORMAT(72('='),/,'=',/,
     +    '=       SOIL HORIZON PHYSICAL PROPERTIES CONDITIONS',/,'=',/,
     +    '= RECORD #   DESCRIPTION',/,'= --------   -----------',/,
     +    '=     1         FORM HYDRAULIC STATE IS PRESENTED',/,'=',T18,
     +    '0 - TENSIOMETRIC POTENTIAL',T60,'[CM OF WATER]',/,'=',T18,
     +    '1 - SOIL WATER CONTENT',T62,'[CM^3/CM^3]',/,
     +    '=     2         HYDRAULIC STATE, TEMPERATURE STATE',T70,
     +    '[C]',/,'=',/,
     +    '=            ... REPEAT RECORD 2 FOR ALL HORIZONS',/,72('='),
     +    /,I2)
 1100 FORMAT(2F10.3)
 1200 FORMAT(72('='),/,'=       ... INITIAL CONC FOR EACH NUTRIENT POOL
     +LISTED BELOW',/,'=',/,'= ITEM  #    DESCRIPTION',/,
     +    '= --------   -----------',/,
     +    '=      1         CR-1 - SLOW RESIDUE POOL',T56,
     +    '[UG-CARBON/G-SOIL]',/,
     +    '=      2         CR-2 - FAST RESIDUE POOL',T56,
     +    '[UG-CARBON/G-SOIL]',/,
     +    '=      3         OM-1 - FAST SOIL HUMUS POOL',T56,
     +    '[UG-CARBON/G-SOIL]',/,
     +    '=      4         OM-2 - MEDIUM SOIL HUMUS POOL',T56,
     +    '[UG-CARBON/G-SOIL]',/,
     +    '=      5         OM-3 - SLOW SOIL HUMUS POOL',T56,
     +    '[UG-CARBON/G-SOIL]',/,
     +    '=      6         CO2  - CARBON SINK POOL',T56,
     +    '[UG-CARBON/G-SOIL]',/,
     +    '=      7         HET1 - AEROBIC HETEROTROPHS POPULATION',T54,
     +    '[# ORGANISMS/G-SOIL]',/,
     +    '=      8         AUTO - AUTOTROPHS POPULATION',T54,
     +    '[# ORGANISMS/G-SOIL]',/,
     +    '=      9         HET2 - ANEROBIC HETEROTROPHS POPULATION',
     +    T54,'[# ORGANISMS/G-SOIL]',/,
     +    '=      10        UREA-N - UREA CONCENTRATION',T61,
     +    '[UG-N/G-SOIL]',/,
     +    '=      11        NO3-N - NITROGEN CONCENTRATION',T61,
     +    '[UG-N/G-SOIL]',/,
     +    '=      12        NH4-N - AMMONIUM CONCENTRATION',T61,
     +    '[UG-N/G-SOIL]',/,
     +    '=      13        PO4-P - PHOSPHATE CONCENTRATION',T61,
     +    '[UG-N/G-SOIL]',/,'=      14        N2  - NITROGEN SINK POOL',
     +    T61,'[UG-N/G-SOIL]',/,'=',/,
     +    '=       ...REPEAT FOR EACH HORIZON',/,72('='))
 1300 FORMAT(5F8.1,F5.1,3F11.1,8F9.3)
      END
C
      SUBROUTINE OUTMSEA(OMSEA)
C
C======================================================================
C
C       PURPOSE:  PRINTS OUT MSEA SPECIFIC OUTPUT
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       OMSEA  I  SAVED VALUES FOR MSEA OUTPUT [MIXED UNITS]
C
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN PROGRAM MODULE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  3.1
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXANA=135)
C
      DOUBLE PRECISION OMSEA(MXANA)
      CHARACTER*7, XSG
      LOGICAL FIRST6
      SAVE FIRST6
      DATA FIRST6 /.TRUE./
C
      IF(FIRST6) THEN
        WRITE(83,1300)
        WRITE(83,1000) (I,I=3,MXANA)
        WRITE(83,1100)
        FIRST6=.FALSE.
      ENDIF
C
      DO I=1,MXANA
        if (ABS(OMSEA(I)).lt.1.0d-24) OMSEA(I)=0.0D0
      ENDDO
      WRITE(83,1200) (OMSEA(I),I=1,MXANA)
      write(xsg,'(i7.7)') int(OMSEA(1)*1.D3)
      IF (OMSEA(88).GT.0.0D0) WRITE(109,FMT=101) XSG(3:7),
     + OMSEA(88),OMSEA(86),OMSEA(85),OMSEA(3)*10.0D0,OMSEA(89),OMSEA(90)
      DO 10 I=1,MXANA
        OMSEA(I)=0.0D0
   10 CONTINUE
      RETURN
c 1000 FORMAT(3X,'(1)',7X,'(2)',12(5X,'(',I2,')'),26(8X,'(',I2,')'),59(7
c     +    X,'(',I2,')'),(7x,'(',i3,')'))
101   FORMAT(A5,6(F6.1))
 1000 FORMAT(3X,'(1)',9X,'(2)',97(12X,'(',I2,')'),36(11x,'(',I3,')'))
 1100 FORMAT(2152('='))
C
c 1200 FORMAT(F8.3,1X,12(F8.4,1x),27(G11.3,1x),4(F10.2,1x),36(G10.3,1x),
c     +       4(F8.4,3x),16(F8.3,3x))
 1200 FORMAT(F8.3,1X,134(G15.6,1x))
 1300 FORMAT('1) TIME (YEAR.DAY)',T42,
     +    '21) NITROGEN UPTAKE (KG-N/HA/DAY)',T80,
     +    '41) TOTAL ABOVE GROUND BIOMASS (KG/HA)',T120,
     +    '61) WATERTABLE DEPTH (CM)',T160,
     +    '81) TALL REFERENCE ET (CM)',T200,
     +    '101) N2O EMISSION FROM NITRIFICATION (Kg N/ha/day)',T260,
     +    '121) DRP LOSS TILE (G-P/HA/DAY)',/
     +    '2) STORED SOIL WATER (CM)',T42,
     +    '22) TOTAL CROP ABOVEGROUND (KG-N/HA)',T80,
     +    '42) TOTAL BELOW GROUND BIOMASS (KG/HA)',T120,
     +    '62) PLANT HEIGHT (CM)',T160,
     +    '82) SHORT REFERENCE ET (CM)',T200,
     +    '102) NxO EMISSION FROM NITRIFICATION (Kg N/ha/day)',T260,
     +    '122) PP LOSS TILE (G-P/HA/DAY)' ,/
     +     '3) PRECIPITATION (CM/DAY)',T42,
     +    '23) TOTAL CROP BELOWGROUND (KG-N/HA)',T80,
     +    '43) LEAF AREA INDEX',T120,
     +    '63) DEPTH OF ROOTS (CM)',T160,
     +    '83) POTENTIAL CROP ET (PET) (CM)',T200,
     +    '103) GHG ABSORBED FROM NITRIFICATION (KG N/HA/DAY)',T260,
     +    '123) PLANT P UPTAKE (G-P/HA/DAY)',/
     +    '4) IRRIGATION (CM/DAY)',T42,'24) TOTAL IN GRAIN (KG-N/HA)',
     +    T80,'44) BIOMASS OF GRAIN (KG/HA)',T120,
     +    '64) PLANT TEMPERATURE STRESS',T160,
     +    '84) ACTUAL ET (CM)',T200,
     +    '104) GHG ABSORBED FROM DENITRIFICAITON (KG N/HA/DAY)',T260,
     +    '124) DRP LOSS LATERAL FLOW (G-P/HA/DAY)',/
     +    '5) INFILTRATION (CM/DAY)',
     +    T42,'25) FROM N-FIXATION (KG-N/HA/DAY)',T80,
     +    '45) PLANT GROWTH STAGE (0-1)',T120,
     +    '65) LEAF BIOMASS (G/PLANT)',T160,
     +    '85) MINIMUM AIR TEMPERATURE (oC)',T200,
     +    '105) SNOW MELT (CM)',T260,
     +    '125) PP LOSS LATERAL FLOW (G-P/HA/DAY)',/
     +    '6) ACTUAL EVAPORATION (CM/DAY)',T42,
     +    '26) DENITRIFICATION (KG-N/HA/DAY)',T80,
     +    '46) PEST #1 APPLIED (KG/HA/DAY)',T120,
     +    '66) STEM BIOMASS (G/PLANT)',T160,
     +    '86) MAXIMUM AIR TEMPERATURE (oC)',T200,
     +    '106) POTENTIAL WATER UPTAKE (DSSAT, CM)',T260,
     +    '126) DRP LOSS DEEP SEEPAGE (G-P/HA/DAY)',/
     +    '7) ACTUAL TRANSPIRATION (CM/DAY)',T42,
     +    '27) VOLATILIZATION (KG-N/HA/DAY)',T80,
     +    '47) PEST #1 IN SOIL (KG/HA)',T120,
     +    '67) ROOT BIOMASS (G/PLANT)',T160,
     +    '87) MEAN AIR TEMPERATURE (oC)',T200,
     +    '107) PESTICIDE #1 ERODED (KG/HA/DAY)',T260,
     +    '127) PP LOSS DEEP SEEPAGE (G-P/HA/DAY)',/
     +    '8) POTENTIAL EVAPORATION(CM/DAY)',T42,
     +    '28) MINERALIZATION (KG-N/HA/DAY)',T80,
     +    '48) PEST #1 IN TILE DRAINAGE (KG/HA/DAY)',T120,
     +    '68) SEED BIOMASS (G/PLANT)',T160,
     +    '88) SOLAR RADIATION (MJ/M2/DAY)',T200,
     +    '108) PESTICIDE #2 ERODED (KG/HA/DAY)',T260,
     +    '128) PLANT P STRESS (0-1) (1-NO STRESS,0-FULL STRESS) ',/
     +    '9) POTENTIAL TRANSPIRATION (CM/DAY)',T42,
     +    '29) N IN RUNOFF (KG-N/HA/DAY)',T80,
     +    '49) PEST #1 IN DEEP SEEPAGE (KG/HA/DAY)',T120,
     +    '69) STANDING DEAD BIOMASS (G/PLANT)',T160,
     +    '89) RELATIVE HUMIDITY (%)',T200,
     +    '109) PESTICIDE #3 ERODED (KG/HA/DAY)',T260,
     +    '129) PLANT P (KG/HA/DAY)',/
     +    '10) DEEP SEEPAGE (CM/DAY)',T42,
     +    '30) N IN DEEP SEEPAGE (KG-N/HA/DAY)',T80,
     +    '50) PEST #1 IN RUNOFF (KG/HA/DAY)',T120,
     +    '70) LITTER BIOMASS (G/PLANT)',T160,
     +    '90) WINDSPEED (KM/DAY)',T200,
     +    '110) SOIL NH4 ERODED (KG/HA/DAY)',T260,
     +    '130) AIR TEMPERATURE AT 2:00 PM (oC)'/
     +    '11) TILE DRAINAGE (CM/DAY)',T42,
     +    '31) N IN TILE DRAINAGE (KG-N/HA/DAY)',T80,
     +    '51) PEST #2 APPLIED (KG/HA/DAY)',T120,
     +    '71) DEAD ROOT BIOMASS (G/PLANT)',T160,
     +    '91) TOTAL ICE IN SOIL (CM)',T200,
     +    '111) ORGANIC-N ERODED (KG/HA/DAY)',T260,
     +    '131) CURRENT LEAF TEMPERATURE  AT 2:00 PM (oC)'/
     +    '12) RUNOFF (CM/DAY)',T42,
     +    '32) SLOW RESIDUE POOL (KG-N/HA)',T80,
     +    '52) PEST #2 IN SOIL (KG/HA)',T120,
     +    '72) SURFACE RESIDUE MASS (KG/HA)',T160,
     +    '92) SNOW DEPTH (CM)',T200,
     +    '112) ORGANIC-C ERODED (KG/HA/DAY)',T260,
     +    '132) LEAF TEMPERATURE OF NON-TRANSPIRING PLANT (oC)'/
     +    '13) PLANT WATER STRESS',T42,
     +    '33) FAST RESIDUE POOL (KG-N/HA)',T80,
     +    '53) PEST #2 IN TILE DRAINAGE (KG/HA/DAY)',T120,
     +    '73) STANDING DEAD RESIDUE MASS (KG/HA)',T160,
     +    '93) SURFACE RESIDUE TEMP (oC)',T200,
     +    '113) TOTAL SEDIMENT ERODED (KG/HA/DAY)',T260,
     +    '133) LEAF TEMPERATURE OF FULLY TRANSPIRING PLANT (oC)'/
     +    '14) PLANT USABLE N IN ROOT DEPTH(KG-N/HA)',T42,
     +    '34) FAST HUMUS POOL (KG-N/HA)',T80,
     +    '54) PEST #2 IN DEEP SEEPAGE (KG/HA/DAY)',T120,
     +    '74) DEADEND MACROPORE PERCENTAGE',T160,
     +    '94) FROZEN DEPTH (CM)',T200,
     +    '114) FRACTION OF RESIDUE COVER',T260,
     +    '134) CROP WATER STRESS INDEX (CSWI)'/
     +    '15) FERTILIZER APPLIED (KG-N/HA/DAY)',T42,
     +    '35) TRANSITION HUMUS POOL (KG-N/HA)',T80,
     +    '55) PEST #2 IN RUNOFF (KG/HA/DAY)',T120,
     +    '75) N IN LATERAL FLOW (KG-N/HA/DAY)',T160,
     +    '95) THAWING DEPTH (CM)',T200,
     +    '115) WATER ADDED DUE TO USING MEASURED SWC (CM)',T260,
     +    '135) INORGANIC N ONTO ION EXCHANGE SITES (KG N/HA/DAY)'/
     +    '16) MANURE APPLIED (KG-N/HA/DAY)',T42,
     +    '36) SLOW HUMUS POOL (KG-N/HA)',T80,
     +    '56) PEST #3 APPLIED (KG/HA/DAY)',T120,
     +    '76) LATERAL WATER FLOW (CM/DAY)',T160,
     +    '96) DAILY CH4 EMISSION (KG-C/HA/DAY)',T200,
     +    '116) PEST #1 IN LATERAL FLOW (KG/HA/DAY)'/
     +    '17) N IN IRRIGATION WATER (KG-N/HA/DAY)',T42,
     +    '37) AEROBIC HETEROTROPHS (KG-N/HA)',T80,
     +    '57) PEST #3 IN SOIL (KG/HA)',T120,
     +    '77) DAILY IMMOBILIZATION (KG-N/HA/DAY)',T160,
     +    '97) DAILY N2O EMISSION (Kg-N/HA/DAY)',T200,
     +    '117) PEST #2 IN LATERAL FLOW (KG/HA/DAY)'/
     +    '18) N IN RAIN WATER (KG-N/HA/DAY)',T42,
     +    '38) AUTOTROPHS (KG-N/HA)',T80,
     +    '58) PEST #3 IN TILE DRAINAGE (KG/HA/DAY)',T120,
     +    '78) DAILY CO2 RELEASE (KG/HA/DAY)',T160,
     +    '98) DAILY NxO EMISSION (KG-N/HA/DAY)',T200,
     +    '118) PEST #3 IN LATERAL FLOW (KG/HA/DAY)'/
     +    '19) SON FROM DEAD ROOTS (KG-N/HA/DAY)',T42,
     +    '39) ANEROBIC HETEROTROPHS (KG-N/HA)',T80,
     +    '59) PEST #3 IN DEEP SEEPAGE (KG/HA/DAY)',T120,
     +    '79) TOTAL SOIL HUMUS C (KG/HA)',T160,
     +    '99) TOTAL SOIL INORGANIC N (Kg-N/ha)',T200,
     +    '119) DRP LOSS RUNOFF (G-P/HA/DAY)',/
     +    '20) SON FROM SOIL RESIDUE (KG-N/HA/DAY)',T42,
     +    '40) PLANT NITROGEN STRESS',T80,
     +    '60) PEST #3 IN RUNOFF (KG/HA/DAY)',T120,
     +    '80) TOTAL SOIL RESIDUE C (KG/HA)',T160,
     +    '100) WATER STRESS FOR LEAF EXPANSION',T200,
     +    '120) PP LOSS RUNOFF(G-P/HA/DAY)',/)
C
      END
C
      SUBROUTINE STATIN(XNU,NN,TL,BD,RPOOL,SOILHP,SOILPP,CC,CONCX2,
     +    THETA,T,RM,SDEAD,RCN,SDCN,NUTEQ,NDXN2H,H,AEF,SNOWPK,FRACOM,
     +    RPOP,FTR,FPW,SOLTP1,SOLTP2,FRACON,CN) !,THETAI,PORI)
C
C======================================================================
C
C       PURPOSE:  READS BINARY FILE THAT CONTAINS ENDING STATE OF THE
C             CARBON POOLS IN THE NUTRIENT MODEL WHEN LAST TERMINATED.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       XNU
C       NN
C       NFILE  P  UNIT NUMBER FOR BINARY FILE
C
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN PROGRAM MODULE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  3.1
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MAXHOR=12,MXCHEM=15,MXPEST=3,IFILE=85,CARBCV=
     +    1.0D0/0.58D0,MAXSCT=11)
C
      DOUBLE PRECISION XNU(MXNOD,25),TXNU(MXNOD,25),BD(NN),
     +    CC(MXNOD,MXCHEM),TCC(MXNOD,MXCHEM),SOILHP(13,MAXHOR),
     +    SOILPP(8,MAXHOR),THETA(MXNOD),T(MXNOD),CONCX2(MXNOD,MXPEST),
     +    TCONCX(MXNOD,MXPEST),TL(NN),RPOOL(MXNOD,2),CN(9),
     +    TSOILHP(13,MAXHOR),TSOILPP(8,MAXHOR),TH(MXNOD),TT(MXNOD),
     +    H(MXNOD),FRACOM(MXNOD),RPOP(3),FPW(MXNOD),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3),TSOLTP1(MAXHOR,5),FRACON(MXNOD),
     +    TSOLTP2(MAXSCT,MAXHOR,3),THETAI(MXNOD),PORI(MXNOD)
C
      INTEGER NDXN2H(MXNOD),NUTEQ(13)
C
      OPEN(IFILE,FILE='STATE.DAT',STATUS='OLD',ERR=90
     +    )
C
      PRINT*,' READING FROM BINARY FILE STATE.BIN'
      READ(IFILE,*) NNTMP,TXNU,TSOILHP,TSOILPP,TCC,TCONCX,TH,TT,TRM,
     +    TSDEAD,TRCN,TSDCN,TSNOWPK,TFTR,TSOLTP1,TSOLTP2 !,THETAI,PORI
C
      CLOSE(IFILE)
      IF(NNTMP.NE.NN) THEN
        PRINT*,'FILE STATE.BIN DOES NOT HAVE THE SAME NUMBER OF LAYERS'
        PRINT*,'AS THE SYSTEM WROTE WHEN THE FILE WAS CREATED.  DISABLE'
        PRINT*,'THE SWITCH TO ACTIVATE READ OR CORRECT FILE.'
        PRINT*
        PRINT*,'NUMBER READ:',NNTMP,'    NUMBER EXPECTED:',NN
        PRINT*
        STOP
      ENDIF
C
C     ..RESET HORIZON SPECIFIC STUFF
      DO 30 J=1,MAXHOR
        IF(NUTEQ(3).EQ.1) THEN
          DO 10 I=1,13
            SOILHP(I,J)=TSOILHP(I,J)
   10     CONTINUE
        ENDIF
        IF(NUTEQ(4).EQ.1) THEN
          DO 20 I=1,8
            SOILPP(I,J)=TSOILPP(I,J)
   20     CONTINUE
        ENDIF
   30 CONTINUE
C
C     ..RESET NUMERICAL LAYER SPECIFIC STUFF
      DO 80 J=1,NN
        JN=NDXN2H(J)
        IF(NUTEQ(4).EQ.1) BD(J)=SOILPP(3,JN)
        IF(NUTEQ(7).EQ.1) THEN
          H(J)=TH(J)
          THETA(J)=WC(H(J),SOILHP(1,JN),JN,0)
          THETA(J)=MIN(THETA(J),SOILHP(6,JN)*AEF)
          FPW(J)=1.0D2*THETA(J)/(1.0D0-SOILPP(3,JN)/SOILPP(2,JN))
        ENDIF
        IF(NUTEQ(5).EQ.1) THEN
          DO 40 I=1,9
            XNU(J,I)=TXNU(J,I)/BD(J)
   40     CONTINUE
        ENDIF
        IF(NUTEQ(6).EQ.1) THEN
          DO 50 I=10,13
            XNU(J,I)=TXNU(J,I)/THETA(J)
   50     CONTINUE
          CC(J,9)=XNU(J,11)
          CC(J,10)=XNU(J,12)
          CC(J,12)=XNU(J,10)
          TCC(J,9)=TXNU(J,11)
          TCC(J,10)=TXNU(J,12)
          TCC(J,12)=TXNU(J,10)
        ENDIF
        IF(NUTEQ(8).EQ.1) T(J)=TT(J)
        IF(NUTEQ(9).EQ.1) THEN
          DO 60 I=1,MXCHEM
            CC(J,I)=TCC(J,I)/THETA(J)
   60     CONTINUE
          IF(NUTEQ(6).EQ.0) THEN
            CC(J,9)=XNU(J,11)
            CC(J,10)=XNU(J,12)
            CC(J,12)=XNU(J,10)
          ELSE
            XNU(J,11)=CC(J,9)
            XNU(J,12)=CC(J,10)
            XNU(J,10)=CC(J,12)
          ENDIF
        ENDIF
        IF(NUTEQ(10).EQ.1) THEN
          DO 70 I=1,MXPEST
            CONCX2(J,I)=TCONCX(J,I)/BD(J)
   70     CONTINUE
        ENDIF
C
C       ..GET RESIDUE MASS AND CONVERT FROM UG-C/G-SOIL ==> G-C/CM**2
        IF(NUTEQ(4).EQ.1) THEN
          FACT=1.0D6/(TL(J)*BD(J))
          RPOOL(J,1)=XNU(J,1)/FACT
          RPOOL(J,2)=XNU(J,2)/FACT
        ENDIF
C
C       ..UPDATE FRAC ORGANIC MATTER
        TOTWT=(XNU(J,1)+XNU(J,2)+XNU(J,3)+XNU(J,4)+XNU(J,5))*CARBCV
        TOTWT=TOTWT+(XNU(J,7)/RPOP(1)+XNU(J,8)/RPOP(2)+XNU(J,9)/
     +      RPOP(3))*CARBCV
        WHUMUS=TOTWT*TL(J)*BD(J)/1.0D6
        WTSOIL=TL(J)*BD(J)
        FRACOM(J)=WHUMUS/WTSOIL
        TOTON=XNU(J,1)/CN(1)+XNU(J,2)/CN(2)+XNU(J,3)/CN(3)+
     &        XNU(J,4)/CN(4)+XNU(J,5)/CN(5)+XNU(J,7)/RPOP(1)/CN(7)+
     &        XNU(J,8)/RPOP(2)/CN(8)+XNU(J,9)/RPOP(3)/CN(9)
        FRACON(J)=TOTON*TL(J)*BD(J)/1.0D6/WTSOIL
   80 CONTINUE
C
      IF(NUTEQ(11).EQ.1) THEN
        RM=TRM
        RCN=TRCN
        SDEAD=TSDEAD
        SDCN=TSDCN
        FTR=TFTR
      ENDIF
      IF(NUTEQ(12).EQ.1) THEN
        SNOWPK=TSNOWPK
      ELSE
        SNOWPK=0.0D0
      ENDIF
        IF(NUTEQ(8).EQ.1) THEN
            DO I=1,MAXHOR
                DO J=1,5
                SOLTP1(I,J)=TSOLTP1(I,J)
                ENDDO
                DO K=1,MAXSCT
                    SOLTP2(K,I,1)=TSOLTP2(K,I,1)
                    SOLTP2(K,I,2)=TSOLTP2(K,I,2)
                    SOLTP2(K,I,3)=TSOLTP2(K,I,3)
                ENDDO
            ENDDO
        ENDIF
C
      RETURN
   90 PRINT*,'ERROR OPENING THE FILE: STATE.BIN'
      PRINT*
      PRINT*,' CHECK TO MAKE SURE THE FILE IS IN THE CURRENT DIR.'
      STOP
C
      END
C
      SUBROUTINE STATOT(XNU,NN,BD,SOILHP,SOILPP,CC,CONCX2,THETA,T,RM,
     +    SDEAD,RCN,SDCN,H,SNOWPK,FTR,SOLTP1,SOLTP2)  !,THETAI,PORI)
C
C======================================================================
C
C       PURPOSE:  WRITES BINARY FILE THAT CONTAINS ENDING STATE OF THE
C             CARBON POOLS IN THE NUTRIENT MODEL WHEN LAST TERMINATED.
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       XNU
C       NN
C       IFILE  P  UNIT NUMBER FOR BINARY FILE
C
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN PROGRAM MODULE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  3.1
C
C======================================================================
C 
      USE VARIABLE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MAXHOR=12,MXCHEM=15,MXPEST=3,IFILE=85,
     +       MAXSCT=11)
C
      DOUBLE PRECISION XNU(MXNOD,25),TXNU(MXNOD,25),BD(NN),
     +    CC(MXNOD,MXCHEM),TCC(MXNOD,MXCHEM),H(MXNOD),
     +    SOILHP(13,MAXHOR),SOILPP(8,MAXHOR),THETA(MXNOD),T(MXNOD),
     +    CONCX2(MXNOD,MXPEST),TCONCX(MXNOD,MXPEST),SOLTP1(MAXHOR,5),
     +    SOLTP2(MAXSCT,MAXHOR,3),THETAI(MXNOD),PORI(MXNOD)
C
      OPEN(IFILE,FILE='STATE.DAT',STATUS='UNKNOWN',
     +    ERR=50)
C
C     ..WRITE OUT BINARY FILE CONTAINING NUTRIENT STATE FOR VALIDATION
C     USE, ACTUAL VALUES ARE WRITTEN IN G-C FROM ORIG G-C/G-SOIL
      DO 40 J=1,NN
        DO 10 I=1,20
          IF(I.LE.9) THEN
            TXNU(J,I)=XNU(J,I)*BD(J)
          ELSEIF(I.GE.14) THEN
            TXNU(J,I)=XNU(J,I)
          ENDIF
   10   CONTINUE
        TXNU(J,11)=CC(J,9)*THETA(J)
        TXNU(J,12)=CC(J,10)*THETA(J)
        TXNU(J,10)=CC(J,12)*THETA(J)
        DO 20 I=1,MXCHEM
          TCC(J,I)=CC(J,I)*THETA(J)
   20   CONTINUE
        DO 30 I=1,MXPEST
          TCONCX(J,I)=CONCX2(J,I)*BD(J)
   30   CONTINUE
   40 CONTINUE
      WRITE(IFILE,*) NN,TXNU,SOILHP,SOILPP,TCC,TCONCX,H,T,RM,SDEAD,RCN,
     +    SDCN,SNOWPK,FTR,SOLTP1,SOLTP2  !,THETAI,PORI
      CLOSE(UNIT=IFILE)
C
      RETURN
C     ..ERROR CONDITIONS FOR OPENNING FILE
   50 PRINT*,'ERROR OPENING THE FILE: STATE.BIN'
      PRINT*
      PRINT*,' CHECK TO MAKE SURE THE FILE IS IN THE CURRENT DIR.'
      STOP
C
      END
C
      SUBROUTINE OPNINP(INP1,INP2,INP3,INP4,INP5,INP6,INP7,JBDAY,JEDAY,
     +    IYYYB,METMOD,JULBDAY,JULEDAY,IYB,IYE,iweather,scenname)
C
C======================================================================
C
C       PURPOSE:  OPENS ALL THE INPUT FILES FROM NAMES READIN
C
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O DESCRIPTION
C       --------  --- -----------
C       INP1  I/O POINTER TO CNTRL.DAT DATAFILE
C       INP2  I/O POINTER TO RZWQM.DAT DATAFILE
C       INP3  I/O POINTER TO DAYMET.DAT DATAFILE
C       INP4  I/O POINTER TO BRKPNT.DAT DATAFILE
C       INP5  I/O POINTER TO RZINIT.DAT DATAFILE
C       INP6  I/O POINTER TO PLGEN.DAT DATAFILE
C       INP7  I/O POINTER TO SNOW.DAT DATAFILE
C       IPNAME     L  POINTER TO FILE NAMES INPUT FILE
C
C       EXTERNAL REFERENCES:
C
C       CALLED FROM: MAIN PROGRAM MODULE
C
C       PROGRAMMER:  KEN ROJAS
C
C       VERSION:  97
C
C======================================================================
C
      USE VARIABLE
      common /Gleams_hydsol/gleams_clay,gleams_silt,gleams_por,   !chd  GLEAMS
     +        gleams_om1,gleams_sand,gleams_BulkDn,gleams_Dacre, 
     +        gleams_CHS,gleams_ei,gleams_exrain,thirtyRR,  
     +        IBYEAR,IEYEAR
      INTEGER IPNAME,INP1,INP2,INP3,INP4,INP5,INP6,INP7,iweather,iwzone,
     +        ibyear,ieyear
      DOUBLE PRECISION METMOD(8,12),XLAT,XLONG,gleams_clay,gleams_silt,    !chd  GLEAMS
     +    gleams_por,gleams_om1,gleams_sand,gleams_BulkDn,gleams_Dacre, 
     +    gleams_CHS,gleams_ei,gleams_exrain,thirtyRR  
      PARAMETER(IPNAME=80,R2D=180.0D0/3.141592654D0)
      CHARACTER FILENAME*255,SCENNAME*255,string*255,iswpar*1
C CHANGE MADE BY LIWANG MA TO LOCATE ANALYSIS FILE
c      COMMON SCENNAME
C
C ..  OPEN FILE WITH ALL THE NAMES
      OPEN(IPNAME,FILE='IPNAMES.DAT',STATUS='UNKNOWN',ERR=30)
C
C     ..  OPEN CNTRL.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP1,FILE=FILENAME,STATUS='UNKNOWN',ERR=40)
      
      Scnraioname = Filename    ! Debasis

C
C     ..  OPEN RZWQM.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP2,FILE=FILENAME,STATUS='UNKNOWN',ERR=50)
C
C     ..  OPEN DAYMET.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP3,FILE=FILENAME,STATUS='UNKNOWN',ERR=60)
C
C     ..  INITIALIZE DAYMET FILE
      CALL ECHO(INP3)
      read (inp3,1000) string
        k=inumb(string)
      backspace (inp3)
	 if (k.eq.6) then
      READ(INP3,*) IDUM,IDUM,IDUM,IDUM,IDUM,IDUM
	   IWEATHER=0
C	   xlat=40
c	   xlong=104.6383D0   !GREELEY, COLORADO LOCATION
       else if (k.eq.7) then
      READ(INP3,*) IDUM,IDUM,IDUM,IDUM,IDUM,IDUM,IWEATHER
c	   xlong=104.6383D0   !GREELEY, COLORADO LOCATION
       else if (k.eq.9) then
      READ(INP3,*) IDUM,IDUM,IDUM,IDUM,IDUM,IDUM,IWEATHER,xlat1,xlong1
       else if (k.eq.10) then
      READ(INP3,*) IDUM,IDUM,IDUM,IDUM,IDUM,IDUM,IWEATHER,xlat1,xlong1
     +            ,iwzone1
       endif
c        xlat=xlat/r2d
c        xlong=xlong/r2d
c        if (iwzone.eq.0) iwzone=2   !=1 arid <10 in rainfall, =2 semi-arid, 10-20 in rainfall, =3 humid, >20 in rainfall
      CALL ECHO(INP3)
C
C     ..  OPEN BRKPNT.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP4,FILE=FILENAME,STATUS='UNKNOWN',ERR=70)
C
C     ..  OPEN RZINIT.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP5,FILE=FILENAME,STATUS='UNKNOWN',ERR=80)
C
C     ..  OPEN PLGEN.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP6,FILE=FILENAME,STATUS='UNKNOWN',ERR=90)
C
C     ..  OPEN SNOW.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP7,FILE=FILENAME,STATUS='UNKNOWN',ERR=100)
C
C     ..  GET PROJECT ANALYSIS FOR OUTPUT PURPOSES
      READ(IPNAME,1000) SCENNAME
      OPEN(UNIT=83,FILE=SCENNAME,STATUS='UNKNOWN',ERR=110) !,RECL=1650)
C
C     ..  GET SIMULATION PERIOD
      READ(IPNAME,*) IDB,IMB,IYB,IDE,IME,IYE
      ! CHD - update GLEAMS date parameters
      IBYEAR = IYB
      IEYEAR = IYE
      Sday = IDB            !Debasis
      Smon = IMB
      Syear = IYB
      Eday = IDE
      Emon = IME
      Eyear = IYE
C
C     ..  GET METEOROLOGY MODIFIERS MATRIX
      DO 10 I=1,8
        READ(IPNAME,*) (METMOD(I,J),J=1,12)
   10 CONTINUE
C
C ..  GET macropore express fraction
C      READ (IPNAME,*) xpresf
C
C     ..  ALL DONE, ALL FILES OPENED SUCCESSFULLY.
      CLOSE(IPNAME)
C
C     ..  ADJUST DATES FOR SIMULATION PERIOD
      IF(IYB.LT.1900.OR.IYE.LT.1900) THEN
        PRINT*,' <<< ERROR IN DATA IPNAMES >>> DATE BEFORE 1900'
        IF(IYB.LT.1900) IYB=IYB+1900
        IF(IYE.LT.1900) IYE=IYE+1900
      ENDIF
C LIWANG MA
        JULBDAY = JULDATE (IDB,IMB,IYB)
        JULEDAY = JULDATE (IDE,IME,IYE)
C LIWANG MA
      IF(IYB.GT.IYE) THEN
        PRINT*,' <<< ERROR IN DATES >>> YEARS NOT SEQ'
        STOP
      ELSEIF(IYB.EQ.IYE) THEN
        JBDAY=JDATE(IDB,IMB,IYB)
        JEDAY=JDATE(IDE,IME,IYE)
        IF(JBDAY.GT.JEDAY) THEN
          PRINT*,' <<< ERROR IN DATES >>> DATES NOT SEQ'
          STOP
        ENDIF
      ELSE
        JBDAY=JDATE(IDB,IMB,IYB)
        JEDAY=JDATE(IDE,IME,IYE)
        DO 20 IYYY=IYB,IYE-1
          JEDAY=JEDAY+JDATE(31,12,IYYY)
   20   CONTINUE
      ENDIF
      IYYYB=IYB
      RETURN
C
C     ..  ERROR OCCURED WITH FILE OPENNING PROCEDURE PRINT NOTICE
   30 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE IPNAMES.DAT --'
      GOTO 120
   40 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE CNTRL.DAT --'
      GOTO 120
   50 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE RZWQM.DAT --'
      GOTO 120
   60 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE DAYMET.DAT --'
      GOTO 120
   70 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE BRKPNT.DAT --'
      GOTO 120
   80 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE RZINIT.DAT --'
      GOTO 120
   90 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE PLGEN.DAT --'
      GOTO 120
  100 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILE SNOW.DAT --'
      GOTO 120
  110 PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN ANALYSIS FILE --'
  120 CLOSE(IPNAME)
      PRINT*,' USING FILENAME:',FILENAME
      STOP
 1000 FORMAT(A255)
      END
      SUBROUTINE PRNTYR(IUNIT,IYYY)
C
C     THIS SIMPLE ROUTINE WRITES THE THE YEAR OUT TO ANY FILE
      INTEGER IUNIT,IYYY
      WRITE(IUNIT,*) IYYY
      RETURN
      END
      SUBROUTINE DAYJACK(ID,IM,IYYY,ICODE)
      PARAMETER (NUMNDK=5)
      INTEGER ID,IM,IYYY,ICODE
      INTEGER LID,LIM,LIYYY,JSEQDAY,FLAG
      LOGICAL CURSTATE(NUMNDK)
      SAVE LID,LIM,LIYYY,JSEQDAY,CURSTATE
      DATA JSEQDAY/0/,LID/0/,LIM/0/,LIYYY/0/,CURSTATE/NUMNDK*.FALSE./

      CALL DAYJACK_Memory('GET',LID,LIM,LIYYY,JSEQDAY,CURSTATE)
      IF (ID.GT.0) THEN
        IF (LID.NE.ID) JSEQDAY=JSEQDAY+1
        LID=ID
      ENDIF
      IF (IM.GT.0) LIM=IM
      IF (IYYY.GT.0) LIYYY=IYYY

      IF (ICODE.EQ.1) THEN
        CURSTATE(1)=.TRUE.        !NON CONVERGENCE
      ELSE IF (ICODE.EQ.2) THEN
        CURSTATE(2)=.TRUE.        !WATER MASS BALANCE OFF
      ELSE IF (ICODE.EQ.3) THEN
        CURSTATE(3)=.TRUE.        !NITROGEN MASS BALANCE OFF
      ELSE IF (ICODE.EQ.4) THEN
        CURSTATE(4)=.TRUE.        !Carbon MASS BALANCE OFF
      ELSE IF (ICODE.EQ.5) THEN
        CURSTATE(5)=.TRUE.        !Pesticide MASS BALANCE OFF
      ENDIF
c
c      if (jseqday.eq.500) curstate(3)=.true.  !4 testing only
c      if (jseqday.eq.1000) curstate(2)=.true. !4 testing only


      FLAG=0
      DO 10 I=1,NUMNDK
        IF (CURSTATE(I))FLAG=FLAG+2**(I-1) !SET BINARY INDICATOR
 10   CONTINUE

      OPEN(64,FILE='DAYJACK.OUT',STATUS='UNKNOWN')
      WRITE(64,'(I8,4I6)') JSEQDAY,LID,LIM,LIYYY,FLAG
      CLOSE(64)
      CALL DAYJACK_Memory('PUT',LID,LIM,LIYYY,JSEQDAY,CURSTATE)
      RETURN
      END
C
      SUBROUTINE AVERAGES(INP3,TAAVG)
	DOUBLE PRECISION TEMPMIN,TEMPMAX,TAAVG
	INTEGER N,INP3,iday1,iday2,iyear1,iyear2
	CHARACTER FILENAME*255
	PARAMETER (IPNAME=80)
C ..  OPEN FILE WITH ALL THE NAMES
      OPEN(IPNAME,FILE='IPNAMES.DAT',STATUS='UNKNOWN')
C
C     ..  OPEN CNTRL.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
C
C     ..  OPEN RZWQM.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
C
C     ..  OPEN DAYMET.DAT DATAFILE
      READ(IPNAME,1000) FILENAME
      OPEN(INP3,FILE=FILENAME,STATUS='UNKNOWN')
      CALL ECHO(INP3)
      READ(INP3,1000) FILENAME
      CALL ECHO(INP3)
	READ (INP3,*) iday1, iyear1
	READ (INP3,*) iday2, iyear2
	backspace (inp3)
	backspace (inp3)
      taavg=0.0d0
	n=0
	if (iday1.eq.iday2) then
      DO WHILE (.TRUE.)
	READ (INP3,*) iday1, iyear1,tempmin
	taavg=taavg+tempmin
	N=N+1
      END DO
	else
      DO WHILE (.TRUE.)
	READ (INP3,*) iday1, iyear1,tempmin,tempmax
	taavg=taavg+(tempmin+tempmax)/2
	N=N+1
      end do
	endif
      taavg=taavg/N
	close (inp3)
	close (ipname)
 1000 FORMAT(A255)
      END
