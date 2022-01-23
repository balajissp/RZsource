C-----------------------------------------------------------------------
C
C     MAIN CERES DRIVER
C
C     DSSAT Version 4.0
C
C     SEPTEMBER 7, 2001   Gerrit Hoogenboom
C     October 12, 2001 Liwang Ma
C     MARCH 5, 2002, LIWANG MA
c     UPDATED TO 4.0, Sept 30, 2006
c     updated on Jan 31, 2008
C     Adapted for Root Zone Water Quality Model
C
C-----------------------------------------------------------------------
      SUBROUTINE DSSATDRV(XLATR,TMAXR,TMINR,SRADR,REFHTR,WNDHTR,
     &         SWR,STR,SATR,DULR,LLR,DSLT,NNR,NO3R,NH4R,TLTR,
     &         ACTTRN,PET,ACTEVP,PER,PES,CROPR,VARNOR,YRSIMR,PLTPPR,
     &         RWSPCR,AZIRR,SDPTHR,iptyper,totminr,totimm,totden,totvol,
     &         BASE,DMDNIT,FIRST,FIXN,GS,HEIGHT,JGS,RLAI,
     &         PLTSLV,PWRTS,RDFR,RLVR,TRNO3R,WCG,TOTPUP,
     &         RAINR,RUNOFF,DRAINR,DEPIR,ANO3R,ANH4R,TOTOR,TOTPR,
     &         TOTFR,TSHPR,TDAYR,BDR,RZPAR,CGPH,RZOC,RZON,YRSIMRS,
     &         HDATE,
     &         OMSEA,TLAI,CNR,RPOOL,TADRT,VRNAME,crainr,RM,RCNR,
     &         FIRST15,nreps,yieldr,ihtyper,iharvr,
     &         RADHGT,NLAYRI,HPC,TADRTC,CO2R,windrun,
     &         iswpar,sdwtpl,sdage,sprlap,iemrg,pedon,
     &         TOTFERT,TOTMANU,CIRR,salb,iweather,unupr,SSURFT,RCO2,
     &         resage,rwl,rootnr,imagic)


      USE ModuleDefs
      IMPLICIT NONE

      CHARACTER*1 USSWI, PLDS,ISIMI,RNMODE,iswpar,PLME
      CHARACTER*2  CROPR,PRCROP, CG, CROP
      CHARACTER*3  SPECIES
      CHARACTER*5  SLTX
      CHARACTER*4  WSTA,WSTA1
      CHARACTER*6  MODELX,VARNOR,VARNO,ECONO,VARTY
      CHARACTER*8  EXPER
      CHARACTER*12 MODEL
      CHARACTER*10 CROPS(49),CROPD,SLNO,PEDON
      CHARACTER*16 VRNAME,ECONAM
      CHARACTER*255 TITLET,TITSIM
      CHARACTER*50 SLDESC
      CHARACTER*60 ENAME
	CHARACTER*1  MEWTH,MESIC,IDETH,IDETP,IOX,ISWPHO,ISWPOT
	CHARACTER*1 ISWSYM
C      character filename*160
      character*80 LINE
      CHARACTER*120 WTHSTR
      CHARACTER* 12 OUTS,OUTO,OUTG,OUTPN,OUTPC,OUTSC,OUTSN,OUTSR
      CHARACTER* 12 OUTT,OUTD,OUTWAT,OUTSP,OUTM,OUTWTH,OUTSPAM
      CHARACTER* 12 OUTP,OUTSOMC,OUTRSTG,SOUTE,SOUTR,SEVAL,PNBAL
      CHARACTER* 12 PCBAL,SNBAL,SCBAL,SPBAL,SWBAL,PPBAL
      CHARACTER* 12 FILET,FILEA, FILEC, FILEE, FILES
	CHARACTER*255 PATHCR,PATHSR,PATHER
      CHARACTER*255 FILECC,FILEGG

      INTEGER I,JGS(2),NLAYRR,YRSIMR,NLDS,JDAY,NSENS,NSWAB
      INTEGER YRPLT,NYRS,STGDOY(20),NLAYRI,NNR,NLAYRO,ICOUNT1,icount2
      INTEGER TRTNO,CRID,LUNIO,ROTNO,ROTOPT,CRPNO,YEAR,nrep
      INTEGER IFIRST, ITRIM, YRIC, REP,RSEED1,NSWI,NARES,ILEN
      INTEGER YRSIMRS, HDATE(3),NREPs, IPDSSAT, L, MXNOD,mxana
      integer ihtyper,iharvr,HAREND,iptyper,iemrg,iweather,nhour
	INTEGER YREND,ISTAGE, YR, DOY, MDATE, TIMDIF,NHAR,dslayer,imagic
      parameter (MXNOD=300,MXANA=135,dslayer=20)

      REAL CES, CET, CEP, CRAIN, CSD1, CSD2, ICSDUR,salb,GRAINR
      REAL SLNF,SLPF,WRESND,EFINOC,EFNFIX,ICWD,AZIR,BDR(mxnod),TDUL,CN2,
     +     TRNH4U,TRNO3U,RESAMT,SOILNC,DSOILN,TOTNAP,SOILNX,WR(dslayer)
      REAL PGFAC3,TROOT, ANH4R,ANO3R,TOTOR,TOTPR,TOTFR,AMTNIT,TSHPR,
     +  SATR(mxnod),DULR(mxnod),LLR(mxnod),SWR(mxnod),STR(mxnod),
     +  NO3R(mxnod),asimulated(40),UNUPR(mxnod),RXLAI,
     +  NH4R(mxnod),RDFR(mxnod),RLVR(mxnod),SWFCAB(5),TLTR(mxnod),
     +  ZLYR(mxnod),DSLT(dslayer),humc(dslayer),humn(dslayer)
c      REAL XLAT,RWUEP1,REFHT,WINDHT,BETN,CANWH,CANHT,NDEM
      REAL BETN,CANWH,CANHT,NDEM,CANHTC,CANWHC,EFFSW(mxnod),
     &  ESW(dslayer),SWCN2(dslayer),TOTPU(dslayer),TOTPP,
     &  WCGG,TOTPUP,DEPIR,RUNOFF,RAINR,rzon(mxnod),
     &  DRAINR,CGPH(mxnod),RZOC(mxnod),TRDFR,crainr,qsr(mxnod),wuf,TPESW
      REAL SWCON1,SWCON3,SWDF1,SWDF2,TRUNOF,TDRAIN,TOTIR,TOTAPW
     &     ,S0N,ISINB,sdwtpl,sdage,sprlap,wtdep,cumres,dsnc,
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNOX,CIRR,effirr,
     &    oldstress1,oldstress2,TDSOIL,TNSOIL,TMSOIL,
     &    NDTH,WTNOO,NNOFF,WTNNO,WNDOT,DWNOD      

C-RZ
      REAL TOTRTRES,TOTWTNRRS,RM1,RMN1,RTM1,RTMN1
      REAL RTRES(MXNOD),WTNRRS(MXNOD),TOPRES,WTNRES,DEPMAX
	REAL ROWSPC, PLTPOP, SDEPTH, PHINT, DRAIN, PESW,RLV(NL)
      REAL TSW, TLL, TSAT, TRNU, ANO3, ANH4,TLCH,TSON
	REAL SW(NL), ST(NL), NO3(NL), NH4(NL),FAC(NL),RWU(NL)
	REAL RWUMX, TRWUP, PORMIN, SATFAC, RWUEP1,SNO3(NL),SNH4(NL)
	REAL EP, ES, EO, ET, SWFAC, TURFAC, ICRES, TWILEN
      REAL DEC,DAYL,SOC,SNDN,SNUP, HARVFRAC(3), SNOW, EOP
	REAL KEP, KCAN, NSTRES, UNO3(NL), UNH4(NL),XLAI,EP1
	REAL CUMDEP,AINO3,AINH4, IRRAMT,TSWINI,SWINIT(dslayer),TSOC
      REAL CO2,SRAD,TMAX,TMIN,HPC,SRFTEMP   !SRFTEMP NEED TO HAVE VALUES, LIWANG MA
      REAL GRWT,STWT,NUPD,GNAD,CNAD,WFG,TFG,CWAD,RTWTS,RTWTB,STMWTB,wflf

C PARAMETERS FOR CROPGRO NEED TO CHECK THEIR VALUES
	REAL XHLAI,EORATIO,KSEVAP,KTRANS,HS,HTMAX,BIOHALF
C PARAMETERS NEED TO BE DEFINED/PASSED FOR CERES
      REAL LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,avg_hroot
	REAL RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,RZrwu(mxnod),RZtrwup
	REAL NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,WTNFX,srdot
      real TNH4,TNO3,THUMC,THUMN,tnh4no3,conv1,ceo,pronod,nodgr
      double precision hrt(24),hrts(24),hru(24),hrh(24),hrzpar(24)
     +       ,HRTH(24),SSURFT(24),RCO2,resage,rootnr(mxnod),rwl(mxnod)
      DOUBLE PRECISION alaireset, WSI(10), heightset
c      REAL CUMSENSURF,  CUMSENSOIL  !cumul. senesced matter, soil & surf
c      REAL CUMSENSURFN, CUMSENSOILN !cumul. senes. N soil and surface

	INTEGER NOUTDO, NOUTDG, NOUTDS, NOUTDW, NOUTDC
	INTEGER NVEG0,RSTAGE,istress,iresetlai, iresetht1
C
      DOUBLE PRECISION XLATR,TMAXR,TMINR,SRADR,REFHTR,WNDHTR,
     &            ACTTRN,PET,PER,PES,ACTEVP,
     &            PLTPPR,RWSPCR,AZIRR,SDPTHR,
     &            BASE,DMDNIT,FIXN,GS,HEIGHT,RLAI,
     &            PLTSLV,PWRTS,TRNO3R,WCG,yieldr(3),
     &            PCNLR,PCNSTR,PCNRTR,PCNGRNR,
     &            TDAYR,RZPAR,OMSEA(mxana),TLAI,CNR(9),RPOOL(MXNOD,2),
     &            TADRT(2),PCN,S, RMASS, RM, RCNR, FCN, CNEW,
     &            PLHGHT,PLALFA,RADHGT,stem2,oldhgt,TADRTC(2),CO2R,
     &            windrun,stalk,oldheight,totminr,totimm,totden,totvol,
     &            TOTFERT,TOTMANU,RMASS1

      LOGICAL FIRST, FIRST15
      PARAMETER (IPDSSAT =69)
C      DATA FIRST15/.TRUE./
      DATA CROPS/'DRY BEAN  ','PEANUT    ','SOYBEAN   ','COWPEA    ',
     &           'PEA       ','CHICKPEA  ','PIGEONPEA ','PEPPER    ',
     &           'RICE      ','FALLOW    ','MAIZE     ','WHEAT     ',
     &           'MILLET    ','SORGHUM   ','BARLEY    ','CASSAVA   ',
     &           'POTATO    ','TOMATO    ','C3-CROPS  ','C4-CROPS  ',
     &           'BAHIA     ','GRASS-1   ','GRASS-2   ','GRASS-3   ',
     &           'GRASS-4   ','GRASS-5   ','GRASS-6   ','GRASS-7   ',
     &           'GRASS-8   ','BRACHIARIA','SUGARCANE ','AROIDS    ',
     &           'SUNFLOWER ','PINEAPPLE ','TARO      ','TANIER    ',
     &           'COTTON    ','VELVETBEAN','CABBAGE   ','FABA BEAN ',
     &           'CITRUS    ','MILLET-PRO','MILLET-FOX','TRITICALE ',
     &           'CANOLA    ','SUGARBEET ','ALFALFA','BERMUDAGRASS',
     &           'OILCROP-SUN'/
c used for David Nielsen' corn data
c      DATA SWCON1/2.67E-3/,SWCON3/6.68/
c default one from DSSAT
c      DATA SWCON1/1.32E-3/,SWCON3/7.01/
C      DATA SPECIES/'CER'/
c used for UFGA8201 data
c      DATA SWCON1/0.10E-3/,SWCON3/7.01/
c
      PARAMETER (LUNIO  = 125)
c      common/stress/istress !,oldstress1, oldstress2
      common /wuptake/qsr,wtdep,trwup,RZrwu,RZtrwup,istress,avg_hroot
      common/hourweather/ hrt,hrts,hru,hrh,HRZPAR,HRTH
      COMMON/SOILTEMP/TDSOIL,TMSOIL, TNSOIL
      common /wsi/ wsi, alaireset, heightset, iresetlai, iresetht1
      common asimulated
      save oldhgt,PLHGHT,PLALFA
!     ------------------------------------------------------------------
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

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      Type (WeatherType) WEATHER
      TYPE (PlStresType) PlantStres
      TYPE (PLANTVARTYPE)   PLANTVAR
C-----------------------------------------------------------------------
C    CERES Input variables
C-----------------------------------------------------------------------
c
      CALL Y2K_DOY(YRSIMR)
      CALL Y2K_DOY(YRSIMRS)
C      if (iresetht1.eq. 1) userht=height
      IF (FIRST) THEN
      RM1=0.0
	RMN1=0.0
      RTM1=0.0
	RTMN1=0.0
C
        CONTROL % DYNAMIC = runinit
        CONTROL % YRSIM = YRSIMRS
        YRPLT = YRSIMR
        CONTROL % YRPLT = YRPLT
        CONTROL % iemrg = iemrg
	  YREND = -99
        MDATE = -99
c from Alt_Plant.For
      oldheight = 0.0d0
      EORATIO  = 1.0
      KCAN     = 0.85
      KEP      = 1.0
      KSEVAP   = -99.
      KTRANS   = 1.0
      NSTRES   = 1.0
      PORMIN   = 0.02
c      RLV      = 0.0
      RWUEP1   = 1.5
      RWUMX    = 0.03
      SENESCE % ResWt  = 0.0
      SENESCE % ResLig = 0.0
      SENESCE % ResE   = 0.0
      STGDOY   = 9999999
      UNH4     = 0.0
      UNO3     = 0.0
      XHLAI    = 0.0
      XLAI     = 0.0
c from Alt_plant.for
c
        
	  if (hdate(1).gt.0) CALL Y2K_DOY(HDATE(1))        
        ISTAGE = 7
        CANWH  = 0.0
        CANHT  = 0.0
        CEP     = 0.0
        CES     = 0.0
        CET     = 0.0
        CRAIN   = 0.0
        CSD1    = 0.0
        CSD2    = 0.0
        NDEM    = 0.0
        ceo     = 0.0
        effirr  = 1.0
        TRUNOF  = 0.0
        TDRAIN  = 0.0
        totir   = 0.0
        tswini  = 0.0
        ICSDUR  = 1
        nrep = nreps + 1
        nreps = nreps + 1
        if (first15) then
         CONTROL % RUN = 0
         CONTROL % MULTI = 0
        endif
      IF (CONTROL % MULTI .GE. 1) THEN
c        CALL YR_DOY(control%YRSIM,YR,ISIM)
c        CALL YR_DOY(YRPLT,YRP,ISIMP)
c        control % YRSIM = YRP * 1000 + ISIMP
         control % YRSIM = control % yrplt
      endif

      CONTROL % RUN = control% run + 1
      CONTROL % MULTI = CONTROL % MULTI +1
      CONTROL % REPNO = 1
c	CONTROL % RNMODE = 'N'
	CONTROL % NYRS = 1
C	CONTROL % YRDIF = YRDIF
      CONTROL % CROP   = CROPR
      CONTROL % RNMODE = 'N'
      ISWITCH % MESOM = 'R'
C -------------------------------------------------------------------
      ENAME='NOT SPECIFIED'
      EXPER = 'EXPDSSAT'
      control% exper = exper
      CG = CROPR
c      TITLET = ENAME
c          TITLET = "RZWQM-CERES"//"-"//TITLET
          ENAME  = "RZWQM2"//"-"//ENAME
c	    CROP = CONTROL % CROP
c       control% titlet = titlet
       control% ename = ename
          VARNO  = VARNOR
C          YEAR = (ICHAR(WSTA1(1:1)) - 48)*10 + (ICHAR(WSTA1(2:2)) - 48 )
          SLTX = '   '
          SLNO = '   '
          WSTA = 'RZWQM'
c          PEDON = '   '
          ECONAM = '  '
          WTHSTR = '     '
          NSENS  = 0
          SLDESC = '       '
C-----------------------------------------------------------------------
      if (CONTROL % crop.eq.'MZ') then
          CONTROL % FILEA = exper//".MZA"
          CONTROL % FILET = exper//".MZT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'WH') then
          CONTROL % FILEA = exper//".WHA"
          CONTROL % FILET = exper//".WHT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'BA') then
          CONTROL % FILEA = exper//".BAA"
          CONTROL % FILET = exper//".BAT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'ML') then     !this is pearl millet
          CONTROL % FILEA = exper//".MLA"
          CONTROL % FILET = exper//".MLT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'SB') then
          CONTROL % FILEA = exper//".SBA"
          CONTROL % FILET = exper//".SBT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'PN') then
          CONTROL % FILEA = exper//".PNA"
          CONTROL % FILET = exper//".PNT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'BN') then
          CONTROL % FILEA = exper//".BNA"
          CONTROL % FILET = exper//".BNT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'FB') then
          CONTROL % FILEA = exper//".FBA"
          CONTROL % FILET = exper//".FBT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'CA') then
          CONTROL % FILEA = exper//".CAA"
          CONTROL % FILET = exper//".CAT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'VB') then
          CONTROL % FILEA = exper//".VBA"
          CONTROL % FILET = exper//".VBT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'BR') then
          CONTROL % FILEA = exper//".BRA"
          CONTROL % FILET = exper//".BRT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'CB') then
          CONTROL % FILEA = exper//".CBA"
          CONTROL % FILET = exper//".CBT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'CH') then
          CONTROL % FILEA = exper//".CHA"
          CONTROL % FILET = exper//".CHT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'CP') then
          CONTROL % FILEA = exper//".CPA"
          CONTROL % FILET = exper//".CPT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'PR') then
          CONTROL % FILEA = exper//".PRA"
          CONTROL % FILET = exper//".PRT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'CO') then
          CONTROL % FILEA = exper//".COA"
          CONTROL % FILET = exper//".COT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'TM') then
          CONTROL % FILEA = exper//".TMA"
          CONTROL % FILET = exper//".TMT"
          SPECIES ='GRO'
      endif
c      if (CONTROL % crop.eq.'SC') then
c          CONTROL % FILEA = exper//".SCA"
c          CONTROL % FILET = exper//".SCT"
c          SPECIES ='CAN'
c      endif
c      if (CONTROL % crop.eq.'SU') then
c          CONTROL % FILEA = exper//".SUA"
c          CONTROL % FILET = exper//".SUT"
c          SPECIES ='OIL'
c      endif
      if (CONTROL % crop.eq.'G0') then
          CONTROL % FILEA = exper//".G0A"
          CONTROL % FILET = exper//".G0T"
          SPECIES ='GRO'
      endif
c      if (CONTROL % crop.eq.'PI') then
c          CONTROL % FILEA = exper//".PIA"
c          CONTROL % FILET = exper//".PIT"
c          SPECIES ='ALO'
c      endif
      if (CONTROL % crop.eq.'SG') then
          CONTROL % FILEA = exper//".SGA"
          CONTROL % FILET = exper//".SGT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'PT') then
          CONTROL % FILEA = exper//".PTA"
          CONTROL % FILET = exper//".PTT"
          SPECIES ='SUB'
      endif
      if (CONTROL % crop.eq.'MO') then    !add for proso millet according to Sasi
          CONTROL % FILEA = exper//".MOA"
          CONTROL % FILET = exper//".MOT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'MX') then    !add for fox millet according to Sasi
          CONTROL % FILEA = exper//".MXA"
          CONTROL % FILET = exper//".MXT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'TR') then    !add for triticale according to Sasi
          CONTROL % FILEA = exper//".TRA"
          CONTROL % FILET = exper//".TRT"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'BS') then    !RMarquez - sugarbeet
          CONTROL % FILEA = exper//".BSA"
          CONTROL % FILET = exper//".BST"
          SPECIES ='CER'
      endif
      if (CONTROL % crop.eq.'SU') then    !RMarquez - sunflower
          CONTROL % FILEA = exper//".SUA"
          CONTROL % FILET = exper//".SUT"
          SPECIES ='GRO'
      endif
      if (CONTROL % crop.eq.'AL') then    !RMarquez - alfalfa
          CONTROL % FILEA = exper//".ALA"
          CONTROL % FILET = exper//".ALT"
          SPECIES ='FRM'
      endif
      if (CONTROL % crop.eq.'BM') then    !Bermudagrass
          CONTROL % FILEA = exper//".BMA"
          CONTROL % FILET = exper//".BMT"
          SPECIES ='FRM'
      endif
      if (CONTROL % crop.eq.'SF') then
          CONTROL % FILEA = exper//".SFA"
          CONTROL % FILET = exper//".SFT"
          SPECIES ='CER'
      endif


C-RZ
      PRCROP = CONTROL % CROP
C-----------------------------------------------------------------------
C    Get Additional Planting Information from RZWQM
C-----------------------------------------------------------------------
c          ROWSPC  = REAL(RWSPCR) / 100    'it is correct for GRO model
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%HDATE(1) = HDATE(1)
          if ((iharvr.eq.1).or.(iharvr.eq.3).or.(iharvr.eq.5)) then
          PLANTVAR%HBPC = 0.0     !Harvest grain or root only, no by-product harvested
          PLANTVAR%HPC = HPC      !harvest efficiency is only applied to grain or root
          PLANTVAR%HRPC = 0.0d0   !assume root is havested at this efficiency
          else if ((iharvr.eq.2).or.(iharvr.eq.4)) then
          PLANTVAR%HBPC = HPC     !Harvest biomass/stover at this efficiency
          PLANTVAR%HPC = 100.0    !assume yield is havested at 100% based on Rob Malone's request
          PLANTVAR%HRPC = 0.0d0   !assume root is havested at this efficiency
          else if (iharvr.eq.6) then  !harvest everything both above and below ground
          PLANTVAR%HBPC = HPC      !Harvest biomass/stover at this efficiency
          PLANTVAR%HPC = HPC       !assume yield is havested at this efficiency
          PLANTVAR%HRPC = HPC      !assume root is havested at this efficiency
          endif
          PLANTVAR%ROWSPC  = REAL(RWSPCR)
          PLANTVAR%PLTPOP  = REAL(PLTPPR)
          ROWSPC  = REAL(RWSPCR)
          PLTPOP  = REAL(PLTPPR)
          PLANTVAR%AZIR    = REAL(AZIRR)   ! not needed for CERES?
          PLANTVAR%SDEPTH  = REAL(SDPTHR)
          SDEPTH  = REAL(SDPTHR)
          IF (ROWSPC .GT. 0 .AND. PLTPOP .GT. 0) THEN
          BETN = 1./(ROWSPC*PLTPOP)
          ELSE
          BETN = 0.
          ENDIF
      if (iptyper.eq.0) PLANTVAR%PLME = 'S'
      if (iptyper.eq.1) PLANTVAR%PLME = 'T'
      if (iptyper.eq.2) PLANTVAR%PLME = 'P'
         PLANTVAR%ATEMP = real((tmaxr+tminr)/2.0)
         PLANTVAR%SDWTPL = sdwtpl
         PLANTVAR%SDAGE = sdage
         PLANTVAR%SPRLAP = sprlap
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
C-----------------------------------------------------------------------
      NYRS = 1
      REP = 1
      RSEED1 = 2150
      MEWTH = 'M'
      control % MESIC = 'M'
	NHAR = 1
C      MELI = 'E'
C      CONTROL % FILEIO = 'D:\RZWQM_DSSAT40\IBWA8334.INP'
      ISWITCH % MEEVP = 'R'  !dynamic ET need height
C      ISWITCH % MEPHO = 'C'
      ISWITCH % IPLTI = 'R'
      ISWITCH % IIRRI = 'R'
      ISWITCH % IFERI = 'R'
      ISWITCH % IRESI = 'R'
      ISWITCH % IDETR = 'N'
         if (ihtyper.eq.1) ISWITCH % IHARI = 'M'
         if (ihtyper.eq.3) ISWITCH % IHARI = 'R'
          IDETH = 'N'
C-----------------------------------------------------------------------
C     This subroutine assumes that DS(L) values are depths to the bottom
C     of layer L
C
C     DS(L) can be interactively modified in the sensitivity analysis
C-----------------------------------------------------------------------
      SOILPROP % DS(1) =  5.
      SOILPROP % DS(2) = 15.
      SOILPROP % DS(3) = 30.
      SOILPROP % DS(4) = 45.
      SOILPROP % DS(5) = 60.
      DO I = 6, dslayer
         SOILPROP % DS(I) = SOILPROP % DS(I - 1) + 30.
      END DO
      DO I = 1,dslayer
         DSLT(I) = SOILPROP % DS(I)
      END DO
C added by Liwang Ma, Oct. 22, 2004 to limit RZWQM layers to DS(dslayer)
      SOILPROP % SALB = SALB
	DO I=NNR,1,-1
	   IF (TLTR(I).LE.SOILPROP % DS(dslayer)) THEN
	      NLAYRI=I
	      GO TO 10
	    ENDIF
	END DO
c      NLAYRI  = NNR
10    CALL LYRSET (NLAYRI, TLTR, SOILPROP % NLAYR, SOILPROP % DS, 
     &             SOILPROP % DLAYR, DEPMAX)
      CALL realMATCH (NLAYRI,TLTR,LLR,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,DULR,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,SATR,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,BDR,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,CGPH,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,RZOC,SOILPROP % NLAYR, SOILPROP % DS)
c      CALL realMATCH (NLAYRI,TLTR,RZON,SOILPROP % NLAYR, SOILPROP % DS)

      dsnc = min(210.0,depmax)    ! set to maximum 210 cm for the 10 layers. should we set to depmax?
      cumdep = depmax
C-GH  WCG sets root growth distribution.  Should be transferred from RZMAN
c        WCGG = REAL(WCG)
C          SOILPROP % NLAYR = NLAYR
        DO I = 1, SOILPROP % NLAYR
C          SOILPROP % DS(I) = DS(I)
C          SOILPROP % DLAYR(I) = DLAYR(I)
          SOILPROP % LL(I)  = LLR(I)
          SOILPROP % DUL(I) = DULR(I)
          SOILPROP % SAT(I) = SATR(I)
          SOILPROP % BD(I) = BDR(I)
          SOILPROP % PH(I) = CGPH(I)
          SOILPROP % OC(I) = RZOC(I)
          TSOC   = TSOC +SOILPROP%OC(I)*1000.0 * SOILPROP%BD(I)
     &             *SOILPROP%DLAYR(I)
c liwang ma 8/13/2003
          FAC(i)     = 1.0/(SOILPROP % BD(i)*1.E-01*SOILPROP % DLAYR(i))
        ENDDO
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,RZOC,NLAYRI,TLTR)
C-GH
C-----------------------------------------------------------------------
C    Potential root growth distribution.
C    Allen Jones et al.
C-----------------------------------------------------------------------
C          IF (I .EQ. 1) THEN
C             SHF(1) = 1.00
C          ELSE
C             SHF(I) = (1.0 - (DS(I-1)+0.5*(DS(I)-DS(I-1)))/DEPMAX)**WCGG
C          ENDIF
        NSWAB = 5
c not needed for CERES???
c        DO I = 1,NSWAB
c          SWFCAB(I) = 1.0
c        END DO
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
C    Pass weather variables
C    ISTRESS=0, USE DSSAT WATER STRESS, ET FROM DSSAT
C    ISTRESS=1, USE NIMAH-HANKS T FOR UPTAKE AND WATER STRESS 
C    ISTRESS=2, USE (NIMAH-HANKS T+AE)/PET
C    ISTRESS=3, USE Nimah-Hanks/SHAW ET
C    ISTRESS=4, USE potenital water potential/SHAW ET
C-----------------------------------------------------------------------
C      IF (ISTRESS.EQ.1) THEN
C          TRWUP=RZtrwup
C      ELSE IF (ISTRESS.EQ.2) THEN
C          EOP=EO
C          TRWUP=RZTRWUP+ES*0.1
C      ENDIF
      CONTROL % YRDOY  = YRSIMR
C      YRDOY  = CONTROL % YRDOY
      WEATHER % XLAT   = REAL(XLATR)
      WEATHER % TMAX   = REAL(TMAXR)
      WEATHER % TMIN   = REAL(TMINR)
      WEATHER % SRAD   = REAL(SRADR)
c      WEATHER % TDAY = (WEATHER%TMAX+WEATHER%TMIN)/2
c      WEATHER % TAVG = (WEATHER%TMAX+WEATHER%TMIN)/2
      WEATHER % TA = (WEATHER%TMAX+WEATHER%TMIN)/2.0
      srftemp = str(1)  !(WEATHER%TMAX+WEATHER%TMIN)/2.0
      WEATHER % WINDSP = real(windrun)
      IF (RZPAR.EQ.0.0D0) RZPAR    = 2.0D0 * SRADR
      WEATHER % TGROAV = WEATHER % TAVG
      WEATHER % PAR    = REAL(RZPAR)
      WEATHER % REFHT  = REAL(REFHTR)
      WEATHER % WINDHT = REAL(WNDHTR)
      WEATHER % RAIN   = RAINR * 10.0
C-----------------------------------------------------------------------
C    Pass water balance variables
C-----------------------------------------------------------------------
C
      snow = real(omsea(92)*10.0d0)
      DRAIN  = DRAINR * 10.0
      RUNOFF = RUNOFF * 10.0
      DEPIR  = DEPIR * 10.0
      IRRAMT = DEPIR
      TOTAPW = CIRR
            unh4=0.0
            uno3=0.0
            trnu=0.0
C-----------------------------------------------------------------------
C    Pass soil layer variables
C-----------------------------------------------------------------------
      CALL realMATCH (NLAYRI,TLTR,SWR,SOILPROP % NLAYR, SOILPROP %DS)
      CALL realMATCH (NLAYRI,TLTR,STR,SOILPROP % NLAYR, SOILPROP %DS)
      CALL realMATCH (NLAYRI,TLTR,NO3R,SOILPROP % NLAYR, SOILPROP %DS)
      CALL realMATCH (NLAYRI,TLTR,NH4R,SOILPROP % NLAYR, SOILPROP %DS)
      CALL realMATCH (NLAYRI,TLTR,RZOC,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,RZON,SOILPROP % NLAYR, SOILPROP % DS)
      CALL realMATCH (NLAYRI,TLTR,RZrwu,SOILPROP % NLAYR, SOILPROP % DS)
c      CALL realMATCH (NLAYRI,TLTR,TOTPUP,SOILPROP % NLAYR, SOILPROP %DS)
      TSW = 0.0
      TLL = 0.0
      TDUL = 0.0
      TSAT = 0.0
      TRNU = 0.0
      ANO3 = ANO3R
      ANH4 = ANH4R
      AMTNIT = TOTFR
      TLCH = TOTPR
      TSON = TSHPR
c     TSIN = ANH4 + ANO3
      DO I = 1,SOILPROP % NLAYR
        CONV1=SOILPROP % DLAYR(I)*soilprop%BD(I)*0.1
        HUMC(I) = RZOC(I)*conv1*10000.
        HUMN(I) = RZON(I)*conv1*10000.
        SW(I)    = SWR(I)
        ST(I)    = STR(I)
        NH4(I)   = NH4R(I)*FAC(I)*SOILPROP % DLAYR(I)
        NO3(I)   = NO3R(I)*FAC(I)*SOILPROP % DLAYR(I)
        SNO3(I) = NO3(I)/FAC(I)
        SNH4(I) = NH4(I)/FAC(I)
      IF (ISTRESS.GE.1.AND.ISTRESS.LT.4) THEN
          RWU(I)=RZrwu(I)
      ENDIF
c        TOTPU(I) = TOTPUP(I)
        TLL = TLL + SOILPROP % LL(I)*SOILPROP % DLAYR(I)
        TDUL = TDUL + SOILPROP % DUL(I)*SOILPROP % DLAYR(I)
c        EFFSW(I) = MIN(DUL(I),SW(I))
        EFFSW(I) = SW(I)
        TSW = TSW + EFFSW(I)*SOILPROP % DLAYR(I)
        TSAT = TSAT + SOILPROP % SAT(I)*SOILPROP % DLAYR(I)
        ESW(I) = SOILPROP%DUL(I)-SOILPROP%LL(I)
      END DO
      PESW = max(TSW - TLL,0.0)
      TPESW = TDUL - TLL
c      PESW = min(TDUL - TLL,tsw-tll)
c      PESW = MAX(0.0,PESW)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Compute SWCN2 for each soil layer.  Adjust SWCN2 for extremely
C     high LL to avoid water uptake limitations.
C-----------------------------------------------------------------------

      DO I = 1,dslayer
        SWCN2(I) = 120 - 250 * SOILPROP % LL(I)
        IF (SOILPROP % LL(I) .GT. 0.30) SWCN2(I) = 45.0
      END DO
C-----------------------------------------------------------------------

c   'actual transpiration
c      EP      = REAL(PET*10)  ! potential transpiration
      EP      = REAL(ACTTRN)*10.0
CZ-MA
c   'actual evaporation
      ES      = REAL(ACTEVP)*10.0
c     'potential ET
      EO      = REAL(PET+PER+PES)*10.0
c     'actual ET
      ET      = REAL(ACTTRN+ACTEVP)*10.0
C      'POTENTIAL TRANSPORATION
      EOP = real(PET) * 10.0
C potential and actual in DSSAT model
c change by Liwang Ma from CEP = CEP + EP because EP was used for both
c
C-----------------------------------------------------------------------
C    Pass weather variables
C    ISTRESS=0, USE DSSAT WATER STRESS, ET FROM DSSAT
C    ISTRESS=1, USE NIMAH-HANKS T FOR UPTAKE AND WATER STRESS 
C    ISTRESS=2, USE (NIMAH-HANKS T+AE)/PET
C    ISTRESS=3, USE Nimah-Hanks/SHAW ET
C    ISTRESS=4, USE potential root water uptake/SHAW ET
C-----------------------------------------------------------------------
C      IF ((ISTRESS.EQ.1).or.(istress.eq.3).or.(istress.eq.4)) THEN
      IF ((ISTRESS.EQ.1).or.(istress.eq.3)) THEN
          TRWUP=RZtrwup
      ELSE IF (ISTRESS.EQ.2) THEN
          EOP=EO
          TRWUP=RZTRWUP+ES*0.1
      ENDIF
c
      CEP     = CEP + EP
      CES     = CES + ES
      CET     = CET + ET
      ceo     = ceo + eo
      ICSDUR  = ICSDUR + 1

      CRAIN = CRAIN + WEATHER % RAIN
      TRUNOF  = TRUNOF + RUNOFF
      TDRAIN  = TDRAIN + DRAIN
      TOTIR   = TOTIR + DEPIR
C      CRAIN   = CRAINR * 10.0 - TOTIR
c      write (666,*) trwu, ep, es, eo, et
!     Soil profile accumulations.
      TNH4   = 0.0
      TNO3   = 0.0
      THUMC  = 0.0
      THUMN  = 0.0
      DO L = 1, soilprop%NLAYR
        TNH4  = TNH4  + SNH4(L)
        TNO3  = TNO3  + SNO3(L)
        THUMC = THUMC + HUMC(L)
        THUMN = THUMN + HUMN(L)
      ENDDO
      TNH4NO3 = TNH4 + TNO3
      TOTAML = real(totvol)
      TMINERALIZE = real(totminr)
      TIMMOBILIZE = real(totimm)
      TNOX = real(totden) 
      TOTNAP = REAL(TOTFERT)
        CALL GetPutSPAM ("PUT", CEO, CEP, CES, EP)
        CALL GetPutIRRIG("PUT", EFFIRR, TOTIR)
C-----------------------------------------------------------------------
      IF (FIRST) THEN
C-----------------------------------------------------------------------
C    Read file names from IPDSSAT.DAT
C-----------------------------------------------------------------------
      CALL READRZX (CONTROL % CROP, EFINOC, EFNFIX, 
     + FILES, FILEE, FILEC,
     + CONTROL % FROP, ISWITCH % IDETC, ISWITCH % IDETD, 
     + ISWITCH % IDETG, ISWITCH % IDETL, ISWITCH % IDETN, 
     + ISWITCH % IDETO, IDETP, ISWITCH % IDETS,
     + ISWITCH % IDETW, IOX, ISWITCH % ISWDIS, ISWITCH % ISWNIT,
     + ISWPHO, ISWPOT, ISWITCH%ISWSYM, ISWITCH % ISWWAT,
     + PATHSR, PATHER, PATHCR, SOILPROP%SLNF, SOILPROP%SLPF, SPECIES, 
     + SOILPROP % WR, CONTROL % TRTNO,ISWITCH % MEPHO,iswpar,nlayro)

      PGFAC3   = SOILPROP % SLPF
      FILES(6:8)="040"
      FILEE(6:8)="040"
      FILEC(6:8)="040"
      CONTROL % MODEL = FILES(1:8)
      MODEL = CONTROL % MODEL
      CONTROL % FILEC  =  FILEC
      CONTROL % FILEE  =  FILEE
      CONTROL % FILES  =  FILES
      CONTROL % PATHSR =  PATHSR
      CONTROL % PATHCR =  PATHCR
      CONTROL % PATHER =  PATHER
      PLANTVAR%EFINOC = EFINOC
      PLANTVAR%EFNFIX = EFNFIX
      TITLET = PATHCR
      control% titlet = titlet
c      control% trtno = max(control% trtno, control% run)
      IF (INDEX ('BNSBPNPECHPPVBCPCBFBAL',CONTROL % CROP) .EQ. 0) THEN
          ISWITCH%ISWSYM='N'
      endif
      if (imagic.eq.-8) then
      ISWITCH % ISWWAT = 'N'
      ISWITCH % ISWNIT = 'N'
      ENDIF
c
      IF (((INDEX('GROgro',MODEL(3:5)) .eq.0).and.
     &    (INDEX('FRMfrm',MODEL(3:5)) .eq.0)).and.
     &  (ISWITCH % MEPHO .EQ. 'L')) THEN
        ISWITCH % MEPHO = 'C'
      ENDIF
      IF (ISWITCH% ISWWAT .EQ. 'Y') THEN
        PESW = MAX(0.0, TSW - TLL)
      ELSE
        !If water not being simulated, use DUL as water content
        PESW = MAX(0.0, TDUL - TLL)
        SWFAC  = 1.0
        TURFAC = 1.0
      ENDIF
c
          CALL GETPUT_CONTROL('PUT',CONTROL)
          CALL GETPUT_ISWITCH('PUT',ISWITCH)
          CALL GETPUT_SOILPROP('PUT',SOILPROP)
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
C-----------------------------------------------------------------------
        CALL IPVARC (FILEC,NSENS,CONTROL%RNMODE,VARNO,
     &                  VARTY,VRNAME,
     &                  PATHCR,ECONO,CONTROL%CROP,PHINT,HTMAX,BIOHALF)
          PLHGHT=DBLE(HTMAX)
          IF (PLHGHT.LE.0.0D0) THEN
            IF (INDEX('MXMO',control%CROP) .GT. 0) then   !  BASED ON POPULATION 1.6 MILLION/HA FROM DAVID NIELSEN
               PLHGHT=93.7D0
               BIOHALF=1.92D0
            ELSE IF (INDEX('ML',CONTROL%CROP) .GT. 0) THEN   !  BASED ON POPULATION 60000/HA FROM DAVID NIELSEN
               PLHGHT=93.14D0
               BIOHALF=9.69D0
            ELSE IF (INDEX('SG',CONTROL%CROP) .GT. 0) THEN    !  BASED ON POPULATION 30000/HA FROM DAVID NIELSEN
               PLHGHT=93.14D0
               BIOHALF=19.39D0
            ELSE IF (INDEX('MZ',CONTROL%CROP) .GT. 0) THEN    !  BASED ON POPULATION 76000/HA FROM DAVID NIELSEN
               PLHGHT=244.6D0
               BIOHALF=43.07D0
            ELSE IF (INDEX('BS',CONTROL%CROP) .GT. 0) THEN
               PLHGHT=35.0d0
               BIOHALF=15.0d0
            ENDIF
            PRINT*, 'CALIBRATE PLANT HEIGHT, DEFAULT VALUE MAY NOT WORK'
          ENDIF
      if (dble(biohalf).gt.100.0d0) then
              PLALFA=-2.0D0*PLHGHT*LOG(0.5D0)/DBLE(BIOHALF/10.0/pltpop)
      elseif (dble(biohalf).gt.0.0d0) then
              PLALFA=-2.0D0*PLHGHT*LOG(0.5D0)/DBLE(BIOHALF)
      endif      
      icres = real(RM)
      cumres = real (RM)
	DO L = 1,SOILPROP % NLAYR
        SWINIT(L) = SW(L)
        TSWINI = TSWINI + SW(L) * SOILPROP % DLAYR(L)
      ENDDO
        TSWINI = TSWINI + (ET + RUNOFF + DRAIN - depir)*0.1 -rainr  ! SW INTO DSSAT HAS TAKEN OUT ET, RUNOFF AND DRAIN
C-----------------------------------------------------------------------
C    Set Species Identification
C-----------------------------------------------------------------------
      IF (CONTROL % CROP .EQ. 'BN') CRID = 1
      IF (CONTROL % CROP .EQ. 'PN') CRID = 2
      IF (CONTROL % CROP .EQ. 'SB') CRID = 3
      IF (CONTROL % CROP .EQ. 'CP') CRID = 4
      IF (CONTROL % CROP .EQ. 'PE') CRID = 5
      IF (CONTROL % CROP .EQ. 'CH') CRID = 6
      IF (CONTROL % CROP .EQ. 'PP') CRID = 7
      IF (CONTROL % CROP .EQ. 'PR') CRID = 8
      IF (CONTROL % CROP .EQ. 'RI') CRID = 9
      IF (CONTROL % CROP .EQ. 'FA') CRID = 10
      IF (CONTROL % CROP .EQ. 'MZ') CRID = 11
      IF (CONTROL % CROP .EQ. 'WH') CRID = 12
      IF (CONTROL % CROP .EQ. 'ML') CRID = 13
      IF (CONTROL % CROP .EQ. 'SG') CRID = 14
      IF (CONTROL % CROP .EQ. 'BA') CRID = 15
      IF (CONTROL % CROP .EQ. 'CS') CRID = 16
      IF (CONTROL % CROP .EQ. 'PT') CRID = 17
      IF (CONTROL % CROP .EQ. 'TM') CRID = 18
      IF (CONTROL % CROP .EQ. 'C3') CRID = 19
      IF (CONTROL % CROP .EQ. 'C4') CRID = 20
      IF (CONTROL % CROP .EQ. 'G0') CRID = 21
      IF (CONTROL % CROP .EQ. 'G1') CRID = 22
      IF (CONTROL % CROP .EQ. 'G2') CRID = 23
      IF (CONTROL % CROP .EQ. 'G3') CRID = 24
      IF (CONTROL % CROP .EQ. 'G4') CRID = 25
      IF (CONTROL % CROP .EQ. 'G5') CRID = 26
      IF (CONTROL % CROP .EQ. 'G6') CRID = 27
      IF (CONTROL % CROP .EQ. 'G7') CRID = 28
      IF (CONTROL % CROP .EQ. 'G8') CRID = 29
      IF (CONTROL % CROP .EQ. 'BR') CRID = 30
      IF (CONTROL % CROP .EQ. 'SC') CRID = 31
      IF (CONTROL % CROP .EQ. 'SU') CRID = 33
      IF (CONTROL % CROP .EQ. 'PI') CRID = 34
      IF (CONTROL % CROP .EQ. 'TR') CRID = 35
      IF (CONTROL % CROP .EQ. 'TN') CRID = 36
      IF (CONTROL % CROP .EQ. 'CO') CRID = 37
      IF (CONTROL % CROP .EQ. 'VB') CRID = 38
      IF (CONTROL % CROP .EQ. 'CB') CRID = 39
      IF (CONTROL % CROP .EQ. 'FB') CRID = 40
      IF (CONTROL % CROP .EQ. 'CT') CRID = 41
      IF (CONTROL % CROP .EQ. 'MO') CRID = 42       !proso millet
      IF (CONTROL % CROP .EQ. 'MX') CRID = 43       !fox millet
      IF (CONTROL % CROP .EQ. 'TR') CRID = 44       !triticale
      IF (CONTROL % CROP .EQ. 'CA') CRID = 45       !CANOLA
      if (CONTROL % CROP .EQ. 'BS') CRID = 46       !RMarquez - SugarBeet
      if (CONTROL % CROP .EQ. 'AL') CRID = 47       !ALFALFA
      if (CONTROL % CROP .EQ. 'BM') CRID = 48       !Bermuda Grass
      if (CONTROL % CROP .EQ. 'SF') CRID = 49       !CERES-Based SUNFLOWER model

      CROPD = CROPS(CRID)
c           CALL GETPUT_SOILPROP('GET',SOILPROP)

      CALL OPGEN (CUMDEP,TPESW,VRNAME,ANO3,ANH4,TLL,TDUL,TSAT,
     &     TSWINI,CONTROL%RUN,CONTROL%MODEL,CONTROL%CROP,CROPD,
     &     TITLET,ECONO,VARTY,
     &     ESW,SWINIT,NO3,NH4,TSOC,WTHSTR,NYRS,
     &     EXPER,CG, ENAME,YRPLT,                !LIWANG MA, RZWQM-DSSAT
     &     PEDON,SLTX,SLDESC,WSTA,ICRES,TOTAPW,TOTNAP)               ! LIWANG MA, RZWQM-DSSAT

C        CALL OPSOILC (IDETO,RNMODE,NOUTDO,LL,DUL,SAT,
C     &   DLAYR,SW,DS,NLAYR,ESW,SHF,BD,PH,NO3,NH4,OC,
C     &   TLL,TDUL,TSAT,TPESW,TSW,TRNO3U,TRNH4U,TSOC,
C     &   SWCON,U,SALB,CN2,CROPD,VRNAME,VARTY,SLPF,
C     &   ECONAM,ECONO,SLNF,NOUTDO,ISWWAT,NYRS)

      ENDIF
C
            WEATHER % CO2 = REAL(CO2R)
            IF (WEATHER%TDEW.LE.0.0) WEATHER%TDEW=WEATHER%TMIN
            CALL YR_DOY(CONTROL % YRDOY, YR, DOY)
            CALL DAYLEN (DOY, WEATHER % XLAT, DAYL, DEC, SNDN, SNUP) 
            CALL TWILIGHT (DOY, WEATHER % XLAT, TWILEN)
            CALL SOLAR(DAYL, DEC, weather%SRAD, weather%XLAT,     !Input
     &    weather%CLOUDS, ISINB, S0N)                             !Output
          if (iweather.eq.0) then         !convert from daily to hourly weather
            CALL HMET(weather%CLOUDS, DAYL, DEC, ISINB, weather%PAR,
     &    weather%REFHT,SNDN, SNUP, S0N, weather%SRAD,           !Input
     &    weather%TDEW, weather%TMAX,weather%TMIN,              !Input
     &    weather%WINDHT, weather%WINDSP, weather%XLAT,                     !Input
     &    weather%AZZON, weather%BETA, weather%FRDIFP, weather%FRDIFR,
     &    weather% PARHR, weather%RADHR,      !Output
     &    weather%RHUMHR, weather%TAIRHR, weather%TAVG, weather%TDAY,
     &    weather% TGRO, weather%TGROAV,       !Output
     &    weather%TGRODY,weather%WINDHR)                                  !Output
          else if (iweather.eq.1) then
         nhour=0
         weather%TDAY=0.0
         weather%TAVG=0.0
         do i=1,24
          if (hrzpar(i).le.0.0d0) hrzpar(i)=hrts(i)*2.0d0  !the unit here is umol/m2/s from w/m2.
          weather% PARHR(i)=real(hrzpar(i))
          weather%RADHR(i)=real(hrts(i))
          weather%RHUMHR(i)=real(hrh(i))
          weather%TAIRHR(i)=real(hrt(i))
          weather%WINDHR(i)=real(hru(i))
c
        weather%TAVG = weather%TAVG + weather%TAIRHR(i)

        IF (i .GE. SNUP .AND. i .LE. SNDN) THEN
          weather%TDAY = weather%TDAY + weather%TAIRHR(i)
          Nhour = Nhour + 1
        ENDIF
      
         enddo
      weather%TAVG = weather%TAVG / REAL(24)
      weather%TDAY = weather%TDAY / REAL(Nhour)

      weather%TGRODY = weather%TDAY
      weather%TGROAV = weather%TAVG
      DO i=1,24
        weather%TGRO(i) = weather%TAIRHR(i)
C       Calculate real and solar time.
        HS = REAL(i)    !* TINCR

C       Calculate sun angles and hourly weather variables.
        CALL HANG(
     &    DEC, HS, weather%XLAT,                                  !Input
     &    weather%AZZON(i), weather%BETA(i))                              !Output
       CALL FRACD(
     &    weather%BETA(i), weather%CLOUDS, HS, weather%RADHR(i), 
     &     S0N, SNDN,SNUP,                                              !Input
     &    weather%FRDIFP(i), weather%FRDIFR(i))                          !Output
      ENDDO
          endif

            HARVFRAC(1) = plantvar%HPC /100. ! from DSSAT040.INP
            HARVFRAC(2) = plantvar%HBPC /100. 
            HARVFRAC(3) = plantvar%HRPC /100. 
C-----------------------------------------------------------------------
C         CONTROL % DAS = CONTROL%YRDOY
         WEATHER % DAYL = DAYL  
         WEATHER % TWILEN = TWILEN
         WEATHER % SNDN = SNDN
         WEATHER % SNUP = SNUP  
         CONTROL % DAS = MAX(0,TIMDIF(CONTROL%YRSIM,CONTROL%YRDOY))
         CO2 = WEATHER % CO2
         TMAX = WEATHER % TMAX
         TMIN = WEATHER % TMIN
         SRAD = WEATHER % SRAD
c
      TDSOIL=0.0
      TNSOIL=0.0
      TMSOIL=0.0
      ICOUNT1=0
      icount2=0
      DO I=1,24
c          if (SSURFT(I).gt.-273.0d0) then
        IF (i .GE. SNUP .AND. i .LE. SNDN) THEN
          TDSOIL=TDSOIL+real(SSURFT(I))
          ICOUNT1=ICOUNT1+1
      ELSE
          TNSOIL=TNSOIL+real(SSURFT(I))
          icount2=icount2+1
      ENDIF
c          endif
      ENDDO
      if ((icount1.eq.0).or.(icount2.eq.0)) stop 'check soil temp'
      TMSOIL=(TNSOIL+TDSOIL)/(icount1+icount2)
      TDSOIL=TDSOIL/ICOUNT1
      TNSOIL=TNSOIL/ICOUNT2
C-----------------------------------------------------------------------
C     CALL PLANT GRWOTH MODULES
C-----------------------------------------------------------------------

      CALL GETPUT_CONTROL('PUT', CONTROL) 
      CALL GETPUT_ISWITCH('PUT', ISWITCH)    
c---- START  DYNAMIC = RUNINIT --------------------
       if (control % dynamic .eq. runinit) then

c      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

        IF (ISWITCH % MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF

! CROPGRO
      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)  !Output
      else IF (INDEX('FRMfrm',MODEL(3:5)) .GT.0) THEN
       PRINT *, "CALLING FORAGE"
       Call FORAGE(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP,              !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)    !Output
      else     
      call Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,wflf,turfac,                                 !Input
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI,              !Output
     &    LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,    !OUTPUT
     &    RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,cwad,rtwts,             !Output
     &    ISTAGE,TRNU,SWFAC,tfg,grwt,nupd,cnad,gnad,wfg,stwt)           

       endif
C
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT,
     &     EXPER,CG, ENAME)                !LIWANG MA, RZWQM-DSSAT
!      ENDIF
	endif
      IF (FIRST) then
	      CALL GETPUT_CONTROL('get', CONTROL) 
             control % dynamic = seasinit
            CALL GETPUT_CONTROL('PUT', CONTROL) 
      endif
c---- END OF  DYNAMIC = RUNINIT --------------------
c---- START  DYNAMIC = SEASINIT --------------------

      IF (CONTROL % DYNAMIC .EQ. SEASINIT) THEN
C
c      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, soilprop%DLAYR, IRRAMT, soilprop%LL,             !Input
     &    soilprop%NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input
      CALL OpSoilNC(CONTROL, ISWITCH, 
     &    AMTNIT, CUMRES, DSNC, HUMC, HUMN, NH4, NO3, 
     &    THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, 
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNOX)
      CALL OPSTEMP(CONTROL, ISWITCH, DOY, ST)

!     Water balance output initialization
      CALL Wbal(CONTROL, ISWITCH, 
c     &    DRAIN, ES, FLOODWAT, IRRAMT, weathre%RAIN, RUNOFF, SNOW,
c     &    SWDELTS, SWDELTU, SWDELTX, soilprop%DLAYR, soilprop%NLAYR,
     &    DRAIN, ES, IRRAMT, weather%RAIN, RUNOFF, SNOW,
     &    soilprop%DLAYR, soilprop%NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)
c        CALL SoilNBal (CONTROL, ISWITCH, 
c     &    ALGFIX, AMTNIT, CUMFNRO, CUMRESN, CUMSENN, HARVRES,!Input
c     &    NBUND, NLAYR, RESLEFTN, TFON, THUMN, TLCH, TNH4,!Input
c     &    TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP)    !Input

C-----------------------------------------------------------------------
C    Calculate potential root water uptake
C-----------------------------------------------------------------------
c
      CALL ROOTWU(CONTROL % DYNAMIC,SOILPROP % DLAYR,     !INPUT
     &     SOILPROP % LL, SOILPROP % NLAYR,               !INPUT
     &      PORMIN, RLV, RWUMX, SOILPROP % SAT, SW,       !Input
     &      RWU, SATFAC, TRWUP)                             !Output
c
        IF (ISWITCH % MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
! CROPGRO
      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)  !Output
      else IF (INDEX('FRMfrm',MODEL(3:5)) .GT.0) THEN
       PRINT *, "CALLING FORAGE"
       Call FORAGE(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP,              !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)    !Output
      else
      call Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,wflf, turfac,                                !Input
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI,              !Output
     &    LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,    !OUTPUT
     &    RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,cwad,rtwts,             !Output
     &    ISTAGE,TRNU,SWFAC,tfg,grwt,nupd,cnad,gnad,wfg,stwt)           

       endif
C
C       ENDIF
c          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
c          KSEVAP = KEP        ! from Alt_plant
      IF (ISWITCH % IDETS .EQ. 'Y' .OR. ISWITCH%IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT,
     &     EXPER,CG, ENAME)                !LIWANG MA, RZWQM-DSSAT
      ENDIF
      ENDIF
C-----------------------------------------------------------------------
c---- END OF  DYNAMIC = SEASINIT  --------------------
c---------- START  DYNAMIC = RATE --------------------
C       CALL GETPUT_WEATHER('PUT', WEATHER)    
	      CALL GETPUT_CONTROL('get', CONTROL) 
             CONTROL % DYNAMIC = RATE
            CALL GETPUT_CONTROL('PUT', CONTROL) 
C
      CALL OPSTRESS(CONTROL, W=WEATHER)
C-----------------------------------------------------------------------
C    Calculate potential root water uptake
C-----------------------------------------------------------------------
      CALL AUTHAR(CONTROL, ISWITCH %ISWWAT, 
     &    SOILPROP%DLAYR, SOILPROP%DUL, ISWITCH%IDETO, 
     &    ISWITCH%IHARI, SOILPROP%LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output

      CALL ROOTWU(CONTROL % DYNAMIC,SOILPROP % DLAYR,     !INPUT
     &     SOILPROP % LL, SOILPROP % NLAYR,               !INPUT
     &      PORMIN, RLV, RWUMX, SOILPROP % SAT, SW,       !Input
     &      RWU, SATFAC, TRWUP)                             !Output
c
        IF (ISWITCH % MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
!-----------------------------------------------------------------------
!       ACTUAL ROOT WATER EXTRACTION
!-----------------------------------------------------------------------
c        IF (ISWITCH %ISWWAT .EQ. 'Y') THEN
C         Calculate actual soil water uptake and transpiration rates
c          CALL XTRACT(
c     &      SOILPROP %NLAYR, SOILPROP %DLAYR, SOILPROP %LL, 
c     &      SW, TRWUP,        !Input
c     &      EOP, RWU)                                      !Input/Output
c        DO L = 1,SOILPROP % NLAYR
c              qsr(l) = rwu(l)/24.0/SOILPROP % dlayr(l)
c        END DO
c        CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,qsr,NNR,TLTR)
c        ENDIF   !ISWWAT = 'Y'
! CROPGRO
      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)  !Output
      else IF (INDEX('FRMfrm',MODEL(3:5)) .GT.0) THEN
       PRINT *, "CALLING FORAGE"
       Call FORAGE(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP,              !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)    !Output
      else
      call Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,wflf,turfac,                                 !Input
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI,              !Output
     &    LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,    !OUTPUT
     &    RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,cwad,rtwts,             !Output
     &    ISTAGE,TRNU,SWFAC,tfg,grwt,nupd,cnad,gnad,wfg,stwt)           

       endif
C
	      CALL GETPUT_CONTROL('get', CONTROL) 
                 CONTROL % DYNAMIC = INTEGR
            CALL GETPUT_CONTROL('PUT', CONTROL) 
c---- END OF DYNAMIC = RATE --------------------
c---- START  DYNAMIC = INTEGR ------------------
        IF (ISWITCH % MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
! CROPGRO
      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)  !Output
      else IF (INDEX('FRMfrm',MODEL(3:5)) .GT.0) THEN
       PRINT *, "CALLING FORAGE"
       Call FORAGE(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP,              !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)    !Output
      else
      call Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT, wflf,turfac,                                !Input
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI,              !Output
     &    LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,    !OUTPUT
     &    RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,cwad,rtwts,             !Output
     &    ISTAGE,TRNU,SWFAC,tfg,grwt,nupd,cnad,gnad,wfg,stwt)           

       endif
C
      CALL AUTHAR(CONTROL, ISWITCH %ISWWAT,      !check here for YREND
     &    SOILPROP%DLAYR, SOILPROP%DUL, ISWITCH%IDETO, 
     &    ISWITCH%IHARI, SOILPROP%LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, soilprop%DLAYR, IRRAMT,soilprop%LL,             !Input
     &    soilprop%NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input
c---- END OF DYNAMIC = INTEGR --------------------
c---- START  DYNAMIC = OUTPUT --------------------

	      CALL GETPUT_CONTROL('get', CONTROL) 
                 CONTROL % DYNAMIC = OUTPUT
            CALL GETPUT_CONTROL('PUT', CONTROL) 
c
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, soilprop%DLAYR, IRRAMT,soilprop%LL,             !Input
     &    soilprop%NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input

      CALL OpSoilNC(CONTROL, ISWITCH, 
     &    AMTNIT, CUMRES, DSNC, HUMC, HUMN, NH4, NO3, 
c     &    RESLEFT, RESLEFTN,
     &    THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, 
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNOX)
      CALL OPSTEMP(CONTROL, ISWITCH, DOY, ST)

!     Water balance daily output 
      CALL Wbal(CONTROL, ISWITCH, 
     &    DRAIN, ES, IRRAMT, weather%RAIN, RUNOFF, SNOW,
     &    soilprop%DLAYR, soilprop%NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)

c        CALL SoilNBal (CONTROL, ISWITCH, 
c     &    ALGFIX, AMTNIT, CUMFNRO, CUMRESN, CUMSENN, HARVRES,!Input
c     &    NBUND, NLAYR, RESLEFTN, TFON, THUMN, TLCH, TNH4,!Input
c     &    TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP)    !Input

        IF (ISWITCH % MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
! CROPGRO
      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)  !Output
      else IF (INDEX('FRMfrm',MODEL(3:5)) .GT.0) THEN
       PRINT *, "CALLING FORAGE"
       Call FORAGE(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP,              !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)    !Output
      else
      call Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT, wflf,turfac,                                !Input
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI,              !Output
     &    LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,    !OUTPUT
     &    RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,cwad,rtwts,             !Output
     &    ISTAGE,TRNU,SWFAC,tfg,grwt,nupd,cnad,gnad,wfg,stwt)           

       endif
C
c---- END OF  DYNAMIC = OUTPUT --------------------

      CALL AUTHAR(CONTROL, ISWITCH %ISWWAT, 
     &    SOILPROP%DLAYR, SOILPROP%DUL, ISWITCH%IDETO, 
     &    ISWITCH%IHARI, SOILPROP%LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output

      if (first) first=.false.
c
      CALL OPSTRESS(CONTROL, E=ET)
C
       CALL GETPUT_PlantStres('GET',PlantStres)
C       SWFAC = PlantStres % SWFAC
c       TURFAC = PLANTSTRES % TURFAC
C------------------------------------------------------------------
C------------------------------------------------------------------
c calculate water uptake
C------------------------------------------------------------------
      IF (real(PET) .GT. 0.0) THEN
cc        IF (real(PET) .LE. TRWUP) THEN
cc          WUF = real(PET)/(TRWUP)
cc        ELSE
          WUF = 1.0   !check for upper limit is moved to RZDAY.FOR
cc        ENDIF
c      write (666,*) ep, trwu, wuf
        DO L = 1,SOILPROP % NLAYR
          IF (SW(L) .GT. SOILPROP % LL(L)) THEN
               rwu(L) = RWU(L)*WUF
               qsr(l) = rwu(l)/24.0/SOILPROP % dlayr(l)
           else IF (SW(L) .LT. SOILPROP % LL(L)) THEN
               qsr(L)=0.0
               rwu(L)=0.0
cc             rwu(L) = RWU(L) - (SOILPROP % LL(L)-SW(L))*
cc     &                SOILPROP % DLAYR(L)
cc              qsr(l) = rwu(l)/24.0/SOILPROP % dlayr(l)
cc            ENDIF
          ENDIF
        END DO
      ELSE
        DO L = 1,SOILPROP % NLAYR
          RWU(L) = 0.0
          qsr(l) = 0.0
        ENDDO
      ENDIF
        CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,qsr,NNR,TLTR)
C ========================================================================
      IF (NDEM .LT. 0.0) THEN
         NDEM = 0.0
      ENDIF
c      DMDNIT = DBLE(NDEM)
C  DMDNIT nitrogen demand from plant (g-n/plant)
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          ROWSPC  = PLANTVAR%ROWSPC
          PLTPOP  = PLANTVAR%PLTPOP
c      FIRST  = FIRST
      JDAY = INT(((REAL(YRSIMR)/1000.)-INT(REAL(YRSIMR)/1000.))*1000.)
      IF ((INDEX('GROgro',MODEL(3:5)) .GT.0) .or.
     &   (INDEX('FRMfrm',MODEL(3:5)) .GT.0)) THEN
c      IF (CONTROL % CROP .EQ. 'SB') THEN
	  if (CONTROL % YRDOY.eq.yrend) then
              nfixn=0.0d0
              fixn=0.0d0
	  else
               FIXN   = DBLE((NFIXN+(NODGR * 0.16 * PRONOD)) * 10.)   !kg N/ha/d
	  endif
	         GS = DBLE(RSTAGE/8.0)
               BASE   = DBLE((CANWH * MIN(CANWH,BETN)))
               HEIGHT = DBLE(CANHT*100.)
	      IF (CONTROL % DAS .EQ. NVEG0) THEN
	         JGS(1) = 1
            ELSE IF (CONTROL % YRDOY .EQ. MDATE .and.Hdate(1).lt.0)THEN
c            ELSE IF (CONTROL % DAS .EQ. MDATE) THEN
               JGS(2) = 1
            ENDIF
      ELSE                            !check if this is correct, 9-25-2007
      IF (INDEX('PT',control%CROP) .GT. 0) THEN
          HEIGHT = DBLE(CANHT*100.0)     !in CERES model, CANHT is in m
      else IF (INDEX('BAWHTR',control%CROP) .GT. 0) then
          HEIGHT = DBLE(CANHT)     !in barley and wheat model, CANHT is in cm
c          Height = DBLE(98.94d0*(1.0d0-dexp(-1.342d0*Biomas*10.0))) ! added by Fang
      else if (INDEX('MZBSSF',control%CROP) .GT. 0) then   ! regression equation from David Nielsen's 85 corn study (Ma et al., 2003, ASAE)
          stalk = dble(biomas*10.0-NINT (grnwt * EARS * 10.0))
c          Height = 244.6d0*(1.0d0-dexp(-0.0518d0*stalk/244.6d0))
          STEM2=STALK/10.D0/PLTPOP
          HEIGHT=PLHGHT*(1.0D0-EXP(-PLALFA*STEM2/(2.0D0*PLHGHT)))
c          Height = 244.6d0*(1.0d0-dexp(-0.0318d0*stalk/244.6d0))
c          Height = 237.8d0*(1.0d0-dexp(-7.151d0*stalk))  ! revised from Yucheng by Fang
c      this is from Tom Trout from 2008 and 2009 corn data
c          Height = 0.02057*stalk
          CANHT = REAL(HEIGHT)/100.0
      else if (INDEX('SGMXMO',control%CROP) .GT. 0) then   ! regression equation from David Nielsen's study
          stalk = dble(biomas*10.0-NINT (grnwt * PLTPOP * 10.0))
C          Height = 93.14d0*(1.0d0-dexp(-0.111d0*stalk/93.14d0))
          STEM2=STALK/10.D0/PLTPOP
          HEIGHT=PLHGHT*(1.0D0-EXP(-PLALFA*STEM2/(2.0D0*PLHGHT)))
          CANHT = REAL(HEIGHT)/100.0
      else if (INDEX('ML',control%CROP) .GT. 0) then   ! regression equation from David Nielsen's study
          stalk = dble(biomas*10.0-
     &                max(NINT (grnwt * PLTPOP * 10.0),nint(sdwt*10)))
C          Height = 93.7d0*(1.0d0-dexp(-0.0211d0*stalk/93.7d0))
          STEM2=STALK/10.D0/PLTPOP
          HEIGHT=PLHGHT*(1.0D0-EXP(-PLALFA*STEM2/(2.0D0*PLHGHT)))
          CANHT = REAL(HEIGHT)/100.0
      endif
          oldheight = max(oldheight, height)
          height = oldheight
c
         IF (ISTAGE .LE. 6) THEN
            GS     = DBLE(ISTAGE / 6.)
         ELSE
            GS     = 0.0
         ENDIF
         IF (CONTROL % YRDOY .EQ. STGDOY(9)) THEN
c         JGS(1) = JDAY
         JGS(1) = 1
c         JGS(2) = 366
         ELSE IF (CONTROL % YRDOY .EQ. STGDOY(6).and.Hdate(1).lt.0) THEN
c         JGS(1) = 366
c         JGS(2) = JDAY
         JGS(2) = 1
         ENDIF
         ENDIF
c added to decrease effective LAI after maturity, limited to 30 day dry down period
      if ((CONTROL % YRDOY.gt.mdate).and.(hdate(1).gt.mdate).and.
     &    (mdate.gt.0)) then
      rxlai=xhlai-(xhlai-0.0d0)/(min(30,hdate(1)-mdate))
     &           *(CONTROL % YRDOY-mdate)
      else
      rxlai=xhlai
      endif
c end of LAI adjust. remember leaf biomass is not changed
c         
      RLAI    = max(DBLE(rXLAI),0.0d0)
      TLAI    = max(DBLE(rXLAI),0.0d0)
      PLTSLV = DBLE(PLTPOP * 10000.)
      PWRTS  = DBLE(PLTPOP * 10000.)

      TROOT  = 0.0
      DO I = 1,SOILPROP % NLAYR
c take into account N uptake
           SNO3(i) = SNO3(i) - UNO3(i)
           SNH4(i) = SNH4(i) - UNH4(i)

           NO3(I) = SNO3(I)*FAC(I)
           NH4(I) = SNH4(I)*FAC(I)
           if (rlv(i).lt.0.0) rlv(i)=0.0  !temp fix in case rlv(i) < 0.0  Liwang Ma
           TROOT = TROOT + RLV(I) * SOILPROP % DLAYR(I)
           RLVR(I) =  RLV(I)
c liwang Ma 8/13/2003
           NO3R(i) = NO3(i)/(FAC(I)*SOILPROP % DLAYR(I))
           NH4R(i) = NH4(i)/(FAC(I)*SOILPROP % DLAYR(I))
           UNUPR(i) = (UNO3(i)+UNH4(i))/(SOILPROP % DLAYR(I))
      ENDDO
c      if (troot.gt.0.0) then
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,RLVR,NLAYRI,TLTR)
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,NO3R,NLAYRI,TLTR)
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,NH4R,NLAYRI,TLTR)
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,UNUPR,NLAYRI,TLTR)
C           TRNO3R= dble(TRNU)   !MOVE TO LATER
c     endif
      TRDFR = 0
      DO I = 1,NLAYRI
           IF (TROOT .GT. 0.) THEN
             IF (I.EQ.1) RDFR(I) = RLVR(I) * TLTR(I)/TROOT
             IF (I.NE.1) RDFR(I) = RLVR(I) * (TLTR(I)-TLTR(I-1))/TROOT
           TRDFR = TRDFR + RDFR(I)
           ELSE
             RDFR(I) = 0.0
             RLVR(I) = 0.0
           ENDIF
      ENDDO
C      IF ((TRDFR.NE.0.0) .AND.
C     &   ((TRDFR.LT.0.99) .OR. (TRDFR.GT.1.01))) THEN
C       PAUSE 'total root NE 1.0'
C     ENDIF
CZ-MA GET PARAMETERS FOR PLANT GROWTH
C           CALL GETPUT_PLANTVAR('GET',PLANTVAR)

C ADDED BY LIWANG MA TO CREATE PLANT4.PLT.
C      IF (BIOMAS .GT. 0.0 .AND. SDWT .GE. 0.0) THEN
         IF (CONTROL % CROP .EQ. 'MZ'.or.CONTROL % CROP .EQ. 'SF') THEN
C            HI    = SDWT*EARS/BIOMAS
            GRAINR = (grnwt * EARS * 10.0)
         ELSE IF ((CONTROL % CROP .EQ. 'WH').or.
     &        (CONTROL % CROP .EQ. 'BA').or.
     &        (control%crop .eq.'TR')) THEN
            GRAINR = (GRWT * PLTPOP * 10.0)
	      BIOMAS = (CWAD/10.0)
	      TRNU = NUPD
	      stovn = (CNAD-GNAD)/PLTPOP/10.0
		  grainn = GNAD/PLTPOP/10.0
	      SWFAC = WFG
            turfac=wflf
	      STMWT = STWT
c         else if (CONTROL % CROP .EQ. 'ML') then
         ELSE if (INDEX('SGMLMOMX',control%CROP) .GT. 0) then
c it seems to me that grnwt was not caculated at harverst and sdwt was calculated other times
c Liwang Ma, 1-12-2008
            GRAINR = max((grnwt * PLTPOP * 10.0),(sdwt*10))
c
         ELSE if (INDEX('PT',control%CROP) .GT. 0) then
C            HI    = SDWT*PLTPOP/BIOMAS
            GRAINR = (SDWT * PLTPOP * 10.0)
         ELSE if (INDEX('BS',control%CROP) .GT. 0) then
            GRAINR = (RTWT * PLTPOP * 10.0)
         ENDIF
C      ENDIF
c      IF (CONTROL % CROP .EQ. 'SB') THEN
      IF ((INDEX('GROgro',MODEL(3:5)) .GT.0) .or.
     &   (INDEX('FRMfrm',MODEL(3:5)) .GT.0)) THEN
	  if (CONTROL % YRDOY.ne.yrend) then
          TRNO3R= dble((TRNU+NFIXN+(NODGR * 0.16 * PRONOD))/PLTPOP)  !g/plant
	  else
	    trno3r=dble(trnu/pltpop)
	  endif                        
	     BIOMAS = TOPWT
	     RTWT = RTWTB/PLTPOP
	     RTWTS = SRDOT/PLTPOP
	     LFWT = WTLF/PLTPOP
	     STMWT = STMWTB/PLTPOP
           GRAINR = (SDWT * 10.0)
	     PCNLR = DBLE(PCNL/100.0)
	     PCNSTR = DBLE(PCNST/100.0)
	     PCNRTR = DBLE(PCNRT/100.0)
	     PCNGRNR = DBLE(PCNSD/100.0)
           STOVN = (LFWT*PCNL+STMWT*PCNST)/100.0
           GRAINN = SDWT*PCNSD/100.0/PLTPOP
           ROOTN  = RTWT*PCNRT/100.0

	ELSE
          TRNO3R= dble(TRNU)  !DAILY N UPTAKE, g/plant
          IF (STOVWT.NE.0.0) THEN
           PCNLR = DBLE(STOVN/STOVWT)
          ELSE
           PCNLR = 0.0D0
          ENDIF
          IF ((LFWT+STMWT).NE.0.0) THEN
           PCNSTR = DBLE(STOVN/(LFWT + STMWT))
          ELSE
           PCNSTR = 0.0D0
          ENDIF
          IF (RTWT.NE.0.0) THEN
           PCNRTR = DBLE(ROOTN/RTWT)
          ELSE
           PCNRTR = 0.0D0
          ENDIF
          IF (SDWT.NE.0.0) THEN
           PCNGRNR = DBLE(GRAINN*PLTPOP/SDWT)
          ELSE
           PCNGRNR = 0.0D0
          ENDIF
C
	ENDIF

c return dead residue and root to the soil system each day
         TOPRES = SENESCE % ResWt(0)
         WTNRES = SENESCE % ResE(0,1)
         if (DBLE(WTNRES).ne.0.0d0) then
	   FCN = DBLE(TOPRES)/2.5d0/DBLE(WTNRES)
         else
         FCN=0.0d0
         endif
         if (fcn.gt.0.0d0 .and.DBLE(topres).gt.0.0d0) then
         RCNR = CNEW(RM,DBLE(TOPRES),RCNR,FCN,0)
         RM = RM + DBLE(TOPRES)
         cumres = real(RM)
         PLANTVAR % RESAMT = TOPRES
         endif
C     ..PARTITION DEAD ROOT CARBON   :::USE DWL FOR LAYER DIST
C      PCN = DBLE((RTWT/2.5)/(PCNRTR*RTWT))
         TotRTRES = 0.0
         TotWTNRRS = 0.0
      DO 94 I=1,soilprop%nlayr !NLayro
         RTRES(I) = SENESCE % ResWt(I)/SOILPROP%DLAYR(I)
         WTNRRS(I) = SENESCE % ResE(I,1)/SOILPROP%DLAYR(I)
         TotRTRES = TotRTRES + SENESCE % ResWt(I)
         TotWTNRRS = TotWTNRRS + SENESCE % ResE(I,1)
94    CONTINUE
         OMSEA(69)=DBLE(TOPRES/PLTPOP)/10.0D0
         OMSEA(71)=DBLE(TOTRTRES/PLTPOP)/10.0D0
C
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,RTRES,NLAYRI,TLTR)
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,WTNRRS,NLAYRI,TLTR)
c limit to NLAYRI DS(20)      DO 90 I = 1, NNR
      DO 95 I = 1, NLAYRI
         if (DBLE(WTNRRS(I)).ne.0.0d0) then
         PCN = DBLE((RTRES(I))/2.5)/DBLE(WTNRRS(I))
         else
         PCN=0.0
         endif
c 1.0d-5 is coverting kg/ha to g/cm3
        if (i.eq.1) then
        RMASS = DBLE(RTRES(I)*1.0d-5/2.5d0)*dble(tltr(i))
        else
        RMASS = DBLE(RTRES(I)*1.0d-5/2.5d0)*dble(tltr(i)-tltr(i-1))
        endif
        IF (PCN.GT.0.0D0) THEN
          IF (PCN .LE. CNR(1)) THEN
            S = 0.0D0
          ELSE IF (PCN .GE. CNR(2)) THEN
            S = 1.0D0
          ELSE
            S = (1.0D0/PCN-1.0D0/CNR(1)) / (1.0D0/CNR(2)-1.0D0/CNR(1))
          ENDIF
          RPOOL(I,1)=RPOOL(I,1) + RMASS*(1.0D0-S)
          RPOOL(I,2)=RPOOL(I,2) + RMASS*S
C
C         .. SAVE FOR MASS BALANCE CALCULATIONS (CONVERT TO KG/HA)
c need to check if qckplnt and qckturf need this mass balance or not. Liwang Ma, 5-30-2007
          TADRT(1) = TADRT(1)+RMASS*(1.0D0-S)*1.0D5/ CNR(1)
          TADRT(2) = TADRT(2) + RMASS *S*1.0D5/CNR(2)
          TADRTC(1) = TADRTC(1)+RMASS*(1.0D0-S)*1.0D5
          TADRTC(2) = TADRTC(2) + RMASS *S*1.0D5
          RMASS1=RMASS*1.0D5
          IF (PCN .LE. CNR(1)) THEN
              RCO2=RCO2+RMASS1-(RMASS1*(1.0D0-S)/PCN)*CNR(1)
          ELSE IF (PCN .GE. CNR(2)) THEN
              RCO2=RCO2+RMASS1-(RMASS1*S/PCN)*CNR(2)
          endif
c
        END IF
   95 CONTINUE
      RM1=RM1+TOPRES
	RMN1=RMN1+WTNRES
      RTM1=RTM1+TOTRTRES
	RTMN1=RTMN1+TOTWTNRRS
      CALL SGATE (TDAYR,62,DBLE(GRAINR/PLTPOP/10.0D0))
      WRITE (94,2940) MOD(CONTROL % YRDOY,1000),CANHT,XLAI,BIOMAS*10,
     &  GRAINR
2940    FORMAT(1X,I7,2(1X,F8.3),1X,F8.1,1X,F8.0) !RM - last var is a real.
      if (iresetht1.eq.1) height=HEIGHTSET
      CALL SGATE (TDAYR,12,HEIGHT)
      CALL SGATE (TDAYR,11,RLAI)
      CALL SGATE (TDAYR,50,PLTSLV)
      CALL SGATE (TDAYR,49,GS)
      CALL SGATE (TDAYR,79,DBLE(BIOMAS*10.0D0))
c      CALL SGATE (TDAYR,81,DBLE(RTWT*PLTPOP*10.0D0+RTM1))
      CALL SGATE (TDAYR,81,DBLE(RTWT*PLTPOP*10.0D0))
c      CALL SGATE (TDAYR,61,DBLE(RTWT+RTM1/PLTPOP/10.0D0))
      CALL SGATE (TDAYR,61,DBLE(RTWT))
      CALL SGATE (TDAYR,65,DBLE(RTWTS))
      CALL SGATE (TDAYR,14,DBLE(RTDEP))
      CALL SGATE (TDAYR,59,DBLE(STMWT))
c      CALL SGATE (TDAYR,58,DBLE(LFWT+RM1/PLTPOP/10.0D0))
      CALL SGATE (TDAYR,58,DBLE(LFWT))
      CALL SGATE (TDAYR,16,DBLE(SWFAC))
      CALL SGATE (TDAYR,18,DBLE(NSTRES))
c      CALL SGATE (TDAYR,66,DBLE(PCNLR*LFWT+RMN1/PLTPOP/10.0D0))
      CALL SGATE (TDAYR,66,DBLE(PCNLR*LFWT))
      CALL SGATE (TDAYR,67,DBLE(PCNSTR*STMWT))
c      CALL SGATE (TDAYR,69,DBLE(PCNRTR*RTWT+RTMN1/PLTPOP/10.0D0))
      CALL SGATE (TDAYR,69,DBLE(PCNRTR*RTWT))
      CALL SGATE (TDAYR,70,DBLE(PCNGRNR*GRAINR/PLTPOP/10.0D0))
C WRITE OMSEA FOR ANALYSIS FILE
      OMSEA(22) = DBLE(stovn+grainn)*PLTPOP*10.0D0  !+DBLE(RMN1)
      OMSEA(23) = DBLE(rootn*PLTPOP*10.0D0)   !+DBLE(RTMN1)
      OMSEA(24) = DBLE(grainn*pltpop*10.d0)
      OMSEA(25) = FIXN
      OMSEA(41) = DBLE(BIOMAS*10.0D0)
      OMSEA(42) = DBLE(RTWT*PLTPOP*10.0D0)  !+DBLE(RTM1)
      OMSEA(43) = RLAI
      OMSEA(44) = DBLE(GRAINR)
c      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
c      OMSEA(45) = dble(RSTAGE)
c      else     
c      OMSEA(45) = dble(ISTAGE)
c      endif
      OMSEA(45) = GS    !Changed to growth stage (0-1) instead of classes (1-7)
      OMSEA(62) = HEIGHT
      OMSEA(13) = DBLE(SWFAC)
      OMSEA(40) = DBLE(NSTRES)
      OMSEA(63) = DBLE(RTDEP)
      OMSEA(64) = DBLE(TFG)
c      OMSEA(65) = DBLE(LFWT+RM1/PLTPOP/10.0D0)
      OMSEA(65) = DBLE(LFWT)
      OMSEA(66) = DBLE(STMWT)
c      OMSEA(67) = DBLE(RTWT+RTM1/PLTPOP/10.0D0)
      OMSEA(67) = DBLE(RTWT)
      OMSEA(68) = DBLE(GRAINR/PLTPOP/10.0D0)
      OMSEA(100) = DBLE(TURFAC)
      OMSEA(106) = DBLE(TRWUP)
      
      DO I = 1,NLAYRI
       rwl(i)=dble(rtwt*rdfr(i))
       rootnr(i)=dble(rootn*rdfr(i))
      enddo
c
C so far, harvest only on specific dates, Liwang Ma
c      IF ((YRDOY .eq. HDATE(1))) then
c      IF (YRDOY .eq. harend) then
      IF (CONTROL % YRDOY .eq. yrend) then
c      close (999)   !close FORGMOW.DAT file Unit=MOWLUN
c pass yield to RZWQM
         yieldr(1) = dble(grainr)
         yieldr(2) = DBLE(BIOMAS*10.0D0)
         yieldr(3) = DBLE(RTWT*PLTPOP*10.0D0)
         TRWUP=0.0
c
	      CALL GETPUT_CONTROL('get', CONTROL) 
 	           CONTROL % DYNAMIC = FINAL
            CALL GETPUT_CONTROL('PUT', CONTROL) 
c
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, soilprop%DLAYR, IRRAMT,soilprop%LL,             !Input
     &    soilprop%NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input
      CALL OpSoilNC(CONTROL, ISWITCH, 
     &    AMTNIT, CUMRES, DSNC, HUMC, HUMN, NH4, NO3, 
c     &    RESLEFT, RESLEFTN,
     &    THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, 
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNOX)
      CALL OPSTEMP(CONTROL, ISWITCH, DOY, ST)
!     Water balance seasonal output 

      CALL Wbal(CONTROL, ISWITCH, 
     &    DRAIN, ES, IRRAMT, weather%RAIN, RUNOFF, SNOW,
     &    soilprop%DLAYR, soilprop%NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)

c        CALL SoilNBal (CONTROL, ISWITCH, 
c     &    ALGFIX, AMTNIT, CUMFNRO, CUMRESN, CUMSENN, HARVRES,!Input
c     &    NBUND, NLAYR, RESLEFTN, TFON, THUMN, TLCH, TNH4,!Input
c     &    TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP)    !Input

        IF (ISWITCH % MEPHO .EQ. 'L') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
! CROPGRO
      IF (INDEX('GROgro',MODEL(3:5)) .GT.0) THEN
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)  !Output
      else IF (INDEX('FRMfrm',MODEL(3:5)) .GT.0) THEN
       PRINT *, "CALLING FORAGE"
       Call FORAGE(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP,              !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI,NVEG0,RTWTB,STMWTB,
     &    NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD,RSTAGE,SDWT,TRNU,
     &    WTNFX,pronod,nodgr,rtdep,swfac,turfac,srdot)    !Output
      else
      call Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT, wflf,turfac,                                !Input
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI,              !Output
     &    LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS,    !OUTPUT
     &    RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP,cwad,rtwts,             !Output
     &    ISTAGE,TRNU,SWFAC,tfg,grwt,nupd,cnad,gnad,wfg,stwt)           

       endif
C
       write(unit=101,fmt=*) yrplt,asimulated(25),asimulated(1),
     &                       asimulated(4),control%crop
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT,
     &     EXPER,CG, ENAME)                !LIWANG MA, RZWQM-DSSAT
	      CALL GETPUT_CONTROL('GET', CONTROL) 
 	           CONTROL % DYNAMIC = SEASINIT
            CALL GETPUT_CONTROL('PUT', CONTROL) 
!      ENDIF
      gs=0.0d0
      first = .true.
      JGS(2) = 1
      yrend = -99
      resage = 0.0d0
c   to reset grain yield to zero if needed
         grainr = 0.0
         grnwt = 0.0
         sdwt = 0.0
C ADD BIOMASS TO SURFACE RESIDUE POOL NEED MORE WORK
C         RM = RM + DBLE(BIOMAS-RTWT*PLTPOP)*10.D0
C        FCN = DBLE(LFWT+STMWT)/2.5/DBLE(STOVN)
C        RCNR = CNEW(RM,DBLE(BIOMAS-RTWT*PLTPOP)*10.D0,RCNR,FCN,0)
c       if ((iharvr.eq.3).or.(iharvr.eq.1)) then
         TOPRES = HARVRES % ResWt(0)
         WTNRES = HARVRES % ResE(0,1)
         if (DBLE(WTNRES).ne.0.0d0) then
	   FCN = DBLE(TOPRES)/2.5d0/DBLE(WTNRES)
         else
         FCN=0.0d0
         endif
         if (fcn.gt.0.0d0 .and.dble(topres).gt.0.0d0) then
         RCNR = CNEW(RM,DBLE(TOPRES),RCNR,FCN,0)
         RM = RM + DBLE(TOPRES)
         cumres = real(RM)
         PLANTVAR % RESAMT = TOPRES
         endif
c        endif
C     ..PARTITION DEAD ROOT CARBON   :::USE DWL FOR LAYER DIST
C      PCN = DBLE((RTWT/2.5)/(PCNRTR*RTWT))
      DO 93 I=1,soilprop%nlayr  !NLayro
         RTRES(I) = HARVRES % ResWt(I)/SOILPROP%DLAYR(I)
         WTNRRS(I) = HARVRES % ResE(I,1)/SOILPROP%DLAYR(I)
93    CONTINUE
C
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,RTRES,NLAYRI,TLTR)
      CALL realMATCH (SOILPROP % NLAYR,SOILPROP % DS,WTNRRS,NLAYRI,TLTR)
c limit to NLAYRI DS(20)      DO 90 I = 1, NNR
      DO 90 I = 1, NLAYRI
         if (DBLE(WTNRRS(I)).ne.0.0d0) then
         PCN = DBLE((RTRES(I))/2.5)/DBLE(WTNRRS(I))
         else
         PCN=0.0
         endif
c 1.0d-5 is coverting kg/ha to g/cm3
        if (i.eq.1) then
        RMASS = DBLE(RTRES(I)*1.0d-5/2.5d0)*dble(tltr(i))
        else
        RMASS = DBLE(RTRES(I)*1.0d-5/2.5d0)*dble(tltr(i)-tltr(i-1))
        endif
        IF (PCN.GT.0.0D0) THEN
          IF (PCN .LE. CNR(1)) THEN
            S = 0.0D0
          ELSE IF (PCN .GE. CNR(2)) THEN
            S = 1.0D0
          ELSE
            S = (1.0D0/PCN-1.0D0/CNR(1)) / (1.0D0/CNR(2)-1.0D0/CNR(1))
          ENDIF
          RPOOL(I,1)=RPOOL(I,1) + RMASS*(1.0D0-S)
          RPOOL(I,2)=RPOOL(I,2) + RMASS*S
C          NDTH=DWNOD                  !SET ALL NODULES TO DEATH POOL
C
C         .. SAVE FOR MASS BALANCE CALCULATIONS (CONVERT TO KG/HA)
c need to check if qckplnt and qckturf need this mass balance or not. Liwang Ma, 5-30-2007
          TADRT(1) = TADRT(1)+RMASS*(1.0D0-S)*1.0D5/ CNR(1)
          TADRT(2) = TADRT(2) + RMASS *S*1.0D5/CNR(2)
          TADRTC(1) = TADRTC(1)+RMASS*(1.0D0-S)*1.0D5
          TADRTC(2) = TADRTC(2) + RMASS *S*1.0D5
          RMASS1=RMASS*1.0D5
          IF (PCN .LE. CNR(1)) THEN
              RCO2=RCO2+RMASS1-(RMASS1*(1.0D0-S)/PCN)*CNR(1)
          ELSE IF (PCN .GE. CNR(2)) THEN
              RCO2=RCO2+RMASS1-(RMASS1*S/PCN)*CNR(2)
          endif
        END IF
   90 CONTINUE
C

      RETURN
      endif
      FIRST15 = .FALSE.
CZ-MA
      RETURN
C500   PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN IPDSSAT --'
C1000  PRINT*,' -- ERROR -- ERROR -- UNABLE TO OPEN FILEX --'
      END
       subroutine HrootStress(TURFAC,SWFAC,EOP)
       real turfac, swfac,HMIN
      REAL  RZrwu(300),RZtrwup,AVG_HROOT,WTDEP,qsr(300),TRWUP
      INTEGER ISTRESS
        DATA HMIN/15000.0/
       common /wuptake/qsr,wtdep,trwup,RZrwu,RZtrwup,istress,avg_hroot
C       TURFAC=(HMIN-AVG_HROOT)/(HMIN-1000)*(TRWUP/(EOP*0.1))
C       SWFAC=(HMIN-AVG_HROOT)/(HMIN-5000)*(TRWUP/(EOP*0.1))  
       TURFAC=(HMIN-AVG_HROOT)/(HMIN-1000)*SWFAC
       SWFAC=(HMIN-AVG_HROOT)/(HMIN-5000)*SWFAC  
       TURFAC=MIN(1.0,MAX(TURFAC,0.0))
       SWFAC=MIN(1.0,MAX(SWFAC,0.0))
       return
       end
