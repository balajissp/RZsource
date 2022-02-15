      SUBROUTINE GETPUT_MZ_CERES(CODE, STNAME, 
     +  YREND, ICSDUR, ISDATE, ISTAGE, LEAFNO, IDURP, LUNIO, 
     + MDATE, NR2, NOUTDO, RSTAGE, STGDOY, YREMRG,
     + AGEFAC, APTNUP, BIOMAS, BWAH, CANHT, CANNAA, CANWAA, 
     + CANWH, CARBO, CUMDEP, CUMDTT, CUMPH, DEPMAX, DM, 
     + DTT, EARS, EARWT, ESW, G3, GNP, GNUP, GPP, 
     + GPSM, GRNWT, GRAINN, GRORT, HI, HIP, LAI, LFWT, NSTRES,
     + KCAN, KEP, MAXLAI, P3, P5, PCNGRN, PCNVEG, PCNL, PCNSD, PCNST, 
     + PCNRT, PDWI, PGRORT, PHINT, PLA, PLAG, PODNO, PODWT, PORMIN, PTF, 
     + PLTPOP, RCNP, RLV, RLWR, ROOTN, ROWSPC, RTDEP, RTWO, RTWT, 
     + RTWTO, RUE, RWUEP1, RWUMX, SATFAC, SDEPTH, SDSIZE, SDSZ, 
     + SDWT, SEEDNO, SENLA, SDWTAH, SHELPC, SHF, SI1, SI3, 
     + SKERWT, SLA, STOVER, STOVN, STOVWT, STMWT, STMWTO, SUMDTT, SUMP, 
     + SWFAC,TANC,TLNO, TOPWT, TOTNUP, TRNU, TURFAC, UNH4, UNO3, 
     + VSTAGE, VMNC, WTCO, WTLF, WTLO, WTSO, WTNCAN, WTNVEG, WTNLF, 
     + WTNSD, WTNST, WTNUP, XHLAI, XGNP, XLAI, XN, XNTI, XSTAGE, YIELD,
     + AREALF,CLW,CSW,LAGSD,LNGPEG, SLDOT,SSDOT,WLFDOT, PHTIM, 
     + WTSD, SDNO, WTSHE, SHELN,
     + SDDES, SWIDOT, WSHIDT, ASMDOT, DISLA, NPLTD, 
     + PPLTD, WLIDOT,WRIDOT,WSIDOT)
      USE ModuleDefs
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE
      CHARACTER*10 STNAME(20)

      INTEGER YREND, ICSDUR, ISDATE, ISTAGE, LEAFNO, IDURP, LUNIO, 
     + MDATE, NR2, NOUTDO, RSTAGE, STGDOY(20), YREMRG

      REAL AGEFAC, APTNUP, BIOMAS, BWAH, CANHT, CANNAA, CANWAA, 
     + CANWH, CARBO, CUMDEP, CUMDTT, CUMPH, DEPMAX, DM, 
     + DTT, EARS, EARWT, ESW(20), G3, GNP, GNUP, GPP, 
     + GPSM, GRNWT, GRAINN, GRORT, HI, HIP, LAI, LFWT, NSTRES,
     + KCAN, KEP, MAXLAI, P3, P5, PCNGRN, PCNVEG, PCNL, PCNSD, PCNST, 
     + PCNRT, PDWI, PGRORT, PHINT, PLA, PLAG, PODNO, PODWT, PORMIN, PTF, 
     + PLTPOP, RCNP, RLV(20), RLWR, ROOTN, ROWSPC, RTDEP, RTWO, RTWT, 
     + RTWTO, RUE, RWUEP1, RWUMX, SATFAC, SDEPTH, SDSIZE, SDSZ, 
     + SDWT, SEEDNO, SENLA, SDWTAH, SHELPC, SHF(20), SI1(6), SI3(6), 
     + SKERWT, SLA, STOVER, STOVN, STOVWT, STMWT, STMWTO, SUMDTT, SUMP, 
     + SWFAC,TANC,TLNO, TOPWT, TOTNUP, TRNU, TURFAC, UNH4(20), UNO3(20), 
     + VSTAGE, VMNC, WTCO, WTLF, WTLO, WTSO, WTNCAN, WTNVEG, WTNLF, 
     + WTNSD, WTNST, WTNUP, XHLAI, XGNP, XLAI, XN, XNTI, XSTAGE, YIELD    

      REAL AREALF,CLW,CSW,LAGSD,LNGPEG, SLDOT,SSDOT,WLFDOT, PHTIM(365), 
     + WTSD(NCOHORTS), SDNO(NCOHORTS), WTSHE(NCOHORTS), SHELN(NCOHORTS),
     + SDDES(NCOHORTS), SWIDOT, WSHIDT, ASMDOT, DISLA, NPLTD, 
     + PPLTD, WLIDOT,WRIDOT,WSIDOT

      CHARACTER*10 MEM_STNAME(20)
      INTEGER MEM_YREND, MEM_ICSDUR, MEM_ISDATE, MEM_ISTAGE, MEM_LEAFNO, 
     + MEM_IDURP, MEM_LUNIO, MEM_MDATE, MEM_NR2, MEM_NOUTDO, MEM_RSTAGE,
     + MEM_STGDOY(20), MEM_YREMRG
      REAL MEM_AGEFAC, MEM_APTNUP, MEM_BIOMAS, MEM_BWAH, MEM_CANHT, 
     + MEM_CANNAA, MEM_CANWAA, MEM_CANWH, MEM_CARBO, MEM_CUMDEP, 
     + MEM_CUMDTT, MEM_CUMPH, MEM_DEPMAX, MEM_DM, MEM_DTT, MEM_EARS, 
     + MEM_EARWT, MEM_ESW(20), MEM_G3, MEM_GNP, MEM_GNUP, MEM_GPP, 
     + MEM_GPSM, MEM_GRNWT, MEM_GRAINN, MEM_GRORT, MEM_HI, MEM_HIP, 
     + MEM_LAI, MEM_LFWT, MEM_NSTRES, MEM_KCAN, MEM_KEP, MEM_MAXLAI, 
     + MEM_P3, MEM_P5, MEM_PCNGRN, MEM_PCNVEG, MEM_PCNL, MEM_PCNSD, 
     + MEM_PCNST, MEM_PCNRT, MEM_PDWI, MEM_PGRORT, MEM_PHINT, MEM_PLA, 
     + MEM_PLAG, MEM_PODNO, MEM_PODWT, MEM_PORMIN, MEM_PTF, MEM_PLTPOP, 
     + MEM_RCNP, MEM_RLV(20), MEM_RLWR, MEM_ROOTN, MEM_ROWSPC,MEM_RTDEP, 
     + MEM_RTWO, MEM_RTWT, MEM_RTWTO, MEM_RUE, MEM_RWUEP1, MEM_RWUMX, 
     + MEM_SATFAC, MEM_SDEPTH, MEM_SDSIZE, MEM_SDSZ, MEM_SDWT, 
     + MEM_SEEDNO, MEM_SENLA, MEM_SDWTAH, MEM_SHELPC, MEM_SHF(20), 
     + MEM_SI1(6), MEM_SI3(6), MEM_SKERWT, MEM_SLA, MEM_STOVER, 
     + MEM_STOVN, MEM_STOVWT, MEM_STMWT, MEM_STMWTO, MEM_SUMDTT, 
     + MEM_SUMP, MEM_SWFAC,MEM_TANC,MEM_TLNO, MEM_TOPWT, MEM_TOTNUP, 
     + MEM_TRNU, MEM_TURFAC, MEM_UNH4(20), MEM_UNO3(20), MEM_VSTAGE, 
     + MEM_VMNC, MEM_WTCO, MEM_WTLF, MEM_WTLO, MEM_WTSO, MEM_WTNCAN, 
     + MEM_WTNVEG, MEM_WTNLF, MEM_WTNSD, MEM_WTNST, MEM_WTNUP, 
     + MEM_XHLAI, MEM_XGNP, MEM_XLAI, MEM_XN, MEM_XNTI, MEM_XSTAGE, 
     + MEM_YIELD    

      REAL MEM_AREALF,MEM_CLW,MEM_CSW,MEM_LAGSD,MEM_LNGPEG, MEM_SLDOT,
     + MEM_SSDOT,MEM_WLFDOT, MEM_PHTIM(365), MEM_WTSD(NCOHORTS), 
     + MEM_SDNO(NCOHORTS), MEM_WTSHE(NCOHORTS), MEM_SHELN(NCOHORTS), 
     + MEM_SDDES(NCOHORTS), MEM_SWIDOT, MEM_WSHIDT, MEM_ASMDOT, 
     + MEM_DISLA,MEM_NPLTD,MEM_PPLTD,MEM_WLIDOT,MEM_WRIDOT, MEM_WSIDOT

      SELECT CASE(CODE)
        CASE('GET')
        STNAME = MEM_STNAME
        YREND = MEM_YREND
        ICSDUR = MEM_ICSDUR
        ISDATE = MEM_ISDATE
        ISTAGE = MEM_ISTAGE
        LEAFNO = MEM_LEAFNO
        IDURP = MEM_IDURP
        LUNIO = MEM_LUNIO
        MDATE = MEM_MDATE
        NR2 = MEM_NR2
        NOUTDO = MEM_NOUTDO
        RSTAGE = MEM_RSTAGE
        STGDOY = MEM_STGDOY
        YREMRG = MEM_YREMRG
        AGEFAC = MEM_AGEFAC
        APTNUP = MEM_APTNUP
        BIOMAS = MEM_BIOMAS
        BWAH = MEM_BWAH
        CANHT = MEM_CANHT
        CANNAA = MEM_CANNAA
        CANWAA = MEM_CANWAA
        CANWH = MEM_CANWH
        CARBO = MEM_CARBO
        CUMDEP = MEM_CUMDEP
        CUMDTT = MEM_CUMDTT
        CUMPH = MEM_CUMPH
        DEPMAX = MEM_DEPMAX
        DM = MEM_DM
        DTT = MEM_DTT
        EARS = MEM_EARS
        EARWT = MEM_EARWT
        ESW = MEM_ESW
        G3 = MEM_G3
        GNP = MEM_GNP
        GNUP = MEM_GNUP
        GPP = MEM_GPP
        GPSM = MEM_GPSM
        GRNWT = MEM_GRNWT
        GRAINN = MEM_GRAINN
        GRORT = MEM_GRORT
        HI = MEM_HI
        HIP = MEM_HIP
        LAI = MEM_LAI
        LFWT = MEM_LFWT
        NSTRES = MEM_NSTRES
        KCAN = MEM_KCAN
        KEP = MEM_KEP
        MAXLAI = MEM_MAXLAI
        P3 = MEM_P3
        P5 = MEM_P5
        PCNGRN = MEM_PCNGRN
        PCNVEG = MEM_PCNVEG
        PCNL = MEM_PCNL
        PCNSD = MEM_PCNSD
        PCNST = MEM_PCNST
        PCNRT = MEM_PCNRT
        PDWI = MEM_PDWI
        PGRORT = MEM_PGRORT
        PHINT = MEM_PHINT
        PLA = MEM_PLA
        PLAG = MEM_PLAG
        PODNO = MEM_PODNO
        PODWT = MEM_PODWT
        PORMIN = MEM_PORMIN
        PTF = MEM_PTF
        PLTPOP = MEM_PLTPOP
        RCNP = MEM_RCNP
        RLV = MEM_RLV
        RLWR = MEM_RLWR
        ROOTN = MEM_ROOTN
        ROWSPC = MEM_ROWSPC
        RTDEP = MEM_RTDEP
        RTWO = MEM_RTWO
        RTWT = MEM_RTWT
        RTWTO = MEM_RTWTO
        RUE = MEM_RUE
        RWUEP1 = MEM_RWUEP1
        RWUMX = MEM_RWUMX
        SATFAC = MEM_SATFAC
        SDEPTH = MEM_SDEPTH
        SDSIZE = MEM_SDSIZE
        SDSZ = MEM_SDSZ
        SDWT = MEM_SDWT
        SEEDNO = MEM_SEEDNO
        SENLA = MEM_SENLA
        SDWTAH = MEM_SDWTAH
        SHELPC = MEM_SHELPC
        SHF = MEM_SHF
        SI1 = MEM_SI1
        SI3 = MEM_SI3
        SKERWT = MEM_SKERWT
        SLA = MEM_SLA
        STOVER = MEM_STOVER
        STOVN = MEM_STOVN
        STOVWT = MEM_STOVWT
        STMWT = MEM_STMWT
        STMWTO = MEM_STMWTO
        SUMDTT = MEM_SUMDTT
        SUMP = MEM_SUMP
        SWFAC = MEM_SWFAC
        TANC = MEM_TANC
        TLNO = MEM_TLNO
        TOPWT = MEM_TOPWT
        TOTNUP = MEM_TOTNUP
        TRNU = MEM_TRNU
        TURFAC = MEM_TURFAC
        UNH4 = MEM_UNH4
        UNO3 = MEM_UNO3
        VSTAGE = MEM_VSTAGE
        VMNC = MEM_VMNC
        WTCO = MEM_WTCO
        WTLF = MEM_WTLF
        WTLO = MEM_WTLO
        WTSO = MEM_WTSO
        WTNCAN = MEM_WTNCAN
        WTNVEG = MEM_WTNVEG
        WTNLF = MEM_WTNLF
        WTNSD = MEM_WTNSD
        WTNST = MEM_WTNST
        WTNUP = MEM_WTNUP
        XHLAI = MEM_XHLAI
        XGNP = MEM_XGNP
        XLAI = MEM_XLAI
        XN = MEM_XN
        XNTI = MEM_XNTI
        XSTAGE = MEM_XSTAGE
        YIELD = MEM_YIELD    
        AREALF = MEM_AREALF
        CLW = MEM_CLW
        CSW = MEM_CSW
        LAGSD = MEM_LAGSD
        LNGPEG = MEM_LNGPEG
        SLDOT = MEM_SLDOT
        SSDOT = MEM_SSDOT
        WLFDOT = MEM_WLFDOT
        PHTIM = MEM_PHTIM
        WTSD = MEM_WTSD
        SDNO = MEM_SDNO
        WTSHE = MEM_WTSHE
        SHELN = MEM_SHELN
        SDDES = MEM_SDDES
        SWIDOT = MEM_SWIDOT
        WSHIDT = MEM_WSHIDT
        ASMDOT = MEM_ASMDOT
        DISLA = MEM_DISLA
        NPLTD = MEM_NPLTD
        PPLTD = MEM_PPLTD
        WLIDOT = MEM_WLIDOT
        WRIDOT = MEM_WRIDOT
        WSIDOT = MEM_WSIDOT
      CASE('PUT')
        MEM_STNAME = STNAME
        MEM_YREND = YREND
        MEM_ICSDUR = ICSDUR
        MEM_ISDATE = ISDATE
        MEM_ISTAGE = ISTAGE
        MEM_LEAFNO = LEAFNO
        MEM_IDURP = IDURP
        MEM_LUNIO = LUNIO
        MEM_MDATE = MDATE
        MEM_NR2 = NR2
        MEM_NOUTDO = NOUTDO
        MEM_RSTAGE = RSTAGE
        MEM_STGDOY = STGDOY
        MEM_YREMRG = YREMRG
        MEM_AGEFAC = AGEFAC
        MEM_APTNUP = APTNUP
        MEM_BIOMAS = BIOMAS
        MEM_BWAH = BWAH
        MEM_CANHT = CANHT
        MEM_CANNAA = CANNAA
        MEM_CANWAA = CANWAA
        MEM_CANWH = CANWH
        MEM_CARBO = CARBO
        MEM_CUMDEP = CUMDEP
        MEM_CUMDTT = CUMDTT
        MEM_CUMPH = CUMPH
        MEM_DEPMAX = DEPMAX
        MEM_DM = DM
        MEM_DTT = DTT
        MEM_EARS = EARS
        MEM_EARWT = EARWT
        MEM_ESW = ESW
        MEM_G3 = G3
        MEM_GNP = GNP
        MEM_GNUP = GNUP
        MEM_GPP = GPP
        MEM_GPSM = GPSM
        MEM_GRNWT = GRNWT
        MEM_GRAINN = GRAINN
        MEM_GRORT = GRORT
        MEM_HI = HI
        MEM_HIP = HIP
        MEM_LAI = LAI
        MEM_LFWT = LFWT
        MEM_NSTRES = NSTRES
        MEM_KCAN = KCAN
        MEM_KEP = KEP
        MEM_MAXLAI = MAXLAI
        MEM_P3 = P3
        MEM_P5 = P5
        MEM_PCNGRN = PCNGRN
        MEM_PCNVEG = PCNVEG
        MEM_PCNL = PCNL
        MEM_PCNSD = PCNSD
        MEM_PCNST = PCNST
        MEM_PCNRT = PCNRT
        PDWI = MEM_PDWI
        MEM_PGRORT = PGRORT
        MEM_PHINT = PHINT
        MEM_PLA = PLA
        MEM_PLAG = PLAG
        MEM_PODNO = PODNO
        MEM_PODWT = PODWT
        MEM_PORMIN = PORMIN
        MEM_PTF = PTF
        MEM_PLTPOP = PLTPOP
        MEM_RCNP = RCNP
        MEM_RLV = RLV
        MEM_RLWR = RLWR
        MEM_ROOTN = ROOTN
        MEM_ROWSPC = ROWSPC
        MEM_RTDEP = RTDEP
        MEM_RTWO = RTWO
        MEM_RTWT = RTWT
        MEM_RTWTO = RTWTO
        MEM_RUE = RUE
        RWUEP1 = MEM_RWUEP1
        MEM_RWUMX = RWUMX
        MEM_SATFAC = SATFAC
        MEM_SDEPTH = SDEPTH
        MEM_SDSIZE = SDSIZE
        MEM_SDSZ = SDSZ
        MEM_SDWT = SDWT
        MEM_SEEDNO = SEEDNO
        MEM_SENLA = SENLA
        MEM_SDWTAH = SDWTAH
        MEM_SHELPC = SHELPC
        MEM_SHF = SHF
        MEM_SI1 = SI1
        MEM_SI3 = SI3
        MEM_SKERWT = SKERWT
        MEM_SLA = SLA
        MEM_STOVER = STOVER
        MEM_STOVN = STOVN
        MEM_STOVWT = STOVWT
        MEM_STMWT = STMWT
        MEM_STMWTO = STMWTO
        MEM_SUMDTT = SUMDTT
        MEM_SUMP = SUMP
        MEM_SWFAC = SWFAC
        MEM_TANC = TANC
        MEM_TLNO = TLNO
        MEM_TOPWT = TOPWT
        MEM_TOTNUP = TOTNUP
        MEM_TRNU = TRNU
        MEM_TURFAC = TURFAC
        MEM_UNH4 = UNH4
        MEM_UNO3 = UNO3
        MEM_VSTAGE = VSTAGE
        MEM_VMNC = VMNC
        MEM_WTCO = WTCO
        MEM_WTLF = WTLF
        MEM_WTLO = WTLO
        MEM_WTSO = WTSO
        MEM_WTNCAN = WTNCAN
        MEM_WTNVEG = WTNVEG
        MEM_WTNLF = WTNLF
        MEM_WTNSD = WTNSD
        MEM_WTNST = WTNST
        MEM_WTNUP = WTNUP
        MEM_XHLAI = XHLAI
        MEM_XGNP = XGNP
        MEM_XLAI = XLAI
        MEM_XN = XN
        MEM_XNTI = XNTI
        MEM_XSTAGE = XSTAGE
        MEM_YIELD = YIELD    
        MEM_AREALF = AREALF
        MEM_CLW = CLW
        MEM_CSW = CSW
        MEM_LAGSD = LAGSD
        MEM_LNGPEG = LNGPEG
        MEM_SLDOT = SLDOT
        MEM_SSDOT = SSDOT
        MEM_WLFDOT = WLFDOT
        MEM_PHTIM = PHTIM
        MEM_WTSD = WTSD
        MEM_SDNO = SDNO
        MEM_WTSHE = WTSHE
        MEM_SHELN = SHELN
        MEM_SDDES = SDDES
        MEM_SWIDOT = SWIDOT
        MEM_WSHIDT = WSHIDT
        MEM_ASMDOT = ASMDOT
        MEM_DISLA = DISLA
        MEM_NPLTD = NPLTD
        MEM_PPLTD = PPLTD
        MEM_WLIDOT = WLIDOT
        MEM_WRIDOT = WRIDOT
        MEM_WSIDOT = WSIDOT
 
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_CERES


      SUBROUTINE GETPUT_MZ_NFACTO(CODE, AGEFAC, NDEF3, NFAC, NSTRES)
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE
      REAL AGEFAC, NDEF3, NFAC, NSTRES
      REAL MEM_AGEFAC, MEM_NDEF3, MEM_NFAC, MEM_NSTRES


      SELECT CASE(CODE)
        CASE('GET')
          AGEFAC = MEM_AGEFAC
          NDEF3 = MEM_NDEF3
          NFAC = MEM_NFAC
          NSTRES = MEM_NSTRES
        CASE('PUT')
          MEM_AGEFAC = AGEFAC
          MEM_NDEF3 = NDEF3
          MEM_NFAC = NFAC
          MEM_NSTRES = NSTRES
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_NFACTO


      SUBROUTINE GETPUT_MZ_GROSUB(CODE, SECTION, FILEIO, C80, FILECC, 
     +  AREALF, ASMDOT, BD, CANHT, CANWH, CARBOT, CO2X, CO2Y, 
     +  DUMMY, EP1, pp1, pp2, FSLFW, FSLFN, GRF, GROEAR, GROGRN, 
     +  GROLF, GROSTM, HI, HIP, LAIDOT, LEAFNOE, CumLeafSenes, 
     +  CumLeafSenesY, CumLfNSenes, LFWTE, LIFAC, TAW, DTAW, NDEF3, 
     +  NFAC, NPOOL, NPOOL1, NPOOL2, NSDR, NSINK, P3, PAR, 
     +  PARSR, PC, PCARB, PCNGRN, PCNL, PCNRT, PCNSD, PCNST, PCO2, 
     +  PRFTC, SLPF, PLAE, PLAS, PODNO, PODWT, PPLTD, PRFT, RANC, 
     +  RANCE, RGFILL, RGFIL, RLWR, RMNC, RNLAB, RNOUT, RSGR, RSGRT, 
     +  RTWO, RTWTE, RTWTO, RUE, RWUMX, SATFAC, SDSIZE, SDSZ, SDWT, 
     +  SEEDNO, SEEDRV, SEEDRVE, SFAC, SHELPC, SI1, SI2, SI3, SI4, 
     +  SLAN, SLA, SLFC, SLFN, SLFT, SLFW, Stg2CLS, STMWTO, STMWTE, 
     +  SUMEX, SUMRL, SWEXF, SWIDOT, SWMAX, SWMIN, TANCE, 
     +  TAVGD, TCNP, TEMPM, TFAC, TI, TMNC, TNLAB, TOPWT, TSS, 
     +  VANC, VSTAGE, WLIDOT, WRIDOT, WSIDOT, WTLF, XANC, XLFWT, 
     +  XNF, YIELDB, RZrwu, RZtrwup, AVG_HROOT, WTDEP, qsr, TRWUP1, 
     +  CMAT, EMAT, FOUND, I, ICOLD, ISECT, L, LINC, LNUM, LUNCRP, 
     +  LUNIO, NWSD, RSTAGE, PATHL, yrplt, ISTRESS, iresetlai,iresetht1,
     +  alaireset, WSI, heightset)
C       USE ModuleDefs
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE

      CHARACTER*6     SECTION 
      CHARACTER*30    FILEIO 
      CHARACTER*80    C80
      CHARACTER*255   FILECC

      REAL        AREALF, ASMDOT, BD(20), CANHT, CANWH, CARBOT, 
     + CO2X(10), CO2Y(10), DUMMY, EP1, pp1, pp2, FSLFW, FSLFN, 
     + GRF, GROEAR, GROGRN, GROLF, GROSTM, HI, HIP, LAIDOT, LEAFNOE,
     + CumLeafSenes, CumLeafSenesY, CumLfNSenes, LFWTE, LIFAC, TAW,DTAW, 
     + NDEF3, NFAC, NPOOL, NPOOL1, NPOOL2, NSDR, NSINK, P3, PAR, 
     + PARSR, PC, PCARB, PCNGRN, PCNL, PCNRT, PCNSD, PCNST, PCO2, 
     + PRFTC(4), SLPF, PLAE, PLAS, PODNO, PODWT, PPLTD, PRFT, RANC, 
     + RANCE, RGFILL, RGFIL(4), RLWR, RMNC, RNLAB, RNOUT, RSGR, RSGRT, 
     + RTWO, RTWTE, RTWTO, RUE, RWUMX, SATFAC, SDSIZE, SDSZ, SDWT, 
     + SEEDNO, SEEDRV, SEEDRVE, SFAC, SHELPC, SI1(6), SI2(6), SI3(6), 
     + SI4(6), SLAN, SLA, SLFC, SLFN, SLFT, SLFW, Stg2CLS, STMWTO, 
     + STMWTE, SUMEX, SUMRL, SWEXF, SWIDOT, SWMAX, SWMIN, 
     + TANCE, TAVGD, TCNP, TEMPM, TFAC, TI, TMNC, TNLAB, TOPWT, 
     + TSS(20), VANC, VSTAGE, WLIDOT, WRIDOT, WSIDOT, WTLF, XANC, 
     + XLFWT, XNF, YIELDB, RZrwu(300), RZtrwup, AVG_HROOT, WTDEP, 
     + qsr(300), TRWUP1
      INTEGER     CMAT, EMAT, FOUND, I, ICOLD, ISECT, L, LINC, LNUM, 
     + LUNCRP, LUNIO, NWSD, RSTAGE, PATHL, yrplt, ISTRESS, iresetlai, 
     + iresetht1
      DOUBLE PRECISION alaireset, WSI(10),heightset

      CHARACTER*6     MEM_SECTION 
      CHARACTER*30    MEM_FILEIO 
      CHARACTER*80    MEM_C80
      CHARACTER*255   MEM_FILECC

      REAL     MEM_AREALF, MEM_ASMDOT, MEM_BD(20), MEM_CANHT, MEM_CANWH,
     + MEM_CARBOT, MEM_CO2X(10), MEM_CO2Y(10), MEM_DUMMY, MEM_EP1, 
     + MEM_pp1, MEM_pp2, MEM_FSLFW, MEM_FSLFN, MEM_GRF, MEM_GROEAR, 
     + MEM_GROGRN, MEM_GROLF, MEM_GROSTM, MEM_HI, MEM_HIP, MEM_LAIDOT, 
     + MEM_LEAFNOE, MEM_CumLeafSenes, MEM_CumLeafSenesY, MEM_CumLfNSenes
     + , MEM_LFWTE, MEM_LIFAC, MEM_TAW, MEM_DTAW, MEM_NDEF3, MEM_NFAC, 
     + MEM_NPOOL, MEM_NPOOL1, MEM_NPOOL2, MEM_NSDR, MEM_NSINK, MEM_P3, 
     + MEM_PAR, MEM_PARSR, MEM_PC, MEM_PCARB, MEM_PCNGRN, MEM_PCNL, 
     + MEM_PCNRT, MEM_PCNSD, MEM_PCNST, MEM_PCO2, MEM_PRFTC(4), MEM_SLPF
     + , MEM_PLAE, MEM_PLAS, MEM_PODNO, MEM_PODWT, MEM_PPLTD, MEM_PRFT, 
     + MEM_RANC, MEM_RANCE, MEM_RGFILL, MEM_RGFIL(4), MEM_RLWR, MEM_RMNC
     + , MEM_RNLAB, MEM_RNOUT, MEM_RSGR, MEM_RSGRT, MEM_RTWO, MEM_RTWTE, 
     + MEM_RTWTO, MEM_RUE, MEM_RWUMX, MEM_SATFAC, MEM_SDSIZE, MEM_SDSZ, 
     + MEM_SDWT, MEM_SEEDNO, MEM_SEEDRV, MEM_SEEDRVE, MEM_SFAC, 
     + MEM_SHELPC, MEM_SI1(6), MEM_SI2(6), MEM_SI3(6), MEM_SI4(6), 
     + MEM_SLAN, MEM_SLA, MEM_SLFC, MEM_SLFN, MEM_SLFT, MEM_SLFW, 
     + MEM_Stg2CLS, MEM_STMWTO, MEM_STMWTE, MEM_SUMEX, MEM_SUMRL, 
     + MEM_SWEXF, MEM_SWIDOT, MEM_SWMAX, MEM_SWMIN, MEM_TANCE,MEM_TAVGD, 
     + MEM_TCNP, MEM_TEMPM, MEM_TFAC, MEM_TI, MEM_TMNC, MEM_TNLAB, 
     + MEM_TOPWT, MEM_TSS(20), MEM_VANC, MEM_VSTAGE, MEM_WLIDOT, 
     + MEM_WRIDOT, MEM_WSIDOT, MEM_WTLF, MEM_XANC, MEM_XLFWT, MEM_XNF, 
     + MEM_YIELDB, MEM_RZrwu(300), MEM_RZtrwup, MEM_AVG_HROOT, MEM_WTDEP
     + , MEM_qsr(300), MEM_TRWUP1
      INTEGER  MEM_CMAT, MEM_EMAT, MEM_FOUND, MEM_I, MEM_ICOLD, 
     + MEM_ISECT, MEM_L, MEM_LINC, MEM_LNUM, MEM_LUNCRP, MEM_LUNIO, 
     + MEM_NWSD, MEM_RSTAGE, MEM_PATHL, MEM_yrplt, MEM_ISTRESS, 
     + MEM_iresetlai, MEM_iresetht1
      DOUBLE PRECISION MEM_alaireset, MEM_WSI(10), MEM_heightset

      SELECT CASE(CODE)
        CASE('GET')
          SECTION = MEM_SECTION
          FILEIO = MEM_FILEIO
          C80 = MEM_C80
          FILECC = MEM_FILECC
          AREALF = MEM_AREALF
          ASMDOT = MEM_ASMDOT
          BD = MEM_BD
          CANHT = MEM_CANHT 
          CANWH = MEM_CANWH 
          CARBOT = MEM_CARBOT 
          CO2X = MEM_CO2X
          CO2Y = MEM_CO2Y
          DUMMY = MEM_DUMMY 
          EP1 = MEM_EP1
          pp1 = MEM_pp1
          pp2 = MEM_pp2
          FSLFW = MEM_FSLFW
          FSLFN = MEM_FSLFN 
          GRF = MEM_GRF 
          GROEAR = MEM_GROEAR 
          GROGRN = MEM_GROGRN 
          GROLF = MEM_GROLF 
          GROSTM = MEM_GROSTM 
          HI = MEM_HI
          HIP = MEM_HIP
          LAIDOT = MEM_LAIDOT
          LEAFNOE = MEM_LEAFNOE 
          CumLeafSenes = MEM_CumLeafSenes
          CumLeafSenesY = MEM_CumLeafSenesY
          CumLfNSenes = MEM_CumLfNSenes
          LFWTE = MEM_LFWTE
          LIFAC = MEM_LIFAC
          TAW = MEM_TAW
          DTAW = MEM_DTAW
          NDEF3 = MEM_NDEF3 
          NFAC = MEM_NFAC 
          NPOOL = MEM_NPOOL 
          NPOOL1 = MEM_NPOOL1 
          NPOOL2 = MEM_NPOOL2 
          NSDR = MEM_NSDR 
          NSINK = MEM_NSINK 
          P3 = MEM_P3 
          PAR = MEM_PAR 
          PARSR = MEM_PARSR
          PC = MEM_PC 
          PCARB = MEM_PCARB
          PCNGRN = MEM_PCNGRN
          PCNL = MEM_PCNL 
          PCNRT = MEM_PCNRT 
          PCNSD = MEM_PCNSD
          PCNST = MEM_PCNST
          PCO2 = MEM_PCO2 
          PRFTC = MEM_PRFTC
          SLPF = MEM_SLPF
          PLAE = MEM_PLAE 
          PLAS = MEM_PLAS 
          PODNO = MEM_PODNO
          PODWT = MEM_PODWT
          PPLTD = MEM_PPLTD 
          PRFT = MEM_PRFT 
          RANC = MEM_RANC 
          RANCE = MEM_RANCE
          RGFILL = MEM_RGFILL 
          RGFIL = MEM_RGFIL
          RLWR = MEM_RLWR
          RMNC = MEM_RMNC 
          RNLAB = MEM_RNLAB
          RNOUT = MEM_RNOUT
          RSGR = MEM_RSGR
          RSGRT = MEM_RSGRT
          RTWO = MEM_RTWO
          RTWTE = MEM_RTWTE
          RTWTO = MEM_RTWTO 
          RUE = MEM_RUE 
          RWUMX = MEM_RWUMX
          SATFAC = MEM_SATFAC 
          SDSIZE = MEM_SDSIZE
          SDSZ = MEM_SDSZ
          SDWT = MEM_SDWT
          SEEDNO = MEM_SEEDNO
          SEEDRV = MEM_SEEDRV 
          SEEDRVE = MEM_SEEDRVE
          SFAC = MEM_SFAC 
          SHELPC = MEM_SHELPC
          SI1 = MEM_SI1
          SI2 = MEM_SI2
          SI3 = MEM_SI3
          SI4 = MEM_SI4
          SLAN = MEM_SLAN 
          SLA = MEM_SLA
          SLFC = MEM_SLFC 
          SLFN = MEM_SLFN 
          SLFT = MEM_SLFT 
          SLFW = MEM_SLFW 
          Stg2CLS = MEM_Stg2CLS 
          STMWTO = MEM_STMWTO
          STMWTE = MEM_STMWTE 
          SUMEX = MEM_SUMEX
          SUMRL = MEM_SUMRL 
          SWEXF = MEM_SWEXF 
          SWIDOT = MEM_SWIDOT 
          SWMAX = MEM_SWMAX 
          SWMIN = MEM_SWMIN 
          TANCE = MEM_TANCE
          TAVGD = MEM_TAVGD 
          TCNP = MEM_TCNP 
          TEMPM = MEM_TEMPM 
          TFAC = MEM_TFAC 
          TI = MEM_TI 
          TMNC = MEM_TMNC 
          TNLAB = MEM_TNLAB
          TOPWT = MEM_TOPWT
          TSS = MEM_TSS
          VANC = MEM_VANC 
          VSTAGE = MEM_VSTAGE 
          WLIDOT = MEM_WLIDOT
          WRIDOT = MEM_WRIDOT
          WSIDOT = MEM_WSIDOT 
          WTLF = MEM_WTLF
          XANC = MEM_XANC 
          XLFWT = MEM_XLFWT 
          XNF = MEM_XNF 
          YIELDB = MEM_YIELDB 
          RZrwu = MEM_RZrwu
          RZtrwup = MEM_RZtrwup
          AVG_HROOT = MEM_AVG_HROOT
          WTDEP = MEM_WTDEP
          qsr = MEM_qsr
          TRWUP1 = MEM_TRWUP1
          CMAT = MEM_CMAT
          EMAT = MEM_EMAT
          FOUND = MEM_FOUND
          I = MEM_I
          ICOLD = MEM_ICOLD
          ISECT = MEM_ISECT
          L = MEM_L
          LINC = MEM_LINC
          LNUM = MEM_LNUM
          LUNCRP = MEM_LUNCRP
          LUNIO = MEM_LUNIO
          NWSD = MEM_NWSD
          RSTAGE = MEM_RSTAGE
          PATHL = MEM_PATHL
          yrplt = MEM_yrplt
          ISTRESS = MEM_ISTRESS
          iresetlai = MEM_iresetlai
          iresetht1 = MEM_iresetht1
          alaireset = MEM_alaireset
          WSI = MEM_WSI
          heightset = MEM_heightset
        CASE('PUT')
          MEM_SECTION = SECTION
          MEM_FILEIO = FILEIO
          MEM_C80 = C80
          MEM_FILECC = FILECC
          MEM_AREALF = AREALF
          MEM_ASMDOT = ASMDOT
          MEM_BD = BD
          MEM_CANHT = CANHT
          MEM_CANWH = CANWH
          MEM_CARBOT = CARBOT
          MEM_CO2X = CO2X
          MEM_CO2Y = CO2Y
          MEM_DUMMY = DUMMY
          MEM_EP1 = EP1
          MEM_pp1 = pp1
          MEM_pp2 = pp2
          MEM_FSLFW = FSLFW
          MEM_FSLFN = FSLFN
          MEM_GRF = GRF
          MEM_GROEAR = GROEAR
          MEM_GROGRN = GROGRN
          MEM_GROLF = GROLF
          MEM_GROSTM = GROSTM
          MEM_HI = HI
          MEM_HIP = HIP
          MEM_LAIDOT = LAIDOT
          MEM_LEAFNOE = LEAFNOE
          MEM_CumLeafSenes = CumLeafSenes
          MEM_CumLeafSenesY = CumLeafSenesY
          MEM_CumLfNSenes = CumLfNSenes
          MEM_LFWTE = LFWTE
          MEM_LIFAC = LIFAC
          MEM_TAW = TAW
          MEM_DTAW = DTAW
          MEM_NDEF3 = NDEF3
          MEM_NFAC = NFAC
          MEM_NPOOL = NPOOL
          MEM_NPOOL1 = NPOOL1
          MEM_NPOOL2 = NPOOL2
          MEM_NSDR = NSDR
          MEM_NSINK = NSINK
          MEM_P3 = P3
          MEM_PAR = PAR
          MEM_PARSR = PARSR
          MEM_PC = PC
          MEM_PCARB = PCARB
          MEM_PCNGRN = PCNGRN
          MEM_PCNL = PCNL
          MEM_PCNRT = PCNRT
          MEM_PCNSD = PCNSD
          MEM_PCNST = PCNST
          MEM_PCO2 = PCO2
          MEM_PRFTC = PRFTC
          MEM_SLPF = SLPF
          MEM_PLAE = PLAE
          MEM_PLAS = PLAS
          MEM_PODNO = PODNO
          MEM_PODWT = PODWT
          MEM_PPLTD = PPLTD
          MEM_PRFT = PRFT
          MEM_RANC = RANC
          MEM_RANCE = RANCE
          MEM_RGFILL = RGFILL
          MEM_RGFIL = RGFIL
          MEM_RLWR = RLWR
          MEM_RMNC = RMNC
          MEM_RNLAB = RNLAB
          MEM_RNOUT = RNOUT
          MEM_RSGR = RSGR
          MEM_RSGRT = RSGRT
          MEM_RTWO = RTWO
          MEM_RTWTE = RTWTE
          MEM_RTWTO = RTWTO
          MEM_RUE = RUE
          MEM_RWUMX = RWUMX
          MEM_SATFAC = SATFAC
          MEM_SDSIZE = SDSIZE
          MEM_SDSZ = SDSZ
          MEM_SDWT = SDWT
          MEM_SEEDNO = SEEDNO
          MEM_SEEDRV = SEEDRV
          MEM_SEEDRVE = SEEDRVE
          MEM_SFAC = SFAC
          MEM_SHELPC = SHELPC
          MEM_SI1 = SI1
          MEM_SI2 = SI2
          MEM_SI3 = SI3
          MEM_SI4 = SI4
          MEM_SLAN = SLAN
          MEM_SLA = SLA
          MEM_SLFC = SLFC
          MEM_SLFN = SLFN
          MEM_SLFT = SLFT
          MEM_SLFW = SLFW
          MEM_Stg2CLS = Stg2CLS
          MEM_STMWTO = STMWTO
          MEM_STMWTE = STMWTE
          MEM_SUMEX = SUMEX
          MEM_SUMRL = SUMRL
          MEM_SWEXF = SWEXF
          MEM_SWIDOT = SWIDOT
          MEM_SWMAX = SWMAX
          MEM_SWMIN = SWMIN
          MEM_TANCE = TANCE
          MEM_TAVGD = TAVGD
          MEM_TCNP = TCNP
          MEM_TEMPM = TEMPM
          MEM_TFAC = TFAC
          MEM_TI = TI
          MEM_TMNC = TMNC
          MEM_TNLAB = TNLAB
          MEM_TOPWT = TOPWT
          MEM_TSS = TSS
          MEM_VANC = VANC
          MEM_VSTAGE = VSTAGE
          MEM_WLIDOT = WLIDOT
          MEM_WRIDOT = WRIDOT
          MEM_WSIDOT = WSIDOT
          MEM_WTLF = WTLF
          MEM_XANC = XANC
          MEM_XLFWT = XLFWT
          MEM_XNF = XNF
          MEM_YIELDB = YIELDB
          MEM_RZrwu = RZrwu
          MEM_RZtrwup = RZtrwup
          MEM_AVG_HROOT = AVG_HROOT
          MEM_WTDEP = WTDEP
          MEM_qsr = qsr
          MEM_TRWUP1 = TRWUP1
          MEM_CMAT = CMAT
          MEM_EMAT = EMAT
          MEM_FOUND = FOUND
          MEM_I = I
          MEM_ICOLD = ICOLD
          MEM_ISECT = ISECT
          MEM_L = L
          MEM_LINC = LINC
          MEM_LNUM = LNUM
          MEM_LUNCRP = LUNCRP
          MEM_LUNIO = LUNIO
          MEM_NWSD = NWSD
          MEM_RSTAGE = RSTAGE
          MEM_PATHL = PATHL
          MEM_yrplt = yrplt
          MEM_ISTRESS = ISTRESS
          MEM_iresetlai = iresetlai
          MEM_iresetht1 = iresetht1
          MEM_alaireset = alaireset
          MEM_WSI = WSI
          MEM_heightset = heightset
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_GROSUB



      SUBROUTINE GETPUT_MZ_PHENOL(CODE, ABSTRES, ACOEF, BARFAC, C1, 
     + CUMDTT, DEC, DGET, DJTI, DLV, DOPT, DSGT, DSGFT, DTT, DUMMY, 
     + EARS, GDDE, GPP, KCAN, KEP, P2O, P3, P9, PDTT, PSKER, RATEIN, 
     + ROPT, RUE, S1, SIND, SNDN, SNUP, SUMDTT, SWCG, SWSD, TBASE, 
     + TDSOIL, TEMPCN, TEMPCR, TEMPCX, TH, TLNO, TMSOIL, TNSOIL, TOPT, 
     + XNTI, XS, XSTAGE, tdsoil1, tnsoil1, tmsoil1, FOUND, ISTAGE, L, 
     + L0, LINC, LNUM, LUNIO, MDATE, NDAS, YREMRG, ISDATE, PATHL, 
     + LUNECO, ISECT, LUNCRP, STGDOY, SECTION, ECOTYP, C255, FILEGC, 
     + ECONAM, C80)
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE

      REAL            ABSTRES         
      REAL            ACOEF           
      REAL            BARFAC 
      REAL            C1              
      REAL            CUMDTT          
      REAL            DEC             
      REAL            DGET
      REAL            DJTI
      REAL            DLV             
      REAL            DOPT                      
      REAL            DSGT
      REAL            DSGFT
      REAL            DTT             
      REAL            DUMMY           
      REAL            EARS            
      REAL            GDDE
      REAL            GPP            
      REAL            KCAN
      REAL            KEP
      REAL            P2O            
      REAL            P3             
      REAL            P9             
      REAL            PDTT
      REAL            PSKER          
      REAL            RATEIN         
      REAL            ROPT           
      REAL            RUE
      REAL            S1             
      REAL            SIND           
      REAL            SNDN           
      REAL            SNUP           
      REAL            SUMDTT         
      REAL            SWCG
      REAL            SWSD           
      REAL            TBASE          
      REAL            TDSOIL         
      REAL            TEMPCN         
                                     
      REAL            TEMPCR         
      REAL            TEMPCX         
      REAL            TH             
      REAL            TLNO           
      REAL            TMSOIL         
      REAL            TNSOIL         
      REAL            TOPT           
      REAL            XNTI           
      REAL            XS             
      REAL            XSTAGE, tdsoil1,tnsoil1,tmsoil1         

      INTEGER         FOUND          
      INTEGER         ISTAGE         
      INTEGER         L              
      INTEGER         L0             
      INTEGER         LINC           
      INTEGER         LNUM           
      INTEGER         LUNIO          
      INTEGER         MDATE          
      INTEGER         NDAS           
      INTEGER         YREMRG         
      INTEGER ISDATE          
      INTEGER PATHL,IPATH
      INTEGER LUNECO
      INTEGER ISECT
      INTEGER LUNCRP
      INTEGER         STGDOY(20)     

      CHARACTER*6     SECTION        
      CHARACTER*6 ECOTYP
      CHARACTER*255 C255
      CHARACTER*255    FILEGC
      CHARACTER*16  ECONAM
      CHARACTER*80 C80

      REAL            MEM_ABSTRES         
      REAL            MEM_ACOEF           
      REAL            MEM_BARFAC 
      REAL            MEM_C1              
      REAL            MEM_CUMDTT          
      REAL            MEM_DEC             
      REAL            MEM_DGET
      REAL            MEM_DJTI
      REAL            MEM_DLV             
      REAL            MEM_DOPT                      
      REAL            MEM_DSGT
      REAL            MEM_DSGFT
      REAL            MEM_DTT             
      REAL            MEM_DUMMY           
      REAL            MEM_EARS            
      REAL            MEM_GDDE
      REAL            MEM_GPP            
      REAL            MEM_KCAN
      REAL            MEM_KEP
      REAL            MEM_P2O            
      REAL            MEM_P3             
      REAL            MEM_P9             
      REAL            MEM_PDTT
      REAL            MEM_PSKER          
      REAL            MEM_RATEIN         
      REAL            MEM_ROPT           
      REAL            MEM_RUE
      REAL            MEM_S1             
      REAL            MEM_SIND           
      REAL            MEM_SNDN           
      REAL            MEM_SNUP           
      REAL            MEM_SUMDTT         
      REAL            MEM_SWCG
      REAL            MEM_SWSD           
      REAL            MEM_TBASE          
      REAL            MEM_TDSOIL         
      REAL            MEM_TEMPCN         
                                     
      REAL            MEM_TEMPCR         
      REAL            MEM_TEMPCX         
      REAL            MEM_TH             
      REAL            MEM_TLNO           
      REAL            MEM_TMSOIL         
      REAL            MEM_TNSOIL         
      REAL            MEM_TOPT           
      REAL            MEM_XNTI           
      REAL            MEM_XS             
      REAL            MEM_XSTAGE, MEM_tdsoil1,MEM_tnsoil1,MEM_tmsoil1         

      INTEGER         MEM_FOUND          
      INTEGER         MEM_ISTAGE         
      INTEGER         MEM_L              
      INTEGER         MEM_L0             
      INTEGER         MEM_LINC           
      INTEGER         MEM_LNUM           
      INTEGER         MEM_LUNIO          
      INTEGER         MEM_MDATE          
      INTEGER         MEM_NDAS           
      INTEGER         MEM_YREMRG         
      INTEGER MEM_ISDATE          
      INTEGER MEM_PATHL,MEM_IPATH
      INTEGER MEM_LUNECO
      INTEGER MEM_ISECT
      INTEGER MEM_LUNCRP
      INTEGER MEM_STGDOY(20)     

      CHARACTER*6     MEM_SECTION        
      CHARACTER*6 MEM_ECOTYP
      CHARACTER*255 MEM_C255
      CHARACTER*255   MEM_FILEGC
      CHARACTER*16  MEM_ECONAM
      CHARACTER*80 MEM_C80



      SELECT CASE(CODE)
        CASE('GET')
            ABSTRES = MEM_ABSTRES         
            ACOEF = MEM_ACOEF           
            BARFAC = MEM_BARFAC 
            C1 = MEM_C1              
            CUMDTT = MEM_CUMDTT          
            DEC = MEM_DEC             
            DGET = MEM_DGET
            DJTI = MEM_DJTI
            DLV = MEM_DLV             
            DOPT = MEM_DOPT                      
            DSGT = MEM_DSGT
            DSGFT = MEM_DSGFT
            DTT = MEM_DTT             
            DUMMY = MEM_DUMMY           
            EARS = MEM_EARS            
            GDDE = MEM_GDDE
            GPP = MEM_GPP            
            KCAN = MEM_KCAN
            KEP = MEM_KEP
            P2O = MEM_P2O            
            P3 = MEM_P3             
            P9 = MEM_P9             
            PDTT = MEM_PDTT
            PSKER = MEM_PSKER          
            RATEIN = MEM_RATEIN         
            ROPT = MEM_ROPT           
            RUE = MEM_RUE
            S1 = MEM_S1             
            SIND = MEM_SIND           
            SNDN = MEM_SNDN           
            SNUP = MEM_SNUP           
            SUMDTT = MEM_SUMDTT         
            SWCG = MEM_SWCG
            SWSD = MEM_SWSD           
            TBASE = MEM_TBASE          
            TDSOIL = MEM_TDSOIL         
            TEMPCN = MEM_TEMPCN         
            TEMPCR = MEM_TEMPCR         
            TEMPCX = MEM_TEMPCX         
            TH = MEM_TH             
            TLNO = MEM_TLNO           
            TMSOIL = MEM_TMSOIL         
            TNSOIL = MEM_TNSOIL         
            TOPT = MEM_TOPT           
            XNTI = MEM_XNTI           
            XS = MEM_XS             
            XSTAGE = MEM_XSTAGE
            tdsoil1 = MEM_tdsoil1
            tnsoil1 = MEM_tnsoil1
            tmsoil1 = MEM_tmsoil1         
            FOUND = MEM_FOUND          
            ISTAGE = MEM_ISTAGE         
            L = MEM_L              
            L0 = MEM_L0             
            LINC = MEM_LINC           
            LNUM = MEM_LNUM           
            LUNIO = MEM_LUNIO          
            MDATE = MEM_MDATE          
            NDAS = MEM_NDAS           
            YREMRG = MEM_YREMRG         
            ISDATE = MEM_ISDATE          
            PATHL = MEM_PATHL
            IPATH = MEM_IPATH
            LUNECO = MEM_LUNECO
            ISECT = MEM_ISECT
            LUNCRP = MEM_LUNCRP
            STGDOY = MEM_STGDOY
            SECTION = MEM_SECTION        
            ECOTYP = MEM_ECOTYP
            C255 = MEM_C255
            FILEGC = MEM_FILEGC
            ECONAM = MEM_ECONAM
            C80 = MEM_C80
        CASE('PUT')
            MEM_ABSTRES = ABSTRES         
            MEM_ACOEF = ACOEF           
            MEM_BARFAC = BARFAC 
            MEM_C1 = C1              
            MEM_CUMDTT = CUMDTT          
            MEM_DEC = DEC             
            MEM_DGET = DGET
            MEM_DJTI = DJTI
            MEM_DLV = DLV             
            MEM_DOPT = DOPT                      
            MEM_DSGT = DSGT
            MEM_DSGFT = DSGFT
            MEM_DTT = DTT             
            MEM_DUMMY = DUMMY           
            MEM_EARS = EARS            
            MEM_GDDE = GDDE
            MEM_GPP = GPP            
            MEM_KCAN = KCAN
            MEM_KEP = KEP
            MEM_P2O = P2O            
            MEM_P3 = P3             
            MEM_P9 = P9             
            MEM_PDTT = PDTT
            MEM_PSKER = PSKER          
            MEM_RATEIN = RATEIN         
            MEM_ROPT = ROPT           
            MEM_RUE = RUE
            MEM_S1 = S1             
            MEM_SIND = SIND           
            MEM_SNDN = SNDN           
            MEM_SNUP = SNUP           
            MEM_SUMDTT = SUMDTT         
            MEM_SWCG = SWCG
            MEM_SWSD = SWSD           
            MEM_TBASE = TBASE          
            MEM_TDSOIL = TDSOIL         
            MEM_TEMPCN = TEMPCN         
            MEM_TEMPCR = TEMPCR         
            MEM_TEMPCX = TEMPCX         
            MEM_TH = TH             
            MEM_TLNO = TLNO           
            MEM_TMSOIL = TMSOIL         
            MEM_TNSOIL = TNSOIL         
            MEM_TOPT = TOPT           
            MEM_XNTI = XNTI           
            MEM_XS = XS             
            MEM_XSTAGE = XSTAGE
            MEM_tdsoil1 = tdsoil1
            MEM_tnsoil1 = tnsoil1
            MEM_tmsoil1 = tmsoil1         
            MEM_FOUND = FOUND          
            MEM_ISTAGE = ISTAGE         
            MEM_L = L              
            MEM_L0 = L0             
            MEM_LINC = LINC           
            MEM_LNUM = LNUM           
            MEM_LUNIO = LUNIO          
            MEM_MDATE = MDATE          
            MEM_NDAS = NDAS           
            MEM_YREMRG = YREMRG         
            MEM_ISDATE = ISDATE          
            MEM_PATHL = PATHL
            MEM_IPATH = IPATH
            MEM_LUNECO = LUNECO
            MEM_ISECT = ISECT
            MEM_LUNCRP = LUNCRP
            MEM_STGDOY = STGDOY
            MEM_SECTION = SECTION        
            MEM_ECOTYP = ECOTYP
            MEM_C255 = C255
            MEM_FILEGC = FILEGC
            MEM_ECONAM = ECONAM
            MEM_C80 = C80
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_PHENOL


      SUBROUTINE GETPUT_MZ_ROOTGR(CODE, L1, RLDF, RLNEW, RLV, RNFAC, 
     +      RNLF, RTDEP, RTEXF, RTSURV, SWDF, SWEXF, TRLDF)
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE
      INTEGER     L1       
      REAL        RLDF(20), RLNEW, RLV(20), RNFAC, RNLF, RTDEP, RTEXF, 
     + RTSURV, SWDF, SWEXF, TRLDF      

      INTEGER     MEM_L1       
      REAL        MEM_RLDF(20), MEM_RLNEW, MEM_RLV(20), MEM_RNFAC, 
     + MEM_RNLF, MEM_RTDEP, MEM_RTEXF, 
     + MEM_RTSURV, MEM_SWDF, MEM_SWEXF, MEM_TRLDF      

      SELECT CASE(CODE)
        CASE('GET')
            L1 = MEM_L1
            RLDF = MEM_RLDF
            RLNEW = MEM_RLNEW
            RLV = MEM_RLV
            RNFAC = MEM_RNFAC
            RNLF = MEM_RNLF
            RTDEP = MEM_RTDEP
            RTEXF = MEM_RTEXF
            RTSURV = MEM_RTSURV
            SWDF = MEM_SWDF
            SWEXF = MEM_SWEXF
            TRLDF = MEM_TRLDF
        CASE('PUT')
            MEM_L1 = L1
            MEM_RLDF = RLDF
            MEM_RLNEW = RLNEW
            MEM_RLV = RLV
            MEM_RNFAC = RNFAC
            MEM_RNLF = RNLF
            MEM_RTDEP = RTDEP
            MEM_RTEXF = RTEXF
            MEM_RTSURV = RTSURV
            MEM_SWDF = SWDF
            MEM_SWEXF = SWEXF
            MEM_TRLDF = TRLDF
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_ROOTGR


      SUBROUTINE GETPUT_MZ_OPNIT(CODE, DAS, NOUTDN, DAP,L, NLAYR
     + , OUTPN, PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, 
     + WTNLF, WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, FEXIST, 
     + FIRST)
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE
      INTEGER DAS, NOUTDN, DAP,L, NLAYR
      CHARACTER*12 OUTPN
      REAL PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF, 
     + WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN
      LOGICAL FEXIST, FIRST

      INTEGER MEM_DAS, MEM_NOUTDN, MEM_DAP, MEM_L, MEM_NLAYR
      CHARACTER*12 MEM_OUTPN
      REAL MEM_PCNL, MEM_WTNCAN, MEM_WTNSD, MEM_WTNVEG, MEM_PCNGRN, 
     + MEM_PCNVEG, MEM_WTNUP, MEM_WTNLF, MEM_WTNST, MEM_PCNST, 
     + MEM_PCNRT, MEM_CUMSENSURFN, MEM_CUMSENSOILN
      LOGICAL MEM_FEXIST, MEM_FIRST

      SELECT CASE(CODE)
        CASE('GET')
          DAS = MEM_DAS
          NOUTDN = MEM_NOUTDN
          DAP = MEM_DAP
          L = MEM_L
          NLAYR = MEM_NLAYR
          OUTPN = MEM_OUTPN
          PCNL = MEM_PCNL
          WTNCAN = MEM_WTNCAN
          WTNSD = MEM_WTNSD
          WTNVEG = MEM_WTNVEG
          PCNGRN = MEM_PCNGRN
          PCNVEG = MEM_PCNVEG
          WTNUP = MEM_WTNUP
          WTNLF = MEM_WTNLF
          WTNST = MEM_WTNST
          PCNST = MEM_PCNST
          PCNRT = MEM_PCNRT
          CUMSENSURFN = MEM_CUMSENSURFN
          CUMSENSOILN = MEM_CUMSENSOILN
          FEXIST = MEM_FEXIST
          FIRST = MEM_FIRST
        CASE('PUT')
          MEM_DAS = DAS
          MEM_NOUTDN = NOUTDN
          MEM_DAP = DAP
          MEM_L = L
          MEM_NLAYR = NLAYR
          MEM_OUTPN = OUTPN
          MEM_PCNL = PCNL
          MEM_WTNCAN = WTNCAN
          MEM_WTNSD = WTNSD
          MEM_WTNVEG = WTNVEG
          MEM_PCNGRN = PCNGRN
          MEM_PCNVEG = PCNVEG
          MEM_WTNUP = WTNUP
          MEM_WTNLF = WTNLF
          MEM_WTNST = WTNST
          MEM_PCNST = PCNST
          MEM_PCNRT = PCNRT
          MEM_CUMSENSURFN = CUMSENSURFN
          MEM_CUMSENSOILN = CUMSENSOILN
          MEM_FEXIST = FEXIST
          MEM_FIRST = FIRST
      END SELECT
      RETURN
      END SUBROUTINE GETPUT_MZ_OPNIT


      SUBROUTINE GETPUT_MZ_OPGROW(CODE, OUTG, NOUTDG, DAP, DAS, 
     +      PODWTD, CUMSENSURF, CUMSENSOIL, FEXIST, FIRST)
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE
      CHARACTER*12 OUTG
      INTEGER NOUTDG, DAP, DAS
      REAL PODWTD, CUMSENSURF, CUMSENSOIL
      LOGICAL         FEXIST, FIRST

      CHARACTER*12 MEM_OUTG
      INTEGER MEM_NOUTDG, MEM_TIMDIF, MEM_DAP, MEM_DAS
      REAL MEM_PODWTD, MEM_CUMSENSURF, MEM_CUMSENSOIL
      LOGICAL         MEM_FEXIST, MEM_FIRST

      SELECT CASE(CODE)
        CASE('GET')
            OUTG = MEM_OUTG
            NOUTDG = MEM_NOUTDG
            DAP = MEM_DAP
            DAS = MEM_DAS
            PODWTD = MEM_PODWTD
            CUMSENSURF = MEM_CUMSENSURF
            CUMSENSOIL = MEM_CUMSENSOIL
            FEXIST = MEM_FEXIST
            FIRST = MEM_FIRST

        CASE('PUT')
            MEM_OUTG = OUTG
            MEM_NOUTDG = NOUTDG
            MEM_DAP = DAP
            MEM_DAS = DAS
            MEM_PODWTD = PODWTD
            MEM_CUMSENSURF = CUMSENSURF
            MEM_CUMSENSOIL = CUMSENSOIL
            MEM_FEXIST = FEXIST
            MEM_FIRST = FIRST
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_OPGROW


C       SUBROUTINE GETPUT_MZ_OPHARV(CODE)
C       IMPLICIT NONE
C       SAVE
C       CHARACTER(len=3) CODE

C       CHARACTER*6  SECTION
C       CHARACTER*10 STNAME(20)
C       CHARACTER*12 FILEA

C       INTEGER DFLR, DMAT
C       INTEGER DNR1, DNR7, FOUND
C       INTEGER IFLR, IMAT
C       INTEGER ISENS, LINC, LNUM, LUNIO
C       INTEGER TRTNO, YRNR1, YRNR2, YRNR3
C       INTEGER YRNR5, YRSIM, YRPLT
      
C       REAL BWAH, BWAM
C       REAL HI
C       REAL PBIOMS, PSDWT
C       REAL SDRATE
C       REAL SDWTAH, SDWTAM
C       REAL WTNCAN, WTNUP, XGNP, XLAI, XN
C       REAL YIELD,RNAD,RWAD 

C       INTEGER ACOUNT
C       CHARACTER*4 OLAB(40), OLAP(40)  !OLAP modified for dap
C       CHARACTER*6 X(40)
C       CHARACTER*8 Simulated(40), Measured(40)
C       CHARACTER*35 DESCRIP(40)

C       SELECT CASE(CODE)
C         CASE('GET')

C         CASE('PUT')

C       END SELECT

C       RETURN
C       END SUBROUTINE GETPUT_MZ_OPHARV


      SUBROUTINE GETPUT_MZ_NUPTAKE(CODE, ANDEM, DNG, DROOTN, DSTOVN, FAC
     +  , FACTOR, FNH4, FNO3, NDEM, NUF, RFAC, RNDEM, RNH4U, RNLOSS, 
     +  RNO3U, SMDFR, SNH4, SNO3, TNDEM, TRLV, TRNLOS, XMIN, XNDEM)
      IMPLICIT NONE
      SAVE
      CHARACTER(len=3) CODE

      REAL        ANDEM , MEM_ANDEM  
      REAL        DNG , MEM_DNG         
      REAL        DROOTN , MEM_DROOTN      
      REAL        DSTOVN , MEM_DSTOVN      
      REAL        FAC(20), MEM_FAC(20)     
      REAL        FACTOR , MEM_FACTOR      
      REAL        FNH4 , MEM_FNH4        
      REAL        FNO3 , MEM_FNO3        
      REAL        NDEM , MEM_NDEM        
      REAL        NUF , MEM_NUF         
      REAL        RFAC , MEM_RFAC        
      REAL        RNDEM , MEM_RNDEM       
      REAL        RNH4U(20), MEM_RNH4U(20)   
      REAL        RNLOSS , MEM_RNLOSS      
      REAL        RNO3U(20), MEM_RNO3U(20)   
      REAL        SMDFR , MEM_SMDFR       
      REAL        SNH4(20), MEM_SNH4(20)    
      REAL        SNO3(20), MEM_SNO3(20)    
      REAL        TNDEM , MEM_TNDEM       
      REAL        TRLV , MEM_TRLV        
      REAL        TRNLOS , MEM_TRNLOS      
      REAL        XMIN , MEM_XMIN        
      REAL        XNDEM , MEM_XNDEM       


      SELECT CASE(CODE)
        CASE('GET')
            ANDEM = MEM_ANDEM
            DNG = MEM_DNG
            DROOTN = MEM_DROOTN
            DSTOVN = MEM_DSTOVN
            FAC = MEM_FAC
            FACTOR = MEM_FACTOR
            FNH4 = MEM_FNH4
            FNO3 = MEM_FNO3
            NDEM = MEM_NDEM
            NUF = MEM_NUF
            RFAC = MEM_RFAC
            RNDEM = MEM_RNDEM
            RNH4U = MEM_RNH4U
            RNLOSS = MEM_RNLOSS
            RNO3U = MEM_RNO3U
            SMDFR = MEM_SMDFR
            SNH4 = MEM_SNH4
            SNO3 = MEM_SNO3
            TNDEM = MEM_TNDEM
            TRLV = MEM_TRLV
            TRNLOS = MEM_TRNLOS
            XMIN = MEM_XMIN
            XNDEM = MEM_XNDEM

        CASE('PUT')
            MEM_ANDEM = ANDEM
            MEM_DNG = DNG
            MEM_DROOTN = DROOTN
            MEM_DSTOVN = DSTOVN
            MEM_FAC = FAC
            MEM_FACTOR = FACTOR
            MEM_FNH4 = FNH4
            MEM_FNO3 = FNO3
            MEM_NDEM = NDEM
            MEM_NUF = NUF
            MEM_RFAC = RFAC
            MEM_RNDEM = RNDEM
            MEM_RNH4U = RNH4U
            MEM_RNLOSS = RNLOSS
            MEM_RNO3U = RNO3U
            MEM_SMDFR = SMDFR
            MEM_SNH4 = SNH4
            MEM_SNO3 = SNO3
            MEM_TNDEM = TNDEM
            MEM_TRLV = TRLV
            MEM_TRNLOS = TRNLOS
            MEM_XMIN = XMIN
            MEM_XNDEM = XNDEM
      END SELECT

      RETURN
      END SUBROUTINE GETPUT_MZ_NUPTAKE




      SUBROUTINE MAIZE_GROWTH_STATE(CODE, IFILE)
      ! subroutine to save/retrieve all parameters of dssat maize model 
      ! from disk, used only by RZinterrupt.for
      ! placed in this location for modularity
      IMPLICIT NONE
      CHARACTER(len=3) CODE
      INTEGER IFILE

      ! MZ_ROOTGR
      INTEGER     L1       
      REAL        RLDF(20), RLNEW, RLV(20), RNFAC, RNLF, RTDEP, RTEXF, 
     + RTSURV, SWDF, SWEXF, TRLDF      

      ! MZ_OPNIT
      INTEGER DAS, NOUTDN, DAP,L, NLAYR, TIMDIF
      CHARACTER*12 OUTPN
      REAL PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, WTNLF, 
     + WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN
      LOGICAL FEXIST, FIRST

      CHARACTER*12 OUTG
      INTEGER NOUTDG
      REAL PODWTD, CUMSENSURF, CUMSENSOIL

      REAL        ANDEM , MEM_ANDEM  
      REAL        DNG , MEM_DNG         
      REAL        DROOTN , MEM_DROOTN      
      REAL        DSTOVN , MEM_DSTOVN      
      REAL        FAC(20), MEM_FAC(20)     
      REAL        FACTOR , MEM_FACTOR      
      REAL        FNH4 , MEM_FNH4        
      REAL        FNO3 , MEM_FNO3        
      REAL        NDEM , MEM_NDEM        
      REAL        NUF , MEM_NUF         
      REAL        RFAC , MEM_RFAC        
      REAL        RNDEM , MEM_RNDEM       
      REAL        RNH4U(20), MEM_RNH4U(20)   
      REAL        RNLOSS , MEM_RNLOSS      
      REAL        RNO3U(20), MEM_RNO3U(20)   
      REAL        SMDFR , MEM_SMDFR       
      REAL        SNH4(20), MEM_SNH4(20)    
      REAL        SNO3(20), MEM_SNO3(20)    
      REAL        TNDEM , MEM_TNDEM       
      REAL        TRLV , MEM_TRLV        
      REAL        TRNLOS , MEM_TRNLOS      
      REAL        XMIN , MEM_XMIN        
      REAL        XNDEM , MEM_XNDEM       

      SELECT CASE(CODE)
        CASE('GET')

        READ(IFILE,*) L1, RLDF, RLNEW, RLV, RNFAC, 
     +      RNLF, RTDEP, RTEXF, RTSURV, SWDF, SWEXF, TRLDF
        CALL GETPUT_MZ_ROOTGR('PUT', L1, RLDF, RLNEW, RLV, RNFAC, 
     +      RNLF, RTDEP, RTEXF, RTSURV, SWDF, SWEXF, TRLDF)

        READ(IFILE,*) DAS, NOUTDN, DAP,L, NLAYR
     + , OUTPN, PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, 
     + WTNLF, WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, FEXIST, 
     + FIRST
      CALL GETPUT_MZ_OPNIT('PUT', DAS, NOUTDN, DAP,L, NLAYR
     + , OUTPN, PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, 
     + WTNLF, WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, FEXIST, 
     + FIRST)

      READ(IFILE,*) OUTG, NOUTDG, DAP, DAS, 
     +      PODWTD, CUMSENSURF, CUMSENSOIL, FEXIST, FIRST
      CALL GETPUT_MZ_OPGROW('PUT', OUTG, NOUTDG, DAP, DAS, 
     +      PODWTD, CUMSENSURF, CUMSENSOIL, FEXIST, FIRST)

      READ(IFILE,*) ANDEM, DNG, DROOTN, DSTOVN, FAC
     +  , FACTOR, FNH4, FNO3, NDEM, NUF, RFAC, RNDEM, RNH4U, RNLOSS, 
     +  RNO3U, SMDFR, SNH4, SNO3, TNDEM, TRLV, TRNLOS, XMIN, XNDEM
      CALL GETPUT_MZ_NUPTAKE('PUT', ANDEM, DNG, DROOTN, DSTOVN, FAC
     +  , FACTOR, FNH4, FNO3, NDEM, NUF, RFAC, RNDEM, RNH4U, RNLOSS, 
     +  RNO3U, SMDFR, SNH4, SNO3, TNDEM, TRLV, TRNLOS, XMIN, XNDEM)

       CASE('PUT')
        CALL GETPUT_MZ_ROOTGR('GET', L1, RLDF, RLNEW, RLV, RNFAC, 
     +      RNLF, RTDEP, RTEXF, RTSURV, SWDF, SWEXF, TRLDF)
        WRITE(IFILE,*) L1, RLDF, RLNEW, RLV, RNFAC, 
     +      RNLF, RTDEP, RTEXF, RTSURV, SWDF, SWEXF, TRLDF

      CALL GETPUT_MZ_OPNIT('GET', DAS, NOUTDN, DAP,L, NLAYR
     + , OUTPN, PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, 
     + WTNLF, WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, FEXIST, 
     + FIRST)
        WRITE(IFILE,*) DAS, NOUTDN, DAP,L, NLAYR
     + , OUTPN, PCNL, WTNCAN, WTNSD, WTNVEG, PCNGRN, PCNVEG, WTNUP, 
     + WTNLF, WTNST, PCNST, PCNRT, CUMSENSURFN, CUMSENSOILN, FEXIST, 
     + FIRST

      CALL GETPUT_MZ_OPGROW('GET', OUTG, NOUTDG, DAP, DAS, 
     +      PODWTD, CUMSENSURF, CUMSENSOIL, FEXIST, FIRST)
      WRITE(IFILE,*) CODE, OUTG, NOUTDG, DAP, DAS, 
     +      PODWTD, CUMSENSURF, CUMSENSOIL, FEXIST, FIRST

      CALL GETPUT_MZ_NUPTAKE('GET', ANDEM, DNG, DROOTN, DSTOVN, FAC
     +  , FACTOR, FNH4, FNO3, NDEM, NUF, RFAC, RNDEM, RNH4U, RNLOSS, 
     +  RNO3U, SMDFR, SNH4, SNO3, TNDEM, TRLV, TRNLOS, XMIN, XNDEM)

      WRITE(IFILE,*) ANDEM, DNG, DROOTN, DSTOVN, FAC
     +  , FACTOR, FNH4, FNO3, NDEM, NUF, RFAC, RNDEM, RNH4U, RNLOSS, 
     +  RNO3U, SMDFR, SNH4, SNO3, TNDEM, TRLV, TRNLOS, XMIN, XNDEM
      END SELECT  

      RETURN
      END SUBROUTINE MAIZE_GROWTH_STATE