C       MODULE MemoryDefs
      ! Many subroutines in the project use static variables using 
      ! SAVE option. This module modifies it to help resume execution 
      ! from any arbitrary state. 
      ! All storage elements have `mem_` prefix
      ! Programmer: Balaji S. Pokuri
      ! 
      SUBROUTINE MAQUE_Memory(CODE, ADIW, JSPLT, SPLT1, FIRST7, NYR, 
     +  IRPL, NYRC, NYRP, ADIWMONTH, totadiw, curradiw)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MAXHOR=12,MXAPP=200,MXCHEM=15,MXSPEC=10,
     +          MXTG=500,MXPEST=3)
      CHARACTER (len=3)  CODE
      INTEGER IRPL, NYR, NYRC, MEM_IRPL, MEM_NYR, MEM_NYRC
      DIMENSION ADIW(MXAPP),MEM_ADIW(MXAPP),NYRP(MXPEST)
     + ,MEM_NYRP(MXPEST),ADIWMONTH(12,200),MEM_ADIWMONTH(12,200),
     + totadiw(20),MEM_totadiw(20)
      LOGICAL SPLT1, MEM_SPLT1, FIRST7, MEM_FIRST7
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          ADIW = MEM_ADIW
          JSPLT = MEM_JSPLT
          SPLT1 = MEM_SPLT1
          FIRST7 = MEM_FIRST7
          NYR = MEM_NYR
          IRPL = MEM_IRPL
          NYRC = MEM_NYRC
          NYRP = MEM_NYRP
          ADIWMONTH =  MEM_ADIWMONTH
          totadiw = MEM_TOTADIW
          curradiw = MEM_curradiw
        CASE('PUT')
          MEM_ADIW = ADIW
          MEM_JSPLT = JSPLT
          MEM_SPLT1 = SPLT1
          MEM_FIRST7 = FIRST7
          MEM_NYR = NYR
          MEM_IRPL = IRPL
          MEM_NYRC = NYRC
          MEM_NYRP = NYRP
          MEM_ADIWMONTH =  ADIWMONTH
          MEM_totadiw = TOTADIW
          MEM_curradiw = curradiw
      END SELECT
      
      RETURN
      END SUBROUTINE MAQUE_Memory
      
C       END MODULE

      SUBROUTINE MAPLNT_Memory(CODE,nreps,FIRST11,first15,PCN,TTPLNT,
     +  TMPLNT,TXPLNT,IJ)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(MXNOD=300,MAXHOR=12,MXAPP=200,MXCHEM=15,MXSPEC=10,
     +          MXTG=500,MXPEST=3)
      CHARACTER (len=3)  CODE
      INTEGER IJ, MEM_IJ
      DOUBLE PRECISION PCN(0:MXNOD), MEM_PCN(0:MXNOD)
      LOGICAL FIRST11,first15, MEM_FIRST11, MEM_FIRST15
      DATA MEM_FIRST11/.TRUE./,MEM_FIRST15/.TRUE./,mem_ij/0/
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          nreps = MEM_nreps
          FIRST11 = MEM_FIRST11
          FIRST15 = MEM_FIRST15
          PCN = MEM_PCN
          TTPLNT = MEM_TTPLNT
          TMPLNT = MEM_TMPLNT
          TXPLNT = MEM_TXPLNT
          IJ = MEM_IJ
        CASE('PUT')
          MEM_nreps = nreps
          MEM_FIRST11 = FIRST11
          MEM_FIRST15 = FIRST15
          MEM_PCN = PCN
          MEM_TTPLNT = TTPLNT
          MEM_TMPLNT = TMPLNT
          MEM_TXPLNT = TXPLNT
          MEM_IJ = IJ
      END SELECT
      
      RETURN
      END SUBROUTINE MAPLNT_Memory


      SUBROUTINE DSSATDRV_Memory(CODE,oldhgt,PLHGHT,PLALFA,ISTAGE)
      DOUBLE PRECISION oldhgt,PLHGHT,PLALFA,ISTAGE
      CHARACTER (len=3)  CODE
      SAVE

      SELECT CASE(CODE)
        CASE('GET')
          oldhgt = MEM_oldhgt
          PLHGHT = MEM_PLHGHT
          PLALFA = MEM_PLALFA
          ISTAGE = MEM_ISTAGE
        CASE('PUT')
          MEM_oldhgt = oldhgt
          MEM_PLHGHT = PLHGHT
          MEM_PLALFA = PLALFA
          MEM_ISTAGE = ISTAGE
      END SELECT
      
      RETURN
      END SUBROUTINE DSSATDRV_Memory


      SUBROUTINE DAYJACK_Memory(CODE,LID,LIM,LIYYY,JSEQDAY,CURSTATE)
C       IMPLICIT NONE
      PARAMETER (NUMNDK=5)
      INTEGER LID,LIM,LIYYY,JSEQDAY
      LOGICAL CURSTATE(NUMNDK)
      INTEGER MEM_LID,MEM_LIM,MEM_LIYYY,MEM_JSEQDAY
      LOGICAL MEM_CURSTATE(NUMNDK)
      DATA MEM_JSEQDAY/0/,MEM_LID/0/,MEM_LIM/0/,MEM_LIYYY/0/
     + ,MEM_CURSTATE/NUMNDK*.FALSE./
      CHARACTER (len=3)  CODE
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          LID = MEM_LID
          LIM = MEM_LIM
          LIYYY = MEM_LIYYY
          JSEQDAY = MEM_JSEQDAY
C           CURSTATE = MEM_CURSTATE
        CASE('PUT')
          MEM_LID = LID
          MEM_LIM = LIM
          MEM_LIYYY = LIYYY
          MEM_JSEQDAY = JSEQDAY
C           MEM_CURSTATE = CURSTATE
      END SELECT
      
      RETURN
      END SUBROUTINE DAYJACK_Memory


      SUBROUTINE OPWBAL_Memory(CODE, IDETW, ISWWAT, RNMODE, DAS, DOY, 
     +  DYNAMIC, ERRNUM, FROP, NAVWB, NAP, NOUTDW, RUN, YEAR, 
     +  YRDOY, REPNO, AVWTD, PESW, TLL, TOTBUNDRO, TOTIR, TSW)
      IMPLICIT NONE
      SAVE

      CHARACTER (len=3)  CODE
      CHARACTER*1 IDETW, ISWWAT, RNMODE,MEM_IDETW,MEM_ISWWAT,MEM_RNMODE
      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, NAVWB, NAP, NOUTDW,
     + RUN, YEAR, YRDOY, REPNO, MEM_DAS, MEM_DOY, MEM_DYNAMIC, 
     + MEM_ERRNUM, MEM_FROP, MEM_NAVWB, MEM_NAP, MEM_NOUTDW, MEM_RUN, 
     + MEM_YEAR, MEM_YRDOY, MEM_REPNO

      REAL AVWTD, PESW, TLL, TOTBUNDRO, TOTIR, TSW, 
     + MEM_AVWTD, MEM_PESW, MEM_TLL, MEM_TOTBUNDRO, MEM_TOTIR, MEM_TSW

      SELECT CASE(CODE)
        CASE('GET')
          IDETW = MEM_IDETW
          ISWWAT = MEM_ISWWAT
          RNMODE = MEM_RNMODE
          DAS = MEM_DAS
          DOY = MEM_DOY
          DYNAMIC = MEM_DYNAMIC
          ERRNUM = MEM_ERRNUM
          FROP = MEM_FROP
          NAVWB = MEM_NAVWB
          NAP = MEM_NAP
          NOUTDW = MEM_NOUTDW
          RUN = MEM_RUN
          YEAR = MEM_YEAR
          YRDOY = MEM_YRDOY
          REPNO = MEM_REPNO
          AVWTD = MEM_AVWTD
          PESW = MEM_PESW
          TLL = MEM_TLL
          TOTBUNDRO = MEM_TOTBUNDRO
          TOTIR = MEM_TOTIR
          TSW = MEM_TSW
        CASE('PUT')
          MEM_IDETW = IDETW
          MEM_ISWWAT = ISWWAT
          MEM_RNMODE = RNMODE
          MEM_DAS = DAS
          MEM_DOY = DOY
          MEM_DYNAMIC = DYNAMIC
          MEM_ERRNUM = ERRNUM
          MEM_FROP = FROP
          MEM_NAVWB = NAVWB
          MEM_NAP = NAP
          MEM_NOUTDW = NOUTDW
          MEM_RUN = RUN
          MEM_YEAR = YEAR
          MEM_YRDOY = YRDOY
          MEM_REPNO = REPNO
          MEM_AVWTD = AVWTD
          MEM_PESW = PESW
          MEM_TLL = TLL
          MEM_TOTBUNDRO = TOTBUNDRO
          MEM_TOTIR = TOTIR
          MEM_TSW = TSW
      END SELECT
      
      RETURN
      END SUBROUTINE OPWBAL_Memory


      SUBROUTINE Alt_Plant_Memory(CODE, NVALP0, YREMRG, MDATE, STGDOY, 
     +  CANHT, EORATIO, KCAN, KEP, KSEVAP, KTRANS, NSTRES, PORMIN, 
     +  RWUEP1, RWUMX, XLAI, XHLAI, RLV, UNO3, UNH4, LFWT, STMWT, 
     +  XSTAGE, DTT, BIOMAS, SDWT, GRNWT, EARS, RTWT, STOVWT, STOVN, 
     +  ROOTN, GRAINN, RTDEP, NFIXN, TOPWT, WTLF, PCNL, PCNST, PCNRT, 
     +  PCNSD, trnu, swfac, grwt, cwad, tfg, wfg, cnad, stwt, nupd, 
     +  gnad, rtwts, istage, FixCanht, HARVRES, SENESCE)
      USE ModuleDefs
      USE FloodModule

      IMPLICIT NONE
      SAVE

      CHARACTER (len=3)  CODE
      
      INTEGER NVALP0
      INTEGER YREMRG, MDATE
      integer istage
      INTEGER STGDOY(20)

      REAL CANHT, EORATIO
      REAL KCAN, KEP, KSEVAP, KTRANS, NSTRES
      REAL PORMIN, RWUEP1, RWUMX
      REAL XLAI, XHLAI

      REAL LFWT,STMWT,XSTAGE,DTT,BIOMAS,SDWT,GRNWT,EARS
      REAL RTWT,STOVWT,STOVN,ROOTN,GRAINN,RTDEP
      REAL NFIXN,TOPWT,WTLF,PCNL,PCNST,PCNRT,PCNSD
      real trnu,swfac,grwt,cwad,tfg,wfg,cnad,stwt,nupd,gnad,rtwts
      REAL, DIMENSION(20) :: RLV, UNO3, UNH4
      LOGICAL FixCanht

      TYPE (ResidueType)  HARVRES, SENESCE

      INTEGER MEM_NVALP0
      INTEGER MEM_YREMRG, MEM_MDATE
      INTEGER MEM_STGDOY(20)

      REAL MEM_CANHT, MEM_EORATIO
      REAL MEM_KCAN, MEM_KEP, MEM_KSEVAP, MEM_KTRANS, MEM_NSTRES
      REAL MEM_PORMIN, MEM_RWUEP1, MEM_RWUMX
      REAL MEM_XLAI, MEM_XHLAI

      REAL, DIMENSION(20) :: MEM_RLV, MEM_UNO3, MEM_UNH4
      REAL MEM_LFWT,MEM_STMWT,MEM_XSTAGE,MEM_DTT,MEM_BIOMAS,MEM_SDWT,
     + MEM_GRNWT,MEM_EARS
      REAL MEM_RTWT,MEM_STOVWT,MEM_STOVN,MEM_ROOTN,MEM_GRAINN,MEM_RTDEP
      REAL MEM_NFIXN,MEM_TOPWT,MEM_WTLF,MEM_PCNL,MEM_PCNST,MEM_PCNRT,
     + MEM_PCNSD
      real MEM_trnu,MEM_swfac,MEM_grwt,MEM_cwad,MEM_tfg,MEM_wfg,
     + MEM_cnad,MEM_stwt,MEM_nupd,MEM_gnad,MEM_rtwts
      integer MEM_istage
      LOGICAL MEM_FixCanht

      TYPE (ResidueType) MEM_HARVRES, MEM_SENESCE

      SELECT CASE(CODE)
        CASE('GET')
            NVALP0 = MEM_NVALP0
            YREMRG = MEM_YREMRG
            MDATE = MEM_MDATE
            STGDOY = MEM_STGDOY
            CANHT = MEM_CANHT
            EORATIO = MEM_EORATIO
            KCAN = MEM_KCAN
            KEP = MEM_KEP
            KSEVAP = MEM_KSEVAP
            KTRANS = MEM_KTRANS
            NSTRES = MEM_NSTRES
            PORMIN = MEM_PORMIN
            RWUEP1 = MEM_RWUEP1
            RWUMX = MEM_RWUMX
            XLAI = MEM_XLAI
            XHLAI = MEM_XHLAI
            RLV = MEM_RLV
            UNO3 = MEM_UNO3
            UNH4 = MEM_UNH4
            LFWT = MEM_LFWT
            STMWT = MEM_STMWT
            XSTAGE = MEM_XSTAGE
            DTT = MEM_DTT
            BIOMAS = MEM_BIOMAS
            SDWT = MEM_SDWT
            GRNWT = MEM_GRNWT
            EARS = MEM_EARS
            RTWT = MEM_RTWT
            STOVWT = MEM_STOVWT
            STOVN = MEM_STOVN
            ROOTN = MEM_ROOTN
            GRAINN = MEM_GRAINN
            RTDEP = MEM_RTDEP
            NFIXN = MEM_NFIXN
            TOPWT = MEM_TOPWT
            WTLF = MEM_WTLF
            PCNL = MEM_PCNL
            PCNST = MEM_PCNST
            PCNRT = MEM_PCNRT
            PCNSD = MEM_PCNSD
            trnu = MEM_trnu
            swfac = MEM_swfac
            grwt = MEM_grwt
            cwad = MEM_cwad
            tfg = MEM_tfg
            wfg = MEM_wfg
            cnad = MEM_cnad
            stwt = MEM_stwt
            nupd = MEM_nupd
            gnad = MEM_gnad
            rtwts = MEM_rtwts
            istage = MEM_istage
            FixCanht = MEM_FixCanht
            HARVRES = MEM_HARVRES
            SENESCE = MEM_SENESCE
        CASE('PUT')
            MEM_NVALP0 = NVALP0
            MEM_YREMRG = YREMRG
            MEM_MDATE = MDATE
            MEM_STGDOY = STGDOY
            MEM_CANHT = CANHT
            MEM_EORATIO = EORATIO
            MEM_KCAN = KCAN
            MEM_KEP = KEP
            MEM_KSEVAP = KSEVAP
            MEM_KTRANS = KTRANS
            MEM_NSTRES = NSTRES
            MEM_PORMIN = PORMIN
            MEM_RWUEP1 = RWUEP1
            MEM_RWUMX = RWUMX
            MEM_XLAI = XLAI
            MEM_XHLAI = XHLAI
            MEM_RLV = RLV
            MEM_UNO3 = UNO3
            MEM_UNH4 = UNH4
            MEM_LFWT = LFWT
            MEM_STMWT = STMWT
            MEM_XSTAGE = XSTAGE
            MEM_DTT = DTT
            MEM_BIOMAS = BIOMAS
            MEM_SDWT = SDWT
            MEM_GRNWT = GRNWT
            MEM_EARS = EARS
            MEM_RTWT = RTWT
            MEM_STOVWT = STOVWT
            MEM_STOVN = STOVN
            MEM_ROOTN = ROOTN
            MEM_GRAINN = GRAINN
            MEM_RTDEP = RTDEP
            MEM_NFIXN = NFIXN
            MEM_TOPWT = TOPWT
            MEM_WTLF = WTLF
            MEM_PCNL = PCNL
            MEM_PCNST = PCNST
            MEM_PCNRT = PCNRT
            MEM_PCNSD = PCNSD
            MEM_trnu = trnu
            MEM_swfac = swfac
            MEM_grwt = grwt
            MEM_cwad = cwad
            MEM_tfg = tfg
            MEM_wfg = wfg
            MEM_cnad = cnad
            MEM_stwt = stwt
            MEM_nupd = nupd
            MEM_gnad = gnad
            MEM_rtwts = rtwts
            MEM_istage = istage
            MEM_FixCanht = FixCanht
            MEM_HARVRES = HARVRES
            MEM_SENESCE = SENESCE
      END SELECT
      
      RETURN
      END SUBROUTINE Alt_Plant_Memory


      SUBROUTINE CROPGRO_Memory(CODE, DETACH, ECONO, 
     &  NDLEAF, NDSET, NOUTDO, NR1, NR2, NR5, NR7, YREMRG, YRNR1, YRNR2,
     &  YRNR3, YRNR5, YRNR7, AGEFAC, AGRSD3, AREALF, ASMDOT, AGRLF, 
     &  AGRRT, AGRSH2, AGRSTM, AGRSD1, AGRSD2, AGRVG, AGRVG2, AGRNOD, 
     &  AGRSH1, BETN, CANNAA, CANWAA, CDMREP, CGRSH, CGRSD, CNODMN, 
     &  CO2, CSAVEV, CNDFX, CTONODR, CTONOD, CAVVEG, CADPR1, CMOBMX, 
     &  CMINEP, CLW, CSW, CNOD, CRUSLF, CRUSRT, CRUSST, CRUSSH, CADLF, 
     &  CADST, CANWH, CMINEA, DAYL, DWNOD, DWNODA, DISLA, DRPP, DTX, 
     &  DXR57, EP1, EXCESS, FNINSH, FRACDN, FRCNOD, F, FNINL, FNINR, 
     &  FNINS, FNINSD, FRLF, FRRT, FRSTM, FREEZ1, FREEZ2, GDMSD, 
     &  GRRAT1, GROWTH, GRWRES, LAIMX, LAGSD, LNGPEG, MAINR, NPLTD, 
     &  NDMNEW, NADRT, NADST, NGRLF, NGRRT, NGRST, NADLF, NMINEA, 
     &  NDMOLD, NDMREP, NDMSDR, NDMTOT, NDMVEG, NMINEP, NMOBR, NDTH, 
     &  NAVL, NRUSSH, NGRSD, NGRSH, NRUSLF, NRUSRT, NRUSST, POTLIP, 
     &  POTCAR, PPLTD, PAR, PCTMAT, PLIGLF, PLIGNO, PLIGRT, PLIGST, 
     &  PODWTD, PG, PCLSD, PCCSD, PCNSH, PODWT, PODNO, PGAVL, PLTPOP, 
     &  PCARSH, PCH2O, PLIPSH, PLIGSD, PLIGSH, PMINSD, PMINSH, POASD, 
     &  POASH, PROLFI, PRORTI, PROSHI, PROSTI, R30C2, RCH2O, RES30C, 
     &  RPROAV, RFIXN, RLIG, RLIP, RMIN, RNITP, RNH4C, RNO3C, ROA, RPRO, 
     &  RO, RP, RHOS, ROWSPC, RVSTGE, RSPNO3, RSPNH4, SEEDNI, 
     &  NAVLV, PROVEG, SDNPL, SENNOD, SENRT, SATFAC, SDWTAH, SDRATE, 
     &  SDIDOT, SDVAR, SHVAR, SDGR, SDPROR, SHELWT, SLA, SLDOT, SWIDOT, 
     &  SEEDNO, SLAAD, SLNDOT, SSDOT, SSNDOT, TDAY, TDUMX, TDUMX2, 
     &  TGROAV, TMIN, TAVG, TURADD, TRNH4U, TRNO3U, TNLEAK, TTFIX, 
     &  TOTWT, VSTAGE, WLFDOT, WSIDOT, WRIDOT, WTNCAN, WTNLA, WTNLO, 
     &  WTNNA, WTNNAG, WTNNO, WTNNOD, WTNRA, WTNRO, WTNSA, WTNSDA, 
     &  WTNSDO, WTNSHA, WTNSHO, WTNSO, WTNUP, WTNOO, WTLO, WTSO, WTRO, 
     &  WSDDTN, WSHDTN, WTABRT, WTSHMT, WTCO, WTSHO, WTSDO, WCRLF, 
     &  WCRRT, WCRSH, WLDOTN, WRDOTN, WSDOTN, WCRST, WNRLF, WNRRT, 
     &  WNRSH, WNRST, WTNRT, WTNSD, WTNSH, WTNST, WTNEW, WTMAIN, WTNLF, 
     &  WSHIDT, WLIDOT, XPOD, XFRT, XLAI, PHTHRS, TGRO, SDDES, WTSD, 
     &  WTSHE, SDNO, SHELN, PHTIM, PNTIM, FLWN, PUNCSD, PUNCTR, 
     &  RZrwu, RZtrwup, AVG_HROOT, WTDEP, qsr, TRWUP1, ISTRESS, KCAN, 
     &  KEP, RWUEP1)
      CHARACTER(len=3) CODE

      INTEGER, PARAMETER :: 
     &    NL       = 20,  !Maximum number of soil layers 
     &    TS       = 24,  !Number of hourly time steps per day
     &    NAPPL    = 250, !Maximum number of applications or operations
     &    NCOHORTS = 300, !Maximum number of cohorts
     &    NELEM    = 1,   !Number of elements modeled (currently only N)

         !Dynamic variable values
     &    RUNINIT  = 1, 
     &    INIT     = 2,  !Will take the place of RUNINIT & SEASINIT
                         !     (not fully implemented)
     &    SEASINIT = 2, 
     &    RATE     = 3,
     &    EMERG    = 3,  !Used for some plant processes.  
     &    INTEGR   = 4,  
     &    OUTPUT   = 5,  
     &    FINAL    = 6

      CHARACTER*1 DETACH
      CHARACTER*6 ECONO
      CHARACTER*255 FILECC, FILEGC

      INTEGER NDLEAF, NDSET, NOUTDO,
     &    NR1, NR2, NR5, NR7
      INTEGER YREMRG, YRNR1, YRNR2,
     &    YRNR3, YRNR5, YRNR7

      REAL AGEFAC, AGRSD3, AREALF, ASMDOT
      REAL AGRLF, AGRRT, AGRSH2, AGRSTM
      REAL AGRSD1, AGRSD2, AGRVG, AGRVG2
      REAL AGRNOD, AGRSH1
      REAL BETN
      REAL CANNAA, CANWAA, CDMREP, CGRSH, CGRSD, CNODMN, CO2, CSAVEV
      REAL CNDFX, CTONODR, CTONOD, CAVVEG
      REAL CADPR1, CMOBMX, CMINEP
      REAL CLW, CSW
      REAL CNOD, CRUSLF, CRUSRT, CRUSST,
     &    CRUSSH, CADLF, CADST, CANWH, CMINEA
      REAL DAYL, DWNOD
      REAL DWNODA, DISLA, DRPP, DTX, DXR57
      REAL EP1, EXCESS
      REAL FNINSH, FRACDN, FRCNOD,
     &    F, FNINL, FNINR, FNINS, FNINSD,
     &    FRLF, FRRT, FRSTM
      REAL FREEZ1, FREEZ2
      REAL GDMSD, GRRAT1, GROWTH, GRWRES
      REAL LAIMX, LAGSD, LNGPEG
      REAL MAINR
      REAL NPLTD, NDMNEW,
     &    NADRT,  NADST,  NGRLF,  NGRRT,  NGRST,
     &    NADLF, NMINEA,
     &    NDMOLD, NDMREP, NDMSDR, NDMTOT, NDMVEG,
     &    NMINEP, NMOBR, NDTH
      REAL NAVL, NRUSSH, NGRSD, NGRSH
      REAL NRUSLF, NRUSRT, NRUSST
      REAL POTLIP, POTCAR, PPLTD,
     &    PAR, PCTMAT
      REAL PLIGLF, PLIGNO, PLIGRT, PLIGST
      REAL PODWTD, PG, PCLSD, PCCSD, PCNSH, PODWT
      REAL PODNO
      REAL PGAVL, PLTPOP, PCARSH, PCH2O, PLIPSH,
     &    PLIGSD, PLIGSH, PMINSD, PMINSH, POASD, POASH,
     &    PROLFI, PRORTI, PROSHI, PROSTI
      REAL R30C2, RCH2O, RES30C, RPROAV,
     &    RFIXN, RLIG, RLIP, RMIN, RNITP,
     &    RNH4C, RNO3C, ROA, RPRO
      REAL RO, RP, RHOS, ROWSPC
      REAL RVSTGE, RSPNO3, RSPNH4
      REAL SEEDNI, NAVLV, PROVEG, SDNPL

!     Senescence variables computed for soil N routines.
      REAL SENNOD(NL), SENRT(NL)

      REAL SATFAC, SDWTAH,
     &    SDRATE, SDIDOT,
     &    SDVAR, SHVAR, SDGR, SDPROR, SHELWT,
     &    SLA, SLDOT, SWIDOT, SEEDNO
      REAL SLAAD, SLNDOT, SSDOT, SSNDOT
      REAL TDAY, TDUMX, TDUMX2, TGROAV, TMIN, TAVG, TURADD,
     &    TRNH4U, TRNO3U, TNLEAK, TTFIX, TOTWT
      REAL VSTAGE
      REAL WLFDOT, WSIDOT, WRIDOT
      REAL WTNCAN, WTNLA, WTNLO, WTNNA, WTNNAG
      REAL WTNNO, WTNNOD, WTNRA, WTNRO, WTNSA, WTNSDA
      REAL WTNSDO, WTNSHA, WTNSHO, WTNSO, WTNUP, WTNOO
      REAL WTLO, WTSO, WTRO, WSDDTN, WSHDTN, WTABRT, WTSHMT
      REAL WTCO, WTSHO, WTSDO, WCRLF, WCRRT,
     &    WCRSH, WLDOTN, WRDOTN,  WSDOTN,
     &    WCRST, WNRLF, WNRRT, WNRSH, WNRST,
     &    WTNRT, WTNSD, WTNSH, WTNST, WTNEW, WTMAIN, WTNLF,
     &    WSHIDT, WLIDOT
      REAL XPOD, XFRT, XLAI

      REAL PHTHRS(20)
      REAL TGRO(TS)
      REAL SDDES(NCOHORTS)
      REAL WTSD(NCOHORTS), WTSHE(NCOHORTS), SDNO(NCOHORTS) 
      REAL SHELN(NCOHORTS)
      REAL PHTIM(365)
      REAL PNTIM(365)

      REAL FLWN(NCOHORTS)

!CHP - puncture variables, not functional
      REAL PUNCSD, PUNCTR
      REAL  RZrwu(300),RZtrwup,AVG_HROOT,WTDEP,qsr(300),TRWUP1
      INTEGER ISTRESS

!     Species-dependant variables exported to SPAM module:
      REAL KCAN, KEP, RWUEP1

      CHARACTER*1 MEM_DETACH
      CHARACTER*6 MEM_ECONO
      CHARACTER*255 MEM_FILECC, MEM_FILEGC

      INTEGER MEM_NDLEAF, MEM_NDSET, MEM_NOUTDO,
     &    MEM_NR1, MEM_NR2, MEM_NR5, MEM_NR7
      INTEGER MEM_YREMRG, MEM_YRNR1, MEM_YRNR2,
     &    MEM_YRNR3, MEM_YRNR5, MEM_YRNR7

      REAL MEM_AGEFAC, MEM_AGRSD3, MEM_AREALF, MEM_ASMDOT
      REAL MEM_AGRLF, MEM_AGRRT, MEM_AGRSH2, MEM_AGRSTM
      REAL MEM_AGRSD1, MEM_AGRSD2, MEM_AGRVG, MEM_AGRVG2
      REAL MEM_AGRNOD, MEM_AGRSH1
      REAL MEM_BETN
      REAL MEM_CANNAA, MEM_CANWAA, MEM_CDMREP, MEM_CGRSH, MEM_CGRSD, 
     + MEM_CNODMN, MEM_CO2, MEM_CSAVEV
      REAL MEM_CNDFX, MEM_CTONODR, MEM_CTONOD, MEM_CAVVEG
      REAL MEM_CADPR1, MEM_CMOBMX, MEM_CMINEP
      REAL MEM_CLW, MEM_CSW
      REAL MEM_CNOD, MEM_CRUSLF, MEM_CRUSRT, MEM_CRUSST,
     &    MEM_CRUSSH, MEM_CADLF, MEM_CADST, MEM_CANWH, MEM_CMINEA
      REAL MEM_DAYL, MEM_DWNOD
      REAL MEM_DWNODA, MEM_DISLA, MEM_DRPP, MEM_DTX, MEM_DXR57
      REAL MEM_EP1, MEM_EXCESS
      REAL MEM_FNINSH, MEM_FRACDN, MEM_FRCNOD,
     &    MEM_F, MEM_FNINL, MEM_FNINR, MEM_FNINS, MEM_FNINSD,
     &    MEM_FRLF, MEM_FRRT, MEM_FRSTM
      REAL MEM_FREEZ1, MEM_FREEZ2
      REAL MEM_GDMSD, MEM_GRRAT1, MEM_GROWTH, MEM_GRWRES
      REAL MEM_LAIMX, MEM_LAGSD, MEM_LNGPEG
      REAL MEM_MAINR
      REAL MEM_NPLTD, MEM_NDMNEW,
     &    MEM_NADRT,  MEM_NADST,  MEM_NGRLF,  MEM_NGRRT,  MEM_NGRST,
     &    MEM_NADLF, MEM_NMINEA,
     &    MEM_NDMOLD, MEM_NDMREP, MEM_NDMSDR, MEM_NDMTOT, MEM_NDMVEG,
     &    MEM_NMINEP, MEM_NMOBR, MEM_NDTH
      REAL MEM_NAVL, MEM_NRUSSH, MEM_NGRSD, MEM_NGRSH
      REAL MEM_NRUSLF, MEM_NRUSRT, MEM_NRUSST
      REAL MEM_POTLIP, MEM_POTCAR, MEM_PPLTD,
     &    MEM_PAR, MEM_PCTMAT
      REAL MEM_PLIGLF, MEM_PLIGNO, MEM_PLIGRT, MEM_PLIGST
      REAL MEM_PODWTD,MEM_PG, MEM_PCLSD, MEM_PCCSD, MEM_PCNSH, MEM_PODWT
      REAL MEM_PODNO
      REAL MEM_PGAVL, MEM_PLTPOP, MEM_PCARSH, MEM_PCH2O, MEM_PLIPSH,
     &    MEM_PLIGSD, MEM_PLIGSH, MEM_PMINSD, MEM_PMINSH, MEM_POASD,
     &    MEM_POASH, MEM_PROLFI, MEM_PRORTI, MEM_PROSHI, MEM_PROSTI
      REAL MEM_R30C2, MEM_RCH2O, MEM_RES30C, MEM_RPROAV,
     &    MEM_RFIXN, MEM_RLIG, MEM_RLIP, MEM_RMIN, MEM_RNITP,
     &    MEM_RNH4C, MEM_RNO3C, MEM_ROA, MEM_RPRO
      REAL MEM_RO, MEM_RP, MEM_RHOS, MEM_ROWSPC
      REAL MEM_RVSTGE, MEM_RSPNO3, MEM_RSPNH4
      REAL MEM_SEEDNI, MEM_NAVLV, MEM_PROVEG, MEM_SDNPL

!     Senescence variables computed for soil N routines.
      REAL MEM_SENNOD(NL), MEM_SENRT(NL)

      REAL MEM_SATFAC, MEM_SDWTAH,
     &    MEM_SDRATE, MEM_SDIDOT,
     &    MEM_SDVAR, MEM_SHVAR, MEM_SDGR, MEM_SDPROR, MEM_SHELWT,
     &    MEM_SLA, MEM_SLDOT, MEM_SWIDOT, MEM_SEEDNO
      REAL MEM_SLAAD, MEM_SLNDOT, MEM_SSDOT, MEM_SSNDOT
      REAL MEM_TDAY, MEM_TDUMX, MEM_TDUMX2, MEM_TGROAV, MEM_TMIN, 
     &    MEM_TAVG, MEM_TURADD,
     &    MEM_TRNH4U, MEM_TRNO3U, MEM_TNLEAK, MEM_TTFIX, MEM_TOTWT
      REAL MEM_VSTAGE
      REAL MEM_WLFDOT, MEM_WSIDOT, MEM_WRIDOT
      REAL MEM_WTNCAN, MEM_WTNLA, MEM_WTNLO, MEM_WTNNA, MEM_WTNNAG
      REAL MEM_WTNNO, MEM_WTNNOD, MEM_WTNRA, MEM_WTNRO, MEM_WTNSA, 
     & MEM_WTNSDA
      REAL MEM_WTNSDO, MEM_WTNSHA, MEM_WTNSHO, MEM_WTNSO, MEM_WTNUP,
     &  MEM_WTNOO
      REAL MEM_WTLO, MEM_WTSO, MEM_WTRO, MEM_WSDDTN, MEM_WSHDTN, 
     & MEM_WTABRT, MEM_WTSHMT
      REAL MEM_WTCO, MEM_WTSHO, MEM_WTSDO, MEM_WCRLF, MEM_WCRRT,
     &    MEM_WCRSH, MEM_WLDOTN, MEM_WRDOTN,  MEM_WSDOTN,
     &    MEM_WCRST, MEM_WNRLF, MEM_WNRRT, MEM_WNRSH, MEM_WNRST,
     &    MEM_WTNRT, MEM_WTNSD, MEM_WTNSH, MEM_WTNST, MEM_WTNEW,
     &    MEM_WTMAIN, MEM_WTNLF, MEM_WSHIDT, MEM_WLIDOT
      REAL MEM_XPOD, MEM_XFRT, MEM_XLAI

      REAL MEM_PHTHRS(20)
      REAL MEM_TGRO(TS)
      REAL MEM_SDDES(NCOHORTS)
      REAL MEM_WTSD(NCOHORTS), MEM_WTSHE(NCOHORTS), MEM_SDNO(NCOHORTS) 
      REAL MEM_SHELN(NCOHORTS)
      REAL MEM_PHTIM(365)
      REAL MEM_PNTIM(365)

      REAL MEM_FLWN(NCOHORTS)

      REAL MEM_PUNCSD, MEM_PUNCTR
      REAL  MEM_RZrwu(300),MEM_RZtrwup,MEM_AVG_HROOT,MEM_WTDEP,
     + MEM_qsr(300),MEM_TRWUP1
      INTEGER MEM_ISTRESS

!     Species-dependant variables exported to SPAM module:
      REAL MEM_KCAN, MEM_KEP, MEM_RWUEP1

      SELECT CASE(CODE)
        CASE('GET')
          DETACH = MEM_DETACH
          ECONO = MEM_ECONO
          FILECC = MEM_FILECC
          FILEGC = MEM_FILEGC

          NDLEAF = MEM_NDLEAF
          NDSET = MEM_NDSET
          NOUTDO = MEM_NOUTDO
          NR1 = MEM_NR1
          NR2 = MEM_NR2
          NR5 = MEM_NR5
          NR7 = MEM_NR7
          YREMRG = MEM_YREMRG
          YRNR1 = MEM_YRNR1
          YRNR2 = MEM_YRNR2
          YRNR3 = MEM_YRNR3
          YRNR5 = MEM_YRNR5
          YRNR7 = MEM_YRNR7

          AGEFAC = MEM_AGEFAC
          AGRSD3 = MEM_AGRSD3
          AREALF = MEM_AREALF
          ASMDOT = MEM_ASMDOT
          AGRLF = MEM_AGRLF
          AGRRT = MEM_AGRRT
          AGRSH2 = MEM_AGRSH2
          AGRSTM = MEM_AGRSTM
          AGRSD1 = MEM_AGRSD1
          AGRSD2 = MEM_AGRSD2
          AGRVG = MEM_AGRVG
          AGRVG2 = MEM_AGRVG2
          AGRNOD = MEM_AGRNOD
          AGRSH1 = MEM_AGRSH1
          BETN = MEM_BETN
          CANNAA = MEM_CANNAA
          CANWAA = MEM_CANWAA
          CDMREP = MEM_CDMREP
          CGRSH = MEM_CGRSH
          CGRSD = MEM_CGRSD
          CNODMN = MEM_CNODMN
          CO2 = MEM_CO2
          CSAVEV = MEM_CSAVEV
          CNDFX = MEM_CNDFX
          CTONODR = MEM_CTONODR
          CTONOD = MEM_CTONOD
          CAVVEG = MEM_CAVVEG
          CADPR1 = MEM_CADPR1
          CMOBMX = MEM_CMOBMX
          CMINEP = MEM_CMINEP
          CLW = MEM_CLW
          CSW = MEM_CSW
          CNOD = MEM_CNOD
          CRUSLF = MEM_CRUSLF
          CRUSRT = MEM_CRUSRT
          CRUSST = MEM_CRUSST
          CRUSSH = MEM_CRUSSH
          CADLF = MEM_CADLF
          CADST = MEM_CADST
          CANWH = MEM_CANWH
          CMINEA = MEM_CMINEA
          DAYL = MEM_DAYL
          DWNOD = MEM_DWNOD
          DWNODA = MEM_DWNODA
          DISLA = MEM_DISLA
          DRPP = MEM_DRPP
          DTX = MEM_DTX
          DXR57 = MEM_DXR57
          EP1 = MEM_EP1
          EXCESS = MEM_EXCESS
          FNINSH = MEM_FNINSH
          FRACDN = MEM_FRACDN
          FRCNOD = MEM_FRCNOD
          F = MEM_F
          FNINL = MEM_FNINL
          FNINR = MEM_FNINR
          FNINS = MEM_FNINS
          FNINSD = MEM_FNINSD
          FRLF = MEM_FRLF
          FRRT = MEM_FRRT
          FRSTM = MEM_FRSTM
          FREEZ1 = MEM_FREEZ1
          FREEZ2 = MEM_FREEZ2
          GDMSD = MEM_GDMSD
          GRRAT1 = MEM_GRRAT1
          GROWTH = MEM_GROWTH
          GRWRES = MEM_GRWRES
          LAIMX = MEM_LAIMX
          LAGSD = MEM_LAGSD
          LNGPEG = MEM_LNGPEG
          MAINR = MEM_MAINR
          NPLTD = MEM_NPLTD
          NDMNEW = MEM_NDMNEW
          NADRT = MEM_NADRT
          NADST = MEM_NADST
          NGRLF = MEM_NGRLF
          NGRRT = MEM_NGRRT
          NGRST = MEM_NGRST
          NADLF = MEM_NADLF
          NMINEA = MEM_NMINEA
          NDMOLD = MEM_NDMOLD
          NDMREP = MEM_NDMREP
          NDMSDR = MEM_NDMSDR
          NDMTOT = MEM_NDMTOT
          NDMVEG = MEM_NDMVEG
          NMINEP = MEM_NMINEP
          NMOBR = MEM_NMOBR
          NDTH = MEM_NDTH
          NAVL = MEM_NAVL
          NRUSSH = MEM_NRUSSH
          NGRSD = MEM_NGRSD
          NGRSH = MEM_NGRSH
          NRUSLF = MEM_NRUSLF
          NRUSRT = MEM_NRUSRT
          NRUSST = MEM_NRUSST
          POTLIP = MEM_POTLIP
          POTCAR = MEM_POTCAR
          PPLTD = MEM_PPLTD
          PAR = MEM_PAR
          PCTMAT = MEM_PCTMAT
          PLIGLF = MEM_PLIGLF
          PLIGNO = MEM_PLIGNO
          PLIGRT = MEM_PLIGRT
          PLIGST = MEM_PLIGST
          PODWTD = MEM_PODWTD
          PG = MEM_PG
          PCLSD = MEM_PCLSD
          PCCSD = MEM_PCCSD
          PCNSH = MEM_PCNSH
          PODWT = MEM_PODWT
          PODNO = MEM_PODNO
          PGAVL = MEM_PGAVL
          PLTPOP = MEM_PLTPOP
          PCARSH = MEM_PCARSH
          PCH2O = MEM_PCH2O
          PLIPSH = MEM_PLIPSH
          PLIGSD = MEM_PLIGSD
          PLIGSH = MEM_PLIGSH
          PMINSD = MEM_PMINSD
          PMINSH = MEM_PMINSH
          POASD = MEM_POASD
          POASH = MEM_POASH
          PROLFI = MEM_PROLFI
          PRORTI = MEM_PRORTI
          PROSHI = MEM_PROSHI
          PROSTI = MEM_PROSTI
          R30C2 = MEM_R30C2
          RCH2O = MEM_RCH2O
          RES30C = MEM_RES30C
          RPROAV = MEM_RPROAV
          RFIXN = MEM_RFIXN
          RLIG = MEM_RLIG
          RLIP = MEM_RLIP
          RMIN = MEM_RMIN
          RNITP = MEM_RNITP
          RNH4C = MEM_RNH4C
          RNO3C = MEM_RNO3C
          ROA = MEM_ROA
          RPRO = MEM_RPRO
          RO = MEM_RO
          RP = MEM_RP
          RHOS = MEM_RHOS
          ROWSPC = MEM_ROWSPC
          RVSTGE = MEM_RVSTGE
          RSPNO3 = MEM_RSPNO3
          RSPNH4 = MEM_RSPNH4
          SEEDNI = MEM_SEEDNI
          NAVLV = MEM_NAVLV
          PROVEG = MEM_PROVEG
          SDNPL = MEM_SDNPL

!     Senescence variables computed for soil N routines.
          SENNOD = MEM_SENNOD(NL)
          SENRT = MEM_SENRT(NL)

          SATFAC = MEM_SATFAC
          SDWTAH = MEM_SDWTAH
          SDRATE = MEM_SDRATE
          SDIDOT = MEM_SDIDOT
          SDVAR = MEM_SDVAR
          SHVAR = MEM_SHVAR
          SDGR = MEM_SDGR
          SDPROR = MEM_SDPROR
          SHELWT = MEM_SHELWT
          SLA = MEM_SLA
          SLDOT = MEM_SLDOT
          SWIDOT = MEM_SWIDOT
          SEEDNO = MEM_SEEDNO
          SLAAD = MEM_SLAAD
          SLNDOT = MEM_SLNDOT
          SSDOT = MEM_SSDOT
          SSNDOT = MEM_SSNDOT
          TDAY = MEM_TDAY
          TDUMX = MEM_TDUMX
          TDUMX2 = MEM_TDUMX2
          TGROAV = MEM_TGROAV
          TMIN = MEM_TMIN
          TAVG = MEM_TAVG
          TURADD = MEM_TURADD
          TRNH4U = MEM_TRNH4U
          TRNO3U = MEM_TRNO3U
          TNLEAK = MEM_TNLEAK
          TTFIX = MEM_TTFIX
          TOTWT = MEM_TOTWT
          VSTAGE = MEM_VSTAGE
          WLFDOT = MEM_WLFDOT
          WSIDOT = MEM_WSIDOT
          WRIDOT = MEM_WRIDOT
          WTNCAN = MEM_WTNCAN
          WTNLA = MEM_WTNLA
          WTNLO = MEM_WTNLO
          WTNNA = MEM_WTNNA
          WTNNAG = MEM_WTNNAG
          WTNNO = MEM_WTNNO
          WTNNOD = MEM_WTNNOD
          WTNRA = MEM_WTNRA
          WTNRO = MEM_WTNRO
          WTNSA = MEM_WTNSA
          WTNSDA = MEM_WTNSDA
          WTNSDO = MEM_WTNSDO
          WTNSHA = MEM_WTNSHA
          WTNSHO = MEM_WTNSHO
          WTNSO = MEM_WTNSO
          WTNUP = MEM_WTNUP
          WTNOO = MEM_WTNOO
          WTLO = MEM_WTLO
          WTSO = MEM_WTSO
          WTRO = MEM_WTRO
          WSDDTN = MEM_WSDDTN
          WSHDTN = MEM_WSHDTN
          WTABRT = MEM_WTABRT
          WTSHMT = MEM_WTSHMT
          WTCO = MEM_WTCO
          WTSHO = MEM_WTSHO
          WTSDO = MEM_WTSDO
          WCRLF = MEM_WCRLF
          WCRRT = MEM_WCRRT
          WCRSH = MEM_WCRSH
          WLDOTN = MEM_WLDOTN
          WRDOTN = MEM_WRDOTN
          WSDOTN = MEM_WSDOTN
          WCRST = MEM_WCRST
          WNRLF = MEM_WNRLF
          WNRRT = MEM_WNRRT
          WNRSH = MEM_WNRSH
          WNRST = MEM_WNRST
          WTNRT = MEM_WTNRT
          WTNSD = MEM_WTNSD
          WTNSH = MEM_WTNSH
          WTNST = MEM_WTNST
          WTNEW = MEM_WTNEW
          WTMAIN = MEM_WTMAIN
          WTNLF = MEM_WTNLF
          WSHIDT = MEM_WSHIDT
          WLIDOT = MEM_WLIDOT
          XPOD = MEM_XPOD
          XFRT = MEM_XFRT
          XLAI = MEM_XLAI

          PHTHRS = MEM_PHTHRS
          TGRO = MEM_TGRO
          SDDES = MEM_SDDES
          WTSD = MEM_WTSD
          WTSHE = MEM_WTSHE
          SDNO = MEM_SDNO
          SHELN = MEM_SHELN
          PHTIM = MEM_PHTIM
          PNTIM = MEM_PNTIM

          FLWN = MEM_FLWN

          PUNCSD = MEM_PUNCSD
          PUNCTR = MEM_PUNCTR
          RZrwu = MEM_RZrwu
          RZtrwup = MEM_RZtrwup
          AVG_HROOT = MEM_AVG_HROOT
          WTDEP = MEM_WTDEP
          qsr = MEM_qsr
          TRWUP1 = MEM_TRWUP1
          ISTRESS = MEM_ISTRESS

          KCAN = MEM_KCAN
          KEP = MEM_KEP
          RWUEP1 = MEM_RWUEP1

        CASE('PUT')
          MEM_DETACH = DETACH
          MEM_ECONO = ECONO
          MEM_FILECC = FILECC
          MEM_FILEGC = FILEGC
          MEM_NDLEAF = NDLEAF
          MEM_NDSET = NDSET
          MEM_NOUTDO = NOUTDO
          MEM_NR1 = NR1
          MEM_NR2 = NR2
          MEM_NR5 = NR5
          MEM_NR7 = NR7
          MEM_YREMRG = YREMRG
          MEM_YRNR1 = YRNR1
          MEM_YRNR2 = YRNR2
          MEM_YRNR3 = YRNR3
          MEM_YRNR5 = YRNR5
          MEM_YRNR7 = YRNR7
          MEM_AGEFAC = AGEFAC
          MEM_AGRSD3 = AGRSD3
          MEM_AREALF = AREALF
          MEM_ASMDOT = ASMDOT
          MEM_AGRLF = AGRLF
          MEM_AGRRT = AGRRT
          MEM_AGRSH2 = AGRSH2
          MEM_AGRSTM = AGRSTM
          MEM_AGRSD1 = AGRSD1
          MEM_AGRSD2 = AGRSD2
          MEM_AGRVG = AGRVG
          MEM_AGRVG2 = AGRVG2
          MEM_AGRNOD = AGRNOD
          MEM_AGRSH1 = AGRSH1
          MEM_BETN = BETN
          MEM_CANNAA = CANNAA
          MEM_CANWAA = CANWAA
          MEM_CDMREP = CDMREP
          MEM_CGRSH = CGRSH
          MEM_CGRSD = CGRSD
          MEM_CNODMN = CNODMN
          MEM_CO2 = CO2
          MEM_CSAVEV = CSAVEV
          MEM_CNDFX = CNDFX
          MEM_CTONODR = CTONODR
          MEM_CTONOD = CTONOD
          MEM_CAVVEG = CAVVEG
          MEM_CADPR1 = CADPR1
          MEM_CMOBMX = CMOBMX
          MEM_CMINEP = CMINEP
          MEM_CLW = CLW
          MEM_CSW = CSW
          MEM_CNOD = CNOD
          MEM_CRUSLF = CRUSLF
          MEM_CRUSRT = CRUSRT
          MEM_CRUSST = CRUSST
          MEM_CRUSSH = CRUSSH
          MEM_CADLF = CADLF
          MEM_CADST = CADST
          MEM_CANWH = CANWH
          MEM_CMINEA = CMINEA
          MEM_DAYL = DAYL
          MEM_DWNOD = DWNOD
          MEM_DWNODA = DWNODA
          MEM_DISLA = DISLA
          MEM_DRPP = DRPP
          MEM_DTX = DTX
          MEM_DXR57 = DXR57
          MEM_EP1 = EP1
          MEM_EXCESS = EXCESS
          MEM_FNINSH = FNINSH
          MEM_FRACDN = FRACDN
          MEM_FRCNOD = FRCNOD
          MEM_F = F
          MEM_FNINL = FNINL
          MEM_FNINR = FNINR
          MEM_FNINS = FNINS
          MEM_FNINSD = FNINSD
          MEM_FRLF = FRLF
          MEM_FRRT = FRRT
          MEM_FRSTM = FRSTM
          MEM_FREEZ1 = FREEZ1
          MEM_FREEZ2 = FREEZ2
          MEM_GDMSD = GDMSD
          MEM_GRRAT1 = GRRAT1
          MEM_GROWTH = GROWTH
          MEM_GRWRES = GRWRES
          MEM_LAIMX = LAIMX
          MEM_LAGSD = LAGSD
          MEM_LNGPEG = LNGPEG
          MEM_MAINR = MAINR
          MEM_NPLTD = NPLTD
          MEM_NDMNEW = NDMNEW
          MEM_NADRT = NADRT
          MEM_NADST = NADST
          MEM_NGRLF = NGRLF
          MEM_NGRRT = NGRRT
          MEM_NGRST = NGRST
          MEM_NADLF = NADLF
          MEM_NMINEA = NMINEA
          MEM_NDMOLD = NDMOLD
          MEM_NDMREP = NDMREP
          MEM_NDMSDR = NDMSDR
          MEM_NDMTOT = NDMTOT
          MEM_NDMVEG = NDMVEG
          MEM_NMINEP = NMINEP
          MEM_NMOBR = NMOBR
          MEM_NDTH = NDTH
          MEM_NAVL = NAVL
          MEM_NRUSSH = NRUSSH
          MEM_NGRSD = NGRSD
          MEM_NGRSH = NGRSH
          MEM_NRUSLF = NRUSLF
          MEM_NRUSRT = NRUSRT
          MEM_NRUSST = NRUSST
          MEM_POTLIP = POTLIP
          MEM_POTCAR = POTCAR
          MEM_PPLTD = PPLTD
          MEM_PAR = PAR
          MEM_PCTMAT = PCTMAT
          MEM_PLIGLF = PLIGLF
          MEM_PLIGNO = PLIGNO
          MEM_PLIGRT = PLIGRT
          MEM_PLIGST = PLIGST
          MEM_PODWTD = PODWTD
          MEM_PG = PG
          MEM_PCLSD = PCLSD
          MEM_PCCSD = PCCSD
          MEM_PCNSH = PCNSH
          MEM_PODWT = PODWT
          MEM_PODNO = PODNO
          MEM_PGAVL = PGAVL
          MEM_PLTPOP = PLTPOP
          MEM_PCARSH = PCARSH
          MEM_PCH2O = PCH2O
          MEM_PLIPSH = PLIPSH
          MEM_PLIGSD = PLIGSD
          MEM_PLIGSH = PLIGSH
          MEM_PMINSD = PMINSD
          MEM_PMINSH = PMINSH
          MEM_POASD = POASD
          MEM_POASH = POASH
          MEM_PROLFI = PROLFI
          MEM_PRORTI = PRORTI
          MEM_PROSHI = PROSHI
          MEM_PROSTI = PROSTI
          MEM_R30C2 = R30C2
          MEM_RCH2O = RCH2O
          MEM_RES30C = RES30C
          MEM_RPROAV = RPROAV
          MEM_RFIXN = RFIXN
          MEM_RLIG = RLIG
          MEM_RLIP = RLIP
          MEM_RMIN = RMIN
          MEM_RNITP = RNITP
          MEM_RNH4C = RNH4C
          MEM_RNO3C = RNO3C
          MEM_ROA = ROA
          MEM_RPRO = RPRO
          MEM_RO = RO
          MEM_RP = RP
          MEM_RHOS = RHOS
          MEM_ROWSPC = ROWSPC
          MEM_RVSTGE = RVSTGE
          MEM_RSPNO3 = RSPNO3
          MEM_RSPNH4 = RSPNH4
          MEM_SEEDNI = SEEDNI
          MEM_NAVLV = NAVLV
          MEM_PROVEG = PROVEG
          MEM_SDNPL = SDNPL
          MEM_SENNOD = SENNOD
          MEM_SENRT = SENRT
          MEM_SATFAC = SATFAC
          MEM_SDWTAH = SDWTAH
          MEM_SDRATE = SDRATE
          MEM_SDIDOT = SDIDOT
          MEM_SDVAR = SDVAR
          MEM_SHVAR = SHVAR
          MEM_SDGR = SDGR
          MEM_SDPROR = SDPROR
          MEM_SHELWT = SHELWT
          MEM_SLA = SLA
          MEM_SLDOT = SLDOT
          MEM_SWIDOT = SWIDOT
          MEM_SEEDNO = SEEDNO
          MEM_SLAAD = SLAAD
          MEM_SLNDOT = SLNDOT
          MEM_SSDOT = SSDOT
          MEM_SSNDOT = SSNDOT
          MEM_TDAY = TDAY
          MEM_TDUMX = TDUMX
          MEM_TDUMX2 = TDUMX2
          MEM_TGROAV = TGROAV
          MEM_TMIN = TMIN
          MEM_TAVG = TAVG
          MEM_TURADD = TURADD
          MEM_TRNH4U = TRNH4U
          MEM_TRNO3U = TRNO3U
          MEM_TNLEAK = TNLEAK
          MEM_TTFIX = TTFIX
          MEM_TOTWT = TOTWT
          MEM_VSTAGE = VSTAGE
          MEM_WLFDOT = WLFDOT
          MEM_WSIDOT = WSIDOT
          MEM_WRIDOT = WRIDOT
          MEM_WTNCAN = WTNCAN
          MEM_WTNLA = WTNLA
          MEM_WTNLO = WTNLO
          MEM_WTNNA = WTNNA
          MEM_WTNNAG = WTNNAG
          MEM_WTNNO = WTNNO
          MEM_WTNNOD = WTNNOD
          MEM_WTNRA = WTNRA
          MEM_WTNRO = WTNRO
          MEM_WTNSA = WTNSA
          MEM_WTNSDA = WTNSDA
          MEM_WTNSDO = WTNSDO
          MEM_WTNSHA = WTNSHA
          MEM_WTNSHO = WTNSHO
          MEM_WTNSO = WTNSO
          MEM_WTNUP = WTNUP
          MEM_WTNOO = WTNOO
          MEM_WTLO = WTLO
          MEM_WTSO = WTSO
          MEM_WTRO = WTRO
          MEM_WSDDTN = WSDDTN
          MEM_WSHDTN = WSHDTN
          MEM_WTABRT = WTABRT
          MEM_WTSHMT = WTSHMT
          MEM_WTCO = WTCO
          MEM_WTSHO = WTSHO
          MEM_WTSDO = WTSDO
          MEM_WCRLF = WCRLF
          MEM_WCRRT = WCRRT
          MEM_WCRSH = WCRSH
          MEM_WLDOTN = WLDOTN
          MEM_WRDOTN = WRDOTN
          MEM_WSDOTN = WSDOTN
          MEM_WCRST = WCRST
          MEM_WNRLF = WNRLF
          MEM_WNRRT = WNRRT
          MEM_WNRSH = WNRSH
          MEM_WNRST = WNRST
          MEM_WTNRT = WTNRT
          MEM_WTNSD = WTNSD
          MEM_WTNSH = WTNSH
          MEM_WTNST = WTNST
          MEM_WTNEW = WTNEW
          MEM_WTMAIN = WTMAIN
          MEM_WTNLF = WTNLF
          MEM_WSHIDT = WSHIDT
          MEM_WLIDOT = WLIDOT
          MEM_XPOD = XPOD
          MEM_XFRT = XFRT
          MEM_XLAI = XLAI
          MEM_PHTHRS = PHTHRS
          MEM_TGRO = TGRO
          MEM_SDDES = SDDES
          MEM_WTSD = WTSD
          MEM_WTSHE = WTSHE
          MEM_SDNO = SDNO
          MEM_SHELN = SHELN
          MEM_PHTIM = PHTIM
          MEM_PNTIM = PNTIM
          MEM_FLWN = FLWN
          MEM_PUNCSD = PUNCSD
          MEM_PUNCTR = PUNCTR
          MEM_RZrwu = RZrwu
          MEM_RZtrwup = RZtrwup
          MEM_AVG_HROOT = AVG_HROOT
          MEM_WTDEP = WTDEP
          MEM_qsr = qsr
          MEM_TRWUP1 = TRWUP1
          MEM_ISTRESS = ISTRESS
          MEM_KCAN = KCAN
          MEM_KEP = KEP
          MEM_RWUEP1 = RWUEP1
      END SELECT  

      RETURN
      END SUBROUTINE CROPGRO_Memory

      SUBROUTINE GRSTAG_Memory(CODE,FACT)
C       IMPLICIT NONE
      DOUBLE PRECISION FACT
      CHARACTER (len=3)  CODE
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          FACT = MEM_FACT
        CASE('PUT')
          MEM_FACT = FACT
      END SELECT
      
      RETURN
      END SUBROUTINE GRSTAG_Memory

      SUBROUTINE ADJDT_Memory(CODE,ASAVE,TFIRST,DELTSV)
C       IMPLICIT NONE
      PARAMETER(MXNOD=300)
      DOUBLE PRECISION DELTSV, ASAVE(MXNOD)
      LOGICAL TFIRST
      DOUBLE PRECISION MEM_DELTSV, MEM_ASAVE(MXNOD)
      LOGICAL MEM_TFIRST
      CHARACTER (len=3)  CODE
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          ASAVE = MEM_ASAVE
          TFIRST = MEM_TFIRST
          DELTSV = MEM_DELTSV
        CASE('PUT')
          MEM_ASAVE = ASAVE
          MEM_TFIRST = TFIRST
          MEM_DELTSV = DELTSV
      END SELECT
      
      RETURN
      END SUBROUTINE ADJDT_Memory


      SUBROUTINE PHYSCL_Memory(CODE, PERIOD, SPAN, DELT, START, NBPR, 
     +    COR, TTRO, STMSEG, ND, FIRST5, JSTDAY,
     +    TSTART, OLDSTR, TRFDD, TCII, TROI, NBPRI, CANIRR, HROOT, RNDR, 
     +    SNP, SSTART, PKTEMP, hrootdummy, IDIMIYOLD, wthour, totalsub)
C       IMPLICIT NONE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER (len=3)  CODE
      PARAMETER(MAXBP=50,MXNOD=300,MXNODT=3001,MAXHOR=12,MAXSCT=11,
     +    MXCHEM=15,MXPEST=3,MXSPEC=10,MXAPP=200,MXANA=135,MXTG=500)
      PARAMETER (IPSTO = MAXBP*2,IPSTM = IPSTO*2, THRSNO = 0.0D0)
      CHARACTER PERIOD*5, MEM_PERIOD*5
      DOUBLE PRECISION FACT, MEM_FACT
      LOGICAL SPAN,START,FIRST5,CANIRR,SSTART
      LOGICAL MEM_SPAN,MEM_START,MEM_FIRST5,MEM_CANIRR,MEM_SSTART
      DIMENSION STMSEG(MAXBP,2,2), MEM_STMSEG(MAXBP,2,2)
C
      DATA MEM_PERIOD /'STORM'/, MEM_SPAN /.FALSE./, MEM_COR /1.0D-6/, 
     +    MEM_START /.TRUE./, MEM_DELT /1.0D-4/, MEM_FIRST5 /.TRUE./,
     +    MEM_TROI /0.0D0/, MEM_JSTDAY /0/, MEM_TRFDD /0.0D0/,
     +    MEM_TCII /0.0D0/,MEM_STMSEG /IPSTM*0.0D0/,MEM_CANIRR/.FALSE./
     +    ,MEM_TSTART /0.0D0/,MEM_RNDR /0.0D0/, MEM_NBPRI /0/,
     +    MEM_SNP /0.0D0/,MEM_SSTART /.TRUE./,MEM_PKTEMP /0.0D0/

      SELECT CASE(CODE)
        CASE('PUT')
          MEM_PERIOD = PERIOD
          MEM_SPAN = SPAN
          MEM_DELT = DELT
          MEM_START = START
          MEM_NBPR = NBPR
          MEM_COR = COR
          MEM_TTRO = TTRO
          MEM_STMSEG = STMSEG
          MEM_ND = ND
          MEM_FIRST5 = FIRST5
          MEM_JSTDAY = JSTDAY
          MEM_TSTART = TSTART
          MEM_OLDSTR = OLDSTR
          MEM_TRFDD = TRFDD
          MEM_TCII = TCII
          MEM_TROI = TROI
          MEM_NBPRI = NBPRI
          MEM_CANIRR = CANIRR
          MEM_HROOT = HROOT
          MEM_RNDR = RNDR
          MEM_SNP = SNP
          MEM_SSTART = SSTART
          MEM_PKTEMP = PKTEMP
          MEM_hrootdummy = hrootdummy
          MEM_IDIMIYOLD = IDIMIYOLD
          MEM_wthour = wthour
          MEM_totalsub = totalsub
        CASE('GET')
          PERIOD = MEM_PERIOD
          SPAN = MEM_SPAN
          DELT = MEM_DELT
          START = MEM_START
          NBPR = MEM_NBPR
          COR = MEM_COR
          TTRO = MEM_TTRO
          STMSEG = MEM_STMSEG
          ND = MEM_ND
          FIRST5 = MEM_FIRST5
          JSTDAY = MEM_JSTDAY
          TSTART = MEM_TSTART
          OLDSTR = MEM_OLDSTR
          TRFDD = MEM_TRFDD
          TCII = MEM_TCII
          TROI = MEM_TROI
          NBPRI = MEM_NBPRI
          CANIRR = MEM_CANIRR
          HROOT = MEM_HROOT
          RNDR = MEM_RNDR
          SNP = MEM_SNP
          SSTART = MEM_SSTART
          PKTEMP = MEM_PKTEMP
          hrootdummy = MEM_hrootdummy
          IDIMIYOLD = MEM_IDIMIYOLD
          wthour = MEM_wthour
          totalsub = MEM_totalsub
      END SELECT
      
      RETURN
      END SUBROUTINE PHYSCL_Memory

      SUBROUTINE RZMAIN_Memory(CODE,)
      IMPLICIT NONE
      SAVE

      SELECT CASE(CODE)
        CASE('GET')
        CASE('PUT')
      END SELECT


      RETURN
      END SUBROUTINE RZMAIN_Memory