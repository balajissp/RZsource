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


      SUBROUTINE DSSATDRV_Memory(CODE,oldhgt,PLHGHT,PLALFA)
      DOUBLE PRECISION oldhgt,PLHGHT,PLALFA
      CHARACTER (len=3)  CODE
      SAVE
      
      SELECT CASE(CODE)
        CASE('GET')
          oldhgt = MEM_oldhgt
          PLHGHT = MEM_PLHGHT
          PLALFA = MEM_PLALFA
        CASE('PUT')
          MEM_oldhgt = oldhgt
          MEM_PLHGHT = PLHGHT
          MEM_PLALFA = PLALFA
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


