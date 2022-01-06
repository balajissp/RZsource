C--this is an include file for the Gleams portion of RZ--
c      Implicit Double Precision (A-H,O-Z)
c
       Parameter (AcresPerHectare = 2.4710d0)
       Parameter (FeetPerMeter    = 3.281d0)
       Parameter (SqrFeetPerAcre  = 43560.0d0)
       Parameter (CmPerInch       = 2.54d0)
       Parameter (MillimetersPerFoot    = 304.8d0)
       Parameter (LbsPerFt2_to_KgPerHa = 48830.76) ! (43560/2000)*2242
	   PARAMETER (MXCHEM =  15)		! from RZ_Params: {po4=16 sab 6/2002}
       PARAMETER (MXNOD  = 300)		! from RZ_Params: {from 40}
       PARAMETER (MXPEST =   3)		! from RZ_Params: 
c
c       common /Gleams_hydsol/clay,silt,Por,OM1,sand,BD !need to be set in RZ or via interface
      common /Gleams_hydsol/clay,silt,por,om1,sand,BulkDn,Dacre,     !bulkdn is not used at all
     +        CHS,ei,exrain,thirtyRR,BYEAR,EYEAR
      common/ErosionLoss/CumulErodedSolute(6)              !1=po4, 2..4 = Pest#1..#3, 5=NH4/NO3
      common/po4log/ErosionActive,UseEnrichment
       COMMON /CONS/ AGRAV,MSDH2O,WTDH2O,WTDSOI,KINVIS,KCH,YALCON,
     &              BETA_ero, NBAROV,NBARCH
       COMMON /PART/DIA(10),SPGR(10),FRAC(10),FALL(10),EQSAND(10),
     1               SOLCLY,SOLSLT,SOLSND,SOLORG,SSCLY,SSSLT,SSSND,
     1               SSORG,SSSOIL,FRCLY(10),FRSLT(10),FRSND(10),
     1               FRORG(10),DCL(11),DDCL(10),FRACK(10) 
       COMMON /Cblk_Int/Npart,Nxk,nxf,nxc1,nxc2,FLAGC1,FLAGS1,CTLO1,
     &             FLAGC2,FLAGS2,CTLO2,CTL,PAC
       COMMON /ROUT/ XSOIL(4),KSOIL(4),XFACT(4),DAOVR,SLNGTH,
     &               CFACT(4,40),PFACT_ero(4,40),NFACT(4,40),
     1               XCHN1(4),NCHN1(4,40),CCHN1(4,40),SCHN1(4,40),
     1               DCHN1(4,40),WCHN1(4,40),
     1               CTLZ1,CTLN1,CTLSL1,RA1,RN1,YBASE1,DACHU1,
     1               DACHL1,Z1,LNGTH1,LEFF1,
     1               XCHN2(4),NCHN2(4,40),CCHN2(4,40),SCHN2(4,40),
     1               DCHN2(4,40),WCHN2(4,40),
     1               CTLZ2,CTLN2,CTLSL2,RA2,RN2,YBASE2,DACHU2,
     1               DACHL2,Z2,LNGTH2,LEFF2                             out
       COMMON /CHAN/ XSTAR(2,40),DEPA(2,40),DEPB(2,40),WIDA(2,40),
     1               WIDB(2,40),WERA(2,40),WERB(2,40)
       COMMON /POND/ DAPND,INTAKE,FRONT,DRAW,SIDE,FS,B_ero,DIAO,C_ero
       COMMON /FALL/ RE(9),CDRE2(9),CDDRE(9)
       COMMON /joeb2/ indjoe
      Common/GL_input_R/XPOVR(40),SPOVR(40),XPCHN1(40),SPCHN1(40),
     &                  XPCHN2(40),SPCHN2(40),DAREA(4),WLW
      Common/GL_input_I/CDATE_ero(41),Ndates,Nyears,eroout,
     &                  FLGSEQ, NPTSO, NPTSC1,metflg,Nptsc2,
     &                  Elem,FlgTmp
       COMMON /ARRAY/ X1_ero(40),S1_ero(40),X2_ero(40),S2_ero(40)
       COMMON /PASS/ RNFALL,RUNOFF
       COMMON /BCKERO/ ELMLOS(4), ELMMON(4), ELMANN(4), ELMENR(4),
     &                 ENRMON(4), ENRANN(4)                             
       COMMON /EROEND/ TRAINT, TRUNOT, TGST(4,10)       
       COMMON /GDATES/ NMONTH, NYEAR, IEOMY ,NRAINT, NRUNOT, Sdate
       common /cover/slr
       double precision KINVIS,KCH,NBAROV,NBARCH,MSDH2O,KSOIL,NFACT,
     &        NCHN1,NCHN2,
     &        LNGTH1,LEFF1,LNGTH2,LEFF2,INTAKE,XPOVR,SPOVR,XPCHN1,Agrav,
     &        SPCHN1,XPCHN2,SPCHN2,DAREA,SSCLY,SSSLT,SSSND,SSORG,
     &        SSSOIL,FRCLY,FRSLT,FRSND,FRORG,DCL,DDCL,Xsoil,Xfact,Cfact,
     &        Pfact_ero,Daovr,Slngth,xchn1,cchn1,schn1,dchn1,wchn1,
     &        ctlz1,RE,CDRE2,CDDRE,Dacre,CHS,WLW,frac,frack,
     &        ctln1,ctlsl1,ra1,rn1,ybase1,dachu1,z1,xchn2,cchn2,schn2,
     &        dchn2,wchn2,ctlz2,ctln2,ctlsl2,ra2,rn2,ybase2,dachu2,
     &        dachl2,z2,xstar,depa,depb,wida,widb,wera,werb,dapnd,
     &        front,draw,side,fs,b_ero,diao,c_ero,clay,silt,por,om1,
     &        dachl1,Beta_ero,YALCON,sand,BulkDn,Dia,thirtyRR,
     &        RNFALL,RUNOFF,EXRAIN,EI,ELMLOS, ELMMON, ELMANN, 
     &        ELMENR,ENRMON, ENRANN,TRAINT, TRUNOT, TGST,slr
      Integer Nxk,Nxf,Nxc1,nxc2,indjoe,NRAINT, NRUNOT, NMONTH, NYEAR, 
     &        IEOMY,FLAGC1,FLAGS1,CTLO1,FLAGC2,FLAGS2,CTLO2,CTL,PAC,
     &        BYEAR,EYEAR,CDATE_ero,Ndates,Nyears,EROOUT,Sdate,
     &        FLGSEQ, NPTSO, NPTSC1,METFLG, Nptsc2,Elem,FlgTmp
      


