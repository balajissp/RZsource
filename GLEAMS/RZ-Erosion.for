c-sab erosion model input begins by Steve Bloom, University of Florida
      Subroutine ResetGleamsOrganicFraction(FRACOM1)
C======================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      include 'Rz_Params.inc' 
      include 'Rz_Gleams.inc'  !sab 6/2002
      OM1 = FRACOM1 !organic material at top -reset daily in RzMain
      return
      end
      SUBROUTINE Read_Gleams_Erosion_Input(IO,ErosionIsActive)
C======================================================================
      include 'Rz_Gleams.inc'  !sab 6/2002
      Dimension XTEMP(40),SLTEMP(40),DnYear(40)
      double precision Xtemp,Sltemp
      integer DnYear
	character ErrMsg*80
      logical ReadError,AtEOF,ErosionIsActive
      Data ReadError/.false./,AtEOF/.false./          !assume everything is ok
      TotalSedimentLoss = 0.0d00                      !initial cumulative loss variable
      ErosionisActive = .false.                       !set to a non-erosion model state	 
   10 read(io,'(A)',Err=30, End=20) ErrMsg            !read 1st line of descriptive block
       If (ErrMsg.NE.'=UF-Erosion-ADDENDUM') goto 10
       Goto 40                                        !ready to read Gleams Parameters
   20  AtEOF = .true.                                 !AtEOF takes precedence over ReadError 
   30  ReadError = .true.
   40 If (AtEOF) then
        PRINT *, "CALLING LogMessage"
        Call LogMessage('Erosion Model is inactive')  !notify non-function 
      Else If (ReadError) then
        PRINT *, "CALLING LogMessage"
        Call LogMessage('Error during scan for Gleams parameters') !error message to screen
	  Call LogMessage('STOP')                       !kill run 
      Else
        PRINT *, "CALLING Echo"
        Call Echo(Io)                                 ! skip the rest of the descriptive block
        ErosionisActive = .true.                        !set to an active erosion model state	 
        PRINT *, "CALLING LogMessage"
        Call LogMessage('GLEAMS EROSION/SEDIMENT YIELD- Version 3.0')
        Call PrepareToReadErosionParameters
        PRINT *, "CALLING ReadErosionParameters"
        Call ReadErosionParameters(Io,Xtemp,Sltemp)
        PRINT *, "CALLING ReportErosionParameters"
        Call ReportErosionParameters(Io,Xtemp,Sltemp,DnYear)
        PRINT *, "CALLING ProcessErosionParameters"
        Call ProcessErosionParameters(DnYear)
      endif
      return
      end
C======================================================================
      SUBROUTINE PrepareToReadErosionParameters
C======================================================================
c this subroutine was originally part of EROSA
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision REB(9),CDRB(9),CDDB(9)
      DATA REB  /0.001d0,0.01d0,0.1d0,1.0d0,10.0d0,100.0d0,1000.0d0,
     1            10000.0d0,100000.0d0/
      DATA CDRB /0.011d0,0.22d0,2.2d0,22.8d0,420.0d0,11000.0d0,
     1            480000.0d0,4.0d+07,4.8d+09/
      DATA CDDB /2.05d+07,2.05d+05,2.05d+03,24.0d0,0.41d0,
     1            1.05d-02,4.3d-04,4.0d-05,4.4d-06/
      DO 10 J=1,9
           RE(J)    = DLOG(REB(J))
           CDRE2(J) = DLOG(CDRB(J))
           CDDRE(J) = DLOG(CDDB(J))
  10  CONTINUE
      Return
      end
C======================================================================
      SUBROUTINE ReadErosionParameters(Io,Xtemp,Sltemp)
C======================================================================
c this subroutine was originally:
c      Subroutine INIPAR(BYEAR,EYEAR,CDATE,NDATES,NYEARS,EROOUT,
c     &             FLGUPD,FLGSEQ,NPTSO,XPOVR,SPOVR,NPTSC1,XPCHN1,
c     &             SPCHN1,NPTSC2,XPCHN2,SPCHN2,DAREA,METFLG,POR,OM1)
c       COMMON /HEAD2/ TITLE2 - not used in RZ
c Common blocks originally included here were : Cons, Part, Rout, Pond, Undef
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      Dimension XTEMP(40),SLTEMP(40)
      double precision Xtemp,Sltemp
      Integer IO,Ncard
      Character I_4*4, Astr*255
      LOGICAL UseEnrichment,ErosionActive ! RM - had to define the types.
       NCARD  = 0   !card 0 = triplet of Gleams hydrology parameters
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       Read (Io,'(A)',Err=290) Astr
       UseEnrichment = (Astr(1:1) .eq. 'T')
       Write(Astr,'(A,L2)')'Use Enrichment Factor = ',UseEnrichment
       PRINT *, "CALLING Logmessage"
       Call Logmessage(Astr)
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       Read (Io,'(A)',Err=290) Astr
       PRINT *, "CALLING RemoveComments"
       Call RemoveComments(Astr)
       READ (Astr,*,Err=290) Dacre_temp,CHS_temp,WLW   !Christopher Demar reads only WLW. He may have changed the input format
c       READ (Astr,*,Err=290) WLW   !Christopher Demar reads only WLW. He may have changed the input format
       NCARD  = 4   !cards1..3 are title cards in gleams - not used in RZ
c       Call EchoLine(Io)
c       Read (Io,'(A)',Err=290) Astr
c       Call RemoveComments(Astr)
c       READ (Astr,*,Err=290) FLGSEQ,metflg    ! Demar reads only FLGSEQ, others from RZWQM2
c       READ (Astr,*,Err=290) EROOUT
       EROOUT=5
       FLGSEQ=1                                                       ! Overland flow only
       metflg=1                                                       ! metric system only
       Call Initialize 
       NCARD  = 5
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) SSCLY 
       NPART  = 5
       CALL ProcessComposition
       DO 20 K=1,NPART
   20    DIA(K) = DIA(K)/MillimetersPerFoot
C READ VALUES FOR OVERLAND FLOW THAT WILL REMAIN CONSTANT WITH RESPECT TO TIME
       NCARD  = 6                                        
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NPTSO,DAOVR
       if (daovr.le.0.0d0) then
           erosionactive=.false.   !no erosion if drainage area is zero.
           PRINT *, "CALLING LogMessage"
           Call LogMessage('DRAINAGE AREA IS ZERO <===================')
       endif
c
       Ncard = 7
       READ  (IO,*,Err=290) (XTEMP(I),SLTEMP(I), I=1,NPTSO)
       if (sltemp(1).le.0.0d0) then
           erosionactive=.false.   !no erosion if slope is zero.
           PRINT *, "CALLING LogMessage"
           Call LogMessage('SOIL SLOPE IS ZERO <======================')
       endif
       NCARD  = 8                                        
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NXK,(XSOIL(I),KSOIL(I),I=1,NXK)
      Goto 300                                                    !if here, all is ok, so hop and exit
  290 Call LogMessage('Error while reading card'//I_4(Ncard))     !error message to screen
	Call LogMessage('STOP')                                     !kill run otherwise
  300 return
      END
C======================================================================
      SUBROUTINE ReportErosionParameters(Io,Xtemp,Sltemp,DnYear)
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  !sab 6/2002
      include 'Rz_Gleams.inc'  !sab 6/2002
      Dimension XTEMP(40),SLTEMP(40)
      double precision Xtemp,Sltemp
      Integer DNYEAR(40),FLAGP(10),IO,SetReportFlag
       IF (XSOIL(NXK) .NE. 1.0d0) THEN
          PRINT *, "CALLING LogMessage"
          Call LogMessage('  LAST XSOIL VALUE READ IN WAS NOT 1.0,'//
     &      ' BUT WAS RESET TO 1.0')
          XSOIL(NXK) = 1.0d0
       ENDIF
       IF (METFLG .EQ. 1) THEN
          DAOVR    = DAOVR*AcresPerHectare    !now in English units
          DO 95 I=1,NPTSO
   95       XTEMP(I) =XTEMP(I) * FeetPerMeter
       ENDIF
       DAOVR    = DAOVR*SqrFeetPerAcre    ! now in ft^2
       DAREA(1) = DAOVR
       CALL PROFIL(XTEMP,SLTEMP,AVGSLP,FLAGP,NSEG)
       DO 100 I=1,NXK
  100     XSOIL(I) = SLNGTH*XSOIL(I)
       CALL ADDPTS(NXK,XSOIL,NPTSO,XPOVR,SPOVR)
       IF (FLGSEQ.GE.3.AND.FlgSeq.LE.6)                                 {1=overland
     &   Call ReadChannelParameters(Io,NSC1,CTLO1,ra1,rn1,dachl1,       {2=overland+impound
     &           dachu1,z1,FlagS1,FlagC1,lngth1,CTLZ1,CTLN1,CTLSL1,     {3=overland+Channel1
     &           Ybase1,Darea(2),Leff1,XPchn1,SPCHN1,NPTSC1)            {4=overland+Channel1+Channel2
       IF (FLGSEQ.EQ.4.OR.FlgSeq.EQ.6)                                  {5=overland+Channel1+impound
     &   Call ReadChannelParameters(Io,NSC2,CTLO2,ra2,rn2,dachl2,       {6=overalnd+Channel1+Channel2 +impound
     &           dachu2,z2,FlagS2,FlagC2,lngth2,CTLZ2,CTLN2,CTLSL2,     {in data stream, specs are over, 
     &           Ybase2,Darea(3),Leff2,XPchn2,SPCHN2,NPTSC2)            { c1,c2,impound
       IF (FLGSEQ.EQ.2.OR.FlgSeq.GE.5) 
     &   Call ReadImpoundmentParameters(io)
C
C      CALL THE SUBROUTINE FOR READING THE MANAGEMENT PARAMETERS THAT
C      CAN CHANGE WITH RESPECT TO TIME AND BE UPDATED IF DESIRED
C
       CALL UPDPAR(IO,0,DNYEAR,METFLG)
C
C      PRINT THE INITIAL OUTPUT ASSOCIATED WITH PHYSICAL CONSTANTS
C      AND THE SOIL AND ERODED SEDIMENT PARAMETERS
C
       FLGTMP = SetReportFlag(Eroout)
       CALL OUTBEG(FLGTMP)
       CALL ReportOverlandSpatialParameters(FLAGP,AVGSLP,NSEG)
       ! RM - ReportOverlandTermporalParameters too large of a name.
       IF (FLGTMP.EQ.1) CALL RptOverlandTermporalParameters(DNYEAR)
       IF (FLGSEQ.GE.3.AND.FlgSeq.LE.6) then
         CALL ReportChannelSpatial(1,LNGTH1,NPTSC1,XPCHN1,SPCHN1)
         If (Flgtmp.EQ.1) call ReportChannelTemporal(1,DNYEAR)
       endif
       IF (FLGSEQ.EQ.4.OR.FlgSeq.EQ.6) then
         CALL ReportChannelSpatial(2,LNGTH2,NPTSC2,XPCHN2,SPCHN2)
         If (Flgtmp.EQ.1) call ReportChannelTemporal(2,DNYEAR)
       endif
       IF (FLGSEQ.EQ.2.OR.FlgSeq.GE.5) CALL ReportImpoundment
      return
      END
C======================================================================
      SUBROUTINE ReadChannelParameters(Io,NSC,CTLO,ra,rn,dachl,dachu,z,
     &   FlagS,FlagC,lngth,CTLZ,CTLN,CTLSL,Ybase,DareaIndx,Leff,XPchn,
     &   SPCHN,NPTSC)
C======================================================================
c this subroutine was originally part of EROSA
C READ AND ECHO THE VALUES FOR CHANNEL THAT WILL REMAIN CONSTANT WITH RESPECT TO TIME
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  !sab 6/2002
      include 'Rz_Gleams.inc'  !sab 6/2002
      Dimension XSLP(10),SSLP(10),XpChn(40),SpChn(40)
      double precision lngth,xslp,sslp,leff,topl,Deltl,diff,xcheck
      integer CTLO,Flags,FlagC,idwn,NPTSC
      character I_4*4
       NCARD  =  9                        
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NSC,CTLO,ra,rn,dachl,dachu,z 
       FLAGS = 1
       FLAGC = 3
       NCARD  = NCARD+1
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) (XSLP(I),SSLP(I),I=1,NSC)
       lngth = xslp(nsc)                
       NCARD  = NCARD+1
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) CTLZ,CTLN,CTLSL
       YBASE1 = 0.0
       CTLSL1   = DSIN(DATAN(CTLSL1))
       IF (METFLG .EQ. 1) THEN
          LNGTH=LNGTH * FeetPerMeter
          DACHL=DACHL * AcresPerHectare
          DACHU=DACHU * AcresPerHectare
          DO 10 I=1,NSC
   10        XSLP(I) = XSLP(I)*FeetPerMeter
       ENDIF
       DACHU   = DACHU*SqrFeetPerAcre
       DACHL   = DACHL*SqrFeetPerAcre
       DAREAIndx = DACHL
       LEFF    = LNGTH/(1.0d0-DACHU/DACHL)
       TOPL    = LEFF-LNGTH
       DELTL   = LEFF/10.0d0
       IDWN    = NSC+1
       DO 20 I=1,NSC
           XSLP(IDWN) = XSLP(IDWN-1)+TOPL
           SSLP(IDWN) = SSLP(IDWN-1)
   20      IDWN        = IDWN-1
       NSC      = NSC+1
       XSLP(1)  = TOPL
       SSLP(1)  = SSLP(2)
       XPCHN(1) = TOPL
       SPCHN(1) = SSLP(1)
       XPCHN(2) = TOPL+DELTL-dMOD(TOPL,DELTL)
       DIFF      =(XPCHN(2)-TOPL)/LEFF
       IF (DABS(DIFF).LT.0.0010d0) XPCHN(2)=XPCHN(2)+DELTL
       XCHECK    = XPCHN(2)+0.1*DELTL
       I = 2
 120   CONTINUE
           IF (XCHECK.GE.LEFF) GO TO 130
           CALL TABLE(2,NSC,XSLP,SSLP,XPCHN(I),SPCHN(I))
           I         = I+1
           XPCHN(I) = XPCHN(I-1)+DELTL
           XCHECK    = XCHECK+DELTL
       GO TO 120
 130   CONTINUE
       XPCHN(I) = LEFF
       SPCHN(I) = SSLP(NSC)
       NPTSC    = I
       goto 300
  290 Call LogMessage('Error while reading card'//I_4(Ncard))     !error message to screen
	Call LogMessage('STOP')                                     !kill run otherwise
  300 return
      end
C======================================================================
      SUBROUTINE ReadImpoundmentParameters(io)
C======================================================================
C      READ AND ECHO THE VALUES FOR THE IMPOUNDMENT THAT WILL REMAIN
C      CONSTANT WITH RESPECT TO TIME
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  !sab 6/2002
      include 'Rz_Gleams.inc'  !sab 6/2002
      character I_4*4
       NCARD  = 12                                                   
       PAC    = 1
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) DAPND,INTAKE,FRONT,DRAW,SIDE,ctl,DIAO,C   
       IF (METFLG .EQ. 1) THEN
          DAPND = DAPND * AcresPerHectare
          INTAKE= INTAKE / CmPerInch
          DIAO  = DIAO / CmPerInch
       ENDIF
       DAPND    = DAPND*SqrFeetPerAcre
       DAREA(4) = DAPND
       DIAO     = DIAO/12.0d0 !now in feet
       IF (CTL.EQ.1) C_ero = 11968.0d0*DIAO**2
       INTAKE = INTAKE/43200.0d0
       IF (PAC.GT.1) GO TO 300
       FS     = (((FRONT+DRAW)/FRONT)**2)/(SIDE*DRAW)
       B_ero      = 2.0d0
       goto 300
  290 Call LogMessage('Error while reading card'//I_4(Ncard))     !error message to screen
	Call LogMessage('STOP')                                     !kill run otherwise
  300 return
      end
C======================================================================
      SUBROUTINE ProcessErosionParameters(DnYear)
C======================================================================
c this subroutine was originally part of EROSA
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  !sab 6/2002
      include 'Rz_Gleams.inc'  !sab 6/2002
      Dimension DnYear(40)
      Integer DnYear
       ELEM=0
C
C      THE INITIAL WIDTH AND DEPTH VALUES FOR EACH SEGMENT OF THE
C      CHANNELS MODELED IN THIS SIMULATION ARE SET UP HERE WITH
C      THE CALLS TO SPREAD.   INDXD IS THE INDEX FOR THE ARRAY OF
C      DATES (CDATE) THAT CONTROLS THE PARAMETERS SELECTION.
C
       LINDXD = 1
       INDXD  = 1
       INDXND = 2
       IF (FLGSEQ.LE.2) GO TO 30
       CALL SPREAD(0,1,INDXD,1,NXC1,XCHN1,DCHN1,NPTSC1,XPCHN1,DEPA)
       CALL SPREAD(0,2,INDXD,1,NXC1,XCHN1,DCHN1,NPTSC1,XPCHN1,DEPB)
       CALL SPREAD(0,1,INDXD,1,NXC1,XCHN1,WCHN1,NPTSC1,XPCHN1,WIDA)
       CALL SPREAD(0,2,INDXD,1,NXC1,XCHN1,WCHN1,NPTSC1,XPCHN1,WIDB)
       IF (FLGSEQ.EQ.3.OR.FLGSEQ.EQ.5) GO TO 30
       CALL SPREAD(0,1,INDXD,2,NXC2,XCHN2,DCHN2,NPTSC2,XPCHN2,DEPA)
       CALL SPREAD(0,2,INDXD,2,NXC2,XCHN2,DCHN2,NPTSC2,XPCHN2,DEPB)
       CALL SPREAD(0,1,INDXD,2,NXC2,XCHN2,WCHN2,NPTSC2,XPCHN2,WIDA)
       CALL SPREAD(0,2,INDXD,2,NXC2,XCHN2,WCHN2,NPTSC2,XPCHN2,WIDB)
  30   CONTINUE
       if (flgseq .eq. 1) then
          indjoe = 1
       else if (flgseq.eq.2 .or. flgseq.eq.5 .or. flgseq.eq.6) then
          indjoe = 4
       else if (flgseq .eq. 3) then
          indjoe = 2
       else
          indjoe = 3
       endif
      return
      end
C======================================================================
       SUBROUTINE SPREAD(INIT,FLAG,ND,NC,NIN,XIN,YIN,NOUT,XOUT,YOUT)
C======================================================================
C======================================================================
      double precision XIN(4),YIN(4,40),XOUT(40),YOUT(2,40),Y
      INTEGER FLAG,INIT,ND,NC,NIN,NOUT,IIN
       save
       IIN = 1
       Y   = YIN(IIN,ND)
       IF (INIT.EQ.0) Y = dABS(Y)
       DO 20 IOUT=1,NOUT
           IF (IIN.EQ.NIN) GO TO 10
           IF (XOUT(IOUT).LT.XIN(IIN)) GO TO 10
           IF (FLAG.EQ.1.AND.Y.GT.0) YOUT(NC,IOUT) = Y
           IIN = IIN+1
           Y   = YIN(IIN,ND)
           IF (INIT.EQ.0) Y = dABS(Y)
           IF (FLAG.EQ.2.AND.Y.GT.0) YOUT(NC,IOUT) = Y
           GO TO 20
  10       CONTINUE
           IF (Y.GT.0) YOUT(NC,IOUT) = Y
  20   CONTINUE
       RETURN
       END
C======================================================================
      SUBROUTINE Initialize
C======================================================================
C      Set SOIL COMPOSITION VALUES
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      NBAROV = 0.010d0
      NBARCH = 0.030d0
      WTDSOI = (1.0d0-POR)*2.65d0*62.4d0   
      KCH    = 0.135d0
      YALCON = 0.635d0
      WTDH2O = 62.4d0
      MSDH2O = 1.94d0
      AGRAV  = 32.2d0
      Beta_ero   = 1.56d0
      solcly = clay    !these are fractions in RZ -- thereofre no need to /100
      solslt = silt 
      solsnd = 1.00d0 - solcly - solslt 
      ssorg  = 2000.0d0         !specific surface area assumptions m^2/gm
      SSSLT  = 4.0d0            !pg 73 - Gleams v.3.0 2000 
      SSSND  = 0.05d0
      Call Set_SSSoil
      return
      end
C======================================================================
      SUBROUTINE Set_SSSoil
C======================================================================
C this routine sets SSSoil - which changes as organic fraction changes
C RZ:Fracom1 -> OM1 -> SolOrg & SSSoil = f(SolOrg).
C SSSoil is used in the enrichment calculation
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      save
      TOTAL  = SOLORG+1.0d0
      SOLORG = OM1
      SSSOIL = SSORG*SOLORG/1.73d0+(SSCLY*SOLCLY+SSSLT*SOLSLT+SSSND*
     1          SOLSND)/TOTAL
      return
      end
C======================================================================
      SUBROUTINE ProcessComposition  !was PRTCMP
C======================================================================
C   FOSTER & NEIBLING PARTICLE SIZES FOR LARGE AND SMALL AGGREGATES.
c see page 42 of Foster, G. R. Chapter 3 of the CREAMS documentation
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      save
       DIA(1)   = 0.002d0
       DIA(2)   = 0.010d0
       DIA(4)   = 0.300d0
       IF (SOLCLY .GT. 0.15d0) DIA(4) = 2.0d0*SOLCLY
       DIA(5)   = 0.200d0
       SPGR(1)   = 2.60d0
       SPGR(2)   = 2.65d0
       SPGR(3)   = 1.80d0
       SPGR(4)   = 1.60d0
       SPGR(5)   = 2.65d0
       FRAC(1)  = 0.26d0*SOLCLY
       FRAC(5)  = SOLSND*(1.0d0-SOLCLY)**5.0d0
       IF(SOLCLY.GT.0.25d0) GO TO 10
         DIA(3)   = 0.030d0
         FRAC(3)  = 1.8d0*SOLCLY
         GO TO 40
  10   CONTINUE
       IF(SOLCLY.GE.0.60d0) GO TO 20
         DIA(3)   = 0.20d0*(SOLCLY-0.25d0)+0.030d0
       IF(SOLCLY.GE.0.50d0) GO TO 30
         FRAC(3)  = 0.45d0-0.6d0*(SOLCLY - 0.25d0)
       GO TO 40
  20   CONTINUE
       DIA(3)   = 0.1d0
  30   CONTINUE
       FRAC(3)  = 0.60d0 * SOLCLY
  40   CONTINUE
       FRAC(2)  = SOLSLT - FRAC(3)
        IF (FRAC(2) .GT. 0.0d0 ) GO TO 45
        FRAC(2) = 0.0001d0
        FRAC(3) = SOLSLT - FRAC(2)
  45   CONTINUE
       FRAC(4)  = 1.0d0-FRAC(1)-FRAC(2)-FRAC(3)-FRAC(5)
       IF(FRAC(4).GT.0.0d0) GO TO 60
        CRCT     = 1.0d0/(1.0d0+DABS(FRAC(4))+0.0001d0)
        FRAC(4) = 0.0001d0
         DO 50 K=1,NPART
  50       FRAC(K) = FRAC(K)*CRCT
  60   CONTINUE
       RATIOM   = SOLORG/SOLCLY
       FRCLY(1) = 1.0d0
       FRSLT(1) = 0.0d0
       FRSND(1) = 0.0d0
       FRORG(1) = FRCLY(1)*RATIOM
       FRCLY(2) = 0.0d0
       FRSLT(2) = 1.0d0
       FRSND(2) = 0.0d0
       FRORG(2) = FRCLY(2)*RATIOM
       FRCLY(3) = SOLCLY/(SOLCLY+SOLSLT)
       FRSLT(3) = SOLSLT/(SOLCLY+SOLSLT)
       FRSND(3) = 0.0d0
       FRORG(3) = FRCLY(3)*RATIOM
       FRCLY(4) = (SOLCLY-FRAC(1)-(FRCLY(3)*FRAC(3)))/FRAC(4)
       FRSLT(4) = (SOLSLT-FRAC(2)-(FRSLT(3)*FRAC(3)))/FRAC(4)
       FRSND(4) = (SOLSND-FRAC(5))/FRAC(4)
       FRORG(4) = FRCLY(4)*RATIOM
       FRCLY(5) = 0.0d0
       FRSLT(5) = 0.0d0
       FRSND(5) = 1.0d0
       FRORG(5) = FRCLY(5)*RATIOM
       FRCLYT   = 0.5d0*SOLCLY
       FRCLY1   = 0.95d0*FRCLYT
       IF(FRCLY(4).GE.FRCLY1.OR.FRCLY1.LE.0.0d0) RETURN
       F1F2F5   = FRAC(1)+FRAC(2)+FRAC(5)
       FRCLY(4) = FRCLYT
       FRAC(3)  = (SOLCLY-FRCLY(4)-FRAC(1)+FRCLY(4)*F1F2F5)/
     1            (FRCLY(3)-FRCLY(4))
       GO TO 40
       END
C
C======================================================================
      SUBROUTINE PROFIL(XTEMP,SLTEMP,AVGSLP,FLAGO,NSEG)
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
C
      double precision XTEMP(40),SLTEMP(40)
      INTEGER FLAGO(10)
      character I_4*4,G12_5*12
      Data Apprx0/1.0E-20/
      save
C
      NEWPTS=1
      NSEG=1
      ILAST=1
      INOW=ILAST
C
      IF((NPTSO.GT.10).OR.(NPTSO.LE.0))THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('RUN STOPPED : INPUT ERROR. THE NUMBER OF '
     &                   //'POINTS READ IN MUST BE > 0 OR <= 10')
         PRINT *, "CALLING Logmessage"
         Call Logmessage('  NPTSO='//I_4(NPtso))
         PRINT *, "CALLING LogMessage"
         Call LogMessage('STOP')
      else  IF(XTEMP(NPTSO).EQ.0.0)THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('RUN STOPPED : INPUT ERROR. EITHER THE '//
     &       'LAST DISTANCE POINT WAS READ AS 0 OR THE NUMBER OF')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DISTANCE AND SLOPE POINTS WAS LESS THAN '//
     &        'NPTSO OR ONLY 1 POINT WAS READ & THE DISTANCE WAS ZERO')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('STOP')
      ENDIF
C
      np = 0
      IF(NPTSO.EQ.1)THEN
         XPOVR(1)=0.0
         SPOVR(1)=SLTEMP(1)
         XPOVR(2)=XTEMP(1)
         SPOVR(2)=SPOVR(1)
         AVGSLP=SPOVR(2)
         SLNGTH=XPOVR(2)
         FLAGO(1)=0
         NPTSO=2
         np = 1
      ENDIF
C
      IF (DABS(XTEMP(1)+XTEMP(2)).LE.Apprx0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('RUN STOPPED : INPUT ERROR. 1st 2 TWO DISTANCE'
     &       //' POINTS CAN NOT BE 0; Xtemp(1)=Xtemp(2)')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('STOP')
      else IF(DABS(XTEMP(NPTSO-1)-XTEMP(NPTSO)).LE.Apprx0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('RUN STOPPED : INPUT ERROR.LAST 2 DISTANCE '//
     &       'POINTS CAN NOT BE EQUAL:')
         PRINT *, "CALLING LogMessage"
         Call LogMessage(' XTEMP(NPTSO-1)='//G12_5(Xtemp(NPTSO-1))//
     &       '& XTEMP(NPTSO)='//G12_5(Xtemp(NPTSO)))
         PRINT *, "CALLING LogMessage"
         Call LogMessage('STOP')
      ENDIF
C
      if (np .eq. 1) return
C
      SLNGTH=XTEMP(NPTSO)
      AVGSLP=0.0
C
      IF(NPTSO.EQ.2)THEN
         IF(XTEMP(1).NE.0.0)GO TO 30
            IF(SLTEMP(1).EQ.SLTEMP(2))THEN
               XPOVR(1)=0.0
               SPOVR(1)=SLTEMP(1)
               XPOVR(2)=SLNGTH
               SPOVR(2)=SPOVR(1)
               AVGSLP=SPOVR(2)
               FLAGO(1)=0
               RETURN
            ELSE
               XTEMP(1)=0.0
               DLENG=SLNGTH/5.0
               DO 10 J=1,6
                  XPOVR(J)=dFLOAT(J-1)*DLENG
                  CALL TABLE(2,2,XTEMP,SLTEMP,XPOVR(J),SLOPE)
                  SPOVR(J)=SLOPE
 10            CONTINUE
               DO 20 J=2,6
                  DELX=XPOVR(J)-XPOVR(J-1)
                  DELS=(SPOVR(J)+SPOVR(J-1))/2.0
                  AVGSLP=AVGSLP+((DELX*DELS)/SLNGTH)
 20            CONTINUE
               IF(SLTEMP(2).GT.SLTEMP(1))THEN
                  FLAGO(1)=1
               ELSE
                  FLAGO(1)=-1
               ENDIF
               NPTSO=6
               RETURN
            ENDIF
      ENDIF
C
 30   IF(XTEMP(1).NE.0.0)THEN
         X1_ero(1)=0.0
         S1_ero(1)=SLTEMP(1)
         NPTSO=NPTSO+1
         DO 40 I=2,NPTSO
            X1_ero(I)=XTEMP(I-1)
            S1_ero(I)=SLTEMP(I-1)
 40      CONTINUE
      ELSE
         DO 50 I=1,NPTSO
            X1_ero(I)=XTEMP(I)
            S1_ero(I)=SLTEMP(I)
 50      CONTINUE
      ENDIF
C
      X2_ero(1)=X1_ero(1)
      S2_ero(1)=S1_ero(1)
C
      IF(S1_ero(2) .GT. S1_ero(1)) IFLAG=1
      IF(S1_ero(2) .EQ. S1_ero(1)) IFLAG=0
      IF(S1_ero(2) .LT. S1_ero(1)) IFLAG=-1
C
      FLAGO(NSEG)=IFLAG
      NSEG=NSEG+1
C
      DO 60 I=2,NPTSO
C
         IF((X1_ero(I).LT.X1_ero(I-1)).OR.(S1_ero(I).LT.0.0))THEN
           PRINT *, "CALLING LogMessage"
           Call LogMessage('RUN STOPPED : INPUT ERROR. DISTANCE PTS '
     &     //'MUST ALWAYS INCREASE - SLOPE CAN NOT BE NEGATIVE')
           PRINT *, "CALLING LogMessage"
           Call LogMessage(' XTEMP ='//G12_5(X1_ero(I))//
     &                   ' SLTEMP ='//G12_5(S1_ero(i)))
           PRINT *, "CALLING LogMessage"
           Call LogMessage('STOP')
         ENDIF
C
         IF(S1_ero(I).GT.S1_ero(I-1))THEN
            IF(X1_ero(I).EQ.X1_ero(I-1))THEN
               INOW=I-1
               CALL NEWPT1(I,IFLAG,ILAST,INOW,NEWPTS)
               IFLAG=1
               FLAGO(NSEG)=2
               NSEG=NSEG+1
            ELSE
               IF(IFLAG.NE.1)THEN
                  IF(IFLAG.EQ.0)THEN
                     INOW=I-1
                     IFLAG=1
                     FLAGO(NSEG)=IFLAG
                     NSEG=NSEG+1
                     NEWPTS=NEWPTS+1
                     X2_ero(NEWPTS)=X1_ero(INOW)
                     S2_ero(NEWPTS)=S1_ero(INOW)
                     ILAST=INOW
                  ELSE
                     INOW=I-1
                     IFLAG=1
                     FLAGO(NSEG)=IFLAG
                     NSEG=NSEG+1
                     IF(INOW.NE.ILAST)
     1               CALL NEWPT2(ILAST,INOW,NEWPTS)
                  ENDIF
               ENDIF
            ENDIF
         ELSE IF(S1_ero(I).EQ.S1_ero(I-1))THEN
            IF(IFLAG.NE.0)THEN
               INOW=I-1
               IFLAG=0
               FLAGO(NSEG)=IFLAG
               NSEG=NSEG+1
               IF(INOW.NE.ILAST)
     1         CALL NEWPT2(ILAST,INOW,NEWPTS)
            ENDIF
         ELSE IF(S1_ero(I).LT.S1_ero(I-1))THEN
            IF(X1_ero(I).EQ.X1_ero(I-1))THEN
               INOW=I-1
               CALL NEWPT1(I,IFLAG,ILAST,INOW,NEWPTS)
               IFLAG=-1
               FLAGO(NSEG)=-2
               NSEG=NSEG+1
            ELSE
               IF(IFLAG.NE.-1)THEN
                  IF(IFLAG.EQ.0)THEN
                     INOW=I-1
                     IFLAG=-1
                     FLAGO(NSEG)=IFLAG
                     NSEG=NSEG+1
                     NEWPTS=NEWPTS+1
                     X2_ero(NEWPTS)=X1_ero(INOW)
                     S2_ero(NEWPTS)=S1_ero(INOW)
                     ILAST=INOW
                  ELSE
                     INOW=I-1
                     IFLAG=-1
                     FLAGO(NSEG)=IFLAG
                     NSEG=NSEG+1
                     IF(INOW.NE.ILAST)
     1               CALL NEWPT2(ILAST,INOW,NEWPTS)
                  ENDIF
               ENDIF
            ENDIF
         ELSE
         ENDIF
C
         IF(I.EQ.NPTSO)THEN
            INOW=NPTSO
            NSEG=NSEG-1
            IF(S1_ero(I).EQ.S1_ero(I-1))THEN
               NEWPTS=NEWPTS+1
               X2_ero(NEWPTS)=X1_ero(INOW)
               S2_ero(NEWPTS)=S1_ero(INOW)
            ELSE
               CALL NEWPT2(ILAST,INOW,NEWPTS)
            ENDIF
         ENDIF
C
         IF(NEWPTS.GT.26)THEN
            PRINT *, "CALLING LogMessage"
            Call LogMessage('RUN STOPPED : INPUT ERROR. THERE ARE TOO '
     &         //'MANY CURVED SLOPED SEGMENTS. MAXIMUM NUMBER OF ')
            PRINT *, "CALLING LogMessage"
            Call LogMessage('SEGMENTS ALLOWED : ')
            PRINT *, "CALLING LogMessage"
            Call LogMessage('1) TWO CONCAVE AND TWO CONVEX WITH EACH '
     &         //'BEING SEPARATED BY A UNIFORM SEGMENT; OR')
            PRINT *, "CALLING LogMessage"
            Call LogMessage('2) TWO CONCAVE AND THREE CONVEX OR THREE'
     &         //' CONCAVE AND TWO CONVEX IN DIFFERENT')
            PRINT *, "CALLING LogMessage"
            Call LogMessage('   COMBINATIONS (FIVE CURVED - NO UNIFORM)'
     &         //' THE SUBROUTINE ADDS POINTS TO CURVED')
            PRINT *, "CALLING LogMessage"
            Call LogMessage('SEGMENTS - THE TOTAL NUMBER OF POINTS'
     &         //' SHOULD BE LESS THAN OR EQUAL TO 26')
            PRINT *, "CALLING LogMessage"
            Call LogMessage('TOTAL NUMBER OF POINTS RETURNED BEFORE '
     &        //'EXITING MAIN LOOP ='//I_4(NewPts))
            PRINT *, "CALLING LogMessage"
            Call LogMessage('STOP')
          ENDIF
C
 60   CONTINUE
C
      NPTSO=NEWPTS
C
      DO 70 J=1,NPTSO
         XPOVR(J)=X2_ero(J)
         SPOVR(J)=S2_ero(J)
 70   CONTINUE
C
      DO 80 J=2,NPTSO
         DELX=XPOVR(J)-XPOVR(J-1)
         DELS=(SPOVR(J)+SPOVR(J-1))/2.0
         AVGSLP=AVGSLP+((DELX*DELS)/SLNGTH)
 80   CONTINUE
C
      RETURN
      END
C======================================================================
       SUBROUTINE ADDPTS(NIN,XIN,NOUT,XOUT,YOUT)
C
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
       double precision INTRPL,XIN(4),XOUT(40),YOUT(40),dif
       integer NIN
       save
      if (nin .le. 0) return
       IIN  = 1
       IOUT = 1
  10   CONTINUE
           DIF  = (XOUT(IOUT)-XIN(IIN)) /XIN(NIN)
           IF (dABS(DIF).LT.0.001)GO TO 30
           IF (XOUT(IOUT).LT.XIN(IIN)) GO TO 40
           NOUT = NOUT+1
           IDWN = NOUT
  20       CONTINUE
               XOUT(IDWN) = XOUT(IDWN-1)
               YOUT(IDWN) = YOUT(IDWN-1)
               IDWN       = IDWN-1
               IF (IDWN.GT.IOUT) GO TO 20
           YOUT(IOUT) = INTRPL(XOUT(IOUT-1),YOUT(IOUT-1),XOUT(IOUT+1),
     1                  YOUT(IOUT+1),XIN(IIN))
  30       CONTINUE
           XOUT(IOUT)=XIN(IIN)
           IIN  = IIN+1
           IF (IIN.GT.NIN) RETURN
  40       CONTINUE
           IOUT = IOUT+1
           IF (IOUT.LE.NOUT) GO TO 10
       RETURN
       END
C======================================================================
       Double Precision FUNCTION INTRPL(X1,Y1,X2,Y2,X)
C 
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INTRPL = ((Y2-Y1)*(X-X1)/(X2-X1))+Y1
       RETURN
       END
C======================================================================
       SUBROUTINE TABLE(FLAG,LENGTH,COLMN1,COLMN2,GIVEN,FOUND)
C 
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision INTRPL,COLMN1(LENGTH),COLMN2(LENGTH)
      INTEGER FLAG
      character G12_5*12,I_4*4
       save
       IF (FLAG.EQ.2) GO TO 20
       IF (FLAG.EQ.3) GO TO 40
       IF (FLAG.EQ.4) GO TO 60
       DO 10 I=1,LENGTH
           NPOS = I
           IF (COLMN1(I).LT.GIVEN) GO TO 80
           if ((i.eq.length) .and. (colmn1(i) .eq. given)) go to 80
  10   CONTINUE
       GO TO 100
  20   CONTINUE
       DO 30 I=1,LENGTH
           NPOS = I
           IF (COLMN1(I).GT.GIVEN) GO TO 80
           if ((i.eq.length).and.(colmn1(i).eq.given))go to 80
  30   CONTINUE
       GO TO 100
  40   CONTINUE
       DO 50 I=1,LENGTH
           NPOS = I
           IF (COLMN2(I).LT.GIVEN) GO TO 90
           if ((i.eq.length).and.(colmn2(i).eq.given))go to 90
  50   CONTINUE
       GO TO 100
  60   CONTINUE
       DO 70 I=1,LENGTH
           NPOS = I
           IF (COLMN2(I).GT.GIVEN) GO TO 90
           if ((i.eq.length).and.(colmn2(i).eq.given))go to 90
  70   CONTINUE
       GO TO 100
  80   CONTINUE
       IF (NPOS.EQ.1) GO TO 100
       FOUND = INTRPL(COLMN1(NPOS-1),COLMN2(NPOS-1),COLMN1(NPOS),
     1                COLMN2(NPOS),GIVEN)
       RETURN
  90   CONTINUE
       IF (NPOS.EQ.1) GO TO 100
       FOUND = INTRPL(COLMN2(NPOS-1),COLMN1(NPOS-1),COLMN2(NPOS),
     1                COLMN1(NPOS),GIVEN)
       RETURN
 100   CONTINUE
      PRINT *, "CALLING LogMessage"
      Call LogMessage('Routine Table:GIVEN IS OUTSIDE THE RANGE OF '
     &   //'THE TABLE.USE THIS INFO TO IDENTIFY THE FUNCTION WHERE')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('THE PROBLEM OCCURED')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('FLAG: 1 - GIVEN COLUMN 1 FIND COLUMN 2 '//
     &    '(COLUMN 1 DECREASES)')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('      2 - GIVEN COLUMN 1 FIND COLUMN 2 '//
     &    '(COLUMN 1 INCREASES)')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('      3 - GIVEN COLUMN 2 FIND COLUMN 1 '//
     &    '(COLUMN 2 DECREASES)')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('      4 - GIVEN COLUMN 2 FIND COLUMN 1 '//
     &    '(COLUMN 2 INCREASES)')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('THE FLAG = '//I_4(Flag)//' GIVEN VALUE = '//
     &    G12_5(Given))
      PRINT *, "CALLING Logmessage"
      Call Logmessage('THIRD VALUE FROM COLUMN 1 = '//G12_5(Colmn1(3))
     &   //' THIRD VALUE FROM COLUMN 2 = '//G12_5(Colmn2(3)))
      PRINT *, "CALLING LogMessage"
      Call LogMessage('STOP')
      END
C======================================================================
       SUBROUTINE UPDPAR(IO,FLGINI,DNYEAR,METFLGx)
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
       INTEGER FLGINI,DNYEAR(40),DATBEG,DATEND
       CHARACTER SPACE*4,STARS*4,MARKER(20)*4,Astr*255,I_4*4
       save
      Data  SPACE/'    '/, STARS/'****'/
       IF (FLGINI.GT.0) Call LogMessage(' ')
C
C      READ AND ECHO THE NUMBER OF YEARS IN A CROP ROTATION
C
       NCARD  = 13
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NYEARS
       IF (NYEARS.LE.0) RETURN
       NDATES = 0
       DO 20 NYEAR=1,NYEARS
           DNYEAR(NYEAR) = 0
           DATBEG        = NDATES+1
           DATEND        = DATBEG+9
           IF (DATEND.GT.40) DATEND = 40
C
C          READ AND ECHO THE DATES FOR ONE YEAR
C
           NCARD  = 15
           PRINT *, "CALLING EchoLine"
           Call EchoLine(Io)
           READ (io,2000,Err=290) (Cdate_ero(IDATE),IDATE=DATBEG,DATEND)
2000       format (10i8)
           DO 10 IDATE=DATBEG,DATEND
               IF (Cdate_ero(IDATE).LE.0) GO TO 15
               Cdate_ero(IDATE)  = Cdate_ero(IDATE)+1000*NYEAR
               NDATES        = NDATES+1
               DNYEAR(NYEAR) = DNYEAR(NYEAR)+1
  10       CONTINUE
  15     Continue
  20   CONTINUE
       Cdate_ero(NDATES+1) = 400+1000*NYEARS
       IF (FLGINI.GT.0) GO TO 50
C
C      READ AND ECHO THE DISTANCE VALUES FOR OVERLAND FLOW PARAMETERS
C      THAT WILL CHANGE WITH RESPECT TO TIME
C
       NCARD  = 16
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NXF,(XFACT(I),I=1,NXF)
C
C      ADJUST THE DIMENSIONLESS X VALUES WITH THE OVERLAND SLOPE LENGTH,
C      SET UP THE X ARRAY FOR EACH POINT ALONG THE OVERLAND PROFILE AND
C      THEN ADJUST THE SLOPE AT EACH POINT
C
       DO 30 I=1,NXF
           XFACT(I) = SLNGTH*XFACT(I)
  30   CONTINUE
       CALL ADDPTS(NXF,XFACT,NPTSO,XPOVR,SPOVR)
       DO 40 I=1,NPTSO
           SPOVR(I) = DSIN(DATAN(SPOVR(I)))
  40   CONTINUE
  50   CONTINUE
C
C      READ AND ECHO THE OVERLAND FLOW PARAMETERS THAT WILL
C      CHANGE WITH RESPECT TO TIME
C
       DATBEG = 1
       DO 70 NYEAR=1,NYEARS
           DATEND = DATBEG + DNYEAR(NYEAR)-1
           DO 60 IXF=1,NXF
               NCARD  = 17
               PRINT *, "CALLING EchoLine"
               Call EchoLine(Io)
               READ (IO,*,Err=290)(CFACT(IXF,IDATE),IDATE=DATBEG,DATEND)
               NCARD  = NCARD+1
               PRINT *, "CALLING EchoLine"
               Call EchoLine(Io)
               READ (IO,*,Err=290)(Pfact_ero(IXF,IDATE),IDATE=
     &                                     DATBEG,DATEND)
               NCARD  = NCARD+1
               PRINT *, "CALLING EchoLine"
               Call EchoLine(Io)
               READ (IO,*,Err=290)(NFACT(IXF,IDATE),IDATE=DATBEG,DATEND)
  60       CONTINUE
           DATBEG = DATBEG+DNYEAR(NYEAR)
  70   CONTINUE
       IF (FLGSEQ.LE.2) RETURN
       IF (FLGINI.GT.0) GO TO 100
C
C      READ AND ECHO THE DISTANCE VALUES FOR CHANNEL ONE PARAMETERS
C      THAT WILL CHANGE WITH RESPECT TO TIME
C
       NCARD  = 20
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NXC1,(XCHN1(I),I=1,NXC1)
C
C      ADJUST THE DIMENSIONLESS X VALUES WITH THE EFFECTIVE CHANNEL
C      LENGTH, SET UP THE X ARRAY FOR EACH POINT ALONG THE CHANNEL AND
C      THEN ADJUST THE SLOPE AT EACH POINT
C
       DO 80 I=1,NXC1
           XCHN1(I) = LNGTH1*XCHN1(I)+LEFF1-LNGTH1
  80   CONTINUE
       CALL ADDPTS(NXC1,XCHN1,NPTSC1,XPCHN1,SPCHN1)
       DO 90 I=1,NPTSC1
           SPCHN1(I) = DSIN(DATAN(SPCHN1(I)))
  90   CONTINUE
 100   CONTINUE
C
C      READ AND ECHO THE CHANNEL ONE PARAMETERS THAT WILL
C      CHANGE WITH RESPECT TO TIME
C
       DATBEG = 1
       NDAY   = 0
       DO 150 NYEAR=1,NYEARS
           DATEND = DATBEG+DNYEAR(NYEAR)-1
           DO 140 IXC1=1,NXC1
              NCARD  = 21
              PRINT *, "CALLING EchoLine"
              Call EchoLine(Io)
              READ (IO,*,Err=290)(NCHN1(IXC1,IDATE),IDATE=DATBEG,DATEND)
               IF (FLAGC1.LT.3) GO TO 130
               NBAD = 0
               DO 120 IDATE=DATBEG,DATEND
                   MARK = 2*(IDATE-DATBEG+1)
                   IF (NCHN1(IXC1,IDATE).GE.NBARCH) GO TO 110
                   NBAD = NBAD+1
                   NCHN1(IXC1,IDATE) = NBARCH
                   MARKER(MARK-1)    = STARS
                   MARKER(MARK)      = STARS
                   GO TO 120
 110               CONTINUE
                   MARKER(MARK-1)    = SPACE
                   MARKER(MARK)      = SPACE
 120           CONTINUE
               IF (NBAD.EQ.0) GO TO 130
               WRITE (Astr,'(20A4)') (MARKER(I),I=1,MARK)
               PRINT *, "CALLING LogMessage"
               Call LogMessage(Astr)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('NATURALLY ERODED CHANNEL - THE '//
     &             'MANNINGS N MUST EQUAL THAT FOR BARE SOIL')
 130           CONTINUE
               NCARD  = NCARD+1
              PRINT *, "CALLING EchoLine"
              Call EchoLine(Io)
              READ (IO,*,Err=290)(DCHN1(IXC1,IDATE),IDATE=DATBEG,DATEND)
               NCARD  = NCARD+1
              PRINT *, "CALLING EchoLine"
              Call EchoLine(Io)
              READ (IO,*,Err=290)(WCHN1(IXC1,IDATE),IDATE=DATBEG,DATEND)
                DO 132 IDATE=DATBEG,DATEND
                   IF (IDATE .EQ. 1) THEN
                      IF (DCHN1(IXC1,1) .LT. 0.) THEN
                         CCHN1(IXC1,1) = 0.20d0
                         NDAY = 30
                      ELSE
                         CCHN1(IXC1,1) = 0.05d0
                         NDAY = 0
                      ENDIF
                   ELSE
                      IF (DCHN1(IXC1,IDATE) .GT. 0.0d0) THEN
                         CCHN1(IXC1,IDATE) = 0.05d0
                         NDAY = 0
                      ELSE
                         NDAY = NDAY + (Cdate_ero(IDATE) - 
     &                       Cdate_ero(IDATE-1))
                         IF (NDAY .GE. 150) THEN
                            CCHN1(IXC1,IDATE) = 0.60d0
                         ELSE
                            CCHN1(IXC1,IDATE) = 0.10d0 + 0.003333d0*NDAY
                         ENDIF
                      ENDIF
                   ENDIF
 132            CONTINUE
                DO 135 JDATE=DATBEG,DATEND
                       SCHN1(IXC1,JDATE) = DCHN1(IXC1,JDATE)
                       IF (JDATE .EQ. 1) THEN
                          SCHN1(IXC1,JDATE)=ABS(SCHN1(IXC1,JDATE))
                       ELSE
                          NK=JDATE-1
                       ENDIF
        IF (NK .GE. DATBEG) THEN
           IF (SCHN1(IXC1,JDATE) .LT. 0.) THEN
              SCHN1(IXC1,JDATE)=SCHN1(IXC1,NK)
           ENDIF
        ELSE
           IF (SCHN1(IXC1,JDATE) .LT. 0.) THEN
              IF (METFLG .EQ. 1) THEN
              SCHN1(IXC1,JDATE) = SCHN1(IXC1,NK)/FeetPerMeter
              ELSE
              SCHN1(IXC1,JDATE) = SCHN1(IXC1,NK)
              ENDIF
           ENDIF
        ENDIF
 135            CONTINUE
                IF (METFLG .EQ. 1) THEN
                   DO 138 MDATE=DATBEG,DATEND
                      SCHN1(IXC1,MDATE)=SCHN1(IXC1,MDATE)*FeetPerMeter
                      DCHN1(IXC1,MDATE)=DCHN1(IXC1,MDATE)*FeetPerMeter
                      WCHN1(IXC1,MDATE)=WCHN1(IXC1,MDATE)*FeetPerMeter
 138               CONTINUE
                ENDIF
 140       CONTINUE
           DATBEG = DATBEG+DNYEAR(NYEAR)
 150   CONTINUE
       IF (FLGSEQ.EQ.3.OR.FLGSEQ.EQ.5) RETURN
       IF (FLGINI.GT.0) GO TO 180
C
C      READ AND ECHO THE DISTANCE VALUES FOR CHANNEL TWO PARAMETERS
C      THAT WILL CHANGE WITH RESPECT TO TIME
C
       NCARD  = 20
       PRINT *, "CALLING EchoLine"
       Call EchoLine(Io)
       READ (IO,*,Err=290) NXC2,(XCHN2(I),I=1,NXC2)
C
C      ADJUST THE DIMENSIONLESS X VALUES WITH THE EFFECTIVE CHANNEL
C      LENGTH, SET UP THE X ARRAY FOR EACH POINT ALONG THE CHANNEL AND
C      THEN ADJUST THE SLOPE AT EACH POINT
C
       DO 160 I=1,NXC2
           XCHN2(I) = LNGTH2*XCHN2(I)+LEFF2-LNGTH2
 160   CONTINUE
       CALL ADDPTS(NXC2,XCHN2,NPTSC2,XPCHN2,SPCHN2)
       DO 170 I=1,NPTSC2
           SPCHN2(I) = SIN(ATAN(SPCHN2(I)))
 170   CONTINUE
 180   CONTINUE
C
C      READ AND ECHO THE CHANNEL TWO PARAMETERS THAT WILL
C      CHANGE WITH RESPECT TO TIME
C
       DATBEG = 1
       DO 230 NYEAR=1,NYEARS
           DATEND = DATBEG+DNYEAR(NYEAR)-1
           DO 220 IXC2=1,NXC2
               NCARD  = 21
              PRINT *, "CALLING EchoLine"
              Call EchoLine(Io)
              READ (IO,*,Err=290)(NCHN2(IXC2,IDATE),IDATE=DATBEG,DATEND)
               IF (FLAGC2.LT.3) GO TO 210
               NBAD = 0
               DO 200 IDATE=DATBEG,DATEND
                   MARK = 2*(IDATE-DATBEG+1)
                   IF (NCHN2(IXC2,IDATE).GE.NBARCH) GO TO 190
                   NBAD = NBAD+1
                   NCHN2(IXC2,IDATE) = NBARCH
                   MARKER(MARK-1)    = STARS
                   MARKER(MARK)      = STARS
                   GO TO 200
 190               CONTINUE
                   MARKER(MARK-1)    = SPACE
                   MARKER(MARK)      = SPACE
 200           CONTINUE
               IF (NBAD.EQ.0) GO TO 210
               WRITE (Astr,'(20A4)') (MARKER(I),I=1,MARK)
               PRINT *, "CALLING LogMessage"
               Call LogMessage(Astr)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('NATURALLY ERODED CHANNEL - THE '//
     &             'MANNINGS N MUST EQUAL THAT FOR BARE SOIL')
 210           CONTINUE
               NCARD  = NCARD+1
              PRINT *, "CALLING EchoLine"
              Call EchoLine(Io)
              READ (IO,*,Err=290)(DCHN2(IXC2,IDATE),IDATE=DATBEG,DATEND)
              NCARD  = NCARD+1
              PRINT *, "CALLING EchoLine"
              Call EchoLine(Io)
              READ (IO,*,Err=290)(WCHN2(IXC2,IDATE),IDATE=DATBEG,DATEND)
               DO 215 IDATE=DATBEG,DATEND
                   SCHN2(IXC2,IDATE)=DCHN2(IXC2,IDATE)
                   IF (IDATE .EQ. 1) THEN
                      SCHN2(IXC2,IDATE)=DABS(SCHN2(IXC2,IDATE))
                   ENDIF
                  IF (IDATE .GT. 1) NK=IDATE-1
                  IF (SCHN2(IXC2,IDATE) .LT. 0.0d0)
     &                SCHN2(IXC2,IDATE)=SCHN2(IXC2,NK)
 215            CONTINUE
                DO 218 IDATE=DATBEG,DATEND
                   IF (IDATE .EQ. 1) THEN
                      IF (DCHN2(IXC2,1) .LT. 0.) THEN
                         CCHN2(IXC2,1) = 0.20d0
                         NDAY = 30
                      ELSE
                         CCHN2(IXC2,1) = 0.05d0
                         NDAY = 0
                      ENDIF
                   ELSE
                      IF (DCHN2(IXC2,IDATE) .GT. 0.0) THEN
                         CCHN2(IXC2,IDATE) = 0.05d0
                         NDAY = 0
                      ELSE
                         NDAY = NDAY + (Cdate_ero(IDATE) - 
     &                        Cdate_ero(IDATE-1))
                         IF (NDAY .GE. 150) THEN
                            CCHN2(IXC2,IDATE) = 0.60d0
                         ELSE
                            CCHN2(IXC2,IDATE) = 0.10d0 + 0.003333d0*NDAY
                         ENDIF
                      ENDIF
                   ENDIF
 218            CONTINUE
                DO 217 JDATE=DATBEG,DATEND
                       SCHN2(IXC2,JDATE) = DCHN2(IXC2,JDATE)
                       IF (JDATE .EQ. 1) THEN
                          SCHN2(IXC2,JDATE)=DABS(SCHN2(IXC2,JDATE))
                       ELSE
                          NK=JDATE-1
                       ENDIF
        IF (NK .GE. DATBEG) THEN
           IF (SCHN2(IXC2,JDATE) .LT. 0.) THEN
              SCHN2(IXC2,JDATE)=SCHN2(IXC2,NK)
           ENDIF
        ELSE
           IF (SCHN2(IXC2,JDATE) .LT. 0.) THEN
              IF (METFLG .EQ. 1) THEN
              SCHN2(IXC2,JDATE) = SCHN2(IXC2,NK)/FeetPerMeter
              ELSE
              SCHN2(IXC2,JDATE) = SCHN2(IXC2,NK)
              ENDIF
           ENDIF
        ENDIF
 217            CONTINUE
                IF (METFLG .EQ. 1) THEN
                   DO 219 MDATE=DATBEG,DATEND
                      SCHN2(IXC2,MDATE)=SCHN2(IXC2,MDATE)*FeetPerMeter
                      DCHN2(IXC2,MDATE)=DCHN2(IXC2,MDATE)*FeetPerMeter
                      WCHN2(IXC2,MDATE)=WCHN2(IXC2,MDATE)*FeetPerMeter
 219               CONTINUE
                ENDIF
 220       CONTINUE
           DATBEG = DATBEG+DNYEAR(NYEAR)
 230   CONTINUE
      Goto 300                                                    !if here, all is ok, so hop and exit
  290 Call LogMessage('Error while reading card '//I_4(Ncard))    !error message to screen
	Call LogMessage('STOP')                                     !kill run otherwise
  300 return
       END
C======================================================================
       SUBROUTINE OUTBEG(FLAG) 
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      INTEGER FLAG     
      character Bl*16,Astr*255, G12_5*12,I_4*4
       save
      data Bl/'                '/
       IF (FLAG.EQ.0) GO TO 10
       PRINT *, "CALLING LogMessage"
       Call LogMessage('INITIAL CONSTANTS')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('BEGINNING YEAR FOR THIS RUN '//I_4(Byear))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('MANNING N BARE SOIL (OVER) '//G12_5(Nbarov))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('MANNING N BARE SOIL (CHAN) '//G12_5(Nbarch))
       IF (METFLG .EQ. 0) THEN
          PRINT *, "CALLING LogMessage"
          Call LogMessage('WT. DENSITY SOIL (IN PLACE) '//G12_5(WtdSoi)
     &             //' LBSF/FT**3 ')
          PRINT *, "CALLING LogMessage"
          Call LogMessage('CHANNEL ERODIBILITY FACTOR '//G12_5(Kch)//
     &          '  (LBS/FT**2 SEC)/(LBS/FT**2)**1.05')
       ELSE
          BD = WTDSOI/62.4d0
          CHK=KCH*0.1317d0
          PRINT *, "CALLING LogMessage"
          Call LogMessage('WT. DENSITY SOIL (IN PLACE) '//G12_5(BD)
     &          //' G/CM**3 ')
          PRINT *, "CALLING LogMessage"
          Call LogMessage('CHANNEL ERODIBILITY FACTOR '//G12_5(Kch)//
     &          ' (TONNE-HA-H/HA-MJ-MM)')
       ENDIF
       PRINT *, "CALLING LogMessage"
       Call LogMessage('YALIN CONSTANT (ALL PART.) '//G12_5(Yalcon))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('MOMENTUM COEFF. FOR NONUNIFORM VELOCITY '//
     &      'IN CROSS SECTION '//G12_5(Beta_ero)//' (NO UNITS)')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('DISTRIBUTION OF PRIMARY PARTICLES'
     &    //' AND ORGANIC MATTER IN THE ORIGINAL SOIL MASS')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('   TYPE        FRACTION     SPECIFIC SURFACE'
     &                           //' (M**2/G OF SOIL)')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('CLAY'//G12_5(SolCly)//'  '//G12_5(SSCly))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('SILT'//G12_5(SolSlt)//'  '//G12_5(SSSlt))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('SAND'//G12_5(SolSnd)//'  '//G12_5(SSSnd))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('            (M**2/G OF ORGANIC CARBON)')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('ORGANIC MATTER'//G12_5(SolOrg)//'  '//
     &                G12_5(SSOrg))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('(ORGANIC CARBON = ORGANIC MATTER/1.73)')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('INDEX OF SPECIFIC SURFACE '//G12_5(SSSoil)//
     &                ' M**2/G OF TOTAL SOIL')
  10   CONTINUE
       PRINT *, "CALLING LogMessage"
       Call LogMessage('PARTICLE SPECIFICATIONS')
       IF (METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('TYPE        DIA.'//Bl//'SPGRAV.   FRAC. IN')
         PRINT *, "CALLING LogMessage"
         Call LogMessage(' NO.        MM'//Bl//'GM/CM**3  DETACH. SED')
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('TYPE       DIA.    EQSAND DIA. '//
     &                       '  SPGRAV.   FRAC. IN')
         PRINT *, "CALLING LogMessage"
         Call LogMessage(' NO.        MM         MM      '//
     1                ' GM/CM**3  DETACH. SED.')
       ENDIF
       DO 20 K=1,NPART
           DIAMM = 304.8d0*DIA(K)
           WRITE (Astr,'(10X,I2,8X,F5.3,6X,12X,F4.2,7X,F4.2)') 
     &                    K,DIAMM, SPGR(K),FRAC(K)
           PRINT *, "CALLING LogMessage"
           Call LogMessage(Astr)
  20   CONTINUE
       IF (FLAG.NE.0) then
         PRINT *, "CALLING LogMessage"
         Call LogMessage('PARTICLE COMPOSITION')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('TYPE'//Bl//'PRIMARY PARTICLE FRACTIONS')
         PRINT *, "CALLING LogMessage"
         Call LogMessage(' NO.      CLAY       SILT       SAND'//
     &             'ORGANIC MATTER')
         DO 30 K=1,NPART
           WRITE (Astr,'((13X,I2,1X,3(5X,F6.3),8X,F6.3))') 
     &          K,FRCLY(K),FRSLT(K),FRSND(K),FRORG(K)
           PRINT *, "CALLING LogMessage"
           Call LogMessage(Astr)
  30     CONTINUE
       endif
       RETURN
       END
C======================================================================
       SUBROUTINE ReportOverlandSpatialParameters(FLAGOV,AVGSLP,NSEG)
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      INTEGER FLAGOV(10)
      character Ptype(-1:1)*20,Stype(-2:2)*30, G12_5*12,I_4*4
      save
      Data Ptype/'SIMPLE CONCAVE','UNIFORM','SIMPLE CONVEX'/
      Data Stype/'A SHARP CONCAVE SLOPE BREAK','CONCAVE','UNIFORM',
     &           'CONVEX','A SHARP CONVEX SLOPE BREAK'/
       DACRES = DAOVR/43560.0d0
       PRINT *, "CALLING LogMessage"
       Call LogMessage('OVERLAND FLOW INPUTS')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('OVERLAND FLOW TOPOGRAPHY')
       IF(METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('OVERLAND AREA     '//G12_5(Dacres)//' ACRES')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('SLOPE LENGTH      '//G12_5(Slngth)//'   FT')
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('OVERLAND AREA     '//G12_5(DACRES/2.471d0)
     &         //' Hectares')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('SLOPE LENGTH      '//
     &       G12_5(SLNGTH/FeetPerMeter)//'   M')
       ENDIF
       PRINT *, "CALLING LogMessage"
       Call LogMessage('AVERAGE SLOPE     '//G12_5(AvgSlp))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('SEGMENT TYPES DEFINING OVERLAND FLOW PROFILE')
       IF(NSEG.EQ.1)THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('The Profile is '//Ptype(Flagov(1)))
       ELSE
          DO 10 I=1,NSEG
            PRINT *, "CALLING LogMessage"
            Call LogMessage('Segment '//I_4(I)//' is '//
     &                              Stype(Flagov(i)))
  10      CONTINUE
       ENDIF
       PRINT *, "CALLING LogMessage"
       Call LogMessage('SLOPE STEEPNESS ALONG OVERLAND FLOW PROFILE')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('DISTANCE    DISTANCE      SLOPE')
       IF(METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('  FEET       NONDIM.')
       PRINT *, "CALLING LogMessage"
       Call LogMessage(' METERS      NONDIM.')
          DO 20 I=1,NPTSO
   20        Call LogMessage(G12_5(XPOVR(I))//G12_5(XPOVR(I)/SLNGTH)
     &                 //G12_5(SPOVR(I)) )
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage(' METERS      NONDIM.')
          DO 22 I=1,NPTSO
  22         Call LogMessage(G12_5(XPOVR(I)/FeetPerMeter)//
     &           G12_5(XPOVR(I)/SLNGTH)//G12_5(SPOVR(I)))
       ENDIF
       PRINT *, "CALLING LogMessage"
       Call LogMessage('SOIL ERODIBILITY ALONG OVERLAND FLOW PROFILE')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('DISTANCE    DISTANCE      SOIL')
       IF(METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('  FEET       NONDIM.      EROD')
         DO 30 I=1,NXK
  30       Call LogMessage(G12_5(XSoil(I))//
     &           G12_5(Xsoil(I)/SLNGTH)//G12_5(Ksoil(I)))
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage(' METERS      NONDIM.      EROD')
         DO 32 I=1,NXK
  32       Call LogMessage(G12_5(XSoil(I)/FeetPerMeter)//
     &           G12_5(Xsoil(I)/SLNGTH)//G12_5(Ksoil(I)))
       ENDIF
       RETURN
       END
C======================================================================
       ! RM - ReportOverlandTermporalParameters too large of a name.
       SUBROUTINE RptOverlandTermporalParameters(DNYEAR)
C 
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      INTEGER FLAGOV(10)
      character Astr*255, G12_5*12,I_4*4
      INTEGER DATBEG,DATEND,DNYEAR(40)
       save
      PRINT *, "CALLING LogMessage"
      Call LogMessage('OVERLAND FLOW COVER AND MANAGEMENT PARAMETERS')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('(ONE TABLE FOR EACH YEAR OF THE '//I_4(Nyears)//
     &                   ' YEAR ROTATION)')
       DATBEG = 1
       DO 20 NYEAR=1,NYEARS
         DATEND = DATBEG+DNYEAR(NYEAR)-1
         WRITE (Astr,'(A,I2,A,10I8)') 'Year ',NYEAR,' Dates:',
     &                          (Cdate_ero(IDATE),IDATE=DATBEG,DATEND)
         PRINT *, "CALLING logMessage"
         Call logMessage(Astr)
         DO 10 I=1,NXF
           IF (METFLG .EQ. 0) THEN
              PRINT *, "CALLING LogMessage"
              Call LogMessage('X ='//G12_5(XFACT(I))//' Ft')
           ELSE
              PRINT *, "CALLING LogMessage"
              Call LogMessage('X ='//G12_5(XFACT(I)/3.281)//' Ft')
           ENDIF
           WRITE (Astr,'(10F8.3)') (CFACT(I,IDATE),IDATE=DATBEG,DATEND)
           PRINT *, "CALLING LogMessage"
           Call LogMessage('SOIL LOSS RATIO   '// Astr)
           WRITE (Astr,'((10F8.3))')(Pfact_ero(I,IDATE),
     &                                      IDATE=DATBEG,DATEND)
           PRINT *, "CALLING LogMessage"
           Call LogMessage('CONTOURING FACTOR '// Astr)
           WRITE (Astr,'((10F8.3))')(NFACT(I,IDATE),IDATE=DATBEG,DATEND)
           PRINT *, "CALLING LogMessage"
           Call LogMessage('MANNINGS N        '// Astr)
  10       CONTINUE
           DATBEG = DATBEG+DNYEAR(NYEAR)
  20   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE ReportChannelSpatial(INDXC,LNGTHL,NPTSC,XPCHN,SPCHN)
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision LNGTHL,XPCHN(NPTSC),SPCHN(NPTSC),LNGTHU,LEff
      integer FlagC,Flags,Ctlo
      Character Stype(3)*60,Astr*255,G12_5*12,I_4*4
      Data Stype/'A TRIANGULAR SHAPED CHANNEL  SIDE SLOPE=',
     &           'A RECTANGULAR SHAPED CHANNEL BEST FIT SIDE SLOPE=',
     &           'A NATURALLY ERODED CHANNEL   SIDE SLOPE='/
       save
       IF (INDXC.NE.2) then
         PRINT *, "CALLING MapVars"
         Call MapVars(FLAGC,FLAGC1,FLAGS,FLAGS1,CTLO ,CTLO1,CTLZ ,CTLZ1,
     &     CTLN ,CTLN1,CTLSL,CTLSL1,RA   ,RA1,RN   ,RN1,YBASE,YBASE1,
     &     Z    ,Z1,LEFF ,LEFF1,DACREU,DACHU1,DACREL,DACHL1)
       else
         PRINT *, "CALLING MapVars"
         Call MapVars(FLAGC,FLAGC2,FLAGS,FLAGS2,CTLO ,CTLO2,CTLZ ,CTLZ2,
     &     CTLN ,CTLN2,CTLSL,CTLSL2,RA   ,RA2,RN   ,RN2,YBASE,YBASE2,
     &     Z    ,Z2,LEFF ,LEFF2,DACREU,DACHU2,DACREL,DACHL2)
       endif
       LNGTHU         =LEFF-LNGTHL
       PRINT *, "CALLING LogMessage"
       Call LogMessage('CHANNEL '//I_4(Indxc)//' CHARACTERISTICS')
       IF (METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('CHANNEL LENGTH         '//
     &           G12_5(Lngthl)//'   FT')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DRAINAGE AREA UPPER END'//
     &           G12_5(Dacreu)//' ACRES')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('EFFCT. LENGTH UPPER END'//
     &           G12_5(Lngthu)//'   FT')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DRAINAGE AREA LOWER END'//
     &           G12_5(Dacrel)//' ACRES')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('EFFCT. LENGTH LOWER END'//
     &           G12_5(Leff)//'   FT')
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('CHANNEL LENGTH         '//
     &           G12_5(Lngthl/3.281)//'   M')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DRAINAGE AREA UPPER END'//
     &           G12_5(Dacreu/2.471)//' HECTARES')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('EFFCT. LENGTH UPPER END'//
     &           G12_5(Lngthu/3.281)//'   M')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DRAINAGE AREA LOWER END'//
     &           G12_5(Dacrel/2.471)//' HECTARES')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('EFFCT. LENGTH LOWER END'//
     &           G12_5(Leff/3.281)//'  M')
       ENDIF
       IF (FLAGC.EQ.1) Call LogMessage(Stype(1)//G12_5(Z))
       IF (FLAGC.EQ.2) Call LogMessage(Stype(2)//G12_5(Z))
       IF (FLAGC.EQ.3) Call LogMessage(Stype(3)//G12_5(Z))

       IF (FLAGS.NE.1) then
         PRINT *, "CALLING LogMessage"
         Call LogMessage('ENERGY GRADELINE:  SLOPE OF ENERGY '//
     &           'GRADELINE = SLOPE OF CHANNEL')
       ELSE !flags=1
         PRINT *, "CALLING LogMessage"
         Call LogMessage('ENERGY GRADELINE: USES THE ENERGY '
     &          //'GRADELINE CURVES')
         IF (CTLO.EQ.2) THEN
           PRINT *, "CALLING LogMessage"
           Call LogMessage('UNIFORM FLOW CONTROL:TRIANGULAR CHANNEL')
           PRINT *, "CALLING LogMessage"
           Call LogMessage('BOTTOM SLOPE = '//G12_5(Ctlsl))
           PRINT *, "CALLING LogMessage"
           Call LogMessage('SIDE SLOPE   = '//G12_5(Ctlz))
           PRINT *, "CALLING LogMessage"
           Call LogMessage('MANNINGS N   = '//G12_5(Ctln))
         ELSE IF (CTLO.EQ.3) THEN 
           PRINT *, "CALLING LogMessage"
           Call LogMessage('UNIFORM FLOW CONTROL:TRIANGULAR CHANNEL')
           PRINT *, "CALLING LogMessage"
           Call LogMessage('BOTTOM SLOPE = '//G12_5(Ctlsl))
           PRINT *, "CALLING LogMessage"
           Call LogMessage('SIDE SLOPE   = '//G12_5(Ctlz))
           PRINT *, "CALLING LogMessage"
           Call LogMessage('MANNINGS N   = N AT END OF CHANNEL')
         ELSE IF (CTLO.EQ.4) THEN
           PRINT *, "CALLING LogMessage"
           Call LogMessage('RATING CURVE CONTROL:Q = RA*(Y-YBASE)**RN')
           PRINT *, "CALLING LogMessage"
           Call LogMessage('RA    = '//G12_5(Ra))
           PRINT *, "CALLING LogMessage"
           Call LogMessage('RN    = '//G12_5(Rn))
           IF (METFLG .EQ. 0) THEN
             PRINT *, "CALLING LogMessage"
             Call LogMessage('YBASE = '//G12_5(Ybase)//' FT.')
           ELSE
             PRINT *, "CALLING LogMessage"
             Call LogMessage('YBASE = '//G12_5(Ybase/3.281)//' M')
           ENDIF
         ELSE
           PRINT *, "CALLING LogMessage"
           Call LogMessage('CRITICAL DEPTH CONTROL')
           PRINT *, "CALLING LogMessage"
           Call LogMessage('TRIANGULAR CONTROL CHANNEL :SIDE SLOPE = '
     &         //G12_5(Ctlz))
         ENDIF
       ENDIF !of flags.ne.1
       PRINT *, "CALLING LogMessage"
       Call LogMessage('SLOPE STEEPNESS ALONG CHANNEL '//I_4(Indxc))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('DISTANCE    DISTANCE      SLOPE')
       IF (METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('  FEET       NONDIM.')
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('  METERS     NONDIM.')
       ENDIF
       DO 80 I=1,NPTSC
           XSTAR(INDXC,I) = XPCHN(I)/LEFF
           IF (METFLG .EQ. 0) THEN
              WRITE (Astr,'(F7.1)') XPCHN(I)
           ELSE
              WRITE (Astr,'(F7.1)') XPCHN(I)/3.281
           endif
           WRITE (Astr(9:),'(2(4X,F8.3))') XSTAR(INDXC,I),SPCHN(I)
           PRINT *, "CALLING LogMessage"
           Call LogMessage(Astr)
  80   CONTINUE
       RETURN
       END
C======================================================================
      Subroutine MapVars(FLAGC,FLAGCx,FLAGS,FLAGSx,CTLO ,CTLOx,CTLZ ,
     &  CTLZx,CTLN ,CTLNx,CTLSL,CTLSLx,RA   ,RAx,RN   ,RNx,YBASE,YBASEx,
     &     Z    ,Zx,LEFF ,LEFFx,DACREU,DACHUx,DACREL,DACHLx)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision Leff,Leffx
      integer FlagC,Flags,Ctlo,flagcx,flagsx,ctlox
         FLAGC  = FLAGCx
         FLAGS  = FLAGSx
         CTLO   = CTLOx
         CTLZ   = CTLZx
         CTLN   = CTLNx
         CTLSL  = CTLSLx
         RA     = RAx
         RN     = RNx
         YBASE  = YBASEx
         Z      = Zx
         LEFF   = LEFFx
         DACREU = DACHUx/43560.0d0
         DACREL = DACHLx/43560.0d0
       RETURN
       END
C======================================================================
       SUBROUTINE ReportChannelTemporal(INDXC,DNYEAR)
C 
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
       DIMENSION DISTM1(4),TCHN1(4,40),SIDE1(4,40),CENT1(4,40)
       DIMENSION WIDT1(4,40),DISTM2(4)
       DIMENSION TCHN2(4,40),SIDE2(4,40),CENT2(4,40),WIDT2(4,40)
       INTEGER DNYEAR(40),DATBEG,DATEND
       Character Astr*255, G12_5*12,I_4*4
       save
       PRINT *, "CALLING LogMessage"
       Call LogMessage('CHANNEL '//I_4(Indxc)//' COVER AND MANAGEMENT'
     &                  //' PARAMETERS')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('   (ONE TABLE FOR EACH YEAR OF THE '//
     &         I_4(Nyears)//' YEAR ROTATION)')
       DATBEG = 1
       DO 50 NYEAR=1,NYEARS
         DATEND = DATBEG+DNYEAR(NYEAR)-1
         WRITE (Astr,'(A,I2,A,10I8)') 'Year ',NYEAR,' Dates: ',
     &                  (Cdate_ero(IDATE),IDATE=DATBEG,DATEND)
         PRINT *, "CALLING LogMessage"
         Call LogMessage(Astr)
         IF(INDXC.NE.2) THEN
           IF(METFLG .EQ. 0) THEN
           DO 10 I=1,NXC1
               PRINT *, "CALLING LogMessage"
               Call LogMessage('X = '//G12_5(Xchn1(i))//' FT')
               WRITE (Astr,1000) (NCHN1(I,IDATE),IDATE=DATBEG,DATEND)
 1000            Format(10(F8.3,1X))
               PRINT *, "CALLING LogMessage"
               Call LogMessage('MANNINGS N         '//Astr)
               WRITE (Astr,1000) (CCHN1(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('CR SHEAR (LB/FT**2)'//Astr)
               WRITE (Astr,1000) (SCHN1(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('DEPTH SIDE (FT)    '//Astr)
               WRITE (Astr,1000) (DCHN1(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('DEPTH MIDDLE (FT)  '//Astr)
               WRITE (Astr,1000) (WCHN1(I,IDATE),IDATE=DATBEG,DATEND)
   10          Call LogMessage('WIDTH (FT)         '//Astr)
           ELSE
               DO 13 J=1,NXC1
                  DO 12 K=DATBEG,DATEND
                     TCHN1(J,K)=CCHN1(J,K)*0.4882d0
                     SIDE1(J,K)=SCHN1(J,K)/FeetPerMeter
                     CENT1(J,K)=DCHN1(J,K)/FeetPerMeter
                     WIDT1(J,K)=WCHN1(J,K)/FeetPerMeter
  12              CONTINUE
                  DISTM1(J)=XCHN1(J)/FeetPerMeter
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('X = '//G12_5(Distm1(J))//' M')
                  WRITE (Astr,1000) (NCHN1(J,IDATE),IDATE=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('MANNINGS N         '//Astr)
                  WRITE (Astr,1000) (TCHN1(J,M),M=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('CR SHEAR (G/CM**2)'//Astr)
                  WRITE (Astr,1000) (SIDE1(J,M),M=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('DEPTH SIDE (M)    '//Astr)
                  WRITE (Astr,1000) (CENT1(J,M),M=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('DEPTH MIDDLE (M)  '//Astr)
                  WRITE (Astr,1000) (WIDT1(J,M),M=DATBEG,DATEND)
   13             Call LogMessage('WIDTH (M)         '//Astr)
           ENDIF
        ELSE
           IF(METFLG .EQ. 0) THEN
           DO 30 I=1,NXC2
               WRITE (Astr,1000) (NCHN2(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('MANNINGS N         '//Astr)
               WRITE (Astr,1000) (CCHN2(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('CR SHEAR (LB/FT**2)'//Astr)
               WRITE (Astr,1000) (SCHN2(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('DEPTH SIDE (FT)    '//Astr)
               WRITE (Astr,1000) (DCHN2(I,IDATE),IDATE=DATBEG,DATEND)
               PRINT *, "CALLING LogMessage"
               Call LogMessage('DEPTH MIDDLE (FT)  '//Astr)
               WRITE (Astr,1000) (WCHN2(I,IDATE),IDATE=DATBEG,DATEND)
   30          Call LogMessage('WIDTH (FT)         '//Astr)
           ELSE
               DO 33 J=1,NXC2
                  DISTM2(J)=XCHN2(J)/FeetPerMeter
                  DO 32 K=DATBEG,DATEND
                     TCHN2(J,K)=CCHN2(J,K)*0.4882d0
                     SIDE2(J,K)=SCHN2(J,K)/FeetPerMeter
                     CENT2(J,K)=DCHN2(J,K)/FeetPerMeter
                     WIDT2(J,K)=WCHN2(J,K)/FeetPerMeter
  32              CONTINUE
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('X = '//G12_5(Distm2(i))//' M')
                  WRITE (Astr,1000) (NCHN2(I,IDATE),IDATE=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('MANNINGS N         '//Astr)
                  WRITE (Astr,1000) (TCHN2(J,M),M=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('CR SHEAR (G/CM**2)'//Astr)
                  WRITE (Astr,1000) (SIDE2(J,M),M=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('DEPTH SIDE (M)    '//Astr)
                  WRITE (Astr,1000) (CENT2(J,M),M=DATBEG,DATEND)
                  PRINT *, "CALLING LogMessage"
                  Call LogMessage('DEPTH MIDDLE (M)  '//Astr)
                  WRITE (Astr,1000) (WIDT2(J,M),M=DATBEG,DATEND)
   33             Call LogMessage('WIDTH (M)         '//Astr)
           ENDIF
         endif
         DATBEG = DATBEG+DNYEAR(NYEAR)
  50   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE ReportImpoundment 
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision INTAKI
      character G12_5*12,I_4*4
       save
       DACREi  = DAPND/43560.0d0
       INTAKI = INTAKE*43200.0d0
      PRINT *, "CALLING LogMessage"
      Call LogMessage('IMPOUNDMENT INPUTS')
       IF(METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DRAINAGE AREA '//G12_5(Dacrei)//' ACRES')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('INTAKE RATE   '//G12_5(Intaki)//' IN/HR')
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('DRAINAGE AREA '//G12_5(Dacrei/2.741)//' HA')
         PRINT *, "CALLING LogMessage"
         Call LogMessage('INTAKE RATE   '//G12_5(Intaki*2.54)//' CM/HR')
       ENDIF
       PRINT *, "CALLING LogMessage"
       Call LogMessage('IMPOUNDMENT GEOMETRY:(SURFACE AREA = '//
     &                         'FS * DEPTH**B)')
       PRINT *, "CALLING LogMessage"
       Call LogMessage('FS = '//G12_5(FS))
       PRINT *, "CALLING LogMessage"
       Call LogMessage('B  = '//G12_5(B_ero))
       IF(PAC.LT.1) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('COMPUTED FROM SLOPES:FRONT = '//G12_5(Front)//
     &      ' DRAW='//G12_5(Draw)//' SIDE='//G12_5(Side))
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('SUPPLIED BY USER')
       ENDIF
       PRINT *, "CALLING LogMessage"
       Call LogMessage('IMPOUNDMENT EXIT:(Q = C * SQRT(DEPTH))')
       IF(CTL.LT.1) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage('WATER EXITS THROUGH A PIPE OUTLET')
         IF(METFLG .EQ. 0) THEN
           PRINT *, "CALLING LogMessage"
           Call LogMessage('ORIFICE DIAMETER= '//G12_5(DIAO)//' FT')
         ELSE
           PRINT *, "CALLING LogMessage"
           Call LogMessage('ORIFICE DIAMETER= '//G12_5(DIAO/3.281)
     &           //' M')
         ENDIF
         PRINT *, "CALLING LogMessage"
         Call LogMessage('ORIFICE COEFICIENT = '//G12_5(C_ero))
       ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage('ORIFICE COEFICIENT READ IN: C = '//
     &       G12_5(C_ero))
       ENDIF
       RETURN
       END
C======================================================================
      SUBROUTINE NEWPT1(I,IFLAG,ILAST,INOW,NEWPTS)
C
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      save
C
      X1_ero(INOW)=X1_ero(INOW)-0.005d0*SLNGTH
      X1_ero(I)=X1_ero(I)+0.005d0*SLNGTH
C
      IF (IFLAG.EQ.0)THEN
         NEWPTS=NEWPTS+1
         X2_ero(NEWPTS)=X1_ero(INOW)
         S2_ero(NEWPTS)=S1_ero(INOW)
      ELSE
         DLENG=(X1_ero(INOW)-X1_ero(ILAST))/5.0d0
         DO 10 J=2,6
            NEWPTS=NEWPTS+1
            X2_ero(NEWPTS)=(dFLOAT(J-1)*DLENG)+X1_ero(ILAST)
            CALL TABLE(2,NPTSO,X1_ero,S1_ero,X2_ero(NEWPTS),SLOPE)
            S2_ero(NEWPTS)=SLOPE
 10      CONTINUE
      ENDIF
C
      NEWPTS=NEWPTS+1
      X2_ero(NEWPTS)=X1_ero(I)
      S2_ero(NEWPTS)=S1_ero(I)
C
      INOW=I
      ILAST=INOW
C
      RETURN
      END
C======================================================================
      SUBROUTINE NEWPT2(ILAST,INOW,NEWPTS)
C======================================================================
C
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
C
      DLENG=(X1_ero(INOW)-X1_ero(ILAST))/5.0d0
C
      DO 10 J=2,6
         NEWPTS = NEWPTS+1
         X2_ero(NEWPTS)=(dFLOAT(J-1)*DLENG)+X1_ero(ILAST)
         CALL TABLE(2,NPTSO,X1_ero,S1_ero,X2_ero(NEWPTS),SLOPE)
         S2_ero(NEWPTS)=SLOPE
 10   CONTINUE
C
      ILAST=INOW
C
      RETURN
       END
C======================================================================
      Subroutine EchoLine(Io)
C======================================================================
      Integer IO
      character Astr*255
      Read(Io,'(A)',Err=20) Astr
      PRINT *, "CALLING LogMessage"
      Call LogMessage(Astr)
      BACKSPACE (UNIT=IO)
      goto 30
   20   Call LogMessage('Error on reading erosion parameter card')
        PRINT *, "CALLING LogMessage"
        Call LogMessage('STOP')
   30 return
      end
C======================================================================
      SUBROUTINE DailyErosion (SOLOSS,ENRICH,Idate,TEMPC,
     &                             RzRnFall,RzRunoff)
C======================================================================
C was erosb(SOLOSS,ENRICH,FLGMET,TEMPC,BCKEND) - bckend is now hardwired 
c   as '0' = no selection; FlgMet is in commonblk as MetFlg
c temp is in C in RZ and Gleams -- TempC here is mean temp at top
c soloss is lbs/ft^2 * conversion factor => kg/ha
c idate = yyyyddd = sdate & jdate in Gleams
C======================================================================
      Implicit Double Precision (A-H,O-Z)
c      include 'Rz_Params.inc'  
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision TGSY(4,10),DATE,TGSM(4,10),GS(10),CONC(10)
      INTEGER WCHMON,DP,BCKEND,BE,jan(6) 
      save
      data jan/1,4,2,3,4,4/
      data TRAINY/0.0d0/,TRAINM/0.0d0/,TRUNOY/0.0d0/,TRUNOM/0.0d0/
      Sdate = Idate
      RnFall = RzRnFall / CmPerInch  !rz rain in cm - needs to be in inches
      Runoff = RzRunoff / CmPerInch  ! ditto for runoff
      Call Set_SSSoil
c      Call CalculatePeakRunoff   ! uses Runoff - results in Exrain, both in cmb
      PRINT *, "CALLING SetupDailyErosion"
      Call SetupDailyErosion(Indxy,Indxd,IndxNd,Lyear)
      IF (EROOUT.GT.3) CALL ReportStormSummary
      !Call IncrementRain_N_Runoff_Accumulators(NRAINY,NRAINM,NRUNOY, ! RM - name too long.
      PRINT *, "CALLING IncRain_N_Runoff_Acc"
      Call IncRain_N_Runoff_Acc(NRAINY,NRAINM,NRUNOY,
     &                            NRUNOM,TRAINY,TRAINM,TRUNOY,TRUNOM )
      IF (RUNOFF.LE.0.0.OR.EXRAIN.LE.0.0) GO TO 220
CC  CALCULATE THE PARTICLE FALL VELOCITY FOR THE MEAN DAILY TEMPERATURE
       CALL EQVDIA(TEMPC)
C
C CALL THE ROUTING PROCEDURE FOR THE OVERLAND FLOW SECTION BEING MODELED.
       ELEM   = 1
       slr = 0.0d0
       do 105 k=1,nxf
  105    if (slr .lt. cfact(k,indxd)) slr = cfact(k,indxd)
       CALL ROUTE(INDXD, NPTSO,XPOVR,SPOVR,GS,CONC,SOLOSS)
       CALL STROUT(GS,CONC,Daovr,ENRICH)
       IF (SOLOSS.GT.0.0d0) then
         DO 110 K=1,NPART
             TGST(ELEM,K) = TGST(ELEM,K)+GS(K)
             TGSY(ELEM,K) = TGSY(ELEM,K)+GS(K)
  110        TGSM(ELEM,K) = TGSM(ELEM,K)+GS(K)
       endif
       IF (FLGSEQ.EQ.1) GO TO 210
       IF (FLGSEQ.EQ.2) GO TO 190
C
C      IF INDXD HAS CHANGED THEN THE DEPTH AND WIDTH PARAMETERS MUST
C      BE REASSIGNED WITH SPREAD.
C
       IF (INDXD.EQ.LINDXD) GO TO 130
       CALL SPREAD(1,1,INDXD,1,NXC1,XCHN1,DCHN1,NPTSC1,XPCHN1,DEPA)
       CALL SPREAD(1,2,INDXD,1,NXC1,XCHN1,DCHN1,NPTSC1,XPCHN1,DEPB)
       CALL SPREAD(1,1,INDXD,1,NXC1,XCHN1,WCHN1,NPTSC1,XPCHN1,WIDA)
       CALL SPREAD(1,2,INDXD,1,NXC1,XCHN1,WCHN1,NPTSC1,XPCHN1,WIDB)
 130   CONTINUE
C
C      CALL THE ROUTING PROCEDURE FOR THE FIRST CHANNEL BEING MODELED
C
       ELEM   = 2
       CALL ROUTE(INDXD, NPTSC1,  XPCHN1,SPCHN1, GS,CONC,SOLOSS)
       CALL STROUT(GS,CONC,DACHL1,ENRICH)
       IF (SOLOSS.Gt.0.0d0) then
         DO 140 K=1,NPART
           TGST(ELEM,K) = TGST(ELEM,K)+GS(K)
           TGSY(ELEM,K) = TGSY(ELEM,K)+GS(K)
  140      TGSM(ELEM,K) = TGSM(ELEM,K)+GS(K)
       endif
       IF (FLGSEQ.EQ.3) GO TO 210
       IF (FLGSEQ.EQ.5) GO TO 190
C
C      IF INDXD HAS CHANGED THEN THE DEPTH AND WIDTH PARAMETERS MUST
C      BE REASSIGNED WITH SPREAD.
C
       IF (INDXD.EQ.LINDXD) GO TO 160
       CALL SPREAD(1,1,INDXD,2,NXC2,XCHN2,DCHN2,NPTSC2,XPCHN2,DEPA)
       CALL SPREAD(1,2,INDXD,2,NXC2,XCHN2,DCHN2,NPTSC2,XPCHN2,DEPB)
       CALL SPREAD(1,1,INDXD,2,NXC2,XCHN2,WCHN2,NPTSC2,XPCHN2,WIDA)
       CALL SPREAD(1,2,INDXD,2,NXC2,XCHN2,WCHN2,NPTSC2,XPCHN2,WIDB)
 160   CONTINUE
C
C      CALL THE ROUTING PROCEDURE FOR THE SECOND CHANNEL BEING MODELED
C
       ELEM   = 3
       CALL ROUTE(INDXD, NPTSC2,  XPCHN2,SPCHN2, GS,CONC,SOLOSS)
       CALL STROUT(GS,CONC,DACHL2,ENRICH)
       IF (SOLOSS.GT.0.0d0) then
         DO 170 K=1,NPART
           TGST(ELEM,K) = TGST(ELEM,K)+GS(K)
           TGSY(ELEM,K) = TGSY(ELEM,K)+GS(K)
  170      TGSM(ELEM,K) = TGSM(ELEM,K)+GS(K)
       endif
       IF (FLGSEQ.LT.6) GO TO 210
 190   CONTINUE
C
C      CALL THE IMPOUNDMENT ROUTING PROCEDURE IF AN IMPOUNDMENT IS
C      BEING MODELED.
C
       ELEM   = 4
       CALL IMPOND(GS,CONC,SOLOSS)
       CALL STROUT(GS,CONC,DAPND,ENRICH)
       IF (SOLOSS.GT.0.0d0) then
         DO 200 K=1,NPART
           TGST(ELEM,K) = TGST(ELEM,K)+GS(K)
           TGSY(ELEM,K) = TGSY(ELEM,K)+GS(K)
  200      TGSM(ELEM,K) = TGSM(ELEM,K)+GS(K)
       endif
       GO TO 230
 210   CONTINUE
C
C      CHECK FOR SOIL LOSS AND IF NONE OCCURED SET THE SEDIMENT
C      ENRICHMENT RATIO TO ZERO.
C
       IF (SOLOSS.GT.0.0) GO TO 230
 220   CONTINUE
       SOLOSS = 0.0d0
       ENRICH = 0.0d0
 230   CONTINUE
C
       LINDXD = INDXD
       ELEM   = jan(flgseq)                                             jan
c       IF (IEOMY .GE. 2 .AND. EROOUT .GE. 2) THEN                       DATE
       IF (IEOMY .GE. 2) THEN                                           DATE
          CALL MONOUT(NMONTH,NYEAR,NRAINM,NRUNOM,TRAINM,TRUNOM,TGSM,
     &                      BCKEND)
       ENDIF                                                            DATE
       IF (IEOMY .GE. 3) THEN                                           DATE
          CALL ANNOUT(LYEAR,NRAINY,NRUNOY,TRAINY,TRUNOY,TGSY)
       ENDIF                                                            DATE
       SoLoss = SoLoss *LbsPerFt2_to_KgPerHa
       If (Soloss.le.0.0d0) then            !if there is no soil lost
         Soloss = 0.0d0                     !there is no enrichment
         enrich = 0.0d0
       endif
       RETURN
       END
C======================================================================
       SUBROUTINE SetupDailyErosion(Indxy,Indxd,IndxNd,Lyear)
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      Logical FirstPass
      integer Indxy,Indxd,IndxNd,Lyear
      save
      data FirstPass/.true./ 
      Indxy = 0
      Lyear = 0
       DO 10 I=1, 4                                                        BE
          ELMLOS(I) = 0.0d0                                                BE
          ELMENR(I) = 0.0d0                                                BE
10     CONTINUE                                                            BE
       RUNOFF = RUNOFF / 12.0d0                   ! in to foot
       EXRAIN = EXRAIN / 43200.0d0               ! in/hr to ft/s. Extrain is supplied by RZWQM2, not needed here CHD
       if (FirstPass) then                                                 temple
          INDXY  = -NYEARS  ! set index to -# years in rotation
          INDXD  = NDATES   ! set to total number of dates
          INDXND = 1                                                       temple
          FirstPass = .false.
       endif                                                               temple
       IF (LYEAR .NE. NYEAR) then
         INDXY  = INDXY+(NYEAR-LYEAR)
         LYEAR  = NYEAR              
         IF (INDXY.GT.NYEARS) then
           INDXY  = INDXY-NYEARS     
           INDXD  = NDATES           
           INDXND = 1                
           IF (INDXY.GT.NYEARS) INDXY = MOD(INDXY,NYEARS)
           IF (INDXY.EQ.0) INDXY = NYEARS                
         endif
       endif
C
C      NDATE IS USED TO COMPARE AGAINST THE NEXT VALUE OF Cdate_ero
C      FOR DECIDING WHETHER OR NOT TO UPDATE INDXD.  INDXND IS THE
C      INDEX FOR THE NEXT VALUES OF Cdate_ero.
C
      If (Ndates.GT.1) then
        NDATE = MOD(SDATE,1000)+1000*INDXY
   90   IF (NDATE.GT.Cdate_ero(INDXND)) then
          INDXD  = INDXND
          INDXND = INDXND+1
          GO TO 90
        endif
      endif
      Return
      end
C======================================================================
       SUBROUTINE ReportStormSummary
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      character Alab(4)*18,I_4*4,G12_5*12,Astr*255
      data Alab/'  Rainfall     =','  Runoff Volume=',
     &          '  Excess Rain  =','  EI           ='/
      save
      IF (RNFALL .GT. 0.0d0 ) then
        RUNTMP = 12.0d0 * RUNOFF
        EXRTMP = 43200.0d0*EXRAIN    !done in RZWQM, no need to convert here, by CHD
        !EXRTMP = EXRAIN
        Write(Astr,'(A,I10,A)')'==== Storm Inputs === on ',Sdate,' DOY'
        PRINT *, "CALLING LogMessage"
        Call LogMessage(Astr)
        IF (METFLG .EQ. 0) THEN
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(1)//G12_5(RnFall)//' inches')
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(2)//G12_5(Runtmp)//' inches')
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(3)//G12_5(Exrtmp)//' in/hr')
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(4)//G12_5(EI)//
     &' Wischmeier Engl. units')
        ELSE
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(1)//G12_5(RnFall*CmPerInch)//' cm')
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(2)//G12_5(Runtmp*CmPerInch)//' cm')
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(3)//G12_5(Exrtmp*CmPerInch)//' cm/hr')
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Alab(4)//G12_5(EI*17.02d0)//
     &' MJ-mm/ha-hr')
        ENDIF
        IF (RUNOFF.LE.0.0d0) 
     &     Call LogMessage('***  NO RUNOFF - NO SOIL LOSS  ***')
      endif
      RETURN
      END
C======================================================================
       !SUBROUTINE IncrementRain_N_Runoff_Accumulators(NRAINY,NRAINM, RM - name too long
       SUBROUTINE IncRain_N_Runoff_Acc(NRAINY,NRAINM,
     &               NRUNOY,NRUNOM,TRAINY,TRAINM,TRUNOY,TRUNOM )
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       save
       IF (RNFALL .GT. 0.0d0) THEN
          NRAINT = NRAINT+1
          NRAINY = NRAINY+1
          NRAINM = NRAINM+1
          TRAINT = TRAINT+RNFALL
          TRAINY = TRAINY+RNFALL
          TRAINM = TRAINM+RNFALL
       ENDIF
C
C      CHECK TO SEE WHETHER OR NOT ANY RUNOFF OCCURED AND INCREMENT THE
C      COUNTERS FOR THE NUMBER OF RUNOFF EVENTS AND THE RUNOFF TOTALS
C      BEFORE STARTING THE ROUTING CALCULATIONS
C
      IF (RUNOFF.GT.0.0d0.AND.EXRAIN.GT.0.0d0) THEN
        NRUNOT = NRUNOT+1
        NRUNOY = NRUNOY+1
        NRUNOM = NRUNOM+1
        TRUNOT = TRUNOT+RUNOFF
        TRUNOY = TRUNOY+RUNOFF
        TRUNOM = TRUNOM+RUNOFF
      endif
      Return
      end
C======================================================================
       SUBROUTINE EQVDIA (TEMPC)
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       save
       DATA  A,B,C,D,E,F/1.79d-06, -6.127d-08, 1.522d-09, -2.44d-11,
     &                   1.72d-13, 0.092903d0/
       KINVIS=(A+(B*TEMPC)+(C*TEMPC**2.)+(D*TEMPC**3.)+(E*TEMPC**4))/F
       DO 60 K=1,NPART
           FALL(K)   = FALVEL(SPGR(K),DIA(K),KINVIS,AGRAV)
           EQSAND(K) = SEDDIA(2.65d0,FALL(K),KINVIS,AGRAV)
  60   CONTINUE
       IF (EQSAND(NPART).LT.EQSAND(NPART-1)) THEN
         TMP             = EQSAND(NPART-1)
         EQSAND(NPART-1) = EQSAND(NPART)
         EQSAND(NPART)   = TMP
         TMP             = SPGR(NPART-1)
         SPGR(NPART-1)    = SPGR(NPART)
         SPGR(NPART)      = TMP
       endif
       DCL(1) = 0.0
       DO 80 K=1,NPART
           DCL(K+1) = EQSAND(K)
           DDCL(K)  = DCL(K+1)-DCL(K)
  80   CONTINUE
       IF (SPGR(NPART).LT.SPGR(NPART-1)) THEN
         TMP             = EQSAND(NPART-1)
         EQSAND(NPART-1) = EQSAND(NPART)
         EQSAND(NPART)   = TMP
         TMP             = SPGR(NPART-1)
         SPGR(NPART-1)   = SPGR(NPART)
         SPGR(NPART)     = TMP
       endif
       RETURN
       END
C======================================================================
       Double Precision FUNCTION FALVEL(SPG,DIAi,KINVIS,AGRAV)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      COMMON /FALL/ RE(9),CDRE2(9),CDDRE(9)
      double precision KINVIS,DIAi,Agrav,Spg
       save
       RTSID  = ((SPG-1.0d0)*AGRAV*(DIAi**3)/(KINVIS**2))*(8.0d0/6.0d0)
       IF (RTSID.LT.0.024d0) then
          FALVEL = (DIAi**2)*(SPG-1.0d0)*AGRAV/(KINVIS*18.0d0)
       ELSE 
         RTSID  = DLOG(RTSID)
         CALL TABLE(4,9,RE,CDRE2,RTSID,REY)
         REY    = dEXP(REY)
         FALVEL = REY*KINVIS/DIAi
       endif
       RETURN
       END
C======================================================================
       Double Precision FUNCTION SEDDIA(SPG,VS,KINVIS,AGRAV)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
       COMMON /FALL/ RE(9),CDRE2(9),CDDRE(9)
       double precision KINVIS
       save
       RTSID  = 1.3333d0*AGRAV*(SPG-1.0d0)*KINVIS/(VS**3)
       IF (RTSID.GT.2.0E+06) then
         SEDDIA = dSQRT(18.0d0*VS*KINVIS/((SPG-1.0d0)*AGRAV))
       else 
         RTSID  = dLOG(RTSID)
         CALL TABLE(3,9,RE,CDDRE,RTSID,REY)
         REY    = dEXP(REY)
         SEDDIA = REY*KINVIS/VS
       endif
       RETURN
       END

C======================================================================
       SUBROUTINE ANNOUT(LYEAR,NRAINY,NRUNOY,TRAINY,TRUNOY,TGSY)
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision TGSY(4,10)
      integer SetReportFlag
      Character Vlab*41,ObjLab(4)*13, G12_5*12,I_4*4
      data Vlab/'                            Values from '/
      data ObjLab/'Overland Flow','Channel One','Channel Two',
     &            'Impoundment'/
      save
      RUNVOL = TRUNOY*DAREA(ELEM)
      TRUNOY = 12.0d0*TRUNOY
      PRINT *, "CALLING LogMessage"
      Call LogMessage('=== Annual Summary for Year '//I_4(Lyear))
      IF (METFLG .EQ. 0) THEN
         PRINT *, "CALLING LogMessage"
         Call LogMessage(I_4(Nrainy)//' storms produced '//
     &     G12_5(TrainY)//' in. of rainfall')
         PRINT *, "CALLING LogMessage"
         Call LogMessage(I_4(Nrunoy)//' storms produced '//
     &     G12_5(TrunoY)//' in. of runoff')
      ELSE
         PRINT *, "CALLING LogMessage"
         Call LogMessage(I_4(Nrainy)//' storms produced '//
     &     G12_5(TrainY*CmPerInch)//' cm of rainfall')
         PRINT *, "CALLING LogMessage"
         Call LogMessage(I_4(Nrunoy)//' storms produced '//
     &     G12_5(TrunoY*CmPerInch)//' cm of runoff')
      ENDIF
      IF (NRUNOY.GT.0) then
         FLGTMP = SetReportFlag(EROOUT)
         DO 50 J=1,ELEM
           IF ((J.EQ.2.AND.FLGSEQ.EQ.2).or.(J.EQ.3.AND.FLGSEQ.EQ.2)
     &        .or.(J.EQ.3.AND.FLGSEQ.EQ.5)) then
c             nada -- do nothing
           else
             PRINT *, "CALLING LogMessage"
             Call LogMessage(Vlab//ObjLab(J))
             Tgs =Sum_TGS(j,Tgsy,Npart)
             IF (TGS.LE.0.0d0) then
                PRINT *, "CALLING LogMessage"
                Call LogMessage('               *** NO SOIL LOSS ***')
             else
               Call DisplayHeader
               PRINT *, "CALLING ReportConcentrations"
               Call ReportConcentrations(j,Tgsy,TGS,RunVol)
             endif
           endif
  50     CONTINUE
       endif !of if (Nrunoy.gt.0 -- was 60 continue
       PRINT *, "CALLING ZeroOut"
       Call ZeroOut(Nrainy,Nrunoy,TrainY,TrunoY,TGSY,Elem,Npart)
       RETURN
       END
C======================================================================
       SUBROUTINE DisplayHeader
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      PRINT *, "CALLING LogMessage"
      Call LogMessage('                     THE QUANTITY OF ERODED'
     &         //' SEDIMENT IN RUNOFF')
      PRINT *, "CALLING LogMessage"
      Call LogMessage('      PART.   FRAC. IN   SOIL LOSS         '
     &        //'CONCENTRATIONS (SOIL/WATER)    ')
      IF (METFLG .EQ. 0) THEN
        PRINT *, "CALLING LogMessage"
        Call LogMessage('      TYPE   SED. LOAD      LBS.      LBSF'
     &            //'/FT**3    LBSF/LBSF     PPM (WT)')
      ELSE
        PRINT *, "CALLING LogMessage"
        Call LogMessage('      TYPE   SED. LOAD       KG        KG/'
     &           //'M**3        KG/KG       PPM (WT)')
      ENDIF
      return
      end
C======================================================================
       SUBROUTINE ReportConcentrations(j,Tgsy,TGS,RunVol)
C======================================================================
c TYPE   SED. LOAD       KG        KG/M**3        KG/KG       PPM (WT)
c  K       frac        tgsy(j,k)     conc         concpc        ppm
c        f(tgsy/tgs)                             (conc/wtdH2o)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision CONC(10),Tconc,Tcncpc,Tppm,Soloss,TGSY(4,10)
      Character Astr*255,AlossLab*20,G12_5*12
      Data AlossLab/'  Annual Soil Loss '/
      TCONC  = 0.0d0
      TCNCPC = 0.0d0
      TPPM   = 0.0d0
      DO 20 K=1,NPART
        CONC(K) = TGSY(J,K)/RUNVOL
        CONCPC  = CONC(K)/WTDH2O
        PPM     = CONCPC*1.0d+06
        FRACK(k)    = TGSY(J,K)/TGS    !need to check FRAC definition, Liwang, 11-8-2013
        IF (FLGTMP.EQ.1) THEN
          IF (METFLG.EQ.0) THEN
             WRITE (Astr,4002) K,FRACK(k),TGSY(J,K),CONC(K),CONCPC,PPM
4002   FORMAT(7X,I2,6X,F5.2,4X,F9.0,4X,F9.4,4X,F9.4,4X,F10.0)
          ELSE
             CONCM=CONC(K)*16.01d0
             TGSYM=TGSY(J,K)/2.205d0
             WRITE (Astr,4002) K,FRACK(k),TGSYM,CONCM,CONCPC,PPM
          ENDIF
          PRINT *, "CALLING LogMessage"
          Call LogMessage(Astr)
        ENDIF
        TCONC   = TCONC  + CONC(K)
        TCNCPC  = TCNCPC + CONCPC
        TPPM    = TPPM   + PPM
  20  CONTINUE
      DACREi  = DAREA(J)/43560.0d0
      SOLOSS = TGS/(2000.0d0*DACREi)
      If (FlgTmp .NE. 0) then
        TGSm = TGS
        IF (METFLG .EQ. 0) TGSm = TGS/2.205d0
        WRITE (Astr,'(8x,3(9X,F9.4),4X,F10.0)') TGSm,TCONC,TCNCPC,TPPM
        PRINT *, "CALLING LogMessage"
        Call LogMessage('          Total'//Astr)
        IF (METFLG .EQ. 0) THEN
           PRINT *, "CALLING LogMessage"
           Call LogMessage('ANNUAL SOIL LOSS FOR AREA '//G12_5(SoLoss)
     &       //' TONS/ACRE == (AREA = '//G12_5(Dacrei)//' ACRES)')
           ELMANN(J) = SOLOSS                                        
         ELSE
           DHECT=DACREi/2.471d0
           TPH=SOLOSS*2.242d0
           PRINT *, "CALLING LogMessage"
           Call LogMessage('ANNUAL SOIL LOSS FOR AREA '//G12_5(tph)
     &       //' TONNES/HA == (AREA = '//G12_5(Dhect)//' HECTARES)')
           ELMANN(J) = tph                                           
         ENDIF
         CALL ENRCMP(2,CONC,ENRICH)
         ENRANN(J) = ENRICH
      else
         CALL ENRCMP(1,CONC,ENRICH)
         ENRANN(J) = ENRICH 
         IF (METFLG .EQ. 0) THEN
           PRINT *, "CALLING LogMessage"
           Call LogMessage(AlossLab//G12_5(Soloss)//' tons/acre')
           ELMANN(J) = SOLOSS
         ELSE
           TPH=SOLOSS*2.242d0
           PRINT *, "CALLING LogMessage"
           Call LogMessage(AlossLab//G12_5(tph)//' tonnes/hectare')
           ELMANN(J) = TPH 
         ENDIF
         PRINT *, "CALLING LogMessage"
         Call LogMessage('     Enrichment Ratio='//G12_5(Enrich))
      endif 
      return
      end 
C======================================================================
       SUBROUTINE ENRCMP(FLAG,CONC,ENRICH)
C======================================================================
c calculation of enrichment factor
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      character G12_5*12
       double precision CONC(10)
       INTEGER FLAG 
       save
       SUMSED = 0.0d0
       SUMCLY = 0.0d0
       SUMSLT = 0.0d0
       SUMSND = 0.0d0
       SUMORG = 0.0d0
       DO 10 K=1,NPART
           SUMSED = SUMSED+CONC(K)
           SUMCLY = SUMCLY+CONC(K)*FRCLY(K)
           SUMSLT = SUMSLT+CONC(K)*FRSLT(K)
           SUMSND = SUMSND+CONC(K)*FRSND(K)
  10   CONTINUE
       SEDCLY = SUMCLY/SUMSED
       SEDSLT = SUMSLT/SUMSED
       SEDSND = SUMSND/SUMSED
       SEDORG = SOLORG*SEDCLY/SOLCLY
       TOTAL  = 1.0+SEDORG
       SSSED  = SSORG*SEDORG/1.73d0+(SSCLY*SEDCLY+SSSLT*SEDSLT+SSSND*
     1          SEDSND)/TOTAL
       ENRICH = SSSED/SSSOIL
       IF (FLAG.GT.1) then
         PRINT *, "CALLING Logmessage"
         Call Logmessage('DISTRIBUTION OF PRIMARY PARTICLES '//
     &                  'AND ORGANIC MATTER IN THE ERODED SEDIMENT')
         PRINT *, "CALLING Logmessage"
         Call Logmessage('TYPE           FRACTION')
         PRINT *, "CALLING Logmessage"
         Call Logmessage('CLAY            '//G12_5(SedCly))
         PRINT *, "CALLING Logmessage"
         Call Logmessage('SILT            '//G12_5(SedSlt))
         PRINT *, "CALLING Logmessage"
         Call Logmessage('SAND            '//G12_5(SedSnd))
         PRINT *, "CALLING Logmessage"
         Call Logmessage('ORGANIC MATTER      '//G12_5(Sedorg))
         PRINT *, "CALLING Logmessage"
         Call Logmessage('INDEX OF SPECIFIC SURFACE '//G12_5(SSSed)//
     &                    ' M**2/G OF TOTAL SEDIMENT')
         PRINT *, "CALLING Logmessage"
         Call Logmessage('ENRICHMENT RATIO OF SPECIFIC SURFACE '//
     &            G12_5(Enrich))
       endif
       RETURN
       END
C======================================================================
       SUBROUTINE ROUTE(DATE,NPTS,X,SLOPE,GS,CONC,SOLOSS)
C======================================================================
C      THIS SUBROUTINE IS USED TO ROUTE SEDIMENT THROUGH THE OVERLAND
C      FLOW AND CHANNEL SECTION OF THE MODEL
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       COMMON /FLOWInt/ FLAGS
       Common/Flow/QB,QE,YCR,YNOR,YE,SFE,RUNPND
       COMMON /R000/ CHECKU,COEFF,CROP,CRSH,CTLN,CTLSL,CTLZ,C1,C3,DAL,
     &               DAU,DEPSID,DX,EATA,EFFSHL,EFFSHU,ENDMAN,EXCESS,
     &               EXCOLD,EXPON,PRACT,QLAT,QLE,QU,RA,
     &               RATEX,RN,SFA,SFL,SFU,SSFB,SSFE,SUMDF,SUMEXD,
     &               SUMPL,SUMPLD,SUMTC,SUMTCL,TEMDEP,TEMWER,WFA,WFL,
     &               WFU,XBEG,XDBIG,XDBMIN,SDEMAX,XDSMAL,YBASE,Z
       COMMON /R000int/ I,ICHAN,IFACT,INDXC,ISOIL,K,NDEP,
     &               NEWP,NEXCES,NK,NT,NT2,NZ

       double precision X(40),SLOPE(40),GS(10),CONC(10),
     1      DLAT(10),PHI(10),POTLD(10),DTCDX(10),XDE(10),
     1      DDE(10),GSDE(10),GSTDE(10),EXDET(10),XDBEG(10),GSTDB(10),
     1      TCM(10),DU(10),TCU(10),GSU(10),GSTU(10),DL(10),TCL(10),
     1      GSL(10),GSTL(10),DF(10),LEFF,KEROD,NMANOV,
     1      NMANCH,MAXE,INTRPL
       INTEGER DATE,FLAG1,FLAGC,FLAGS,CTLO,FLAGCT 
       save
       DATA COVSH /1000.0d0/
       If (Elem.Eq.1) then 
         DAL    = DAOVR
         LEFF   = SLNGTH
         ISOIL  = 1
         KEROD  = KSOIL(ISOIL)
         IFACT  = 1
         CROP   = CFACT(IFACT,DATE)
         PRACT  = PFACT_ero(IFACT,DATE)
         NMANOV = NFACT(IFACT,DATE)
         EATA   = 0.5
       else If (Elem.Eq.2) then
         PRINT *, "CALLING MapChannel"
         Call MapChannel(1,FLAGCT ,FLAGS,CTLO,CTLZ,CTLN,CTLSL,
     &              RA,RN,YBASE,DAU,DAL,Z,LEFF,ENDMAN,ICHAN,CRSH,
     &              DEPSID,NMANCH,EATA,INDXC,Date)
       else
         PRINT *, "CALLING MapChannel"
         Call MapChannel(2,FLAGCT ,FLAGS,CTLO,CTLZ,CTLN,CTLSL,
     &              RA,RN,YBASE,DAU,DAL,Z,LEFF,ENDMAN,ICHAN,CRSH,
     &              DEPSID,NMANCH,EATA,INDXC,Date)
      endif
       SFE    = SLOPE(NPTS)
       DO 740 I=2,NPTS
           IF(I.GT.2) GO TO 90
C
C          INITIALIZE VARIABLES FOR TOP SEGMENT
C
           NEWP   = 1
           IF(ELEM.GT.1) GO TO 50
C
C          INITIALIZE OVERLAND FLOW VARIABLES FOR TOP SEGMENT
C
           EFFSHU = 0.0
           WFU    = 1.0
           SFU    = SLOPE(1)  !Demar uses SFU=tan(slope(1)), why?
           QU     = 0.0
           QE     = EXRAIN*LEFF
           QLAT   = EXRAIN
           DO 40 K=1,NPART
               DU(K)   = 0.0
               TCU(K)  = 0.0
               GSU(K)  = 0.0
               GSTU(K) = 0.0
  40       CONTINUE
           GO TO 110
C
C          INITIALIZE CHANNEL VARIABLES FOR TOP SEGMENT
C
  50       CONTINUE
           QB     = EXRAIN*DAU
           QE     = EXRAIN*DAL
           QU     = EXRAIN*DAU
           QLAT   = QE/LEFF
           DO 60 K=1,NPART
               GSTU(K) = QU*CONC(K)
               DLAT(K) = QLAT*CONC(K)
  60       CONTINUE
           CALL FSLPAR(CTLO,CTLZ,CTLN,CTLSL,RA,RN,
     1                 YBASE,Z,ENDMAN,LEFF,IBEG,
     1                 FLAG1,C1,C3,SLOPE(NPTS),SSFE)
           TB     = 2.0*RUNOFF*DAL/QE
           IF(QU.GT.0.0d0) GO TO 80
           WFU    = 0.0d0
           FLAGC  = FLAGCT
           IF(FLAGC.EQ.2) WFU = WIDB(INDXC,1)
           EFFSHU = 0.0d0
           SFU    = 0.0d0
           DO 70 K=1,NPART
               DU(K)   = 0.0d0
               TCU(K)  = 0.0d0
               GSU(K)  = 0.0d0
               GSTU(K) = 0.0d0
  70       CONTINUE
           GO TO 110
  80       CONTINUE
           XBEG = XSTAR(INDXC,1)
           IF(FLAGS.EQ.1) CALL FRICHN(QU,NMANCH,Z,C1,C3,
     1                                1,FLAG1,SLOPE(1),XSTAR(INDXC,1),
     1                                LEFF,YE,XBEG,IBEG,SSFB,SSFE,SFU)
           IF(FLAGS.EQ.2) SFU = SLOPE(1)
           IF(SFU.LE.0.0) SFU = 0.000001
           GO TO 110
  90       CONTINUE
           NEWP   = 0
           SFU    = SFL
           QU     = QLE
           WFU    = WFL
           EFFSHU = EFFSHL
           DO 100 K=1,NPART
               DU(K)   = DL(K)
               TCU(K)  = TCL(K)
               GSU(K)  = GSL(K)
               GSTU(K) = GSTL(K)
 100       CONTINUE
 110       CONTINUE
           QLE     = QE*X(I)/LEFF
           DX     = X(I)-X(I-1)
           IF(ELEM.EQ.2) GO TO 140
           IF(ELEM.EQ.3) GO TO 150
           SFL    = SLOPE(I)       !Demar uses SFL=tan(slope(I)), why?
           SFA    = (SFL+SFU)/2
           WFL    = 1.0
C
C          TEST WHETHER THE X DEPENDANT VARIABLES NEED TO BE UPDATED
C
           IF(X(I).LE.XSOIL(ISOIL)) GO TO 120
           NEWP   = 1
           ISOIL  = ISOIL+1
           KEROD  = KSOIL(ISOIL)
 120       CONTINUE
           IF(X(I).LE.XFACT(IFACT)) GO TO 130
           NEWP   = 1
           IFACT  = IFACT+1
           CROP   = CFACT(IFACT,DATE)
           PRACT  = PFACT_ero(IFACT,DATE)
           NMANOV = NFACT(IFACT,DATE)
 130       CONTINUE
           CALL INRILL(SFA,KEROD,CROP,PRACT,DLAT)
           IF(NEWP.EQ.1) CALL SHOVR(QU,SFU,NMANOV,EFFSHU)
           CALL SHOVR(QLE,SFL,NMANOV,EFFSHL)
           GO TO 190
 140       CONTINUE
           IF(X(I).LE.XCHN1(ICHAN)) GO TO 160
           NEWP   = 1
           ICHAN  = ICHAN+1
           CRSH   = CCHN1(ICHAN,DATE)
           DEPSID = SCHN1(ICHAN,DATE)
           NMANCH = NCHN1(ICHAN,DATE)
           GO TO 160
 150       CONTINUE
           IF(X(I).LE.XCHN2(ICHAN)) GO TO 160
           NEWP   = 1
           ICHAN  = ICHAN+1
           CRSH   = CCHN2(ICHAN,DATE)
           DEPSID = SCHN2(ICHAN,DATE)
           NMANCH = NCHN2(ICHAN,DATE)
 160       CONTINUE
           XBEG = XSTAR(INDXC,IBEG)
           IF(FLAGS.EQ.1.AND.I.LT.NPTS) CALL FRICHN(QLE,NMANCH,Z,
     1       C1,C3,I,FLAG1,SLOPE(I),XSTAR(INDXC,I),LEFF,YE,XBEG,IBEG,
     1       SSFB,SSFE,SFL)
           IF(FLAGS.EQ.2.AND.I.LT.NPTS) SFL = SLOPE(I)
           IF(I.EQ.NPTS) SFL = SFE
           IF(SFL.LE.0.0) SFL = 0.000001
           IF(NEWP.EQ.0.OR.QU.EQ.0.0) GO TO 180
           FLAGC  = FLAGCT
           IF(FLAGC.EQ.3.AND.DEPB(INDXC,I-1).EQ.0.0) FLAGC = 2
           CALL HYDCHN(FLAGC,QU,SFU,C1,Z,WIDB(INDXC,I-1),WFU,NMANCH,
     1                CRSH,COVSH,EFFSHU)
           DO 170 K=1,NPART
               GSU(K) = GSTU(K)/WFU
 170       CONTINUE
 180       CONTINUE
           FLAGC  = FLAGCT
           IF(FLAGC.EQ.3.AND.DEPA(INDXC,I).EQ.0.0) FLAGC = 2
           CALL HYDCHN(FLAGC,QLE,SFL,C1,Z,WIDA(INDXC,I),WFL,NMANCH,
     1                 CRSH,COVSH,EFFSHL)
 190       CONTINUE
           WFA    = (WFL+WFU)/2.0d0
           DO 200 K=1,NPART
               PHI(K)   = EATA*FALL(K)*WFA/QLAT
 200       CONTINUE
           IF(NEWP.EQ.0.OR.QU.EQ.0.0d0) GO TO 280
           CALL TRNCAP(EFFSHU,TCU,GSU)
           EXCESS = 1.0
           DO 210 K=1,NPART
               IF(TCU(K).EQ.0.0d0) GO TO 220
               EXCESS   = dMIN1(EXCESS,(1.0d0-GSU(K)/TCU(K)))
 210       CONTINUE
           IF(EXCESS.GT.0.0d0) GO TO 240
 220       CONTINUE
           DO 230 K=1,NPART
               DU(K)    = (PHI(K)/X(I-1))*(TCU(K)*WFU-GSTU(K))
               IF(DU(K).GT.0.0d0) DU(K) = 0.0d0
 230       CONTINUE
           EXCESS = 1.0d0
           GO TO 280
 240       CONTINUE
           MAXE   = 1000.0d0
           IF(ELEM.GT.1) GO TO 250
           CALL RILL(X(I-1),SLOPE(I-1),KEROD,CROP,PRACT,MAXE,EXCESS,DF)
           GO TO 260
 250       CONTINUE
           FLAGC  = FLAGCT
           IF(FLAGC.EQ.3.AND.DEPB(INDXC,I-1).EQ.0.0) FLAGC = 2
           CALL DCAP(1,FLAGC,QU,SFU,C1,Z,EFFSHU,DEPSID,DEPB(INDXC,I-1),
     1               WERB(INDXC,I-1),WFU,NMANCH,CRSH,COVSH,MAXE,
     1               EXCESS,TB,DF)
           IF(FLAGC.EQ.2.AND.WERB(INDXC,I-1).GT.WFU)
     1       WIDB(INDXC,I-1) = WERB(INDXC,I-1)
 260       CONTINUE
           DO 270 K=1,NPART
               DU(K)    = DF(K)*WFU
 270       CONTINUE
           EXCESS = 1.0d0
 280       CONTINUE
           DO 290 K=1,NPART
               POTLD(K) = (GSTU(K)+DLAT(K)*DX)/WFL
 290       CONTINUE
           CALL TRNCAP(EFFSHL,TCL,POTLD)
           NT     = 0
           NK     = 0
           DO 300 K=1,NPART
               DTCDX(K) = (TCL(K)*WFL-TCU(K)*WFU)/DX
               IF(I.EQ.2.AND.X(1).EQ.0.0.AND.DTCDX(K).LT.DLAT(K))
     1           DU(K)  = (PHI(K)/(1.0+PHI(K)))*(DTCDX(K)-DLAT(K))
               IF(DU(K).GT.0.0d0) NT = NT+1
               CHECKU   = TCU(K)*WFU
               IF(CHECKU.GT.GSTU(K)) NK = NK+1
 300       CONTINUE
           IF(NK.EQ.NPART) GO TO 540
           IF(NT.EQ.NPART) GO TO 540
           NZ     = 0
           DO 330 K=1,NPART
               COEFF    = 1.0d0
               XRAT     = X(I-1)/X(I)
               EXPON    = PHI(K)+1.0d0
               CALL UNDFLO(COEFF,XRAT,EXPON)
               DL(K)    = (PHI(K)/(1.0+PHI(K)))*(DTCDX(K)-DLAT(K))*
     1                    (1.0-XRAT**EXPON)
               COEFF    = dABS(DU(K))
               CALL UNDFLO(COEFF,XRAT,EXPON)
               DL(K)    = DL(K)+(DU(K)*XRAT**EXPON)
               IF(DL(K).GT.0.0) GO TO 310
               NZ       = NZ+1
               XDE(K)   = X(I)
               DDE(K)   = DL(K)
               GSDE(K)  = TCL(K)-(DL(K)*X(I)/PHI(K))/WFL
               GSTDE(K) = GSDE(K)*WFL
               GSL(K)   = GSDE(K)
               GO TO 330
 310           CONTINUE
               IF(DU(K).EQ.0.0d0) GO TO 320
               if (DTCDX(K)-DLAT(K).eq.0.0d0) GO TO 320  !need to check if we can goto 320 to avoid dividing by zero error, Liwang Ma, 1-19-2013
               aa=((1.0+PHI(K))/PHI(K))
               bb=(DU(K)/(DTCDX(K)-DLAT(K)))
               if (1.0d0-aa*bb.lt.0.0d0) go to 320   !!need to check if we can goto 320 to avoid taking square root of a negative value, Liwang Ma, 1-19-2013
c
               XDE(K)   = X(I-1)*(1.0d0-((1.0+PHI(K))/PHI(K))*
     1                    (DU(K)/(DTCDX(K)-DLAT(K))))**
     1                    (1.0d0/(1.0d0+PHI(K)))
               GSTDE(K) = DTCDX(K)*(XDE(K)-X(I-1))+TCU(K)*WFU
               DDE(K)   = 0.0d0
               GO TO 330
 320           CONTINUE
               XDE(K)   = X(I-1)
               GSDE(K)  = GSU(K)
               GSTDE(K) = GSDE(K)*WFU
               DDE(K)   = 0.0d0
 330       CONTINUE
           IF(NZ.EQ.NPART) GO TO 720
           XDEMAX = X(I-1)
           DO 340 K=1,NPART
             XDEMAX   = dMAX1(XDEMAX,XDE(K))
 340       CONTINUE
           IF(XDEMAX.LT.X(I)) GO TO 360
           DO 350 K=1,NPART
               IF(XDE(K).EQ.X(I)) GO TO 350
               DL(K)    = 0.0d0
               GSL(K)   = (GSTDE(K)+DLAT(K)*(X(I)-XDE(K)))/WFL
 350       CONTINUE
           GO TO 720
 360       CONTINUE
           DO 370 K=1,NPART
               DDE(K)   = 0.0d0
               GSTDE(K) = GSTDE(K)+DLAT(K)*(XDEMAX-XDE(K))
 370       CONTINUE
 380       CONTINUE
           MAXE   = 1000.0d0
           EXCESS = 1.0d0
           IF(ELEM.GT.1) GO TO 390
           CALL RILL(X(I),SLOPE(I),KEROD,CROP,PRACT,MAXE,EXCESS,DF)
           GO TO 400
 390       CONTINUE
           TEMDEP = DEPA(INDXC,I)
           TEMWER = WERA(INDXC,I)
           FLAGC  = FLAGCT
           IF(FLAGC.EQ.3.AND.DEPA(INDXC,I).EQ.0.0) FLAGC = 2
           CALL DCAP(1,FLAGC,QLE,SFL,C1,Z,EFFSHL,DEPSID,TEMDEP,TEMWER,
     1               WFL,NMANCH,CRSH,COVSH,MAXE,EXCESS,TB,DF)
 400       CONTINUE
           DO 410 K=1,NPART
               DL(K)    = DF(K)*WFL
               POTLD(K) = (GSTDE(K)+DLAT(K)*(X(I)-XDEMAX)+
     1                    ((DL(K)+DDE(K))*(X(I)-XDEMAX)/2.0))/WFL
 410       CONTINUE
           CALL TRNCAP(EFFSHL,TCL,POTLD)
           NT2    = 0
           DO 420 K=1,NPART
               IF(TCL(K).GE.POTLD(K)) NT2 = NT2+1
 420       CONTINUE
           IF(NT2.EQ.NPART) GO TO 500
           SUMTCL = 0.0d0
           SUMPLD = 0.0d0
           DO 430 K=1,NPART
               SUMTCL = SUMTCL + TCL(K)
               SUMPLD = SUMPLD + POTLD(K)
 430       CONTINUE
           DO 440 K=1,NPART
               EXDET(K) = ((TCL(K)*WFL-GSTDE(K)-DLAT(K)*(X(I)-XDEMAX))*
     1                    (2.0d0/(X(I)-XDEMAX))-DDE(K))/WFL
 440       CONTINUE
      SUMDF=0.0d0
      SUMEXD=0.0d0
      DO 442 K=1,NPART
        SUMDF=SUMDF+DF(K)
        SUMEXD=SUMEXD+EXDET(K)
 442  CONTINUE
      MAXE   = 1000.0d0
      EXCESS = SUMTCL/SUMPLD
      EXCOLD=EXCESS
      NEXCES=0
 452  CONTINUE
      NEXCES=NEXCES+1
      IF(NEXCES.GT.20d0)GO TO 494
 453  CONTINUE
           IF(EXCESS.LT.0.0d0) EXCESS = 0.0d0
           IF(ELEM.GT.1) GO TO 460
           CALL RILL(X(I),SLOPE(I),KEROD,CROP,PRACT,MAXE,EXCESS,DF)
           GO TO 470
 460       CONTINUE
           TEMDEP = DEPA(INDXC,I)
           TEMWER = WERA(INDXC,I)
           CALL DCAP(2,FLAGC,QLE,SFL,C1,Z,EFFSHL,DEPSID,TEMDEP,
     1               TEMWER,WFL,NMANCH,CRSH,COVSH,MAXE,
     1               EXCESS,TB,DF)
 470       CONTINUE
           EXCESS = 1.0d0
           MAXE   = 1000.0d0
           DO 480 K=1,NPART
               DL(K)    = DF(K)*WFL
               GSL(K)   = (GSTDE(K)+DLAT(K)*(X(I)-XDEMAX)+((DL(K)+
     1                    DDE(K))*(X(I)-XDEMAX)/2.0d0))/WFL
               POTLD(K) = GSL(K)
 480       CONTINUE
           CALL TRNCAP(EFFSHL,TCL,POTLD)
      SUMTCL=0.0d0
      SUMPLD=0.0d0
      SUMDF=0.0d0
      SUMEXD=0.0d0
           DO 490 K=1,NPART
      SUMTCL=SUMTCL+TCL(K)
      SUMPLD=SUMPLD+POTLD(K)
               EXDET(K) = ((TCL(K)*WFL-GSTDE(K)-DLAT(K)*(X(I)-XDEMAX))*
     1                    (2.0d0/(X(I)-XDEMAX))-DDE(K))/WFL
      SUMEXD=SUMEXD+EXDET(K)
      SUMDF=SUMDF+DF(K)
 490       CONTINUE
      IF(ABS(SUMTCL-SUMPLD)/SUMTCL.LT.0.01)GO TO 494
      RATEX=SUMEXD/SUMDF
      IF(RATEX.LE.0.0d0)RATEX=SUMTCL/SUMPLD
      EXCESS=EXCOLD*RATEX
      EXCOLD=EXCESS
      GO TO 452
 494  CONTINUE
      DO 495 K=1,NPART
                GSL(K) = TCL(K)
 495  CONTINUE
           GO TO 530
 500       CONTINUE
           DO 520 K=1,NPART
               GSL(K)   = POTLD(K)
 520       CONTINUE
 530       CONTINUE
           IF(ELEM.EQ.1) GO TO 720
           DEPA(INDXC,I) = TEMDEP
           WERA(INDXC,I) = TEMWER
           FLAGC  = FLAGCT
           IF(FLAGC.EQ.3.AND.DEPA(INDXC,I).EQ.0.0) FLAGC = 2
           IF(FLAGC.EQ.2.AND.WERA(INDXC,I).GT.WFL)
     1       WIDA(INDXC,I) = WERA(INDXC,I)
           GO TO 720
 540       CONTINUE
      NT=0
      DO 542 K=1,NPART
      IF(TCL(K).LE.POTLD(K))NT=NT+1
 542  CONTINUE
      IF(NT.EQ.NPART)GO TO 630
           DO 550 K=1,NPART
               POTLD(K) = (GSTU(K)+DLAT(K)*DX+DU(K)*DX/2.0)/WFL
 550       CONTINUE
           CALL TRNCAP(EFFSHL,TCL,POTLD)
           NT     = 0
           DO 560 K=1,NPART
               DTCDX(K) = (TCL(K)*WFL-TCU(K)*WFU)/DX
      IF(TCL(K).LE.POTLD(K))NT=NT+1
 560       CONTINUE
           IF(NT.EQ.NPART) GO TO 590
 570       CONTINUE
           DO 580 K=1,NPART
               DDE(K)   = DU(K)
               XDEMAX   = X(I-1)
               GSDE(K)  = GSU(K)
               GSTDE(K) = GSDE(K)*WFU
 580       CONTINUE
           GO TO 380
 590       CONTINUE
      XDSMAL=X(I-1)
      XDBIG=X(I)
      NDEP=0
 599  CONTINUE
           DO 610 K=1,NPART
               IF(POTLD(K).LE.TCL(K)) GO TO 600
      XDBEG(K)=(2.0*(TCL(K)*WFL-GSTU(K)-DLAT(K)*DX)/DU(K))+X(I-1)
               GO TO 610
 600           CONTINUE
               XDBEG(K) = X(I)
 610       CONTINUE
           XDBMIN = X(I)
           DO 620 K=1,NPART
               XDBMIN   = dMIN1(XDBMIN,XDBEG(K))
 620       CONTINUE
      IF(XDBMIN.LE.XDSMAL)XDBMIN=XDSMAL
 624  CONTINUE
      DO 625 K=1,NPART
      POTLD(K)=(GSTU(K)+DLAT(K)*DX+DU(K)*(XDBMIN-X(I-1))/2.0d0)/WFL
 625  CONTINUE
      CALL TRNCAP(EFFSHL,TCL,POTLD)
      NT=0
      SUMTC=0.0d0
      SUMPL=0.0d0
      NDEP=NDEP+1
      IF(NDEP.EQ.4)GO TO 6291
      DO 627 K=1,NPART
      SUMTC=SUMTC+TCL(K)
      SUMPL=SUMPL+POTLD(K)
      IF(TCL(K).LE.POTLD(K))NT=NT+1
 627  CONTINUE
      IF(ABS((SUMTC-SUMPL)/SUMTC).LT.0.01)GO TO 629
      IF(NT.EQ.NPART)GO TO 628
      XDSMAL=XDBMIN
      XDBMIN=(XDSMAL+XDBIG)/2.0d0
      GO TO 624
 628  CONTINUE
      XDBIG=XDBMIN
      GO TO 599
 629  CONTINUE
6292  CONTINUE
      DO 6290 K=1,NPART
      DL(K)=0.0d0
      GSL(K)=TCL(K)
6290  CONTINUE
      GO TO 720
6291  CONTINUE
      GO TO 6292
 630  CONTINUE
      NZ=0
      NK=0
      DO 632 K=1,NPART
      IF(DU(K).GT.0.0)NZ=NZ+1
      IF(GSU(K).EQ.TCU(K))NK=NK+1
 632  CONTINUE
      IF(NZ.EQ.NPART.AND.NK.EQ.NPART)GO TO 640
      DO 635 K=1,NPART
      IF(TCL(K).GE.POTLD(K))GO TO 635
      XDBEG(K)=((TCU(K)*WFU-GSTU(K))/(DU(K)/2.0+DLAT(K)-DTCDX(K)))+
     2   X(I-1)
 635  CONTINUE
      GO TO 690
 640  CONTINUE
      DO 645 K=1,NPART
      IF(TCL(K).GE.POTLD(K))GO TO 645
      XDBEG(K)=DX*DU(K)/(2.0d0*DLAT(K)+DU(K))+X(I-1)
 645  CONTINUE
 690       CONTINUE
           DO 710 K=1,NPART
               IF(POTLD(K).LE.TCL(K)) GO TO 700
               COEFF    = 1.0d0
               XRAT     = XDBEG(K)/X(I)
               EXPON    = PHI(K)+1.0d0
               CALL UNDFLO(COEFF,XRAT,EXPON)
               DL(K)    = (PHI(K)/(1.0+PHI(K)))*(DTCDX(K)-DLAT(K))*
     1                    (1.0-XRAT**EXPON)
               GSL(K)   = TCL(K)-DL(K)*X(I)/(PHI(K)*WFL)
               GO TO 710
 700           CONTINUE
               GSL(K)   = POTLD(K)
               DL(K)    = 0.0d0
 710       CONTINUE
 720       CONTINUE
           DO 730 K=1,NPART
               GSTL(K)  = GSL(K)*WFL
 730       CONTINUE
 740   CONTINUE
       SOLOSS = 0.0d0
       DO 750 K=1,NPART
           CONC(K) = GSTL(K)/QE
           GS(K)   = RUNOFF*DAL*CONC(K)
           SOLOSS  = SOLOSS+GS(K)/DAL
 750   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE MapChannel(Indx,FLAGCT ,FLAGS,CTLO,CTLZ,CTLN,CTLSL,
     &              RA,RN,YBASE,DAU,DAL,Z,LEFF,ENDMAN,ICHAN,CRSH,
     &              DEPSID,NMANCH,EATA,INDXC,Date)
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      integer ctlo,flagct,flags,Date
      double precision leff,nmanch
      If (Indx.Eq.1) then
         FLAGCT = FLAGC1
         FLAGS  = FLAGS1
         CTLO   = CTLO1
         CTLZ   = CTLZ1
         CTLN   = CTLN1
         CTLSL  = CTLSL1
         RA     = RA1
         RN     = RN1
         YBASE  = YBASE1
         DAU    = DACHU1
         DAL    = DACHL1
         Z      = Z1
         LEFF   = LEFF1
         ENDMAN = NCHN1(NXC1,DATE)
         ICHAN  = 1
         CRSH   = CCHN1(ICHAN,DATE)
         DEPSID = SCHN1(ICHAN,DATE)
         NMANCH = NCHN1(ICHAN,DATE)
         EATA   = 1.0
         INDXC  = 1
       else
         FLAGCT = FLAGC2
         FLAGS  = FLAGS2
         CTLO   = CTLO2
         CTLZ   = CTLZ2
         CTLN   = CTLN2
         CTLSL  = CTLSL2
         RA     = RA2
         RN     = RN2
         YBASE  = YBASE2
         DAU    = DACHU2
         DAL    = DACHL2
         Z      = Z2
         LEFF   = LEFF2
         ENDMAN = NCHN2(NXC2,DATE)
         ICHAN  = 1
         CRSH   = CCHN2(ICHAN,DATE)
         DEPSID = SCHN2(ICHAN,DATE)
         NMANCH = NCHN2(ICHAN,DATE)
         EATA   = 1.0
         INDXC  = 2
       endif
       RETURN
       END
C======================================================================
       SUBROUTINE FSLPAR(CTLO,CTLZ,CTLN,CTLSL,RA,RN,
     1                   YBASE,Z,ENDMAN,LEFF,IBEG,FLAG1,
     1                   C1,C3,ENDSLP,SSFE)
C======================================================================
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       COMMON /FLOWInt/ FLAGS
       Common/Flow/QB,QE,YCR,YNOR,YE,SFE,RUNPND
       double precision Leff,NSFE
       INTEGER FLAGS,FLAG1,CTLO
       save
       IBEG   = 1
       C1     = (Z**2.5d0/(2.0d0*DSQRT(Z**2.0d0+1.0d0)))**(2.0d0/3.0d0)
       YNOR   = UNIFOR(ENDSLP,Z,ENDMAN,QE)
       YCR    = CRIT(Z,QE)
       YE     = YNOR
       IF (FLAGS.EQ.2) RETURN
       IF (CTLO.EQ.1) YTR = YCR
       IF (CTLO.EQ.2) YTR = UNIFOR(CTLSL,CTLZ,CTLN,QE)
       IF (CTLO.EQ.3) YTR = UNIFOR(CTLSL,CTLZ,ENDMAN,QE)
       IF (CTLO.EQ.4) YTR = YBASE+(QE/RA)**(1.0d0/RN)
       IF (YNOR.GT.YCR) GO TO 20
       FLAGS  = 2
       IF (CTLO.GT.1.AND.YTR.GT.YNOR) GO TO 10
       YE     = YNOR
       SFE    = ENDSLP
       RETURN
  10   CONTINUE
       YE     = YTR
       SFE    = FSLQ(QE,ENDMAN,C1,YE)
       RETURN
  20   CONTINUE
       FLAGS  = 1
       IF (CTLO.EQ.1) GO TO 30
       IF (YTR.LT.YNOR) YTR = YNOR
       NSFE   = ENDMAN
       GO TO 40
  30   CONTINUE
       NSFE   = NBARCH
  40   CONTINUE
       FLAG1  = 5
       YE     = YTR
       SFE    = FSLQ(QE,NSFE,C1,YE)
       SSFE   = (ENDSLP-SFE)*LEFF/YE
       C3     = 2.0d0*BETA_ero*QE**2.0d0/(AGRAV*Z**2.0d0*YE**5.0d0)
       RETURN
       END
C======================================================================
       Double Precision FUNCTION FSLQ(Q,N,C1,Y)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision N
       FSLQ = (Q*N/(1.49d0*C1*Y**(8.0d0/3.0d9)))**2.0d0
       RETURN
       END
C======================================================================
       SUBROUTINE FRICHN(Q,N,Z,C1,C3,I,FLAG1,SLOPE,XSTARi,LEFF,YE,
     1                   XBEG,IBEG,SSFB,SSFE,SF)
C======================================================================
c equation I-117-127 for spatially varied flow equations
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 50/51
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision LEFF,INTRPL,N
       INTEGER FLAG1
       save
       YCRCH  = CRIT(Z,Q)
       SC     = FSLQ(Q,N,C1,YCRCH)
       IF (SLOPE.LT.SC) GO TO 10
       SF     = SLOPE
       RETURN
  10   CONTINUE
       SSTAR  = SLOPE*LEFF/YE
       IF (FLAG1.EQ.1) GO TO 20
       IF (FLAG1.EQ.2) GO TO 50
       IF (FLAG1.EQ.3) GO TO 90
       IF (FLAG1.EQ.4) GO TO 120
       IF (C3.LT.0.3) GO TO 40
  20   CONTINUE
       FLAG1  = 1
       IF (XSTARi.LE.0.90001d0) GO TO 30
       SSF    = INTRPL(XBEG,SSFB,1.0d0,SSFE,XSTARi)
       GO TO 150
  30   CONTINUE
       IF (SSTAR.GE.0.0.AND.SSTAR.LE.1.2)
     1   SSF  = 0.2777d0-3.3110d0*XSTARi+9.1683d0*XSTARi**2-
     1          8.9551d0*XSTARi**3
       IF (SSTAR.GT.1.2.AND.SSTAR.LE.4.8)
     1   SSF  = 2.6002d0-8.0678d0*XSTARi+15.6502d0*XSTARi**2-
     1          11.7998d0*XSTARi**3
       IF (SSTAR.GT.4.8.AND.SSTAR.LE.20.0)i
     1   SSF  = 3.8532d0-12.9501d0*XSTARi+21.1788d0*XSTARi**2-
     1          12.1143d0*XSTARi**3
       IF (SSTAR.GT.20.0) SSF = 0.0
       IBEG   = I
       SSFB   = SSF
       GO TO 150
  40   CONTINUE
       FLAG1  = 2
       IF (.NOT.(C3.LT.0.3.AND.C3.GE.0.03)) GO TO 80
  50   CONTINUE
       IF (SSTAR.EQ.0.0) GO TO 70
       IF (XSTARi.LE.0.80001) GO TO 60
       SSF    = INTRPL(XBEG,SSFB,1.0d0,SSFE,XSTARi)
       GO TO 150
  60   CONTINUE
       SSF    = 2.0553d0-6.9875d0*XSTARi+11.4184d0*XSTARi**2-
     1          6.4588d0*XSTARi**3
       IBEG   = I
       SSFB   = SSF
       GO TO 150
  70   CONTINUE
       SSF    = 0.0392d0-0.4774d0*XSTARi+1.0775d0*XSTARi**2-
     1          1.3694d0*XSTARi**3
       IBEG   = I
       SSFB   = SSF
       GO TO 150
  80   CONTINUE
       FLAG1  = 3
       IF (.NOT.(C3.LT.0.03.AND.C3.GE.0.007)) GO TO 120
  90   CONTINUE
       IF (SSTAR.EQ.0.0) GO TO 110
       IF (XSTARi.LE.0.80001) GO TO 100
       SSF    = INTRPL(XBEG,SSFB,1.0d0,SSFE,XSTARi)
       GO TO 150
 100   CONTINUE
       SSF    = 1.5386d0-5.2042d0*XSTARi+8.4477d0*XSTARi**2-
     1          4.7401d0*XSTARi**3
       IBEG   = I
       SSFB   = SSF
       GO TO 150
 110   CONTINUE
       SSF    = 0.0014d0-0.0162d0*XSTARi-0.0926d0*XSTARi**2-
     1          0.0377d0*XSTARi**3
       IBEG   = I
       SSFB   = SSF
       GO TO 150
 120   CONTINUE
       FLAG1  = 4
       IF (SSTAR.EQ.0.0) GO TO 140
       IF (XSTARi.LE.0.70001) GO TO 130
       SSF    = INTRPL(XBEG,SSFB,1.0d0,SSFE,XSTARi)
       GO TO 150
 130   CONTINUE
       SSF    = 1.2742d0-4.7020d0*XSTARi+8.4755d0*XSTARi**2-
     1          5.3332d0*XSTARi**3
       IBEG   = I
       SSFB   = SSF
       GO TO 150
 140   CONTINUE
       SSF    = -0.0363d0*XSTARi**2
       IBEG   = I
       SSFB   = SSF
 150   CONTINUE
       SFSTAR = SSTAR-SSF
       SF     = SFSTAR*YE/LEFF
       RETURN  
       END
C======================================================================
       Double Precision FUNCTION CRIT(Z,Q)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       save
       CRIT = (2.0d0*BETA_ero*Q**2/(AGRAV*Z**2))**0.2d0
       RETURN
       END
C======================================================================
       SUBROUTINE CURFIT(XU,YU,SU,XL,YL,SL,NPT,XC,YC,SC)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision XC(NPT),YC(NPT),SC(NPT)
       save
       SA = (SU+SL)/2.0d0
       A1 = -SU/SA
       A2 = ((-SL/SA)-A1)/2.0d0
       DO 10 I=1,NPT
           XS    = dFLOAT(I)/dFLOAT(NPT+1)
           XC(I) = XU+XS*(XL-XU)
           YS    = 1.0d0+A1*XS+A2*XS**2
           YC(I) = YL+YS*(YU-YL)
           SS    = A1+2*A2*XS
           SC(I) = dABS(SA*SS)
  10   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE INRILL(SLOPE,KEROD,CROP,PRACT,DLAT)
C======================================================================
c equation I-88 for interrill detachment from modified USLE
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 44
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision KEROD,DLAT(10)
       save
       DETINR = (0.04597d0/6.574d0)*(EI*(30.0d0*SLOPE+0.43d0))*
c     &             EXRAIN/43200d0*KEROD*CROP*PRACT/RUNOFF   !should convert to ft/s?
     &             EXRAIN*KEROD*CROP*PRACT/RUNOFF            !EXRAIN in ft/s
       DO 10 K=1,NPART
           DLAT(K) = DETINR*FRAC(K)
  10   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE SHOVR(Q,SF,NMANOV,EFFSH)
C======================================================================
c equation I-112 for shear stress calculation
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 44
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision NMANOV
       save
       R     = (Q*NBAROV/(1.49d0*DSQRT(SF)))**0.6d0
       EFFSH = WTDH2O*R*SF*((NBAROV/NMANOV)**0.9d0)
       RETURN
       END
C======================================================================
       SUBROUTINE HYDCHN(FLAGC,Q,SF,C1,Z,WB,W,N,CRSH,COVSH,EFFSH)
C======================================================================
C      COMPUTE HYDRAULICS IN CHANNELS
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision N,NT,LC,MULSH,XLC(16),FGLC(16),XXB(27),FHXB(27)
       INTEGER FLAGC
       save
       DATA XLC /0.0d0,0.01d0,0.02d0,0.04d0,0.06d0,0.08d0,0.1d0,0.12d0,
     1           0.14d0,0.16d0,0.18d0,0.2d0,0.22d0,0.24d0,0.26d0,0.28d0/
       DATA FGLC /100000.0d0,32.91d0,15.487d0,7.307d0,4.849d0,3.713d0,
     1           3.075d0,2.676d0,2.408d0,2.222d0,2.089d0,1.994d0,
     1           1.928d0,1.884d0,1.858d0,1.84866d0/
       DATA XXB /0.0d0,0.01d0,0.02d0,0.04d0,0.06d0,0.08d0,0.1d0,
     1           0.12d0,0.14d0,0.16d0,0.18d0,0.2d0,0.22d0,0.24d0,0.26d0,
     1           0.28d0,0.3d0,0.32d0,0.34d0,0.36d0,0.38d0,0.4d0,0.42d0,
     1           0.44d0,0.46d0,0.48d0,0.5d0/
       DATA FHXB /0.0d0,.000474d0,.00154d0,.00509d0,.0104d0,.0177d0,
     1           .0269d0,.0384d0,.0524d0,.0693d0,.0897d0,.114d0,.1432d0,
     1           .1782d0,.2207d0,.2724d0,.3361d0,.4159d0,.5176d0,
     1            .6506d0,.8307d0,1.0858d0,1.4722d0,2.1212d0,3.4264d0,
     1            7.3566d0,10000.0d0/
       IF (FLAGC.EQ.2) GO TO 20
       IF (FLAGC.GE.3) GO TO 30
  10   CONTINUE
C
C      TRIANGULAR SECTION
C
       AP    = (Q*N/(1.49d0*dSQRT(SF)))**0.375d0
       Y     = AP/(C1**0.375d0)
       W     = 2.0*Y*Z
       A     = Z*(Y**2)
       NT    = N
       GO TO 40
  20   CONTINUE
C
C      RECTANGULAR SECTION
C
       IF (WB.EQ.0.0) GO TO 10
       AP    = (Q*N/(1.49d0*dSQRT(SF)))**0.375d0
       W     = WB
       HXB   = (AP/W)**(8.0d0/3.0d0)
       IF (HXB.GT.0.114) GO TO 25
       XBO   = 0.2d0
       DO 23 M=1,25
          XBN    =((1.0d0-2.0d0*XBO)*HXB)**0.6d0
          DIF    =ABS((XBN-XBO)/XBN)
          IF (DIF.LE. 0.001) GO TO 24
          XBO    =XBN
 23    CONTINUE
       IF (M.LT.25) GO TO 24
       PRINT *, "CALLING LogMessage"
       Call LogMessage('  *****DID NOT CONVERGE IN RECTANGULAR SECTION,'
     1    //' CALL GEORGE FOSTER FOR HELP.******')
 24    CONTINUE
       XB       =XBN
       GO TO 27
 25    CONTINUE
       CALL TABLE(4,27,XXB,FHXB,HXB,XB)
 27    CONTINUE
       Y     = W*XB/(1.0d0-2.0d0*XB)
       A     = Y*W
       NT    = N
       GO TO 40
  30   CONTINUE
C
C      NATURALLY ERODED SECTION
C
       AP    = (Q*NBARCH/(1.49d0*dSQRT(SF)))**0.375d0
       GLC   = AP*WTDH2O*SF/CRSH
       IF (GLC.LT.1.84866d0) GO TO 50
       CALL TABLE(3,16,XLC,FGLC,GLC,LC)
       RSTAR = (-0.34707d0*(0.5d0-LC)**3)-(0.54213d0*(0.5-LC)**2)+
     1         (0.66383d0*(0.5d0-LC))
       W     = (AP/RSTAR**0.625d0)*(0.73d0-1.46d0*LC)
       IF (FLAGC.EQ.4) GO TO 60
       HXB   = (AP/W)**(8.0d0/3.0d0)
       CALL TABLE(4,27,XXB,FHXB,HXB,XB)
       Y     = W*XB/(1.0d0-2.0d0*XB)
       A     = Y*W
       NT    = NBARCH
  40   CONTINUE
C
C      COMPUTATION OF SHEAR STRESS COMPONENTS
C
       V     = Q/A
       RSH   = (V*NBARCH/(1.49d0*dSQRT(SF)))**(1.5d0)
       RCOV  = (V*(NT-NBARCH)/(1.49d0*dSQRT(SF)))**(1.5d0)
       EFFSH = WTDH2O*RSH*SF
       MULSH = WTDH2O*RCOV*SF
       IF (MULSH.LT.COVSH) GO TO 60
       N     = NBARCH
       NT    = N
       IF (FLAGC.EQ.2) GO TO 20
       IF (FLAGC.GE.3) GO TO 30
       GO TO 10
  50   CONTINUE
       IF (WB.EQ.0.0d0) GO TO 10
       GO TO 20
  60   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE RILL(X,SLOPE,KEROD,CROP,PRACT,MAXE,EXCESS,DF)
C======================================================================
c equation I-89 for interrill detachment from modified USLE
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 44
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision KEROD,MAXE,DF(10)
       save
       EXPO   = 2.0d0
       IF(X.GT.75.0d0) EXPO = 1.0d0+1.0245d0*dEXP(-0.0002595d0*X)
       DETRIL = (0.04597d0*EXPO/6.574d0)*(12632.0d0*RUNOFF*EXRAIN**
     &     (1.0d0/3.0d0))*(430.0d0*SLOPE**2)*((X/72.6d0)**(EXPO-1.0d0))
     &     *EXRAIN*KEROD* CROP*PRACT/RUNOFF
       IF(DETRIL.GT.MAXE) DETRIL = MAXE
       DETRIL = EXCESS*DETRIL
       DO 10 K=1,NPART
           DF(K) = DETRIL*FRAC(K)
  10   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE DCAP(FLAGM,FLAGT,Q,SF,C1,Z,EFFSH,DEPSID,DEPMID,WEROD,
     1                 WFLOW,N,CRSH,COVSH,MAXE,EXCESS,TB,DF)
C======================================================================
c equation I-128--143 for detachment capacity 
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 44
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision N,DF(10),MAXE,XXCF(17),FFXCF(17),XXB(27),
     &                  FHXB(27)
       INTEGER FLAGT,FLAGM
       save
       DATA XXCF /0.0d0,0.02d0,0.04d0,0.06d0,0.08d0,0.10d0,0.12d0,
     &            0.14d0,0.16d0,0.18d0,0.20d0,0.22d0,0.24d0,0.26d0,
     &            0.28d0,0.30d0,0.32d0/
       DATA FFXCF/1000.0d0,33.872d0,12.571d0,7.3030d0,5.1102d0,
     1            3.9575d0,3.2659d0,2.8419d0,2.5040d0,2.2818d0,
     1            2.1194d0,1.9997d0,1.9118d0,1.8489d0,1.8068d0,
     1            1.7829d0,1.7758d0/
       DATA XXB  /0.0d0,0.01d0,0.02d0,0.04d0,0.06d0,0.08d0,0.1d0,
     1           0.12d0,0.14d0,0.16d0,0.18d0,0.2d0,0.22d0,0.24d0,0.26d0,
     1            0.28d0,0.3d0,0.32d0,0.34d0,0.36d0,0.38d0,0.4d0,0.42d0,
     1            0.44d0,0.46d0,0.48d0,0.5d0/
       DATA FHXB/0.0d0,0.000474d0,0.00154d0,0.00509d0,0.0104d0,0.0177d0,
     1            0.0269d0,0.0384d0,0.0524d0,0.0693d0,0.0897d0,0.114d0,
     &            0.1432d0,0.1782d0,0.2207d0,0.2724d0,0.3361d0,0.4159d0,
     &            0.5176d0,0.6506d0,0.8307d0,1.0858d0,1.4722d0,2.1212d0,
     &            3.4264d0,7.3566d0,10000.0d0/
       TIMPOT = 0.0
       ADJSH  = 1.35d0*EFFSH
       IF (ADJSH.GT.CRSH) GO TO 30
  10   CONTINUE
       DO 20 K=1,NPART
           DF(K) = 0.0d0
  20   CONTINUE
       DCT    = 0.0d0
       RETURN
  30   CONTINUE
       TIMSH  = TB*(1.0d0-(CRSH/(1.35d0*EFFSH)))
       IF (DEPMID.EQ.0.0) GO TO 90
       IF (FLAGT.EQ.3) GO TO 40
       CALL HYDCHN(4,Q,SF,C1,Z,WFLOW,WEROD,N,CRSH,COVSH,EFFSH)
       GO TO 50
  40   CONTINUE
       WEROD  = WFLOW
  50   CONTINUE
       DIFSH  = 1.35d0*EFFSH-CRSH
       IF (DIFSH.LE.0.0d0) GO TO 10
       DI     = EXCESS*(KCH*((1.35d0*EFFSH-CRSH)**1.05d0))
       TIMPOT = DEPMID*WTDSOI/DI
       IF (TIMPOT.LT.TIMSH) GO TO 90
  60   CONTINUE
       DCT    = DI*TIMSH*WEROD/(TB*WFLOW)
       IF (FLAGM.EQ.1) GO TO 70
       IF (DCT.LT.MAXE) GO TO 70
       DI     = DI*MAXE/DCT
       DCT    = MAXE
  70   CONTINUE
       DO 80 K=1,NPART
           DF(K) = DCT*FRAC(K)
  80   CONTINUE
       DEPMID = DEPMID-DI*TIMSH/WTDSOI
       IF (DEPMID.LT.0.005d0) DEPMID= 0.0d0
       RETURN
  90   CONTINUE
       TIMEX  = TIMSH-TIMPOT
       AB     = (Q*NBARCH/(1.49d0*dSQRT(SF)))
       IF (WEROD.EQ.0.0d0) CALL HYDCHN(4,Q,SF,C1,Z,WFLOW,WEROD,N,CRSH,
     1                              COVSH,EFFSH)
       HXB    = AB/(WEROD**(8.0d0/3.0d0))
       CALL TABLE(4,27,XXB,FHXB,HXB,XB)
       DIFSH  = EFFSH*SHDIST(XB)-CRSH
       IF (DIFSH.GT.0.0) GO TO 100
       IF (DEPMID.LE.0.0) GO TO 10
       TIMSH  = TIMPOT
       GO TO 60
 100   CONTINUE
       DWDTI  = EXCESS*2.0d0*KCH*(DIFSH**1.05d0)/WTDSOI
       AD     = (AB**0.375d0)*WTDH2O*SF/CRSH
       IF (AD.LE.1.7758d0)GO TO 10
       CALL TABLE(3,17,XXCF,FFXCF,AD,XCF)
       WFIN   = (AB**0.375d0)*((XCF*(1.0d0-2.0d0*XCF)/XCF**
     &              (8.0d0/3.0d0))**0.375d0)
       IF (WFIN.LE.WEROD) GO TO 10
       TSTAR  = TIMEX*DWDTI/(WFIN-WEROD)
       WSTAR  = (1.0d0-dEXP(-1.0176d0*TSTAR))/1.0176d0
       WE     = WSTAR*(WFIN-WEROD)+WEROD
       EROS   = (WE-WEROD)*DEPSID+DEPMID*WEROD
       DCT    = EROS*WTDSOI/(TB*WFLOW)
       IF (FLAGM.EQ.1) GO TO 110
       IF (DCT.LT.MAXE) GO TO 110
       DCT    = MAXE
       EROS   = DCT*TB*WFLOW/WTDSOI
 110   CONTINUE
       DO 120 K=1,NPART
           DF(K) = DCT*FRAC(K)
 120   CONTINUE
       IF (EROS.LT.DEPMID*WEROD) GO TO 130
       EROSL  = EROS-DEPMID*WEROD
       WEROD  = EROSL/DEPSID+WEROD
       DEPMID = 0.0d0
       RETURN
 130   CONTINUE
       DEPMID = DEPMID-EROS/WEROD
       RETURN
       END
C======================================================================
       SUBROUTINE TRNCAP(EFFSH,TC,QS)
C======================================================================
c equation I-93-98 for calculating sediment transport capacity
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 44
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision QS(10),WS(10),WSQRAT(10),TC(10),
     1      COEF(10),YCRIT(10),DELTA(10),SIGMA(10),P(10),DLTRAT(10)
       INTEGER SNDX,FLAGD1,FLAGD2,FLAGD3
       save
       VSTAR       = dSQRT(EFFSH/MSDH2O)
       COEF(NPART) = VSTAR*AGRAV*MSDH2O
       fmd         = coef(npart)
       T           = 0.0d0
       DO 10 K=1,NPART
           fmd     = COEF(NPART)*DIA(K)*SPGR(K)
           COEF(K) = COEF(NPART)*DIA(K)*SPGR(K)
  10   CONTINUE
       DO 40 K=1,NPART
           fmd = qs(k)
           IF (QS(K).EQ.0.0d0) QS(K) = 1.0d-20
           REYN     = VSTAR*DIA(K)/KINVIS
           YCRIT(K) = SHIELD(REYN)
           DELTA(K) = (VSTAR**2/(SPGR(K)-1.0d0)/AGRAV/
     1                DIA(K)/YCRIT(K))-1.0d0
           fmd      = delta(k)
           IF (DELTA(K).GT.0.0d0) GO TO 20
           DELTA(K) = 0.0d0
           P(K)     = 0.0d0
           GO TO 30
  20       CONTINUE
           SIGMA(K) = DELTA(K)*2.45d0*SPGR(K)**(-0.4d0)*
     1                dSQRT(YCRIT(K))
           P(K)     = YALCON*DELTA(K)*(1.0d0-1.0d0/
     1                SIGMA(K)*dLOG(1.0d0+SIGMA(K)))
  30       CONTINUE
           fmds     = sigma(k)
           fmdp     = p(k)
           T        = T+DELTA(K)
  40   CONTINUE
       IF (T.EQ.0.0d0) T = 1000.0d0
       DO 50 K=1,NPART
           DLTRAT(K) = DELTA(K)/T
           WS(K)     = P(K)*DLTRAT(K)*COEF(K)
  50   CONTINUE
  60   CONTINUE
       FLAGD1 = 0
       FLAGD2 = 0
       FLAGD3 = 0
       DO 70 K=1,NPART
           WSQRAT(K) = WS(K)/QS(K)
           IF (WSQRAT(K).GT.1.0) FLAGD3 = FLAGD3+1
           IF (WSQRAT(K).GE.1.0) FLAGD1 = FLAGD1+1
           IF (WSQRAT(K).LE.1.0) FLAGD2 = FLAGD2+1
  70   CONTINUE
       IF (FLAGD2.EQ.NPART) GO TO 80
       IF (FLAGD3.EQ.NPART) GO TO 80
       IF (FLAGD1.EQ.NPART) GO TO 140
       GO TO 100
  80   CONTINUE
       DO 90 K=1,NPART
           TC(K) = WS(K)
  90   CONTINUE
       RETURN
 100   CONTINUE
       SMDRQT = 0.0d0
       SMDRAT = 0.0d0
       DO 120 K=1,NPART
           IF (WSQRAT(K).LT.1.0) GO TO 110
           fmdq   = qs(k)
           fmdc   = coef(k)
           fmdp   = p(k)
           SMDRQT = SMDRQT+QS(K)/COEF(K)/P(K)
           WS(K)  = QS(K)
           GO TO 120
 110       CONTINUE
           IF (WSQRAT(K).LT.1.0) SMDRAT = SMDRAT+DLTRAT(K)
 120   CONTINUE
       EXCAP = 1.0d0-SMDRQT
       DO 130 K=1,NPART
           IF (SMDRAT.EQ.0.0d0) SMDRAT = 1000000.0d0
           IF (WSQRAT(K).LT.1.0d0) WS(K) = DLTRAT(K)/SMDRAT*EXCAP*P(K)*
     1                                  COEF(K)
 130   CONTINUE
       GO TO 60
 140   CONTINUE
       SMDRAT = 0.0d0
       DO 150 K=1,NPART
           SMDRAT = SMDRAT+QS(K)/(COEF(K)*P(K))
 150   CONTINUE
       A = 1.0d0/SMDRAT
       DO 160 K=1,NPART
           TC(K) = A*QS(K)
 160   CONTINUE
       RETURN
       END
C======================================================================
       SUBROUTINE UNDFLO(COEFF,FACTOR,EXPON)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       save
       DATA POWER /30.0d0/
       IF (FACTOR.EQ.0.0d0) RETURN
       EXP10  = EXPON*DLOG10(FACTOR)
       IF (DABS(EXP10).GT.POWER) GO TO 10
       IF (COEFF.EQ.0.0d0) RETURN
       EXPROD = EXP10+DLOG10(COEFF)
       IF (ABS(EXPROD).GT.POWER) GO TO 10
       RETURN
  10   CONTINUE
       FACTOR = 0.0d0
       RETURN
       END
C======================================================================
       Double Precision FUNCTION UNIFOR(SLOPE,Z,N,Q)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       double precision SLOPE,Z,N,Q
       C1     = (Z**2.5d0/(2.0d0*dSQRT(Z**2+1.0d0)))**(2.0d0/3.0d0)
       UNIFOR = (Q*N/(C1*1.49d0*dSQRT(SLOPE)))**0.375d0
       RETURN
       END
C======================================================================
       SUBROUTINE STROUT(GS,CONC,AnArea,ENRICH)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
       COMMON /FLOWInt/ FLAGS
       Common/Flow/QB,QE,YCR,YNOR,YE,SFE,RUNPND
      double precision GS(10),CONC(10),Uconv1(0:1),Uconv2(0:1),
     &       Uconv3(0:1),Uconv4(0:1),Uconv5(0:1),Uconv6(0:1)
      Character*8 source
      integer Flags
      Character Element(4)*40,Unit1(0:1)*3,G12_5*12,FsLab*15,
     &          Unit2(0:1)*10,Unit3(0:1)*3,Unit4(0:1)*20,
     &          Header(-1:1)*80,Astr*255
       save
      Data Element/
     &  'VALUES FROM OVERLAND FLOW','VALUES FROM CHANNEL ONE',
     &  'VALUES FROM CHANNEL TWO','VALUES FROM IMPOUNDMENT'/
      Data FsLab/'FRICTION SLOPE '/
      Data Unit1/' in',' cm'/,Uconv1/1.0d0,CmPerInch/
      data Unit2/' ft**3/sec',' m**3/sec '/,Unit3/' ft',' m '/
      Data Unit4/' tons/acre',' tonnes/hectare'/
      Data Uconv2/1.0d0,0.02831d0/,Uconv3/1.0d0,FeetPerMeter/,
     &     Uconv4/1.0d0,2.242d0/,Uconv5/1.0d0,2.205d0/,
     &     Uconv6/1.0d0,16.01d0/
      Data Header/'TYPE   SED. LOAD      ',
     &  'LBS.      LBSF/FT**3    LBSF/LBSF     PPM (WT)',
     &  ' KG         KG/M**3       KG/KG       PPM (WT)'/
      IF (EROOUT.GE.4) then
        PRINT *, "CALLING LogMessage"
        Call LogMessage(Element(Elem))
        IF (EROOUT.NE.4) then
          IF (ELEM.EQ.4) THEN
            RUNPND = 12.0d0*RUNPND
            PRINT *, "CALLING LogMessage"
            Call LogMessage('  RUNOFF FROM IMPOUNDMENT='//
     &                 G12_5(RunPnd*Uconv1(MetFlg))//Unit1(MetFlg))  !2.54
           endif
          IF (ELEM.NE.1.AND.ELEM.NE.4) THEN
           IF (FLAGS.EQ.1) 
     &        Call LogMessage(FsLab//'FROM ENERGY GRADLINE CURVES')
           IF (FLAGS.EQ.2.AND.YE.LE.YNOR) 
     &        Call LogMessage(FsLab//'= CHANNEL SLOPE')
           IF (FLAGS.EQ.2.AND.YE.GT.YNOR) 
     &        Call LogMessage(FsLab//'= CHANNEL SLOPE EXCEPT AT END')
           PRINT *, "CALLING Logmessage"
           Call Logmessage('PEAK DISCHARGE UPPER END'//
     &                        G12_5(Qb*Uconv2(metflg))//Unit2(MetFlg))
           PRINT *, "CALLING Logmessage"
           Call Logmessage('PEAK DISCHARGE LOWER END'//
     &                        G12_5(Qe*Uconv2(metflg))//Unit2(MetFlg))
           PRINT *, "CALLING Logmessage"
           Call Logmessage('CRITICAL DEPTH          '//
     &                       G12_5(Ycr/Uconv3(metflg))//Unit3(MetFlg))
           PRINT *, "CALLING Logmessage"
           Call Logmessage('NORMAL DEPTH            '//
     &                      G12_5(Ynor/Uconv3(metflg))//Unit3(MetFlg))
           PRINT *, "CALLING Logmessage"
           Call Logmessage('CONTROL DEPTH           '//
     &                        G12_5(Ye/Uconv3(metflg))//Unit3(MetFlg))
           PRINT *, "CALLING Logmessage"
           Call Logmessage('FRICTION SLOPE AT END   ')
         endif ! Elem.NE.1
       endif
      endif
       TGS    = 0.0d0
       DO 20 K=1,NPART
   20     TGS = TGS + GS(K)
       SOLOSS = 43560.0d0/2000.0d0*TGS/AnArea 
       elmlos(elem) = Soloss*Uconv4(MetFlg)
       IF (TGS.LE.0.0d0) then
         PRINT *, "CALLING LogMessage"
         Call LogMessage('*** NO SOIL LOSS ***')
       ELSE IF (EROOUT.GT.4) then
         PRINT *, "CALLING Logmessage"
         Call Logmessage('THE QUANTITY OF ERODED SEDIMENT IN RUNOFF')
         PRINT *, "CALLING Logmessage"
         Call Logmessage('PART.   FRAC. IN   SOIL LOSS         CONC'//
     &                      'ENTRATIONS (SOIL/WATER)')
         PRINT *, "CALLING Logmessage"
         Call Logmessage(Header(-1)(1:30)//Header(Metflg))
         TCONC  = 0.0d0
         TCNCPC = 0.0d0
         TPPM   = 0.0d0
         DO 30 K=1,NPART
           CONCPC = CONC(K)/WTDH2O
           PPM    = CONCPC*1.0d+06
           FRACK(K)= GS(K)/TGS
           WRITE(Astr,'(7X,I2,6X,F5.2,3(4X,F9.4),4X,F10.0)') K,Frack(k),
     &         GS(K)/Uconv5(MetFlg),CONC(K)*Uconv6(Metflg),CONCPC,PPM
           PRINT *, "CALLING LogMessage"
           Call LogMessage(Astr)
           TCONC  = TCONC  + CONC(K)
           TCNCPC = TCNCPC + CONCPC
   30      TPPM   = TPPM   + PPM
         Write(Astr,'(A,17X,F9.0,3(4X,F10.4))')'Total',                   !2.205
     &     Tgs/Uconv5(metFlg),Tconc*Uconv6(MetFlg),Tcncpc,Tppm           !16.01 
         PRINT *, "CALLING LogMessage"
         Call LogMessage(Astr)
         PRINT *, "CALLING LogMessage"
         Call LogMessage('Average soil loss for area='//                 !2.242
     &        G12_5(SoLoss*Uconv4(Metflg))//Unit4(metflg))
         CALL ENRCMP(2,CONC,ENRICH)
         elmenr(elem) = enrich                                            
      else
        CALL ENRCMP(1,CONC,ENRICH)
         elmenr(elem) = enrich   
        PRINT *, "CALLING LogMessage"
        Call LogMessage('Average Soil Loss='//
     &      G12_5(Soloss*Uconv4(MetFlg))//Unit4(Metflg))
        PRINT *, "CALLING LogMessage"
        Call LogMessage('Enrichment Ratio='//G12_5(Enrich))
      ENDIF
c      IF (TGS.GT.0.0d0) THEN
      IF (RUNOFF.GT.0.0d0) THEN     !CHD Demar uses RUNOFF instead of TGS, why?
          RUNTMP = 12.0d0 * RUNOFF
          EXRTMP = 43200.0d0*EXRAIN  !*2.54     !Demar uses EXRTMP=2.54*exrain   !in/hr to cm/hr
           !EXRTMP=2.540d0*EXrain            !CHD version
          IF (ELEM.EQ.1) THEN
              source = 'overland'
          ELSEIF (ELEM.EQ.2) THEN
              source = 'channel1'
          ELSEIF (ELEM.EQ.3) THEN
              source = 'channel2'
          ELSEIF (ELEM.EQ.4) THEN
              source = 'impound '
          ENDIF
        ! chdTOD write line to output file
        ! changed to the same output as GLEAMS original outputs. Liwang 1-29-2015
        ! sdate, surface_h2o, runoff_vol, excess_rain, avg_soil_loss, [p1_frac, p1_soil_loss, p1_soilconc (cm^3/cm^3)] for p1-p5
          WRITE(654,9654) sdate, source, RnFall*CmPerInch, 
c     &       Runtmp*CmPerInch, Exrtmp  !*CmPerInch,  !Demar changes
c     &       Soloss*Uconv4(MetFlg),
c     &       (Frack(K), GS(K)/Uconv5(MetFlg), 
c     &       Conc(K)*Uconv6(MetFlg)*0.001/WTDSOI/62.4d0, K=1,5)
     &       Runtmp*CmPerInch, Exrtmp*CmPerInch,
     &       Soloss*Uconv4(Metflg)*1.0d3,                                  !kg/ha
c     &       Soloss*2000*12/(WTDSOI*RUNTMP*43560),
     &       tconc*Uconv6(metflg),
     &       (Frack(K), gs(K)/Uconv5(MetFlg), 
     &       Conc(K)*Uconv6(metflg),k=1,5)   !    /WTDSOI, K=1,5)                              !Demar took the Uconv6(MetFlg)*0.001/62.4.d0 out, why
     
9654      FORMAT (I12.7, 3X,A, F14.7, 3X, F14.7, 3X, F16.7, 3X, ES20.7,
     &           3X, ES20.7,
     &           3X, F10.7, 3X, ES18.7, 3X, ES20.7,
     &           3X, F10.7, 3X, ES18.7, 3X, ES20.7,
     &           3X, F10.7, 3X, ES18.7, 3X, ES20.7,
     &           3X, F10.7, 3X, ES18.7, 3X, ES20.7,
     &           3X, F10.7, 3X, ES18.7, 3X, ES20.7)
      ENDIF
      RETURN
      END
C======================================================================
       SUBROUTINE MONOUT(LMONTH,LYEAR,NRAINM,NRUNOM,TRAINM,TRUNOM,
     &                       TGSM,BCKEND)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision CONC(10),TGSM(4,10),Uconv1(0:1),Uconv2(0:1),
     &       Uconv3(0:1),Uconv4(0:1),Uconv5(0:1)
      INTEGER BCKEND,SetReportFlag
      CHARACTER MONTHS(12)*3,Unit1(0:1)*9,Unit2(0:1)*12,Unit3(0:1)*9,
     &          I_4*4,G12_5*12,Element(4)*40,Astr*255
       save
      Data Months/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     &            'OCT','NOV','DEC'/
      Data Unit1/' in. of ','  cm of '/,Unit2/' tons/ac ',' tonnes/ha '/
     &     Unit3/' acres','hectares'/
      Data Uconv1/1.0d0, CmPerInch/,  Uconv2/1.0d0, 2.205d0/,
     &     Uconv3/1.0d0, 16.01d0/, Uconv4/1.0d0, 2.242d0/,
     &     Uconv5/1.0d0,2.471d0/
      Data Element/
     &      'VALUES FROM OVERLAND FLOW','VALUES FROM CHANNEL ONE',
     &      'VALUES FROM CHANNEL TWO',  'VALUES FROM IMPOUNDMENT'/
      RUNVOL = TRUNOM*DAREA(ELEM)
      TRUNOM = 12.0d0*TRUNOM
      IF (EROOUT .GE. 2) THEN
        PRINT *, "CALLING Logmessage"
        Call Logmessage('MONTHLY SUMMARY FOR '//Months(Lmonth)//', '//
     &     I_4(Lyear))
        PRINT *, "CALLING LogMessage"
        Call LogMessage(I_4(Nrainm)//' STORMS PRODUCED '//
     &      G12_5(TRAINM*Uconv1(MetFlg))//unit1(metflg)//'rainfall')
        PRINT *, "CALLING LogMessage"
        Call LogMessage(I_4(Nrunom)//' STORMS PRODUCED '//
     &      G12_5(TrunoM*Uconv1(MetFlg))//unit1(metflg)//'runoff')
      endif
      IF (NRUNOM.NE.0) then
       FLGTMP = SetReportFlag(EROOUT)
       DO 50 J=1,ELEM
         IF ((J.EQ.2.AND.FLGSEQ.EQ.2).OR.(J.EQ.3.AND.FLGSEQ.EQ.2).or.
     &         (J.EQ.3.AND.FLGSEQ.EQ.5) ) then
c             nada
         else
           IF (EROOUT .GE. 2) Call LogMessage(Element(j))
           Tgs =Sum_TGS(j,Tgsm,Npart)
           IF (TGS.LE.0.0) then
              PRINT *, "CALLING Logmessage"
              Call Logmessage('*** NO SOIL LOSS ***')
           else
             Call ReportMonthlyHeaders
             PRINT *, "CALLING SumConcs"
             Call SumConcs(j,Conc,Runvol,TGs,TGsm,Tconc,Tcncpc,Tppm)
             DACREi  = DAREA(J)/43560.0d0
             SOLOSS = TGS/(2000.0d0*DACREi)
             ELMMON(J) = SOLOSS*Uconv4(Metflg)
             IF (FLGTMP.NE.0) then
               PRINT *, "CALLING ReportTotals"
               Call ReportTotals(Tgs,Tconc,Tcncpc,Tppm,Elmmon(j),Dacrei)
               CALL ENRCMP(2,CONC,ENRICH)
               ENRMON(J) = ENRICH                                           BE
             else
               CALL ENRCMP(1,CONC,ENRICH)
               ENRMON(J) = ENRICH                                           BE
               ELMMON(J) = SOLOSS*UConv4(Metflg)
               IF (EROOUT .GE. 2) THEN                                       BE
                 PRINT *, "CALLING LogMessage"
                 Call LogMessage('Monthly soil loss '//G12_5(Elmmon(j))
     &                  //Unit2(MetFlg))
                 PRINT *, "CALLING LogMessage"
                 Call LogMessage('ENRICHMENT RATIO  '//G12_5(Enrich))
               endif
             endif
           endif ! of if(tgs=0
         endif ! of j=2...etc
  50   CONTINUE
      endif
      PRINT *, "CALLING ZeroOut"
      Call ZeroOut(Nrainm,Nrunom,Trainm,Trunom,TGSm,Elem,Npart)
       RETURN
       END
C======================================================================
      Subroutine ReportMonthlyHeaders
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      CHARACTER Header(-3:1)*40
      Data Header/'  THE QUANTITY OF ERODED SEDIMENT IN RUNOFF',
     &'PART.   FRAC. IN   SOIL LOSS        CONCENTRATIONS (SOIL/WATER)',
     &'TYPE   SED. LOAD      LBS.    ',
     &'  LBSF/FT**3    LBSF/LBSF     PPM (WT)',
     &'    KG/M**3       KG/KG       PPM (WT)'/
      IF (EROOUT .GE. 2.and.FLGTMP.EQ.1) THEN
        PRINT *, "CALLING Logmessage"
        Call Logmessage(Header(-3))
        PRINT *, "CALLING Logmessage"
        Call Logmessage(Header(-2))
        PRINT *, "CALLING Logmessage"
        Call Logmessage(Header(-1)(1:30)//Header(Metflg))
      ENDIF
      return
      end
C======================================================================
      subroutine SumConcs(j,Conc,Runvol,TGs,TGsm,Tconc,Tcncpc,Tppm)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision CONC(10),TGSM(4,10),Uconv2(0:1),Uconv3(0:1)
      Character Astr*255
      Data Uconv2/1.0d0, 2.205d0/,Uconv3/1.0d0, 16.01d0/
      TCONC  = 0.0d0
      TCNCPC = 0.0d0
      TPPM   = 0.0d0
      DO 20 K=1,NPART
        CONC(K) = TGSM(J,K)/RUNVOL
        CONCPC  = CONC(K)/WTDH2O
        PPM     = CONCPC*1.0d+06
        FRACK(k)    = TGSM(J,K)/TGS
        IF (FLGTMP .EQ. 1 .AND. EROOUT .GE. 2) THEN              
           WRITE (Astr,'(7X,I2,6X,F5.2,3(4X,F9.4),4X,F10.0)') K,
     &       FRACK(k),TGSM(J,K)/Uconv2(metflg),CONC(K)*Uconv3(Metflg), !2.205, 16.01
     &                                  CONCPC,PPM
           PRINT *, "CALLING Logmessage"
           Call Logmessage(Astr)
        ENDIF
        TCONC   = TCONC  + CONC(K)
        TCNCPC  = TCNCPC + CONCPC
   20   TPPM    = TPPM   + PPM
      return
      end
C======================================================================
      Subroutine ReportTotals(Tgs,Tconc,Tcncpc,Tppm,AElmmon,Dacrei)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision Uconv2(0:1),Uconv5(0:1)
      CHARACTER Unit2(0:1)*12,Unit3(0:1)*9,I_4*4,G12_5*12,Astr*255
       save
      Data Unit2/' tons/ac ',' tonnes/ha '/,Unit3/' acres','hectares'/
      Data Uconv2/1.0d0, 2.205d0/,
     &     Uconv5/1.0d0,2.471d0/
      IF (EROOUT .GE. 2) THEN
        Write(Astr,'(10X,A,17X,3(F9.4,4X),F10.0)')'TOTAL',
     &             TGS/Uconv2(Metflg),TCONC,TCNCPC,TPPM                  !2.205,
        PRINT *, "CALLING Logmessage"
        Call Logmessage(Astr)
        PRINT *, "CALLING LogMessage"
        Call LogMessage('Monthly Soil Loss for area='//
     &     G12_5(AElmmon)//Unit2(metflg)//' (area='//
     &     G12_5(DACREi/Uconv5(Metflg))//Unit3(metflg))                   !2.471
        PRINT *, "CALLING Logmessage"
        Call Logmessage(Astr)
      ENDIF
      return
      end
C======================================================================
      Integer Function SetReportFlag(Eroout)
C======================================================================
      integer Flgtmp,Eroout
       FlgTmp = MOD(EROOUT,2)       !0=abbreviated reports, 1=full reports
       IF (EROOUT.EQ.6) FLGTMP = 1
       SetReportFlag = FlgTmp
      return
      end
C======================================================================
      Double Precision Function Sum_TGS(j,Tgs_matrix,Npartx)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      Dimension Tgs_Matrix(4,10)
      include 'Rz_Gleams.inc'  !sab 6/2002
      TGS    = 0.0d0
      DO 10 K=1,NPARTx
   10    TGS = TGS + TGS_Matrix(J,K)
      Sum_TGS = TGS
      return
      end
C======================================================================
      Subroutine ZeroOut(Nrain,Nruno,Train,Truno,TGS,Elem,Npartx)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      Dimension TGS(4,10)
      Integer Elem
       NRAIN = 0
       NRUNO = 0
       TRAIN = 0.0d0
       TRUNO = 0.0d0
       DO 70 J=1,ELeM
           DO 70 K=1,NPartx
   70        TGS(J,K) = 0.0d0
      return
      end
C======================================================================
       SUBROUTINE IMPOND(GS,CONC,SOLOSS)
C======================================================================
c equation I-146-155 for impoundment
c see Foster, G. R. et al. 1980. Chapter 3, in CREAMS documentation, page 44
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      include 'Rz_Gleams.inc'  !sab 6/2002
      double precision GS(10),CONC(10)
      save
      V   = DAPND*RUNOFF
      ZS  = -6.68d-06*FS-0.0903d0*B_ero+1.19d-04*C_ero-3.42d-06*V
     &            -20400.0d0* INTAKE
      YS  = 3.28d-05*FS+0.123d0*B_ero-2.40d-04*C_ero+8.10d-06*V
     &            -11880.0d0*INTAKE
      A1  = 113.6d0*dEXP(ZS)
      B1  = -0.152d0*dEXP(YS)
       IF (INTAKE.EQ.0.0) then
         FCRO   = 1.0d0
      else
        ZOUT   = -9.29d-06*FS+0.0282d0*B_ero+1.25d-04*C_ero-3.08d-06*V
     &           -33359.0d0*INTAKE
        FCRO   = 0.95d0*dEXP(ZOUT)
        IF (FCRO.GT.1.0d0) FCRO = 1.0d0
        IF (FCRO.GT.0.0d0) then 
           RUNPND = FCRO*RUNOFF
           CONFCT = 3.048d+05
           SOLOSS = 0.0d0
           DO 30 K=1,NPART
             PCT     = (A1/(B1*CONFCT*DDCL(K)))*(dEXP(B1*CONFCT*
     1               DCL(K+1))-dEXP(B1*CONFCT*DCL(K)))
             IF (PCT.GT.100.0d0) PCT  = 100.0d0
             CONC(K) = CONC(K)*(PCT/100.0d0)/FCRO
             GS(K)   = RUNPND*DAPND*CONC(K)
   30        SOLOSS  = SOLOSS+GS(K)/DAPND
        else
           PRINT *, "CALLING Logmessage"
           Call Logmessage('***  NO RUNOFF - NO SOIL LOSS  ***'//
     &                     '(IMPOUNDMENT CONTAINED ALL RUNOFF)')
           RUNOFF = 0.0d0
           RUNPND = 0.0d0
           SOLOSS = 0.0d0
         endif
      endif
      RETURN
      END
C======================================================================
      Double Precision FUNCTION SHDIST(X)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      IF (X.LT.0.02) then
        SHDIST = 0.13d0*X/0.02d0
      else
        SHDIST = dEXP(0.12692d0-0.51634d0*dLOG(X)-
     1          0.40825d0*dLOG(X)**2-0.03442d0*
     1          dLOG(X)**3)
      endif
      RETURN
      END
C======================================================================
      Double Precision FUNCTION SHIELD(REYN)
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      double precision Y(8),R(8)
      save
      DATA Y /0.0772d0,0.0579d0,0.04d0,0.035d0,0.034d0,0.045d0,
     &        0.055d0,0.057d0/
      DATA R /1.0d0,2.0d0,4.0d0,8.0d0,12.0d0,100.0d0,400.0d0,1000.0d0/
      IF (REYN.LT.R(1)) GO TO 30
      IF (REYN.GT.R(8)) GO TO 40
      DO 10 I=2,8
           IF (R(I-1).LE.REYN.AND.R(I).GE.REYN) GO TO 20
  10   CONTINUE
  20   CONTINUE
      SLOPE  = (dLOG(Y(I))-dLOG(Y(I-1)))/(dLOG(R(I))-
     1          dLOG(R(I-1)))
       YCR    = dLOG(Y(I-1))+SLOPE*(dLOG(REYN)-dLOG(R(I-1)))
       GO TO 50
  30   CONTINUE
       I      = 2
       SLOPE  = (dLOG(Y(I))-dLOG(Y(I-1)))/(dLOG(R(I))-
     1          dLOG(R(I-1)))
       YCR    = dLOG(Y(1))-SLOPE*(dLOG(R(1))-dLOG(REYN))
       GO TO 50
  40   CONTINUE
       I      = 8
       SLOPE  = (dLOG(Y(I))-dLOG(Y(I-1)))/(dLOG(R(I))-
     1          dLOG(R(I-1)))
       YCR    = Y(8)+SLOPE*(dLOG(REYN)-dLOG(R(8)))
  50   CONTINUE
       SHIELD = dEXP(YCR)
       RETURN
       END
C======================================================================
cc      Subroutine CalculatePeakRunoff    !Demar comments this out and use peak runoff from RZWQM2.
C======================================================================
c equations lifted verbatum from GLEAMS code, see R. E. Smith and J. R. Williams 1980, CREAMS
c manual, page 13-35, in:Knisel, W. G. Ed. CREAMS: A field-scale model for chemical, runoff, 
c  and erosion from agricultural management systems. USDA, Conservation Research Report No. 26
c  640 pp., Illus.
C======================================================================
cc      Implicit Double Precision (A-H,O-Z)
cc      include 'Rz_Gleams.inc'  !sab 6/2002
cc      P1 = 200.0d0* ((DACRE/640.0d0)**0.7d0)*((CHS*5280.0d0)**0.159d0)
cc     1       *(WLW**(-0.187d0))
c * * *  coefficient .824 == .917 * (1/640)**.0166
cc      P2    =0.824d0*DACRE**0.0166d0
cc      PR = P1*Runoff**P2         !runoff in inc file
cc      Exrain = 0.991d0*PR/DACRE  ! as is exrain from ft3/s to in/hr
cc      RETURN
cc      END
C======================================================================
      Subroutine RemoveComments(Astr)
C======================================================================
c equations lifted verbatum from GLEAMS code 
C======================================================================
      Implicit Double Precision (A-H,O-Z)
      Character*(*) Astr
      Logical DoReplace
      DoReplace = .false.
      Do 10 i = 1,Len(Astr)
        If (Astr(i:i) .eq. '{') DoReplace = .true.
   10   If (DoReplace) Astr(i:i) = ' '
      return
      end
      SUBROUTINE ApplyErosion_n_Report(Tday,SoLoss,Enrich,cc,slks,
     &          freund,CONCX2,COBIND,BD,TL,theta,XNU,CN,erodedmass,ii)
C======================================================================
c
C The rationale for the formulation used here is taken from Creams
C (Conservation Report 26, pg.69)
c SorbedConc is in milli-g solute/kg soil mass; Soloss is in kg/ha, Enrich is
c dimensionless. Solute lost due to erosion is SorbedConc*soloss*enrich
C in kg/ha.  The results are accumulated in CumulErodedSolute(6).
C Scalar reports indices are IndexScalar(1..6)
C (1.0D+5*BD*TL)is a conversion factor from kg/ha soil loss to kg soil loss from the first soil layer
c  per unit area
c no soil depth and bulk density were adjusted for soil loss in the first soil layer
C
C NEED TO UNDERSTAND SURFACE RESIDUE EROSION, SOIL ORGANIC C/N EROSION
c the code is modified to elimite soil N and soil C and pesticide from soil numerical layers
c once it is totally eroded although no soil depth is reduced.
C-------
      Implicit Double Precision (A-H,O-Z)
      !include 'Rz_Params.inc'  !sab 6/2002
      include 'Rz_Gleams.inc'
      PARAMETER (CONV=1.0D-1)
      DIMENSION CC(MXNOD,MXCHEM), SLKS(MXNOD,MXPEST,2), TL(mxnod),
     &          CONCX2(MXNOD,MXPEST),CObind(MXNOD,MXPEST),BD(MXNOD),
     &          theta(mxnod),erodedmass(6),XNU(MXNOD,25),CN(9),Tconv(5),
     &          totalmass(5),erode(5)
      Dimension SorbedConc(6),IndexScalar(6),freund(mxpest)
c     &          CumulErodedSolute(6)
      Data IndexScalar/23,24,25,7,45,46/   !pesticides 1, 2, and 3 plus NH4-N, OM-N, OM-C
      data TotalSedimentLost/0.0d0/
      save TotalSedimentLost
c conversion factor from kg/ha soil loss to kg per unit volume
      conv10 = bd(1)*TL(1)*1.0d+5
      ii=0
      ierosiondp=1
      do i=1,5                                                   !only first 5 numerical layers. 
      Tconv(i) = bd(i)*TL(i)*1.0d+5
      if (i.eq.1) totalmass(i)=Tconv(i)
      if (i.gt.1) totalmass(i)=totalmass(i-1)+Tconv(i)
      erode(i)=0.0d0
      ErodedMass(i)=0.0d0
      enddo
c
      TotalSedimentLost = TotalSedimentLost + Soloss*enrich   !soloss in kg/ha
      totalnew=0.0d0
      do i=1,5
          if (totalnew.lt.soloss*enrich) then
      if (TotalSedimentLost.le.totalmass(i)) then
         if (TotalSedimentLost-soloss*enrich.le.totalmass(i)) then
         if (i.eq.1) then
             erode(i)=soloss*enrich
             else
          erode(i)=min(soloss*enrich-totalnew,totalmass(i)-
     &            (TotalSedimentLost-soloss*enrich))
          endif
c              erode(i)=soloss*enrich
         else
              erode(i)=totalmass(i)-(TotalSedimentLost-soloss*enrich)
          endif
       elseif (TotalSedimentLost.gt.totalmass(i)) then
          if (TotalSedimentLost-soloss*enrich.le.totalmass(i)) then
        erode(i)=totalmass(i)-(TotalSedimentLost-soloss*enrich+totalnew)
          else
              erode(i)=0.0d0                     !layer i is gone. 
              ierosiondp=i+1                     !so start i+1 next runoff event
          endif
c      else
c          erode(i)=tconv(i)
       endif
           if (erode(i).gt.0.0d0) then
               ii=i
               totalnew=totalnew+erode(i)
           endif
         endif
      enddo
c
c      SorbedConc(1) = Pnode(1,2)+Pnode(1,3)       ! NEEDS MORE WORK, LMA
c      SorbedConc(1) = 0.0d0                        ! set to zero for Phosphrus
C PESTICIDE LOSSES
      do k=ierosiondp,ii
      do 10 ipest = 1,3
          slksf=FSLKS(SLKS(k,IPest,1),CC(k,IPest+12),
     +                FREUND(IPest),theta(k),bd(k))
	conv11 = (theta(k)+bd(k)*slksf)*tl(k)*1.0d+5
      SorbedConc(ipest) = (CC(k,IPest+12) * SLKSF+
     &       CONCX2(k,IPEST) + COBIND(k,IPEST))/1.0d6
	  CONCX2(k,ipest) = CONCX2(k,ipest)*(1.0D0 - erode(k)/tconv(k))
	  CObind(k,ipest) = CObind(k,ipest)*(1.0D0 - erode(k)/tconv(k))
	  cc(k,ipest+12) = cc(k,ipest+12)*(1.0d0-
     &          slksf*erode(k)/conv11)
   10 continue
C NH4-N LOSS, BECAUSE NH4 IS ASSUMED TO BE ADSORBED
      CC(k,10) = CC(k,10)*theta(k)/bd(k)              ! convert from ug/ml to ug/g 
      SorbedConc(4) = CC(k,10)/1.0d6              ! convert from ug/ml to ug/g to mg/ha
     
c      SorbedConc(5) = 0.0d0 !set to No3-N/NH4 sorption
C SOIL ORGANICS
      SorbedConc(5)=(XNU(k,1)/CN(1)+XNU(k,2)/CN(2)+XNU(k,3)/CN(3)
     +             +XNU(k,4)/CN(4)+XNU(k,5)/CN(5))/1.d6              !1.d6 convert to kg basis from ug/g
      SorbedConc(6)=(XNU(k,1)+XNU(k,2)+XNU(k,3)+XNU(k,4)+XNU(k,5))
     +             /1.d6
      do 20 I = 1,6
        ErodedMass(i) = ErodedMass(i)+SorbedConc(i)*erode(k)
        CumulErodedSolute(I) = CumulErodedSolute(I)+ErodedMass(i)
c        Call Sgate(Tday,IndexScalar(I),CumulErodedSolute(I))
   20 continue 
	CC(k,10)=CC(k,10)*(1.0D0 - erode(k)/1.d6/(tconv(k)*1d-6))                      !NH4-N loss
	CC(k,10)=CC(k,10)*bd(k)/theta(k)                                   !convert back to ug/ml
      xnu(k,12)=cc(k,10)                                                 !change xnu just in case
C      TOTAL1=(XNU(1,1)/CN(1)+XNU(1,2)/CN(2)+XNU(1,3)/CN(3)
C     +             +XNU(1,4)/CN(4)+XNU(1,5)/CN(5))*TL(1)*BD(1)*0.1D0
	  XNU(k,1) = XNU(k,1)*(1.0D0-erode(k)/1.d6/(tconv(k)*1d-6))
	  XNU(k,2) = XNU(k,2)*(1.0D0-erode(k)/1.d6/(tconv(k)*1d-6))
	  XNU(k,3) = XNU(k,3)*(1.0D0-erode(k)/1.d6/(tconv(k)*1d-6))
	  XNU(k,4) = XNU(k,4)*(1.0D0-erode(k)/1.d6/(tconv(k)*1d-6))
	  XNU(k,5) = XNU(k,5)*(1.0D0-erode(k)/1.d6/(tconv(k)*1d-6))
        enddo
        CALL SGATE(TDAY,4,Soloss)                              !write out daily soil loss in kg/ha
        do i=1,6
        PRINT *, "CALLING Sgate"
        Call Sgate(Tday,IndexScalar(I),ErodedMass(i))          !write out daily pesticide loss in mg/ha
        enddo
C
C	  XNU(1,5) = (XNU(1,5)*TL(1)*BD(1)*0.1D0 - 
C     +             XNU(1,5)/1.D6*SOLOSS*ENRICH)/(TL(1)*BD(1)*0.1D0)
C      TOTAL2=(XNU(1,1)/CN(1)+XNU(1,2)/CN(2)+XNU(1,3)/CN(3)
C     +             +XNU(1,4)/CN(4)+XNU(1,5)/CN(5))*TL(1)*BD(1)*0.1D0
C      TOLOSS=TOTAL1-TOTAL2
      Return
      end
      Subroutine LogMessage(Msg)
C
C======================================================================
C
C       PURPOSE: to print Msg to a log file and stop execution if so commanded
C
C       PROGRAMMER:   S.A.Bloom
C
C       VERSION:  1.0
C
C======================================================================
C
      save
      character*(*) Msg
	logical firstpass
	data firstpass/.true./
	If (Firstpass) then
        OPEN (888,FILE='Gleams.log',STATUS='UNKNOWN')
        firstpass = .false.
	else
	! RM - access='append' replaced with position='append'
        open (888, position='Append', file='Gleams.log')
	endif
      write(888,'(1X,A)') Msg
      write(*,  '(1X,A)') Msg
      Close(888)
      If (Len(msg).GE.4.and.msg(1:4) .EQ. 'STOP') Stop
	return
	end
C
C======================================================================
      Character*4 Function I_4(IValue)
C
C======================================================================
C
C       PURPOSE: returns string equivalent of an integer
C       PROGRAMMER:   S.A.Bloom
C======================================================================
C
      Character Str*4
      Integer Ivalue
      Write(Str,'(I4)') IValue
      I_4 = Str
      Return
      END
C======================================================================
      Character*12 Function G12_5(Value)
C
C======================================================================
C
C       PURPOSE: returns string equivalent of a real value
C
C       PROGRAMMER:   S.A.Bloom
C
C       VERSION:  1.0
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      Character Str*12,Aresult*12
      Write(Str,'(G12.5)') Value
      Aresult = '            '
      do 10 i = 1,len(Str)
   10    Aresult(i:i) = str(i:i)
      G12_5 = Aresult
      Return
      END
