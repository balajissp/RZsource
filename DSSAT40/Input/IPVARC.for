C=======================================================================
C  IPVAR, Subroutine
C
C  Reads in genetic information for crop CERES
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEC,NSENS,VARNO,VARTY,VRNAME,PATHCR,ECONO
C
C  LOCAL  : LINE,BLANK,ANS,ERRKEY,C255,FILECC,ERRNUM,LINVAR,LUNVAR,I,ISECT,
C           PATHL,NLOOP,NLVAR,FLAG,VAR
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEVAR SENS INPUT
C
C  Calls  : ERROR CLEAR IGNORE VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE IPVARC (FILEC,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &                  PATHCR,ECONO,CROP,PHINT,HTMAX,BIOHALF)

C-RZ  SUBROUTINE IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
C-RZ &                  PATHGE,ECONO,CROP)

      USE ModuleDefs

      IMPLICIT NONE
c      INCLUDE  'COMGEN.BLK'

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS
      CHARACTER*2   CROP
      CHARACTER*6   VARTY,VARNO,ERRKEY,ECONO
      CHARACTER*12  FILEC
      CHARACTER*16  VRNAME
      CHARACTER*255  PATHCR
      CHARACTER*255  FILECC
      CHARACTER*255 C255,STRING
      INTEGER       I,L,NSENS,NLVAR,LUNVAR,LINVAR,ISECT,NLOOP
      INTEGER       ERRNUM,PATHL,K,itrim
      REAL          FLAG,VAR
C LIWNAG MA, RZWQM-DSSAT
C      REAL  P1,P2,P5,G2,G3,PHINT
C	REAL P1V,P1D,G1,P2O,P2R,G4
      REAL          P1,P1V,P1D,P2,P2O,P2R,P3,P4,P5,G1,G2,G3,G4
      REAL          PHINT,PD,TC,O1,GPROT,LT50H
      REAL          CSDVAR,PHTHRS(20),SDPDVR,SLAVAR,LFMAX,XFRUIT,WTPSD
      REAL          SFDUR,PODUR,PPSEN,PH2T5,SIZELF
      REAL          RATPOT,PI1,PI2,DTTPI
      REAL          PCINT,PCGRD
      REAL          GCOEFF(15),HTMAX,BIOHALF
      REAL          SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ
      TYPE (PLANTVARTYPE) PLANTVAR

      PARAMETER (LUNVAR = 119)
      PARAMETER (ERRKEY = 'IPVAR ')
      PARAMETER (BLANK  = ' ')

C-KEN      PATHL  = INDEX (PATHGE,BLANK)
      PATHL  = LEN_TRIM(PATHCR)

      IF (PATHL .LE. 1) THEN
         FILECC = FILEC
       ELSE
C-KEN         FILEGG = PATHGE(1:(PATHL-1)) // FILEG
         FILECC = PATHCR(1:(PATHL)) // FILEC
      ENDIF

C-----------------------------------------------------------------------
C    Read Cultivar Specific Genetics/Cultivar Parameter File
C-----------------------------------------------------------------------

      OPEN (LUNVAR,FILE = FILECC,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEC,0)
      IF (NSENS .EQ. 1) THEN
         I  = 1
         NLOOP = 0
         IF (RNMODE .EQ. 'I') THEN
            CALL CLEAR
            WRITE (*,100)
         ENDIF
  200    CONTINUE
         CALL IGNORE (LUNVAR,LINVAR,ISECT,C255)
         IF (ISECT .EQ. 0) GO TO 211
         IF (ISECT .EQ. 2) GO TO 200
         IF (C255(1:1) .EQ. ' ' .OR. C255(1:1) .EQ. '*' ) GO TO 200
         READ (C255, 700, IOSTAT=ERRNUM) VARTY,VRNAME,ECONO
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEC,LINVAR)
         IF (RNMODE .EQ. 'I') THEN
            WRITE (*,750) I,VARTY,VRNAME,ECONO,ECONO(3:4)
         ENDIF
         IF (VARTY .EQ. VARNO) NLVAR = I
C
C        Write Pause Statement Every 15 lines
C
         IF (MOD(I,15) .EQ. 0 .AND. RNMODE .EQ. 'I') THEN
            WRITE (*,300)
            READ  (5,'(A1)') ANS
         ENDIF
         I  = I + 1
         GOTO 200

  211    CONTINUE
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,FILEC,LINVAR)
         LINE(1) = ' '
         ! RM - $ at end of a string = new line, replaced with ADVANCE='no'
         IF (RNMODE .EQ. 'I') WRITE (5,1300, ADVANCE='no') NLVAR
         READ (5, 1400) LINE
         CALL VERIFY (LINE,VAR,FLAG)
         IF (VAR .LE. 0) THEN
            VAR = NLVAR
          ELSE IF ((FLAG .GT. 0) .OR. (VAR .GT. (I-1))) THEN
            WRITE (*,1200) I -1
            GO TO 211
          ELSE IF (VAR .NE. NINT(VAR)) THEN
            WRITE (*,1201)
            GO TO 211
          ELSE IF (VAR .GT. 0) THEN
            NLVAR = NINT(VAR)
          ELSE
            GO TO 211
         ENDIF

         REWIND (LUNVAR)
      ENDIF

      I = 0

 2010 CONTINUE
      I = I + 1
 2000 CONTINUE
      CALL IGNORE (LUNVAR, LINVAR, ISECT, C255)
      IF (ISECT .EQ. 0) then
        print*,'end of file hit in cultivar file: ',fileCC
        print*,'did not find variety: ',vrname
        goto 9999
	endif
c      IF (ISECT .EQ. 0) CALL ERROR (ERRKEY,2,FILEC,LINVAR)
      IF (ISECT .EQ. 2) GO TO 2000
      IF (C255(1:1) .EQ. ' ' .OR. C255(1:1) .EQ. '*') GO TO 2000

!     CROPGRO crops
      IF(INDEX('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8BRVBCPCBFBCOCTCASU
     &ALBM', CROP) .GT. 0) THEN
         READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,CSDVAR,
     &        PPSEN,PH2T5,PHTHRS(6),PHTHRS(8),PHTHRS(10),PHTHRS(13),
     &        LFMAX,SLAVAR,SIZELF,XFRUIT,WTPSD,SFDUR,SDPDVR,PODUR
         IF (LFMAX  .LE. 0) CALL ERROR (ERRKEY,22,FILEC,LINVAR)
         IF (SLAVAR .LE. 0) CALL ERROR (ERRKEY,23,FILEC,LINVAR)
         IF (SIZELF .LE. 0) CALL ERROR (ERRKEY,23,FILEC,LINVAR)
         IF (XFRUIT .LE. 0) CALL ERROR (ERRKEY,24,FILEC,LINVAR)
         IF (WTPSD  .LE. 0) CALL ERROR (ERRKEY,25,FILEC,LINVAR)
         IF (SDPDVR .LE. 0) CALL ERROR (ERRKEY,26,FILEC,LINVAR)
         IF (SFDUR  .LE. 0) CALL ERROR (ERRKEY,27,FILEC,LINVAR)
         IF (PODUR  .LE. 0) CALL ERROR (ERRKEY,28,FILEC,LINVAR)
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%CSDVAR =  CSDVAR
          PLANTVAR%PPSEN =  PPSEN
          PLANTVAR%PH2T5 =  PH2T5
          PLANTVAR%PHTHRS(6) =  PHTHRS(6)
          PLANTVAR%PHTHRS(8) =  PHTHRS(8)
          PLANTVAR%PHTHRS(10) =  PHTHRS(10)
          PLANTVAR%PHTHRS(13) =  PHTHRS(13)
          PLANTVAR%LFMAX =  LFMAX
          PLANTVAR%SLAVAR =  SLAVAR
          PLANTVAR%SIZELF =  SIZELF
          PLANTVAR%XFRUIT =  XFRUIT
          PLANTVAR%WTPSD  =  WTPSD
          PLANTVAR%SFDUR  =  SFDUR
          PLANTVAR%SDPDVR =  SDPDVR
          PLANTVAR%PODUR  =  PODUR
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)

       ELSEIF (INDEX ('MZMLSGWHBAMOMXTRBSSF',CROP) .GT. 0) THEN
         !Maize
         IF (CROP .EQ. 'MZ'.or.CROP.EQ.'BS') THEN
          read (C255,1110) string
          k=itrim(string)
        IF (K.LE.68) THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,PHINT
            ELSE 
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,PHINT,HTMAX,BIOHALF
            ENDIF
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
         PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%P1 =  P1
          PLANTVAR%P2 =  P2
          PLANTVAR%P5 =  P5
          PLANTVAR%G2 =  G2
          PLANTVAR%G3 =  G3
          PLANTVAR%PHINT =  PHINT
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
C  OILCROP-SUN          
         ELSEIF (CROP .EQ. 'SF') THEN
          read (C255,1110) string
          k=itrim(string)
        IF (K.LE.68) THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,O1
            ELSE 
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,O1,HTMAX,BIOHALF
            ENDIF
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
         PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%P1 =  P1
          PLANTVAR%P2 =  P2
          PLANTVAR%P5 =  P5
          PLANTVAR%G2 =  G2
          PLANTVAR%G3 =  G3
          PLANTVAR%O1 =  O1
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)

         !Wheat
          ELSE IF ((CROP .EQ. 'WH').or.(crop.eq.'TR')) THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1V,P1D,P5,G1,G2,G3,PHINT
C-GH     &            P1V,P1D,P5,G1,G2,G3,PHINT,GPROT,LT50H,P1
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%P1V =  P1V
          PLANTVAR%P1D =  P1D
          PLANTVAR%P5 =  P5
          PLANTVAR%G1 =  G1
          PLANTVAR%G2 =  G2
          PLANTVAR%G3 =  G3
          PLANTVAR%PHINT =  PHINT
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)

         !Sorghum
          ELSE IF ((CROP .EQ. 'SG')
     &           .or.(crop.eq.'MO').or.(crop.eq.'MX')) THEN
          read (C255,1110) string
          k=itrim(string)
        IF (K.LE.75) THEN
        READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G2,PHINT
        ELSE 
        READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G2,PHINT,HTMAX,BIOHALF
        ENDIF
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%P1 =  P1
          PLANTVAR%P2O =  P2O
          PLANTVAR%P2R =  P2R
          PLANTVAR%P5 =  P5
          PLANTVAR%G1 =  G1
          PLANTVAR%G2 =  G2
          PLANTVAR%PHINT =  PHINT
C          PLANTVAR%HTMAX =  HTMAX
C          PLANTVAR%BIOHALF =  BIOHALF
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)

         !Millet
          ELSE IF (CROP .EQ. 'ML') THEN
          read (C255,1110) string
          k=itrim(string)
        IF (K.LE.75) THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G4,PHINT
            ELSE 
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G4,PHINT,HTMAX,BIOHALF
            ENDIF
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%P1 =  P1
          PLANTVAR%P2O =  P2O
          PLANTVAR%P2R =  P2R
          PLANTVAR%P5 =  P5
          PLANTVAR%G1 =  G1
          PLANTVAR%G4 =  G4
          PLANTVAR%PHINT =  PHINT
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)

         !Barley
          ELSE IF (CROP .EQ. 'BA') THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1V,P1D,P5,G1,G2,G3,PHINT
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%P1V =  P1V
          PLANTVAR%P1D =  P1D
          PLANTVAR%P5 =  P5
          PLANTVAR%G1 =  G1
          PLANTVAR%G2 =  G2
          PLANTVAR%G3 =  G3
          PLANTVAR%PHINT =  PHINT
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
         ENDIF

      !Potato
       ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            G2,G3,G4,PD,P2,TC
C LIWANG MA, RZWQM-DSSAT
           CALL GETPUT_PLANTVAR('GET',PLANTVAR)
          PLANTVAR%VARNO =  VARNO
          PLANTVAR%VRNAME =  VRNAME
          PLANTVAR%ECONO =  ECONO
          PLANTVAR%G2 =  G2
          PLANTVAR%G3 =  G3
          PLANTVAR%G4 =  G4
          PLANTVAR%PD =  PD
          PLANTVAR%P2 =  P2
          PLANTVAR%TC =  TC
          CALL GETPUT_PLANTVAR('PUT',PLANTVAR)
      !Cassava
       ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
            READ (C255, 900,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           (GCOEFF(L),L=1,15)
      !Rice
       ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
            READ (C255,950,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2R,P5,P2O,G1,G2,G3,G4

      !Sugarcane
       ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
            READ (C255,1000,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,RATPOT,LFMAX,G1,PI1,PI2,DTTPI
      !Sunflower
!       ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
!            READ (C255,1100,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!     &            P1,P2,P5,G2,G3,O1
      !Pineapple
       ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
            READ (C255,1150,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P2,P3,P4,G2,G3,PHINT
      !Taro, tanier
       ELSEIF (INDEX ('TRTN',CROP) .GT. 0) THEN
            READ (C255,1175,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD
      !Cotton
!       ELSEIF (CROP .EQ. 'CO') THEN
!            READ (C255,1185,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
!     &            SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ
      ENDIF

      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEC,LINVAR)
      IF (((VARTY .NE. VARNO) .AND. (NSENS .EQ. 0)) .OR.
     &        ((I .LT. NLVAR) .AND. (NSENS .EQ. 1))) GO TO 2010

      VARNO = VARTY
9999  CLOSE (LUNVAR)
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (T30,'VARIETY SELECTION',/,T30,'=================',
     &     //,T43,'ECOTYPE',2X,'MATURITY',
     &      /,2X,'NO.',1X,'ENTRY',3X,'VARIETY',22X,'GROUP',5X,'GROUP',
     &      /,2X,'---',1X,'------',2X,20('-'),8X,'-------',2X,
     &           '--------')
  300 FORMAT (/,'  More.... press < ENTER > key')
  700 FORMAT (A6,1X,A16,1X,A6,6X,A2)
  750 FORMAT (I4,') ',A6,2X,A16,13X,A6,6X,A2)
c 800 FORMAT (A6,1X,A16,1X,A6,15(1X,F5.0))
  800 FORMAT (A6,1X,A16,1X,A6,15(F6.0))
  900 FORMAT (A6,1X,A16,1X,A6,2(F6.1),F6.2,2(F6.1),F6.2,F6.0,
     &        F6.3,F6.2,5(F6.0))
  950 FORMAT (A6,1X,A16,1X,A6,5(F6.1),F6.4,2(F6.2))
 1000 FORMAT (A6,1X,A16,1X,A6,1X,7(F6.1))
c1100 FORMAT (A6,1X,A16,1X,A6,1X,F6.2,F8.4,F7.2,F8.2,F7.3,F4.0)
 1100 FORMAT (A6,1X,A16,1X,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,F5.0,1X,F5.2,
     &        1X,F5.0)
 1150 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.1,F6.0,F6.1,F6.2,F6.1)
 1175 FORMAT (A6,1X,A16,1X,A6,4(F6.0),2(F6.2),3(F6.1))
 1185 FORMAT (A6,1X,A16,1X,A6,6(F6.0))
 1200 FORMAT (6X,'ERROR! Variety Selection must be between 1 & ',I3,/)
 1201 FORMAT (6X,'ERROR! Variety Selection must be an INTEGER value',/)
 1300 FORMAT (/,6X,'VARIETY SELECTED ===>',1X,I4,
     &        /,6X,'NEW SELECTION ?  --->',3X,' ') !RM removed $, replaced with Advance='no'
 1400 FORMAT (80A1)
 1110 FORMAT(A255)
      END
