C=======================================================================
C  FIND, Subroutine, J.W.Jones, 01/03/91
C  Finds appropriate SECTION in a file of logical unit number LUNUM by
C  searching for a 6-character NAME at beginning of each line.
C-----------------------------------------------------------------------
C  INPUT : LUNUM  - logical unit number of the file to read
C          NAME  - 6-character variable name of section to find
C  OUTPUT: LNUM  - Line number of the file currently being read
C          FOUND - Indicator of completion of find routine
C                    0 - End-of-file encountered i.e. name not found
C                    1 - NAME was found
C  LOCAL :
C  IFILE : LUNUM
C  NOTES : Modified N.B. Pickering, 08/27/91
C=======================================================================

      SUBROUTINE FIND(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE
      INTEGER FOUND,I,LNUM,LUNUM
      CHARACTER SECTION*6,NAME*6,UPCASE*1
C
C     Initialization.
C
      FOUND = 0
      LNUM  = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO
C
C     Loop to read through data file.
C
   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
         DO I = 1,LEN(SECTION)
            SECTION(I:I) = UPCASE(SECTION(I:I))
         END DO
C
C        String found, set FOUND to 1, and exit loop.
C
         IF (NAME .EQ. SECTION) then
            FOUND = 1
            GOTO 20
C
C           String not found, set FOUND to 0.
C
          ELSE
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END

C=======================================================================
C  IGNORE, Subroutine, J.W.Jones, 01/03/91
C----------------------------------------------------------------------------
C  PURPOSE: To read lines as an n-character variable and check it
C           for a blank line or for a comment line denoted by ! in col 1.
C  INPUTS:  LUN - Logical unit number of the file to be read
C           LINEXP - Starting line number at which this routine begins to
C                    read the file
C  OUTPUTS: LINEXP - Line number last read by the routine
C           ISECT - Indicator of completion of IGNORE routine
C                   0 - End of file encountered
C                   1 - Found a good line to read
C                   2 - End of Section in file encountered, denoted by *
C                       in column 1
C           CHARTEST - n-character variable containing the contents of
C                      the last line read by the IGNORE routine
C----------------------------------------------------------------------------
C
      SUBROUTINE IGNORE(LUN,LINEXP,ISECT,CHARTEST)

      CHARACTER BLANK*(80),CHARTEST*(*)
      INTEGER   LENGTH, LUN,LINEXP,ISECT
      DATA BLANK/'                                                    '/

      LENGTH = LEN(CHARTEST)

      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
C     Check to see if all of this section has been read
      IF(CHARTEST(1:1) .EQ. '*'  .OR. CHARTEST(1:1) .EQ. '$') THEN
C        End of section encountered
         ISECT = 2
         RETURN
      ENDIF
C
C     Check for blank lines and comments (denoted by ! in column 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
!         IF(CHARTEST(1:80).NE.BLANK)THEN
         IF(CHARTEST(1:LENGTH).NE.BLANK)THEN
C           FOUND A GOOD LINE TO READ
            RETURN
         ENDIF
      ENDIF
      GO TO 30
C     To read the next line
 70   ISECT = 0
      RETURN
      END SUBROUTINE IGNORE

C=======================================================================
C  IGNORE2, Subroutine, J.W.Jones, 01/03/91
C----------------------------------------------------------------------------
C  PURPOSE: To read lines as an n-character variable and check it
C           for a blank line or for a comment line denoted by ! in col 1.
C           Also check for second tier of data as notated by @ in the first
C           column.
C----------------------------------------------------------------------------
! Revision history
! 06/14/2005 CHP Return w/ ISECT=2, when '*' in column 1 found (previously 
!                looked for next data line.
C----------------------------------------------------------------------------
C INPUTS:  LUN - Logical unit number of the file to be read
C          LINEXP - Starting line number at which this routine begins to
C                   read the file
C OUTPUTS: LINEXP - Line number last read by the routine
C          ISECT - Indicator of completion of IGNORE2 routine
C                  0 - End of file encountered
C                  1 - Found a good line to read
C                  2 - End of Section in file encountered, denoted by *
C                      in column 1
C                  3 - Second tier headers found
C          CHARTEST - 80-character variable containing the contents of
C                     the last line read by the IGNORE2 routine
C=======================================================================
      SUBROUTINE IGNORE2(LUN,LINEXP,ISECT,CHARTEST)
      CHARACTER BLANK*80,CHARTEST*(*)
      INTEGER LUN,LINEXP,ISECT
      DATA BLANK/'                                                    '/
C----------------------------------------------------------------------------
      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
C     CHECK TO SEE IF ALL OF THIS SECTION HAS BEEN READ

      IF(CHARTEST(1:1) .EQ. '*' )THEN
C       INTERMEDIATE HEADER FOUND.  
        ISECT = 2
!        GOTO 30
        RETURN
      ENDIF

      IF(CHARTEST(1:1) .EQ.'@') THEN
C       NEXT TIER ENCOUNTERED
        ISECT = 3
        RETURN
      ENDIF
C
C     CHECK FOR BLANK LINES AND COMMENTS (DENOTED BY ! IN COLUMN 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
        IF(CHARTEST.NE.BLANK)THEN
C         FOUND A GOOD LINE TO READ
          RETURN
        ENDIF
      ENDIF

      GO TO 30
C       TO READ THE NEXT LINE
 70   ISECT = 0

      RETURN
      END SUBROUTINE IGNORE2

C=======================================================================
C  HFIND, Subroutine  GPF 7/95
C  Finds appropriate HEADER in a file of logical unit number LUNUM
C  by searching for a 5-character NAME following the '@' at the
C  beginning of a header line
C-----------------------------------------------------------------------
C  INPUT  : LUNUM  logical unit of file to read
C           NAME   variable name of header section to find (5-char)
C  OUTPUT : LNUM   line number of file currently read
C           ISECT  return status of find routine
C                  0  - EOF, name not found
C                  1  - NAME found
C                  2  - End of section encountered, denoted by *
C=======================================================================
      SUBROUTINE HFIND(LUNUM,NAME,LNUM,ISECT)

      IMPLICIT NONE
      INTEGER ISECT,I,LNUM,LUNUM
      CHARACTER HEADER*5,NAME*(*),UPCASE*1,LINE*128
C
C     Initialization, save initial line
C
      ISECT = 1
      DO I = 1, LEN(NAME)
         NAME(I:I) = UPCASE(NAME(I:I))
      END DO

C     Loop to read through data file.

   10  IF (.TRUE.) THEN
         READ(LUNUM,'(A)',ERR=20,END=20) LINE
         LNUM = LNUM + 1

C     End of section

         IF (LINE(1:1) .EQ. '*') THEN
            ISECT = 2
            RETURN
         ENDIF

C     Header line

         IF (LINE(1:1) .EQ. '@') THEN
            HEADER='     '
            DO I=2,LEN(LINE)
               IF (LINE(I:I) .NE. ' ') THEN
                  LINE(I:I) = UPCASE(LINE(I:I))
               ENDIF
            ENDDO
            DO I=2,(LEN(LINE)-LEN(NAME)+1)
               HEADER(1:LEN(NAME)) = LINE(I:(I+LEN(NAME)-1))
               IF (HEADER(1:LEN(NAME)) .EQ. NAME) THEN
                 ISECT = 1
                 RETURN
               ENDIF
            ENDDO
         ENDIF
         GOTO 10
      ENDIF
   20 ISECT = 0
      RETURN
      END


!=======================================================================
!  Subroutine READA
!   Reads measured development and final harvest data from FILEA 
!   and maps measured data to appropriate headers for output to 
!   OVERVEIW.OUT
!-----------------------------------------------------------------------
!  Revision history:
!  08/12/2005 CHP Modified to read "alias" headers for some variables
!=======================================================================
      SUBROUTINE READA(FILEA, OLAB, TRTNO, YRSIM, X)

!-----------------------------------------------------------------------
!     READ DEVELOPMENT AND FINAL HARVEST DATA FROM  FILEA
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER TRTNO,ERRNUM,LUNA,LINEXP,ISECT,NTR,I, J
      INTEGER YRSIM,YR,ISIM
      INTEGER COUNT
!     Headers with aliases -- save column
      INTEGER HWAM, HWAH, BWAM, BWAH, PDFT, R5AT  
      INTEGER, PARAMETER :: NV = 40

      REAL TESTVAL

      CHARACTER*4   OLAB(NV), HD
      CHARACTER*5   HEAD(NV)
      CHARACTER*6   DAT(NV), X(NV)  !, ERRKEY
      CHARACTER*12  FILEA
      CHARACTER*78  MSG(10)
      CHARACTER*255 C255

      LOGICAL FEXIST

      !ERRKEY = 'READA '

C-----------------------------------------------------------------------
C     Initialize measured values to -99 before reading values
C
      X = '   -99'

      CALL GETLUN('FILEA', LUNA)
      LINEXP = 0

      INQUIRE (FILE = FILEA, EXIST = FEXIST)

      IF (FEXIST) THEN
        OPEN (LUNA,FILE = FILEA,STATUS = 'OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) GOTO 5000
        CALL YR_DOY(YRSIM,YR,ISIM)

C       FIND THE HEADER LINE, DESIGNATED BY @TRNO
        DO WHILE (.TRUE.)
          READ(LUNA,'(A)',END=5000) C255
          LINEXP = LINEXP + 1
          IF (C255(1:1) .EQ. '@') EXIT    
        ENDDO

C       FOUND HEADER LINE, SAVE IT IN HEAD AND SEARCH FOR TREATMENT
        DO I = 1,NV
          READ(C255,'(1X,A5)') HEAD(I)
          IF (HEAD(I) .EQ. '     ') THEN
            COUNT = I - 1
            EXIT
          ENDIF
          C255 = C255(7:255)
        ENDDO

C       FIND THE RIGHT TREATMENT LINE OF DATA
        DO I = 1,1000
          CALL IGNORE(LUNA,LINEXP,ISECT,C255)
C
C    Return if no matching treatment is found in file FILA
C    No field measured data are necessary to be able to run the
C    model

          IF (ISECT .EQ. 0) GO TO 100
          READ(C255(1:6),'(2X,I4)',IOSTAT=ERRNUM) NTR
          IF (ERRNUM .NE. 0) GOTO 5000
          IF(NTR .EQ. TRTNO) GO TO 60
        ENDDO
  
  60    CONTINUE
  
C       READ DATA LINE
        DO I = 1,COUNT
          READ(C255,'(A6)',IOSTAT=ERRNUM) DAT(I)
          IF (ERRNUM .NE. 0) GOTO 5000

          !Test for numeric value -- set non-numeric values to -99
          READ(C255,'(F6.0)',IOSTAT=ERRNUM) TESTVAL
          IF (ERRNUM .NE. 0 .AND. 
     &        TRIM(ADJUSTL(HEAD(I))) .NE. 'TNAM') THEN
            DAT(I) = '   -99'
          ENDIF 

          C255 = C255(7:255)
        ENDDO
  
!       Search for location within array of headers which can
!       contain the same data.  Store index for later use.
!       Pairs of headers:  
!       'HWAM' or 'HWAH' 
!       'BWAM' or 'BWAH' 
!       'PDFT' or 'R5AT' 
        HWAM = -99; HWAH = -99
        BWAM = -99; BWAH = -99
        PDFT = -99; R5AT = -99
        DO J = 1, NV
          SELECT CASE (OLAB(J))
            CASE('HWAM'); HWAM = J 
            CASE('HWAH'); HWAH = J 
            CASE('BWAM'); BWAM = J 
            CASE('BWAH'); BWAH = J 
            CASE('PDFT'); PDFT = J 
            CASE('R5AT'); R5AT = J 
          END SELECT
        ENDDO

C       MATCH HEADER WITH DATA
        DO I = 2, COUNT   !Loop thru FILEA headers
          HD = ADJUSTL(HEAD(I))

!         For "alias" headers already know columns to store data
          SELECT CASE(HD)

          CASE ('HWAM')
            IF (HWAM > 0) THEN
              !store HWAM with HWAM header
              X(HWAM) = DAT(I)    
            ELSEIF (HWAH > 0) THEN
              !store HWAM with HWAH header 
              IF (X(HWAH) == '   -99') X(HWAH) = DAT(I)  
            ENDIF

          CASE ('HWAH')
            IF (HWAH > 0) THEN
              !store HWAH with HWAH header
              X(HWAH) = DAT(I)
            ELSEIF (HWAM > 0) THEN
              !store HWAH with HWAM header
              IF (X(HWAM) == '   -99') X(HWAM) = DAT(I) 
            ENDIF

          CASE ('BWAM')
            IF (BWAM > 0) THEN
              !store BWAM with BWAM header
              X(BWAM) = DAT(I)
            ELSEIF (BWAH > 0) THEN
              !store BWAM with BWAH header
              IF (X(BWAH) == '   -99') X(BWAH) = DAT(I)
            ENDIF

          CASE ('BWAH')
            IF (BWAH > 0) THEN
              !store BWAH with BWAH header
              X(BWAH) = DAT(I)
            ELSEIF (BWAM > 0) THEN
              !store BWAH with BWAM header
              IF (X(BWAM) == '   -99') X(BWAM) = DAT(I)
            ENDIF

          CASE ('PDFT')  
            IF (PDFT > 0) THEN
              !store PDFT with PDFT header
              X(PDFT) = DAT(I)
            ELSEIF (R5AT > 0) THEN
              !store PDFT with R5AT header
              IF (X(R5AT) == '   -99') X(R5AT) = DAT(I)
            ENDIF

          CASE ('R5AT')  
            IF (R5AT > 0) THEN
              !store R5AT with R5AT header
              X(R5AT) = DAT(I)
            ELSEIF (PDFT > 0) THEN
              !store R5AT with PDFT header
              IF (X(PDFT) == '   -99') X(PDFT) = DAT(I)
            ENDIF

!         No aliases for the rest
          CASE DEFAULT
            DO J = 1, NV    !Loop thru crop-specific headers
              IF (OLAB(J) == HD) THEN
                X(J) = DAT(I)
                EXIT
              ENDIF
            ENDDO

          END SELECT
        ENDDO
  
 100    CONTINUE
        CLOSE(LUNA)
        RETURN

!       Error handling
 5000   CONTINUE
        X = '   -99'
        WRITE (MSG(1),'(" Error in FILEA - Measured data not used")')
        CALL WARNING(1, "READA ", MSG)
      ENDIF

      CLOSE (LUNA)
      RETURN
      END SUBROUTINE READA

C=======================================================================
C  GETDESC, Subroutine C.H. Porter
C  Reads DATA.CDE to get descriptions for Measured and predicted data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written.
C  12/17/2004 CHP Increased length of PATHX (path for executable) to 120.
C=======================================================================

      SUBROUTINE GETDESC(COUNT, OLAB, DESCRIP)

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      IMPLICIT NONE

      CHARACTER*4 CODE, OLAB(*)
      CHARACTER*6, PARAMETER :: ERRKEY = 'GETDSC'
      CHARACTER*8  FILECDE
      CHARACTER*35 DESCRIP(*), LONGTEXT
      CHARACTER*78 DATAX
      CHARACTER*78 MSG(3)
      CHARACTER*255 PATHX
	CHARACTER*255 PATHSR

      INTEGER COUNT, ERR, I, LUN, LNUM
      INTEGER IPX,ipath

      LOGICAL FEXIST! RM = old EOF

      DATA FILECDE /'DATA.CDE'/
C-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      CALL GETPUT_CONTROL('GET', CONTROL) 

      DATAX = FILECDE
      INQUIRE (FILE = DATAX, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
!       File does not exist in data directory, check directory
!         with executable.
        !CALL GETARG(0,PATHX,IPX) RM getarg=GET_COMMAND_ARGUMENT
        CALL GET_COMMAND_ARGUMENT(0,PATHX,IPX)
        IPX = Ipath(PATHX)
c        IPX = INDEX(PATHX,' ')  ! Liwang Ma
        IF (IPX .NE. 0) THEN
          IPX = IPX - 1
        ENDIF
        DATAX = PATHX(1:(IPX-12)) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF        

      IF (.NOT. FEXIST) THEN
!       Last, check for file in C:\DSSAT4 directory
        pathsr=control % pathsr
        datax = trim(pathsr) // filecde
c        DATAX = "C:\RZWQM2\databases\dssat\" // FILECDE   !temp fix, Liwang Ma
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF

      IF (FEXIST) THEN
        CALL GETLUN('DTACDE',LUN)
        OPEN (LUN, FILE=DATAX, STATUS = 'OLD')
        
        DO I = 1, COUNT
          DESCRIP(I) = ' '
          REWIND (LUN)
          LNUM = 0
          !DO WHILE (.NOT. EOF (LUN))
          DO
            LNUM = LNUM + 1
            READ(LUN, '(A4,19X,A35)',END=20, IOSTAT=ERR) CODE, LONGTEXT
            IF (CODE .EQ. OLAB(I) .AND. ERR .EQ. 0) THEN
              DESCRIP(I) = LONGTEXT
              EXIT
            ENDIF
          ENDDO 
          !print *, i, " of ", count, olab(i), " ", descrip(i)
   20     IF (DESCRIP(I) .EQ. ' ') THEN
            DESCRIP(I) = OLAB(I)
          ENDIF
        ENDDO
        CLOSE(LUN)
        RETURN
      ELSE
!       Data.CDE file is missing -- stop program with message.
        CALL ERROR(ERRKEY, 29, FILECDE, 0)
      ENDIF

!     Data.cde file can not be found.  Just use OLAB (four character
!       code to fill description array.
        DO I = 1, COUNT
          DESCRIP(I) = OLAB(I)
        ENDDO

        WRITE(MSG(1),11) FILECDE
        WRITE(MSG(2),12) 
        WRITE(MSG(3),13) 
   11   FORMAT(' ',A8,' can not be found.')
   12   FORMAT(' Overview file will display variable labels ')
   13   FORMAT(' for simulated vs. measured data.')

        CALL WARNING(3, ERRKEY, MSG)
        CLOSE(LUN)

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE GETDESC
C=======================================================================

C=======================================================================
C  CHANGE_DESC, Subroutine C.H. Porter
C  Change units from 'YrDoy' to 'DAP' in descriptions for which date
!     conversion has been done.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written.
C=======================================================================

      SUBROUTINE CHANGE_DESC(Descrip)

C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER StartCol, I, LENGTH
      CHARACTER*1 CHAR, UPCASE
      CHARACTER(35) Descrip, NewDesc
C-----------------------------------------------------------------------
!     Convert string to uppercase - save as NewDesc
      LENGTH = LEN(TRIM(Descrip))
      NewDesc = ""
      DO I = 1, LENGTH
        CHAR = Descrip(I:I)
        CHAR = UPCASE(CHAR)
        NewDesc = NewDesc(1:I-1) // CHAR
      ENDDO

!     Find occurance of '(YRDOY)' in description string
      StartCol = INDEX(NewDesc,'(YRDOY)')
      IF (StartCol .GT. 0) THEN
        Descrip = 
     &    Descrip(1:StartCol-1) // '(dap)  ' // Descrip(StartCol+7:35)
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE CHANGE_DESC
C=======================================================================

C=======================================================================
C  READA_Dates, Subroutine C.H. Porter
C  Convert dates from READA to ouptut format
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  05-14-2002 CHP Written.
C=======================================================================
      SUBROUTINE READA_Dates(XDAT, YRSIM, IDAT)

      IMPLICIT NONE

      CHARACTER*6 XDAT
      REAL        RDAT
      INTEGER     ERRNUM, IDAT, ISIM, YR, YRSIM

      CALL YR_DOY(YRSIM, YR, ISIM)
      READ(XDAT(1:6),1000,IOSTAT=ERRNUM) RDAT
      IF (ERRNUM .NE. 0) CALL ERROR('READA ',2,'FILEA',0)
 1000 FORMAT(F6.0)
      IDAT = INT(RDAT)

      IF (IDAT .GT. 0 .AND. IDAT .LT. 1000) THEN
        IF (IDAT .GT. ISIM) THEN
          IDAT = YR*1000 + IDAT
        ELSE
          IDAT = (YR+1)*1000 + IDAT
        ENDIF

      ELSEIF (IDAT .GT. 0 .AND. IDAT .GE. 1000) THEN
        CALL Y2K_DOY(IDAT)
        !CALL FullYear (IDAT, YR, DOY)
        !IDAT = YR*1000 + DOY
      ENDIF

      RETURN
      END SUBROUTINE READA_Dates
C=======================================================================

C=======================================================================
C  PARSE_HEADERS, Subroutine C.H. Porter
C  Reads a line of headers and determines column widths for 
C     corresponding data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/05/2003 CHP Written.
C=======================================================================
      SUBROUTINE PARSE_HEADERS(CHAR, MAXCOL, HEADER, COUNT, COL)

      IMPLICIT NONE
      CHARACTER*(*) CHAR
      INTEGER MAXCOL
!     Up to MAXCOL headers per line, up to 10 characters long each
      CHARACTER*15  HEADER(MAXCOL), TEXT
      INTEGER COUNT, COL(MAXCOL,2), I, J, LENGTH
      LOGICAL SPACES

!     Initialize
      HEADER = '          '
      COUNT = 0
      COL = 0
      LENGTH = LEN(TRIM(CHAR))    !Don't use first character
      IF (LENGTH .LE. 1) RETURN   !No headers to parse, go back empty

      COUNT = 1
      COL(COUNT,1) = 1
      SPACES = .TRUE. !Allows for multiple spaces between headers

!     Look for spaces between headers
      DO I = 2, LENGTH       
!       A "!" signifies -- do not read the rest of the record 
        IF (CHAR(I:I) .EQ. '!') THEN
          LENGTH = I-1
          EXIT
        
        ELSEIF (CHAR(I:I) .EQ. ' ') THEN
          IF (SPACES) THEN
            CYCLE         !Treat multiple blanks as one
          ELSE
            COL(COUNT,2) = I - 1
            HEADER(COUNT) = ADJUSTL(CHAR(COL(COUNT,1):COL(COUNT,2)))
            COUNT = COUNT + 1
            COL(COUNT,1) = I + 1
            SPACES = .TRUE.
          ENDIF
        ELSE
          SPACES = .FALSE.
          CYCLE
        ENDIF
      ENDDO
      COL(COUNT,2) = LENGTH
      HEADER(COUNT) = ADJUSTL(CHAR(COL(COUNT,1):COL(COUNT,2)))
      HEADER(1) = ADJUSTL(CHAR(2:COL(1,2)))

!     Take out trailing periods from header names
      DO I = 1, COUNT
        TEXT = HEADER(I)
        LENGTH = LEN(TRIM(TEXT))
        DO J = LENGTH, 1, -1
          IF (TEXT(J:J) .EQ. '.' .OR. TEXT(J:J) .EQ. ' ') THEN
            LENGTH = LENGTH - 1
          ELSE
            EXIT
          ENDIF
        ENDDO
        HEADER(I) = TEXT(1:LENGTH)
      ENDDO
!--------------------------------------------------------------------
!     Need to adjust starting and ending columns for left-justified 
!         column headers
      DO I = 1, COUNT
        IF (TRIM(HEADER(I)) .EQ. 'SOIL_NAME') THEN
          !For SOIL_NAME, start in column 2 and read 50 characters
          COL(COUNT,1) = 2
          COL(COUNT,2) = 51
        ENDIF

        IF (TRIM(HEADER(I)) .EQ. 'SOIL_SCS') THEN
          !For SOIL_SCS, start 2 columns over from previous column 
          !  and read 50 characters
          IF (COUNT .GT. 1) THEN
            COL(COUNT,1) = COL(COUNT-1,2) + 2
            COL(COUNT,2) = COL(COUNT,1) + 49
          ELSE
            COL(COUNT,1) = 2
            COL(COUNT,2) = 51
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END SUBROUTINE PARSE_HEADERS
C=======================================================================



