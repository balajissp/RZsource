C=======================================================================
C  ERROR, Subroutine, N.B. Pickering
C  Outputs error messages to screen from file ERROR.DAT
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/1991 NBP Written
C  10/31/1994 GH  Add option to read MODEL.ERR from model EXE path
C  12/04/2001 CHP Added call to GETLUN for unit number.
!  11/05/2002 AJG Increased the size of PATHX.
C  07/21/2003 CHP Added call to WARNING.out for error messages.  
C                 Added generic messages for open and read errors.
C  11/23/2004 CHP Increased length of PATHX (path for executable) to 120.
C-----------------------------------------------------------------------
C  Input : ERRKEY,ERRNUM,FILE,LNUM)
C  Output: message to screen
C  Local :
C  Ifile : MODEL.ERR
C  FN/SUB: FIND
C=======================================================================

      SUBROUTINE ERROR (ERRKEY,ERRNUM,FILE,LNUM)

      USE ModuleDefs     !Definitions of constructed variable types, 
      IMPLICIT      NONE

      CHARACTER*(*) ERRKEY,FILE
      CHARACTER*9   EFILE
      CHARACTER     AKEY*6,BLANK*80,KEY*6,LINE*80
      CHARACTER*78  MSG(10)
      CHARACTER*100 ERRORX, SAVE_ERRORX
      CHARACTER*255 PATHX
	CHARACTER*255 PATHSR

      INTEGER       ANUM,ERRNUM,LNUM, LUN, LUNIT, I , ELUN
      INTEGER       IMSG, IO
      INTEGER(KIND=4)     IPX

      LOGICAL       FOUND,FEXIST

      PARAMETER     (BLANK = ' ')
      TYPE (ControlType) CONTROL
      CALL GETPUT_CONTROL('GET', CONTROL) 

      IMSG = 1
      EFILE = 'ERROR.OUT'
      CALL GETLUN('ERRORO', ELUN)

      INQUIRE (FILE = EFILE, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = ELUN, FILE = EFILE, STATUS = 'OLD', 
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = ELUN, FILE = EFILE, STATUS = 'NEW')
        WRITE(ELUN,'("*RUN-TIME ERRORS OUTPUT FILE",//)')
      ENDIF

C-SUN CALL GETARG(0,PATHX)
      !CALL GETARG(0,PATHX,IPX) RM getarg=GET_COMMAND_ARGUMENT
      CALL GET_COMMAND_ARGUMENT(0,PATHX,IPX)

      IF (PATHX(IPX-3:IPX-3) .EQ. '.') THEN
          ERRORX = PATHX(1:(IPX-12)) // 'MODEL.ERR   '
        ELSE
          ERRORX = PATHX(1:(IPX-8)) // 'MODEL.ERR   '
      ENDIF

!     If ERRORX file is not in executable directory, try std. location
      INQUIRE (FILE = ERRORX, EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        SAVE_ERRORX = ERRORX
        pathsr = control % pathsr
        errorx = trim(pathsr)//"model.err"
c        ERRORX = 'C:\RZWQM2\databases\dssat\MODEL.ERR'    !temp fix, Liwang Ma
      ENDIF

      INQUIRE (FILE = ERRORX,EXIST = FEXIST)
      IF (FEXIST) THEN

         CALL GETLUN('ERRORX', LUN)
         OPEN (LUN,FILE=ERRORX,STATUS='OLD')
C
C        Initialization
C
         FOUND = .FALSE.
         IF (ERRNUM .GT. 6000 .OR. ERRNUM .LT. 0) THEN
            KEY = 'MISC  '
         ELSE
            KEY = ERRKEY
         ENDIF
C
C        Loop to search for error message in file MODEL.ERR.
C
   10    DO WHILE(.TRUE.)
           READ (LUN,'(A)',END=20) LINE
           AKEY = LINE(1:6)
           IF (AKEY .EQ. KEY) THEN
              READ (LINE,'(6X,I5)') ANUM
              IF (ANUM .EQ. ERRNUM) THEN
                 FOUND = .TRUE.
                 GOTO 20
              ENDIF
            ELSE
              FOUND = .FALSE.
           ENDIF
         ENDDO

   20    IF (FOUND) THEN
            WRITE (*,*)
            WRITE (ELUN,*)
   30       READ  (LUN,'(A)',END=40) LINE
            IF (LINE .NE. BLANK) THEN
               WRITE (*,*) LINE
               WRITE (ELUN,*) LINE
               WRITE(MSG(IMSG),'(A77)') LINE  ; IMSG = IMSG+1
               GOTO 30
            ENDIF
          ELSEIF (KEY .NE. 'GENERI') THEN
          !Check for generic message numbers
             KEY = 'GENERI'
             REWIND (LUN)
             GO TO 10

!        As an alternate, could have generic messages generated in code.
!            CALL GENERIC_MSG(ERRNUM, LINE)
!            WRITE (*,'(/,A78,/)') LINE
!            WRITE (ELUN,'(/,A78,/)') LINE
!            WRITE (MSG(IMSG),'(A78)') LINE
!            IMSG = IMSG + 1

          ELSE
!           Could not find error message in file
            WRITE (MSG(IMSG),'(A,A,I5)') 'Unknown ERROR. ',
     &           'Error number: ',ERRNUM
            WRITE (ELUN,'(/,A78,/)') MSG(IMSG)
            WRITE (*,'(/,A78)') MSG(IMSG)
            WRITE (*,'(A,A,/)') "Please check WARNING.OUT file for ",
     &            "additional information."
            IMSG = IMSG + 1
          ENDIF

   40    IF (FILE .EQ. ' ') THEN
            WRITE (*,'(2A/)') 'Error key: ',ERRKEY
            WRITE (ELUN,'(2A/)') 'Error key: ',ERRKEY
            WRITE (MSG(IMSG),'(2A)') 'Error key: ',ERRKEY
            IMSG = IMSG + 1
          ELSE
            WRITE (*,'(3A,I5,2A/)')
     &    'File: ',FILE,'   Line: ',LNUM,'   Error key: ',ERRKEY
            WRITE (ELUN,'(3A,I5,2A/)')
     &    'File: ',FILE,'   Line: ',LNUM,'   Error key: ',ERRKEY
            I = MIN(LEN(FILE),37)
            WRITE (MSG(IMSG),'(3A,I5,2A)')
     &    'File: ',FILE(1:I),'   Line: ',LNUM,'   Error key: ',ERRKEY
            IMSG = IMSG + 1
         ENDIF
      ELSE
C                                                                !BDB
C        Tell user that error file can not be found and give a   !BDB
C        generic error message.                                  !BDB
C                                                                !BDB
         ERRORX = SAVE_ERRORX
         WRITE (*,50) TRIM(ERRORX)
         WRITE (ELUN,50) TRIM(ERRORX)
         WRITE (MSG(IMSG),51) TRIM(ERRORX)
         IMSG = IMSG + 1
   50    FORMAT('Could not locate error file: ',A,/)
   51    FORMAT('Could not locate error file: ',A48)

         !Check for generic message numbers
         CALL GENERIC_MSG(ERRNUM, LINE)
         WRITE (*,'(/,A78,/)') LINE
         WRITE (ELUN,'(/,A78,/)') LINE
         WRITE (MSG(IMSG),'(A78)') LINE
         IMSG = IMSG + 1

         WRITE (*,60)  FILE, LNUM, ERRKEY   
         WRITE (ELUN,60)  FILE, LNUM, ERRKEY   
         WRITE (MSG(IMSG),60) FILE, LNUM, ERRKEY
         IMSG = IMSG + 1
   60    FORMAT('File: ',A12,'   Line: ',I5,' Error key: ',A)
      ENDIF

      WRITE (*, *) CHAR(7)
      WRITE (*,260,ADVANCE='no')
260   FORMAT (1X,'Please press < ENTER > key to continue ',2X)
c      READ  (*, *)

      CLOSE (LUN)
      CLOSE (ELUN)
      CALL WARNING(IMSG-1, ERRKEY, MSG)

      INQUIRE (FILE = "LIST.OUT", EXIST = FEXIST)
      IF (FEXIST) THEN
        CALL GETLUN('LIST', LUN)
        OPEN (FILE="LIST.OUT", UNIT=LUN)
        !Skip over first four lines in LIST.OUT file
        DO I=1,4
          READ(LUN,'(A80)') LINE
        ENDDO

        !Read list of unit numbers that have been opened and close each
        !DO WHILE (.NOT. EOF(LUN)) - RM old EOF variable
        DO
          READ(LUN, '(I4)',IOSTAT=IO) LUNIT
          IF (IO /= 0) EXIT ! ERROR/EOF
          IF (LUNIT .NE. LUN) THEN
            CLOSE (LUNIT)
          ENDIF
        ENDDO
      ENDIF

      CLOSE (LUN)
      STOP 99
      END SUBROUTINE ERROR

!=========================================================================
      SUBROUTINE GENERIC_MSG(ERRNUM, MESSAGE)
!     If error messages cannot be found in MODEL.ERR file, or if MODEL.ERR
!     file cannot be found, check for generic message type.

      IMPLICIT NONE
      INTEGER ERRNUM
      CHARACTER*(*) MESSAGE

      !Check for generic message numbers
      SELECT CASE(ERRNUM)
        CASE(29)
          WRITE(MESSAGE,35) 'File not found. Please check ',
     &      'file name or create file. Error number: ', ERRNUM 
        CASE(33)
          WRITE(MESSAGE,35) 'End of file encountered. ',
     &      'Error number: ',ERRNUM
        CASE(59)
          WRITE(MESSAGE,35) 'Syntax error. ',
     &      'Error number: ',ERRNUM
        CASE(64)
          WRITE(MESSAGE,35) 'Invalid format in file. ',
     &      'Error number: ', ERRNUM
        CASE DEFAULT 
          WRITE(MESSAGE,35) 'Unknown ERROR. ',
     &      'Error number: ',ERRNUM
      END SELECT

   35 FORMAT(A,A,I5)

      END SUBROUTINE GENERIC_MSG
!=========================================================================
