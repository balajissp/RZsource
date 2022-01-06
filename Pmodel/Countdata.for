      SUBROUTINE Countdata (unit_no,Ndata)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine counts the number of data lines in a file
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  20/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.  Unit_No            INTEGER           -                  File Number that need to be read
C 2.  Ndata              INTEGER           -                  Number of Data line in the file
C 3.  Iostat             INTEGER           -                  Flag for file reading error
C
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 1. 
C
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================      
       IMPLICIT NONE
       INTEGER,INTENT(IN) :: Unit_no
       INTEGER,INTENT(OUT) :: Ndata
       INTEGER :: Iostat,i
       Ndata = 0   
        DO
           READ(unit_no,*,IOSTAT=iostat) 
           IF(Iostat/=0) EXIT
           Ndata = Ndata + 1
        END DO
       REWIND(unit_no)
       RETURN
      END SUBROUTINE Countdata