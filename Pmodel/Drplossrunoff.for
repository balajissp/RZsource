       SUBROUTINE DRPLOSSRUNOFF(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine calculates the DRP loss through runoff
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  23/08/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C     
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================  
      USE VARIABLE
      IMPLICIT NONE
      INTEGER :: i,j,k
      INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
      
      Fertpmlossrnf = 0.0
      Manpmlossrnf  = 0.0
      
      DO i = 1, Slnf
         Fertpmlossrnf = Fertpmlossrnf + Fertpmrunoff(i)
      END DO
      
      DO i = 1, Slnm
          Manpmlossrnf = Manpmlossrnf + Manipmrunoff(i)
          Manpmlossrnf = Manpmlossrnf + Manopmrunoff(i)
      END DO
      
      CALL KgToKgha(Fertpmlossrnf,Fertpmlossrnf,Areap)
      CALL KgToKgHa(Manpmlossrnf,Manpmlossrnf,Areap)
      
      Drplossrnf = Labpmlossrnf + Fertpmlossrnf + Manpmlossrnf
      
      WRITE(115,'(A,F12.5,A)')'Fertilizer P Loss through Runoff =',
     +  Fertpmlossrnf*1000,' gm/ha'
      WRITE(115,'(A,F12.5,A)')'Manure P Loss through Runoff =',
     +  Manpmlossrnf*1000,' gm/ha'     
      WRITE(115,'(A,F10.5,A)')'DRP  Loss through Runoff =',
     + Drplossrnf*1000,' gm/ha'
      
      
      END SUBROUTINE DRPLOSSRUNOFF