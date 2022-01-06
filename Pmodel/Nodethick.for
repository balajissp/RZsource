      SUBROUTINE NODETHICK(T,D,N)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine calculates the node/Soil layer thickness
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  30/03/2017       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.  N                  INTEGER           -                  Total Number of Nodes
C 2.  D                  REAL              m                  Depth of a Node
C 3.  T                  REAL              m                  Thickness  of a Node
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
       INTEGER,INTENT(IN):: N
       DOUBLE PRECISION,INTENT(IN),DIMENSION(N) :: D
       DOUBLE PRECISION,INTENT(OUT),DIMENSION(N) :: T
       INTEGER :: i
       T(1)=D(1)
       DO i=2,N
           T(i) = D(i) - D(i-1)
       END DO
       
       RETURN
        
       END SUBROUTINE NODETHICK