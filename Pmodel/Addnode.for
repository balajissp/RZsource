      SUBROUTINE ADDNODE()
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine add the P pools of the nodes to the P pools of the soil layers
C      
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  31/03/2017       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C       i                  INTEGER            -                 Loop Counter 
C       j                  INTEGER            -                 Loop Counter 
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
      USE VARIABLE
      IMPLICIT NONE
      INTEGER :: i,j    

      DO i=1,Nsoil
          Labp(i) = 0.0
          ResdueP1(i) =0.0
          ResdueP2(i) =0.0
          HumusP1(i) = 0.0
          HumusP2(i) = 0.0
          HumusP3(i) = 0.0
                  
      END DO
      
      
      DO i=1,Nnode
       DO j=1,Nsoil  
          IF((Dnode(i)-Dsoil(j))<0.001) THEN      
          Labp(j) = Labp(j) + Labpnode(i)
          ResdueP1(j) = ResdueP1(j) + ResdueP1node(i)
          ResdueP2(j) = ResdueP2(j) + ResdueP2node(i)
          HumusP1(j) = HumusP1(j) + HumusP1node(i)
          HumusP2(j) = HumusP2(j) + HumusP2node(i)
          HumusP3(j) = HumusP3(j) + HumusP3node(i)  
          EXIT
          END IF
       END DO   
       
      END DO 
       
       
      END SUBROUTINE ADDNODE