      SUBROUTINE BREAKNODE()
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine assigns P pools and other soil properties of the soil node layers
C     from the soil layer in which it belongs
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
       
 
      DO i=1,Nnode
       DO j=1,Nsoil
          IF((Dnode(i)-Dsoil(j))<0.001) THEN   
          Labpnode(i) = Labp(j)*Tnode(i)/Tsoil(j)
          ResdueP1node(i)=ResdueP1(j)*Tnode(i)/Tsoil(j)
          ResdueP2node(i)=ResdueP2(j)*Tnode(i)/Tsoil(j)
          HumusP1node(i)= HumusP1(j)*Tnode(i)/Tsoil(j)
          HumusP2node(i)= HumusP2(j)*Tnode(i)/Tsoil(j)
          HumusP3node(i)= HumusP3(j)*Tnode(i)/Tsoil(j)        
          Claynode(i) = Clayp(j)
          BDsoilnode(i) = BDsoil(j)
          EXIT    
          END IF
       END DO   
       
      END DO 
       
       
      END SUBROUTINE BREAKNODE