       SUBROUTINE LABPLOSS(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine calculates the soil P loss from soil labile P pool
C     through runoff
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
C      Kd1                 REAL               m3/Kg             Soil Partitioing Coefficient of first layer
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
       INTEGER:: m,j
       CHARACTER(LEN=100)::String,N
       DOUBLE PRECISION :: Kd1
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       
      
       
       Kd1 = (100.00 + 250.00*(Clayp(1)/100.00))*0.001
       Labpmlossrnf = (Labp(1)*Runoff*0.01*Pextr)/
     +               (Bdsoil(1)*Dsoil(1)*Kd1)
       
      
       IF(Labpmlossrnf>Labp(1)) Labpmlossrnf=Labp(1)
       Labp(1) = Labp(1) - Labpmlossrnf
       IF(Labp(1) < 0.0) Labp(1) = 0.0
       
       
       WRITE(115,'(/,A,/)')'DRP loss Through Runoff'
        
       WRITE(115,'(A,F12.5,A)')'Labile P Loss through Runoff =',
     +  Labpmlossrnf*1000,' gm/ha'
       
       IF(Labpmlossrnf>0.0) THEN
          WRITE(115,'(/,A,/)')'Soil Labile P pool are Updated'
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A10,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A12,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F12.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
          
       END IF
       
       END SUBROUTINE LABPLOSS