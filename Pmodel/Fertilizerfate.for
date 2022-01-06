       SUBROUTINE FERTILIZERFATE(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine simulates the fate of  fertilizer P due to rainfall and 
C     infiltration soil and loss in runoff after its application
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  25/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter 
C      Pdfactor           REAL               -                 P Distribution Factor Between Runoff and Infiltration
C      m                  INTEGER            -                 Loop Counter 
C      String             Character          -                 Holds the Formats of the output
C      N                  Character          -                 A Variable       
C
C FUNCTIONS/SUBROUTINE CALLED ::
C
C            NAME                    DESCRIPTION
C    ----------              ------------------           
C 1. KgHaToKg             This Subroutine Converts Variables from Kg/ha to Kg
C 2. KgToKgHa             This Subroutine Converts Variables from Kg to Kg/ha
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================   
       USE VARIABLE
       IMPLICIT NONE
       INTEGER :: i,j,m
       DOUBLE PRECISION :: Pdfactor
       CHARACTER(LEN=100)::String,N
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       DO i = 1, Slnf
            
        IF(Rain>0) THEN
             
             Norain(i) = Norain(i) + 1
           
             IF(Norain(i) == 1) THEN
                  Fertprelease(i) = Avfertp(i)
                  Avfertp(i) = Avfertp(i) - Fertprelease(i)
                  Fertpsorp(i) = 0.0
                  Dailyfertpsop(i)=0.0
             ELSEIF(Norain(i) == 2) THEN
                  Fertprelease(i) = Resfertp(i)*0.4 
                  Resfertp(i) = Resfertp(i) - Fertprelease(i)
             ELSEIF(Norain(i)>2) THEN
                  Fertprelease(i) = Resfertp(i)*0.075
                  Resfertp(i) = Resfertp(i) - Fertprelease(i)
             ENDIF
                
             IF (Resfertp(i)<0.001) Resfertp(i)= 0.0
             
        ELSE
            Fertprelease(i)=0.0
        END IF
        
             
        IF (Norain(i)==0) THEN
               
         SELECT CASE (Scov(i))
         CASE(1)
             Fertpfr(i) = -0.12*LOG(Daysfert(i))+0.65
         CASE(2)
             Fertpfr(i) = -0.12*LOG(Daysfert(i))+0.75
         CASE(3)
             Fertpfr(i) = -0.12*LOG(Daysfert(i))+0.85
         END SELECT    
         
         Avfertp(i) = Appdayavfertp(i) * Fertpfr(i)
         Fertpsorp(i) = Appdayavfertp(i)-AvfertP(i)
        
        
         IF(Avfertp(i) < 0.0) THEN
          Avfertp(i) = 0.0
          Fertpsorp(i) = Appdayavfertp(i)
         END IF
         
         Dailyfertpsop(i) = Fertpsorp(i) - Dummy2(i) 
         
         CALL KgHaToKg(Labp(1),Labp(1),Areap)
          Labp(1) = Labp(1) + Dailyfertpsop(i)
         CALL KgToKgHa(Labp(1),Labp(1),Areap)
      
         Daysfert(i) = Daysfert(i) + 1 
         
         END IF
        
        IF(Rain > 0) THEN
            
         Pdfactor = 0.034 * EXP(3.4*Runoff/
     +   Rain)
         
         Fertpcrunoff(i) = (Fertprelease(i)*Pdfactor)/
     +   (Rain*Areap*100.00)
              
        ELSE
            
           Fertpcrunoff(i)=0.0
           
        END IF
        
         Fertpmrunoff(i) = Fertpcrunoff(i)*Runoff*
     +   Areap*100.00 
         
         IF(Fertpmrunoff(i)>Fertprelease(i)) THEN
          Fertpmrunoff(i)=Fertprelease(i)     
         END IF
         
       DO J=1,Nsoil
           CALL KgHaToKg(Labp(j),Labp(j),Areap)
       END DO
       
         Labp(1) = Labp(1) + (Fertprelease(i)-Fertpmrunoff(i))*0.8
         Labp(2) = Labp(2) + (Fertprelease(i)-Fertpmrunoff(i))*0.2
         
         FertPLabP1(i) = (Fertprelease(i)-Fertpmrunoff(i))*0.8    !Kg
         FertPLabP2(i) = (Fertprelease(i)-Fertpmrunoff(i))*0.2    !Kg
         
         
       DO J=1,Nsoil
           CALL KgToKgHa(Labp(j),Labp(j),Areap)
       END DO 
       
      IF(Avfertp(i)>0.0 .OR. Resfertp(i)>0.0) THEN
        
       WRITE(115,'(/,A,I2,/)')'Daily Fertilizer P Dynamics: Fertilizer N
     +O'                                                    ,i
       
      IF(Norain(i)==0) THEN
          
      WRITE(115,'(A,I2,A)')'No Rainfall has occured since Fertilizer NO'
     +  ,i,        ' Application.The Fertilizer P is abosorbed into the 
     +  Soil Labile P Pool of the first soil layer.'
     
      WRITE(115,'(A,I2,A,I2,A)')'Number of Days of No rainfall since Fer
     +tilizer NO ',i,' Application =',INT(Daysfert(i)-1),' Day'
       
      WRITE(115,'(A,F12.5,A)')'Fertilizer P Sorped =',
     +                                       Dailyfertpsop(i)*1000,' gm'
      WRITE(115,'(A,F12.5,A6)')'Available Fertilizer P=',Avfertp(i),
     +                                                        ' Kg'
      WRITE(115,'(A,F12.5,A6)')'Residual Fertilizer P=',Resfertp(i)
     +                                                ,       'Kg' 
      ELSE
          
      IF(Rain>0) THEN  
      WRITE(115,'(A)'),'Rainfall  Happens'    
      WRITE(115,'(A,I2,A,I5,A)')'Number of Rainy days since Fertilizer N
     +O',i,' Application =',Norain(i),' Day'      
      END IF
      
      WRITE(115,'(A,F12.5,A)')'Amount of Fertilizer P Released =',
     + Fertprelease(i)*1000,' gm' 
      WRITE(115,'(A,F12.5,A6)')'Available Fertilizer P=',Avfertp(i)
     +                                                        ,'Kg'
      WRITE(115,'(A,F12.5,A6)')'Residual Fertilizer P=',Resfertp(i)
     +                                                ,       'Kg'
      
      END IF
      
      
      WRITE(115,'(A,F12.5,A6)')'Fertilizer P Mass in Runoff',
     +                                Fertpmrunoff(i)*1000,' gm'
      
      
      IF(Dailyfertpsop(i)>0.0 .OR.Fertprelease(i)>0.0) THEN
          WRITE(115,'(/,A,/)')'Soil Labile P pools are Updated'
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A10,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A12,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F12.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
      END IF
      
          
      END IF
       
       
       
       END DO
       
       END SUBROUTINE FERTILIZERFATE