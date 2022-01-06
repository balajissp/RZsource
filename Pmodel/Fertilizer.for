       SUBROUTINE FERTILIZER(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine Apply the Fertilizer in the Field
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  21/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C 1.  i                  INTEGER            -                 Loop Counter
C 2.  k                  INTEGER            -                 Variable to track no of soil layers
C                                                             that have higher depth than fertilizer 
C                                                             application depth
C 3.  m                  INTEGER            -                 Loop Counter
C 4.  Fact               REAL               -                 Factor based on which subsurface applied
C                                                             fertilizer P is added to soil profile
C 5.  Sumfact            REAL               -                 Sum of the Factor
C 6.  j                  INTEGER            -                 Loop Counter    
C 7.  String             Character          -                 Holds the Formats of the output
C 8.  N                  Character          -                 A Variable 
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 1. KgHaToKg             This Subroutine Converts Variables from Kg/ha to Kg
C 2. KgToKgHa             This Subroutine Converts Variables from Kg to Kg/ha
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================   
      USE VARIABLE
      IMPLICIT NONE
      INTEGER :: i,k,m,j
      DOUBLE PRECISION :: Fact,Sumfact
      CHARACTER(LEN=100)::String,N
      INTEGER,INTENT(IN) :: ID,IM,IYYY
      INTEGER :: Day,Month,Year
      DAY = ID
      Month = IM
      Year = IYYY
      DO i = 1, Nfert
          IF(Day==Fday(i).AND.Month==Fmon(i).AND.Year==Fyear(i)) THEN
           Slnf = i 
           Daysfert(Slnf) = 1
           Norain(slnf) = 0
            FertilizerP = Fertp(slnf) / AreaP
            IF(Fertd(slnf) == 0) THEN
             Flefts(slnf) = Flefts(slnf)/100.00    
             Appdayavfertp(slnf) =  Fertp(slnf)*0.75
             Appdayresfertp(slnf) = Fertp(slnf)*0.25
            ELSE
              Flefts(slnf) = Flefts(slnf)/100.00                       ! Converts Fertilizer lefts of on surface from % to Fraction
              Appdayavfertp(slnf) = Fertp(slnf)*0.75*Flefts(slnf)
              Appdayresfertp(slnf) = Fertp(slnf)*0.25*
     +        Flefts(slnf)
              
              DO k = 1, Nsoil
                  IF (Dsoil(k) > Fertd(Slnf)) EXIT
              END DO
              
              DO m=1,Nsoil
                  CALL kgHaToKg(Labp(m),Labp(m),Areap)
              END DO
              
                Sumfact = 0.0
                
              DO m=1,k-1
                  Fact = Tsoil(m)/Fertd(Slnf)
                  Labp(m)=Labp(m)+Fertp(Slnf)*(1.0-Flefts(Slnf))*Fact
                  Sumfact = Sumfact + Fact
              END DO
              
              Fact = 1 - Sumfact
              
              Labp(k) = Labp(k)+Fertp(Slnf)*(1.0-Flefts(Slnf))*Fact
              
              DO m=1,Nsoil
                  CALL kgToKgha(Labp(m),Labp(m),Areap)
              END DO
              
            END IF
            
          Avfertp(Slnf)= Appdayavfertp(slnf)
          Resfertp(Slnf) = Appdayresfertp(Slnf)
          
          WRITE(115,'(A,I2,X,A,/)')'Fertilizer NO',Slnf,'Applied'
          WRITE(115,'(A,F12.5,A4)')'Fertilizer PMass=',Fertp(Slnf),'Kg' 
          WRITE(115,'(A,F12.5,A4)')'Application Depth=',Fertd(Slnf),'m'
          WRITE(115,'(A,F12.5,A4)')'% Left on Surface=',Flefts(Slnf)
     +                                                   *100.0 , '%'   
          WRITE(115,'(A,F12.5,A6)')'Available Fertilizer P=',
     +                                            Avfertp(Slnf)    ,'Kg'
          WRITE(115,'(A,F12.5,A6)')'Residual Fertilizer P=',
     +                                        Resfertp(Slnf),'Kg'

         IF(Fertd(Slnf)>0.0) THEN
          WRITE(115,'(/,A,/)')'Soil Labile P pools are Updated'
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
        END IF 
                    
          END IF
          
          
      END DO
       
      END SUBROUTINE FERTILIZER