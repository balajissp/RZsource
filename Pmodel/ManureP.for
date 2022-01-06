       SUBROUTINE MANUREP(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine APPLY THE MANURE IN THE FIELD
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  29/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter 
C      S                  REAL               -                 Fraction of Manure left on surface during application
C      L                  REAL               -                 Fraction of Manure left on surface after infiltration
C      k                  REAL               -                 Loop Counter
C      Fact               REAL               -                 Factor based on which subsurface applied
C                                                              Manure P is added to soil profile
C      Sumfact            REAL               -                 Sum of the Factor
C      String             Character          -                 Holds the Formats of the output
C      N                  Character          -                 A Variable 
C      k                  REAL               -                 Loop Counter
C     
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
      INTEGER :: i,j,k,m
      DOUBLE PRECISION :: S,L,Fact,Sumfact
      CHARACTER(LEN=100)::String,N
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY

      DO i = 1,Nman
          
          IF(Day==Mday1(i).AND.Month==Mmon(i).AND.Year==Myear1(i)) THEN
          
          Slnm = i
          Manpmass(Slnm) = Appdaymanmass(Slnm)* Mantpper(Slnm)/100.00
          ManuP = Manpmass(Slnm)/AreaP
          Mancova(Slnm) = Areap * Manfc(Slnm)/100.00
          Manmoist(Slnm) = (1-Manwcper(Slnm)/100.00)
          Manmass(Slnm) = Appdaymanmass(Slnm)
          
          
          ResdueP0 = Tempu1-0.01
          
          S = Mlefts(Slnm)/100.00
          L = 1.0
          
          IF(Manwcper(Slnm)<=15.0)  L= 0.4
            
            DO j=1,Nsoil
                CALL KgHaToKg(Labp(j),Labp(j),Areap)
                CALL KgHaToKg(Actp(j),Actp(j),Areap)
            END DO
            
            
           Manwip(Slnm) = (Manpmass(Slnm)*S*L)*(Manweipper(Slnm)/100.00)
           Manwop(Slnm) = (Manpmass(Slnm)*S*L)*(Manweopper(Slnm)/100.00)
           Mansop(Slnm) = (Manpmass(Slnm)*S*L)*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.75
     +     
           Mansip(Slnm) = (Manpmass(Slnm)*S*L)*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.25
            
           Actp(1) = Actp(1)+(Manpmass(Slnm)*S*(1-L))*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.25
           
           Labp(1) = Labp(1) + (Manpmass(Slnm)*S*(1-L))
     +     *(Manweipper(Slnm)/100.00) + 
     +      (Manpmass(Slnm)*S*(1-L))*(Manweopper(Slnm)/100.00)*0.95 + 
     +      (Manpmass(Slnm)*S*(1-L))*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.75
     +     *0.95
            
           Labp(2) = Labp(2)+ (Manpmass(Slnm)*S*(1-L))*
     +     (Manweopper(Slnm)/100.00)*0.05 +  (Manpmass(Slnm)*S*(1-L))*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.75
     +     *0.05
           
           
          DO k = 1, Nsoil
              IF (Dsoil(k) > Mand(Slnm)) EXIT
          END DO
           
          Sumfact = 0.0
          
          DO j = 1, k-1
              
              Fact = Tsoil(j)/Mand(Slnm)
              Labp(j) = Labp(j) + (Manpmass(Slnm)*(1-S))
     +        *(Manweipper(Slnm)/100.00)*Fact + 
     +        (Manpmass(Slnm)*(1-S))*(Manweopper(Slnm)/100.00)*Fact + 
     +        (Manpmass(Slnm)*(1-S))*Fact*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.75
     +     
           Actp(j) = Actp(j)+(Manpmass(Slnm)*(1-S))*Fact*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.25
           
            Sumfact = Sumfact + Fact
            
          END DO  
          
            Fact = 1 - Sumfact 
            
            Labp(k) = Labp(k) + (Manpmass(Slnm)*(1-S))
     +        *(Manweipper(Slnm)/100.00)*Fact + 
     +        (Manpmass(Slnm)*(1-S))*(Manweopper(Slnm)/100.00)*Fact + 
     +        (Manpmass(Slnm)*(1-S))*Fact*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.75
     +     
           Actp(k) = Actp(k)+(Manpmass(Slnm)*(1-S))*Fact*
     +     (1- (Manweipper(Slnm)/100.00)-(Manweopper(Slnm)/100.00))*0.25
           
           DO j=1,Nsoil
                CALL KgToKgHa(Labp(j),Labp(j),Areap)
                CALL KgToKgHa(Actp(j),Actp(j),Areap)
           END DO
           
          WRITE(115,'(A,I2,X,A)')'Manure NO',Slnm,'Applied'
          WRITE(115,'(A,F10.5,A4)')'Manure Mass=',Manmass(Slnm),'Kg' 
          WRITE(115,'(A,F10.5,A4)')'Manure PMass=',Manpmass(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'Manure Dry Matter=',Manwcper(Slnm),
     + '%'     
          WRITE(115,'(A,F10.5,A4)')'Manure Moisture=',Manmoist(Slnm),'-'
          WRITE(115,'(A,F10.5,A4)')'Manure Cover Area=',
     +                  Mancova(Slnm)*10000,'m2'
          WRITE(115,'(A,F10.5,A4)')'Application Depth=',Mand(Slnm),'m'
          WRITE(115,'(A,F10.5,A4)')'% Left on Surface=',Mlefts(Slnm),'%'
          WRITE(115,'(A,F10.5,A4)')'MANSIP =',Mansip(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'MANSOP =',Mansop(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'MANWIP =',Manwip(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'MANWOP =',Manwop(Slnm),'Kg'
          
          
          IF(Manwcper(Slnm)<=15.00 .OR. Mand(Slnm)>0.0) THEN
          WRITE(115,'(/,A,/)')'Soil Labile & Active P pool are Updated'
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
          
          WRITE(115,'(/)')
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("ActiveP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Actp(m),m=1,Nsoil) 
          END IF
          
           END IF
      END DO
      
      END SUBROUTINE MANUREP