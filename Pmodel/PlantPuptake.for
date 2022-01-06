       SUBROUTINE PLANTPUPTAKE(ID,IM,IYYY,GS)
      
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine Calculates daily plant P uptake
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  12/08/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter 
C      Phufrac            REAL               -                 Fraction of potential heat unit at maturity
C                                                              accumulated on a day
C      p1                 REAL               -                 Shape coefficient
C      p2                 REAL               -                 Shape coefficient
C      f1                 REAL               -                 Fraction used to calulate p1,p2
C      f2                 REAL               -                 Fraction used to calulate p1,p2
C      Puptop             REAL               Kg/ha             P uptake from the top of the soil layer
C      Pupbtm             REAL               Kg/ha             P uptake from the botom of the soil layer
C
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C    KgHaToKg             This Subroutine Converts Variables from Kg/ha to Kg
C    KgToKgHa             This Subroutine Converts Variables from Kg to Kg/ha
C    CROPDAYCHECK         The Subroutine checks if on a particularday there is 
C                         standing crop on the field or not
C    HEATUNIT             The Subroutine CALCULATES THE HEAT UNIT OF A PARTICULAR DAY 
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================  
       USE VARIABLE
       IMPLICIT NONE
       INTEGER :: i,j,m
       DOUBLE PRECISION, DIMENSION(:) :: Phufrac(Ncrop)
       DOUBLE PRECISION :: p1,p2,f1,f2,Pupbtm,Puptop,thetap,GS
       LOGICAL :: Flag1
       CHARACTER(LEN=100)::String,N
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       Flag1 = .False.
       PlntP_Up = 0.0
       
        DO i =1,Ncrop
          IF(Day==Cpday(i).AND.Month==Cpmon(i).AND.Year==Cpyear(i))THEN
              WRITE(115,'(/,A,I2,X,A)')'Crop No',i,"is Planted"
           END IF
        END DO 
       
       CALL CROPDAYCHECK(ID,IM,IYYY)
      
             
       IF(Cropday) THEN
           
             WRITE(115,'(/,A,I2,X,A)')'Crop No',Slnc,"is at Field"
             WRITE(115,'(A,F15.5,X,A)')'Actual Plant Biomass =',
     +                        Actbiomass,"Kg/ha"
             WRITE(115,'(A,F15.5,X,A)')'Potential Plant Biomass =',
     +                        Potbiomass,"Kg/ha"
             WRITE(115,'(A,F15.5,X,A)')'AboveGround Plant Biomass =',
     +                        Avgrbiom,"Kg/ha"
             WRITE(115,'(A,F15.5,X,A)')'BelowGround Plant Biomass =',
     +                        Blgrbiom,"Kg/ha"
             WRITE(115,'(A,F12.5,X,A)')'Yeild =',
     +                        Yeild,"Kg/ha"
             WRITE(115,'(A,F12.5,X,A)')'Leaf Areap Index =',
     +                        LAIP," "
             WRITE(115,'(A,F12.5,X,A)')'Plant Height =',
     +                        Plnth,"m"
             WRITE(115,'(A,F12.5,X,A)')'Root Depth =',
     +                        Rootd,"m"   
          
         CALL HEATUNIT(ID,IM,IYYY)
         
         WRITE(115,'(/,A,F12.5)')'Heat Unit Accumulated = ', Hu(Slnc)
         
         Phufrac(Slnc) =  GS   !Hu(Slnc)/Phumat(Slnc)
         if (Biopfracemg(Slnc)-Biopfracmat(Slnc).gt.0.0d0) then
         f1 = (Biopfrac50mat(Slnc)-Biopfracmat(Slnc))/  
     +        (Biopfracemg(Slnc)-Biopfracmat(Slnc))
         
         f2 = 0.00001/(Biopfracemg(Slnc)-Biopfracmat(Slnc))
         else
         print*, 'P at emergence <= P at maturity, Pls Correct!'
         stop
         endif
         p2 = (LOG(0.5/(1-f1)-0.5)-LOG(1.0/(1-f2)-1.0))/0.5
         p1 = LOG(0.5/(1-f1)-0.5) + p2*0.5
         
         Biopfrac(Slnc) = (Biopfracemg(Slnc)-Biopfracmat(Slnc))*
     +    (1-Phufrac(Slnc)/(Phufrac(Slnc)+EXP(p1-p2*Phufrac(Slnc))))
     +   + Biopfracmat(Slnc)
         
     
       Biopopt(Slnc)=MAX(Biopfrac(Slnc)*Actbiomass,0.0)
         
       IF(Biopopt(Slnc)<Biopact(Slnc)) Biopopt(Slnc) = Biopact(Slnc)
       
       WRITE(115,'(2(A,F12.5))')'P1= ',p1,'     P2= ',p2
       WRITE(115,'(A,F12.5)') 'Optimum P fraction in Biomass= ',
     +                                                  Biopfrac(Slnc)
       WRITE(115,'(A,F12.5,A)') 'Optimum P mass in Biomass= ',  
     +                                      Biopopt(Slnc)*1000,' gm/ha'
       WRITE(115,'(A,F12.5,A)') 'Actual P mass in Biomass= ',  
     +                                      Biopact(Slnc)*1000,' gm/ha'
         
         Plantpdemand(Slnc)= MAX(1.5*Min(Biopopt(Slnc)-Biopact(Slnc),
     +    4.0*Biopfracmat(Slnc)*Dlypotbiomsincrs),
     +        0.0)    
         
       WRITE(115,'(A,F12.5,A)') 'Plant P Demand = ',  
     +                                  Plantpdemand(Slnc)*1000,' gm/ha'
         
      IF(Rootd>0.0) THEN
         DO j = 1, Nsoil
           IF (Dsoil(j) >= Rootd) EXIT
         END DO 
      ELSE
           j = 0
      ENDIF
      
       WRITE(115,'(A,I3)')'Plant P Demand can be Filled up from the Labi
     +le P pools of the Soil layers upto soil layer no ',j    
       
        DO i = 1,j
           Puptop = (Plantpdemand(Slnc)/(1-EXP(-Pupdist(Slnc))))*
     + (1-EXP(-Pupdist(Slnc)*(Dsoil(i)-Tsoil(i))/
     +                             Rootd))
           Pupbtm = (Plantpdemand(Slnc)/(1-EXP(-Pupdist(Slnc))))*
     +    (1-EXP(-Pupdist(Slnc)*Dsoil(i)/
     +                            Rootd))
          Potpuptake(i) = MAX(Pupbtm - Puptop,0.0)
          
          WRITE(115,'(/,A10,I2)')'Layer No: ',i
          WRITE(115,'(3A20)')'Potential P Uptake','Plant P Demand',
     +                                       'Labile P'
          WRITE(115,'(3A20)')'gm/ha','gm/ha','gm/ha'
     +                                       
          WRITE(115,'(3F20.5)') Potpuptake(i)*1000,
     +                          Plantpdemand(Slnc)*1000,Labp(i)*1000
          
          Actpuptake(i) = MIN(Potpuptake(i),Plantpdemand(Slnc),Labp(i))
     +                                                              
          IF(Actpuptake(i)>0.0) Flag1 = .True.
          
          WRITE(115,'(A,F12.5,A)')'Actual P Uptake = ',
     +                                     Actpuptake(i)*1000,'gm/ha'
          Plantpdemand(Slnc) = Plantpdemand(Slnc)-Actpuptake(i)
          IF(Plantpdemand(Slnc)<=0.0) Plantpdemand(Slnc) = 0.0
          Labp(i) = Labp(i)-Actpuptake(i)
          IF(Labp(i)<=0.0) Labp(i) = 0.0
          Biopact(Slnc) = Biopact(Slnc)+Actpuptake(i)       
        END DO
        
        PlntP = Biopact(Slnc)
        
        IF(Biopopt(Slnc)>0.0 .AND. Psts) THEN
      !      thetap = 200.0 *(Biopact(Slnc)/Biopopt(Slnc)-0.5)    ! Should be 0.5 not 0.05, Neitsch SWAT 2009 Eq 5.3.1.9 P375; Zhiming
      !      Pstress(Slnc) = thetap/(thetap+EXP(3.535-0.02597*thetap))  ! Not used now. Zhiming
      ! Pstress was found to be negative in some cases; 
      ! so min is set for Pstress; zhiming
      !   IF(Biopopt(Slnc) /= Actbiomass* Biopfracmat(Slnc))   then
      !  Pstress(Slnc) = (Biopact(Slnc) - Actbiomass* Biopfracmat(Slnc))
      !   +                / (Biopopt(Slnc) - Actbiomass* Biopfracmat(Slnc))   ! Zhiming is trying new stress calculation 
      !         ELSE 
      !        Pstress(Slnc) = 0.0
      !      END IF
       Pstress(Slnc) = Biopact(Slnc) / Biopopt(Slnc)       ! Zhiming is trying new stress calculation
            IF(Pstress(Slnc) <0.0)      Pstress(Slnc) = 0.0
            IF(Pstress(Slnc) > 1.0)     Pstress(Slnc) = 1.0
 
            WRITE(115,'(/,A,F12.5,A)')'Plant P stress = ',
     +                Pstress(Slnc),' (1-No Stress,0-Full Stress)'
        ELSE
            Pstress(Slnc) = 1.0
        END IF
        
        
        WRITE(115,'(/,A,F12.5,A)')'Actual P in Plant Biomass = ',
     +                                     Biopact(Slnc)*1000,' gm/ha'
        
         IF(Flag1) THEN
          WRITE(115,'(/,A,/)')'Soil Labile P  pool is Updated'
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A10,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A12,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F12.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
        END IF
        
       ELSE
           DO i = 1, Nsoil
             Actpuptake(i) = 0.0
           END DO
           Pstress(Slnc) = 1.0  !zhiming added this to aviod no value before planting           
       END IF
       
       DO i = 1,Nsoil
           PlntP_Up = PlntP_Up + Actpuptake(i)
       END DO
         
      END SUBROUTINE PLANTPUPTAKE