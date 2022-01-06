       SUBROUTINE TILLAGE(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine APPLY THE TILLAGE IN THE FIELD
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  11/08/2016       D SADHUKHAN          ORIGNIAL CODE
C 2.  07/1/2021         Zhiming Qi           tillage incorporation & mixing efficiency 
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter 
C      k                  INTEGER            -                 Loop Counter 
C      Soilmass           REAL               Kg                Soil mass of a layer 
C      Tsoilmass          REAL               Kg                Toltal soil mass
C      Tlabp              REAL               Kg                Total Labile P
C      Tactp              REAL               Kg                Total Active P
C      Tstabip            REAL               Kg                Total Stable Inorganic P
C      Tfrsop             REAL               Kg                Total Fresh Organic P
C      Tstabop            REAL               Kg                Total Stable Organic P
C      Tcrpres            REAL               Kg                Total Crop Residue
C      Ratio              REAL               Kg                Ratio of Soil mass of a layer to Total Soil Mass
C   Tillage incorporation coefficnet and mixing coefficient are different concepts
C   They both from the EPIC model, please see our RZ-P manual for details (Zhiming Qi)
C   (See P Vadas Surphos or Knisel 199,  FHANTM-MACP database)
C      Tillinceffi        REAL               -                Tillage incorporation coefficnet, same as tillage intensity in RZWQM
C      Tillmixeffi        REAL               -                Manure P mixing coefficient due to tillage; see table in RZ-P Manual
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
      INTEGER :: i,j,k,m,l,t
      DOUBLE PRECISION :: Tsoilmass,Tlabp,Tactp,Ratio,Tstabip,Tfrsop,
     + Tstabop,Tcrpres
      DOUBLE PRECISION :: TresdueP1,TresdueP2,ThumusP1,ThumusP2,ThumusP3
      DOUBLE PRECISION ,DIMENSION(:) :: Soilmass(Nsoil)
      DOUBLE PRECISION :: Sr
      CHARACTER(LEN=100)::String,N
      INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
      DO i = 1, 200
          IF(Day==Tday1(i).AND.Month==Tmon(i).AND.Year==Tyear(i)) THEN
           Slnt = i  
           WRITE(115,'(/,A,I2,X,A)')'Tillage',i,"is Applied"  
           WRITE(115,'(A,F10.5,X,A)')'Depth of Tillage =',Tilld(i),"m"
           WRITE(115,'(A,F10.5,X,A)')'Incorporation Efiiciencty =',
     +                           Tillinceffi(i),"%"
           WRITE(115,'(A,F10.5,X,A,/)')'Mixing Efiiciencty =',
     +                           Tillmixeffi(i),"%"
            
            Tillinceffi(Slnt) = Tillinceffi(Slnt)/100.00
            Tillmixeffi(Slnt) = Tillmixeffi(Slnt)/100.00
            
              DO j = 1,Nsoil
                  CALL KghaToKg(Labp(j),Labp(j),Areap)
                  CALL KghaToKg(Actp(j),Actp(j),Areap)
                  CALL KgHaToKg(Stabip(j),Stabip(j),Areap)
                  CALL KgHaToKg(Frsop(j),Frsop(j),Areap)
                  CALL KgHaToKg(Stabop(j),Stabop(j),Areap)
                  Call KgHaToKg(Crpres(j),Crpres(j),Areap)
                  CALL KgHaToKg(ResdueP0,ResdueP0,AreaP)
                  CALL KgHaToKg(ResdueP1(j),ResdueP1(j),AreaP)
                  CALL KgHaToKg(ResdueP2(j),ResdueP2(j),AreaP)
                  CALL KgHaToKg(HumusP1(j),HumusP1(j),AreaP)
                  CALL KgHaToKg(HumusP2(j),HumusP2(j),AreaP)
                  CALL KgHaToKg(HumusP3(j),HumusP3(j),AreaP)
                  
              END DO
           
              Sr = (1/CP0 - 1/CP(1,1))/(1/CP(1,2)-1/CP(1,1))
              
              IF(Sr>1.0) Sr = 1.0
              IF(Sr<0.0) Sr = 0.0    
              
              !ResdueP1(1)=ResdueP1(1)+ ResdueP0*(1-Sr)*Tillinceffi(Slnt)
              !ResdueP2(1)=ResdueP2(1)+ ResdueP0*Sr*Tillinceffi(Slnt)
              !ResdueP0 = MAX(ResdueP0-ResdueP0*Tillinceffi(Slnt),0.0) 
              
              
              DO j = 1,Slnf
              IF(Avfertp(j)>0.0 .OR. Resfertp(j)>0.0) THEN
                 Labp(1) = Labp(1) + Avfertp(j)*Tillinceffi(Slnt)
                 Labp(1) = Labp(1) + Resfertp(j)*Tillinceffi(Slnt)
                 WRITE(115,'(A,I2,A)')'Fertilizer P Pool of Fertilizer N
     +O' ,j,' is incorporated into the Labile P pool of the First Soil l
     +ayer'        
          WRITE(115,'(/,A,/)')'Soil Labile P  pool is Updated'
          
                DO t=1,Nsoil 
                CALL KgToKgHa(Labp(t),Labp(t),Areap)
                END DO
                
                l=Nsoil  
                WRITE(N,'(I3)') l
                String = '('//TRIM(N)//'(A8,I2,2X))'
                WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
                String = '('//TRIM(N)//'(A10,2X))'
                WRITE(115,String)("Kg/ha",m=1,Nsoil) 
                String = '('//TRIM(N)//'(F10.5,2X))'
                WRITE(115,String) (Labp(m),m=1,Nsoil) 
                
                DO t=1,Nsoil  
                CALL KghaToKg(Labp(t),Labp(t),Areap)    
                END DO
                
                Avfertp(j)=MAX(Avfertp(j)-Avfertp(j)*Tillinceffi(Slnt),
     +                                                     0.0)
                Resfertp(j)=MAX(Resfertp(j)-Resfertp(j)*
     +                               Tillinceffi(Slnt),0.0)
                
                
                
           WRITE(115,'(/,A,F10.5,A6)')'Available Fertilizer P=',
     +                                           Avfertp(j),'Kg'
           WRITE(115,'(A,F10.5,A6)')'Residual Fertilizer P=',Resfertp(j)
     +                                                ,       'Kg'
              END IF     
                 
              END DO
              
              
              
      DO j = 1,Slnm
      IF(Manwip(j)>0.0 .OR. Manwop(j)>0.0 .OR. Mansip(j)>0.0    
     +        .OR. Mansop(j)>0.0) THEN   
          
      Labp(1) = Labp(1) + Manwip(j)*Tillinceffi(Slnt)   !may need to change this to reduce transfer to labile P pool Zhiming
      Labp(1) = Labp(1) + Manwop(j)*Tillinceffi(Slnt)   ! probably they are from Vadas et al. just remove them?
      Actp(1) = Actp(1) + Mansip(j)*Tillinceffi(Slnt)
C      Actp(1) = Actp(1) + Mansop(j)*Tillinceffi(Slnt)   ! do not move Manure Stable organic P to active inorganic P pool by tillage; Zhiming
C      Stabop(1) = Stabop(1) + Mansop(j)*Tillinceffi(Slnt) ! or  move manure Stable OP to soil Stable OP, Zhiming; proven to be not good 
       
      
      
      WRITE(115,'(/,A,I2,A)')'Manure P Pool of Manure No ',j,'is incorpo
     +rated into the Labile and Active P pool of the First Soil layer'
      
      WRITE(115,'(/,A,/)')'Soil Labile P and Active P pools are Updated'
          
      
      DO t=1,Nsoil 
          CALL KgToKgHa(Labp(t),Labp(t),Areap)
          Call KgToKgha(Actp(t),Actp(t),Areap)
      END DO
          l=Nsoil  
          WRITE(N,'(I3)') l
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
      
       DO t= 1, Nsoil
           CALL KgHaToKg(Labp(t),Labp(t),Areap)
           Call KgHaToKg(Actp(t),Actp(t),Areap)
       END DO      
      
      Manwip(j) = MAX(Manwip(j) - Manwip(j)*Tillinceffi(Slnt),0.0)
      Manwop(j) = MAX(Manwop(j) - Manwop(j)*Tillinceffi(Slnt),0.0)
      Mansip(j) = MAX(Mansip(j) - Mansip(j)*Tillinceffi(Slnt),0.0)
C      Mansop(j) = MAX(Mansop(j) - Mansop(j)*Tillinceffi(Slnt),0.0) ! do not move Manure Stable organic P to active inorganic P pool by tillage; Zhiming
C      Mansop(j) = MAX(Mansop(j) - Mansop(j)*Tillinceffi(Slnt),0.0)  ! or move manure Stable OP to soil Stable OP, Zhiming, proven to be not good 
      
                  
      WRITE(115,'(/,A,F10.5,A4)')'MANSIP =',Mansip(Slnm),'Kg'
      WRITE(115,'(A,F10.5,A4)')'MANSOP =',Mansop(Slnm),'Kg'
      WRITE(115,'(A,F10.5,A4)')'MANWIP =',Manwip(Slnm),'Kg'
      WRITE(115,'(A,F10.5,A4)')'MANWOP =',Manwop(Slnm),'Kg' 
      
                  
      END IF    
      END DO
              
            
              DO k = 1, Nsoil
                  IF (Dsoil(k) > Tilld(Slnt)) EXIT
              END DO
              
              Tsoilmass = 0.0
              Tlabp = 0.0
              Tactp = 0.0
              Tstabip=0.0
              Tfrsop=0.0
              Tstabop=0.0
              Tcrpres =0.0
              TresdueP1=0.0
              TresdueP2=0.0
              ThumusP1=0.0
              ThumusP2=0.0
              ThumusP3=0.0
              
              
              DO j= 1, k
                  Soilmass(j) = Areap * Tsoil(j) * Bdsoil(j) * 10000
                  Tsoilmass = Tsoilmass + Soilmass(j)
                  Tlabp = Tlabp + Labp(j)
                  Tactp = Tactp + Actp(j)
                  Tstabip = Tstabip + Stabip(j)
                  Tfrsop = Tfrsop + Frsop(j)
                  Tstabop =  Tstabop +  Stabop(j)
                  Tcrpres =  Tcrpres +  Crpres(j) 
                  !TresdueP1 = TresdueP1 + ResdueP1(j)
                  !TresdueP2 = TresdueP2 + ResdueP2(j)
                  !ThumusP1 =  ThumusP1  + HumusP1(j)
                  !ThumusP2 =  ThumusP2 + HumusP2(j)
                  !ThumusP3 =  ThumusP3 + HumusP3(j)
              END DO
          
              DO j =1, k
                  Ratio = Soilmass(j)/Tsoilmass
                  Labp(j) = (1-Tillmixeffi(Slnt))*Labp(j) + Tlabp*Ratio*
     +                       Tillmixeffi(Slnt)         
                  Actp(j) = (1-Tillmixeffi(Slnt))*Actp(j) + Tactp*Ratio*
     +                       Tillmixeffi(Slnt)  
                  Stabip(j) = (1-Tillmixeffi(Slnt))*Stabip(j) + Tstabip*
     +                         Ratio*Tillmixeffi(Slnt) 
                  Frsop(j) = (1-Tillmixeffi(Slnt))*Frsop(j) + TFrsop*
     +                         Ratio*Tillmixeffi(Slnt) 
                  Stabop(j) = (1-Tillmixeffi(Slnt))*Stabop(j) + Tstabop*
     +                         Ratio*Tillmixeffi(Slnt) 
                  Crpres(j) = (1-Tillmixeffi(Slnt))*Crpres(j) + Tcrpres*
     +                         Ratio*Tillmixeffi(Slnt) 
     !!             ResdueP1(j) = (1-Tillmixeffi(Slnt))*ResdueP1(j)+ 
     !!+                                TresdueP1*Ratio*Tillmixeffi(Slnt) 
     !!             ResdueP2(j) = (1-Tillmixeffi(Slnt))*ResdueP2(j)+ 
     !!+                                TresdueP2*Ratio*Tillmixeffi(Slnt)
     !!             HumusP1(j) = (1-Tillmixeffi(Slnt))*HumusP1(j)+ 
     !!+                                ThumusP1*Ratio*Tillmixeffi(Slnt)
     !!             HumusP2(j) = (1-Tillmixeffi(Slnt))*HumusP2(j)+ 
     !!+                                ThumusP2*Ratio*Tillmixeffi(Slnt)
     !!             HumusP3(j) = (1-Tillmixeffi(Slnt))*HumusP3(j)+ 
     !!+                                ThumusP3*Ratio*Tillmixeffi(Slnt)
              END DO
              
              DO j= 1, Nsoil
                 CALL KgToKgha(Labp(j),Labp(j),Areap)
                 Call KgToKgha(Actp(j),Actp(j),Areap)
                 CALL KgToKgHa(Stabip(j),Stabip(j),Areap)
                 CALL KgToKgHa(Frsop(j),Frsop(j),Areap)
                 CALL KgToKgHa(Stabop(j),Stabop(j),Areap)
                 CALL KgToKgHa(Crpres(j),Crpres(j),Areap)
                 CALL KgToKgHa(ResdueP0,ResdueP0,AreaP)
                 CALL KgToKgHa(ResdueP1(j),ResdueP1(j),AreaP)
                 CALL KgToKgHa(ResdueP2(j),ResdueP2(j),AreaP)
                 CALL KgToKgHa(HumusP1(j),HumusP1(j),AreaP)
                 CALL KgToKgHa(HumusP2(j),HumusP2(j),AreaP)
                 CALL KgToKgHa(HumusP3(j),HumusP3(j),AreaP)
              END DO
              
      WRITE(115,'(/,A)')'Soil P pools and Crop Residue of soil layers up
     +to tillage depth are Mixed'  
      
      WRITE(115,'(/,A,/)')'Soil P pools and Crop Residue are Updated'
          l=Nsoil  
          WRITE(N,'(I3)') l
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
      
          WRITE(115,'(/)')
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("StabIP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Stabip(m),m=1,Nsoil) 
      
          
          WRITE(115,'(/)')
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("StabOP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Stabop(m),m=1,Nsoil) 
      
          WRITE(115,'(/)')
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("FrsOP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Frsop(m),m=1,Nsoil) 
          
          
          WRITE(115,'(/)')
          String = '('//TRIM(N)//'(A8,I2,2X))'
          WRITE(115,String) ("Crpres",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A10,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F10.5,2X))'
          WRITE(115,String) (Crpres(m),m=1,Nsoil) 
          
              
          END IF
      END DO
              
      END SUBROUTINE TILLAGE