      SUBROUTINE PFLUX(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine CALCULATES P Flux between the P pools
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  17/08/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter                                                        
C      A                  REAL               -                 A Variable
C      B                  REAL               -                 A Variable
C      Base               REAL               -                 A Variable
C      Pdesrpfac          REAL               -                 P desorption factor
C      Psrpfac            REAL               -                 P sorption factor
C      Tfac               REAL               -                 Soil Temperature Factor
C      Smfac              REAL               -                 Soil Moisture Factor
C      Cnratio            REAL               -                 C N Ratio
C      Cpratio            REAL               -                 C P Ratio
C      Nutridfac          REAL               -                 Nutrient Cycling Residue Decompostion Factor
C      Cpratiof           REAL               -                 C P Ratio Factor
C      Cnratiof           REAL               -                 C N Ratio Factor
C      Resdecomr          REAL               Day-1             Residue Decomposition rate 
C      Resdcom            REAL               Kg/ha             Residue Decomposition
C      Frsopminr          REAL               Day-1             Fresh organic P Mineralization Rate
C      Frsopmin           REAL               Kg/ha             Fresh organic P Mineralization
C      K                  REAL               Day-1             Rate Constant of Residue Decompostion/Mineralization
C      Micrbpconc         REAL               -                 Microbial P Concentration
C      Labpimmobr         REAL               Day-1             Labile P Immobilization rate
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C 
C
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================   
       USE VARIABLE
       IMPLICIT NONE
       INTEGER :: i,j
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       
       DOUBLE PRECISION,DIMENSION(:)::A(Nsoil),B(Nsoil),Base(Nsoil),
     +  Pdesrpfac(Nsoil)
     + ,Psrpfac(Nsoil),Tfac(Nsoil),Smfac(Nsoil),Cnratio(Nsoil),   
     + Cpratio(Nsoil),Nutridfac(Nsoil),Cpratiof(Nsoil),Cnratiof(Nsoil), 
     + Resdecomr(Nsoil),Resdcom(Nsoil),Frsopminr(Nsoil),Frsopmin(Nsoil),
     + K(Nsoil),Micrbpconc(Nsoil),Labpimmobr(Nsoil)
       
       DAY = ID
       Month = IM
       Year = IYYY
       
       WRITE(115,'(/,A)')'P MOVEMENT WITHIN THE SOIL LAYER & RESIDUE DEC
     +OMPOSITION'
       
       CALL BREAKNODE
       
       DO i=1, Nnode
          Pflowr1frsopnode(i) = MAX(ResdueP1node(i)*Pdrn(i,1),0.0)
          Pflowr2frsopnode(i) = MAX(ResdueP2node(i)*Pdrn(i,2),0.0)
          Pflowh1stbopnode(i) = MAX(HumusP1node(i)*Pdrn(i,3),0.0)
          Pflowh2stbopnode(i) = MAX(HumusP2node(i)*Pdrn(i,4),0.0)
          Pflowh3stbopnode(i) = MAX(HumusP3node(i)*Pdrn(i,5),0.0)   
       END DO
       
       
       CALL ADDNODE
       
       DO I=1,Nsoil
          Pflowr1frsop(i)=0.0
          Pflowr2frsop(i)=0.0
          Pflowh1stbop(i)=0.0
          Pflowh2stbop(i)=0.0
          Pflowh3stbop(i)=0.0   
       END DO
       
       DO i=1,Nnode
       DO j=1,Nsoil  
          IF((Dnode(i)-Dsoil(j))<0.001) THEN      
           Pflowr1frsop(j)=Pflowr1frsop(j)+Pflowr1frsopnode(i)
           Pflowr2frsop(j)=Pflowr2frsop(j)+Pflowr2frsopnode(i)
           Pflowh1stbop(j)=Pflowh1stbop(j)+Pflowh1stbopnode(i)
           Pflowh2stbop(j)=Pflowh2stbop(j)+Pflowh2stbopnode(i)
           Pflowh3stbop(j)=Pflowh3stbop(j)+Pflowh3stbopnode(i)      
          EXIT
          END IF
       END DO
       END DO
       
        Pflowresfrso1 = ResdueP0*0.0015                                       ! P FLOW FROM Surface RESIDUE POOL TO FRESH ORGANIC P POOL of 1st Layer
!        Frsop(1) = Frsop(1) + Pflowresfrso1
        
!      IF(Pflowresfrso1>0.0) THEN  
!      WRITE(115,'(/,A,/)') 'P Flow From Surface Residue P Pool To Fre
!     +shorganic P Pool of the 1st Soil Layer'   
!      WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowresfrso1*1000,' gm/ha'  
!      WRITE(115,'(A)') 'Fresh organic Pool of the 1st Soil Layer is Up
!     +dated' 
!      WRITE(115,'(A,F12.5,A)')'Fresh organic Pool of the 1st Soil Layer
!     + =', Frsop(1),' Kg/ha'
!      ENDIF
      
      
       DO i=1,Nsoil  
           
           
        WRITE(115,'(/,A,I3,/)')'Layer No :',i
        
        WRITE(115,'(5(A12,2X))')'LabileP','ActiveP','StabOP',
     +    'StabIP','FrshOP'
        
        WRITE(115,'(5(A12,2X))')' Kg/ha',' Kg/ha',' Kg/ha',
     +       ' Kg/ha',' Kg/ha'
        WRITE(115,'(10(F12.5,2X),/)')Labp(i),Actp(i),Stabop(i),
     + Stabip(i),Frsop(i)
                    
!         Frsop(i) = Frsop(i) + Pflowr1frsop(i)                                 ! P FLOW FROM RESIDUE POOL 1 TO FRESH ORGANIC P POOL
         
!      IF(Pflowr1frsop(i)>0.0) THEN
!      WRITE(115,'(/,A,/)') 'P Flow From  Residue P Pool1 To Fre
!     +shorganic P Pool'   
!      WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowr1frsop(i)*1000,' gm/ha'  
!      WRITE(115,'(A)') 'Fresh organic Pool is Updated' 
!      WRITE(115,'(A,F12.5,A)')'FrsOP =', Frsop(i),' Kg/ha'
!      ENDIF
         
!         Frsop(i) = Frsop(i) + Pflowr2frsop(i)                                 ! P FLOW FROM RESIDUE POOL 2 TO FRESH ORGANIC P POOL 
         
!      IF(Pflowr2frsop(i)>0.0) THEN
!      WRITE(115,'(/,A,/)') 'P Flow From  Residue P Pool2 To Fre
!     +shorganic P Pool'   
!      WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowr2frsop(i)*1000,' gm/ha'  
!      WRITE(115,'(A)') 'Fresh organic Pool is Updated' 
!      WRITE(115,'(A,F12.5,A)')'FrsOP =', Frsop(i),' Kg/ha'
!      ENDIF
         
!         Stabop(i) = Stabop(i) + Pflowh1stbop(i)                               ! P flow from Humus 1 Pool to Stable Organic Pool
         
           
!      IF(Pflowh1stbop(i)>0.0) THEN
!      WRITE(115,'(/,A,/)') 'P Flow From  Humus Pool1 To Stab
!     +le Organic P Pool'   
!      WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowh1stbop(i)*1000,' gm/ha'  
!      WRITE(115,'(A)') 'StabOP Pool is Updated' 
!      WRITE(115,'(A,F12.5,A)')'StabOP =', Stabop(i),' Kg/ha'
!      ENDIF 
         
!         Stabop(i) = Stabop(i) + Pflowh2stbop(i)                               ! P flow from Humus 2 Pool to Stable Organic Pool
         
!       IF(Pflowh2stbop(i)>0.0) THEN
!      WRITE(115,'(/,A,/)') 'P Flow From  Humus Pool2 To Stab
!     +le Organic P Pool'   
!      WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowh2stbop(i)*1000,' gm/ha'  
!      WRITE(115,'(A)') 'StabOP Pool is Updated' 
!      WRITE(115,'(A,F12.5,A)')'StabOP =', Stabop(i),' Kg/ha'
!      ENDIF   
         
!         Stabop(i) = Stabop(i) + Pflowh3stbop(i)                               ! P flow from Humus 3 Pool to Stable Organic Pool
        
!       IF(Pflowh3stbop(i)>0.0) THEN
!      WRITE(115,'(/,A,/)') 'P Flow From  Humus Pool3 To Stab
!     +le Organic P Pool'   
!      WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowh3stbop(i)*1000,' gm/ha'  
!      WRITE(115,'(A)') 'StabOP Pool is Updated' 
!      WRITE(115,'(A,F12.5,A,/)')'StabOP =', Stabop(i),' Kg/ha'
!      ENDIF      
         
         
         
        Labp(i) = Labp(i)/(Tsoil(i)*Bdsoil(i)*0.01)             ! Converts Labp Kg/ha to mg/Kg
        Psc(i)=-0.045*LOG(Clayp(i))+0.001*Labp(i)-0.035*Orgc(i)+0.43 
        Labp(i) = Labp(i)*(Tsoil(i)*Bdsoil(i)*0.01)             ! Converts Labp mg/Kg to Kg/Ha
        IF(Psc(i)<0.1) Psc(i) = 0.1
        IF(Psc(i)>0.75) Psc(i) = 0.75    
        A(i) = 0.918*EXP(-4.603*Psc(i))
        B(i) =-0.238*LOG(A(i))-1.126
        Base(i)=-1.08*Psc(i) + 0.79
        Tfac(i) =EXP(0.115*Soiltemp(i)-2.88)
        IF(Tfac(i)<0.0) Tfac(i)=0.0
        Smfac(i) = Soilwater(i)/Fc(i)
        
        WRITE(115,'(A,F10.5)')'PSC = ',Psc(i)
        WRITE(115,'(A,F10.5)')'Factor A =',A(i)
        WRITE(115,'(A,F10.5)')'Factor B =',B(i)
        WRITE(115,'(A,F10.5)')'Base =',Base(i)
        WRITE(115,'(A,F10.5)')'Soil Moisture Factor=',Smfac(i)
       
        
        Pflowactlab(i) = 0.0
        Pflowlabact(i) = 0.0
        Pdesrpfac(i) = 0.0
        Psrpfac(i) = 0.0
        
        IF(Labp(i) < Actp(i)*Psc(i)/(1-Psc(i))) THEN                           ! Flow From Active P Pool To Labile P Pool
         Daydesrp(i) = Daydesrp(i) + 1
         Daysrp(i) = 0
         Pdesrpfac(i)= ABS(Base(i)*Daydesrp(i)**-0.29)
         Pflowactlab(i)=Pdesrpfac(i)*(Actp(i)*Psc(i)/(1-Psc(i))-Labp(i))
         IF(Pflowactlab(i)>Actp(i)) Pflowactlab(i)=Actp(i)
         Actp(i) = Actp(i) - Pflowactlab(i)
         IF(Actp(i)<0.0) Actp(i) = 0.0
         Labp(i) = Labp(i) + Pflowactlab(i)
         
         WRITE(115,'(/,A,/)') 'P Flow From Active P Pool To Labile P Poo
     +ol'   
         WRITE(115,'(A,F12.5)')'P Desroption Factor =',Pdesrpfac(i)
         WRITE(115,'(A,I12)')'Number Days of Desorption =',Daydesrp(i)
         WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowactlab(i)*1000,' gm/ha'
         IF(Pflowactlab(i)>0.0) THEN
         WRITE(115,'(A)') 'Active P Pool and Labile P Pool are Updated' 
         WRITE(115,'(A,F12.5,A)')'Labile P Pool =',Labp(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)')'Active P Pool =',Actp(i),' Kg/ha'
         ENDIF
        ELSEIF (Labp(i) > Actp(i)*Psc(i)/(1-Psc(i))) THEN                         ! Flow From Labile P Pool To Active P Pool
         Daydesrp(i) = 0
         Daysrp(i) = Daysrp(i) + 1
         Psrpfac(i) = ABS(A(i)*Daysrp(i)**B(i))
         Pflowlabact(i) = Psrpfac(i)*(Labp(i)-Actp(i)*Psc(i)/(1-Psc(i)))
         IF(Pflowlabact(i)>Labp(i)) Pflowlabact(i)=Labp(i)
         Labp(i) = Labp(i)-Pflowlabact(i)
         IF(Labp(i)<0.0) Labp(i) = 0.0
         Actp(i) = Actp(i)+Pflowlabact(i)  
         
         WRITE(115,'(/,A,/)') 'P Flow From Labile P Pool To Active P Poo
     +l'   
         WRITE(115,'(A,F12.5)')'P Sroption Factor =',Psrpfac(i)
         WRITE(115,'(A,I10)')'Number Days of Sorption =',Daysrp(i)
         WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowlabact(i)*1000,' gm/ha'
         IF(Pflowlabact(i)>0.0) THEN
         WRITE(115,'(A)') 'Active P Pool and Labile P Pool are Updated' 
         WRITE(115,'(A,F12.5,A)')'Labile P Pool =',Labp(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)')'Active P Pool =',Actp(i),' Kg/ha'
         END IF
        ELSE
          Daydesrp(i) = 0
          Daysrp(i) = 0     
        END IF
        
        Pflowactstbi(i)= 0.0
        Pflowstbiact(i)= 0.0
        
        IF(RatioSIPtoAIP(i)*Actp(i)>Stabip(i)) THEN                                         ! Flow From Active P Pool to Stable Inorganic P Pool
          Pflowactstbi(i)= 0.0006*(RatioSIPtoAIP(i)*Actp(i)-Stabip(i))
          IF(Pflowactstbi(i)>Actp(i)) Pflowactstbi(i)= Actp(i)
          IF(Pflowactstbi(i)<0.0) Pflowactstbi(i)=0.0
          Actp(i)=Actp(i)-Pflowactstbi(i)
          IF(Actp(i)<=0.0) Actp(i) = 0.0
          Stabip(i) = Stabip(i) + Pflowactstbi(i)
          
         WRITE(115,'(/,A,/)') 'P Flow From Active P Pool to StabIP Pool'
       WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowactstbi(i)*1000,' gm/ha'
         IF(Pflowactstbi(i)>0.0) THEN
         WRITE(115,'(A)') 'Active P Pool and StabIP Pool are Updated'   
         WRITE(115,'(A,F12.5,A)')'Active P Pool =',Actp(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)')'StabIP Pool =',Stabip(i),' Kg/ha'
         ENDIF 
        ELSE                                                                   ! Flow From Stable Inorganic P Pool To Active P Pool  
          Pflowstbiact(i) = 0.1*0.0006*(Stabip(i)-RatioSIPtoAIP(i)
     +                        *Actp(i))
          IF(Pflowstbiact(i)>Stabip(i)) Pflowstbiact(i)=Stabip(i)
          IF(Pflowstbiact(i)<0.0) Pflowstbiact(i)=0.0
          Stabip(i) = Stabip(i) - Pflowstbiact(i)
          IF(Stabip(i)<0.0) Stabip(i) = 0.0
          Actp(i) = Actp(i) + Pflowstbiact(i)
          
         WRITE(115,'(/,A,/)') 'P Flow From StabIP Pool To Active P Pool'
       WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowstbiact(i)*1000,' gm/ha'
         IF(Pflowstbiact(i)>0.0) THEN
         WRITE(115,'(A)') 'Active P Pool and StabIP Pool are Updated'     
         WRITE(115,'(A,F12.5,A)')'Active P Pool =',Actp(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)')'StabIP Pool =',Stabip(i),' Kg/ha'
         ENDIF
        END IF
        
     !!    WRITE(115,'(/,A,/)') 'P Flow From Decomposed Residue To FresOP 
     !!+Pool'    
     !!    WRITE(115,'(A,F12.5,A)')'Crop Residue  =',Crpres(i),' Kg/ha'
         
         
         
 !        Pflowr1h2(i)= 0.0
 !        Pflowr1h1(i) = 0.0
         
         !Pflowr1frsop(i) = 0.0
         !Pflowr1frsop(i) = MAX(ResdueP1(i)*Pdcomr(1),0.0)
         
         !IF(Pdcomr(1)*ResdueP1(i)<ResdueP1(i)) THEN
         ! Pflowr1h2(i) = Coefi14*Pdcomr(1)*ResdueP1(i)
         ! Pflowr1frsop(i) = Pdcomr(1)*ResdueP1(i)*(1-Coefi14) 
         !ELSE
         !  Pflowr1h2(i) = Coefi14*ResdueP1(i)
         !  Pflowr1frsop(i) = ResdueP1(i)*(1-Coefi14)
         !ENDIF
       
         
     !!    ResdueP1(i) = MaX(ResdueP1(i)-Pflowr1h2(i)-
     !!+                                           Pflowr1frsop(i),0.0)  
     !!    HumusP2(i) =  HumusP2(i) + Pflowr1h2(i)                               ! P FLOW FROM RESIDUE POOL 1 TO HUMUS POOL 2
         
     !!    
     !!    Pflowr2h1(i)= 0.0
         !Pflowr2frsop(i) = 0.0
         !Pflowr2frsop(i) = MAX(Pdcomr(2)*ResdueP2(i),0.0)
         
         !IF(Pdcomr(2)*ResdueP2(i)<ResdueP2(i)) THEN
         !   Pflowr2h1(i) = Coefi23*Pdcomr(2)*ResdueP2(i)
         !   Pflowr2frsop(i)= Pdcomr(2)*ResdueP2(i)*(1-Coefi23)
         !   
         !ELSE
         !   Pflowr2h1(i) = Coefi23*ResdueP2(i)
         !   Pflowr2frsop(i)= ResdueP2(i)*(1-Coefi23)
         !END IF
         !
         !ResdueP2(i)= Max(ResdueP2(i)-Pflowr2frsop(i)- Pflowr2h1(i),0.0)
         !HumusP1(i) = HumusP1(i) + Pflowr2h1(i)                               ! P FLOW FROM RESIDUE POOL 2 TO HUMUS POOL 1  
        
         
   !      Pflowh1h2=0.0
         !Pflowh1stbop = 0.0
         !Pflowh1stbop = MAX(Pdcomr(3)*HumusP1(i),0.0)
         
         !IF(Pdcomr(3)*HumusP1(i)< HumusP1(i)) THEN
         !    Pflowh1h2 (i)= Coefi34*Pdcomr(3)*HumusP1(i)
         !    Pflowh1stbop(i) = Pdcomr(3)*HumusP1(i)*(1-Coefi34)   
         !ELSE
         !    Pflowh1h2 (i)= Coefi34*HumusP1(i)
         !    Pflowh1stbop(i) = HumusP1(i)*(1-Coefi34)
         !END IF
         !
         !HumusP1(i) = MAX(HumusP1(i)-Pflowh1stbop(i)-Pflowh1h2(i),0.0)
         !HumusP2(i) = HumusP2(i) + Pflowh1h2(i)                                ! P Flow from Humus pool 1 to Humus Pool 2
        
         
         !Pflowh2h3=0.0
         !Pflowh2h1=0.0
         !Pflowh2stbop = 0.0
         !Pflowh2stbop = MAX(Pdcomr(4)*HumusP2(i),0.0)
         
     !!    IF(Pdcomr(4)*HumusP2(i)< HumusP2(i)) THEN
     !!        Pflowh2h3(i)= Coefi45*Pdcomr(4)*HumusP2(i)
     !!        Pflowh2h1(i)= Coefi43*Pdcomr(4)*HumusP2(i)
     !!        Pflowh2stbop(i) = Pdcomr(4)*HumusP2(i)*(1-Coefi45-Coefi43) 
     !!    ELSE
     !!        Pflowh2h3(i)= Coefi45*HumusP2(i)
     !!        Pflowh2h1(i)= Coefi43*HumusP2(i)
     !!        Pflowh2stbop(i) = HumusP2(i)*(1-Coefi45-Coefi43) 
     !!    END IF
     !!    
     !!    HumusP2(i) = MAX(HumusP2(i)-Pflowh2stbop(i)-Pflowh2h3(i)-
     !!+                                          Pflowh2h1(i),0.0)
     !!    HumusP1(i) = HumusP1(i) + Pflowh2h1(i)                                ! P Flow from Humus pool 2 to Humus Pool 1
     !!    HumusP3(i) = HumusP3(i) + Pflowh2h3(i)                                ! P Flow from Humus pool 2 to Humus Pool 3
        
         
      !   Pflowh3h1(i)=0.0
         !Pflowh3stbop(i) = 0.0 
         !Pflowh3stbop(i) = MAX(Pdcomr(5)*HumusP3(i),0.0)
         !IF (Pdcomr(5)*HumusP3(i)<HumusP3(i)) THEN
         !  Pflowh3h1(i)= Coefi53*Pdcomr(5)*HumusP3(i)
         !  Pflowh3stbop(i) = (1-Coefi53)*Pdcomr(5)*HumusP3(i)
         !ELSE
         !  Pflowh3h1(i)= Coefi53*HumusP3(i)
         !  Pflowh3stbop(i) = (1-Coefi53)*HumusP3(i) 
         !END IF
         !
         !HumusP3(i)= MAX(HumusP3(i)-Pflowh3h1(i)- Pflowh3stbop(i),0.0)
         !HumusP1(i) = HumusP1(i) + Pflowh3h1(i)                                 ! P Flow from Humus pool 3 to Humus Pool 1
         
         
        IF(ResdueP0>0) THEN
           CP0 = ResdueC0/ResdueP0
        ELSE 
           CP0 = 500.0D0   !USE DEFAULT CP RATIO IN RESIDUE
        END IF
        
        IF(ResdueP1(i)>0) THEN
           CP(i,1) = ResdueC1(i)/ResdueP1(i)
        ELSE
            CP(i,1) = 500.0D0   !USE DEFAULT CP RATIO IN RESIDUE
        END IF
        
        IF(ResdueP2(i)>0) THEN
          CP(i,2) = ResdueC2(i)/ResdueP2(i)
        ELSE
          CP(i,2) = 0.0
        END IF
        
        IF(HumusP1(i)>0) THEN
          CP(i,3) = HumusC1(i)/HumusP1(i)
        ELSE
          CP(i,3) = 0.0
        END IF
        
        IF(HumusP2(i)>0) THEN
          CP(i,4) = HumusC2(i)/HumusP2(i)
        ELSE
          CP(i,4) = 0.0
        END IF
           
        IF(HumusP3(i)>0) THEN
          CP(i,5) = HumusC3(i)/HumusP3(i) 
        ELSE
          CP(i,5) = 0.0
        END IF   
           
                                                ! Computes CP ratio for each residue and humus p pool
     
        
         
        IF(Soiltemp(i) > 0.0 ) THEN
         Tfac(i) = MIN((0.1 + (0.9*Soiltemp(i))/
     +    (Soiltemp(i)+EXP(7.63-0.312*
     +      Soiltemp(i)))),1.0)
        ELSE
          Tfac(i) = 0.0
        END IF
        
        
        !WRITE(115,'(A,F12.5)')'Temp Factor  =',Tfac(i)
        
        IF((Labp(i)+Frsop(i))>0.00001) THEN
         Cpratio(i) = 0.4*Crpres(i)/(Labp(i)+Frsop(i))
         IF(Cpratio(i)>5000.00)Cpratio(i)=5000.00
         Cpratiof(i)= EXP(-0.693*(Cpratio(i)-200.0)/200.0)
        ELSE
           Cpratio(i) = 1.0
           Cpratiof(i) =1.0  
        END IF
        
        !WRITE(115,'(A,F12.5)')'CP Ratio Factor  =',Cpratiof(i)
        
        Soilnfresh(i) = Crpres(i)*0.0055
        
        !WRITE(115,'(A,F12.5)')'NFresh=',Soilnfresh(i,Day,Month,Year)
        
        IF((Soilnfresh(i)+ 
     +      Soilno3(i))>0.00001) THEN
         Cnratio(i) = 0.4*Crpres(i)/(Soilnfresh(i) + 
     +                               Soilno3(i))
         IF(Cnratio(i)>500.00) Cnratio(i) = 500.00
         Cnratiof(i) = EXP(-0.693*(Cnratio(i)-25.0)/25.0)
        ELSE
          Cnratio(i) =1.0
          Cnratiof(i)=1.0
        END IF
        
        !WRITE(115,'(A,F12.5)')'CN Ratio Factor  =',Cnratiof(i)
        
        Nutridfac(i) = MIN(Cpratiof(i),Cnratiof(i),1.0)
        
        !WRITE(115,'(A,F12.5)')'Nutri Factor  =',Nutridfac(i)
        
        IF(IniCrpres(i)>0.0) THEN
        
        IF((Crpres(i)/IniCrpres(i))>=0.8) THEN
           K(i) = 0.8
        ELSE
          IF((Crpres(i)/IniCrpres(i))>=0.1) THEN
           K(i) = 0.05
          ELSE
           K(i) = 0.0095
          ENDIF
        ENDIF
        ELSE
            K(i) = 0.0
        END IF
        
        !WRITE(115,'(A,F12.4)')'K Factor  =',K(i)    
        
        Resdecomr(i) = K(i)*Nutridfac(i)*SQRT(Tfac(i)*Smfac(i))                       ! Crop Residue Decomposition
        Resdcom(i) = Resdecomr(i)*Crpres(i)
        
     !!   WRITE(115,'(A,F12.5)')'Residue Decomposition rate=',Resdecomr(i)
     !!   WRITE(115,'(A,F12.5,A)')'Crop Residue Decompostion =',
     !!+                                        Resdcom(i)*1000,' gm/ha'
        
        IF(Resdcom(i)>Crpres(i)) Resdcom(i) = Crpres(i)
        Crpres(i) = Crpres(i) - Resdcom(i)
        IF(Crpres(i)<0.0) Crpres(i)=0.0
        
        !Pflowresfrso(i) = Resdcom(i)*0.0003
        !Frsop(i) = Frsop(i) + Pflowresfrso(i)                                         ! Decomposed crop residue is added to Fresh organic P Pool
        
        
        
        IF(Resdcom(i)>0.0) THEN       
         WRITE(115,'(A)')'Crop Residue Pool Updated'
         WRITE(115,'(A,F12.5,A)')'Crop Residue  =',Crpres(i),' Kg/ha'
         WRITE(115,'(A)')'Decomposed Crop Residue added to FrsOP Pool'
         WRITE(115,'(A,F12.5,A)'),'FreOP = ',Frsop(i),' Kg/ha'
        END IF
        !
        
        
        Pflowfrsolab(i) = 0.0
        Pflowfrsostbo(i) = 0.0
        
        Frsopminr(i)=K(i)*Nutridfac(i)*SQRT(Tfac(i)*Smfac(i))
        Frsopmin(i) = Frsopminr(i)*Frsop(i)*3.0                                         ! Mineralization of Fresh Organic P Pool
        ! was Frsopminr(i)*Frsop(i)* 0.6; Zhiming removed 0.6; and times 3.0 instead as frsopmin was too slow.  
        IF(Frsopmin(i)>Frsop(i)) Frsopmin(i)=Frsop(i)
        Frsop(i)=Frsop(i) - Frsopmin(i)
        IF(Frsop(i)<0.0) Frsop(i) = 0.0
        Pflowfrsolab(i) = Frsopmin(i)*0.7
        Pflowfrsostbo(i)  = Frsopmin(i)*0.3
        Labp(i) = Labp(i) + Pflowfrsolab(i)
        Stabop(i) = Stabop(i) + Pflowfrsostbo(i)
        
        WRITE(115,'(/,A,/)')'P Flow from FrsOP Pool to Labile P & StabOP
     +Pool (Mineralization)'
        WRITE(115,'(A,F12.5)')'FrsOP Mineralization rate=',Frsopminr(i)
        WRITE(115,'(A,F12.5,A)')'P Flow =',Frsopmin(i)*1000,' gm/ha'
        IF(Frsopmin(i)>0.0) THEN
         WRITE(115,'(/,A,/)')'Minealized P added to the Labile P & StabO
     +P Pool'    
         WRITE(115,'(/,A,/)')'Labile P,StabOP, FrsOP Pool Updated'
         WRITE(115,'(A,F12.5,A)'),'Labile P = ',Labp(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)'),'StabOP = ',Stabop(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)'),'FreOP = ',Frsop(i),' Kg/ha'         
        END IF
        
        
        IF(Labp(i)>=10.00) THEN
           Micrbpconc(i) =0.02
        ELSE
           Micrbpconc(i) = 0.01 + 0.001*Labp(i)
        END IF
        
        Pflowlabfrso(i) = 0.0
        
        Labpimmobr(i) = 0.16*K(i)*Nutridfac(i)*SQRT(Tfac(i)*Smfac(i)) 
        Pflowlabfrso(i) = Labpimmobr(i) * Micrbpconc(i)* Crpres(i)                         ! Immobilization of Labile P Pool
        IF(Pflowlabfrso(i)>Labp(i)) Pflowlabfrso(i)=Labp(i)
        Labp(i) = Labp(i) - Pflowlabfrso(i)
        IF(Labp(i)<0.0) Labp(i) = 0.0
        Frsop(i) =  Frsop(i) + Pflowlabfrso(i)
        
        WRITE(115,'(/,A,/)')'P Flow from Labile P to FrsOP Pool(Immobili
     +zation)'
        WRITE(115,'(A,F12.5)'),'Microbial Concetration = ',Micrbpconc(i)
        WRITE(115,'(A,F12.5,A)')'Immobilization Rate =',Labpimmobr(i)
        WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowlabfrso(i)*1000,' gm/ha'
        IF(Pflowlabfrso(i)>0.0) THEN
          WRITE(115,'(/,A,/)')'Immobilized P added to the FrsOP Pool'
          WRITE(115,'(A)')'Labile P & FrsOP Pool Updated'
          WRITE(115,'(A,F12.5,A)'),'Labile P = ',Labp(i),' Kg/ha'
          WRITE(115,'(A,F12.5,A)'),'FreOP = ',Frsop(i),' Kg/ha'
        ENDIF
        
        Pflowstbolab(i) = 0.0
        
        Pflowstbolab(i) = 0.003*MIN(Tfac(i),Smfac(i))*Stabop(i)                           ! Flow From Stable Organic P Pool To Labile P Pool  
        IF(Pflowstbolab(i)>Stabop(i)) Pflowstbolab(i)=Stabop(i)
        Stabop(i) = Stabop(i) - Pflowstbolab(i)
        IF(Stabop(i)<0.0) Stabop(i)= 0.0
        Labp(i) = Labp(i) + Pflowstbolab(i)
        
        WRITE(115,'(/,A)')'P Flow from StabOP To Labile P Pool'
        WRITE(115,'(A,F12.5,A)')'P Flow =',Pflowstbolab(i)*1000,' gm/ha'
        IF(Pflowstbolab(i)>0.0) THEN
         WRITE(115,'(A)') 'StabOP To Labile P Pool are Updated'    
         WRITE(115,'(A,F12.5,A)'),'Labile P = ',Labp(i),' Kg/ha'
         WRITE(115,'(A,F12.5,A)'),'StabOP = ',Stabop(i),' Kg/ha'    
        END IF
        
         WRITE(115,'(/,10(A12,2X))')'LabileP','ActiveP','StabOP',
     +    'StabIP','FrshOP',"ResidueP1","ResidueP2","HumusP1","HumusP2",
     +     "HumusP3"  
        
        WRITE(115,'(10(A12,2X))')' Kg/ha',' Kg/ha',' Kg/ha',
     +       ' Kg/ha',' Kg/ha',' Kg/ha',' Kg/ha',' Kg/ha', ' Kg/ha',
     +   ' Kg/ha'
        WRITE(115,'(10(F12.5,2X),/)')Labp(i),Actp(i),Stabop(i),
     + Stabip(i),Frsop(i),ResdueP1(i),ResdueP2(i),HumusP1(i),HumusP2(i),
     +   HumusP3(i)
        END DO
      
      END SUBROUTINE PFLUX