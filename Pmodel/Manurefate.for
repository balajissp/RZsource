      SUBROUTINE MANUREFATE(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine simulates the fate of Manure P due to rainfall and 
C     infiltration soil and loss in runoff after its application
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  02/08/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter 
C      Tfact              REAL               -                 Temperature Factor
C      W                  REAL               cm3/gm            Water to Manure Ratio
C      Pdfactor           REAL               -                 P Distribution Factor Between Runoff and Infiltration
C FUNCTIONS/SUBROUTINE CALLED ::
C
C      NAME                    DESCRIPTION
C    ----------              ------------------           
C 1. KgHaToKg             This Subroutine Converts Variables from Kg/ha to Kg
C 2. KgToKgHa             This Subroutine Converts Variables from Kg to Kg/ha  
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================   
       USE VARIABLE
       IMPLICIT NONE
       INTEGER :: i,j,m
       DOUBLE PRECISION :: Tfact,W,Pdfactor
       CHARACTER(LEN=100)::String,N
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       
       DO i = 1, Slnm
          
           Tfact = (2048*Tempt**2-
     +     Tempt**4)/32**4
           IF( Tfact <0.0) Tfact = 0.0
           IF (Tfact >1.0) Tfact = 1.0
           
           IF(Rain> 0.4) THEN
             Manmoist(i)= Manmoist(i) + (0.27-0.3*Manmoist(i))
           ELSEIF(Rain> 0.1) THEN
             Manmoist(i) =  Manmoist(i)
           ELSE
             Manmoist(i) =  Manmoist(i)-(0.075-0.05*Manmass(i)/
     +                      Appdaymanmass(i))*Tfact
           ENDIF
           
           IF (Manmoist(i)>0.9) Manmoist(i) = 0.9
           IF (Manmoist(i)<0.0) Manmoist(i) = 0.0
       
       Mandcomr(i) = 0.003*(Tfact**0.5)      
       Manasimr(i) = 30*EXP(2.5*Manmoist(i))
       
       Mandcom(i) = Manmass(i) * Mandcomr(i)
       IF(Mandcom(i)<0.0) Mandcom(i) =0.0
       IF(Mandcom(i)>Manmass(i)) Mandcom(i) = Manmass(i)   
       
       IF(Manmass(i)>0.0) THEN
         Mancovadcom(i) = (Mandcom(i) / Manmass(i))*Mancova(i)
         IF(Mancovadcom (i)< 0.0) Mancovadcom (i) = 0.0
         IF(Mancovadcom (i) > Mancova(i)) Mancovadcom (i) = Mancova(i)
       ELSE
         Mancovadcom (i) = 0.0
      ENDIF
       
       Mansipdcom(i) = 0.0025*MIN(Tfact,Manmoist(i))* Mansip(i)
       IF(Mansipdcom(i)<0.0) Mansipdcom(i) = 0.0
       IF(Mansipdcom(i)>Mansip(i)) Mansipdcom(i) = Mansip(i)
       
       Mansopdcom(i) = 0.01*MIN(Tfact,Manmoist(i))*Mansop(i)
       IF( Mansopdcom(i) < 0.0)  Mansopdcom(i) = 0.0
       IF( Mansopdcom(i) > Mansop(i)) Mansopdcom(i) = Mansop(i)
       
       Manwopdcom(i) = 0.1*MIN(Tfact,Manmoist(i))*Manwop(i)
       IF(Manwopdcom(i) <0.0) Manwopdcom(i) = 0.0
       IF(Manwopdcom(i)>Manwop(i)) Manwopdcom(i)= Manwop(i)
       
       IF( Manmass(i)> 0.0 ) THEN
       
       Manasim(i) = Manasimr(i) * Tfact * Mancova (i) 
       IF(Manasim(i) < 0.0) Manasim(i) = 0.0
       IF(Manasim(i) > Manmass(i)) Manasim(i) = Manmass(i)   
       
       Mancovaasim(i) = (Manasim(i)/Manmass(i))* Mancova(i)
       IF(Mancovaasim(i)< 0.0 ) Mancovaasim(i) = 0.0
       IF(Mancovaasim(i)> Mancova(i) ) Mancovaasim(i) = Mancova(i)
       
       Manwipasim(i) = (Manasim(i)/Manmass(i))* Manwip(i)
       IF(Manwipasim(i)<0.0) Manwipasim(i) = 0.0
       IF(Manwipasim(i)>Manwip(i)) Manwipasim(i) = Manwip(i)
       
       Manwopasim(i) = (Manasim(i)/Manmass(i))* Manwop(i)
       IF(Manwopasim(i)<0.0) Manwopasim(i) = 0.0
       IF(Manwopasim(i)>Manwop(i)) Manwopasim(i) = Manwop(i)
       
       Mansipasim(i)= (Manasim(i)/Manmass(i))*Mansip(i)
       IF(Mansipasim(i)<0.0) Mansipasim(i) = 0.0
       IF(Mansipasim(i)>Mansip(i)) Mansipasim(i) = Mansip(i)
       
       Mansopasim(i) = (Manasim(i)/Manmass(i))*Mansop(i)
       IF( Mansopasim(i)<0.0)  Mansopasim(i) = 0.0
       IF( Mansopasim(i)>Mansop(i)) Mansopasim(i)=Mansop(i)
       
       ELSE
            Manasim(i) = 0.0
            Mancovaasim(i) = 0.0
            Manwipasim(i) = 0.0
            Manwopasim(i) = 0.0
            Mansipasim(i)  = 0.0
            Mansopasim(i)  = 0.0
       END IF
       
       
       Mansip(i) = MAX(0.0,Mansip(i)-Mansipdcom(i)-Mansipasim(i))
       Mansop(i) = MAX(0.0,Mansop(i)-Mansopdcom(i)-Mansopasim(i))
       Manwop(i) = MAX(0.0,Manwop(i)-Manwopdcom(i)-Manwopasim(i))
       Manwip(i) = MAX(0.0,Manwip(i)-Manwipasim(i))
       
       Manwip(i) = Manwip(i)+ Manwopdcom(i)+Mansipdcom(i)+Mansopdcom(i)
     +             *0.75
       
       Manwop(i) = Manwop(i) + Mansopdcom(i) * 0.25
       
       ManSO_WI(i) = Mansopdcom(i)*0.75
       ManSI_WI(i) = Mansipdcom(i)
       ManWO_WI(i) = Manwopdcom(i)
       ManSO_WO(i) = Mansopdcom(i) * 0.25
       
       Manmass(i) = MAX(0.0,Manmass(i)-Mandcom(i)-Manasim(i))
       Mancova(i) = MAX(0.0,Mancova(i)-Mancovadcom(i)-Mancovaasim(i))
       IF(Manmass(i)<0.00001) Manmass(i) = 0.0
       
       
       DO j= 1, Nsoil
           CALL KgHaToKg(Labp(j),Labp(j),Areap)
           PRINT *, "CALLING KgHaToKg"
           Call KgHaToKg(Actp(j),Actp(j),Areap)
       END DO
       
       
       
       Actp(1) = Actp(1) + Mansipasim(i) * 0.6
       Labp(1) = Labp(1) + Manwipasim(i)*0.6 +Manwopasim(i)*0.6+
     +           Mansopasim(i)*0.6
       
       ManSI_ActP1(i) = Mansipasim(i) * 0.6
       ManWI_LabP1(i) = Manwipasim(i)*0.6
       ManWO_LabP1(i) = Manwopasim(i)*0.6
       ManSO_LabP1(i) = Mansopasim(i)*0.6
       
       IF(Dsoil(2)<=0.15) THEN
           Actp(2) = Actp(2) + Mansipasim(i) * 0.3
           Actp(3) = Actp(3) + Mansipasim(i) * 0.1
           Labp(2) = Labp(2) + Manwipasim(i)*0.3 +Manwopasim(i)*0.3+
     +           Mansopasim(i)*0.3
           Labp(3) = Labp(3) + Manwipasim(i)*0.1 +Manwopasim(i)*0.1+
     +           Mansopasim(i)*0.1
          
          ManSI_ActP2(i) = Mansipasim(i) * 0.3
          ManSI_ActP3(i) = Mansipasim(i) * 0.1
          
          ManWI_LabP2(i) = Manwipasim(i)*0.3
          ManWI_LabP3(i) = Manwipasim(i)*0.1
          
          ManWO_LabP2(i) = Manwopasim(i)*0.3
          ManWO_LabP3(i) = Manwopasim(i)*0.1
          
          ManSO_LabP2(i) = Mansopasim(i)*0.3
          ManSO_LabP3(i) = Mansopasim(i)*0.1
           
           
       ELSE
           Actp(2) = Actp(2) + Mansipasim(i) * 0.4
           Labp(2) = Labp(2) + Manwipasim(i)*0.4 +Manwopasim(i)*0.4+
     +           Mansopasim(i)*0.4
          
           ManSI_ActP2(i) = Mansipasim(i) * 0.4
           ManWI_LabP2(i) = Manwipasim(i)*0.4
       END IF
       
     
          
      WRITE(115,'(/,A,I2,/)')'Daily Manure P Dynamics: Manure NO',i
      WRITE(115,'(A,F12.5,A)')'Manure Mass Decompostion=',
     +     Mandcom(i)*1000,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure Cover Area Decompostion='
     +                                      ,Mancovadcom(i)*10000,' m2' 
      WRITE(115,'(A,F12.5,A)')'Manure MANSIP Decompostion =',
     +                                         Mansipdcom(i)*1000,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure MANSOP Decompostion =',
     +                                          Mansopdcom(i)*1000,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure MANWOP Decompostion =',
     +                                      Manwopdcom(i)*1000    ,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure Mass Assimilation=',
     +                                        Manasim(i)*1000,     ' gm'
      WRITE(115,'(A,F12.5,A)')'Manure Cover Area Assimilation='
     +                                      ,Mancovaasim(i)*10000,' m2' 
      WRITE(115,'(A,F12.5,A)')'Manure MANSIP Assimilation =',
     +                                         Mansipasim(i)*1000 ,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure MANSOP Assimilation =',
     +                                          Mansopasim(i)*1000,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure MANWOP Assimilation =',
     +                                         Manwopasim(i)*1000 ,' gm'
      WRITE(115,'(A,F12.5,A)')'Manure MANWIP Assimilation=',
     +                                         Manwipasim(i)*1000 ,' gm'
      
      
      WRITE(115,'(A,F10.5,A4)')'Manure Mass=',Manmass(Slnm),'Kg' 
      WRITE(115,'(A,F10.5,A4)')'Manure Moisture=',Manmoist(Slnm),'-'
      WRITE(115,'(A,F10.5,A4)')'Manure Cover Area=',Mancova(Slnm)*1000
     +0          ,'m2'
          WRITE(115,'(A,F10.5,A4)')'MANSIP =',Mansip(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'MANSOP =',Mansop(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'MANWIP =',Manwip(Slnm),'Kg'
          WRITE(115,'(A,F10.5,A4)')'MANWOP =',Manwop(Slnm),'Kg'
       
      IF(Mansipasim(i)>=0.0 .OR. Mansopasim(i)>=0.0.OR.Manwopasim(i)
     + >=0.0 .OR. Manwipasim(i)>=0.0) THEN
          
      WRITE(115,'(/,A)')'Assimilated P is added to Soil Labile & ActiveP
     +Pool'
      WRITE(115,'(/,A,/)')'Soil Labile P & Active P pool are Updated'
        
      DO j= 1, Nsoil
           CALL KgToKgha(Labp(j),Labp(j),Areap)
           PRINT *, "CALLING KgToKgha"
           Call KgToKgha(Actp(j),Actp(j),Areap)
      END DO
 
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A10,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A12,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F12.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
          
          WRITE(115,'(/)')
          String = '('//TRIM(N)//'(A10,I2,2X))'
          WRITE(115,String) ("ActiveP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A12,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F12.5,2X))'
          WRITE(115,String) (Actp(m),m=1,Nsoil) 
          
       DO j= 1, Nsoil
           CALL KgHaToKg(Labp(j),Labp(j),Areap)
           PRINT *, "CALLING KgHaToKg"
           Call KgHaToKg(Actp(j),Actp(j),Areap)
       END DO  
          
          
      END IF
          
 
      IF(Rain > 0.0 .AND. Manmass(i)> 0.0) THEN
          
           W = (Rain/ Manmass(i))*Mancova(i)*100000
           IF((Mtype(i)>=1 .AND. Mtype(i) <=4) .OR.
     +                     (Mtype(i)>=11 .AND. Mtype(i) <=15)) THEN
            Manextrc(i) = MIN(1.0,1.2*W/(W+73.1))
           ELSE
            Manextrc(i) = MIN(1.0,2.2*W/(W+300.1))
           ENDIF
         
      Maniprelease(i) = MAX(0.0,Manextrc(i)*Manwip(i))
      Manoprelease(i) = MAX(0.0,Manextrc(i)*Manwop(i)*1.6)
       
      IF(Maniprelease(i)>Manwip(i))Maniprelease(i)=Manwip(i)
      IF(Manoprelease(i)>Manwop(i))Manoprelease(i)=Manwop(i)
       
      Manwip(i) = MAX(0.0,Manwip(i) - Maniprelease(i))
      Manwop(i) = MAX(0.0,Manwop(i) - Manoprelease(i))
       
      IF(Runoff>0.0) THEN
        Pdfactor =(Runoff/Rain)**0.225
      ELSE
        Pdfactor = 0.0
      ENDIF
       
       Manipcrunoff(i) = (Maniprelease(i) * Pdfactor)/
     + (Rain*Areap *100)
       
       Manopcrunoff(i) = (Manoprelease(i) * Pdfactor)/
     + (Rain*Areap *100)
       
       Manipmrunoff(i) = Manipcrunoff(i)*Runoff*Areap*100
       Manopmrunoff(i) = Manopcrunoff(i)*Runoff*Areap*100
       
      IF(Manipmrunoff(i)>Maniprelease(i))Manipmrunoff(i)=Maniprelease(i)
      IF(Manopmrunoff(i)>Manoprelease(i))Manopmrunoff(i)=Manoprelease(i)
       
      ELSE
            W = 0.0
            Manextrc(i) = 0.0
            Maniprelease(i)= 0.0
            Manoprelease(i) = 0.0
            Manipcrunoff(i) = 0.0
            Manopcrunoff(i)=  0.0
            Manipmrunoff(i) = 0.0
            Manopmrunoff(i) = 0.0
            Pdfactor = 0.0
      ENDIF
       
      
      Labp(1) = Labp(1) + (Maniprelease(i)-Manipmrunoff(i))*0.6
      Labp(1) = Labp(1) + (Manoprelease(i)-Manopmrunoff(i))*0.6
      
      IF(Dsoil(2)<0.15) THEN
        Labp(2) = Labp(2) +(Maniprelease(i)-Manipmrunoff(i))*0.3
        Labp(2) = Labp(2) +(Manoprelease(i)-Manopmrunoff(i))*0.3
        Labp(3) = Labp(3) +(Maniprelease(i)-Manipmrunoff(i))*0.1
        Labp(3) = Labp(3) +(Manoprelease(i)-Manopmrunoff(i))*0.1
      ELSE
        Labp(2) = Labp(2) +(Maniprelease(i)-Manipmrunoff(i))*0.4
        Labp(2) = Labp(2) +(Manoprelease(i)-Manopmrunoff(i))*0.4 
      ENDIF
      
       DO j= 1, Nsoil
           CALL KgToKgha(Labp(j),Labp(j),Areap)
           PRINT *, "CALLING KgToKgha"
           Call KgToKgha(Actp(j),Actp(j),Areap)
       END DO
 
       WRITE(115,'(/,A,F12.5,A4)')'Manure Inorganic P Release =',
     +                                        Maniprelease(i)*1000,'gm'
       WRITE(115,'(A,F12.5,A4)')'Manure Organic P Release =',
     +                                        Manoprelease(i)*1000,'gm'
       
      WRITE(115,'(A,F12.5,A4)')'Manure Inorganic P Mass Runoff =',
     +                                        Manipmrunoff(i)*1000,'gm'
      WRITE(115,'(A,F12.5,A4)')'Manure Organic P P Mass Runoff =',
     +                                        Manopmrunoff(i)*1000,'gm'
       
          WRITE(115,'(A,F12.5,A4)')'MANSIP =',Mansip(Slnm),'Kg'
          WRITE(115,'(A,F12.5,A4)')'MANSOP =',Mansop(Slnm),'Kg'
          WRITE(115,'(A,F12.5,A4)')'MANWIP =',Manwip(Slnm),'Kg'
          WRITE(115,'(A,F12.5,A4)')'MANWOP =',Manwop(Slnm),'Kg'
      
      IF(Maniprelease(i)>0.0 .OR.Manoprelease(i)>0.0) THEN
          
      WRITE(115,'(/,A,/)')'Soil Labile P pool  is  Updated'
        
          j=Nsoil  
          WRITE(N,'(I3)') j
          String = '('//TRIM(N)//'(A10,I2,2X))'
          WRITE(115,String) ("LabileP",m,m=1,Nsoil) 
          String = '('//TRIM(N)//'(A12,2X))'
          WRITE(115,String)("Kg/ha",m=1,Nsoil) 
          String = '('//TRIM(N)//'(F12.5,2X))'
          WRITE(115,String) (Labp(m),m=1,Nsoil) 
      
      END IF
      
      
       
       
       END DO
       
      
       END SUBROUTINE MANUREFATE