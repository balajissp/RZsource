      SUBROUTINE PPLOSSTILE(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine simulates the loss Particulate P (PP) Through Tile
C     Drainage
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  22/11/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      j                  INTEGER            -                 Loop Counter 
C      Frcn               REAL               -                 Fraction of land surface covered by
C                                                              Crop Canopy  
C      C                  REAL               -                 Emperical Crop factor for the particle detachment
C      E                  REAL               J/m2/mm           Kinetic Energy of Rain
C      Mmax               REAL               gm/gm soil        Maximum amount of readily availble dispersible paricle
C      K                  REAL               gm/m2/day         A Factor
C      Dparti             REAL               gm/m2/day         Detachment of soil particle 
C      Cspmac             REAL               gm/m2/mm          Concentration of suspeneded particle in macropore      
C      Msp                REAL               gm/m2             Mass of Suspended particle reaching Groundwater Reservoir
C      Fmd                REAL               -                 A factor
C      Inppmgw            REAL               Kg                Incoming PP mass to groundwater reservoir
C
C FUNCTIONS/SUBROUTINE CALLED ::
C
C      NAME                    DESCRIPTION
C    ----------              ------------------           
C     CROPDAYCHECK         The Subroutine checks if on a particularday there is 
C                          standing crop on the field or not
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================   
        USE VARIABLE
        IMPLICIT NONE
        INTEGER :: i
        DOUBLE PRECISION :: Frcn,C,E,Mmax,K,Dparti,Cspmac,Msp,Fmd
        DOUBLE PRECISION :: Inppmgw
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
        
        CALL CROPDAYCHECK(ID,IM,IYYY)
        
        
        IF(CROPDAY) THEN
        Frcn=6.5*(LAIP**0.75)*(Plntr(Slnc)*1000)**-0.48
          IF(Frcn>1.0) THEN 
              Frcn=1.0
          ENDIF
          C = 1-Frcn*EXP(-0.34*Plnth*0.6)
        ELSE
          C= 1.0
        END IF
        
        
        
        IF(Rain>0) THEN
         E = 29.0*(1-0.72*EXP(-0.05*Rain*10.0))
        ELSE
         E= 0.0
        END IF
        
        Mmax = (0.362*Clayp(1) - 0.518)/100.00
        
        IF(Mmax > 0.0) THEN
         K = Kdh*E*Rain*10.0*C + Kr/Mmax
         Ms = MIN((Kr + (K*Ms-Kr)*EXP(-K/(Bdsoil(1)*1000*Dsoil(1))))/K,
     +            Mmax)
        ELSE
         Ms = 0.0
        END IF
        
       
        
        DO i = 1, Ntill
          IF(Day==Tday1(i).AND.Month==Tmon(i).AND.Year==Tyear(i)) THEN
           IF(Ms<0.5*Mmax) Ms=0.5*Mmax        
          END IF
        END DO
              
              
        WRITE(115,'(/,A,/)')'PP loss Through Tile Drainage'
        WRITE(115,'(A,F12.5,A)')'Max = ',Mmax,' gm/gm soil'
        WRITE(115,'(A,F12.5,A)')'Ms = ',Ms,' gm/gm soil'
         
        Dparti = MAX(Kdh*E*Rain*10.0*Ms*C,0.0)
        
        
        Tdparti = Tdparti + Dparti
        
        WRITE(115,'(A,F12.5,A)')'Soil Detachments= ',Dparti,' gm/m2/day'
        WRITE(115,'(A,F12.5,A)')'Total Detached Particle mass = ',
     +                                             Tdparti,' gm/m2/day'
        Cspmac = Tdparti/
     +         (Rain*10.0+Dnode(1)*1000.D0*
     +                            Soilwater(1))  
        
        
        IF(Macflow>0.0) THEN
         Msp = Cspmac*Macflow*1000*
     +                   EXP(-Kf*Dgwt)      
         Tdparti = 0.0
        ELSE
         Msp = 0.0  
        END IF
        
        Fmd = Msp/(Bdsoil(1)*1000*Dsoil(1))
        
        
        
        WRITE(115,'(A,F12.5,A)')'Soil Particle Concentration in Macro Fl
     +ow=',   Cspmac,' gm/m2/mm'
        
        WRITE(115,'(A,F12.5,A)')'Flux of Soil particle Reaching Groundwa
     +ter ='          ,   Msp,' gm/m2'
        WRITE(115,'(A,F12.5)')'Factor Fmd = ',Fmd
        
        WRITE(115,'(A,F12.5,A)')'Total Detached Particle mass = ',
     +                                   Tdparti,     ' gm/m2/day'
        
        
        PPlabp = MIN(Fmd*Labp(1),Labp(1))
        PPactp = MIN(Fmd*Actp(1),Labp(1))
        PPstabip = MIN(Fmd*Stabip(1),Stabip(1))
        PPfrsop = MIN(Fmd*Frsop(1),Frsop(1))
        PPstabop = MIN(Fmd*Stabop(1),Stabop(1))
        
        WRITE(115,'(A,F12.5,A)')'PP loss from Labile P pool=',
     +                                         PPlabp*1000,   ' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from Active P pool=',
     +                                  PPactp*1000,         ' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from StabIP pool=',
     +                                    PPstabip*1000,      ' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from StabOP pool=',
     +                                            PPstabop*1000,' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from FrsOP pool=',PPfrsop*1000,
     +                                                          ' gm/ha'
        
        Labp(1) = MAX(Labp(1)-PPlabp,0.0)
        Actp(1) = MAX(Actp(1)-PPactp,0.0)
        Stabip(1)=MAX(Stabip(1)-PPstabip,0.0)
        Frsop(1)= MAX(Frsop(1)-PPfrsop,0.0)
        Stabop(1)=MAX(Stabop(1)-PPstabop,0.0)
        
      
     
      IF(PPlabp>0.0 .OR. PPactp>0.0 .OR. PPstabip>0.0 .OR.
     + PPfrsop>0.0 .OR. PPstabop>0.0  ) THEN
          
       WRITE(115,'(/,A,/)')'Soil P pool of the first soil layer  are upd
     +ated' 
       WRITE(115,'(5(A12,2X))')'LabileP','ActiveP','StabOP',
     +                                'StabIP','FrshOP'
        WRITE(115,'(5(A12,2X))')' Kg/ha',' Kg/ha',' Kg/ha',
     +                                ' Kg/ha',' Kg/ha'
        WRITE(115,'(5(F12.5,2X),/)')Labp(1),Actp(1),Stabop(1),
     +                                    Stabip(1),Frsop(1)
       END IF
        
        
        
        PPmanp = 0.0
        
        DO i=1,Slnm
         PPmanwip(i) = MIN(Fmd*(Manwip(i)/Areap),Manwip(i)/Areap)
         PPmanwop(i) = MIN(Fmd*(Manwop(i)/Areap),Manwop(i)/Areap)
         PPmansop(i) = MIN(Fmd*(Mansop(i)/Areap),Mansop(i)/Areap)
         PPmansip(i) = MIN(Fmd*(Mansip(i)/Areap),Mansip(i)/Areap)
         PPmanp = PPmanp + PPmanwip(i)+PPmanwop(i)+PPmansop(i)+
     +             PPmansip(i)
         
        IF(PPmanwip(i)>0.0 .OR.PPmanwop(i)>0.0 .OR. PPmansop(i)>0.0 .OR.
     +  PPmansip(i)>0.0 ) THEN
        WRITE(115,'(/,A,I3)') 'PP Loss for Manure P pool,Manure No',i   
        WRITE(115,'(A,F12.5,A)')'PP loss from MANWIP pool =',
     +                                        PPmanwip(i)*1000,' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from MANWOP pool =',
     +                                        PPmanwop(i)*1000,' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from MANSIP pool =',
     +                                        PPmansip(i)*1000,' gm/ha'
        WRITE(115,'(A,F12.5,A)')'PP loss from MANSOP pool =',
     +                                        PPmansop(i)*1000,' gm/ha'
        END IF
        END DO
        WRITE(115,'(A,F12.5,A)')'Total PP loss from Manure P pool =',
     +                                             PPmanp*1000,' gm/ha'
        
        
        DO i=1,Slnm
          Manwip(i) = MAX(Manwip(i)-PPmanwip(i)*Areap,0.0)
          Manwop(i) = MAX(Manwop(i)-PPmanwop(i)*Areap,0.0)
          Mansop(i) = Max(Mansop(i)-PPmansop(i)*Areap,0.0)
          Mansip(i) = MAX(Mansip(i)-PPmansip(i)*Areap,0.0)
          
        IF(PPmanwip(i)>0.0 .OR.PPmanwop(i)>0.0 .OR. PPmansop(i)>0.0 .OR.
     +  PPmansip(i)>0.0 ) THEN
         WRITE(115,'(/,A,I3)') 'Manure P pools are updated,Manure No',i 
         WRITE(115,'(A,F12.5,A4)')'MANSIP =',Mansip(i),'Kg'
         WRITE(115,'(A,F12.5,A4)')'MANSOP =',Mansop(i),'Kg'
         WRITE(115,'(A,F12.5,A4)')'MANWIP =',Manwip(i),'Kg'
         WRITE(115,'(A,F12.5,A4)')'MANWOP =',Manwop(i),'Kg'  
        END IF  
        END DO
        
        PPfertp = 0.0
        
        DO i = 1,Slnf
         PPavfertp(i) = MIN(Fmd*(Avfertp(i)/Areap),Avfertp(i)/Areap)
         PPresfertp(i) = MIN(Fmd*(Resfertp(i)/Areap),Resfertp(i)/Areap)
         PPfertp = PPfertp + PPavfertp(i) + PPresfertp(i)
         
         IF(PPavfertp(i)>0.0 .OR.PPresfertp(i)>0.0) THEN
         WRITE(115,'(/,A,I3)') 'PP Loss for Fertilizer P pool,Fertilizer
     +No',i 
         WRITE(115,'(A,F12.5,A)')'PP loss from AvfertP pool =',
     +                                        PPavfertp(i)*1000,' gm/ha'
         WRITE(115,'(A,F12.5,A)')'PP loss from ResfertP pool =',
     +                                      PPresfertp(i)*1000,' gm/ha'
         END IF
        END DO
        
        WRITE(115,'(A,F12.5,A)')'Total PP loss from Fertilizer P pool ='
     +                                       ,    PPfertp*1000,' gm/ha'
    
        DO i = 1,Slnf
          Avfertp(i)= MAX(Avfertp(i)-PPavfertp(i)*Areap,0.0)
          Resfertp(i)=MAX(Resfertp(i)-PPresfertp(i)*Areap,0.0)
          
          IF(PPavfertp(i)>0.0 .OR.PPresfertp(i)>0.0) THEN
          WRITE(115,'(/,A,I3)')'Fertilizer P pools are updated, Frertili
     +zer No',i  
          WRITE(115,'(A,F12.5,A6)')'Available Fertilizer P=',Avfertp(i)
     +                                                        ,'Kg'
          WRITE(115,'(A,F12.5,A6)')'Residual Fertilizer P=',Resfertp(i)
     +                                                ,       'Kg'
          
          END IF    
        END DO
        
        Inppmgw=(PPlabp+PPactp+PPstabip+PPfrsop+PPstabop+PPmanp+PPfertp)
     +           *Areap  
        
        Hgwt = MAX(Dimvly - Dgwt,0.0) 
        
       
        !Tiledrain(Day,Month,Year) = Tiledrain(Day,Month,Year)*0.01
        
        IF(Tiledrain>0.0 .AND.Hgwt>0.0) THEN 
            Mppgw = (Hgwt*Inppmgw)/Tiledrain +
     +             (Inippmgw-(Hgwt*Inppmgw)/Tiledrain)*
     +              EXP(-Tiledrain/Hgwt) 
        ELSE
            Mppgw =  Inppmgw + Inippmgw
        END IF
        
      WRITE(115,'(A,F12.5,A)')'Initial PP Mass in Groundwater=',
     +                                            Inippmgw*1000,' gm' 
      WRITE(115,'(A,F12.5,A)')'Incoming PP Mass to Groundwater=',
     +                                            Inppmgw*1000,' gm'  
      WRITE(115,'(A,F12.5,A)')'Final PP Mass in Groundwater=',Mppgw*1000
     +                                                      , ' gm' 
        
        
        IF(Hgwt>0.0) THEN
         Cppgw = (Inippmgw+ Mppgw)/(2*Hgwt*Areap*10**4)
        ELSE
         Cppgw = 0.0
        END IF
        
        WRITE(115,'(A,F12.5,A)')'Concentratuion of PP in Groundwater=', 
     +                                              Cppgw*1000,' gm/m3' 
        
        PPlosstdrain=(Cppgw*Tiledrain*Areap*10**4)
     +   /Areap
        
        
        
        IF(PPlosstdrain*Areap>Mppgw) THEN
            PPlosstdrain = Mppgw/Areap
        END IF
        
        WRITE(115,'(A,F12.5,A)')'PP loss through Tile Drainage=', 
     +                                        PPlosstdrain*1000,' gm/ha'
        
       PPlossdesp= (Cppgw*Deepsepg*Areap*10**4)
     +   /Areap
       
       PPlosslatflow = (Cppgw*Latflow*Areap*10**4)
     +   /Areap
        
         !PPlossdesp=0.0
         !PPlosslatflow=0.0
       
      Inippmgw = MAX(Mppgw-(PPlossdesp+PPlosslatflow)*Areap,0.0)
        
       
        
      END SUBROUTINE PPLOSSTILE