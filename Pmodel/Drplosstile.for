      SUBROUTINE DRPLOSSTILE(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine calculates the DRP loss through tile drainage
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  14/10/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      k                  INTEGER            -                 Identifier of soil layer in which GW Table exists
C      r                  REAL               -                 Ratio of macro flow to matrix flow out of first layer
C      Indrpmgw           REAL               Kg                Incoming mass of DRP to groudwater reservoir 
C      
C 
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C  ADDNODE                   The Subroutine add the P pools of the nodes to the P pools of the soil layers
C  BREAKNODE                 The Subroutine assigns P pools and other soil properties of the soil node layers
C                            from the soil layer in which it belongs
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================  
       USE VARIABLE
       IMPLICIT NONE
       INTEGER :: i,k
       DOUBLE PRECISION :: r,Indrpmgw,d1,Indrpmacgw
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       
      
       CALL BREAKNODE
       
       
       DO i = 1, Nnode
          CALL KgHaToKg(Labpnode(i),Labpnode(i),Areap)
       END DO
       
       
        DO k = 1, Nnode
           IF(Dnode(k)>= Dgwt) EXIT    
        END DO
           
       k= MIN(k,Nnode)
        
       WRITE(115,'(/,A,/)')'DRP loss Through TileDrainage'
       WRITE(115,'(A,I3)')'Ground Water table is at Soil Node No',k
       
       
       DO i=1, k
           
        WRITE(115,'(/,A,I3,/)')'Node No :',i  
        WRITE(115,'(A,F12.8,A)')'Labile P Pool =',Labpnode(i),
     +                                                        ' Kg'
       WRITE(115,'(A,F12.5,A)')'Matrix Flow =',
     +                     Matflow(i),' Cm'  
        
       
        IF(Matflow(i)>=0.0) THEN
        
        IF(i>1) THEN
          Labpnode(i) = Labpnode(i)+Pleachmat(i-1)*
     +            (Matflow(i-1)/100.00)*Areap*10**4
        IF(Pleachmat(i-1)*(Matflow(i-1)/100.00)>0.0) 
     +                                                              THEN
          WRITE(115,'(A,I3,A,I3)')'Labile P Pool of soil node NO ',i,' U
     +pdated due to P leached with matrixflow from the Soil node No ',
     + i-1      
         WRITE(115,'(A,F12.8,A)')'Labile P Pool =',Labpnode(i),
     +                                                        ' Kg'
        END IF
        
        END IF

        
        Kd(i) = (100+250*Claynode(i)/100)*0.001
        Massly(i) = Bdsoilnode(i)*Tnode(i)
        
        Clabpsw(i)=(Labpnode(i)/(Areap*(10**4)))/
     +(Kd(i)*Massly(i)+Soilwaternode(i)*Tnode(i))
           
        
      Pleachmat(i)=Clabpsw(i)*(1-EXP(-(0.01*Matflow(i))/
     +    (Kd(i)*Massly(i)+Soilwaternode(i)*Tnode(i))))
      
      
        
       IF(Pleachmat(i)<=0.0) Pleachmat(i) = 0.0
        
       IF(Matflow(i) == 0.0) THEN
           Pleachmat(i) = 0.0 
       ELSE
          IF(Pleachmat(i)*(Matflow(i)*0.01)*Areap*10**4 >
     +                                            Labpnode(i)) THEN
            Pleachmat(i) = Labpnode(i)/((Matflow(i)*0.01)
     +                                          *Areap*10**4)  
          ENDIF 
       ENDIF
     
       
      WRITE(115,'(A,I3,F12.8,A)')'P Leached with matrix flow from Soil n
     +ode NO ',i,Pleachmat(i)*Matflow(i)*0.01*(10**4)
     +                      *1000*Areap                       ,   ' gm'
       
      Labpnode(i)=MAX(Labpnode(i)-Pleachmat(i)
     +    *Matflow(i)*0.01*Areap*10**4,0.0)  
        
        IF(Pleachmat(i)*Matflow(i)>0.0) THEN
         WRITE(115,'(A,I3,A)')'Labile P Pool of Soil node No ',i,' is Up
     +dated'     
      WRITE(115,'(A,F12.8,A)')'Labile P Pool =',Labpnode(i),' Kg'
        END IF
        
        ELSE 
            
        IF(i>1) THEN        
        Matflow(i) =-1.0*Matflow(i)   
        Kd(i) = (100+250*Claynode(i)/100)*0.001
        Massly(i) = Bdsoilnode(i)*Tnode(i)      
        
        Clabpsw(i)=(Labpnode(i)/(Areap*(10**4)))/
     +    (Kd(i)*Massly(i)+Soilwaternode(i)*Tnode(i))
            
        Pleachmat(i)=Clabpsw(i)*
     +                  (1-EXP(-(0.01*Matflow(i))   
     +    /(Kd(i)*Massly(i)+Soilwaternode(i)*Tnode(i))))
        
           
        IF(Pleachmat(i)<=0.0) Pleachmat(i) = 0.0
        
        IF(Pleachmat(i)*(Matflow(i)*0.01)*Areap*10**4 >
     +                                            Labpnode(i)) THEN
        Pleachmat(i) = Labpnode(i)/((Matflow(i)*0.01)
     +                                          *Areap*10**4)  
        END IF  

      WRITE(115,'(A,I3,F12.5,A)')'P Leached with matrix flow from Soil n
     +ode NO ',i,Pleachmat(i)*Matflow(i)*0.01*(10**4)
     +                *1000*Areap     , ' gm'
      
      
      Labpnode(i)=MAX(Labpnode(i)-Pleachmat(i)*
     +            Matflow(i)*0.01*Areap*10**4,0.0)  
      
      Labpnode(i-1) = Labpnode(i-1)+Pleachmat(i)*
     +                  (Matflow(i)/100.00)*Areap*10**4
        
      IF(Pleachmat(i)*Matflow(i)>0.0) THEN
      WRITE(115,'(A,I3,A)')'Labile P Pool of Soil node No ',i,' is Updat
     +ed'     
      WRITE(115,'(A,F12.8,A)')'Labile P Pool =',Labpnode(i),' Kg'
      
      WRITE(115,'(A,I3,A,I3)')'Labile P Pool of soil node NO ',i-1,' Up
     +dated due to P leached with matrixflow from the Soil node No ',
     + i      
      WRITE(115,'(A,F12.8,A)')'Labile P Pool =',Labpnode(i-1),
     +                                                        ' Kg'
        END IF
      
      
      Matflow(i) =-1.0*Matflow(i)  
       
        END IF       
            
        END IF
         
       END DO
       
       d1 = Matflow(1) 
       
       IF(Matflow(1)< 0.0) THEN
           Matflow(1) = 0.0
       END IF
           
       IF((Macflow+ Matflow(1)*0.01)>0)
     +                                                            THEN
        r = Macflow/(Macflow+ 
     +                       Matflow(1)*0.01)  
       ELSE 
        r = 0.0
       ENDIF
       
       Matflow(1) = d1
       
       Pleachmac = r*Clabpsw(1)*(1-EXP(-Rain/
     +    (Kd(1)*Massly(1)+Soilwaternode(1)*Tnode(1))))
       
       IF(Pleachmac<=0.0) Pleachmac=0.0
       Labpnode(1)=Max(Labpnode(1)-Pleachmac*Macflow*
     +         Areap*10**4,0.0)
      
      WRITE(115,'(/,A,F12.5,A)')'P Leached with Macro flow',
     + Pleachmac*1000,' gm/m3'
      
      IF(Pleachmac>0.0) THEN
      WRITE(115,'(A,I3,A)')'Labile P Pool of first soil node is updated'
      WRITE(115,'(A,F10.5,A)')'Labile P Pool =',Labpnode(1)/Areap,
     + ' Kg/ha'
      END IF
         
        
      Hgwt = MAX(Dimvly-Dgwt,0.0)

      
      Indrpmgw = (Pleachmac*Macflow*Areap*10**4)+ 
     + (Pleachmat(k)*(ABS(Matflow(k))*0.01)*Areap*10**4)
      
      Indrpmacgw = Pleachmac*Macflow*Areap*10**4
       
       
       IF(Tiledrain>0.0 .AND. Hgwt > 0.0) THEN   
          Mdrpgw = (Hgwt*Indrpmgw)/Tiledrain +
     +    (Inidrpmgw-(Hgwt*Indrpmgw)/Tiledrain)*
     +              EXP(-Tiledrain/Hgwt) 
          
          Mdrpmacgw = (Hgwt*Indrpmacgw)/Tiledrain +
     +    (Inidrpmacgw-(Hgwt*Indrpmacgw)/Tiledrain)*
     +              EXP(-Tiledrain/Hgwt) 
       ELSE
          Mdrpgw =  Indrpmgw + Inidrpmgw
          Mdrpmacgw  = Indrpmacgw + Inidrpmacgw
       END IF
       
       
        
      WRITE(115,'(A,F12.5,A)')'Initial DRP Mass in Groundwater=',
     +                                             Inidrpmgw*1000,' gm' 
       WRITE(115,'(A,F12.5,A)')'Incoming DRP Mass to Groundwater=',
     +                                              Indrpmgw*1000,' gm'
      WRITE(115,'(A,F12.5,A)')'Final DRP Mass in Groundwater=',
     +                                            Mdrpgw*1000   , ' gm' 
       
       IF(Hgwt > 0.0) THEN
        Cdrpgw = (Inidrpmgw + Mdrpgw)/(2*Hgwt*Areap*10**4)
        Cdrpmacgw = (Inidrpmacgw + Mdrpmacgw)/(2*Hgwt*Areap*10**4)
       ELSE
        Cdrpgw = 0.0
        Cdrpmacgw = 0.0
       END IF
       
      WRITE(115,'(A,F12.5,A)')'Concentratuion of DRP in Groundwater=', 
     +                                              Cdrpgw*1000,' gm/m3'
       
       
      Drplosstdrain=(Cdrpgw*Tiledrain*Areap*10**4)
     + /Areap
      
      Drpmactdrain = (Cdrpmacgw*Tiledrain*Areap*10**4)
     + /Areap
      
      IF(Drplosstdrain*Areap>Mdrpgw) THEN
          Drplosstdrain = Mdrpgw/Areap
      ENDIF
      
      IF(Drpmactdrain*Areap>Mdrpmacgw) THEN
         Drpmactdrain = Mdrpmacgw/Areap
      ENDIF
      
      WRITE(115,'(A,F12.5,A)')'DRP loss through TileDrainage=', 
     +                                       Drplosstdrain*1000,' gm/ha'
      
      Drplossdesp = (Cdrpgw*Deepsepg*Areap*10**4)
     + /Areap
     
      Drplosslatflow = (Cdrpgw*Latflow*Areap*10**4)
     + /Areap
      
       !Drplossdesp =0.0
       !Drplosslatflow=0.0
      
      Inidrpmgw = MAX(Mdrpgw-(Drplossdesp+Drplosslatflow)*Areap,0.0) 
      Inidrpmacgw = MAX(Mdrpmacgw-Drpmactdrain*Areap,0.0) 
      

       
       DO i = 1, Nnode
        CALL KgToKgHa(Labpnode(i),Labpnode(i),Areap)
       END DO    
      
      CALL ADDNODE
      
      
      END SUBROUTINE  DRPLOSSTILE