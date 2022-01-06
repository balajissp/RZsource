      SUBROUTINE PPLOSSRUNOFF(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine calculates the Particulate P (PP) loss through runoff
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
C      x                  REAL            -                   A Variable
C     Enr                 REAL            -                   Enrichment Ratio
C     i                   INTEGER         -                   Loop counter
C     
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C    SEDIMENT                The subroutine calculates the daily sediment yeild using MUSLE method
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================  
      USE VARIABLE
      IMPLICIT NONE
      INTEGER :: i
      DOUBLE PRECISION :: x,Enr
      INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
      
       
       x = 100.00/(Bdsoil(1)*Tsoil(1))
      
      PPclabp = Labp(1)*x
      PPcactp = Actp(1)*x
      PPcstabip = Stabip(1)*x
      PPcstabop = Stabop(1)*x  
      PPcfrsop = Frsop(1)*x
      
      
      DO i = 1, Slnm
       PPcmanwop(i) = (Manwop(i)/Areap)*x
       PPcmanwip(i) = (Manwip(i)/Areap)*x
       PPcmansip(i) = (Mansip(i)/Areap)*x
       PPcmansop(i) = (Mansop(i)/Areap)*x
      END DO
      
      DO i = 1, Slnf
        PPcavfertp(i) = (Avfertp(i)/Areap)*x
        PPcresfertp(i) = (Resfertp(i)/Areap)*x
      END DO 
      
C      CALL SEDIMENT
      
       Sed = Sedi
       
      
      IF(Runoff > 0.0 .AND. Sed > 0.0) THEN
       Enr = 0.78*
     +    ((Sed/(10.0*Areap*Runoff*10.0))**-0.2468)
     
      ELSE
       Enr = 0.0
      ENDIF
     
      
      PPmlabp = MIN((0.001*PPclabp*Sed*Enr)/Areap,Labp(1)) 
      PPmactp = MIN((0.001*PPcactp*Sed*Enr)/Areap,Actp(1))
      PPmstabip = MIN((0.001*PPcstabip*Sed*Enr)/Areap,Stabip(1))
      PPmstabop = MIN((0.001*PPcstabop*Sed*Enr)/Areap,Stabop(1))
      PPmfrsop = MIN((0.001*PPcfrsop*Sed*Enr)/Areap,Frsop(1))
      
      PPmmanp = 0.0
      
      DO i=1,Slnm
        PPmmanwop(i) = MIN((0.001*PPcmanwop(i)*Sed*Enr)/Areap,Manwop(i))
        PPmmanwip(i) = MIN((0.001*PPcmanwip(i)*Sed*Enr)/Areap,Manwip(i))
        PPmmansip(i) = MIN((0.001*PPcmansip(i)*Sed*Enr)/Areap,Mansip(i))
        PPmmansop(i) = MIN((0.001*PPcmansop(i)*Sed*Enr)/Areap,Mansop(i))
        PPmmanp = PPmmanp + PPmmanwop(i)+ PPmmanwip(i)+PPmmansip(i)+
     +            PPmmansop(i)
      END DO
      
      PPmfertp = 0.0
      
      DO i=1,Slnf
       PPmavfertp(i)=MIN((0.001*PPcavfertp(i)*Sed*Enr)/Areap,Avfertp(i))
        PPmresfertp(i)=MIN((0.001*PPcresfertp(i)*Sed*Enr)/Areap,
     +                      Resfertp(i))
        PPmfertp = PPmfertp + PPmavfertp(i) + PPmresfertp(i)
      END DO
        
        
      PPlossrnf = PPmlabp+ PPmactp + PPmstabip + PPmstabop + PPmfrsop + 
     +            PPmmanp + PPmfertp
      
      Labp(1) = MAX(Labp(1) - PPmlabp,0.0)
      ActP(1) = MAX(Actp(1) -  PPmactp,0.0)
      Stabip(1) = MAX(Stabip(1) - PPmstabip,0.0)
      Stabop(1) = MAX(Stabop(1) - PPmstabop,0.0)
      Frsop(1) = MAX(Frsop(1) - PPmfrsop,0.0)
      

      
      DO i = 1,Slnm
         Manwop(i) = MAX(Manwop(i)-PPmmanwop(i),0.0) 
         Manwip(i) = MAX(Manwip(i)-PPmmanwip(i),0.0)
         Mansip(i) = MAX(Mansip(i)-PPmmansip(i),0.0)
         Mansop(i) = MAX(Mansop(i)-PPmmansop(i),0.0)
      END DO
      
      DO i=1,Slnf
          Avfertp(i) = MAX(Avfertp(i)-PPmavfertp(i),0.0)
          Resfertp(i) = MAX(Resfertp(i)-PPmresfertp(i),0.0)
      END DO

      
      WRITE(115,'(/,A,/)')'PP loss Through Runoff'
      
      WRITE(115,'(/,A,F12.5,A)')'PP Loss from Fertiliser P Pool through 
     +Runoff =',  PPmfertp*1000,' gm/ha'
      WRITE(115,'(A,F12.5,A)')'PP Loss from Manure P Pool through Runoff
     + =',  PPmmanp*1000,' gm/ha'    
      WRITE(115,'(A,F12.5,A)')'PP Loss from Labile P Pool through Runoff
     + =',  PPmlabp*1000,' gm/ha'
      WRITE(115,'(A,F12.5,A)')'PP Loss from Active P Pool through Runoff
     + =',  PPmactp*1000,' gm/ha'
      WRITE(115,'(A,F12.5,A)')'PP Loss from StabOP Pool through Runoff
     + =',  PPmstabop*1000, 'gm/ha'
      WRITE(115,'(A,F12.5,A)')'PP Loss from StabIP Pool through Runoff
     + =',  PPmstabip*1000,' Kg/ha'
      WRITE(115,'(A,F12.5,A)')'PP Loss from FrsOP Pool through Runoff
     + =',  PPmfrsop*1000,' gm/ha'
      WRITE(115,'(A,F12.5,A)')'Total PP Loss from  through Runoff = '
     + , PPlossrnf*1000,' gm/ha'
      
      IF(PPmfertp>0.0) THEN
      DO i=1,Slnf    
      WRITE(115,'(/,A,I3)')'Fertilizer P Pools Updated,Ferilizer No ',i
      WRITE(115,'(A,F12.5,A6)')'Available Fertilizer P=',Avfertp(i)
     +                                                        ,'Kg'
      WRITE(115,'(A,F12.5,A6)')'Residual Fertilizer P=',Resfertp(i)
     +                                                ,       'Kg'   
      END DO
      END IF
      
      
      IF(PPmmanp>0.0) THEN
      DO i=1,Slnm  
        WRITE(115,'(/,A,I3)')'Manure P Pools Updated, Manure No ',i 
        WRITE(115,'(A,F12.5,A4)')'MANSIP =',Mansip(Slnm),'Kg'
        WRITE(115,'(A,F12.5,A4)')'MANSOP =',Mansop(Slnm),'Kg'
        WRITE(115,'(A,F12.5,A4)')'MANWIP =',Manwip(Slnm),'Kg'
        WRITE(115,'(A,F12.5,A4)')'MANWOP =',Manwop(Slnm),'Kg'
      END DO
      END IF
      
      IF(PPmlabp>0.0 .OR.PPmactp>0.0 .OR. PPmstabop>0.0 .OR.
     +PPmstabip>0.0 .OR. PPfrsop>0.0) THEN
       WRITE(115,'(/,A)')'SOIL P Pools of the First Layer are Updated'  
       WRITE(115,'(A,F12.5,A)')'Labile P Pool =',Labp(1),' Kg/ha'
       WRITE(115,'(A,F12.5,A)')'Active P Pool =',Actp(1),' Kg/ha'
       WRITE(115,'(A,F12.5,A)')'StabIP Pool =',Stabip(1),' Kg/ha'
       WRITE(115,'(A,F12.5,A)'),'StabOP = ',Stabop(1),' Kg/ha'
       WRITE(115,'(A,F12.5,A)'),'FreOP = ',Frsop(1),' Kg/ha'
      END IF
        
      END SUBROUTINE PPLOSSRUNOFF