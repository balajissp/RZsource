      SUBROUTINE WRITELOG(ID,IM,IYYY)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The subroutine WRITES THE LOG FILE
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  03/02/2017       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                  INTEGER            -                 Loop Counter 
C      String             Character          -                 Holds the Formats of the output
C      N                  Character          -                 A Variable 
C FUNCTIONS/SUBROUTINE CALLED ::
C
C       NAME                    DESCRIPTION
C    ----------              ------------------           
C   
C===============================================================================
C                         END OF SPECIFICATION
C===============================================================================  
      USE VARIABLE
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: ID,IM,IYYY
      INTEGER :: i,Day,Month,Year
      DAY = ID
      Month = IM
      Year = IYYY
      IF(Flag) THEN 
         WRITE(115,'(A,/)')'INITIALIZATION OF SOIL P POOLS'
         WRITE(115,'(A,I2,/)')"No of Soil Layer=",Nsoil
         DO i = 1,Nsoil
             WRITE(115,'(A5,I2,A7,/)')'Layer',i,'(Kg/ha)'
             WRITE(115,'(5(A12,2X))')'LabileP','ActiveP','StabOP',
     +                                'StabIP','FrshOP'
             WRITE(115,'(5(F12.5,2X),/)')Labp(i),Actp(i),Stabop(i),
     +                                    Stabip(i),Frsop(i)
         END DO
         
         WRITE(115,'(A,F12.5,A)')'Initial DRP mass in GW Reservoir =',
     +    Inidrpmgw,' Kg/ha'
         
         WRITE(115,'(A,F12.5,A,/)')'Initial PP mass in GW Reservoir = ',
     +    Inippmgw,' Kg/ha'
         
         IF(Psts) THEN
             WRITE(115,'(A)')'Crop P Stress Computation is Enabled'
         ELSE
             WRITE(115,'(A)')'Crop P Stress Computation is Disabled'
         ENDIF
             
         WRITE(115,'(/,A)')'START OF SIMULATION'
         Flag = .False.
      END IF           
          
          WRITE(115,'(/,A)')'===========================================
     +================================================================='
          WRITE(115,'(/,A6,2(I2,A2),I4,/)'),"Date:",Day,'-',Month,'-',
     +                                               Year   
         
      WRITE(115,'(A20,F10.5,A4)')"Rainfall=",Rain,'Cm'
      WRITE(115,'(A20,F10.5,A4)')"Runoff=",Runoff,'Cm'
      WRITE(115,'(A20,F10.5,A4)')"GW Depth=",Dgwt,'m'
          WRITE(115,'(A20,F10.5,A4)')"Macropore Flow=",
     +                                     Macflow,'m'
          WRITE(115,'(A20,F10.5,A8)')"Sediment=",Sedi,
     +                                                          'MT/day'
          WRITE(115,'(A20,F10.5,A4)')"Tile Drainage=",
     +                                Tiledrain,'Cm'
          
          WRITE(115,'(A20,F10.5,A6)')"Surface Residue P=",
     +                               ResdueP0,'Kg/ha'
      
      DO i = 1,Nsoil
       WRITE(115,'(/,A5,I2,/)')'Layer',i
       WRITE(115,'(8(A20,2X))') 'SoilNO3 (Kg/ha)',
     +        'SoilTemp (0C)','SoilWater (m3/m3)',"ResidueP1(Kg/ha)", 
     +        "ResidueP2(Kg/ha)", "HumusP1(Kg/ha)", "HumusP2(Kg/ha)",
     +         "HumusP3(Kg/ha)"   
       WRITE(115,'(8(F20.5,2X),/)') SoilNo3(i),
     +  SoilTemp(i),SoilWater(i),
     +  ResdueP1(i),ResdueP2(i),HumusP1(i),HumusP2(i),HumusP3(i)
             
      END DO
       
      END SUBROUTINE WRITELOG