
      SUBROUTINE WRITEHEADER
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine writes the header to the output files
C     
C RECORDS OF REVISION ::
C
C        DATE           PROGRAMER            DESCRIPTION
C      --------      ----------------    ------------------
C 1.  19/07/2016       D SADHUKHAN          ORIGNIAL CODE
C
C 
C DEFINATION OF THE VARIABLES ::
C
C           NAME           TYPE              UNITS               DESCRIPTION
C      ---------------    -------        ----------------     -------------------
C      i                 INTEGER            -                  Loop Counter
C      N                 Character          -                  A Variable
C      String            Character          -                  Holds the Formats of the output
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
       INTEGER :: i
       CHARACTER(LEN=100)::String,N
       i = Nsoil*5
       WRITE(N,'(I3)') i
       String = "(3A6,2X,"//TRIM(N)//"(A10,I2,2X),4A14)"
       WRITE(112,String)"Day","Month","Year",("LabileP_",i,i=1,Nsoil),
     + ("ActiveP_",i,i=1,Nsoil),("StabIP_",i,i=1,Nsoil),
     + ("StabOP_",i,i=1,Nsoil),("FrshOP_",i,i=1,Nsoil),"TotalSoilP",
     +  "TotalManP","TotalFertP","TotalGWP"
       String = "(3A6,2X,"//TRIM(N)//"(A12,2X),4A14)"
       WRITE(112,String) " - "," - "," - ",('Kg/ha',i=1,Nsoil*5+4)
       
       Write(N,'(I3)') Nsoil
       String = "(3A6,2X,12(A12,2X),"//TRIM(N)//"(A10,I2,2X))"
       WRITE(113,String) "Day","Month","Year","LabPRnf","FertPRnf",
     +     "ManPRnf","DRPRnF","PPRnf","DRPTile","DRPMacTile","PPTile",
     +  "DRPLFlow","PPLFlow","DRPDesp","PPDesp",
     +  ("PlantPUP",i,i=1,Nsoil)
       
       String = "(3A6,2X,12(A12,2X),"//TRIM(N)//"(A12,2X))"
       WRITE(113,String) "-","-","-",("gm/ha",i=1,Nsoil+11)
       
       i = Nsoil*8
       WRITE(N,'(I3)') i
       String ='(3A6,2X,'//TRIM(N)//'(A18,I2,2X),18(A20,2X))'
       WRITE(114,String)"Day","Month","Year",("ActToLab",i,i=1,Nsoil),
     + ("LabToAct",i,i=1,Nsoil),("ActToStbi",i,i=1,Nsoil), 
     + ("StbiToAct",i,i=1,Nsoil),
     + ("FrsoToLab",i,i=1,Nsoil),("FrsoToStbo",i,i=1,Nsoil), 
     + ("LabToFrso",i,i=1,Nsoil),("StboToLab",i,i=1,Nsoil),
     + 'FertPToLabP1','FertPToLabP2','ManSO_To_WI','ManSI_To_WI',
     + 'ManWO_To_WI','ManSO_To_WO','ManWI_To_LabP1','ManWI_To_LabP2',
     + 'ManWI_To_LabP3','ManWO_To_LabP1','ManWO_To_LabP2',
     + 'ManWO_To_LabP3','ManSO_To_LabP1','ManSO_To_LabP2',
     + 'ManSO_To_LabP3','ManSI_To_ActP1','ManSI_To_ActP2',
     + 'ManSI_To_ActP3'
       
       String = "(3A6,2X,"//TRIM(N)//"(A20,2X),18(A20,2X))"
       WRITE(114,String)"-","-","-",("gm/ha",i=1,Nsoil*8+18)
       
       WRITE(115,'(A,/)')"------------LOG FILE OF P MODULE---------"
       
       
       String ="(3A6,8(A22,2X))"
       WRITE(116,String)"Day","Month","Year","AvFertP","ResFertP",
     + "ManWOP","ManWIP","ManSOP","ManSIP",'TotalFertP', 'TotalManP'
       
       String = "(3A6,8(A22,2X))"
       WRITE(116,String)"-","-","-",("Kg/ha",i=1,8)
       
       String ="(3A6,11(A22,2X))"
       WRITE(117,String)"Day","Month","Year","IniSoilP","FertP_Input",
     + "ManP_Input","ResidueP_Input","PLoss_LF","PLoss_DS",'PLoss_RO',
     + 'Ploss_DR','PlnatP_Up', 'FinalSoilP','Pbalance_Error'  
       
       String = "(3A6,11(A22,2X))"
       WRITE(117,String)"-","-","-",("Kg/ha",i=1,11)
       
       String ="(A10,11(A22,2X))"
       WRITE(118,String)"Year","IniSoilP","FertP_Input",
     + "ManP_Input","ResidueP_Input","PLoss_LF","PLoss_DS",'PLoss_RO',
     + 'Ploss_DR','PlnatP_Up', 'FinalSoilP','Pbalance_Error'  
       
       String = "(A10,11(A22,2X))"
       WRITE(118,String)"-",("Kg/ha",i=1,11)
       
       
       
       END SUBROUTINE WRITEHEADER