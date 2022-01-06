      SUBROUTINE TOTALP(ID,IM,IYYY,AllSoilP)
C================================================================================
C                              START OF SPECIFICATION
C=================================================================================
C PURPOSE ::
C
C     The Subroutine calulates the Total Soil P 
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
C
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
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       DOUBLE PRECISION,INTENT(OUT) :: AllSoilP     
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       TotalsoilP = 0.0
       TotalResHumP = 0.0
       AllSoilP = 0.0
       
       DO i =1,Nsoil
         TotalsoilP = TotalsoilP + Labp(i) + Actp(i) + Stabip(i) +
     +                + Stabop(i) + Frsop(i)  
         TotalResHumP = ResdueP1(i)+ ResdueP2(i)+ HumusP1(i) 
     +    + HumusP2(i) + HumusP3(i)+ TotalResHumP
       END DO
       
       TotalResHumP = TotalResHumP + ResdueP0
       
       Totm =0.0
       TotManwip = 0.0
       TotManwop = 0.0
       TotMansip = 0.0
       TotMansop = 0.0
       TotManWI_LabP1 = 0.0
       TotManWI_LabP2 = 0.0
       TotManWI_LabP3 = 0.0
       TotManWO_LabP1 = 0.0
       TotManWO_LabP2 = 0.0
       TotManWO_LabP3 = 0.0
       TotManSO_LabP1 = 0.0
       TotManSO_LabP2 = 0.0
       TotManSO_LabP3 = 0.0
       TotManSI_ActP1 = 0.0         
       TotManSI_ActP2 = 0.0
       TotManSI_ActP3 = 0.0
       TotManSO_WI = 0.0
       TotManSI_WI = 0.0
       TotManWO_WI = 0.0
       TotManSO_WO = 0.0
       
       DO i=1,Slnm
           Totm = Totm + Manwip(i)+Manwop(i)+Mansop(i)+Mansip(i)
           TotManwip = TotManwip + Manwip(i)
           TotManwop = TotManwop + Manwop(i)
           TotMansop = TotMansop + Mansop(i)
           TotMansip = TotMansip + Mansip(i) 
           TotManWI_LabP1 = TotManWI_LabP1 + ManWI_LabP1(i)
           TotManWI_LabP2 = TotManWI_LabP2 + ManWI_LabP2(i)
           TotManWI_LabP3 = TotManWI_LabP3 + ManWI_LabP3(i)
           TotManWO_LabP1 = TotManWO_LabP1 + ManWO_LabP1(i)
           TotManWO_LabP2 = TotManWO_LabP2 + ManWO_LabP2(i)
           TotManWO_LabP3 = TotManWO_LabP3 + ManWO_LabP3(i)
           TotManSO_LabP1 = TotManSO_LabP1 + ManSO_LabP1(i)
           TotManSO_LabP2 = TotManSO_LabP2 + ManSO_LabP2(i)
           TotManSO_LabP3 = TotManSO_LabP3 + ManSO_LabP3(i)
           TotManSI_ActP1 = TotManSI_ActP1 + ManSI_ActP1(i)          
           TotManSI_ActP2 = TotManSI_ActP2 + ManSI_ActP2(i)
           TotManSI_ActP3 = TotManSI_ActP3 + ManSI_ActP3(i)
           TotManSO_WI = TotManSO_WI + ManSO_WI(i)
           TotManSI_WI = TotManSI_WI + ManSI_WI(i)
           TotManWO_WI = TotManWO_WI + ManWO_WI(i)
           TotManSO_WO = TotManSO_WO + ManSO_WO(i)
           
       END DO
       
       CALL KgToKgHa(Totm,Totm,Areap)
       CALL KgToKgHa(TotManwip,TotManwip,Areap)
       CALL KgToKgHa(TotManwop,TotManwop,Areap)
       CALL KgToKgHa(TotMansop,TotMansop,Areap)
       CALL KgToKgHa(TotMansip,TotMansip,Areap)
       CALL KgToKgHa(TotManWI_LabP1,TotManWI_LabP1,Areap)
       CALL KgToKgHa(TotManWI_LabP2,TotManWI_LabP2,Areap)
       CALL KgToKgHa(TotManWI_LabP3,TotManWI_LabP3,Areap)
       CALL KgToKgHa(TotManWO_LabP1,TotManWO_LabP1,Areap)
       CALL KgToKgHa(TotManWO_LabP2,TotManWO_LabP2,Areap)
       CALL KgToKgHa(TotManWO_LabP3,TotManWO_LabP3,Areap)
       CALL KgToKgHa(TotManSO_LabP1,TotManSO_LabP1,Areap)
       CALL KgToKgHa(TotManSO_LabP2,TotManSO_LabP2,Areap)
       CALL KgToKgHa(TotManSO_LabP3,TotManSO_LabP3,Areap)
       CALL KgToKgHa(TotManSI_ActP1,TotManSI_ActP1,Areap)
       CALL KgToKgHa(TotManSI_ActP2,TotManSI_ActP2,Areap)
       CALL KgToKgHa(TotManSI_ActP3,TotManSI_ActP3,Areap)
       CALL KgToKgHa(TotManSO_WI,TotManSO_WI,Areap)
       CALL KgToKgHa(TotManSI_WI,TotManSI_WI,Areap)
       CALL KgToKgHa(TotManWO_WI,TotManWO_WI,Areap)
       CALL KgToKgHa(TotManSO_WO,TotManSO_WO,Areap)
       
       
       Totfrt =0.0
       TotAvfertp = 0.0
       TotResfertp = 0.0
       TotFertPLabP1 = 0.0
       TotFertPLabP2 = 0.0
       
       Do i =1,Slnf
           Totfrt = Totfrt + Avfertp(i) + Resfertp(i)
           TotAvfertp = TotAvfertp + Avfertp(i)
           TotResfertp = TotResfertp + Resfertp(i)
           TotFertPLabP1 = TotFertPLabP1 + FertPLabP1(i)
           TotFertPLabP2 = TotFertPLabP2 + FertPLabP2(i)
       END DO
       
       Mgwp = (Mdrpgw+Mppgw)/Areap
        
       CALL KgToKgHa(Totfrt,Totfrt,Areap)
       CALL KgToKgHa(TotAvfertp,TotAvfertp,Areap)
       CALL KgToKgHa(TotResfertp,TotResfertp,Areap)
       CALL KgToKgHa(TotFertPLabP1,TotFertPLabP1,Areap)
       CALL KgToKgHa(TotFertPLabP2,TotFertPLabP2,Areap)
       
       IF(Day == Sday .AND. Month == Smon .AND. Year == Syear) THEN
          AllSoilP = TotalsoilP + Totm + Totfrt +  (Inidrpmgw + 
     +        Inippmgw)/AreaP
       
       ELSE
          AllSoilP = TotalsoilP + Totm + Totfrt + Mgwp
      
       END IF
       
       END SUBROUTINE TOTALP
      