      SUBROUTINE WRITEOUTPUT(ID,IM,IYYY)
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
C 1.  28/08/2016       D SADHUKHAN          ORIGNIAL CODE
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
       INTEGER :: i
       CHARACTER(LEN=100)::String,N
       INTEGER,INTENT(IN) :: ID,IM,IYYY
       INTEGER :: Day,Month,Year
       DAY = ID
       Month = IM
       Year = IYYY
       
       WRITE(115,'(/,A,/)')'Soil P pools of the Soil layer'
       
       DO i=1,Nsoil
        
        WRITE(115,'(A,I2,/)')'Soil Layer No', i
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
       
      
       
       i = Nsoil*5   
       WRITE(N,'(I3)') i
       String = "(3I6,2X,"//TRIM(N)//"(F12.5,2X),4F14.5)"
       WRITE(112,String)Day,Month,Year,(Labp(i),i=1,Nsoil),
     + (Actp(i),i=1,Nsoil),(Stabip(i),i=1,Nsoil),(Stabop(i),i=1,Nsoil),
     + (Frsop(i),i=1,Nsoil),TotalsoilP,Totm,Totfrt,Mgwp
       
       
       Write(N,'(I3)') Nsoil
       String = "(3I6,2X,12(F12.5,2X),"//TRIM(N)//"(F12.5,2X))"
       WRITE(113,String) Day,Month,Year,Labpmlossrnf*1000,
     +     Fertpmlossrnf*1000, Manpmlossrnf*1000,Drplossrnf*1000,
     +     PPlossrnf*1000,Drplosstdrain*1000, Drpmactdrain*1000, 
     +     PPlosstdrain*1000,Drplosslatflow*1000,PPlosslatflow*1000,
     +     Drplossdesp*1000,PPlossdesp*1000,
     +  (Actpuptake(i)*1000,i=1,Nsoil)  
       
       
       i= Nsoil*8+18
       Write(N,'(I3)') i
       String = '(3I6,2X,'//TRIM(N)//'(F20.5,2X))'
       WRITE(114,String) Day,Month,Year,(Pflowactlab(i)*1000,i=1,Nsoil),
     + (Pflowlabact(i)*1000,i=1,Nsoil),(Pflowactstbi(i)*1000,i=1,Nsoil),
     + (Pflowstbiact(i)*1000,i=1,Nsoil),
     + (Pflowfrsolab(i)*1000,i=1,Nsoil),
     + (Pflowfrsostbo(i)*1000,i=1,Nsoil),
     + (Pflowlabfrso(i)*1000,i=1,Nsoil),(Pflowstbolab(i)*1000,i=1,Nsoil)
     + ,TotFertPLabP1*1000,TotFertPLabP2*1000,TotManSO_WI*1000,
     +  TotManSI_WI*1000,TotManWO_WI*1000,TotManSO_WO*1000,
     + TotManWI_LabP1*1000,TotManWI_LabP2*1000,TotManWI_LabP3*1000,  
     + TotManWO_LabP1*1000,TotManWO_LabP2*1000,TotManWO_LabP3*1000,
     + TotManSO_LabP1*1000,TotManSO_LabP2*1000,TotManSO_LabP3*1000,
     + TotManSI_ActP1*1000,TotManSI_ActP2*1000,TotManSI_ActP3*1000
       
       
       
       ADDPLANTP = 0.0
       DO i= 1,Nsoil   
          ADDPLANTP = ADDPLANTP + Actpuptake(i)*1000.0    
       END DO
       
       
       String ="(3I6,8(F22.5,2X))"
       WRITE(116,String) Day,Month,Year,TotAvfertp,
     + TotResfertp,TotManwop,TotManwip,TotMansop,TotMansip,Totfrt,Totm 

         
       END SUBROUTINE WRITEOUTPUT