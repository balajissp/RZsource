C
      SUBROUTINE CONDUC(N,Z,RK,CON)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE CALCULATES THE CONDUCTANCE TERM BETWEEN TWO NODES
C     USING THE GEOMETRIC MEAN AND THE SPACE INCREMENT
C
C                                                     K(I)
C        K(I) = ( K(I)*K(I+1) )**1/2 =>  CON(I) = -------------
C                                                 Z(I+1) - Z(I)
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       CON       I/O   cond term between nodes I and I+1  K/(Z(I+1)-Z(I))
C       I          L    canopy, snow, residue or soil layer
C       N          I    node num starting from surface material
C
C
C       COMMENTS:
C
C       CALLED FROM:
C
C       PROGRAMMER:
C
C       VERSION:
C
C======================================================================
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION Z(N),RK(N),CON(N)
C
      DO 10 I=1,N-1
            CON(I)= DSQRT(RK(I)*RK(I+1))/(Z(I+1) - Z(I))
   10 CONTINUE
      RETURN
      END
