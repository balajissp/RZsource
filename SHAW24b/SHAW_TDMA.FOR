      SUBROUTINE TDMA(N,A,B,C,D,X)
C
C======================================================================
C
C       PURPOSE:
C     THIS SUBROUTINE SOLVES A TRI-DIAGONAL MATRIX OF THE FORM :
C
C              | B1  C1   0   0  .  .   . | |X1|   |D1|
C              | A2  B2  C2   0  .  .   . | |X2|   |D2|
C              |  0  A3  B3  C3  .  .   . | |X3| = |D3|
C              |  .   .   .   .     .   . | | .|   | .|
C              |  0   0   0   0  . BN  AN | |XN|   |DN|
C
C       REF:
C
C       VARIABLE DEFINITIONS:
C       VARIABLE  I/O   DESCRIPTION
C       --------  ---   -----------
C       A          I    a coeff vector
C       B         I/O   b coeff vector
C       C         I/O   c coeff vector
C       D         I/O   d coeff vector
C       N          I    node num starting from surface material
C       X         I/O   solution vector
C
C       COMMENTS:
C
C       EXTERNAL REFERENCES:
C                       NONE
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
      DIMENSION A(N),B(N),C(N),D(N),X(N)
C
      DO 10 I=2,N
         C(I-1)=C(I-1)/B(I-1)
         D(I-1)=D(I-1)/B(I-1)
         B(I)=B(I)-A(I)*C(I-1)
         D(I)=D(I)-A(I)*D(I-1)
   10 CONTINUE
      X(N)=D(N)/B(N)
      DO 20 I=N-1,1,-1
         X(I)=D(I)-C(I)*X(I+1)
   20 CONTINUE
      RETURN
      END
