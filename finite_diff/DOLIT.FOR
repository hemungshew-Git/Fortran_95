* -------------------------------------------------------- * 
      PROGRAM DOLIT
* -------------------------------------------------------- *
* Main program                                             *
*     This program solves a system of linear equations     *
*     using Dolittle LU decomposition                      *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     LUD, SOLVE                                           *
* -------------------------------------------------------- *
* Variables used                                           *
*     N - System size                                      *
*     A - Coefficient matrix of the system                 *
*     B - Right side vector                                *
*     L - Lower triangular matrix                          *
*     U - Upper triangular matrix                          *
*     FACT - Factorisation status                          * 
* -------------------------------------------------------- *
* Constants used                                           *
*     YES,NO                                               *
* -------------------------------------------------------- *

      INTEGER N,YES,NO,FACT
      REAL A,U,L,B,X
      EXTERNAL LUD,SOLVE
      PARAMETER( YES = 1, NO = 0 )
      DIMENSION A(10,10),U(10,10),L(10,10),B(10),X(10)

      WRITE(*,*)
      WRITE(*,*) '            SOLUTION BY DOLITTLE METHOD '
      WRITE(*,*)
 
* Read input data

      WRITE(*,*) 'What is the size of A?'
      READ(*,*) N
      WRITE(*,*) 'Input coefficients a(i,j), row-wise, ',
     +           'one row on each line'
      DO 10 I = 1, N
         READ(*,*) (A(I,J), J=1,N)
10    CONTINUE
      WRITE(*,*) 'Input vector B on one line'
      READ(*,*) (B(I), I=1, N)

* LU factorisation

      CALL LUD(N,A,U,L,FACT)

      IF( FACT .EQ. YES ) THEN
*        Print LU matrices

* Print matrix U
         WRITE(*,*)
         WRITE(*,*) 'MATRIX U'
         DO 20 I = 1,N
            WRITE(*,111) (U(I,J),J=1,N)
20       CONTINUE

* Print matrix L
         WRITE(*,*)
         WRITE(*,*) 'MATRIX L'
         DO 30 I=1,N
            WRITE(*,111) (L(I,J),J=1,N)
30       CONTINUE

      ELSE
         WRITE(*,*)
         WRITE(*,*) 'FACTORISATION NOT POSSIBLE'
         WRITE(*,*)
         STOP

      ENDIF

* Solve for X

      CALL SOLVE(N,U,L,B,X)                     

      WRITE(*,*)
      WRITE(*,*) 'SOLUTION VECTOR X'
      WRITE(*,*)                                                                         
      WRITE(*,111) (X(I), I=1,N)
      WRITE(*,*) 

111   FORMAT(3F15.6)

      STOP
      END

* -------------- End of main program DOLIT --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE LUD(N,A,U,L,FACT) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine decomposes the matrix A into L and   *
*     U matrices using Dolittle algorithm                  * 
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - System size                                      *
*     A - Coefficient matrix of the original system        *
* Output                                               *
*     U - Decomposed upper triangular matrix               *
*     L - Decomposed lower triangular matrix               *
*     FACT - Fact about decomposition (yes or no)          *
* -------------------------------------------------------- *
* Local Variables                                          *
*     SUM                                                  *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER N,YES,NO,FACT
      REAL A,U,L,SUM
      PARAMETER( YES = 1, NO = 0 )
      DIMENSION A(10,10), U(10,10), L(10,10)

* Initialise U and L matrices

      DO 1 I = 1,N
         DO 1 J = 1,N
            U(I,J) = 0.0
            L(I,J) = 0.0
1     CONTINUE

* Compute the elements of U and L

      DO 10 J = 1,N
         U(1,J) = A(1,J)
10    CONTINUE

      DO 20 I = 1,N
         L(I,I) = 1.0
20    CONTINUE

      DO 30 I = 2,N
         L(I,1) = A(I,1)/U(1,1)
30    CONTINUE

      DO 100 J = 2,N

         DO 50 I = 2,J
            SUM = A(I,J)
            DO 40 K = 1,I-1
               SUM = SUM - L(I,K) * U(K,J)
40          CONTINUE
            U(I,J) = SUM
50       CONTINUE

         IF( U(J,J) .LE. 1.E-6 ) THEN
            FACT = NO
            RETURN
         ENDIF

         DO 70 I = J+1,N
            SUM = A(I,J)
            DO 60 K = 1,J-1
               SUM = SUM - L(I,K) * U(K,J)
60          CONTINUE
            L(I,J) = SUM/U(J,J)
70       CONTINUE

100   CONTINUE
 
      FACT = YES                                                                                                         
                  
      RETURN
      END
* ---------------- End of subroutine LUD ----------------- *
* -------------------------------------------------------- * 
      SUBROUTINE SOLVE(N,U,L,B,X)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine obtains the solution vector X        *
*     using the coefficients of L and U matrices           * 
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - System size                                      *
*     U - Upper triangular matrix                          *
*     L - Lower triangular matrix                          *
*     B - Right side vector                                *
* Output                                                   *
*     X - Solution vector                                  *
* -------------------------------------------------------- *
* Local Variables                                          *
*     SUM, Z(vector)                                       *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER N
      REAL U,L,SUM,B,X,Z 
      DIMENSION U(10,10),L(10,10),B(10),X(10),Z(10)

* Forward substitution                                     

      Z(1) = B(1) 
      DO 20 I = 2,N
         SUM = 0.0
         DO 10 J = 1,I-1
            SUM = SUM + L(I,J) * Z(J)
10       CONTINUE
      Z(I) = B(I) - SUM
20    CONTINUE

* Back substitution                                        

      X(N) = Z(N)/U(N,N)
      DO 40 I = N-1,1,-1
         SUM = 0.0 
         DO 30 J = I+1,N
            SUM = SUM + U(I,J) * X(J)
30       CONTINUE
         X(I) = (Z(I) - SUM)/U(I,I)
40    CONTINUE

      RETURN
      END
* --------------- End of subroutine SOLVE ---------------- *
