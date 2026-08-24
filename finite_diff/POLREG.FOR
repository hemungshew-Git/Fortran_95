* -------------------------------------------------------- * 
      PROGRAM POLREG
* -------------------------------------------------------- *
* Main program                                             *
*     This program fits a polynomial curve to a given      *
*     set of data points by the method of least squares          *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NORMAL, GAUSS                                        *
* -------------------------------------------------------- *
* Variables used                                           *
*     X,Y - Arrays of data values                          *
*     N - Number of data points                            *
*     MP - Order of the polynomial under construction      *
*     M - Number of polynomial coefficients                *
*     C - Coefficient matrix of normal equations           *
*     B - Right side vector of normal equations            *
*     A - Array of coefficients of the polynomial          *
* -------------------------------------------------------- *
* Constants used                                           *
*     MAX - Maximum number of data points                  *
* -------------------------------------------------------- *

      REAL X,Y,C,A,B               
      INTEGER N,MP,M,MAX
      PARAMETER (MAX = 10)
      DIMENSION X(MAX),Y(MAX),C(MAX,MAX),A(MAX),B(MAX)

      WRITE(*,*)
      WRITE(*,*) '             POLYNOMIAL REGRESSION'
      WRITE(*,*) 

* Reading data values

      WRITE(*,*) 'Input number of data points(N)'
      READ(*,*) N 
      WRITE(*,*) 'Input order of polynomial(MP) required'
      READ(*,*) MP
      WRITE(*,*) 'Input data values X and Y,',
     +           'one set on each line'

      DO 10 I = 1, N
         READ(*,*) X(I), Y(I)
10    CONTINUE

* Testing the order

      IF(N.LE.MP) THEN
         WRITE(*,*) 'REGRESSION IS NOT POSSIBLE'
         GO TO 20
      ENDIF
                          
* Number of polynomial coefficients

      M = MP+1

* Computation of elements of C and B

      CALL NORMAL(X,Y,C,B,N,M,MAX)

* Computation of coefficients a(1) to a(m)

      CALL GAUSS(M,C,B,A)

* Output of coefficients a(1) to a(m)

      WRITE(*,*)
      WRITE(*,*) 'POLYNOMIAL COEFFICIENTS'
      WRITE(*,*)
      WRITE(*,*) (A(I), I=1,M)
      WRITE(*,*)

20    STOP
      END
* ------------- End of main program POLREG --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE NORMAL(X,Y,C,B,N,M,MAX)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine computes the coefficients            *
*     of normal equations                                  *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Number of data points                            *
*     X,Y - Arrays of data values                          *
*     M - Number of coefficients of the polynomial         *
*     MAX - Maximum size of arrays                         *
* Output                                                   *
*     C - Coefficient matrix of normal equations           *
*     B - Right side vector of normal equations            *
* -------------------------------------------------------- *
* Local Variables                                          *
*     NIL                                                  *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *
      REAL  X,Y,C,B
      INTEGER  N,M,MAX
      DIMENSION X(MAX),Y(MAX),C(MAX,MAX),B(MAX)

      DO 30 J=1,M
         DO 20 K=1,M
            C(J,K) = 0.0
            L1 = K+J-2
            DO 10 I=1,N
               C(J,K) = C(J,K) + X(I) ** L1
10          CONTINUE
20       CONTINUE
30    CONTINUE

      DO 50 J= 1,M
         B(J) = 0.0
         L2 = J-1
         DO 40 I = 1,N
            B(J) = B(J) + Y(I) * X(I) ** L2
40       CONTINUE
50    CONTINUE

      RETURN
      END

* --------------- End of subroutine NORMAL --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE GAUSS(N,A,B,X) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine solves a set of n linear             *
*     equations by Gauss elimination method                *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Number of equations                              *
*     A - Matrix of coefficients                           *
*     B - Right side vector                                *
* Output                                                   *
*     X - Solution vector                                  *
* -------------------------------------------------------- *
* Local Variables                                          *
*     PIVOT, FACTOR, SUM                                   *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *
      INTEGER N
      REAL A,B,X, PIVOT,FACTOR,SUM
      DIMENSION A(10,10), B(10), X(10)

* ------------------ Elimination begins ------------------ *

      DO 33 K = 1, N-1
         PIVOT = A(K,K)
         DO 22 I = K+1, N
            FACTOR = A(I,K)/PIVOT
            DO 11 J = K+1, N
               A(I,J) = A(I,J) - FACTOR * A(K,J)
11          CONTINUE 
            B(I) = B(I) - FACTOR * B(K)
22       CONTINUE
33    CONTINUE

* --------------- Back substitution begins --------------- *

      X(N) = B(N)/A(N,N)
      DO 55 K = N-1,1,-1
         SUM = 0
         DO 44 J = K+1,N
            SUM = SUM + A(K,J) * X(J)
44       CONTINUE 
         X(K) = (B(K) - SUM)/A(K,K)
55    CONTINUE

      RETURN
      END

* --------------- End of subroutine GAUSS ---------------- *
