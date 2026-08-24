* -------------------------------------------------------- * 
      PROGRAM SPLINE
* -------------------------------------------------------- *
* Main program                                             *
*     This program computes the interpolation value at     *
*     a specified value, given a set of table  points,     *
*     using the natural cubic spline interpolation         *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     GAUSS                                                *
* -------------------------------------------------------- *
* Variables used                                           *
*     N  - Number of data points.                          *
*     X  - N by 1 array of data points.                    *
*     F  - N by 1 array of function values                 *
*     XP - Point at which interpolation is required        *
*     FP - Interpolation value at XP                       *
*     A  - Array of second derivatives (N-2 by 1)          *
*     D  - Array representing right side of (9.32)         *
*          (N-2 by 1)                                      *
*     C  - Matrix (N-2 by N-2) representing the            *
*          coefficients of second derivatives              *
*     H  - Array of distances between data points          *
*          ( h(i) = x(i) - x(i-1) )                        *
*     DF - Array of differences of functions               *
* -------------------------------------------------------- *
* Constants used                                           *
*     MAX - Maximum number of table points permitted       *
* -------------------------------------------------------- *

      INTEGER N,MAX
      REAL XP,FP,X,F,A,D,C,H,DF,U  
      PARAMETER (MAX=10)
      DIMENSION X(MAX),F(MAX),A(MAX),D(MAX),C(MAX,MAX),
     +          H(MAX),DF(MAX),U(MAX)

* Read input data

      WRITE(*,*) 'Input number of data points n' 
      READ(*,*) N 
      WRITE(*,*) 'Input data points X(I) and function'
      WRITE(*,*) 'values F(I), one set in each line'
      DO 5 I = 1,N   
         READ(*,*) X(I), F(I)
5     CONTINUE

      WRITE(*,*) 'Input XP'
      READ(*,*) XP

* Compute distances between data points
* and function differences
 
      DO 10 I = 2,N
         H(I) = X(I) - X(I-1)
         DF(I) = F(I) - F(I-1)
10    CONTINUE

* Initialise C matrix

      DO 30 I = 2,N-1
         DO 20 J = 2, N-1
            C(I,J) = 0.0 
20       CONTINUE
30    CONTINUE

* Compute diagonal elements of C 
      
      DO 40 I = 2,N-1
         C(I,I) = 2.0 * (H(I)+H(I+1))
40    CONTINUE

* Compute off_diagonal elements of C

      DO 50 I = 3,N-1
         C(I-1,I) = H(I)
         C(I,I-1) = H(I)
50    CONTINUE

* Compute elements of D array

      DO 60 I = 2,N-1
         D(I) = (DF(I+1)/H(I+1) - DF(I)/H(I)) * 6.0
60    CONTINUE

* Compute elements of A using Gaussian elimination
* Change array subscripts from 2 to n-1 to 1 to n-1 
* before calling GAUSS

      M = N-2
      DO 80 I = 1,M
         D(I) = D(I+1)
         DO 70 J = 1,M
            C(I,J) = C(I+1,J+1)
70       CONTINUE
80    CONTINUE

      CALL GAUSS(M,C,D,A)

* Compute the coefficients of natural cubic spline

      DO 90 I = N-1,2,-1
         A(I) = A(I-1)
90    CONTINUE
      A(1) = 0.0
      A(N) = 0.0
                                
* Locate the domain of XP

      I = 2
100   IF( XP .LE. X(I) ) GO TO 110
      I = I+1
      GO TO 100

* Compute interpolation value at XP
* Use equation (9.30)

110   U(I-1) = XP - X(I-1)
      U(I) = XP - X(I)
      Q1 = H(I)**2 * U(I) - U(I)**3
      Q2 = U(I-1)**3 - H(I)**2 * U(I-1)
      Q3 = F(I) * U(I-1) - F(I-1) * U(I)
      FP = (A(I-1) * Q1 + A(I) * Q2)/(6.0 * H(I)) + Q3/H(I)

* Write results

      WRITE(*,*)
      WRITE(*,*) 'SPLINE INTERPOLATION'
      WRITE(*,*)
      WRITE(*,*) 'Interpolation value  =',FP
      WRITE(*,*) 

      STOP
      END
* ----------------- End of main SPLINE ------------------- *
* -------------------------------------------------------- * 
      SUBROUTINE GAUSS(N,A,B,X) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine solves a set of n linear             *
*     equations using Gauss elimination method             *
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
      REAL A,B,X,PIVOT,FACTOR,SUM
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
