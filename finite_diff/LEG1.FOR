*--------------------------------------------------------- * 
      PROGRAM LEG1
* -------------------------------------------------------- *
* Main program                                             *
*     This program solves a system of linear equations     *
*     using simple Gaussian elimination method             *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     GAUSS1                                               *
* -------------------------------------------------------- *
* Variables used                                           *
*     N - Number of equations in the system                *
*     A - Matrix of coefficients                           *
*     B - Right side vector                                *
*     X - Solution vector                                  *
* -------------------------------------------------------- *
* Constants used                                           *
*     STATUS - Solution status                             *
* -------------------------------------------------------- *
      REAL A,B,X
      INTEGER STATUS,N
      EXTERNAL GAUSS1  
      DIMENSION A(10,10), B(10), X(10)

      WRITE(*,*)
      WRITE(*,*) '          SOLUTION BY SIMPLE GAUSS METHOD'
      WRITE(*,*)

      WRITE(*,*) 'What is the size of the system(n)?'
      READ(*,*) N     
      WRITE(*,*) 'Input coefficients a(i,j), row-wise, ',
     +           'one row on each line'
      DO 20 I = 1, N
         READ(*,*) (A(I,J),J=1,N)
20    CONTINUE

      WRITE(*,*) 'Input vector b'
      READ(*,*) (B(I), I = 1, N)

      CALL GAUSS1(N,A,B,X,STATUS)

      IF(STATUS .NE. 0) THEN
         WRITE(*,*)                   
         WRITE(*,*) 'SOLUTION VECTOR X'
         WRITE(*,*) 
         WRITE(*,*) (X(I), I = 1, N)  
         WRITE(*,*)
      ELSE
         WRITE(*,*)
         WRITE(*,*) 'SINGULAR MATRIX, NO SOLUTION'
         WRITE(*,*) 'REORDER EQUATIONS'
         WRITE(*,*)
      ENDIF
   
      STOP
      END

* --------------- End of main program LEG1 --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE GAUSS1(N,A,B,X,STATUS) 
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
*     STATUS - Solution status                             *  
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

      REAL A,B,X,PIVOT,FACTOR,SUM
      INTEGER STATUS,N
      DIMENSION A(10,10), B(10), X(10)

* ------------------ Elimination begins ------------------ *

      DO 33 K = 1, N-1
         PIVOT = A(K,K)
         IF(PIVOT .LT. 0.000001) THEN
            STATUS = 0                                                 
            RETURN
         ENDIF
         STATUS = 1 
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

* --------------- End of subroutine GAUSS1 --------------- *
