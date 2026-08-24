* -------------------------------------------------------- * 
      PROGRAM LEG2
* -------------------------------------------------------- *
* Main program                                             *
*     This program solves a system of linear equations     *
*     using Gaussian elimination with partial pivoting     *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     Gauss2                                               *
* -------------------------------------------------------- *
* Variables used                                           *
*     N - Number of equations                              *
*     A - Coefficients matrix                              *
*     B - Right side vector                                *
*     X - Solution vector                                  *
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,B,X
      INTEGER N
      EXTERNAL GAUSS2
      DIMENSION A(10,10), B(10), X(10)
     
      WRITE(*,*)
      WRITE(*,*) '       GAUSS METHOD WITH PARTIAL PIVOTING'
      WRITE(*,*)

      WRITE(*,*) 'What is the size of the system(n)?'
      READ(*,*) N     

      WRITE(*,*) 'Input coefficients a(i,j), row-wise'
      WRITE(*,*) 'one row on each line'
      DO 20 I = 1, N
         READ(*,*) (A(I,J),J=1,N)                                   
20    CONTINUE
      WRITE(*,*) 'Input vector b'
      READ(*,*) (B(I), I = 1, N)

      CALL GAUSS2(N,A,B,X) 

      WRITE(*,*)                   
      WRITE(*,*) 'SOLUTION VECTOR X'                   
      WRITE(*,*) 
      WRITE(*,*) (X(I), I = 1, N)  
      WRITE(*,*)

      STOP
      END

* --------------- End of main program LEG1 --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE GAUSS2(N,A,B,X) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine solves a system of linear equations  *
*     using Gauss elimination method with partial pivoting *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     A - Coefficient matrix                               *
*     B - Right Side vector                                *
*     N - Size of the system                               *
* Output                                                   *
*     X - Solution vector                                  *
* -------------------------------------------------------- *
* Local Variables                                          *
*     NIL                                                  *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     ELIM,BSUB                                            *
* -------------------------------------------------------- *

      REAL A,B,X
      INTEGER N
      EXTERNAL ELIM, BSUB
      DIMENSION A(10,10), B(10), X(10)

* Forward elimination
      
      CALL ELIM(N,A,B)

* Solution by back substitution

      CALL BSUB(N,A,B,X)

      RETURN
      END

* --------------- End of subroutine GAUSS2 --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE ELIM(N,A,B)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine performs forward elimination         *
*     incorporating partial pivoting technique             *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     A - Coefficient matrix                               *
*     B - Right side vector                                *
*     N - System size                                      *
* Output                                                   *
*     A - Modified A                                       *
*     B - Modified B                                       *
* -------------------------------------------------------- *
* Local Variables                                          *
*     FACTOR                                               *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     PIVOT                                                *
* -------------------------------------------------------- *

      REAL A,B,X,FACTOR
      INTEGER N
      EXTERNAL PIVOT
      DIMENSION A(10,10),B(10)
                                          
      DO 33 K = 1, N-1                                     
         CALL PIVOT (N,A,B,K)                                       
         DO 22 I = K+1, N
            FACTOR = A(I,K)/A(K,K)
            DO 11 J = K+1, N
               A(I,J) = A(I,J) - FACTOR * A(K,J)                                       
11          CONTINUE 
            B(I) = B(I) - FACTOR * B(K)
22       CONTINUE
33    CONTINUE

      RETURN
      END

* ---------------- End of subroutine ELIM ---------------- *
* -------------------------------------------------------- * 
      SUBROUTINE PIVOT(N,A,B,K)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine performs the task of partial         *
*     pivoting (reordering of equations)                   *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - System size                                      *
*     A - Coefficients matrix                              *
*     B - Right side vector                                *
*     K - Row under consideration for pivoting             *
* Output                                                   *
*     A - Modified A (after pivoting)                      *
*     B - Modified B (after pivoting)                      *
* -------------------------------------------------------- *
* Local Variables                                          *
*     LARGE,TEMP,P                                         *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL LARGE,TEMP,A,B
      INTEGER P,N,K
      INTRINSIC ABS
      DIMENSION A(10,10), B(10)
                                            
* Find pivot P

      P = K
      LARGE = ABS(A(K,K))
      DO 11 I = K+1, N
         IF(ABS(A(I,K)) .GT. LARGE) THEN
            LARGE = ABS(A(I,K))
            P = I
         ENDIF
11    CONTINUE

* Exchange rows P and K

      IF(P.NE.K) THEN

         DO 22 J = K,N
            TEMP = A(P,J)
            A(P,J) = A(K,J)
            A(K,J) = TEMP
22       CONTINUE

         TEMP = B(P)
         B(P) = B(K)
         B(K) = TEMP

      ENDIF
                                                                   
      RETURN
      END
* --------------- End of subroutine PIVOT ---------------- *
* -------------------------------------------------------- * 
      SUBROUTINE BSUB(N,A,B,X) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine obtains the solution vector X        *
*     by back substitution                                 *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - System size                                      *
*     A - Coefficient matrix (after elimination)           *
*     B - Right side vector (after elimination)            *
* Output                                                   *
*     X - Solution vector                                  *
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

      INTEGER N
      REAL A,B,X, SUM
      DIMENSION A(10,10), B(10), X(10)
      
      X(N) = B(N)/A(N,N)
      DO 55 K = N-1, 1, -1
         SUM = 0 
         DO 44 J = K+1, N 
            SUM = SUM + A(K,J) * X(J)
44       CONTINUE                                                                   
      X(K) = (B(K) - SUM)/A(K,K)
55    CONTINUE

      RETURN
      END
* --------------- End of subroutine BSUB ----------------- *
