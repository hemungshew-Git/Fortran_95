* -------------------------------------------------------- * 
      PROGRAM ROMBRG 
* -------------------------------------------------------- *
* Main program                                             *
*     This program performs Romberg integration            *
*     by bisecting the intervals N times                   *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F,ABS                                                *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     A - Starting point of the interval                   *
*     B - End point of the interval                        *
*     H - Width of the interval                            *
*     N - Number of times bisection is done                *
*     M - Number of trapezoids                             *  
*     R - Matrix of Romberg integral values                * 
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
* -------------------------------------------------------- *

      REAL A,B,EPS,H,R,SUM,F,X,ABS
      INTEGER N,M     
      INTRINSIC ABS
      EXTERNAL F                        
      PARAMETER( EPS = 0.00001 )
      DIMENSION R(10,10)

      WRITE(*,*) 'Input endpoints of the interval'
      READ(*,*) A,B
      WRITE(*,*) 'Input maximum number of times'
      WRITE(*,*) 'the subintervals are bisected'
      READ(*,*) N

* Compute using entire interval as one trapezoid             
      H = B-A
      R(1,1) = H * (F(A) + F(B))/2.0
      WRITE(*,*)
      WRITE(*,*) R(1,1)

      DO 30 I = 2, N+1

*          Determine number of trapezoids for I_th refinement
         M = 2**(I-2)
*          Reduce step size for I_th refinement                         
         H = H/2
      
*     Use recursive trapezoidal rule for M strips         
         SUM = 0.0
         DO 10 K = 1,M
            X = A+(2*K-1)*H
            SUM = SUM + F(X)
            R(I,1) = R(I-1,1)/2.0+H*SUM
10       CONTINUE

*     Compute Richardson's improvements
         DO 20 L = 2,I
            R(I,L) = R(I,L-1)+(R(I,L-1) - R(I-1,L-1))/(4**L-1)
20       CONTINUE

*     Write the results of improvements for I_th refinement
         WRITE(*,*) (R(I,L),L = 1,I)

*     Test for desired accuracy
         IF(ABS(R(I-1,I-1) - R(I,I)) .LE. EPS) THEN
*           Stop further refinement
            WRITE(*,*)
            WRITE(*,*) 'ROMBERG INTEGRATION =', R(I,I)
            WRITE(*,*)
            GOTO 40
         ENDIF

*     Continue with the refinement process
30    CONTINUE

* write the final result

      WRITE(*,*)
      WRITE(*,*) 'ROMBERG INTEGRATION = ', R(N+1,N+1)
      WRITE(*,*) '(Exit from loop)'
      WRITE(*,*)

40    STOP
      END

* ----------------- End of main ROMBRG ------------------- *
* -------------------------------------------------------- * 
* Function subprogram F(X)                                 *
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = 1.0/X   

      RETURN
      END

* ----------------- End of function F(X) ----------------- *
