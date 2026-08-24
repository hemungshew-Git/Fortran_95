* -------------------------------------------------------- * 
      PROGRAM MULTIR
* -------------------------------------------------------- *
* Main program                                             *
*     The program finds all the real roots of a polynomial *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NEWTON                                               *
*     DFLAT                                                *
* -------------------------------------------------------- *
* Variables used                                           *
*     N - Degree of polynomial                             *
*     A - Polynomial coefficients A(N+1)                   *
*     X0 - Initial guess                                   *
*     XR - Root obtained by Newton method                  *
*     ROOT - Root Vector                                   *
*     STATUS - Solution status                             *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
*     MAXIT - Maximum iterations permitted                 *
* -------------------------------------------------------- *

      REAL A,X0,XR,ROOT,EPS
      INTEGER N,MAXIT,STATUS
      PARAMETER( EPS=0.000001, MAXIT=50 )
      DIMENSION A(11), ROOT(10)

      WRITE(*,*)
      WRITE(*,*) '    EVALUATION OF MULTIPLE ROOTS     '
      WRITE(*,*)

      WRITE(*,*) 'Input N, the degree of polynomial'
      READ(*,*) N
      WRITE(*,*) 'Input poly coefficients, A(1) to A(N+1)'
      READ(*,*) (A(I), I=1, N+1)
      WRITE(*,*) 'Input initial guess X'
      READ(*,*) X0  
      WRITE(*,*)

      DO 200 I = N, 2, -1

*        Find I_th root 
         CALL NEWTON(N,A,X0,EPS,MAXIT,STATUS,XR)

         IF (STATUS .EQ. 2) THEN
            DO 100 J = N, I+1, -1
100            WRITE(*,*) 'ROOT',J,'  =', ROOT(J)
            WRITE(*,*) 'Next root does not converge in'
            WRITE(*,*) MAXIT, ' iterations'
            WRITE(*,*)
            STOP 
         ENDIF

         ROOT(I) = XR

*        Deflate the polynomial by division (X - XR)
         CALL DFLAT(N,A,XR)

         X0 = XR  
*        Proceed to find next root

200   CONTINUE

* Compute the last root  
      ROOT(1) = - A(1)/A(2)

* Write results
                    
      WRITE(*,*) 'ROOTS OF POLYNOMIAL ARE:'
      WRITE(*,*)
      DO 300 I = 1, N
         WRITE(*,*) 'ROOT',I,' =', ROOT(I) 
300   CONTINUE
      WRITE(*,*)

      STOP
      END

* -------------- End of main program MULTIR -------------- *
* -------------------------------------------------------- * 
      SUBROUTINE NEWTON(N,A,X0,EPS,MAXIT,STATUS,XR)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine finds a root of the polynomial       *
*     using the Newton-Raphson method                      *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Degree of polynomial                             *
*     A - Array of polynomial coefficients                 *
*     X0 - Initial guess for a root                        *
*     EPS - Error bound                                    *
*     MAXIT - Maximum iterations permitted                 *
* Output                                                   *
*     STATUS - Solution status                             *
*     XR - Root obtained by Newton method                  *
* -------------------------------------------------------- *
* Local Variables                                          *
*     COUNT - Number of iterations performed               *
*     FX - Value of polynomial function at X0              *
*     FDX - Value of function derivative at X0             *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,X0,EPS,XR,ABS
      INTEGER N,MAXIT,STATUS
      INTRINSIC ABS
      DIMENSION A(11) 

      COUNT = 1

* Compute the value of function at X0

100   FX = A(N+1)    
      DO 111 I = N, 1, -1
         FX = FX * X0 + A(I)
111   CONTINUE 

* Compute the value of derivative at X0

      FDX = A(N+1) * N
      DO 222 I = N, 2, -1
         FDX = FDX * X0 + A(I) * (I-1)
222   CONTINUE

* Compute a root XR

      XR = X0 - FX/FDX

* Test for accuracy
      IF(ABS((XR-X0)/XR).LE.EPS) THEN
         STATUS = 1
         RETURN
      ENDIF

* Test for convergence                               
      IF(COUNT .LT. MAXIT) THEN
         X0 = XR
         COUNT = COUNT + 1
         GOTO 100
      ELSE
         STATUS = 2
         RETURN
      ENDIF 

      END

* --------------- End of subroutine NEWTON --------------- * 
* -------------------------------------------------------- * 
      SUBROUTINE DFLAT(N,A,XR) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine reduces the degree of polynomial     *
*     by one using synthetic division                      *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Degree of polynomial                             *
*     A - Array of coefficients of input polynomial        *
*     XR - A root of the input polynomial                  *
* Output                                                   *
*     A - coefficients of the reduced polynomial           *                                                                                                                        
* -------------------------------------------------------- *
* Local Variables                                          *
*     B                                                    *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,B,XR
      INTEGER N
      DIMENSION A(11), B(11)

* Evaluate the coefficients of the reduced polynomial
                                      
      B(N+1) = 0     
      DO 1 I = N, 1, -1     
         B(I) = A(I+1) + XR * B(I+1)
1     CONTINUE 

* Change coefficients from B array to A array

      DO 2 I = 1, N+1 
         A(I) = B(I)
2     CONTINUE       
                                        
      RETURN       
      END

* --------------- End of subroutine DFLAT ---------------- *
