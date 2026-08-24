* -------------------------------------------------------- * 
      PROGRAM POLY  
* -------------------------------------------------------- *
* Main program                                             *
*     Program POLY evaluates a polynomial of degree n at   *
*     any point X using Horner's rule                      *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     HORNER                                               *
* -------------------------------------------------------- *
* Variables used                                           *
*     N - Degree of polynomial                             *
*     A - Array of polynomial coefficients                 *
*     X - Point of evaluation                              *
*     P - Value of polynomial at X                         *
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER N
      REAL A, X, P
      EXTERNAL HORNER
      DIMENSION A(10) 

      WRITE(*,*) 'Input degree of polynomial, N'
      READ(*,*) N 

      WRITE(*,*) 'Input polynomial coefficients ( A(0) to A(N) )'
      DO 100 I = 1, N+1
         READ(*,*) A(I)
100   CONTINUE

      WRITE(*,*) 'Input value of X (point of evaluation)'
      READ(*,*) X

* Evaluating polynomial at X using Horner's method

      CALL HORNER( N,A,X,P )

* Writing the result

      WRITE(*,*)
      WRITE(*,*) 'F(X)  = ', P, '   at X  = ', X
      WRITE(*,*)

      STOP
      END
* ---------------- End of main program POLY -------------- *
* -------------------------------------------------------- *
      SUBROUTINE HORNER( N,A,X,P )                           
* -------------------------------------------------------- *
* Subroutine                                               *
*     HORNER computes the value of a polynomial of order   *
*     n at any given point x.                              *  
* ---------------------------------------------------------*
* Arguments                                                *
* Input                                                    *  
*     N - Degree of polynomial                             *
*     A - Polynomial coefficients (array of size N+1)      *
*     X - Point of interest of evaluation                  *
* Output                                                   *
*     P - Value of polynomial at X                         *
* ---------------------------------------------------------*
* Local Variables                                          *
*     NIL                                                  *
* ---------------------------------------------------------*
* Functions invoked                                        *
*     NIL                                                  *
* ---------------------------------------------------------*
* Subroutines called                                       *
*     NIL                                                  *
* ---------------------------------------------------------*
      REAL A, X, P
      INTEGER N
      DIMENSION A(10)

      P = A(N+1)
      DO 111 I = N, 1, -1
         P = P*X + A(I)
111   CONTINUE

      RETURN
      END
* --------------- End of Subroutine HORNER --------------- *
