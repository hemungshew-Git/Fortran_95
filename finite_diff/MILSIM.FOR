* -------------------------------------------------------- * 
      PROGRAM MILSIM
* -------------------------------------------------------- *
* Main program                                             *
*     This program solves the first order differential     *
*     equation y' = f(x,y) using Milne-Simpson method      *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F,INT                                                *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X(1) - Initial value of independent variable         *
*     Y(1) - Initial value of dependent variable           *
*       XP - Point of solution                             *
*        N - Number of steps                               *
*        H - Step-size                                     *
*        X - Array of independent variable                 *
*        Y - Array of dependent variable                   *
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL X,Y,H,XP,M1,M2,M3,M4,SUM1,SUM2,F
      INTEGER N,INT
      INTRINSIC INT
      EXTERNAL F
      DIMENSION X(10), Y(10)

* Read values

      WRITE(*,*) 'Input initial values of x and y'
      READ(*,*) X(1),Y(1)
      WRITE(*,*) 'Input x at which y is required'
      READ(*,*) XP
      WRITE(*,*) 'Input step-size h'
      READ(*,*) H

* Compute number of computations involved

      N = INT((XP-X(1))/H+0.5)

* We need four starting points for Milne-Simpson method.
* Initial values form the first point.  Remaining three
* points are obtained using 4th order RK method
    
      WRITE(*,*)
      WRITE(*,*) 'INITIAL VALUES', X(1), Y(1)
      WRITE(*,*)

* Computing three points by RK method

      WRITE(*,*) 'THREE VALUES BY RK METHOD'

      DO 10 I = 1,3
         M1 = F(X(I),Y(I))   
         M2 = F(X(I)+0.5*H, Y(I)+0.5*M1*H)
         M3 = F(X(I)+0.5*H, Y(I)+0.5*M2*H)
         M4 = F(X(I)+H, Y(I)+M3*H)
         X(I+1) = X(I)+H
         Y(I+1) = Y(I)+(M1+2.0*M2+2.0*M3+M4)*H/6.0          
         WRITE(*,*) I, X(I+1), Y(I+1)
10    CONTINUE

      WRITE(*,*)   
      WRITE(*,*) 'VALUES OBTAINED BY MILNE-SIMPSON METHOD'

      DO 20 I = 4, N
         F2 = F(X(I-2), Y(I-2))
         F3 = F(X(I-1), Y(I-1))
         F4 = F(X(I), Y(I))

* Predicted value of y (by Milne's formula)
        
         Y(I+1) = Y(I-3)+4.0*H/3.0*(2.0*F2-F3+2.0*F4)
         X(I+1) = X(I) + H
         F5 = F(X(I+1), Y(I+1))

* Corrected value of Y (by Simpson's formula)

         Y(I+1) = Y(I-1) + H/3.0 *(F3+4.0*F4+F5)  

         WRITE(*,*) I, X(I+1), Y(I+1)
20    CONTINUE

      WRITE(*,*)
      WRITE(*,*)'Value of Y at X  =', X(N+1),'  is', Y(N+1)
      WRITE(*,*)


      STOP
      END

* ------------------ End of main MILSIM ------------------ *
* -------------------------------------------------------- *
* Function subprogram                                      *
* -------------------------------------------------------- *

      REAL FUNCTION F(X,Y)
      REAL X,Y
    
      F = 2.0 * Y/X

      RETURN
      END

* ------------------ End of function F(X,Y) -------------- *
