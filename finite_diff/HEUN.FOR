* -------------------------------------------------------- * 
      PROGRAM HEUN 
* -------------------------------------------------------- *
* Main program                                             *
*     This program solves the first order differential     *
*     equation y' = f(x, y) using the Heun's method        *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F,INT                                                *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X - Initial value of independent variable            *
*     Y - Initial value of dependent variable              *
*    XP - Point of solution                                *
*     H - Step-size                                        *
*     N - Number of steps                                  *
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL X,Y,XP,H,M1,M2,F
      INTEGER N,INT
      INTRINSIC INT
      EXTERNAL F

      WRITE(*,*)
      WRITE(*,*)'            SOLUTION BY HEUNS METHOD'
      WRITE(*,*)

* Input values

      WRITE(*,*) 'Input initial values of x and y'
      READ(*,*) X,Y
      WRITE(*,*) 'Input x at which y is required'
      READ(*,*) XP
      WRITE(*,*) 'Input step-size h'
      READ(*,*) H

* Compute number of steps required 

      N = INT((XP-X)/H+0.5)

* Compute Y recursively at each step

      DO 20 I = 1,N
         M1 = F(X,Y)   
         M2 = F(X+H,Y+M1*H)
         X = X+H
         Y = Y+0.5*H*(M1+M2)
         WRITE(*,*) I,X,Y
20    CONTINUE

* Write the final result

      WRITE(*,*)
      WRITE(*,*)'Value of Y at X  =', X, '  is', Y
      WRITE(*,*)

      STOP
      END

* ------------------ End of main HEUN -------------------- *
* -------------------------------------------------------- *
* Function subprogram                                      *
* -------------------------------------------------------- *

      REAL FUNCTION F(X,Y)
      REAL X,Y
    
      F = 2.0 * Y/X

      RETURN
      END

* ------------------ End of function F(X,Y) -------------- *
