* -------------------------------------------------------- * 
      PROGRAM RUNGE4    
* -------------------------------------------------------- *
* Main program                                             *
*     This program computes the solution of first order    *
*     differential equation of type  y' = f(x,y)  using    *
*     the 4th order Runge-Kutta method                     *
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

      REAL X,Y,XP,H,M1,M2,M3,M4,F
      INTEGER N,INT
      INTRINSIC INT
      EXTERNAL F

      WRITE(*,*)
      WRITE(*,*)'        SOLUTION BY 4_TH ORDER R-K METHOD'
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

* Compute Y at each step

      WRITE(*,*)'      -----------------------------------------'
      WRITE(*,*)'        STEP           X                 Y     '
      WRITE(*,*)'      -----------------------------------------'
      DO 40 I = 1,N
         M1 = F(X,Y)   
         M2 = F(X+0.5*H,Y+0.5*M1*H)
         M3 = F(X+0.5*H,Y+0.5*M2*H)
         M4 = F(X+H,Y+M3*H)
         X = X+H 
         Y = Y+(M1+2.0*M2+2.0*M3+M4)*H/6.0
         WRITE(*,*) I,X,Y
40    CONTINUE
      WRITE(*,*)'      -----------------------------------------'

* Write the final value of Y

      WRITE(*,*)
      WRITE(*,*)'Value of Y at X  =', '  is', Y
      WRITE(*,*)
     
      STOP
      END

* ----------------- End of main RUNGE4 ------------------- *
* -------------------------------------------------------- *
* Function subprogram                                      *
* -------------------------------------------------------- *

      REAL FUNCTION F(X,Y)
      REAL X,Y
    
      F = 2.0 * Y/X

      RETURN
      END

* ------------------ End of function F(X,Y) -------------- *
