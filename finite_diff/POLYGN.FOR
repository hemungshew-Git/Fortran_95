* -------------------------------------------------------- * 
      PROGRAM POLYGN
* -------------------------------------------------------- *
* Main program                                             *
*     This program solves the differential equation        *
*     of type y' = f(x, y) by polygon method               *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F,INT                                                *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X - Initial value of the independent variable        *
*     Y - Initial value of the dependent variable          *
*    XP - Point of solution                                *
*     H - Incremental step-size                            *
*     N - Number of computational steps required           *    
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL X,Y,XP,H,M1,M2,F
      INTEGER N,INT
      INTRINSIC INT
      EXTERNAL F

      WRITE(*,*)
      WRITE(*,*)'            SOLUTION BY POLYGON METHOD'
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

* compute Y at each step

      DO 30 I = 1,N
         M1 = F(X,Y)   
         M2 = F(X+0.5*H,Y+0.5*H*M1)
         X = X+H 
         Y = Y+M2*H
         WRITE(*,*) I,X,Y
30    CONTINUE

* Write the final value of Y

      WRITE(*,*)
      WRITE(*,*) 'Value of Y at X  =', X, '  is', Y
      WRITE(*,*)

      STOP
      END

* ----------------- End of main POLYGN ------------------- *
* -------------------------------------------------------- *
* Function subprogram                                      *
* -------------------------------------------------------- *

      REAL FUNCTION F(X,Y)
      REAL X,Y
    
      F = 2.0 * Y/X

      RETURN
      END

* ------------------ End of function F(X,Y) -------------- *
