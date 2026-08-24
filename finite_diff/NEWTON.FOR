* -------------------------------------------------------- *
      PROGRAM NEWTON
* -------------------------------------------------------- *
* Main program                                             *
*     This program finds a root of a non-linear equation   *
*     by Newton-Raphson method                             *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS,F,FD                                             *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X0 - Initial vale of x                               *
*     XN - New value of x                                  *
*     FX - Function value at x                             *
*     FDX - Value of function derivative at x              *
*     COUNT - Number of iterations done                    *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
*     MAXIT - Maximum number of iterations permitted       *
* -------------------------------------------------------- *

      REAL X0,XN,FX,FDX,ABS,EPS,F,FD
      INTEGER COUNT, MAXIT
      INTRINSIC ABS
      EXTERNAL F,FD
      PARAMETER(EPS = 0.000001, MAXIT = 100)

      WRITE(*,*) 'Input initial value of x'
      READ(*,*) X0

      WRITE(*,*)
      WRITE(*,*) '       SOLUTION BY NEWTON-RAPHSON METHOD'
      WRITE(*,*)

      COUNT = 1
100   FX = F(X0)
      FDX = FD(X0)
      XN = X0 -FX/FDX
      IF(ABS((XN-X0)/XN) .LT.EPS) THEN
         WRITE(*,*) 'Root = ', XN
         WRITE(*,*) 'Function value = ', F(XN)
         WRITE(*,*) 'Number of iterations = ', COUNT
         WRITE(*,*)
      ELSE
         X0 = XN
         COUNT = COUNT + 1
         IF(COUNT.LE.MAXIT) THEN
            GO TO 100
         ELSE
            WRITE(*,*)
            WRITE(*,*) 'SOLUTION DOES NOT CONVERGE IN'
            WRITE(*,*)  MAXIT, ' ITERATIONS'
            WRITE(*,*)
         ENDIF
      ENDIF

      STOP
      END

* ----------------- End of main program ------------------ *
* -------------------------------------------------------- *
* Function subprogram F(x)
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = X*X+X-2

      RETURN
      END

* ----------------- End of function F(X) ----------------- *
* -------------------------------------------------------- *
* Function subprogram FD(x)
* -------------------------------------------------------- *

      REAL FUNCTION FD(X)
      REAL X

      FD = 2*X+1

      RETURN
      END

* ----------------- End of function FD(X)----------------- *
