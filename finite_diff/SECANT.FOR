* -------------------------------------------------------- *
      PROGRAM SECANT
* -------------------------------------------------------- *
* Main program                                             *
*     This program finds a root of a non-linear            *
*     equation by secant method                            *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F                                                    *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     SEC                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     A - Left endpoint of interval                        *
*     B - Right endpoint of interval                       *
*     ROOT - Final solution                                *
*     COUNT - Number of iterations completed               *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
*     MAXIT - Maximum number of iterations permitted       *
* -------------------------------------------------------- *

      REAL A,B,ROOT,EPS,F
      INTEGER COUNT,STATUS, MAXIT
      EXTERNAL F,SEC
      PARAMETER(EPS = 0.000001, MAXIT = 50)

      WRITE(*,*)
      WRITE(*,*) '       SOLUTION BY SECANT METHOD'
      WRITE(*,*)

      WRITE(*,*) 'Input two starting points'
      READ(*,*) A,B

      CALL SEC(A,B,X1,X2,EPS,ROOT,COUNT,MAXIT,STATUS)

      IF( STATUS .EQ. 1 ) THEN
         WRITE(*,*)
         WRITE(*,*)'       DIVISION BY ZERO'
         WRITE(*,*)
         WRITE(*,*)' Last X1    =',X1
         WRITE(*,*)' Last X2    =',X2
         WRITE(*,*)' ITERATIONS =',COUNT
         WRITE(*,*)
      ELSE IF( STATUS .EQ. 2 ) THEN
         WRITE(*,*)
         WRITE(*,*) 'NO CONVERGENCE IN ',MAXIT,' ITERATIONS'
         WRITE(*,*)
      ELSE
         WRITE(*,*)
         WRITE(*,*) 'Root = ', ROOT
         WRITE(*,*) 'Function value at root = ',F(ROOT)
         WRITE(*,*)
         WRITE(*,*) 'Number of iterations = ',COUNT
         WRITE(*,*)
      ENDIF

      STOP
      END

* ----------------- End of main program ------------------ *
* -------------------------------------------------------- *
      SUBROUTINE SEC(A,B,X1,X2,EPS,ROOT,COUNT,MAXIT,STATUS)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine computes a root of an equation using *
*     the secant method                                    *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     A - Left end point                                   *
*     B - Right end point                                  *
*     EPS - Error bound                                    *
*     MAXIT - Maximum iterations allowed                   *
* Output                                                   *
*     X1 - New left point                                  *
*     X2 - New right point                                 *
*     ROOT - Final solution                                *
*     COUNT - Number of iterations done                    *
*     STATUS - Status of completion of the task            *
* -------------------------------------------------------- *
* Local Variables                                          *
*     X3,F1,F2,ERROR                                       *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F, ABS                                               *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,B,X1,X2,X3,EPS,ROOT,F1,F2,F,ABS,ERROR
      INTEGER COUNT,STATUS,MAXIT
      INTRINSIC ABS
      EXTERNAL F

* Function values at initial points
      X1 = A
      X2 = B
      F1 = F(A)
      F2 = F(B)

* Compute the root iteratively
      COUNT = 1

111   IF(ABS(F1-F2) .LE. 1.E-10) THEN
         STATUS = 1
         RETURN
      ENDIF

      X3 = X2-F2 *(X2-X1)/(F2-F1)
      ERROR = ABS((X3-X2)/X3)

* Test for accuracy
      IF(ERROR .GT. EPS) THEN

* ----Test for convergence
         IF( COUNT .EQ. MAXIT ) THEN
            STATUS = 2
            RETURN
         ENDIF

         X1 = X2
         X2 = X3
         F1 = F2
         F2 = F(X3)
         COUNT = COUNT + 1
         GO TO 111
*        and compute next approximation

      ENDIF

      ROOT = X3
      STATUS = 3

      RETURN
      END

* ---------------- End of subroutine SEC ----------------- *
* -------------------------------------------------------- *
* Function subprogram F(x)
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = X*X+X-2

      RETURN
      END

* ----------------- End of function F(X) ----------------- *

