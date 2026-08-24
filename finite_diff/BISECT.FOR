* -------------------------------------------------------- * 
      PROGRAM BISECT
* -------------------------------------------------------- *
* Main program                                             *
*     This program finds a root of a non-linear equation   *
*     using the bisection method                           *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F                                                    *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     BIM                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     A - Left endpoint of interval                        *
*     B - Right endpoint of interval                       *
*     S - Status                                           *
*     ROOT - Final Solution                                *
*     COUNT - Number of Iterations done                    *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
* -------------------------------------------------------- *

      REAL A,B,ROOT,EPS,F
      INTEGER S, COUNT
      EXTERNAL BIM,F 
      PARAMETER(EPS=0.000001)

      WRITE(*,*)
      WRITE(*,*) '       SOLUTION BY BISECTION METHOD'
      WRITE(*,*)

      WRITE(*,*) 'Input starting values'
      READ(*,*) A,B

      CALL BIM(A,B,EPS,S,ROOT,COUNT)                          
         
      IF(S.EQ.0) THEN
         WRITE(*,*)
         WRITE(*,*) 'Starting points do not bracket any root'
         WRITE(*,*) '(Check whether they bracket EVEN roots)'
         WRITE(*,*)
      ELSE 
         WRITE(*,*)
         WRITE(*,*) 'Root       = ', ROOT
         WRITE(*,*) 'F(Root)    = ', F(ROOT)
         WRITE(*,*)
         WRITE(*,*) 'ITERATIONS = ', COUNT
         WRITE(*,*)
      ENDIF                               

      STOP
      END

* ----------------- End of main program ------------------ *

* -------------------------------------------------------- * 
      SUBROUTINE BIM(A,B,EPS,S,ROOT,COUNT)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine finds a root of non-linear equation  *
*     in the interval [A,B] using the bisection method     *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     A - Left endpoint of interval                        *
*     B - Right endpoint of interval                       *
*     EPS - Error bound                                    *
* Output                                                   *
*     S - Status                                           *
*     ROOT - Final Solution                                *
*     COUNT - Number of Iterations                         *
* -------------------------------------------------------- *
* Local Variables                                          *
*     X1,X2,X0,F0,F1,F2                                    *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F,ABS                                                *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,B,ROOT,EPS,F,X1,X2,X0,F0,F1,F2,ABS
      INTEGER S,COUNT
      EXTERNAL F      
      INTRINSIC ABS

* Function values at initial points
      X1 = A    
      X2 = B     
      F1 = F(A)    
      F2 = F(B)

* Test if initial values bracket a SINGLE root
      IF(F1*F2 .GT.0) THEN
         S = 0  
         RETURN
      ENDIF

* Bisect the interval and locate the root iteratively
      COUNT = 1
111   X0 = (X1+X2)/2.0
      F0 = F(X0)

      IF (F0 .EQ. 0) THEN
         S=1
         ROOT = X0
         RETURN
      ENDIF



      IF(F1*F0 .LT.0) THEN
         X2 = X0
      ELSE
         X1 = X0
         F1 = F0
      ENDIF

* Test for accuracy and repeat the process, if necessary
      IF(ABS((X2-X1)/X2).LT.EPS) THEN
         S = 1
         ROOT = (X1+X2)/2.0
         RETURN
      ELSE
         COUNT = COUNT + 1
         GO TO 111
      ENDIF

      END 

* ---------------- End of subroutine BIM ----------------- *
* -------------------------------------------------------- * 
* Function subprogram F(x)
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = X*X+X-2

      RETURN
      END

* ----------------- End of function F(X) ----------------- *
