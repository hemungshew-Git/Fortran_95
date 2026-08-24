* -------------------------------------------------------- * 
      PROGRAM FALSE 
* -------------------------------------------------------- *
* Main program                                             *
*     This program finds a root of a non-linear equation   *
*     by false position method                             *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F                                                    *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     FAL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     A - Left endpoint of interval                        *
*     B - Right endpoint of interval                       *
*     S - Status                                           *
*     ROOT - Final solution                                *
*     COUNT - Number of iterations completed               *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
* -------------------------------------------------------- *

      REAL A,B,ROOT,EPS,F
      INTEGER S, COUNT
      EXTERNAL FAL,F 
      PARAMETER(EPS = 0.00001)

      WRITE(*,*) 'Input starting values'
      READ(*,*) A,B                                      
      
      WRITE(*,*)
      WRITE(*,*) '      SOLUTION BY FALSE POSITION METHOD'
      WRITE(*,*)

      CALL FAL(A,B,EPS,S,ROOT,COUNT)
     
      IF(S.EQ.0) THEN
         WRITE(*,*) 'Starting points do not bracket any root'
         WRITE(*,*)
      ELSE
         WRITE(*,*) 
         WRITE(*,*) 'Root = ', ROOT
         WRITE(*,*) 'F(ROOT) = ', F(ROOT)
         WRITE(*,*) 'NO.OF ITERATIONS = ', COUNT
         WRITE(*,*)
      ENDIF                               

      STOP
      END

* ----------------- End of main FALSE -------------------- *
* -------------------------------------------------------- * 
      SUBROUTINE FAL(A,B,EPS,S,ROOT,COUNT)


* -------------------------------------------------------- *
* Subroutine                                               *
*     FAL finds a root of a non-linear equation            *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     A - Left-end point of interval                       *
*     B - Right-end point of interval                      *
*     EPS - Error bound                                    *
* Output                                                   *
*     S - Status of completion of task                     *
*     ROOT - Final solution                                *
*     COUNT - Number of iterations done                    *
* -------------------------------------------------------- *
* Local Variables                                          *
*     X0,X1,X2,F0,F1,F2                                    *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F,ABS                                                *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,B,EPS,X0,X1,X2,F0,F1,F2,F,ABS
      INTEGER S,COUNT
      INTRINSIC ABS
      EXTERNAL F

      X1 = A 
      X2 = B 
      F1 = F(X1)      
      F2 = F(X2) 
  
*Test if A and B bracket a root

      IF(F1*F2 .GT.0) THEN
         S = 0  
         RETURN  
      ENDIF         
      WRITE(*,*)'             X1                X2'

      COUNT = 1 
111   X0 = X1 - F1 * (X2-X1)/(F2-F1)
      F0 = F(X0)
      IF(F1*F0 .LT.0) THEN
         X2 = X0
         F2 = F0
      ELSE      
         X1 = X0      
         F1 = F0
      ENDIF
      WRITE(*,*) X1,X2 

*Test if desired accuracy is achieved
 

      IF(ABS((X2-X1)/X2) .LT.EPS) THEN
         S = 1  
         ROOT = (X1+X2)*0.5
         RETURN 
      ELSE
         COUNT = COUNT+1
         GO TO 111
      ENDIF 

      END          

* ---------------- End of subroutine FAL ----------------- *
* -------------------------------------------------------- *
* Function subprogram F(X)
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = X*X+X-2

      RETURN
      END

* ----------------- End of function F(X) ----------------- *
