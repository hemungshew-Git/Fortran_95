* -------------------------------------------------------- * 
      PROGRAM TRAPE1
* -------------------------------------------------------- *
* Main program                                             *
*     This program integrates a given function             *
*     using the trapezoidal rule                           *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     F                                                    *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     A - Lower limit of integration                       *
*     B - Upper limit of integration                       *
*     H - Segment width                                    *
*     N - Number of segments                               *
*   ICT - Value of integral                                *                                                            
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *                                                             
* -------------------------------------------------------- *

      INTEGER  N 
      REAL A,B,H,SUM,ICT
      EXTERNAL F

      WRITE(*,*) 'Give initial value of X'
      READ(*,*) A
      WRITE(*,*) 'Give final value of X'
      READ(*,*) B
      WRITE(*,*) 'What is the segment width?'
      READ(*,*) H
 
      N = (B-A)/H
     
      SUM = (F(A) + F(B))/2.0
      DO 10 I = 1, N-1
         SUM = SUM + F(A+I*H)
10    CONTINUE

      ICT = SUM * H

      WRITE(*,*)
      WRITE(*,*) 'INTEGRATION BETWEEN', A,'      AND', B 
      WRITE(*,*)
      WRITE(*,*) 'WHEN H =', H, '      IS', ICT
      WRITE(*,*)

      STOP
      END

* ----------------- End of main TRAPE1 ------------------- *
* -------------------------------------------------------- *
* Function subprogram F(X)                                 *
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = 1-EXP(-X/2.0)

      RETURN
      END
* ----------------- End of function F(X) ----------------- *
