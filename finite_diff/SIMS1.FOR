* -------------------------------------------------------- * 
      PROGRAM SIMS1 
* -------------------------------------------------------- *
* Main program                                             *
*     This program integrates a given function             *
*     using the Simpson's 1/3 rule                         *
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
*   ICS - Value of the integral                            *                                                             
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *                                                             
* -------------------------------------------------------- *

      INTEGER N,M     
      REAL A,B,H,SUM,ICS,X,F1,F2,F3
      EXTERNAL F

      WRITE(*,*) 'Initial value of X'     
      READ(*,*) A
      WRITE(*,*) 'Final value of X' 
      READ(*,*) B
      WRITE(*,*) 'Number of segments (EVEN number)'
      READ(*,*) N
      
      H = (B-A)/N
      M = N/2

      SUM = 0.0
      X = A
      F1 = F(X)
      DO 10 I = 1,M
         F2 = F(X+H)
         F3 = F(X+2*H)
         SUM = SUM + F1 + 4*F2 + F3
         F1 = F3
         X = X + 2*H
10    CONTINUE

      ICS = SUM * H/3.0

      WRITE(*,*)
      WRITE(*,*) 'INTEGRAL FROM', A, '  TO', B
      WRITE(*,*)
      WRITE(*,*) 'WHEN H = ',H, '  IS', ICS
      WRITE(*,*)

      STOP
      END   

* ----------------- End of main SIMS1 -------------------- 
* -------------------------------------------------------- *
* Function subprogram F(X)                                 *
* -------------------------------------------------------- *

      REAL FUNCTION F(X)
      REAL X

      F = 1-EXP(-X/2.0)

      RETURN
      END

* ----------------- End of function F(X) ----------------- *
