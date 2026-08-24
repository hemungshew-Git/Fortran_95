* -------------------------------------------------------- * 
      PROGRAM FIXEDP
* -------------------------------------------------------- *
* Main program                                             *
*     This program finds a root of a function using the    *
*     fixedp point iteration method                        *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     G, ABS                                               *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X0 - Initial guess                                   *
*     X - Estimated root                                   *
*     ERROR - Relative error                               *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
*     MAXIT - Maximum Iterations allowed                   *
* -------------------------------------------------------- *

      REAL X0,X,ERROR,G,ABS,EPS
      INTEGER MAXIT
      INTRINSIC ABS   
      EXTERNAL G    
      PARAMETER(EPS = 0.00001)

      WRITE(*,*)
      WRITE(*,*) ' SOLUTION BY FIXED POINT ITERATION METHOD'
      WRITE(*,*)

      WRITE(*,*) 'Input initial estimate of root'
      READ(*,*) X0
      WRITE(*,*) 'Maximum iterations allowed'
      READ(*,*) MAXIT

      WRITE(*,*)
      WRITE(*,*)'        ITERATION     VALUE OF X          ERROR'     

      DO 100 I = 1, MAXIT
         X = G(X0)  
         ERROR = ABS((X-X0)/X)
         WRITE(*,*) I,X,ERROR  
         IF(ERROR .LT.EPS) THEN 
           WRITE(*,*)
           STOP          
         ENDIF                         
         X0 = X
100   CONTINUE 

      WRITE(*,*) 'Process does not converge to a root'
      write(*,*) 'Exit from loop'

      STOP 
      END

* -------------- End of main program FIXEDP -------------- * 
* -------------------------------------------------------- *
* Function subprogram G(x)
* -------------------------------------------------------- *

      REAL FUNCTION G(X)
      REAL X

      G = 2.0-X*X   

      RETURN
      END

*------------------ End of function G(X) ----------------- *
