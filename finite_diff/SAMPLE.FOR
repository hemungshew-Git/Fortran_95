* -------------------------------------------------------- * 
      PROGRAM SAMPLE
* -------------------------------------------------------- *
* Main program                                             *
*     A program to evaluate a function at different points *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X - Independent variable                             *
*     F - Function value                                   *
* COUNT - Counter to store number of evaluations           *
* -------------------------------------------------------- *
* Constants used                                           *
*     N - Number of function values                        *
* -------------------------------------------------------- *

      REAL X, F
      INTEGER COUNT, N
      PARAMETER( N = 5 )                  

      WRITE(*,*) 'Input value of X'
      READ(*,*) X 

      WRITE(*,*) '          OUTPUT OF SAMPLE PROGRAM'
      WRITE(*,*) '             X                F   '
                
      COUNT = 0 

100   F = X * X
      WRITE(*,*) X, F
      X = X + X
      COUNT = COUNT + 1
      IF( COUNT .LT. N ) GO TO 100

      STOP
      END
* -------------------------------------------------------- *
