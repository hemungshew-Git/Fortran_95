* -------------------------------------------------------- * 
      PROGRAM NEWINT
* -------------------------------------------------------- *
* Main program                                             *
*     This program constructs the Newton interpolation     *
*     polynomial for a given set of data points and then   *
*     computes interpolation value at a specified value    *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     N  - Number of data points                           *
*     X  - Array of independent data points                *
*     F  - Array of function values                        *
*     XP - Desired point for interpolation                 *
*     FP - Interpolation value at XP                       *
*     D  - Difference table                                *
*     A  - Array of coefficients of interpolation          *
*          polynomial                                      *
* -------------------------------------------------------- *
* Constants used                                           *
*     NIL                                                  *                                                           
* -------------------------------------------------------- *

      INTEGER N
      REAL XP,FP,SUM,PI,X,F,A,D
      DIMENSION X(10),F(10),A(10),D(10,10)

      WRITE(*,*) 'Input number of data points'
      READ(*,*) N 
      WRITE(*,*) 'Input the values of X and F(x), ',
     +           'one set on each line'
      DO 10 I = 1,N
         READ(*,*) X(I), F(I)
10    CONTINUE

* Construct difference table D

      DO 20 I = 1,N
         D(I,1) = F(I)
20    CONTINUE

      DO 40 J = 2,N
         DO 30 I = 1, N-J+1
            D(I,J) = (D(I+1,J-1)-D(I,J-1))/(X(I+J-1)-X(I))
30       CONTINUE
40    CONTINUE

* Set the coefficients of interpolating polynomial

      DO 50 J = 1,N
         A(J) = D(1,J)
50    CONTINUE

* Compute interpolation value

      WRITE(*,*) 'Input XP where interpolation is required' 
      READ(*,*) XP

      SUM = A(1)
      DO 70 I = 2,N
         PI = 1.0
         DO 60 J = 1, I-1
            PI = PI *(XP-X(J))
60       CONTINUE
         SUM = SUM + A(I) * PI
70    CONTINUE

      FP = SUM

* Write results

      WRITE(*,*)
      WRITE(*,*) 'NEWTON INTERPOLATION'
      WRITE(*,*)
      WRITE(*,*) 'Interpolated Function Value'
      WRITE(*,*) 'at X = ', XP, ' is', FP
      WRITE(*,*)

      STOP
      END
* ----------------- End of main NEWINT ------------------- *
