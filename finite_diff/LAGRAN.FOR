* -------------------------------------------------------- * 
      PROGRAM LAGRAN
* -------------------------------------------------------- *
* Main program                                             *
*     This program computes the interpolation value at a   *
*     specified point, given a set of data points, using   *
*     the Lagrange interpolation representation            *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     XN   - Number of data sets                           *
*     X(I) - Data points                                   *
*     F(I) - Function values at data points                *
*     XP   - Point at which interpolation is required      *
*     FP   - Interpolated value at XP                      *
*     LF   - Lagrangian factor                             *
* -------------------------------------------------------- *
* Constants used                                           *
*     MAX - Maximum number of data points permitted        *                                                             
* -------------------------------------------------------- *

      INTEGER N,MAX
      REAL X,F,FP,LF,SUM           
      PARAMETER( MAX = 10 )
      DIMENSION X(MAX),F(MAX)

      WRITE(*,*) 'Input number of data points(N)'
      READ(*,*) N 
      WRITE(*,*) 'Input data points X(I) and Function values F(I)'
      WRITE(*,*) 'one set in each line'
      DO 10 I = 1,N
         READ(*,*) X(I), F(I)
10    CONTINUE

      WRITE(*,*) 'Input X value at which ',
     +           'interpolation is required'
      READ(*,*) XP

      SUM = 0.0
      DO 30 I = 1,N
         LF = 1.0
         DO 20 J = 1,N
            IF (I.NE.J) THEN
               LF = LF * (XP - X(J)) / (X(I) - X(J))
            ENDIF
20       CONTINUE
         SUM = SUM + LF * F(I)
30    CONTINUE
      FP = SUM
      
      WRITE(*,*) 
      WRITE(*,*) 'LAGRANGIAN INTERPOLATION'
      WRITE(*,*)
      WRITE(*,*) 'Interpolated Function Value'
      WRITE(*,*) 'at X = ', XP, '  is', FP
      WRITE(*,*)

      STOP
      END
* ----------------- End of main LAGRAN ------------------- *
