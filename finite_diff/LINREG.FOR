* -------------------------------------------------------- *
      PROGRAM LINREG
* -------------------------------------------------------- *
* Main program                                             *
*     This program fits a line Y = A + BX  to a given      *
*     set of data points by the method of least squares       *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     NIL                                                  *
* -------------------------------------------------------- *
* Variables used                                           *
*     X, Y - Data arrays                                   *
*     N - Number of data sets                              *
*     SUMX - Sum of x values                               *
*     SUMY - Sum of y values                               *
*     SUMXX - Sum of squares of x values                   *
*     SUMXY - Sum of products of x and y                   *
*     XMEAN - Mean of x values                             *
*     YMEAN - Mean of y values                             *
*     A - y intercept of the line                          *
*     B - Slope of the line                                *
* -------------------------------------------------------- *
* Constants used                                           *
*     MAX - Limit for number of data points                *
* -------------------------------------------------------- *

      INTEGER MAX,N
      REAL X,Y,SUMX,SUMY,SUMXX,SUMXY,XMEAN,YMEAN,DENOM,A,B
      INTRINSIC ABS
      PARAMETER( MAX = 10 )
      DIMENSION X(MAX),Y(MAX)

      WRITE(*,*)
      WRITE(*,*) '             LINEAR REGRESSION'
      WRITE(*,*)

* Reading data values

      WRITE(*,*) 'Input number of data points N'
      READ(*,*) N
      WRITE(*,*) 'Input X and Y values, ',
     +           'one set on each line '

      DO 10 I = 1, N
         READ(*,*) X(I), Y(I)
10    CONTINUE

* Computing constants A and B

      SUMX = 0.0
      SUMY = 0.0
      SUMXY = 0.0
      SUMXX = 0.0

      DO 20 I = 1, N
         SUMX = SUMX + X(I)
         SUMY = SUMY + Y(I)
         SUMXX = SUMXX + X(I) * X(I)
         SUMXY = SUMXY + X(I) * Y(I)
20    CONTINUE

      XMEAN = SUMX/N
      YMEAN = SUMY/N
      DENOM = N * SUMXX - SUMX * SUMX

      IF (ABS (DENOM) .GT. 0.00001) THEN
         B = (N * SUMXY - SUMX * SUMY)/DENOM
         A = YMEAN - B * XMEAN
      ELSE
         WRITE(*,*)
         WRITE(*,*) 'NO SOLUTION'
         STOP
      ENDIF

* Printing results

      WRITE(*,*)
      WRITE(*,*) 'LINEAR REGRESSION LINE Y = A + BX'
      WRITE(*,*)
      WRITE(*,*) 'THE COEFFICIENTS ARE:'
      WRITE(*,*) '   A = ', A
      WRITE(*,*) '   B = ', B
      WRITE(*,*)

      STOP
      END
* ----------------- End of main LINREG ------------------- *
