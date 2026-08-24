* -------------------------------------------------------- * 
      PROGRAM COMPR
* -------------------------------------------------------- *
* Main program                                             *
*     This program locates all the roots, both real        *
*     and complex, using Bairstow's method                 *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     INPUT,BSTOW,QUAD,OUTPUT                              *
* -------------------------------------------------------- *
* Variables used                                           *
*     N - Degree of polynomial                             *
*     A - Array of coefficients of polynomial              *
*     U0,V0 - Initial values of coefficients of the        *
*             quadratic factor                             *
*     U , V - Computed values of coefficients of the       *
*             quadratic factor                             *
*     B - Coefficients of the reduced polynomial           *
*     X1,X2 - Roots of the quadratic factor                *
*     TYPE  - Type of roots (real, imaginary or equal)      *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
* -------------------------------------------------------- *

      INTEGER N, TYPE
      REAL A,B,U0,V0,U,V,X1,X2,EPS,D0,D1,D2
      PARAMETER( EPS = 1.E-6 )
      DIMENSION A(11),B(11)

      WRITE(*,*)
      WRITE(*,*) '       EVALUATION OF COMPLEX ROOTS    '
      WRITE(*,*)

      CALL INPUT(N,A,U0,V0)

100   IF(N.GT.2) THEN

*-----obtain a quadratic factor
         CALL BSTOW(N,A,B,U0,V0,U,V,EPS)

         D2 = 1 
         D1 = -U
         D0 = -V

*-----find roots of the quadratic factor                           
         CALL QUAD(D2,D1,D0,X1,X2,TYPE) 

*-----print the roots
         CALL OUTPUT(N,TYPE,X1,X2)

*-----set the coefficients of the factor polynomial
         N = N-2 
         DO 200 I = 1, N+1
            A(I) = B(I+2)
200      CONTINUE

*-----set initial values for next quadratic factor
         U0 = U 
         V0 = V      
         GOTO 100

      ENDIF
                                        
      IF(N.EQ.2) THEN

*-----polynomial is a quadratic one
         CALL QUAD(A(3),A(2),A(1),X1,X2,TYPE)
         CALL OUTPUT(N,TYPE,X1,X2)
      ELSE

*-----last root of an odd order polynomial
         ROOT = - A(1)/A(2)
         WRITE(*,*)
         WRITE(*,*) 'Final root = ', ROOT
         WRITE(*,*) 

      ENDIF

      STOP
      END

* -------------- End of main program COMPR --------------- *
* -------------------------------------------------------- * 
      SUBROUTINE INPUT(N,A,U0,V0)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine reads polynomial details and         *
*     initial values of the quadratic coefficients         *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *                                                             
*     NIL                                                  *                                                                       
* Output                                                   *
*     N - Degree of polynomial                             *
*     A - Polynomial coefficients                          *
*     U0,V0 - Initial values of the quadratic factor       *
* -------------------------------------------------------- *
* Local Variables                                          *
*     NIL                                                  *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      REAL A,U0,V0
      INTEGER N
      DIMENSION A(11)


      WRITE(*,*) 'Input degree of polynomial (N)'
      READ(*,*) N
      WRITE(*,*) 'Input polynomial coefficients A(N+1) to A(1)'
      DO 11 I = N+1, 1, -1
         READ(*,*) A(I)
11    CONTINUE
      WRITE(*,*) 'Give initial values U0 and V0'
      READ(*,*) U0,V0 

      RETURN
      END

* --------------- End of subroutine INPUT ---------------- *
* -------------------------------------------------------- * 
      SUBROUTINE BSTOW(N,A,B,U0,V0,U,V,EPS)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine finds the quadratic factor using     *
*     multivariable Newton's method and also finds the     *
*     reduced polynomial                                   *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Degree of polynomial                             *
*     A - Polynomial coefficients                          *
*     U0,V0 - Initial guess for the coefficients of the    *
*             quadratic factor                             *
*     EPS - Error bound                                    *
* Output                                                   *
*     U,V - Computed coefficients of the quadratic factor  *
*       B - Coefficients of the reduced polynomial         *
* -------------------------------------------------------- *
* Local Variables                                          *
*     D,DELU,DELV,C                                        *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER N
      REAL A,B,U0,V0,U,V,EPS,D,DELU,DELV,C
      INTRINSIC ABS
      DIMENSION A(11), B(11), C(11)

      COUNT = 1                                    
100   B(N+1) = A(N+1)
      B(N) = A(N) + U0 * B(N+1)
      DO 111 I = N-1, 1, -1 
         B(I) = A(I) + U0 * B(I+1) + V0 * B(I+2)
111   CONTINUE 

      C(N+1) = 0 
      C(N) = B(N+1)
      DO 222 I = N-1, 1, -1 
         C(I) = B(I+1) + U0 * C(I+1) + V0 * C(I+2)
222   CONTINUE
      
      D = C(2) * C(2) - C(1) * C(3)
      DELU = -(B(2) * C(2) - B(1) * C(3))/D
      DELV = -(B(1) * C(2) - B(2) * C(2))/D
      U = U0 + DELU
      V = V0 + DELV                                   
      IF( ABS(DELU/U).LE.EPS .AND. ABS(DELV/V).LE.EPS ) THEN
         RETURN
      ENDIF

      IF(COUNT .LT. 100) THEN
         U0 = U
         V0 = V
         COUNT = COUNT + 1
         GOTO 100
      ELSE
         WRITE(*,*)
         WRITE(*,*) 'NO CONVERGENCE IN 100 ITERATIONS'
         WRITE(*,*)
         STOP
      ENDIF

      END  

* --------------- End of subroutine BSTOW ---------------- *
* -------------------------------------------------------- * 
      SUBROUTINE QUAD(A,B,C,X1,X2,TYPE)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine solves a quadratic equation of       *
*             2                                            *
*     type  AX  + BX + C                                   *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     A,B,C - Coefficients of the quadratic equation       *                                                                                                                            
* Output                                                   *
*     X1,X2 - Roots of the quadratic equation              *
*     TYPE - Type of roots                                 *
* -------------------------------------------------------- *
* Local Variables                                          *
*     Q                                                    *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     SQRT,ABS                                             *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER TYPE,IMAGE,EQUAL,UNEQUAL
      REAL A,B,C,X1,X2,SQRT,ABS
      INTRINSIC SQRT,ABS
      PARAMETER( IMAGE = 1, EQUAL = 2, UNEQL = 3 )


      Q = B * B - 4 * A * C 

      IF(Q.LT.0) THEN 
*-------------------------Roots are complex
         X1 = -B/(2*A)         
         X2 = SQRT(ABS(Q))/(2*A)
         TYPE = IMAGE 
      ELSE IF(Q.EQ.0) THEN
*-------------------------Roots are real and equal     
         X1 = -B/(2*A) 
         X2 = X1 
         TYPE = EQUAL
      ELSE
*-------------------------Roots are real and unequal
         X1 = (-B + SQRT(Q))/(2*A)    
         X2 = (-B - SQRT(Q))/(2*A)     
         TYPE = UNEQL
      ENDIF

      RETURN 
      END

* ---------------- End of subroutine QUAD ---------------- *
* -------------------------------------------------------- * 
      SUBROUTINE OUTPUT(N,TYPE,X1,X2) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine displays the roots of the quadratic  *
*     equation                                             *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Degree of the polynomial from which              *
*         the quadratic factor was obtained                *
*     TYPE  - Type of roots                                *
*     X1,X2 - Roots of the quadratic factor                *
* Output                                                   *
*     NIL                                                  *
* -------------------------------------------------------- *
* Local Variables                                          *
*     NIL                                                  *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER N, TYPE,IMAGE,EQUAL,UNEQL
      REAL X1,X2
      PARAMETER( IMAGE = 1, EQUAL = 2, UNEQL = 3 )                                            
                                    
      WRITE(*,*)
      WRITE(*,*) 'Roots of quadratic factor at n  = ',N
      WRITE(*,*) 

      IF(TYPE .EQ. IMAGE) THEN 
         
         WRITE(*,*) 'Root1 = ', X1, '  + ', X2,'j'
         WRITE(*,*) 'Root2 = ', X1, '  - ', X2,'j'

      ELSE IF(TYPE .EQ. EQUAL) THEN

         WRITE(*,*) 'Root1 = ', X1 
         WRITE(*,*) 'Root2 = ', X1 

      ELSE

         WRITE(*,*) 'Root1 = ', X1 
         WRITE(*,*) 'Root2 = ', X2 

      ENDIF

      RETURN
      END

* --------------- End of subroutine OUTPUT --------------- *
