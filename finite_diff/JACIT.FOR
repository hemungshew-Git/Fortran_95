* -------------------------------------------------------- *
      PROGRAM JACIT
* -------------------------------------------------------- *
* Main program                                             *
*     This program uses the subprogram JACOBI to solve     *
*     a system of equations by Jacobi iteration method     *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     JACOBI                                               *
* -------------------------------------------------------- *
* Variables used                                           *
*     A - Coefficient matrix                               *
*     B - Right side vector                                *
*     N - System size                                      *
*     X - Solution vector                                  *
*     COUNT - Number of iterations completed               *
*     STATUS - Convergence status                          *
* -------------------------------------------------------- *
* Constants used                                           *
*     EPS - Error bound                                    *
*     MAXIT - Maximum iterations permitted                 *
* -------------------------------------------------------- *

      REAL A,B,X,EPS
      INTEGER N,COUNT,MAXIT,STATUS
      PARAMETER(EPS=0.000001,MAXIT = 50)
      DIMENSION A(10,10), B(10), X(10)

      WRITE(*,*)
      WRITE(*,*) '          SOLUTION BY JACOBI ITERATION'
      WRITE(*,*)

      WRITE(*,*) 'What is the size of the system(n)?'
      READ(*,*) N
      WRITE(*,*) 'Input coefficients a(i,j), row-wise'
      WRITE(*,*) 'one row on each line'

      DO 20 I = 1, N
         READ(*,*) (A(I,J),J=1,N)
20    CONTINUE

      WRITE(*,*) 'Input vector b'
      READ(*,*) (B(I), I = 1, N)

      CALL JACOBI(N,A,B,X,EPS,COUNT,MAXIT,STATUS)

      IF(STATUS .EQ.2) THEN
         WRITE(*,*)
         WRITE(*,*) 'NO CONVERGENCE IN', MAXIT, ' ITERATIONS'
         WRITE(*,*)
      ELSE
         WRITE(*,*)
         WRITE(*,*) 'SOLUTION VECTOR X'
         WRITE(*,*)
         WRITE(*,*) (X(I), I = 1, N)
         WRITE(*,*)
         WRITE(*,*) 'ITERATIONS = ',COUNT
         WRITE(*,*)
      ENDIF

      STOP
      END

* --------------- End of main program JACIT -------------- *
* -------------------------------------------------------- *
      SUBROUTINE JACOBI(N,A,B,X,EPS,COUNT,MAXIT,STATUS)
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine solves a system of n linear          *
*     equations using the Jacobi iteration method          *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Number of equations                              *
*     A - Matrix of coefficients of the equations          *
*     B - Right side vector                                *
*     EPS - Error bound                                    *
*     MAXIT - Maximum iterations allowed                   *
* Output                                                   *
*     X - Solution vector
*     COUNT - Number of iterations done                    *
*     STATUS - Convergence status                          *
* -------------------------------------------------------- *
* Local Variables                                          *
*     XO,SUM                                               *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *

      INTEGER N,KEY,COUNT,MAXIT,STATUS
      REAL A,B,X,XO,EPS
      DOUBLE PRECISION SUM
      INTRINSIC ABS
      DIMENSION A(10,10),B(10),X(10),XO(10)

* Initial values of X
      DO 10 I=1,N
        XO(I) = B(I)/A(I,I)
10    CONTINUE

      COUNT = 1
15    KEY = 0

* Computing values of X(I)
      DO 30 I = 1,N
         SUM = B(I)
         DO 20 J = 1,N
            IF(I.EQ.J) GOTO 20
            SUM = SUM - A(I,J) * XO(J)
20       CONTINUE

         X(I) = SUM/A(I,I)
         IF(KEY .EQ. 0) THEN

* Testing for accuracy
            IF(ABS((X(I)-XO(I))/X(I)) .GT. EPS) THEN
               KEY = 1
            ENDIF

         ENDIF
30    CONTINUE

      IF(KEY.EQ.1) THEN

* Testing for convergence
         IF(COUNT .EQ. MAXIT) THEN
            STATUS = 2
            RETURN
         ELSE
            STATUS = 1
            DO 40 I = 1,N
              XO(I) = X(I)
 40         CONTINUE
         ENDIF

         COUNT = COUNT+1
         GO TO 15

      ENDIF

      RETURN
      END
* -------------- End of subroutine JACOBI ---------------- *
