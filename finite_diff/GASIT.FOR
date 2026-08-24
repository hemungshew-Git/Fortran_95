* -------------------------------------------------------- * 
      PROGRAM GASIT
* -------------------------------------------------------- *
* Main program                                             *
*     This program uses the subprogram GASEID to solve a   *
*     system of equations by Gauss-Seidel iteration method *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     NIL                                                  *
* -------------------------------------------------------- *
* Subroutines used                                         *
*     GASEID                                               *
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
      PARAMETER(EPS=0.000001,MAXIT=50)
      DIMENSION A(10,10), B(10), X(10)

      WRITE(*,*)
      WRITE(*,*) '       SOLUTION BY GAUSS-SEIDEL ITERATION'
      WRITE(*,*)

      WRITE(*,*) 'What is the size of the system(n)?'
      READ(*,*) N     
      WRITE(*,*) 'Input coefficients a(i,j), row-wise' 
      WRITE(*,*) 'one row on each line'

      DO 20 I = 1,N
         READ(*,*) (A(I,J),J=1,N)
20    CONTINUE

      WRITE(*,*) 'Input vector b'
      READ(*,*) (B(I), I = 1, N)

      CALL GASEID(N,A,B,X,EPS,COUNT,MAXIT,STATUS)                                                                                         

      IF(STATUS .EQ. 2) THEN
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

* --------------- End of main program GASIT -------------- *
* -------------------------------------------------------- * 
      SUBROUTINE GASEID(N,A,B,X,EPS,COUNT,MAXIT,STATUS) 
* -------------------------------------------------------- *
* Subroutine                                               *
*     This subroutine solves a system of linear            *
*     equations using Gauss-Seidel iteration algorithm     *
* -------------------------------------------------------- *
* Arguments                                                *
* Input                                                    *
*     N - Number of equations                              *
*     A - Coefficient matrix                               *
*     B - Right side vector                                *
*     EPS - Error bound                                    *
*     MAXIT - Maximum iterations allowed                   *
* Output                                                   *
*     X - Solution vector                                  *
*     COUNT - Number of iterations done                    *
*     STATUS - Status of convergence                       *                                                          
* -------------------------------------------------------- *
* Local Variables                                          *
*     DUMMY,SUM,KEY                                        *
* -------------------------------------------------------- *
* Functions invoked                                        *
*     ABS                                                  *
* -------------------------------------------------------- *
* Subroutines called                                       *
*     NIL                                                  *
* -------------------------------------------------------- *
         
      INTEGER N,KEY,COUNT,MAXIT,STATUS
      REAL A,B,X,EPS,DUMMY
      DOUBLE PRECISION SUM
      DIMENSION A(10,10),B(10),X(10),XO(10)
      INTRINSIC ABS

* Initial values of X
      DO 10 I = 1,N
        X(I) = B(I) / A(I,I)
10    CONTINUE

      COUNT = 1
11    KEY = 0

* Computing X(I) values
      DO 30 I = 1,N
         SUM = B(I)
         DO 20 J = 1,N 
            IF(I.EQ.J) GOTO 20
            SUM = SUM - A(I,J) * X(J)
20       CONTINUE

         DUMMY = SUM/A(I,I)
         IF(KEY .EQ. 0) THEN
*           Testing for accuracy

            IF(ABS((DUMMY - X(I))/DUMMY) .GT. EPS) THEN
               KEY = 1
            ENDIF

         ENDIF
         X(I) = DUMMY
30    CONTINUE

      IF(KEY .EQ. 1) THEN
*        Testing for convergence

         IF(COUNT .EQ. MAXIT) THEN
            STATUS = 2
            RETURN
         ELSE
            STATUS = 1
            COUNT = COUNT + 1
            GOTO 11
         ENDIF

      ENDIF

      RETURN   
      END      
* --------------- End of subroutine GASEID -----------------*
