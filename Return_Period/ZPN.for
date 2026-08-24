        DIMENSION A(50,50)
        INTEGER Z,N,P

        DO 10 I=1,2
        
        READ(*,*) (A(I,J),J=1,3) 
10      CONTINUE
*----------------------------------------------------------
        Z = 0
        N = 0
        P = 0
        
        DO 1 I=1,2
        DO 2 J=1,3
        IF (A(I,J).NE.0) GOTO 2
        Z=Z+1

        WRITE(*,*)I,J
2       CONTINUE
1       CONTINUE

        WRITE(*,*) 'TOTAL NUMBER OF ZERO : ', Z
        
*---------------------------------------------------------
        DO 3 I=1,2
        DO 4 J=1,3
        IF (A(I,J).LE.0) GOTO 4 
        P=P+1

        WRITE(*,*)I,J
4       CONTINUE
3       CONTINUE

        WRITE(*,*) 'TOTAL NUMBER OF POSITIVE : ',P
        
*--------------------------------------------------------
        DO 5 I=1,2
        DO 6 J=1,3
        IF (A(I,J).GE.0) GOTO 6 
        N=N+1

        WRITE(*,*)I,J
6       CONTINUE
5       CONTINUE
        
        WRITE(*,*) 'TOTAL NUMBER OF NEGATIVE : ',N
        STOP
        END
*----------------------------------------------------------
