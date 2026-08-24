C       DIMENSION X(100)

        INTEGER N(100),A,B

        REAL X(100),MINVAL,MAXVAL
        
        MINVAL = 999999999.999
        MAXVAL = 0.000
        
        WRITE(*,*)'INPUT VALUES OF B'
        READ(*,*) B

        DATA N/100*0/

C       WHERE B =THE NO. OF RANGES YOU SELECT=THE BIN ARRARY
C       WHERE M =TOTAL NO. OF ELEMENTS IN DATA ARRARY         
C       WHERE A =NO. TO DEVIDE THE X max    


        WRITE(*,*)'INPUT VALUES OF M'
        READ(*,*) M

        OPEN(UNIT= 4, FILE='ET.DAT', STATUS='OLD')

        DO 100 I = 1, M

        READ(4,*) X(I)

100     CONTINUE

C        CLOSE(4)

        DO 30 I = 1,M
        IF(X(I) .LT. MINVAL)THEN
        MINVAL= X(I)
        ENDIF

        IF(X(I) .GT. MAXVAL)THEN
        MAXVAL= X(I)                                                                                            
        WRITE(*,*)'MINIMUM',     MINVAL,'     MAXIMUM',     MAXVAL
        ENDIF
30      CONTINUE

        WRITE(*,*)'MINIMUM',     MINVAL,'     MAXIMUM',     MAXVAL
        
        A=INT(MAXVAL/B)+1

        DO 10 I=1,M

        J=INT( X(I)/A )+1

        N(J)=N(J)+1

10      CONTINUE

        DO 110 I= 1, B

        A = N(I)

        WRITE(*,*) 'FREQUENCY=',  N(I)

110     CONTINUE

        END
