        REAL A(31,2),T,P,T1,T2
C       REAL A(D,M)
c       once you add all the months in the data file, you can use j=1, 12

        INTEGER  D,M,Y

        OPEN(UNIT= 4, FILE='ASTRO.DAT', STATUS='OLD')

        DO 100 I=1, 31

        READ(4,*) (A(I,J),J=1,2)

100     CONTINUE

                   
        WRITE(*,*)'INPUT T1=TIME(HR), T2=TIME(MINUTE), D=DATE(NUMBER), M=MONTH(NUMBER),
     1  for e.g: june=1, july=2 SO ON ! '

        READ(*,*)T1,T2, D,M

        T=T1+(T2/60)

        IF (D.LT.31) THEN
        P=A(D,M)+( (A(D+1,M)-A(D,M))*(T-5.5) )/24
        ELSE
        
        P=A(D,M)+( (A(1,M+1)-A(D,M))*(T-5.5) )/24
        ENDIF

        WRITE(*,*)' POSITION OF MOON IN DEGREE =',   P


        STOP
        END

