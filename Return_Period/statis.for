        DIMENSION A(10,10)
        REAL AVG,SD,SQ,KU,N
        OPEN(UNIT= 4, FILE='STATIS1.DAT', STATUS='OLD')

        DO 1 I=1,10
        READ(4,*)(A(I,J),J=1,10)
1       CONTINUE

        N=100
        SUM=0
        SUM2=0

        DO 2 I=1,10
        DO 3 J=1,10

        SUM=SUM+A(I,J)
        SUM2=SUM2+A(I,J)**2
                
3       CONTINUE
2       CONTINUE
        avg=sum/n
        SD=SQRT(ABS( (  (SUM2)-(SUM)**2/N  )/(N-1) )   )
        WRITE (*,100)  AVG,   SD

100     FORMAT(3X, 'AVG=', F6.2,3X,'STANDARD DEVIATION=', F6.2)

        SUM3=0
        SUM4=0

        DO 4 I=1,10
        DO 5 J=1,10

        sum3 = sum3 + ( a(i,j)-avg )**3
        sum4 = sum4 + ( a(i,j)-avg )**4  
                  
5       CONTINUE
4       CONTINUE

        C1=N/((N-1)*(N-2))

        SQ=C1*(sum3/SD**3)

        A1=N*(N+1)/((N-1)*(N-2)*(N-3))
        B1= 3*(N-1)**2/((N-2)*(N-3))

        KU=A1*(sum4/SD**4)-B1

        WRITE (*,*)'SKEW=   ',   SQ,     'KURTOSIS=    ', KU   

        STOP
        END
