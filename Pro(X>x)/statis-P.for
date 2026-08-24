C       FINDING MOMENTS COLUMN WISE OF ENTIRE POPULATION

        DIMENSION A(10,10),MEAN(10),STD(10),SSQ(10),KKU(10),COV(10)
     1   ,var(10),SERCO(10),SS1(10),SS2(10)

        REAL AVG,SD,SQ,KU,N,MEAN,STD,SSQ,KKU,COV,var,SERCO
     1   ,S1,S2,CCO,SS1,SS2


        OPEN(UNIT= 4, FILE='STATIS.DAT', STATUS='OLD')

        N=7

        DO 1 I=1,7
        READ(4,*)(A(I,J),J=1,10)
1       CONTINUE

***NO OF COLUMNS=10 & BELOW EACH COLUMN THE PARAMETERS FOR ALL 7 ROWS***

        DO 2 j=1,10

        SUM=0
        
        DO 3 i=1,7

        SUM=SUM+A(I,J)

3       CONTINUE

        AVG=SUM/N
        mean(j)=avg
2       CONTINUE

        WRITE (*,100) (mean(J),j=1,10)
100     FORMAT(3X, 'Mean(j)=', 10F6.2 )

***************************************************
        DO 22 j=1,10
        SUM2=0
        DO 33 i=1,7

        SUM2=SUM2+(A(I,J)-MEAN(J))**2

33      CONTINUE

        SD=SQRT(ABS(SUM2/N) )
        STD(J)=SD
        VAR(J)=STD(J)**2
        COV(J)= MEAN(J)/STD(J)
                
22      CONTINUE

*****************************************************
        WRITE (*,110) (std(J),j=1,10)
        WRITE(*,115)(COV(J),J=1,10)
        WRITE(*,116)(var(J),J=1,10)

110     FORMAT(3X, 'Std(j)=', 10f6.2 )
115     FORMAT(3X, 'Cof(j)=', 10f6.2 )        
116     FORMAT(  2X, 'Var(j)=',10(1x,f6.2)    )   

        DO 4 j=1,10

        SUM3=0
        SUM4=0
        DO 5 i=1,7
        sum3 = sum3 + ( a(i,j)-mean(j) )**3
        sum4 = sum4 + ( a(i,j)-mean(j) )**4
        
5       CONTINUE

        C1=N/((N-1)*(N-2))

        SQ=C1*(sum3/STD(j)**3)
        ssq(j)=SQ

        A1=N*(N+1)/((N-1)*(N-2)*(N-3))
        B1= 3*(N-1)**2/((N-2)*(N-3))

        KU=A1*(sum4/STD(j)**4)-B1
        kku(j)=KU        

4       CONTINUE
            
        WRITE (*,120) (SSQ(J),j=1,10)
        WRITE (*,130) (KKU(J),j=1,10)

120     FORMAT(3X, 'Skew(j)=', 10F6.2)

130     FORMAT(3X, 'Kurto(j)=', 10f6.2)
        
        DO 7 j=1,10

        S2=0

        DO 6 i=1,6

        S2=S2+(A(I,J)-MEAN(J))*(A(I+1,J)-MEAN(J))

6       CONTINUE

        S2=S2/(N-1)
        SS2(J)=S2        
7       CONTINUE

        DO 111 J=1,10

        SERCO(J)=SS2(J)/VAR(J)
        
111     CONTINUE

        WRITE (*,200) (SERCO(J),j=1,10)
200     FORMAT(1X, 'SERCO(j)=',10(1x,f6.4)    )


        STOP
        END
