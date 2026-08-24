C       PROGRAM ET2.FOR
        DIMENSION X(40)
        REAL XMEAN,CO

        WRITE(*,*)'INPUT VALUES OF N'
        READ (*,*) N

        OPEN(UNIT= 4, FILE='ET.DAT', STATUS='OLD')

        DO 100 I = 1, N

        READ(4,*) X(I)

100     CONTINUE
*****************************************************************                  
         SUM=0
         SUM2=0
         SUM3=0

         DO 12 I=1,N
         SUM=SUM+X(I)
       
12      CONTINUE

        AV=SUM/N
        XMEAN=AV
*******************************************************************
         

         DO 45 I=1,N-1
         SUM2=SUM2+(X(I)-AV)*(X(I+1)-AV)
45       CONTINUE   

         SUM2=SUM2/(N-1)
*******************************************************
        DO 15 I=1,N

        SUM3=SUM3+(X(I)-AV)**2
15      CONTINUE

        VAR=SUM3/N
        SD=SQRT(VAR)
        CO=SUM2/VAR
**********************************************************
        

        WRITE(*,*)' XMEAN=',XMEAN
        WRITE(*,*)' STANDARD DEVIATION=',SD
        WRITE(*,*)' SERCO=  ',    CO
        STOP
        END
      
        





