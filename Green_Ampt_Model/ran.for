        DIMENSION FREQ(100)

        REAL X(100),MINVAL,MAXVAL,INTL,FREQ
        
        MINVAL = 999999999.999
        MAXVAL = 0.000
        
C       N =TOTAL NO. OF ELEMENTS IN DATA ARRARY
C       INTL=(4-2)=(6-4)=2

        WRITE(*,*)'Input Total data nos & data range interval'
        READ(*,*) N,INTL

        OPEN(UNIT= 4, FILE='ET.DAT', STATUS='OLD')

        DO 100 I = 1, N

        READ(4,*) X(I)

100     CONTINUE

        DO 30 I = 1, N
        IF(X(I) .LT. MINVAL)THEN
        MINVAL= X(I)
        ENDIF

        IF(X(I) .GT. MAXVAL)THEN
        MAXVAL= X(I)                                                                                            WRITE(*,*)'MINIMUM',     MINVAL,'     MAXIMUM',     MAXVAL
        ENDIF
30      CONTINUE

        WRITE(*,*)'MINIMUM',     MINVAL,'     MAXIMUM',     MAXVAL

C       J=NO OF INTERVAL; FOR RANGE [2 TO 10]; INTL=(4-2)=(6-4)=2
C       MINVAL=2; MAXVAL=10; [J=8/2 +1=5]
        
        J=( (MAXVAL- MINVAL)/INTL ) + 1

        DO 12 I=1,J

        FREQ(I)=0
        
12      CONTINUE
************************************************************************
        DO 110 I= 1, N
        


        LL=MINVAL+INTL
        DO 120 K=1, J

        IF(X(I) .LE. LL) GOTO 555
        LL=LL+INTL
120     CONTINUE


555     FREQ(K)=FREQ(K)+1
        
110     CONTINUE
       

        DO 111 I=1,J
        WRITE(*,*) 'INTERVAL NO=  '  ,  I,  'FREQUENCY=   '  ,  FREQ(I)

111     CONTINUE

        STOP
        END
