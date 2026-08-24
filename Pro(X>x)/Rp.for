C      * ............................................
c      * frequency analysis and return period       *
C      *                                            *
C       SDXS = SAMPLE Standard Deviation of X
C       SDYS = SAMPLE Standard Deviation of Y=ln(X)
C       SDX= POPULATION STANDARD DEVIATION
c       SDY =POPULATION STANDARD DEVIATION of Y=ln(X)
C      *............................................*

        PARAMETER(P=3.14159)
        DIMENSION X(100),Y(100)
        REAL X,Y,XT,XTT,YT,YTT,FX,WX,WLX,XM,YM,FLX,Q1,A1,B1,C1
        REAL LAMDA,Y1,NU,wgx,glx,tg,i1,k1,x1,wxx,fxx
        INTEGER T,TL,NR,GM,Q,FACT
       

        WRITE(*,*)'ENTER THE SIZE OF THE SYSTEM'

        READ(*,*) NR

        OPEN(UNIT=4, FILE='TEST.DAT', STATUS='OLD')
        
        DO 10 I=1,NR

        READ(4,*) X(I)

10      CONTINUE
        CLOSE(4)

C       ESTIMATION OF THE PARAMETERS

        XT=0
        YT=0
              
        DO 20 I=1,NR
        
        XT=XT+X(I)
        Y(I)=LOG(X(I))
        YT=YT+Y(I)
                
20      CONTINUE

        XM=XT/NR
        YM=YT/NR

        WRITE(*,*)'XM=', XM, 'YM=', YM 
        
        XTT=0
        YTT=0

        DO 25 I=1,NR

        XTT = XTT +( X(I)-XM )**2
        YTT = YTT + ( Y(I)-YM )**2
25      CONTINUE
        
        SDX=SQRT( XTT/(NR-1) )
        SDY=SQRT( YTT/(NR-1) )
                
        SDXS=SQRT ( XTT/NR )
        SDYS=SQRT( YTT/NR )

        WRITE(*,*)'SDX=', SDX, 'SDY=', SDY

        WRITE(*,*)'SDXS=', SDXS, 'SDYS=', SDYS

C       ....................................................................
C       CALCULATION OF THE RERURN PERIOD BY NORMAL DISTRIBUTION
C       .....................................................................

        WRITE(*,*) 'INPUT THE DISCHARGE FOR WHICH U WANT THE RP'

        READ(*,*) Q
                
        WX=0

        DO 30 I=0, Q*100

        X1=I
        I1=(X1/100)

        K1=1/( SDX*SQRT(2*P) )

        FX=0.01*K1*EXP(- ( (I1-XM)**2/(2*SDX**2)  )  )
                                                     
        WX=WX+FX

30      CONTINUE
        WX1=1-WX

        WRITE(*,*)'..................................................'
        WRITE(*,*)'ND # P(X>x)=', WX1
        WRITE(*,*)'..................................................'
                
        WRITE(*,*)'..................................................'
        WRITE(*,*)'ND # P(X<x)=', WX
        WRITE(*,*)'..................................................'

        T=1.0/(1-WX)
        WRITE(*,*)'RERUN PERIOD BY NORMAL DISTRIBUTION=', T
C       ***********************************************************
C       CALCULATION OF THE RERURN PERIOD BY LOG-NORMAL DISTRIBUTION
C       ***********************************************************

        WRITE(*,*) 'INPUT THE DISCHARGE FOR WHICH U WANT THE RP'
        READ(*,*) Q

C       HERE SQRT(2*P)=2.51 USED BELOW
C       IN DO STARTING FROM 1 NOT 0 AS EARLIER ! AS X/100=0.00 WILL STORED
C       IN I1 AND MAKE ALOG OF IT INFINITY! INSTEAD HERE DO STARTS FROM
C       1/100=0.01 TO 20 IN A STEP OF [1/100 = 0.01]
        
        WLX=0

        DO 40 I=1, Q*100

        X1=I
        I1=(X1/100)
        
        A=1.0/(I1*SDY*2.51)

        B= EXP( -  ( ALOG(I1)-YM )**2/(2*SDY**2)     ) 

        FLX=0.01*A*B

        WLX= (WLX+FLX)
        
40      CONTINUE

        WLX1=1-WLX

        WRITE(*,*)'..................................................'
        WRITE(*,*)'LND # P(X>x)=', WLX1
        WRITE(*,*)'..................................................'
                
        WRITE(*,*)'..................................................'
        WRITE(*,*)'LND # P(X<x)=', WLX
        WRITE(*,*)'..................................................'
        T1=1.0/(1-WLX)
        WRITE(*,*)'RETURN PERIOD BY LOG-NORMAL DISTRIBUTION=',T1
C      .......................................................
C      * CALCULATION OF THE RERUN PERIOD BY GAMMA DISTRIBUTION
C      *.......................................................

        WRITE(*,*)'GAMA DISTRIBUTION'

C       ................................................
C       ESTIMATION OF PARAMETERS FOR GAMA DISTRIBUTION
C       .................................................
C       (i)BY THE METHOD OF MOMENTS
C       ................................................

        WRITE(*,*)'R.P BY THE METHOD OF MOMENTS'

        nu=(xm/sdx)**2

        LAMDA=xm/sdx**2

        WRITE(*,*)'nu=', nu, '   lamda=', lamda

60      CONTINUE

        wxx=0
        do 1 i=0,10000*100
        i1=i/100

        fxx= 0.01 *  (i1)**(nu-1) *  exp(-i1)
        wxx=wxx+fxx

1       continue

        gamma=nu*wxx

        write(*,*) 'gamma(nu)=', wxx,  'gamma(nu+1)=', gamma
       
        WRITE(*,*)'INPUT THE DISCHARGE FOR WHICH U WANT THE RP'
        READ(*,*) Q
        WGX=0
        DO 70 I=0,Q*100

        X1=I
        I1=(X1/100)

C       GLX=( (LAMDA**NU) * ( I1**(NU-1) )) * EXP(-LAMDA*I1)/FACT

        A1= LAMDA**(NU)
        B1= I1**(NU-1)
        C1= EXP(- (LAMDA*I1) )

        GLX=(0.01)* (A1*B1*C1)/wxx

        WGX=WGX+GLX

70      CONTINUE

        WGX1=1-WGX

        WRITE(*,*)'..................................................'
        WRITE(*,*)'GD # METHOD OF MOMENTS # P(X>x)=', WGX1
        WRITE(*,*)'..................................................'
                
        WRITE(*,*)'..................................................'
        WRITE(*,*)'GD # METHOD OF MOMENTS # P(X<x)=', WGX
        WRITE(*,*)'..................................................'

        TG=1.0/(1-WGX)

        WRITE(*,*)'RETRN PERIOD BY THE GAMMA DISTRIBUTION=', TG

C        ............................................
C       (ii) BY THE METHOD OF MAXIMUM LIKELIHOOD
C        .............................................

        WRITE(*,*)'R.P BY THE METHOD OF MAXIMUM LIKELIHOOD'

        Y1=LOG(XM)-(YT/NR)

        WRITE(*,*)'Y1=',    Y1

        NU=(0.5000876 + 0.1648852*Y1- 0.0544274*Y1**2)/Y1

        WRITE (*,*)'CORRECTED FOR THE BIAS'

        NU=NU-3*NU/NR
        
        LAMDA=NU/XM

        WRITE(*,*)'NU=', NU, '   LAMDA=', LAMDA

65      CONTINUE

        wxx=0
        do 2 i=0,10000*100
        i1=i/100

        fxx= 0.01 *  (i1)**(nu-1) *  exp(-i1)
        wxx=wxx+fxx

2       continue

        gamma=nu*wxx

        write(*,*) 'gamma(nu)=', wxx,  'gamma(nu+1)=', gamma

       
C       WRITE(*,*)'INPUT THE DISCHARGE FOR WHICH U WANT THE RP'
C       NO NEED TO READ AGAIN AS IT WILL TAKE THE Q VALUE FROM ABOVE
C       READ(*,*) Q

        WGX=0

        DO 75 I=0, Q*100

        X1=I
        I1=(X1/100)
C       GLX=( (LAMDA**NU) * ( I1**(NU-1) )) * EXP(-LAMDA*I1)/FACT

        A1= LAMDA**NU
        B1= I1**(NU-1)
        C1= EXP(-(LAMDA*I1))

        GLX=(0.01)*(A1*B1*C1)/wxx

        WGX=WGX+GLX

75      CONTINUE
            
        WGX1=1-WGX

        WRITE(*,*)'..................................................'
        WRITE(*,*) 'GD(MAX-LIKELY)# P(X>x) =', WGX1
        WRITE(*,*)'..................................................'
        
        WRITE(*,*)'..................................................'
        WRITE(*,*) 'GD(MAX-LIKELY)# P(X<x) =', WGX  
        WRITE(*,*)'..................................................'

        TG=1.0/(1-WGX)

        WRITE(*,*)'RETRN PERIOD BY THE GAMMA DISTRIBUTION=', TG

        WRITE(*,*)'THANK U :-) A SMALL PROGRAM DEVELOPED'
        WRITE(*,*) '         BY ASTROLOGER ARUN'
        END









