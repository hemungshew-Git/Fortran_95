C      * ............................................
c      * frequency analysis and return period       *
C      *                                            *
C      *............................................*

        PARAMETER(P=3.14159)

        REAL X,Y,XT,XTT,YT,YTT,FX,WX,WLX,XM,YM,FLX,Q1,A1,B1,C1

        REAL LAMDA,Y1,NU,wgx,glx,tg,I1,K1,X1


C       TO DECLARE LAMDA IS 'REAL' IS IMPORTANT OTHERWISE U WILL
C       NOT GET ANY VALUE OF GLX IN GAMMA DISTRIBUTION

        INTEGER T,TL,NR,GM,Q,FACT

        DIMENSION X(100),Y(100)


c       DOUBLE PRECISION FACT
C       SDXS=SAMPLE STANDARD DEVIATION
C       SDX=POPULATION STANDARD DEVIATION

        WRITE(*,*)'ENTER THE SIZE OF THE SYSTEM'

        READ(*,*) NR

C        WRITE(*,*)'THE X(NR) VALUES'

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

        WRITE(*,*)'XM=', XM, 'YM=', YM , '  XT=', XT

        XTT=0
        YTT=0

        DO 25 I=1,NR

        XTT = XTT +( X(I)-XM )**2
        YTT = YTT + ( Y(I)-YM )**2
25      CONTINUE

        WRITE(*,*)'XTT=', XTT,'YTT=',YTT

C        SDX=SQRT( ABS ( XTT/(NR-1) ))
        
C        SDY=SQRT(ABS( YTT/(NR-1) ))

        SDX=SQRT( XTT/(NR-1) )
        SDY=SQRT( YTT/(NR-1) )
        

C       NO PROBLEM EVEN IF ABSOLUTE IS NOT TAKEN

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
        DO 30 I=0, Q*100-100
C       P=3.14159, AS ALREADY DECLARED IN THE BEGINING

        X1=I
        I1=(1.0+X1/100)

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

        WLX=0

        DO 40 I=0, Q*100-100

        X1=I
        I1=(1.0+X1/100)

        
C       HERE SQRT(2*P)=2.51 USED BELOW

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
        NU=(XM/SDX)**2 

        LAMDA=XM/SDX**2

        WRITE(*,*)'NU=', NU, '   LAMDA=', LAMDA

        nu1=int(nu)+1

        FACT=nu1-1

        DO 60 I=FACT,3,-1

        FACT=FACT*(I-1)

c       WRITE(*,*)'I = ', I, '    FACT=', FACT

60      CONTINUE

        WRITE(*,*)      'FACT=',       FACT
        WRITE(*,*)'INPUT THE DISCHARGE FOR WHICH U WANT THE RP'
        READ(*,*) Q
        WGX=0
        DO 70 I=0,Q*100-100

        X1=I
        I1=(1.0+X1/100)
        

C       GLX=( (LAMDA**NU) * ( I1**(NU-1) )) * EXP(-LAMDA*I1)/FACT

        A1= LAMDA**NU
        B1= I1**(NU-1)
        C1= EXP(- (LAMDA*I1) )

        GLX=0.01*(A1*B1*C1)/FACT

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

        nu1=int(nu)+1

        FACT=nu1-1

        DO 65 I = FACT,3,-1

        FACT = FACT*(I-1)

c        WRITE(*,*)'I = ', I, '    FACT=', FACT

65      CONTINUE

        WRITE(*,*)' FACT=    '     , FACT

C        WRITE(*,*)'INPUT THE DISCHARGE FOR WHICH U WANT THE RP'

C        READ(*,*) Q

        WGX=0

        DO 75 I=1,Q*100-100

        X1=I
        I1=(1.0+X1/100)
       

C       GLX=( (LAMDA**NU) * ( I1**(NU-1) )) * EXP(-LAMDA*I1)/FACT

        A1= LAMDA**NU
        B1= I1**(NU-1)
        C1= EXP(-(LAMDA*I1))

        GLX=0.01*(A1*B1*C1)/FACT
        WGX=WGX+GLX
75      CONTINUE
              
        WGX1=1-WGX

        WRITE(*,*)'..................................................'
        WRITE(*,*)'GD(MAX-LIKELY)# P(X<x) =', WGX
        WRITE(*,*)'..................................................'


        WRITE(*,*)'..................................................'
        WRITE(*,*)'GD(MAX-LIKELY)# P(X>x) =', WGX1
        WRITE(*,*)'..................................................'

        TG=1.0/(1-WGX)

        WRITE(*,*)'RETRN PERIOD BY THE GAMMA DISTRIBUTION=', TG

        WRITE(*,*)'THANK U :-) A SMALL PROGRAM DEVELOPED'
        WRITE(*,*) '         BY ASTROLOGER ARUN'
        END









