c       Treatment of Zeros in ND, LND using mean & std without zero values
c       e.g- if ND=75, N=20 THEN use avg and std of remaining 55 values!

        PARAMETER(P=3.14159)
        INTEGER A,B
        REAL wx,fx,wx1,xm,sdx,ym,sdy,k,kz,x,nd,n,i1,cov
        real wlx,flx,wlx1

C       CALCULATION OF CUMULATIVE F(Z) BY NORMAL DISTRIBUTION

       WRITE(*,*) 'TYPE THE VALUE OF A & B SEPARATED BY A COMMA'
        
        READ(*,*) A,B

        WRITE(*,*) 'TYPE size,avg,std & no of zeros'

        READ(*,*) nd, xm,sdx,n
        kz=(nd-n)/nd
        WX=0

        DO 20 I=A*100,B*100

        X=I

        I1=(X/100)

C       IN AN INTERVAL OF 0.01; WHICH BEING MULTIPLIED TO FX

        k=1/( sdx*SQRT(2*P)  )

        FX=(0.01)* k * EXP(- (I1-xm)**2/(2*sdx**2)  )
                
        wx = wx + fx

        WRITE(*,*)'I1=', I1,  'fraction-P(X<=x)=' ,  wx
        
20      continue

        wx=wx

        wx1=(1-wx)

        wxz=1-kz*(1-wx)
             

        WRITE(*,*)'******************************************'
        WRITE(*,*)'  PROBABILITY(X<=x) =  ',  WX

        WRITE(*,*)'  PROBABILITY(X>x) =  ',  wx1

        WRITE(*,*)'  Prob(X<=x) taking zeros into consideraion = ',  WX
**************************************************************************
C       CALCULATION OF CUMULATIVE F(Z) BY LND-pg 107,140,147

        WRITE(*,*) 'TYPE THE VALUE OF A & B SEPARATED BY A COMMA'
        
        READ(*,*) A,B

        cov=sdx/xm

        ym=0.5*alog(xm**2/(cov**2+1) )
        sdy=sqrt ( alog(cov**2+1) )

        wlx=0
        
        DO 30 i=A*100,B*100

        x=i/100

        i1=alog(x)

C       IN AN INTERVAL OF 0.01; WHICH BEING MULTIPLIED TO FX


        k=1/(x*sdy*SQRT(2*P)  )

        flx=0.01 * k * EXP(- (i1-ym)**2/(2*sdy**2)  )
                
        wlx = (wlx + flx)

        WRITE(*,*) I1, wlx
C125     format(2x,'I1=', F7.4, 2X,'fraction-P(X<=x)=', F7.4)        

30      continue

        wlx1=(1-wlx)

        wlxz=1-kz*(1-wlx)
        wlxz1=1-wlxz        

        WRITE(*,*)'******************************************'
        WRITE(*,*)'  PROBABILITY(q<=Q) =  ', wlx

        WRITE(*,*)'  PROBABILITY(q>Q) =  ',  wlx1

        WRITE(*,*)'  Prob(q>Q) taking zeros into consideraion=', wlxz

        WRITE(*,*)'  Prob(q<=Q) taking zeros into consideraion=', wlxz1


        END

