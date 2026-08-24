*calculation of Qb(m)f using fourier series! equ 6.4.6 pg-247 "Hydrosystem"***********

C        Qbmy = 1/Y * Sum (y=1 to Y)[Qb(m,y)]

        parameter (pie=3.14159)
        dimension Qbmy(12), a(6),b(6),Qf(72)
        
        real Q

        data Qbmy/-0.9236,-1.4974,-1.11,-0.8558,-0.6178,1.0742,2.9954,
     1             1.9242,0.10912, -0.0848, -1.0238, -1.2278/

c       calculation of fourier coefficients
       
        sum=0
        do 1 m=1,12
        Q = Qbmy(m)

        sum=sum+Q
1       continue

        a0=sum/m
        write(*,*) a0
*******************************************************************************           

        do 2 k=1,6

        sum1=0
        sum2=0

        do 3 m=1,12

        Q = Qbmy(m)

        sum1=sum1 + Q * cos( 2*pie*k*m/12 )
        sum2=sum2 + Q * sin( 2*pie*k*m/12 )

3       continue

        aa= sum1/6 
        bb= sum2/6 
c        write(*,*) aa, bb

        a(k)=aa
        b(k)=bb

2       continue
******************************************************************************  
* Regeneration of Qb(m)f i.e [Qb(m) by fourier series coefficients]*******
* only the coeffi which are significantly different from zero are taken****

        do 4 k=1,6
        ak=a(k)
        bk=b(k)

        if ( abs(ak).lt.(0.15) ) then
        ak=0
        endif

        if (abs(bk).lt.(0.15) ) then
        bk=0
        endif

        write(*,*) ak, bk

        a(k)=ak
        b(k)=bk

4       continue
******************************************************************************
c        do 5 k=1,6
c        as=a(k)
c        bs=b(k)
c        write(*,*) as, bs
c5      continue
*****************************************************************************
        do 6 m=1,12

        Qbmf=0
        do 7 k=1,6

        as=a(k)
        bs=b(k)
        a0=0

       Qbmf=Qbmf+a0*cos(2*pie*k*m/12)+as*cos( 2*pie*k*m/12 )
     1       +bs*sin( 2*pie*k*m/12 )

c----------------------------------------------------------------------------
c      write(*,*) as,bs
c      it will write the screend values of coefficients, ak & bk!
c      as under loop so repeated way total=m*k=72 Nos of as and 72 Nos of bs!
c      now if you write(*,*) as, bs outside loop k(i.e outside continue No:7)
c      in that case only the last value of as & bs will write! total 12+12)
c      a0=0 as its value is less then 0.15
c----------------------------------------------------------------------------
c        write(*,*) Qbmf
c        i=m*k
c        Qf(i)=Qbmf
c----------------------------------------------------------------------------
c       Qf(i)=Qbmf ; here i=m*k=12*6=72 Nos of Qbmf will get!
c       which are not needed so haphazard way! so screening is done to avoid
c       repeated values for each k below!
c-----------------------------------------------------------------------------



7      continue
c       write(*,*) as,bs
      
       write(*,100)  Qbmf

100    format(3x,'Qbmf=',12f7.4)

6      continue
       end
