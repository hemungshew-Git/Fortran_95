*calculation of fourier Coefficients!equ 6.4.6 pg-247 "Hydrosystem"***********
* only the coeffi which are significantly different from zero are taken****

C       here the Qbmy using is = 1/Y * Sum (y=1 to Y)[Qb(m,y)]

        parameter (pie=3.14159)
        dimension Qbmy(12), a(6),b(6)
        
        real Q,ak,bk
        
        data Qbmy/-0.9236,-1.4974,-1.11,-0.8558,-0.6178,1.0742,2.9954,
     1             1.9242,0.10912, -0.0848, -1.0238, -1.2278/

c       calculation of fourier coefficients
       
        sum=0
        do 1 m=1,12
        Q = Qbmy(m)

        sum=sum+Q
1       continue

        a0=sum/m
        write(*,*) 'a0 = ' , a0
******************************************************           
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
        write(*,100) k, aa ,  k, bb
100     format(3X, 6('a',I2,'=', F9.5,3X,'b',I2,'=', F9.5) )

2       continue
        end
* only the coeffi which are significantly different from zero are taken****


        



