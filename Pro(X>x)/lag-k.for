c         ***Auto correlation coefficients****

c         Measure of Persistancy/Consistency of observation
c         It can be identified by measuring the "Auto correlation
c         coefficients" of different time lags!

          dimension x(100),summx(10),summy(10),summxy(10)
          real cor1,sumxx,sumyy,sumxy

          write(*,*)'input total no of variables in the data series='
          read(*,*) n

          OPEN(UNIT= 4, FILE='lag-k.dat', STATUS='OLD')
c            OPEN(UNIT= 3, FILE='lag-k.out', STATUS='NEW')

          do 1 i=1, n
          read(4,*) x(i)
1         continue
c-----------------------------------------------------------------------------
c         calculation of avg
          sum=0
          do 2 i=1,n
          sum=sum+x(i)
2         continue

          avg=sum/n
          write(*,*) 'avg='   ,  avg
c----------------------------------------------------------------------------
c          write(*,*)'input lag no! for lag-1 value of k=1'
c          read(*,*) k
c---------------------------------------------------------------------------
          do 4 k=1,10

          sum2x=0
          sum2y=0
          sumxy=0
          
          do 3 i=1, n-k
                     
          sum2x=sum2x + (x(i)-avg)**2
          sum2y=sum2y + (x(i+k)-avg)**2
          sumxy=sumxy + (x(i) - avg)*(x(i+k)-avg)
3         continue

          summx(k)=sum2x
          summy(k)=sum2y
          summxy(k)=sumxy
          
4         continue

          do 5 k=1,10
          sum2x=summx(k)
          sum2y=summy(k)
          sumxy=summxy(k)

          cor1=sumxy/sqrt(sum2x*sum2y)
c         write(*,*) sumxy, sum2x, sum2y
          write(*,*) 'Corr-Coeificient-lag',k, cor1
5         continue                
          end
c----------------------------------------------------------------------------
          

