c     To generate serial numbers in an array

      Dimension a(100,10)

      WRITE(*,*)'INPUT THE INITIAL VALUE OF THE ARRAY'

      READ(*,*) x

      sum=0

      do 1 i=1,100
      do 2 j=1,10

      sum=sum+x

      a(i,j)=sum

2     CONTINUE
1     CONTINUE

c     WRITE(*,*) (   (a(i,j),i=1,100),     j=1,10 )

      do 3 i=1,100

      write(*,100) (a(i,j),j=1,10)

100   format( 100(10f7.0)  )

3     CONTINUE
      
      end
