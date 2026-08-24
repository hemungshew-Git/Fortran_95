c     To generate serial numbers in an array
      dimension a(10,10),b(10,10)

      WRITE(*,*)'INPUT THE INITIAL VALUE OF THE ARRAY'

      READ(*,*) x
      a(1,1)=x

       k=1
10     do 2 i=1,3
       do 3 j=1,3
      
      sum=k*a(i,j)+k*(k-1)*0.5

      k=k+1
      write(*,*) sum

3     continue
2     continue

      if (k.lt.9) goto 10

c..............................................................................
      do 6 i=1,10
      do 7 j=1,10

      b(i,j)=sum
      
7     continue
6     continue
c----------------------------------------------------------------------------
      do 8 i=1,10
c      write(*,100) (b(i,j),j=1,10)

100   format( 10(10f7.0)  )

8     CONTINUE
      
      end
