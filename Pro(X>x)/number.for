c     To generate serial numbers in an array

      integer m,n,x
      dimension a(50,50)
      
      WRITE(*,*)'INPUT THE INITIAL VALUE OF THE ARRAY'
      write(*,*) 'Numbers of rows & Number of columns'
      READ(*,*) x,m,n

      a(1,1)=x

      do 1 i=1,m
      do 2 j=2,n

      a(1,j)=a(1,j-1)+m
      
2     continue
1     continue

      do 3 i=2,m
      do 4 j=1,n

      a(i,j)=a(i-1,j)+1

4     continue
3     continue
        
************************************************************************

      do 6 i=1,m

      write(*,100) (a(i,j),j=1,n)

100   format( 50(50f7.0)  )

6     CONTINUE
      
      end
