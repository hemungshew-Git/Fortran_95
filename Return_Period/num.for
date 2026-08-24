c     To write a 2-D ARRAY into single 1-D array

      Dimension a(10,10)

      open(unit= 4, file='num.dat', status='old')
      open(unit= 5, file='num.out', status='new')
      do 1 i = 1,4

      read(4,*) (a(i,j),j=1,4)
1     continue

      do 2 i=1,4
      do 3 j=1,4

c     To write row-wise

      a1=a(i,j)

      write(5,100) a1

100   format(f7.2 )


3     continue
2     continue
c-------------------------------------------------------

      do 4 i=1,4
      do 5 j=1,4

c     To write column-wise

      a2=a(j,i)

      write(5,101) a2
101   format(10x,f7.2 )

5     continue
4     continue
      
      end
