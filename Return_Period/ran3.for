c     Generating 100 R.N  and place them in a 2-D Arrary

      dimension a(25),b(5,5)

      write(*,*)'input a number after which all other sl-number comes'
      read(*,*) x

      do 6 i = 1,25
      sum=(i+x)

      write(*,8) sum

      a(i)=sum
8     format(f6.1)

6     continue
c-------------------------------------
      
      i=1

      do 3 j = 1,5
      do 4 k = 1,5
      
      b(j,k)=a(i)

      i=i+1      

4     continue
3     continue
c---------------------------------------------------

      write(*,*) 'now the matrix'

      do 2 i = 1,5

      write(*,5)(b(i,j),j=1,5)
5     format(5(f6.1))

2     continue
      stop
      end
      
      
