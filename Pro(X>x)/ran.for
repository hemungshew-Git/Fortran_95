      dimension a(100), b(10,10)
      real x
      write(*,*) 'input the initial number'
      read(*,*) x

      do 1 i=1,100
      sum=x+i
      write(*,*) sum

c     array reference not allowed come if you give this below
c     a=sum

      a(i)=sum
1     continue
c------------------------------------------------------
      i=1
      do 44 j=1,10
      do 55 k=1,10

      b(j,k) = a(i)

      i=i+1
55    continue
44    continue



      do 2 j=1,10
      write(*,4)(b(j,k),k=1,10)
4     format(3x,10f6.2)
2     continue

      stop
      end
