      real x, f

      integer k, n

      parameter ( n = 10 )

      write(*,*) 'Input value of X'

      read(*,*) x

      open (UNIT= 3, FILE ='RESULT.OUT', STATUS ='NEW')

      write(3,*) '   OUTPUT OF SAMPLE PROGRAM'

      write(3,*)   '       x                f   '
                
c     [count or key] = k

      k = 0

100   f = x**2

      write(3,*) x, f

      x=x+1

      k=k+1

      if(k.lt.n) go to 100

      stop
      end

* -------------------------------------------------------- *
