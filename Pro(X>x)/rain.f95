      program rain
      real f, t, sum
      write (*,*) 'Input value of t'
      read (*,*) t
      
      sum = 0.0
      do 10  i = 1,12
         sum = sum + f(i,t)
      write (*,*) 'month=',i,'=',sum,'inches'
10    continue
      write (*,*) 'Monthly rainfall is from i= 1(Jan) to 12(Dec)'
      write (*,*) '============================================='
      
      write (*,*) 'Annual rainfall is ', sum, 'inches'
      stop
      end
!here f menas Rain; this Rain = f(i,t); is a function depends on parameter i means month & t
 
real function f(i,t)
      f= 0.1*t*(i**2 + 14*i + 46)
      if (f .LT. 0) f = 0.0
      return
      end
