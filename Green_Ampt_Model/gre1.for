
c **************************************************************************
c *  (Source: Applied Hydrology by Chow/Maidement/Mays)                    *                                                *
c *     example 4.3.1, 4.4.1, 4.4.2,      pg 116-120                       *
c *  Green-Ampt Model for calculation of Infiltration rate and amount      *
c                                                                          *
c **************************************************************************
      dimension g(11,4),f(101),FA(101)

      real nue,k,theta,chi,f,inf,ri,tp,tp2
      

      open(unit= 4, file='gre.dat', status='old')

      do 1 i = 1,11
      read(4,*) (g(i,j),j=1,4)
1     continue

      write(*,*) 'please insert the soil class'

      write(*,*)'********************************'
      write(*,*)'type 1 (for sand)'                   
                                                    
      write(*,*)'type 2 for loamy sand'             
                                                    
      write(*,*)'type 3 for sandy loam'             
                                                    
      write(*,*)'type 4 for loam'                   
                                                    
      write(*,*)'type 5 for silt loam'              
                                                    
      write(*,*)'type 6 for sandy clay loam'        
                                                    
      write(*,*)'type 7 for clay loam'              
                                                    
      write(*,*)'type 8 for silty clay loam'        
                                                    
      write(*,*)'type 9 for sandy clay'             
                                                    
      write(*,*)'type 10 for silty clay'            
                                                    
      write(*,*)'type 11 for clay'                  

      write(*,*)'******************************'

      read(*,*) n

      nue=g(n,1)
      theta=g(n,2)
      chi=g(n,3)
      k=g(n,4)

c      WRITE(*,*) NUE, THETA,CHI,K

      write(*,*)'Please Input the rainfall duration in hour'
      write(*,*)'And the initial effective saturation in decimal'
      read(*,*) t,se

      a=chi*(1-se)*theta
c     write(*,*)'chi*deltheta=', a

       i = 0

42     i = i + 1
             
      f(1)=k*t

      f(i+1)=k*t+a*alog(1+(f(i))/a)

      if (abs(f(i) - f(i+1)).gt.0.001) goto 42

      write(*,*)'------------------------------------------------------'

      write(*,*)'After successive substitution, the solution converges'
      write(*,*)'in iteration no =  ' ,  i
      write(*,40) f(i)
40    format(3x, 'The Cumulative Infiltration Depth in cm = '  , f6.3)

      write(*,*)'------------------------------------------------------'

c     calculation of infiltration rate
      w=f(i)
      inf=k*((a/w)+1)
      write(*,*)'------------------------------------------------------'
      write(*,80) inf
80    format(3x, 'infiltrantion rate in cm/hr = ' , f10.6)


      write(*,*)'------------------------------------------------------'

c     calculation of ponding time tp for rainfall intensity i
      write(*,*) 'Insert the Rainfall Intensity in cm/hr'
      read(*,*)ri

      tp=k*a/(ri*(ri-k))

      fp=ri*tp

      write(*,*)'------------------------------------------------------'

      tp1=tp*60
      write(*,45)  tp1
45    format(3x, 'ponding time in minutes = ' , f10.6)
      tp2=tp
      
      write(*,55) tp2
55    format(3x, 'The ponding time in hours = ' , f6.2)

      write(*,*)'------------------------------------------------------'
      write(*,65) fp
65    format(3x, 'The Depth of water infiltrate in cm =' , f10.6)
      inf=ri

      write(*,75) inf
      write(*,*)'Here Infiltration Rate equals to Rainfall Intensity' 
75    format(3x, 'Rate of infiltration in cm/hr=', f6.3)

      write(*,*)'------------------------------------------------------'


      WRITE(*,*) 'Calculation of Actual Infiltration after Rainfall'

      write(*,*)'Do you want to calculate it ?'
      write(*,*)'Prese 1 or any number---- IF YES----'
      read(*,*) yes

      if (yes.eq.1) goto 1000
      

1000      FA(1)=k*(t-tp)


       i = 0

41     i = i + 1

      a1=abs(a+FA(i))
      b1=abs(a+fp)

c     FA(i+1)=fp + a*alog( abs(a-FA(i))/(abs(a-fp) ) ) + k*(t-tp)

      FA(i+1)=fp + a*alog(a1/b1) + k*(t-tp)

      if (abs(FA(i) - FA(i+1)).gt.0.001) goto 41

          
      write(*,*)'------------------------------------------------------'
      write(*,*)'After successive substitution the solution converges'

      write(*,*)'in iteration no =' , i

      write(*,90) FA(i)
      
90    format(3x,'The Cumulative Infiltration Depth in cm =', f10.6)
      write(*,*)'------------------------------------------------------'

c     'Calculation of Actual Infiltration Rate'
       
      w1=FA(i)
      Ainf=k*((a/w1)+1)

      write(*,*)'------------------------------------------------------'

      write(*,100) Ainf

100   format(3X, 'Actual Infiltrantion Rate in cm/hr  =' ,  f10.6)

      write(*,*)'------------------------------------------------------'
    
      stop
      end


