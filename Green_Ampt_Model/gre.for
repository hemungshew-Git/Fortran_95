c **********************************************************************
c
c *   Assuming that the iteration converges after 100 iterations       *
c *   Green-Ampt Model for calculation of Infiltration rate and amount *
c
c **********************************************************************
      dimension g(11,4),f(101),FA(101)

      real nue,k,theta,chi,f,inf,ri
      open(unit= 4, file='gre.dat', status='old')

      do 1 i = 1,11
      read(4,*) (g(i,j),j=1,4)
1     continue

      write(*,*) 'please insert the soil class'

      write(*,*)'********************************'
      write(*,*)'TYPE 1 (for sand)'                   
                                                    
      write(*,*)'TYPE 2 for loamy sand'             
                                                    
      write(*,*)'TYPE 3 for sandy loam'             
                                                    
      write(*,*)'TYPE 4 for loam'                   
                                                    
      write(*,*)'TYPE 5 for silt loam'              
                                                    
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

      write(*,*)'rainfall duration in hour and initial effective
     1 saturation in decimal'
      read(*,*) t,se

      a=chi*(1-se)*theta
c      write(*,*)'chi*deltheta=', a

      f(1)=k*t

      do 4 i=1,100

      f(i+1)=k*t+a*alog(1+(f(i))/a)

4     continue

      write(*,*)'------------------------------------------------------'
      write(*,*)'After successive substitution, the infiltrantion
     1 depth or the cumulative infiltration in cm=', f(101)
      write(*,*)'------------------------------------------------------'

c     calculation of infiltration rate
      w=f(26)
      inf=k*((a/w)+1)
      write(*,*)'------------------------------------------------------'
      write(*,*)'infiltrantion rate in cm/hr=', inf
      write(*,*)'------------------------------------------------------'

c     calculation of ponding time tp for rainfall intensity i
      write(*,*) 'Insert the Rainfall Intensity in cm/hr'
      read(*,*)ri

      tp=k*a/(ri*(ri-k))
      fp=ri*tp
      write(*,*)'------------------------------------------------------'
      write(*,*) 'ponding time in hour=', tp
      write(*,*)'------------------------------------------------------'
      write(*,*) 'depth of water infiltrate in cm=', fp
      write(*,*)'------------------------------------------------------'

c     calculation of actual infiltration rate

      FA(1)=k*(t-tp)

      do 5 i=1,100
      a1=abs(a+FA(i))
      b1=abs(a+fp)

c     FA(i+1)=fp + a*alog( abs(a-FA(i))/(abs(a-fp) ) ) + k*(t-tp)

      FA(i+1)=fp + a*alog(a1/b1) + k*(t-tp)
5     continue
      write(*,*)'------------------------------------------------------'

      write(*,*)'After Successive Substitution, the Actual Infiltration
     1 Amount or the Cumulative Depth in cm=', FA(101)
      write(*,*)'------------------------------------------------------'

c     calculation of actual infiltration rate
      w1=FA(101)
      Ainf=k*((a/w1)+1)
      write(*,*)'------------------------------------------------------'
      write(*,*)'Actual infiltrantion rate in cm/hr=', Ainf
      write(*,*)'------------------------------------------------------'
      stop
      end
