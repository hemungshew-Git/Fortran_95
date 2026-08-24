      dimension g(11,4),f(26)

      real nue,k,theta,chi,f
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


      t=1.0
      se=0.3

      a=chi*(1-se)*theta
      write(*,*) a

      f(1)=k*t

      do 4 i=1,25

      f(i+1)=k*t+a*alog(1+(f(i))/a)

4     continue

      write(*,*)f
      
      stop

      end
