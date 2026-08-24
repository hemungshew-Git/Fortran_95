
c *************************************************************************
c *  1) Under Continuous ponding & 2) Under Constant Rainfall Intensity(i)*       *
c *   Assuming that the iteration converges after 100 iterations          *
c *   Green-Ampt Model for calculation of Infiltration rate and amount    *
c *   (Source: Applied Hydrology by Chow/Maidement/Mays)    
c **************************************************************************
      dimension g(11,4),f(101),FA(101)

      real nue,k,theta,chi,f,inf,Ainf,ri,x1,t1,t,inf2,w1,a1,a2,F2,H1,L2
      integer m, delt,yes,n,n1

     
      open(unit= 4, file='gre.dat', status='old')
      open(unit= 14, file='gre.out', status='new')


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

      write(14,*)'For this type of soil, the input datas are: '
      WRITE(14,*)'Porosity =',NUE, 
      write(14,*)'Effective porosity = ',THETA
      write(14,*)'Wetting front soil suction head =' , CHI
      write(14,*)'Hydrulic conductivity=', K

      write(14,*)'*---------------------------------------------------*'
      write(14,*)'Calculation of cumulative infiltration & infiltration'
      write(14,*)'rate under constant ponding'
      write(14,*)' '
      write(14,*)' '
      write(14,*)'*--------------------------------------------------*'

      write(*,*)'Please Input the rainfall duration in hour'
      read(*,*) t
      write(*,*)'Input time step; eg: for every half an hour interval'
      write(*,*)'time step =30;for every 10 minute interval it is = 10'
      read(*,*)delt

      write(*,*)'Input the initial effective saturation in decimal'
      read(*,*) se      
      
      

c     ponding starts  when i=inf & then t=tp
c     i= rainfall intensity, inf=infiltration rate, tp=time of ponding
      
      


      a=chi*(1-se)*theta
c     write(14,*)'chi*deltheta=', a


      m=t*60

      do 111 j=1,m,delt
      x1=j
      t1= x1/60

      f(1)=k*t1

      do 4 i=1,100

      f(i+1)=k*t1 + a*alog(1+(f(i))/a)

4     continue
   
      w=f(101)

      inf=k*((a/w)+1)
      
      write(14,*) j, f(101), inf
111   continue

       write(14,*)'*------------------------------------------------*'
       write(14,*)'* 1st column = time t in (minute)                *'
       write(14,*)'* 2nd column = cumulative infiltration f in (cm) *'
       write(14,*)'* 3rd column = infiltration rate inf in (cm/hr)  *'
       write(14,*)'*------------------------------------------------*'

c     calculation of ponding time tp for rainfall intensity i

      write(*,*) 'Insert the Rainfall Intensity in cm/hr'
      read(*,*)ri

      tp=k*a/(ri*(ri-k))

      fp=ri*tp

      write(*,*)'---------------------------------------------------'

      tp1=tp*60

      write(14,45)  tp1
45    format(3x, 'ponding time in minutes = ' , f10.6)

      tp2=tp

      write(14,55) tp2

55    format(3x, 'The ponding time in hours = ' , f6.2)

      write(*,*)'---------------------------------------------------'
      write(14,65) fp

65    format(3x, 'The Depth of water infiltrate in cm =' , f10.6)
      

      write(14,*)'*---------------------------------------------------*'
      write(14,*)'calculation of cumulative infiltration & infiltration'
      write(14,*)'rate under constant rainfall intensity'
      write(14,*)' '
      write(14,*)' '
      write(14,*)'*--------------------------------------------------*'


      do 112 j=1,m,delt

      x1=j
      t1= x1/60

      FA(1)=k*(t1-tp)

      do 5 i=1,100

      a1=abs(a+FA(i))
      b1=abs(a+fp)


       FA(i+1)=fp + a*alog(a1/b1) + k*(t1-tp)

5      continue

c     'Calculation of Actual Infiltration Rate'

      w1=FA(101)

       Ainf=k*((a/w1)+1)


      write(14,*) j, FA(101), Ainf
112   continue

      write(14,*)'*------------------------------------------------*'
      write(14,*)'* 1st column = time t in (minutes)               *'
      write(14,*)'* 2nd column = cumulative infiltration f in (cm) *'
      write(14,*)'* 3rd column = infiltration rate inf in (cm/hr)  *'
      write(14,*)'*------------------------------------------------*'
      WRITE(*,*) 'please check the output-result in "gre.out" file'
      write(*,*)' '
      write(*,*)' '



      WRITE(*,*)'Calculation of Cumulative Infiltration & Infiltration'
      write(*,*)'rate after Rainfall for double layer soil'

      write(*,*)'Do you want to calculate it ?'
      write(*,*)'Prese 1 or any number---- IF YES----'
      read(*,*) yes

      if (yes.eq.1) goto 1001
      

1001  open(unit= 4, file='gre.dat', status='old')
      open(unit= 15, file='gre1.out', status='new')

        do 12 i = 1,11
      read(4,*) (g(i,j),j=1,4)
12    continue

      write(*,*)'Input the soil class in the lower second layer'

      write(*,*)'***********************************************'
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

      
      theta1=g(n,2)
      chi1=g(n,3)
      k1=g(n,4)

      
       a1=(1-se)*theta
       a2=(1-se)*theta1

      write(*,*)'a1.a2=', a1, a2
      
      write(*,*)'Input depth (H1) of the Upper layer of the soil'
     
      write(*,*)'Input wetting front length (L2) in the lower-layer'
      read(*,*) H1,L2


      inf2=(k*k1*(chi1+H1+L2) )/(H1*k1 + L2*k)

      F2= H1*a1 + L2*a2
      u=a2*H1*k1-a2*k*(chi1+H1)*log( 1 + (L2/(chi1+H1)) )
      t=L2*a2/k1 + u/(k*k1)


      write(15,*)'calculation of cumulative Infiltration and the '
      write(15,*)'Infiltration rate in the lower-layer'

      WRITE(15,*) 'inf2=', inf2,   'F2=',  F2, 't=', t
      
      WRITE(*,*) 'please check the output-result in "gre1.out" file'
      stop
      end


