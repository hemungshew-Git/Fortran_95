
c *************************************************************************
c *   Green-Ampt Model for calculation of Infiltration rate & the amount  *
c *   Assuming that the iteration converges after 100 iterations          *

c *   (Source: Applied Hydrology by Chow/Maidement/Mays)

c **************************************************************************
      dimension g(11,4),f(101),FA(101)

      real nue,k,theta,chi,f,inf,ri,x1,t1, t,infp
      integer m, delt

     
      open(unit= 4, file='gre.dat', status='old')
      open(unit= 14, file='green.out', status='new')

      write(14,*)'****************************************************'
      write(14,*)'* Green-Ampt Model for Calculation of Infiltration *'
      write(14,*)'* rate & the amount under various scenarios        *'
      write(14,*)'****************************************************'
      
      write(14,*)'(Source: Applied Hydrology by Chow/Maidement/Mays) '
      write(14,*)'----------------------------------------------------'

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
      write(14,*)' '

      write(*,*)'Insert the Rainfall Intensity (i) in cm/hr'
      read(*,*) ri

201   if (ri.lt.k) then
      
      write(*,*)'Input value of i must > k (hydraulic conductivity)'
      write(*,*)'In Green-Ampt method, ponding time (tp) is posative'
      write(*,*)'& finite only if i > k'

      write(*,*)'Insert a greater value of Rainfall Intensity in cm/hr'
      read(*,*) ri
      goto 201
      endif

      if (ri.gt.k) then
      goto 202
      endif

202   write(*,*)'Please Input the rainfall duration in hour'
      write(*,*)'eg: for half an  hour rainfall duration write 0.5;'
      write(*,*)'for 5 hrs rainfall write 5  '
      read(*,*) t
      write(*,*)'Input Time Step; eg: for every half an hour interval'
      write(*,*)'time step= 30; for every 10 minute interval it is= 10'
      write(*,*)'for every 2 hours interval it is = 2*60 = 120 '
      read(*,*)delt
      write(*,*)'Input the initial effective saturation in decimal'
      read(*,*) se      
   
      write(14,*)'The input datas are: '
      write(14,*)' '
      WRITE(14,*)'Porosity                               =',NUE
      write(14,*)'Effective porosity                     =',THETA
      write(14,*)'Wetting front soil suction head        =', CHI
      write(14,*)'Hydrulic conductivity (K)              =', K
      write(14,*)'rainfall intensity    (i) in cm/hr     =', ri
      write(14,*)'rainfall duration     (t) in hours     =', t
      write(14,*)'time steps      (delta t) in minutes   =', delt
      write(14,*)'initial effective saturation           =', se
      write(14,*)' '
      write(14,*)'---------------------------------------------------'

         
      a=chi*(1-se)*theta
c      write(14,*)'chi*deltheta=', a

      write(14,*)'Case 1: Under Constant Ponding'
      write(14,*)' '
      write(14,*)'*-------------------------------------------------*'
      write(14,*)'* 1st column = t (rainfall duration) in hours     *'
      write(14,*)'* 2nd column = t (rainfall duration) in minutes   *'               
      write(14,*)'* 3rd column = f (cumulative infiltration) in cm  *'
      write(14,*)'* 4th column = inf (infiltration rate) in cm/hr   *'
      write(14,*)'*-------------------------------------------------*'
      
c      dimension f(101),FA(101)
c      real t,x1,t1, t,infp, f,FA
c      integer j, m, delt
       
c------------------------------
       m=t*60

      do 111 j=1,m
      x1=j
      t1= x1/60
c-----------------------------
      
      f(1)=k*t1

      do 4 i=1,100

      f(i+1)=k*t1 + a*alog(1+(f(i))/a)

4     continue
   
      w=f(101)

      inf=k*((a/w)+1)
      if ( mod (j,delt).eq.0) then
      
      write(14,89) t1, j, f(101), inf
89    format(3x,f5.2,3x,I5,3X,f10.4,3X,f10.4)

      endif

111   continue

      write(14,*)'---------------------------------------------------'
      write(14,*)' '

      write(14,*)'*--------------------------------------------------*'
      write(14,*)' Case2 : When ponding just starts '
      write(14,*)'*--------------------------------------------------*'
      write(14,*)'*                                                  *'
      write(14,*)'*   t  = Rainfall duration                         *'
      write(14,*)'*   tp = Time required for ponding to occur after  *'
      write(14,*)'*        rain starts at t = 0;                     *'
      write(14,*)'*   fp = Infiltrate depth at ponding in cm         *'
      write(14,*)'* infp = Infiltrate rate at ponding in cm/hr       *'
      write(14,*)'*--------------------------------------------------*'

      tp=k*a/(ri*(ri-k))

      fp=ri*tp
      infp=ri

      tp1=tp*60

      write(14,*)' '
      
      write(14,45) tp1
45    format(5x, 'tp    in minutes     =   ' , f10.6)

      tp2=tp
      write(14,55) tp2
55    format(5x, 'tp    in hours       =   ' , f6.2)
      write(14,65) fp

65    format(5x, 'fp    in cm          = ' , f10.4)

      write(14,70) infp

70    format(5x, 'infp  in cm/hr       = ' , f10.4)

      write(14,*)'*------------------------------------------------*'
      write(14,*)' '
      write(14,*)'Note:'
      write(14,*)' '
      write(14,*)'Please, check the values of (fp & infp) as obtained'
      write(14,*)'above [in Case 2] against the results below       '
      write(14,*)'[in Case 3] in (cloumn 3 & column 4), when t = tp;'              '

      write(14,*)' '
      write(14,*)' '
      write(14,*)'*--------------------------------------------------*'
      write(14,*)'Case 3 : Under Constant Rainfall Intensity(i)'
      write(14,*)'*-------------------------------------------------*'
      write(14,*)'* 1st column = t (rainfall duration) in hours     *'
      write(14,*)'* 2nd column = t (rainfall duration) in minutes   *'               *'
      write(14,*)'* 3rd column = f (cumulative infiltration) in cm  *'
      write(14,*)'* 4th column = inf (infiltration rate) in cm/hr   *'
      write(14,*)'*-------------------------------------------------*'
       write(14,*)' '

      do 112 j=1,m

      x1=j
      t1= x1/60

      FA(1)=k*(t1-tp)

      do 5 i=1,100

      a1=abs(a+FA(i))
      b1=abs(a+fp)


       FA(i+1)=fp + a*alog(a1/b1) + k*(t1-tp)
5      continue

      w1=FA(101)

       Ainf=k*((a/w1)+1)

      if ( mod (j,delt).eq.0) then

      write(14,88) t1, j, FA(101), Ainf
88    format(3x,f5.2,3x,I5,3X,f10.4,3X,f10.4)
      endif
112   continue

      write(14,*)'*------------------------------------------------*'

      write(14,*)' '
      write(14,*)'Some Observations are : '
      write(14,*)' '
      write(14,*)'When ponding just starts(at t= tp): '
      write(14,*)'----------------------------------- '
      write(14,*)'infp = inf = i   &   fp = f [as in Case 2 & Case 3]'
      write(14,*)'where i= input rainfall intensity.'
      write(14,*)' '
      write(14,*)' '
      write(14,*)'Any (-) ve values of (f &/or inf), in the begining'
      write(14,*)'time steps, indicates "No infiltration" '
      write(14,*)' '
      write(14,*)'Like rate & duration of rainfall(as input data), the'
      write(14,*)'rate & duration of irrigated also may be considered.'
      write(14,*)' '
      write(14,*)'*------------------------------------------------*'
      write(14,*)' '
      write(14,*)' '
      write(14,*)'Some factors overlooked here are :'
      write(14,*)' '
      write(14,*)'Vegetation cover, Air confinement ahead of the'  
      write(14,*)'wetting front, Surface roughness, Plant roots etc;' 
      write(14,*)' '
      write(14,*)'these retard the flow & creat additional pore spaces'
      write(14,*)'so infiltration & K (Hydraulic conductivity) of'
      write(14,*)'the soil increases!'
      write(14,*)' '
      write(14,*)'a small program by Mr. A. Borgohain '
      write(14,*)'------------------------------------thanks!'

      write(*,*)'See the results in the output file: [ green.out ]'


      stop
      end


