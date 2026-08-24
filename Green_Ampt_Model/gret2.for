
c *************************************************************************
c *   Green-Ampt Model for calculation of Infiltration rate & the amount  *
c *   Assuming that the iteration converges after 100 iterations          *

c *   (Source: Applied Hydrology by Chow/Maidement/Mays)

c **************************************************************************
      dimension g(11,4),ri(10),IR(10),F(10),inf(10),R(10)

      real nue,k,theta,chi,                            IR
      integer n

     
      open(unit= 4, file='gre.dat', status='old')
      open(unit=7, file='rain.dat', status='old')
      open(unit= 14, file='gre.out', status='new')

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

      write(*,*) 'Initial satutration'
      read(*,*) se

      write(14,*)'The input datas are: '
      write(14,*)' '
      WRITE(14,*)'Porosity                               =',NUE
      write(14,*)'Effective porosity                     =',THETA
      write(14,*)'Wetting front soil suction head        =', CHI
      write(14,*)'Hydrulic conductivity (K)              =', K
      write(14,*)'initial effective saturation           =', se
      write(14,*)' '
      write(14,*)'---------------------------------------------------'

         
      a=chi*(1-se)*theta
      write(*,*)'chi*deltheta=', a

      write(14,*)' '
      write(14,*)' '
      write(14,*)'*--------------------------------------------------*'
      write(14,*)'Case 4 : Under varying Rainfall Intensity(i)'
      write(14,*)'*-------------------------------------------------*'
      
      write(*,*)'Input the total duration of rainfall in hour'
      read(*,*) t
      write(*,*)'Input the time step in which IRA is measured'
      read(*,*)  delt

      m = t*60/delt
    
      R(1)=0




      do 77 i=1,m

      read(7,*) IR(i)


      R(i+1)= R(i) + IR(i)
      ri(i)=IR(i)*60/delt

77    continue
c-----------------------------------------------------------------------
      write(14,*)'The "Incremental rainfall amount (IRA)" '
      write(14,*)'in (cm) measured in equal time steps within the '
      write(14,*)'entire duration of rainfall are::'
      write(14,*)' ' 
      write(14,*) IR(i)

      write(14,*)'The calculated  rainfall intensity are ::'
      write(14,*)' ' 
      write(14,*)(ri(i), i=1,m)
      write(14,*)' '
      write(*,*) (R(i+1), i=1,m)
c------------------------------------------------------------------------


      F(1)=0

      do 78 i=1,m
      
      F(i+1)= F(i) + k*delt/60
c---------------------------------------------------------------      
      inf(i+1)=k*( (a/(F(i+1) ) )+1)

      
      if ( inf(i+1).gt.ri(i) ) then
      F(i+1)= R(i+1)
      endif

      if ( inf(i+1).lt.ri(i) ) then
      WRITE(14,*) i
      endif

78    continue

      write(14,*) ( F(i+1), i=1,m)
      
      
      write(14,*)'a small program by Mr. A. Borgohain '
      write(14,*)'------------------------------------thanks!'

      write(*,*)'See the results in the output file: [ gre.out ]'
      stop
      end


