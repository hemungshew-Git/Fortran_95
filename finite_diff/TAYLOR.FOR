
c
c     solution of initial boundary value problem:
c      Dy = xy^(1/3), y(1)=1 by Taylor method of 4.order
c
	use dfport
      implicit double precision (a-h,o-z)
      double precision abcissae(0:1000),ord_taylor(0:1000),
     +                 ord_egzact(0:1000),ord_abserr(0:1000),
     +                 ord_relerr(0:1000),ord_euler(0:1000)
      write(*,*) ' Starting point: (x0,y0)'
      read(*,*) x0,y0
      write(*,*) ' h :'
      read(*,*) h
      write(*,*) ' number of points:'
      read(*,*) n
c
c     Numerical solution
c
      call taylor (x0,y0,h,n,abcissae,ord_taylor)
c
c     Egzact solution
c
      call exact(x0,h,n,ord_egzact)
      open (10, file='taylor1.dat',form='formatted')
      rewind 10
      do, i=0,n
          write(10,'(3(1x,f20.15) )')
     +         abcissae(i),ord_taylor(i),ord_egzact(i)
      enddo
      close (10)
c
c     Analyzing the computed solution
c     (abs.err., rel.err)
c
      do, i=0,n
          ord_abserr(i)=ord_egzact(i)-ord_taylor(i)
          ord_relerr(i)=ord_abserr(i)/ord_egzact(i)
      enddo
      open (20, file='taylor2.dat',form='formatted')
      rewind 20
      do, i=1,n
          write(20,'(3(1x,f20.15) )')
     +         abcissae(i),ord_abserr(i),ord_relerr(i)
      enddo
      close (20)

c
c     Numerical solution
c
      call euler(x0,y0,h,n,abcissae,ord_euler)
c
c     Egzact solution
c
      call exact(x0,h,n,ord_egzact)
      open (10, file='euler1.dat',form='formatted')
      rewind 10
      do, i=0,n
          write(10,'(3(1x,f20.15) )')
     +         abcissae(i),ord_euler(i),ord_egzact(i)
      enddo
      close (10)
c
c     Analyzing the computed solution
c     (abs.err., rel.err)
c
      do, i=0,n
          ord_abserr(i)=ord_egzact(i)-ord_euler(i)
          ord_relerr(i)=ord_abserr(i)/ord_egzact(i)
      enddo
      open (20, file='euler2.dat',form='formatted')
      rewind 20
      do, i=1,n
          write(20,'(3(1x,f20.15) )')
     +         abcissae(i),ord_abserr(i),ord_relerr(i)
      enddo
      close (20)


      i=system("c:\num_metode_fizike\gnuplot\wgnupl32.exe tayeul.dem")
	IF( I .EQ. -1 ) STOP 'Could not spawn gnuplot program'
      end
      subroutine taylor(x0,y0,h,n,abcissae,ordinatae)
c
c     INPUT:
c     (x0,y0) = starting point
c     h       = net spacing
c     n       = number of points (i=0,1,...n)
c
c     OUTPUT:
c     (abcissae,ordinatae) = solution of difequ
c
c     TEMPORARY STORAGE:
c     deriv = solution derivatives up to order 4,
c             array of dimension 5
c
      implicit double precision (a-h,o-z)
      double precision abcissae(0:n),ordinatae(0:n)
      double precision derivative(0:4)
      x=x0
      y=y0
      abcissae(0)=x0
      ordinatae(0)=y0
c     computing solution at the next step by Horner scheme
      do, i=1,n
c         computing derivatives of the solution
          call deriv(x,y,derivative)
          nfac=24
          y=derivative(4)/dble(nfac)
          do, j=3,0,-1
              nfac=nfac/(j+1)
              y=y*h+derivative(j)/dble(nfac)
          enddo
          x=x0+h*dble(i)
          abcissae(i)=x
          ordinatae(i)=y
      enddo
      return
      end
      subroutine deriv(x,y,derivative)
c     INPUT:
c     x = evaluating point
c     OUTPUT:
c     derivative = an arrray (0:4) containing derivatives
c                  of the solution
c
      implicit double precision (a-h,o-z)
      double precision derivative(0:4)
      if (dabs(y).le.1.d-12) then
          write(*,*) ' y should never be close to 0'
          stop
      endif
      tmp=y**(1.d0/3.d0)
      derivative(0)=y
      derivative(1)=x*tmp
      derivative(2)=1.d0/3.d0*x*x/tmp+tmp
      derivative(3)=-1.d0/9.d0*x*x*x/y+x/tmp
      derivative(4)=1.d0/9.d0*x*x*x*x/(tmp**5)
     +              -2.d0/3.d0*x*x/y+1.d0/tmp
      return
      end
      subroutine exact(x0,h,n,ordinatae)
      implicit double precision (a-h,o-z)
      double precision ordinatae(0:n)
c
c     The exact solution of Dy = xy^(1/3), y(1)=1
c     tabulated from x0, with mesh h in 0:n points
c
      do, i=0,n
          x=x0+dble(i)*h
          ordinatae(i)=((x**2+2.d0)/3.d0)**(3.d0/2.d0)
      enddo
      return
      end

