Calculation of Return Period= RP = T = (1-1/P) for ND, LND, Person 
typr III (P-III) which is also "Gamma-distri") & Log Person type-III!

here=P=P(q)=Prob(q<Q)


        dimension p(25), t(25), w(25), z(25), q(25),a(25),kt(25)
     1   ,ql(25)
        REAL a1,w1,p1,q1,t1,z1,avg,std,sk,kt1,ql1,av,st,kt

        data p/1,2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,
     1        80,85,90,95,98,99/

c       For ND frequency factor Kt=z

        write(*,*)'enter size,avg & std of the system,'
        read(*,*) n,avg,std

        do 1 i=1,n

        p1=p(i)/100
        t1=1/p1

        t(i)=t1

             if(0.0 .lt.p1.and.p1.le.0.5) then
        w1=sqrt(  alog(1/p1**2) )  

             else
        w1=sqrt( alog(1/(p1-1)**2) ) 
        endif
        
        a1=(2.51+0.802*w1+0.0103*w1**2)/(1+1.43*w1+
     1   0.189*w1**2+0.0013*w1**3)

        a(i)=a1
        z1=w1-a1
        z(i)=z1
        
        q1=avg+z1*std

        q(i)=q1
        w(i)=w1
        

1       continue
        write(*,*)'Estimation of Q in  ND '
        write(*,*)'Estimation of (Freq-factor) Kt = z(for ND)'

        write(*,*)'  RP=T   |  w(i)  |   a(i)  |     z(i)  |   q(i)  '
        write(*,*)'----------------------------------------------------'
        
        do 4 i=1,n
        write(*,100) t(i),w(i),a(i),z(i),q(i) 
100     format(1x,f6.1,3x, f5.2,3x, f7.3,5x,f7.2,7x,f8.2)

4       continue
**********************************************************************

       write(*,*)' FOR LND or FOR LP-III  Q for difernet return period
     1 (T) or Probability (P) '

       write(*,*)'For "log normal distribution (LND)" PUT Skew = 0 '
       write(*,*)'For "gamma distribution which is same as Person-III
     1 type, PUT Coeffi of Skew of normal data & for log person-III put
     1 skew of the transformed log data '

       write(*,*)'Enter log-avg,log-std & skew of the data-system,'

       read(*,*) avg,std,sk
       c=sk/6

       do 2 i=1,n
       
c      z1=z(i) ; if you cancel this statement then the last value
c      from the previous loop will take in the below equation!! so

        z1=z(i)

        kt1=z1+(z1**2-1)*c+0.3333*(z1**3-6*z1)*c**2 -
     1       (z1**2-1)*c**3 +z1*c**4+0.33333*c**3

        ql1=avg+kt1*std
************************************************************************
C        ql1=10**ql1 -----use this if u take log base 10 of the data set
c        ql1=exp(ql1)-----use this if u take log base e
*************************************************************************
        ql1=exp(ql1)


        kt(i)=kt1
        ql(i)=ql1

2       continue

c      earlier like p(i), t(i), w(i) etc, I am not declaring kt(i) as real
c      and so the problem!! and kt(i) is written as integer!!

        write(*,*)'(Freq-factor)Kt || Estimated Q in  LND or in LP-III'
        write(*,*) ' (1-1/T) * 100 = Prob ( q > Q ) '

        write(*,*)'RP=T||P(q>Q)|| Kt || Est. Q in [ LND/LP-III/LP-III]'

        write(*,*)'---------------------------------------------------'

        do 3 i=1,n

        write(*,500) t(i), p(i), kt(i),ql(i)
500     format(1x,f5.1, 1x,'||', 1x, f 5.2, 1x,'||', 1x, f7.3, 3x, '||'
     1   , 6x, f15.2 )
      
3       continue

        end
