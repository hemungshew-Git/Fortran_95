

c                     """"""""""""""""""""""""""""""""""""""""""""
C                        DESIGN OF CANAL BY LEAST-COST METHOD 
c                     """"""""""""""""""""""""""""""""""""""""""""            


C                           MAIN PROGRAMME 


	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)

	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax

	open(unit=1,file='br49.in')
	open(unit=2,file='br49.ou')
	open(unit=3,file='br49.ou1')
	open(unit=4,file='br49.ou2')
	g=9.81
c       ______________________________________________________________
	  read(1,*)nsr,(sr,cha(sr),qmax(sr),qmin(sr),mrl(sr),sr=1,nsr)         
	 WRITE(*,*)nsr,(cha(sr),qmax(sr),qmin(sr),mrl(sr),sr=1,nsr)
	 read(1,*) so1,so2,sin,ratio1, ratio2,rin,bin,n1,n2,vmax,vmin,
     *   pdwl,pbw,dmax,Fr,wlbk,wrbk,wrbr,wlbr,t,tbb,tbg,fb,fb2,tls,tlb,
     *   tbs,tbbb,alfa,dxx,chds,zumax
	 read(1,*) choice
	 if(choice.ne.0)then
	   read(1,*) sor,ratior,blor
	 endif
	 read(1,*)ucec,ucef1,ucef2,ucls,uclb,ucbs,ucbb,ucpl,ucbk1,ucbk2,
     *        ucbk3,ucsf,ucla,uctur
	 read(1,*)unit
	 read(1,*) m,(ch(i),gl(i),i=1,m)
c       ___________________________________________________________________
c       ___________________________________________________________________

	cmin=0
	i=1
	so(i)=so1
10      ratio(i)=ratio1

20      call DESIGN
	call EXTREME
	if(check(i,7).ne.0)then
	   ilst=i
	   go to 40
	endif
	blo(i)=blomin(i)
	xx=0
30      if(t.eq.0)then
	   call ESTMTR
	else
	   call ESTMTT
	endif 

	call CSTMIN

	ilst=i        
	i=i+1
	ratio(i)=ratio(i-1)
	so(i)=so(i-1)

	BLOMIN(I)=BLOMIN(I-1)
	BLOMAX(I)=BLOMAX(I-1)

	blo(i)=blo(i-1)+bin
	if(blo(i).le.blomax(i)) then
	  yh(i)=yh(i-1)
	    b(i)=b(i-1)
	  go to 30
	endif

	if(((blo(i)-blomax(i)).lt.bin).and.(xx.ne.1))then
	  yh(i)=yh(i-1)
	     b(i)=b(i-1)
	       blo(i)=blomax(i)
	     xx=1    
	   go to 30

	endif

40      ratio(i)=ratio(ilst)+rin
	if(ratio(i).le.ratio2)go to 20

	 so(i)=so(ilst)+sin   
	if(so(i).le.so2)go to 10

	call RESULT      
	call DRAW
	stop
	end



			SUBROUTINE DESIGN

	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)

	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax




c  CALCULATION OF MANNING'S  EQUIVALENT ROUGHNESS


	n=((n1**(3./2.)*ratio(i)+2*n2**(3./2.)*sqrt(1+t**2))/(ratio(i)+
     *  2*sqrt(1+t**2)))**(2./3.)

c  CALCULATION OF DEPTH OF FLOW BY MANNING'S FORMULA      

	aa=(n*qmin(1))*((ratio(i)+2*((1+t**2))**.5))**(2./3.)
	bb=(((so(i))**.5)*(ratio(i)+T)**(5./3.))
	yh(i)=(aa/bb)**(3./8.)
	B(i)=RATIO(i)*YH(i)  

	if(t.eq.0)RETURN

	if(b(i).le.5)then
	 c1=b(i)/.05
	 c2=nint(c1)
	 b(i)=c2*.05
	elseif(b(i).gt.5.and.b(i).le.10)then
	 c1=b(i)/.1
	 c2=nint(c1)
	 b(i)=c2*.1
	else
	 if(b(i).gt.10)then
	 c1=b(i)/.25
	 c2=nint(c1)
	 b(i)=c2*.25
	 endif
	endif


C CHECKING FLOW CONDITION FOR ALL THE SUBREACHES

       do 15 sr=1,nsr
	yn1=yh(i)
20     wa1=(b(i)+t*yn1)*yn1
	wp1=(b(i)+2*sqrt(1+t**2)*yn1)
	wr1=wa1/wp1
	wq1=wa1/n*wr1**(2./3.)*sqrt(so(i))
	if (abs(wq1-qmin(sr)).gt.1E-3)then
	  yn1=yn1*(qmin(sr)/wq1)**(3./8.)
	  go to 20

	else      
	  wv1=qmin(sr)/wa1
	endif

	yn2=yh(i)
30      wa2=(b(i)+t*yn2)*yn2
	wp2=(b(i)+2*sqrt(1+t**2)*yn2)
	wr2=wa2/wp2
	wq2=wa2/n*wr2**(2./3.)*sqrt(so(i))

	if (abs(wq2-qmax(sr)).gt.1E-3)then
	  yn2=yn2*(qmax(sr)/wq2)**(3./8.)
	  go to 30
	else      
	  wv2=qmax(sr)/wa2
	endif

	 Fr1=v1/(9.81*yn1)
	 Fr2=v2/(9.81*yn2)

	if(wv1.gt.vmax.or.wv2.gt.vmax)check(i,1)=1
	if(wv1.lt.vmin.or.wv2.lt.vmin)check(i,2)=2

	dwl=yn2-yn1
	if(dwl.gt.pdwl) check(i,3)=3

	if(b(i).le.pbw)check(i,4)=4
	 if(Fr1.gt.Fr.or.Fr2.gt.Fr)check(i,5)=5

	if(sr.eq.1)then
	   yh(i)=yn2
	   yh1(i)=yn1
	endif

15      continue

	RETURN
	END
	

		SUBROUTINE ESTMTT
	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)


	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax


C  ESTIMATE OF TRAPEZOIDAL CANAL

	tvc(i)=0
	tvf(i)=0
	w2=0
	tl2=0
	tla=0
	hmax=0
 
       do 30 k=1,m      
	vfn(k)=0
	w2=w1
	tl2=tl1
	bl(1)=blo(i)        
	brl=bl(k)+yh(i)+fb
	bkl=brl+fb2
	tw1=b(i)+(brl-bl(k))*t*2.
	tw11=tw1+wrbr+wlbr
	tw2=tw11+((bkl-brl)*tbb*2.)
	tw22=tw2+wlbk+wrbk
	b1=tw22+((bkl-gl(k))*tbg*2.)       
	w1=b1+2

	if(bkl.le.gl(k))then
	   tl1=0        
	else
	   tl1=sqrt(1+tbg*tbg)*(bkl-gl(k))
	endif
C  CALCULATION OF EARTHWORK IN FILLING AND CUTTING
  
	if(gl(k).le.bl(k))then
	   af(k)=(tw22+b1)*.5*(bkl-gl(k))-(tw2+tw11)*.5*(bkl-brl)-(tw1
     *           + b(i))*.5*(brl-bl(k))
	   ac(k)=0 
	elseif(gl(k).gt.bl(k).and.gl(k).le.brl)then
	   tw3=b(i)+2*(gl(k)-bl(k))*t
	   af(k)=(tw22+b1)*.5*(bkl-gl(k))-(tw2+tw11)*.5*(bkl-brl)-(tw1
     *          +tw3)*.5*(brl-gl(k))
	   ac(k)=(tw3+b(i))*.5*(gl(k)-bl(k))
	elseif(gl(k).gt.brl.and.gl(k).le.bkl)then
	   tw3=tw11+(gl(k)-brl)*tbb*2
	   af(k)=(tw22+b1)*.5*(bkl-gl(k))-(tw2+tw3)*.5*(bkl-gl(k))
	   ac(k)=(b(i)+tw1)*.5*(brl-bl(k))+(tw3+tw11)*.5*(gl(k)-brl)
	else
	 if(gl(k).gt.bkl)then
	   tw2=tw11+(gl(k)-brl)*tbb*2.
	   af(k)=0
	   ac(k)=(tw1+b(i))*.50*(brl-bl(k))+(tw2+tw11)*.5*(gl(k)-brl)
	 endif
	endif

	if(k.eq.1)go to 20 

	  vc(k)=(ac(k-1)+ac(k))*.5*(ch(k)-ch(k-1))                    
	  vf(k)=(af(k-1)+af(k))*.5*(ch(k)-ch(k-1))
	  tvc(i)=tvc(i)+vc(k)
	  tvf(i)=tvf(i)+vf(k)
	  
	  if((vf(k)-vc(k)).gt.0)then
40          vfn(k)=vf(k)-vc(k)
	  endif
	  tvfn(i)=tvfn(i)+vfn(k)        

	  ala=(w1+w2)*.5*(ch(k)-ch(k-1))          
	  tla=ala+tla
	  atur=2*.5*(tl1+tl2)*(ch(k)-ch(k-1))
	  tat(i)=atur+tat(i)
	  hcut=gl(k)-bl(k)
	  if(hcut.gt.hmax)hmax=hcut
20        bl(k+1)=bl(k)-so(i)*(ch(k+1)-ch(k))

	if(hmax.gt.dmax)check(i,6)=6
30      CONTINUE  


C  CALCULATION OF OTHER QUANTITIES

	lc=ch(m)-ch(1)
	delb1=tls*sqrt(1+t**2)
	delb2=tbs*sqrt(1+t**2)
	bb=b(i)+2*delb1-2*t*tlb
	bbb=bb+2*delb2-2*t*tbbb
	qls(i)=2*(yh(i)+fb)*sqrt(1+t**2)*tls*lc 
	qlb(i)=.5*(b(i)+2*delb1+bb)*tlb*lc
	qbs(i)=2*(yh(i)+fb+tlb)*sqrt(1+t**2)*tbs*lc
	qbb(i)=.5*(bb+2*delb2+bbb)*tbbb*lc
	qpl(i)=(b(i)+2*(yh(i)+fb)*sqrt(1+t**2))*lc
	
	if(unit.eq.2.)then
	   qls(i)=2*(yh(i)+fb)*sqrt(1+t**2)*lc
	   qlb(i)=b(i)*lc
	   qbs(i)=2*(yh(i)+fb+tlb)*sqrt(1+t**2)*lc
	   qbb(i)=bb*lc
	   qpl(i)=(b(i)+2*(yh(i)+fb)*sqrt(1+t**2))*lc 
	endif

	tvfn1(i)=tvfn(i)*.7
	tvfn2(i)=tvfn(i)*.3

	cec(i)=tvc(i)*ucec
	cef1(i)=tvfn1(i)*ucef1                  
	cef2(i)=tvfn2(i)*ucef2
	cef(i)=cef1(i)+cef2(i)                  
	cef(i)=tvfn(i)*ucef                  
	cls(i)=qls(i)*ucls
	clb(i)=qlb(i)*uclb
	cbs(i)=qbs(i)*ucbs
	cbbb(i)=qbb(i)*ucbb
	cpl(i)=qpl(i)*ucpl
	ctur(i)=tat(i)*uctur
c   Calculation of total construction cost cost1      
	cost1(i)=cec(i)+cef1(i)+cef2(i)+cls(i)+clb(i)+cbs(i)+cbbb(i)
     *            +cpl(i)+ctur(i)
	 
C   CALCULATION OF LAND ACQUISITION  COST
	cost2(i)=tla*ucla

C   CALCULATION OF MAINTENANCE COST
	cost3(i)=0.01*cost1(i)

C   CALCULATION OF TOTAL COST
	tcost(i)=cost1(i)+cost2(i)+cost3(i) 

	RETURN
	END
	


		SUBROUTINE ESTMTR



	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2,ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp),bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)

	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp),abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax

C  ESTIMATE OF RECTANGULAR CANAL
	DF=.45
	TBW=.125
	TBF=.25
	TCF=.08
	uccc=12
	tvc(i)=0
	tvf(i)=0
	w2=0
	tl2=0
	tla=1
	hmax=0
       do 30 k=1,m      
	vfn(k)=0
	
	w2=w1
	tl2=tl1
	bl(1)=blo(i)        
	brl=bl(k)+yh(i)+fb
	bkl=brl+fb2
	tw22=b(i)+2*tbw+wlbk+wrbk       
	b1=tw22+((bkl-gl(k))*tbg*2.)       
	w1=b1+2
       
	if(bkl.le.gl(k))then
	   tl1=0        
	else
	   tl1=sqrt(1+tbg*tbg)*(bkl-gl(k))
	endif
C********************************************************
	    ac2=(tbf+tcf)*(df+tcf)                    

	 if(gl(k).lt.(bl(k)-.16))then
	    af(k)=(wlbk+tbg*(bkl-gl(k))*.5)*(bkl-gl(k))
     *         +(wrbk+tbg*(bkl-gl(k))*.5)*(bkl-gl(k))
	    ac1=0
	    ac(k)=ac1+ac2
	    asf(k)=(b(i)+tbw-tbf)*(bl(k)-.16-gl(k))
	    abk1(k)=2*(yh(i)+fb+.16)*tbw                                         brick-walls    
	    abk3(k)=2*(df+(bl(k)-gl(k)-.16))*tbf                             brick-foundation
	    apl(k)=2*tbw+2*(yh(i)+fb)+1*b(i)

	 elseif((gl(k).ge.(bl(k)-.16)).and.(gl(k).lt.bkl))then
	    af(k)=(wlbk+tbg*(bkl-gl(k))*.5)*(bkl-gl(k))
     *          +(wrbk+tbg*(bkl-gl(k))*.5)*(bkl-gl(k))
	    ac1=(b(i)+tbw+tbf+tcf)*(gl(k)-bl(k)+.16)
	    ac(k)=ac1+ac2  
	    asf(k)=0
	    abk1(k)=2*(yh(i)+fb+.16)*tbw                                            brick-walls    
	    abk3(k)=2*df*tbf                                               brick-foundation 
	    apl(k)= 2*tbw+2*(yh(i)+fb)+1*b(i)  
	 else 
	  if(gl(k).ge.bkl)then
	    af(k)=0
	    ac1=(b(i)+tbw+tbf+tcf)*(gl(k)-bl(k)+.16)
	    asf(k)=0
	    ac(k)=ac1+ac2
	    abk1(k)=2*(gl(k)-bl(k)+.16)*tbw                                       brick-walls    
	    abk3(k)=2*df*tbf                                               brick-foundation
	    apl(k)=2*tbw+2*(gl(k)-bl(k))+1*b(i)                                  
	  endif
	 endif                  
	 if (k.eq.1) goto 20
	 dist=ch(k)-ch(k-1)
	 vc(k)=(ac(k-1)+ac(k))*.5*dist
	 vf(k)=(af(k-1)+af(k))*.5*dist
	 vsf=(asf(k-1)+asf(k))*.5*dist
	 vbk1=(abk1(k-1)+abk1(k))*.5*dist
	  vbk3=(abk3(k-1)+abk3(k))*.5*dist
	  qnpl=(apl(k-1)+apl(k))*.5*dist

	 tvc(i)=tvc(i)+vc(k)
	 tvf(i)=tvf(i)+vf(k)
	 tvsf(i)=tvsf(i)+vsf      
	 tvbk1(i)=tvbk1(i)+vbk1
	 tvbk3(i)=tvbk3(i)+vbk3
	 tqnpl(i)=tqnpl(i)+qnpl
	  if((vf(k)-vc(k)).gt.0)then
40          vfn(k)=vf(k)-vc(k)
	  endif
	  tvfn(i)=tvfn(i)+vfn(k)        

	  ala=(w1+w2)*.5*(ch(k)-ch(k-1))          
	  tla=ala+tla
	  atur=2*.5*(tl1+tl2)*(ch(k)-ch(k-1))
	  tat(i)=atur+tat(i)
	  hcut=gl(k)-bl(k)
	  if(hcut.gt.hmax)hmax=hcut

20         bl(k+1)=bl(k)-so(i)*(ch(k+1)-ch(k))
	if(hmax.gt.dmax)check(i,6)=6
30       CONTINUE

CALCULATION OF OTHER QUANTITIES
 
	 lc=ch(m)-ch(1) 
	 tvbk2(i)=(b(i)*.08)*lc
	 tvcc(i)=(2*(tbf+tcf)*tcf+b(i)*tcf)*lc

	tvfn1(i)=tvfn(i)*.7
	tvfn2(i)=tvfn(i)*.3

	cec(i)=tvc(i)*ucec
	cef1(i)=tvfn1(i)*ucef1                  
	cef2(i)=tvfn2(i)*ucef2


C       cef(i)=tvf(i)*ucef      
	csf(i)=tvsf(i)*ucsf
	cbk1(i)=tvbk1(i)*ucbk1
	cbk2(i)=tvbk2(i)*ucbk2
	cbk3(i)=tvbk3(i)*ucbk3
	ccc(i)=tvcc(i)*uccc
	cpl(i)=tqnpl(i)*ucpl
	ctur(i)=tat(i)*uctur
c      Calculation of total construction cost cost1      

	cost1(i)=cec(i)+cef1(i)+cef2(i)+csf(i)+cbk1(i)+cbk2(i)+cbk3(i)
     *          +ccc(i)+cpl(i)+ctur(i)


C   CALCULATION OF LAND ACQUISITION  COST
	cost2(i)=tla*ucla

C   CALCULATION OF MAINTENANCE COST
	cost3(i)=0.01*cost1(i)

C   CALCULATION OF TOTAL COST
	tcost(i)=cost1(i)+cost2(i)+cost3(i) 
	RETURN
	END
	     


	     
	      SUBROUTINE CSTMIN


	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)


	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax

	if(choice.eq.1)then
	  if(ABS(so(i)-sor).le..00001)then
	    if(ABS(ratio(i)-ratior).le..1)then  
	      if(ABS(blo(i)-blor).le..1)then
		 ir=i
		   j=2
		 go to 20
	      endif
	    endif
	  endif
	endif

35      if(ratio(i).eq.ratio(i-1))then
	   do 40 kk=1,7
	     check(i,kk)=check((i-1),kk) 
40         continue
	endif

30      j=1     

	do 33 kk=1,6
	  if(check(i,kk).gt.0.)then
	  RETURN
	  endif

33      continue

       if(cmin.eq.0)then 
	   cmin=tcost(i)
	   imin=i
	elseif(tcost(i).lt.cmin)then
	   cmin=tcost(i)
	   imin=i
	else
	   RETURN
	endif

20      do 10 k=1,m
	  aff(j,k)=af(k)
	  acc(j,k)=ac(k)
	  vff(j,k)=vf(k)
	  vfnn(j,k)=vfn(k)
	  vcc(j,k)=vc(k)
	  bll(j,k)=bl(k)

10      continue
	if(j.eq.2)go to 30

	RETURN
	END
	    
 

		   SUBROUTINE RESULT      


	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)


	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)


	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax

c TO WRITE MINIMUM AND MAXIMUM FEASIBLE UPSTREAM BED LEVELS
 
       write(2,160)
160    format(//,15x,'MINIMUM AND MAXIMUM UP-STREAM FEASIBLE BED LEVELS',
     *         /,15X,50('~'))
       write(2,170)
170    format(80('_')/,2x,'Sl',10x,'SO',10x,'RATIO',10X,'MINBLO',10X,
     *        'MAXBLO'/80('_'))

      do 1000 i=1,ilst

	  IF(BLOMIN(i).EQ.BLOMIN(I-1))GOTO 1000

	   write(2,180)i,so(i),ratio(i),blomin(i),blomax(i)
180        format(2x,i4,4x,f7.5,8x,f6.3,8x,f6.2,10x,f6.2)
1000  continue

      write(2,190)
190   format(80('_'))

C TO WRITE ABSTRACT OF RESULTS

	write(2,2)
2       format(////30X,20('~'),/30X,'ABSTRACT OF RESULTS'/,30X,20('~'))

	do 5 i=1,ilst

	if(i.gt.1)go to 7

	write(2,3)
3       format(/,80('-'),/t3,'SL',t11,'SO',t17,'RATIO',t27,'B',t35,'YH',
     *       t42,'BLO',t52,'TCOST',t66,'REMARKS')

7       if(so(i).ne.x1.and.ratio(i).ne.x2)then
	   write(2,10)i,so(i),ratio(i),b(i),yh(i),blo(i),tcost(i),
     *           (check(i,kk),kk=1,7)
10         format(80('-'),/,1x,i4,2x,f7.5,1x,f6.3,2x,f6.2,2x,f6.2,2x,
     *          f7.2,2x,f12.2,2x,7(i2))
	elseif (so(i).eq.x1.and.ratio(i).ne.x2)then
	   write(2,20)ratio(i),b(i),yh(i),blo(i),tcost(i),
     *          (check(i,kk),kk=1,7)
20         format(17x,62('-'),/,15x,f6.3,2x,f6.2,2x,f6.2,2x,f7.2,2x,
     *               f12.2,2x,7(i2))
	else
	   if(so(i).eq.x1.and.ratio(i).eq.x2)then
	       do 25  kk=1,7
		  check(i,kk)=check((i-1),kk)
25             continue
	     write(2,30)b(i),yh(i),blo(i),tcost(i),(check(i,kk),kk=1,7)
30           format(23x,f6.2,2x,f6.2,2x,f7.2,2x,f12.2,2x,7(i2))
	   endif
	endif
	x1=so(i)
	x2=ratio(i)
5       continue

	write(2,38)
38      format(80('_')//)
	write(2,39)
39      format(1x,'REMARKS:',/, 
     * 6x,'1  INDICATES VELOCITY EXCEEDS MAX. PERMISSIBLE VELOCITY',/,
     * 6x,'2  INDICATES VELOCITY LESS THAN MIN. PERMISSIBLE VELOCITY',/,
     * 6x,'3  INDICATES WATER FLUCTUATION EXCEEDS MAX.PERMISSIBLE VALUE'
     * ,/,6x,'4  INDICATES BED-WITH LESS THAN MIN. PERMISSIBLE BED-WIDTH'
     * ,/,6x,'5  INDICATES FROUDE No.  EXCEEDS MAX. PERMISSIBLE VALUE',
     * ,/,6x,'6  INDICATES HEIGHT OF CUTTING EXCEEDS MAX. PERMISSIBLE V
     *ALUE')        



c  TO WRITE LEAST-COST PARAMETERS OF THE CANAL

	i=imin
	write(2,380)imin,tcost(imin)
380     format(80('-')////,t19,34('-')/,20x,'LEAST COST FOUND AT SL.No.'
     *    ,i4,/20x,'MINIMUM COST=   Rs.',F12.2/,t19,34('-')/)

	write(2,390)so(imin),ratio(i),blo(i),b(i),yh(i),blo(i)
390     format(///25x,'LEAST-COST PARAMETERS OF THE CANAL',/22x,40('-')
     *   ,//14X,'BED SLOPE(so)=',t41,f7.5,/15x,'B/Y RATIO (ratio)=',
     *   t42,f6.3,/15x,'BED LEVEL AT 0 CH (blo) =',t42,f7.2,2X,'METER'
     *   /15x,'BED WIDTH (b)=',t42,f6.2,2X,'METER'/15x,
     *   'DEPTH OF FLOW (yh)=',t42,f6.2,2X,'METER'/,13x,
     *   'BED LEVEL AT UPSTREAM(blo)=',t42,f6.2,2x,'METER'/)

C  TO WRITE EARTH-WORK STATEMENT OF THE CANAL

	i=imin       
      do 75 j=1,2
       
	if(j.eq.1.and.tcost(imin).eq.0)then
	write(2,31)
	write(*,31)
31      format(////,10x,55('-'),//15X,
     *   'NO FEASIBLE DESIGN WITHIN THE SPECIFIED RANGES',//15X,
     *    '0F BED SLOPE,B/Y RATIO AND UPSTREAM BED LEVELS',//,10x,
     *     55('-')////)   
	go to 75
	endif
	if(j.eq.2.and.choice.eq.0.)go to 75     
	if(j.eq.2.and.choice.eq.1)i=ir
	if(j.eq.2.and.choice.eq.1.and.ir.eq.0)go to 75
32      do 200  k=1,m
	  if(k.eq.1)then
	     write(2,35)i,so(i),ratio(i),blo(i),b(i),yh(i)
35           format(////,t30,'E/W STATEMENT OF THE CANAL-',2x,i4,/,t26,
     *       40('~')/,t3,'SO=',f7.5,t15,'RATIO=',f6.3,t32,'BLO=',f7.2,
     *       1x,'M',t48,'B=',f6.2,1x,'M',t60,' Y=',f6.2,1x,'M'/,80('-')
     *       , /, t3,'chainage',t15,'GL',T24,'BL',T29,'area of',T38,
     *       'area of',T48,'vol.of',T58,'vol.of',T71,'NET VOL.'/,t29,
     *       'cutting',T38,'filling',T50,'cut.',t60,'fill.',T71,'OF 
     *       FILL.'/,80('-')//)
	     go to 50
	  endif
	  write(2,40)vcc(j,k),vff(j,k),vfnn(j,k)
40        format(44x,3(f10.2,1x))
50        write(2,60)ch(k),gl(k),bll(j,k),acc(j,k),aff(j,k)
60        format(1x,f9.3,4(1x,f7.2,))
200     CONTINUE        
	write(2,70)tvc(i),tvf(i),tvfn(i)
70      format(/,80('-'),//,20x'total volume of cutting='f10.2,2x,'CUM'
     *     /,20x,'total volume of filling=',f10.2,2x,'CUM'/,20x,
     *      'total net  volume of filling=',f10.2,2x,'CUM'//) 
75    continue



C TO WRITE ESTIMATE OF TRAPEZOIDAL CANAL

      do 365 i=1,ilst 

       write(4,80)i,unit,so(i),ratio(i),blo(i),b(i),yh(i)
80     format(//,t30'ESTIMATE OF THE CANAL-',i5,/,t26,40('~'),t67,
     *      'Read UNIT',i1,//,t6,'SO=',f7.5,t17,'RATIO=',f6.3,t32,'BLO='
     *      ,f7.2,1x,'M',t48,'B=',f6.2,1x,'M',t60,' Y=',f6.2,1x,'M'/,
     *      80('-')/,t5'ITEM',t25, 'QUANTITY',t36,'UNIT1',t42,'UNIT2',
     *      t52,'RATE',t65,'AMOUNT(Rs.)',/,80('-'))
       write(4,90)tvc(i),ucec,cec(i)
90     format(t5,'E/W-CUTTING',t22,f10.3,5x,'cum',2x,'cum'1x,f10.2,
     *       5x,f12.2)  
       write(4,100)tvfn1(i),ucef1,cef1(i)
100    format(t5,'E/W-FILLING(H.L)',t21,f11.3,5x,'cum',2x,'cum'1x,
     *       f10.2,5x,f12.2)
       write(4,105)tvfn2(i),ucef2,cef2(i)
105    format(t15,'(CART)',t21,f11.3,5x,'cum',2x,'cum'1x,f10.2,
     *       5x,f12.2)      


      if(t.gt.0)then  

       write(4,110)qls(i),ucls,cls(i)
110     format(t5,'LINING(SIDES)',t22,f10.3,5x,'cum',2x,'sqm'1x,f10.2,
     *       5x,f12.2)
       write(4,120)qlb(i),uclb,clb(i)
120     format(t5,'LINING(BED)',,t22,f10.3,5x,'cum',2x,'sqm'1x,f10.2,
     *       5x,f12.2)        
       write(4,130)qbs(i),ucbs,cbs(i)
130     format(t5'BASES(SIDES)',t22,f10.3,5x,'cum',2x,'sqm'1x,f10.2,
     *       5x,f12.2)  
       write(4,140)qbb(i),ucbb,cbbb(i)
140     format(t5'BASES(BED)',t22,f10.3,5x,'cum',2x,'sqm'1x,f10.2,
     *       5x,f12.2)  
      write(4,150)qpl(i),ucpl,cpl(i)
150     format(t5'PLASTERING',t22,f10.3,5x,'sqm',2x,'sqm'1x,f10.2,
     *       5x,f12.2)  
	write(4,155)tat(i),uctur,ctur(i)
155     format(t5,'TURFING',t22,f10.3,5x,'sqm',2x,'sqm'1x,f10.2,
     *       5x,f12.2/,80('-'))  

       else  
C  TO WRITE RESULTS OF RECT. CANAL

      write(4,310)tvsf(i),ucsf,csf(i)
310    format(t5,'SAND FILLING',t22,f10.3,5x,'cum',2x,'sqm',1x,f10.2,
     *       5x,f12.2)
       write(4,320)tvbk1(i),ucbk1,cbk1(i)
320    format(t5,'B/W in WALLS(1:4)',t22,f10.3,5x,'cum',2x,'sqm',1x,
     *       f10.2,5x,f12.2)
     
       write(4,330)tvbk2(i),ucbk2,cbk2(i)
330    format(t5,'B/W in BED(1:4)',t22,f10.3,5x,'cum',2x,'sqm',1x,f10.2,
     *       5x,f12.2)
       write(4,340)tvbk3(i),ucbk3,cbk3(i)
340    format(t5,'B/W in FOUNDATION',t22,f10.3,5x,'cum',2x,'sqm',1x,
     *       f10.2,5x,f12.2)
       
       write(4,350)tvcc(i),uccc,ccc(i)
350    format(t5,'1:3:6 C.C in BASE',t22,f10.3,5x,'cum',2x,'sqm',1x,
     *     f10.2,5x,f12.2)
	  
       write(4,360)tqnpl(i),ucpl,cpl(i)
360    format(t5,'CEMENT PLASTER',t22,f10.3,5x,'cum',2x,'sqm',1x,f10.2,
     *       5x,f12.2)
	write(4,363)tat(i),uctur,ctur(i)
363    format(t5,'TURFING',t22,f10.3,5x,'cum',2x,'sqm',1x,f10.2,
     *       5x,f12.2,/,80('-'))  
      endif
	write(4,370)cost1(i),cost2(i),cost3(i),tcost(i)
370     format(t32,'Total construction  cost = Rs',f12.2/,t25,
     *      'Total  cost of land acquisition = Rs',f12.2/,t31,
     *      'Total cost of maintenance = Rs',f12.2/,t25,48('-')/,
     *      t46,'Total cost = Rs',f12.2)

365   CONTINUE

	RETURN          
	END



C      SUBROUTINE TO CALCULATE PROFILE
C      ---------------------------------        
	
			   SUBROUTINE EXTREME

	integer p,pp,ppp,sr,check
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)


c  Common only for rect.canal  below 
    
	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

c  Common only for GVF  below  
	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax


	 WRITE(*,*)nsr,(cha(sr),qmax(sr),qmin(sr),mrl(sr),sr=1,nsr)

	g=9.81
	usbl=0
	lc=ch(m)-ch(1)

	dsbl=usbl-so(i)*lc
	z1=dsbl
	chi=chds
	write(3,70)i,so(i),ratio(i)
70      format(//25x,'CALCULATION OF MINIMUM U/S BED LEVEL'//25x,
     *     37('-'),/35x,'SL NO.=',i5/ 
     *   25x,'SO=',f7.6,3x,'RATIO=',f7.2
     *     / 80('-')/,2x,'sr',4x,'dx',9x,
     *    'ch',13x,'y1',8x,'z2',8x,'wsl',7x,'mrl',10x,'d',/,80('-'),/)


       do 40 sr=nsr,2,-1        

C ** CALCULATION OF NORMAL FLOW-DEPTH OF THE SUBREACH  

	y=qmin(sr)/b(i)
20      wa=(b(i)+t*y)*y
	wp=(b(i)+2*sqrt(1+t**2)*y)
	wr=wa/wp
	wq=wa/n*wr**(2./3.)*sqrt(so(i))
	  if (abs(wq-qmin(sr)).gt.1E-3)then
	    y=y*(qmin(sr)/wq)**(3./8.)
	    go to 20
	  else    
	    y1=y
	endif


C CALCULAION OF GVF BY STANDARD STEP METHOD USING NEWTON-RAPHSON METHOD 

	a1=y1*(b(i)+t*y1)
	p1=b(i)+2*y1*sqrt(1+t**2.)
	r1=a1/p1
	sf1=((n*qmin(sr))**2.)/((a1**2.)*(r1**(4./3.)))
	h1=z1+y1+alfa*(qmin(sr)**2)/(2*g*a1**2.)                
	wsl=y1+z1

	  if(sr.ne.nsr)lsr=cha(sr+1)-cha(sr)
	 if(sr.eq.nsr)lsr=chds-cha(sr)
	 if(lsr.lt.abs(dxx))then
	    dx=-lsr
	    ns=1
	 else
	    ns=lsr/dxx
	    dx=lsr/ns
	    ns=abs(ns)
	endif   
	 do 30 nn=ns+1,2,-1 

	  write(3,60)chi,y1,z1,wsl
60      format(/16x,f10.3,3xf8.5,3x,f8.3,3x,f8.3,3x,f8.3,
     *         3x,f8.3,3x,f8.2)

c       Calculation for section 2 by Newton-Raphson Method

	ye=y1
	z2=z1-so(i)*dx
	kk=0
16      kk=kk+1
	a2=ye*(b(i)+t*ye)
	p2=b(i)+2*ye*sqrt(1+t**2.)
	r2=a2/p2
	sfe=(n*qmin(sr))**2./((a2**2.)*(r2**(4./3.)))
	h2=z2+ye+alfa*(qmin(sr)**2.)/(2*g*a2**2)                 
	tw=b(i)+2*t*ye

	drdy=tw/p2-r2*2*(sqrt(1+t**2.))/p2      
	f=ye+alfa*qmin(sr)**2./(2*g*(a2**2.))+.5*(sf1+sfe)*dx+z2-h1
	df=1-alfa*qmin(sr)**2.*tw/(g*a2**3.)-dx*sfe*(tw/a2+(2./3.)
     *       *drdy/r2)
	dy=f/df
	y=ye-dy
	if(abs(dy).gt..0001.and.kk.lt.500)then
	   ye=y
	   go to 16
	elseif(abs(dy).gt..0001.and.kk.gt.500)then
	   write(*,25)ns,sr
25         format( 'iterations failed at section=',i3,'of subreach',i3)
	else
	   if(abs(dy).lt..0001.and.kk.lt.500)then

	      sf1=sfe
	      z1=z2
	      y1=y
	      a1=a2
	      p1=p2
	      r1=r2
	      h1=h2      
	  endif
	endif
	 wsl=z1+y1
	 chi=chi+dx
30      continue
	d(sr)=wsl-mrl(sr)
	write(3,65)sr,dx,cha(sr),y1,z2,wsl,mrl(sr),d(sr)
65      format(/1x,i3,1x,f10.3,1x,f10.3,3xf8.5,3x,f8.3,3x,f8.3,3x,f8.3,
     *         2x,f8.3,3x,f8.2)  
       
c      CALCULATION OF MINIMUM d(sr)

	if(sr.lt.nsr)go to 50
	dmin=d(nsr)
50      dmin=min(dmin,d(sr))

40      continue

	write(3,80)dmin       
80      format(80('-'),//,30x,'dmin=',f10.3,'METER')


C **  CALCULATION OF BLOMIN
	blomax(i)=zumax-yh(i)
	blomin(i)=usbl-dmin
	if(blomax(i).lt.blomin(i))then
	    check(i,7)=7

	write(3,90)blomin(i)
90      format(30x,'blomin='f10.3,2X,'METER')
	    RETURN
	endif
	 WRITE(*,*)5
	RETURN
	end
	



	       SUBROUTINE DRAW


	integer p,pp,ppp,sr,check,unit
	real n,n1,n2,mrl,lsr,lc,pbw,pdwl
	parameter (p=1500,pp=500,ppp=20)
	dimension mrl(ppp),d(ppp),qmin(ppp),cha(ppp),qmax(ppp)

	common qmax, qmin,so(p),ratio(p),t,n1,n2
	common vmax,vmin,pdwl,pbw,Fr,dmax
	common so1,so2, ratio1,ratio2,rin,sin,blomax(p),blomin(p),bin,
     *     tcost(p),sor,ratior,blor
       common m,wlbk,wrbk,wlbr,wrbr,tbb,tbg,bl(pp),blo(p),fb,fb2,yh(p),
     *           yh1(p)
	common b(p),tls,tlb,tbs,tbbb,ucec,ucef1,ucef2,ucls,uclb,ucbs,
     *        ucbb,ucla    
	common ucpl,uctur,i,ch(pp),gl(pp),af(pp),ac(pp),vc(pp),
     *        vf(pp),vfn(pp),acc(2,pp),aff(2,pp),vcc(2,pp),vff(2,pp),
     *        vfnn(2,pp), bll(2,pp)
	common qls(p),qlb(p),qbs(p),qbb(p),qpl(p),tat(p),cec(p),cef(p),
     *         cef1(p),cef2(p),cls(p),clb(p),cbs(p),cbbb(p),cpl(p),
     *         ctur(p),cost1(p),cost2(p),cost3(p),unit
	common tvc(p),tvf(p),tvfn(p),tvfn1(p),tvfn2(p),ilst,imin,j,
     *         choice,ir,check(p,7)


	common tbf,tbw,tcf,ucsf,ucbk1,ucbk2,ucbk3,
     *      asf(pp), abk1(pp),abk2(pp),abk3(pp),apl(pp),
     *       tvsf(p),tvbk1(p),tvbk2(p),tvbk3(p),
     *        tvcc(p),tqnpl(p),csf(p),cbk1(p),cbk2(p),cbk3(p),ccc(p)

	common y1,alfa,dxx,z1,nsr,cha,chds,mrl,d,n,zumax

	     
	if(t.eq.0)RETURN

	write(2,20)wrbk,wlbk
20      format(///,t12,f5.2,t59,f5.2)

	write(2,10)
10      format(t9,'|<',8('-'),'>|',,t57,'|<',t59,10('-'),
     *        t68,'>|' )
	write(2,15)
15      format(t12,'wrbk',t59,'wlbk')             

	write(2,30)fb,t,tbg,yh(imin),b(imin),so(imin)
30      format(,t9,11('_'),t57,13('_'),/,t8,'/',t21,'\',t32,'F.B.=',
     *      f4.2,t56,'/',t70,'\1:',t73,f4.2,/,t6,'/',t23,'\--FSL',T29,
     *      25('-'),T54,'/',T72,'\',/, t2,'__/',t25,'\',t37,'|',t52,
     *      '/1:',t55,f5.2,t74,'\_GL__',/,t27,'\',t35,f5.2,t50,'/',/ ,
     *       t29,'\',t30,9('-'),t37,('|'),t38,10('-'),t48,('/'),/
     *       t30,('|<---B='),t37,f5.2,t42,3('-'),t46,('>|'),//,t56,'SO='
     *       f6.5///,t52,'DIMENSIONS IN METERS',///,
     *       t25,'TYPICAL SECTION OF LEAST-COST CANAL',/,T25,35('~')) 

       
	RETURN
	END
