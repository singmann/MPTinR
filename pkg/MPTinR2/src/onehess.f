      subroutine onehess(kerncat,kernpar,freepar,zweig,trees,indi,
     1      A,B,branch,C,iDaten,noc,x,hess,ifail)
c    use numerical_libraries
c      
c    ! ---- Subroutine export declarations.
c
c    !dec$attributes dllexport, stdcall :: onefisch
c    !dec$attributes alias:'onefisch' :: onefisch
c
c    ! ---- Declaration of exported variables.
c
c    !dec$attributes value :: kerncat
c    !dec$attributes value :: kernpar
c    !dec$attributes value :: zweig
c    !dec$attributes value :: trees
c    !dec$attributes value :: indi
c    !dec$attributes reference :: A
c    !dec$attributes reference :: B
c    !dec$attributes reference :: Branch
c    !dec$attributes reference :: C
c    !dec$attributes reference :: iDaten
c    !dec$attributes reference :: noc
c    !dec$attributes reference :: x
c    !dec$attributes reference :: ifi
c    !dec$attributes reference :: ifail

      implicit none

c      integer, parameter :: fp = selected_real_kind(15,300)

	integer :: kerncat
	integer :: kernpar
	integer :: freepar
	integer :: parno
	integer :: zweig
	integer :: trees
   	integer :: indi
	integer :: catno
	integer :: A(kerncat,zweig,kernpar)
	integer :: B(kerncat,zweig,kernpar)
	double precision :: C(kerncat,zweig)
	integer :: iDaten(indi,kerncat)
	integer :: Branch(kerncat)
	logical :: noc(kernpar)
	double precision :: x(kernpar)
	double precision :: hess(freepar,freepar)
	integer  ifail
	integer  nout
c	integer irp,irc
	integer n,ntotal,npers,eparno,iseed
c see init
      logical silent
c
       silent=.false.
	  call init(silent)
       call class1fi(n,x,hess,ifail)
       call schluss(silent)

       contains 
c
	 subroutine init(silent)
	 logical silent
	 integer restout, errout,i,j
c
c	 call umach(2,nout)
	 if (silent) then
          nout=9
c       call umach(-2,nout)
c  	 open(nout,file='onefisch.out')
c 	 errout=10
c 	 call umach(-3,errout)
c 	 open(errout,file='onefisch.err')
       endif
c
	 iseed = 0
	 n=0
	 do 5 i=1,kernpar
	 if (noc(i)) n=n+1
 5     continue
       eparno = n
	 catno=kerncat
	 parno=kernpar
	 ntotal=0
	 do 10 i=1,indi
	 do 10 j=1,kerncat
	 ntotal=ntotal+idaten(i,j)
 10    continue
       end subroutine init
c	 
       subroutine schluss(silent)
	 logical silent
       if (silent) then
	  close(9)
	  close(10)
	 endif
	 end subroutine schluss
c
         subroutine rob1(x,pj,dmj)
c berechnet  pj, dmj robust fuer Einsatz in class1fi
	 double precision x(parno), pj(catno)
       double precision hpj(catno,kernpar),p,dmj(kerncat,eparno+1)
	 integer is1,ia,ib,i,j,k,ij,ic,ih,ihs,jh
       double precision d0ha,d0hb,d1ha,d1hb,pz1(kernpar),dx
c
	 do 105 j=1,catno
          pj(j)=0.0d0
c	   ic = (j-1)/kerncat
c	   ij = j - ic*kerncat
	   do 15 ih=1,kernpar
	    hpj(j,ih)=0.0d0
 15      continue
         do 105 k=1,branch(j)
            p=c(j,k)
	      do 25 ih=1,kernpar
c	       is1=index1(ic+1,ih)
	       if (noc(ih)) then
        	       pz1(ih)=p
	       else
	           pz1(ih)=0.0d0
	      endif
 25         continue
            do 90 ih=1,kernpar
c	       i = index1(ic+1,ih)
		   d0ha=1.0d0
		   d0hb=1.0d0
		   d1ha=0.0d0
		   d1hb=0.0d0
             ia=a(j,k,ih)
             if (ia.ne.0) then
	        if (ia.gt.1) d0ha=x(ih)**(ia-1)
              if (noc(ih)) d1ha=ia*d0ha
			d0ha=d0ha*x(ih)
		   endif
	       ib=b(j,k,ih)
	       if (ib.ne.0) then 
			if (ib.gt.1) d0hb=(1-x(ih))**(ib-1)
	         if (noc(ih)) d1hb=-ib*d0hb
               d0hb=d0hb*(1-x(ih))
			endif
	        dx=d0ha*d0hb
              p=p*dx
			do 55 ihs=1, kernpar
c	        is1=index1(ic+1,ihs)
	        if (noc(ihs)) then
			if (ihs.ne.ih) pz1(ihs)=pz1(ihs)*dx
	        if (ihs.eq.ih) pz1(ihs)=pz1(ihs)*(d0ha*d1hb+d1ha*d0hb)
              endif
 55            continue
 90         continue
            pj(j)=pj(j)+p
	      do 104 ihs=1,kernpar
c 	        is1=index1(ic+1,ihs)
	        if (noc(ihs)) hpj(j,ihs)=hpj(j,ihs)+pz1(ihs)
 104        continue
 105        continue
            j=0
            do 200 j=1,kerncat
	      is1=0
c is1 ist Zähler für eparno, gelinkt mit i
	      do 200 i=1,parno
	      if (noc(i)) then
	      is1=is1+1
            dmj(j,is1)=hpj(j,i)
            endif
 200        continue
            do 300 j=1,kerncat
	      dmj(j,eparno+1)=0.0d0
 300        continue
       end subroutine rob1
c      
       subroutine class1fi(n,x,h,ifail)
       integer n, ifail, errout
	 double precision h(n,n),x(parno)
	 double precision pj(catno),dmj(kerncat,eparno+1)
       integer i,j,k,l
c
       call rob1(x,pj,dmj)
c       call first(mix,pj,m1)
	 ifail=0
       do 10 i=1,catno
	 if (pj(i).le.0.0d0) ifail=3
 10    continue
	 if (ifail.eq.0) then
	 do 100 i=1,n
	 do 100 j=1,i
        h(i,j)=0.0d0
       do 100 k=1,catno
	  h(i,j)=h(i,j)+(ntotal)*dmj(k,i)/(pj(k))*dmj(k,j)
 100   continue
       do 200 i=1,n-1
       do 200 j=i+1,n
       h(i,j)=h(j,i)
 200   continue
       endif
c   	   errout=10
c	open(errout,file='onehess.err')
c		write(errout,*) (kerncat)
c		write(errout,*) (kernpar)
c		write(errout,*) (zweig)
c   		write(errout,*) (trees)
c		write(errout,*) (indi)		
c		write(errout,*) (A(i,1,1),i=1,kerncat)
c		write(errout,*) (B(i,1,1),i=1,kerncat)
c		write(errout,*) (branch(i),i=1,kerncat)
c		write(errout,*) (C(i,1),i=1,kerncat)
c		write(errout,*) (iDaten(1,i),i=1,kerncat)
c		write(errout,*) (noc(i),i=1,kernpar)
c		write(errout,*) (x(i),i=1,kernpar)
c		write(errout,*) (hess(1,i),i=1,kernpar+1)
c	    write(errout,*) (hess(2,i),i=1,kernpar+1)
c	    write(errout,*) (ifail)
c	    write(errout,*) (ntotal)
c	    write(errout,*) (n)
c	    write(errout,*) (catno)
c	   write(errout,*) (npers)
c	   write(errout,*) (eparno)
c	   write(errout,*) (h(1,i),i=1,kernpar+1)
c	   write(errout,*) (h(2,i),i=1,kernpar+1)
c	   close(10)
       end subroutine class1fi
c
c
      end subroutine onehess