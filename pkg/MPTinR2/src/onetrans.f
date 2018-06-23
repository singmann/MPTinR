      subroutine onelc(kerncat,kernpar,zweig,trees,indi,a,b,branch,c,
     1  idaten,noc2,x,g2,likeli)
c	use numerical_libraries
c      
c    ! ---- Subroutine export declarations.
c
c    !dec$attributes dllexport, stdcall :: lc
c    !dec$attributes alias:'lc':: lc

c
c    ! ---- Declaration of exported variables.
c
c    !dec$attributes value :: kerncat
c    !dec$attributes value :: kernpar
c    !dec$attributes value :: parno
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
c    !dec$attributes reference :: g2
c    !dec$attributes reference :: likeli
       implicit none

      integer, parameter :: fp = selected_real_kind(15,300)
	integer :: kerncat
	integer :: kernpar
	integer :: zweig
	integer :: trees
	integer :: indi
	integer :: catno
	integer :: A(kerncat,zweig,kernpar)
	integer :: B(kerncat,zweig,kernpar)
	integer :: Branch(kerncat)
	real(fp) :: C(kerncat,zweig)
	integer :: iDaten(indi,kerncat)
	integer :: noc2(kernpar)
	real(fp) :: x(kernpar)
	real(fp) :: likeli
	real(fp) :: g2
c
      integer i,parno
	double precision acc2
      double precision daten(kerncat)
c
       integer iseed,nout
	 integer n,ntotal,eparno
c see init
	 logical silent, noc(kernpar)
c	
c
c      open(1,file='a:ftest.txt')
       silent=.true.
	 call init(silent)
        do 40 i=1,kernpar
	    IF (noc2(i) .EQ. 1) then
	    noc(i)=.TRUE.
	     ELSE 
		 noc(i) = .FALSE.
	     ENDIF
 40     continue
	 acc2=10D-7*sqrt(float(n+1)/25.0D0)

       call kernem(acc2,daten,x)
	 call loglik(x,daten,g2,likeli)
       call schluss(silent)
c
       contains
	 subroutine init(silent)
	 logical silent
	 integer restout, errout,i,j
	 double precision xx
c
c	 call umach(2,nout)
c	 if (silent) then
c          nout=9
c       call umach(-2,nout)
c  	 open(nout,file='onetrans.aus')
c 	 errout=10
c 	 call umach(-3,errout)
c 	 open(errout,file='onetrans.err')
c       endif
c
	 iseed = 0
	 n=0
	 do 5 i=1,parno
	 if (noc(i)) n=n+1
 5     continue
       eparno = n
	 catno=kerncat
	 parno=kernpar
	 ntotal=0
	 do 10 j=1,kerncat
	 daten(j)=0.0D0
	 do 10 i=1,indi
	 ntotal=ntotal+idaten(i,j)
	 daten(j)=daten(j)+1.0D0*idaten(i,j)
 10    continue
c	write(errout,*) indi,ntotal
c	write(errout,*) (idaten(1,i),i=1,kerncat)
       end subroutine init
c	 
       subroutine schluss(silent)
	 logical silent
       if (silent) then
c	  close(9)
c	  close(10)
	 endif
	 end subroutine schluss
c
c
	 subroutine est0(daten,x,m,pj)
c berechnet  pj, und mij fuer Klasse ic, entspricht altem estimate
	 double precision x(parno), pj(catno)
	 double precision m(kerncat,zweig)
	 double precision d0ha,d0hb, daten(catno)
	 integer ih,i,j,k,ij,ia,ib
c
	 do 100 j=1,kerncat
         pj(j)=0
	   do 100 k=1,branch(j)
            m(j,k)=c(j,k)
            do 90 i=1,kernpar
		   d0ha=1
		   d0hb=1
	       ia=a(j,k,i)
             if (ia.ne.0) d0ha=x(i)**ia
             ib=b(j,k,i)
	       if (ib.ne.0) d0hb=(1-x(i))**ib
              m(j,k)=m(j,k)*d0ha*d0hb
 90         continue
            pj(j)=pj(j)+m(j,k)
 100        continue           
       do 200 j=1,kerncat
       do 200 k=1,branch(j)
       if (daten(j).eq.0) then
	    m(j,k)=0
       else
	  if (m(j,k).ne.0) m(j,k)=(daten(j)*m(j,k)) / pj(j)
       endif
 200   continue
       end subroutine est0
c       
	 subroutine maximize(daten,x,pj)
c	 e-Schritt wird aufgerufen,
c      dann m-Schritt in innerem EM-Algorithmus; update von x
	 double precision m(kerncat,zweig)
       double precision pj(catno), daten(catno)
       double precision x(parno)
c	 external dset
c
       integer i,j,k,ic,ih
       double precision yz(parno),yn(parno)
c
       call dset(parno,0.0D0,yz,1)
	 call dset(parno,0.0D0,yn,1) 
	 call est0(daten,x,m,pj)
       do 100 i=1,kernpar
         if (noc(i)) then          
	    do  50 j=1,kerncat
          do  50 k=1,branch(j)
                yz(i)=yz(i)+a(j,k,i)*m(j,k)
                yn(i)=yn(i)+b(j,k,i)*m(j,k)
 50       continue
         endif 
 100   continue
       do 200 i=1,parno
	 if (noc(i)) then
	 if (yz(i).ne.0) x(i)=yz(i)/(yz(i)+yn(i))
       if (yz(i).eq.0) x(i)=0
	 endif
 200   continue
       end subroutine maximize
c
	 subroutine kernem(acc,daten,x)
c	 kontrolliert inneren em-Algorithmus, wie frueher control
       double precision x(parno), daten(catno)
       double precision acc, pj(catno)
c	 external ddisl2,dcopy
c
       double precision xalt(parno)
       integer i
c
 10    call dcopy(parno,x,1,xalt,1)
       call maximize(daten,x,pj)
      if (ddisl2(parno,x,1,xalt,1).ge.acc) goto 10
      end subroutine kernem
c
c
	subroutine loglik(x,daten,g2,likeli)
	implicit none
c berechnet log-likelihood
	 double precision x(parno), pj(catno), m(kerncat,zweig)
       double precision daten(catno), y
	 integer j
	 double precision g2, likeli
c
	 call est0(daten,x,m,pj)
       likeli = 0
	 g2 = 0
       do 20 j=1,catno
       if (daten(j).gt.0) then
	  g2=g2 + 2*daten(j)*(dlog(daten(j)/(pj(j)*ntotal)))
	  likeli=likeli - 2*daten(j)*dlog(pj(j))
	 endif
 20    continue
       end subroutine loglik
c
       subroutine dset(k,c,x,step)
       integer k,step,i
       double precision c,x(k)
       do 100 i=1,k,step
        x(i)=c
 100   continue
       end subroutine dset
c
       subroutine dcopy(k,x,step1,x2,step2)
       integer k,step1,step2,i
       double precision x(k),x2(k)
       do 100 i=1,k
        x2(i)=x(i)
 100   continue
       end subroutine dcopy
c 
       double precision function ddisl2(N, X, INCX, Y, INCY)
       integer n,incx,incy
       double precision x(n),y(n),h
       h=0.0d0
       do 100 i=1,n
        h=h+ (x(i)-y(i))**2
 100   continue
        ddisl2=dsqrt(h)
       end function ddisl2       
c 
       end subroutine onelc
