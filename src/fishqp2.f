c==== 
c==== these quadpack routines were downloaded from the Netlib 
C==== mathematical library at:
c==== http://www.netlib.no/
c==== under http://www.netlib.no/netlib/quadpack/index.html
c====
C==== See also:  
C==== R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner
C====   Quadpack: a Subroutine Package for Automatic Integration
C====   Springer Verlag, 1983.    Series in Computational Mathematics v.1
C====   515.43/Q1S  100394Z
C====
C==== there do not appear to be any copyright restrictions, though
C==== the usual rules for "shareware" should be applied !
C====
c==== RKH, May 1999
c====
c==== RKH 15/6/99 SPECIAL version for FISH, with SINGLE precision
c==== interface and hardwired workspace for max 512 subdivisions
c==== Copies ONE,TWO & THREE to allow recursive calls in 2 or 3 dimensional
c==== integrals.  N.B. change name of /INTEG1/ common block
c==== I don't think dqelg, dpsrt and d1mach need to be repeatd ?
c
      subroutine dqagsTWO(f,a1,b1,epsabs1,epsrel1,result1,abserr1,
     *   neval,ier,krule)
c
c====      subroutine dqags(f,a,b,epsabs,epsrel,result,abserr,neval,ier,
c====     *   limit,lenw,last,iwork,work)
c***begin prologue  dqags
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a1
c***keywords  automatic integrator, general-purpose,
c             (end-point) singularities, extrapolation,
c             globally adaptive
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & prog. div. - k.u.leuven
c***purpose  the routine calculates an approximation result to a given
c            definite integral  i = integral of f over (a,b),
c            hopefully satisfying following claim for accuracy
c            abs(i-result).le.max(epsabs,epsrel*abs(i)).
c***description
c
c        computation of a definite integral
c        standard fortran subroutine
c        double precision version
c
c
c        parameters
c         on entry
c            f      - was double precision, NOW single
c                     function subprogram defining the integrand
c                     function f(x). the actual name for f needs to be
c                     declared e x t e r n a l in the driver program.
c
c            a      - double precision
c                     lower limit of integration
c
c            b      - double precision
c                     upper limit of integration
c
c            epsabs - double precision
c                     absolute accuracy requested
c            epsrel - double precision
c                     relative accuracy requested
c                     if  epsabs.le.0
c                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
c                     the routine will end with ier = 6.
c
c         on return
c            result - double precision
c                     approximation to the integral
c
c            abserr - double precision
c                     estimate of the modulus of the absolute error,
c                     which should equal or exceed abs(i-result)
c
c            neval  - integer
c                     number of integrand evaluations
c
c            ier    - integer
c                     ier = 0 normal and reliable termination of the
c                             routine. it is assumed that the requested
c                             accuracy has been achieved.
c                     ier.gt.0 abnormal termination of the routine
c                             the estimates for integral and error are
c                             less reliable. it is assumed that the
c                             requested accuracy has not been achieved.
c            error messages
c                     ier = 1 maximum number of subdivisions allowed
c                             has been achieved. one can allow more sub-
c                             divisions by increasing the value of limit
c                             (and taking the according dimension
c                             adjustments into account. however, if
c                             this yields no improvement it is advised
c                             to analyze the integrand in order to
c                             determine the integration difficulties. if
c                             the position of a local difficulty can be
c                             determined (e.g. singularity,
c                             discontinuity within the interval) one
c                             will probably gain from splitting up the
c                             interval at this point and calling the
c                             integrator on the subranges. if possible,
c                             an appropriate special-purpose integrator
c                             should be used, which is designed for
c                             handling the type of difficulty involved.
c                         = 2 the occurrence of roundoff error is detec-
c                             ted, which prevents the requested
c                             tolerance from being achieved.
c                             the error may be under-estimated.
c                         = 3 extremely bad integrand behaviour
c                             occurs at some points of the integration
c                             interval.
c                         = 4 the algorithm does not converge.
c                             roundoff error is detected in the
c                             extrapolation table. it is presumed that
c                             the requested tolerance cannot be
c                             achieved, and that the returned result is
c                             the best which can be obtained.
c                         = 5 the integral is probably divergent, or
c                             slowly convergent. it must be noted that
c                             divergence can occur with any other value
c                             of ier.
c                         = 6 the input is invalid, because
c                             (epsabs.le.0 and
c                              epsrel.lt.max(50*rel.mach.acc.,0.5d-28)
c                             or limit.lt.1 or lenw.lt.limit*4.
c                             result, abserr, neval, last are set to
c                             zero.except when limit or lenw is invalid,
c                             iwork(1), work(limit*2+1) and
c                             work(limit*3+1) are set to zero, work(1)
c                             is set to a and work(limit+1) to b.
c
c         dimensioning parameters
c            limit - integer
c                    dimensioning parameter for iwork
c                    limit determines the maximum number of subintervals
c                    in the partition of the given integration interval
c                    (a,b), limit.ge.1.
c                    if limit.lt.1, the routine will end with ier = 6.
c
c            lenw  - integer
c                    dimensioning parameter for work
c                    lenw must be at least limit*4.
c                    if lenw.lt.limit*4, the routine will end
c                    with ier = 6.
c
c            last  - integer
c                    on return, last equals the number of subintervals
c                    produced in the subdivision process, detemines the
c                    number of significant elements actually in the work
c                    arrays.
c
c         work arrays
c            iwork - integer
c                    vector of dimension at least limit, the first k
c                    elements of which contain pointers
c                    to the error estimates over the subintervals
c                    such that work(limit*3+iwork(1)),... ,
c                    work(limit*3+iwork(k)) form a decreasing
c                    sequence, with k = last if last.le.(limit/2+2),
c                    and k = limit+1-last otherwise
c
c            work  - double precision
c                    vector of dimension at least lenw
c                    on return
c                    work(1), ..., work(last) contain the left
c                     end-points of the subintervals in the
c                     partition of (a,b),
c                    work(limit+1), ..., work(limit+last) contain
c                     the right end-points,
c                    work(limit*2+1), ..., work(limit*2+last) contain
c                     the integral approximations over the subintervals,
c                    work(limit*3+1), ..., work(limit*3+last)
c                     contain the error estimates.
c
c***references  (none)
c***routines called  dqagse,xerror
c***end prologue  dqags
c
c====
c====
      real  a1,b1,abserr1,epsab1,epsrel1,f,result1
c====
      double precision a,abserr,b,epsabs,epsrel,result,work
      integer ier,iwork,last,lenw,limit,lvl,l1,l2,l3,neval
C
      common/INTEG2/lenw,limit,work(2001),iwork(500)
      data lenw/2001/,limit/500/
c
c==== now in common:      dimension iwork(limit),work(lenw)
c
      external f
c
c         check validity of limit and lenw.
c
c***first executable statement  dqags
      limit=500
      lenw=2048
      a=dble(a1)
      b=dble(b1)
      epsabs=dble(epsabs1)
      epsrel=dble(epsrel1)
c
      ier = 6
      neval = 0
      last = 0
      result = 0.0d+00
      abserr = 0.0d+00
      if(limit.lt.1.or.lenw.lt.limit*4) go to 10
c
c         prepare call for dqagse.
c
      l1 = limit+1
      l2 = limit+l1
      l3 = limit+l2
c
      call dqagseTWO(f,a,b,epsabs,epsrel,limit,result,abserr,neval,
     *  ier,work(1),work(l1),work(l2),work(l3),iwork,last,krule)
c====
      result1=sngl(result)
      abserr1=sngl(abserr)
c
c         call error handler if necessary.
c
      lvl = 0
10    if(ier.eq.6) lvl = 1
      if(ier.ne.0) call messageTWO(ier)
      ier=0
      return
      end
c
c==== this to replace XERROR call
C==== RKH 11/6/99
      subroutine messageTWO(ier)
      goto (1,2,3,4,5,6),ier
      write(6,100)ier
      return
1     write(6,101)
      return
2     write(6,102)
      return
3     write(6,103)
      return
4     write(6,104)
      return
5     write(6,105)
      return
6     write(6,106)
      return
100   format(1x,'ERROR dqagsTWO(: integration invalid  ier =',i12)
101   format(1x,'ERROR dqagsTWO(: max no of sub-divisions')
102   format(1x,'WARN dqagsTWO(: round-off underestimates integ. error ?')
103   format(1x,'WARN dqagsTWO(: bad integrand behaviour')
104   format(1x,'WARN dqagsTWO(: poor convergence')
105   format(1x,'ERROR dqagsTWO(: very poor convergence ')
106   format(1x,'ERROR dqagsTWO(: invalid input parameters ')
      end     
      subroutine dqagseTWO(f,a,b,epsabs,epsrel,limit,result,abserr,neval,
     *   ier,alist,blist,rlist,elist,iord,last,krule)
c==== 14/6/99 RKH
c==== pass f as single, and krule to use
c
c***begin prologue  dqagse
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a1
c***keywords  automatic integrator, general-purpose,
c             (end point) singularities, extrapolation,
c             globally adaptive
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c***purpose  the routine calculates an approximation result to a given
c            definite integral i = integral of f over (a,b),
c            hopefully satisfying following claim for accuracy
c            abs(i-result).le.max(epsabs,epsrel*abs(i)).
c***description
c
c        computation of a definite integral
c        standard fortran subroutine
c        double precision version
c
c        parameters
c         on entry
c            f      - double precision
c                     function subprogram defining the integrand
c                     function f(x). the actual name for f needs to be
c                     declared e x t e r n a l in the driver program.
c
c            a      - double precision
c                     lower limit of integration
c
c            b      - double precision
c                     upper limit of integration
c
c            epsabs - double precision
c                     absolute accuracy requested
c            epsrel - double precision
c                     relative accuracy requested
c                     if  epsabs.le.0
c                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
c                     the routine will end with ier = 6.
c
c            limit  - integer
c                     gives an upperbound on the number of subintervals
c                     in the partition of (a,b)
c
c         on return
c            result - double precision
c                     approximation to the integral
c
c            abserr - double precision
c                     estimate of the modulus of the absolute error,
c                     which should equal or exceed abs(i-result)
c
c            neval  - integer
c                     number of integrand evaluations
c
c            ier    - integer
c                     ier = 0 normal and reliable termination of the
c                             routine. it is assumed that the requested
c                             accuracy has been achieved.
c                     ier.gt.0 abnormal termination of the routine
c                             the estimates for integral and error are
c                             less reliable. it is assumed that the
c                             requested accuracy has not been achieved.
c            error messages
c                         = 1 maximum number of subdivisions allowed
c                             has been achieved. one can allow more sub-
c                             divisions by increasing the value of limit
c                             (and taking the according dimension
c                             adjustments into account). however, if
c                             this yields no improvement it is advised
c                             to analyze the integrand in order to
c                             determine the integration difficulties. if
c                             the position of a local difficulty can be
c                             determined (e.g. singularity,
c                             discontinuity within the interval) one
c                             will probably gain from splitting up the
c                             interval at this point and calling the
c                             integrator on the subranges. if possible,
c                             an appropriate special-purpose integrator
c                             should be used, which is designed for
c                             handling the type of difficulty involved.
c                         = 2 the occurrence of roundoff error is detec-
c                             ted, which prevents the requested
c                             tolerance from being achieved.
c                             the error may be under-estimated.
c                         = 3 extremely bad integrand behaviour
c                             occurs at some points of the integration
c                             interval.
c                         = 4 the algorithm does not converge.
c                             roundoff error is detected in the
c                             extrapolation table.
c                             it is presumed that the requested
c                             tolerance cannot be achieved, and that the
c                             returned result is the best which can be
c                             obtained.
c                         = 5 the integral is probably divergent, or
c                             slowly convergent. it must be noted that
c                             divergence can occur with any other value
c                             of ier.
c                         = 6 the input is invalid, because
c                             epsabs.le.0 and
c                             epsrel.lt.max(50*rel.mach.acc.,0.5d-28).
c                             result, abserr, neval, last, rlist(1),
c                             iord(1) and elist(1) are set to zero.
c                             alist(1) and blist(1) are set to a and b
c                             respectively.
c
c            alist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the left end points
c                     of the subintervals in the partition of the
c                     given integration range (a,b)
c
c            blist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the right end points
c                     of the subintervals in the partition of the given
c                     integration range (a,b)
c
c            rlist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the integral
c                     approximations on the subintervals
c
c            elist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the moduli of the
c                     absolute error estimates on the subintervals
c
c            iord   - integer
c                     vector of dimension at least limit, the first k
c                     elements of which are pointers to the
c                     error estimates over the subintervals,
c                     such that elist(iord(1)), ..., elist(iord(k))
c                     form a decreasing sequence, with k = last
c                     if last.le.(limit/2+2), and k = limit+1-last
c                     otherwise
c
c            last   - integer
c                     number of subintervals actually produced in the
c                     subdivision process
c
c***references  (none)
c***routines called  d1mach,dqelg,dqk21,dqpsrt
c***end prologue  dqagse
c
c====
      real f
c====
      double precision a,abseps,abserr,alist,area,area1,area12,area2,a1,
     *  a2,b,blist,b1,b2,correc,dabs,defabs,defab1,defab2,d1mach,dmax1,
     *  dres,elist,epmach,epsabs,epsrel,erlarg,erlast,errbnd,errmax,
     *  error1,error2,erro12,errsum,ertest,oflow,resabs,reseps,result,
     *  res3la,rlist,rlist2,small,uflow
      integer id,ier,ierro,iord,iroff1,iroff2,iroff3,jupbnd,k,ksgn,
     *  ktmin,last,limit,maxerr,neval,nres,nrmax,numrl2
      logical extrap,noext
c
      dimension alist(limit),blist(limit),elist(limit),iord(limit),
     * res3la(3),rlist(limit),rlist2(52)
c
      external f
c
c            the dimension of rlist2 is determined by the value of
c            limexp in subroutine dqelg (rlist2 should be of dimension
c            (limexp+2) at least).
c
c            list of major variables
c            -----------------------
c
c           alist     - list of left end points of all subintervals
c                       considered up to now
c           blist     - list of right end points of all subintervals
c                       considered up to now
c           rlist(i)  - approximation to the integral over
c                       (alist(i),blist(i))
c           rlist2    - array of dimension at least limexp+2 containing
c                       the part of the epsilon table which is still
c                       needed for further computations
c           elist(i)  - error estimate applying to rlist(i)
c           maxerr    - pointer to the interval with largest error
c                       estimate
c           errmax    - elist(maxerr)
c           erlast    - error on the interval currently subdivided
c                       (before that subdivision has taken place)
c           area      - sum of the integrals over the subintervals
c           errsum    - sum of the errors over the subintervals
c           errbnd    - requested accuracy max(epsabs,epsrel*
c                       abs(result))
c           *****1    - variable for the left interval
c           *****2    - variable for the right interval
c           last      - index for subdivision
c           nres      - number of calls to the extrapolation routine
c           numrl2    - number of elements currently in rlist2. if an
c                       appropriate approximation to the compounded
c                       integral has been obtained it is put in
c                       rlist2(numrl2) after numrl2 has been increased
c                       by one.
c           small     - length of the smallest interval considered up
c                       to now, multiplied by 1.5
c           erlarg    - sum of the errors over the intervals larger
c                       than the smallest interval considered up to now
c           extrap    - logical variable denoting that the routine is
c                       attempting to perform extrapolation i.e. before
c                       subdividing the smallest interval we try to
c                       decrease the value of erlarg.
c           noext     - logical variable denoting that extrapolation
c                       is no longer allowed (true value)
c
c            machine dependent constants
c            ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c           oflow is the largest positive magnitude.
c
c***first executable statement  dqagse
      epmach = d1mach(4)
c
c            test on validity of parameters
c            ------------------------------
      ier = 0
      neval = 0
      last = 0
      result = 0.0d+00
      abserr = 0.0d+00
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      if(epsabs.le.0.0d+00.and.epsrel.lt.dmax1(0.5d+02*epmach,0.5d-28))
     *   ier = 6
      if(ier.eq.6) go to 999
c
c           first approximation to the integral
c           -----------------------------------
c
      uflow = d1mach(1)
      oflow = d1mach(2)
      ierro = 0
c
c==== try different routines  RKH 14/6/99
c
      if(krule.eq.0)call dqk21TWO(f,a,b,result,abserr,defabs,resabs)
      if(krule.eq.1)call dqk31TWO(f,a,b,result,abserr,defabs,resabs)
      if(krule.eq.2)call dqk61TWO(f,a,b,result,abserr,defabs,resabs)
c
c           test on accuracy.
c
      dres = dabs(result)
      errbnd = dmax1(epsabs,epsrel*dres)
      last = 1
      rlist(1) = result
      elist(1) = abserr
      iord(1) = 1
      if(abserr.le.1.0d+02*epmach*defabs.and.abserr.gt.errbnd) ier = 2
      if(limit.eq.1) ier = 1
      if(ier.ne.0.or.(abserr.le.errbnd.and.abserr.ne.resabs).or.
     *  abserr.eq.0.0d+00) go to 140
c
c           initialization
c           --------------
c
      rlist2(1) = result
      errmax = abserr
      maxerr = 1
      area = result
      errsum = abserr
      abserr = oflow
      nrmax = 1
      nres = 0
      numrl2 = 2
      ktmin = 0
      extrap = .false.
      noext = .false.
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      if(dres.ge.(0.1d+01-0.5d+02*epmach)*defabs) ksgn = 1
c
c           main do-loop
c           ------------
c
      do 90 last = 2,limit
c
c           bisect the subinterval with the nrmax-th largest error
c           estimate.
c
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        erlast = errmax
        if(krule.eq.0)then
        call dqk21TWO(f,a1,b1,area1,error1,resabs,defab1)
        call dqk21TWO(f,a2,b2,area2,error2,resabs,defab2)
        else if(krule.eq.1)then
        call dqk31TWO(f,a1,b1,area1,error1,resabs,defab1)
        call dqk31TWO(f,a2,b2,area2,error2,resabs,defab2)
        else if(krule.eq.2)then
        call dqk61TWO(f,a1,b1,area1,error1,resabs,defab1)
        call dqk61TWO(f,a2,b2,area2,error2,resabs,defab2)
        end if
c
c           improve previous approximations to integral
c           and error and test for accuracy.
c
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if(defab1.eq.error1.or.defab2.eq.error2) go to 15
        if(dabs(rlist(maxerr)-area12).gt.0.1d-04*dabs(area12)
     *  .or.erro12.lt.0.99d+00*errmax) go to 10
        if(extrap) iroff2 = iroff2+1
        if(.not.extrap) iroff1 = iroff1+1
   10   if(last.gt.10.and.erro12.gt.errmax) iroff3 = iroff3+1
   15   rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = dmax1(epsabs,epsrel*dabs(area))
c
c           test for roundoff error and eventually set error flag.
c
        if(iroff1+iroff2.ge.10.or.iroff3.ge.20) ier = 2
        if(iroff2.ge.5) ierro = 3
c
c           set error flag in the case that the number of subintervals
c           equals limit.
c
        if(last.eq.limit) ier = 1
c
c           set error flag in the case of bad integrand behaviour
c           at a point of the integration range.
c
        if(dmax1(dabs(a1),dabs(b2)).le.(0.1d+01+0.1d+03*epmach)*
     *  (dabs(a2)+0.1d+04*uflow)) ier = 4
c
c           append the newly-created intervals to the list.
c
        if(error2.gt.error1) go to 20
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 30
   20   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
c
c           call subroutine dqpsrt to maintain the descending ordering
c           in the list of error estimates and select the subinterval
c           with nrmax-th largest error estimate (to be bisected next).
c
   30   call dqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)
c ***jump out of do-loop
        if(errsum.le.errbnd) go to 115
c ***jump out of do-loop
        if(ier.ne.0) go to 100
        if(last.eq.2) go to 80
        if(noext) go to 90
        erlarg = erlarg-erlast
        if(dabs(b1-a1).gt.small) erlarg = erlarg+erro12
        if(extrap) go to 40
c
c           test whether the interval to be bisected next is the
c           smallest interval.
c
        if(dabs(blist(maxerr)-alist(maxerr)).gt.small) go to 90
        extrap = .true.
        nrmax = 2
   40   if(ierro.eq.3.or.erlarg.le.ertest) go to 60
c
c           the smallest interval has the largest error.
c           before bisecting decrease the sum of the errors over the
c           larger intervals (erlarg) and perform extrapolation.
c
        id = nrmax
        jupbnd = last
        if(last.gt.(2+limit/2)) jupbnd = limit+3-last
        do 50 k = id,jupbnd
          maxerr = iord(nrmax)
          errmax = elist(maxerr)
c ***jump out of do-loop
          if(dabs(blist(maxerr)-alist(maxerr)).gt.small) go to 90
          nrmax = nrmax+1
   50   continue
c
c           perform extrapolation.
c
   60   numrl2 = numrl2+1
        rlist2(numrl2) = area
        call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
        ktmin = ktmin+1
        if(ktmin.gt.5.and.abserr.lt.0.1d-02*errsum) ier = 5
        if(abseps.ge.abserr) go to 70
        ktmin = 0
        abserr = abseps
        result = reseps
        correc = erlarg
        ertest = dmax1(epsabs,epsrel*dabs(reseps))
c ***jump out of do-loop
        if(abserr.le.ertest) go to 100
c
c           prepare bisection of the smallest interval.
c
   70   if(numrl2.eq.1) noext = .true.
        if(ier.eq.5) go to 100
        maxerr = iord(1)
        errmax = elist(maxerr)
        nrmax = 1
        extrap = .false.
        small = small*0.5d+00
        erlarg = errsum
        go to 90
   80   small = dabs(b-a)*0.375d+00
        erlarg = errsum
        ertest = errbnd
        rlist2(2) = area
   90 continue
c
c           set final result and error estimate.
c           ------------------------------------
c
  100 if(abserr.eq.oflow) go to 115
      if(ier+ierro.eq.0) go to 110
      if(ierro.eq.3) abserr = abserr+correc
      if(ier.eq.0) ier = 3
      if(result.ne.0.0d+00.and.area.ne.0.0d+00) go to 105
      if(abserr.gt.errsum) go to 115
      if(area.eq.0.0d+00) go to 130
      go to 110
  105 if(abserr/dabs(result).gt.errsum/dabs(area)) go to 115
c
c           test on divergence.
c
  110 if(ksgn.eq.(-1).and.dmax1(dabs(result),dabs(area)).le.
     * defabs*0.1d-01) go to 130
      if(0.1d-01.gt.(result/area).or.(result/area).gt.0.1d+03
     * .or.errsum.gt.dabs(area)) ier = 6
      go to 130
c
c           compute global integral sum.
c
  115 result = 0.0d+00
      do 120 k = 1,last
         result = result+rlist(k)
  120 continue
      abserr = errsum
  130 if(ier.gt.2) ier = ier-1
  140 continue
c====
      if(krule.eq.0)neval =  42*last-21
      if(krule.eq.1)neval =  62*last-31
      if(krule.eq.2)neval = 122*last-61
  999 return
      end
c
      subroutine dqk21TWO(f,a,b,result,abserr,resabs,resasc)
c***begin prologue  dqk21
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a2
c***keywords  21-point gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c***purpose  to compute i = integral of f over (a,b), with error
c                           estimate
c                       j = integral of abs(f) over (a,b)
c***description
c
c           integration rules
c           standard fortran subroutine
c           double precision version
c
c           parameters
c            on entry
c              f      - double precision
c                       function subprogram defining the integrand
c                       function f(x). the actual name for f needs to be
c                       declared e x t e r n a l in the driver program.
c
c              a      - double precision
c                       lower limit of integration
c
c              b      - double precision
c                       upper limit of integration
c
c            on return
c              result - double precision
c                       approximation to the integral i
c                       result is computed by applying the 21-point
c                       kronrod rule (resk) obtained by optimal addition
c                       of abscissae to the 10-point gauss rule (resg).
c
c              abserr - double precision
c                       estimate of the modulus of the absolute error,
c                       which should not exceed abs(i-result)
c
c              resabs - double precision
c                       approximation to the integral j
c
c              resasc - double precision
c                       approximation to the integral of abs(f-i/(b-a))
c                       over (a,b)
c
c***references  (none)
c***routines called  d1mach
c***end prologue  dqk21
c
      double precision a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,
     *  d1mach,epmach,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc,
     *  resg,resk,reskh,result,uflow,wg,wgk,xgk
      integer j,jtw,jtwm1
      real f
      external f
c
      dimension fv1(10),fv2(10),wg(5),wgk(11),xgk(11)
c
c           the abscissae and weights are given for the interval (-1,1).
c           because of symmetry only the positive abscissae and their
c           corresponding weights are given.
c
c           xgk    - abscissae of the 21-point kronrod rule
c                    xgk(2), xgk(4), ...  abscissae of the 10-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 10-point gauss rule
c
c           wgk    - weights of the 21-point kronrod rule
c
c           wg     - weights of the 10-point gauss rule
c
c
c gauss quadrature weights and kronron quadrature abscissae and weights
c as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
c bell labs, nov. 1981.
c
      data wg  (  1) / 0.0666713443 0868813759 3568809893 332 d0 /
      data wg  (  2) / 0.1494513491 5058059314 5776339657 697 d0 /
      data wg  (  3) / 0.2190863625 1598204399 5534934228 163 d0 /
      data wg  (  4) / 0.2692667193 0999635509 1226921569 469 d0 /
      data wg  (  5) / 0.2955242247 1475287017 3892994651 338 d0 /
c
      data xgk (  1) / 0.9956571630 2580808073 5527280689 003 d0 /
      data xgk (  2) / 0.9739065285 1717172007 7964012084 452 d0 /
      data xgk (  3) / 0.9301574913 5570822600 1207180059 508 d0 /
      data xgk (  4) / 0.8650633666 8898451073 2096688423 493 d0 /
      data xgk (  5) / 0.7808177265 8641689706 3717578345 042 d0 /
      data xgk (  6) / 0.6794095682 9902440623 4327365114 874 d0 /
      data xgk (  7) / 0.5627571346 6860468333 9000099272 694 d0 /
      data xgk (  8) / 0.4333953941 2924719079 9265943165 784 d0 /
      data xgk (  9) / 0.2943928627 0146019813 1126603103 866 d0 /
      data xgk ( 10) / 0.1488743389 8163121088 4826001129 720 d0 /
      data xgk ( 11) / 0.0000000000 0000000000 0000000000 000 d0 /
c
      data wgk (  1) / 0.0116946388 6737187427 8064396062 192 d0 /
      data wgk (  2) / 0.0325581623 0796472747 8818972459 390 d0 /
      data wgk (  3) / 0.0547558965 7435199603 1381300244 580 d0 /
      data wgk (  4) / 0.0750396748 1091995276 7043140916 190 d0 /
      data wgk (  5) / 0.0931254545 8369760553 5065465083 366 d0 /
      data wgk (  6) / 0.1093871588 0229764189 9210590325 805 d0 /
      data wgk (  7) / 0.1234919762 6206585107 7958109831 074 d0 /
      data wgk (  8) / 0.1347092173 1147332592 8054001771 707 d0 /
      data wgk (  9) / 0.1427759385 7706008079 7094273138 717 d0 /
      data wgk ( 10) / 0.1477391049 0133849137 4841515972 068 d0 /
      data wgk ( 11) / 0.1494455540 0291690566 4936468389 821 d0 /
c
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           absc   - abscissa
c           fval*  - function value
c           resg   - result of the 10-point gauss formula
c           resk   - result of the 21-point kronrod formula
c           reskh  - approximation to the mean value of f over (a,b),
c                    i.e. to i/(b-a)
c
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
c***first executable statement  dqk21
      epmach = d1mach(4)
      uflow = d1mach(1)
c
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
c
c           compute the 21-point kronrod approximation to
c           the integral, and estimate the absolute error.
c
      resg = 0.0d+00
      fc = dble(f(sngl(centr)))
      resk = wgk(11)*fc
      resabs = dabs(resk)
      do 10 j=1,5
        jtw = 2*j
        absc = hlgth*xgk(jtw)
        fval1 = dble(f(sngl(centr-absc)))
        fval2 = dble(f(sngl(centr+absc)))
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,5
        jtwm1 = 2*j-1
        absc = hlgth*xgk(jtwm1)
        fval1 = dble(f(sngl(centr-absc)))
        fval2 = dble(f(sngl(centr+absc)))
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(11)*dabs(fc-reskh)
      do 20 j=1,10
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if(resasc.ne.0.0d+00.and.abserr.ne.0.0d+00)
     *  abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if(resabs.gt.uflow/(0.5d+02*epmach)) abserr = dmax1
     *  ((epmach*0.5d+02)*resabs,abserr)
      return
      end
      subroutine dqk31TWO(f,a,b,result,abserr,resabs,resasc)
c***begin prologue  dqk31
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a2
c***keywords  31-point gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c***purpose  to compute i = integral of f over (a,b) with error
c                           estimate
c                       j = integral of abs(f) over (a,b)
c***description
c
c           integration rules
c           standard fortran subroutine
c           double precision version
c
c           parameters
c            on entry
c              f      - double precision
c                       function subprogram defining the integrand
c                       function f(x). the actual name for f needs to be
c                       declared e x t e r n a l in the calling program.
c
c              a      - double precision
c                       lower limit of integration
c
c              b      - double precision
c                       upper limit of integration
c
c            on return
c              result - double precision
c                       approximation to the integral i
c                       result is computed by applying the 31-point
c                       gauss-kronrod rule (resk), obtained by optimal
c                       addition of abscissae to the 15-point gauss
c                       rule (resg).
c
c              abserr - double precison
c                       estimate of the modulus of the modulus,
c                       which should not exceed abs(i-result)
c
c              resabs - double precision
c                       approximation to the integral j
c
c              resasc - double precision
c                       approximation to the integral of abs(f-i/(b-a))
c                       over (a,b)
c
c***references  (none)
c***routines called  d1mach
c***end prologue  dqk31
      double precision a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,
     *  d1mach,epmach,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc,
     *  resg,resk,reskh,result,uflow,wg,wgk,xgk
      integer j,jtw,jtwm1
      real f
      external f
c
      dimension fv1(15),fv2(15),xgk(16),wgk(16),wg(8)
c
c           the abscissae and weights are given for the interval (-1,1).
c           because of symmetry only the positive abscissae and their
c           corresponding weights are given.
c
c           xgk    - abscissae of the 31-point kronrod rule
c                    xgk(2), xgk(4), ...  abscissae of the 15-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 15-point gauss rule
c
c           wgk    - weights of the 31-point kronrod rule
c
c           wg     - weights of the 15-point gauss rule
c
c
c gauss quadrature weights and kronron quadrature abscissae and weights
c as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
c bell labs, nov. 1981.
c
      data wg  (  1) / 0.0307532419 9611726835 4628393577 204 d0 /
      data wg  (  2) / 0.0703660474 8810812470 9267416450 667 d0 /
      data wg  (  3) / 0.1071592204 6717193501 1869546685 869 d0 /
      data wg  (  4) / 0.1395706779 2615431444 7804794511 028 d0 /
      data wg  (  5) / 0.1662692058 1699393355 3200860481 209 d0 /
      data wg  (  6) / 0.1861610000 1556221102 6800561866 423 d0 /
      data wg  (  7) / 0.1984314853 2711157645 6118326443 839 d0 /
      data wg  (  8) / 0.2025782419 2556127288 0620199967 519 d0 /
c
      data xgk (  1) / 0.9980022986 9339706028 5172840152 271 d0 /
      data xgk (  2) / 0.9879925180 2048542848 9565718586 613 d0 /
      data xgk (  3) / 0.9677390756 7913913425 7347978784 337 d0 /
      data xgk (  4) / 0.9372733924 0070590430 7758947710 209 d0 /
      data xgk (  5) / 0.8972645323 4408190088 2509656454 496 d0 /
      data xgk (  6) / 0.8482065834 1042721620 0648320774 217 d0 /
      data xgk (  7) / 0.7904185014 4246593296 7649294817 947 d0 /
      data xgk (  8) / 0.7244177313 6017004741 6186054613 938 d0 /
      data xgk (  9) / 0.6509967412 9741697053 3735895313 275 d0 /
      data xgk ( 10) / 0.5709721726 0853884753 7226737253 911 d0 /
      data xgk ( 11) / 0.4850818636 4023968069 3655740232 351 d0 /
      data xgk ( 12) / 0.3941513470 7756336989 7207370981 045 d0 /
      data xgk ( 13) / 0.2991800071 5316881216 6780024266 389 d0 /
      data xgk ( 14) / 0.2011940939 9743452230 0628303394 596 d0 /
      data xgk ( 15) / 0.1011420669 1871749902 7074231447 392 d0 /
      data xgk ( 16) / 0.0000000000 0000000000 0000000000 000 d0 /
c
      data wgk (  1) / 0.0053774798 7292334898 7792051430 128 d0 /
      data wgk (  2) / 0.0150079473 2931612253 8374763075 807 d0 /
      data wgk (  3) / 0.0254608473 2671532018 6874001019 653 d0 /
      data wgk (  4) / 0.0353463607 9137584622 2037948478 360 d0 /
      data wgk (  5) / 0.0445897513 2476487660 8227299373 280 d0 /
      data wgk (  6) / 0.0534815246 9092808726 5343147239 430 d0 /
      data wgk (  7) / 0.0620095678 0067064028 5139230960 803 d0 /
      data wgk (  8) / 0.0698541213 1872825870 9520077099 147 d0 /
      data wgk (  9) / 0.0768496807 5772037889 4432777482 659 d0 /
      data wgk ( 10) / 0.0830805028 2313302103 8289247286 104 d0 /
      data wgk ( 11) / 0.0885644430 5621177064 7275443693 774 d0 /
      data wgk ( 12) / 0.0931265981 7082532122 5486872747 346 d0 /
      data wgk ( 13) / 0.0966427269 8362367850 5179907627 589 d0 /
      data wgk ( 14) / 0.0991735987 2179195933 2393173484 603 d0 /
      data wgk ( 15) / 0.1007698455 2387559504 4946662617 570 d0 /
      data wgk ( 16) / 0.1013300070 1479154901 7374792767 493 d0 /
c
c
c           list of major variables
c           -----------------------
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           absc   - abscissa
c           fval*  - function value
c           resg   - result of the 15-point gauss formula
c           resk   - result of the 31-point kronrod formula
c           reskh  - approximation to the mean value of f over (a,b),
c                    i.e. to i/(b-a)
c
c           machine dependent constants
c           ---------------------------
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c***first executable statement  dqk31
      epmach = d1mach(4)
      uflow = d1mach(1)
c
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
c
c           compute the 31-point kronrod approximation to
c           the integral, and estimate the absolute error.
c
      fc = dble(f(sngl(centr)))
      resg = wg(8)*fc
      resk = wgk(16)*fc
      resabs = dabs(resk)
      do 10 j=1,7
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = dble(f(sngl(centr-absc)))
        fval2 = dble(f(sngl(centr+absc)))
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,8
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = dble(f(sngl(centr-absc)))
        fval2 = dble(f(sngl(centr+absc)))
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(16)*dabs(fc-reskh)
      do 20 j=1,15
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if(resasc.ne.0.0d+00.and.abserr.ne.0.0d+00)
     *  abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if(resabs.gt.uflow/(0.5d+02*epmach)) abserr = dmax1
     *  ((epmach*0.5d+02)*resabs,abserr)
      return
      end
      subroutine dqk61TWO(f,a,b,result,abserr,resabs,resasc)
c***begin prologue  dqk61
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a2
c***keywords  61-point gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c***purpose  to compute i = integral of f over (a,b) with error
c                           estimate
c                       j = integral of dabs(f) over (a,b)
c***description
c
c        integration rule
c        standard fortran subroutine
c        double precision version
c
c
c        parameters
c         on entry
c           f      - double precision
c                    function subprogram defining the integrand
c                    function f(x). the actual name for f needs to be
c                    declared e x t e r n a l in the calling program.
c
c           a      - double precision
c                    lower limit of integration
c
c           b      - double precision
c                    upper limit of integration
c
c         on return
c           result - double precision
c                    approximation to the integral i
c                    result is computed by applying the 61-point
c                    kronrod rule (resk) obtained by optimal addition of
c                    abscissae to the 30-point gauss rule (resg).
c
c           abserr - double precision
c                    estimate of the modulus of the absolute error,
c                    which should equal or exceed dabs(i-result)
c
c           resabs - double precision
c                    approximation to the integral j
c
c           resasc - double precision
c                    approximation to the integral of dabs(f-i/(b-a))
c
c
c***references  (none)
c***routines called  d1mach
c***end prologue  dqk61
c
      double precision a,dabsc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,
     *  d1mach,epmach,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc,
     *  resg,resk,reskh,result,uflow,wg,wgk,xgk
      integer j,jtw,jtwm1
      real f
      external f
c
      dimension fv1(30),fv2(30),xgk(31),wgk(31),wg(15)
c
c           the abscissae and weights are given for the
c           interval (-1,1). because of symmetry only the positive
c           abscissae and their corresponding weights are given.
c
c           xgk   - abscissae of the 61-point kronrod rule
c                   xgk(2), xgk(4)  ... abscissae of the 30-point
c                   gauss rule
c                   xgk(1), xgk(3)  ... optimally added abscissae
c                   to the 30-point gauss rule
c
c           wgk   - weights of the 61-point kronrod rule
c
c           wg    - weigths of the 30-point gauss rule
c
c
c gauss quadrature weights and kronron quadrature abscissae and weights
c as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
c bell labs, nov. 1981.
c
      data wg  (  1) / 0.0079681924 9616660561 5465883474 674 d0 /
      data wg  (  2) / 0.0184664683 1109095914 2302131912 047 d0 /
      data wg  (  3) / 0.0287847078 8332336934 9719179611 292 d0 /
      data wg  (  4) / 0.0387991925 6962704959 6801936446 348 d0 /
      data wg  (  5) / 0.0484026728 3059405290 2938140422 808 d0 /
      data wg  (  6) / 0.0574931562 1761906648 1721689402 056 d0 /
      data wg  (  7) / 0.0659742298 8218049512 8128515115 962 d0 /
      data wg  (  8) / 0.0737559747 3770520626 8243850022 191 d0 /
      data wg  (  9) / 0.0807558952 2942021535 4694938460 530 d0 /
      data wg  ( 10) / 0.0868997872 0108297980 2387530715 126 d0 /
      data wg  ( 11) / 0.0921225222 3778612871 7632707087 619 d0 /
      data wg  ( 12) / 0.0963687371 7464425963 9468626351 810 d0 /
      data wg  ( 13) / 0.0995934205 8679526706 2780282103 569 d0 /
      data wg  ( 14) / 0.1017623897 4840550459 6428952168 554 d0 /
      data wg  ( 15) / 0.1028526528 9355884034 1285636705 415 d0 /
c
      data xgk (  1) / 0.9994844100 5049063757 1325895705 811 d0 /
      data xgk (  2) / 0.9968934840 7464954027 1630050918 695 d0 /
      data xgk (  3) / 0.9916309968 7040459485 8628366109 486 d0 /
      data xgk (  4) / 0.9836681232 7974720997 0032581605 663 d0 /
      data xgk (  5) / 0.9731163225 0112626837 4693868423 707 d0 /
      data xgk (  6) / 0.9600218649 6830751221 6871025581 798 d0 /
      data xgk (  7) / 0.9443744447 4855997941 5831324037 439 d0 /
      data xgk (  8) / 0.9262000474 2927432587 9324277080 474 d0 /
      data xgk (  9) / 0.9055733076 9990779854 6522558925 958 d0 /
      data xgk ( 10) / 0.8825605357 9205268154 3116462530 226 d0 /
      data xgk ( 11) / 0.8572052335 4606109895 8658510658 944 d0 /
      data xgk ( 12) / 0.8295657623 8276839744 2898119732 502 d0 /
      data xgk ( 13) / 0.7997278358 2183908301 3668942322 683 d0 /
      data xgk ( 14) / 0.7677774321 0482619491 7977340974 503 d0 /
      data xgk ( 15) / 0.7337900624 5322680472 6171131369 528 d0 /
      data xgk ( 16) / 0.6978504947 9331579693 2292388026 640 d0 /
      data xgk ( 17) / 0.6600610641 2662696137 0053668149 271 d0 /
      data xgk ( 18) / 0.6205261829 8924286114 0477556431 189 d0 /
      data xgk ( 19) / 0.5793452358 2636169175 6024932172 540 d0 /
      data xgk ( 20) / 0.5366241481 4201989926 4169793311 073 d0 /
      data xgk ( 21) / 0.4924804678 6177857499 3693061207 709 d0 /
      data xgk ( 22) / 0.4470337695 3808917678 0609900322 854 d0 /
      data xgk ( 23) / 0.4004012548 3039439253 5476211542 661 d0 /
      data xgk ( 24) / 0.3527047255 3087811347 1037207089 374 d0 /
      data xgk ( 25) / 0.3040732022 7362507737 2677107199 257 d0 /
      data xgk ( 26) / 0.2546369261 6788984643 9805129817 805 d0 /
      data xgk ( 27) / 0.2045251166 8230989143 8957671002 025 d0 /
      data xgk ( 28) / 0.1538699136 0858354696 3794672743 256 d0 /
      data xgk ( 29) / 0.1028069379 6673703014 7096751318 001 d0 /
      data xgk ( 30) / 0.0514718425 5531769583 3025213166 723 d0 /
      data xgk ( 31) / 0.0000000000 0000000000 0000000000 000 d0 /
c
      data wgk (  1) / 0.0013890136 9867700762 4551591226 760 d0 /
      data wgk (  2) / 0.0038904611 2709988405 1267201844 516 d0 /
      data wgk (  3) / 0.0066307039 1593129217 3319826369 750 d0 /
      data wgk (  4) / 0.0092732796 5951776342 8441146892 024 d0 /
      data wgk (  5) / 0.0118230152 5349634174 2232898853 251 d0 /
      data wgk (  6) / 0.0143697295 0704580481 2451432443 580 d0 /
      data wgk (  7) / 0.0169208891 8905327262 7572289420 322 d0 /
      data wgk (  8) / 0.0194141411 9394238117 3408951050 128 d0 /
      data wgk (  9) / 0.0218280358 2160919229 7167485738 339 d0 /
      data wgk ( 10) / 0.0241911620 7808060136 5686370725 232 d0 /
      data wgk ( 11) / 0.0265099548 8233310161 0601709335 075 d0 /
      data wgk ( 12) / 0.0287540487 6504129284 3978785354 334 d0 /
      data wgk ( 13) / 0.0309072575 6238776247 2884252943 092 d0 /
      data wgk ( 14) / 0.0329814470 5748372603 1814191016 854 d0 /
      data wgk ( 15) / 0.0349793380 2806002413 7499670731 468 d0 /
      data wgk ( 16) / 0.0368823646 5182122922 3911065617 136 d0 /
      data wgk ( 17) / 0.0386789456 2472759295 0348651532 281 d0 /
      data wgk ( 18) / 0.0403745389 5153595911 1995279752 468 d0 /
      data wgk ( 19) / 0.0419698102 1516424614 7147541285 970 d0 /
      data wgk ( 20) / 0.0434525397 0135606931 6831728117 073 d0 /
      data wgk ( 21) / 0.0448148001 3316266319 2355551616 723 d0 /
      data wgk ( 22) / 0.0460592382 7100698811 6271735559 374 d0 /
      data wgk ( 23) / 0.0471855465 6929915394 5261478181 099 d0 /
      data wgk ( 24) / 0.0481858617 5708712914 0779492298 305 d0 /
      data wgk ( 25) / 0.0490554345 5502977888 7528165367 238 d0 /
      data wgk ( 26) / 0.0497956834 2707420635 7811569379 942 d0 /
      data wgk ( 27) / 0.0504059214 0278234684 0893085653 585 d0 /
      data wgk ( 28) / 0.0508817958 9874960649 2297473049 805 d0 /
      data wgk ( 29) / 0.0512215478 4925877217 0656282604 944 d0 /
      data wgk ( 30) / 0.0514261285 3745902593 3862879215 781 d0 /
      data wgk ( 31) / 0.0514947294 2945156755 8340433647 099 d0 /
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           dabsc  - abscissa
c           fval*  - function value
c           resg   - result of the 30-point gauss rule
c           resk   - result of the 61-point kronrod rule
c           reskh  - approximation to the mean value of f
c                    over (a,b), i.e. to i/(b-a)
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
      epmach = d1mach(4)
      uflow = d1mach(1)
c
      centr = 0.5d+00*(b+a)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
c
c           compute the 61-point kronrod approximation to the
c           integral, and estimate the absolute error.
c
c***first executable statement  dqk61
      resg = 0.0d+00
      fc = dble(f(sngl(centr)))
      resk = wgk(31)*fc
      resabs = dabs(resk)
      do 10 j=1,15
        jtw = j*2
        dabsc = hlgth*xgk(jtw)
        fval1 = dble(f(sngl(centr-dabsc)))
        fval2 = dble(f(sngl(centr+dabsc)))
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j=1,15
        jtwm1 = j*2-1
        dabsc = hlgth*xgk(jtwm1)
        fval1 = dble(f(sngl(centr-dabsc)))
        fval2 = dble(f(sngl(centr+dabsc)))
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
  15    continue
      reskh = resk*0.5d+00
      resasc = wgk(31)*dabs(fc-reskh)
      do 20 j=1,30
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if(resasc.ne.0.0d+00.and.abserr.ne.0.0d+00)
     *  abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if(resabs.gt.uflow/(0.5d+02*epmach)) abserr = dmax1
     *  ((epmach*0.5d+02)*resabs,abserr)
      return
      end
c====
