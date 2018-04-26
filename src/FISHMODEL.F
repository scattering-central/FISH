c====  18/10/02 RKH, BIG changes to     COMMON/RODSETC/
c        03/09/03 small changes to JY.LE.0 routes
C2345 789012345678901234567890123456789012345678901234567890123456789012
      SUBROUTINE DERIV(QQIN,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,KS,J,
     *                     JB,JY,NPSMEAR,D)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C====
C==== revised Jan 89 so as to be more helpful to smearing routine.
C====
C==== CALC INTENSITY AND IT'S DERIVATIVES WITH RESPECT TO
C====  REFINING AND TIED PARAMS AT A SINGLE Q VALUE
C====  KS is the refining data set
C==== BEWARE OF DIVIDE BY ZERO ACCIDENTS !!!
C==== ANY CHANGES MADE HERE TO I(Q) CALC MUST HAVE MATCHING CHANGES IN
C==== SUBROUTINE CALCQ
C====  NOTE additive terms of model 3 and 4 "backgrounds" are stored
c====  separately in SUMW so are insensitive to multiplicative structure factor
C====  or square type operations. Thus they may appear anywhere in the 
C====  card deck.   If in doubt, examine the code closely !
C====  NOTE tests for models 3&4 when derivatives are adjusted.
C====
      INCLUDE 'FISHDIM.PAR'
      DIMENSION D(MV),DP(MV),DX(MV),DF(MV)
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
c==== THIS COMMON ADDED BY ASHLEY FOR MODEL 13 TO KEEP VARIABLES BETWEEN CALLS
      COMMON/GAMFUN/GDF(4),IGAMMA
      DATA IGAMMA/0/,GDF/4*0/
C2345 789012345678901234567890123456789012345678901234567890123456789012
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/MODEL24/JJJ
C==== this for model 23 charged particles, intermediate parameters are calc
C==== ahead of time by SSQFNSET()
      REAL*8 dcoeff
      REAL coeff
      common /sqhpc/ dcoeff(4,5),coeff(6,5)
C==== FIRST CALC INTENSITY (MUST WORK REGARDLESS OF WHICH DERIVS ARE NEEDED
      QQ=QQIN
      CALC1=0.0
      SUM=0.0
      SUMW=0.0
C====        NOTE SUMPQ IS ALSO INITIALISED BY MODEL 9
      SUMPQ=0.0
      SUMF=0.0
      DSUMF=0.0
      SUMX=0.0
      PRODSQ=1.0
      BETARATIO=1.0
      SUMBUG=0.0
      DO 10 I=1,NP
      DP(I)=0.0
      DX(I)=0.0
      DF(I)=0.0
   10 D(I)=0.0
C==== MUST ZERO D() FIRST ELSE CONDER CUMULATIVELY MULTIPLIES THEM !!!
      IM1=KS
      QQ2=QQ*QQ
      DO 20000 I=1,NP
      I1=I-1
      LT=MOD(LTYP(I),10)
C==== MODEL 88 SWITCH DATA SETS, PARAMS UNTIL NEXT 88 CARD ARE FOR SET KS=LT 
C==== ONLY, USE 88 0 TO RETURN TO GENERAL USAGE, USE CONSTRAINTS TO TIE COMMON
C==== PARAMETERS ACROSS SECTIONS
      IF(LM(I).NE.88)GOTO 40
      IM1=LT
      IF(IM1.LE.0)IM1=KS
C==== the MODEL 88 card itself does nothing else, skip out here
c==== else it follows the 99 route !    RKH 7/11/94
      GOTO 20000
   40 IF(LT.NE.1)GOTO 10000
C==== CALC INT ONCE FOR EACH MODEL, AT PARAM TYPE=1
      IM=LM(I)
      IF(IM1.NE.KS)IM=2
C23456789012345678901234567890123456789012345678901234567890123456789012
      GOTO (100,200,300,400,500,600,700,800,900,20000,1100,1200,
     *             1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,
     *             2300,2400,2500,2600,2700,2800,2900),IM
C==== (IM OUT OF RANGE OF ASSIGNED GOTO IS NON-RELIABLE FORTRAN )
C==== SKIP OUT IF ANY OTHER MODEL NUMBERS,   RKH 7/11/94
      IF(IM.NE.99)GOTO 20000
C==== MODEL 99 IS RESERVED FOR OVERALL SCALE FACTOR AND TO SET RESULT OF CALC
      DSUM=SUM+SUMW
      SUM=SUM*V(I)
      SUMW=SUMW*V(I)
      SUMPQ=SUMPQ*V(I)
      CALC1=SUM+SUMW
      IF(I1.GE.1)THEN
      DO 50 JJ=1,I1
   50 D(JJ)=D(JJ)*V(I)
      END IF
      GOTO 10000
C==== MODEL 1 LTYP=1 uniform sphere
c====         LTYP=11 cylinder - end on view, added 3/2/97 RKH
c====         LTYP=21 Schultz spheres added 19/04/2002 RKH
  100 QR=QQ*V(I+1)
      IF(LTYP(I).EQ.1)THEN
      SQR=SIN(QR)
      CQR=COS(QR)
      F=3.*(SQR-QR*CQR)/QR**3
      DSUM=F**2
c
      ELSE IF(LTYP(I).EQ.11)THEN
      F=TRJ1XBYX(QR)
      DSUM=F**2
c
      ELSE IF(LTYP(I).EQ.21)THEN
      DSUM=RKCHEN(QQ,V(I+1),V(I+2),F)
      END IF
      SUM=SUM+DSUM*V(I)
      SUMPQ=SUMPQ+DSUM*V(I)
      SUMF=SUMF+ F*V(I)
      SUMX=SUMX+ V(I)
C==== V(I) IS SCALE I(Q=0,R) , V(I+1) IS RADIUS FOR SPHERICAL PARTICLE
      GOTO 10000
C==== MODEL 2 IS A DUMMY NEEDED FOR PARAMETERS OF A CONSTRAINT , SO DOES NOTHING
C==== HERE.  
  200 GOTO 10000
C==== MODEL 3 QUADRATIC BACKGROUND, A +B*Q +C*Q**2, add to calc but save
C====               in workspace JW
  300 DSUM=V(I)
      IF(LTYP(I).EQ.1)DSUM=DSUM+V(I+1)*QQ+V(I+2)*QQ2
      SUMW=SUMW+DSUM
      GOTO 10000
C==== MODEL 4 ADD A SCALED "EXPERIMENTAL" OR OTHER BACKGROUND FROM
C==== WORKSPACE JB  IF EXTRAPOLATING ETC. USE J=0
  400 IF(LTYP(I).NE.1)GOTO 410
      IF(J.EQ.0)GOTO 10000
      DSUM=C(J,JB)*V(I)
      SUMW=SUMW+DSUM
      GOTO 10000
C==== add scaled and shifted resolution function from set IV = 9 ( for now) RKH 11/10/94
c==== this version of cubic interp, returns ZERO if QQ is out of range
  410 IV=9
      DSUM=CUBIC2(QQ+V(I+1),Q(NC3(IV),IV),
     >                  C(NC3(IV),IV),NC4(IV)-NC3(IV)+1,IIV)
      SUMW=SUMW+V(I)*DSUM
      GOTO 10000
C==== MODEL 5 FOR PREDICATE DATA,NOT USED HERE
  500 GOTO 10000
C==== MODEL 6 FOR POLYDISPERSE SPHERES
  600 IF(JY.LE.0.0)GOTO 10000
      CALL POLCAL(QQ,C(1,JY),POLA,DSUMF,DSUMX)
      SUM=SUM+POLA
      SUMPQ=SUMPQ+POLA
      SUMF=SUMF+DSUMF
      SUMX=SUMX+DSUMX
      IF(IY.EQ.1)GOTO 10600
      GOTO 20000
C==== MODEL 7  CRITICAL SCATTERING KAPPA, ZETA,  MUST COME AFTER REST OF
C==== INTENSITY CALC AS IS A MULTIPLIER, CHANGES ALL PRECEEDING DERIVATIVES
  700 QQZ=QQ2*V(I+1)
      DENOM=1.0/(1.0+V(I+1)*QQZ)
      SSQ=1.0+V(I)*DENOM
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
C==== MODEL 8 AND 9 TOGETHER DO INTERACTING dilute SPHERICAL PARTICLES
C==== OR RATHER COMPLICATED CONTRAST SHAPES
C==== SCALE IS PROPORTIONAL TO DELTA RHO, SECOND PARAM IS RADIUS
C==== THE POLYDISPERSE EQUIVALENT NEEDS MODELS 6 AND 10 IN CONJUNCTION.
C==== spherical shell
  800 IF(LTYP(I).EQ.1)THEN
      QR=QQ*V(I+1)
      SQR=SIN(QR)
      DSUM=12.56637062*(SQR-QR*COS(QR))/QQ**3
c==== end-on cylindrical shell
      ELSE IF(LTYP(I).EQ.61)THEN
      QR=QQ*V(I+1)
      SS1=3.141592654*V(I+1)**2
      DSUM=SS1*TRJ1XBYX(QR)
      ELSE 
C==== find previous radius or set it zero
      RRA=0.0
      IF(I-2.GT.0)THEN
      IF(LM(I-2).EQ.8)RRA=V(I-1)
      END IF
      IF(LTYP(I).EQ.11)THEN
C==== linear shell
      QR=QQ*V(I+1)
      SQR=SIN(QR)
      DSUM=12.56637062*( 2.*( COS(QQ*RRA)-COS(QR) )/QQ 
     >  + RRA*SIN(QQ*RRA) -V(I+1)*SQR )
     >      /( (QQ**3)*( V(I+1) -RRA ) )
C==== now look for a previous REFINING, model 8, radius and modify its deriv.
      IF(I-2.GT.0)THEN
      IF(LM(I-2).EQ.8.AND.ABS(PS(I-1)).GT.0.01)D(I-1) = D(I-1)+
     > 12.56637062*V(I)*(-SIN(QQ*RRA) +QQ*RRA*COS(QR*RRA))/
     >  ( (QQ**3)*(V(I+1)-RRA) ) + DSUM*V(I)/(V(I+1)-RRA)
      END IF
c==== exponential shells
      ELSE IF(LTYP(I).LE.51)THEN
      DSUM=SHELLEXP(LTYP(I),QQ,RRA,V(I+1))
C==== now look for a previous REFINING, model 8, radius and modify its deriv.
c==== must do here and not with rest of derivs below in case this radius is OFF
      IF(I-2.GT.0)THEN
      IF(LM(I-2).EQ.8.AND.ABS(PS(I-1)).GT.0.01)D(I-1) = D(I-1)+
     >   V(I)*(SHELLEXP(LTYP(I),QQ,RRA+DEL(I-1),V(I+1))-DSUM)/DEL(I-1)
      END IF
c==== 17/4/00 Gaussian shell
      ELSE IF(LTYP(I).EQ.71)THEN
      DSUM=SHELLGAUSSD(QQ,RRA,V(I+1),DA,DB)
C==== now look for a previous REFINING, model 8, radius and modify its deriv.
      IF(I-2.GT.0)THEN
      IF(LM(I-2).EQ.8.AND.ABS(PS(I-1)).GT.0.01)D(I-1) = D(I-1) + V(I)*DA
      END IF
      END IF
C====  end of 3 param shells
      END IF
c==== finish all model 8 shells here
      SUM=SUM+DSUM*V(I)
      GOTO 10000
C==== MODEL 9  SQUARE OPERATION IS NOT A TERMINATOR, STILL NEED A MODEL 99 CARD
c====  12/08/03 add beta correction stuff
  900 F=2.*SUM*V(I)
      IF(I1.GE.1)THEN
      DO 901 JJ=1,I1
  901 IF(LM(JJ).NE.3.AND.LM(JJ).NE.4)D(JJ)=D(JJ)*F
      END IF
      DSUMF=SUM
	DSUM=SUM*SUM
      SUM=DSUM*V(I)
	SUMF=DSUMF*V(I)
	SUMX=V(I)
      SUMPQ=SUM
      GOTO 10000
C==== MODEL 10 , COMPLEX CONTRAST for model 6 polydispersity is used in POLCAL
C====           
C==== MODEL 11 GENERAL POLYNOMIAL A +B*Q +C*Q**2 +D*Q**3 +E*Q**4 + .... Q**7
 1100 QQ5=QQ2*QQ2*QQ
      IF(LTYP(I).NE.1)GOTO 1105
      DSUM=V(I)+V(I+1)*QQ+V(I+2)*QQ2+V(I+3)*QQ*QQ2+
     * V(I+4)*QQ2*QQ2+V(I+5)*QQ5+V(I+6)*QQ5*QQ+V(I+7)*QQ5*QQ2
      SUM=SUM+DSUM
      SUMPQ=SUMPQ+DSUM
      GOTO 10000
 1105 IF(LTYP(I).NE.21)GOTO 1110
      SUM=SUM+V(I)
      SUMPQ=SUMPQ+V(I)
      GOTO 10000
C==== delta function at Q=0, V(I+1) is a non-refining tolerance
 1110 IF(ABS(QQ).LE.ABS(V(I+1)))THEN
      DSUM=V(I)/(2.0*ABS(V(I+1) ) )
      SUM=SUM+DSUM
      SUMPQ=SUMPQ+DSUM
      END IF
      GOTO 10000
C==== MODEL 12  GUINIER RADIUS FIT or  MODIFIED POROD  K.Q**-4exp(-sig**2.Q**2)
 1200 IF(LTYP(I).GE.11)GOTO 1211
      DSUM=-V(I+1)*QQ2*0.333333333333
C==== expspec avoids underflows
      SUM1=EXPSPEC(V(I+1)*DSUM)
      SUM=SUM+SUM1*V(I)
      SUMPQ=SUMPQ+SUM1*V(I)
      GOTO 10000
C====  if sigma is larger than ca. 10.0 then the exp damping rapidly reaches
C====  underflow, moving the Q**-4 inside helps a little
 1211 IF(LTYP(I).GE.21)GOTO 1221
      DSUM=EXPSPEC(-(V(I+1)**2)*QQ2 - 4.*ALOG(QQ) )
      SUM1=V(I)*DSUM
      SUM=SUM+SUM1
      SUMPQ=SUMPQ+SUM1
      GOTO 10000
C==== 20/3/98 4th version of off match diffuse Porod, perhaps its OK this time !
 1221 DA=EXPSPEC(-(V(I+2)**2)*QQ2 - ALOG(QQ2) )
      DB=EXPSPEC(-(V(I+2)**2+V(I+5)**2)*QQ2 - ALOG(QQ2) )
      DC=EXPSPEC(-0.5*(2.*(V(I+2)**2)+V(I+5)**2)*QQ2 - ALOG(QQ2) )
      CQR=COS(QQ*V(I+4))
      DSUM= ( DA*(V(I+1)-V(I+3))**2 + DB*V(I+3)**2 +
     >      2.0*(V(I+1)-V(I+3))*V(I+3)*CQR*DC)/QQ2
      SUM1=V(I)*DSUM
      SUM=SUM+SUM1
      SUMPQ=SUMPQ+SUM1
      GOTO 10000
C==== MODEL 13 FRACTAL DIMENSIONALITY      CORRECTED BY RKH 28/7/88
C        MUST COME AFTER REST SINCE IT IS A MULTIPLIER WORKS ON
C        A SIMILAR BASIS AS O-Z SCATTERING
C           THREE CARDS   1)DIMENSIONALITY  D
C                         2)AGGREGATE SIZE  ZETA
C                         3)SINGLE RADIUS ie  RADIUS OF SINGLE DROPLET
C                                  TIED TO PARAMETER IN SINGLE FIT
C==== USES TEIXEIRA FORMULA WITH R**-D, THIS IS SINHA'S C/D
C====   16/3/88 CHANGED LEN, LG AND LL TO RLEN,RLG, AND RLL AS THEY ARE NOT
C==== DECLARED REAL     TRAP D.LT.1 AS GAMMA FN RETURNS ERROR
C==== LTYP(I)=11 is for Andrew Allen model, for spherical blobs,
C==== see J.Appl.Cryst. 24(91)624-634 and Harwell Report MPD/NBS/361 
c==== (but beware typos. in the equations). Three extra cards:
C====                    4) delta rho (in 1.0e10cm**-2 )
c====                    5) f overall vol fraction of fractal phase
c====                    6) fL local volume fraction
c====                    7) S rough (max) surface area ( cm*-1)
c==== NOTE this variant is a structure factor * form factor !
c
1300  IF(V(I).LT.1.0)V(I)=1.0
      DA=V(I)
      G=DA-1
C==== ASHLEY'S GAMMA FUNCTION CALC GAFU WAS SLOW, ONLY GOOD TO 3 OR 4 SIG FIGS
C==== AND FOR MODEST VALUES OF G, REPLACED BY NAG ROUTINE S14AAF WHICH DOES A
C==== TSCHEBYCHEV EXPANSION 10/8/88, ifail=1 G too large, =2 G too large & -ve
C==== =3 G too small, =4 G a -ve integer for which gamma is infinite
C==== following more tests, local routine GAMMA used, results similar to S14AAE
C==== - accurate to 5-6-7 sig figs, S14AAF double prec version is not noticeably
C====  any better !  G MUST BE LESS THAN 34 ON VAX
      IF(IGAMMA.NE.1.OR.GDF(2).NE.G)THEN
C====        GA=GAFU(G)
c====        IFAIL=0
C====        GA= S14AAE(G,IFAIL)
        GA=GAMMA(G)
        IGAMMA=1
        GDF(2)=G
        GDF(3)=GA
C==== THIS IS DERIV OF GAMMA(D-1) DIVIDED BY GAMMA(D-1)
        GDF(4)=10000.*( GAMMA(G+.0001)/GA - 1.0)
      ELSE
        GA=GDF(3)
      ENDIF
      QL=QQ*V(I+1)
C==== USE ABS(Ro) TO AVOID UNDEFINED EXPONENTIATION
      QR=QQ*ABS(V(I+2))
      QL2=QL*QL
      GATANQL= G*ATAN(QL)
      SS1=SIN(GATANQL)
      SS3=(1.+(1./QL2))**(G/2.0)
C
      IF(LTYP(I).NE.1)GOTO 1350 
      SS2=DA*GA
      SS4=QR**DA
      SSQ=(1.+ SS1*SS2/(SS3*SS4) )
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
1350  VS=4.*3.141592654*(V(I+2)**3)/3.0
      SS4=(2.*QR)**DA
      SS5=1.E-04*V(I+4)*(1.-V(I+4))*(V(I+3)**2)*VS
      F=1./( 1.+QR*QR/5. + V(I+4)*(1.-V(I+4))*VS* 
     >      ((1.-V(I+5))*QQ2)**2/ (2.*3.141592654E-08*V(I+6)) )
      DSUM=F*SS5*( (1.-V(I+5))**2 + 8.*V(I+5)*SS1/(SS4*SS3) )
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      SUMF=SUMF+SQRT(ABS(DSUM))
      SUMX=SUMX+1.0
      GOTO 10000
C==== MODEL 14 
c====   LTYP= 1 GAUSSIAN COIL    ( 2 params: I(0),Rg )
C====       =11 schultz polydisp version added 20/2/92 RKH
c====            ( 3 params: I(0),Rg,U )
C====       =21 wormlike chain ( 5 params: I(0),N,L,Rax,U ) RKH 29/7/92
c====           I'm not sure that SUMF will properly allow for 
c====           polydispersity ???
c====       =31 STAR POLYMER Benoit equation ( 3 params: I(0),Rg,F) RKH 12/5/94
c====           when no. of branches F=1 this is same as LTYP=1
c====       =41 STAR POLYMER Dozier et.al. ( 5 params: i(0),Rg,
C====           (alpha/Nf),zeta, Flory nu (0.5 theta solv to .6 good) )
c====       =51 Liebler diblock copolymer ( 5 params, scale, N, a, f, chi)
c====       =61 HSHAPE block copolymer ( 5 params, scale, Rg1, Fa, Fb, chi)
C====       =71 Kholodenko wormlike chain
 1400 IF(LTYP(I).EQ.21)GOTO 1420
      IF(LTYP(I).EQ.41)GOTO 1440
      IF(LTYP(I).EQ.51)GOTO 1450
      IF(LTYP(I).EQ.61)GOTO 1460
      IF(LTYP(I).EQ.71.OR.LTYP(I).EQ.81)GOTO 1470
      U=0.0
      FSTAR=1.0
C==== Bug of 7/92 here fixed 29/3/93 RKH, used to say UU not V(I+2)
      IF(LTYP(I).EQ.11)U=ABS(V(I+2))
      IF(LTYP(I).EQ.31)FSTAR=AMAX1(1.0 , ABS(V(I+2)) )
C==== need U,Y & QR for derivatives section below
      RR=V(I+1)**2
      QR=RR*QQ2
      Y=QR/( (1.0+2.0*U)*(3.0-2.0/FSTAR) )
      SUM1=DEBGAUSS(LTYP(I),QQ,RR,V(I+2))
c==== 12/08/03 save DSUMF to correct deriv of scale beta stuff
 1418 SUMPQ=SUMPQ+SUM1*V(I)
      SUM=SUM+SUM1*V(I)
	DSUMF=SQRT(ABS(SUM1))
      SUMF=SUMF+DSUMF*V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
 1420 SUM1=WORM(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1440 SUM1=STARD(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1450 SUM1=DIBLK(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1460 SUM1=HSHAPE(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1470 IF(NMETH(I).LE.0)CALL INTPIC2(I) 
      SUM1=r_KHOL(QQ,V(I+1),V(I+2),V(I+3),V(I+4),V(I+5),V(I+6),
     >                     DSUMF,I,LTYP(I))
      SUMPQ=SUMPQ+SUM1*V(I)
      SUM=SUM+SUM1*V(I)
      SUMF=SUMF+DSUMF*V(I)
      DSUMX=V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 15 RESOLUTION SMEARING - SET FLAGS HERE TO CALL LATER - 3 CARDS
 1500 NPSMEAR=I
      GOTO 10000
C==== MODEL 16, TEUBNER & STREY, J.CHEM.PHYS. 87(88)3195, I(0),I(MAX), Q(MAX)
 1600 IF(LTYP(I).EQ.11)GOTO 1610
      QQ5 = (QQ/V(I+2))**2 -1.0
      DSUM = V(I)*V(I+1)/ ( (V(I+1)-V(I))*(QQ5**2) + V(I) )
      SUM=SUM+DSUM
      SUMPQ=SUMPQ+DSUM
      GOTO 10000
C====  TEUBNER&STREY,GELS ETC. 1/(P0 + P1*Q**2 + P2*Q**4)
 1610 DSUM= 1.0 / ( V(I) + V(I+1)*QQ2 + V(I+2)*QQ2*QQ2 )
      SUM=SUM+DSUM
      SUMPQ=SUMPQ+DSUM
      GOTO 10000
C==== MODEL 17 Debye/Wang  see Z.-Y.Wang, M.Konno & S.Saito,
C====    J.Chem.Phys.90(1989)1281  I(Q=0), Q0, A0,  PUT Q0=0 FOR DEBYE RANDOM
C==== TWO PHASE SYSTEM, care to avoid div by zero at Q=0 or Q0=0
 1700 QQ7=0.0
      IF(ABS(QQ).GT.1.E-30)QQ7=1.0/QQ
C==== oops QQ2 is reserved ALWAYS for Q**2
      QQ6=QQ7*V(I+1)
      SS1= 1+ (V(I+2)*(QQ + V(I+1)))**2
      SS3= 1+ (V(I+2)*(QQ - V(I+1)))**2
      SS2=SS1*SS1
      SS4=SS3*SS3
      QQ3=0.5*(1 + (V(I+2)*V(I+1))**2)**2
      DSUM=QQ3*( (1.0+QQ6)/SS2 + (1.0-QQ6)/SS4 )
      SUM=SUM + DSUM*V(I)
      SUMPQ=SUMPQ + DSUM*V(I)
      GOTO 10000
C==== MODEL 18, TWO SHELL ROD ( AND OTHER ELLIPSOIDS ETC., ONE DAY ?)
C==== PARAMETER ORDER IS UNUSUAL FOR CONVENIENCE OF NUMERICAL INTEGRATION
C==== SEE J.CHEM.SOC.FARADAY TRANS 83(1987)1445-1452
C====     SCALE V(I) IS A*NP*RHO2**2,  (VOLUMES PI*R**2*L INCLUDED IN CALC)
C====     OUTER RADIUS IS R, SCATTERING DENSITY RHO2
C====     INNER RADIUS IS R-DELR, "       "     RHO1
C====     CONTRAST IS (RHO1-RHO2)/RHO2    SET THIS ZERO FOR UNIFORM CYLINDER
C====    THIS REQUIRES YET ANOTHER COMMON BLOCK ! AND FUNCTION RODFF
C====               RADIUS  LENGTH,DELTA-R, CONTRAST
 1800 IF(LTYP(I).LE.11)THEN
      IF(NMETH(I).LE.0)CALL INTPIC(I)
      DSUM= RODFF(QQ,V(I+1),V(I+2),V(I+3),V(I+4),DSUMF,I)
      ELSE
      IF(NMETH(I).LE.0)CALL INTPIC3(I)
      DSUM= RODSHEAR(QQ,V(I+1),DSUMF,I,LTYP(I))
      END IF
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      DSUMX=V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 19, CORRELATION HOLE S(Q),   MULTIPLIER
C====                PARAMS ARE "HOLE" VOL FRACT (ETA), AND HOLE RADIUS
 1900 QR=QQ*V(I+1)
      SQR= SIN(QR)
      SS1= 3.0*(QR*COS(QR)-SQR)/(QR**3)
      SSQ=1.0+V(I)*SS1
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
C==== MODEL 20, +CONST*Q**N  PARAMS ARE CONST AND N
 2000 SUM1=QQ**V(I+1)
      SUM=SUM+ V(I)*SUM1
      SUMPQ=SUMPQ+V(I)*SUM1
      GOTO 10000
C==== MODEL 21 ellipsoid, radii R1:R1:R2, params scale, R1, and X where R2=a*R1
 2100 IF( NMETH(I).LE.0)CALL INTPIC(I)
      DSUM=ELLIPSOID(QQ,V(I+1),V(I+2),DSUMF,I)
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C====  HARD SPHERES STRUCTURE FACTOR,  ETA=VOL FRACTION, SIGMA/2= SPHERE RADIUS
C====   - see Ashcroft & Lekner Phys.Rev. 145(1966)83
C==== 8/2/2 add Sharma & Sharma square well version as LTYP=11
 2200 IF(LTYP(I).NE.11)THEN
      SSQ= HARDSPH(QQ,V(I),V(I+1))
      ELSE
      SSQ= HARD_SHARMA(QQ,V(I),V(I+1),V(I+2),V(I+3))
      END IF
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
C==== INTERACTING CHARGED PARTICLES - HAYTER-PENFOLD ETC. ( FAGOTTI CODE)
 2300 SSQ=SOFQ(QQ,DCOEFF(1,1),COEFF(1,1))
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
C==== MODEL 24, TWO SHELL ELLIPSOID
 2400 JJJ=LTYP(I)
      IF(NMETH(I).LE.0)CALL INTPIC(I)
      DSUM=ELLIPSHELL(QQ,V(I+1),V(I+2),V(I+3),V(I+4),DSUMF,I)
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 25, SOFQ HAYTER-PENFOLD (MODEL 23) + CRITICAL SCTTERING (MODEL 7)
 2500 SSQ1=SOFQ(QQ,DCOEFF(1,1),COEFF(1,1))
      SSQ=SSQ1 + V(I+4)/(1.0+(V(I+5)*QQ)**2.0)
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
c==== model 26, fractal surfaces.  (q*zeta >> 1 )  added  by HS, checked by RKH
c====           Schmidt et.al.  J.Chem.Phys. 90 5016 1989
c====		V(1) = area*consts      V(2) = fractal dimension 
c==== 26 11 is A.Allen version (see model 13 comments)
c==== which has  V(1)= S/V (in cm-1), V(2)=D,
c====     V(3) = delta rho (in 1.e10 cm**-2)   V(4) = zeta
C==== note V(1) and V(3) will not adjust together !, V(3) is intended to
C==== be tied to model 13.
 2600 DA=V(I+1)
C==== calculate Gamma function GA
      G=5.-DA    !argument of Gamma function
      IF(IGAMMA.NE.1.OR.GDF(2).NE.G)THEN
        GA=GAMMA(G)
        IGAMMA=1
        GDF(2)=G
        GDF(3)=GA
C==== THIS IS DERIV OF GAMMA(5-D) DIVIDED BY GAMMA(5-D)
        GDF(4)= 10000.*( GAMMA(G+.0001) -GA )/GA
      ELSE
        GA=GDF(3)
      ENDIF
      SS3 = QQ**(DA -6.)
      IF(LTYP(I).NE.1)GOTO 2650
c==== calculate sine(A-B)--> cos
      ARG = DA * 1.570796327
      SS2 = -COS (ARG)
c====
      DSUM = GA*SS2*SS3
      SUM = SUM + DSUM*V(I)
      SUMPQ = SUMPQ + DSUM*V(I)
      GOTO 10000
2650  QL=QQ*V(I+3)
      ARG=(DA-1.)*ATAN(QL)
      SS4=1.0E-12*3.141592654*GA*(V(I+2)**2)*(V(I+3)**4)
C==== take abs in case fit trys -ve zeta, avoids undefined exponentiation
      SS5=ABS(QL)**(DA-6.0)
      DSUM = SS4*SS5*((1. +1./(QL*QL) )**( (DA-5.)*0.5 ) )*SIN(ARG)
      SUM = SUM + DSUM*V(I)
      SUMPQ = SUMPQ + DSUM*V(I)
      GOTO 10000
C==== model 27  1  Lorentz  +  Debye-Bueche (for gelatin etc.)
c====          11  Lorentz peak for inelastic neutrons
c====          21  Gaussian peak
c====          31  Voigt for broadened Gaussians ( from WIFD)
c====          41  Asymmetric peak expt.
c====          51  Asymmetric peak expt. where deriv matched
c====          61  Ikeda/Carpenter function (NB has 5 params not 4 )
c====          71  as 21 Gaussian, extra 2 params sigma2 & sigma3
c====          81  Gaussian convoluted with exponential
2700  IF(LTYP(I).NE.1)GOTO 2710
      QQZ = QQ2*V(I+1)
      DENOM = 1.0/( 1.0 + V(I+1)*QQZ )
      SUM1 = V(I)*DENOM
      QQA = QQ2*V(I+3)
      DENOM2 = 1./(1.+ V(I+3)*QQA)
      DSUM = V(I+2)*DENOM2**2
      SUM = SUM + SUM1 + DSUM
      SUMPQ=SUMPQ + SUM1 + DSUM
      GOTO 10000
C==== model 27 type 11, Lorentzian for quasi-elastic scattering
2710  IF(LTYP(I).NE.11)GOTO 2720
      QQA=QQ-V(I+2)
      DENOM=1.0/(QQA**2 + 0.25*V(I+1)**2)
      SUM1=V(I)*V(I+1)*DENOM/6.283185307
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
c==== Gaussian peak, params are height, FWHM=sqrt(2.*loge(2))sig=2.354820045sig
c==== and position  CHECK EXP(ZERO) ???
2720  IF(LTYP(I).NE.21.AND.LTYP(I).NE.71)GOTO 2730
      SS1=AMAX1(1.E-12,ABS(V(I+1)/2.35482004))
      SS5=QQ-V(I+2)
      SS2=SS5/SS1
C==== extra terms RKH 3/4/96
      IF(LTYP(I).EQ.71)THEN
      SS4=V(I+3)*ABS(SS5)+V(I+4)*SS5
      SS2=SS2/(1.+SS4/SS1)      
      END IF
      DSUM=EXPSPEC(-0.5*SS2**2)
      SUM1=V(I)*DSUM
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
2730  IF(LTYP(I).NE.31)GOTO 2740
      SS1= QQ-V(I+3)
C====             X ,SIGMA, GAMMA  YVAL DERX,DERS,DERG
      CALL VOIGT(SS1,V(I+1),V(I+2), DSUM, SS3, SS4, SS5)
      SUM1=V(I)*DSUM
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
c==== Asym peak, params are height, FWHM=sqrt(2.*loge(2))sig=2.354820045sig
c==== position, and tail W 
2740  IF(LTYP(I).NE.41)GOTO 2750
      SS2=(QQ-V(I+2))
      SS5=SS2
      IF(SS5.LE.0.0)THEN
      SS1=AMAX1(1.E-12,ABS(V(I+1)/2.35482004))
      SS2=SS2/SS1
      DSUM=EXPSPEC(-0.5*SS2**2)
      ELSE
C==== try exponential tail end:
      SS1=AMAX1(1.E-12,ABS(V(I+3)))
      SS2=SS2/SS1
      DSUM=EXPSPEC(-SS2)
      END IF
C
      SUM1=V(I)*DSUM
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
c==== Asym peak, params are height, FWHM=sqrt(2.*loge(2))sig=2.354820045sig
c==== position, and tail W, joined to match 1st derivative
2750  IF(LTYP(I).NE.51)GOTO 2760
      SS2=(QQ-V(I+2))
      SS5=SS2
      SS1=AMAX1(1.E-12,ABS(V(I+1)/2.35482004))
      SS3=SS1**2/AMAX1(1.E-12,ABS(V(I+3)))
      IF(SS5.LE.SS3)THEN
      SS2=SS2/SS1
      DSUM=EXPSPEC(-0.5*SS2**2)
      ELSE
C==== try exponential tail end, matched derivative
      SS1=AMAX1(1.E-12,ABS(V(I+3)))
      SS2=SS2/SS1
      DSUM=EXPSPEC(SS3/(2.*SS1) -SS2)
      END IF
C
      SUM1=V(I)*DSUM
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
2760  IF(LTYP(I).NE.61)GOTO 2780
      SS2=(QQ-V(I+2))
      DSUM=RIKCAR(SS2,V(I+1),V(I+3),V(I+4))
      SUM1=V(I)*DSUM
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
2780  IF(LTYP(I).NE.81)GOTO 10000
C==== Guassian convoluted with an exponential:
      DSUM=CRTPFN(QQ,V(I+1),V(I+2),V(I+3))
      SUM1=V(I)*DSUM
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
C==== model 28 polydisp (Scultz) infinite layer - Kotlarchyk & Ritzau
c====  LTYP=41 & 51 have extra parameters via RLAYFF2
c====  ltyp=31 was scuppered 4/98 to 1/99
c
2800  IF(LTYP(I).LE.31)THEN
      DSUM= RLAYFF(LTYP(I),QQ,V(I+1),V(I+2),V(I+3),V(I+4),DSUMF)
      ELSE
      DSUM= RLAYFF2(LTYP(I),QQ,V(I+1),DSUMF)
      END IF      
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      DSUMX=V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C==== model 29 structure factor for 1d paracrystal - Kotlarchyk & Ritzau
C==== LYTP=11 is for 3 phase paracrystal ( 9 parameters ! )
2900  IF(LTYP(I).NE.1)GOTO 2910
      SSQ= PARCRYS(QQ,V(I),V(I+1),V(I+2),V(I+3))
      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
     *            LM,D,DP,DF,DX)
      GOTO 10000
c====   29 11 or 21 scale
c====   29  2 N is converted to nearest integer (may need manual increments)
c====   29  3 sld2 peak
c====   29  4 Ybar peak position
c====   29  5 or 15 Gaussian sigma y or top hat width y
c====   29  6 sld1 dip
c====   29  7 D=Xbar+Ybar
c====   29  8 or 18 Gaussian sigma x or top hat width x
c====   29  9 sld3 bkg
c====   29 10 Rsigma for Lorentz (IF first LTYP is 21)
2910  DSUM=PARA3(QQ,LTYP(I),LTYP(I+4),LTYP(I+7),V(I+1))
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      DSUMX=V(I)
      SUMX=SUMX+V(I)
C
C==== CHECK ALL PARAMETERS TO SEE IF THEY ARE REFINING
10000 IF(ABS(PS(I)).LT.0.01)GOTO 20000
      GOTO(10100,20000,10300,10400,20000,20000,10700,10800,10900,
     *     20000,11100,11200,11300,11400,20000,11600,11700,11800,
     *     11900,12000,12100,12200,12300,12400,12500,12600,12700,
     *     12800,12900),IM
C==== IM=99
      D(I)=DSUM
      GOTO 20000
10100 GOTO(10101,10102,10103),LT
C==== SPHERE - DERIVATIVES
10101 D(I)=DSUM
      DP(I)=DSUM
      DF(I)=F
      DX(I)=1.0
      GOTO 20000
C=== corrected 28/7/92 from 10102 DDF=3.0*(QR*SQR -F)/V(I)
10102 IF(LTYP(I-1).EQ.1)THEN
      DDF=3.0*(SQR/QR -F)/V(I)
      ELSE IF(LTYP(I-1).EQ.11)THEN
      DDF=(TRJ1XBYX(QQ*(V(I)+DEL(I)))-F)/DEL(I)
      ELSE IF(LTYP(I-1).EQ.21)THEN
      D(I)=V(I-1)*( RKCHEN(QQ,V(I)+DEL(I),V(I+1),DSUMF)-DSUM)/DEL(I) 
      DP(I)=D(I)
      DF(I)=V(I-1)*(DSUMF-F)/DEL(I)
      GOTO 20000
      END IF      
C      
      DF(I)=V(I-1)*DDF
      D(I)=2.0*V(I-1)*F*DDF
      DP(I)=D(I)
      GOTO 20000
10103 IF(LTYP(I-2).EQ.21)THEN
      D(I)=V(I-2)*( RKCHEN(QQ,V(I-1),V(I)+DEL(I),DSUMF)-DSUM)/DEL(I) 
      DP(I)=D(I)
      DF(I)=V(I-2)*(DSUMF-F)/DEL(I)
      END IF
      GOTO 20000
10300 IF(LT.EQ.1)D(I)=1.0
      IF(LT.EQ.2)D(I)=QQ
      IF(LT.EQ.3)D(I)=QQ2
      GOTO 20000
10400 IF(LTYP(I).EQ.1.AND.J.GT.0)D(I)=C(J,JB)
      IF(LTYP(I).EQ.11)D(I)=DSUM
      IF(LTYP(I).EQ.12)THEN
      IV=9
      D(I)=( CUBIC2(QQ+V(I)+DEL(I),Q(NC3(IV),IV),
     >   C(NC3(IV),IV),NC4(IV)-NC3(IV)+1,IIV)   -DSUM)/DEL(I)
      END IF
      GOTO 20000
C==== POLDER MUST ONLY BE CALLED ONCE, IT DOES ITS OWN CHECKS TO SEE WHICH ARE REFINING
C==== note you don't get here by the normal route, but direct from model 6
C==== once only per Q
10600 CALL POLDER(QQ,E(1,JY),POLA,DSUMF,DSUMX,DP,DF,DX)
      GOTO 20000
10700 IF(LT.EQ.1)D(I)=SUM1*BETARATIO*DENOM
      IF(LT.EQ.2)D(I)=-2.*SUM1*BETARATIO*V(I-1)*QQZ*DENOM*DENOM
      GOTO 20000
10800 IF(LT.EQ.1)D(I)=DSUM
      IF(LT.EQ.2)THEN
       IF(LTYP(I-1).EQ.1)THEN
        D(I)=12.56637062*V(I-1)*V(I)*SQR/QQ
c23456789012345678901234567890123456789012345678901234567890123456789012
       ELSE IF(LTYP(I-1).EQ.61)THEN
        D(I)=V(I-1)*(2.*DSUM/V(I) +
     >      (SS1*TRJ1XBYX(QQ*(V(I)+DEL(I)))-DSUM)/DEL(I) )
       ELSE IF(LTYP(I-1).EQ.11)THEN
        D(I)=12.56637062*V(I-1)*(SQR -QR*COS(QR))/( (QQ**3)*(V(I)-RRA) )
     >   -DSUM*V(I-1)/(V(I)-RRA)
C==== any previous REFINING, model 8, radius is done above at 800
       ELSE IF(LTYP(I-1).GE.21.AND.LTYP(I-1).LE.51)THEN
C==== four types of exponential shell !
        D(I)= V(I-1)*
     >   (SHELLEXP(LTYP(I-1),QQ,RRA,V(I)+DEL(I))-DSUM)/DEL(I)
C==== any previous REFINING, model 8, radius is done above at 800
       ELSE IF(LTYP(I-1).EQ.71)THEN
	 D(I)=V(I-1)*DB
       END IF
      END IF
      GOTO 20000
C==== 12/08/03 added DP,DF,DX here
10900 D(I)=DSUM
      DP(I)=D(I)
	DX(I)=1.0
	DF(I)=DSUMF
      GOTO 20000
11100 IF(LTYP(I).EQ.1)D(I)=1.0
      IF(LTYP(I).EQ.21)D(I)=1.0
      IF(LTYP(I).EQ.2)D(I)=QQ
      IF(LTYP(I).EQ.3)D(I)=QQ2
      IF(LTYP(I).EQ.4)D(I)=QQ2*QQ
      IF(LTYP(I).EQ.5)D(I)=QQ2*QQ2
      IF(LTYP(I).EQ.6)D(I)=QQ5
      IF(LTYP(I).EQ.7)D(I)=QQ5*QQ
      IF(LTYP(I).EQ.8)D(I)=QQ5*QQ2
      IF(LTYP(I).EQ.31.AND.ABS(QQ).LE.ABS(V(I+1)))D(I)=1./
     >                            (2.0*ABS(V(I+1) ) )
      GOTO 20000
11200 IF(LTYP(I).EQ.1)D(I)=SUM1
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.1)D(I)=2.*DSUM*SUM1*V(I-1)
      IF(LTYP(I).EQ.11)D(I)=DSUM
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.11)D(I)=-2.*V(I)*QQ2*SUM1
      IF(LTYP(I).EQ.21)D(I)=DSUM
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.21)D(I)=
     >              2.*V(I-1)*((V(I)-V(I+2))*DA +V(I+2)*CQR*DC  )/QQ2
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.21)D(I)=
     >       -2.*V(I-2)*V(I)* ( DA*(V(I-1)-V(I+1))**2 +
     >      DB*V(I+1)**2 +   2.0*( V(I-1)-V(I+1) )*V(I+1)*CQR*DC )
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.21)D(I)=2.*V(I-3)*
     > ( (V(I)-V(I-2))*DA +  V(I)*DB + ( V(I-2)-2.*V(I) )*CQR*DC  )/QQ2
      IF(LT.EQ.5.AND.LTYP(I-4).EQ.21)D(I)= -2.*V(I-4)*
     >          (   ( V(I-3)-V(I-1) )*V(I-1)*SIN(QQ*V(I))*DC  )/QQ
      IF(LT.EQ.6.AND.LTYP(I-5).EQ.21)D(I)= -2.*V(I-5)*V(I)*
     >     ( DB*V(I-2)**2  + (V(I-4)-V(I-2))*V(I-2)*CQR*DC )
      GOTO 20000
11300 IF(LT.EQ.3)THEN
      IF(LTYP(I).EQ.3)THEN
        D(I)= -DA*SUM1*BETARATIO*(SSQ-1.)/V(I)
      ELSE
        D(I)= ( (3.-DA)*DSUM + F*DA*SS5*(1.-V(I+3))**2 )/V(I) -
     >      F*DSUM*(  2.*QR*QQ/5. + 2.*V(I+2)*(1.-V(I+2))*(V(I)**2)* 
     >       ((1.-V(I+3))*QQ2 )**2 /(V(I+4)*1.0E-08)  )
      END IF
      ELSE IF(LT.EQ.2)THEN
        IF(LTYP(I).EQ.2)THEN
        D(I)= SUM1*(SSQ-1.)*G*BETARATIO*
     >                ( 1./V(I) +COS(GATANQL)*QQ/SS1 )/(1. +QL2)
        ELSE
        D(I)=SS5*8.*V(I+4)*F*G*( -SS1/V(I) + 
     >                    QQ*COS(GATANQL))/(SS3*SS4*(1.+QL2))
        END IF
      ELSE IF(LT.EQ.1)THEN
          IF(QR.LE.0)THEN
          RLG=+1E20
          ELSE
          RLG=-ALOG(QR)
          ENDIF
        RLL=ALOG(1.+1./QL2)
        IF(LTYP(I).EQ.1)THEN
        D(I)=SUM1*(SSQ-1.)*BETARATIO*
     +    (1./DA+ GDF(4) + RLG + ATAN(QL)*COS(GATANQL)/SS1 -0.5*RLL)
        ELSE
C==== now the A.Allen spherical blobs fractal:
        D(I)=F*SS5*8.*V(I+5)*SS1*(RLG -ALOG(2.0) +
     >          ATAN(QL)*COS(GATANQL)/SS1  -0.5*RLL )/(SS3*SS4)
        END IF
      ELSE IF(LT.EQ.4)THEN
        D(I)=2.*F*1.E-04*V(I+1)*(1.-V(I+1))*V(I)*VS*
     >     ( (1.-V(I+2))**2 + 8.*V(I+2)*SS1/(SS4*SS3) )
      ELSE IF(LT.EQ.5)THEN
        D(I)=DSUM*(1.-2.*V(I))*( 1./(V(I)*(1.-V(I))) - 
     >   VS*F*((1.-V(I+1))*QQ2)**2/(2.0*3.141592654E-08*V(I+2)))
      ELSE IF (LT.EQ.6)THEN
        D(I)=2.*F*(1.-V(I))*(-SS5 + DSUM*V(I-1)*(1.-V(I-1))*VS*
     >           QQ2*QQ2/(2.0*3.141592654E-08*V(I+1)) )
      ELSE IF (LT.EQ.7) THEN
       D(I)=F*DSUM*V(I-2)*(1.-V(I-2))*VS*
     >           (( (1.-V(I-1))*QQ2/V(I) )**2)/
     >           (2.0*3.141592654E-08 )
      ENDIF
      GOTO 20000
11400 GOTO(11410,11420,11430,11440,11450,11460,11470),LT
      GOTO 20000
11410 D(I)=SUM1
C==== 12/08/03 added DP,DF,DX here
      DP(I)=D(I)
	DX(I)=1.0
	DF(I)=DSUMF
      GOTO 20000
C====  d/dRg  do d/dY. dY/dRg, poly version, do numerically
11420 IF(LTYP(I-1).EQ.21)GOTO 11425
      IF(LTYP(I-1).EQ.41)GOTO 11426
      IF(LTYP(I-1).EQ.51)GOTO 11427
      IF(LTYP(I-1).EQ.61)GOTO 11428
      IF(LTYP(I-1).EQ.71.OR.LTYP(I-1).EQ.81)GOTO 11429
      IF(U.GT.1.0E-02.AND.Y.GT.0.01)D(I)=V(I-1)*( 2.0*
     >   ( (1.0+U*(Y+DEL(I)))**(-1.0/U) + Y + DEL(I) -1.0)/
     >     ( (1.0+U)*((Y+DEL(I))**2) ) -SUM1)*
     >  (2.0*QQ2*V(I)) / ( (1.0+2.0*U)*DEL(I) )
      IF(U.LE.1.0E-02.AND.Y.GT.0.01)THEN
      AA=EXPSPEC(-Y)
C==== monodisp version, exact d/dRg
      IF(LTYP(I-1).EQ.1)D(I)=V(I-1)*
     >     (2.0- (Y+2.0)*AA -Y)/(0.25*V(I)*Y*Y*FSTAR)
C==== star version, exact d/dRg  CORRECTED 24/4/97 RKH
      IF(LTYP(I-1).EQ.31)D(I)=V(I-1)*
     >(2.0- (Y+2.0)*AA -Y +(1.0-FSTAR)*(AA-1.0)*((Y+1.)*AA-1.0) )
     >     /(0.25*V(I)*Y*Y*FSTAR)
      END IF
C==== low Q expansion for ANY version, exact [OOPS was SUM1=.. until 29/7/92]
      IF(Y.LE.0.01)D(I)= V(I-1)*2.0*
     >   QQ2*V(I)*(-1.0+ (1.0+3.0*U)/6.0 )
      GOTO 20000
C==== WORM sub-model N
11425 D(I)= V(I-1)*( WORM(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3)) 
     >      -SUM1)/DEL(I)
      GOTO 20000
C==== STAR DOZIER MODEL
11426 D(I)= V(I-1)*( STARD(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3))
     >      -SUM1)/DEL(I)
      GOTO 20000
C==== DIBLOCK
c23456789012345678901234567890123456789012345678901234567890123456789012
11427 D(I)= V(I-1)*( DIBLK(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3)) 
     >       -SUM1)/DEL(I)
      GOTO 20000
11428 D(I)= V(I-1)*(HSHAPE(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3)) 
     >       -SUM1)/DEL(I)
      GOTO 20000
11429 D(I)=  DSUMX*(r_KHOL(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3),V(I+4),V(I+5),
     >   DSUMF2,I-1,LTYP(I-1)) -SUM1)/DEL(I)
      GOTO 11809
C==== note dP(Q)/dU is a pain as Y is also a function of U !
C==== the result here is little good at very low Q - where the data
C==== will not be sensitive to U in any case and 2nd deriv term is rather
C==== high.
11430 IF(LTYP(I-2).EQ.21)GOTO 11437
      IF(LTYP(I-2).EQ.31)GOTO 11436
      IF(LTYP(I-2).EQ.41)GOTO 11438
      IF(LTYP(I-2).EQ.51)GOTO 11439
      IF(LTYP(I-2).EQ.61)GOTO 11433
      IF(LTYP(I-2).EQ.71.OR.LTYP(I-2).EQ.81)GOTO 11434
      IF(Y.LE.0.01.OR.U.LE.1.0E-02)GOTO 11435
      Y=QR/(1.0 + 2.0*(U+ DEL(I)) )
      D(I)= V(I-2)*( 2.0*( (1.0+(U+DEL(I))*Y)**(-1.0/(U+DEL(I))) 
     > +Y -1.0)/  ( (1.0+U+DEL(I))*(Y**2) ) -SUM1 )/DEL(I)
      GOTO 20000
C==== low Q expansion, exact deriv.
11435 D(I)=V(I-2)*Y*Y/12.0
      GOTO 20000
C==== d/dF also a pain as Y is a function of F, do numerically:
11436 D(I)= V(I-2)*(DEBGAUSS(LTYP(I-2),QQ,RR,V(I)+DEL(I))-SUM1)/DEL(I)
      GOTO 20000
C==== WORM sub-model L
11437 D(I)= V(I-2)*( WORM(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2)) 
     >     -SUM1 )/DEL(I)
      GOTO 20000
C==== STAR DOZIER sub-model 
11438 D(I)= V(I-2)*( STARD(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2)) 
     >     -SUM1 )/DEL(I)
      GOTO 20000
C==== DIBLOCK sub-model 
11439 D(I)= V(I-2)*( DIBLK(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2)) 
     >     -SUM1 )/DEL(I)
      GOTO 20000
11433 D(I)= V(I-2)*( HSHAPE(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2)) 
     >     -SUM1 )/DEL(I)
      GOTO 20000
11434 D(I)= DSUMX*( r_KHOL(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2),V(I+3),V(I+4),
     >       DSUMF2,I-2,LTYP(I-2))  -SUM1 )/DEL(I)
      GOTO 11809
C==== sub-models - third param
11440 IF(LTYP(I-3).EQ.21)D(I)= V(I-3)*
     > ( WORM(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1)) -SUM1 )/DEL(I)
      IF(LTYP(I-3).EQ.41)D(I)= V(I-3)*
     > ( STARD(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1)) -SUM1 )/DEL(I)
      IF(LTYP(I-3).EQ.51)D(I)= V(I-3)*
     > ( DIBLK(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1)) -SUM1 )/DEL(I)
      IF(LTYP(I-3).EQ.61)D(I)= V(I-3)*
     > (HSHAPE(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1)) -SUM1 )/DEL(I)
      IF(LTYP(I-3).EQ.71.OR.LTYP(I-3).EQ.81)THEN
        D(I)= DSUMX*
     > (r_KHOL(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1),V(I+2),V(I+3),
     >                   DSUMF2,I-3,LTYP(I-3)) -SUM1 )/DEL(I)
      GOTO 11809
      END IF
      GOTO 20000
C==== sub-modelS - 4th param
11450 IF(LTYP(I-4).EQ.21)D(I)= V(I-4)*
     >  ( WORM(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I)) -SUM1 )/DEL(I)
      IF(LTYP(I-4).EQ.41) D(I)= V(I-4)*
     > ( STARD(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I)) -SUM1 )/DEL(I)
      IF(LTYP(I-4).EQ.51) D(I)= V(I-4)*
     > ( DIBLK(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I)) -SUM1 )/DEL(I)
      IF(LTYP(I-4).EQ.61) D(I)= V(I-4)*
     > (HSHAPE(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I)) -SUM1 )/DEL(I)
      IF(LTYP(I-4).EQ.71) D(I)=0.0
      IF(LTYP(I-4).EQ.81)THEN
          D(I)= DSUMX*
     > (r_KHOL(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I),V(I+1),V(I+2),
     >        DSUMF2,I-4,LTYP(I-4)) -SUM1 )/DEL(I)
      GOTO 11809
      END IF
      GOTO 20000
11460 D(I)=0.0
      IF(LTYP(I-5).EQ.81)THEN
          D(I)= DSUMX*
     > (r_KHOL(QQ,V(I-4),V(I-3),V(I-2),V(I-1),V(I)+DEL(I),V(I+1),
     >        DSUMF2,I-5,LTYP(I-5)) -SUM1 )/DEL(I)
      GOTO 11809
      END IF
      GOTO 20000
11470 D(I)=0.0
      IF(LTYP(I-6).EQ.71.OR.LTYP(I-6).EQ.81)THEN
          D(I)= DSUMX*
     > (r_KHOL(QQ,V(I-5),V(I-4),V(I-3),V(I-2),V(I-1),V(I)+DEL(I),
     >        DSUMF2,I-6,LTYP(I-6)) -SUM1 )/DEL(I)
      GOTO 11809
      END IF
      GOTO 20000
11600 IF(LTYP(I).EQ.1)D(I)=(DSUM/V(I))*(1.0+ DSUM*(QQ5**2-1.0)/V(I+1) )
      IF(LTYP(I).EQ.2)D(I)=( DSUM/(V(I-1)*V(I)) )*(V(I-1)-DSUM*(QQ5**2))
c23456789012345678901234567890123456789012345678901234567890123456789012
      IF(LTYP(I).EQ.3)D(I)=( (2.0*DSUM*QQ)**2 )*(V(I-1)-V(I-2))*QQ5/
     *                        ( V(I-2)*V(I-1)*( V(I)**3) )
      IF(LTYP(I).EQ.11)D(I)=-1.0*DSUM**2
      IF(LTYP(I).EQ.21)D(I)=-QQ2*DSUM**2
      IF(LTYP(I).EQ.31)D(I)=-(QQ2*DSUM)**2
      GOTO 20000
11700 IF(LT.EQ.1)D(I)= DSUM
      IF(LT.EQ.2)D(I)= 4.*V(I+1)*V(I+1)*V(I)*DSUM*V(I-1)/ 
     2   (1.+(V(I)*V(I+1))**2) + V(I-1)*QQ3*( (1.+ QQ7)/SS2 +
     3  (1.-QQ7)/SS4 -4.*(1.+QQ6)*V(I+1)*V(I+1)*(QQ+V(I))/(SS1*SS2) +
     4                4.*(1.-QQ6)*V(I+1)*V(I+1)*(QQ-V(I))/(SS3*SS4) )
      IF(LT.EQ.3)D(I)= 4.*V(I)*V(I-1)*V(I-1)*DSUM*V(I-2)/ 
     2   (1.+(V(I)*V(I-1))**2) + V(I-2)*QQ3*( 
     3   -4.*(1.+QQ6)*V(I)*(QQ+V(I-1))**2/(SS2*SS1)
     4   -4.*(1.-QQ6)*V(I)*(QQ-V(I-1))**2/(SS3*SS4) )
C=== line above had - sign missing until 30/1/90 RKH
      GOTO 20000
C==== RESORT TO NUMERICAL DERIVATIVES FOR RODS
11800 GOTO(11801,11802,11803,11804,11805,11806,11807,11807),LT
11801 D(I)=DSUM
C==== 12/08/03  added DF & DP here
      DP(I)=D(I)
      DX(I)=1.0
      DF(I)=DSUMF
      GOTO 20000
11802 IF(LTYP(I-1).LE.11)THEN
      D(I)=DSUMX*( RODFF(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3),
     >      DSUMF2,I-1) - DSUM )/DEL(I)
      ELSE
      V(I)=V(I)+DEL(I)
      D(I)=DSUMX*( RODSHEAR(QQ,V(I),DSUMF2,I-1,LTYP(I-1))- DSUM )/DEL(I)
      V(I)=V(I)-DEL(I)
      END IF
      GOTO 11809
11803 IF(LTYP(I-2).LE.11)THEN
      D(I)=DSUMX*( RODFF(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2),
     >      DSUMF2,I-2)  - DSUM )/DEL(I)
      ELSE
      V(I)=V(I)+DEL(I)
      D(I)=DSUMX*( RODSHEAR(QQ,V(I-1),DSUMF2,I-2,LTYP(I-2))
     >         - DSUM )/DEL(I)
      V(I)=V(I)-DEL(I)
      END IF
      GOTO 11809
11804 IF(LTYP(I-3).LE.11)THEN
      D(I)=DSUMX*( RODFF(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1),
     >      DSUMF2,I-3)  - DSUM )/DEL(I)
      ELSE
      V(I)=V(I)+DEL(I)
      D(I)=DSUMX*( RODSHEAR(QQ,V(I-2),DSUMF2,I-3,LTYP(I-3)) 
     >       - DSUM )/DEL(I)
c23456789012345678901234567890123456789012345678901234567890123456789012
      V(I)=V(I)-DEL(I)
      END IF
      GOTO 11809
11805 IF(LTYP(I-4).LE.11)THEN
      D(I)=DSUMX*( RODFF(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I),
     >      DSUMF2,I-4)  - DSUM )/DEL(I)
      ELSE
      V(I)=V(I)+DEL(I)
      D(I)=DSUMX*( RODSHEAR(QQ,V(I-3),DSUMF2,I-4,LTYP(I-4)) 
     >        - DSUM )/DEL(I)
      V(I)=V(I)-DEL(I)
      END IF
      GOTO 11809
C==== GAMMA for shear rod
11806 V(I)=V(I)+DEL(I)
      D(I)= RODSHEAR(QQ,V(I-4),DSUMF2,I-5,LTYP(I-5))
C====      D(I)= DSUMX*(RODSHEAR(QQ,V(I-4),DSUMF2,I-5,LTYP(I-5)) - DSUM )/DEL(I)
c====      V(I)=V(I)-DEL(I)
C==== as expt 28/8/96 - due to large 2nd derivs ,try double calc here
      V(I)=V(I)-2.*DEL(I)
      D(I)=0.5*DSUMX*(D(I) - RODSHEAR(QQ,V(I-4),DSUMF2,I-5,LTYP(I-5))
     >        )/DEL(I)
      V(I)=V(I)+DEL(I)
      GOTO 11809
C==== psi and +-dpsi should not be refining !
11807 D(I)=0.0
      GOTO 20000
11809 DP(I)=D(I)
      DF(I)=DSUMX*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
11900 IF(LT.EQ.1)D(I)=SUM1*BETARATIO*SS1
      IF(LT.EQ.2)D(I)= -3.0*V(I-1)*BETARATIO*QQ*( (SQR/QR + SS1)/QR )
      GOTO 20000
12000 IF(LT.EQ.1)D(I)=SUM1
      IF(LT.EQ.2)D(I)=ALOG(QQ)*V(I-1)*SUM1
      GOTO 20000
C==== ELLIPSOID use numerical derivatives
12100 GOTO(12101,12102,12103),LT
12101 D(I)=DSUM
      DP(I)=D(I)
      DX(I)=1.0
C==== 12/08/03 added DF here
	DF(I)=DSUMF
      GOTO 20000
12102 D(I)=V(I-1)*(ELLIPSOID(QQ,V(I)+DEL(I),V(I+1),DSUMF2,I-1)-
     *                       DSUM)/DEL(I)
      DP(I)=D(I)
      DF(I)=V(I-1)*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
12103 D(I)=V(I-2)*(ELLIPSOID(QQ,V(I-1),V(I)+DEL(I),DSUMF2,I-2)-
     *                       DSUM)/DEL(I)
      DP(I)=D(I)
      DF(I)=V(I-2)*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
C==== HARD SPHERES STRUCTURE FACTOR - NUMERICAL DERIVS
12200 GOTO (12210,12220,12230,12240),LT
      GOTO 20000
12210 IF(LTYP(I).NE.11)D(I)=SUM1*BETARATIO*
     >         (HARDSPH(QQ,V(I)+DEL(I),V(I+1))-SSQ)/DEL(I)
      IF(LTYP(I).EQ.11)D(I)=SUM1*BETARATIO*
     >     (HARD_SHARMA(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3))-SSQ)/DEL(I)
      GOTO 20000
12220 IF(LTYP(I-1).NE.11)D(I)=SUM1*BETARATIO*
     >         (HARDSPH(QQ,V(I-1),V(I)+DEL(I))-SSQ)/DEL(I)
      IF(LTYP(I-1).EQ.11)D(I)=SUM1*BETARATIO*
     >     (HARD_SHARMA(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2))-SSQ)/DEL(I)
      GOTO 20000
12230 IF(LTYP(I-2).EQ.11)D(I)=SUM1*BETARATIO*
     >     (HARD_SHARMA(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1))-SSQ)/DEL(I)
      GOTO 20000
12240 IF(LTYP(I-3).EQ.11)D(I)=SUM1*BETARATIO*
     >     (HARD_SHARMA(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I))-SSQ)/DEL(I)
      GOTO 20000
C==== RMSA STRUCTURE FACTOR - NUMERICAL DERIVATIVES [ CHECK THIS ! RKH 15/1/91]
12300 D(I)=SUM1*BETARATIO*( SOFQ( QQ,DCOEFF(1,LT+1),COEFF(1,LT+1) ) 
     *           -SSQ ) /DEL(I)
      GOTO 20000
C==== TWO SHELL ELLIPSOID - NUMERICAL DERIVATIVE
12400 GOTO(12401,12402,12403,12404,12405),LT
12401 D(I)=DSUM
      DP(I)=D(I)
	DX(I)=1.0
C==== 12/08/03 add DF here
	DF(I)=DSUMF
      GOTO 20000
12402 D(I)=V(I-1)*(ELLIPSHELL(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3),
     *       DSUMF2,I-1)-DSUM)/DEL(I)
      DP(I)=D(I)
      DF(I)=V(I-1)*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
12403 D(I)=V(I-2)*(ELLIPSHELL(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2),
     *       DSUMF2,I-2)-DSUM)/DEL(I)
      DP(I)=D(I)
      DF(I)=V(I-2)*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
12404 D(I)=V(I-3)*(ELLIPSHELL(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1),
     *       DSUMF2,I-3)-DSUM)/DEL(I)
      DP(I)=D(I)
      DF(I)=V(I-3)*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
12405 D(I)=V(I-4)*(ELLIPSHELL(QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I),
     *       DSUMF2,I-4)-DSUM)/DEL(I)
      DP(I)=D(I)
      DF(I)=V(I-4)*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
C==== RMSA + CRITICAL SCATTERING STRUCTURE FACTOR - DERIVATIVES
12500 IF(LT.GE.1.AND.LT.LE.4)THEN
      D(I)=SUM1*BETARATIO*( SOFQ( QQ,DCOEFF(1,LT+1),COEFF(1,LT+1) ) 
     *           -SSQ1 ) /DEL(I)
      ELSE
      IF(LT.EQ.5)D(I)=SUM1/(1.0+(V(I+1)*QQ)**2.0)
      IF(LT.EQ.6)D(I)=-2.0*SUM1*V(I-1)*QQ**2.0*V(I)/
     >             (1.0+(V(I)*QQ)**2.0)**2
      ENDIF
      GOTO 20000
12600 IF(LT.EQ.1)THEN
        D(I)= DSUM
      ELSE IF(LT.EQ.2)THEN
      IF(LTYP(I).EQ.2)THEN
       D(I)= DSUM*V(I-1) * (ALOG(QQ) - GDF(4))
     +       + V(I-1) * SS3 * GA * 1.570796327 * COS(ARG)
      ELSE
        D(I)=DSUM*V(I-1)*(-GDF(4)+ 0.5*ALOG(1.+1./(QL*QL)) +
     >               ATAN(QL)/TAN(ARG) ) - V(I-1)*SS4*SS5*ALOG(ABS(QL))
      END IF
      ELSE IF(LT.EQ.3)THEN
       D(I)=2.*DSUM*V(I-2)/V(I)
      ELSE IF(LT.EQ.4)THEN
C2345 789012345678901234567890123456789012345678901234567890123456789012
       D(I)=DSUM*V(I-3)*( (DA-2.)/V(I) +
     >       (5-DA)/(V(I)*(1.+QL*QL)) -
     >       QQ*(DA-1.)/ (TAN(ARG)*(1.+QL*QL)) )
      ENDIF
      GOTO 20000
12700 IF(LTYP(I).EQ.1)D(I)=DENOM
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.1)D(I)=-2.0*SUM1*DENOM*QQZ
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.1)D(I)=DENOM2**2
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.1)D(I)=-4.0*DSUM*DENOM2*QQA
c
      IF(LTYP(I).EQ.11)D(I)=V(I+1)*DENOM/6.283185307
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.11)D(I)=V(I-1)*(DENOM/6.283185307)*
     >                       (1.0-0.5*DENOM*(V(I)**2))
C==== 0.5 INSERTED IN ABOVE LINE 8/2/96
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.11)D(I)=V(I-2)*V(I-1)*
     >                          QQA*DENOM**2/3.141592654
C
      IF(LTYP(I).EQ.21.OR.LTYP(I).EQ.31.OR.LTYP(I).EQ.41
     >  .OR.LTYP(I).EQ.51.OR.LTYP(I).EQ.61
     >  .OR.LTYP(I).EQ.71.OR.LTYP(I).EQ.81 )D(I)=DSUM
C==== note the V(I) below craftily includes the 2.35 !
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.21)D(I)=SS2**2*SUM1/V(I)
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.21)D(I)=SS2*SUM1/SS1
C
c23456789012345678901234567890123456789012345678901234567890123456789012
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.71)D(I)=SS2**2*SUM1/
     >                               (V(I)+SS4*2.354820045)
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.71)D(I)=
     >    SS2*SUM1*(1.-ABS(SS2)*V(I+1)-SS2*V(I+2))/(SS1+SS4)
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.71)D(I)=ABS((SS2**3)*SUM1)
      IF(LT.EQ.5.AND.LTYP(I-4).EQ.71)D(I)=(SS2**3)*SUM1
C
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.31)D(I)=V(I-1)*SS4
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.31)D(I)=V(I-2)*SS5
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.31)D(I)=-V(I-3)*SS3
C
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.41.AND.SS5.LE.0.0)D(I)=
     >                  SS2**2*SUM1/V(I)
C==== else its zero
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.41)THEN
      IF(SS5.LE.0.0)THEN
      D(I)=SS2*SUM1/SS1
      ELSE
      D(I)=SUM1/SS1
      END IF
      END IF
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.41.AND.SS5.GT.0.0)D(I)=SS2*SUM1/SS1
C==== else again its zero
C
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.51)THEN
c==== note we fit: fwhm=2.35 sig, dI/df  = dI/dsig * dsig/df = (dI/dsig)/2.35
      IF(SS5.LE.SS3)THEN
      D(I)=SS2**2*SUM1/V(I)
      ELSE
      D(I)=(V(I)/(2.354820045*SS1)**2)*SUM1
      END IF
      END IF
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.51)THEN
      IF(SS5.LE.SS3)THEN
      D(I)=SS2*SUM1/SS1
      ELSE
      D(I)=SUM1/SS1
      END IF
      END IF
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.51.AND.SS5.GT.SS3)D(I)=
     >    (-SS3/SS1 + SS2)*SUM1/SS1
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.61)D(I)=V(I-1)*
     >       (RIKCAR(SS2,V(I)+DEL(I),V(I+2),V(I+3)) -DSUM)/DEL(I)
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.61)D(I)= -V(I-2)*
     >       (RIKCAR(SS2+DEL(I),V(I-1),V(I+1),V(I+2)) -DSUM)/DEL(I)
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.61)D(I)= V(I-3)*
     >       (RIKCAR(SS2,V(I-2),V(I)+DEL(I),V(I+1)) -DSUM)/DEL(I)
      IF(LT.EQ.5.AND.LTYP(I-4).EQ.61)D(I)= V(I-4)*
     >       (RIKCAR(SS2,V(I-3),V(I-1),V(I)+DEL(I)) -DSUM)/DEL(I)
C==== else its zero
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.81)D(I)=V(I-1)*
     >       (CRTPFN(QQ,V(I)+DEL(I),V(I+1),V(I+2)) -DSUM)/DEL(I)
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.81)D(I)=V(I-2)*
     >       (CRTPFN(QQ,V(I-1),V(I)+DEL(I),V(I+1)) -DSUM)/DEL(I)
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.81)D(I)=V(I-3)*
     >       (CRTPFN(QQ,V(I-2),V(I-1),V(I)+DEL(I)) -DSUM)/DEL(I)
      GOTO 20000
C==== RESORT TO NUMERICAL DERIVATIVES FOR POLYDISP LAYER
12800 IF(LTYP(I-LT+1).LE.31)GOTO(12801,12802,12803,12804,12805),LT
      IF(LT.GT.1)THEN
      V(I)=V(I)+DEL(I)
      D(I)=DSUMX*(RLAYFF2(LTYP(I-LT+1),QQ,V(I-LT+2),DSUMF2)-DSUM)/DEL(I)
      V(I)=V(I)-DEL(I)
      GOTO 12809
      END IF
C
12801 D(I)=DSUM
      DX(I)=1.0
C==== 12/08/03 added DP and DF here
	DP(I)=D(I)
	DF(I)=DSUMF
      GOTO 20000
C==== interfacial T or layer L
12802 D(I)=DSUMX*
     2  ( RLAYFF(LTYP(I-1),QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3),DSUMF2)
     3   - DSUM )/DEL(I)
      GOTO 12809
c==== layer L or contrast
12803 D(I)=DSUMX*
     2  ( RLAYFF(LTYP(I-2),QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2),DSUMF2)
     3   - DSUM )/DEL(I)
      GOTO 12809
c==== sig(L)/L or shell H or 1/kappa
12804 D(I)=DSUMX*
     2  ( RLAYFF(LTYP(I-3),QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1),DSUMF2)
     3    - DSUM )/DEL(I)
      GOTO 12809
c==== Lorentz R*Sigma
12805 D(I)=DSUMX*
     2  ( RLAYFF(LTYP(I-4),QQ,V(I-3),V(I-2),V(I-1),V(I)+DEL(I),DSUMF2)
     3    - DSUM )/DEL(I)
12809 DP(I)=D(I)
      DF(I)=DSUMX*(DSUMF2-DSUMF)/DEL(I)
      GOTO 20000
12900 IF(LTYP(I).EQ.1)THEN
      D(I)=SUM1*BETARATIO*
     >        (PARCRYS(QQ,V(I)+DEL(I),V(I+1),V(I+2),V(I+3))-SSQ)/DEL(I)
      ELSE IF(LT.EQ.2.AND.LTYP(I-1).EQ.1)THEN
      D(I)=SUM1*BETARATIO*
     >        (PARCRYS(QQ,V(I-1),V(I)+DEL(I),V(I+1),V(I+2))-SSQ)/DEL(I)
      ELSE IF(LT.EQ.3.AND.LTYP(I-2).EQ.1)THEN
      D(I)=SUM1*BETARATIO*
     >        (PARCRYS(QQ,V(I-2),V(I-1),V(I)+DEL(I),V(I+1))-SSQ)/DEL(I)
      ELSE IF(LT.EQ.4.AND.LTYP(I-3).EQ.1)THEN
      D(I)=0.0
C==== 3 Phase paracryst model, do numerically
      ELSE IF(LTYP(I).EQ.11.OR.LTYP(I).EQ.21)THEN
      D(I)=DSUM
C==== 29 21 LTYP=10 gives LT =0 for Lorentz Rsigma
      ELSE IF((LT.EQ.0).AND.(LTYP(I-9).EQ.21))THEN
      V(I)=V(I)+DEL(I)
c====         scale            pass V() starting at N
      D(I)= V(I-9)*(PARA3(QQ,LTYP(I-9),
     >               LTYP(I-5),LTYP(I-2),V(I-8))-DSUM)/DEL(I)
      V(I)=V(I)-DEL(I)
C==== now all the rest of model 29 11 or 29 21
      ELSE IF((LT.GE.2).AND.(LTYP(I+1-LT).GE.11))THEN
      V(I)=V(I)+DEL(I)
c====         scale            pass V() starting at N
      D(I)= V(I+1-LT)*(PARA3(QQ,LTYP(I+1-LT),
     >               LTYP(I+5-LT),LTYP(I+8-LT),V(I+2-LT))-DSUM)/DEL(I)
      V(I)=V(I)-DEL(I)
      END IF
20000 CONTINUE
      RETURN
      END
C
      SUBROUTINE SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,
     *           PRODSQ,BETARATIO,LM,D,DP,DF,DX)
      INCLUDE 'FISHDIM.PAR'
      DIMENSION LM(MV),D(MV),DP(MV),DF(MV),DX(MV)
c==== 2/11/00 use K8=1 to NOT apply beta correction, NB this is done
c==== in betafun, not here !
c====
C==== correct for polydisperse or anisotropic particles (SEE MODEL 6)
c==== I(Q) = P(Q) * (1.0 + BETA(Q)*( S(Q)- 1.0) )
C====  BETA(Q) = SUMF**2/ (SUMX * SUMPQ)
C====
C====  NOTE the SUMX is needed to get the normalisation correct.
C====            SUMPQ is assumed to be SUM i.e. that the model is being
C====            used logically ! SUMPQ is used in the main routines only
C===             as a monitor to save P(Q) separately to I(Q)
C====  SUMF, SUMX and SUMPQ may depend on parameters for P(Q)
C====  so dP(q)/dv  = dP(Q,original)/dv * BETA(Q)  + P(Q) * dBETA(Q)/dv
C
C====  where P(Q) dBETA(Q)/dv = ( 2.*SUMF/SUMX)*dSUMF/dv 
C====                  - (SUMF/SUMX)**2 * dSUMX/dv  - BETA(Q)dSUMPQ/dv
C====
C====  and the three partial derivatives are stored in DF(), DX() and DP()
C====
C==== input: SUM,SUMF,SUMX,SUMPQ,SSQ,LM
C==== output: SUM1,BETARATIO,SSQP,PRODSQ
C==== modify: D,DF,DX,DP

      BETARATIO=BETAFUN(SUMF,SUMX,SUM)
      SSQP= 1.00 + BETARATIO*(SSQ-1.0)
C
C==== MULTIPLY ALL PREVIOUS DERIVATIVES  20/3/98 add test on I1.gt.0 !
      IF(I1.GE.1)THEN
      DO 100 JJ=1,I1
      IF(LM(JJ).NE.3.AND.LM(JJ).NE.4)D(JJ)=D(JJ)*SSQP
      IF(ABS(BETARATIO-1.0).LT.1.E-08)GOTO 100
      FX=SUMF/SUMX
      D(JJ)=D(JJ)+(SSQ-1.0)*( FX*(2.0*DF(JJ) -FX*DX(JJ)) -BETARATIO*DP(JJ))
  100 CONTINUE
C
C==== make sure the derivs of BETARATIO are only used once :
      DO 300 JJ=1,I1
      DP(JJ)=0.0
      DF(JJ)=0.0
  300 DX(JJ)=0.0
      END IF
C
      SUM1=SUM
      SUM=SUM*SSQP
      PRODSQ=PRODSQ*SSQP
      RETURN
      END
C
      SUBROUTINE CALCUL(KS,JC,JD,JB,JY,JW,JAM,JPQ,JSQ,JBT,JBUG,SSE,NPSMEAR,MS)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== THIS SHOULD PERFORM A CALC FOR A COMPLETE DATA SET
C==== IF JD=JC, SHOULD DO A CALC OVER THE KC RANGE , WHICH MAY BE LARGER FOR PLOTS ETC. SEE PICT2
C==== 18/2/92 NOTE now zeroes the unused parts of JC or JW calc or bkg arrays
C==== NOTE  this routine will call smearing here if needed
C====
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      SSE=0.0
      J1=NC1(JD)
      J2=NC2(JD)
      J3=NC3(JD)
      IF(J1*J2.LE.0)J1=J3
      J4=NC4(JD)
      NPSMEAR=0
c==== 22/12/95 be even safer, zero the lot here first !
      DO J=1,J4
      C(J,JC)=0.0
      IF(JW.GT.0)C(J,JW)=0.0
      IF(JPQ.GT.0)C(J,JPQ)=0.0
      IF(JSQ.GT.0)C(J,JSQ)=0.0
      IF(JBT.GT.0)C(J,JBT)=0.0
      IF(JBUG.GT.0)C(J,JBUG)=0.0
      END DO
C
C==== add step MS to loop to speed up Marquardt for long calcs & to 
c==== make it plain on plots !!!
c
      DO 20000 J=1,J4,MS
      IF(J.LT.J1.OR.(J.GT.J2.AND.J.LT.J3))GOTO 20000
      SUMW=0.0
      QQ=Q(J,JD)
      CALL CALCQ(KS,QQ,CALC,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                J,JB,JY,NPSMEAR)
      C(J,JC)=CALC
C=== if the models were used sensibly  CALC= SUMPQ*PRODSQ + SUMW
c===     PRODSQ includes corrections for polydispersity by
C====                         S(Q)prime= 1.0 + BETA(Q)*(SQ()-1.0)
      IF(JW.GT.0)C(J,JW)=SUMW
      IF(JPQ.GT.0)C(J,JPQ)=SUMPQ
      IF(JSQ.GT.0)C(J,JSQ)=PRODSQ
      IF(JBT.GT.0)C(J,JBT)=BETARATIO
      IF(JBUG.GT.0)C(J,JBUG)=SUMBUG
20000 CONTINUE
      IF(NPSMEAR.GT.0)CALL SMEARING(KS,JC,JD,JW,JB,JY,JAM,J1,J4)
C==== 14/2/92 remove calc of SSE out of main loop to AFTER the smearing !
      DO 30000 J=J1,J4
      IF(J.GT.J2.AND.J.LT.J3)GOTO 30000
C==== ASSUME WEIGHTS ARE IN E(1,JC)
      SSE=SSE+E(J,JC)*(C(J,JD)-C(J,JC))**2
30000 CONTINUE
      RETURN
      END
C
      SUBROUTINE CALCQ(KS,QQ,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                      J,JB,JY,NPSMEAR)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985, this revision Jan 1989.
C====
C====  Calculate I(Q) at one point in Q, should be totally general as to which 
C====  Q value is used EXCEPT for model 4 experimental background
C====  which is why J and JB are passed through.   JY is needed as the location
C==== for the polydispersity workspace.  NPSMEAR is set here and passed back.
C====   the reason for rewriting the CALCUL routine this way round is so that 
C====  the smearing routine may extrapolate outside of the usual Q range.
c==== We get round this problem as model 4 does 
c==== not need to be smeared so  PASS  J=0 to ignore model 4
C==== 7/11/94 RKH doscovered KS not being passed through to here !
c
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
c==== THIS COMMON ADDED BY ASHLEY FOR MODEL 13
      COMMON/GAMFUN/GDF(4),IGAMMA
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/MODEL24/JJJ
C==== this for model 23 charged particles, intermediate parameters are calc
C==== ahead of time by SSQFNSET()
      REAL*8 dcoeff
      REAL coeff
      common /sqhpc/ dcoeff(4,5),coeff(6,5)
      CALC1=0.0
      SUM=0.0
      SUMW=0.0
      SUMPQ=0.0
      SUMF=0.0
      DSUMF=0.0
      SUMX=0.0
      PRODSQ=1.0
      BETARATIO=1.0
      SUMBUG=0.0
      QQ2=QQ*QQ
      IM1=KS
      DO 10000 I=1,NP
C====
C==== model 88, switches parameters for different data sets
C====  e.g. cards following 88 2 are for set KS=2 only, until
C==== another card 88 n,  note 88 0 reverts to use by all KS
C====  By this means a single model may be used, with different
C==== scale parameters and backgrounds for each of several sets of data
C==== refined simultaneously.
C====
      IF(LM(I).NE.88)GOTO 40
      IM1=MOD(LTYP(I),10)
      IF(IM1.LE.0)IM1=KS
C==== the MODEL 88 card itself does nothing else, skip out here
c==== else it follows the 99 route !    RKH 7/11/94
      GOTO 10000
40    IF(MOD(LTYP(I),10).NE.1)GOTO 10000
      IM=LM(I)
      IF(IM1.NE.KS)IM=2
      GOTO(100,10000,300,400,10000,600,700,800,900,10000,1100,1200,
     *   1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400,
     *   2500,2600,2700,2800,2900),IM
C=== SKIP OUT ANYTHING OTHER THAN MODEL 99     RKH 7/11/94
      IF(IM.NE.99)GOTO 10000
      CALC1=(SUM+SUMW)*V(I)
      SUMW=SUMW*V(I)
      SUMPQ=SUMPQ*V(I)
      GOTO 10000
  100 QR=QQ*V(I+1)
      IF(LTYP(I).EQ.1)THEN
      SQR=SIN(QR)
      CQR=COS(QR)
      F=3.*(SQR-QR*CQR)/QR**3
      DSUM=V(I)*F**2
c
      ELSE IF(LTYP(I).EQ.11)THEN
      F=TRJ1XBYX(QR)
      DSUM=V(I)*F**2
c
c====         LTYP=21 Schultz spheres added 19/04/2002 RKH
      ELSE IF(LTYP(I).EQ.21)THEN
      DSUM=V(I)*RKCHEN(QQ,V(I+1),V(I+2),F)
      END IF
c
      SUMF=SUMF+F*V(I)
      SUMX=SUMX+V(I)
      SUMPQ=SUMPQ+DSUM 
      SUM=SUM+DSUM
      GOTO 10000
  300 SUMW=SUMW+V(I)
      IF(LTYP(I).EQ.1)SUMW=SUMW+V(I+1)*QQ+V(I+2)*QQ*QQ
      GOTO 10000
  400 IF(LTYP(I).NE.1)GOTO 410
      IF(J.GT.0)SUMW=SUMW+V(I)*C(J,JB)
      GOTO 10000
C==== add scaled and shifted resolution function from set IV = 9 ( for now) RKH 11/10/94
c==== this version of cubic interp, returns ZERO if QQ is out of range
  410 IV=9
      SUMW=SUMW+V(I)*CUBIC2(QQ+V(I+1),Q(NC3(IV),IV),
     >                  C(NC3(IV),IV),NC4(IV)-NC3(IV)+1,IIV)
      GOTO 10000
  600 IF(JY.LE.0)GOTO 10000
	CALL POLCAL(QQ,C(1,JY),POLA,DSUMF,DSUMX)
      SUMPQ=SUMPQ+POLA
      SUMF=SUMF+DSUMF
      SUMX=SUMX+DSUMX
      SUM=SUM+POLA
      GOTO 10000
  700 SSQ=(1.0+  V(I)/( 1.0+ (V(I+1)*QQ)**2)  )
C==== 20/3/98 discovered that betafun NOT being called here
c==== thus getting cranky results for ROD * critical scatter
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
      GOTO 10000
  800 IF(LTYP(I).EQ.1)THEN
      QR=QQ*V(I+1)
      SUM=SUM+12.56637062*V(I)*(SIN(QR)-QR*COS(QR))/(QQ*QQ2)
c==== end-on cylindrical shell
      ELSE IF(LTYP(I).EQ.61)THEN
      QR=QQ*V(I+1)
      SUM=SUM+V(I)*3.141592654*(V(I+1)**2)*TRJ1XBYX(QR)
      ELSE
C==== find previous radius or set it zero
      RRA=0.0
      IF(I-2.GT.0)THEN
      IF(LM(I-2).EQ.8)RRA=V(I-1)
      END IF
      IF(LTYP(I).EQ.11)THEN
      QR=QQ*V(I+1)
C====             ( beware SQR not defined here)
      SUM=SUM+V(I)*12.56637062*(2.*(COS(QQ*RRA)-COS(QR))/QQ 
     >   + RRA*SIN(QQ*RRA) -V(I+1)*SIN(QR) )
     >               /( (QQ*QQ2)*( V(I+1) -RRA ) )
      ELSE IF(LTYP(I).LE.51)THEN
c==== exponential shells
      SUM=SUM + V(I)*SHELLEXP(LTYP(I),QQ,RRA,V(I+1))
	ELSE IF(LTYP(I).EQ.71)THEN
C==== Gaussian shell
	SUM=SUM+V(I)*SHELLGAUSS(QQ,RRA,V(I+1))
      END IF
      END IF
      GOTO 10000
C==== SQUARE OPERATION   12/08/03 add beta correction stuff
  900 DSUMF=SUM
	DSUM=SUM*SUM
      SUM=DSUM*V(I)
	SUMF=DSUMF*V(I)
	SUMX=V(I)
      SUMPQ=SUM
C==== MODEL 10, COMPLEX CONTRAST, IS USED IN POLCAL ONLY, SEE MODEL 6
      GOTO 10000
C==== MODEL 11 GENERAL POLYNOMIAL
 1100 QQ5=QQ2*QQ2*QQ
      IF(LTYP(I).NE.1)GOTO 1105 
      DSUM=V(I)+QQ*V(I+1)+QQ2*V(I+2)+QQ*QQ2*V(I+3)+
     * QQ2*QQ2*V(I+4)+QQ5*V(I+5)+QQ5*QQ*V(I+6)+QQ5*QQ2*V(I+7)
      SUM=SUM+DSUM
      SUMPQ=SUMPQ+DSUM
      GOTO 10000
 1105 IF(LTYP(I).NE.21)GOTO 1110
      SUM=SUM+V(I)
      SUMPQ=SUMPQ+V(I)
      GOTO 10000
C==== delta function at Q=0, V(I+1) is a non-refining tolerance
 1110 IF(ABS(QQ).LE.ABS(V(I+1)))THEN
      DSUM=V(I)/(2.0*ABS(V(I+1) ) )
      SUM=SUM+DSUM
      SUMPQ=SUMPQ+DSUM
      END IF
      GOTO 10000
C==== MODEL 12  GUINIER RADIUS FIT or  MODIFIED POROD  K.Q**-4exp(-sig**2.Q**2)
 1200 IF(LTYP(I).EQ.1)  SUM1=V(I)*EXPSPEC(-V(I+1)**2*QQ2*0.333333333333)
      IF(LTYP(I).EQ.11) SUM1=V(I)*EXPSPEC(-(V(I+1)**2)*QQ2 -2.*ALOG(QQ2))
      IF(LTYP(I).EQ.21) SUM1=V(I)*(
     >   ((V(I+1)-V(I+3))**2)*EXPSPEC(-(V(I+2)**2)*QQ2 - ALOG(QQ2)) +
     >   ( V(I+3)**2)*EXPSPEC(-( V(I+2)**2+V(I+5)**2 )*QQ2 - ALOG(QQ2))+
     >  2.*( V(I+1)-V(I+3) )*V(I+3)*COS( QQ*V(I+4) )*
     >    EXPSPEC( -0.5*( 2.*V(I+2)**2+V(I+5)**2 )*QQ2 - ALOG(QQ2)) )/QQ2
      SUM=SUM+SUM1
      SUMPQ=SUMPQ+SUM1
      GOTO 10000
C==== MODEL 13  FRACTALS
1300  IF(V(I).LT.1.0)V(I)=1.0
      DA=V(I)
      G=DA-1
C==== see comments in DERIV on gamma fn.
      IF(IGAMMA.NE.1.OR.GDF(2).NE.G)THEN
        GA=GAMMA(G)
        IGAMMA=1
        GDF(2)=G
        GDF(3)=GA
C==== THIS IS DERIV OF GAMMA(D-1) DIVIDED BY GAMMA(D-1)
        GDF(4)=10000.*( GAMMA(G+.0001)/GA - 1.0)
      ELSE
        GA=GDF(3)
      ENDIF
      QL=QQ*V(I+1)
      QR=QQ*ABS(V(I+2))
      QL2=QL*QL
      S1=SIN(G*ATAN(QL))
      S3=(1.+1./QL2)**(G/2.)
      IF(LTYP(I).NE.1)GOTO 1350
      S2=DA*GA
      S4=QR**DA
      SSQ=(1.+ S1*S2/(S3*S4) )
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
      GOTO 10000
C==== A.Allen model for fractal spherical blobs, NOTE this is S(Q)P(Q)
c==== and has 7 cards.
1350  S4=(2.*QR)**DA
C2345 789012345678901234567890123456789012345678901234567890123456789012
      F=1./(1.0+ QR**2/5.0 + V(I+4)*(1.-V(I+4))*(2.0*
     >  (V(I+2)**3)/3.0 )*( ((1.-V(I+5))*QQ2)**2 )/(1.0E-08*V(I+6)) )
      DSUM=F*1.0E-04*V(I+4)*(1.-V(I+4))*(V(I+3)**2)*4.0*3.141592654*
     > (V(I+2)**3)*( (1.-V(I+5))**2 + 8.*V(I+5)*S1/(S3*S4) )/3.0
      SUMF=SUMF+SQRT(ABS(DSUM))
      SUMX=SUMX+1.0
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      GOTO 10000
C==== MODEL 14 GAUSSIAN COIL ETC.
 1400 IF(LTYP(I).EQ.21)GOTO 1420
      IF(LTYP(I).EQ.41)GOTO 1440
      IF(LTYP(I).EQ.51)GOTO 1450
      IF(LTYP(I).EQ.61)GOTO 1460
      IF(LTYP(I).EQ.71.OR.LTYP(I).EQ.81)GOTO 1470
      RR=V(I+1)**2
      DSUM=DEBGAUSS(LTYP(I),QQ,RR,V(I+2))
 1418 SUMF=SUMF+SQRT(ABS(DSUM))*V(I)
      SUMX=SUMX+V(I)
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      GOTO 10000
 1420 DSUM=WORM(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1440 DSUM=STARD(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1450 DSUM=DIBLK(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1460 DSUM=HSHAPE(QQ,V(I+1),V(I+2),V(I+3),V(I+4))
      GOTO 1418
 1470 IF(NMETH(I).LE.0)CALL INTPIC2(I)
      DSUM=V(I)*R_KHOL(QQ,V(I+1),V(I+2),V(I+3),V(I+4),V(I+5),V(I+6),
     >     DSUMF, I, LTYP(I) )
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      SUMF=SUMF+V(I)*DSUMF
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 15 SMEARING OF DATA - SET FLAGS HERE
 1500 NPSMEAR=I      
      GOTO 10000
C==== MODEL 16, TEUBNER & STREY,  I(O), I(M) Q(M)
 1600 IF(LTYP(I).EQ.11)GOTO 1610
      DSUM=V(I)*V(I+1)/( (V(I+1)-V(I))*( (QQ/V(I+2))**2 -1)**2 +V(I) )
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      GOTO 10000
 1610 DSUM= 1./(V(I) + V(I+1)*QQ2 +V(I+2)*QQ2*QQ2 )
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      GOTO 10000
 1700 QQ6=0.0
      IF(ABS(QQ).GT.1.E-30)QQ6=V(I+1)/QQ
      SS2=( 1+ (V(I+2)*(QQ + V(I+1)))**2)**2
      SS4=( 1+ (V(I+2)*(QQ - V(I+1)))**2)**2
      QQ3=0.5*(1 + (V(I+2)*V(I+1))**2)**2
      DSUM=QQ3*( (1.0+QQ6)/SS2 + (1.0-QQ6)/SS4 )
      SUM=SUM + DSUM*V(I)
      SUMPQ=SUMPQ + DSUM*V(I)
      GOTO 10000
C==== MODEL 18, TWO SHELL ROD
 1800 IF(LTYP(I).LE.11)THEN
      IF(NMETH(I).LE.0)CALL INTPIC(I)
      DSUM=V(I)*RODFF(QQ,V(I+1),V(I+2),V(I+3),V(I+4),DSUMF,I)
      ELSE
      IF(NMETH(I).LE.0)CALL INTPIC3(I)
      DSUM=V(I)*RODSHEAR(QQ,V(I+1),DSUMF,I,LTYP(I))
      END IF
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      SUMF=SUMF+V(I)*DSUMF
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 19, CORRELATION HOLE
 1900 QR=QQ*V(I+1)
      SSQ=( 1.0 +3.0*V(I)*(QR*COS(QR)-SIN(QR))/(QR**3) )
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
      GOTO 10000
C==== MODEL 20, CONST*Q**N
 2000 DSUM= V(I)*QQ**V(I+1)
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      GOTO 10000
C==== MODEL 21, ELLIPSOID
 2100 IF(NMETH(I).LE.0)CALL INTPIC(I)
      DSUM= V(I)*ELLIPSOID(QQ,V(I+1),V(I+2),DSUMF,I)
      SUMPQ=SUMPQ+DSUM
      SUM=SUM+DSUM
      SUMF=SUMF+V(I)*DSUMF
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 22, HARD SPHERE STRUCTURE FACTOR
 2200 IF(LTYP(I).NE.11)THEN
      SSQ= HARDSPH(QQ,V(I),V(I+1))
      ELSE
      SSQ= HARD_SHARMA(QQ,V(I),V(I+1),V(I+2),V(I+3))
      END IF
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
      GOTO 10000
C==== MODEL 23, CHARGE PARTICLES HAYTER/PENFOLD ETC. ( FAGOTTI CODE)
 2300 SSQ=SOFQ(QQ,DCOEFF(1,1),COEFF(1,1))
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
      GOTO 10000
C==== MODEL 24, TWO SHELL ELLIPSOID
 2400 JJJ=LTYP(I)
      IF(NMETH(I).LE.0)CALL INTPIC(I)
      DSUM=ELLIPSHELL(QQ,V(I+1),V(I+2),V(I+3),V(I+4),DSUMF,I)
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C==== MODEL 25, SOFQ HAYTER-PENFOLD (MODEL 23) + CRITICAL SCATTERING (MODEL 7)
 2500 SSQ=SOFQ(QQ,DCOEFF(1,1),COEFF(1,1))+V(I+4)/(1.0+(V(I+5)*QQ)**2.0)
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
C      SSQ1=SOFQ(QQ,DCOEFF(1,1),COEFF(1,1)) replaced these 3 line RKH 12/2/92
C      CALL SORTOUT(I1,SUM,SUM1,SUMF,SUMX,SSQ,SSQP,PRODSQ,BETARATIO,
C     *            LM,D,DP,DF,DX)
      GOTO 10000
 2600 DA=V(I+1)
C==== calculate Gamma function GA
      G=5.-DA    !argument of Gamma function
      IF(IGAMMA.NE.1.OR.GDF(2).NE.G)THEN
        GA=GAMMA(G)
        IGAMMA=1
        GDF(2)=G
        GDF(3)=GA
C==== THIS IS DERIV OF GAMMA(5-D) DIVIDED BY GAMMA(5-D)
        GDF(4)= 10000.*( GAMMA(G+.0001) -GA )/GA
      ELSE
        GA=GDF(3)
      ENDIF
      IF(LTYP(I).NE.1)GOTO 2650
c==== calculate sine
      SS2 = SIN( (DA -1.) * 1.570796327)
c==== calculate q dependence
      SS3 = QQ**( DA - 6. )
c====
      DSUM = GA*SS2*SS3
      SUM = SUM + DSUM*V(I)
      SUMPQ = SUMPQ + DSUM*V(I)
      GOTO 10000
C2345 789012345678901234567890123456789012345678901234567890123456789012
C==== A.Allen version - see comments in DERIV
2650  DSUM=V(I)*3.141592654*1.0E-12* (V(I+3)**4) *GA* (V(I+2)**2) *
     >      ( (ABS(QQ*V(I+3)))**(DA-6.) ) * 
     >      ( (1.+1./( (QQ*V(I+3))**2) )**(0.5*(DA-5.)) )*
     >       SIN( (DA-1.)*ATAN( QQ*V(I+3) ) )
      SUM = SUM + DSUM
      SUMPQ = SUMPQ + DSUM
      GOTO 10000
C==== model 27   Lorentz + Debye-Bueche  for gelatin etc.
2700  IF(LTYP(I).NE.1)GOTO 2710
      DSUM = V(I)/(1.0+QQ2*V(I+1)**2) + 
     >      V(I+2)/((1.0+QQ2*(V(I+3)**2))**2)
      SUM = SUM + DSUM
      SUMPQ = SUMPQ + DSUM
      GOTO 10000
C==== model 27 type 11, Lorentzian for quasi-elastic scattering
2710  IF(LTYP(I).NE.11)GOTO 2720
      SUM1=V(I)*V(I+1)/(( (QQ-V(I+2))**2+0.25*V(I+1)**2)*6.283185307)
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
c==== Gaussian peak, params are height, FWHM=sqrt(2.*loge(2))sig=2.354820045sig
c==== and position  CHECK EXP(ZERO) ???  25/1/96
2720  IF(LTYP(I).NE.21.AND.LTYP(I).NE.71)GOTO 2730
      SS1=AMAX1(1.E-12,ABS(V(I+1)/2.35482004))
      IF(LTYP(I).EQ.21)THEN
      SUM1 = V(I)*EXPSPEC(-0.5*((QQ-V(I+2))/SS1)**2)
      ELSE
C==== add term for type 71 RKH 2/4/96
      SS2=QQ-V(I+2)
      SUM1 = V(I)*EXPSPEC(-0.5*(SS2/(SS1+V(I+3)*ABS(SS2)+V(I+4)*SS2))**2)
      END IF
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
2730  IF(LTYP(I).NE.31)GOTO 2740
      SS1=QQ-V(I+3)
      SUM1 = V(I)*VOIGT1( SS1,V(I+1),V(I+2))
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
2740  IF(LTYP(I).NE.41)GOTO 2750
      SS1=AMAX1(1.E-12,ABS(V(I+1)/2.35482004))
      SS2=QQ-V(I+2)
      IF(SS2.LE.0.0)THEN
      SUM1 = V(I)*EXPSPEC(-0.5*(SS2/SS1)**2)
      ELSE
C==== try exponential tail end
      SUM1 = V(I)*EXPSPEC(-SS2/AMAX1(1.E-12,V(I+3)) )
      END IF
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
2750  IF(LTYP(I).NE.51)GOTO 2760
      SS1=AMAX1(1.E-12,ABS(V(I+1)/2.35482004))
      SS4=AMAX1(1.E-12,ABS(V(I+3)))
      SS3=SS1**2/SS4
      SS2=QQ-V(I+2)
      IF(SS2.LE.SS3)THEN
      SUM1 = V(I)*EXPSPEC(-0.5*(SS2/SS1)**2)
      ELSE
C==== try exponential tail end with matched derivative
      SUM1 = V(I)*EXPSPEC(-SS2/AMAX1(1.E-12,V(I+3)) + 0.5*SS3/SS4 )
      END IF
      SUM = SUM + SUM1
      SUMPQ=SUMPQ + SUM1
      GOTO 10000
C==== Ikeda & Carpenter moderator pilse shape
2760  IF(LTYP(I).NE.61)GOTO 2780
      DSUM=V(I)*RIKCAR(QQ-V(I+2),V(I+1),V(I+3),V(I+4))
      SUM = SUM + DSUM
      SUMPQ=SUMPQ + DSUM
      GOTO 10000
2780  IF(LTYP(I).NE.81)GOTO 10000
C==== Guassian convoluted with an exponential:
      DSUM=V(I)*CRTPFN(QQ,V(I+1),V(I+2),V(I+3))
      SUM = SUM + DSUM
      SUMPQ=SUMPQ + DSUM
      GOTO 10000
C==== model 28 polydisp layer - Kotlarchyk & Ritzau
2800  IF(LTYP(I).LE.31)THEN
      DSUM= RLAYFF(LTYP(I),QQ,V(I+1),V(I+2),V(I+3),V(I+4),DSUMF)
      ELSE
      DSUM= RLAYFF2(LTYP(I),QQ,V(I+1),DSUMF)
      END IF      
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      DSUMX=V(I)
      SUMX=SUMX+V(I)
      GOTO 10000
C==== model 29 structure factor for 1d paracrystal - Kotlarchyk & Ritzau
2900  if(ltyp(i).ne.1)goto 2910
      SSQ= PARCRYS(QQ,V(I),V(I+1),V(I+2),V(I+3))
      BETARATIO= BETAFUN(SUMF,SUMX,SUMPQ)
      SSQP=1.0 + BETARATIO*(SSQ-1.0)
      PRODSQ=PRODSQ*SSQP
      SUM=SUM*SSQP
      GOTO 10000
2910  DSUM=PARA3(QQ,LTYP(I),LTYP(I+4),LTYP(I+7),V(I+1))
      SUMPQ=SUMPQ+DSUM*V(I)
      SUM=SUM+DSUM*V(I)
      SUMF=SUMF+DSUMF*V(I)
      DSUMX=V(I)
      SUMX=SUMX+V(I)
10000 CONTINUE
      RETURN
      END
C
      FUNCTION BETAFUN(SUMF,SUMX,SUMPQ)
c==== 2/11/00 add test on K8=1 to stop betaratio correction
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      BETAFUN=1.0
      IF(LS(3).EQ.1)RETURN
      IF(ABS(SUMX).GT.1.E-37.AND.ABS(SUMPQ).GT.1.0E-37)BETAFUN= 
     >                  (SUMF/SUMX)*(SUMF/SUMPQ)
      RETURN
      END
C
C		MODIFY RODS & ELLIPSES TO CALC SUMF AND USE RKH GAUSS QUADS
C
C
      FUNCTION ELLIPSOID(QQ1,R1,X1,DSUMF1,NPAR1)
C==== FORM FACTOR FOR ELLISOID RADII R:R: X*R
C==== COPYRIGHT BY RKH 6/1/89 these routines edited down from the rods
C==== one, so beware of identical arrays in common blocks - though
C==== they should never see each other
C====  SEE  B.Sjoberg, J.Appl.Cryst. 11(1978)73-79
c==== use NAG Gaussian quadratures etc., which need
c==== a single function passed through the call
      EXTERNAL ELLF2
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
c      COMMON/ELLS/NGPTELL,BETA(401),WTBETA(2001),IWORK(300),
c     *             NGAUSS(6),DELRELL,DELXELL
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
C      DATA DELRELL/.05/,DELXELL/.005/
      BET1=0.0
c=== this is pi/2.0
      BET2=PIBY2
      QQ=QQ1
      R=R1
      QR1=QQ1*R1
      XELL=X1
      KOUNT=0
      NPAR=NPAR1
      METH=NMETH(NPAR)
      IF1ELL=0
      CALL INTEGR1(METH,NPAR,ELLF2,BET1,BET2,EPSAELL,EPSRELL,
     > SUM,SUM1,ABSERR,IFAIL)
      ELLIPSOID=SUM
      KOUNT1=KOUNT
      ABSERR1=ABSERR
C==== some own methods ( i.e.not NAG) compute F(Q)**2 and F(Q)together, 
c==== passing F(Q) through common TO INTEGR1( )
      IF(NMETH(NPAR).LE.NSIMPLE)THEN
      DSUMF1=SUM1
      KOUNT=-KOUNT
C====
      ELSE
      DSUMF1=0.0
      KOUNT=0
      ABSERR=0.0
      IF(NEEDF1.EQ.1)THEN
C==== EXPLICIT CALC OF F(Q) NEEDED
      IF1ELL=1
      CALL INTEGR1(METH,NPAR,ELLF2,BET1,BET2,EPSAELL,EPSRELL,
     > SUM1,SUM,ABSERR,IFAIL)
      DSUMF1=SUM1
      END IF
      END IF
      RETURN
      END
C
C
      FUNCTION ELLF2(BETA)
C==== special version for NAG routines to do integral over beta
C==== FORM FACTOR F(Q)**2 FOR ELLIPSOID RADII R:R:X*R
C==== this cab be simplified to an integral over 0 to 1 by
C==== change of variable, but then will be harder to change to
C==== general ellipsoid etc.
C==== COPYRIGHT BY RKH 6/1/89
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      SB=SIN(BETA)
      U=QR1*SQRT(SB**2 + (XELL*COS(BETA))**2)
         IF(ABS(U).GT.1.0E-12)THEN
           PHIU=3.0*(SIN(U) -U*COS(U) )/ (U**3)
         ELSE
           PHIU=1.0
         END IF

      DSUM= SB*PHIU
      IF(IF1ELL.EQ.1)THEN
C==== compute f(q) sin(beta)
      ELLF2=DSUM
      ELSE
C==== compute F(Q)**2 sin(beta)  but pass F(Q)sin(beta) back through common
      ELLF2= DSUM*PHIU
      END IF
C====
      KOUNT=KOUNT+1
      RETURN
      END
C
      FUNCTION RODFF(QQ1,R2,RL1,DELR1,CONT1,DSUMF1,NPAR1)
C==== FORM FACTOR FOR RODS,OUTER RADIUS R, LENGTH RL  (NOT HALF LENGTH AS IN MOST 
C====  FORMULAE), INNER RADIUS R-DELR, CONT = (RHO.INNER - RHO.OUTER)/RHO.OUTER
C==== COPYRIGHT BY RKH 29/7/88
c==== modified 17/10/88 to use NAG Gaussian quadratures etc., which need
c==== a single function passed through the call
c==== changed 11/3/93 RKH, to have thin shell on ENDS as well as sides of rod.
c==== MAJOR rewrite 20/5/96
      EXTERNAL RODF2
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
c      COMMON/RODS/NGPTROD,BETA(401),WTBETA(2001),IWORK(300),
c     *             NGAUSS(6),DELROD
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      BET1=0.0
c=== this is pi/2.0
      BET2=PIBY2
      QQ=QQ1
      NPAR=NPAR1
      R=R2
      RL=ABS(RL1)
      DELR=DELR1
      DELRCAP=DELR1
C==== 6/6/95 allow for no end cap case 18 11 on first card of model.   RKH
      IF(LTYP(NPAR).EQ.11)DELRCAP=0.0
      CONT=CONT1
      QR1=QQ*(R-DELR)
      QL1=QQ*(RL*0.5-DELRCAP)
      QR2=QQ*R
      QL2=QQ*RL*0.5
      V2=SQRT(PI*R*R*RL)
      V1=CONT*PI*((R-DELR)**2)*(RL-2.*DELRCAP)/V2
c==== change the normalisation so that P(Q)=1/(N*V2*RHO(2-3)**2) at Q=0 ( I hope)
      KOUNT=0
      METH=NMETH(NPAR)
      IF1ROD=0
      CALL INTEGR1(METH,NPAR,RODF2,BET1,BET2,EPSAROD,EPSRROD,
     > SUM,SUM1,ABSERR,IFAIL)
      RODFF=SUM
      KOUNT1=KOUNT
      ABSERR1=ABSERR
C==== some own methods ( i.e.not NAG) compute F(Q)**2 and F(Q)together, 
c==== passing F(Q) through common TO INTEGR1( )
      IF(NMETH(NPAR).LE.NSIMPLE)THEN
      DSUMF1=SUM1
      KOUNT=-KOUNT
c====
      ELSE
      DSUMF1=0.0
      KOUNT=0
      ABSERR=0.0
      IF(NEEDF1.EQ.1)THEN
C==== EXPLICIT CALC OF F(Q) NEEDED
      IF1ROD=1
      CALL INTEGR1(METH,NPAR,RODF2,BET1,BET2,EPSAROD,EPSRROD,
     > SUM1,SUM,ABSERR,IFAIL)
      DSUMF1=SUM1
      END IF
      END IF
      RETURN
      END
C
      FUNCTION RODF2(THETA1)
C==== FORM FACTOR F(Q)**2 FOR RODS,OUTER RADIUS R, LENGTH RL  (NOT HALF LENGTH AS IN MOST 
C====  FORMULAE), INNER RADIUS R-DELR, CONT = (RHO.INNER - RHO.OUTER)/RHO.OUTER
C==== COPYRIGHT BY RKH 29/7/88
c==== changed 11/3/93 RKH, to have thin shell on ENDS as well as sides of rod.
C====  UNLESS 6/6/95 LTYP(NPAR)=11 on first card of model !
c==== lots more changes, including adsorbing RODF1, 20/5/96 RKH
c
      INCLUDE 'FISHDIM.PAR'
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      STH=SIN(THETA1)
      CTH=COS(THETA1)
      QBS=QR2*STH
      QHC=QL2*CTH
C==== USE NAG LIBRARY ROUTINE S17AFF FOR BESSEL FUNCTION J1(X)  ( S17AEF IS J0(X) )
c==== removed 29/3/93 RKH
C==== IFAIL=1 FOR X TOO LARGE
C      IFAILB=0
C====          trap potential div by zero if THETA1=0.0 or Q=0.0
C==== outer cylinder, N.B.  V2 is now SQRT(V2) !
      DSUM=V2*TRJ1XBYX(QBS)
      IF(ABS(QHC).GT.1.E-12)DSUM=DSUM*SIN(QHC)/QHC

C==== inner cylinder  N.B. V1 is now V1/SQRT(V2)
      IF(ABS(CONT).GT.1.E-12)THEN
      QAS=QR1*STH
      QHC1=QL1*CTH

      DSUM1= V1*TRJ1XBYX(QAS)
      IF(ABS(QHC1).GT.1.E-12)DSUM1=DSUM1*SIN(QHC1)/QHC1
      ELSE
      DSUM1=0.0
      END IF

C==== add the two terms
      DSUM=DSUM+DSUM1
      IF(IF1ROD.EQ.1)THEN
C==== compute f(q) sin(THETA1)
c==== 1/11/00 remove /V2
      DSUM= STH*DSUM
      RODF2=DSUM
      ELSE
C==== compute F(Q)**2 sin(THETA1)  but pass F(Q)sin(THETA1) back through common
      RODF2= STH*(DSUM**2)
c==== 1/11/00 oops had bug since 20/5/96 ??? remove /V2
      DSUM=DSUM*STH
      END IF
C====
C      IF(IFAILB.NE.0)WRITE(JS,1001)IFAIL,QQ,R,STH
C1001  FORMAT(1X,'FUNCTION RODF2,  BESSEL IFAIL=',I3,'  QQ,R,SB=',1P3E12.4)
C====
      KOUNT=KOUNT+1
      RETURN
      END
C
C      FUNCTION FUNJ1S(X) REPLACED BY IMPROVED ROUTINE
C
      FUNCTION TRJ1XBYX(X)
C==== SINGLE PRECISION 2.0*(J1(x)/x) first order Bessel function of the first kind
C====   DIVIDED by X
C
C==== lifted from book numerical recipes in C (bother no FORTRAN
c==== version in RAL computing group room !)   R.K.Heenan 26/3/93
C==== Comparison with the NAG routine show a max difference up to  a few %
c==== on passing through X=7, as if there were some kind of 1/(x-7) 
c==== singularity.  
C==== The simple series expansion show that this routine has the fault !
c==== so THIS VERSION USES THE SERIES ONE FOR X=6 to X= 8
c
c==== NOTE FISH requires a wide range of values of X from ca .01 to 1000.
c==== The difference between this routine and a double precision version
c==== was within 0.14% on ISISE VAX
C==== NOTE  J1(-x)/(-x) = -J1(x)/(-x) = J1(x)/x
      AX=ABS(X)
      IF(AX.LT.1.E-12)THEN
C==== This small X cut off point not rigourously tested:
      TRJ1XBYX=1.0
      ELSE IF(AX.LT.6.0)THEN
      Y=X*X
      A1=72362614232.0+Y*(-7895059235.0+Y*(242395853.1+
     >   Y*(-2972611.439+Y*(15704.48260+Y*(-30.16036606)))))
      A2=144725228442.0+Y*(2300535178.0+Y*(18583304.74+
     >   Y*(99447.43394+Y*(376.9991397+Y))))
      TRJ1XBYX=2.0*A1/A2
c====    RJ1X= X*A1/A2
c
      ELSE IF(AX.LT.8)THEN
C==== use the series expansion here
      TRJ1XBYX=2.0*BESSEL(1,X)/X
C====    RJ1X=BESSEL(1,X)
c
      ELSE
      Z=8.0/AX
      Y=Z*Z
      XX=AX-2.356194491
      A1=1.0+Y*(0.00183105+Y*(-0.3516396496E-04+
     >    Y*(0.24575520174E-05+Y*(-0.240337019E-06))))
      A2=0.04687499995+Y*(-0.2002690873E-03+
     >   Y*(0.8449199096E-05+Y*(-0.88228987E-06+
     >   Y*0.105787412E-06)))
      TRJ1XBYX=2.0*SQRT(0.636619772/AX)*(COS(XX)*A1 -Z*SIN(XX)*A2)/AX
C====    RJ1X=SQRT(0.636619772/AX)*(COS(XX)*A1 -Z*SIN(XX)*A2)*SIGN(1.,X)
      END IF
      RETURN
      END
c
      function bessel(n,z)
c==== simple series summation integer n, here ONLY for n=0 or 1
c==== unless add a factorial loop or function to start off dsum below.
c==== works well for n=1  (agrees with NAG), checked to z=1000.
c==== for n=0 starts to get bad z>10 ( errors >0.0001), 
c==== VERY bad z>15 when does not converge properly.
      data nmax/32000/,tol/1.e-12/
      real z
      integer n,k
      front=1.0
      if(n.ge.1)front=(0.5*z)**n
      sum=front
      top=-0.25*z*z
      if(n.eq.0)dsum=front*top
      if(n.eq.1)dsum=front*0.5*top
c==== in general start at dsum= front*top/ (n+1)!
      k=1
10    sum=sum+dsum
      k=k+1
      dsum=dsum*( top/(float(k)*float(k+n)) )
      if(abs(dsum).gt.tol.and.k.le.nmax)goto 10
      if(k.gt.nmax)write(6,101)z,dsum
101   format(1x,'nmax iterations exceeded at z=',1pe10.3,' dsum= ',e10.3)
      bessel=sum
      return
      end      
C
      FUNCTION HARDSPH(QQ,ETA,SIGMA)
C==== structure factor for hard sphere - Percus Yevick equations
C==== as solved by N.W.Ashcroft & J.Lekner Phys.Rev. 145(1966)83
C
C==== NOTE in original paper SIGMA is diameter of spheres, here it
C==== is radius.   SET SIGMA=0 TO SKIP OUT
C
      HARDSPH=1.0
      IF(SIGMA.LE.1.E-12)RETURN
      D=(1.-ETA)**4
      A=(1.+2.*ETA)**2/D
c
      IF(QQ.LT.1.E-12)THEN
          HARDSPH=1./A
      ELSE
      B=-6.*ETA*(1.+0.5*ETA)**2/D
      G=0.5*ETA*A
      X=QQ*SIGMA*2.0
      X2=X*X
      X4=X2*X2
      S=SIN(X)
      C=COS(X)
      FF=A*(S-X*C)/X + B*(2.*X*S -(X2-2.)*C -2.)/X2 +
C 
     *   G*( (4.*X*X2 -24.*X)*S -(X4 -12.*X2 +24.)*C +24. )/X4
C
      HARDSPH= 1./(1. + 24.*ETA*FF/X2 )
      ENDIF
      RETURN
      END
c
      FUNCTION HARD_SHARMA(QQ,ETA,SIGMA,EPS,RLAM)
C==== structure factor for hard sphere - Percus Yevick equations
C==== as solved by N.W.Ashcroft & J.Lekner Phys.Rev. 145(1966)83
C==== 8/2/2 version by R.V.Sharma & K.C.Sharma, Physica A 89(1977)213-218
c==== for square potn of HEIGHT epsilon (in units kT) extending to lambda 
c==== times the diameter (lambda say 1.2 - 1.7)
c==== 13/2/2 i.e. -ve eps/kT is attractive, +ve is repulsive
c==== (to be same as charged sphere case), checked o/p against indep calcs
c==== by R.Triolo
C
c==== note they have extra higher order terms in alpha,beta & gamma
c==== than Ashcroft & Lekner, with which they else wise agree.
c====
C==== NOTE in original paper SIGMA is diameter of spheres, here it
C==== is radius.   SET SIGMA=0 TO SKIP OUT
C
      HARD_SHARMA=1.0
      IF(SIGMA.LE.1.E-12)RETURN
      D=(1.-ETA)**4
      A=( (1.+2.*ETA)**2 +ETA**3*(ETA-4.)   )/D
c
c      IF(QQ.LT.1.E-12)THEN
c          HARD_SHARMA=1./A
c      ELSE
      B=-(ETA/3.0)*(18.+ 20.*ETA -12.*ETA**2 +ETA**4)/D
      G=0.5*ETA*A
      X=QQ*SIGMA*2.0
      X2=X*X
      X4=X2*X2
      S=SIN(X)
      C=COS(X)
      RLX=RLAM*X
      FF=A*(S-X*C)/X + B*(2.*X*S -(X2-2.)*C -2.)/X2 +
C 
     >   G*( (4.*X*X2 -24.*X)*S -(X4 -12.*X2 +24.)*C +24. )/X4 +
C
     >   (EPS/X)*( SIN(RLX) -RLX*COS(RLX) +X*C -S)
C
      HARD_SHARMA= 1./(1. + 24.*ETA*FF/X2 )
c      ENDIF
      RETURN
      END
c
	function rlognorm(r,sig,rbar)
c...	this subroutine calculates the log-normal distribution.
c====    RKH  16/5/89,    0.3989... = 1./sqrt(2*pi)
c==== oops the sigmas got muddled, corrected 19/8/89
c====   assumes ( check) rbar= exp( mu - 0.5*sigma**2)
      rlognorm=0.0
      if(rbar.lt.1.e-33.OR.r.lt.1.e-33)return

      sig2 = alog( (sig/rbar)**2 +1.0 )
      if(sig2.lt.1.e-33)return

      rmu = alog(rbar) - 0.5*sig2

C==== expspec avoids underflows
      rlognorm=expspec(-0.5*(log(r) - rmu)**2/sig2 )*
     *                   0.3989422803/(sqrt(sig2)*r)
      return
      end

	function SCHULTZ(r,sigma,rbar)
c...	this subroutine calculates the SCHULTZ distribution.
c...	originally by W.I.F.David, corrected and modified by RKH 27/1/89
c
	anorm= 1./rbar
	x= anorm*r
	s= anorm*sigma
	z= 1./(s*s) - 1.
c
	if (x.le.0.0)then
		SCHULTZ= 0.
		if (z .eq. 0.) SCHULTZ= anorm
        else
c====           take logs of everything to give max possible dynamic range !
		v= z+1.
c
	   if (v.le.34)then
c====       v< 34, gamma function should work
	    alntem= v*alog(v) +z*alog(x)-x*v +alog(anorm)-alog(GAMMA(v))
	   else
c====          use asymptotic expansion of gamma for large numbers
c===           for which some terms cancel
		vv= v*v
		den= 1. + 1./(12.*v) + 1./(288.*vv) -
     +			139./(51840.*v*vv) - 571./(2488320.*vv*vv)
		alntem= z*alog(x) +(1.-x)*v + alog(anorm) +0.5*alog(v) 
     *                      + alog(0.3989422803/den)
	   endif
		if(alntem.lt.-88.5)then
			SCHULTZ=0.0
		else if(alntem.gt.84)then
			write(6,1001)r,sigma,rbar,x,z
1001	format(1x,'overflow in Schultz',/,' r,sigma,rbar,x,z=',1p5e12.3)
		        SCHULTZ=1.e37
		else
			SCHULTZ= exp( alntem )
		endif
	endif
	return
	end

      function GAMMA(xx)
c...  This program was taken from [WIFD.HARWELL]VB01.FOR and
c     modified to accomodate VAX overflow limits.
c     W.I.F.David 5-JUN-84   
c       see comments on usage in DERIV() of FISHMODEL
      if(xx-34.)6,6,4	!!now 34. was 57.
  4   ier=2
      gamma=8.8e36	!!was 1.e38 now 8.8.e36
      return
6     x=xx
      err=1.0e-6
      ier=0
      gamma=1.
      if(x-2.0)50,50,15
10    if(x-2.0)110,110,15
15    x=x-1.0
      gamma=gamma*x
      goto 10
50    if(x-1.0)60,120,110
60    if(x-err)62,62,80
62    y=float(int(x))-x
      if(abs(y)-err)130,130,64
64    if(1.0-y-err)130,130,70
70    if(x-1.0)80,80,110
80    gamma=gamma/x
      x=x+1.
      goto 70
110   y=x-1.
      gy=1.0+y*(-0.5771017+y*(0.9858540+y*(-0.8764218+y*(0.8328212+
     1 y*(-0.5684729+y*(0.2548205+y*(-0.05149930)))))))
      gamma=gamma*gy
120   return
130   ier=1
      return
      end
c
c
      FUNCTION ELLIPSHELL(QQ1,VV11,VV21,VV31,VV41,DSUMF1,NPAR1)
C==== FORM FACTOR FOR ELLIPSOID WITH SHELL RADII R : R : X*R
C==== THESE ROUTINES EDITED DOWN FROM THE ROD AND ELLIPSOID ONES
C==== USE NAG GAUSSIAN QUADRATURE         23/2/89
      EXTERNAL ELLSH2
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
c      COMMON/ELLSHELL/NGPTELL,BETA(401),WTBETA(2001),IWORK(300),
c     *             NGAUSS(6),DELRELLSH,DELXELLSH,DELDELTA,DELCONTR
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      COMMON/MODEL24/JJJ
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
C      DATA DELRELLSH/.05/,DELXELLSH/.005/,DELDELTA/.005/,DELCONTR/0.5/
      BET1=0.0
c=== this is pi/2.0
      BET2=PIBY2
      QQ=QQ1
      VV1=VV11
      VV2=VV21
      VV3=VV31
      VV4=VV41
      KOUNT=0
      NPAR=NPAR1
      METH=NMETH(NPAR)
      IF1ELL=0
      CALL INTEGR1(METH,NPAR,ELLSH2,BET1,BET2,EPSAESH,EPSRESH,
     > SUM,SUM1,ABSERR,IFAIL)
      ELLIPSHELL=SUM
      KOUNT1=KOUNT
      ABSERR1=ABSERR
C==== some own methods ( i.e.not NAG) compute F(Q)**2 and F(Q)together, 
c==== passing F(Q) through common TO INTEGR1( )
      IF(NMETH(NPAR).LE.NSIMPLE)THEN
      DSUMF1=SUM1
      KOUNT=-KOUNT
C====
      ELSE
      DSUMF1=0.0
      KOUNT=0
      ABSERR=0.0
      IF(NEEDF1.EQ.1)THEN
C==== EXPLICIT CALC OF F(Q) NEEDED
      IF1ELL=1
      CALL INTEGR1(METH,NPAR,ELLSH2,BET1,BET2,EPSAESH,EPSRESH,
     > SUM1,SUM,ABSERR,IFAIL)
      DSUMF1=SUM1
      END IF
      END IF
      RETURN
      END
C
	FUNCTION ELLSH2(BETA)
C==== special version for NAG routines to do integral over beta
C==== FORM FACTOR F(Q)**2 FOR ELLIPSOID with shell RADII R:R:X*R
C==== this cab be simplified to an integral over 0 to 1 by
C==== change of variable, but then will be harder to change to
C==== general ellipsoid etc.
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      COMMON/MODEL24/JJJ
	SB=SIN(BETA)
	CB=COS(BETA)

	IF(JJJ.EQ.1)GOTO 1
	IF(JJJ.EQ.11)GOTO 11
	IF(JJJ.EQ.21)CALL AGGRE(CB,SB,UU1,UU2,CONST)
	IF(JJJ.EQ.31)CALL AGGRE(CB,SB,UU1,UU2,CONST)
	GOTO100
	
 1	R=VV1
	X=VV2
	RE=VV3
	CONST=VV4
	ee=SQRT(SB**2.0+(X*CB)**2.0)
	UU1=QQ*R*ee
	UU2=QQ*re*ee
	GOTO 100	

 11	R=VV1
	X=VV2
	RDEL=VV3
	CONST=VV4
	ADD=R+RDEL
	ADX=R*X+RDEL
	UU1=QQ*R*SQRT(SB**2.0+(X*CB)**2.0)
	UU2=QQ*SQRT((SB*ADD)**2.0+(ADX*CB)**2.0)

 100	IF(ABS(UU2).GT.1.0E-24)THEN
	PPUU2=3.0*(SIN(UU2)-UU2*COS(UU2))/UU2**3
	SUM =PPUU2
	ELSE
	SUM =1.0
	ENDIF
	IF(ABS(UU1).GT.1.0E-24)THEN
	PPUU1=3.0*(SIN(UU1)-UU1*COS(UU1))/UU1**3
	SUM =SUM +CONST*PPUU1
	ELSE
	SUM =SUM +CONST
	ENDIF

      DSUM = SB*SUM
      ELLSH2 = DSUM
c==== 29/11/96 this always gave F**2, replaced IF1ESH by IF1ELL
C==== IF1ESH is redundant !
      IF(IF1ELL.EQ.0)ELLSH2=ELLSH2*SUM
C====
      KOUNT=KOUNT+1
      RETURN
      END
C
	subroutine AGGRE(CB,SB,UU1,UU2,CONST)
C
C   1/3/89                Complains have to be addressed to Cesare!!!!
C
C   This subroutine for ltyp(1)=21,31  MODEL 24 
C   It changes fitting parameters for the ellipsoid with shell
C   It is suppose to be use for micellar systems in water, but it can
C   handle even problems in which an hydrocarbon is solubilazed in the 
C   micelles. 
C   The parameters became : 1) scale 2) aggregation number 3) axial ratio
C   4) charge  5)fraction of the hydrocarbon chain in the inner core
C   Parameters 4 and 5 can be easily changed to suit your paricular problem.
C   It uses a simple geometrical model to calculate the ellipsoidal parameters
C   (like the axis or the shell thickness, and the scattering length densities)
C   and the passes them to the subroutine ellsh2 for the 
C   integration. It needs 24 constants. These are:
C   1)spare (for lambda)                
C   2)volume CH3 group in the chain  A**3
C   3)sum of the scattering lenghts (sosl) of the CH3 group CM*1.E12
C   4)Head group volume           A**3
C   5)sosl of the head group        CM*1.E12
C   6)hydration number of the head group 
C   7)Counterion volume         A**3
C   8)sosl of the counterion    CM*1.E12
C   9)hydration number of the counterion
C  10)solvent volume        A**3
C  11)sols solvent          CM*1.E12
C  12)sld solvent            A-2
C  13)[c-cmc]               mol*l-1
C  14)Na*1e-19=6.0231*1.0e+4
C  15)Volume of the hydrocarbon chain minus the CH3 group A**3
C  16)sols of the hydrocarbon chain minus the CH3 group CM*1.E12
C  17)calculated from the program::   absolute scale CM-1
C  18)calculated from the program::   inner axis A
C  19)calculated from the program::   outer axis A
C  20)calculated from the program::   sld of the core A-2
C  21)calculated from the program::   sld of the outer shell A-2
C  22)[c] oil dissoved in the micelles  mol*l-1
C  23)Oil volume           A**3
C  24)sols oil CM*1.E12
C
C  for example the following are the constants needed for a pure micellar 
C  system of SDS (C12H250SO3Na) in D2O at a concentration of .165 mol*l-1  
C
C1.000E+00 5.430E+01-4.570E-01 6.000E+01 2.600E+00 6.000E+00 1.360E+01 3.620E-01
C6.000E+00 3.020E+01 1.920E+00 6.390E-06 1.650E-01 6.023E+04 2.720E+02-9.130E-01
C0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
C
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      COMMON/MODEL24/JJJ
	REAL*4 AA(4),ROOT(3)
	c=0.6204
c===== 3/(4pi) = cc
	CC=0.238732414

	AGG=VV1
	X=VV2
	QAZ=VV3
	ALF=VV4

	V1=(CON(15)*ALF+CON(2)+CON(22)/CON(13)*CON(23))*AGG
	VT=(CON(2)+CON(15)+CON(4)+CON(7)+CON(22)/CON(13)*CON(23)
     *  +CON(10)*(CON(6)+CON(9)))*AGG-(CON(7)+CON(10)*CON(9))*QAZ
	V2=VT-V1
	R=C*(V1/X)**0.33333
	B1=(CON(3)+CON(16)*ALF+CON(22)/CON(13)*CON(24))*AGG
	RHO1=B1*1.0E-4/V1
	B2=(CON(16)*(1.0-ALF)+CON(5)+CON(8)+CON(11)*(CON(6)+CON(9)))*AGG
     *  -(CON(8)+CON(11)*CON(9))*QAZ
	RHO2=B2*1.0E-4/V2
	CON(17)=((RHO2-CON(12))*VT)**2.0*CON(13)*CON(14)/AGG
	CON(18)=r
	CON(20)=RHO1
	CON(21)=RHO2

	IF(JJJ.EQ.21)THEN
	RE=C*(VT/X)**0.33333
	ee=SQRT(SB**2.0+(X*CB)**2.0)
	UU1=QQ*R*ee
	UU2=QQ*re*ee
	CON(19)=re
	CONST=(RHO1-RHO2)/(RHO2-CON(12))*(R/RE)**3.0
	ELSE
	AA(1)=CC*V2
	AA(2)=-(2.0*X+1.0)*R**2
	AA(3)=-(X+2.0)*R
	AA(4)=-1.0
	CALL PA03A(AA,ROOT,NREAL)
	DO 10 I=1,NREAL
	IF(ROOT(I).GT.0.0)GOTO 20
 10 	CONTINUE
 20	RE=ROOT(I)
	CON(19)=re
	ADD=R+RE
	ADX=R*X+RE
	CONST=(RHO1-RHO2)/(RHO2-CON(12))*R**3.0*X/(ADD**2.0*ADX)
	UU1=QQ*R*SQRT(SB**2.0+(X*CB)**2.0)
	UU2=QQ*SQRT((SB*ADD)**2.0+(ADX*CB)**2.0)
	ENDIF
	return
	end
C
C THIS CALCULATES THE ROOTS FOR A CUBIC. USED IN MODEL 24 LTYP(31)
C#       PA03A          12/09/80
CNAME PA03A(R)
      SUBROUTINE PA03A(AA,R,N)
C  STANDARD FORTRAN 66(A VERIFIED PFORT SUBROUTINE)
C**************************************************************
C*  PURPOSE....                                               *
C*  TO FIND THE ROOTS OF THE REAL CUBIC ......                *
C*          A(4)*X**3+A(3)*X**2+A(2)*X+A(1)                   *
C*                                                            *
C*  ARGUMENT LIST....                                         *
C*  AA  IS A REAL ARRAY OF LENGTH 4 WHICH WILL CONTAIN THE    *
C*      COEFFICIENTS A.                                       *
C*  R   IS A REAL ARRAY WHICH WILL HAVE ITS COMPONENTS        *
C*      SET TO THE ROOTS.IF THERE ARE THREE REAL ROOTS,THEN   *
C*      R(1).LE.R(2).LE.R(3). FOR ONE REAL ROOT,R(1) IS SET   *
C*      TO IT,R(2) IS SET TO THE REAL PART OF BOTH COMPLEX    *
C*      ROOTS AND R(3) IS SET TO THE IMAGINARY PART WHICH IS  *
C*      IS POSITIVE.THE DUMMY VALUE OF 1D+70 IS RETURNED FOR  *
C*      EACH INFINITE ROOT.THIS CORRESPONDS TO A ZERO         *
C*      LEADING COEFFICIENT.                                  *
C*  N   IS AN INTEGER WHOSE VALUE WILL BE SET BY THE ROUTINE  *
C*      TO THE NUMBER OF REAL ROOTS.                          *
C**************************************************************
      DIMENSION A(3),R(3),B(2),AA(4)
      DATA   C1/1./,C5/.5/,C3/3./,C0/0./,C4/4./
      DATA   C23/0.666666666666666667/,XINF/1.E+35/
C
C     IS LEADING COEFFICIENT ZERO,IE INFINITE ROOT.
      IF(AA(4).EQ.C0)GO TO 80
      IF(AA(1).EQ.C0)GOTO70
      A(3)=AA(3)/(C3*AA(4))
      A(2)=AA(2)/(C3*AA(4))
      A(1)=AA(1)/AA(4)
      X=A(2)-A(3)*A(3)
      Y=A(1)-A(3)*(X+X+A(2))
      Z=Y**2+C4*X**3
      IF(Z.GE.C0)GOTO100
C
C     THERE ARE THREE REAL ROOTS.
      N=3
      R(1)=-2.0*SQRT(-X)
      Y=Y/(R(1)*X)
      X=R(1)
      Y =ATAN2(SQRT(C1-Y),SQRT(C1+Y))*C23
      IF(A(3).LT.C0)Y=Y+2.094395102393195
C
C     CALCULATE ROOT WHICH DOES NOT INVOLVE CANCELLATION
      R(1)=X*COS(Y)-A(3)
C
C     DEFLATE CUBIC FROM OPTIMAL END.
   10 B(1)=-A(1)/R(1)
      B(2)=(B(1)-C3*A(2))/R(1)
      IF(ABS(R(1)**3).LE.ABS(A(1)))B(2)=R(1)+C3*A(3)
   20 X=B(2)*B(2)-C4*B(1)
C
C     IS THE PAIR OF ROOTS REAL OR COMPLEX.
      IF(X.LT.C0)GOTO 60
      R(3)=-SIGN(C5,B(2))*(SQRT(X)+ABS(B(2)))
      R(2)=C0
      IF(R(3).NE.C0)R(2)=B(1)/R(3)
      IF(R(1).LE.R(2))GOTO30
      TA=R(2)
      R(2)=R(1)
      R(1)=TA
   30 IF(R(2).LE.R(3))GO TO 50
      TA=R(3)
      IF(R(1).LE.R(3))GO TO 40
      TA=R(1)
      R(1)=R(3)
   40 R(3)=R(2)
      R(2)=TA
   50 N=3
      GO TO 150
   60 R(2)=-C5*B(2)
      R(3)=C5*SQRT(-X)
      N=1
      GO TO 150
   70 R(1)=C0
      B(1)=AA(2)/AA(4)
      B(2)=AA(3)/AA(4)
      GO TO 20
C
C     THE CUBIC HAS LEADING COEFFICIENT ZERO,IE QUADRATIC
   80 R(1)=XINF
      IF(AA(3).EQ.C0)GO TO 90
      B(1)=AA(1)/AA(3)
      B(2)=AA(2)/AA(3)
      GO TO 20
C
C     CUBIC HAS FIRST TWO LEADING COEFFICIENTS ZERO
   90 IF(AA(2).NE.C0)R(1)=-AA(1)/AA(2)
      R(2)=XINF
      R(3)=XINF
      N=3
      GO TO 150
C
C     THERE IS ONE REAL ROOT.
  100 N=1
      TA=SQRT(Z)
      TB=(ABS(Y)+TA)*C5
      TC=TB**0.333333333333333333
      IF(TC.GT.0.0)GO TO 110
      R(1)=-A(3)
      R(2)=-A(3)
      R(3)=-A(3)
      N=3
      GO TO 150
  110 TC=TC-(TC**3-TB)/(C3*TC*TC)
      TE=TC*TC+ABS(X)
      TF=C1/((X/TC)**2+TE)
      IF(X.LT.C0)GOTO 120
      X=TE/TC
      Z=Y*TF
      GOTO130
  120 X=TA*TF
      Z=SIGN(C1,Y)*TE/TC
  130 IF(Z*A(3).LT.C0)GOTO 140
      R(1)=-Z-A(3)
      GO TO 10
  140 R(2)=C5*Z-A(3)
      R(3)=C5*SQRT(C3)*ABS(X)
      R(1)=-A(1)/(R(2)*R(2)+R(3)*R(3))
  150 RETURN
      END
C
      FUNCTION RLAYFF(ITYP,QQ,TT,RLBAR,GL,RSIG,F)
C==== FORM FACTOR FOR POLYDISPERSE SHEET - Kotlarchyk & Ritzau, 
C====   J.Appl.Cryst. 24(1991)753-758,  eqns (16) & (17) with 1/Q**2
C====  included as per eqn (13)          12/2/92 RKH
C====  interfacial thickness TT = (2*pi)**0.5 * SIG    CORRECTED 7/4/92
C==== GL = SIGMA(L)/LBAR = (Z+1)**-0.5
C==== 12/3/93  if ITYP=1 then normal route,
c====    "     if ITYP=11 then omit Lorentz term (same as putting RSIG 
c====                       very large )
C==== 16/3/93 add extra parameter RSIG to model, as Lorentz factor,
c==== see Skipper et.al. J.Chem.Phys 94(1991)5751-5760
c==== 16/3/93  if LTYP=21 then use 2 shell model of Kotlarchyk, Sheu
c==== & Capel, Phys Rev A 46(1992)928-939)
c====         params are LW, (rhoH-rhoO)/(rhoW-rhoO), LH, RSIG
c
c==== 18/3/91 if ITYP=31 use core + exponential shell model of RKH
C====         set contrast zero to ignore this shell, avoid 1/KAPPA=zero !
c====         params are LW, (rhoH-rhoO)/(rhoW-rhoO), 1/KAPPA, RSIG
c
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
C
      IF(ITYP.LT.21)THEN
C
      ZP1=1.0/GL**2
      Z=ZP1-1.0
      ZM1=Z-1.0      
      AL=ZP1/(QQ*RLBAR)
      AL2=2.0*AL
c==== 7/4/92 SIG not GL in line below
C==== 23/4/98 WHY did this have *RLBAR in here ???
C====      Q2S2= -(QQ*TT*0.3989422803*RLBAR)**2
      Q2S2= -(QQ*TT*0.3989422803)**2
C
C====      F=(AL2**ZP1)*SIN(Z*ATAN(1.0/AL2))*EXP(0.5*Q2S2)/
C====     >  (Z* ( ( AL2**2 +1)**(0.5*Z) )*QQ )
C==== rearrange in attempt to avoid overflows ( note AL can be 100, Z say 25 )

      F= (1.0 + 1.0/AL2**2)**(-0.5*ZP1)*SQRT( AL2**2 +1)*
     >    SIN(Z*ATAN(1.0/AL2))*EXPSPEC(0.5*Q2S2)/Z

      IF(ITYP.EQ.1) F=F/SQRT(1.0 + 0.5*(QQ*RSIG)**2)

C2345 789012345678901234567890123456789012345678901234567890123456789012
C
C====      RLAYFF= (AL2**ZP1)*( (AL2**(1.0-Z)) -((AL2**2 +4.0)**(-0.5*ZM1))*
C====     >   COS( ZM1*ATAN(1.0/AL) ) )*EXP(Q2S2)/ (2.0*Z*ZM1*QQ*QQ)

C==== rearrange to avoid overflows ( note AL can be 100, Z say 25 )
C==== included an extra factor of 4.0 to get beta(Q) to go to 1.0 at low
C==== Q and P(Q) to go to 1.0/Q**2
      RLAYFF= 4.0*(AL**2)*( 1.0 -((1.0 +4.0/(AL2**2) )**(-0.5*ZM1))*
     >   COS( ZM1*ATAN(1.0/AL) ) )*EXPSPEC(Q2S2)/ (2.0*Z*ZM1)
      IF(ITYP.EQ.1) RLAYFF=RLAYFF/(1.0 + 0.5*(QQ*RSIG)**2)

      RETURN
C
      ELSE IF(ITYP.EQ.21)THEN
C==== assume ITYP=21
      WL=TT
      RHRW=RLBAR
      HL=GL
      QWL=0.5*WL*QQ
      QHL=0.5*HL*QQ
      IF(QWL.GT.1.E-12)THEN
      SQWL=SIN(QWL)/QWL
      ELSE
      SQWL=1.0
      END IF

      IF(QHL.GT.1.E-12)THEN
      SQHL=SIN(QHL)/QHL
      ELSE
      SQHL=1.0
      END IF

      F= (WL*SQWL +2.*RHRW*HL*SQHL*COS( QWL + QHL ) )/
     >      SQRT(1.0 + 0.5*(QQ*RSIG)**2)
      RLAYFF = F*F

      RETURN
C
      ELSE IF(ITYP.EQ.31)THEN
C==== core plus exponential shell
      WL=TT
      RHRW=RLBAR
      QWL=0.5*WL*QQ

      IF(GL.GT.1.E-03)THEN
      RKAP=1./GL
      ELSE
      RKAP=1000.
      END IF
      
      IF(QWL.GT.1.E-12)THEN
      SQWL=SIN(QWL)/QWL
      ELSE
      SQWL=1.0
      END IF


C2345 789012345678901234567890123456789012345678901234567890123456789012
      F= (WL*SQWL+2.*RHRW*(RKAP*COS(QWL)-QQ*SIN(QWL))/(RKAP**2+QQ**2) )/
     >      SQRT(1.0 + 0.5*(QQ*RSIG)**2)
      RLAYFF = F*F

      RETURN
      END IF
      write(6,901)ityp
901   format(1x,'ERROR function RLAYFF not valid for ITYP=',i6)
      rlayff=0.0
      END
C
C
      FUNCTION RLAYFF2(ITYP,QQ,V,F)
C==== 23/4/98 RKH, special versions of:
C==== FORM FACTOR FOR POLYDISPERSE SHEET - Kotlarchyk & Ritzau, 
C====   J.Appl.Cryst. 24(1991)753-758,  eqns (16) & (17) with 1/Q**2
C==== see comments in RLAYFF above
c==== V(1)= Rrho1, 2= L1 (core), 3= rho2, 4= L2 (shell), 5=rho3 (solv)
C====   6 = Rsigma,  7=t diffuseness
c====
      DIMENSION V(7)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
C
C
C==== assume ITYP=41 or 51
      WL=V(2)
      QWL=0.5*WL*QQ

      IF(QWL.GT.1.E-12)THEN
      SQWL=SIN(QWL)/QWL
      ELSE
      SQWL=1.0
      END IF

      IF(ITYP.EQ.41)THEN
      HL=V(4)
      QHL=0.5*HL*QQ
      IF(QHL.GT.1.E-12)THEN
      SQHL=SIN(QHL)/QHL
      ELSE
      SQHL=1.0
      END IF
C
      F= ( (V(1)-V(5))*WL*SQWL +
     >     2.*(V(3)-V(5))*HL*SQHL*COS( QWL + QHL ) )*
     > EXP( -0.5*(QQ*V(7)*0.3989422803)**2)/ SQRT(1.0 +0.5*(QQ*V(6))**2)
      RLAYFF2 = F*F

      RETURN
C
      ELSE IF(ITYP.EQ.51)THEN
C==== core plus exponential shell
C
      IF(V(4).GT.1.E-03)THEN
      RKAP=1./V(4)
      ELSE
      RKAP=1000.
      END IF
      
C2345 789012345678901234567890123456789012345678901234567890123456789012
      F= ((V(1)-V(5))*WL*SQWL+
     > 2.*(V(3)-V(5))*(RKAP*COS(QWL)-QQ*SIN(QWL))/(RKAP**2+QQ**2) )*
     > EXP( -0.5*(QQ*V(7)*0.3989422803)**2)/ SQRT(1.0 +0.5*(QQ*V(6))**2)
      RLAYFF2 = F*F

      RETURN
      END IF
      write(6,901)ityp
901   format(1x,'ERROR function RLAYFF2 not valid for ITYP=',i6)
      rlayff2=0.0
      END
C
      FUNCTION PARCRYS(QQ,RM,D,GD,SPARE)
C==== Model 29, structure factor for M stacked layers separation D
C====  polydispersity GD - Kotlarchyk & Ritzau, 
C====   J.Appl.Cryst. 24(1991)753-758,  eqns (9)-(12)
C====   12/2/92 RKH
C==== CHECK we may have to restrict M to integers !
C
      RMU=1.0
      QDM=QQ*D*RMU
      AA= -0.5*(GD*QDM)**2
C====      F=EXPSPEC(-0.5*(GD*QDM)**2)
C====      F2=F*F
C====      FPM=F**RM
C====  take the powers of F inside the exponential
      F=EXPSPEC(AA)  
      F2=EXPSPEC(2.0*AA)
      FPM=EXPSPEC(RM*AA)
C
c==== this hits severe problems when Q goes small and F goes up towards
c====  1.0 from below.  We may have to argue that the cos(M*QDM) oscillates
C==== faster than cos(QDM) so averages to zero ?
c
C      PARCRYS = (1.0-F2)/(1.0-2.*F*COS(QDM) +F2) +(1.0/RM)*
C     > ( -2.0*F*( (1.0+F2)*COS(QDM) -2.0*F -FPM*COS(QDM*(RM+1.)) +
C     > 2.0*FPM*F*COS(RM*QDM) -FPM*F2*COS( (RM-1.0)*QDM ) ) )/
C     > ( 1.0 - 2.0*F*COS(QDM) + F2)**2
c
c==== rearranged, 13/2/92, gives same results
c==== tested 14/2/92 in double precsion, single prec. gave same results except
c==== small errors at very low Q ( <.002), and through peak (<.00002),
c==== with RM=10, D=100, GD=0.15 as typical parameters.
c
      PARCRYS=(1.0/RM)*(4.0*F2+ RM*(1.0-F2*F2)-4.0*F2*FPM*COS(QDM*RM) +
     > COS(QDM)*2.*F*( -RM*(1.-F2) -(1.+F2) +FPM*(1.+F2)*COS(RM*QDM) ) +
     > SIN(QDM)*2.*F*FPM*(F2-1.)*SIN(RM*QDM) )/
     > ( 1.0 - 2.0*F*COS(QDM) + F2)**2
      RETURN
c==== for very low GD, and always at low Q the results tend to the 
c==== equation for a perfectly ordered system:
c      PARCRYS=(1.0/RM)*(SIN(0.5*RM*QDM)/SIN(0.5*QDM))**2

      END
c
c
      function WORM(Q,N,L,RAX,U)
c...	this subroutine calculates the Kratky-Porod worm like,
c==== "persistence chain" as elaborated by M.Ragnetti & R.C.Oberthur,
c==== Colloid & Polymer Sci. 264(1986)32-45, with corrections assumed
c==== to eqn (17) & (18) to give correct high Q limits !
C==== RG is computed from the other parameters and is returned.
c==== 				RKH 16/7/92 
c==== 28/7/92 reparametrise for L and N= L/l number of persistence lengths
c==== per chain.  Arrange so that I(Q=0)=1.0
c==== Rax is the radius of gyration of the cross section assuming a
c==== Gaussian scattering density distribution.
c
      REAL*4 L,RAX,U,PA,PU,QL,Y,N
      QL=Q*L
      IF(QL.LE.3.1)THEN
      RG2=N*L*L/6.0
      X=RG2*Q*Q
      PA=DEBGAUSS(1,Q,RG2,0.0) + ( 4./15. + 7./(15.*X) -
     >  (11./15. + 7./(15.*X) )*EXPSPEC(-X) )/N

      ELSE IF(QL.LE.9.4)THEN
      Y=(L*Q)**2
      PA=(12. + 4.*Y/15. -9.55*EXPSPEC(-4.*Y*Y) +
     >  4.0*EXPSPEC(-6.*Y*Y*Y) + 4.9*EXPSPEC(-9.*(Y**4.)) -
     >  3.7*EXPSPEC(-10.*(Y**5.)) )/(N*Y)
      ELSE
        IF(QL.LE.13.4)THEN
          A=440.
          B=0.215
        ELSE
          A=429.
          B=0.225
        END IF

      Y=(L*Q)**2
C==== TRY L*Q IN NUMERATOR not (L*Q)**2
      PA=(3.141592654*L*Q +4./3.)/
     > ( (1.0  - A*COS(B*Y)/(Y**4.1) )*N*Y )
C=== try putting L*(Q**2) for whole denominator.
      END IF
C
      PU=1.0
      IF(U.GT.0.01)THEN
      RG2=N*L*L/6.0
      RGCORR2=RG2*(1.+U)/(1.+2.*U)
      PU=DEBGAUSS(11,Q,RG2,U)/DEBGAUSS(1,Q,RGCORR2,0.0)
      END IF
C
      WORM=PA*PU*EXPSPEC(-0.5*(RAX*Q)**2 )
      RETURN
      END
c
      function DIBLK(Q,RN,LL,F,CHI)
c==== this subroutine calculates the LIEBLER DIBLOCK COPOLYMER
C==== L.LEIBLER, Macromolecule 13(1980)1602-1617 eqns IV-2 to IV-8
c==== 		R.K.Heenan 13/1/97
C
c==== NOTE MAY NOT BE VERY ROBUST TO OVERFLOWS, NON-PHYSICAL PARAMS ETC. !!!
C
      real*4 LL
      LT=1
      UU=0.0
C====  compute Rg**2
      RR = RN*LL*LL/6.0
      BIGF = DEBGAUSS(LT,Q,RR,UU)
      FM = 1.0-F
      D  = (F**2)* DEBGAUSS(LT,Q,RR*F ,UU)
      DM = (FM**2)*DEBGAUSS(LT,Q,RR*FM,UU)
      DEN = D*DM -0.25*(BIGF -D -DM)**2
      DIBLK = RN/( BIGF/DEN  -2.0*CHI*RN)
      RETURN
      END
c
      function DEBGAUSS(LTYP,QQ,RR,UU)
C==== P(Q) for Debye Gaussian coil.  Polydisp if LTYP >10 (i.e. 11 or 21)
c
C==== NOTE input RR is RG SQUARED not RG, but QQ is just Q
c
c==== carefully modified Feb 1992 by R.K.Heenan to avoid over & underflows
C==== 12/5/94 star polymer equation added as LTYP=31, fo rwhich UU is then
c==== the number of arms F
c
c==== polynomial expansion is used at small QR
c
c==== Written by R.K.Heenan, RAL
c
      QR=RR*(QQ**2)
      U=0.0
      FSTAR=1.0
      IF(LTYP.EQ.11)U=ABS(UU)
      IF(LTYP.EQ.31)FSTAR=AMAX1(1.0,ABS(UU))
C====  note U and FSTAR cannot be used simultaneously !
      Y=QR/( (1.0+2.0*U)*(3.0 -2.0/FSTAR) )
C==== polydisp version
      IF(U.GT.1.0E-02.AND.Y.GT.0.01)DEBGAUSS=
     >         2.0*( (1.0+U*Y)**(-1.0/U) +Y -1.0)/( (1.0+U)*(Y**2) )
C==== monodisp version
C2345 789012345678901234567890123456789012345678901234567890123456789012
      IF(U.LE.1.0E-02.AND.Y.GT.0.01)THEN
        AA=(EXPSPEC(-Y)-1.0)/(FSTAR*Y)
        DEBGAUSS= 2.*(AA + 1./FSTAR)/Y
        IF(FSTAR.GT.1.0)DEBGAUSS=DEBGAUSS+ FSTAR*(FSTAR-1.0)*AA*AA
      END IF
C==== EXPSPEC avoids underflows as QR**2 can be large
C==== low Q expansion for ALL versions
      IF(Y.LE.0.01)DEBGAUSS= 1.0 - QR*(1.0+ (0.25 - 0.75*U)*Y )/3.0
      return
      end
c
      FUNCTION EXPSPEC(A)
C==== expspec avoids underflows invoking the error handler
c==== the -88.5 is a machine dependent number
c
c==== The usual EXP( ) function may be fine - depends whether 
c==== a particular compiler gives underflow errors
cc
c==== Written by R.K.Heenan, RAL
c
      EXPSPEC=0.0
      IF(A.GT.-88.5.AND.A.LT.88.0)THEN
         EXPSPEC=EXP(A)
      ELSE IF(A.GE.85.2)THEN
         EXPSPEC=1.0E37
      END IF
C==== NOTE EXP(88.02)=1.684996E38
C==== NOTE EXP(-88.7)=3.006635E-39
      RETURN
      END
C
      function STARD(q,rg,alNf,zeta,rnu)
c==== eq (5) of Richter et.al. J.Phys.IV, C8(1993) 3
C==== quoting Dozier et.al. Macromols 24(1991)2810
c==== RKH 13/5/94, NOT checked rigourously for over/under flow etc.
c
c==== alNf is alpha/(N*f) and gives the relative size of the second part
c==== which modifies the initial guinier region
c==== zeta is a screening length
c==== rnu is the Fory exponent, should be 0.6 in a good solvent, 0.5 in a
c==== theta solvent  ( mu = 2/3 TO 1 )
      qr=q*rg
      qz=q*zeta
      rmu=1./rnu -1.0
      STARD = EXPSPEC(-qr*qr/3.) +
     >            12.56637061*alNf*sin(rmu*atan(qz))/
     >                        (qz*(1.0+qz*qz)**(rmu*0.5))
      return
      end
cc
      function 	HSHAPE(Q,RG1,fa,fb,chi)
C==== I(Q) for H-SHAPE.  R.K.Heenan 1/10/98
C==== see D.J.Read, Macromols 31(1998)899-911
c====
      REAL Q,RG1,fa,fb,chi,f1,f2,
     >  G1,G2,G3,H1,H2,H3,J1,J2,J3,SAA,SAB,SBB
      Y1=(Q*RG1)**2
      f1=fa*0.25
      CALL HGHJ(Y1,f1,G1,H1,J1)
C==== should test fa > 0
      f2=0.25*(1.0-fa-fb)
      Y2=Y1*f2/f1
      CALL HGHJ(Y2,f2,G2,H2,J2)
      Y3=Y1*fb/f1
      CALL HGHJ(Y3,fb,G3,H3,J3)
      SAA=4.0*(J1+ (1.0+2.*G3)*(H1*G2)**2 )
      SAB=4.0*H1*( H2*(1.0+G2+2.0*G2*G3) +H3*G2 )
      SBB=4.0*J2 + J3 + 4.0*(1.0+2.0*G3)*H2**2  +8.0*H2*H3
      HSHAPE=  (SAA*SBB -SAB**2) / (SAA +SBB +2.0*SAB)
      IF(ABS(CHI).GT.1.0E-30)HSHAPE=1.0/(1.0/HSHAPE -2.0*CHI)
      RETURN
      END
C
      SUBROUTINE HGHJ(Y,F,G,H,J)
C==== I(Q) for H-SHAPE.  R.K.Heenan 24/9/98
C==== see D.J.Read, Macromols 31(1998)899-911
      REAL Y,F,G,H,J
      IF(Y.LT.0.01)THEN
      G=1.0 -Y +0.5*Y**2 -Y**3/6.0
      H= F*(1.0 -0.5*Y +Y**2/6.0 )
      J= (F**2)*(1.0 -Y/3.0 +Y**2/6.0)
      ELSE
      G=EXPSPEC(-Y)
      H=F*(1.0 -G)/Y
      J=2.0*((F/Y)**2)*(G -1.0 +Y)
      END IF
      RETURN
      END
c



C==== LAGRANGE CUBIC INTERPOLATION (again !) for
c==== supplied function CORR, at increasing but not necessarily equally
C==== spaced values of R.
c==== i must be .GE.3 on input and nr.GE.4,  on exit r(i) <= rr < r(i+1),
C==== except 2 =< i <= nr-2, 
c==== outside of r(i) to r(nr) result is set ZERO !!, with no warning flag set.
c
      function cubic2(rr,r,corr,nr,i)
c
      dimension r(nr),corr(nr)
c
      if(rr.lt.r(1).or.rr.gt.r(nr))then
      cubic2 = 0.0      
      return
      else
      i=max(2,i)
      i=min(i,nr-2)
c
4     if (rr.lt.r(i)) then
           if(i.le.2)goto 6
           i=i-1
           goto 4
        else 
5          if(rr.gt.r(i+1))then
           if (i.ge.nr-2) goto 6
           i=i+1
           goto 5
           end if
        end if
6       x1= r(i-1)
        x2= r(i)
        x3= r(i+1)
        x4= r(i+2)
        Y1= corr(i-1)
        Y2= corr(i)
        Y3= corr(i+1)
        Y4= corr(i+2)
c====  Lagrange cubic interpolation through four unequally spaced points
c====  note the cubic is forced through the points, so assume statistics are
c====  very good.  
        cubic2 = 
     >  y1*( (rr-x2)*(rr-x3)*(rr-x4) )/( (x1-x2)*(x1-x3)*(x1-x4) )+
     >  y2*( (rr-x1)*(rr-x3)*(rr-x4) )/( (x2-x1)*(x2-x3)*(x2-x4) )+
     >  y3*( (rr-x1)*(rr-x2)*(rr-x4) )/( (x3-x1)*(x3-x2)*(x3-x4) )+
     >  y4*( (rr-x1)*(rr-x2)*(rr-x3) )/( (x4-x1)*(x4-x2)*(x4-x3) )  
      return
      end if
      end
C
      FUNCTION SHELLGAUSSD(QQ,A,B,DA,DB)
C==== 17/4/00 F(Q) for a Gaussian shell, equivalent to rectangular one
c==== from R=a to R=b centred at (a+b)/2
c==== Scattering equation as per M.Gradzielski, D.Langevin, L.Magid & R.Strey
c==== J.Phys.Chem 99(1995)13232-13238
C==== 0.39894228= 1/SQRT(2*PI)
C==== NOTE in the derivs we use the approximation (correct at infinite r )
c====    T = (B-A)/SQRT(2PI)
      real L
	L=B-A
	R=0.5*(A+B)
	R2=R**2
C==== iterate to find T, note the extra 2.0 in denominator of second calc, found
c==== empirically to speed up convergence (RKH 14/4/00)
	T1=0.39894228*L
	T=0.39894228*L*(R2+L**2/12.)/(R2+2.0*T1**2)
	T=0.39894228*L*(R2+L**2/12.)/(R2+T**2)
	T2=T**2
      QQ2=QQ**2
	EXP1=EXP(-0.5*QQ2*T1**2)
	IF(ABS(QQ).GT.1.E-12)THEN
	SHELLGAUSSD=
     >     12.56637061*EXP(-0.5*QQ2*T2)*L*(R*SIN(QQ*R)+QQ*T2*COS(QQ*R))/QQ
	FAPP=12.56637061*EXP1*L*(R*SIN(QQ*R)+QQ*T1**2*COS(QQ*R))/QQ
C==== now the derivatives
	DL = FAPP*(1.0/L -QQ2*L/6.283185307) + 4.0*L**2*COS(QQ*R)*EXP1
	DR = 12.56637061*EXP1*L*
     >                  ( (1.0-(QQ*T1)**2)*SIN(QQ*R) + QQ*R*COS(QQ*R) )/QQ
	ELSE
C==== small Q expansions to avoid divide by zero
      SHELLGAUSSD=12.56637061*L*(1.0-0.5*QQ*T2)*(R**2 + T2)
	DL=12.56637061*(1.0-0.5*QQ2*T1**2)*(R**2 + T1**2) -
     >      2.0*QQ*L**2*(R**2 + T1**2 ) + 4.0*l**2*(1.0-0.5*QQ2*T1**2)
      DR=12.56637061*2.0*R*L*(1.0-0.5*QQ*T1**2)
	END IF
c
	DA= -DL + 0.5*DR
	DB=  DL + 0.5*DR
	RETURN
	END

C
      FUNCTION SHELLGAUSS(QQ,A,B)
C==== 17/4/00 F(Q) for a Gaussian shell, equivalent to rectangular one
c==== from R=a to R=b centred at (a+b)/2
c==== Scattering equation as per M.Gradzielski, D.Langevin, L.Magid & R.Strey
c==== J.Phys.Chem 99(1995)13232-13238
C==== 0.39894228= 1/SQRT(2*PI)
C==== NOTE in the derivs we use the approximation (correct at infinite r )
c====    T = (B-A)/SQRT(2PI)
      real L
	L=B-A
	R=0.5*(A+B)
	R2=R**2
C==== iterate to find T, note the extra 2.0 in denominator of second calc, found
c==== empirically to speed up convergence (RKH 14/4/00)
	T1=0.39894228*L
	T=0.39894228*L*(R2+L**2/12.)/(R2+2.0*T1**2)
	T=0.39894228*L*(R2+L**2/12.)/(R2+T**2)
	T2=T**2
      QQ2=QQ**2
	EXP1=EXP(-0.5*QQ2*T1**2)
	IF(ABS(QQ).GT.1.E-12)THEN
	SHELLGAUSS=
     >     12.56637061*EXP(-0.5*QQ2*T2)*L*(R*SIN(QQ*R)+QQ*T2*COS(QQ*R))/QQ
	FAPP=12.56637061*EXP1*L*(R*SIN(QQ*R)+QQ*T1**2*COS(QQ*R))/QQ
	ELSE
C==== small Q expansion to avoid divide by zero
      SHELLGAUSS=12.56637061*L*(1.0-0.5*QQ*T2)*(R**2 + T2)
	END IF
	RETURN
	END


      FUNCTION SHELLEXP(LT,Q,RLAST,RTHIS)
C==== 9/11/95 copyright R.K.Heenan, RAL
c==== scattering from exponential spherical shells
c==== used for MODEL 8 & 10
c====  note that the lower radius of the shell may be extracted from the 
c====  preceding shell (defaults to zero if this is the first shell)
c====  The sign of the contrast step is the same as the derivative of the
c====  sld(r) curve.
c
c====  8  1   as appropriate
c====  8  2   A
c====  8 21  contrast step   DOWN hill to infinity  sld= exp( -(r-A)/L )
c====  8  2   A+3.5L         - this needed to define and fit L
c====                        Obviously this one should be the LAST shell
c====                        in your contrast profile !
c====                        Note the mean of this sld curve is r= A+L
c====                        and its standard deviation is L
c====                       When A is LARGE then L is the mean thickness
c====                       of the shell.
c
c====  8  1  as needed
c====  8  2   A
c====  8 31  contrast step   DOWN hill with step to zero at A+3.5L
c====  8  2   A+3.5L                 sld= exp( -(r-A)/L )
c====                        This includes the scatter from the small step
c====                        left at A+3.5L back to zero sld
c
C====  8  1   ZERO           dummy shell to let you define & fit L !
c====  8  2   A-3.5L
c====  8 41  contrast step   UP hill from zero  sld= exp( -(A-r)/L )
c====  8  2   A
c====                        this includes the scatter from the small step
c====                        left at A under the sld exponential
c 
c====  8  1   as needed      could use this to compensate the "step"
c====  8  2   A-3.5L
c====  8 51  contrast step   UP hill cut off at (A-3.5L)  sld= exp( -(A-r)/L )
c====  8  2   A
c====                        this includes the scatter from the small step
c====                        left at A+3.5L back to zero sld, BUT not the
c====                        possible big step down.
c
      INTEGER LT
      REAL L
      L=(RTHIS-RLAST)/3.5
C
      IF(LT.LE.31)THEN
C==== down hill
      A=RLAST
      SIGN=+1.0
      ELSE
C==== up hill
      A=RTHIS
      SIGN=-1.0
      END IF
C
      QA=Q*A
      SQA=SIN(QA)
      CQA=COS(QA)
      QL=Q*L
      QL2=QL*QL
      SHELLEXP=(  QA*(SIGN*QL*SQA -CQA) +SQA + 
     >      2.*QL2*(SQA+SIGN*QL*CQA)/(1.+QL2)   ) / (1.+QL2)
C
      IF(LT.NE.21)THEN
C==== or remove the r=0 to minus infinity part by
        IF(LT.EQ.41)THEN
        SCALE=EXP(-A/L)
        A=0.0
        ELSE
c==== cut off at A + 3.5L [ or A -3.5L ]
        A=A+SIGN*3.5*L
        SCALE=EXP(-3.5)
        END IF
        QA=Q*A
        SQA=SIN(QA)
        CQA=COS(QA)
C==== NOTE contrast step is -ve [+ve] for down [up] hill, so need 
c==== to add [ add] the step here
        SHELLEXP = SHELLEXP + SCALE*(SQA-QA*CQA)
C
c====  then subtract the rest of the exponential to infinity [ minus infinity]
        SHELLEXP = SHELLEXP - SCALE*(  QA*(SIGN*QL*SQA -CQA) +SQA + 
     >      2.*QL2*(SQA+SIGN*QL*CQA)/(1.+QL2)   ) / (1.+QL2)
      END IF      
C
      SHELLEXP=SHELLEXP*12.56637062/Q**3
      RETURN
      END
CC
      SUBROUTINE VOIGT(X,SIGMA,GAMMA,YVAL,DERX,DERS,DERG)
C
C *** VOIGT BY WIFD JUN 84 ***
C
CH Calculates normalised Voigt function
C
CD...      VOIGT is the normalised VOIGT function; a convolution of a
CD      normalised Gaussian (half-width= sigma) and Lorentzian
CD      (Cauchy) function (full-width at half-height= width). The
CD      function is calculated by noting that the VOIGT function
CD      is, to within a scale factor, equal to the real part of the
CD      complex error function.
C            W.I.F.David            6-JUN-84
C            Neutron Division
C            RAL                  ext. 5179
C
      DOUBLE PRECISION WR,WI,XX,YY
C
      OVRTPI=0.564189584
      OVRT2=0.707106781
      BTEM=OVRT2/SIGMA
      ATEM=OVRTPI*BTEM
      XTEM=X*BTEM
      YTEM=0.5*GAMMA*BTEM
      XX= DBLE(XTEM)
      YY= DBLE(YTEM)
      CALL WERF(WR,WI,XX,YY)
      SWR=SNGL(WR)
      SWI=SNGL(WI)
      CTEM=ATEM*BTEM
      YVAL=ATEM*SWR
      DWRDX=-2.*(XTEM*SWR-YTEM*SWI)
      DWRDY= 2.*(YTEM*SWR+XTEM*SWI-OVRTPI)
      DERX=CTEM*DWRDX
      DERS=-ATEM*(SWR+DWRDX*XTEM+DWRDY*YTEM)/SIGMA
      DERG=0.5*CTEM*DWRDY
C
      RETURN
      END
c
	function VOIGT1(xdel,sigma,gamma)
c
c		W.I.F.David		6-JUN-84
c		Neutron Division
c		RAL			ext. 5179
c
c
	real*8		wr,wi,xx,yy
c
	xx= dble(0.707106781*xdel/sigma)
	yy= dble(0.353553391*gamma/sigma)
	call WERF(wr,wi,xx,yy)
	VOIGT1= 0.39894228*sngl(wr)/sigma
c
        return
        end
c
c
	subroutine WERF(rs1,rs2,xx,yy)
c	W.I.F.David 25-May-84
	implicit real*8		(a-h,o-z)
	real*8			lambda
	logical			b
	x=dabs(xx)
	y=dabs(yy)
	if (y .lt. 4.29 .and. x .lt. 5.33) go to 1
	h= 0.
	nc= 0
	nu= 8
	lambda= 0.
	b= .true.
	go to 2
 1	s=(1.0-y/4.29)*dsqrt(1.0-x**2/28.41)
	h=1.6*s
	h2=2.0*h
	nc=6+idint(23.0*s)
	nu=9+idint(21.0*s)
	lambda=h2**nc
	b= .false.
	if (lambda .eq. 0.) b= .true.
 2	r1=0.
	r2=0.
	s1=0.
	s2=0.
	n=nu+1
 3	n=n-1
	fn=n+1
	t1=y+h+fn*r1
	t2=x-fn*r2
	c=0.5/(t1**2+t2**2)
	r1=c*t1
	r2=c*t2
	if (h .le. 0.0 .or. n .gt. nc) go to 4
	t1= lambda+s1
	s1=r1*t1-r2*s2
	s2=r2*t1+r1*s2
	lambda=lambda/h2
 4	if (n .gt. 0) go to 3
	if (b) go to 6
	rs1=s1
	rs2=s2
	go to 7
 6	rs1=r1
	rs2=r2
 7	rs1= 1.12837916709551*rs1
	if (y .eq. 0.0) rs1= dexp(-x**2)
	rs2= 1.12837916709551*rs2
	if (xx .lt. 0) rs2= -rs2
	return
	end
c
      FUNCTION rIKCAR(T,siglam,betaINV,RATIO)
C====  see S.Ikeda & J.M.Carpenter, Nucl. Instr. & Methods A239(1985)536-544
c====  moderator pulse shape function
c====  T in musec, betainv in musec,  0 < RATIO < 1
C====  SIGLAM= SIGMA/LAMBDA  in (CM**-1 / ANGSTROM)
c====   R.K.Heenan, ISIS, RAL, 25/3/96 and later modifications
c====
      ALPHA=ABS(SIGLAM/2.5277842)
      BETAINV=AMAX1(5.0,BETAINV)
      BETA=1./BETAINV
      RATIO=AMAX1(0.0,RATIO)
      RATIO=AMIN1(RATIO,1.0)
c
      AT=ALPHA*T
      BT=-BETA*T
      AMB=ALPHA-BETA
      EAT=EXPSPEC(-AT)
C
      RIKCAR=0.5*ALPHA*(1.-RATIO)*(AT**2)*EAT +
     >   RATIO*(ALPHA**3)*BETA*
     >   (EXPSPEC(BT) -(1.+AMB*T +0.5*(AMB*T)**2 )*EAT )/(AMB**3)
C
      RETURN
      END
C
      FUNCTION CRTPFN(TT,SIG,TZERO,TAUS)
C==== see J.M.Carpenter,R.A.Robinson,A.D.Taylor & D.J.Picton,
c==== Nucl. Instrs. & Meths a234(1985)542-551 who ref this eqn to another
c==== conference paper by Carpenter et.al.
      taus=abs(taus)
      if(taus.lt.1.e-12)taus=1.e-12
      bs=1.0/taus
      sig=abs(sig)
      if(sig.lt.1.e-12)sig=1.e-12
      CRTPFN=bs*expspec( 0.5*(bs*sig)**2 - bs*(tt-tzero) )
     >   *errfunc( (bs*sig -(tt-tzero)/sig )*0.707106781 )
      return
      end
c
      FUNCTION ERRFUNC(X)
c==== complement of error function
C====  ( 2/SQRT(PI) )* INTEGRAL( X TO infinity) OF EXP(-X**2)dX
C====  R.K.Heenan, ISIS, RAL, 12/3/96 using W.H.Press et.al.
c==== "Numerical recipes in C" p176
c==== Fractional Error "should be less than 1.2e-07 over whole range"
c====
      z=ABS(X)
      t=1.0/(1.0+0.5*z)
      ERRFUNC=t*exp(-z**2-1.26551223 +t*(1.00002368+t*(0.37409196+
     > t*(0.09678418+
     > t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
     > t*(-0.82215223+t*0.17087277)))))))))
      if(x.lt.0.0)ERRFUNC=2.0-ERRFUNC
      RETURN
      END
C
      FUNCTION ERRFUN(X)
C====  ( 2/SQRT(PI) )* INTEGRAL( 0 TO X) OF EXP(-X**2)dX
C====  R.K.Heenan, ISIS, RAL, 12/3/96 using W.H.Press et.al.
c==== "Numerical recipes in C" p176
c==== Error "should be less than 1.2e-07 over whole range"
c====
      z=ABS(X)
      t=1.0/(1.0+0.5*z)
      ERRFUN=1.0- t*exp(-z**2-1.26551223 +t*(1.00002368+t*(0.37409196+
     > t*(0.09678418+
     > t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
     > t*(-0.82215223+t*0.17087277)))))))))
      if(x.le.0.0)errfun=-errfun
      RETURN
      END
C
      SUBROUTINE qromb(func,a,b,ss,EPS)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER JMAX,JMAXP,K,KM
      REAL a,b,func,ss,EPS
      EXTERNAL func
c==== POLINT does a K point interpolation,was 5 now 7
      PARAMETER (JMAX=20, JMAXP=JMAX+1, K=7, KM=K-1)
CU    USES polint,trapzd
      INTEGER j
      REAL dss,h(JMAXP),s(JMAXP)
      h(1)=1.
      do 11 j=1,JMAX
        call trapzd(func,a,b,s(j),j)
        if (j.ge.K) then
          call polint(h(j-KM),s(j-KM),K,0.,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
      write(*,*) 'too many steps in qromb'
      END
      SUBROUTINE trapzd(func,a,b,s,n)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL del,sum,tnm,x
      if (n.eq.1) then
        s=0.5*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5*(s+(b-a)*sum/tnm)
      endif
      return
      END
      SUBROUTINE polint(xa,ya,n,x,y,dy)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER n,NMAX
      REAL dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.)write(*,*)  'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END
C
C
      SUBROUTINE qrombTWO(func,a,b,ss,EPS)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER JMAX,JMAXP,K,KM
      REAL a,b,func,ss,EPS
      EXTERNAL func
      PARAMETER (JMAX=20, JMAXP=JMAX+1, K=7, KM=K-1)
CU    USES polint,trapzd
      INTEGER j
      REAL dss,h(JMAXP),s(JMAXP)
      h(1)=1.
      do 11 j=1,JMAX
        call trapzdTWO(func,a,b,s(j),j)
        if (j.ge.K) then
          call polintTWO(h(j-KM),s(j-KM),K,0.,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
      write(*,*)  'too many steps in qromb'
      END
      SUBROUTINE trapzdTWO(func,a,b,s,n)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL del,sum,tnm,x
      if (n.eq.1) then
        s=0.5*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5*(s+(b-a)*sum/tnm)
      endif
      return
      END
      SUBROUTINE polintTWO(xa,ya,n,x,y,dy)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER n,NMAX
      REAL dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.)write(*,*)  'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END
C
C
      SUBROUTINE qrombTHREE(func,a,b,ss,EPS)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER JMAX,JMAXP,K,KM
      REAL a,b,func,ss,EPS
      EXTERNAL func
      PARAMETER (JMAX=20, JMAXP=JMAX+1, K=7, KM=K-1)
CU    USES polint,trapzd
      INTEGER j
      REAL dss,h(JMAXP),s(JMAXP)
      h(1)=1.
      do 11 j=1,JMAX
        call trapzdTHREE(func,a,b,s(j),j)
        if (j.ge.K) then
          call polintTHREE(h(j-KM),s(j-KM),K,0.,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
      write(*,*)  'too many steps in qromb'
      END
      SUBROUTINE trapzdTHREE(func,a,b,s,n)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL del,sum,tnm,x
      if (n.eq.1) then
        s=0.5*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5*(s+(b-a)*sum/tnm)
      endif
      return
      END
      SUBROUTINE polintTHREE(xa,ya,n,x,y,dy)
C==== NUMERICAL RECIPES in Fortran, W.H.Press et.al
      INTEGER n,NMAX
      REAL dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.)write(*,*)  'failure in polint'
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END
C
      FUNCTION RODSHEAR(QQ1,VAR,DSUMF,NPAR1,LTYP)
C==== FORM FACTOR FOR SHEARED CORE/SHELL ROD  by RKH 13/12/95 
c==== remove NAG integration routines - use only local quadratures !
c==== See Hayter & Penfold J.Phys.Chem. 88(1984)4589-4593
c
C==== RODS, var(1) = OUTER RADIUS R, 
c====       var(2) = LENGTH RL  (NOT HALF LENGTH AS IN MOST FORMULAE), 
C====       var(3) = shell DELR, 
C====       var(4) = CONTRAST = (RHO.INNER - RHO.OUTER)/RHO.OUTER
C====       var(5) = BIG GAMMA
C====       var(6) = PSI mid, angle of Q to x axis, in DEGREES
c====       var(7) = half range DPSI, so PSI is integrated from
c====                PSI-DPSI to PSI + DPSI
C====
C==== NOTE that the first parameter in the model, V(NPAR), the scale factor
C==== is NOT passed through here so var(1) is V(NPAR+1)
C====
C==== COPYRIGHT BY RKH at RAL 13/12/95, REWRITTEN 21/5/96
C==== LTYP = 21, 31 sheared rods as
c====               Hayter & Penfold J.Phys.Chem. 88(1984)4589-4593
C====        41, 51 Maier-Saupe added Aug 96, by RKH
C====        61, 71 "end on" Maier Saupe Feb 97 by  RKH, corrected 7/4/97 RKH
C==== second LTYP in each case is for "no cap" version of core/shell rod
c
      EXTERNAL SHEAR1,SHEAR2,SHEAR3,SAUPE
      INCLUDE 'FISHDIM.PAR'
      DIMENSION VAR(7)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      NPAR=NPAR1
      LTYPROD=LTYP
C==== pass through common to keep Gaussian quads happy !
      QQ=QQ1
      R=ABS(VAR(1))
      RL=ABS(VAR(2))
      DELR=VAR(3)
      DELRCAP=VAR(3)
C==== calculate as much as possible, once only, and pass through common:
C==== allow for no end cap case 18 31 on first card of model.   RKH
      IF(LTYPROD.EQ.31.OR.LTYPROD.EQ.51.OR.LTYPROD.EQ.71)DELRCAP=0.0
      CONT=VAR(4)
      QR1=QQ*(R-DELR)
      QL1=QQ*(RL*0.5-DELRCAP)
      QR2=QQ*R
      QL2=QQ*RL*0.5
      V2=SQRT(PI*R*R*RL)
      V1=CONT*PI*((R-DELR)**2)*(RL-2.*DELRCAP)/V2
c==== change the normalisation so that P(Q)=1/(N*V*dRHO**2) at Q=0, 
c==== and  ZERO SHEAR.
c
c==== XXXYYY TEST fudge EPSABS for QA02
      EPSASHR=EPSRSHR*V2*V2*(0.005/QQ)**3
C==== just to confuse you M1,M2,M3 are for theta, phi and psi
      M1=NMETH(NPAR)
      M2=NMETH(NPAR+1)
      M3=NMETH(NPAR+2)
c
      PSIQ=VAR(6)
      DPSIQ=VAR(7)
C==== initialise angular distribution functions:
      IF(LTYPROD.EQ.21.OR.LTYPROD.EQ.31)THEN
      IF(ABS(VAR(5)).GT.1.E-24)THEN
      TPHIZER = ATAN(8.0/VAR(5))
      ELSE
      TPHIZER=PIBY2
      END IF
      C2PZ=COS(TPHIZER)
C
      ELSE
C==== Maier-Saupe A*exp(m*cos**2(theta) )   store A in TPHIZER and m in RMSPE
c====  where orientation width m is VAR(5) as per BIGGAMMA
c==== integral from 0 to pi/2 of exp(m*cos(theta)**2) sin(theta) dTheta
c==== is same as integral from 0 to 1 of exp(m X**2) dX
      RMSPE=VAR(5)
      IF(ABS(RMSPE-PREVRMSPE).GT.0.00001)THEN
      KOUNT=0
      ZER=0.0
      ONE=1.0
      CALL INTEGR1(NMETH(NPAR+3),NPAR+3,SAUPE,ZER,ONE,
     > EPSASPE,EPSRSPE,SUM,SUM1,ABSERR,IFAIL)
      TPHIZER=1./SUM
      PREVRMSPE=RMSPE
      END IF
      IF(LTYPROD.EQ.61.OR.LTYPROD.EQ.71)THEN
C==== ignore angle psi for end-on view, as is independent of psi
      PSIQ=0.0
      DPISQ=0.0
      END IF
      END IF
C
C==== set up ranges of integrations
C==== N.B. if you make changes here then alter INTPIC3 also !!!
      TH1=0.0
      TH2=PI
      IF(ABS(DPSIQ).LE.0.0001.AND.ABS(PSIQ-90.0).LE.0.0001)TH2=PIBY2
      PH1=0.0
      PH2=2.0*PI
      IF(LTYPROD.EQ.61.OR.LTYPROD.EQ.71)THEN
C==== reduce range of integration for end-on view
      TH2=PIBY2
      PH2=PIBY2
      END IF
c
      IF1SHR=0
      IF1ROD=0
C==== KOUNT is for SHEAR1(theta), KOUNT1 for SHEAR2(phi),
c====   and KOUNT2 for SHEAR3(psi)
      KOUNT=0
      KOUNT1=0
      KOUNT2=0
C
c==== check own quadrature still set up OK     REMOVED RKH 9/4/97
C==== ( will get done each data set if refining two sets, else should
c==== store the integration range for each scheme also ! )
C====  got to catch 90 10 going to 90 0 or vice versa, as can only reduce
c==== range of TH1 to TH2 for 90 0
C      IF((ABS(PSIQ-PREVPSIQ).GT.0.00001.OR.
C     >    ABS(DPSIQ-PREVDPSIQ).GT.0.00001).AND.M1.LE.NSIMPLE)THEN
C      NMETH(NPAR)=NGAUSS(M1)
C      NSIMP=129
C      IF(IQ(NPAR).GT.0)NSIMP=NGPT(IQ(NPAR))
C      CALL QUADSET(NPAR,TH1,TH2,NSIMP)
C      NMETH(NPAR)=M1
C      PREVPSIQ=PSIQ
C      PREVDPSIQ=DPSIQ
C      END IF
C
      IF(ABS(DPSIQ).LE.0.0001)THEN
C==== no integration over PSIQ
      IF(ABS(PSIQ-90.).LE.0.0001)THEN
C
C==== simplified formula, only a single integration
      CALL INTEGR1(M1,NPAR,SHEAR1,TH1,TH2,EPSASHR,EPSRSHR,
     >   SUM,SUM1,ABSERR,IFAIL)
c==== for some reason I doubled here, to allow for reduced range of integration
c==== but there is a compensating *0.5 in SHEAR1 
      SUM=SUM*2.0
      SUM1=SUM1*2.0
      ELSE
C
C==== double integration
      CALL INTEGR2(M2,NPAR+1,SHEAR2,PH1,PH2,EPSASHR,EPSRSHR,
     >    SUM,SUM1,ABSERR,IFAIL)
      END IF
      ELSE
C==== full, very slow, integration
C
      PSI1=PSIQ-DPSIQ
      PSI2=PSIQ+DPSIQ
c==== check own quadrature still set up OK   REMOVED RKH 9/4/97
C      IF((ABS(PSI1-PREVPSI1).GT.0.00001.OR.
C     >  ABS(PSI2-PREVPSI2).GT.0.00001).AND.M3.LE.NSIMPLE)THEN
C      NMETH(NPAR+2)=NGAUSS(M3)
C      NSIMP=129
C      IF(IQ(NPAR+2).GT.0)NSIMP=NGPT(IQ(NPAR+2))
C      CALL QUADSET(NPAR+2,PSI1,PSI2,NSIMP)
C      NMETH(NPAR+2)=M3
C      PREVPSI1=PSI1
C      PREVPSI2=PSI2
C      END IF
C
      CALL INTEGR3(M3,NPAR+2,SHEAR3,PSI1,PSI2,
     > EPSASHR,EPSRSHR,SUM,SUM1,ABSERR,IFAIL)
      SUM=SUM/(2.0*DPSIQ)
      SUM1=SUM1/(2.0*DPSIQ)
      END IF
C==== XXXXX check whether the SUM & SUM1 pass through correctly !
      RODSHEAR=SUM
      DSUMF = 0.0
      RETURN
      END
c
      FUNCTION SHEAR3(PSIQ1)
      EXTERNAL SHEAR2
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      PSIQ=PSIQ1
      CALL INTEGR2(M2,NPAR+1,SHEAR2,PH1,PH2,EPSASHR,EPSRSHR,
     >   SUM,SUM1,ABSERR,IFAIL)
      KOUNT2=KOUNT2+1
      SHEAR3=SUM
      RETURN
      END
c
      FUNCTION SHEAR2(PHI1)
      EXTERNAL SHEAR1
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      PHI=PHI1
      CALL INTEGR1(M1,NPAR,SHEAR1,TH1,TH2,EPSASHR,EPSRSHR,
     >   SUM,SUM1,ABSERR,IFAIL)
      KOUNT1=KOUNT1+1
      SHEAR2=SUM
      RETURN
      END
C
      FUNCTION SHEAR1(THETA)
c==== 13/12/95 version for sheared rods routine RODSHEAR, rewritten 21/5/96
c==== to suit adaptive NAG integration schemes.
c====  F = 2.0*V*sin(Q*H*cos(gam))*J1(Q*R*sin(gam))/( Q*H*cos(gam)*Q*R*sin(gam))
c====    where V = pi*(R**2)*RL and H = RL/2
c==== for now this only computes F(Q)**2 and not F(Q)
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
c
      IF(ABS(PSIQ-90.).LE.0.001)THEN
c==== special cases when PSI =90 - the *0.5 here cancels with *2.0 in RODSHEAR
c==== ( could be tidied up ! )
c
      SHEAR1=RODF2(THETA)
C==== N.B. get sin(theta) back from RODF2, which also INCLUDES *STH and does KOUNT
c
      IF(LTYPROD.EQ.21.OR.LTYPROD.EQ.31)THEN
      SHEAR1=SHEAR1*0.5*(1.-C2PZ)/(1.0-(STH**2)*C2PZ)**1.5
c
      ELSE IF(LTYPROD.EQ.41.OR.LTYPROD.EQ.51)THEN
C==== Maier-Saupe distribution
c==== TPHIZER contains the normalisation constant
      SHEAR1=SHEAR1*0.5*TPHIZER*EXPSPEC(RMSPE*(1.0-STH**2))
c
      ELSE IF(LTYPROD.EQ.61.OR.LTYPROD.EQ.71)THEN
C==== end on, Maier-Saupe distribution - should NEVER get here !
      SHEAR1=0.0
      END IF
C
      ELSE
      STH=SIN(THETA)
      PSIQRAD=PSIQ*3.141592654/180.
      CGAMP = STH*COS(PHI)*COS(PSIQRAD)
C
      IF(ABS(PSIQ).GT.0.001)THEN
      AA = COS(THETA)*SIN(PSIQRAD)
      CGAMM = CGAMP - AA
      CGAMP = CGAMP + AA
      SGAMM = SQRT(1.0 - CGAMM**2)
      END IF
C
C==== presume sign of SGAMMP & SGAMMM does not matter ????
      SGAMP = SQRT(1.0 - CGAMP**2)
c
C==== NOW for the orientation function ( beware 1/(4pi) included later )
      IF(LTYPROD.EQ.21.OR.LTYPROD.EQ.31)THEN
      STH=STH*((1.0-C2PZ)*(1.0+(STH**2)*C2PZ)**1.5)/
     >     ((1.0 -(STH**2)*C2PZ*COS(2.0*PHI-TPHIZER) )**2 )
c
      ELSE IF(LTYPROD.EQ.41.OR.LTYPROD.EQ.51)THEN
C==== Maier-Saupe distribution   CHECK - DOES THIS NEED TWO TERMS STILL ???
c==== TPHIZER contains the normalisation constant
      STH=STH*TPHIZER*EXPSPEC(RMSPE*(1.0-STH**2))
c
      ELSE IF(LTYPROD.EQ.61.OR.LTYPROD.EQ.71)THEN
C==== end on, Maier-Saupe distribution, normalisation checked 9/4/97
      STH=STH*8.0*TPHIZER*EXPSPEC(RMSPE*(1.0-STH**2))
      END IF
c
c=== do the first term:
      QBS=QR2*SGAMP
      QHC=QL2*CGAMP
C====          trap potential div by zero if beta=0.0 or Q=0.0
C==== outer cylinder
      DSUM=V2*TRJ1XBYX(QBS)
      IF(ABS(QHC).GT.1.E-12)DSUM=DSUM*SIN(QHC)/QHC
C
C==== inner cylinder
      IF(ABS(CONT).GT.1.E-12)THEN
      QAS=QR1*SGAMP
      QHC1=QL1*CGAMP
      DSUM1=V1*TRJ1XBYX(QAS)
      IF(ABS(QHC1).GT.1.E-12)DSUM1=DSUM1*SIN(QHC1)/QHC1
      ELSE
      DSUM1=0.0
      END IF
C
C==== add the inner & outer cylinder terms
      SHEAR1 = STH*(DSUM1+DSUM)**2
c
c==== Now the same again for the gamma minus terms:
      IF(ABS(PSIQ).GT.0.001)THEN
c=== do the first term:
      QBS=QR2*SGAMM
      QHC=QL2*CGAMM
C==== outer cylinder
      DSUM=V2*TRJ1XBYX(QBS)
      IF(ABS(QHC).GT.1.E-12)DSUM=DSUM*SIN(QHC)/QHC
C
C==== inner cylinder
      IF(ABS(CONT).GT.1.E-12)THEN
      QAS=QR1*SGAMM
      QHC1=QL1*CGAMM
      DSUM1=V1*TRJ1XBYX(QAS)
C
      IF(ABS(QHC1).GT.1.E-12)DSUM1=DSUM1*SIN(QHC1)/QHC1
      END IF
C==== now halve the gamma plus term, and add on the gamma minus one
      SHEAR1=0.5*(SHEAR1+STH*(DSUM1+DSUM)**2)
      END IF
C==== 0.079577... = 1./(4*PI)  
c====  Omitted from orientation function above, is needed to get absolute
c==== intensities to agree with other routines.
C==== (the PI could cancel with that in V1 & v2, but I've kept V1 & V2 same
c====  as for normal rod )
      SHEAR1=SHEAR1*0.07957747155
      KOUNT=KOUNT+1
      END IF

C====
      RETURN
      END
C
      SUBROUTINE INTPIC(NPAR1)
C==== HELP USER TO CHOOSE BEST INTEGRATION SCHEME BY DOING TRIAL CALCULATIONS
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan, at RAL, 20/5/96
C==== modified 17/10/88 to use NAG library, 
c==== Uses SINGLE precision NAG library, N.B. there is trouble if
c==== you try to mix in the double prec. versions - presume over the
c==== internal routines used by D01BA
C==== Mods 20/5/96 to use for models 18 (rod), 21( ellipse) 
c==== 24 (core/shell ellipse ) which now SHARE a common block /RODSETC/
c==== 12/6/96 replace NAG by own Gaussian quads, as cannot repeat the NAG ones !
c====
c==== 6/8/99 remove all NAG & Harwell, add quadpack and generally tidy up !
c====
      INCLUDE 'FISHDIM.PAR'
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      DATA NGAUSS/2,3,4,7,8,9,6*0/, NSIMPLE/6/, NTOT/10/
c====      DATA NGAUSS/4,10,16,32,48,64,6*0/
C==== NOTE - if you introduce new integration routines, then not only will
c==== you need to change INTPIC(),INTPIC3(),INTEGR1(), INTEGR2() & INTEGR3() 
C==== but also the code in the calling routines 
C====  RODFF, ELLIPSE, ELLIPSHELL, & RODSHEAR etc
c==== that sorts out whether F(Q) has to be computed separately from F(Q)**2.
C==== Should store X1 & x2 for each integration, and a flag for how F(Q)
c==== is calculated - would remove a lot of testing in these other routines.
      DATA INTNAME/
     > 'Simpson rule            ','GAUSS  4               ',
     > 'GAUSS 10                ','GAUSS 16               ',
     > 'GAUSS 32                ','GAUSS 64               ',
     > 'QROMB(use small EPSREL) ','GAUSS10/KRONROD21      ',
     > 'G15/KRONROD31           ','G30/KRONROD61 quadpack ',
     >  2*' '/
      NPAR=NPAR1
      PIBY2=2.0*ATAN(1.0)
      PI=2.0*PIBY2
      BET1=0.0
      BET2=PIBY2
      WRITE(JS,51)(I,INTNAME(I),I=1,10)
   51 FORMAT(1X,I3,5X,A24)
C
   10 IF(LM(NPAR).EQ.18)THEN
      WRITE(JS,101)
  101 FORMAT(1X,'CHOOSE INTEGRATION SCHEME FOR RODS FORM FACTOR =  (*)',
     > /,1X,' 9 & 10 ARE BEST FOR LONG RODS (even if Gauss look converged',
     > /,1x,' derivs are not good)',
     * /,1X,'( 1-10, -11 TO TEST ALL )')
c      READ(IS,*,ERR=10)METH
c     integrationscheme
      METH=10
      IF(METH.LE.-11.OR.METH.EQ.1)THEN
      WRITE(JS,104)
  104 FORMAT(1X,'NSIMP = ',$)
      READ(IS,*)NSIMP
      END IF
C
      IF(METH.LE.-11)THEN
      WRITE(JS,102)
  102 FORMAT(1X,'TEST RODS FORM FACTOR FOR Q, R, L, DR, CONTR = ?  (*)')
      READ(IS,*,ERR=10)QQT,RT,RLT,DELRT,CONTT
      END IF
C
      ELSE IF(LM(NPAR).EQ.21)THEN
      WRITE(JS,201)
  201 FORMAT(1X,'CHOOSE INTEGRATION SCHEME FOR ELLIPSOID FORM FACTOR =  (*)',
     * /,1X,'( 1-10,  -11 TO TEST ALL )')
c      READ(IS,*,ERR=10)METH
c     integrationscheme
      METH=10
      IF(METH.LE.-11.OR.METH.EQ.1)THEN
      WRITE(JS,104)
      READ(IS,*)NSIMP
      END IF
C
      IF(METH.LE.-11)THEN
      WRITE(JS,202)
  202 FORMAT(1X,'TEST ELLISOID FORM FACTOR FOR Q, R, X = ?  (*)')
      READ(IS,*,ERR=10)QQT,RT,XT
      END IF
      ELSE IF(LM(NPAR).EQ.24)THEN

      WRITE(JS,301)
  301 FORMAT(1X,'CHOOSE INTEGRATION SCHEME FOR CORE/SHELL ELLIPSOID ',
     * /,1X,'( 1-10, -11 TO TEST ALL ) ',$)
c      READ(IS,*,ERR=10)METH
c     integrationscheme
      METH=10
      IF(METH.LE.-11.OR.METH.EQ.1)THEN
      WRITE(JS,104)
      READ(IS,*)NSIMP
      END IF
C
      IF(METH.LE.-11)THEN
      WRITE(JS,302)
  302 FORMAT(1X,'TEST CORE/SHELL ELLISOID FOR Q, V1, V2, V3, V4 = ?',/,
     > ' (consult manual for parameter definitions)  (*)  ',$)
      READ(IS,*,ERR=10)QQT,VV1T,VV2T,VV3T,VV4T
      END IF
      ELSE
      RETURN
      END IF

      IF(METH.LE.-11.OR.METH.GT.NSIMPLE)THEN
      WRITE(JS,501)
  501 FORMAT(1X,'absolute & relative errors for adaptive integrations',/,
     >  '  EPSABS, EPSREL ( try 0.001 ( & test) 0.001)= ',$)
c      READ(5,*,ERR=10)EPSABS,EPSREL
c     integrationscheme
      EPSABS=0.001
      EPSREL=0.001
C
      IF(LM(NPAR).EQ.18)THEN
      EPSAROD=EPSABS
      EPSRROD=EPSREL
      ELSE IF(LM(NPAR).EQ.21)THEN
      EPSAELL=EPSABS
      EPSRELL=EPSREL
      ELSE IF(LM(NPAR).EQ.24)THEN
      EPSAESH=EPSABS
      EPSRESH=EPSREL
      END IF
      END IF
C
600   NEEDF1=1
      IF(METH.LE.-11.OR.METH.GT.NSIMPLE)THEN
      WRITE(JS,107)
  107 FORMAT(1X,'Do you need to calculate F(Q) as well as F(Q)**2,',/,
     > ' i.e. do you have an S(Q) ? ( 0-no, 1-yes) ',$)
c      READ(IS,*)NEEDF1
c     integrationscheme
      NEEDF1=1
      IF(NEEDF1.LT.0.OR.NEEDF1.GT.1)GOTO 600
      END IF
C
      IF(METH.GE.1.AND.METH.LE.NTOT)GOTO 210
      IF(METH.GT.-11)GOTO 10
      DO I=1,NTOT
      NMETH(NPAR)=I
C==== set up wts etc
      IF(I.LE.NSIMPLE)THEN
      NMETH(NPAR)=NGAUSS(I)
      CALL QUADSET(NPAR,BET1,BET2,NSIMP)
      NMETH(NPAR)=I
      END IF
      IF(LM(NPAR).EQ.18)THEN
      SUM=RODFF(QQT,RT,RLT,DELRT,CONTT,SUMF,NPAR)
      ELSE IF(LM(NPAR).EQ.21)THEN
      SUM=ELLIPSOID(QQT,RT,XT,SUMF,NPAR)
      ELSE IF(LM(NPAR).EQ.24)THEN
      SUM=ELLIPSHELL(QQT,VV1T,VV2T,VV3T,VV4T,SUMF,NPAR)
      END IF
      WRITE(JS,504)I,INTNAME(I),KOUNT1,SUM,KOUNT,SUMF
  504 FORMAT(1X,I2,1X,A24,I5,
     *' F^2=',1pe15.8,I5,' F=',E15.8)

      IF(I.GE.7.AND.I.LE.10)WRITE(JS,503)ABSERR1,ABSERR
  503 FORMAT(1X,36X,'+-',1PE12.4,9X,'+-',E12.4)
c====      END IF
      END DO
C====
      GOTO 10
210   IF(METH.LE.NSIMPLE)THEN
C==== set up wts etc
      I=METH
      NMETH(NPAR)=NGAUSS(I)
      CALL QUADSET(NPAR,BET1,BET2,NSIMP)
      NMETH(NPAR)=I
      END IF

      NMETH(NPAR)=METH
      RETURN
      END
C
      SUBROUTINE INTEGR1(METH,NPAR,FUNCT,X1,X2,EPSABS,EPSREL,
     > SUM,SUM1,ABSERR,IFAIL)
C====
C==== 9/8/99 change use of IFAIL, only use now by G-K quadpack
C====  reset to zero AFTER printing error message, else cause skip
C====  of actual integrations in dqK61TWO etc
C====
C==== NOTE - only this routine sorts out F(Q) & F(Q)**2, so the part for
c==== some methods is different here ! SOME MORE WORK WILL BE NEEDED TO
C==== GET F(Q) OTHER THAN FDR ONE DIMENSIONAL INTEGRALS !
C====
c==== 12/6/96 pass NPAR also, to point to correct storage for own
c==== Gaussian quadratures.  Note the METH here is not same as in QUADSET
c==== hence we use NGAUSS( ) to look up the corresponding schem in QUADSET
c==== which is used for polydispersity, but not called in the same way as here
c==== since this part was all changed to copy NAG !
c
      INCLUDE 'FISHDIM.PAR'
      EXTERNAL FUNCT
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
c==== NOTE need /rodsetc/ for DSUM,  
c==== NPAR in /rodsetc/ is the first card for the model, those passed
c==== through to INTEGRn() routines may vary !
c==== NOTE: change names of ABSERR, IFAIL & NPAR
c====  which are passed through call here !
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR00,ABSERR11,KOUNT,KOUNT1,KOUNT2,IFAIL00,
     > PSIQ,PHI,STH,NPAR00,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
c==== this in fishqp1.f      DATA NWK1/2001/,NIWK1/500/
      IFAIL=0
      SUM=0.0
      SUM1=0.0
      ABSERR=-9999.
      IF(METH.LE.NSIMPLE)THEN
c==== RKH's own quadrature routine - can be called "recursively" since
c==== the weights & abscissae are set up in separate storage for each
c==== dimension of an integration.
c==== of what NEEDF1 is
      I=IB(IQ(NPAR))
      DO J=1,NGPT(IQ(NPAR))
      SUM=SUM+WTR(I+J)*FUNCT(RG(I+J))
      SUM1=SUM1+WTR(I+J)*DSUM
      END DO            
C
      ELSE IF(METH.EQ.7)THEN
      CALL QROMB(FUNCT,X1,X2,SUM,EPSREL)
C
      ELSE IF(METH.LE.10)THEN
      CALL dqagsONE(FUNCT,X1,X2,EPSABS,EPSREL,SUM,ABSERR,
     *            NEVAL,IFAIL,METH-8)
      END IF
      RETURN
      END
CXXXXXX check passing of NEVAL 
C
C
      SUBROUTINE INTEGR2(METH,NPAR,FUNCT,X1,X2,EPSABS,EPSREL,
     > SUM,SUM1,ABSERR,IFAIL)
C==== ALMOST duplicate of INTEGR1, but does not sum up F(Q), only F(Q)**2 
c==== calls second version of gauss-Kronrod etc
C==== N.B. common block name changes, though arrays are called the same !
      INCLUDE 'FISHDIM.PAR'
      EXTERNAL FUNCT
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
c==== this in fishqp2.f      DATA NWK1/2001/,NIWK1/500/
      SUM=0.0
      SUM1=0.0
      ABSERR=-9999.
      IF(METH.LE.NSIMPLE)THEN
c==== RKH's own quadrature routine - can be called "recursively" since
c==== the weights & abscissae are set up in separate storage for each
c==== dimension of an integration.
c==== SOME routines ( not RODSHEAR) pass F(Q) back via DSUM, regardless
c==== of what NEEDF1 is
      I=IB(IQ(NPAR))
      DO J=1,NGPT(IQ(NPAR))
      SUM=SUM+WTR(I+J)*FUNCT(RG(I+J))
C==== not used here !!!    SUM1=SUM1+WTR(I+J)*DSUM
      END DO            
C
      ELSE IF(METH.EQ.7)THEN
      CALL QROMBtwo(FUNCT,X1,X2,SUM,EPSREL)
C
      ELSE IF(METH.LE.10)THEN
      CALL dqagsTWO(FUNCT,X1,X2,EPSABS,EPSREL,SUM,ABSERR,
     *            NEVAL,IFAIL,METH-8)
      END IF
      RETURN
      END
C
      SUBROUTINE INTEGR3(METH,NPAR,FUNCT,X1,X2,EPSABS,EPSREL,
     > SUM,SUM1,ABSERR,IFAIL)
C==== ALMOST duplicate of INTEGR1, but does not sum up F(Q), only F(Q)**2 
C==== calls third version of Gauss-Kronrod etc,
c====  since not all FORTRAN can call recursively
C==== N.B. common block name changes, though arrays are called the same !
      INCLUDE 'FISHDIM.PAR'
      EXTERNAL FUNCT
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
c==== this in fishqp3.f      DATA NWK1/2001/,NIWK1/500/
      SUM=0.0
      SUM1=0.0
      ABSERR=-9999.
      IF(METH.LE.NSIMPLE)THEN
c==== RKH's own quadrature routine - can be called "recursively" since
c==== the weights & abscissae are set up in separate storage for each
c==== dimension of an integration.
c==== SOME routines ( not RODSHEAR) pass F(Q) back via DSUM, regardless
c==== of what NEEDF1 is
      I=IB(IQ(NPAR))
      DO J=1,NGPT(IQ(NPAR))
      SUM=SUM+WTR(I+J)*FUNCT(RG(I+J))
c==== not used here !!!     SUM1=SUM1+WTR(I+J)*DSUM
      END DO            
C
      ELSE IF(METH.EQ.7)THEN
      CALL QROMBthree(FUNCT,X1,X2,SUM,EPSREL)
C
      ELSE IF(METH.LE.10)THEN
      CALL dqagsTHREE(FUNCT,X1,X2,EPSABS,EPSREL,SUM,ABSERR,
     *            NEVAL,IFAIL,METH-8)
      END IF
      RETURN
      END
C
      SUBROUTINE INTPIC3(NPAR1)
C==== HELP USER TO CHOOSE BEST INTEGRATION SCHEME BY DOING TRIAL CALCULATIONS
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan, at RAL, 21/5/96
C==== special version for multi-dimensional integrals
      EXTERNAL SAUPE
      INCLUDE 'FISHDIM.PAR'
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
c==== data statements for INTNAME are in subroutine INTPIC( )
      NPAR=NPAR1
      PIBY2=2.0*ATAN(1.0)
      PI=2.0*PIBY2
      QQT=0.0
      WRITE(JS,51)(I,INTNAME(I),I=1,NTOT)
   51 FORMAT(1X,I3,5X,A24)
C
      IF(LM(NPAR).EQ.18.AND.(LTYP(NPAR).EQ.41.OR.LTYP(NPAR).EQ.51.
     > OR.LTYP(NPAR).EQ.61.OR.LTYP(NPAR).EQ.71))THEN

 2010 WRITE(JS,2101)
 2101 FORMAT(1X,'CHOOSE INTEGRATION SCHEME for MAIER-SAUPE',/,
     > 1x,'NORMALISATION = Integral( exp(RMSPE*X**2) ) over X = 0 to 1 ',/,
     *' integration scheme M4 (-11 to test all) = ',$) 
c      READ(IS,*,ERR=10)M1
c     integrationscheme
      M1=10
C
      IF(M1.GE.1.AND.M1.LE.NTOT)GOTO 2210
      IF(M1.GT.-11)GOTO 2010
      WRITE(JS,2502)
 2502 FORMAT(1X,'Maier-Saupe width RMSPE = ',$)
      READ(IS,*)RMSPE
      WRITE(JS,1041)
      READ(IS,*)NSIMP
      WRITE(JS,2501)
 2501 FORMAT(1X,'Enter absolute (for QA02) & relative errors for adaptive',/,
     >  ' integrations  EPSABS,EPSREL ( try 0.001, 0.001) = ?',$)
      READ(5,*,ERR=10)EPSASPE,EPSRSPE
      DO I=1,NTOT
      KOUNT=0
      NMETH(NPAR+3)=I
C==== set up wts etc
      IF(I.LE.NSIMPLE)THEN
      NMETH(NPAR+3)=NGAUSS(I)
      CALL QUADSET(NPAR+3,0.0,1.0,NSIMP)
      NMETH(NPAR+3)=I
      END IF
      ABSERR=0.0
      ABSERR1=0.0
      ZER=0.0
      ONE=1.0
      CALL INTEGR1(I,NPAR+3,SAUPE,ZER,ONE,EPSASPE,EPSRSPE,
     >   SUM,SUM1,ABSERR,IFAIL)
      WRITE(JS,2504)I,INTNAME(I),KOUNT,SUM
 2504 FORMAT(1X,I2,1X,A24,I8,
     *' S=',1pe15.8)

      IF(I.GT.NSIMPLE.AND.I.LE.NTOT)WRITE(JS,2503)ABSERR
 2503 FORMAT(1X,39X,'+-',1PE12.4)
c====      END IF
      END DO
C====
      GOTO 2010

 2210 IF(M1.EQ.1)THEN
      WRITE(JS,2041)
 2041 FORMAT(1X,'NSIMP Maier-Saupe = ',$)
      READ(IS,*)NSIMP
      ELSE IF(M1.GT.NSIMPLE)THEN
      WRITE(JS,2501)
c      READ(5,*,ERR=10)EPSASPE,EPSRSPE
c     integrationscheme
      EPSASPE=0
      EPSRSPE=0
      END IF
      IF(M1.LE.NSIMPLE)THEN
C==== temporarily set NMETH to get correct RKH quadrature
      NMETH(NPAR+3)=NGAUSS(M1)
      CALL QUADSET(NPAR+3,0.0,1.0,NSIMP)
      END IF
C==== finally set NMETH to suit INTEGR1 etc.
      NMETH(NPAR+3)=M1
      END IF
      PREVRMSPE=-999999.
c
   10 IF(LM(NPAR).EQ.18)THEN
      WRITE(JS,1010)
 1010 FORMAT(1X,'CHOOSE INTEGRATION SCHEMES FOR SHEARED RODS',/,
     > 1x,' ( SET K6=-12 TO GET HERE )',/,
     * 1X,' (a) for sin(THETA)dTHETA,')
      IF(LTYP(NPAR).EQ.61.OR.LTYP(NPAR).EQ.71)THEN
      WRITE(JS,1012)
      ELSE
      WRITE(JS,1011)
      END IF
 1011 FORMAT(1X,' (b) for dPHI ( ignored when PSI=90)',/,
     > 1X,' (c) for angle PSI of Q to x axis (ignored if DPSIQ=0)',/,
     > 1X,' [try 1 (359 pts), 9, 2 or 9,9,2 ]:  M1,M2,M3= ',$) 
 1012 FORMAT(1X,' (b) for dPHI ',/,
     > 1X,' (c) for angle PSI of Q to x axis (ignored if DPSIQ=0)',/,
     > 1X,' [ NOTE value of PSI is ignored, so DPSIQ should be zero]',/,
     > 1X,' M-S end-on needs 2d integration, TRY & TEST CAREFULLY  ',/,
     > 1X,'   1 (with 359 pts), 1 (359), 2  ]:  M1,M2,M3= ',$) 
C
c      READ(IS,*,ERR=10)M1,M2,M3
c     intgrationscheme
      M1=9
      M2=9
      M3=2
      ELSE
      RETURN
      END IF
c
      IF(M1.GT.NSIMPLE.OR.M2.GT.NSIMPLE.OR.M3.GT.NSIMPLE)THEN
      WRITE(JS,501)
  501 FORMAT(1X,'absolute & relative errors for adaptive',/,
     >  ' integrations ( at present are same for all 2 or 3 loops)',/,
     >  ' EPSABS,EPSREL ( try 0.01 & test !, 0.001) ',/,
     >  ' [EPSABS is fudged in sheared rods]     = ?',$)
      READ(5,*,ERR=10)EPSABS,EPSREL
      IF(LM(NPAR).EQ.18)THEN
      EPSASHR=EPSABS
      EPSRSHR=EPSREL
      ELSE
      RETURN
      END IF
      END IF
 
      IF(LM(NPAR).EQ.18)WRITE(JS,107)
  107 FORMAT(1X,'NOTE SHEARED RODS only calculates F(Q)**2 not F(Q)',/,
     > 1X,'so an S(Q) will not be corrected. ',$)
C      READ(IS,*)NEEDF1
C
C==== set up quadratures for sheared rods
      BET1=0.0
      IF(M1.LE.NSIMPLE)THEN
      IF(M1.EQ.1)THEN
      WRITE(JS,1041)
 1041 FORMAT(1X,'NSIMP theta = ',$)
      READ(IS,*)NSIMP
      END IF
C
      BET2=PI
C==== model dependent range check here !
      IF( ABS(V(NPAR+6)-90.).LE.0.0001.AND.
     >    ABS(V(NPAR+7)).LE.0.001)BET2=PIBY2
C      PREVPSIQ=V(NPAR+6)  REMOVED RKH 9/4/97
C      PREVDPSIQ=V(NPAR+7)
      IF(LTYP(NPAR).EQ.61.OR.LTYP(NPAR).EQ.71)THEN
C==== M-Saupe end-on, ignore psi values
      BET2=PIBY2
C      PREVPSIQ=0.0
C      PREVDPSIQ=0.0
      END IF
C
      NMETH(NPAR)=NGAUSS(M1)
      CALL QUADSET(NPAR,BET1,BET2,NSIMP)
      END IF
C
      IF(M2.LE.NSIMPLE)THEN
      IF(M2.EQ.1)THEN
      WRITE(JS,1042)
 1042 FORMAT(1X,'NSIMP phi = ',$)
      READ(IS,*)NSIMP
      END IF
C
      BET2=2.0*PI
C
      IF(LTYP(NPAR).EQ.61.OR.LTYP(NPAR).EQ.71)THEN
C==== M-Saupe end-on
      BET2=PIBY2
      END IF
c
      NMETH(NPAR+1)=NGAUSS(M2)
      CALL QUADSET(NPAR+1,BET1,BET2,NSIMP)
      END IF
C
      IF(M3.LE.NSIMPLE)THEN
      IF(M3.EQ.1)THEN
      WRITE(JS,1043)
 1043 FORMAT(1X,'NSIMP psi = ',$)
      READ(IS,*)NSIMP
      END IF
C
      BET1=V(NPAR+6)-V(NPAR+7)
      BET2=V(NPAR+6)+V(NPAR+7)
C      PREVPSI1=BET1
C      PREVPSI2=BET2
C
      IF(ABS(V(NPAR+7)).GT.0.00001)THEN
      NMETH(NPAR+2)=NGAUSS(M3)
      CALL QUADSET(NPAR+2,BET1,BET2,NSIMP)
      END IF
      END IF
C
      NMETH(NPAR)=M1
      NMETH(NPAR+1)=M2
      NMETH(NPAR+2)=M3
C
      WRITE(JS,102)
  102 FORMAT(1X,'Test integrations now ? ( ans 1 ) ',$)
      READ(IS,'(I1)')I
      IF(I.NE.1)GOTO 210
      WRITE(JS,131)
  131 FORMAT(1X,'Integrate at Q= Q1 to Q2 step Q3;  Q1,Q2,Q3 = ',$)
      READ(5,*)QQQ1,QQQ2,QQQ3
C
      I=(QQQ2-QQQ1)/QQQ3+1
      QQT=QQQ1
      DO II=1,II
      IF(LM(NPAR).EQ.18)THEN
      SUM=RODSHEAR(QQT,V(NPAR+1),DSUMF,NPAR,LTYP(NPAR))
      WRITE(JS,499)EPSASHR
  499 FORMAT(1X,'EPSASHR=',1PE12.4)
      ELSE
      SUM=0.0
      END IF
C==== XXXYYY sort out the KOUNT's ! and the ABSERR !
  503 FORMAT(1X,3I3,1X,F10.5,1X,
     *' F^2=',1pe13.6,15X,3I7)

      IF(NMETH(NPAR).GT.NSIMPLE.AND.NMETH(NPAR).LE.NTOT)THEN
      WRITE(JS,504)m1,m2,m3,QQT,SUM,abserr,KOUNT2,KOUNT1,KOUNT
      ELSE
      WRITE(JS,503)m1,m2,m3,QQT,SUM,KOUNT2,KOUNT1,KOUNT
      END IF

  504 FORMAT(1X,3I3,1X,F10.5,1X,
     *' F^2=',1pe13.6,'+-',E13.6,3I7)
      QQT=QQT+QQQ3
      END DO
C====
      WRITE(JS,601)
  601 FORMAT(1X,' Use these choices ? ( ans 1) ',$)
      READ(IS,*)I
      IF(I.NE.1)GOTO 10
c==== the way out is here, BUT first look for Maier-Saupe form:
  210 RETURN
      END
C
      FUNCTION SAUPE(X)
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      SAUPE=EXPSPEC(RMSPE*X*X)
      KOUNT=KOUNT+1
      RETURN
      END
c
      REAL*4 function para3(Q,LTYPEMODEL,LTY,LTX,V)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 Q,sld1,sld2,sld3,xbar,ybar,sigx,sigy
      REAL*4 V(9)
c==== 2 phase, N layer =V(1), paracrystal with background medium:
c====
C==== 16/03/04 if LTYPMODEL=21 then V(9) is Lorentz Rsigma
c====          else V(9) is not to be used!
c====
c==== The distribution of thicknesses of the two layer types
c==== are described by the 1-d Fourier transform of the DISTRIBUTION.
c==== Layers themselves are infinite, flat, sheets with sharp boundaries.
c====
c==== NOTE sld2 is for peak at position = Ybar
c====      sld1 is for dip  at position = Xbar=Dspace-Ybar
c====      sld3 is for the medium 
c==== 
c==== LTY=5 for Gaussian of sigy,  15 for top hat width 
c==== LTX=8  "       "   "  sigx   18  "   "   "   "
C==== 
C==== the equations are not symmetrical, set sld3=sld1 if medium
C==== is of same scattering density.  The fit here is "over determined"
c==== in order to simplify constraints on the sld's.  You may only
c==== be able to fit (sld3-sld1)/(sld2-sld1)
c====
c==== Some of this modified from DALLFIT.FOR of P.Jenkins & T.Waigh,
c==== Cambridge.                               R.K.Heenan RAL 30/9/97
C==== Equations are based on 
c==== W.Wenig & R.Bramer, Colloid & Polymer Sci. 256(1978)125-132
C==== ( NOTE missing term in their eq 3, see the appendix instead)
c==== using methodology and notation of 
C==== I.H.Hall, E.A.Mahmoud, P.D.Carr and Y.D.Geng, Colloid & Polymer Sci.
c==== 265(1987)383-393
c==== which assumes background medium has scattering density sld3=sld1
C==== RKH has checked eqns 4 in the latter, which agree with here once 
c==== psi & phi are swapped and we multiply, not divide, by G in (4d),
c==== as spotted already at Cambridge.  The new cross terms added
c==== by Paul Jenkins at Cambridge have been checked and found OK by RKH.
c==== NOTE that initially the Y & X names for peak & dip are opposite to
c==== the Cambridge routine - but AA and PSI still finish up being
c==== the "peak" as they should be !
c
C==== (AA*BB)**NN terms 
c====  in GG,EE and YY only work for INTEGER N  for rectangular distribs.
c==== as AA & BB may easily be NEGATIVE ! 
      PARAMETER(PI=3.14159265358)
C -------------------------------------------------------------------------
      RNN = V(1)
c==== if either phase has top hat distribution, force Nlayers to INTEGER
c==== ( else don't as the least squares can then vary the value of Nlayers )
      NN = NINT(V(1))
      if( LTY .GE.10.or.LTX.GE.10) RNN = DFLOAT(NN)
      sld2=V(2)
      ybar=V(3)
      sigy=V(4)
      sld1=V(5)
      XBAR=v(6)-YBAR
      sigx=V(7)
      sld3=V(8)
c
      if(LTY.LT.10)then
C==== Gaussian
         AA = -0.5*DBLE((Q*sigY)**2)
         AANN = DEXP(AA*RNN)
         AA = DEXP(AA)
      else
c==== Top Hat full width is passed in sigma
         AA = DSIN(DBLE(0.5*Q*SIGY))/DBLE(0.5*Q*SIGY)
         AANN = AA**NN
      END IF
      if(LTX.LT.10)then
         BB = -0.5*DBLE((Q*sigX)**2)
         BBNN = DEXP( BB*RNN)
         BB = DEXP(BB)
      else
         BB = DSIN(DBLE(0.5*Q*SIGX))/DBLE(0.5*Q*SIGX)
         BBNN = BB**NN
      END IF

         PSI = DBLE(Q*YBAR)
         PHI = DBLE(Q*XBAR)
         XX = PSI + PHI
         AABBNN = AANN*BBNN
c
C==== MIGHT speed up if compute all cos's first ??? 
c
c==== these are all as per Hall, except we divide by NN
c
         FF = 1 + (AA*BB)**2 - 2*AA*BB*DCOS(XX)
C
         CC = (1 - (AA*BB)**2 - AA*(1 - BB**2)*DCOS(PSI) -
     >   BB*(1 - AA**2)*DCOS(PHI))
C
         GG = 1 - (AABBNN)*DCOS(RNN*XX)
C
         DD = BB*(((1 - AA**2)*(1 - (AA*BB)**2)*DSIN(XX)*DSIN(PSI))
     >   + ((((1 + ((AA*BB)**2))*DCOS(XX)) -2*AA*BB)
     >   * (((1 + (AA**2))*DCOS(PSI)) - 2*AA)))*GG
C 
        EE = (AABBNN)*BB*DSIN(RNN*XX)*
     >   ((1 - (AA*BB)**2)*((1 + AA**2)*DCOS(PSI) - 2*AA)*DSIN(XX)
     >   - (1 - AA**2)*((1 + (AA*BB)**2)*DCOS(XX) - 2*AA*BB)*DSIN(PSI))
c
c====  now for the new cross terms
         WW = GG*(1 + ((AA*BB)**2) + BB*(1 + (AA**2))*DCOS(PSI-XX) -
     >   AA*(1 + (BB**2))*DCOS(PSI) - 2*AA*BB*DCOS(XX))
C
         YY = -(AABBNN)*DSIN(RNN*XX)*
     >    (BB*(1 - (AA**2))*DSIN(PSI-XX)
     >   + AA*(1 - (BB**2))*DSIN(PSI))
C
C==== now add together with sld differences

         PP = (2.0*DBLE( SLD1 -SLD2)**2)*( CC/FF + (DD+EE)/(RNN *FF**2))

         ZZ = 2.0*DBLE(sld1-sld3)*DBLE(sld1-sld2)*(WW + YY)/(FF*RNN)

         ZP = 2.0*(DBLE(sld1-sld3)**2)*GG/RNN

C -------------------------------------------------------------------------
        sum = PP + ZZ + ZP
c==== finally divide by Q**2
c==== 17/03/04 add Lorentz term
        if(ltypemodel.eq.11)then
        para3 = SNGL(sum) /Q**2
        else if(ltypemodel.eq.21)then
        para3 = SNGL(sum) /(Q**2*(1.0+0.5*(Q*V(9))**2))
        else
        para3 = 0.0e+00
        end if
      return
      end
c
      REAL*4 function RKCHEN(Q1,R1,SIGR1,F1)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 Q1,R1,SIGR1,F1
c==== this subroutine calculates the KOTLARCHYK & CHEN ANALYTIC SCHULTZ
C====  M.Kotlarchyk & S-H.Chen, J.Chem.Phys. 79(1983)2461-2469
c==== 				RKH 18/04/2002
c====  RKH 19/402 THIS VERSION USES A**B NOT PP(A,B)
c====
      Q=DBLE(Q1)
      R=DBLE(ABS(R1))
      SIGR=DBLE(ABS(SIGR1))
c
      PI = 2.D+00*DASIN(1.D+00)
      Z1 = (1.0D+00/SIGR)**2
      Z = Z1-1.0D+00
      AL = Z1/(Q*R)
c==== rescale so that fitted scale is usual 10**-24 phi (del rho)**2
c==== as per model 6, see manual !  Ensure RESCALE is +ve else get sqrt(-ve) !
      RESCALE= ( 4.0D+00*PI*R**3/3.00D+00 ) /
     >  ((1.0D+00 + SIGR**2)*(1.0D+00 + 2.0D+00*SIGR**2 ) )
C      
C==== <F**2>, NOTE move (alpha*(z+1) ) from eqn 25 into eqn 27 of K & Chen
c====  and rearrange the alpha terms, as alpha gets large !
      AL4= 1.0D+00 + 4.0D+00/(AL**2)  
      AT2A = DATAN(2.0D+00/AL)
      C1 = 4.5D+00*(AL/Z1)**6
      G11 = 1.0D+00
      G12A = -AL4**( -0.5D+00*Z1 )
      G12B = DCOS( Z1*AT2A )
      G13 = (Z+2.0D+00)*Z1/(AL**2)
      G14A = (Z+2.0D+00)*Z1*AL4**( -0.5D+00*(Z+3.0D+00) )/(AL**2)
      G14B = DCOS( (Z+3.0D+00)*AT2A )
      G15A = -2.0D+00*Z1*AL4**( -0.5D+00*(Z+2.0D+00) )/AL
      G15B = DSIN( (Z+2.0D+00)*AT2A )
      F2 = RESCALE*C1*(G11 +G12A*G12B +G13 +G14A*G14B +G15A*G15B )
      RKCHEN = SNGL(F2)
C
c      IF(IBUG.GE.1)THEN
c      WRITE(JF,1001)Q,F2,C1,G11,G12A,G12B,G13,G14A,G14B,G15A,G15B
c      END IF

C====
C==== <F> as derived from K & Chen eqn for beta(Q)
c====  and rearranged the alpha terms, as alpha gets large !
      AT1A = DATAN(1./AL)
      C2 = 3.0D+00*( 1.0D+00 + 1.0D+00/AL**2)**( -0.5D+00*Z1 )/(R*Q)**3
      G21 = DSIN( Z1*AT1A )
      G22 = -Z1/DSQRT( 1.0D+00 + AL**2 )* DCOS( (Z+2.0D+00)*AT1A )
      F = DSQRT(RESCALE)*C2*( G21 + G22 )
      F1= SNGL(F)
C
c      IF(IBUG.GE.1)THEN
c      WRITE(JF,1001)Q,F,C2,G21,G22A,G22B
c      END IF
c1001  FORMAT(1X,F7.4,1P11D10.2)
      RETURN
      END
c
c==== 19/4/02 RKH, not used but may be useful ?
C      REAL*8 function PP(A,B)
C      REAL*8 A,B,C
C
C      C=DLOG(A)*B
C      PP=DEXPSPEC(C)
C
C      end
c
c==== 19/4/02 RKH, not used but may be useful ?
C      REAL*8 FUNCTION DEXPSPEC(A)
C      REAL*8 A
C==== expspec avoids underflows invoking the error handler
c==== 19/04/2002 RKH tested on Compaq/DEC Alpha
c==== the CONSTANTS here are machine dependent numbers
C      IF(A.GE.-709.8D+00.AND.A.LE.709.08D+00)THEN
C       DEXPSPEC=DEXP(A)
C      ELSE IF(A.GT.709.08D+00)THEN
C==== N.B. overflow stops program whilst underflow does not !
C       WRITE(6,101)A
C101    FORMAT(1X,'overflow in DEXPSPEC',f10.3)
C      DEXPSPEC=8.90289D+307
C      ELSE
C      DEXPSPEC=0.0d+00
C      END IF
C
C      RETURN
C      END
C
c
      function R_KHOL(Q1,RN,ELL,RAX,DELR1,CONT1,POLY,DSUMF,NPAR1,LTYP)
C==== I(Q) for Kholodenko worm  R.K.Heenan 8/10/2002,  
c====       guinier Rax is        LTYP=71
C==== 25/10/02 add core/shell cyl LTYP=81
c====  A.L.Kholodenko, Macromolecules 26(1993)4179-4183
c==== see also e.g. P.Hickl, M.Ballauff, U.Scherf, K.Mullen & P.Lindner
c==== Macromolecules 30(1997)273-279
      EXTERNAL R_KHOLSUB,R_KHOLSUB2,VOLELLFUN
      INCLUDE 'FISHDIM.PAR'
C====      DIMENSION VAR(7)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
c==== need these for NSIMPLE and NMETH
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
c
c==== borrow some other params to pass through common
      LTYPROD=LTYP
      QQ=Q1
      XELL=ABS(ELL)
      QL3 = ABS(Q1*ELL/3.0)
      RN3 = 3.0*ABS(RN)
c
C==== do the CONSTANT cross sectional bit here
      IF(RAX.GE.1.0.AND.LTYPROD.EQ.71)THEN
C==== Rax is radial radius gyration
      PCXF=EXPSPEC(-0.25*(QQ*RAX)**2)
C
      ELSE IF(LTYPROD.EQ.81)THEN
c==== core/shell cylinder, R1 is in Rax, R2 = R1+DELR1,
c====  CONT is (rho2 - rho3)/(rho1 -rho3), take care for hollow cylinder !
      IF(ABS(RAX).LT.0.001)THEN
      WRITE(JS,1001)
      WRITE(JF,1001)
1001  FORMAT(1X,'R1 set to 0.01 to avoid divide by zero')
      RAX=0.01
      END IF
C
      PCXF=(1.0-CONT1)*TRJ1XBYX(Q1*RAX) + 
     >      CONT1*TRJ1XBYX(Q1*(RAX+DELR1))*( (RAX+DELR1)/RAX )**2
      ELSE
C==== Kholdenko rod alone
      PCXF=1.0
      END IF
C
      PCXF2=PCXF**2
C
C==== integration schemes M1,M2, are for y (z in paper) variable in P(Q)
c====    and for polydisp length Lcontour = n*ELL
      NPAR=NPAR1
      M1=NMETH(NPAR)
      M2=NMETH(NPAR+1)
      IF1KHOL=0
c
      KOUNT=0
      ZER=0.0
      IF(ABS(POLY).LT.0.01 ) THEN

      IF(RAX.GE.1.0.AND.LTYPROD.EQ.71)THEN
C==== Rax is radial radius gyration, note vol assumes cylinder R = sqrt(2)xRg
      VOL = ABS(3.141592654*2.*RAX*RAX*RN*XELL)
C
      ELSE IF(LTYPROD.EQ.81)THEN
c==== core/shell cylinder, R1 is in Rax, R2 = R1+DELR1,
c==== use VOL for CORE ONLY,for scale=PHIcore.(rho1 - rho3)**2
      VOL = ABS(3.141592654*RAX*RAX*RN*XELL)
      ELSE
C==== Kholdenko rod alone
      VOL = ABS(RN*XELL)
      END IF
C
C==== may need to re-do the QUADSET if RN3 has changed at all
      IF(M1.LE.NSIMPLE.AND.ABS(RN3-PREVRN3).GT.0.01)THEN
C====                            get back NSIMP to re-use here
      NSIMPSON=NGPT(IQ(NPAR))
c==== this actually works !  - old fudge here needs sorting properly !
      M1=NMETH(NPAR)
      NMETH(NPAR)=NGAUSS(M1)
      CALL QUADSET(NPAR,ZER,RN3,NSIMPSON )
      NMETH(NPAR)=M1
      PREVRN3=RN3
      END IF
C==== monodisperse case, single integration over y
      CALL INTEGR1(M1,NPAR,R_KHOLSUB,ZER,RN3,
     >           EPSAKHOL,EPSRKHOL,SUM,SUM1,ABSERR,IFAIL)
c
c==== INTEGR1 does add up F(Q) for simple cases BUT here
c==== F(Q)=sqrt(sum(kohlsub() ) ) NOT sum( sqrt(kohlsub() )
c==== so in both integration methods we have simply
      DSUMF = SQRT(ABS(SUM))*PCXF*VOL
      R_KHOL = SUM*PCXF2*VOL
c
      RETURN
C
      ELSE
C
C==== double integration, OVER POLYDISPERSITY
c====  for P(Q) = < F(Q)**2 >
      KOUNT1=0
c==== borrow these to pass through common to ELLFUN() and VOLELLFUN()
      R=RAX
      RL=ABS(RN*ELL)
      DELR=ABS(POLY)*RL
C==== INTPIC2 allows to check this is adequate for both integrations
      ELLMAX= RL + WMAXKHOL*DELR
      ELLMIN= AMAX1(0.0, RL - WMAXKHOL*DELR)
c
C==== may need to re-do the QUADSET if ELLMAX has changed much
      IF(M2.LE.NSIMPLE.AND.ABS((ELLMAX-PREVELLMAX)).GT.DELR)THEN
C====                                get back NSIMP to re-use here
      NSIMPSON=NGPT(IQ(NPAR+1))
c==== this works !
      M2=NMETH(NPAR+1)
      NMETH(NPAR+1)=NGAUSS(M2)
      CALL QUADSET(NPAR+1,ELLMIN,ELLMAX,NSIMPSON )
      NMETH(NPAR+1)=M2
      PREVELLMAX=ELLMAX
      END IF
c
c==== first get VOLUME to normalise polydispersity function to unit volume
c==== or unit length if Rax <1
c==== using same integration scheme as polydispersity
c==== could do this once only per RUN, but its not much compared to
c==== the next, double, integration and doing it here makes the deriv calcs simpler.
      CALL INTEGR1(M2,NPAR+1,VOLELLFUN,ELLMIN,ELLMAX,EPSAKHOL,EPSRKHOL,
     >    VOL,SUM1,ABSERR,IFAIL)
C
C==== now get F(Q)**2 and for simple cases F(Q) also
      CALL INTEGR1(M2,NPAR+1,R_KHOLSUB2,ELLMIN,ELLMAX,EPSAKHOL,EPSRKHOL,
     >    SUM,SUM1,ABSERR,IFAIL)
c
c==== include cross sectional P(Q)
      SUM = SUM*PCXF2
      SUM1 = SUM1*PCXF
C==== * by VOL to get scale as phi(deltat rho)**2
      DSUMF = SUM1*VOL
      R_KHOL = SUM*VOL 
C==== 
      IF(M2.GT.NSIMPLE.AND.NEEDF1.EQ.1)THEN
c==== repeat calc for N(ELL)*F(Q) if <F(Q)> needed
      KOUNT1=0
      IF1KHOL = 1
      CALL INTEGR1(M2,NPAR+1,R_KHOLSUB2,ELLMIN,ELLMAX,EPSAKHOL,EPSRKHOL,
     >    SUM,SUM1,ABSERR,IFAIL)
      DSUMF = SUM*PCXF*VOL
      END IF

      RETURN

      END IF

      END
C
      FUNCTION R_KHOLSUB2(ELLCONT)
      EXTERNAL R_KHOLSUB
      INCLUDE 'FISHDIM.PAR'
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      R_KHOLSUB2 = ELLFUN(ELLCONT)
C==== NB redefine RN3 for each contour ELLCONT using ELL passed through common
      RN3 = 3.0*ELLCONT/XELL
      ZER=0.0
C==== may need to re-do the QUADSET if RN3 has changed at all
      IF(M1.LE.NSIMPLE.AND.ABS(RN3-PREVRN3).GT.0.01)THEN
C====                            get back NSIMP to re-use here
      NSIMPSON=NGPT(IQ(NPAR))
      M1=NMETH(NPAR)
      NMETH(NPAR)=NGAUSS(M1)
      CALL QUADSET(NPAR,ZER,RN3,NSIMPSON )
      NMETH(NPAR)=M1
      PREVRN3=RN3
      END IF
c
      CALL INTEGR2(M1,NPAR,R_KHOLSUB,ZER,RN3,EPSAKHOL,EPSRKHOL,
     >   SUM,SUM1,ABSERR,IFAIL)
      KOUNT1=KOUNT1+1
C==== compute F(Q) for simple integration to sum over polydisp
      DSUM = R_KHOLSUB2*SQRT(ABS(SUM))
      IF(IF1KHOL.EQ.1)THEN
c==== return F(Q) for other integrations
      R_KHOLSUB2=DSUM
      ELSE
c==== return F(Q)^2 for sum over polydisp to use
      R_KHOLSUB2=R_KHOLSUB2*SUM
      END IF
C
      RETURN
      END
C
      FUNCTION ELLFUN(ELLCONT)
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
C==== RL has mean L and DELR SIGMA
c==== use LTYPROD to pick other distributions
      ELLFUN=SCHULTZ(ELLCONT,DELR,RL)
      RETURN
      END
C
      FUNCTION VOLELLFUN(ELLCONT)
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
C==== R has Rax, the axial Rg radius of gyration,  for a disc R = sqrt(2)xRg
c==== so in ignorance of the proper way to do this, lets assume volume
c==== is contour length L times pi x 2 x (Rg**2)
c==== RL has mean L and DELR SIGMA
c==== use LTYPROD to pick other distributions
      IF(LTYPROD.EQ.71.AND.ABS(R).GE.1.0)THEN
      VOLELLFUN = 3.141592654*2.*R*R*ELLCONT*SCHULTZ(ELLCONT,DELR,RL)
      ELSE  IF(LTYPROD.EQ.81)THEN
      VOLELLFUN = 3.141592654*R*R*ELLCONT*SCHULTZ(ELLCONT,DELR,RL)
      ELSE
      VOLELLFUN = ELLCONT*SCHULTZ(ELLCONT,DELR,RL)
      END IF
      RETURN
      END
c
      FUNCTION R_KHOLSUB(YY)
C==== integrand for P(Q) in Kholodenko worm  
C==== R.K.Heenan 26/9/2002, 6/1/03 oops, change the sign of I2 term !
c==== ( thanks to Vania Croce, Bristol, for spotting this)
c====  A.L.Kholodenko, Macromolecules 26(1993)4179-4183
C==== get QL3 via common
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      IF(QL3.LE.1.0)THEN
      EE=SQRT(1.0 -  QL3**2 )
C==== note sinh(x) = 0.5*(exp(x) - exp(-x) )
c==== for large x this overflows
c==== but  sinh(EY)/sinh(Y) goes to exp(YE)/exp(Y) = exp( Y*(E-1) )
c==== where E is generally around 1
C==== There is a problem at exactly QL3=1 when EE = 0
C====  sinh(x) = x + x^3/3! + x^5/5! + x^7/7! ...
c====  Y*(sinh(EY)/EY) goes to Y*(1.0) = Y
c
        IF(YY.LT.68.)THEN
         IF(ABS(YY*EE).LT.1.0E-24)THEN
          IF(ABS(YY).LT.1.E-8)THEN
          R_KHOLSUB = (2./RN3)*(1.0 - YY/RN3)
          ELSE
          R_KHOLSUB = (2./RN3)*(1.0 - YY/RN3)*YY/SINH(YY)
          END IF
         ELSE
         R_KHOLSUB = (2./RN3)*(1.0 - YY/RN3)*SINH(YY*EE)/(EE*SINH(YY))
         END IF
        ELSE
c==== this can sometimes underflow with large RN and large YY
        R_KHOLSUB = (1.0-YY/RN3)*EXPSPEC( YY*(EE-1.0) - ALOG(RN3/2.0) )/EE
        END IF
      ELSE
c
C====     QL3 > 1
      FF=SQRT( QL3**2 -1.0 )
        IF(YY.LT.68.)THEN
         IF(ABS(YY*FF).LT.1.0E-24)THEN
          IF(ABS(YY).LT.1.E-8)THEN
          R_KHOLSUB = (2./RN3)*(1.0 - YY/RN3)
          ELSE
          R_KHOLSUB = (2./RN3)*(1.0 - YY/RN3)*YY/SINH(YY)
          END IF
         ELSE
c==== according to orig paper, the high Q eqn really is sin() not sinh()
         R_KHOLSUB = (2./RN3)*(1.0-YY/RN3)*SIN(YY*FF)/(FF*SINH(YY))
         END IF
        ELSE
c==== since RN3 can be large this forces underflow even when EXPSPEC(YY)
c==== sends back an OK number, so push the 1/RN3 inside EXPSPEC
        DD  = EXPSPEC( YY+ALOG(RN3)+ALOG(FF) )
          IF(DD.LT.1.E37)THEN
          R_KHOLSUB = (1.0 - YY/RN3)*SIN(YY*FF)/DD
          ELSE
          R_KHOLSUB = 0.0
          END IF
        END IF
      END IF
      RETURN
      END 
C
      SUBROUTINE INTPIC2(NPAR1)
C==== HELP USER TO CHOOSE BEST INTEGRATION SCHEME BY DOING TRIAL CALCULATIONS
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan, at RAL, 21/5/96
C==== 11/10/2002 special version for polydisperse coils etc
      INCLUDE 'FISHDIM.PAR'
      CHARACTER*24 INTNAME
      COMMON/INTEG/INTNAME(12),NGAUSS(12),NSIMPLE,NTOT
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
c==== data statements for INTNAME are in subroutine INTPIC( )
      NPAR=NPAR1
   10 WRITE(JS,51)(I,INTNAME(I),I=1,NTOT)
   51 FORMAT(1X,I3,5X,A24)
C
      WRITE(JS,1010)
 1010 FORMAT(1X,'CHOOSE INTEGRATION SCHEMES for KHOLODENKO ROD',/,
     > 1x,' ( K6=-12 TO GET BACK HERE LATER )',/,
     * 1X,' (a) for P(Q) calculation,'/,
     > 1X,' (b) for polydispersity ',/,
     > 1x,'     integration to Lmean + WMAX*(std dev)',/,
     > 1x,' (try 10 10 4.0 )  M1, M2, WMAX = ',$)
C
c      READ(IS,*,ERR=10)M1,M2,WMAXKHOL
c     integrationscheme
      M1=10
      M2=10
      WMAX=4.0
      IF(M1.LE.0.OR.M2.LE.0.OR.M1.GT.NTOT.OR.M2.GT.NTOT)GOTO 10
c
      IF(M1.GT.NSIMPLE.OR.M2.GT.NSIMPLE)THEN
      WRITE(JS,501)
  501 FORMAT(1X,'absolute & relative errors for adaptive',/,
     >  ' integrations (same for integrations)',/,
     >  ' EPSABS,EPSREL ( try 0.001 0.001 & test )     = ?',$)
c      READ(5,*,ERR=10)EPSAKHOL,EPSRKHOL
c     integrationscheme
      EPSAKHOL=0.001
      EPSRKHOL=0.001
      END IF
c
600   IF1KHOL=1
      WRITE(JS,107)
  107 FORMAT(1X,'Do you need to calculate F(Q) as well as F(Q)**2,',/,
     > ' i.e. do you have an S(Q) ? ( 0-no, 1-yes) ',$)
c      READ(IS,*)NEEDF1
c     integrationscheme
      NEEDF1=1
      IF(NEEDF1.LT.0.OR.NEEDF1.GT.1)GOTO 600
 
C==== set up quadratures
C==== NOTE the range of these integrations changes with model parameters
      BET1=0.0
      IF(M1.LE.NSIMPLE)THEN
      IF(M1.EQ.1)THEN
      WRITE(JS,1041)
 1041 FORMAT(1X,'NSIMP for P(Q) = ',$)
      READ(IS,*)NSIMP
      END IF
C
      BET1=0.0
      BET2=3.0*ABS(V(NPAR+1))
      IF(BET2.LT.1)BET2=10.
      PREVRN3=BET2
      NMETH(NPAR)=NGAUSS(M1)
      CALL QUADSET(NPAR,BET1,BET2,NSIMP)
      END IF
C
      IF(M2.LE.NSIMPLE)THEN
      IF(M2.EQ.1)THEN
      WRITE(JS,1042)
 1042 FORMAT(1X,'NSIMP poldispersity = ',$)
      READ(IS,*)NSIMP
      END IF
C====        Lmean ( 1 + polydisp*Lmean)*WMAXKHOL
      BET2=V(NPAR+1)*V(NPAR+2)* (1.0 + AMAX1(0.05,V(NPAR+4))*WMAXKHOL )
      BET1=AMAX1(0.0,V(NPAR+1)*V(NPAR+2)* 
     >               (1.0 - AMAX1(0.05,V(NPAR+4))*WMAXKHOL ) )
      PREVELLMAX=BET2
C
      NMETH(NPAR+1)=NGAUSS(M2)
      CALL QUADSET(NPAR+1,BET1,BET2,NSIMP)
      END IF
C
      NMETH(NPAR)=M1
      NMETH(NPAR+1)=M2
C
      WRITE(JS,102)
  102 FORMAT(1X,'Test integrations now ? ( ans 1 ) ',$)
c      READ(IS,'(I1)')I
      I=0
      IF(I.NE.1)GOTO 210
      WRITE(JS,131)
  131 FORMAT(1X,'Integrate at Q= Q1 to Q2 step Q3;  Q1,Q2,Q3 = ',$)
      READ(5,*)QQQ1,QQQ2,QQQ3
C
      I=(QQQ2-QQQ1)/QQQ3 +1
      QQT=QQQ1
      DO II=1,I
      SUMF=-999.999
      SUM=R_KHOL(QQT,V(NPAR+1),V(NPAR+2),V(NPAR+3),V(NPAR+4),V(NPAR+5),
     >   V(NPAR+6),SUMF,NPAR,LTYP(NPAR))
C==== XXXYYY sort out the KOUNT's ! and the ABSERR !
  503 FORMAT(1X,2I3,1X,F10.5,1X,
     *' F^2=',1pe13.6,15X,' F=',E13.6,2I7)

      IF(NMETH(NPAR).GT.NSIMPLE.AND.NMETH(NPAR).LE.NTOT)THEN
      WRITE(JS,504)m1,m2,QQT,SUM,abserr,SUMF,KOUNT1,KOUNT
      ELSE
      WRITE(JS,503)m1,m2,QQT,SUM,SUMF,KOUNT1,KOUNT
      END IF

  504 FORMAT(1X,2I3,1X,F10.5,1X,
     *' F^2=',1pe13.6,'+-',E13.6,' F=',E13.6,2I7)
      QQT=QQT+QQQ3
      END DO
C====
      WRITE(JS,601)
  601 FORMAT(1X,' Use these choices ? ( ans 1) ',$)
      READ(IS,*)I
      IF(I.NE.1)GOTO 10
  210 RETURN
      END
C
