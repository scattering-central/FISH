      SUBROUTINE SMEARING(KS,KC,KD,KW,KB,KY,JJAM,JJD,KKD)
C==== these data analysis programs are for use only by authorised persons
c==== Added exptl resol functions (as for IRIS)
C==== Copyright R.K.Heenan 1988,1994,1996, bugs sorted 30/6/92
c==== all previous versions did not work well !,
C==== 7/4/93 Added checks on NOREF as crashed when whole fit OFF but some 
c==== params left ON on a CALC only test.
c
c==== New stretched Gaussian 17/4/96 RKH to suit fresh computations for LOQ 
c
C====  this routine written by R.K.Heenan at RAL April 1988
c==== to use MODEL 15 smearing functions on set KS,  is only be used after all
c==== other calculations for this data set as is called by the refinement
c==== routine and NOT by DERIV or CALCUL
C====
C==== NOTE the background, models 3 and/or 4 stored in C(j,KW) is NOT
C==== smeared, which is why we test for models 3 & 4 and skip them.
c====
c====  this routine uses functions FWHMQQ and routine CUBICINTERP
c==== NPSMEAR contains the parameter number of the first
c==== of two cards for MODEL 15
c====
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      COMMON/WORK/DWE(MI),D(MV),DWD2(MI,MI),
     * V2(MV),PM(10),PM2(10),DD(MV,3),WT(10),DBIG(MI,MN),FILL(1859)
      COMMON/DMAT/IID(MV),DWD(MI,MI),SVW(MI),VVV(MI,MI),COV(MI,MI)
C====
      DIMENSION CNEW(MN),DNEW(MI,MN),VV(4),NV(4)
C====
c==== beware of the every MSth point skip
c==== enable refinement of the smearing parameters !
C====  collect necessary parameters from model 15
      NSHAPE=LTYP(NPSMEAR)/10
C====   MODEL 15              NSHAPE=0, standard fit parameters, cubic interpolation
c====                         ca. 0.5 at Q=0.005, falling curve to ca. 0.07
c====                         at Q=0.065, then linear increasing slightly
C====             LTYP 1   SCALE  multiplies sig(q)/q,  usually 1.0
C====		  LTYP 2   NSIMP
C====   MODEL 15              NSHAPE=1, standard fit parameters, exact calc
C====             LTYP 11   SCALE  multiplies sig(q)/q,  usually 1.0
C====		  LTYP 12   NSIMP
C====   MODEL 15               NSHAPE=2, constant resol, CUBIC interpolation
C====             LTYP 21   RESOLUTION  (FWHM of sig(q)/Q )
C====		  LTYP 22   NSIMP
C====   MODEL 15               NSHAPE=3, constant resol, exact calc
C====             LTYP 31   RESOLUTION  (FWHM of sig(q)/Q )
C====		  LTYP 32   NSIMP
C====   MODEL 15            NSHAPE=4, use actual exptl resol curve stored in
c====                       set IV = 9 hardwired below, by cubic interp
c====                       ( for IRIS data ) assume this is a histogram !
C====             LTYP 41   SCALE multiplies width of resol curve ( i.e. rescales it Q values !)
C====		  LTYP 42   not used,
C====   MODEL 15               NSHAPE=5, new calcs 4/96 for LOQ ORDELA 2-10 ang
C====             LTYP 51   abs(SCALE) multiplies width of resol 
c====                       if SCALE is -ve uses (faster) cubic interpolation
c====                       rahter than exact calcs at all points.
C====		  LTYP 52   NSIMP
C====   MODEL 15               NSHAPE=6, new calcs 4/96 for LOQ ORDELA 6-10 ang
C====             LTYP 61   abs(SCALE) multiplies width of resol 
c====                       if SCALE is -ve uses (faster) cubic interpolation
c====                       rahter than exact calcs at all points.
C====		  LTYP 62   NSIMP
C====   MODEL 15               NSHAPE=7, new calcs 4/96 for LOQ HAB 2-10 ang
C====             LTYP 71   abs(SCALE) multiplies width of resol 
c====                       if SCALE is -ve uses (faster) cubic interpolation
c====                       rahter than exact calcs at all points.
C====		  LTYP 72   NSIMP
      SCALE=V(NPSMEAR)
C==== don't smear at all if SCALE is zero
      IF(ABS(SCALE).LT.1.E-06)RETURN
C      WRITE(JS,101)
C      WRITE(JF,101)
C  101 FORMAT(1X,'SMEARING ROUTINE CALLED',/)
C====
      NSIMP=MAX(NINT(V(NPSMEAR+1) ),5)
c==== force NSIMP to be odd
      IF(MOD(NSIMP,2).NE.1)NSIMP=NSIMP+1
c====      WRITE(JS,9001)NPSMEAR,NSHAPE,SCALE,NSIMP
c==== 9001  FORMAT(1X,'NPSMEAR,NSHAPE,SCALE,NSIMP =',2I5,F10.3,I5)
C==== zero the temporary data and derivative arrays
      DO I=JJD,KKD,MS
      CNEW(I)=0.0
      END DO
      IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
      DO J=JJD,KKD,MS
      DO I=1,NPR
      DNEW(I,J)=0.0
      END DO
      END DO
      ENDIF
      IJJJ=0
      IEXT=1
      IF(NSHAPE.NE.4)THEN
c==== original method
C==== loop over old data points
      DO 1000 J=JJD,KKD,MS
        QQ=Q(J,KD)
C==== find the resolution at this Q from function FWHMQQ which give
c==== 2.35*sig(q)/q as a fraction
        CALL LOQRESOL(NSHAPE,QQ,TSIG1BYQ,SIG2,SIG3)
	SIGQ=abs(SCALE)*TSIG1BYQ*QQ/2.354820045
        SIG2=SIG2*ABS(SCALE)
        SIG3=SIG3*ABS(SCALE)
C==== skip round this point if resolution is small or negative
        IF(SIGQ.LT.1.E-05)GOTO 900
C==== start at about exp( -0.5*(3.75**2)) = 0.00088
        Q1=QQ-3.75*SIGQ
        DQ=(7.5*SIGQ)/FLOAT(NSIMP-1)
        IA1=4
        IA2=2
        SUMFF=0.0
C====  loop over resolution shape with NSIMP EQUALLY SPACED points so can use
C====   Simpson rule integration.
c====   If NSHAPE=0 or 2 then
C====  interpolate I(Q) at each point using local cubic function
C====  through nearest 4 points.  This extrapolates cubics at ends of Q range !
C====   as per the exact calculation when NSHAPE=1 or 3
          DO I=1,NSIMP
C==== SWAP IA1 & IA2 each time through, generates 1,4,2,4,2.... 2,4,1 pattern
C==== ( this bit was wrong until 30/6/92, when effectively had only trapezium
c==== rule by mistake)
          IA3=IA1
          IA1=IA2
          IA2=IA3
          IA=IA1
          IF(I.EQ.1)IA=1
          IF(I.EQ.NSIMP)IA=1
C====  calculate value of convolution at this point
c==== new stretched Gaussian 17/4/96 RKH
          A=Q1-QQ
          FF=FLOAT(IA)*EXP(-0.5*(A/(SIGQ +SIG2*ABS(A) +SIG3*A))**2)
          SUMFF=SUMFF+FF
C
C====  interpolate the calculated data and derivatives at this point
C====  using a 4 point Lagrange cubic interpolation for non-equally spaced
C====  points. If forced to extrapolate then skip out and calculate I(Q)
c===    exactly
          IF(NSHAPE.EQ.0.OR.NSHAPE.EQ.2.OR.(NSHAPE.GE.5.AND.SCALE.LT.0.0))
     *                 CALL CUBICINTERP(Q1,JJD,KKD,MS,VV,NV,Q(1,KD),IEXT)
C
          IF(IEXT.EQ.0)THEN
c==== use cubic interpolation
            DO L=1,4
              IF(KW.LE.0)CNEW(J)=CNEW(J)+VV(L)*C(NV(L),KC)*FF
c==== subtract background first if used
              IF(KW.GT.0)CNEW(J)=CNEW(J)+VV(L)*(C(NV(L),KC)-C(NV(L),KW))*FF
            ENDDO
            IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
            DO L=1,4
            DO K=1,NPR
c====  smear the derivs, (bkg models 3 & 4 ARE smeared here, but skipped below )
            DNEW(K,J)= DNEW(K,J)+VV(L)*DBIG(K,NV(L))*FF
            ENDDO
            ENDDO
            ENDIF
          ELSE
C==== IEXT=1 ( default if NSSHAPE=1 or 3) extrapolate outside range of 
c==== original data, use routines DERIV or CALCQ directly
C==== 30/6/92 force Q1 positive here
            QQ1=ABS(Q1)
            IF(NOREF.EQ.1.OR.NPR.EQ.0)THEN
              CALL CALCQ(KS,QQ1,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                               IJJJ,KB,KY,NPSMEAR)
C==== subtract any model 3 background before smearing, ( model 4 is skipped)
              CNEW(J)=CNEW(J)+(CALC1-SUMW)*FF
            ELSE
               CALL DERIV(QQ1,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                         KS,IJJJ,KB,KY,NPSMEAR,D)
               IF(NC.GT.0)CALL CONDER(1,NC,D,D)
               CNEW(J)=CNEW(J)+(CALC1-SUMW)*FF
               DO K=1,NPR
               DNEW(K,J)=DNEW(K,J)+D(IID(K))*FF
               ENDDO
            ENDIF
          ENDIF
          Q1=Q1+DQ
          ENDDO
C==== normalise so that the convolution function has summed to unity
          CNEW(J)=CNEW(J)/SUMFF
          IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
          DO K=1,NPR
           DNEW(K,J)=DNEW(K,J)/SUMFF
          ENDDO
          ENDIF
      goto 1000
C==== just in case sigQ is very small
 900  IF(KW.LE.0)CNEW(J)=C(J,KC)
      IF(KW.GE.0)CNEW(J)=C(J,KC)-C(J,KW)
      IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
      DO K=1,NPR
      DNEW(K,J)=DBIG(K,J)
      END DO
      END IF
C
1000  END DO
C
      ELSE
C==== new method for IRIS data  7/10/94 RKH
C==== loop over old data points
c==== resol function is expected in set 9 !
      IV = 9
      IF(NC4(IV)-NC3(IV).LE.3)THEN
      WRITE(JS,2101)IV
2101  FORMAT(1X,' Not enough data in set ',i3,' for resolution smearing',/)
      RETURN
      END IF
C
C==== loop over the calculated data ===================================
      DO 3000 J=JJD,KKD,MS
        QQ=Q(J,KD)
        SUMFF=0.0
c
C====  loop over resolution shape, which need not have equally spaced points,
c====  though we will assume it is actually so when computing the bin width
C====  using trapezium rule
C====  interpolate calculated (ie smooth !) I(Q) at each point 
c====  using local cubic function through nearest 4 points.
          DO I=NC3(IV),NC4(IV)
          IA=2
          IF(I.EQ.NC3(IV))IA=1
          IF(I.EQ.NC4(IV))THEN
          IA=1
          DQ=Q(I,IV)-Q(I-1,IV)
          ELSE
          DQ=Q(I+1,IV)-Q(I,IV)
          END IF
c==== normalisation sum
          FF = FLOAT(IA)*DQ*C(I,IV)
          SUMFF=SUMFF+FF
C====   compute Q value to be smeared in to sum, expanding resol fn. as needed
          Q1 = QQ + Q(I,IV)*abs(SCALE)
C
C====  interpolate the data and derivatives at this point
C====  using a 4 point Lagrange cubic interpolation for non-equally spaced
C====  points. If forced to extrapolate then skip out and calculate I(Q)
c===    exactly
          CALL CUBICINTERP(Q1,JJD,KKD,MS,VV,NV,Q(1,KD),IEXT)
C
          IF(IEXT.EQ.0)THEN
c==== use cubic interpolation
            DO L=1,4
              IF(KW.LE.0)CNEW(J)=CNEW(J)+VV(L)*C(NV(L),KC)*FF
c==== subtract background first if used
              IF(KW.GT.0)CNEW(J)=CNEW(J)+VV(L)*(C(NV(L),KC)-C(NV(L),KW))*FF
            ENDDO
            IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
            DO L=1,4
            DO K=1,NPR
c====  smear the derivs, (bkg models 3 & 4 ARE smeared here, but skipped below )
            DNEW(K,J)= DNEW(K,J)+VV(L)*DBIG(K,NV(L))*FF
            ENDDO
            ENDDO
            ENDIF
          ELSE
C==== IEXT=1 extrapolate outside range of 
c==== original data, use routines DERIV or CALCQ directly
C==== note for IRIS sign of Q ( ie energy, is important ! )
            IF(NOREF.EQ.1.OR.NPR.EQ.0)THEN
              CALL CALCQ(KS,Q1,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                               IJJJ,KB,KY,NPSMEAR)
C==== subtract any model 3 background before smearing, ( model 4 is skipped)
              CNEW(J)=CNEW(J)+(CALC1-SUMW)*FF
            ELSE
               CALL DERIV(Q1,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                         KS,IJJJ,KB,KY,NPSMEAR,D)
               IF(NC.GT.0)CALL CONDER(1,NC,D,D)
               CNEW(J)=CNEW(J)+(CALC1-SUMW)*FF
               DO K=1,NPR
               DNEW(K,J)=DNEW(K,J)+D(IID(K))*FF
               ENDDO
            ENDIF
          ENDIF
          ENDDO
C==== normalise so that the convolution function has summed to unity
          CNEW(J)=CNEW(J)/SUMFF
          IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
          DO K=1,NPR
           DNEW(K,J)=DNEW(K,J)/SUMFF
          ENDDO
          ENDIF
C
3000  END DO
c==== ================= end of loop over calc data ===============
C
      END IF
c
c====  both methods same from here on .... ==================================
c
C====  to help debug put orig calc curve in space JJAM
      IF(JJAM.GT.0)THEN
      DO J=JJD,KKD,MS
      C(J,JJAM)=C(J,KC)
      ENDDO
      WRITE(JS,102)JJAM
      ENDIF
102   FORMAT(1X,'NOTE for debug purposes, non-smeared calc is in SET',I2)
C
C====  now copy back into the original data arrays
C====  add back the unsmeared background if neccessary
      DO J=JJD,KKD,MS
      IF(KW.LE.0)C(J,KC)=CNEW(J)
      IF(KW.GT.0)C(J,KC)=CNEW(J)+C(J,KW)
      ENDDO
      IF(NOREF.EQ.0.AND.NPR.GT.0)THEN
c==== copy back the smeared derivative array
c====  skip model 3 & 4 backgrounds, as they are not smeared
      DO J=JJD,KKD,MS
      DO K=1,NPR
      IF( LM(IID(K)).NE.3.AND.LM(IID(K)).NE.4)DBIG(K,J)=DNEW(K,J)
      ENDDO
      ENDDO
      ENDIF
      RETURN
      END
c
      SUBROUTINE CUBICINTERP(F,N1,N2,MS,VV,NV,X,IEXT)
      DIMENSION X(N2),VV(4),NV(4)
C==== Determines coefficients VV(i) multiplying function values
C==== at points X( nv(i) ) where points NV(i) are closest four
C==== to x=F for a Lagrange cubic interpolation of non-equally
C==== spaced points
C=====   F normally lies between X(NV(2)) and X(NV(3))
C====  outside ends of range the cubic through the end 4 points would normally
C====  be extrapolated with often disastrous results, so it is now flagged
C====  by IEXT=1
      IEXT=0
      IF(F.LT.X(N1).OR.F.GT.X(N2))THEN
      IEXT=1
      RETURN
      ENDIF
C
      I=N1+2*MS
      DO J=I,N2-MS,MS
      IF(X(J).GT.F) GOTO 100
      ENDDO
      J=N2-MS
100   I=J-MS
      NV(1)=I-MS
      NV(2)=I
      NV(3)=I+MS
      NV(4)=I+2*MS
      XM=X(NV(1))
      XO=X(NV(2))
      X1=X(NV(3))
      X2=X(NV(4))
      VV(1)=(F-XO)*(F-X1)*(F-X2)/((XM-XO)*(XM-X1)*(XM-X2))
      VV(2)=(F-XM)*(F-X1)*(F-X2)/((XO-XM)*(XO-X1)*(XO-X2))
      VV(3)=(F-XM)*(F-XO)*(F-X2)/((X1-XM)*(X1-XO)*(X1-X2))
      VV(4)=(F-XM)*(F-XO)*(F-X1)/((X2-XM)*(X2-XO)*(X2-X1))
      RETURN
      END
C
C====
      SUBROUTINE LOQRESOL(NSHAPE,X,TSIG1BYQ,SIG2,SIG3)
C==== rewritten RKH 9/4/96
c==== Returns parameters for calculated LOQ resolution functions at Q=X
c====
c==== sigma1 and sigma2 are needed for RKH's, better, stretched Gaussian fits
c==== to "exact" curves from RESQ_AVG which convolute moderator time
c==== spreads with the Mildner & Carpenter geometric terms.
c==== i.e. resol proportional to 
c====   exp( -0.5*(A/(sigma1 + sigma2*abs(A) +sigma3*A) )**2 )
c==== where A = (Q -Qmax)
C==== NOTE THIS ROUTINE returns 2.35*sigma1 where 2.354820045 = sqrt(8*loge(2))
c
C==== BEWARE - these empirical fits may not extrapolate well beyond the normal
c==== Q range of LOQ at ISIS !!!!  ( especially option 9 )
C==== No errors are flagged if an unreasonable Q value is supplied !!!
c
C==== The mean FWHM are the simple means from RESQ_AVG.
c====          value of NSHAPE
C1000	format(1x, '0 or 1 - old resol, for LETI, no mod time spread,',/,
C     >         1x,'2 or 3 - constant ',/,
C     >         1x,'5 - LOQ 2-10 2.35sigma1/Q',/,
C     >         1x,'  - LOQ 2-10 sigma2',/,
C     >         1x,'6 - LOQ 6-10 2.35sigma1/Q',/,
C     >         1x,'  - LOQ 6-10 sigma2',/,
C     >         1x,'7 - HAB 2-10 2.35sigma1/Q',/,
C     >         1x,'  - HAB 2-10 sigma2',/,
C     >         1x,'  - HAB 2-10 sigma3',/,
C     >         1x,'81 - LOQ 2-10 mean FWHM',/,
C     >         1x,'82 - LOQ 6-10 mean FWHM',/,
      XX=X*X
      TSIG1BYQ=0.0
      SIG2=0.0
      SIG3=0.0
      IF(NSHAPE.EQ.0.OR.NSHAPE.EQ.1)THEN
C====  LOQ resolution  RKH 8/12/88
C====  this a fit to mean resol summed over lambda = 2 to 10 step .2
c====                                       radius 35 to 335 step 10
c====            with delta radius in resol calc =14
c====   is FWHM =2.35*sig(q)/(q)
	IF(X.LT.0.065)THEN
		IF(X.GE.0.005)THEN
	AA=7.249498E-02 -2.348210*X +26.16275*XX +2.925962E-03/X +
     *       (-9.341829E-11)/(XX*XX)
		ELSE
		AA=0.6
		ENDIF
	ELSE
	AA= 0.07301288 + 0.05305414*X
	ENDIF
	TSIG1BYQ=AA
	RETURN
C
      ELSE IF(NSHAPE.EQ.2.OR.NSHAPE.EQ.3)THEN
        TSIG1BYQ=1.0
        RETURN
C
      ELSE IF(NSHAPE.EQ.81)THEN
c==== loq 2-10 mean FWHM 
	IF(X.LE.0.1)THEN
		IF(X.GE.0.0025)THEN
C==== NB only calc for Q>=0.0086, but eqn seems well behaved to .0025
                 TSIG1BYQ=0.3974226 -9.148721*X +90.84126*XX +
     >           8.015758E-04/X -9.629766E-07/XX -307.3917*X*XX
		ELSE
		TSIG1BYQ=0.55
		ENDIF
	ELSE
	TSIG1BYQ= 0.1226444 -0.3730163*X +0.5043433*XX
	ENDIF
	RETURN
C 
      ELSE IF(NSHAPE.EQ.82)THEN
C==== LOQ 6-10 MEAN FWHM, this is starting to drop off at high Q ( ca 0.1)
c==== would probably be better split into two regions as for 2-10.
      TSIG1BYQ=-0.2399896 +3.701136*X -17.54731*XX +0.01035356/X -
     > 4.404723E-05/XX 
      RETURN
C
      ELSE IF(NSHAPE.EQ.5)THEN
C==== LOQ 2.2-10 2.35 SIG1/Q
      IF(X.LT.0.01294)THEN
      TSIG1BYQ=0.5232100 -11.53542*X -1.989982E-04/X
      ELSE IF(X.LT.0.0937)THEN
      TSIG1BYQ= 0.1592615 -5.178728*X +56.97327*XX +0.005178811/X
     > -2.368187E-05/XX -187.8970*X*XX
      ELSE
c==== cubic at high Q , extrapolates better
      TSIG1BYQ= 0.04654126 +0.5262707*X -3.211298*XX +5.198041*X*XX
      END IF
C
C==== LOQ 2-10 SIGMA2
C==== could not find well-behaved polynomials, so resort to stretched,
c==== asymmetric gaussian
      A=X-0.05020566
      SIG2= 0.1395965*EXP(-0.5*(A/(0.05194613501 -0.3956363*ABS(A)
     >  +0.5258512*A))**2)
C
      RETURN
C
      ELSE IF(NSHAPE.EQ.6)THEN
C==== LOQ 6-10 2.35 SIG1/Q 
      IF(X.LT.0.01294)THEN
C==== SECOND BEST  TSIG1BYQ=0.4992533 -644.6607*XX -4.065384E-04/X
      TSIG1BYQ=0.3994125 +7.747266*X -834.7307*XX
      ELSE IF(X.LT.0.063)THEN
      TSIG1BYQ=-0.9407885 +25.79684*X -314.2358*XX +0.01964366/X -
     > 8.419275E-05/XX +1384.395*X*XX
      ELSE
      A=X
      IF(X.GT.0.0937)A=0.0937
      TSIG1BYQ= 0.2312172 -3.763106*A +20.24911*A*A
      END IF
C==== LOQ 6-10 SIGMA2
      IF(X.GT.0.0086.AND.X.LT.0.099)SIG2=
     >  0.01755692 +0.4736854*X -7.628085*XX -1.152844E-10/(XX*XX)+
     >  13.39186*X*XX -34.57324*XX*XX
      RETURN
C
      ELSE IF(NSHAPE.EQ.7)THEN
C==== LOQ HAB ( high angle bank) 2.2-10, 2.35SIG1/Q
      IF(X.LT.1.028)THEN
      TSIG1BYQ=0.1597649 -0.5389146*X +0.9584754*XX +0.1933661*XX*XX
     > -0.7259599*X*XX -8.809326E-05/XX 
      ELSE
      TSIG1BYQ=0.05225787 +1.299133E-04*X -6.115322E-03*XX
      END IF
C==== LOQ HAB ( high angle bank) 2.2-10, SIGMA2
      IF(X.LT.0.142)THEN
      SIG2=0.0
      ELSE IF(X.LT.0.23)THEN
      SIG2=-0.04714150 +0.3335002*X
      ELSE 
      A=X-0.5026351
      SIG2= 0.09513758*EXP(-0.5*(A/(0.190944782+0.1526626*ABS(A)
     >  +0.2009702*A))**2)
      END IF
C==== LOQ HAB ( high angle bank) 2.2-10, SIGMA3
      IF(X.GT.0.28)SIG3=-0.002381820 +0.006836778*X +0.004521536*XX
      RETURN
C
      END IF
C==== catch any other values here
      RETURN
      END
