c==== 10/11/04 add schultzsum below
c
      SUBROUTINE PICT2
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== SPECIAL PLOTS FOR LEAST SQUARES, ONE FRAME FOR EACH OBS/CALC/DIFF SET
C==== ASSUME Q( ,KC)=Q( ,KD) OVER RANGE OF KD, KC MAY EXTEND BEYOND
C====  DOES CALC  OR   CALC+WT  OR  CALC+(WT)+OBS+DIFF
C==== AXIS TRANSFORMATIONS ARE AS LAST SPECIFIED IN THE PLOT ROUTINE
C==== BY IGP,IG1 AND IG2 IN /AXTYP/
C==== 18/2/92 attempt to transform ERROR BARS 
c
C==== 18/2/92 the original attempted to use as little array space as possible
c==== now /WORK/ is larger we can store the plotted values of OBS,CALC 
c==== and OBS-CALC withou problem, though the code is a little messy in places !
c
c==== 18/1/92  RKH added call to TEXTBOX to print parameter tables on plot
c
      INCLUDE 'FISHDIM.PAR'
      COMMON/WORK/X(MN),Y(MN),EP(MN),TEMP(MN),YOBSP(MN),
     >   XCALP(MN),YCALP(MN),YDIFP(MN),EOBSP(MN),FILL(100784)
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
      COMMON/AXTYP/LABAX(2,9),IX(2),KY(2),ILG(2),ILX(2),ILY(2),IX1(9),
     * IX2(9),ILOG,IGP,IG1,IG2,PMN(2,MW+1),PMX(2,MW+1),RSHIFT(MW),NSET(MW),
     *JTYP(MW),LSYM(MW),LTYP2(MW),LSYM2(MW),IIPF(MW),IIPN(MW),JE(MW),KKC,II
C==== KC changed to KKC in line above to avoid conflict of use with FISH2
      COMMON/SLFIT/SLX1(MW),SLX2(MW),SLY1(MW),SLY2(MW),RVAR(MW),GRAD(MW),
     *RINT(MW),GRE(MW),RE(MW),RG1(MW),RG2(MW),NSL(MW),NUN(MW),RMID,SLUM1(MW),
     *SLUM2(MW),SLUM3(MW),SLUM4(MW)
C==== PASS  II THROUGH /AXTYP/ SO NO ATTEMPTS TO FIT STRAIGHT LINE
c==== expt 10/10/94 put IPF & IPN in a common, RKH
      COMMON/LSPLOT/IPF,IPN
   10 II=9
      NUN(9)=0
      SC=1.0
      SH=0.0
      SCS=1.0
      SHS=0.0
      SCB=1.0
      SHB=0.0
      KS=1
      ISQ=0
      IPQ=0
      IBT=0
      IF(NS.EQ.1)GOTO 16
      WRITE(JS,11)NS
   11 FORMAT(1X,I3,' SETS TO PLOT, WHICH DO YOU WANT ? (-ve to quit) ',$)
      READ(IS,*,ERR=10)KS
      IF(KS.LT.0)RETURN
      IF(KS.EQ.0.OR.KS.GT.NS.OR.JD(KS).EQ.-1)GOTO 10
   16 WRITE(JS,101)
  101 FORMAT(1X,' PLOT CONTROLS  IDEV= 0-EXIT, 1-plot, 2-save file, ',
     *'3-new screen, 4-new file type',
     */,' error bars are IEB*sigma ',
     */,' IPW=1 adds scaled wts',
     */,' IDEL spreads graphs apart (IDEL=2 is default)',
     */,' ITXT = 0 table in file, 1 no table, 2-both, 3-screen only',
     */,'      IDEV,IEB,IPW,IDEL,ITXT =        (5I1) ',$)
c      READ(IS,102,ERR=16)IDEV,IEB,IPW,IIDEL,ITXT
      IDEV=1
      IEB=1
      IPW=0
      IIDEL=0
      ITXT=0
      IF(IDEV.EQ.0)GOTO 900
C
C==== 25/8/99 write to both devices
      IF(IDEV.EQ.2)THEN
      CALL LOCDEV(1,IDEV,IERR)
      GOTO 350
      END IF
C
  102 FORMAT(10I1)
      IF(JPQ(KS).LE.0)GOTO 95
      WRITE(JS,91)
   91 FORMAT(1X,' OVER-PLOT P(Q)  ?  ( LTYP,LSYM (2I1) ) ',$)
c      READ(IS,102)IPQ,IPQSYM
      IPQ=0
      IPQSYM=0
   95 IF(JSQ(KS).LE.0)GOTO 96
      WRITE(JS,92)
   92 FORMAT(1X,' OVER-PLOT RESCALED S(Q)  ? ( LTYP,LSYM (2I1) ) ',$)
c      READ(IS,102)ISQ,ISQSYM
      ISQ=0
      ISQSYM=0
   96 IF(JBT(KS).LE.0)GOTO 100
      WRITE(JS,98)
   98 FORMAT(1X,' OVER-PLOT RESCALED BETA(Q)  ? ( LTYP,LSYM (2I1) ) ',$)
c      READ(IS,102)IBT,IBTSYM
      IBT=0
      IBTSYM=0
  100 IF(IIDEL.EQ.0)IIDEL=2
      IF(IDEV.EQ.1)GOTO 115
      WRITE(JF,103)KS,IEB,IPW,JD(KS),JC(KS),JB(KS)
  103 FORMAT(1X,'LSQ SET',I3,' ERROR BAR=',I2,' WT PLOT=',I2,' OBS=',I2,
     *' CALC=',I2,' BKG=',I2)
      GOTO 115
  115 KD=JD(KS)
      KC=JC(KS)
      KC2=KC
      KW=JW(KS)
      IF(KW.LE.0)GOTO 1115
 1112 WRITE(JS,1111)
 1111 FORMAT(1X,'SUBTRACT BACKGROUND ("WRK",MODELS 3&4) ? (ANS 1) ',$)
c      READ(IS,102,ERR=1112)I
      I=1
      IF(I.NE.1)KW=0
 1115 NNMAX=MAX(NC4(KD),NC4(KC))
      IPN=0
      IF(NC2(KD)-NC1(KD).GE.2)IPN=1
      IPF=0
      KCR=KC
      IF(KD.EQ.KC)GOTO 117
      IF(NC3(KC).GE.NC3(KD).AND.NC4(KC).LE.NC4(KD))GOTO 117
      GOTO 117
 1161 WRITE(JS,116)
  116 FORMAT(1X,'RECALCULATE CALC OVER FULL RANGE USING PRESENT PARAMS',
     * '(ANS 1) ',$)
C===
C===          CHECK -  THIS GOT INTO AN ININITE LOOP  21/2/89
C====
c      READ(IS,102,ERR=1161)I
      I=0
      IF(I.EQ.1)THEN

C==== 12/1/99 when fit >1 sets, recalc causes last set to overwrite it -
c==== oops need to call polset first !
      IF(JY(KS).GT.0)CALL POLSET(Q(1,JY(KS)),C(1,JY(KS)),JY(KS),KS)
C====       WRITE(6,1163)KS,KC,KC2
C==== 1163  FORMAT(1X,'KS,KC,KC2=',3I5)
      CALL CALCUL(KS,KC,KC2,JB(KS),JY(KS),JW(KS),JAM(KS),
     *                 JPQ(KS),JSQ(KS),JBT(KS),JXX(KS),XX,NPSMEAR,MS)
      WRITE(JF,116)
      ELSE
      KCR=KD
      ENDIF
C
C==== PUT CALC IN X() Y(), FIND MIN AND MAX, FORCE TO INCLUDE ZERO AXIS
  117 IF(KW.EQ.0)THEN
      CALL CALC(Q(1,KC),C(1,KC),E(1,KC),KCR,1,IPF,IPN,0.0,XM,XX,YM,YX)
      ELSE
      CALL SUBBKG(TEMP,C(1,KC),C(1,KW),NNMAX)
      CALL CALC(Q(1,KC),TEMP,E(1,KC),KCR,1,IPF,IPN,0.0,XM,XX,YM,YX)
      ENDIF
      IF(YM.GT.0.0)YM=0.0
      IF(YX.LT.0.0)YX=0.0
      RDEL=(YX-YM)*0.025*FLOAT(IIDEL)*0.5
      YM3=YM
C==== 18/2/92 store the calc X and Y
      DO 118 I=1,NC4(KC)
      YCALP(I)=Y(I)
  118 XCALP(I)=X(I)
      IF(KD.EQ.KC)GOTO 230
C
C==== FIND MIN AND MAX OF OBS, 18/2/92 fill x & y again here now, then copy
      IF(KW.LE.0)THEN
      CALL CALC(Q(1,KD),C(1,KD),E(1,KD),KD,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      ELSE
      CALL SUBBKG(TEMP,C(1,KD),C(1,KW),NNMAX)
      CALL CALC(Q(1,KD),TEMP,E(1,KD),KD,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      ENDIF
      DO 210 I=1,NC4(KD)
  210 YOBSP(I)=Y(I)
      DO 211 I=1,NC4(KD)
  211 EOBSP(I)=EP(I)
      IF(YM2.LT.YM)YM=YM2
      IF(YX2.GT.YX)YX=YX2
      IF(XM2.LT.XM)XM=XM2
      IF(XX2.GT.XX)XX=XX2
C
C====                          here  YX 
C====    zero must fall                  OBS and/or CALC
C====    somewhere on Y axis         YM
C====                                   XM          XX
C====
C==== 18/2/92 this used to do TRAN(OBS-CALC) not (TRANS(OBS) -TRANS(CALC))
C==== FIND MIN AND MAX OF OBS-CALC
      DO 215 I=1,NC4(KD)
  215 YDIFP(I)=YOBSP(I)-YCALP(I)
      YM2=0.0
      YX2=0.0
      IF(NC1(KD)*NC2(KD).LE.0)GOTO 217
      DO 216 I=NC1(KD),NC2(KD)
      IF(YDIFP(I).GT.YX2)YX2=YDIFP(I)
      IF(YDIFP(I).LT.YM2)YM2=YDIFP(I)
  216 CONTINUE
  217 IF(NC3(KD)*NC4(KD).LE.0)GOTO 219
      DO 218 I=NC3(KD),NC4(KD)
      IF(YDIFP(I).GT.YX2)YX2=YDIFP(I)
      IF(YDIFP(I).LT.YM2)YM2=YDIFP(I)
  218 CONTINUE
  219 RDEL=(YX-YM+YX2-YM2)*0.025*FLOAT(IIDEL)*0.5
C
      SHIFT=YM-YX2-2.*RDEL
C==== SHIFT OBS-CALC CLEAR OF OTHER GRAPHS
      YM3=YM
      YM=SHIFT+YM2
      DO 220 I=1,NC4(KD)
  220 YDIFP(I)=YDIFP(I)+SHIFT
C
  230 YX=YX+RDEL
      IF(YM.LT.(-RDEL))YM=YM-RDEL
C====
C
C====            top of screen  =   new YX= YX +RDEL
C====                            old YX 
C====                                      OBS and/or CALC
C====                       YM3 =old YM
C====                                   XM          XX
C====                            vertical gap 2*RDEL
C====                              
C====                            max of O-C =  SHIFT+YM2
C====                            SHIFT= zero axis for O-C= YM3-2.*RDEL-YM2
C====            bottom of screen=  new YM= SHIFT - YX2(min of O-C) -RDEL
C====
C==== if there is no O-C region then: if old YM > -RDEL new YM= old YM =YM3 
C====                                 else   new YM = YM3-RDEL
C====  i.e. this makes sure that bottom of graph is zero where sensible
C====   rather then forcing a small -ve Y axis region which looks silly
C====
C==== SPACE FOR ERROR BARS IS NOT CONSIDERED, IIDEL OPTION ALLOWS USER TO FORCE SPACE
      CALL LOCDEV(1,IDEV,IERR)
      IF(IERR.EQ.1)GOTO 350
      CALL LOCAXIS(IDEV,IERR,XM,XX,YM,YX,IG1,IG2,ILOG)
      IF(IERR.EQ.1)GOTO 350
      IE=0
      ILAST=0
      IF(KD.EQ.KC.AND.IPW.EQ.0.AND.ISQ.EQ.0.AND.IPQ.EQ.0.AND.IBT.EQ.0)ILAST=1
  250 J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.2)CALL LOCGRAPH(XCALP(J),YCALP(J),E(J,KCR),IJ,1,0,0,IDEV,IE)
C==== THESE CALL USED TO DRAW SOLID STRAIGHT LINE SEGMENTS FOR CALC 
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(IJ.GE.2)CALL LOCGRAPH(XCALP(J),YCALP(J),E(J,KCR),IJ,1,0,ILAST,IDEV,IE)
      IF(ILAST.EQ.1)GOTO 360
C
C============================== WTS ================================
      IF(IPW.EQ.0)GOTO 290
C==== PLOT SCALED WEIGHTS AS SOLID ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
C==== THIS CALL TO CALC TO FIN MAX AND MIN OF WT
      CALL CALC(Q(1,KD),E(1,KC),E(1,KD),KC,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
C==== ASSUME YM2 IS ZERO
C==== SCALE powers of ten then fill towards top of graph by a "round" number
C
C==== ALLOW FOR ( ALMOST IMPOSSIBLE) CASE WHERE ZERO IS AT TOP OF GRAPH
C==== NOTE WE ALWAYS FORCE ZERO LINE TO APPEAR
      IF(ABS(YX-RDEL).LT.RDEL)SH=YM3
      A=10.0
  262 IF(YX2*SC.LE.YX-SH)GOTO 263
      SC=SC/A
      GOTO 262
  263 IF(YX2*SC.LT.0.1*(YX-SH))GOTO 268
      B=1.0
      IF(YX2*SC.LT.0.65*(YX-SH))B=1.5
      IF(YX2*SC.LT.0.50*(YX-SH))B=2.0
      IF(YX2*SC.LT.0.40*(YX-SH))B=2.5
      IF(YX2*SC.LT.0.25*(YX-SH))B=4.0
      IF(YX2*SC.LT.0.20*(YX-SH))B=5.0
      SC=SC*B
      GOTO 274
  268 SC=SC*A
      GOTO 262
C==== SCALE WT ARRAY
  274 DO 280 I=1,NDIM
  280 Y(I)=Y(I)*SC+SH
      IF(IDEV.EQ.2)WRITE(JF,281)SC,SH
  281 FORMAT(/,1X,'WEIGHTS *',1PE10.2,' + ',E11.3,/)
      J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KC),IJ,1,0,0,IDEV,0)
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(KD.EQ.KC.AND.ISQ.EQ.0.AND.IPQ.EQ.0.AND.IBT.EQ.0)ILAST=1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KC),IJ,1,0,ILAST,IDEV,0)
      IF(ILAST.EQ.1)GOTO 360
C
C================================ P(Q) =============================
  290 IF(IPQ.EQ.0)GOTO 1290
      KK=JPQ(KS)
C==== PLOT P(Q) AS SOLID ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
      CALL CALC(Q(1,KD),C(1,KK),E(1,KK),KK,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,IPQ,IPQSYM,0,IDEV,0)
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(KD.EQ.KK.AND.ISQ.EQ.0.AND.IBT.EQ.0)ILAST=1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,IPQ,IPQSYM,ILAST,IDEV,0)
      IF(ILAST.EQ.1)GOTO 360
C
C=================================== S(Q) ============================
 1290 IF(ISQ.EQ.0)GOTO 2290
      KK=JSQ(KS)
C==== PLOT SCALED S(Q) AS SOLID ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
C==== THIS CALL TO CALC TO FIN MAX AND MIN OF S(Q)
      CALL CALC(Q(1,KD),C(1,KK),E(1,KK),KK,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      IF(ABS(YX-RDEL).LT.RDEL)SHS=YM3
      IF(YM2.GT.0)YM2=0.0
      YX2=YX2-YM2
      A=10.0
 2262 IF(YX2*SCS.LE.YX-SHS)GOTO 2263
      SCS=SCS/A
      GOTO 2262
 2263 IF(YX2*SCS.LT.0.1*(YX-SHS))GOTO 2268
      B=1.0
      IF(YX2*SCS.LT.0.65*(YX-SHS))B=1.5
      IF(YX2*SCS.LT.0.50*(YX-SHS))B=2.0
      IF(YX2*SCS.LT.0.40*(YX-SHS))B=2.5
      IF(YX2*SCS.LT.0.25*(YX-SHS))B=4.0
      IF(YX2*SCS.LT.0.20*(YX-SHS))B=5.0
      SCS=SCS*B
      GOTO 2274
 2268 SCS=SCS*A
      GOTO 2262
C==== SCALE S(Q) ARRAY
 2274 DO 2280 I=1,NDIM
 2280 Y(I)=Y(I)*SCS+SHS
      IF(IDEV.EQ.2)WRITE(JF,2281)SCS,SHS
 2281 FORMAT(/,1X,' S(Q) *',1PE10.2,' + ',E11.3,/)
      J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,ISQ,ISQSYM,0,IDEV,0)
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(KD.EQ.KC.AND.IBT.EQ.0)ILAST=1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,ISQ,ISQSYM,ILAST,IDEV,0)
      IF(ILAST.EQ.1)GOTO 360
C
C==== ============================== BETA(Q) ==============================
 2290 IF(IBT.EQ.0)GOTO 3290
      KK=JBT(KS)
C==== PLOT SCALED BETA(Q) ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
C==== THIS CALL TO CALC TO FIN MAX AND MIN OF WT
      CALL CALC(Q(1,KD),C(1,KK),E(1,KK),KK,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      IF(ISQ.EQ.0)GOTO 3261
C==== IF HAVE ALREADY PLOTTED S(Q) USE SAME SCALE FACTOR, theoretically
C====  beta(Q) can never be >1.0, wheras S(Q) may be, so BETA(Q) ought to be 
C====  smaller and fit on same scale - this avoids them being scaled
C====  differently, which looks silly.
      SCB=SCS
      SHB=SHS
      GOTO 3274
 3261 IF(ABS(YX-RDEL).LT.RDEL)SHB=YM3
      IF(YM2.GT.0)YM2=0.0
      YX2=YX2-YM2
      A=10.0
 3262 IF(YX2*SCB.LE.YX-SHB)GOTO 3263
      SCB=SCB/A
      GOTO 3262
 3263 IF(YX2*SCB.LT.0.1*(YX-SHB))GOTO 3268
      B=1.0
      IF(YX2*SCB.LT.0.65*(YX-SHB))B=1.5
      IF(YX2*SCB.LT.0.50*(YX-SHB))B=2.0
      IF(YX2*SCB.LT.0.40*(YX-SHB))B=2.5
      IF(YX2*SCB.LT.0.25*(YX-SHB))B=4.0
      IF(YX2*SCB.LT.0.20*(YX-SHB))B=5.0
      SCB=SCB*B
      GOTO 3274
 3268 SCB=SCB*A
      GOTO 3262
C==== SCALE BETA(Q) ARRAY
 3274 DO 3280 I=1,NDIM
 3280 Y(I)=Y(I)*SCB+SHB
      IF(IDEV.EQ.2)WRITE(JF,3281)SCB,SHB
 3281 FORMAT(/,1X,' BETA(Q) *',1PE10.2,' + ',E11.3,/)
      J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,IBT,IBTSYM,0,IDEV,0)
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(KD.EQ.KC)ILAST=1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,IBT,IBTSYM,ILAST,IDEV,0)
      IF(ILAST.EQ.1)GOTO 360
C====================================     OBS-CALC ==============
C==== 18/2/92 this used to redo the OBS
 3290 J=NC1(KD)
C==== PLOT THE OBS AS PLUS SIGNS, WITH ERRORS IF NEEDED
      IJ=NC2(KD)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),YOBSP(J),EOBSP(J),IJ,5,3,0,IDEV,IEB)
      J=NC3(KD)
      IJ=NC4(KD)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),YOBSP(J),EOBSP(J),IJ,5,3,ILAST,
     * IDEV,IEB)
      IF(ILAST.EQ.1)GOTO 360
C=== OBS-CALC AS SOLID LINE, 
      CALL LOCLIN(XM2,XX2,SHIFT,SHIFT)
      KCR=JD(KS)
      ILAST=1
      J=NC1(KD)
      IJ=NC2(KD)-J+1
c==== these calls used to draw solid straight line segments for calc and diff
c==== with transformed errors if needed
      IF(J*IJ.GE.2)CALL LOCGRAPH(XCALP(J),YDIFP(J),EOBSP(J),IJ,1,0,0,IDEV,IEB)
      J=NC3(KD)
      IJ=NC4(KD)-J+1
      IF(IJ.GE.2)CALL LOCGRAPH(XCALP(J),YDIFP(J),EOBSP(J),IJ,1,0,ILAST,IDEV,IEB)
C
C================  THE VERY END ===================
C==== call to write parameter table to UNIRAS POSTSCRIPT file added RKH 18/1/93
C==== 25/8/99 flag ITXT added
  360 CALL TEXTBOX(ITXT)
  350 CALL LOCDEV(3,IDEV,IERR)
      IF(IPW.EQ.1)WRITE(JS,281)SC,SH
      IF(ISQ.GT.0)WRITE(JS,2281)SCS,SHS
      IF(IBT.GT.0)WRITE(JS,3281)SCB,SHB
c      GOTO 10
C==== ============ EXIT IF JUST PRESS RETURN
  900 WRITE(JF,102)
      RETURN
      END
C
      SUBROUTINE SUBBKG(T,A,B,N)
C==== RATHER A SILLY ROUTINE TO SUBTRACT TWO ARRAYS    T= A - B
      DIMENSION T(N),A(N),B(N)
      DO 1 I=1,N
    1 T(I)=A(I)-B(I)
      RETURN
      END
C
      SUBROUTINE LEXI(K)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== WRITE OUT A LIST OF THE SETS (STORES OF Q AND C ARRAYS ) THAT ARE IN USE
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC(MW,5),IDC(MW),NIC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      DIMENSION QP(5)
      IF(K.NE.0)GOTO 150
      WRITE(JS,11)
   11 FORMAT(1X,'  LABEL         NCH      L1      L2 MID*10      R1',
     *'      R2   IDC')
      DO 100 I=1,MW
      IF(NCH(I).LE.0)GOTO 100
      DO 50 J=1,5
      N=NC(I,J)
      QP(J)=0.0
      IF(N.GT.0.AND.N.LE.NCH(I))QP(J)=Q(N,I)
   50 CONTINUE
      WRITE(JS,12)I,(LAB(J,I),J=1,3),NCH(I),(NC(I,J),J=1,5),IDC(I),
     *           (QP(J),J=1,5),(NIC(I,J),J=1,4)
  100 CONTINUE
   12 FORMAT(1X,I3,1X,3A4,I5,5I7,I5,/19X,1P5E8.1,/14X,4I10)
      RETURN
  150 DO 200 I=1,MW
      IF(NCH(I).GT.0)WRITE(JS,14)I,(LAB(J,I),J=1,3)
  200 CONTINUE
      RETURN
   14 FORMAT(1X,I3,1X,3A4)
      END
C
      SUBROUTINE WTMEAN
C==== A.R.M ROUTINE TO FIND WT MEAN FROM THE POLYDISPERSITY SET OF DATA 
C==== PRODUCED AFTER FIT
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
C====      COMMON/WORK/L(20),FMT(19),IDAT(3),ITIM(2),IFILL,FILL(2469)
      COMMON/BINPAR/NBIN(6),RBIN(5)
      INTEGER R
	CHARACTER*4 NAME
	SUMWT=0
	SUMWTR=0
	RNUM1=0
	RNUM2=0
C	WRITE(JS,10)
C10	FORMAT(1X,'Enter polydispersity set number ',$)
C	READ(IS,20)IST
C20	FORMAT(I1)
	DO I=1,MW
	WRITE(NAME,'(A4)')LAB(1,I)
	IF(NAME.EQ.'POL '.OR.NAME.EQ.'POLY')THEN
		IST=I
	END IF
	END DO
	IF(IST.EQ.0.OR.IST.GT.6)THEN
		GOTO 100
	END IF
	N=NCH(IST,1)
	WRITE(JS,25)N
25	FORMAT(1X,'Averaged over ',I2,' channels')
	DO R=1,N
	SUMWTR=SUMWTR+(C(R,IST)*Q(R,IST))
	SUMWT=SUMWT+C(R,IST)
	RNUM1=RNUM1+(C(R,IST)/Q(R,IST)**2)
	RNUM2=RNUM2+(C(R,IST)/Q(R,IST)**3)
	END DO
	RNUM=RNUM1/RNUM2
	RWT=SUMWTR/SUMWT	
	WRITE(JS,30)RWT
	WRITE(JF,30)RWT
30	FORMAT(//1X,'The weight mean is ',F10.5,' angstroms')
	WRITE(JS,40)RNUM
	WRITE(JF,40)RNUM
40	FORMAT(//1X,'The number mean is ',F10.5,' angstroms')
100	RETURN
	END
      SUBROUTINE OUTP(IERR,IPASS,JPASS,ITTPASS,IRTPASS,TEXT,LEN,MODE)
C==== THIS VERSION FROM FRIN 11/3/88, WITH ADDED RBIN() AND NBIN()
C==== WRITE INTENSITY DATA TO SCREEN OR FILE, AS FROM ROUTINE DATIN, SEE SETUP
C==== FOR THE CHANNEL NUMBERS.   WRITE IN A BIT TO HANDLE ESD( ) ???
C====  ITT IS TITLE REF, IRT FOR SPECIAL TREATMENTS, E.G. ON CALL FROM QBIN
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      COMMON/WORK/L(20),FMT(19),IDAT(3),ITIM(2),IFILL,FILL(109955)
      COMMON/BINPAR/NBIN(6),RBIN(5)
      LOGICAL LOP
      CHARACTER*24 CDAT
      CHARACTER*255 STR,TEXT
C==== THE PARAMS ARE PASSED AS INTEGERS, FORTRAN DOES NOT LIKES US CHANGING
C==== THEM( e.g. ITT=I) SINCE THEY DO NOT HAVE LOCATIONS IN THE CALLING ROUTINE
C==== SO MAKE A LOCAL COPY HERE
      I1=IPASS
      I2=I1
      J=JPASS
      ITT=ITTPASS
      IRT=IRTPASS
      IF(IRT.NE.0)GOTO 300
   10 J=1
c     Loop between sets I1 and I2
      I=I1
      IF(I2.LT.I1)I2=I1
      ITT=I
  300 IJ=JS
C==== 17/8/99 sort out file name for PC
      IF(J.EQ.1)THEN
      IF(MODE .EQ. 0)CLOSE(JD)
      IJ=JD
 2020 LOP=.FALSE.
      INQUIRE(ERR=2095,UNIT=JD,OPENED=LOP,NAME=STR)
 2023 IF(.NOT. LOP)THEN
      STR=TEXT       
      WRITE(JS,2097)STR
 2097 FORMAT(/1X,'Creating file ',/,1X,A255)
      CALL OPENNEW(JD,IERR,STR)
c      ELSE
c      CALL OPENANY(JD,IERR,STR)
      END IF
      IF(IERR.EQ.1)GOTO 2095 
      ENDIF
      WRITE(JS,2024)STR,I
 2024 FORMAT(1X,'Output file:'/,1x,a255,"I=",I1)      
c
      IF(J.EQ.2)IJ=JF
      IF(NCH(I,1).LE.0)GOTO 200
      WRITE(JS,12)I,(LAB(K,I),K=1,3)
   12 FORMAT(1X,'SET(',I2,' )  ',3A4)
      IF(IJ.NE.JD)GOTO 40
C==== WRITE OUT TO DATA FILE
C==== THE FIRST 24 SPACES OF THE FIRST TITLE ARE ASSUMED TO BE THE DATE AND
C==== TIME SO ARE OVERWRITTEN
      CALL TODAY(CDAT)
C==== IRT=2 IS FROM QBIN, WHEN EXTRA INFO. IS ADDED TO TITLE CARDS
c      WRITE(JS,9)(LAB2(K,1,ITT),K=1,20),(LAB2(K,2,ITT),K=1,20)
c    9 FORMAT(1X,'OLD TITLES:',/,3(1X,20A4,:/) )
   15 WRITE(JS,16)
   16 FORMAT(/,1X,'TITLE:')
c      WRITE(JS,119)CDAT(1:24),(LAB2(K,1,ITT),K=7,20)
      WRITE(JS,151)(LAB2(K,1,ITT),K=1,20)
  119 FORMAT(1X,A24,16A4)
      IF(IRT.NE.2)WRITE(JS,151)(LAB2(K,2,ITT),K=1,20)
      IF(IRT.NE.2)GOTO 18
      A=FLOAT(NBIN(3))/10.
      B=RBIN(4)/RBIN(5)
      WRITE(JS,123)(LAB2(K,2,ITT),K=1,11),A,RBIN(3),B
   23 FORMAT(11A4,1X,'MID',0PF6.1,' LA',F6.3,' D/PS',1PE11.4)
  123 FORMAT(1X,11A4,1X,'MID',0PF6.1,' LA',F6.3,' D/PS',1PE11.4)
   18 KK=0
c     Big chunk of program for changing titles deleted from here
   30 CONTINUE
C====  9/12/98 add a loop to save lots workspaces at once !
C====  ask for title on first one only !
      DO I=I1,I2
      IF(NCH(I,1).GT.0)THEN
C
      WRITE(JS,2088)I
 2088 FORMAT(1X,'Loop ',I1)
      WRITE(JF,12)I,(LAB(K,I),K=1,3)
      WRITE(JF,119)CDAT(1:24),(LAB2(K,1,I),K=6,20)
      WRITE(JD,19)(LAB2(K,1,I),K=1,20)
      IF(IRT.NE.2)THEN
      WRITE(JF,151)(LAB2(K,2,I),K=1,20)
      WRITE(JD, 51)(LAB2(K,2,I),K=1,20)
      ENDIF
      IF(IRT.EQ.2)THEN
      WRITE(JF,123)(LAB2(K,2,ITT),K=1,11),A,RBIN(3),B
      WRITE(JD,23)(LAB2(K,2,ITT),K=1,11),A,RBIN(3),B
      ENDIF
      WRITE(JF,152)(NCH(I,K),K=1,7)
      WRITE(JF,153)(IC(I,K),K=1,4)
      WRITE(JD, 52)(NCH(I,K),K=1,7)
      WRITE(JD, 53)(IC(I,K),K=1,4)
      WRITE(JD, 54)
      N=NCH(I,1)
      WRITE(JD, 55)(Q(K,I),C(K,I),E(K,I),K=1,N)
C
      END IF
      END DO
C
      CALL FLUSH(JD)
      CALL FLUSH(JF)
      ENDFILE(JD)
      RETURN
C==== NO 1X FOR INPUT AND DATA FILE OUTPUT
   19 FORMAT(20A4)
   51 FORMAT(20A4)
   52 FORMAT(16I5)
   53 FORMAT(8I10)
   54 FORMAT(' 3 (F12.5,2E14.6)'  )
   55 FORMAT(0PF12.5,1PE14.6,E14.6 )
C==== ADD 1X FOR SCREEN OR LINEPRINTER FILE
  151 FORMAT(1X,20A4)
  152 FORMAT(1X,16I5)
  153 FORMAT(1X,8I10)
  155 FORMAT(1X,2(0PF12.7,1PE12.5,E12.5) )
C
C==== SCREEN OR LOG
   40 WRITE(IJ,151)(LAB2(K,1,ITT),K=1,20)
      WRITE(IJ,151)(LAB2(K,2,ITT),K=1,20)
      WRITE(IJ,152)(NCH(I,K),K=1,7)
      WRITE(IJ,153)(IC(I,K),K=1,4)
      N=NCH(I,1)
      IF(IJ.EQ.JS)GOTO 100
C==== LOG
      WRITE(IJ,155)(Q(K,I),C(K,I),E(K,I),K=1,N)
      RETURN
C==== SCREEN
  100 WRITE(IJ,101)
  101 FORMAT(1X,'N1,N1 =  (*) (ZEROS TO STOP) ',$)
      READ(IS,*,ERR=100)N1,N2
      IF((N1.LE.0).OR.(N2.LE.0))RETURN
      IF(N2.GT.N)N2=N
      WRITE(IJ,103)(K,Q(K,I),C(K,I),E(K,I),K=N1,N2)
      GOTO 100
  103 FORMAT(1X,I5,0PF12.7,1P2E12.5)
  200 CALL LEXI(0)
c      GOTO 10
      RETURN
 2095 IERR=1
      RETURN    
      END
      SUBROUTINE LRCALC
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
    1 WRITE(JS,101)
  101 FORMAT(1X,'PUT LEFT-RIGHT DIFFERENCE FOR SET I IN SET J,',
     * /,'  I,J= (2I1)')
      READ(IS,2,ERR=1)II,JJ
      IF(II*JJ.LE.0.OR.II.EQ.JJ)RETURN
    2 FORMAT(2I1)
      WRITE(JS,102)
  102 FORMAT(1X,'USE 0-QVALUES, 1-NCH CHANNEL NOS. (I1)')
      READ(IS,2,ERR=1)NNQ
      IF(NC1(II)*NC2(II).LE.0)CALL RANGE1(II)
      IR2=NC4(II)
      IR1=NC3(II)
      L1=NC1(II)
      L2=NC2(II)
      IF(L1*L2.LE.0.OR.IR1*IR2.LE.0)RETURN
      J=IR2
      QR=Q(J,II)
      IF(NNQ.EQ.1)GOTO 400
      DO 300 I=L1,L2
      QL=ABS(Q(I,II))
  220 J=J-1
      IF(J.LT.IR1)GOTO 350
      QR1=QR
      QR=Q(J,II)
      IF(QR.GT.QL)GOTO 220
      IF(QR1.LT.QL)GOTO 300
C=== APPROXIMATE LINEAR INTERPOLATION
      A=(QL-QR)/(QR1-QR)
      C(I,JJ)=(A-1.)*C(J,II) - A*C(J+1,II) + C(I,II)
      E(I,JJ)=(1.-A)*E(J,II) + A*E(J+1,II) + E(I,II)
      Q(I,JJ)=Q(I,II)
  300 CONTINUE
  350 GOTO 600
C==== USE NCH WITH CENTRE=NMC(II)/10 TO NEAREST 0.5 CHANNEL
  400 N2=IFIX(FLOAT(NMC(II))/5. +0.5)
      DO 500 I=L1,L2
      J=N2-I
      IF(J.LT.IR1)GOTO 500
      IF(J.GT.IR2)GOTO 500
      C(I,JJ)=C(I,II)-C(J,II)
      E(I,JJ)=E(I,II)+E(J,II)
      Q(I,JJ)=Q(I,II)
  500 CONTINUE
  600 NCH(JJ)=NCH(II)
      NC1(JJ)=L1
      NC2(JJ)=L2
      NMC(JJ)=NMC(II)
      NC3(JJ)=IR1
      NC4(JJ)=IR2
      IDC(JJ)=IDC(II)
      NSUM(JJ)=0
      IC1(JJ)=0
      IC2(JJ)=0
      IC3(JJ)=0
      CALL LABEL(JJ)
      WRITE(JF,601)JJ,II,(LAB(I,JJ),I=1,3),NNQ
  601 FORMAT(1X,'(',I2,' ) = L-R OF (',I2,' )      ',3A4,'  NNQ = ',I2)
      RETURN
      END
      SUBROUTINE LABEL(II)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
   10 WRITE(JS,3)
    3 FORMAT(1X,'New label :')
      READ(IS,1,ERR=10)(LAB(J,II),J=1,3)
    1 FORMAT(3A4)
      RETURN
      END
c
      SUBROUTINE SCHULTZSUM
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 2004
C==== ADD up and store model 1 ltyp = 21 Schultz distributions
c
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
c====
      WRITE(JS,1001)
 1001 FORMAT(/,'NOTE sets zero, sums into current POL set, step N4 max N5 !',/,
     > ' i.e. does not include model 6 polydisp, and assumes first',/,
     > ' set of a multi-set fit.',/, 'RESCALE FACTOR (e.g. 1.E16) =',$)
	READ(IS,*)RESCALE
      WRITE(JS,1002)
 1002 FORMAT(/,'Do you need divide Rbar by 2pi (also a fudge here, see code)',/,
     > '(i.e. you fitted I(S) )        answer ( 1 for YES) =',$)
	READ(IS,*)I
      R2PI = 1.0
	IF(I.EQ.1)R2PI = 1.0 / 6.283185307
      IF(NP*NS.LE.0)GOTO 900
	KS = 1
	IF(JY(KS).LE.0.OR.JY(KS).GT.MW)GOTO 900
	NSCH = 0
C==== SEARCH FOR MODEL 1, TYPE 21 POLYDISPERSITY
c==== make sure they only apply to data set KS = 1, via model 88 cardS.
      IM1=KS
      DO 50 I=1,NP
      IF(LM(I).EQ.88)THEN
        IM1=LTYP(I)
        IF(IM1.LE.0)IM1=KS
      END IF
      IF(IM1.NE.KS)GOTO 50
      IF(.NOT.(LM(I).EQ.1.AND.LTYP(I).EQ.21))GOTO 50
C==== found the start of one, now check next two records
      IF(I+2.GT.NP)GOTO 50
	NSCH = NSCH +1
	WRITE(JS,41)I,LM(I),LTYP(I)
41    FORMAT(1X,3I4)
      IF(NSCH.EQ.1)THEN
C==== fill up radii first time only
	R = CON(4)*R2PI
	II = 1
   45	Q(II,JY(KS))= R
      C(II,JY(KS)) = 0.0
	E(II,JY(KS)) = 0.0
	R = R + CON(4)*R2PI
	II= II+1
	IF(II.LE.MN.AND.R.LE.CON(5)*R2PI)GOTO 45
	II = II -1
	NC3(JY(KS)) = 1
	NC4(JY(KS)) = II
	NCH(JY(KS)) = II
	END IF
	AA=(4.0*3.141592654/3.0)*( 1.0+V(I+2) )*( 1.0+2.0*V(I+2) )*
     >  (V(I+1)*R2PI)**3
	if(abs(AA).gt.1.e-40)THEN
C==== the 1/(2pi)^4 is to match earlier calcs where I divided R by 2pi AFTER the calculation
	AA=RESCALE*(R2PI**4)*V(I)/AA
	ELSE
	GOTO 900
	END IF
C
	DO J = 1, II 
	IBUG = 0
	C(J,JY(KS)) = C(J,JY(KS)) 
     >	+ AA*SCHULTZ(Q(J,JY(KS)),V(I+1)*V(I+2)*R2PI,V(I+1)*R2PI)
c====       scale                 (sigma/rbar)*rbar     rbar
      END DO
   50 CONTINUE
      WRITE(JS,51)NSCH,JY(KS)
   51	FORMAT(1X,'Summed', i4,' Schultz into set number',i4,/,
     > 'NOTE - it is up to you to check that LARGE sizes can actually',/,
     >  ' be determined from your Q or S range!')
      RETURN
  900 WRITE(JS,901)
  901 FORMAT(1X,'Possible ERROR - or there is nothing to calculate ! ')
      RETURN
	END
      SUBROUTINE SQSCL(KS,SCL)
c     Find scale factor for S(Q) or BETA(Q)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== SPECIAL PLOTS FOR LEAST SQUARES, ONE FRAME FOR EACH OBS/CALC/DIFF SET
C==== ASSUME Q( ,KC)=Q( ,KD) OVER RANGE OF KD, KC MAY EXTEND BEYOND
C====  DOES CALC  OR   CALC+WT  OR  CALC+(WT)+OBS+DIFF
C==== AXIS TRANSFORMATIONS ARE AS LAST SPECIFIED IN THE PLOT ROUTINE
C==== BY IGP,IG1 AND IG2 IN /AXTYP/
C==== 18/2/92 attempt to transform ERROR BARS 
c
C==== 18/2/92 the original attempted to use as little array space as possible
c==== now /WORK/ is larger we can store the plotted values of OBS,CALC 
c==== and OBS-CALC withou problem, though the code is a little messy in places !
c
c==== 18/1/92  RKH added call to TEXTBOX to print parameter tables on plot
c
      INCLUDE 'FISHDIM.PAR'
      COMMON/WORK/X(MN),Y(MN),EP(MN),TEMP(MN),YOBSP(MN),
     >   XCALP(MN),YCALP(MN),YDIFP(MN),EOBSP(MN),FILL(100784)
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
      COMMON/AXTYP/LABAX(2,9),IX(2),KY(2),ILG(2),ILX(2),ILY(2),IX1(9),
     * IX2(9),ILOG,IGP,IG1,IG2,PMN(2,MW+1),PMX(2,MW+1),RSHIFT(MW),NSET(MW),
     *JTYP(MW),LSYM(MW),LTYP2(MW),LSYM2(MW),IIPF(MW),IIPN(MW),JE(MW),KKC,II
C==== KC changed to KKC in line above to avoid conflict of use with FISH2
      COMMON/SLFIT/SLX1(MW),SLX2(MW),SLY1(MW),SLY2(MW),RVAR(MW),GRAD(MW),
     *RINT(MW),GRE(MW),RE(MW),RG1(MW),RG2(MW),NSL(MW),NUN(MW),RMID,SLUM1(MW),
     *SLUM2(MW),SLUM3(MW),SLUM4(MW)
C==== PASS  II THROUGH /AXTYP/ SO NO ATTEMPTS TO FIT STRAIGHT LINE
c==== expt 10/10/94 put IPF & IPN in a common, RKH
      COMMON/LSPLOT/IPF,IPN
   10 II=9
      NUN(9)=0
      SC=1.0
      SH=0.0
      SCS=1.0
      SHS=0.0
      SCB=1.0
      SHB=0.0
      ISQ=0
      IPQ=0
      IBT=0
      IF(NS.EQ.1)GOTO 16
      IF(KS.LT.0)RETURN
      IF(KS.EQ.0.OR.KS.GT.NS.OR.JD(KS).EQ.-1)GOTO 10
c   16 WRITE(JS,101)
   16 CONTINUE
  101 FORMAT(1X,' PLOT CONTROLS  IDEV= 0-EXIT, 1-plot, 2-save file, ',
     *'3-new screen, 4-new file type',
     */,' error bars are IEB*sigma ',
     */,' IPW=1 adds scaled wts',
     */,' IDEL spreads graphs apart (IDEL=2 is default)',
     */,' ITXT = 0 table in file, 1 no table, 2-both, 3-screen only',
     */,'      IDEV,IEB,IPW,IDEL,ITXT =        (5I1) ',$)
c      WRITE(JS,999)KS
  999 FORMAT(1X,' S(Q) for set ',I4)    
c      READ(IS,102,ERR=16)IDEV,IEB,IPW,IIDEL,ITXT
      IDEV=1
      IEB=1
      IPW=0
      IIDEL=0
      ITXT=0
C
C==== 25/8/99 write to both devices
      IF(IDEV.EQ.2)THEN
      CALL LOCDEV(1,IDEV,IERR)
      END IF
C
  102 FORMAT(10I1)
      IF(JPQ(KS).LE.0)GOTO 95
c      WRITE(JS,91)
   91 FORMAT(1X,' OVER-PLOT P(Q)  ?  ( LTYP,LSYM (2I1) ) ',$)
c      READ(IS,102)IPQ,IPQSYM
      IPQ=0
      IPQSYM=0
   95 IF(JSQ(KS).LE.0)GOTO 96
c      WRITE(JS,92)
   92 FORMAT(1X,' OVER-PLOT RESCALED S(Q)  ? ( LTYP,LSYM (2I1) ) ',$)
c      READ(IS,102)ISQ,ISQSYM
      ISQ=1
      ISQSYM=1
   96 IF(JBT(KS).LE.0)GOTO 100
c      WRITE(JS,98)
   98 FORMAT(1X,' OVER-PLOT RESCALED BETA(Q)  ? ( LTYP,LSYM (2I1) ) ',$)
c      READ(IS,102)IBT,IBTSYM
      IBT=0
      IBTSYM=0
  100 IF(IIDEL.EQ.0)IIDEL=2
      IF(IDEV.EQ.1)GOTO 115
c      WRITE(JF,103)KS,IEB,IPW,JD(KS),JC(KS),JB(KS)
  103 FORMAT(1X,'LSQ SET',I3,' ERROR BAR=',I2,' WT PLOT=',I2,' OBS=',I2,
     *' CALC=',I2,' BKG=',I2)
      GOTO 115
  115 KD=JD(KS)
      KC=JC(KS)
      KC2=KC
      KW=JW(KS)
      IF(KW.LE.0)GOTO 1115
 1112 CONTINUE     
c 1112 WRITE(JS,1111)
 1111 FORMAT(1X,'SUBTRACT BACKGROUND ("WRK",MODELS 3&4) ? (ANS 1) ',$)
c      READ(IS,102,ERR=1112)I
      I=1
      IF(I.NE.1)KW=0
 1115 NNMAX=MAX(NC4(KD),NC4(KC))
      IPN=0
      IF(NC2(KD)-NC1(KD).GE.2)IPN=1
      IPF=0
      KCR=KC
      IF(KD.EQ.KC)GOTO 117
      IF(NC3(KC).GE.NC3(KD).AND.NC4(KC).LE.NC4(KD))GOTO 117
      GOTO 117
 1161 CONTINUE     
c 1161 WRITE(JS,116)
  116 FORMAT(1X,'RECALCULATE CALC OVER FULL RANGE USING PRESENT PARAMS',
     * '(ANS 1) ',$)
C===
C===          CHECK -  THIS GOT INTO AN ININITE LOOP  21/2/89
C====
c      READ(IS,102,ERR=1161)I
      I=0
      IF(I.EQ.1)THEN

C==== 12/1/99 when fit >1 sets, recalc causes last set to overwrite it -
c==== oops need to call polset first !
      IF(JY(KS).GT.0)CALL POLSET(Q(1,JY(KS)),C(1,JY(KS)),JY(KS),KS)
C====       WRITE(6,1163)KS,KC,KC2
C==== 1163  FORMAT(1X,'KS,KC,KC2=',3I5)
      CALL CALCUL(KS,KC,KC2,JB(KS),JY(KS),JW(KS),JAM(KS),
     *                 JPQ(KS),JSQ(KS),JBT(KS),JXX(KS),XX,NPSMEAR,MS)
c      WRITE(JF,116)
      ELSE
      KCR=KD
      ENDIF
C
C==== PUT CALC IN X() Y(), FIND MIN AND MAX, FORCE TO INCLUDE ZERO AXIS
  117 IF(KW.EQ.0)THEN
      CALL CALC(Q(1,KC),C(1,KC),E(1,KC),KCR,1,IPF,IPN,0.0,XM,XX,YM,YX)
      ELSE
      CALL SUBBKG(TEMP,C(1,KC),C(1,KW),NNMAX)
      CALL CALC(Q(1,KC),TEMP,E(1,KC),KCR,1,IPF,IPN,0.0,XM,XX,YM,YX)
      ENDIF
      IF(YM.GT.0.0)YM=0.0
      IF(YX.LT.0.0)YX=0.0
      RDEL=(YX-YM)*0.025*FLOAT(IIDEL)*0.5
      YM3=YM
C==== 18/2/92 store the calc X and Y
      DO 118 I=1,NC4(KC)
      YCALP(I)=Y(I)
  118 XCALP(I)=X(I)
      IF(KD.EQ.KC)GOTO 230
C
C==== FIND MIN AND MAX OF OBS, 18/2/92 fill x & y again here now, then copy
      IF(KW.LE.0)THEN
      CALL CALC(Q(1,KD),C(1,KD),E(1,KD),KD,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      ELSE
      CALL SUBBKG(TEMP,C(1,KD),C(1,KW),NNMAX)
      CALL CALC(Q(1,KD),TEMP,E(1,KD),KD,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      ENDIF
      DO 210 I=1,NC4(KD)
  210 YOBSP(I)=Y(I)
      DO 211 I=1,NC4(KD)
  211 EOBSP(I)=EP(I)
      IF(YM2.LT.YM)YM=YM2
      IF(YX2.GT.YX)YX=YX2
      IF(XM2.LT.XM)XM=XM2
      IF(XX2.GT.XX)XX=XX2
C
C====                          here  YX 
C====    zero must fall                  OBS and/or CALC
C====    somewhere on Y axis         YM
C====                                   XM          XX
C====
C==== 18/2/92 this used to do TRAN(OBS-CALC) not (TRANS(OBS) -TRANS(CALC))
C==== FIND MIN AND MAX OF OBS-CALC
      DO 215 I=1,NC4(KD)
  215 YDIFP(I)=YOBSP(I)-YCALP(I)
      YM2=0.0
      YX2=0.0
      IF(NC1(KD)*NC2(KD).LE.0)GOTO 217
      DO 216 I=NC1(KD),NC2(KD)
      IF(YDIFP(I).GT.YX2)YX2=YDIFP(I)
      IF(YDIFP(I).LT.YM2)YM2=YDIFP(I)
  216 CONTINUE
  217 IF(NC3(KD)*NC4(KD).LE.0)GOTO 219
      DO 218 I=NC3(KD),NC4(KD)
      IF(YDIFP(I).GT.YX2)YX2=YDIFP(I)
      IF(YDIFP(I).LT.YM2)YM2=YDIFP(I)
  218 CONTINUE
  219 RDEL=(YX-YM+YX2-YM2)*0.025*FLOAT(IIDEL)*0.5
C
      SHIFT=YM-YX2-2.*RDEL
C==== SHIFT OBS-CALC CLEAR OF OTHER GRAPHS
      YM3=YM
      YM=SHIFT+YM2
      DO 220 I=1,NC4(KD)
  220 YDIFP(I)=YDIFP(I)+SHIFT
C
  230 YX=YX+RDEL
      IF(YM.LT.(-RDEL))YM=YM-RDEL
C====
C
C====            top of screen  =   new YX= YX +RDEL
C====                            old YX 
C====                                      OBS and/or CALC
C====                       YM3 =old YM
C====                                   XM          XX
C====                            vertical gap 2*RDEL
C====                              
C====                            max of O-C =  SHIFT+YM2
C====                            SHIFT= zero axis for O-C= YM3-2.*RDEL-YM2
C====            bottom of screen=  new YM= SHIFT - YX2(min of O-C) -RDEL
C====
C==== if there is no O-C region then: if old YM > -RDEL new YM= old YM =YM3 
C====                                 else   new YM = YM3-RDEL
C====  i.e. this makes sure that bottom of graph is zero where sensible
C====   rather then forcing a small -ve Y axis region which looks silly
C====
C==== SPACE FOR ERROR BARS IS NOT CONSIDERED, IIDEL OPTION ALLOWS USER TO FORCE SPACE
      CALL LOCDEV(1,IDEV,IERR)
      CALL LOCAXIS(IDEV,IERR,XM,XX,YM,YX,IG1,IG2,ILOG)
      IE=0
      ILAST=0
      IF(KD.EQ.KC.AND.IPW.EQ.0.AND.ISQ.EQ.0.AND.IPQ.EQ.0.AND.IBT.EQ.0)ILAST=1
  250 J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.2)CALL LOCGRAPH(XCALP(J),YCALP(J),E(J,KCR),IJ,1,0,0,IDEV,IE)
C==== THESE CALL USED TO DRAW SOLID STRAIGHT LINE SEGMENTS FOR CALC 
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(IJ.GE.2)CALL LOCGRAPH(XCALP(J),YCALP(J),E(J,KCR),IJ,1,0,ILAST,IDEV,IE)
C
C============================== WTS ================================
      IF(IPW.EQ.0)GOTO 290
C==== PLOT SCALED WEIGHTS AS SOLID ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
C==== THIS CALL TO CALC TO FIN MAX AND MIN OF WT
      CALL CALC(Q(1,KD),E(1,KC),E(1,KD),KC,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
C==== ASSUME YM2 IS ZERO
C==== SCALE powers of ten then fill towards top of graph by a "round" number
C
C==== ALLOW FOR ( ALMOST IMPOSSIBLE) CASE WHERE ZERO IS AT TOP OF GRAPH
C==== NOTE WE ALWAYS FORCE ZERO LINE TO APPEAR
      IF(ABS(YX-RDEL).LT.RDEL)SH=YM3
      A=10.0
  262 IF(YX2*SC.LE.YX-SH)GOTO 263
      SC=SC/A
      GOTO 262
  263 IF(YX2*SC.LT.0.1*(YX-SH))GOTO 268
      B=1.0
      IF(YX2*SC.LT.0.65*(YX-SH))B=1.5
      IF(YX2*SC.LT.0.50*(YX-SH))B=2.0
      IF(YX2*SC.LT.0.40*(YX-SH))B=2.5
      IF(YX2*SC.LT.0.25*(YX-SH))B=4.0
      IF(YX2*SC.LT.0.20*(YX-SH))B=5.0
      SC=SC*B
      GOTO 274
  268 SC=SC*A
      GOTO 262
C==== SCALE WT ARRAY
  274 DO 280 I=1,NDIM
  280 Y(I)=Y(I)*SC+SH
C      IF(IDEV.EQ.2)WRITE(JF,281)SC,SH
  281 FORMAT(/,1X,'WEIGHTS *',1PE10.2,' + ',E11.3,/)
      J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KC),IJ,1,0,0,IDEV,0)
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(KD.EQ.KC.AND.ISQ.EQ.0.AND.IPQ.EQ.0.AND.IBT.EQ.0)ILAST=1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KC),IJ,1,0,ILAST,IDEV,0)
C
C================================ P(Q) =============================
  290 IF(IPQ.EQ.0)GOTO 1290
      KK=JPQ(KS)
C==== PLOT P(Q) AS SOLID ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
      CALL CALC(Q(1,KD),C(1,KK),E(1,KK),KK,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      J=NC1(KCR)
      IJ=NC2(KCR)-J+1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,IPQ,IPQSYM,0,IDEV,0)
      J=NC3(KCR)
      IJ=NC4(KCR)-J+1
      IF(KD.EQ.KK.AND.ISQ.EQ.0.AND.IBT.EQ.0)ILAST=1
      IF(J*IJ.GE.1)CALL LOCGRAPH(X(J),Y(J),E(J,KK),IJ,IPQ,IPQSYM,ILAST,IDEV,0)
C
C=================================== S(Q) ============================
 1290 CONTINUE
      KK=JSQ(KS)
C==== PLOT SCALED S(Q) AS SOLID ON TOP GRAPH,NOT OVER THE OBS-CALC PLOT
C==== THIS CALL TO CALC TO FIN MAX AND MIN OF S(Q)
      CALL CALC(Q(1,KD),C(1,KK),E(1,KK),KK,1,IPF,IPN,0.0,XM2,XX2,YM2,YX2)
      IF(ABS(YX-RDEL).LT.RDEL)SHS=YM3
      IF(YM2.GT.0)YM2=0.0
      YX2=YX2-YM2
      A=10.0
 2262 IF(YX2*SCS.LE.YX-SHS)GOTO 2263
      SCS=SCS/A
      GOTO 2262
 2263 IF(YX2*SCS.LT.0.1*(YX-SHS))GOTO 2268
      B=1.0
      IF(YX2*SCS.LT.0.65*(YX-SHS))B=1.5
      IF(YX2*SCS.LT.0.50*(YX-SHS))B=2.0
      IF(YX2*SCS.LT.0.40*(YX-SHS))B=2.5
      IF(YX2*SCS.LT.0.25*(YX-SHS))B=4.0
      IF(YX2*SCS.LT.0.20*(YX-SHS))B=5.0
      SCS=SCS*B
      GOTO 2274
 2268 SCS=SCS*A
      GOTO 2262
 2274 CONTINUE     
c 2274 WRITE(JS,998)SCS
  998 FORMAT(1X,' SCALE FACTOR ',E10.3)
      SCL=SCS
      RETURN
      END
      SUBROUTINE SETLAB2(L,N,LABEL)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      CHARACTER*80 LABEL
      DO I=1,20
      J=I*4-3
      K=I*4
      READ(LABEL(J:K),'(A4)')LAB2(I,L,N)
      END DO
      RETURN
      END
