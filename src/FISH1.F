C==== NEXT CARD IS A PFORT CONTROL CARD:  LIST, SYMBOL-TABLE, XREF-LIST, INTER-UNIT-CHECKS
C* L,NS,NR,P
      SUBROUTINE INIT
C==== This is the FISH data fitting program,
c====  copyright R.K.Heenan, RAL, 1989 and extensions to 1999 and current date.
C====
C==== STARTED ON ICL2960 AT UNIVERSITY OF KENT AS "XRAY"  JUNE 1983,
C====  MOVED TO RAL MAY 1984 AND RENAMED "LOQ" AND THEN "FISH"
C====
C====  ALL users must fully and openly acknowledge their use of FISH
C====   please reference  R.K.Heenan, Rutherford Appleton Laboratory Report
C====                      RAL-89-129
C====
C==== - after all it did take a very large effort to write and is being made
C====   available, ususally for free, in the interests of scientific progress.
C====
C====  This program or parts of it may not
C==== be distributed to others without permission of RKH.  Any such copies
C==== must include copyright declarations etc.  and again usage be acknowledged.
C====
C==== 05/12/06 g77 version that compiles on both Windows (under MinGW/GrWin)
C====          and Linux by Jonathan Rawle
C====
C==== 16/11/05 fix bugs in derivs of Hedgehog constraint 20
C==== 16/05/2005 fixed bug in end-on cylinders, model 8 or 10, LTYP = 61
C====                 - no-one complained in previous 5 years !
C==== 10/11/04 add Schultzsum to ARITH menu
c==== 15/03/04 increase /work/ from 60000 to 110000 to try MN=1024 not 512
C==== bug in y axis ranges of plots fixed, and model 29 ltyp 21 added.
c==== 3/12/3 add constraint 25, increase dimension of /TIE/ from 8 to 12
c==== 11/09/-03 add stuff to /DMAT/ NB there are now two VV( )
c==== 05/08/03 add VOLP() to /TWO/ and change derivs in FISH5
C==== 11/7/01 add OBS = -1 to skip set in model for multi-contrast fits
c====         saves writing new models for different combinations of contrasts
c====  2/11/00 try add K8=1 to switch off BETA(Q) correction to S(Q)
C====          stored in LS(3), improve RNILS reading
c====  1/11/00 found bug in rod*S(Q), calc of F(Q)
c====  3/8/00  add RNILS to read ILL data files
c====
c==== 14/4/00  add READ3 to read 3 column files with no header
c====
c==== 14/10/99 this version modified to run under LINUX as well as Windows & VMS
c====   system dependent routines (such as file OPEN and cursor etc) are now in
C====   FISHSYS.F    Note on LINUX file names are case sensitive !  All commmand
C==== file definitions FCOMMAIN.TXT etc are now LOWER case.  Included .INC and .PAR
c==== files names are UPPERCASE ( as there were too many to change ! )
c====
c==== 8/99  RKH, PGPLOT graphics replaced UNIRAS, all NAG algorithms finally
C==== removed. Now using original quadpack Gauss/Kronrod adaptive integrations.
C==== ( Using double prec codes, with single prec f(x) - check performance
c==== on PC. )  Rewrite i/o to find directory names in FISHPREF.TXT, and
c==== to prompt for names instead of using default FOR004.DAT etc, which
C==== on PC will over-write existing files rather than create a new version.
c====
c==== 3/93 RKH this version now has UNIRAS graphics, including an abbreviated
c==== parameter file output on hardcopy plots.
c==== Routine DELSET added to set up numerical derivative shifts.
C==== Arrays NMETH() etc. added to generalise the numerical integration
c==== schems, thereby starting to allow for multi-dimensional integrals -
c==== this system will require more work to become fully general for all
c==== models.  Meanwhile the old WTPIC etc. set up routines are still used.
c==== New rutine added for J1() Bessel function.  Once the ROD & ELLIPSE
c==== integrations are re-done then there will be no more dependence on NAG
C==== routines, allowing easier shift of the program to PC's etc.
c
C====  minor modification by A.Mackie, Food Res.Inst Norwich, incorporated
C====  11/3/88, including error trapping, OTOKO data input for more recent DL
C====  SAXS data, WTMEAN routine in LOQ3,
C====   FISH6 routines " MODEL" by M.Grimson does
C====  some special calculations, DESMEAR routine uses a Chris Vonk routine
C====   these are not usuallly available in distributed versions.
C====
C====  interacting particles and two shell ellipsoids added in collaboration
C====  with C.Fagotti, early 1989, with thanks to J.Hayter and J.Penfold for
C====  use of their routines.
C====
C====  No warranty is given that this code is free from BUGS or that it will
C==== always work as expected - however every effort has been made to test
C==== new fitting models.  Users of comlex models are recommended to study the
C==== FORTRAN code carefully and/or to generate test data externally to the
C==== program to check that a fit converges to the expected parameters.
C====
C====  VERY NAUGHTY OF THE FRIN PEOPLE TO INCLUDE TAB CHARACTERS IN THEIR
C====   NEW CODE. APOLOGIES TO ANYONE TRANSFERRING THIS TO A NON-VAX COMPUTER !
C====
C==== 3/88 MACHINE DEPENDENT GRAPHICS ROUTINES WILL NOW BE PUT INTO
C====    FISHGRAPH.FOR   INSTEAD OF AT THE END OF FISH2.FOR
C====
C==== beware of machine dependent underflow & overflow checks in FISHMODEL.FOR
C====  e.g. exp(-88.5) is smallest exponential allowed
C====
C====   DATA STORES CHANGED FROM 4096 TO 512 ON RETURN FROM RAL, JAN 1985
C==== ONE TIME CHANNEL OF 64*64 ARGONNE ARRAY OR LOWQ  "DAY 1" CONFIG.
C====
C==== THE DERIV ARRAY NEEDS EXPANDING TO STORE ALL DERIVS AT EACH DATA POINT
C==== SO THE CONVOLUTION FOR INSTRUMENT RESOLUTION BECOMES POSSIBLE.
C====  OOPS THE BLOCK DATA SECTION WAS NOT CORRECT 18/3/88
C====  COMMON BLOCK LENGTHS WERE AT ONE TIME CONSTANT, BUT NOW VARY
C====
C==== THIS CODE MUST REMAIN PORTABLE !!!! SO PLEASE USE STANDARD FORTRAN 4
C==== WHEREVER POSSIBLE.  HOWEVER IT DOES NOT CONFORM TO FULL (OUTDATED ??)
C==== "PFORT" STANDARDS, PARTICULARLY OVER CHANGES IN ARRAY BOUNDARIES INSIDE
C==== COMMON BLOCKS (E.G. NCH(9,7) IN /ONE/ )
C====
C==== COMPILER DEPENDANT POINTS TO WATCH FOR INCLUDE 'OPEN( )', SEE
C==== SETUP ROUTINE.  $ IN FORMATS, REWIND, SOME ARRAY INDICES
C====  E.G. IDAT(I*10+J+1),  DATE AND TIME ROUTINES.
C====
C==== AVOID VARIABLE NAMES OF >6 CHARACTERS WHICH ARE ALLOWED
C====  IN VAX FORTRAN ONLY.
C====
C==== KEEP ALL LOCAL GRAPHICS IN SEPARATE ROUTINES, SEE LOQ2
C====
C========1         2         3         4         5         6         7
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),
     >IDC(MW),NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     >RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
C==== SPACE FOR 6 DETECTOR FILES, SINGLE FIGURE GOOD FOR I1 FORMAT INPUT,
C==== THE MN DIMENSION IS PROGRAMMED IN DATIN, QBIN2 ETC
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
C==== LS2(1-4) are used as parameter pointers by POLCAL in FISH5
C==== LS(1-3), LS2(5-16) are really spare (6/1/89)
C====
C==== SPACE FOR 100 PARAMETERS IN INTENSITY CALC.
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      CHARACTER*1 ICH
      COMMON/CHAR/ICH(32),NICH
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
c     FIT added by JR to store persistent variables for TALK routine
      COMMON/JFIT/INFIT,ITIM
c==== here VVV is VV
      COMMON/DMAT/IID(MV),DWD(MI,MI),SVW(MI),VVV(MI,MI),COV(MI,MI)
       COMMON/POLY/PR1,PR2,NR1,NR2,ABC(5),RB,PA,PB,PCC,PD,IPD,JPD,NPP,
     *       NPP1,NPP2,NPP3,NPM,NSIMP
      COMMON/MAINC/MC1(32),MC2(32),MCLEN(32),MCSTR(20,32),NNCOM,MCD,
     *ISP(2),MP1(32),MP2(32),MPLEN(32),MPSTR(20,32),NNPOM,MPD
C
C==== BEWARE THE 9 dimension here is for 9 types of plot, not 9 workspaces !
C
C==== PROBLEM ?? .... AN LTYP() AND IY APPEAR IN /TWO/ AND /AXTYP/...ACCIDENTAL
C====                 VAR APPEARS IN /TWO/ AND /SLFIT/
      CHARACTER*4 LABAX
      COMMON/AXTYP/LABAX(2,9),IX(2),IIY(2),ILG(2),ILX(2),ILY(2),IX1(9),
     *IX2(9),ILOG,IGP,IG1,IG2,PMN(2,MW+1),PMX(2,MW+1),SHIFT(MW),
     *NSET(MW),LLTYP(MW),LSYM(MW),LTYP2(MW),LSYM2(MW),IPF(MW),IPN(MW),
     *IE(MW),KC,K
      COMMON/SLFIT/SLX1(MW),SLX2(MW),SLY1(MW),SLY2(MW),VVAR(MW),GRAD(MW),
     *RINT(MW),GRE(MW),RE(MW),RG1(MW),RG2(MW),NSL(MW),NUN(MW),RMID,
     *SLUM1(MW),SLUM2(MW),SLUM3(MW),SLUM4(MW)
      COMMON/BINPAR/NBIN(6),RBIN(5)
c==== THIS COMMON ADDED BY ASHLEY FOR MODEL 13
      COMMON/GAMFUN/DF(4),IGAMMA
C==== THIS FOR MODELS 18,21,24 RODS& ELLIPSES, RKH REDONE 5/96
      COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT,
     > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI,
     > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3,
     > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR,
     > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL,
     > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2,
     > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX,
     > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL
C==== the next two blocks are used by interacting particle in FISH7
      REAL*8 ASSQ,BSSQ,CSSQ,FSSQ
      COMMON /SQHPB/ ASSQ,BSSQ,CSSQ,FSSQ,ETA,GEK,AK,U,vv,GAMK,
     *                 SETA,SGEK,SAK,SCAL,G1,SIG
      REAL*8 dcoeff
C==== was   common /sqhpc/ dcoeff(4,5),coeff(6,5),ssqshift(4)
      common /sqhpc/ dcoeff(4,5),coeff(6,5)
c==== this common added by Cesare - to pass card number for start of model 24
      COMMON/MODEL24/JJJ
C==== generalise Gaussian quadratures etc.  RKH 26/3/93
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
C==== general work area - be careful as it is generally used by the least
C==== squares which need to be kept concurrently with lots of other activities.
      COMMON/WORK/RJUNK(110000)
C==== /WORK/  is largest in the least squares where it has DBIG(MI,MN)
C
C==== it is no longer set zero in block data
C==== 13/8/99 rewrite to avoid using system logical names
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
C
      DIMENSION INP(40)
      CHARACTER*256 STRING
C====
C==== =================================================================
      INFIT=0
      ITIM=0
c
      CALL SETUP(-2,STRING,IERROR)
      NDIM=MN
      DO J=1,MW
      DO I=1,MN
      Q(I,J)=0.0
      C(I,J)=0.0
      E(I,J)=0.0
      END DO
      END DO
C
      CALL FLUSH(JS)
      RETURN
      END
c     end of INIT initialisation and setup routine
c
c    2 FORMAT(40A1)
c   10 WRITE(JS,11)
c   11 FORMAT(1X,'Command> ',$)
c      READ(IS,'(A40)',ERR=10)STRING
C==== 23/1/89 start using real character variables, to make READ FILENAME
C==== possible,
C==== 12/8/99 use own version ready for PC  14/10/99 make only first word uppercase
c==== as filenames are case sensitive in Linux !!
c      CALL UPCASEone(STRING)
c      write(js,9999)string
c9999  format(50x,a)
c      READ(STRING,FMT=2,ERR=10)INP
c      I1=1
c      CALL SWITCH(I1,INP,40,MATCH,MATCH2,MC1,MC2,MCLEN,MCSTR,NNCOM)
c      IF(MATCH.EQ.0)THEN
c      WRITE(JS,12)
c   12 FORMAT(1X,'Command>   unrecognised command')
c      GOTO 10
c      END IF
c      GOTO(100,200,300,400,500,600,700,800,900,1000),MATCH
c      GOTO 10
c  100 GOTO(110,120,130,140,150,160),MATCH2
c      GOTO 10
c  110 CALL SETUP(ID,STRING(I1:40),IERROR)
c      IF(IERROR.EQ.0)CALL DATIN
c      GOTO 10
c  120 CALL GETDL
c      GOTO 10
c  130 CALL READBIN
c      GOTO 10
cc==== 14/4/00 add READ3, 3/11/00 add READ2 also
c  140 CALL SETUP(ID,STRING(I1:40),IERROR)
c      IF(IERROR.EQ.0)CALL DATIN3(3)
c      GOTO 10
c  150 CALL SETUP(ID,STRING(I1:40),IERROR)
c      IF(IERROR.EQ.0)CALL DATIN3(2)
c      GOTO 10
c==== 3/8/00 add RNILS, 3/11/00 RENUMBER TO 16, WAS 15
c  160 CALL SETUP(ID,STRING(I1:40),IERROR)
c      IF(IERROR.EQ.0)CALL RNILSILL(STRING(I1:40))
c      GOTO 10
c  200 CALL ARITH
c      GOTO 10
c  300 GOTO(310,320,330,340,350,360,370),MATCH2
c      GOTO 10
c  310 CALL CENTRE
c      GOTO 10
c  320 CALL QBIN
c      GOTO 10
c  330 CALL LRCALC
c      GOTO 10
c  340 CALL GETNUM(I1,40,INP,J,K,R,IFLAG)
c      IF(IFLAG.NE.0)GOTO 10
c      DO 341 I1=J,K
c  341 CALL RANGE1(I1)
c      GOTO 10
c  350 CALL WTMEAN
c      GOTO 10
c  360 CALL MODEL
c      GOTO 10
c  370 CALL DESMEAR
c      GOTO 10
c  400 CALL PICT
c      GOTO 10
c  500 CALL LEXI(MATCH2)
c      GOTO 10
c  600 CALL OUTP(0,0,0,0)
c      GOTO 10
c  700 CALL LSMAIN
c      GOTO 10
c  800 CALL SETUP(-1,STRING(I1:40),IERROR)
c      GOTO 10
c  900 STOP
c 1000 CALL HELPCOM(I1,INP,40,MCSTR,NNCOM,MCD)
c      GOTO 10
c      END
      BLOCK DATA
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NNN(28),
     >     LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      CHARACTER*1 ICH
      COMMON/CHAR/ICH(32),NICH
      COMMON/POLY/P(2),IP(2),P1(10),IP1(8)
      COMMON/BINPAR/NBIN(6),RBIN(5)
      CHARACTER*4 LABAX
      COMMON/AXTYP/LABAX(2,9),IX(2),IY(2),ILG(2),ILX(2),ILY(2),IX1(9),
     *IX2(9),ILOG,IGP,IG1,IG2,PMN(2,MW+1),PMX(2,MW+1),SHIFT(MW),
     *NSET(MW),LLTYP(MW),LSYM(MW),LTYP2(MW),LSYM2(MW),IPF(MW),IPN(MW),
     *IE(MW),KC,K
      COMMON/SLFIT/SLX1(MW),SLX2(MW),SLY1(MW),SLY2(MW),VVAR(MW),
     *GRAD(MW),RINT(MW),GRE(MW),RE(MW),RG1(MW),RG2(MW),NSL(MW),NUN(MW),
     *RMID,SLUM1(MW),SLUM2(MW),SLUM3(MW),SLUM4(MW)
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      DATA NNN/28*0/,
     >     LS2/16*0/,LM/MV*0/,LTYP/MV*0/,V/MV*0./,
     >     ESD/MV*0./,PS/MV*0./,IDEL/MV*-11/,DEL/MV*0./,DV/MV*0./,
     >     CON/24*0./
      DATA NTIE/MV*0/
      DATA ICH/'1','2','3','4','5','6','7','8','9','0',
     *         '.',',','=',' ','-',' ',' ',' ','P','F',
     *         'S','C','N','I','R','E','H','O','T','K',' ',' '/
C=== ICH(16-18) ARE SPARES, ICH(14) IS SPACE, 31-32 SPARES ADDED TO PAD
      DATA NICH/30/
      DATA P/2*0.0/,IP/2*0/,P1/10*0.0/,IP1/8*0/
      DATA NBIN/0,0,1485,166,501,0/,RBIN/.002,1.0, 1.608,6300.,1.0/
C====THESE DECIDE WHAT ROUTINE TRANS DOES FOR VARIOUS AXIS OPTIONS
      DATA LABAX/'LINE','AR  ','LOG1','0   ','LN  ','    ','GUIN','IER '
     * ,'ZIMM','    ','RODS','    ','SHEE','TS  ','USER','LOG-','LOG ',
     * '    '/,IX/1,1/,IY/0,0/,ILG/0,0/,ILX/0,0/,ILY/0,0/,
     * IX1/1,1,1,3,3,3,3,8,2/,IX2/1,2,2,2,4,5,6,9,2/,ILOG/1/,IGP/1/,
     * IG1/1/,IG2/1/
      DATA NUN/MW*0/
      DATA NIQMAX/8/,NWTMAX/2048/
      END
C
      SUBROUTINE FINDCOM
      COMMON/MAINC/MC1(32),MC2(32),MCLEN(32),MCSTR(20,32),NNCOM,MCD,
     *ISP(2),MP1(32),MP2(32),MPLEN(32),MPSTR(20,32),NNPOM,MPD
C==== 13/8/99 rewrite to avoid using system logical names
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      CHARACTER*256 STR
c
      STR='fcommain.txt'
      IF(LFSRC.GT.0)STR=FISHSRCE(1:LFSRC)//'fcommain.txt'
	CALL OPENSHARE(MCD,IERR,STR)
c	IF(IERR.EQ.1)WRITE(JS,901)STR
c901   FORMAT(1X,'ERROR - cannot open file:',/,a80,/)
C==== THE COMMANDS DEFINITION FILE CONTAINS AN I4 INTEGER, WHICH IS READ IN AS
C====   I3,I1, FOR CONTROL FLAGS, RETURNED BY SWITCH AS MATCH1 AND MATCH2
C====  THE NEXT PARAMETER IS THE MINIMUM LENGTH OF COMMAND REQUIRED
C====  THEN THE FULL COMMAND NAME ITSELF
      I=1
   10 READ(MCD,11,END=50,ERR=50)MC1(I),MC2(I),MCLEN(I),
     *                          (MCSTR(J,I),J=1,20)
   11 FORMAT(I3,I1,I3,1X,20A1)
      IF(MC1(I).EQ.0)GOTO 10
      I=I+1
      GOTO 10
   50 NNCOM=I-1
      I=1
      STR='fcomplot.txt'
      IF(LFSRC.GT.0)STR=FISHSRCE(1:LFSRC)//'fcomplot.txt'
      CALL OPENSHARE(MPD,IERR,STR)
c	IF(IERR.EQ.1)WRITE(JS,901)STR
  110 READ(MPD,11,END=150,ERR=150)MP1(I),MP2(I),MPLEN(I),
     *                          (MPSTR(J,I),J=1,20)
      IF(MP1(I).EQ.0)GOTO 110
      I=I+1
      GOTO 110
  150 NNPOM=I-1
      RETURN
      END
      SUBROUTINE SWITCH(I1,INP,INPL,MATCH,MATCH2,MC1,MC2,MCLEN,
     *                   MCSTR,NNCOM)
C==== ANALYSE STRING TO PULL OUT A COMMAND,  THERE ARE MORE SOPHISTICATED WAYS
C==== TO DO THIS, E.G. USING VAX-FORTRAN CHARACTER STRINGS, HOWEVER THE
C==== HELP FILE AND CHARACTER HANDLING HERE SHOULD BE COMPLETELY PORTABLE !!!
C====   VAX COMPUTERS HAVE THEIR OWN DCL STYLE COMMAND HANDLING UTILITIES
C====   AS USED E.G. BY GENIE OR COLETTE AT RAL.
C====
C==== ( FUNCTION KOMP IS ALSO USED BY ROUTINES NCHAR AND GETNUM , IT RETURNS
C====  ZERO IF TWO INTEGERS ARE THE SAME, AVOIDING INTEGER OVERFLOW)
      CHARACTER*1 ICH
      COMMON/CHAR/ICH(32),NICH
      DIMENSION MC1(32),MC2(32),MCLEN(32),MCSTR(20,32),INP(INPL)
      MATCH=0
   10 IF(I1.GT.INPL)RETURN
      IF(KOMP(ICH(14),INP(I1)).NE.0)GOTO 50
C====          SPACE            FIND FIRST WORD OF INP STRING
      I1=I1+1
      GOTO 10
   50 I2=I1+1
C==== STOP AFTER 20 CHARACTERS
   60 IF(I2.EQ.21)GOTO 100
      IF(KOMP(ICH(14),INP(I2)).EQ.0)GOTO 100
      I2=I2+1
      GOTO 60
  100 I3=I2-I1
      I2=I2-1
      DO 200 J=1,NNCOM
      IF(I3.LT.MCLEN(J))GOTO 200
C==== IF STRING INP() IS LONGER THAN THE MINIMUM REQUIRED IT MUST MATCH IN ALL
C==== THE SUPPLIED CHARACTERS
      K=MAX(MCLEN(J),I3)
      II=I1
      DO 150 JJ=1,K
      IF(KOMP(INP(II),MCSTR(JJ,J)).NE.0)GOTO 200
  150 II=II+1
      MATCH=MC1(J)
      MATCH2=MC2(J)
      GOTO 300
  200 CONTINUE
  300 I1=I2+2
C==== RESET I1 TO NEXT CHAR TO BE TESTED IF COMMAND INP() HAS A SECOND WORD
      RETURN
      END
      SUBROUTINE HELPCOM(I1,INP,INPL,MCSTR,NNCOM,MCD)
      CHARACTER*1 ICH
      COMMON/CHAR/ICH(32),NICH
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      DIMENSION MCSTR(20,32),INP(INPL),KST(72)
      ISHORT=0
C==== DEBUG
C====      WRITE(JS,9)I1,INP,MCSTR,NNCOM,MCD
C====    9 FORMAT(1X,I4,1X,20A1,/,8(80A1,/),'NNCOM',I4,' MCD',I4)
      IF(I1.EQ.3)ISHORT=1
   10 IF(I1.GT.INPL)GOTO 500
      IF(KOMP(ICH(14),INP(I1)).NE.0)GOTO 50
C====          SPACE            FIND FIRST WORD OF INP STRING
      I1=I1+1
      GOTO 10
   50 I2=I1+1
   60 IF(I2.EQ.21)GOTO 100
      IF(KOMP(ICH(14),INP(I2)).EQ.0)GOTO 100
      I2=I2+1
      GOTO 60
  100 I3=I2-I1
      I2=I2-1
      IF(I3.EQ.0)GOTO 500
      REWIND MCD
  300 READ(MCD,12,END=900,ERR=900)M1,M2,ML,KST
   12 FORMAT(I3,I1,I3,1X,72A1)
      IF(M1.EQ.0)GOTO 300
  350 J=1
      DO 400 II=I1,I2
      IF(KOMP(INP(II),KST(J)).NE.0)GOTO 300
  400 J=J+1
C==== FOUND SOMETHING TO PRINT
  450 WRITE(JS,502)KST
      READ(MCD,12,END=900,ERR=900)M1,M2,ML,KST
      IF(M1.NE.0)GOTO 350
      GOTO 450
C==== HELP ON ITS OWN PRINTS THE ENTIRE FILE, H JUST A SHORT LIST
  500 IF(ISHORT.EQ.1)GOTO 600
      REWIND MCD
  510 READ(MCD,501,END=900)KST
      WRITE(JS,502)KST
  501 FORMAT(8X,72A1)
  502 FORMAT(1X,72A1)
      GOTO 510
  600 WRITE(JS,611)((MCSTR(JJ,J),JJ=1,17),J=1,NNCOM)
  611 FORMAT(4(1X,17A1))
  900 RETURN
      END
c     Read data from currently open loq file, data set at INDEX
c     into data set no. SET
      SUBROUTINE DATIN(INDEX,ISET)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*76 FMT
      CHARACTER*1 L,L2
      COMMON/WORK/L(80),L2(80),FMTOLD(19),RJUNK(2400),FILL(107541)
      LOGICAL LOGRN
      INTEGER INDEX, ISET
c
c==== 3/11/00 set ERROR to ZERO not SQRT(data) for IFLAG=2
c
C==== USES REWIND(N) AND END= ERR= TYPE FORTRAN READ,  THESE MAY BE MACHINE DEPENDENT ==========
C==== DATA DECK  1) TITLE ETC. 20A4   ( FIRST 20 COLS ARE FOR DATE AND TIME THEN
C====                 SAMPLE NO., RESPONSE FILE NO. , BAKGR NO.)
C====            2) MORE TITLE AND OTHER INPUT E.G. LAMBDA, Q CALIB CONST.
C====            3) NCH(6),IDC    7I5   NMC IS CENTRE*10 ,IDC= 1-DET.CORRECTED,2-BINNED,-ALL ELSE
C====            4) NSUM,IC(3) 4I10
C====            5) IFLAG, FORMAT FOR REST OF DATA I2,1X,19A4
C====            6) DATA E.G. COUNTS PER CHANNEL
C==== IFLAG=1 C( ) CARDS ONLY, 2 - Q( ) C( ) CARDS, 3 - Q( ) C( ) ESD( ) CARDS
C====           ESD( )'S ARE ASSUMED TO BE RELATIVE ERRORS, SO ARE < 1.0
      NENDS=0
      REWIND ID
      WRITE(JS,10)ID,INDEX,ISET
   10 FORMAT(/,1X,'DATA INPUT ROUTINE, READS FROM FORTRAN STREAM NO.',I2,/,
     *1X,i3,'th set from file into set no. ',i2)
      III=-1
    5 III=III+1
      I=0
      IF(III.EQ.INDEX)I=ISET
      IF(III.GT.INDEX)I=99
      READ(ID,20,END=70,ERR=80)L
      READ(ID,20,END=80,ERR=80)L2
   20 FORMAT(20A4)
      WRITE(JS,21)L,L2
   21 FORMAT(1X,20A4)
c   22 WRITE(JS,30)
c   30 FORMAT(1X,'SAVE AS SET NO. = ? ',$)
   22 IEND=0
      IF(I.GT.90)IEND=I-90
    1 FORMAT(I2)
      NENDS=0
      IF(IEND.EQ.1)GOTO 100
      IF (IEND.EQ.5) GOTO 60
      IF(I.LE.0.OR.IEND.GT.1) GOTO 60
      IF(I.GT.MW)GOTO 22
      DO 25 J=1,20
      LAB2(J,1,I)=L(J)
   25 LAB2(J,2,I)=L2(J)
c
c      CALL GETLABEL(I)
c
   32 FORMAT(3A4)
      WRITE(JF,35)I,L,L2,(LAB(J,I),J=1,3)
   35 FORMAT(1X,'SET(',I2,' )  ',20A4,/,10X,20A4,/,5X,3A4,/)
      READ(ID,51,END=80,ERR=80)(NCH(I,J),J=1,7)
      READ(ID,52,END=80,ERR=80)(IC(I,J),J=1,4)
   51 FORMAT(16I5)
   52 FORMAT(8I10)
      N=NCH(I,1)
   53 FORMAT(I2,1X,19A4)
      READ(ID,53,END=80,ERR=80)IFLAG,FMT
      WRITE(JS,53)IFLAG,FMT
      IF(IFLAG-2) 57,590,594
   57 READ(ID,FMT,ERR=80,END=70)(C(J,I),J=1,N)
      DO 58 J=1,N
   58 Q(J,I)=J
  585 DO 586 J=1,N
  586 E(J,I)=0.0
      GOTO 595
  590 READ(ID,FMT,ERR=80,END=70)(Q(J,I),C(J,I),J=1,N)
      GOTO 585
  594 READ(ID,FMT,ERR=80,END=70)(Q(J,I),C(J,I),E(J,I),J=1,N)
  595 CONTINUE
      IF(NCH(I,7).NE.97)GOTO 110
C==== ONE TIME FUDGE FOR SOME CORRUPTED DATA
      DO 108 J=1,N
  108 C(J,I)=ABS(C(J,I))*1.E-05
  110 CONTINUE
C==== COMMENT OUT THE RESETTING OF NEGATIVE DATA 6/3/88 RKH
CC==== SKIP OUT IF WANT TO KEEP NEGATIVE COUNTS, SET NCH(7) TO -99
C  110 IF(NCH(I,7).EQ.-99)GOTO 600
C      DO 598 J=1,N
C      IF(C(J,I).LT.0.0)C(J,I)=1.01010101
C  598 CONTINUE
  600 IF(IC(I,1).GT.0)GOTO 5
      A=0.0
      DO 54 J=1,N
   54 A=A+C(J,I)
C==== COUNT UP NSUM OVER ALL DETECTOR, TO CALC EXPOSURE TIME FROM INITIAL COUNT RATE
      IF(A.GT.1.E08)A=A*1.E-6
C==== AVOID INTEGER OVERFLOW
      IC(I,1)=A
      GOTO 5
   60 READ(ID,51,END=80,ERR=80)N
C==== SKIP OVER DATA
      READ(ID,52)
      READ(ID,53,END=80,ERR=80)IFLAG,FMT
      N=N*IFLAG
      READ(ID,FMT,END=70,ERR=80)(RJUNK(J),J=1,N)
      IF (IEND.EQ.5) GOTO900
      IF(IEND.NE.0)RETURN
      GOTO 5
   70 WRITE(JS,71)
   71 FORMAT(/,1X,'END OF FILE IN DATIN - file rewound')
      WRITE(JF,71)
      NENDS=NENDS+1
C====  this to allow for empty files, which otherwise give an inifinite loop
      IF(NENDS.GT.2)GOTO 80
      GOTO 100
   80 WRITE(JS,81)
      WRITE(JF,81)
   81 FORMAT(1X,'ERROR READING FILE IN DATIN  - FILE WILL BE CLOSED ')
      CLOSE(UNIT=ID)
   90 RETURN
  100 REWIND ID
      GOTO 5
C==++++++++++
C== SEARCH FOR A RUN NUMBER
900   WRITE(JS,1000)
      READ(IS,*,ERR=900)IRNOS
1000  FORMAT(' ENTER RUN NUMBER FOR SEARCH ',$)
      LOGRN=.FALSE.
      DO WHILE (.NOT.LOGRN)
      READ(ID,1010,END=100,ERR=80)IRN
1010  FORMAT(44X,I6)
      IF (IRN.NE.IRNOS) THEN
      READ(ID,1020,END=1100,ERR=80)NT
1020  FORMAT(/,1X,I5)
      NT=INT(REAL(NT)/2)
      DO II=1,NT+2
      READ(ID,*,END=1100,ERR=80)
      ENDDO
      ELSE
      LOGRN=.TRUE.
      BACKSPACE ID
      ENDIF
      ENDDO
      GOTO5
C++ IF END OF FILE
1100  WRITE(JS,1110)
      GOTO 100
1110  FORMAT('  RUN NUMBER NOT FOUND: FILE REWOUND PLEASE REPEAT ')
      END
C====  ++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SETLABEL(I,LABEL)
C==== 3/11/00 silly routine, to get leading space on label,
c==== complicated due to historical way RKH stored 4 characters per integer
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*12 TEXT
      CHARACTER*12 LABEL
c      WRITE(JS,31)
c   31 FORMAT(1X,' ENTER LOCAL LABEL FOR THIS SET  (A11)=',$)
c      READ(IS,'(A11)')TEXT(2:12)
      TEXT(2:12)=LABEL
      TEXT(1:1)=' '
C      WRITE(JS,1001)TEXT
C1001  FORMAT(1X,'TEXT=',A12)
      READ(TEXT(1:4),'(A4)')LAB(1,I)
      READ(TEXT(5:8),'(A4)')LAB(2,I)
      READ(TEXT(9:12),'(A4)')LAB(3,I)
      WRITE(JS,1002)(LAB(J,I),J=1,3)
1002  FORMAT(1X,'LAB=',3A4)
      RETURN
      END

C====  ++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DATIN3(IFLAG,ISET,ISKIP)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*1 L,L2
      COMMON/WORK/L(80),L2(80),FMT(19),RJUNK(2400),FILL(107541)
      LOGICAL LOGRN
      CHARACTER*256 HEAD
c==== 14/4/00 READ3, IFLAG=3, for 3 column Q,I,E file
c==== 3/11/00 IFLAG=2 for Q,I only
C==== USES REWIND(N) AND END= ERR= TYPE FORTRAN READ,  THESE MAY BE MACHINE DEPENDENT ==========
c   22 WRITE(JS,30)
c   30 FORMAT(1X,'SAVE AS SET NO. = ? ',$)
c      READ(IS,1,ERR=22)I
      I=ISET
    1 FORMAT(I3)
      IF(I.LE.0.OR.I.GT.MW)RETURN
c	  DO J=1,20
c      WRITE( LAB2(J,1,I),25 )   '    '
c      WRITE( LAB2(J,2,I),25 )   '    '
c      END DO
c   25 FORMAT(A4)
c     Skip number of header lines passed as parameter ISKIP
      IF(ISKIP.GT.0)THEN
      DO J=1,ISKIP
      READ(ID,799,ERR=595,END=595)HEAD
  799 FORMAT(A256)
      END DO
      END IF
c     Read the data
      J=1
      IF(IFLAG.EQ.3)THEN
  592 READ(ID,*,ERR=595,END=595)Q(J,I),C(J,I),E(J,I)
      J=J+1
	goto 592
      ELSE
  594 READ(ID,*,ERR=595,END=595)Q(J,I),C(J,I)
      J=J+1
	goto 594
      END IF
C
  595 N=J-1
	NCH(I,1)=N
	NCH(I,2)=0
	NCH(I,3)=0
	NCH(I,4)=0
	NCH(I,5)=1
	NCH(I,6)=N
	IF(N.EQ.0)THEN
	WRITE(JS,91)
   91 FORMAT(1X,'No data in this file ?????',/)
      CLOSE(UNIT=ID)
	RETURN
	END IF
   80 WRITE(JS,81)N,Q(1,I),Q(N,I)
      WRITE(JF,81)N,Q(1,I),Q(N,I)
   81 FORMAT(1X,'Found ',I6,'  Data values,  Q=',1pe10.3,'  to ',e10.3,/)
      CLOSE(UNIT=ID)
c
c      CALL GETLABEL(I)
C
      DO J=1,3
      LAB2(J,1,I)=LAB(J,I)
      END DO
   32 FORMAT(3A4)
      WRITE(JF,35)I,L,L2,(LAB(J,I),J=1,3)
   35 FORMAT(1X,'SET(',I2,' )  ',20A4,/,10X,20A4,/,5X,3A4,/)
      IF(IFLAG.EQ.3)RETURN
      WRITE(JS,611)
  611 FORMAT(/,' Note - no errors with this data, if fitting best use K1=2',/
     >     ,' for  ERROR=SQRT(data),  else K1=1 for equal weights',/)
C==== ensure errors are actually set back to zero in a previously used workspace
      DO J=1,N
      E(J,I)=0.0
      END DO
      RETURN
      END
c====
c==== this code originally ILLIP3 from S.M.King, but mostly changed now by RKH
c==== 2/11/00 using info from ILL manual to make work in general case rather than ONLY
c==== skipping ~ 39 lines
      SUBROUTINE RNILSILL(ISET)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
c=      COMMON/ONE/NCH(MW),NC(MW,5),IDC(MW),NIC(MW,4),
c=    *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*1 L,L2
      COMMON/WORK/L(80),L2(80),FMT(19),RJUNK(2400),FILL(107541)
	integer*4	flag
	character*80	title,info,info2,info3
      character*24 cdat
      character*8 filename
      filename=''
1000	format(a80)
1001	format(1x,a80)
      REWIND ID
	read(ID,1000,ERR=29)info			! HEADER
      write(js,1001)info
        read(ID,1000,ERR=29)info2
      write(js,1001)info2
      read(ID,*)IRUN,IEXT,NDATA1,NDATA2,NSKIP,NSKIPP
      READ(ID,*)IVERS,NTXT,NPAR,NPARX,NPDFX,IERRS
      I=3+NTXT+NPAR+MIN(NPARX,20)+NPDFX
c
      IF(I.NE.NSKIP)WRITE(JS,111)I,NSKIP
 111  FORMAT(1X,'BEWARE will skip ',i3,'(minus 2 !) Lines (as line 3),',/,
     *  1x,' line 4 in file does not agree (sum=',i4,' )')
c
c===== read sections 4 through 8
	do i=1,NSKIP-2
	   read(ID,1000,ERR=29)info3
	end do
	GOTO 295
29    WRITE(JS,292)
292   FORMAT(1X,'ERROR reading header section of RNILS, ILL file',/)
      CLOSE(ID)
	RETURN
c====	END OF HEADER BLOCK
295   call today(cdat)
C      call trim1(FILENAME,FILENAME,I)
        title ='ILL '//cdat(1:24)//filename(1:I)//' '//info(1:(50-I))
C	write(js,1001)title
   22 WRITE(JS,30)
   30 FORMAT(1X,'SAVE AS SET NO. = ? ',$)
c      READ(IS,1,ERR=22)I
      I=ISET
    1 FORMAT(I3)
      IF(I.LE.0.OR.I.GT.MW)THEN
	CLOSE(ID)
	RETURN
	END IF
C
C	DO 25 J=1,20
c     LAB2(J,1,I)='    '
c  25 LAB2(J,2,I)='    '
C      WRITE( LAB2(J,1,I), '(A4)' )   '    '
C   25 WRITE( LAB2(J,2,I), '(A4)' )   '    '
C
      READ(TITLE,'(20A4)')(LAB2(J,1,I),J=1,20)
      J1=0
      J=1
c==== now read the data
  594 READ(ID,*,ERR=595,END=595)Q(J,I),C(J,I),E(J,I)
      J=J+1
C
      IF(J.GT.MN)THEN
	WRITE(JS,5945)
5945  FORMAT(1X,'Too many data points - truncated')
	GOTO 595
	END IF
	goto 594
c====
  595 N=J-1
      IF(N.LE.0)THEN
	WRITE(JS,91)
   91 FORMAT(1X,'No data in this file ?????',/)
      CLOSE(UNIT=ID)
	RETURN
	END IF

C==== find first non-zero point
      NCH(I,5)=N
      DO J=1,N
	IF(ABS(C(J,I)).GT.1.E-32)THEN
	NCH(I,5)=J
	GOTO 600
	END IF
	END DO
600	NCH(I,1)=N
	NCH(I,2)=0
	NCH(I,3)=0
	NCH(I,4)=0
C====  find last non-zero point
      NCH(I,6)=NCH(I,5)
      DO J=N,NCH(I,5),-1
	IF(ABS(C(J,I)).GT.1.E-32)THEN
	NCH(I,6)=J
	GOTO 610
      END IF
	END DO
610   write(js,'(7i6)')(nch(i,j),j=1,6)
C
c      CALL GETLABEL(I)
C
c      WRITE(JF,35)I,title,(LAB(J,I),J=1,3)
   35 FORMAT(1X,'SET(',I2,' )  ',a80,/,5X,3A4,/)
      CLOSE(UNIT=ID)
C
9999	return
	end
C====  ++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ARITH
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC(MW,5),IDC(MW),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
   10 WRITE(JS,11)
   11 FORMAT(1X,'1 : +,- OR SCALE, (SET(J=0) IS TAKEN AS 1.0 )',/,
     *       1X,'2 : RAISE TO A POWER',/,
     *       1X,'3 : NORMALISE',/,
     *       1X,'4 : DETECTOR CORR. ( DIV. BY DET.)'/,
     *       1X,'5 : SUM MODEL 1 TYPE 21 SCHULTZ'/,
     *       1X,'6 : RETURN  ')
C====    MATCHING OF CENTRES IS NOT YET PROGRAMMED  ==================
      READ(IS,2,ERR=10)IRT,II
    2 FORMAT(2I1)
      GOTO(100,500,200,300,600,900),IRT
      GOTO 10
  100 WRITE(JS,101)
  101 FORMAT(1X,'SET(K)=A*SET(I)+B*SET(J),   ENTER K,A,I,B,J (*)',$)
      READ(IS,*,ERR=10)K,A,I,B,J
      WRITE(JS,112)K,A,I,B,J
      IF(I*K.EQ.0)GOTO 10
      IF(J.EQ.0)GOTO 115
      IF(IDC(I).NE.IDC(J))WRITE(JS,111)
  115 WRITE(JS,114)
      READ(IS,2,ERR=10)II
      IF(II.NE.1)GOTO 10
  114 FORMAT(1X,'OK (ANS 1)',$)
  112 FORMAT(1X,'(',I2,' ) =',F10.4,' * (',I2,' ) +',F10.4,' * (',I2,
     *' )' )
      WRITE(JF,112)K,A,I,B,J
C==== ASSUME CENTRES ALIGNED , NO CHECKS ON Q'S,CHANGED TO CHECK CENTRES 15/11/83
      CALL LABEL(K)
      IF(J.EQ.0)GOTO 118
      IF(IDC(I).NE.IDC(J))WRITE(JF,111)
  111 FORMAT(1X,'WARNING - SET TYPES MAY BE INCOMPATIBLE')
  118 N=NCH(I)
      NJ=N
      JJ=0
      IF(J.NE.0)JJ=NC(J,3)-NC(I,3)
      IF(NC(I,1)*NC(I,2).LE.0)JJ=0
C==== CORRECT FOR DIFF IN CENTRES, IGNORE IF NO LEFT SIDE DATA ANYWAY
C==== WHEN USERS SHOULD CHECK THAT Q'S MATCH !!!
      JJ=JJ/10
      IF(J.NE.0)NJ=NCH(J)
      DO 120 II=1,N
      AA=A*C(II,I)
      E1=A*E(II,I)
      BB=B
      IF(J.EQ.0)GOTO 119
      JJ=JJ+1
      IF(JJ.LT.1.OR.JJ.GT.NJ)GOTO 117
      BB=BB*C(JJ,J)
      E1=E1+B*E(JJ,J)
      GOTO 119
  117 E(II,K)=0.0
      C(II,K)=1.0101010101
      GOTO 120
  119 E(II,I)=E1
      C(II,K)=AA+BB
  120 Q(II,K)=Q(II,I)
C==== COPY CHANNEL RANGES FROM SET I
      DO 122 II=1,4
  122 IC(K,II)=0
      IDC(K)=IDC(I)
      NCH(K)=N
      DO 124 II=1,5
  124 NC(K,II)=NC(I,II)
      GOTO 10
  200 IF(II.NE.0)GOTO 210
      WRITE(JS,201)
  201 FORMAT(1X,'NORMALISE SET(I), I=? ',$)
      READ(IS,2,ERR=10)II
  210 CALL RANGE1(II)
      AA=0.0
      BB=0.0
      I=NC(II,1)
      J=NC(II,2)
      IF(I*J.LE.0) GOTO 222
      DO 220 K=I,J
      BB=BB+1.
  220 AA=AA+C(K,II)
  222 J=NC(II,4)
      N=NC(II,5)
      DO 225 K=J,N
      BB=BB+1.
  225 AA=AA+C(K,II)
      N=NCH(II)
      IC(II,1)=IFIX(AA)
      CC=0.0
      DD=0.0
      IF(AA.LT.1.E-06)GOTO 235
      CC=BB/AA
      DD=1.0/CC
  240 DO 230 K=1,N
      E(K,II)=E(K,II)*CC
  230 C(K,II)=C(K,II)*CC
  235 WRITE(JF,231)II,AA,NC(II,1),NC(II,2),NC(II,4),NC(II,5),BB,CC,DD
  231 FORMAT(1X,'SET(',I2,' )  SUM=',E12.6,' OVER ',4I6,F6.0,' DATA',/,
     *'  *BY',1PE13.6,' = *1.0/ ',1PE13.5)
      WRITE(JS,231)II,AA,NC(II,1),NC(II,2),NC(II,4),NC(II,5),BB,CC,DD
      GOTO 10
  300 WRITE(JS,301)
  301 FORMAT(1X,'DIVIDE SET(I) BY SET(J=DETECTOR) IJ=? (2I1) ',$)
      READ(IS,2,ERR=10)I,J
      II=NC(I,3)
      JJ=NC(J,3)
      WRITE(JS,321)I,IDC(I),J,IDC(J),II,JJ
  321 FORMAT(1X,'SET(',I2,' )  IDC=',I3,' TO BE DIVIDED BY SET(',I2,
     *' )  IDC=',I3/,' CENTRES(*10)=',2I6,' WILL BE MATCHED, NO CHECK',
     *' ON Q')
      WRITE(JS,114)
      READ(IS,2,ERR=300)K
      IF(K.NE.1)GOTO 10
      N=NCH(I)
      NJ=NCH(J)
      JJ=JJ-II
      IF(NC(I,1)*NC(I,2).LE.0)JJ=0
      JJ=JJ/10
      DO 320 K=1,N
      JJ=JJ+1
      IF(JJ.LT.1.OR.JJ.GT.NJ)GOTO 318
      AA=C(JJ,J)
      IF(ABS(AA).LE.1.E-6)GOTO 318
      E2=E(JJ,J)/AA
      BB=C(K,I)
      C(K,I)=BB/AA
      IF(ABS(BB).LE.1.E-6)GOTO 319
      E1=E(K,I)/BB
      E(K,I)=SQRT(E1*E1+E2*E2)*C(K,I)
      GOTO 320
  318 C(K,I)=1.01010101
  319 E(K,I)=0.0
  320 CONTINUE
      WRITE(JF,321)I,IDC(I),J,IDC(J)
      IDC(I)=1
      GOTO 10
  500 WRITE(JS,590)
      READ(IS,591,ERR=500)K,I,A
      WRITE(JS,592)K,I,A
      WRITE(JS,593)
      READ(IS,594,ERR=500)II
      IF(II.NE.1)GOTO 10
      WRITE(JF,592)K,I,A
      CALL LABEL(K)
      NPTS=NCH(I)
      DO J=1,NPTS
      C(J,K)=C(J,I)**A
c==== this error was wrong 15/3/88
c====      E(J,K)=E(J,I)**A    E'= E* d(C**A)/dC =E*A*C**(A-1) = E*A*C'/C
c====     this could be prone to divide by zero !!!
      E(J,K)=E(J,I)*A*C(J,K)/C(J,I)
      Q(J,K)=Q(J,I)
      END DO
      DO 522 II=1,4
  522 IC(K,II)=IC(I,II)
      IDC(K)=IDC(I)
      NCH(K)=NPTS
      DO 524 II=1,5
  524 NC(K,II)=NC(I,II)
      GOTO 10
  590 FORMAT(/1X,'Enter SET(N) = SET(M)**A   (N,M,A)',$)
  591 FORMAT(2I2,F10.0)
  592 FORMAT(/1X,'SET(',I1,') = SET(',I1,')**',F5.2)
  593 FORMAT(1X,' OK ? (1/0)')
  594 FORMAT(I2)
  600 CALL SCHULTZSUM
      GOTO 10
  900 RETURN
      END
      SUBROUTINE RANGE1(II)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC(MW,6),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      DIMENSION IK(5)
      K=0
   50 WRITE(JS,211)II,(NC(II,I),I=1,5)
  211 FORMAT(1X,'SET(',I2,' ) RUNS  FROM  L1=',I5,' TO L2=',I5,
     *' (SKIP IF L1*L2<=0 )',/,1X,'CENTRE*10=',I5,'  AND R1=',I5,
     *' TO R2=',I5)
      WRITE(JS,114)
  114 FORMAT(1X,'OK ? (ANS 1 )',$)
      READ(IS,1,ERR=50)I
    1 FORMAT(2I1)
      IF(I.EQ.1.AND.K.EQ.0)RETURN
      IF(I.EQ.1)GOTO 400
      WRITE(JS,301)
  301 FORMAT(1X,'ENTER ALL 5 NOS. , FORMAT(*), -VE TO',
     *' RETAIN PREVIOUS ',$)
      READ(IS,*,ERR=50)IK
      DO 320 J=1,5
      IF(IK(J).GE.0)NC(II,J)=IK(J)
  320 CONTINUE
      K=1
      GOTO 50
  400 WRITE(JF,211)II,(NC(II,I),I=1,5)
      RETURN
      END
      SUBROUTINE RANGE(II,IMIN,IMAX)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC(MW,6),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      DIMENSION IK(5)
      K=0
      NC(II,4)=IMIN
      NC(II,5)=IMAX
   50 WRITE(JS,211)II,(NC(II,I),I=1,5)
  211 FORMAT(1X,'SET(',I2,' ) RUNS  FROM  L1=',I5,' TO L2=',I5,
     *' (SKIP IF L1*L2<=0 )',/,1X,'CENTRE*10=',I5,'  AND R1=',I5,
     *' TO R2=',I5)
      WRITE(JF,211)II,(NC(II,I),I=1,5)
      RETURN
      END
      SUBROUTINE CENTRE
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     *RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
   10 WRITE(JS,101)
  101 FORMAT(1X,'FIND CENTRE OF SET(I) TO NEAREST 0.5*CHANNEL NO.',
     */,1X,'I= ? ',$)
      READ(IS,2,ERR=10)I
      IF(I.EQ.0)RETURN
    2 FORMAT(2I1)
   20 WRITE(JS,102)
  102 FORMAT(1X,'L1,L2,MIDPT*10 =?   (*), ZEROS TO STOP ',$)
      READ(IS,*,ERR=20)L1,L2,K
      IF(L1.LE.0.OR.L2.LE.0.OR.K.LE.0)GOTO 300
  141 FORMAT(1X,'SET(',I2,' )   L1=',I5,'  L2=',I5)
      K1=K-30
      K2=K+30
      IF(K2/5-L1.GT.NCH(I))GOTO 20
      IF(K1.LT.10*L2)GOTO 20
      IF(L2-L1.LT.2)GOTO 20
      WRITE(JF,141)I,L1,L2
      DO 200 KK=K1,K2,5
      A=0.0
      DO 150 L=L1,L2
      IR=KK/5-L
      B=C(L,I)-C(IR,I)
  150 A=A+B*B
      WRITE(JS,151)KK,A
      WRITE(JF,151)KK,A
  151 FORMAT(5X,I5,E11.4)
  200 CONTINUE
      GOTO 20
  300 WRITE(JS,301)NMC(I)
  301 FORMAT(1X,'OLD CENTRE*10',I5,/,1X,'NEW CENTRE*10=?   (*) ',$)
      READ(IS,*,ERR=300)K
      WRITE(JF,303)NMC(I),K
  303 FORMAT(1X,'OLD CENTRE*10=',I5,'   NEW CENTRE*10=',I5)
      IF(K.GT.0)NMC(I)=K
      RETURN
      END
      SUBROUTINE QBIN
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     *RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      COMMON/BINPAR/NBIN(6),RBIN(5)
      COMMON/WORK/NCOUNT(MN),DQ,QC,A,TEMP(MN),ET(MN),FILL(106925)
      DATA PI/3.141592654/
    1 WRITE(JS,101)
  101 FORMAT(1X,'CALC Q & QBIN SET(I) INTO SET(J),  I,J=?  (2I1)',/,
     *' PUT J=0 TO ONLY CALC Q FOR SET I, I=J IS OK')
      READ(IS,2,ERR=1)II,JJ
    2 FORMAT(2I1)
      IF(II.EQ.0) RETURN
   10 WRITE(JS,114)II,NC1(II),NC2(II),NMC(II),NC3(II),NC4(II),NBIN,RBIN
  114 FORMAT(1X,'1) THIS RANGE, SET(',I2,') ',5I7,/
     *       1X,'2) PREVIOUS RANGE      ',5I7,/
     *       1X,'3) USE PREVIOUS(=1) ',I2,/
     *       1X,'4) DELTA Q          ',F10.6,/
     *       1X,'5) QMIN/DELTA Q     ',F4.1,/
     *       1X,'6) WAVELENGTH       ',F10.6,/
     *       1X,'7) SAMPLE-DETECTOR  ',F10.3,' RECALCS Q IF SD/PS>0'/
     *       1X,'8) PIXEL SIZE       ',F10.3,/
     *1X, '  CHANGE NO. ? ',$)
      READ(IS,2,ERR=10)I
      IF(I.LE.0.OR.I.GT.8)GOTO 110
      IF(I.NE.1)GOTO 920
      CALL RANGE1(II)
  920 IF(I.LE.2)GOTO 10
      IF(I.NE.3)GOTO 940
      NBIN(6)=MOD(NBIN(6)+1,2)
      GOTO 10
  940 I=I-3
      WRITE(JS,941)
  941 FORMAT(1X,'VALUE = ',$)
      READ(IS,*,ERR=10)RBIN(I)
      GOTO 10
  110 DQ=RBIN(1)
      A=RBIN(2)
      QC=RBIN(4)/RBIN(5)
      RLAM=RBIN(3)
      IF(NBIN(6).EQ.0)GOTO 112
      NC1(II)=NBIN(1)
      NC2(II)=NBIN(2)
      NMC(II)=NBIN(3)
      NC3(II)=NBIN(4)
      NC4(II)=NBIN(5)
  112 WRITE(JF,211)II,NC1(II),NC2(II),NMC(II),NC3(II),NC4(II)
  211 FORMAT(1X,'SET(',I2,' ) RUNS  FROM  L1=',I5,' TO L2=',I5,
     *' (SKIP IF L1*L2<=0 )',/,1X,'CENTRE*10=',I5,'  AND R1=',I5,
     *' TO R2=',I5)
      WRITE(JF,113)II,JJ,DQ,A,QC,RLAM
  113 FORMAT(1X,'QBIN SET(',I2,' ) INTO SET(',I2,' )',/,1X,'DQ=',
     *1PE10.4,'  QMIN/DQ=',0PF6.2,'  SD/PIXEL=',1PE13.6,
     *'  WAVELENGTH=',E13.6)
      IF(QC.LE.0.0)GOTO 118
      RLAM=4.0*PI/RLAM
      AA=FLOAT(NMC(II))/10.0
      DO 115 I=1,NDIM
      THETA=0.5*ATAN(ABS(FLOAT(I)-AA)/QC)
      Q(I,II)=RLAM*SIN(THETA)
  115 CONTINUE
  118 IF(JJ.EQ.0)RETURN
      DO 120 I=1,NDIM
      ET(I)=0.0
      TEMP(I)=0.0
  120 NCOUNT(I)=0
      CALL QBIN2(II,JJ,NC1(II),NC2(II) )
      CALL QBIN2(II,JJ,NC3(II),NC4(II) )
      AA=A*DQ-DQ
      J=0
      IR=-1
      K=0
      DO 300 I=1,NDIM
      AA=AA+DQ
C==== LEAVE SPACE AT LOW AND HIGH Q TO EXTEND CALC BEYOND RANGE OF OBS IN REFINE AND PLOT
      J=J+1
      Q(J,JJ)=AA
      IF(IR)250,260,270
C==== IR=-1, LOOK FOR FIRST OCCUPIED BIN
  250 IF(NCOUNT(I).GT.0)GOTO 252
  251 C(J,JJ)=1.0101010101
      E(J,JJ)=1.0101010101
      GOTO 300
  252 IR=0
      J1=J
  255 QC=1.0/FLOAT(NCOUNT(I))
      C(J,JJ)=TEMP(I)*QC
      NCOUNT(J)=NCOUNT(I)
C==== J IS ALWAYS .LE. I
      E(J,JJ)=ET(I)
C====  THIS ASSUMES ALL ERRORS ARE FROM COUNTING STATISTICS =============
C====      IF(NCOUNT(I).GT.1)E(J,JJ)=E(J,JJ)*SQRT(QC)*QC this form removed
C==== as only works if all E( ) in a bin are similar. Corrected 10/10/84 to
      E(J,JJ)=SQRT(ABS(ET(I)))*QC
C==== ====================================================================
      GOTO 300
C==== IR=0 RECORD ALL OCCUPIED BINS UNTIL NEXT EMPTY ONE THEN SET IR=1
  260 IF(NCOUNT(I).GT.0)GOTO 255
      IR=1
      J2=J-1
      GOTO 251
C==== IR=+1  COUNT UP ANY OCCUPIED BINS LEFT OVER
  270 IF(NCOUNT(I).LE.0)GOTO 251
      K=K+NCOUNT(I)
      GOTO 251
  300 CONTINUE
      WRITE(JS,301)K
  301 FORMAT(1X,I5,' POINTS OF CHOSEN RANGE WERE IGNORED')
  305 WRITE(JS,302)
  302 FORMAT(1X,'DIAGNOSTIC PRINT OUT ? 1-SCREEN(SHORT) ',
     *'2-SCREEN(LONG)',/,
     *       1X,'                       3-FILE        0-CONTINUE ')
      READ(IS,2,ERR=305)IR
      IR=IR+1
      GOTO(310,320,320,330),IR
      GOTO 305
  310 IF(NBIN(6).EQ.1)GOTO 312
      NBIN(1)=NC1(II)
      NBIN(2)=NC2(II)
      NBIN(3)=NMC(II)
      NBIN(4)=NC3(II)
      NBIN(5)=NC4(II)
  312 NCH(JJ)=MIN(NDIM,J2+10)
      NC1(JJ)=0
      NC2(JJ)=0
      NC3(JJ)=J1
      NC4(JJ)=J2
      NMC(JJ)=0
      NSUM(JJ)=0
      DO 315 K=1,3
  315 LAB(K,JJ)=LAB(K,II)
      IC1(JJ)=IC1(II)
      IC2(JJ)=IC2(II)
      IC3(JJ)=IC3(II)
      IDC(JJ)=2
  316 WRITE(JS,317)
  317 FORMAT(1X,'WRITE BINNED SET TO FILE ? (ANS 1) ',$)
      READ(IS,2,ERR=316)IR
      IF(IR.EQ.1) CALL OUTP(JJ,1,II,2)
C====                   SET=JJ,1=TO FILE,TITLE=II,ROUTE=2
      RETURN
  320 IJ=JS
C==== SCREEN  :  SHORTENED FORMATS
      IF(IR.EQ.2.OR.JJ.EQ.II)GOTO 1340
      WRITE(IJ,337)II
      K=NC1(II)
      KK=NC2(II)
      IF(K.GT.0.AND.KK.GE.K)WRITE(IJ,1336)(I,Q(I,II),C(I,II),I=K,KK)
      K=NC3(II)
      KK=NC4(II)
      WRITE(IJ,1336)(I,Q(I,II),C(I,II),I=K,KK)
 1340 WRITE(IJ,337)JJ
      WRITE(IJ,1341)(I,NCOUNT(I),Q(I,JJ),C(I,JJ),I=1,J2)
 1336 FORMAT(2(I5,0PF10.4,1PE11.4))
 1341 FORMAT(2(I5,I3,0PF10.4,1PE11.4,3X))
      GOTO 305
  330 IJ=JF
C==== FILE  :  LONG OUTPUT ALWAYS, WIDER FORMATS TO SAVE PAPER
      IF(II.EQ.JJ)GOTO 340
      WRITE(IJ,337)II
      K=NC1(II)
      KK=NC2(II)
      IF(K.GT.0.AND.KK.GE.K)WRITE(IJ,336)(I,Q(I,II),C(I,II),I=K,KK)
      K=NC3(II)
      KK=NC4(II)
      WRITE(IJ,336)(I,Q(I,II),C(I,II),I=K,KK)
  336 FORMAT(5(I5,0PF10.4,1PE11.4))
  337 FORMAT(1X,'SET(',I2,' )')
  340 WRITE(IJ,337)JJ
      WRITE(IJ,341)(I,NCOUNT(I),Q(I,JJ),C(I,JJ),I=1,J2)
  341 FORMAT(4(I5,I3,0PF10.4,1PE11.4,3X))
      GOTO 305
      END
      SUBROUTINE QBIN2(II,JJ,N1,N2)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     *RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      COMMON/WORK/NCOUNT(MN),DQ,QC,A,TEMP(MN),ET(MN),FILL(106925)
      NBUG=0
      IF(N1*N2.LE.0)RETURN
      QMIN=A*DQ-DQ
      DO 100 I=N1,N2
      NQ=IFIX(  ( ABS(Q(I,II))-QMIN )/DQ   + 0.5)
      IF(NQ.LT.1)GOTO 200
      IF(NQ.GT.NDIM)GOTO 200
      TEMP(NQ)=TEMP(NQ)+C(I,II)
      ET(NQ)=ET(NQ)+E(I,II)**2
      NCOUNT(NQ)=NCOUNT(NQ)+1
      GOTO 100
  200 IF(NBUG.GT.6)GOTO 100
      WRITE(JS,201)I,Q(I,II),NQ
      WRITE(JF,201)I,Q(I,II),NQ
  201 FORMAT(1X,'POINT',I5,' IGNORED, Q=',F10.6,' NQ=',I5)
      NBUG=NBUG+1
  100 CONTINUE
      RETURN
      END
      SUBROUTINE SETUP(II1,FILENAME,IERROR)
      CHARACTER*256 FILENAME
      INCLUDE 'FISHDIM.PAR'
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      COMMON/MAINC/MC1(32),MC2(32),MCLEN(32),MCSTR(20,32),NNCOM,MCD,
     *ISP(2),MP1(32),MP2(32),MPLEN(32),MPSTR(20,32),NNPOM,MPD
C==== SET UP I/O CHANNELS,  II=-2 TO INITIALISE, II=-1 INTERACTIVE
C====                       II=N  to set channel N to FILENAME
C==== INPUTS  IS-SCREEN/TERMINAL, ID-DATA FILE(SEE DATIN), IL=LSQ FILE
C====     UNIT 11 IS USED BY GETWIR FOR BINARY FILES,
C====     IT IS ALWAYS CLOSED AFTER USE.
C==== OUTPUTS JS-       "         JD-NEW DATA FILE,        JL-NEW LSQ FILE
C====         JP-PLOT FILE (DEFAULT IS 10 FOR GINO )
C====         JF-FILE TO STORE A HARD COPY RECORD OF WHAT YOU DID
C====            LIST IT TO A PRINTER TO KEEP FOR REFERENCE
C==== 13/8/99 rewrite to avoid using system logical names
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
C      DIMENSION STR(6)
C      DATA STR/'        ','    ,   ',4*'        '/
      CHARACTER*256 STR
      LOGICAL LEX,LOP
      CHARACTER*1 ICH
      COMMON/CHAR/ICH(32),NICH
c	integer*2 II2
      IERROR=0
C
      II=II1
C===========================================================================
100   IF(II.EQ.-2)THEN
C==== DEFINE CHANNELS FOR MAIN AND PLOT COMMAND FILES
      MCD=15
      MPD=16
      IS=5
      JS=6
      JF=2
      ID=1
      JD=4
      IL=3
      JL=8
      JP=10
C==== 16/8/99 move main intro here, before messages from READFISHPREF
      WRITE(JS,1)MW,MN,MF,MV,MI,MC
    1 FORMAT(/,/,10X,'Welcome to the FISH4a 24/12/2008 fitting program by: ',
     >/,10x,'Dr.R.K.Heenan,ISIS facility,RAL,Didcot, OX11 0QX, UK',
     >/,10x,'Java frontend by Jonathan Rawle, Diamond/ISIS',
     >/,10x,'15/11/05 constraint 20 bug fix, 10/11/04 schultz in ARITH',
     >/,10x,'17/03/04 added 29 21,  MAR 2004 new constraint 25',
     >/,10x,'OCT 2002 new model 14 71 & 14 81 (Kholodenko worm) ',
     >/,10x,'<= 2002 re-set polydisp radius to MEAN, sticky S(Q),',
     >/,10x,'analytic Schultz, constraint 24',
     >/,10x,'   report bugs & send comments to r.k.heenan@rl.ac.uk ',/,
     >/,10x,'   (PLEASE acknowledge use of this program.)',/,
     >/,10X,i3,' sets of length',i6,',  simultaneous fit to',i3,' sets,',
     >/,10X,i3,' parameter model with',i4,' adjusting or tied, ',
     >   i4,' constraints',
     */,10x,' enter HELP  after  Command>   if you are lost.',/)
C
C==== 13/8/99 go find the directories for the files !
c      CALL READFISHPREF
c     Always set system to LNX now. Other preferences will be passed
c     from the Java front end
      SYS='LNX'
C
C==== READ IN THE COMMAND FILES
      CALL FINDCOM
C==== 14/6/93 this was still done when replaced by line in RUNFISH.COM
C==== so removed here !
C===      INQUIRE(UNIT=JF,OPENED=LOP)
C==== 16/8/99 open logging file
      STR='fishlog.txt'
      IF(LFPLT.GT.0)STR=FISHPLOT(1:LFPLT)//'fishlog.txt'
      CALL OPENNEW(JF,IERR,STR)
      IERROR=IERROR+IERR
C
c==== 16/8/99 open LSINP.DAT model file
      STR=FISHLSIN(1:LFLSP)
      CALL OPENSHARE(IL,IERR,STR)
      IERROR=IERROR+IERR
C
c==== 12/10/99 put system calls in separate routine
      CALL STARTSYS
      IF(IERROR.GT.0)GOTO 910
      RETURN
C==========================================================================
c==== interactively change a file
c      ELSE IF(II.EQ.-1)THEN
c      WRITE(JS,202)
c      DO 280 IC=1,30
c      INQUIRE(ERR=280,UNIT=IC,OPENED=LOP,NAME=STR)
c      IF(.NOT.LOP)GOTO 280
c      WRITE(JS,201)IC,STR,LOP
c201   FORMAT(1X,I4,:3X,A,' open=',L1)
c  202 FORMAT(1X,' 1-FORMATTED DATA, 4-NEW DATA, 3-LSQ FILE, 8-NEW LSQ,
c     * 2-MONITOR',/)
c  280 CONTINUE
cC
c      WRITE(JS,101)
c  101 FORMAT(1X,'REDEFINE CHANNEL NO. = ? ',$)
c      READ(IS,102,ERR=100)IC
cC
c      IF(IC.EQ.0)GOTO 200
c  102 FORMAT(I1)
c  103 FORMAT(A)
c      CLOSE(UNIT=IC)
c      WRITE(JS,111)
c      READ(IS,103,ERR=100)STR
c  111 FORMAT(1X,'ENTER FILE NAME e.g. PLOT.DAT  ? ',$)
c      IF(IC.EQ.1.OR.IC.EQ.3)THEN
cC==== check for existence of an input file
c      INQUIRE(ERR=900,FILE=STR,EXIST=LEX)
c      IF(.NOT.LEX)GOTO 900
c      CALL OPENSHARE(IC,IERR,STR)
c      ELSE
c      CALL OPENANY(IC,IERR,STR)
c      ENDIF
	IF(IERR.EQ.1)GOTO 910
C
  200 RETURN
C===========================================================================
C====   open a single file
      ELSE
      IF(FILENAME.EQ.'    ')RETURN
      CALL OPENSHARE(II,IERROR,FILENAME)
      RETURN
      END IF
C===========================================================================
C
  900 WRITE(JS,901)STR
  901 FORMAT(/,1X,'ERROR OPENING FILE :  ',/,1x,A)
  910 II=-1
      GOTO 100
      END
C
      SUBROUTINE READFISHPREF
C==== 13/8/99 rewrite to avoid using system logical names
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
C==== READ IN THE SYSTEM TYPE, COMMAND FILE LOCCATIONS ETC
      CHARACTER*80 TEXT
	TEXT='fishpref.txt'
      CALL OPENSHARE(ID,IERR,TEXT)
	IF(IERR.EQ.1)GOTO 900
 5    READ(ID,101,END=910)TEXT
      CALL TRIM1(TEXT,TEXT,L)
      IF(L.EQ.0.OR.TEXT(1:1).EQ.'!')GOTO 5
      L=MIN(L,3)
      SYS(1:L)=TEXT(1:L)
      CALL UPCASE(SYS)
      IF(SYS.EQ.'WIN'.OR.SYS.EQ.'VAX'.OR.SYS.EQ.'LNX')GOTO 10
      WRITE(JS,501)SYS
501   FORMAT(1X,'ERROR in FISHPREF.TXT,  SYS= ',a3,/,
     >  1x,' only VAX or WIN or LNX allowed ! ',/)
      STOP
10    READ(ID,101,END=910)TEXT
      CALL TRIM1(TEXT,TEXT,L)
      IF(L.EQ.0.OR.TEXT(1:1).EQ.'!')GOTO 10
      FISHSRCE(1:L)=TEXT(1:L)
      LFSRC=L
20    READ(ID,101,END=910)TEXT
      CALL TRIM1(TEXT,TEXT,L)
      IF(L.EQ.0.OR.TEXT(1:1).EQ.'!')GOTO 20
      FISHPLOT(1:L)=TEXT(1:L)
      LFPLT=L
30    READ(ID,101,END=910)TEXT
      CALL TRIM1(TEXT,TEXT,L)
      IF(L.EQ.0.OR.TEXT(1:1).EQ.'!')GOTO 30
      FISHLSIN(1:L)=TEXT(1:L)
      LFLSP=L
C
  101 FORMAT(A80)
      WRITE(JS,1001)SYS,FISHSRCE(1:LFSRC),FISHPLOT(1:LFPLT),
     >FISHLSIN(1:LFLSP)
1001  FORMAT(1X,'FISHPREF.TXT has:  ',/,
     >1x,'system                       : ',A3,/,
     >1x,'source directory             : ',A,/,
     >1x,'fishlog & plot file directory: ',A,/,
     >1x,'LSINP.dat model file is      : ',A,/
     > )
      RETURN
  900 WRITE(JS,901)
  901 FORMAT(/,1X,'ERROR OPENING   FISHPREF.TXT  ',/,
     > ' which contains vital directory names ! '/)
      STOP
  910 WRITE(JS,911)
  911 FORMAT(/,1X,'unexpected end of file  FISHPREF.TXT  ',/,
     > ' which contains vital directory names ! '/)
      STOP
      END
c====
C
      SUBROUTINE TRIM1(A,B,L)
      CHARACTER *(*) A,B
      INTEGER L
C==== 12/8/99 RKH's portable string trimmer
c==== 3/11/00 change to allow spaces in string for windows file names
c==== with spaces.  Thus count down from top end in second loop !
      N=LEN(A)
c==== 3/11/00 remove any trailing spaces
      DO I=N,1,-1
      IASC=ICHAR(A(I:I))
      IF(IASC.GE.33.AND.IASC.LE.126)GOTO 5
      END DO
5     N=I
C
c==== find first non-space
      DO I=1,N
      IASC=ICHAR(A(I:I))
      IF (IASC.GE.33.AND.IASC.LE.126)GOTO 10
      END DO
      B=' '
      L=0
      RETURN
10    I1=I
      J=0
C==== N.B.string B may be overwriting A
      DO I=I1,N
      IASC=ICHAR(A(I:I))
c==== 3/11/00 allow iasc=32, space, also.
C==== this check is now really redundant, but will spot any funny
c==== characters part way along the string.
      IF (IASC.LT.32.OR.IASC.GT.126)GOTO 20
      J=J+1
      B(J:J)=A(I:I)
      END DO
20    L=J
      RETURN
      END
C
      SUBROUTINE UPCASEone(A)
c====  14/10/99 only change up to the first space !
      CHARACTER *(*) A
      INTEGER I,I1,L
C==== 12/8/99 RKH's portable uppercase converter
      L=LEN(A)
	DO I1=1,L
C==== find first non-space
	IF(A(I1:I1).NE.' ')THEN
        DO I=I1,L
c====     quit at first space found
	  IF(A(I:I).EQ.' ')RETURN
        IASC=ICHAR(A(I:I))
        IF (IASC.GE.97.AND.IASC.LE.122)A(I:I)=CHAR(IASC-32)
        END DO
	  RETURN
	END IF
	END DO
      RETURN
      END
C
      SUBROUTINE UPCASE(A)
      CHARACTER *(*) A
      INTEGER L
C==== 12/8/99 RKH's portable uppercase converter
      L=LEN(A)
      DO I=1,L
      IASC=ICHAR(A(I:I))
      IF (IASC.GE.97.AND.IASC.LE.122)A(I:I)=CHAR(IASC-32)
      END DO
      RETURN
      END
c
      SUBROUTINE JAVATESTF
      INCLUDE 'FISHDIM.PAR'
      CHARACTER*4 TITLE
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),
     >IDC(MW),NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     >RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      NDIM=1
      LAB(1,1)='4'
      TITLE='BCDE'
      READ(TITLE, '(A4)' )LAB(1,1)
      TITLE='FGHI'
      READ(TITLE, '(A4)' )LAB(2,1)
      NC1(3)=1
      C(1,1)=C(1,1)+0.004
      RETURN
      END

      SUBROUTINE JAVATESTF2
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),
     >IDC(MW),NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     >RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      NDIM=NDIM+1
      LAB2(4,3,2)='1'
      RETURN
      END

