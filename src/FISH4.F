C==== 13/6/01 constraint 23
C==== 4/12/00 constraint 22, 1/4/99 added constraint 21
c
      SUBROUTINE NCHAR(IP,L,IST,N1,N2)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== THIS FINDS ONE OR TWO SIGNIFICANT LETTERS IN STRING IST()
C==== THEN LEAVES POINTER AT NEXT NON-LETTER
      CHARACTER*1 ICH, IST(L)
      COMMON/CHAR/ICH(32),NICH
      DIMENSION N(2)
      N(1)=0
      N(2)=0
   10 J1=IP
      J2=1
      DO 100 J=J1,L
      DO 50 K=1,NICH
C====      IF( KOMP(IST(J),ICH(K)).EQ.0)GOTO 80
      IF(IST(J).EQ.ICH(K)) GOTO 80
   50 CONTINUE
C==== IF NO MATCH, IGNORE THIS CHAR.
      GOTO 100
   80 IF(K.EQ.14)GOTO 100
C==== IGNORE SPACES
      IF(K.LT.18)GOTO 150
      IF(J2.EQ.3)GOTO 100
      N(J2)=K
      J2=J2+1
  100 IP=IP+1
C==== ON RETURN IP IS THE NEXT CHAR. TO NEEDLOOKING AT
  150 N1=N(1)
      N2=N(2)
      RETURN
      END
      SUBROUTINE GETNUM(IP,L,IST,N1,N2,R,IFLAG)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      CHARACTER*1 IST
      DIMENSION IST(L),N(2)
C==== FIND A NUMBER, TEST FOR .,= OR SPACE,       N1 AND N2 DEFAULT TO 1
C==== ALLOWED FORMS ARE n,m       n m   n=r n=ON  n=OFF  WHERE r=nnn.nnnn ETC.
C==== IFLAG=0 FOR n m,  1- FOR = r,2- FOR TIE, 3- FOR OFF, 4- FOR ON
      CHARACTER*1 ICH
      COMMON/CHAR/ICH(32),NICH
C==== THIS ROUTINE USES NUMERICAL POSITIONS OF LETTERS O,T AND N, BEWARE IF CHANGE ICH()
      COMMON/CH/ISS,ID,IL,JS,JD,JF,JL,JP
      D=0.1
      N(1)=0
      ISP=1
      IEQ=0
      N(2)=0
      R=0.0
      RS=1.0
      IFLAG=0
      IDP=0
      J1=IP
      IS=1
      DO 1000 J=J1,L
      DO 50 K=1,NICH
C====      IF(KOMP(IST(J),ICH(K)).EQ.0)GOTO 80
      IF(IST(J).EQ.ICH(K))GOTO 80
   50 CONTINUE
      GOTO 1000
   80 IF(K.GT.10) GOTO 200
      IF(K.EQ.10)K=0
      IF(IEQ.EQ.1)GOTO 90
C==== BEFORE AN =
      N(ISP)=N(ISP)*10+ K
      GOTO 1000
   90 IF(IDP.EQ.1) GOTO 100
      R=R*10.0+RS*FLOAT(K)
      GOTO 150
  100 R=R+D*RS*FLOAT(K)
      D=D*0.1
  150 IF(IS.EQ.1) GOTO 1000
C==== TEST FOR UNFULFILLED MINUS SIGN, MUST WORK FOR -.2 AND -0.2 ETC.
      IF(ABS(R).LT.1.E-10)GOTO 1000
      R=-R
      RS=-RS
      IS=1
      GOTO 1000
  200 J2=K-10
      GOTO(211,212,213,212,215),J2
C====       .   ,   =  SPACE -
      IF(IEQ.EQ.1)GOTO 1200
C==== IGNORE ALL OTHER CHARS BEFORE =, AFTER = BRANCH OUT, ASSUME ON/OFF/TIE
      GOTO 1000
  211 IF(IEQ.EQ.0)GOTO 212
      IDP=1
C==== AFTER A DECIMAL, AFTER AN =
      GOTO 1000
  212 IF(ISP.EQ.2)GOTO 1000
C==== THIS HERE IN CASE OF LEADING SPACES
      IF(N(1).GT.0)ISP=2
      GOTO 1000
C==== N(1) MUST BE SET BEFORE AN =, AT PRESENT THERE ARE NO " LETTER= " COMMANDS
C==== EXCEPT AT THE END OF =ON=n.m
  213 IEQ=1
      GOTO 1000
  215 IS=-1
      GOTO 1000
C==== 900 WRITE(JS,901)J,L,(IST(I),I=1,L)
C==== 901 FORMAT(1X,'GETNUM ERROR AT',I3,' IN',I3,' STRING',/,80A1)
C====     IFLAG=5
C====     RETURN
 1000 IP=IP+1
      IF(IEQ.EQ.1)IFLAG=1
      GOTO 1400
C==== TEST FOR  =ON OR =OFF
 1200 CALL NCHAR(IP,L,IST,M1,M2)
      IF(M1.EQ.28)M1=M2
C====  LETTER  O
      IFLAG=3
C====  SET TO OFF AS DEFAULT
      IF(M1.EQ.23)IFLAG=4
C====           N
      IF(M1.EQ.29)IFLAG=2
C====            T
C====  =ON OR =O SET TO ON
 1400 IF(N(1).EQ.0)N(1)=1
      N1=N(1)
      N2=MAX(N1,N(2))
      RETURN
      END
      FUNCTION KOMP(I,J)
C==== COMPARES CHARACTERS STORED IN INTEGER FORM
C==== AVOIDS I.EQ.J SINCE (I-J) ON SOME COMPUTERS MIGHT GIVE INTEGER
C==== OVERFLOW IF I AND J ARE OF OPPOSITE SIGNS
      KOMP=1
      IF(I.LT.0)GOTO 20
      IF(J.LT.0)RETURN
   10 KOMP=I-J
      RETURN
   20 IF(J.LT.0)GOTO 10
      RETURN
      END
      SUBROUTINE TALK(STRING)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== SOME CHECKS ON SUBSCRIPT RANGES ARE MADE, CHANGE PROG. IF YOU CHANGE DIMENSIONS
      INCLUDE 'FISHDIM.PAR'
      CHARACTER*1 IST
      DIMENSION IST(80)
      CHARACTER*80 STRING,STR,OUTFILE
      LOGICAL LOP
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),LS(8),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      COMMON/POLY/PR1,PR2,NR1,NR2,ABC(5),RB,PA,PB,PCC,PD,IPD,JPD,NPP,
     *       NPP1,NPP2,NPP3,NPM,NSIMP
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      COMMON/JFIT/INFIT,ITIM
c      DATA ITIM/0/
C==== 13/8/99 rewrite to avoid using system logical names
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
c
      IF(ITIM.EQ.0)WRITE(JS,1)
    1 FORMAT(1X,'FITTING/CALCULATION ROUTINE, TYPE PP TO SEE THE MODEL,',
     * /,1X,'RUN TO CALCULATE, HELP FOR A LIST OF COMMANDS')
C==== NON RELIABLE FORTRAN, ITIM MAY NOT BE OK IN AN OVERLAY
      ITIM=ITIM+1
C      CALL FPRMPT('T:',2)
      L=80
      IF(INFIT.EQ.0)THEN
      IR=0
      INFIT=1
      ELSE
      IR=1
      ENDIF
      GOTO 103
C==== IR CONTROLS WHETHER REFINE APPLIES SHIFTS CALC IN PREVIOUS CYCLE
C==== WHEN IK.NE.1
   10 CALL FLUSH(JS)
      RETURN
  102 FORMAT(1X,'> ')   
c      READ(IS,'(A80)')STRING
  103 CALL UPCASE(STRING)
      WRITE(JS,104)STRING
  104 FORMAT(1X,'> ',A80)
      READ(STRING,FMT=101,ERR=10)(IST(I),I=1,L)
  101 FORMAT(80A1)
      IP=1
      CALL NCHAR(IP,L,IST,N1,N2)
      IF(IP.GT.L.AND.N1.EQ.0)GOTO 10
      IF(N1.EQ.0)GOTO 105
C==== SKIP TO 105  IF NO LETTERS, JUST NUMBERS
C====     WRITE(JS,102)N1,N2
C==== 102 FORMAT(1X,'N1=',I4,' N2=',I4)
      N1=N1-18
C==== SORT OUT SINGLE LETTER COMMANDS
      GOTO(100,2020,100,100,100,2400,2500,100,2700,2800,100,100),N1
C====       P     F   S   C   N    I    R    E   H  ON/OFF  T   K
C==== TEST FOR JUST LETTERS
  100 IF(IP.GT.L)GOTO 1925
  105 CALL GETNUM(IP,L,IST,N3,N4,R,IFLAG)
C====     WRITE(JS,106)N3,N3,R,IFLAG
C==== 106 FORMAT(1X,'N3=',I4,' N4=',I4,' R=',F10.4,' IFLAG=',I2)
      IF(IFLAG.EQ.5)GOTO 10
      IF(IFLAG.NE.0) GOTO 300
C==== PRINT OUT ONLY
  115 IF(N1.EQ.0)GOTO 210
  120 GOTO(1925,10,2100,2200,2300,10,10,10,10,10,1929,3000),N1
C====       P    F   S    C    N   I  R  E  H  O  T   K
C==== TREAT REST AS PRINT V()
  210 CALL PROUT(2,N3,N4,JS)
      GOTO 10
 2100 CALL PROUT(3,N3,N4,JS)
      GOTO 10
 3000 CALL PROUT(0,N3,N4,JS)
      GOTO 10
 2200 CALL CONOP(N3,N4,JS)
      GOTO 10
 2300 CALL PROUT(5,N3,N4,JS)
      GOTO 10
  300 IF(IFLAG.NE.1)GOTO 400
C====  PARAM =R  TYPES
      GOTO(1950,10,2150,2250,2350,10,10,10,10,10,2950,3050),N1
C====       P=      S=    C=  N=                  T=   K=
C==== WITH NO CHARS, ASSUME PARAMS V
      DO 310 I=N3,N4
      V(I)=R
      ESD(I)=0.0
  310 DV(I)=0.0
C==== 9/4/97 force reset of integrations on change of PSI angle in MODEL 18
c==== oriented rods.
      DO I=N3,N4
      IF(LM(I).EQ.18.AND.(MOD(LTYP(I),10).GE.7))GOTO 1956
      END DO
      GOTO 210
C
 1950 WRITE(JS,1951)N3,N4
 1951 FORMAT(1X,'REDEFINE PARAMETERS',I3,' TO',I3)
      DO 1955 I=N3,N4
19511 WRITE(JS,1952)I
 1952 FORMAT(1X,'P',I3,' MODEL,LTYP=  (*)')
      READ(IS,*,ERR=19511)LM(I),LTYP(I)
      DV(I)=0.0
      ESD(I)=0.0
19531 WRITE(JS,1953)
 1953 FORMAT(1X,'LABEL=  (3A4)')
 1955 READ(IS,1954,ERR=19531)(LPAR(J,I),J=1,3)
 1954 FORMAT(3A4)
      NP=MAX(NP,N4)
 1956 WRITE(JS,19532)
19532 FORMAT(1X,'WARNING - all numerical derivs & integrations will be',
     > ' reset !')
      CALL DELSET(0)
C==== initialise all integration schemes
      NIQ=0
      DO III=1,MV
      NMETH(III)=-11
      IQ(III)=0
      END DO
C
      DO III=1,NIQMAX
      IB(III)=0
      NGPT(III)=0
      END DO
C
      GOTO 210
 2150 WRITE(JS,2151)
 2151 FORMAT(1X,'TO CHANGE SETS YOU MUST FIRST "STOP"')
      GOTO 2100
 2250 CALL CONINP(N3,N4,IS,0)
      CALL CONOP(N3,N4,JF)
      GOTO 2200
 2350 N4=MIN(N4,24)
      DO 2355 I=N3,N4
 2355 CON(I)=R
      NN=MAX(N4,NN)
      CALL PROUT(5,N3,N4,JF)
      GOTO 2300
C===  TT PRINTS OBS-CALC TABLE ON SCREEN, MAY LOOK A MESS AS OUTPUT TOO WIDE
 2929 DO 2970 J=1,NS
      KD=JD(J)
      IF(KD.EQ.-1)GOTO 2970
      KC=JC(J)
      WRITE(JS,2961)KD,KC
      MS=LS(4)
      J1=NC1(KD)
      J2=NC4(KD)
      IF(J1*NC2(KD).LE.0)J1=NC3(KD)
      DO 2969 I=J1,J2,MS
      IF(I.GT.NC2(KD).AND.I.LT.NC3(KD))GOTO 2970
      R=C(I,KD)-C(I,KC)
 2969 WRITE(JS,2963)I,Q(I,KD),C(I,KD),C(I,KC),E(I,KC),R
 2963 FORMAT(I6,1PE10.3,2E11.3,E9.1,E11.3)
 2961 FORMAT(10X,'Q',9X,'OBS(',I2,')',3X,'CALC(',I2,')',3X,'WT',
     *7X,'OBS-CALC')
 2970 CONTINUE
      GOTO 10
 2950 N4=MIN(N4,4)
      DO 2955 I=N3,N4
29511 WRITE(JS,2951)I
 2951 FORMAT(1X,'TITLE',I3,' =  (80A1)')
 2955 READ(IS,101,ERR=29511)(LCOM(J,I),J=1,80)
      NT=MAX(NT,N4)
      CALL PROUT(1,N3,N4,JF)
      GOTO 1929
C==== Kn=    CHANGE ONLY IW, IK, IP, MS OR INTEGRATION SCHEMES FOR Q &R,IN LS( )
 3050 DO 3055 I=N3,N4
      IF((I.GE.1.AND.I.LE.4).OR.I.EQ.8)THEN
      LS(I)=IFIX(R+sign(0.5,R))
      IF(I.EQ.1)NOWT=1
      IF(I.EQ.8)WRITE(JS,3051)LS(I)
      IF(I.EQ.8)WRITE(JF,3051)LS(I)
 3051 FORMAT(1X,'EXCLUDE BETA(Q) correction to S(Q) = ',I2,
     >  '  ( 1-exclude, 0-include)' )
      ELSE IF(I.LE.9)THEN
C==== initialise all integration schemes
      NIQ=0
      DO III=1,MV
      NMETH(III)=-11
      IQ(III)=0
      END DO
C
      DO III=1,NIQMAX
      IB(III)=0
      NGPT(III)=0
      END DO
      END IF

C==== CAN BE USED TO FORCE A RECALCULATION OF THE WEIGHT MATRIX
 3055 CONTINUE
      CALL PROUT(0,N3,N4,JF)
      GOTO 3000
C==== SET PARAM =ON ,OFF OR TIE
  400 IF(N1.NE.0) GOTO 10
      R=FLOAT(IFLAG-3)
      IF(IFLAG.EQ.2.OR.IP.GT.L)GOTO 411
C==== CHECK FOR FURTHER NUMBERS  AS  =ON=r OR =OFF=r WHICH SET PARTIAL SHIFTS
      CALL NCHAR(IP,L,IST,N1,N2)
      IF(IP.GT.L)GOTO 411
      CALL GETNUM(IP,L,IST,N1,N2,R,I)
      R=ABS(R)
  411 DO 410 I=N3,N4
      IF(IFLAG.NE.2)GOTO 405
      IF(NTIE(I).GT.0)GOTO 404
      WRITE(JS,401)I
  401 FORMAT(1X,'CANNOT TIE V(',I3,' ) SINCE NO CONSTRAINT FOR IT')
      GOTO 410
  404 NUSE(NTIE(I))=1
      GOTO 408
  405 IF(PS(I).GT.(-0.1))GOTO 408
      IF(NTIE(I).GT.0)NUSE(NTIE(I))=0
  408 PS(I)=R
  410 CONTINUE
      GOTO 210
 1900 N3=1
      N4=9999
      IF(N2.EQ.0)GOTO 210
      N2=N2-18
      GOTO (1902,1920,2100,2200,2300,1924,1902,1902,1902,1928,1929,
     *        1929),N2
C====       PP    (P)F (P)S PC   PN   PI                   PO    PT
C====          PK
 1902 CALL PROUT(9,N3,N3,JS)
      GOTO 10
 1920 CALL TABLE(JF)
      I=NDAT-NPRED
      IF(IR.EQ.1)WRITE(JF,2511)NYC,I,NPRED,NPR,SSE,XDWE,VAR
      CALL PROUT(9,N3,N4,JF)
      CALL CORREL(JF)
C==== PRINT OUT POLYDISP.
      DO 1904 I=1,NS
      KY=JY(I)
      IF(KY.LE.0)GOTO 1904
      IF(NCH(KY).EQ.799.OR.NPP.EQ.0)GOTO 1904
      WRITE(JF,1921)KY
      WRITE(JS,1921)KY
      WRITE(JF,1922)(Q(J,KY),C(J,KY),J=NR1,NR2)
 1921 FORMAT(1X,'SET(',I3,' )   LIST POLYDISPERSITY FUNCTION')
 1922 FORMAT( 3(0PF12.7,1PE12.5) )
      WRITE(JF,2512)
 1904 CONTINUE
      GOTO 10
C==== SINGLE LETTERS, ASSUME (P)X TYPE WITH NO P, EXCEPT CC, ST AND TT
 1925 IF(N1.EQ.4.AND.N2.EQ.22)GOTO 2222
      IF(N1.EQ.3.AND.N2.EQ.29)GOTO 2526
      IF(N1.EQ.11.AND.N2.EQ.29)GOTO 2929
      IF(N1.NE.1)N2=N1+18
      GOTO 1900
 1924 IF(LS(4).NE.1)WRITE(JS,501)LS(4)
      WRITE(JF,1926)
      CALL PICT
 1926 FORMAT(/1X,'PICT ROUTINE')
      GOTO 11
 1928 IF(LS(4).NE.1)WRITE(JS,501)LS(4)
      WRITE(JF,1927)
      CALL PICT2
 1927 FORMAT(/1X,'PLOT ROUTINE')
      GOTO 11
C==== FF FILL FILE, NEW LSQ MODEL FILE
C==== 16/8/99 prompt for s name !
c==== Now implemented in subroutine LSQOUT
 2020 LOP=.FALSE.
      INQUIRE(ERR=2023,UNIT=JL,OPENED=LOP,NAME=OUTFILE)
c
 2023 IF(.NOT.LOP)THEN
      WRITE(JS,2022)
 2022 FORMAT(1X,'Output filename ( this FISH session)= ')
c      READ(IS,'(A80)')OUTFILE
      OUTFILE='TESTFILE.DAT'
      CALL OPENNEW(JL,IERR,OUTFILE)
	IF(IERR.EQ.1)GOTO 2020
      END IF
c
      CALL PROUT(9,1,999,JL)
      CALL FLUSH(JL)
      WRITE(JF,2021)OUTFILE
 2021 FORMAT(/1X,'FF - parameters written to:',/,1X,A80)
      GOTO 10
 1929 CALL PROUT(0,N3,N4,JS)
      CALL PROUT(1,1,3,JS)
      GOTO 10
C==== RUN OR RETURN
 2500 WRITE(JF,2512)
 2512 FORMAT(1X)
      IF(N2.EQ.26)GOTO 2526
      IF(IR.EQ.0)CALL CONTIE(1,NC)
      IF(NYC.EQ.0)CALL PROUT(0,0,0,JF)
      IF(NYC.EQ.0)CALL PROUT(1,0,0,JF)
      CALL REFINE(IR)
      IR=1
      NYC=NYC+1
      IF(NOREF.EQ.1)GOTO 2520
      I=NDAT-NPRED
      WRITE(JS,2511)NYC,I,NPRED,NPR,SSE,XDWE,VAR
      WRITE(JF,2511)NYC,I,NPRED,NPR,SSE,XDWE,VAR
 2511 FORMAT(1X,'CYC',I3,I5,' DATA+',I2,' PRED,',I3,
     *' PAR  SWSE=',1PE10.3,
     *' XDWE=',E10.3,' VAR=',E10.3)
      XGOF=XDWE/SSE
      WRITE(JS,2513)XGOF
 2513 FORMAT(1X,'GOF=',E10.3)
      CALL PROUT(2,1,NP,JF)
      CALL PROUT(3,1,NS,JF)
      GOTO 10
 2520 WRITE(JS,2521)SSE
      WRITE(JF,2521)SSE
 2521 FORMAT(1X,'NOREF=1   SSE=',1PE10.3)
      CALL PROUT(2,1,NP,JF)
      WRITE(JF,2512)
      GOTO 10
C==== STOP
 2526 IF(LS(4).NE.1)WRITE(JS,501)LS(4)
  501 FORMAT(1X,'BEWARE, GAPS IN CALC, MS=K9=',I3,/)
C      CALL FPRMPT('Data:',5)
      RETURN
 2222 CALL CORREL(JS)
      GOTO 10
C====  GLOBAL ON/OFF SWITCH , OVER-RIDES THE PS( ) ARRAY, set TBA array to zero
C==== so OFF followed by ON is a good way to recover from over large shifts.
 2800 NOREF=0
      IF(N2.NE.20)GOTO 10
      NOREF=1
      DO 2810 I=1,NP
 2810 DV(I)=0.0
      GOTO 10
C==== HELP  LISTS A SEPARATE FILE
C23456789012345678901234567890123456789012345678901234567890123456789012
 2700 STR=FISHSRCE(1:LFSRC)//'fhelpfit.txt'
      CALL OPENSHARE(11,IERR,STR)
	IF(IERR.EQ.1)GOTO 10
 2705 READ(11,101,END=2706,ERR=2706)IST
      WRITE(JS,2701)IST
      GOTO 2705
 2701 FORMAT(1X,80A1)
 2706 CLOSE(UNIT=11)
      GOTO 10
C==== INDEX
 2400 CALL LEXI(0)
      IF(NS.EQ.0)GOTO 10
      WRITE(JS,2403)NS
      DO 2402 I=1,NS
      K=JD(I)
      IF(K.GT.0)WRITE(JS,2404,ERR=10)(LAB(LLL,K),LLL=1,3),JD(I),JC(I),
     *  JB(I),JY(I),JW(I),JAM(I),JPQ(I),JSQ(I),JBT(I),JXX(I)
      IF(K.EQ.-1)WRITE(JS,2405)
 2402 CONTINUE
 2403 FORMAT(1X,'REFINING',I3,' SETS   OBS CAL BKG PLY WRK USM  PQ  SQ',
     * ' BET DBG')
 2404 FORMAT(6X,3A4,10I4)
 2405 FORMAT(6X,'skip set in model OBS = -1')
      GOTO 10
   11 WRITE(JS,111)
  111 FORMAT(1X,'TALK ROUTINE')
      GOTO 10
      END
      SUBROUTINE LSQOUT(IERR,FILENAME,MODE)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      CHARACTER*255 FILENAME
      LOGICAL LOP
      IF(MODE.EQ.0)THEN
      CLOSE(JL)
      INQUIRE(ERR=2095,FILE=FILENAME,EXIST=LOP)
      IF(.NOT.LOP)THEN
      WRITE(JS,2097)FILENAME
 2097 FORMAT(/1X,'Creating file ',/,1X,A255)
      CALL OPENNEW(JL,IERR,FILENAME)
      IF(IERR.EQ.1)GOTO 2095
      ELSE
      CALL OPENANY(JL,IERR,FILENAME)
      END IF
      ELSE
      INQUIRE(ERR=2095,UNIT=JL,OPENED=LOP,NAME=FILENAME)
      IF(.NOT.LOP)GOTO 2095
      END IF
 2094 CALL PROUT(9,1,999,JL)
      CALL FLUSH(JL)
      WRITE(JS,2099)FILENAME
 2099 FORMAT(/1X,'Parameters written to: ',/,1X,A255)
      CALL FLUSH(JS)
      ENDFILE(JS)
      IERR=0
      RETURN
 2095 WRITE(JS,2098)FILENAME
 2098 FORMAT(/1X,'Error saving file ',/,1X,A255)
      CALL FLUSH(JS)
      IERR=1
      RETURN
      END
      SUBROUTINE COPYWK(J1,J2,L2,VALUE)
C==== COPIES Q ARRAY OF SET J1 INTO SET J2
C====  SET LABEL TO L2 AND INITIALISE TO VALUE
      INCLUDE 'FISHDIM.PAR'
      CHARACTER*4 L2
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      IF(J2.LE.0)RETURN
      CALL LTRAN(J2,L2)
      NCH(J2)=NCH(J1)
      NC1(J2)=NC1(J1)
      NC2(J2)=NC2(J1)
      NMC(J2)=NMC(J1)
      NC3(J2)=NC3(J1)
      NC4(J2)=NC4(J1)
      DO 208 J=1,NCH(J1)
  208 Q(J,J2)=Q(J,J1)
      DO 209 J=1,NCH(J1)
  209 C(J,J2)=VALUE
      DO 210 J=1,NCH(J1)
  210 E(J,J2)=0.0
      RETURN
      END
C
      SUBROUTINE TESTER
C=====  ENTER 3N IN LSMAIN INPUT   - to test DERIV and CALCUL
c====  30/7/3 RKH try improve
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
      COMMON/POLY/PR1,PR2,NR1,NR2,ABC(5),RB,PA,PB,PCC,PD,IPD,JPD,NPP,
     *       NPP1,NPP2,NPP3,NPM,NSIMP
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      COMMON/WORK/DWE(MI),D(MV),DWD2(MI,MI),
     * V2(MV),PM(10),PM2(10),DD(MV,3),WT(10),DBIG(MI,MN),HH(MI),DHERE(MV),
     > DTIE(MV),DB1(MV),DB2(MV),FILL(1367)
C==== NOTE model 4 exptl bkg will not work as JPT needs to match QQQ
      JPT=1
   10 KS=1
 1003 IF(NS.GT.1)THEN
      WRITE(JS,1006)KS
 1006 FORMAT(1X,I4,' DATA SETS, USE SET KS = ')
      READ(IS,*)KS
      IF(KS.LE.0.OR.KS.GT.NS)GOTO 1003
      END IF
      WRITE(JS,1001)
 1001 FORMAT(1X,'Test at point no  =   (-ve to return) (*) ')
      read(is,*)JPT
      IF(JPT.LE.0)RETURN
      QQQ=Q(JPT,JD(KS))
C
      WRITE(JS,1005)
 1005 FORMAT(1X,' frac shift, min shift = (*) ')
      READ(IS,*)XSHIFT,ADDSHIFT
      ADDSHIFT=AMAX1(ABS(ADDSHIFT),1.0E-30)
      XSHIFT=AMAX1(ABS(XSHIFT),1.0E-30)
C
C==== FORCE CALC of all possible derivatives in DERIV ( V2 used for Marquardt)
      DO 200 I=1,NP
      V2(I)=PS(I)
      IF(PS(I).GT.-1.5)PS(I)=1.0
  200 CONTINUE
C==== TURN OFF SMEARING, DERIV ROUTINE WILL SWITCH ON IF REQUIRED
      NPSMEAR=0
      KY=JY(KS)
      IF(KY.GT.0)CALL POLSET(Q(1,KY),C(1,KY),KY,KS)
      VNORM=ABC(3)
C==== CHECK FOR MODEL 23 INTERACTING PARTICLES which need a preliminary calc.
      CALL SSQFNSET(IERR)
      IF(IERR.LE.0)RETURN
      CALL CALCQ(KS,QQQ,CALC1,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                         JPT,JB(KS),JY(KS),NPSMEAR)
      CALL DERIV(QQQ,CALC1D,SUMWD,SUMPQD,PRODSQD,BETARATIOD,SUMBUGD,
     *                      KS,JPT,JB(KS),JY(KS),NPSMEAR,D)
C==== 30/07/03 at least model 10 poly core/shell is using part of D( ) array in 
C==== to calc derivs and handle the "square" operation. It also calls CONTIE,
c==== but not CONDER
      DO I=1,NP
      DHERE(I)=D(I)
      DTIE(I)=D(I)
      END DO
      CALL CONDER(1,NC,DTIE,DHERE)
c      WRITE(JS,1009)(D(I),I=1,NP)
c 1009 FORMAT(1X,'D() =',1P7E10.2)
      WRITE(JS,1010)QQQ,CALC1,CALC1D,SUMW,SUMWD,SUMPQ,SUMPQD,
     * PRODSQ,PRODSQD,BETARATIO,BETARATIOD,SUMBUG,SUMBUGD
C23456789012345678901234567890123456789012345678901234567890123456789012
1010  FORMAT(/,1X,'Q= ',1PE10.2,'   CALCQ           DERIV',/,
     *  '  CALC   ',2E14.6,/,'  BKG    ',2E14.6,/,'  P(Q)   ',2E14.6,
     */,'  S(Q)   ',2E14.6,/,'  BETA   ',2E14.6,
     *' ONLY CALC IF S(Q) USED',
     */,'  SUMBUG ',2E14.6,/)
C
1500  DO 2000 I=1,NP
      IF(PS(I).LT.-1.5)GOTO 2000
C==== TEST FOR all but ps(i) = -2.0 
      VTEMP=V(I)
      DDV=XSHIFT*ABS(V(I))
      IF(DDV.LT.ADDSHIFT)DDV=ADDSHIFT
C
      V(I)=VTEMP+DDV
      KY=JY(KS)
      IF(NC.GT.0)CALL CONTIE(1,NC)
C==== don't call POLSET here as volume will be renormalised and derivs
C==== will not be same as done by DERIV  30/7/3 TRY IT ANYWAY, seems need
c==== this to get any derivs on polydisp, BUT d/dRbar gets wrong sign.
      IF(KY.GT.0)CALL POLSET(Q(1,KY),C(1,KY),KY,KS)
C==== restore orig value of VNORM to get closer to what FISH does
c====      ABC(3)=VNORM
      IF(KY.GT.0.AND.NPP.GT.0)CALL PCALC(C(1,KY),NMETH(NPP))
C==== CHECK FOR MODEL 23 INTERACTING PARTICLES which need a preliminary calc.
      CALL SSQFNSET(IERR)
      CALL CALCQ(KS,QQQ,CALC2,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                         JPT,JB(KS),JY(KS),NPSMEAR)
C
      V(I)=VTEMP-DDV
      IF(NC.GT.0)CALL CONTIE(1,NC)
c==== need 2nd call to polset here, else calc3=calc2
      IF(KY.GT.0)CALL POLSET(Q(1,KY),C(1,KY),KY,KS)
c====      ABC(3)=VNORM
      IF(KY.GT.0.AND.NPP.GT.0)CALL PCALC(C(1,KY),NMETH(NPP))
C==== CHECK FOR MODEL 23 INTERACTING PARTICLES which need a preliminary calc.
      CALL SSQFNSET(IERR)
      CALL CALCQ(KS,QQQ,CALC3,SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *                         JPT,JB(KS),JY(KS),NPSMEAR)
C
C====   DER 1- UP, 2- DOWN, 3- MEAN, 4- SECOND DERIV store temporarily
C==== as some models produce lots of print out on screen !
      DB1(I)=(CALC2-CALC3)*0.5/DDV
      DB2(I)=(CALC2 -2.*CALC1 +CALC3)/DDV
      V(I)=VTEMP
      IF(NC.GT.0)CALL CONTIE(1,NC)
 2000 CONTINUE
C====  print out the results
      WRITE(JS,1019)QQQ
1019  FORMAT(1X,'Q=',1PE10.2,'  param     shift  DEL(i) ',
     > ' derivs:FISH,+TIE  NUMERICAL  DV*2nd deriv')
C
      do 2200 i=1,np
      DDV=XSHIFT*ABS(V(I))
      IF(DDV.LT.ADDSHIFT)DDV=ADDSHIFT
 2200 WRITE(JS,1021)I,LM(I),LTYP(I),LPAR(1,I),V(I),DDV,DEL(I),
     * DHERE(I),DTIE(I),DB1(I),DB2(I)
 1021 FORMAT(1X,3I3,A4,1PE10.2,2E9.1,3E10.2,E9.1)
c====
      WRITE(JS,2005)
 2005 FORMAT(1X,' frac shift, min shift (-ve to stop) = (*) ')
      READ(IS,*)XSHIFT,ADDSHIFT
      IF(XSHIFT.LT.0.0)GOTO 9000
      ADDSHIFT=AMAX1(ABS(ADDSHIFT),1.0E-30)
      XSHIFT=AMAX1(ABS(XSHIFT),1.0E-30)
      GOTO 1500
 9000 do 250 i=1,np
  250 PS(I)=V2(I)
      GOTO 10
      END
C
      SUBROUTINE DELSET(NFLAG)
C==== 25/3/93 RKH set up numerical derivative shifts where needed
c==== by reading through the model description
C==== NFLAG=1 allows user to change them from option 32 on fit menu
C==== no space to store these in the file
C==== 29/5/96 change to be called each cycle, so can use fractional
c==== as well as absolute shifts !
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
C==== this is rally two separate routines !!!
      IF(NFLAG.EQ.0)THEN
c==== start by assuming all are analytic, IDEL(i)=0, DEL(i)=0.0
      DO I=1,NP
      IDEL(I)=0
      DEL(I)=0.0
      END DO

c==== CHECK - at present POLSET does its own derivative set up
c==== in the model 6 part. 08/08/03 no it does not, and if the Cn in
c==== stick are small or SCALE is small, may give trouble ?
      DO 20000 I=1,NP
      A=ABS(V(I))
      SMALL = AMAX1(1.E-10, 0.01*A)
      HALF=AMAX1(0.005,0.005*A)
      ONE=AMAX1(0.01,0.01*A)
      FIVE=AMAX1(0.05,0.05*A)
      I1=I-1
      LT=MOD(LTYP(I),10)
      IM=LM(I)
C23456789012345678901234567890123456789012345678901234567890123456789012
      GOTO(10100,10200,20000,10400,20000,10600,20000,10800,20000,
     *     10800,20000,20000,20000,11400,20000,20000,20000,11800,
     *     20000,20000,12100,12200,12300,12400,12300,20000,12700,
     *     12800,12900),IM
      GOTO 20000
10100 IF(LT.EQ.2.AND.LTYP(I1).GE.11)DEL(I)=ONE
      IF(LT.EQ.3.AND.LTYP(I-2).GE.21)DEL(I)=ONE
      GOTO 20000
c==== model 2 params are used in numerical derivs in constraints 15 & 16 !
10200 DEL(I)=ONE
      GOTO 20000
C==== shift for IRIS resolution function
10400 IF(LT.EQ.2.AND.LTYP(I1).EQ.11)DEL(I)=.015
      GOTO 20000
c==== polydispersity
10600 DEL(I)=HALF
      IF(LT.EQ.1.AND.LTYP(I).NE.1.AND.LTYP(I).NE.61)DEL(I)=SMALL
      IF(LTYP(I).EQ.72)DEL(I)=SMALL
      GOTO 20000
C==== radii of exponential shells - MODELS 8 & 10
10800 IF(LT.EQ.2)DEL(I)=0.5
      GOTO 20000
c==== coils LTYP(first)= 1 Gaussian, 11 polydisp Gaussian, 21 Worm
C====    31 STAR (Benoit), 41 STAR (Dozier et al), 51 diblock, 61 H-shape,
c====    71 Kholo worm
C==== only the high Q part of the polydisp Gaussian is done numerically,
c==== as at low Q a simple expansion is used.
11400 GOTO(20000,11420,11430,11440,11450,11460,11470),LT
      GOTO 20000
C====  d/dRg polydisp gauss coil OR hshape; or d/dN for Worm & DIBLOCK
C====  
11420 IF(LTYP(I1).EQ.11)DEL(I)=0.1
      IF(LTYP(I1).EQ.21.or.LTYP(I1).GE.41.OR.LTYP(I1).EQ.71)
     >       DEL(I)=AMAX1(1.0, 0.02*V(I))
      GOTO 20000
c====  correl length a for DIBLOCK, fractions for HSHAPE, L for worms
11430 DEl(I)=0.005
      IF(LTYP(I-2).EQ.21.OR.LTYP(I-2).EQ.71)DEL(I)=AMAX1(2.0,0.02*V(I))
      IF(LTYP(I-2).EQ.51)DEL(I)=0.5
      GOTO 20000
C==== WORM sub-model Rax, STAR-D zeta, DIBLOCK f, HSHAPE f, Kholodenko worm Rax
11440 DEL(I)=0.5
      IF(LTYP(I-3).EQ.41)DEL(I)=2.0
      IF(LTYP(I-3).EQ.51)DEL(I)=0.02
      IF(LTYP(I-3).EQ.61)DEL(I)=0.005
      GOTO 20000
C==== WORM sub-model U, STAR-D nu, DIBLOCK chi, HSHAPE chi, worm unused or DELR
11450 DEL(I)= 0.02
      IF(LTYP(I-4).EQ.51)DEL(I)=0.001
      IF(LTYP(I-4).EQ.81)DEL(I)=AMAX1(0.01*V(I),0.2)
      GOTO 20000
C==== Kholo worm unused or contrast
11460 DEL(I)=0.0
      IF(LTYP(I-5).EQ.81)DEL(I)=AMAX1(0.01*V(I),0.05)
      GOTO 20000
C==== Kholo polydisp
11470 DEL(I)=0.0
      IF(LTYP(I-6).EQ.71.OR.LTYP(I-6).EQ.81)DEL(I)=0.02
      GOTO 20000
C==== RESORT TO NUMERICAL DERIVATIVES FOR RODS
11800 GOTO(20000,11802,11803,11802,11802,11802,11807,11807),LT
      GOTO 20000
11802 DEL(I)=HALF
      GOTO 20000
11803 DEL(I)=ONE
      IF(V(I).GT.1000.)DEL(I)=FIVE
      GOTO 20000
C==== 2 degrees for PSI or dPSI
11807 DEL(I)=2.0
      GOTO 20000
C==== ELLIPSOID use numerical derivatives
12100 GOTO(20000,12102,12103),LT
      GOTO 20000
12102 DEL(I)=0.05
      GOTO 20000
12103 DEL(I)=0.005
      GOTO 20000
C==== HARD SPHERES STRUCTURE FACTOR - NUMERICAL DERIVS
12200 IF(LT.EQ.1)DEL(I)=0.005
      IF(LT.EQ.2)DEL(I)=0.005
      IF(LT.EQ.3)DEL(I)=0.005
      IF(LT.EQ.4)DEL(I)=0.005
      GOTO 20000
C==== RMSA STRUCTURE FACTOR - NUMERICAL DERIVATIVES 
C==== MODELS 23 & 25, REPLACING ORIGINAL SSQSHIFT( ) ARRAY
12300 IF(LT.EQ.1)DEL(I)=0.01
      IF(LT.EQ.2)DEL(I)=0.05
      IF(LT.EQ.3)DEL(I)=0.005
      IF(LT.EQ.4)DEL(I)=0.002
      GOTO 20000
C==== TWO SHELL ELLIPSOID - NUMERICAL DERIVATIVE
12400 GOTO(20000,12402,12403,12404,12405),LT
      GOTO 20000
12402 DEL(I)=0.05
      GOTO 20000
12403 DEL(I)=0.005
      GOTO 20000
12404 DEL(I)=0.005
      GOTO 20000
12405 DEL(I)=0.05
      GOTO 20000
12700 IF(LT.EQ.2.AND.LTYP(I-1).EQ.61)DEL(I)=0.001
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.61)DEL(I)=1.0
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.61)DEL(I)=1.0
      IF(LT.EQ.5.AND.LTYP(I-4).EQ.61)DEL(I)=0.001
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.81)DEL(I)=1.0
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.81)DEL(I)=1.0
      IF(LT.EQ.4.AND.LTYP(I-3).EQ.81)DEL(I)=1.0
      GOTO 20000
C==== RESORT TO NUMERICAL DERIVATIVES FOR POLYDISP LAYER
12800 IF(LTYP(I-LT+1).LE.31)GOTO(20000,12802,12803,12804,12805),LT
      DEL(I)=0.1
      IF(LT.EQ.1)DEL(I)=0.0
      IF(LT.EQ.7)DEL(I)=2.0
      GOTO 20000
C
C==== interfacial T or layer L
12802 DEL(I)=0.1
      GOTO 20000
c==== layer L or contrast
12803 DEL(I)=0.1
      GOTO 20000
c==== sig(L)/L or shell H or 1/kappa
12804 DEL(I)=0.02
      IF(LTYP(I-3).EQ.11)DEL(I)=0.2
      GOTO 20000
c==== Lorentz R*Sigma
12805 DEL(I)= 2.0
      GOTO 20000
12900 IF(LT.EQ.1)DEL(I)=0.02
      IF(LT.EQ.2.AND.LTYP(I-1).EQ.1)DEL(I)=0.025
      IF(LT.EQ.3.AND.LTYP(I-2).EQ.1)DEL(I)=0.002
      IF(LT.EQ.2.AND.LTYP(I-1).GE.11)DEL(I)=1.000
      IF(LT.GE.3.AND.LTYP(I+1-LT).GE.11)DEL(I)=ONE
C==== 17/03/04 LTYP(1)=21, LTYPE=10 goes to LT=0 for Rsigma Lorentz!
      IF(LT.EQ.0)DEL(I)=ONE
20000 CONTINUE
c
      DO I=1,NP
      IF(DEL(I).GT.0)IDEL(I)=1
      END DO
C
      RETURN
c==== this part for NFLAG=1 ============================================
      ELSE
C====  print out the results
400   WRITE(JS,1019)
1019  FORMAT(1x,'Shift only shown where numerical derivs. MAY be used.',/,
     >1X,'Integration method METH, store IQ, N points, stored I to J.',/,
     > 13X,'  param      value   shift=DEL(i) ',
     > 'METH IQ N   I   J,     VOLP',/)
      DO I=1,NP
C==== note the NMETH are initialised to -11
      IF(NMETH(I).LE.0)THEN
        IF(ABS(DEL(I)).GT.1.E-24)THEN
        WRITE(JS,1024)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),DEL(I),VOLP(I)
        ELSE
        WRITE(JS,1022)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),VOLP(I)
        END IF
      ELSE
        if(iq(i).le.0)then
c==== 4/2/94 this is NOT supposed to happen, but the code has crashed here
        IF(ABS(DEL(I)).GT.1.E-24)THEN
        WRITE(JS,1031)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),DEL(I),
     > NMETH(I),IQ(I),VOLP(I)
        ELSE
        WRITE(JS,1033)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),
     > NMETH(I),IQ(I),VOLP(I)
        END IF
        else
c==== this is what should happen alaways IF nmeth(i) is set properly
        IF(ABS(DEL(I)).GT.1.E-24)THEN
        WRITE(JS,1021)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),DEL(I),
     > NMETH(I),IQ(I),NGPT(IQ(I)),IB(IQ(I))+1,IB(IQ(I))+NGPT(IQ(I)),
     > VOLP(I)
        ELSE
        WRITE(JS,1023)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),
     > NMETH(I),IQ(I),NGPT(IQ(I)),IB(IQ(I))+1,IB(IQ(I))+NGPT(IQ(I)),
     > VOLP(I)
        END IF
        end if
      END IF
      END DO
 1024 FORMAT(1X,3I3,3A4,1PE12.4,E11.3,20X,E13.5)
 1021 FORMAT(1X,3I3,3A4,1PE12.4,E11.3,5I4,E13.5)
 1022 FORMAT(1X,3I3,3A4,1PE12.4,  11X,20X,E13.5)
 1023 FORMAT(1X,3I3,3A4,1PE12.4,  11X,5I4,E13.5)
 1031 FORMAT(1X,3I3,3A4,1PE12.4,E11.3,2I4,' ERR?',8X,E12.4)
 1033 FORMAT(1X,3I3,3A4,1PE12.4,  11X,3I4,' ERR?',8X,E12.4)
c====
      WRITE(JS,501)
501   FORMAT(/,1X,'CHANGE DEL(I) [N.B. refine resets it!], I=  ',
     >   '( 0 TO CONTINUE) ')
      READ(IS,*)I
      IF(I.LE.0.OR.I.GT.MV)RETURN
      
      WRITE(JS,503)I
503   FORMAT(1X,'DEL(',I3,' ) = ')
      READ(IS,*)DEL(I)
      GOTO 400
      END IF
      END

C
c     Originally part of LSMAIN
c     "Activates" model 
      SUBROUTINE ACMOD(IDX)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,IX,NX1,NX2,IRX,
     >     LS2(12),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      CHARACTER*4 LCAL/' CAL'/,LPOL/' POL'/,LWRK/' WRK'/,LJAM/'UNSM'/,
     *     LPQQ/'P(Q)'/,LSQQ/'S(Q)'/,LBTQ/'BETA'/,LXXQ/'DBUG'/
      NOWT=1    
      JJJ=NDIM
      CALL LSQIN(IDX)
C==== initialise all integration schemes
      NIQ=0
      DO I=1,MV
      NMETH(I)=-11
      IQ(I)=0
      END DO
C
      DO I=1,NIQMAX
      IB(I)=0
      NGPT(I)=0
      END DO
C==== if a model has been read go and check the numerical derivatives:
      NFLAG=0
      IF(NP.GT.0)CALL DELSET(NFLAG)
      IX = 0
      RETURN
      END
c      SUBROUTINE LSMAIN
cC==== these data analysis programs are for use only by authorised persons
cC==== Copyright R.K.Heenan 1985.
cC==== written by R.K.Heenan at UKC and RAL 1983-1986
cC==== MOST OF THIS COULD BE IN THE TALK ROUTINE
cc==== 9/9/3 initialise IX here for type of polydisp calc (see POLSET) 
cc==== to fix bug in refine where radius reset to mean after each cycle of fit.
cc
c      INCLUDE 'FISHDIM.PAR'
c      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
c     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
c     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
c      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
c     >     NDAT,NYC,NPR,IX,NX1,NX2,IRX,
c     >     LS2(12),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
c     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
c      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
c     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
c     * VAR,XDWE,NPSMEAR
c      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
c     >            RG(2048),WTR(2048)
c      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
c      DATA LCAL/' CAL'/,LPOL/' POL'/,LWRK/' WRK'/,LJAM/'UNSM'/,
c     *     LPQQ/'P(Q)'/,LSQQ/'S(Q)'/,LBTQ/'BETA'/,LXXQ/'DBUG'/
c      NOWT=1    
c      JJJ=NDIM
c   10 WRITE(JS,1)
c    1 FORMAT(1X,'1-READ MODEL FILE etc.')
c  102 FORMAT(2I1) 
c      READ(IS,102,ERR=600,END=600)JJ,J2
c      GOTO(100,200,505,400,500,600),JJ
c      GOTO 10
cc     ignore this call      
c  100 CALL LSQIN(1)
cC==== initialise all integration schemes
c      NIQ=0
c      DO I=1,MV
c      NMETH(I)=-11
c      IQ(I)=0
c      END DO
cC
c      DO I=1,NIQMAX
c      IB(I)=0
c      NGPT(I)=0
c      END DO
cC==== if a model has been read go and check the numerical derivatives:
c      NFLAG=0
c      IF(NP.GT.0)CALL DELSET(NFLAG)
c      IX = 0
c      GOTO 10
c
c     Originally part of LSMAIN
c     Takes the number of datasets from NS in COMMON TWO
c     allocates 10 arrays, i.e.  1 2 3 4 5 6 7 8 910
c                               11121314151617181920 etc.
      SUBROUTINE DATSET
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,IX,NX1,NX2,IRX,
     >     LS2(12),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
c====  21/3/17 these are not in a common block
      CHARACTER*4 LCAL/' CAL'/,LPOL/' POL'/,LWRK/' WRK'/,LJAM/'UNSM'/,
     *     LPQQ/'P(Q)'/,LSQQ/'S(Q)'/,LBTQ/'BETA'/,LXXQ/'DBUG'/
c  200 NS=MF
c      WRITE(JS,201)NS
c  201 FORMAT(1X,'NO. OF SETS OF DATA TO FIT ( MAX ',I2,' (*) )= ')
c      READ(IS,*,ERR=200)NS
c      IF(NS.LE.0.OR.NS.GT.MF)GOTO 10
c      WRITE(JS,203)MW
C23456789012345678901234567890123456789012345678901234567890123456789012
c  203 FORMAT(27X,' ENTER DATA SET numbers for:',
c     */,27x,'OBS  observations i.e. data to be fitted ',
c     */,27X,'    (-1 to skip a set in multi-set models)',
c     */,27x,'CALC results of model calculation.',
c     */,27X,'BKG  only needed if you are to use experimental',
c     */,27X,'    subtraction, model 4, ( WORK is ALSO required !)',
c     */,27X,'POLY for polydispersity e.g. for model 6',
c     */,27X,'WORK final background, including flat bkg model 3,',
c     */,27X,'    and scaled exptl. background model 4, which are not',
c     */,27X,'    smeared so are kept separate.',
c     */,27x,'UNSMEARED optional to save unsmeared when smearing.',
c     */,27x,'P(Q) optional to save form factor separately.',
c     */,27x,'S(Q) optional to save structure factor.',
c     */,27x,'BETA(Q) optional beta ratio S(Q) correction',
c     */,27x,'DEBUG(Q) optional for anything else !',
c     */,27x,'Enter 00 (or 2 spaces) for sets not required.',
c     */,27X,'There are ',i3,' sets in this version of FISH.' /)
c      WRITE(JS,202)
c  202 FORMAT(1X,'Obs, Calc, Bkgd, Poly, Work, Umsmeared, P(Q), S(Q)',
c     * ', Beta(Q), Bug(Q)',/,
c     * '  ENTER SET NUMBERS FOR EACH in fortran (10I2) format',/,
c     * '                   O C B P W U P S',/,
c     * '               e.g. 1 2 3 4 5 6 7 8 910')
      NOREF=0
      IX = 0
 1008 FORMAT(10I2)
      DO 210 I=1,NS
      WRITE(JS,211)I
  211 FORMAT(1X,' DATA SET',I3,'      ')
c      READ(IS,1008,ERR=200)KD,KC,JB(I),JY(I),KW,JAM(I),JPQ(I),JSQ(I),
c     *                      JBT(I),JXX(I)
      KD=I*10-9
      KC=I*10-8
      JB(I)=I*10-7
      JY(I)=I*10-6
      KW=I*10-5
      JAM(I)=I*10-4
      JPQ(I)=I*10-3
      JSQ(I)=I*10-2
      JBT(I)=I*10-1
      JXX(I)=I*10-0
      WRITE(JS,1008)KD,KC,JB(I),JY(I),KW,JAM(I),JPQ(I),JSQ(I),
     *                      JBT(I),JXX(I)
c==== AT LEAST AS A TEMP MEASURE COPY OVER Q(KD) INTO Q(KC), USED ONLY FOR PLOTS
c      IF(KD.GT.MW.OR.KC.GT.MW.OR.JB(I).GT.MW.OR.JY(I).GT.MW.OR.KW.GT.MW.OR.
c     * JAM(I).GT.MW.OR.JPQ(I).GT.MW.OR.JSQ(I).GT.MW.OR.JBT(I).GT.MW.
c     * OR.JXX(I).GT.MW.OR.KD.LE.0.OR.KC.LE.0 )THEN
C==== 11/7/01 skip on data set = -1
c      IF(KD.NE.-1)THEN
c      WRITE(6,212)MW
c  212 FORMAT(1X,'OOPS a set missing or >',I3,', did you miss a space out ?',
c     * ' - start again')
c      GOTO 200
c      ENDIF
c      ENDIF
C      WRITE(JS,1001)NDIM,JJJ
C      WRITE(JS,1002)(J,Q(J,KD),Q(J,KC),J=1,NC4(KD))
C1001  FORMAT(1X,'DEBUG   NDIM=',I12,' JJJ=',I12,/,
C     *  '      Q(OBS)    Q(CALC)')
C1002  FORMAT(I4,2F10.5)
      JD(I)=KD
      IF(KD.EQ.-1)GOTO 210
      DO 205 J=1,JJJ
  205 Q(J,KC)=Q(J,KD)
      JC(I)=KC
      JW(I)=KW
      CALL COPYWK(KD,KC,LCAL,0.0)
c==== 03/09/03 remove this, (a) its wrong (b) why was it here?   IF(NMC(KD).GT.10)NC3(KC)=NMC(KD)+1
      CALL LTRAN(JY(I),LPOL)
      IF(JY(I).GT.0)NCH(JY(I))=NDIM
      CALL COPYWK(KD,KW,LWRK,0.0)
      CALL COPYWK(KD,JAM(I),LJAM,0.0)
      CALL COPYWK(KD,JPQ(I),LPQQ,0.0)
      CALL COPYWK(KD,JBT(I),LBTQ,0.0)
      CALL COPYWK(KD,JXX(I),LXXQ,0.0)
      CALL COPYWK(KD,JSQ(I),LSQQ,1.0)
  210 CONTINUE
      NOWT=1
      NYC=0
      NPR=0
      CALL FLUSH(JS)
      RETURN
      END
c  300 GOTO(305,310,320),J2+1
c  305 CALL TALK
c      WRITE(JS,301)
c  301 FORMAT(1X,'DID YOU REMEMBER TO PF AND FF ? ,NOW SAVE CALC SETS'
c     * //)
c      GOTO 10
c  310 CALL TESTER
c      GOTO 10
c  320 NFLAG=1
c      CALL DELSET(NFLAG)
c      GOTO 10
c  400 WRITE(JS,4031)
cC23456789012345678901234567890123456789012345678901234567890123456789012
c 4031 FORMAT(27X,' ENTER DATA SET numbers for:',
c     */,27X,'CALC data to be calculated.',
c     */,27X,'BKG  if you are about to use experimental',
c     */,27X,'    subtraction, model 4, ( WORK is ALSO required !).',
c     */,27X,'POLY is needed for polydispersity e.g. for model 6,',
c     */,27X,'WORK is for polynomial background, model 3,',
c     */,27X,'    and/or observed background model 4.',
c     */,27x,'UNSMEARED optional to save unsmeared when smearing.',
c     */,27x,'P(Q) optional to save form factor separately.',
c     */,27x,'S(Q) optional to save structure factor separately.',
c     */,27x,'BETA(Q) optional beta ratio for polydisperse.',
c     */,27x,'DEBUG(Q) optional for anything else !',
c     */,27x,'Enter zero 00(2 spaces) for sets not required.',/)
c      WRITE(JS,401)
c  401 FORMAT(1X,'Calc, Bkgd, Poly, Work, Umsmeared, P(Q), S(Q)',
c     * ', Beta(Q), Bug(Q)',/,
c     * '  ENTER SET NUMBERS FOR EACH in fortran (10I2) format',/,
c     * '                   C B P W U P S',/,
c     * '               e.g. 2 3 4 5 6 7 8 910',/
c     * '                   ')
c      READ(IS,1008,ERR=400)KD,JB(1),JY(1),JW(1),JAM(1),JPQ(1),JSQ(1),
c     *                      JBT(1),JXX(1)
c      IF(KD.GT.MW.OR.JB(1).GT.MW.OR.JY(1).GT.MW.OR.JW(1).GT.MW.OR.
c     * JAM(1).GT.MW.OR.JPQ(1).GT.MW.OR.JSQ(1).GT.MW.OR.JBT(1).GT.MW.
c     * OR.JXX(1).GT.MW.OR.KD.LE.0)THEN
c      WRITE(6,212)MW
c      GOTO 10
c      ENDIF
c      NOWT=1
cc      LAB(1,KD)=' CAL'
cc      LAB(2,KD)='C ON'
cc      LAB(3,KD)='LY  '
c      WRITE( LAB(1,KD), '(A4)' )   ' CAL'
c      WRITE( LAB(2,KD), '(A4)' )   'C ON'
c      WRITE( LAB(3,KD), '(A4)' )   'LY  '
c      JC(1)=KD
c      JD(1)=KD
cC====TRY THIS OUT, AN OBS-CALC WILL REFERENCE ITSELF
c 4021 WRITE(JS,402)
c  402 FORMAT(1X,'IS NEW Q NEEDED ? (ANS 1)')
c      READ(IS,102,ERR=4021)K
c      IF(K.NE.1)GOTO 412
c      WRITE(JS,403)
c  403 FORMAT(1X,'QMIN, DQ, NCH= (*)')
c      READ(IS,*,ERR=4021)A,B,K
c      IF(K.LE.0.OR.K.GT.JJJ)GOTO 400
c      X=A
c      DO 410 J=1,K
c      Q(J,KD)=X
c  410 X=X+B
c      NS=1
c      NCH(KD)=K
c      NC1(KD)=0
c      NC2(KD)=0
c      NMC(KD)=0
c      NC3(KD)=1
c      NC4(KD)=K
c  412 CALL COPYWK(KD,JW(1),LWRK,0.0)
c      CALL LTRAN(JY(1),LPOL)
c      IF(JY(1).GT.0)NCH(JY(1))=NDIM
c      CALL COPYWK(KD,JAM(1),LJAM,0.0)
c      CALL COPYWK(KD,JPQ(1),LPQQ,0.0)
c      CALL COPYWK(KD,JSQ(1),LSQQ,1.0)
c      CALL COPYWK(KD,JBT(1),LBTQ,0.0)
c      CALL COPYWK(KD,JXX(1),LXXQ,0.0)
c      NOREF=1
c      IX = 0
c      GOTO 10
c  500 IJ=JS
c      GOTO 508
c  505 IJ=JF
c  508 IF(NS.EQ.0)GOTO 520
c      WRITE(IJ,501)NS
c      DO 510 I=1,NS
c      K=JD(I)
c      IF(K.GT.0)WRITE(IJ,502)(LAB(L,K),L=1,3),JD(I),JC(I),
c     *  JB(I),JY(I),JW(I),JAM(I),JPQ(I),JSQ(I),JBT(I),JXX(I)
c      IF(K.EQ.-1)WRITE(IJ,503)
c  510 CONTINUE
c  501 FORMAT(/,1X,'REFINING',I3,' SETS   OBS CAL BKG PLY WRK USM PQ  SQ ',
c     * ' BET DBG')
c  502 FORMAT(6X,3A4,10I4)
c  503 FORMAT(6X,'skip set in model OBS = -1')
c  520 IF(JJ.EQ.3)GOTO 300
cC====      CALL LEXI(J2)
c      GOTO 10
c  600 RETURN
c      END
c     Originally part of LSMAIN
c     Prints out index of sets in use
c     For debugging purposes only
      SUBROUTINE PINDEX
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),
     >IDC(MW),NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),
     >RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,IX,NX1,NX2,IRX,
     >     LS2(12),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8),
     >            RG(2048),WTR(2048)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      CHARACTER*4  LCAL/' CAL'/,LPOL/' POL'/,LWRK/' WRK'/,LJAM/'UNSM'/,
     *     LPQQ/'P(Q)'/,LSQQ/'S(Q)'/,LBTQ/'BETA'/,LXXQ/'DBUG'/
  500 IJ=JS
c      GOTO 508
c  505 IJ=JF
c  508 IF(NS.EQ.0)GOTO 520
      WRITE(IJ,501)NS
      DO 510 I=1,NS
      K=JD(I)
      IF(K.GT.0)WRITE(IJ,502)(LAB(L,K),L=1,3),JD(I),JC(I),
     *  JB(I),JY(I),JW(I),JAM(I),JPQ(I),JSQ(I),JBT(I),JXX(I)
      IF(K.EQ.-1)WRITE(IJ,503)
  510 CONTINUE
  501 FORMAT(/,1X,'REFINING',I3,' SETS   OBS CAL BKG PLY WRK USM PQ  SQ ',
     * ' BET DBG')
  502 FORMAT(6X,3A4,10I4)
  503 FORMAT(6X,'skip set in model OBS = -1')
      CALL FLUSH(IJ)
  600 RETURN
      END
      SUBROUTINE LTRAN(J,LSTR)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      CHARACTER*4 LNUL,LSTR
      DATA LNUL/'    '/
      IF(J.LE.0)RETURN
      DO 10 I=1,3
   10 LAB(I,J)=LNUL
      LAB(1,J)=LSTR
      DO 20 K=1,3
      DO 20 I=1,20
   20 LAB2(I,K,J)=LNUL
      LAB2(7,1,J)=LSTR
      LAB2(7,2,J)=LSTR
      RETURN
      END
      SUBROUTINE LSQIN(IDX)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== THIS ROUTINE CONTAINS CHECKS ON NT (80,4)  AND NP (MV)
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,LS(7),
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      REWIND IL
      WRITE(JS,101)IL
  101 FORMAT(1X,'LSQIN READS MODEL DESCRIPTION FILE FROM FORTRAN STREAM',I2,
     * /,1X,'should skip to required model automatically')
c     loop through the unwanted models until IDX
      DO 99, IJI=0,IDX     
C==== CONTROL CARD 1
   10 READ(IL,111,END=900,ERR=800)NT,NP,NS,NC,NN
  111 FORMAT(16(2X,I3))
C==== CONTROL CARD 2
      READ(IL,114,END=900,ERR=800)IW,IK,LS
  114 FORMAT(16(3X,I2))
      IF(NT.LE.0)GOTO 200
      IF(NT.GT.4)WRITE(JS,119)NT
  119 FORMAT(1X,'NT = ',I4,' IS TOO LARGE, MAX =4, PROGRAM WILL CRASH')
C==== TITLE CARDS
      READ(IL,112,END=900,ERR=800)((LCOM(I,J),I=1,80),J=1,NT)
  112 FORMAT(80A1)
      CALL PROUT(1,1,NT,JS)
  200 IF(NP.LE.0)GOTO 300
C==== PARAMETERS
      IF(NP.GT.MV)WRITE(JS,202)NP
      DO 220 I=1,NP
      READ(IL,201,END=900,ERR=800)LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),
     * ESD(I),PS(I)
C==== CHECK FOR PREDICATE DATA IN MODEL 5, SAVES TIME LATER
      IF(LM(I).EQ.5)LS(1)=1
      NTIE(I)=0
  220 CONTINUE
  201 FORMAT(3X,2I3,3A4,E14.6,E13.3,F6.1)
  202 FORMAT(1X,'NP = ',I4,' IS TOO LARGE, MAX =MV, PROGRAM WILL CRASH')
C==== HORRID INPUT FORMAT ????  WRITE A FORMATTING ROUTINE ??
  300 IF(NS.LE.0)GOTO 400
C==== DATA SETS DESCRIPTION, SKIP ACTUAL CARDS BY DUMMY INPUT
      DO 320 I=1,NS
  320 READ(IL,201,END=900,ERR=800)
  400 IF(NC.LE.0)GOTO 500
C==== CONSTRAINT RELATIONS
      CALL CONINP(1,NC,IL,1)
  500 IF(NN.LE.0)GOTO 600
C==== NUMERICAL CONSTANTS, CON(1) IS MARQUARDT LAMBDA
      READ(IL,501,END=900,ERR=800)(CON(I),I=1,NN)
  501 FORMAT(8E10.3)
  600 J=MIN(4,NP)
      CALL PROUT(2,1,J,JS)
c     finish reading model
   99 CONTINUE 
      WRITE(JS,601)
  601 FORMAT(1X,'USE=')
c     'I' read here
C==== 2/11/00 set redundant flags to zero, and check K8, which may be
c==== set at some old value for METHROD, note funny numbering here
c==== due to commonblock usage above !
      LS(4)=0
      LS(5)=0
      IF(LS(6).GT.0)WRITE(JS,711)
      LS(6)=0
  711 FORMAT(/,1X,'N.B. IF YOU REALLY WANT TO SWITCH OFF BETA(Q) CORRECTION',/,
     * 1X,'TO S(Q),  USE K8=1 BEFORE STARTING FITS',/)
      WRITE(JS,799)
  799 FORMAT(/,1X,'model above activated ^^',/)
      CALL FLUSH(JS)   
      RETURN
c     end of select model routine (I=1)
  800 WRITE(JS,801)
  801 FORMAT(1X,'ERROR reading LSQ data file',/)
      RETURN
  900 WRITE(JS,901)    
  901 FORMAT(1X,'ERROR end of LSQ file reached',/)
      RETURN
      END
      SUBROUTINE PROUT(IP,N1,M1,IJ)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== PRINT OUT parameters numbers N1 to M1 of type IP on channel IJ 
c==== IP= 1 - titles, 2-parameters, 3-data sets, 4-constraints, 5-constants
c====     0 - no of types line, 9- everything
C==== (ASSUME NO PFCC NEEDED USE FOR DATA AND .LP FILES)
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/LS(44),LM(MV),LTYP(MV),LPAR(3,MV),
     *          V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      IF(IP.EQ.0.OR.IP.EQ.9)GOTO 50
      K=IP
      IF(LS(K).LE.0)GOTO 90
      N=MAX(1,N1)
      M=MIN(M1,LS(K))
      M=MAX(N,M)
   10 GOTO(100,200,300,400,500,600),K
   50 WRITE(IJ,51)(LS(I),I=1,5)
   51 FORMAT(' T',I3,' P',I3,' S',I3,' C',I3,' N',I3)
      WRITE(IJ,52)(LS(I),I=17,28)
   52 FORMAT(' W',I3,' K',I3,' IP',I2,' MS',I2,' IY',I2,'  ',
     *I3,'  ',I3,' XB',I2,8I5)
      IF(IP.NE.9) RETURN
      N=1
      K=0
      GOTO 90
  100 WRITE(IJ,112)((LCOM(I,J),I=1,80),J=N,M)
  112 FORMAT(80A1)
      GOTO 90
  200 DO 220 I=N,M
  220 WRITE(IJ,201)I,LM(I),LTYP(I),(LPAR(J,I),J=1,3),V(I),
     *   ESD(I),PS(I),DV(I)
  201 FORMAT(3I3,3A4,1PE14.6,E13.3,0PF6.1,1PE10.2)
      GOTO 90
  300 DO 320 I=N,M
      J=JD(I)
      IF(J.GT.0)WRITE(IJ,301)I,J,(LAB(L,J),L=1,3),JC(I),JB(I),JY(I),FIT(I)
  320 IF(J.EQ.-1)WRITE(IJ,302)I,J
  301 FORMAT(1X,2I3,3A4,'  CALC',I2,' BKG',I2,' POL',I2,
     *'  SSE=',1PE10.3)
  302 FORMAT(1X,2I3,' skip this set in model')
      GOTO 90
  400 CALL CONOP(N,M,IJ)
      GOTO 90
  500 WRITE(IJ,501)(CON(I),I=N,M)
  501 FORMAT(1P8E10.3)
C==== CON(1) IS FOR LAMBDA MARQUARDT
   90 IF(IP.NE.9)RETURN
   95 K=K+1
      IF(K.EQ.6)GOTO 600
      M=LS(K)
      IF(LS(K).LE.0)GOTO 95
      GOTO 10
  600 CALL FLUSH(IJ)
      RETURN
      END
      SUBROUTINE CONINP(N,M,IJ,K)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== don't change values of N,M,IJ or K as they are often only integer
c==== constants - so will then get an access violation !
C==== 12/4/95 increase array dimensions from 4 to 8
c==== 3/12/03 increase to 12
      INCLUDE 'FISHDIM.PAR'
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
C==== K=1 FILE INPUT, K=0 SCREEN       CONSTRAINT INPUT ==============
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      IF(K.EQ.0)GOTO 200
      DO 150 L=N,M
      READ(IJ,101,END=800,ERR=800)NCON(L),(IMOD(I,L),I=1,10)
      I=IMOD(1,L)
      NUSE(L)=0
      IF(NTIE(I).GT.0)NUSE(NTIE(I))=0
      NTIE(I)=L
      IF(I.LE.NP.AND.PS(I).LT.(-0.1))NUSE(L)=1
  101 FORMAT(20I4)
      READ(IJ,102,END=800,ERR=800)(PC(I,L),I=1,8)
      IF(NCON(L).EQ.11.AND.IMOD(5,L).LE.0)THEN
c==== 20/8/97 fix a retrospective change to type 11, add extra
c==== term to older input, to avoid subscript zero error
      IMOD(5,L)=MAX(1,IMOD(4,L))
      PC(5,L)=PC(4,L)
      PC(4,L)=0.0
      END IF
  150 CONTINUE
  102 FORMAT(8F10.0)
C===== EACH RELATION HAS A SET OF INTEGERS AND A SET OF CONSTANTS,
C==== THE FIRST IMOD( ) IS THE ONE TO BE ELIMINATED FROM THE LEAST SQUARES
      RETURN
  200 MM=MIN(M,MC)
      DO 500 L=N,MM
  204 WRITE(JS,201)L
  201 FORMAT(1X,'CONSTRAINT RELATION',I3,' IS TO BE OF TYPE =')
      READ(IJ,*,ERR=200)I
      IF(I.EQ.0)GOTO 500
      NCON(L)=I
      J=4
C==== specials - with more than 4 integers to read - PUT IN CONOP ALSO 
C====                                               AT LABEL 9500  !!
C====      IDIOT   did you read the comment above
      IF(I.EQ.13.OR.I.EQ.17.OR.I.EQ.24)J=7
      IF(I.EQ.15)J=3
      IF(I.EQ.16.OR.I.EQ.20.OR.I.EQ.21)J=6
      IF(I.EQ.19.OR.I.EQ.23)J=8
      IF(I.EQ.11.OR.I.EQ.18)J=5
      IF(I.EQ.25)J=10
c==== see also at line 9500 in CONOP
c
      WRITE(JS,202)J
  202 FORMAT(1X,'PARAMETER NOS. INVOLVED = (ENTER ',I3,' NOS. (*) )')
      READ(IJ,*,ERR=200)(IMOD(I,L),I=1,J)
      I=IMOD(1,L)
      IF(I.LE.0.OR.I.GT.NP)GOTO 204
      NUSE(L)=0
      IF(NTIE(I).GT.0)NUSE(NTIE(I))=0
      NTIE(I)=L
      IF(I.LE.NP.AND.PS(I).LT.(-0.1))NUSE(L)=1
C==== specials - with more than 4 reals to read, FIXED 11/2/02 RKH
      J=4
      IF(NCON(L).EQ.11)J=5
      IF(NCON(L).EQ.19)J=7
      WRITE(JS,203)J
  203 FORMAT(1X,'CONSTANTS = (ENTER',I3,' NOS. (*) )')
      READ(IJ,*,ERR=200)(PC(I,L),I=1,J)
  500 CONTINUE
      NC=MAX(NC,MM)
      RETURN
  800 WRITE(JS,801)
  801 FORMAT(1X,'ERROR reading constraints',/)
      RETURN
      END
      SUBROUTINE CONOP(N,M,IJ)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      IF(M.LT.N)RETURN
      M=MIN(M,NC)
      IF(IJ.EQ.JL) GOTO 9500
      DO 1000 L=N,M
      IM=NCON(L)
      WRITE(IJ,11)L,IM,NUSE(L)
   11 FORMAT(1X,'CONSTRAINT',I3,' TYPE',I3,' USE=',I2)
      GOTO(100,200,300,400,500,600,700,800,900,1010,1100,1200,
     >   1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,
     >   2400,2500),IM
      GOTO 1999
  100 WRITE(IJ,101)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),PC(3,L)
  101 FORMAT(1X,'V(',I3,' )=',F10.5,' * V(',I3,' ) +',F10.5,
     * ' * V(',I3,' ) +',F10.5/)
      GOTO 1000
  200 WRITE(IJ,201)IMOD(1,L),IMOD(2,L),IMOD(3,L)
  201 FORMAT(1X,'V(',I3,' ) = V(',I3,' ) * V(',I3,' )'/ )
      GOTO 1000
  300 WRITE(IJ,301)IMOD(1,L),IMOD(2,L),IMOD(3,L)
  301 FORMAT(1X,'V(',I3,' ) = 2.0* V(',I3,' ) * V(',I3,' )',
     *'   R2-R1=2*L *RM'/ )
      GOTO 1000
  400 WRITE(IJ,401)IMOD(1,L),IMOD(2,L),IMOD(3,L)
  401 FORMAT(1X,'V(',I3,' ) = (1.0- V(',I3,' ) )* V(',I3,')',
     *'   R1=(1-L)*RM'/ )
      GOTO 1000
  500 WRITE(IJ,501)IMOD(1,L),PC(1,L),IMOD(2,L),IMOD(3,L),PC(2,L),PC(3,L),
     *  IMOD(4,L),PC(4,L)
  501 FORMAT(1X,'V(',I3,')=',F10.5,'*V(',I3,')*V(',I3,')**',F10.5,'+',F10.5,
     *'*V(',I3,')+',F10.5, /) 
      goto 1000
  600 WRITE(IJ,601)IMOD(1,L),PC(1,L),PC(2,L),IMOD(2,l),
     *  IMOD(3,L),PC(3,L),PC(4,L)
  601 FORMAT(1X,'V(',I3,')=(',F10.5,'+',F10.5,'*V(',I3,')*V(',
     *   I3,')**',F10.5,')**',F10.5, /) 
      goto 1000
  700 WRITE(IJ,701)IMOD(1,L),PC(1,L),PC(2,L),IMOD(2,L),IMOD(3,L),
     *   PC(3,L),IMOD(4,L),PC(4,L)
  701 FORMAT(1X,'V(',I3,')=((',F10.5,'+',F10.5,'*V(',I3,'))*V(',I3,
     *   ')+',F10.5,'*V(',I3,'))**',F10.5,/)
      GOTO 1000
  800 WRITE(IJ,801)IMOD(1,L),IMOD(2,L),PC(1,L),imod(3,l),PC(2,L),
     *  IMOD(4,L),PC(3,L),PC(4,L)
  801 FORMAT(1X,'V(',I3,')=V(',I3,')*(',F10.5,'*V(',I3,')+',F10.5,')+V(',
     *   I3,')*',F10.5,'+',F10.5, /) 
      goto 1000
  900 WRITE(IJ,901)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),PC(3,L),PC(4,L)
  901 FORMAT(1X,'V(',I3,')=',F10.5,'*(V(',I3,')*',F10.5,'+',F10.5,')**',
     *   F10.5,/) 
      GOTO 1000
 1010 WRITE(IJ,1011)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),
     *  PC(3,L),IMOD(4,L),PC(4,L)
 1011 FORMAT(1X,'V(',I3,')=(',F10.5,'*(V(',I3,')+',F10.5,'*V(',I3,'))**',  
     *   F10.5,'*V(',I3,'))**',F10.5,/)
      GOTO 1000
c==== 13/7/01 this was writing 5 not 4 integers ! - 4/12/3 it should be
c==== need change manual
 1100 WRITE(IJ,1101)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),PC(3,L),
     * IMOD(4,L),PC(4,L),IMOD(5,L),PC(5,L)
 1101 FORMAT(1X,'V(',I3,')=',F10.5,'*V(',I3,' )+',F10.5,
     * '*V(',I3,')+',F10.5,'*V(',I3,')+',F10.5,'*V(',I3,')+',F10.5,/)
      GOTO 1000
 1200 WRITE(IJ,1201)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L)
 1201 FORMAT(1X,'V(',I3,' )= (1 - V(',I3,' ))* ( V(',I3,' ) -',
     * ' V(',I3,') )'/)
      GOTO 1000
 1300 WRITE(IJ,1301)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(2,L),IMOD(6,L),IMOD(3,L),IMOD(7,L)
 1301 FORMAT(1X,'V(',I3,' )= V(',I3,' )- V(',I3,' ) +',
     * ' V(',I3,' )*(V(',I3,' )- V(',I3,' )) +',
     * ' V(',I3,' )*(V(',I3,' )- V(',I3,' ))',/)
      GOTO 1000
 1400 WRITE(IJ,1401)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L)
 1401 FORMAT(1X,'Shear rod BIGGAMMA V(',I3,' )=Fun[ R,L=V(',I3,
     > ',+1),G=V(',I3,'),eta/T=V(',I3,') ]',/)
      GOTO 1000
 1500 WRITE(IJ,1501)IMOD(1,L),IMOD(2,L),IMOD(3,l)
 1501 FORMAT(1X,'Shell V(',I3,')=Fun[ Vs/Vc=V(',I3,
     > '),f=V(',I3,') ]',/)
      GOTO 1000
 1600 WRITE(IJ,1601)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L)
 1601 FORMAT(1X,'contrast V(',I3,')=Fun[ Vs/Vc=V(',I3,
     > '),f=V(',I3,'),core=V(',I3,'),shell=V(',I3,'),',
     > 'solv=V(',I3,') ]',/)
      GOTO 1000
 1700 WRITE(IJ,1701)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L),IMOD(7,L)
 1701 FORMAT(1X,'Scale V(',I3,')=Fun[ scale=V(',I3,
     > '),f=V(',I3,'),shA=V(',I3,'),solA=V(',I3,'),',
     > 'shB=V(',I3,'),solB=V(',I3,') ]',/)
      GOTO 1000
 1800 WRITE(IJ,1801)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L)
 1801 FORMAT(1X,'contrast V(',I3,')=Fun[f=V(',I3,'),core=V(',
     >  I3,'),shell=V(',I3,'),solv=V(',I3,')]',/)
      GOTO 1000
 1900 WRITE(IJ,1901)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),PC(3,L),
     * IMOD(4,L),PC(4,L),IMOD(5,L),PC(5,L),
     * IMOD(6,L),PC(6,L),IMOD(7,L),PC(7,L),IMOD(8,L)
 1901 FORMAT(1X,'V(',I3,')=',F10.5,'*V(',I3,' )+',F10.5,
     * '*V(',I3,')+',F10.5,'*V(',I3,')+',/,F10.5,'*V(',I3,')+',F10.5,
     * '*V(',I3,')+',F10.5,'*V(',I3,')+',F10.5,'*V(',I3,')',/)
      GOTO 1000
 2000 WRITE(IJ,2001)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L)
 2001 FORMAT(1X,'contrast V(',I3,')=Fun[R1=V(',I3,'),Ri=V(',
     >  I3,'),Rj=V(',I3,'),RHO1-RHOn=V(',I3,'),h=V(',i3,')]',/)
      GOTO 1000
 2100 WRITE(IJ,2101)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L)
 2101 FORMAT(1X,'contrast V(',I3,')=Fun[ n/a V(',I3,
     > '),f=V(',I3,'),core=V(',I3,'),shell=V(',I3,'),',
     > 'solv=V(',I3,') ]',/)
      GOTO 1000
c==== 4/12/00   V(I1)=P1*V(I2)/ ( P2 + P3*V(I3) )
 2200 WRITE(IJ,2201)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),PC(3,L),IMOD(3,L)
 2201 FORMAT(1X,'V(',I3,' )=',F10.5,' * V(',I3,' ) / [',F10.5,
     * ' +',F10.5,'*V(',I3,' ) ]',/)
c
      GOTO 1000
 1999 WRITE(IJ,1998)IM
 1998 FORMAT(1X,'ERROR - constraint type not valid ?  ',i12)
      GOTO 1000
c
 2300 WRITE(IJ,2301)(IMOD(K,L),K=1,8),(PC(K,L),K=1,4)
 2301 FORMAT(1X,'part sld V(',I3,')=Fun[Po=V(',I3,
     > '),Pd=V(',I3,'),fc=V(',I3,'),fs=V(',I3,'),',
     > 'sldo=V(',I3,'),dr=V(',i3,'),tl=V(',i3,'),',
     > /,1x,'phi=',4f10.5,']',/)
      GOTO 1000
c
 2400 WRITE(IJ,2401)(IMOD(K,L),K=1,7),(PC(K,L),K=1,4)
 2401 FORMAT(1X,'R1/3 V(',I3,') & R2/3 V(',I3,
     > ')=fun[ Vs/c=V(',I3,'),X1=V(',I3,'),X2=V(',I3,'),',
     > 'Y1=V(',I3,'),N(R1)=',i3,'],',
     > /,4f10.5,/)
      GOTO 1000
c
 2500 WRITE(IJ,2501)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L),IMOD(7,L),IMOD(8,L),IMOD(9,L),IMOD(10,L)
 2501 FORMAT(1X,'V(',I3,')= {V(',I3,')-V(',I3,')}*V(',I3,')+',
     *'{V(',I3,')-V(',I3,')}*V(',I3,')+',
     *'{V(',I3,')-V(',I3,')}*V(',I3,')',/)
 1000 CONTINUE
      RETURN
 9500 DO 9900 L=N,M
C==== should put these special cases in a look-up array !
C==== number of integers
      K=4
      IF(NCON(L).EQ.13.OR.NCON(L).EQ.17.OR.NCON(L).EQ.24)K=7
      IF(NCON(L).EQ.15)K=3
      IF(NCON(L).EQ.16.OR.NCON(L).EQ.20.OR.NCON(L).EQ.21)K=6
      IF(NCON(L).EQ.19.OR.NCON(L).EQ.23)K=8
      IF(NCON(L).EQ.11.OR.NCON(L).EQ.18)K=5
      IF(NCON(L).EQ.25)K=10
C==== number of reals
      KK=4
      IF(NCON(L).EQ.11)KK=5
      IF(NCON(L).EQ.19)KK=7
      WRITE(IJ,9901)NCON(L),(IMOD(I,L),I=1,K)
 9900 WRITE(IJ,9902)(PC(I,L),I=1,KK)
 9901 FORMAT(20I4)
 9902 FORMAT(8F10.5)
      RETURN
      END
      SUBROUTINE CONTIE(N,M)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.  
C==== If you alter this routine, change CONOP and CONDER also !
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/LS(44),LM(MV),LTYP(MV),LPAR(3,MV),
     *          V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
C==== NOTE PC() AND IMOD() IN /TIE/ CHANGE NAME HERE
      COMMON/TIE/P(12,MC),I(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      DIMENSION AA(4),ROOT(3)
C==== NCON=1   V(I1)=P1*V(I2) + P2*V(I3) + P3
C====      2   V(I1)= V(I2)*V(I3)
C====      3   V(I1)=2.0*V(I2)*V(I3)     R2-R1=2*LAMBDA*RMID
C====      4   V(I1)= (1.0- V(I2) )*V(I3)   R1=(1-LAMBDA)*RMID
C====      5   V(I1)=P1*V(I2)*V(I3)**P2+P3*V(I3)+P4
C====	   6   V(I1)=(P1+P2*V(I2)*V(I3)**P3)**P4
C====      7   V(I1)=((P1+P2*V(I2))*V(I3)+P3*V(I4))**P4
C====	   8   V(I1)=V(I2)*(P1*V(I3)+P2)+P3*V(I4)+P4
C====      9   V(I1)=P1*(P2*V(I2)+P3)**P4
C====     10   V(I1)=( P1*(V(I2)+P2*V(I3))**P3 *V(I4) )**P4
C====     11   V(I1)=P1*V(I2) + P2*V(I3) + P3*V(I4) +P4*V(I5) MOD RKH 19/8/97
C====     12   V(I1)=(1.-V(I2))*(V(I3)- V(I4))                RKH 4/95
C====     13   V(I1)= V(I2)-V(I3) +V(I4)*(V(I5)-V(I2))
C====                             +V(I6)*(V(I3)-V(I7)) RKH 4/95
c====     14   tricky function for length of sheared rods
c====     15   solve cubic for shell of ellipse with solvent penetration
c====     16   solve cubic for contrast ( & shell) of core/shell ellipse 
c====     17   fix scale parameter of 2nd data set for ellipse
c====     18   fix (1-2)/(2-3) contrast for solvent in shell
C====     19   V(I1)=P1*V(I2) + P2*V(I3) + P3*V(I4) +P4*V(I5) +P5*V(I6) +
C====                 P6*V(I7) + P7*V(I8)
c====     20   sld steps for linear steps approx to Hedgehog, 22/7/98
C====          best to put this AFTER the constraints on list of Radii !
C====     21   simple ellipse contrast, using given SHELL
c====     22   V(I1)=P1*V(I2)/ ( P2 + P3*V(I3) )
c====     23   part of messy treble layer constraint for oil and drug
c====          partitioning between core & shell of microemulsion
c====     24   constrain by iteration R1/R3 & R2/R3 for core/shell/shell, 
C====          given R3; X1,X2 solv at R1 & R2;Y1 fraction of "polymer" 
c====          in shell 1; so far only for 10 1 and 10 11 type shells
c====     25   V(I1)={V(I2)-V(I3)}V(I4) + {V(I5)-V(I6)}V(I7) +
C====          {V(I8)-V(I9)}V(I10)
c
      DO 1000 L=N,M
      IF(NUSE(L).EQ.0)GOTO 1000
      IM=NCON(L)
      GOTO(100,200,300,400,500,600,700,800,900,1010,1100,1200,
     > 1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,
     > 2400,2500),IM
      GOTO 1999
  100 V(I(1,L))=P(1,L)*V(I(2,L))+P(2,L)*V(I(3,L))+P(3,L)
      GOTO 1000
  200 V(I(1,L))=V(I(2,L))*V(I(3,L))
      GOTO 1000
  300 V(I(1,L))=2.0*V(I(2,L))*V(I(3,L))
      GOTO 1000
  400 V(I(1,L))=(1.0-V(I(2,L)))*V(I(3,L))
      GOTO 1000
  500 V(I(1,L))=P(1,L)*V(I(2,L))*V(I(3,L))**P(2,L)+P(3,L)*V(I(4,L))+P(4,L)
      GOTO 1000
  600 V(I(1,L))=(P(1,L)+P(2,L)*V(I(2,L))*V(I(3,L))**P(3,L))**P(4,L)
      GOTO 1000
  700 V(I(1,L))=((P(1,L)+P(2,L)*V(I(2,L)))*V(I(3,L))+P(3,L)*V(I(4,L)))**P(4,L)
      GOTO 1000
  800 V(I(1,L))=V(I(2,L))*(V(I(3,L))*P(1,L)+P(2,L))+P(3,L)*V(I(4,L))+P(4,L)
      GOTO 1000
  900 V(I(1,L))=P(1,L)*(P(2,L)*V(I(2,L))+P(3,L))**P(4,L)
      GOTO 1000
 1010 V(I(1,L))=(P(1,L)*(V(I(2,L))+P(2,L)*V(I(3,L)))**P(3,L)*V(I(4,L)))**P(4,L)
      GOTO 1000
 1100 V(I(1,L))=P(1,L)*V(I(2,L))+P(2,L)*V(I(3,L))+P(3,L)*V(I(4,L))+
     >          P(4,L)*V(I(5,L)) +P(5,L)
      GOTO 1000
 1200 V(I(1,L))=( 1.0-V(I(2,L)) )*( V(I(3,L))-V(I(4,L)) )
      GOTO 1000
 1300 V(I(1,L))= V(I(2,L)) - V(I(3,L)) +V(I(4,L))*(V(I(5,L))-V(I(2,L)))
     > +V(I(6,L))*(V(I(3,L))-V(I(7,L)))
      GOTO 1000
 1400 RA=AMAX1( ABS(V(I(2,L))),1.0)
      RL=AMAX1(ABS(V(I(2,L)+1)),1.0)
c==== I assume shear rate G (sec**-1) and solv viscosity/temperature 
c==== are on model 2 cards, I(4,L) & I(5,L)  e.g. eta/T = .00920/308.=3.0e-05
c==== for water (?) at 35C
C==== equations are only OK for S >2 
      IF(RA.GT.1.0.AND.RL.GT.RA)SS=ALOG(RL/RA)
      SS=AMAX1(SS,2.0)
C==== 4.1412E-16 IS 3*BOLZTMANN kB, with units conversion for viscosity/temp
      V(I(1,L))= V(I(3,L))*3.141592654E-24*V(I(4,L))*RL**3/
     > ( 4.142E-16*(SS -1.57 +7.0*(0.28-1.0/SS)**2 ))
      GOTO 1000
c
C==== shell for core/shell ellipse
c==== for Vshell(dry)/Vcore, f solv
C====            2              3 
c==== solve cubic for constant shell thickness
 1500 AA(1)=( -V(I(2,L))/(1.0-V(I(3,L))) )*V(I(1,L)-1)*V(I(1,L)-2)**3
      AA(2)= V(I(1,L)-2)**2*(1.0+ 2.0*V(I(1,L)-1) )
      AA(3)= V(I(1,L)-2)*(2.0+ V(I(1,L)-1) )
      AA(4)=1.0
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1590
      END DO
 1590 V(I(1,L))=ROOT(IR)
C
      GOTO 1000
C==== shell for core/shell ellipse, needs FIVE model 2 cards
c==== for Vshell(dry)/Vcore, f solv in shell, sld core, sld shell, sld solv
C====            2              3                  4       5         6
c==== solve cubic for constant shell thickness
C==== same as before but V(I(1,L)) is now the contrast term,
C==== so the offsets for X and R1 change !
 1600 AA(1)=( -V(I(2,L))/(1.0-V(I(3,L))) )*V(I(1,L)-2)*V(I(1,L)-3)**3
      AA(2)= V(I(1,L)-3)**2*(1.0+ 2.0*V(I(1,L)-2) )
      AA(3)= V(I(1,L)-3)*(2.0+ V(I(1,L)-2) )
      AA(4)=1.0
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1690
      END DO
 1690 RH2=(1.0- V(I(3,L)) )*V( I(5,L)) + V(I(3,L))*V(I(6,L))
      V(I(1,L))=( V(I(4,L))-RH2)*V(I(1,L)-2)*V(I(1,L)-3)**3 /
     >        ( (RH2-V(I(6,L)))*(V(I(1,L)-3)+ROOT(IR))**2*
     >         (V(I(1,L)-2)*V(I(1,L)-3)+ROOT(IR)) )
c==== save Vcore, Vtotal & RHO2 in CON(9), CON(10) & CON(11)
c====  L-2 is X, L-3 is Rcore
      LS(5)=MAX(11,LS(5))
      CON(9)=4.188790295*V(I(1,L)-2)*V(I(1,L)-3)**3
c==== 8/12/98 put the **2 on first not second term, RKH
      CON(10)=4.188790295*(( V(I(1,L)-3)+ROOT(IR))**2)*
     >       ( V(I(1,L)-2)*V(I(1,L)-3) +ROOT(IR))
      CON(11)=RH2
      GOTO 1000
 1700 R2A=(1.0-V(I(3,L)))*V(I(4,L)) + V(I(3,L))*V(I(5,L))
      R2B=(1.0-V(I(3,L)))*V(I(6,L)) + V(I(3,L))*V(I(7,L))
      V(I(1,L))=V(I(2,L))*( (R2B-V(I(7,L)))/(R2A-V(I(5,L))) )**2
      GOTO 1000
 1800 R2A=(1.0-V(I(2,L)))*V(I(4,L)) +V(I(2,L))*V(I(5,L))
      IF(ABS(R2A-V(I(5,L))).LE.1.E-4)R2A=V(I(5,L))-0.0001
      V(I(1,L))=(V(I(3,L))-R2A)/(R2A-V(I(5,L)))
      GOTO 1000
 1900 V(I(1,L))=P(1,L)*V(I(2,L))+P(2,L)*V(I(3,L))+P(3,L)*V(I(4,L))+
     >          P(4,L)*V(I(5,L)) + P(5,L)*V(I(6,L)) +
     >          P(6,L)*V(I(7,L)) + P(7,L)*V(I(8,L))
      GOTO 1000
C==== V(J)= (RHO1-RHOn)( Ri**h - Rj**h)R1**(-h)
 2000 V(I(1,L))=V(I(5,L))*(EXPLR(V(I(3,L)),V(I(6,L)))-
     > EXPLR(V(I(4,L)),V(I(6,L))) )*EXPLR(V(I(2,L)),-V(I(6,L))) 
      GOTO 1000
C==== contrast for core/shell ellipse, needs FIVE model 2 cards
c==== used same cards as constr 16  BUT second integer ignored !
C==== Allows direct control of SHELL thickness.
c====  Vshell(dry)/Vcore, f solv in shell, sld core, sld shell, sld solv
C====    2 NOT USED              3              4       5         6
C==== V(J) = ( rho1 -rho2)*Vcore / (rho2 -rho3)*Vtot 
c==== again we put Vcore, Vtot and rho2 in CON(9 to 11 )
 2100 CON(9)=4.188790295*V(I(1,L)-2)*V(I(1,L)-3)**3
      CON(10)=4.188790295*(( V(I(1,L)-3)+V(I(1,L)-1))**2)*
     >       ( V(I(1,L)-2)*V(I(1,L)-3) +V(I(1,L)-1))
      CON(11)=(1.0-V(I(3,L)))*V(I(5,L)) + V(I(3,L))*V(I(6,L))
      V(I(1,L))= ( (V(I(4,L)) - CON(11))*CON(9) )/
     >            ( (CON(11)-V(I(6,L)) )*CON(10))
      GOTO 1000
c==== 4/12/00   V(I1)=P1*V(I2)/ ( P2 + P3*V(I3) )
 2200 V(I(1,L)) = P(1,L)*V(I(2,L))/ (P(2,L) + P(3,L)*V(I(3,L)) )
      GOTO 1000
c==== 13/6/01 part of contrast calc for oil& drug partitioning
c====  V1 del rho prime
c====  V2 P0 ~small, fraction of oil in shell (NOT local conc X !!)
c====  V3 Pd ~1.0, fraction of drug in shell 
c====  V4 phi core - calc using other constraints
c====  V5 phi shell   "
c====  V6 sld oil
c====  V7 sld drug
c====  V8 sld shell (ie surfactant tail)
c====  P1 P3 phi oil      [ P1 & P2 are ZERO for rho3-rho2]
c====  P2 P4 phi drug
 2300 V(I(1,L)) = P(1,L)*( 1.0-V(I(2,L)) )*V(I(6,L))/ V(I(4,L)) +
     >   P(2,L)*( 1.0-V(I(3,L)) )*V(I(7,L))/ V(I(4,L)) +
     >   P(3,L)*V(I(2,L))*( V(I(8,L)) - V(I(6,L)) )/ V(I(5,L)) +
     >   P(4,L)*V(I(3,L))*( V(I(8,L)) - V(I(7,L)) )/ V(I(5,L))
      GOTO 1000
c==== 8/2/2 RKH
c==== R1/R3 ratio    to be found given X1,X2,Y1,R3
c==== R2/R3 ratio    "  "    "     "    "  "  "  "
c==== (dry shell)/core vol ratio
c==== X1 solvent fraction at R1
c==== X2 solvent fraction at R2
c==== Y1 fraction of polymer in first shell
c==== R1 (N.B R2 & R3 assumed at +2 and +4 from R1 )
c==== N.B this constraint ties TWO parameters at once by an iterative method !
 2400 CALL SEEK1(V(I(1,L)),V(I(2,L)),V(I(3,L)),V(I(4,L)),V(I(5,L)),
     >  V(I(6,L)),I(7,L))
      GOTO 1000
 2500 V(I(1,L))= (V(I(2,L)) - V(I(3,L)))*V(I(4,L))+
     >           (V(I(5,L)) - V(I(6,L)))*V(I(7,L))+
     >           (V(I(8,L)) - V(I(9,L)))*V(I(10,L))
      GOTO 1000
 1999 WRITE(IS,1998)IM
 1998 FORMAT(1X,'ERROR - constraint type not valid ?  ',i12)
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE CONDER(N,M,D,DL)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      DIMENSION D(MV),DL(MV)
      COMMON/TWO/LS(44),LM(MV),LTYP(MV),LPAR(3,MV),
     *          V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
C==== NOTE PC() AND IMOD() IN /TIE/ CHANGE NAME HERE
      COMMON/TIE/P(12,MC),I(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      DIMENSION AA(4),ROOT(3)
c
c==== 22/7/98 reverse order of loop here to catch nested constraints
c
      DO 1000 L=M,N,-1
      IF(NUSE(L).EQ.0)GOTO 1000
      IM=NCON(L)
      I1=I(1,L)
      I2=I(2,L)
      I3=I(3,L)
      I4=I(4,L)
      GOTO(100,200,300,400,500,600,700,800,900,1010,1100,1200,
     > 1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,
     > 2400,2500),IM
      GOTO 1000
  100 D(I2)=D(I2)+P(1,L)*DL(I1)
      D(I3)=D(I3)+P(2,L)*DL(I1)
      GOTO 1000
  200 D(I2)=D(I2)+V(I3)*DL(I1)
      D(I3)=D(I3)+V(I2)*DL(I1)
      GOTO 1000
  300 D(I2)=D(I2)+2.0*V(I3)*DL(I1)
      D(I3)=D(I3)+2.0*V(I2)*DL(I1)
      GOTO 1000
  400 D(I2)=D(I2)-V(I3)*DL(I1)
      D(I3)=D(I3)+(1.0-V(I2))*DL(I1)
      GOTO 1000
  500 D(I2)=D(I2)+P(1,L)*V(I3)**P(2,L)*DL(I1)
      D(I3)=D(I3)+V(I2)*V(I3)**(P(2,L)-1.)*DL(I1)*P(1,L)*P(2,L)
      D(I4)=D(I4)+P(3,L)*DL(I1)
      GOTO 1000
  600 D(I2)=D(I2)+P(4,L)*(P(1,L)+P(2,L)*V(I(2,L))*V(I(3,L))**P(3,L))**
     *  (P(4,L)-1.0)*P(2,L)*V(I3)**P(3,L)*DL(I1)
      D(I3)=D(I3)+P(4,L)*(P(1,L)+P(2,L)*V(I(2,L))*V(I(3,L))**P(3,L))**
     *  (P(4,L)-1.0)*P(2,L)*P(3,L)*V(I2)*V(I3)**(P(2,L)-1.0)*DL(I1)
      GOTO 1000
  700 D(I2)=D(I2)+P(4,L)*((P(1,L)+P(2,L)*V(I(2,L)))*V(I(3,L))+P(3,L)*V(I(4,L))
     *   )**(P(4,L)-1.0)*DL(I1)*P(2,L)*V(I3)
      D(I3)=D(I3)+P(4,L)*((P(1,L)+P(2,L)*V(I(2,L)))*V(I(3,L))+P(3,L)*V(I(4,L))
     *   )**(P(4,L)-1.0)*DL(I1)*(P(1,L)+P(2,L)*V(I2))
      D(I4)=D(I4)+P(4,L)*((P(1,L)+P(2,L)*V(I(2,L)))*V(I(3,L))+P(3,L)*V(I(4,L))
     *   )**(P(4,L)-1.0)*DL(I1)*P(3,L)
      GOTO 1000
  800 D(I2)=D(I2)+(V(I3)*P(1,L)+P(2,L))*DL(I1)
      D(I3)=D(I3)+V(I2)*P(1,L)*DL(I1)
      D(I4)=D(I4)+P(3,L)*DL(I1)
      GOTO 1000
  900 D(I2)=D(I2)+DL(I1)*P(1,L)*P(2,L)*P(4,L)*(P(2,L)*V(I2)
     *   +P(3,L))**(P(4,L)-1.0)
      GOTO 1000
 1010 D(I2)=D(I2)+DL(I1)*P(4,L)*(P(1,L)*(V(I2)+P(2,L)*V(I3))**P(3,L)*V(I4))**
     *  (P(4,L)-1.0)*P(1,L)*(V(I2)+P(2,L)*V(I3))**(P(3,L)-1.0)*V(I4)*P(3,L)
      D(I3)=D(I3)+DL(I1)*P(4,L)*(P(1,L)*(V(I2)+P(2,L)*V(I3))**P(3,L)*V(I4))**
     *(P(4,L)-1.0)*P(1,L)*(V(I2)+P(2,L)*V(I3))**(P(3,L)-1.0)*V(I4)*P(3,L)*P(2,L)
      D(I4)=D(I4)+DL(I1)*P(4,L)*(P(1,L)*(V(I2)+P(2,L)*V(I3))**P(3,L)*V(I4))**
     *  (P(4,L)-1.0)*P(1,L)*(V(I2)+P(2,L)*V(I3))**P(3,L)
      GOTO 1000
 1100 D(I2)=D(I2)+P(1,L)*DL(I1)
      D(I3)=D(I3)+P(2,L)*DL(I1)
      D(I4)=D(I4)+P(3,L)*DL(I1)
      D(I(5,L))=D(I(5,L))+P(4,L)*DL(I1)
      GOTO 1000
 1200 D(I2)=D(I2)+ (V(I4)-V(I3))*DL(I1)
      D(I3)=D(I3)+ (1.0  -V(I2))*DL(I1)
      D(I4)=D(I4)+ (V(I2) - 1.0)*DL(I1)
      GOTO 1000
 1300 D(I2)=D(I2)+ (1.0 - V(I4))*DL(I1)
      D(I3)=D(I3)+ ( V(I(6,L)) - 1.0)*DL(I1)
      D(I4)=D(I4)+ (V(I(5,L)) - V(I2) )*DL(I1)
      D(I(5,L))=D(I(5,L))+ V(I4)*DL(I1)
      D(I(6,L))=D(I(6,L))+ V(I3)*DL(I1)
      D(I(7,L))=D(I(7,L))- V(I(6,L))*DL(I1)
      GOTO 1000
 1400 RA=AMAX1( ABS(V(I(2,L))),1.0)
      RL=AMAX1(ABS(V(I(2,L)+1)),1.0)
      TE=1.0/AMAX1(1.0E-18,V(I(4,L)))
c==== I assume shear rate G (sec**-1) and solv viscosity/temperature 
c==== are on model 2 cards, I(3,L) & I(4,L)  e.g. eta/T = .00920/308.=3.0e-05
c==== for water (?) at 35C
C==== equations are only OK for S >2 
      IF(RA.GT.1.0.AND.RL.GT.RA)SS=ALOG(RL/RA)
      SS=AMAX1(SS,2.0)
C==== 4.1412E-16 IS 3*BOLZTMANN kB, with units conversion for viscosity/temp
C==== AA= V(I(3,L))*3.141592654E-24*RL**3/4.142E-16
C==== V(I1)=AA/BB
      BB= TE*(SS -1.57 +7.0*(0.28-1.0/SS)**2 )
      CC= 1.0 + 14.*(0.28-1./SS)/SS**2
      D(I2)=D(I2)+DL(I1)*V(I1)*TE*CC/(RA*BB)
      D(I2+1)=D(I2+1)+DL(I1)*V(I1)*(3.0 - TE*CC/BB )/RL
      D(I3)=D(I3)+ DL(I1)*3.141592654E-24*RL**3/(BB*4.142E-16)
      D(I4)=D(I4)+DL(I1)*V(I1)*TE
      GOTO 1000
c
C==== shell for core/shell ellipse
c==== for Vshell(dry)/Vcore, f solv in shell
C====            2              3 
C==== NOW need to resort to numerical derivatives, as have to solve
c==== a cubic equation for constant shell thickness
c==== ASSUME that shell thickness & contrast are already set up by CONTIE
C==== NOTE THE DEL(I) MUST BE SET FOR THE MODEL 2 CARDS, else get div by zero !
C====
C==== derivs for V(I2) = y=Vshell/Vcore, I3= f= solv in shell, 
c====             I1-1 = X, I1-2 = R
 1500 AA(1)=(-(V(I2)+DEL(I2))/(1.0-V(I3)) )*V(I1-1)*V(I1-2)**3
      AA(2)= V(I(1,L)-2)**2*(1.0+ 2.0*V(I(1,L)-1) )
      AA(3)= V(I(1,L)-2)*(2.0+ V(I(1,L)-1) )
      AA(4)=1.0
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1520
      END DO
 1520 D(I2)=D(I2) + ((ROOT(IR)-V(I1))/DEL(I2) )*DL(I1)
      AA(1)=(-V(I2)/(1.0-(V(I3)+DEL(I3))) )*V(I1-1)*V(I1-2)**3
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1530
      END DO
 1530 D(I3)=D(I3) + ((ROOT(IR)-V(I1))/DEL(I3) )*DL(I1)
      AA(1)=(-V(I2)/(1.0-V(I3)) )*(V(I1-1)+DEL(I1-1))*V(I1-2)**3
      AA(2)= V(I(1,L)-2)**2*(1.0+ 2.0*(V(I1-1)+DEL(I1-1)) )
      AA(3)= V(I(1,L)-2)*(2.0+ V(I1-1)+DEL(I1-1) )
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1505
      END DO
c==== 11/08/03  fixed V(I1-1) to V(I1)
 1505 D(I1-1)=D(I1-1) + ((ROOT(IR)-V(I1))/DEL(I1-1) )*DL(I1)
      AA(1)=( -V(I2)/(1.0-V(I3)) )*V(I1-1)*(V(I1-2)+DEL(I1-2))**3
      AA(2)= (V(I1-2)+DEL(I1-2))**2*(1.0+ 2.0*V(I1-1) )
      AA(3)= (V(I1-2)+DEL(I1-2))*(2.0+ V(I1-1) )
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1507
      END DO
c==== 11/08/03  fixed V(I1-2) to V(I1)
 1507 D(I1-2)=D(I1-2) + ((ROOT(IR)-V(I1))/DEL(I1-2) )*DL(I1)
      GOTO 1000
C==== contrast for core/shell ellipse, needs FIVE model 2 cards
c==== for Vshell(dry)/Vcore, f solv in shell, sld core, sld shell, sld solv
C====            2              3                  4       5         6
C==== NOW need to resort to numerical derivatives, as have to solve
c==== a cubic equation for constant shell thickness
c==== ASSUME that shell thickness & contrast are already set up by CONTIE
C==== NOTE THE DEL(I) MUST BE SET FOR THE MODEL 2 CARDS, else get div by zero !
C====
C==== same as before but V(I(1,L)) is now the contrast term,
C==== so the offsets for X and R1 change !
 1600 AA(1)=( -(V(I2)+DEL(I2))/(1.0-V(I3)) )*V(I1-2)*V(I1-3)**3
      AA(2)= V(I1-3)**2*(1.0+ 2.0*V(I1-2) )
      AA(3)= V(I1-3)*(2.0+ V(I1-2) )
      AA(4)=1.0
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1620
      END DO
 1620 RH2=(1.0 - V(I3) )*V( I(5,L) ) + V(I3)*V(I(6,L))
      VN=( V(I4)-RH2)*V(I1-2)*V(I1-3)**3 /
     >        ( (RH2-V(I(6,L)))*(V(I1-3)+ROOT(IR))**2*
     >         (V(I1-2)*V(I1-3)+ROOT(IR)) )
      D(I2)=D(I2) + ( (VN-V(I1))/DEL(I2) )*DL(I1)
C
      AA(1)=( -V(I2)/(1.0-(V(I3)+DEL(I3))) )*V(I1-2)*V(I1-3)**3
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1630
      END DO
c==== 11/08/03 needs - not + DEL(I3) in first brackets
 1630 RH2=(1.0- V(I3)-DEL(I3) )*V( I(5,L) ) + (V(I3)+DEL(I3))*V(I(6,L))
      VN=( V(I4)-RH2)*V(I1-2)*V(I1-3)**3 /
     >        ( (RH2-V(I(6,L)))*(V(I1-3)+ROOT(IR))**2*
     >         (V(I1-2)*V(I1-3)+ROOT(IR)) )
      D(I3)=D(I3) + ( (VN-V(I1))/DEL(I3) )*DL(I1)
C
      AA(1)=( -V(I2)/(1.0-V(I3)) )*V(I1-2)*V(I1-3)**3
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1640
      END DO
 1640 RH2=(1.0- V(I3) )*V( I(5,L) ) + V(I3)*V(I(6,L))
      VN=( V(I4)+DEL(I4)-RH2)*V(I1-2)*V(I1-3)**3 /
     >        ( (RH2-V(I(6,L)))*(V(I1-3)+ROOT(IR))**2*
     >         (V(I1-2)*V(I1-3)+ROOT(IR)) )
      D(I4)=D(I4) + ( (VN-V(I1))/DEL(I4) )*DL(I1)
C
      I5=I(5,L)
      I6=I(6,L)
      RH2=(1.0- V(I3) )*(V(I5)+DEL(I5)) + V(I3)*V(I6)
      VN=( V(I4)-RH2)*V(I1-2)*V(I1-3)**3 /
     >        ( (RH2-V(I6))*(V(I1-3)+ROOT(IR))**2*
     >         (V(I1-2)*V(I1-3)+ROOT(IR)) )
      D(I5)=D(I5) + ( (VN-V(I1))/DEL(I5) )*DL(I1)
C
      RH2=(1.0- V(I3) )*V(I5) + V(I3)*(V(I6) +DEL(I6) )
      VN=( V(I4)-RH2)*V(I1-2)*V(I1-3)**3 /
     >        ( (RH2- V(I6)-DEL(I6) )*(V(I1-3)+ROOT(IR))**2*
     >         (V(I1-2)*V(I1-3)+ROOT(IR)) )
      D(I6)=D(I6) + ( (VN-V(I1))/DEL(I6) )*DL(I1)
C
      AA(1)=( -V(I2)/(1.0-V(I3)) )*(V(I1-2)+DEL(I1-2))*V(I1-3)**3
      AA(2)= V(I1-3)**2*(1.0+ 2.0*(V(I1-2)+DEL(I1-2)) )
      AA(3)= V(I1-3)*(2.0+ V(I1-2) +DEL(I1-2) )
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1605
      END DO
 1605 RH2=(1.0- V(I3) )*V( I(5,L) ) + V(I3)*V(I(6,L))
      VN=( V(I4)-RH2)*(V(I1-2)+DEL(I2-2)) *V(I1-3)**3 /
     >        ( (RH2-V(I(6,L)))*(V(I1-3)+ROOT(IR))**2*
     >         ((V(I1-2)+DEL(I1-2))*V(I1-3)+ROOT(IR)) )
      D(I1-2)=D(I1-2) + ( (VN-V(I1))/DEL(I1-2) )*DL(I1)
C
      AA(1)=( -V(I2)/(1.0-V(I3)) )*V(I1-2)*(V(I1-3)+DEL(I1-3))**3
      AA(2)= (V(I1-3)+DEL(I1-3))**2*(1.0+ 2.0*V(I1-2) )
      AA(3)= (V(I1-3)+DEL(I1-3))*(2.0+ V(I1-2) )
      CALL PA03A(AA,ROOT,NR)
      DO IR=1,NR
      IF(ROOT(IR).GT.0.0)GOTO 1607
      END DO
 1607 VN=( V(I4)-RH2)*V(I1-2)*(V(I1-3) +DEL(I1-3))**3 /
     > ( (RH2-V(I(6,L)))*( V(I1-3) +DEL(I1-3) + ROOT(IR))**2*
     >         (V(I1-2)*( V(I1-3)+DEL(I1-3)) +ROOT(IR)) )
      D(I1-3)=D(I1-3) + ( (VN-V(I1))/DEL(I1-3) )*DL(I1)
C
      GOTO 1000
 1700 I5=I(5,L)
      I6=I(6,L)
      I7=I(7,L)
      R2A=(1.0-V(I(3,L)))*V(I(4,L)) + V(I(3,L))*V(I(5,L))
      R2B=(1.0-V(I(3,L)))*V(I(6,L)) + V(I(3,L))*V(I(7,L))
      R2BV7=R2B-V(I(7,L))
      R2AV5=R2A-V(I(5,L))
      BIT=2.0*V(I(2,L))*R2BV7/R2AV5

      D(I2)=D(I2) +((R2BV7/R2AV5)**2)*DL(I1)
      D(I3)=D(I3)+ BIT*( (V(I4)-V(I5))*R2BV7/R2AV5**2 -
     >   (V(I7) - V(I6))/R2AV5 )*DL(I1)
      TE= BIT*((V(I3)-1.0)*R2BV7/R2AV5**2)*DL(I1)
      D(I4)=D(I4) +TE
      D(I5)=D(I5) -TE
      TE= BIT*((V(I3)-1.0)/R2AV5)*DL(I1)
      D(I6)=D(I6) -TE
      D(I7)=D(I7) +TE
      GOTO 1000
 1800 I5=I(5,L)
      R2A = (1.0-V(I2))*V(I4) +V(I2)*V(I5)
C==== TRAP DIV BY ZERO !
      IF(ABS(R2A-V(I5)).LE.1.E-04)R2A=V(I5)-0.0001
      R2AV5 = 1.0/(R2A-V(I5))
      R2AV5SQ = R2AV5**2
      D(I2)=D(I2)+ DL(I1)*(V(I3)-V(I5))*(V(I5)-V(I4))*R2AV5SQ
      D(I3)=D(I3)+ DL(I1)*R2AV5
      D(I4)=D(I4)+ DL(I1)*V(I2)*(V(I3)+V(I5)-2.0*R2A)*R2AV5SQ
      D(I5)=D(I5)+ DL(I1)*V(I2)*(V(I5)-V(I3))*R2AV5SQ
      GOTO 1000
 1900 D(I2)=D(I2)+P(1,L)*DL(I1)
      D(I3)=D(I3)+P(2,L)*DL(I1)
      D(I4)=D(I4)+P(3,L)*DL(I1)
      D(I(5,L))=D(I(5,L))+P(4,L)*DL(I1)
      D(I(6,L))=D(I(6,L))+P(5,L)*DL(I1)
      D(I(7,L))=D(I(7,L))+P(6,L)*DL(I1)
      D(I(8,L))=D(I(8,L))+P(7,L)*DL(I1)
      GOTO 1000
c==== note type 20: the radii are usually also constrained
c==== so the order of constraints is important
c====  Rj is zero for last (vertical) step, hence use EXPLR() and
c====  ALG() to avoid errors
C==== RKH 1/4/99 why is there no *DL(I1) here !!!
c====         try adding it, might work better !!!
c==== RKH 15/11/05, try to correct the derivatives, there
c==== were small mistakes in I3, I4 and big one in I6
c==== 
 2000 D(I2)=D(I2) -DL(I1)*V(I(6,L))*V(I(5,L))*(EXPLR(V(I3),V(I(6,L)))-
     >                           EXPLR(V(I4),V(I(6,L))) )*
     >                    EXPLR(V(I2),(-V(I(6,L))-1.0) ) 
      IF(ABS(V(I2)-V(I3)).GT.1.E-12) D(I3)=D(I3)+
     >   DL(I1)*V(I(5,L))*V(I(6,L))*EXPLR(V(I3),V(I(6,L))-1.0)*
     >   EXPLR(V(I2),-V(I(6,L)) )
      D(I4)=D(I4)-DL(I1)*V(I(5,L))*V(I(6,L))*EXPLR(V(I4),V(I(6,L))-1.0)*
     >   EXPLR(V(I2),-V(I(6,L)) )
      D(I(5,L))=D(I(5,L))+DL(I1)*(EXPLR(V(I3),V(I(6,L)))-
     >                     EXPLR(V(I4),V(I(6,L))) )*
     >   EXPLR(V(I2),-V(I(6,L)) )
      D(I(6,L))=D(I(6,L))+DL(I1)*V(I(5,L))*
     >   (  ALG(V(I3)/V(I2))*EXPLR(V(I3),V(I(6,L)))- 
     >      ALG(V(I4)/V(I2))*EXPLR(V(I4),V(I(6,L)))   )*
     >   EXPLR(V(I2),-V(I(6,L)))
      GOTO 1000
C==== 21 contrast for core/shell ellipse, needs FIVE model 2 cards
c==== used same cards as constr 16  BUT second integer ignored !
c==== for Vshell(dry)/Vcore, f solv in shell, sld core, sld shell, sld solv
C====       2 NOT USED           3                  4       5         6
C==== V(J) = ( rho1 -rho2)*Vcore / (rho2 -rho3)*Vtot 
 2100 I5=I(5,L)
      I6=I(6,L)
      VC=4.188790295*V(I1-2)*V(I1-3)**3
      VT=4.188790295*(( V(I1-3)+V(I1-1))**2)*
     >       ( V(I1-2)*V(I1-3) +V(I1-1))
      RH2=(1.0-V(I3))*V(I5) + V(I3)*V(I6)
      V(I1)= ( (V(I4) - RH2)*VC )/
     >            ( (RH2-V(I6)) *VT )
      D(I1-3)=D(I1-3)+DL(I1)*V(I1)*(2.0/V(I1-3) +
     >       (V(I1-3)+V(I1-1))*(V(I1-3)*V(I1-2)+V(I1-1))/
     >       (3.*V(I1-3)*V(I1-2) +2.*V(I1-1) +V(I1-1)*V(I1-2)) )
      D(I1-2)=D(I1-2)+DL(I1)*V(I1)*( V(I1-3)**(-3.0) +
     >                             V(I1-2) + V(I1-1)/V(I1-3) )
      D(I1-1)=D(I1-1)+DL(I1)*V(I1)*(V(I1-3)+V(I1-1))*
     >      (V(I1-3)*V(I1-2) +V(I1-1))/
     >      (2.*V(I1-3)*V(I1-2) +3.*V(I1-1) +V(I1-3) )
      D(I3)=D(I3)+DL(I1)*(VC/VT)*( (V(I5)-V(I6))/(RH2-V(I6)) +
     >               (V(I4)-RH2)/(V(I6)-V(I5)) )
      D(I4)=D(I4)+DL(I1)*(VC/VT)/(RH2-V(I6))
      BB=DL(I1)*(VC/VT)*(-V(I3)/(RH2-V(I6)) +
     >                                (RH2-V(I4))/(1.0-V(I3)) )
      D(I5)=D(I5)-BB
      D(I6)=D(I6)+BB
      GOTO 1000
c==== 4/12/00    V(I1)=P1*V(I2)/ ( P2 + P3*V(I3) )
 2200 D(I2)=D(I2)+ DL(I1)*P(1,L)/(P(2,L) +P(3,L)*V(I3) )
      D(I3)=D(I3)- DL(I1)*P(1,L)*P(3,L)*V(I2)/ (P(2,L)+P(3,L)*V(I3))**2
      GOTO 1000
c==== 13/6/01 oil & drug partitions, 31/8/01 D5 to D8 corrected
 2300 I5=I(5,L)
      I6=I(6,L)
      I7=I(7,L)
      I8=I(8,L)
      D(I2)=D(I2)+ DL(I1)*(-P(1,L)*V(I6)/V(I4) + P(3,L)*(V(I8)-V(I6))/V(I5) )
      D(I3)=D(I3)+ DL(I1)*(-P(2,L)*V(I7)/V(I4) + P(4,L)*(V(I8)-V(I7))/V(I5) )
      D(I4)=D(I4)- DL(I1)*(  P(1,L)*V(I6)*(1.0-V(I2)) +
     >                       P(2,L)*V(I7)*(1.0-V(I3))    ) / (V(I4)**2)
      D(I5)=D(I5)- DL(I1)*(  P(3,L)*V(I2)*(V(I8)-V(I6)) +
     >                       P(4,L)*V(I3)*(V(I8)-V(I7))  ) / (V(I5)**2)
      D(I6)=D(I6)+ DL(I1)*( P(1,L)*(1.0-V(I2))/V(I4) - P(3,L)*V(I2)/V(I5) )
      D(I7)=D(I7)+ DL(I1)*( P(2,L)*(1.0-V(I3))/V(I4) - P(4,L)*V(I3)/V(I5) )
      D(I8)=D(I8)+ DL(I1)*( P(3,L)*V(I2) + P(4,L)*V(I3) )/V(I5)
      GOTO 1000
C==== N.B. this constraint ties TWO params at once !
c==== Oops need to make sure derivs keep s/c, X1,X2,Y1 in physical ranges
c==== so that SEEK1 works.
c==== V(I3)=S/C,  V(I4) = X1(conc solv),  V(I5)=X2,
C==== V(I6)=Y1(frac shell in shell 1),    V(I7+4)=R3
 2400 I5=I(5,L)
      I6=I(6,L)
      I7=I(7,L)
      CALL SEEK1(R13,R23,V(I3)+DEL(I3),V(I4),V(I5),V(I6),I7)
      D(I3)=D(I3)+DL(I1)*(R13-V(I1))/DEL(I3)
      D(I3)=D(I3)+DL(I2)*(R23-V(I2))/DEL(I3)
c
      DD=DEL(I4)
      IF(V(I4)+DD.GT.0.9989)DD=-DD
      CALL SEEK1(R13,R23,V(I3),V(I4)+DD,V(I5),V(I6),I7)
      D(I4)=D(I4)+DL(I1)*(R13-V(I1))/DD
      D(I4)=D(I4)+DL(I2)*(R23-V(I2))/DD
c
      DD=DEL(I5)
      IF(V(I5)+DD.GT.0.9989)DD=-DD
      CALL SEEK1(R13,R23,V(I3),V(I4),V(I5)+DD,V(I6),I7)
      D(I5)=D(I5)+DL(I1)*(R13-V(I1))/DD
      D(I5)=D(I5)+DL(I2)*(R23-V(I2))/DD
c
      DD=DEL(I6)
      IF(V(I6)+DD.GT.1.0)DD=-DD
      CALL SEEK1(R13,R23,V(I3),V(I4),V(I5),V(I6)+DD,I7)
      D(I6)=D(I6)+DL(I1)*(R13-V(I1))/DD
      D(I6)=D(I6)+DL(I2)*(R23-V(I2))/DD
c
c====  N.B. I7 is pointer to R1 not R3, V(I8) IS R3
      I8=I7+4
      V(I8)=V(I8)+DEL(I8)
      CALL SEEK1(R13,R23,V(I3),V(I4),V(I5),V(I6),I7)
      D(I7)=D(I7)+DL(I1)*(R13-V(I1))/DEL(I8)
      D(I7)=D(I7)+DL(I2)*(R23-V(I2))/DEL(I8)
      V(I7)=V(I7)-DEL(I8)
      GOTO 1000
c
 2500 D(I2) = D(I2)+ V(I4)*DL(I1)
      D(I3) = D(I3)- V(I4)*DL(I1)
      D(I4) = D(I4)+ ( V(I2)- V(I3) ) *DL(I1)
      D(I(5,L)) = D(I(5,L))+ V(I(7,L))*DL(I1)
      D(I(6,L)) = D(I(6,L))- V(I(7,L))*DL(I1)
      D(I(7,L)) = D(I(7,L))+ ( V(I(5,L))- V(I(6,L)) )*DL(I1)
      D(I(8,L)) = D(I(8,L))+ V(I(10,L))*DL(I1)
      D(I(9,L)) = D(I(9,L))- V(I(10,L))*DL(I1)
      D(I(10,L)) = D(I(10,L))+ ( V(I(8,L))- V(I(9,L)) )*DL(I1)
 1000 CONTINUE
      RETURN
      END
C
      SUBROUTINE REFINE(IR)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1989
C==== written by R.K.Heenan at UKC and RAL 1983-1986, revised Dec. 1988
C==== to store entire derivative array for resolution smearing
C==== here at last is the routine which does all the hard work.
c==== 18/2/02 after all calcs, reset model 10 radii to mean polydisp radius
C
      INCLUDE 'FISHDIM.PAR'
      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,IX,NX1,NX2,IRX,
     >     LS2(12),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
C==== THE (MV) AND (MI) DIMENSIONS ARE ALSO NEEDED IN DERIV,CONDER,CORREL, MINV ETC.
C==== IN CALL TO MINV AND TEST ON NPR BELOW.
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      COMMON/POLY/PR1,PR2,NR1,NR2,ABC(5),RB,PA,PB,PCC,PD,IPD,JPD,NPP,
     *       NPP1,NPP2,NPP3,NPM,NSIMP
      COMMON/WORK/DWE(MI),D(MV),DWD2(MI,MI),
     * V2(MV),PM(10),PM2(10),DD(MV,3),WT(10),DBIG(MI,MN),HH(MI),FILL(1763)
      COMMON/DMAT/IID(MV),DWD(MI,MI)
      IF(IR.EQ.1)GOTO 365
   10 SSE=0.0
      NDAT=0
      NPRED=0
      NPR=0
      IF(MS.LT.1)MS=1
      IF(NOREF.EQ.1)GOTO 500
      NCUSE=0
      IF(NC.EQ.0)GOTO 40
      DO 30 I=1,NC
      IF(NUSE(I).EQ.1)NCUSE=1
   30 CONTINUE
   40 DO 50 I=1,NP
      DV(I)=0.0
      ESD(I)=0.0
C==== ZERO D() HERE,JUST ONCE , NOT SET IN DERIV ROUTINES,SHOULDN'T BE USED
      IF(PS(I).LT.0.01)GOTO 50
      NPR=NPR+1
      IID(NPR)=I
   50 CONTINUE
c==== 29/5/96 force call to DELSET each time though !
      IFLAG=0
      CALL DELSET(IFLAG)
c==== 28/11/90 add a test for MODEL 16 to do calcs for Teubner & Strey peak
      CALL TSCALC
      IF(NPR.EQ.0)NOREF=1
      IF(NOREF.EQ.1)GOTO 500
C==== 9/12/98 flag if matrices not large enough !
      IF(NPR.GT.MI)THEN
      WRITE(JS,51)NPR,MI
      WRITE(JF,51)NPR,MI
51    FORMAT(1X,'TOO MANY params ON or TIED, NPR,MI= ',2i4)
      NPR=MI
      END IF
C
      DO 60 I=1,NPR
      DWE(I)=0.0
      DO 60 II=I,NPR
      DWD(II,I)=0.0
   60 CONTINUE
C====                            LOOP OVER SETS OF DATA
      DO 200 KS=1,NS
      FIT(KS)=0.0
      SUM=0.0
      KD=JD(KS)
C==== 11/7/01 allow to skip a set if OBS = -1
      IF(KD.EQ.-1)GOTO 200
      KC=JC(KS)
      IF(KD.LE.0.OR.KC.LE.0)GOTO 800
      KB=JB(KS)
      KY=JY(KS)
      KW=JW(KS)
C==== TURN OFF SMEARING, DERIV ROUTINE WILL SWITCH ON IF REQUIRED
      NPSMEAR=0
C==== CALL OUT TO PREDICATE DATA CALC AND REFINEMENT ROUTINE ???
C==== SET UP POLYDISPERSITY
      IF(KY.GT.0)CALL POLSET(Q(1,KY),C(1,KY),KY,KS)
C==== CHECK FOR MODEL 23 AND 25 (INTERACTING PARTICLES) 
C==== WHICH NEED A PRELIMINARY CALCULATION 
      CALL SSQFNSET(IERR)
      IF(IERR.LE.0)RETURN
      IF(IP.NE.0.AND.KY.GT.0)CALL PREDER(NCUSE,E(1,KY))
C====  IP=0 STOPS ALL PEDICATE OBS. , IP=1 ALLOWS THEM, IP=2 IS A FLAG THAT
C====  SPECIAL PREDICATES ARE BEING REFINED,   IY=1 IS A FLAG FOR POLDISP.
C==== COEFFS. REFINING.
      KKD=NC4(KD)
      JJD=NC1(KD)
      IF(NC1(KD)*NC2(KD).LE.0)JJD=NC3(KD)
      IF(KW.LE.0)GOTO 65
C==== INITIALISE WORK SPACE WHERE BKG WILL BE STORED
      DO 64 J=1,KKD
   64 C(J,KW)=0.0
   65 IF(MS.EQ.1)GOTO 70
C==== SET CALC TO ZERO SO INTERMEDIATE POINTS ARE OBVIOUS IF ATTEMPT TO PLOT THEM
      DO 68 J=1,KKD
   68 C(J,KC)=0.0
   70 IF(NOWT.EQ.1)CALL WEIGHT(E(1,KC),E(1,KD),C(1,KD),IW,KS,KKD)
C==== MS=K4 MAY BE USED TO SKIP DATA IN LONG CALCS
C====                 LOOP OVER Q FOR THIS SET
      DO 150 J=JJD,KKD,MS
      IF(J.GT.NC2(KD).AND.J.LT.NC3(KD)) GOTO 150
      NDAT=NDAT+1
      CALL DERIV(Q(J,KD),C(J,KC),SUMW,SUMPQ,PRODSQ,BETARATIO,SUMBUG,
     *           KS,J,KB,KY,NPSMEAR,D)
      IF(KW.GT.0)C(J,KW)=SUMW
      IF(JPQ(KS).GT.0)C(J,JPQ(KS))=SUMPQ
      IF(JSQ(KS).GT.0)C(J,JSQ(KS))=PRODSQ
      IF(JBT(KS).GT.0)C(J,JBT(KS))=BETARATIO
      IF(JXX(KS).GT.0)C(J,JXX(KS))=SUMBUG
      IF(NCUSE.EQ.1)CALL CONDER(1,NC,D,D)
C==== COMPRESS DERIVATIVES D( ) INTO STORE DBIG( ) TO REMOVE CONSTRAINED 
C==== OR OFF PARAMETERS AND SAVE READY FOR A SMEARING CONVOLUTION
C==== 30/4/98 expt with fit to log data
      IF(IW.EQ.3)THEN
      DO I=1,NPR
        IF(ABS(C(J,KC)).GT.1.E-24)THEN
         DBIG(I,J)=D(IID(I))/C(J,KC)
        ELSE
         DBIG(I,J)=0.0
        END IF
      END DO
      ELSE
C==== normal
      DO 80 I=1,NPR
   80 DBIG(I,J)=D(IID(I))
      END IF
C==== END 1ST LOOP OVER DATA POINTS IN THIS SET
  150 CONTINUE
C==== CALL THE SMEARING ROUTINE, WHICH CAN RECOMBINE C(J,KC) AND DBIG(I,J)
C==== BY SOME CONVOLUTION PROCEDURE,  CAN ONLY BE DONE AFTER ALL OTHER 
C==== CALCULATION ON THIS SET
      IF(NPSMEAR.NE.0)CALL SMEARING(KS,KC,KD,KW,KB,KY,JAM(KS),JJD,KKD)
C====
C==== NOW COMPUTE THE LEAST SQUARES MATRICES
  160 DO 190 J=JJD,KKD,MS
      IF(J.GT.NC2(KD).AND.J.LT.NC3(KD)) GOTO 190
      IF(IW.EQ.3)THEN
C==== 30/4/98 expt with fit to log(y)
      IF(ABS(C(J,KC)).GT.1.E-24.AND.ABS(C(J,KD)).GT.1.E-24)THEN
      EE=ALOG(ABS(C(J,KD))/ABS(C(J,KC)))
      ELSE
      EE=0
      END IF
      ELSE
C====       obs - calc
      EE=C(J,KD)-C(J,KC)
      END IF
C==== ASSUME WEIGHTS ARE DIAGONAL, STORED IN E(J,KC)
      W=E(J,KC)
      WE=W*EE
      SUM=SUM+WE*EE
      DO 170 I=1,NPR
  170 DWE(I)=DWE(I)+WE*DBIG(I,J)
C==== FILL LOWER TRIANGLE IN FAST ACCESS ORDER
      DO 180 I=1,NPR
      DO 180 II=I,NPR
  180 DWD(II,I)=DWD(II,I)+W*DBIG(I,J)*DBIG(II,J)
C==== END 2ND LOOP OVER DATA IN A SET
  190 CONTINUE
      FIT(KS)=FIT(KS)+SUM
      SSE=SSE+SUM
C==== END LOOP OVER SETS OF DATA
      IF(KY.LE.0)GOTO 200
C==== RESET FIRST AND LAST POLY, IN CASE THEY ARE PLOTTED
      C(NR1,KY)=2.0*C(NR1,KY)
      C(NR2,KY)=2.0*C(NR2,KY)
  200 CONTINUE
      IF(IK.NE.1)GOTO 240
      IKN=1
      A=1.0+CON(1)
C====  IK=1 MARQUARDT METHOD, LAMBDA STORED IN CON(1),  ( USE N1 TO SEE IN TALK)
      DO 220 I=1,NPR
      DO 210 J=I,NPR
  210 DWD2(J,I)=DWD(J,I)
      DWD(I,I)=DWD(I,I)*A
  220 CONTINUE
      DO 230 I=1,NP
  230 V2(I)=V(I)
  240 DO 245 I=1,NPR
C==== MACHINE DEPENDENT CONSTANT
      IF(DWD(I,I).GT.1.E-30)GOTO 245
      DWD(I,I)=1.0
      WRITE(JS,241)IID(I)
  241 FORMAT(1X,'ZERO DERIVATIVE,  V(',I3,') does not contribute,',/,
     >  '  or data weights may be zero? (try K1=2 if data has no errors) ')
      WRITE(JF,241)IID(I)
  245 CONTINUE
      CALL MINV(DWD,HH,NPR,MI)
      IF(NPR.EQ.1)GOTO 255
      DO 250 I=2,NPR
      II=I-1
      DO 250 J=1,II
  250 DWD(J,I)=DWD(I,J)
  255 DO 260 I=1,NP
      ESD(I)=0.0
  260 DV(I)=0.0
      XDWE=0.0
      DO 290 I=1,NPR
      A=0.0
      DO 280 J=1,NPR
  280 A=A+DWD(I,J)*DWE(J)
      XDWE=XDWE+A*DWE(I)
C==== EXPAND THE SHIFTS  TO FULL DV() ARRAY
  290 DV(IID(I))=A
      IF(IK.NE.1)GOTO 350
C==== START MARQUARDT CALC
      DO 295 I=1,NP
  295 V(I)=V(I)+DV(I)*PS(I)
c
C==== NEXT LINE IS NOREF=1 ENTRY ..................................
  297 SSE2=0.0
      IF(NC.GT.0)CALL CONTIE(1,NC)
      DO 300 KS=1,NS
      FIT(KS)=0.0
C==== 11/7/01 allow skip set if OBS = -1
      IF(JD(KS).EQ.-1)GOTO 300
      KY=JY(KS)
      IF(KY.GT.0)CALL POLSET(Q(1,KY),C(1,KY),KY,KS)
C==== CHECK FOR MODEL 23 INTERACTING PARTICLES which need a preliminary calc.
      CALL SSQFNSET(IERR)
      IF(IERR.LE.0)RETURN
      CALL CALCUL(KS,JC(KS),JD(KS),JB(KS),KY,JW(KS),JAM(KS),
     *              JPQ(KS),JSQ(KS),JBT(KS),JXX(KS),SUM,NPSMEAR,MS)
      IF(NS.GT.1)WRITE(JS,299)KS,SUM,FIT(KS)-SUM
  299 FORMAT(1X,'  S',I1,' SSE=',1PE10.3,'      ',10X,' DEL SSE=',E10.3)
      SSE2=SSE2+SUM
      FIT(KS)=SUM
  300 CONTINUE
c
      IF(NOREF.EQ.1)THEN
      SSE=SSE2
      GOTO 400
      END IF
C==== DEBUG
      A=SSE-SSE2
      WRITE(JS,301)IKN,SSE,XDWE,A,CON(1)
      WRITE(JF,301)IKN,SSE,XDWE,A,CON(1)
  301 FORMAT(1X,I4,' SSE=',1PE10.3,' XDWE=',E10.3,' DEL SSE=',E10.3,
     * ' LAMBDA=',E8.1)
      IF(SSE2.LT.SSE) GOTO 340
      IF(MOD(IKN,5).NE.0)GOTO 305
 3021 WRITE(JS,302)IKN
  302 FORMAT(1X,'ITERATION',I4,'  CONTINUE ? (ANS 1)')
      CALL FLUSH(JS)
      I=0
      WRITE(JS,303)
  303 FORMAT(1X,'Fit diverging')
c      READ(IS,*,ERR=3021),I
      IF(I.NE.1)GOTO 550
C==== REPEAT INVERSION WITH LARGER LAMBDA IF FIT DIVERGES
  305 IKN=IKN+1
      CON(1)=CON(1)*10.0
      A=1.0+CON(1)
      DO 320 I=1,NPR
      DO 310 J=I,NPR
  310 DWD(J,I)=DWD2(J,I)
  320 DWD(I,I)=DWD(I,I)*A
      DO 330 I=1,NP
  330 V(I)=V2(I)
      GOTO 240
  340 CON(1)=CON(1)*0.1
      SSE=SSE2
C==== CALCULATE ESD'S
  350 II=MAX(1,NDAT-NPR+2)
      VAR=SSE/FLOAT(II)
C==== IS THIS CORRECT FOR LARGE LAMBDA ???
      DO 360 I=1,NPR
  360 ESD(IID(I))=SQRT(VAR*ABS(DWD(I,I)))
C==== PARAMS V( ) ARE RESULTS FOR MARQUARDT, PREVIOUS CYCLE FOR NORMAL ROUTE
C..................................................................
C==== LINE 400 IS THE WAY OUT FOR NOREF=1 &  =0 ROUTES
C==== 18/2/02 RKH force model 10 polydisp radii back to calc mean value
c==== 9/9/3 oops this is still being doing for PREVIOUS model if poly is
c==== not being stored & hence POLSET has not been called. Need set IX zero
c==== in LSMAIN. 
  400 IF(IX.EQ.2)THEN
      V(IRX)=PM(4)
      IF(NC.GT.0)CALL CONTIE(1,NC)
      END IF
      RETURN
C................................................................
  365 IR=0
      IF(IK.EQ.1) GOTO  375
C==== APPLY SHIFTS FOR NORMAL ROUTE, DONE AFTER PS MAY HAVE BEEN MODIFIED ETC.
      DO 370 I=1,NP
      IF(PS(I).LT.0.01) GOTO 370
      V(I)=V(I)+DV(I)*PS(I)
  370 CONTINUE
  375 IF(NC.GT.0)CALL CONTIE(1,NC)
      GOTO 10
C...................................................................
C==== this is route for NOREF=1, which then untidily skips back above !
  500 WRITE(JS,501)
      WRITE(JF,501)
  501 FORMAT(1X,'OFF - USING CALCUL ROUTINE')
      IF(NOWT.EQ.0)GOTO 297
C==== STILL NEED WTS FOR SSE IN CALCUL
      DO 520 KS=1,NS
      KD=JD(KS)
      IF(KD.EQ.-1)GOTO 520
      KC=JC(KS)
      KKD=NC4(KD)
      CALL WEIGHT(E(1,KC),E(1,KD),C(1,KD),IW,KS,KKD)
  520 CONTINUE
      GOTO 297
C......................................................................
C==== Marquardt fit only gets worse, restore starting parameters
c==== but note ESD's still used latest lambda 
  550 DO 555 I=1,NP
      DV(I)=0.0
  555 V(I)=V2(I)
      IF(NC.GT.0)CALL CONTIE(1,NC)
      GOTO 350
C......................................................................
  800 WRITE(JS,801)
  801 FORMAT(1X,'ERROR:  DATA SET NUMBER FOR OBS OR CALC IS ZERO !',/
     * ' type STOP and then use options 2 or 4 in menu to choose sets')
      RETURN
      END
      SUBROUTINE CORREL(IJ)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== PRODUCE A PARAMETER CORRELATION MATRIX ON CHANNEL IJ
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      COMMON/WORK/DWE(MI),D(MV),DWD2(MI,MI),
     * V2(MV),IROW(MI),DIAG(MI),FILL(100298)
      COMMON/DMAT/IID(MV),DWD(MI,MI)
C==== DON'T ALTER DWD**-1, MUST BE ABLE TO USE THIS ROUTINE MORE THAN ONCE
      IF(NPR.LT.2)RETURN
      IF(IJ.NE.JS)WRITE(IJ,11)
   11 FORMAT(/1X,'PARAMETER CORRELATION MATRIX *100'/)
      WRITE(IJ,1)(IID(I),I=1,NPR)
    1 FORMAT(15X,30I4)
      DO 50 I=1,NPR
      A=ABS(DWD(I,I))
C==== MACHINE DEPENDENT CONSTANT
      IF(A.LT.1.E-30)A=1.0
   50 DIAG(I)=0.1*SQRT(A)
      DO 100 I=1,NPR
      A=DIAG(I)
      DO 80 J=1,NPR
      B=DWD(I,J)/(A*DIAG(J))
      IF(B.GT.0.0)B=B+0.5
   80 IROW(J)=IFIX(B)
C==== THIS IFIX IS NOT ROUNDED PROPERLY
      II=IID(I)
  100 WRITE(IJ,2)II,(LPAR(J,II),J=1,3),(IROW(K),K=1,NPR)
    2 FORMAT(1X,I2,3A4,30I4)
      IF(IJ.NE.JS)WRITE(IJ,1)
      RETURN
      END
      SUBROUTINE WEIGHT(W,E,C,IW,KS,N)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985, IW=2 added RKH 18/4/96, IW=3 27/4/98
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      DIMENSION W(MN),E(MN),C(MN)
C==== NEED /TWO/ FOR NS, BUT IW IS PASSED THROUGH THE CALL
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IIW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF),
     * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF),
     * VAR,XDWE,NPSMEAR
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
C==== IW=0 UNIT WEIGHTS
      IF(IW.NE.0)GOTO 100
   10 DO 50 I=1,N
   50 W(I)=1.0
      WRITE(JS,2010)
      WRITE(JF,2010)
      IF(KS.NE.NS)RETURN
      NOWT=0
      RETURN
C==== IW=1  WEIGHTS = 1/SIGMA**2 OR IW=2  WT=1/DATA ( FOR ERR=SQRT(DATA) )
C==== 27/4/98 add IW=3 ( for ERR= E(I)/C(I) to try fit log(y) better )
  100 NZER=0
      DO 120 I=1,N
      EE=E(I)*E(I)
      IF(IW.EQ.2)EE=C(I)
      IF(ABS(EE).GT.1.E-24)THEN
C==== DOESN'T FLAG 1.010101, BUT RANGES SHOULD NOT LEAVE SUCH POINTS
C==== IN THE REFINEMENT
      W(I)=1.0/EE
      IF(IW.EQ.3)W(I)=W(I)*(C(I))**2
      ELSE
      W(I)=0.0
      NZER=NZER+1
      END IF
  120 CONTINUE
      IF(NZER.GT.0)WRITE(JS,121)KS,JD(KS),NZER
  121 FORMAT(1X,'WARNING !  FIT ',I3,'  SET(',I3,')  HAS ',I4,
     *  ' ZERO WEIGHTS',/,
     *1X,'  TRY K1=0 OR 2 IF NO ERRORS ARE ASSOCIATED WITH THIS DATA',/)
      IF(KS.NE.NS)RETURN
      NOWT=0
      IF(IW.EQ.1)THEN
      WRITE(JS,2011)
      WRITE(JF,2011)
      ELSE IF(IW.EQ.2)THEN
      WRITE(JS,2012)
      WRITE(JF,2012)
      ELSE IF(IW.EQ.3)THEN
      WRITE(JS,2013)
      WRITE(JF,2013)
      END IF
 2010 FORMAT(1X,'USING UNIT WEIGHTS (K1=0)')
 2011 FORMAT(1X,'USING  1/E(I)**2  WEIGHTS (K1=1)')
 2012 FORMAT(1X,'USING 1/(SQRT(C(I)))**2  WEIGHTS (K1=2)')
 2013 FORMAT(1X,'USING (C(I)/E(I))**2  WEIGHTS (K1=3) FOR log(C(I))')
      RETURN
      END
      SUBROUTINE MINV(B,H,N,ND)
C====  N IS SIZE OF MATRIX, ND IS DIMENSION OF ARRAY SPACE
C==== INVERTS A POSITVE DEFINITE MATRIX BY THE GAUSS-JORDAN METHOD
C==== SEE LINEAR ALGEBRA BY WILKINSON AND REINSCH
C==== B CONTAINS A SYMMETRIC N*N MATRIX , ONLY THE LOWER TRIANGLE
C==== OF WHICH IS USED, BEING REPLACED BY ITS INVERSE.
C==== THE ROUTINE IS MORE EFFICIENT WITH THE ONE DIMENSIONAL ARRAY !
      DIMENSION B(ND*ND),H(ND)
C==== INVESTIGATE USE OF DOUBLE PRECISION ???
      IF(N.EQ.1)GOTO 200
      NN=N+(N-1)*ND
      ND1=ND+1
      N1=N+1
      DO 41 KK=1,N
      K=N1-KK
      P=B(1)
      IF(P)150,150,10
   10 DO 21 I=2,N
      Q=B(I)
      H(I)=Q/P
      IF(I.LE.K) H(I)=-H(I)
      IJ=I-ND1
      IK=I
      DO 11 J=2,I
      IJ=IJ+ND
      IK=IK+ND
   11 B(IJ)=B(IK)+Q*H(J)
   21 CONTINUE
      B(NN)=1.0/P
      IJ=N
      DO 31 I=2,N
      B(IJ)=H(I)
   31 IJ=IJ+ND
   41 CONTINUE
      RETURN
  150 WRITE(6,151)P,KK,K
  151 FORMAT(/' INVERSION FAILED  P=',1PE10.3,'  KK=',I4,'  K=',I4/)
      RETURN
  200 IF(ABS(B(1)).LT.1.E-12)B(1)=1.0
      B(1)=1.0/B(1)
      RETURN
      END
      SUBROUTINE TABLE(IJ)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
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
      DO 100 IS=1,NS
      KD=JD(IS)
      IF(KD.EQ.-1)GOTO 100
      KC=JC(IS)
      KB=JB(IS)
      WRITE(IJ,99)IS,KD,KC,KB
      J1=NC1(KD)
      J2=NC2(KD)
      IF(J1-J2.GT.0)CALL TAB2(J1,J2,KD,KC,KB,IJ)
      J1=NC3(KD)
      J2=NC4(KD)
      IF(J2-J1.GT.0)CALL TAB2(J1,J2,KD,KC,KB,IJ)
  100 CONTINUE
      WRITE(IJ,101)
  101 FORMAT(1X)
      RETURN
   99 FORMAT(//1X,'SET',I3,' DATA',I3,' CALC',I3,' BKG',I3/
     * '  Q         OBS        CALC       O-C        WT         BKG',8X,
     *'  Q         OBS        CALC       O-C        WT         BKG')
      END
      SUBROUTINE TAB2(J1,J2,KD,KC,KB,IJ)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 1985.
C==== written by R.K.Heenan at UKC and RAL 1983-1986
C==== OUTPUT IN TWO COLUMNS, EASIER TO SCAN OBS-CALC FOR SYSTEMATIC RESIDUALS.
      INCLUDE 'FISHDIM.PAR'

      COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW),IDC(MW),
     *NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),
     *C(MN,MW),Q(MN,MW),E(MN,MW),NDIM
      JJ=J2-J1+1
      JR=MOD(JJ,2)
      JJ=JJ-JR
      JJ2=JJ/2
      B1=0.0
      B2=0.0
      IF(JJ2.EQ.0)GOTO 120
      JJ=J1+JJ2-1
      DO 100 J=J1,JJ
      K=J+JJ2
      IF(KB.GT.0)B1=C(J,KB)
      IF(KB.GT.0)B2=C(K,KB)
      D1=C(J,KD)-C(J,KC)
      D2=C(K,KD)-C(K,KC)
  100 WRITE(IJ,101)Q(J,KD),C(J,KD),C(J,KC),D1,E(J,KC),B1,
     *             Q(K,KD),C(K,KD),C(K,KC),D2,E(K,KC),B2
      IF(JR.EQ.0)GOTO 150
  120 D1=C(J2,KD)-C(J2,KC)
      IF(KB.GT.0)B1=C(J2,KB)
      WRITE(IJ,102)Q(J2,KD),C(J2,KD),C(J2,KC),D1,E(J2,KC),B1
  150 RETURN
  101 FORMAT(1PE10.3,3E11.3,E9.1,E11.3,3X,4E11.3,E9.1,E11.3)
  102 FORMAT(66X,1P4E11.3,E9.1,E11.3)
      END
c==== GAMMA FUNCTION FOR ASHLEY'S "FRACTALS" MODEL 13 ==============
      FUNCTION GAFU(XP)
	IMPLICIT DOUBLEPRECISION(A-F,O-Z)
      NX=1E05
      Y=1
      ALP=XP
      DO 3 I=1,NX
      XN=I/(ALP+I)
      Y=Y*XN
3     CONTINUE
      Z=(NX**ALP)/ALP
      GAFU=Z*Y
      END
      SUBROUTINE TSCALC
C====  28/11/90 to calc Teubner & Strey correlation length and domain
C==== size for parameters as at start of cycle
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
C==== SEARCH FOR MODEL 16
      I=1
   10 IF(LM(I).NE.16)GOTO 50
      IF(LTYP(I).NE.1)GOTO 40
      XX=0.5*(V(I+2)**2)*SQRT(V(I+1)/(V(I+1)-V(I)))
      YY=0.5*V(I+2)**2
      ZZ=1/SQRT(XX-YY)
      DD=2.0*3.141592654/SQRT(XX+YY)
      WRITE(JS,101)DD,ZZ
      WRITE(JF,101)DD,ZZ
  101 FORMAT(1X,'MODEL 16  DOMAIN SIZE =',1PE12.4,'    CORREL. L =',1PE12.4)
      GOTO 45
   40 IF(LTYP(I).NE.11)GOTO 45
      XX=0.5*SQRT(V(I+2)/V(I))
      YY=0.25*V(I+1)/V(I+2)
      ZZ=1/SQRT(XX+YY)
      DD=2.0*3.141592654/SQRT(XX+YY)
      WRITE(JS,101)DD,ZZ
      WRITE(JF,101)DD,ZZ
   45 I=I+2
   50 I=I+1
      IF(I.GT.NP)RETURN
      GOTO 10
      END
c
      SUBROUTINE SEEK1(R13,R23,SC,X1in,X2in,Y1in,I7)
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,IDF,IL,JS,JDF,JF,JL,JP
c==== 8/2/2 solve core/shell/shell constraint iteratively,  RKH
c==== R1/R3 ratio    to be found given X1,X2,Y1,R3
c==== R2/R3 ratio    "  "    "     "    "  "  "  "
c==== (vol dry shell)/(vol core)
c==== X1 solvent fraction at R1
c==== X2 solvent fraction at R2
c==== Y1 fraction of polymer in first shell
c==== R1 (N.B R2 & R3 assumed at +2 and +4 from R1 )
c==== N.B this constraint ties TWO parameters at once !
c==== 4*pi/3.0
      fpi3=4.188790205
      b=V(I7+4)
      LTYP1=LTYP(I7+1)
      LTYP2=LTYP(I7+3)
c
c==== force physical values (see also constr 24 in SUBROUTINE CONDER )
      x1=amin1(x1in,0.999)
      x1=amax1(x1,0.0)
      x2=amin1(x2in,0.999)
      x2=amax1(x2,0.0)
      y1=amin1(y1in,1.0)
      y1=amax1(y1,0.0)
c
c      if(x2.gt.0.999)write(js,215)sc,x1,x2,y1
c215   format(1x,'Error ? constr 24',1p4e11.4)
      y2 = 1.0 - y1
c==== solve uniform case first
      aa=1.0 + (sc*y1/(1.0 - x1)) + ((sc*y2)/(1.0 - x2)) 
c
      if(aa.gt.0.0)then
      r13=aa**(-1./3.)
      else
      r13=0.8
      r23=0.9
      write(js,217)sc,x1,x2,y1
217   format(1x,'Error constr 24 r13, set 0.8 & 0.9 sc,x1,x2,y1=',
     >   1p4e10.3)
      return
      end if
c
      aa= (1. + sc*y1/(1.0 - x1) ) /
     >      ( 1. + (sc*y1)/(1. - x1) +(sc*y2)/(1. - x2) )
      if(aa.gt.0.0)then
      r23=aa**(1./3.)
      else
      r13=0.8
      r23=0.9
      write(js,218)sc,x1,x2,y1
218   format(1x,'Error constr 24 r23, set 0.8 & 0.9 sc,x1,x2,y1=',
     >   1p4e10.3)
      return
      end if
c====  skip out now if have exact solution already
      if(ltyp1.eq.1.and.ltyp2.eq.1)return
c
      d=r13*b
      e=r23*b
      e=b-e
      d=b-e-d
      dd=amax1(1.0,0.20*d)
      ee=amax1(1.0,0.20*e)
c
      if((ltyp1.ne.11.and.ltyp1.ne.1).or.
     >   (ltyp2.ne.1.and.ltyp2.ne.11.and.ltyp2.ne.21.and.ltyp2.ne.31)) then
      write(JS,219)ltyp1,ltyp2
219   format(1x,'ERROR constr 24 not coded for shells LTYP=',2i5)
      goto 300
      end if
c
      i=0
c
200   i=i+1
      if(ee.lt.amax1(0.001*e,0.002).and.
     >   dd.lt.amax1(0.001*d,0.002))goto 300
      if(i.gt.256)goto 300
      r1=b-d-e
      vcore=fpi3*r1**3
      if(ltyp1.eq.11)then
      if(x1.le.x2)then
c==== decreasing concentration of polymer (increasing solvent)
c==== 29/10/02 oops found had 3*d**2 here
      Vunder=(1.0-x2)*fpi3*((b-e)**3-(b-d-e)**3)
      s1pcheck=((1.-x1)-(1.-x2))*0.25*fpi3*
     >   (6.*r1**2+4.*r1*d+ d**2)*d +  Vunder
      else
c==== increasing conc
      Vunder=(1.0-x1)*fpi3*((b-e)**3-(b-d-e)**3)
      s1pcheck=((1.-x2)-(1.-x1))*0.25*fpi3*
     >        (6.*r1**2+8.*r1*d+3.*d**2)*d +    Vunder

      end if
      else if (ltyp1.eq.1)then
      s1pcheck=(1.0-x1)*fpi3*((b-e)**3-(b-d-e)**3)
      end if
c
      r2=b-e
      if(ltyp2.eq.11)then
c==== fuzzy second shell always decreases to conc=0.0
c==== 29/10/02 oops found had 3.*e**2 
      s2pcheck=(1.0 - x2)*0.25*fpi3*(6.*r2**2+4.*r2*e+   e**2)*e
      else if(ltyp2.eq.21)then
c==== exponential downhill to infinity
      rL=e/3.5
      s2pcheck=(1.0-x2)*3.0*fpi3*rL*(r2**2 +2.*r2*rL +2.*rL**2)
      else if(ltyp2.eq.31)then
c==== exponential downhill, truncated
      rL=e/3.5
      s2pcheck=(1.0-x2)*3.0*fpi3*rL*(r2**2 +2.*r2*rL +2.*rL**2 -
     >  exp(-e/rL)*(b**2 +2.*b*rL +2.*rL**2)  )
      else
      s2pcheck=(1.0 - x2)*fpi3*(b**3-r2**3)
      end if
c
      y1check=s1pcheck/(sc*vcore)
      y2check=s2pcheck/(sc*vcore)
c=====
C      write(6,102)i,d,e,y1check,y2check
C102   format(1x,i4,'  d, e, y1, y2 =',1p4e14.5)
c
      if(abs(y1-y1check).lt.2.e-04.and.abs(y2-y2check).lt.1.e-04)goto 300

      if(y2check.lt.y2)then
      e=e+ee
      else 
c==== avoid repetitive loops by using diff rescsle here
      if(y1check.ge.y1)ee=ee*0.5
      e=e-ee*0.977
      if(e.lt.0.0)e=0.0
      end if
c
      if(y1check.lt.y1)then
      d=d+dd
      else 
      if(y2check.ge.y2)dd=dd*0.5
      d=d-dd
      if(d.lt.0.0)d=0.0
      end if
c
      goto 200
c
300   r13=(b-d-e)/b
      r23=(b-e)/b
c
      return
      end
