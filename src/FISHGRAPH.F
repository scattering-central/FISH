C==== Dummy old-style graphics routines
C==== Easier to do this than to remove all references!
      SUBROUTINE LOCDEV(I,IDEV,IERR)
      RETURN
      END
      SUBROUTINE LOCAXIS(IDEV,IERR,XN,XX,YN,YX,IG1,IG2,ILOG)
      RETURN
      END
      SUBROUTINE LOCGRAPH(X,Y,E,N,ILTYP,LSYM,ILAST,IDEV,IEB)
    6 RETURN
      END
c====
      SUBROUTINE LOCLINone(X1,X2,Y1,Y2)
      RETURN
      END
c====
      SUBROUTINE LOCLIN(X1,X2,Y1,Y2)
      RETURN
      END
C
      subroutine textbox(II)
      RETURN
      END

c     Output constraints for use no plot
      SUBROUTINE CONOP2(N,TEXT,TEXT2)
C==== these data analysis programs are for use only by authorised persons
C==== Copyright R.K.Heenan 18/1/93 version for graphics window
C==== written by R.K.Heenan at UKC and RAL 1983-1986
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV)
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*80 TEXT,TEXT2
      M=N
      ISQUASH=1
      L=N
      IM=NCON(L)
C
c      IF(ISQUASH.EQ.0)THEN
c      WRITE(TEXT,11)L,IM,NUSE(L)
c      CALL PTXT(TEXT)
c      END IF
C
      WRITE(TEXT,999)
      WRITE(TEXT2,999)
  999 FORMAT(1X,'                                               ',
     * '                               ')
      IF(NUSE(L).EQ.0)GOTO 1001
   11 FORMAT(1X,'CONSTRAINT',I3,' TYPE',I3,' USE=',I2)
      GOTO(100,200,300,400,500,600,700,800,900,1010,1100,1200,1300,
     >  1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,
     >  2400,2500),IM
      GOTO 1001
  100 WRITE(TEXT,101)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),PC(3,L)
  101 FORMAT(1X,'V(',I2,')=',F7.3,'*V(',I2,') +',F7.3,
     * '*V(',I2,') +',F7.3)
      GOTO 1000
  200 WRITE(TEXT,201)IMOD(1,L),IMOD(2,L),IMOD(3,L)
  201 FORMAT(1X,'V(',I2,')=V(',I2,')*V(',I2,')' )
      GOTO 1000
  300 WRITE(TEXT,301)IMOD(1,L),IMOD(2,L),IMOD(3,L)
  301 FORMAT(1X,'V(',I2,')=2.0* V(',I2,')*V(',I2,')',
     *' R2-R1=2*L *RM' )
      GOTO 1000
  400 WRITE(TEXT,401)IMOD(1,L),IMOD(2,L),IMOD(3,L)
  401 FORMAT(1X,'V(',I2,') = (1.0-V(',I2,'))*V(',I2,')',
     *' R1=(1-L)*RM' )
      GOTO 1000
  500 WRITE(TEXT,501)IMOD(1,L),PC(1,L),IMOD(2,L),IMOD(3,L),PC(2,L),PC(3,L),
     *  IMOD(4,L),PC(4,L)
  501 FORMAT(1X,'V(',I2,')=',F7.3,'*V(',I2,')*V(',I2,')**',F7.3,'+',F7.3,
     *'*V(',I2,')+',F7.3 ) 
      goto 1000
  600 WRITE(TEXT,601)IMOD(1,L),PC(1,L),PC(2,L),IMOD(2,l),
     *  IMOD(3,L),PC(3,L),PC(4,L)
  601 FORMAT(1X,'V(',I2,')=(',F7.3,'+',F7.3,'*V(',I2,')*V(',
     *   I2,')**',F7.3,')**',F7.3) 
      goto 1000
  700 WRITE(TEXT,701)IMOD(1,L),PC(1,L),PC(2,L),IMOD(2,L),IMOD(3,L),
     *   PC(3,L),IMOD(4,L),PC(4,L)
  701 FORMAT(1X,'V(',I2,')=((',F7.3,'+',F7.3,'*V(',I2,'))*V(',I2,
     *   ')+',F7.3,'*V(',I2,'))**',F7.3)
      GOTO 1000
  800 WRITE(TEXT,801)IMOD(1,L),IMOD(2,L),PC(1,L),imod(3,l),PC(2,L),
     *  IMOD(4,L),PC(3,L),PC(4,L)
  801 FORMAT(1X,'V(',I2,')=V(',I2,')*(',F7.3,'*V(',I2,')+',F7.3,')+V(',
     *   I2,')*',F7.3,'+',F7.3) 
      goto 1000
  900 WRITE(TEXT,901)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),PC(3,L),PC(4,L)
  901 FORMAT(1X,'V(',I2,')=',F7.3,'*(V(',I2,')*',F7.3,'+',F7.3,')**',
     *   F7.3) 
      GOTO 1000
 1010 WRITE(TEXT,1011)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),
     *  PC(3,L),IMOD(4,L),PC(4,L)
 1011 FORMAT(1X,'V(',I2,')=(',F7.3,'*(V(',I2,')+',F7.3,'*V(',I2,'))**',  
     *   F7.3,'*V(',I2,'))**',F7.3)
      GOTO 1000
 1100 WRITE(TEXT,1101)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),PC(3,L),
     * IMOD(4,L),PC(4,L),IMOD(5,L),PC(5,L)
 1101 FORMAT(1X,'V(',I2,')=',F7.2,'*(',I2,')+',F7.2,
     * '*(',I2,')+',F7.2,'*(',I2,')+',F7.2,'*(',I2,')+',F7.2)
      GOTO 1000
 1200 WRITE(TEXT,1201)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L)
 1201 FORMAT(1X,'V(',I2,')=(1-V(',I2,'))*(V(',I2,')-',
     * 'V(',I2,') )')
      GOTO 1000
 1300 WRITE(TEXT,1301)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(2,L),IMOD(6,L),IMOD(3,L),IMOD(7,L)
 1301 FORMAT(1X,'V(',I2,')=V(',I2,')-V(',I2,')+',
     * 'V(',I2,')*(V(',I2,')-V(',I2,'))+',
     * 'V(',I2,')*(V(',I2,')-V(',I2,'))')
      GOTO 1000
 1400 WRITE(TEXT,1401)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L)
 1401 FORMAT(1X,'Shear rod BIGGAMMA V(',I3,' )=Fun[ R,L=V(',I3,
     > ',+1),G=V(',I3,'),eta/T=V(',I3,') ]')
      GOTO 1000
 1500 WRITE(TEXT,1501)IMOD(1,L),IMOD(2,L),IMOD(3,L)
 1501 FORMAT(1X,'Shl V(',I3,')=Fn[s/c=V(',I3,
     > '),f=V(',I3,') ]')
      GOTO 1000
 1600 WRITE(TEXT,1601)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L)
 1601 FORMAT(1X,'ctr V(',I3,')=Fun[s/c=V(',I3,
     > '),f=V(',I3,'),c=V(',I3,'),s=V(',I3,'),',
     > 'sv=V(',I3,')]')
      GOTO 1000
 1700 WRITE(TEXT,1701)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L),IMOD(7,L)
 1701 FORMAT(1X,'V(',I3,')=Fn[scalA=V(',I3,
     > '),f=V(',I3,'),shA=V(',I3,'),solA=V(',I3,'),',
     > 'shB=V(',I3,'),solB=V(',I3,')]')
      GOTO 1000
 1800 WRITE(TEXT,1801)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L)
 1801 FORMAT(1X,'contrast V(',I3,')=Fun[f=V(',I3,'),core=V(',
     >  I3,'),shell=V(',I3,'),solv=V(',I3,')]')
      GOTO 1000
 1900 WRITE(TEXT,1901)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),IMOD(3,L),PC(3,L),
     * IMOD(4,L)
 1901 FORMAT(1X,'V(',I2,')=',F7.3,'*V(',I2,')+',F7.3,
     * '*V(',I2,')+',F7.3,'*V(',I2,')+')
c      CALL PTXT(TEXT)
      WRITE(TEXT2,1902)PC(4,L),IMOD(5,L),PC(5,L),IMOD(6,L),PC(6,L),
     >   IMOD(7,L),PC(7,L),IMOD(8,L)
 1902 FORMAT(1X,G8.1,'*V',I2,')+',G8.1,'*V',I2,')+',G8.1,
     >   '*V',I2,')+',G8.1,'*V',I2,')')
      GOTO 1000
 2000 WRITE(TEXT,2001)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L)
 2001 FORMAT(1X,'V(',I3,')=F[R1=V(',I3,'),i=V(',
     >  I3,'),j=V(',I3,'),RH1-n=V(',I3,'),h=V(',i3,')]')
      GOTO 1000
 2100 WRITE(TEXT,2101)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L)
 2101 FORMAT(1X,'V(',I3,')=F[ n/a V(',I3,
     > '),f=V(',I3,'),cor=V(',I3,'),shl=V(',I3,'),',
     > 'sol=V(',I3,') ]')
      GOTO 1000
c==== 4/12/00   V(I1)=P1*V(I2)/ ( P2 + P3*V(I3) )
 2200 WRITE(TEXT,2201)IMOD(1,L),PC(1,L),IMOD(2,L),PC(2,L),PC(3,L),
     >  IMOD(3,L)
 2201 FORMAT(1X,'V(',I3,')=',F10.5,'*V(',I3,')/[',F10.5,
     * ' +',F10.5,'*V(',I3,')]')
      GOTO 1000
 2300 WRITE(TEXT,2301)(IMOD(K,L),K=1,8),(PC(K,L),K=1,4)
 2301 FORMAT(1X,'V(',I3,')=F[Po=',I3,
     > ',Pd=',I3,',c=',I3,',s=',I3,
     > ',o=',I3,',d=',i3,',t=',i3,',phi=',4f5.2,']')
      GOTO 1000
c
 2400 WRITE(TEXT,2401)(IMOD(K,L),K=1,7)
 2401 FORMAT(1X,'R13 V(',I3,') & R23 V(',I3,
     > ')=fn[ Vsc=V(',I3,'),X1=V(',I3,'),X2=V(',I3,'),',
     > 'Y1=V(',I3,'),N(R1)=',i3,']')
      GOTO 1000
 2500 WRITE(TEXT,2501)IMOD(1,L),IMOD(2,L),IMOD(3,L),IMOD(4,L),
     *  IMOD(5,L),IMOD(6,L),IMOD(7,L),IMOD(8,L),IMOD(9,L),IMOD(10,L)
 2501 FORMAT(1X,'V(',I3,')= {(',I3,')-(',I3,')}(',I3,')+',
     *'{(',I3,')-(',I3,')}(',I3,')+{(',I3,')-(',I3,')}(',I3,')')
 1000 CONTINUE
 1001 CONTINUE
      RETURN
      END