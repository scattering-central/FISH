	subroutine ssqfnset(ierr)
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
C==== SET UP HAYTER-PENFOLD, RESCALED, CHARGED PARTICLE INTERACTION
C==== CF & RKH 7/2/89, with thanks to H&P for some of their routines
C==== machine dep const in subroutine
C==== checked with JP 28/1/91, their potn should head to Hard sphere
C==== as charge tends to zero ( is exact, though numerical solution
C==== is not guaranteed).  Negative potentil to be allowed for
C==== by reversing sign of GEK (the potn) in SSQSET2(), but beware
C==== can easily get nonsense parameters.  It may be better, especially
C==== for larger particles to use Ornstein-Zernicke ( critical scatter)
C==== plus hard sphere for S(Q) - see e.g. Magid et.al. J.Phys.Chem. 
C==== 88(  )5730
	DIMENSION qqq(1024)
      REAL*8 dcoeff
      REAL coeff
	common /sqhpc/ dcoeff(4,5),coeff(6,5)
      INCLUDE 'FISHDIM.PAR'
      COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED,
     >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV),
     >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24)
      COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
c==== 26/1/99 F90 complier objects to common block of same name
C==== as subroutine, change to GOCM1 !
	COMMON /GOCM1/ IIIFLAG
c==== look for model 23 or 25, interacting charged particles S(Q)
	do 200 ii=1,np
	if(lm(ii).eq.23.or.lm(ii).eq.25)goto 210
 200	continue
	IERR=1
	return
c====  set up shift to use in derivative calculation	ues DEL(I) in DELSET
c 210 	ssqshift(1)=0.01
c 	ssqshift(2)=0.05
c 	ssqshift(3)=0.005
c 	ssqshift(4)=0.002
 210	IIIFLAG=LTYP(II)
c====   set up a q value array -used to find valid solutions for S(Q) parameters
	ddq=3.141592654/16.0
	do 100 i=1,1024
	qqq(i)=(i-1)*ddq
 100	continue
c====   transform 4 input parameters in v() into common block for calc
	jj=1
	call ssqset2(v(ii))
      	CALL sqhpa(qqq,1024,IERR)     
		IF(IERR.LE.0)then
		write(js,1001)ierr,jj
1001	format(/,1x,' ERROR in SPHQA  ierr=',i8,'   jj=',i3,/)
		return
	endif
c====  copy the result into storage for use later in CALCUL and DERIV
	CALL ssqset3(dcoeff(1,jj),coeff(1,jj))
c==== do the same for any refining parameters in model 23
	do 300 i=ii,ii+3
	jj=jj+1
	if(abs(ps(i)).lt.0.01)goto300
	v(i)=v(i)+ del(i)
	call ssqset2(v(ii))
      	CALL sqhpa(qqq,1024,IERR)     
	v(i)=v(i)- del(i)
		IF(IERR.LE.0)then
		write(js,1001)ierr,i
		return
		endif
	CALL ssqset3(dcoeff(1,jj),coeff(1,jj))
 300	continue
	return
	end
c
	subroutine ssqset2(v)
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
c=== copy and rescale parameters from FISH into common block
      	REAL*8 A,B,C,F                                                    
	COMMON /SQHPB/ A,B,C,F,ETA,GEK,AK,U,vv,GAMK,SETA,SGEK,SAK,SCAL,G1,sig
	COMMON /GOCM1/ IIIFLAG
	dimension v(4)
c====    v(1) =rad  v(2)=qaz  v(3)=akk  v(4)=eta
	sig=v(1)*2.
	ak=v(3)*sig
	eta=v(4)
	QAZ=V(2)
C    IIIFLAG=1 ORIGINAL HAYTER PENFOLD S(Q) WITH PENETRATING BACKGROUND
C==== 28/1/91 treat as attractive if charge is NEGATIVE
C    IIIFLAG=11 GOCM MODEL
	IF(IIIFLAG.EQ.1)THEN
	gek=SIGN( 7.119*v(2)**2/(sig*(1+ak/2)**2), QAZ)
	ELSE
	call gocm(sig,eta,qaz,ak,xx2)
	gek=7.122*qaz**2/sig*xx2*exp(-ak)
	ENDIF
	return
	end
c
	subroutine ssqset3(dcf,cf)
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
c==== copy output parameters from common block into storage for later use
C==== in DERIV and CALCUL routines
      	REAL*8 A,B,C,F
	COMMON /SQHPB/ A,B,C,F,ETA,GEK,AK,U,vv,GAMK,SETA,SGEK,SAK,SCAL,G1,sig
	real*8 dcf(4)
	dimension cf(6)
	dcf(1)=a
	dcf(2)=b
	dcf(3)=c
	dcf(4)=f
	cf(1)=sak
	cf(2)=seta
	cf(3)=sgek
	cf(4)=scal
	cf(5)=sig
	return 
	end
c
	function sofq(qq,dcf,cf)
C==== this routine adapted from one of J.Penfold
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
      	IMPLICIT REAL*8 (A-H,O-Z)                                         
      	REAL*4 QQQ,SOFQ,ETAZ,GEKZ,AKZ,U,vV,GAMK,SETA,SGEK,SAK,SCAL,G1
	real*8 dcf(4)
	real*4 qq,cf(6)
	A=DCF(1)
	B=DCF(2)
	C=DCF(3)
	F=DCF(4)
	SAK=CF(1)
	SETA=CF(2)
	SGEK=CF(3)
	SCAL=CF(4)
c=== sig is not used anywhere else in the calculation, except to scale Q here
	qqq=qq*cf(5)
c
      	ETA=SETA                                                          
      	AK=SAK                                                            
      	GEK=SGEK                                                          
      	E24=24.0D0*ETA                                                    
      	X1=DEXP(AK)                                                       
      	X2=0.0D0                                                          
      	IF(SAK.LT.20.0)X2=DEXP(-AK)                                       
      	CK=0.5D0*(X1+X2)                                                  
      	SK=0.5D0*(X1-X2)                                                  
      	AK2=AK*AK
      	IF(qqq.LE.0.0)GOTO 900
      	QK=qqq/SCAL                                                     
      	Q2K=QK*QK                                                         
      	QK2=1.0D0/Q2K                                                     
      	QK3=QK2/QK                                                        
      	QQK=1.0D0/(QK*(Q2K+AK2))                                          
      	SINK=DSIN(QK)                                                     
      	COSK=DCOS(QK)                                                     
      	ASINK=AK*SINK                                                     
      	QCOSK=QK*COSK                                                     
      	AQK=(A*(SINK-QCOSK)                                               
     1	+B*((2.0D0*QK2-1.0D0)*QCOSK+2.0D0*SINK-2.0D0/QK)                  
     2	+0.5D0*ETA*A*(24.0D0*QK3+4.0D0*(1.0D0-6.0D0*QK2)*SINK             
     3	-(1.0D0-12.0D0*QK2+24.0D0*QK2*QK2)*QCOSK))*QK3                    
     4	+C*(CK*ASINK-SK*QCOSK)*QQK                                        
     5	+F*(SK*ASINK-QK*(CK*COSK-1.0D0))*QQK                              
     6	+F*(COSK-1.0D0)*QK2                                               
     7	-GEK*(ASINK+QCOSK)*QQK                                            
      	SOFQ=1.0D0/(1.0D0-E24*AQK)                                       
      	GOTO 200                                                          
 900 	SOFQ=-1.0D0/A                                                    
 200 	CONTINUE                                                          
	return
	end
c
c
      	SUBROUTINE SQHPA(QQ,NPT,IERR)                              
C==== this routine adapted from one of J.Penfold
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
      	DIMENSION QQ(1)
      	REAL*8 A,B,C,F
      	COMMON /SQHPB/ A,B,C,F,ETA,GEK,AK,U,vv,GAMK,SETA,SGEK,SAK,SCAL,G1,SIG
      	IERR=0
      	S=ETA**0.3333333
	AAA=AK*(1.-1./S)
C=== AVOID FLOATING UNDERFLOW - MACHINE DEP CONSTANT
      	IF(AAA.LT.-84.)THEN
	GAMK=1.E-37
	ELSE
	GAMK=2.0*S*GEK*EXP(AAA)
	ENDIF
C
 100 	IERR=0
      	CALL SQCOEF(IERR)
      	RETURN
      	END
c
      	SUBROUTINE SQCOEF(IR)
C==== this routine adapted from one of J.Penfold
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
      	REAL*8 A,B,C,F
      	COMMON /SQHPB/ A,B,C,F,ETA,GEK,AK,U,vV,GAMK,SETA,SGEK,SAK,SCAL,G1,SIG
	COMMON /GOCM1/ IIIFLAG
      	DATA ITM/40/,ACC/5.0E-6/
      	IG=1
      	IF(AK.LT.(1.0+8.0*ETA))GOTO 50
      	IG=0
      	CALL SQFUN(G1,ETA,1,IR)
      	IF(IR.LT.0.OR.G1.GE.0.0)RETURN
 50 	SETA=AMIN1(ETA,0.20)
      	IF(IG.EQ.1.AND.GAMK.LT.0.1)GOTO 400
      	J=0
 100 	J=J+1                                                             
      	IF(J.GT.ITM)GOTO 200                                              
      	IF(SETA.LE.0.0)SETA=ETA/J                                         
      	IF(SETA.GT.0.6)SETA=0.35/J                                        
      	E1=SETA                                                           
      	CALL SQFUN(F1,E1,2,IR)                                            
      	E2=SETA*1.01                                                      
      	CALL SQFUN(F2,E2,2,IR)                                            
      	E2=E1-(E2-E1)*F1/(F2-F1)                                          
      	SETA=E2                                                           
      	DEL=ABS((E2-E1)/E1)                                               
      	IF(DEL.GT.ACC)GOTO 100                                            
        CALL SQFUN(G1,E2,4,IR)                                            
      	IR=J                                                              
      	IF(IG.EQ.1)GOTO 300                                               
      	RETURN                                                            
 200 	IR=-1                                                             
      	RETURN                                                            
 300 	IF(SETA.GE.ETA)RETURN                                             
 400 	CALL SQFUN(G1,ETA,3,IR)                                           
      	IF(IR.LT.0.OR.G1.GE.0.0)RETURN                                    
      	IR=-3                                                             
      	RETURN                                                            
      	END                                                               
c
      	SUBROUTINE SQFUN(FVAL,EVAR,IX,IR)                                 
C==== this routine adapted from one of J.Penfold
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
      	IMPLICIT REAL*8 (A-H,O-Z)                                         
        COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP
      	REAL*4 RETA,RGEK,RAK,U,vV,EVAR,FVAL,GAMK,                          
     1 	SETA,SGEK,SAK,SCAL,G1,SIG
      	COMMON /SQHPB/ A,B,C,F,RETA,RGEK,RAK,U,vV,GAMK,SETA,SGEK,SAK,SCAL,
     1	G1,SIG
	COMMON /GOCM1/ IIIFLAG
      	DATA ACC/1.0D-6/,ITM/40/                                          
      	ETA=EVAR                                                          
      	ETA2=ETA*ETA                                                      
      	ETA3=ETA2*ETA                                                     
      	E12=12.0D0*ETA                                                    
      	E24=E12+E12                                                       
      	SCAL=(RETA/EVAR)**0.33333333                                      
C    IIIFLAG=1 ORIGINAL HAYTER-PENFOLD S(Q) WITH PENETRATING BACKGROUND
C    IIIFLAG=11 GOCM
	IF(IIIFLAG.EQ.1)THEN
      	SAK=RAK/SCAL-2.0*ALOG(1.0-EVAR)*EVAR**0.3333333                   
      	SGEK=RGEK*SCAL*EXP(RAK-SAK)/(1.0-EVAR)**2                         
	ELSE
      	SAK=RAK/SCAL
      	SGEK=RGEK*SCAL*EXP(RAK-SAK)
	ENDIF
      	GEK=SGEK                                                          
      	AK=SAK                                                            
      	AK2=AK*AK                                                         
      	AK1=1.0D0+AK                                                      
      	DAK2=1.0D0/AK2                                                    
      	DAK4=DAK2*DAK2                                                    
      	D=1.0D0-ETA                                                       
      	D2=D*D                                                            
      	DAK=D/AK                                                          
      	DD2=1.0D0/D2                                                      
      	DD4=DD2*DD2                                                       
      	DD45=DD4*2.0D-1                                                   
      	ETA3D=3.0D0*ETA                                                   
      	ETA6D=ETA3D+ETA3D                                                 
      	ETA32=ETA3+ETA3                                                   
      	ETA2D=ETA+2.0D0                                                   
      	ETA2D2=ETA2D*ETA2D                                                
      	ETA21=2.0D0*ETA+1.0D0                                             
      	ETA22=ETA21*ETA21                                                 
      	AL1=-ETA21*DAK                                                    
      	AL2=(14.0D0*ETA2-4.0D0*ETA-1.0D0)*DAK2                            
      	AL3=36.0D0*ETA2*DAK4                                              
      	BE1=-(ETA2+7.0D0*ETA+1.0D0)*DAK                                   
      	BE2=9.0D0*ETA*(ETA2+4.0D0*ETA-2.0D0)*DAK2                         
     	BE3=12.0D0*ETA*(2.0D0*ETA2+8.0D0*ETA-1.0D0)*DAK4                  
     	VU1=-(ETA3+3.0D0*ETA2+45.0D0*ETA+5.0D0)*DAK                       
      	VU2=(ETA32+3.0D0*ETA2+42.0D0*ETA-2.0D1)*DAK2                      
      	VU3=(ETA32+3.0D1*ETA-5.0D0)*DAK4                                  
      	VU4=VU1+E24*AK*VU3                                                
      	VU5=ETA6D*(VU2+4.0D0*VU3)                                         
      	PH1=ETA6D/AK                                                      
      	PH2=D-E12*DAK2                                                    
      	TA1=(ETA+5.0D0)/(5.0D0*AK)                                        
      	TA2=ETA2D*DAK2                                                    
      	TA3=-E12*GEK*(TA1+TA2)                                            
      	TA4=ETA3D*AK2*(TA1*TA1-TA2*TA2)                                   
      	TA5=ETA3D*(ETA+8.0D0)*1.0D-1-2.0D0*ETA22*DAK2                     
      	EX1=DEXP(AK)                                                      
      	EX2=0.0D0                                                         
      	IF(SAK.LT.20.0)EX2=DEXP(-AK)                                      
      	SK=0.5D0*(EX1-EX2)                                                
      	CK=0.5D0*(EX1+EX2)                                                
      	CKMA=CK-1.0D0-AK*SK                                               
      	SKMA=SK-AK*CK                                                     
      	A1=(E24*GEK*(AL1+AL2+AK1*AL3)-ETA22)*DD4                          
      	A2=E24*(AL3*SKMA+AL2*SK-AL1*CK)*DD4                               
      	A3=E24*(ETA22*DAK2-0.5D0*D2+AL3*CKMA-AL1*SK+AL2*CK)*DD4           
      	B1=(1.5D0*ETA*ETA2D2-E12*GEK*(BE1+BE2+AK1*BE3))*DD4               
      	B2=E12*(-BE3*SKMA-BE2*SK+BE1*CK)*DD4                              
      	B3=E12*(0.5D0*D2*ETA2D-ETA3D*ETA2D2*DAK2                          
     1	-BE3*CKMA+BE1*SK-BE2*CK)*DD4                                      
      	V1=(ETA21*(ETA2-2.0D0*ETA+1.0D1)*2.5D-1                           
     1	-GEK*(VU4+VU5))*DD45                                              
      	V2=(VU4*CK-VU5*SK)*DD45                                           
      	V3=((ETA3-6.0D0*ETA2+5.0D0)*D                                     
     1	-ETA6D*(2.0D0*ETA3-3.0D0*ETA2+18.0D0*ETA+1.0D1)*DAK2              
     2	+E24*VU3+VU4*SK-VU5*CK)*DD45                                      
      	PP1=PH1*PH1                                                       
      	PP2=PH2*PH2                                                       
      	PP=PP1+PP2                                                        
      	P1P2=PH1*PH2*2.0D0                                                
      	P1=(GEK*(PP1+PP2-P1P2)-0.5D0*ETA2D)*DD2                           
      	P2=(PP*SK+P1P2*CK)*DD2                                            
      	P3=(PP*CK+P1P2*SK+PP1-PP2)*DD2                                    
      	T1=TA3+TA4*A1+TA5*B1                                              
      	T2=TA4*A2+TA5*B2+E12*(TA1*CK-TA2*SK)                              
      	T3=TA4*A3+TA5*B3+E12*(TA1*SK-TA2*(CK-1.0D0))                      
     1	-4.0D-1*ETA*(ETA+1.0D1)-1.0D0                                     
      	UM1=T2*A2-E12*V2*V2                                               
      	UM2=T1*A2+T2*A1-E24*V1*V2                                         
      	UM3=T2*A3+T3*A2-E24*V2*V3                                         
      	UM4=T1*A1-E12*V1*V1                                               
      	UM5=T1*A3+T3*A1-E24*V1*V3                                         
      	UM6=T3*A3-E12*V3*V3                                               
      	IF(IX.EQ.1)GOTO 100                                               
      	IF(IX.EQ.3)GOTO 100                                               
      	CA=AK2*P1+2.0D0*(B3*P1-B1*P3)                                     
     	CA=-CA/(AK2*P2+2.0D0*(B3*P2-B2*P3))                               
      	FA=-(P1+P2*CA)/P3                                                 
      	IF(IX.EQ.2)FVAL=UM1*CA*CA+(UM2+UM3*FA)*CA+UM4+UM5*FA+UM6*FA*FA    
      	IF(IX.EQ.4)FVAL=-(P1+P2*CA+P3*FA)                                 
 50 	F=FA                                                              
      	C=CA                                                              
      	B=B1+B2*CA+B3*FA                                                  
      	A=A1+A2*CA+A3*FA                                                  
      	vV=(V1+V2*CA+V3*FA)/A                                              
      	G24=E24*GEK*EX1                                                   
      	U=(AK*AK2*CA-G24)/(AK2*G24)                                       
      	RETURN                                                            
 100 	AL1=E12*P2*P2                                                     
      	AL2=E24*P1*P2-B2-B2                                               
      	AL3=E24*P2*P3                                                     
      	AL4=E12*P1*P1-B1-B1                                               
      	AL5=E24*P1*P3-B3-B3-AK2                                           
      	AL6=E12*P3*P3                                                     
      	W16=UM1*AL6-AL1*UM6                                               
      	W15=UM1*AL5-AL1*UM5                                               
      	W14=UM1*AL4-AL1*UM4                                               
      	W13=UM1*AL3-AL1*UM3                                               
      	W12=UM1*AL2-AL1*UM2                                               
      	W26=UM2*AL6-AL2*UM6                                               
      	W25=UM2*AL5-AL2*UM5                                               
      	W24=UM2*AL4-AL2*UM4                                               
      	W36=UM3*AL6-AL3*UM6                                               
      	W35=UM3*AL5-AL3*UM5                                               
      	W34=UM3*AL4-AL3*UM4                                               
      	W32=UM3*AL2-AL3*UM2                                               
      	W46=UM4*AL6-AL4*UM6                                               
      	W56=UM5*AL6-AL5*UM6                                               
      	W3526=W35+W26                                                     
      	W3425=W34+W25                                                     
      	W4=W16*W16-W13*W36                                                
      	W3=2.0D0*W16*W15-W13*W3526-W12*W36                                
      	W2=W15*W15+2.0D0*W16*W14-W13*W3425-W12*W3526                      
      	W1=2.0D0*W15*W14-W13*W24-W12*W3425                                
      	W0=W14*W14-W12*W24                                                
      	IF(IX.EQ.1)GOTO 200                                               
      	G1=0.5D0*ETA2D*DD2*DEXP(-GEK)                                     
      	IF(SGEK.GT.2.0.OR.SAK.GT.1.0)GOTO 150                             
      	E24G=E24*GEK*DEXP(AK)
c==== RKH 1/11/01 fed up with sqrt(-ve) crashes here
      	if(e24g.gt.0.0)then
        PWK=DSQRT(E24G)
        else
        pwk=0.0
        write(js,1491)
1491    format(1x,'SQFUN  ERROR ???    E24G < 0.0')
        end if
      	QPW=(1.0D0-DSQRT(1.0D0+2.0D0*D2*D*PWK/ETA22))*ETA21/D             
      	G1=-QPW*QPW/E24+0.5D0*ETA2D*DD2                                   
 150 	PG=P1+G1                                                          
      	CA=AK2*PG+2.0D0*(B3*PG-B1*P3)+E12*G1*G1*P3                        
      	CA=-CA/(AK2*P2+2.0D0*(B3*P2-B2*P3))                               
      	FAP=-(PG+P2*CA)/P3                                                
      	GOTO 250                                                          
 200 	FAP=(W14-W34-W46)/(W12-W15+W35-W26+W56-W32)                       
 250 	I=0                                                               
 300 	I=I+1                                                             
      	IF(I.GT.ITM)GOTO 400                                              
      	FA=FAP                                                            
      	FUN=W0+(W1+(W2+(W3+W4*FA)*FA)*FA)*FA                              
      	FUND=W1+(2.0D0*W2+(3.0D0*W3+4.0D0*W4*FA)*FA)*FA                   
      	FAP=FA-FUN/FUND                                                   
      	IF(DABS((FAP-FA)/FA).GT.ACC)GOTO 300                              
      	IR=IR+I                                                           
      	GOTO 500                                                          
 400 	IR=-2                                                             
      	RETURN                                                            
 500 	FA=FAP                                                            
      	CA=-(W16*FA*FA+W15*FA+W14)/(W13*FA+W12)                           
      	G1=-(P1+P2*CA+P3*FA)                                              
      	FVAL=G1                                                           
      	IF(ABS(FVAL).LT.1.0E-3)FVAL=0.0                                   
      	SETA=EVAR                                                         
      	GOTO 50                                                           
      	END                                                               
C
	subroutine gocm(sig,eta,qaz,ak,xx2)
C==== copyright C.Fagotti & R.K.Heenan, RAL, Febuary 1989
C   THIS FOR GOCM S(Q)
	REAL*4 A(5),ROOT(4)
	mm=3.0*eta/(1.0-eta)
	A(1)=4.0*(ak**2*(1+mm)**2+24.0*7.122*qaz*eta/sig)
	A(2)=4.0*ak**2*(1+mm)
	A(3)=ak**2-4.0*(1+mm)**2
	A(4)=-4.0*(1+mm)
	A(5)=-1.0
	CALL PA05A(A,ROOT,NREAL)
	do 10 i=1,nreal
	if(root(i).gt.0.0)goto20
 10 	continue
	write(6,1)
 1	format(' no positive root for Gam ')
	stop
 20	gg=root(i)
	gg=gg/2.0
	chi=(gg+mm)/(1.0+gg+mm)
	vv=8.0*mm/ak**3-2.0*chi/ak
	xx=cosh(ak/2.0)+vv*(ak/2.0*cosh(ak/2.0)-sinh(ak/2.0))
	xx2=xx*xx
 	return
	END
C
C   THIS FOR GOCM S(Q) CALCULATE ROOTS OF A QUARTIC
C#       PA05A          22/03/79 Harwell subroutine library
CNAME PA05A(R)                  CHECK
      SUBROUTINE PA05A(A,ROOT,NREAL)
C STANDARD FORTRAN 66 (A VERIFIED PFORT SUBROUTINE)
      DIMENSION A(5),ROOT(4),B(5),ROOT1(3),D(4)
      NREAL=0
      B(1)=1.0
      B(2)=A(4)/A(5)
      B(3)=A(3)/A(5)
      B(4)=A(2)/A(5)
      B(5)=A(1)/A(5)
      D(4)=1.0
      D(3)=-B(3)
      D(2)=B(2)*B(4)-4.0*B(5)
      D(1)=B(5)*(4.0*B(3)-B(2)*B(2))-B(4)*B(4)
      CALL PA03A(D,ROOT1,N)
      IF(N-1)78,78,79
   79 ROOT1(1)=ROOT1(3)
   78 X1=B(2)*B(2)*.25-B(3)+ROOT1(1)
      IF(X1)21,22,23
   22 XM=0.
      XN=SQRT(.25*ROOT1(1)*ROOT1(1)-B(5))
      GO TO 24
   23 XM=SQRT(X1)
      XN=(B(2)*ROOT1(1)-2.0*B(4))*.25/XM
   24 ALPHA=.5*B(2)*B(2)-ROOT1(1)-B(3)
      BETA=4.0 *XN-B(2)*XM
      GAMMA=ALPHA+BETA
      DELTA=ALPHA-BETA
      AA=-.5*B(2)
      J=1
      IF(GAMMA)81,82,82
   82 NREAL=NREAL+2
      J=0
      GAMMA=SQRT(GAMMA)
      GO TO 95
   81 GAMMA=SQRT(-GAMMA)
   95 IF(DELTA)83,84,84
   84 NREAL=NREAL+2
      DELTA=SQRT(DELTA)
      GO TO 96
   83 DELTA=SQRT(-DELTA)
   96 J=NREAL+J
      GO TO (90,91,92,93),J
   90 ROOT(1)=(AA+XM)*.5
      ROOT(2)=.5*GAMMA
   37 ROOT(3)=(AA-XM)*.5
      ROOT(4)=.5*DELTA
      GO TO 31
   91 ROOT(2)=(AA+XM+GAMMA)*.5
      ROOT(1)=(AA+XM-GAMMA)*.5
      GO TO 37
   92 ROOT(2)=(AA-XM+DELTA)*.5
      ROOT(1)=(AA-XM-DELTA)*.5
      ROOT(3)=(AA+XM)*.5
      ROOT(4)=.5*GAMMA
      GO TO 31
   93      BB=(AA+XM+GAMMA)*.5
           DD=(AA-XM+DELTA)*.5
           CC=(AA-XM-DELTA)*.5
           AA=(AA+XM-GAMMA)*.5
      IF(AA-CC)80,80,41
   80 ROOT(1)=AA
      IF(CC-BB)42,42,63
   42 ROOT(2)=CC
      IF(BB-DD)44,44,85
   44 ROOT(3)=BB
   89 ROOT(4)=DD
      GO TO 31
   85 ROOT(3)=DD
   88 ROOT(4)=BB
      GO TO31
   63 ROOT(2)=BB
      ROOT(3)=CC
      GO TO 89
   41 ROOT(1)=CC
      IF(DD-AA)86,86,87
   86 ROOT(2)=DD
      ROOT(3)=AA
      GO TO88
   87 ROOT(2)=AA
      IF(DD-BB)85,85,44
   21 XMD=SQRT(-X1)
      XND=(2.0*B(4)-B(2)*ROOT1(1))*.25/XMD
      ALPHA=.5*B(2)*B(2)-ROOT1(1)-B(3)
      BETA=4.0*XND-B(2)*XMD
      RO=SQRT(ALPHA*ALPHA+BETA*BETA)
      GAMMA=SQRT(.5*(ALPHA+RO))
      IF(GAMMA)71,72,71
   72  DELTA=SQRT(-ALPHA)
      GO TO 73
   71 DELTA=BETA*.5/GAMMA
   73 ROOT(1)=.5*(-.5*B(2)+GAMMA)
      ROOT(2)=.5*(XMD+DELTA)
      ROOT(3)=.5*(-.5*B(2)-GAMMA)
      ROOT(4)=.5*(XMD-DELTA)
   31 RETURN
      END
