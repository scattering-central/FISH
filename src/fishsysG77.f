c==== 20/10/99 there should be versions of this file:
c==== 1) for G77 ( Linux, or to test on PC)
c==== 2) for Digital Visual Fortran on PC ( the only place where readbin works)
c==== 3) for vms
c====  NOTE the VMS linker will run with unsatisfied references, but the
c====   G77 won't !
c
C==== 12/10/99 RKH remove "system" routines from FISH1,FISH4 & FISHGRAPH
c==== to make porting between vms, windows & linux easier
c==== find the comments PCPCPC to swap systems
c====
	SUBROUTINE OPENANY(I,IERR,STR)
	INTEGER I,IERR
	CHARACTER *(*) STR
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
	OPEN(UNIT=I,FILE=STR,STATUS='UNKNOWN',ERR=100)
      IERR=0
	RETURN
100   IERR=1
	WRITE(JS,1001)I,STR
1001  FORMAT(1X,'ERROR in openany, file on channel =',i4,/,1x,A)
      RETURN
	END
c====
c==== try using status='NEW' for log  files, in case of multiple use on VMS
	SUBROUTINE OPENNEW(I,IERR,STR)
	INTEGER I,IERR
	CHARACTER *(*) STR
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
c====	OPEN(UNIT=I,FILE=STR,STATUS='NEW',ERR=100)
c==== PCPCPC 
	OPEN(UNIT=I,FILE=STR,STATUS='UNKNOWN',ERR=100)
      IERR=0
	RETURN
100   IERR=1
	WRITE(JS,1001)I,STR
1001  FORMAT(1X,'ERROR in opennew, file on channel =',i4,/,1x,A)
      RETURN
	END
C
	SUBROUTINE OPENSHARE(I,IERR,STR)
	INTEGER I,IERR
	CHARACTER *13 STR
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
	OPEN(UNIT=I,FILE=STR,STATUS='OLD',ERR=100)
C====      OPEN(UNIT=I,FILE=STR,STATUS='OLD',SHARED,READONLY,ERR=100)
C==== PCPCPC shared & readonly are not in G77 
      IERR=0
	RETURN
100   IERR=1
	WRITE(JS,1001)I,STR
1001  FORMAT(1X,'ERROR in open(share), file on channel =',i4,/,1x,A)
      RETURN
	END
C
	SUBROUTINE CLOSEFILE(I)
	INTEGER I
	CLOSE(UNIT=I)
	RETURN
	END
C
      SUBROUTINE STARTSYS
C==== call here out of SETUP in FISH1
c==== PCPCPC  use DFLIB only in DVF
c      use DFLIB
c
      INCLUDE 'FISHDIM.PAR'
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
C==== 13/8/99 rewrite to avoid using system logical names
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
      IF(SYS.EQ.'WIN')THEN
c====  get the console window cursor actually on !
cc	J=DISPLAYCURSOR(1)
c	
c==== failed to get black on white console !
c	II2=GETTEXTCOLOR()
c	IF(II2.EQ.INT2(15))THEN
C====  if text is white (index 15) then swap BLACK & WHITE
C==== this still leave a black background !
c	II2=REMAPPALETTERGB(INT2(0),#FFFFFF)
c	II2=REMAPPALETTERGB(INT2(7),0)
c	II2=REMAPPALETTERGB(INT2(15),0)
C	write(js,'(i)')ii2
C	II2=SETTEXTCOLOR(II2)
C	II2=SETBKCOLOR(INT2(15))
c	END IF
      END IF
C
      RETURN
	END
C
      SUBROUTINE SYSTEMCALL(TEXT)
C==== 13/8/99 rewrite to avoid using system logical names
C====  this common needed for SYS !
      CHARACTER*3 SYS
      CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN
      COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS
	CHARACTER *(*)TEXT
      LOGICAL STATUS,SYSTEMQQ
	EXTERNAL SYSTEMQQ
	IF(SYS.EQ.'VAX')THEN
c==== PCPCPC put this in for VMS
c====      ISTATUS=LIB$SPAWN(TEXT)
      ELSE IF(SYS.EQ.'WIN')THEN
	STATUS=SYSTEMQQ(TEXT)
	ELSE IF(SYS.EQ.'LNX')THEN
	CALL SYSTEM(TEXT)
      END IF
	RETURN
	END
c====  11/10/99 RKH  - dummy routines for g77 to save having to
c====  remove yet more Dec Visual Fortran calls, as the linker is
c==== not happy with unsatisfied references
c==== PCPCPC comment out or change nnames of these 4 routines for DVF
c==== dummy DVF routine needed for G77(& VMS)
      integer function focusqq(i)
c	write(6,1001)
c1001  format(1x,'error ??? - call to dummy focusqq')
      focusqq=0
	return
	end
C==== PCPCPC
      function inqfocusqq(i)
c	write(6,1001)
c1001  format(1x,'error ??? - call to dummy inqfocusqq')
	inqfocusqq=0
	return
	end
C==== PCPCPC dummy DVF routine for G77 ( & VMS )
      integer function displaycursor(i)
c	write(6,1001)
c1001  format(1x,'error ??? - call to dummy displaycursor')
      displaycursor = 0
	return
	end
C==== PCPCPC dummy DVF routine for G77 ( & VMS)
c====        but then need a dummy called SYSTEM
      LOGICAL FUNCTION systemqq(text)
	 character *(*) text
c==== this should call the G77 "system", when using G77 compiler on WINDOWS
C====  (for which there is no screen driver ! )
      CALL SYSTEM(TEXT)
      SYSTEMQQ=.TRUE.
	return
	end
C
c==== PCPCPC dummy G77 routine system needed in DVF (and VMS), comment out for G77
c      subroutine system(text)
c	character *(*) text
c	write(6,1001)
c1001  format(1x,'error ??? - call to dummy routine system')
c	return
c	end
C
      LOGICAL FUNCTION syslogname(text1,len,text2)
	character *(*) text1, text2
C==== PCPCPC use this in VMS only
c====        syslogname = sys$trnlog(text1,len,text2,,,)
      syslogname=.false.
 	return
	end
C
 
c
C************************************************************************
C                     OTOKO FORMAT READ 
c==== PCPCPC          only works in DVF on PC
C**************************************************************************
      SUBROUTINE READBIN
      INCLUDE 'FISHDIM.PAR'
      INTEGER FILNUM1,FILNUM2,FILNUM3,NCHAN,INDIC(10)
      CHARACTER*4 FILET2
      CHARACTER*2 CFILNUM1,AFNAM(13)*1,FILET1*1
      CHARACTER*13 FILHEAD,FILDAT,FILOUT
      COMMON/ONE/NCH(MW,7),IC(MW,4),
     *LAB(3,MW),LAB2(20,3,MW),RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),
     *NDIM
      COMMON/CH/IS,ID,IL,JS,JD,JF,JL,JP
      CHARACTER*4 L,L2
      COMMON/WORK/L(20),L2(20),FMT(19),RJUNK(2400),FILL(57541)
500   CLOSE(UNIT=ID,STATUS='KEEP')
10    WRITE(JS,5)
5     FORMAT(/1X,'Enter header filename (X99999.XXX) or <ctrl-Z>: ',$)
      READ(IS,'(A13)',END=100)FILHEAD
c====      READ(IS,20,END=100)filet1,filnum1,filnum3,filnum2,filet2
c====      CFILNUM1=CHAR(filnum1)//CHAR(filnum3)
c====      FILHEAD=filet1//CFILNUM1//'000'//filet2
      WRITE(JS,501)FILHEAD
501   format(1X,' opening file ',A13)
      CALL OPENSHARE(ID,IERR,FILHEAD)
	IF(IERR.EQ.1)GOTO 10
      READ(ID,35)L
      READ(ID,35)L2
      WRITE(JS,36)L
      WRITE(JS,36)L2
      WRITE(JS,15)
15    FORMAT(1X,'SET NO.? ',$)
      READ(IS,17)I
17    FORMAT(I1)
      IF(I.EQ.0)GOTO 500
      DO J=1,20
      LAB2(J,1,I)=L(J)
      LAB2(J,2,I)=L2(J)
      END DO
      WRITE(JS,18)
18    FORMAT(1X,'LOCAL LABEL  (A11)=',$)
      READ(IS,22)(LAB(J,I),J=1,3)
22    FORMAT(3A4)
      READ(ID,40)(INDIC(J),J=1,10)
      READ(ID,30)(AFNAM(K),K=1,13)
30    FORMAT(80A1)
35    FORMAT(20A4)
36    FORMAT(1X,20A4)
40    FORMAT(10I8)
      NCHAN=INDIC(1)
      WRITE(JS,41)NCHAN,INDIC(2)
41    FORMAT(1X,'NCHAN=',I8,'  NBLOCKS=',I8)
50    CLOSE(UNIT=ID,STATUS='KEEP')
c====      FILDAT=filet1//CFILNUM1//'001'//filet2
      WRITE(JS,51)
51    FORMAT(/1X,'Enter data filename (X99999.XXX) ',$)
      READ(IS,'(A)',END=100)FILDAT
      WRITE(JS,52)
52    FORMAT(/1X,'Enter block number ',$)
C==== need to add block number in file 
      READ(IS,*)IBLOCK
      IF(IBLOCK.LE.0)IBLOCK=1
      IF(IBLOCK.GT.INDIC(2))IBLOCK=INDIC(2)
      WRITE(JS,521)FILDAT,IBLOCK
C==== PCPCPC convert='BIG_ENDIAN' only seem to work with DVF on PC
521   format(1X,'opening file',A13,'  block',i4)
      OPEN(UNIT=ID,FILE=FILDAT,ACCESS='DIRECT',ERR=50
c     >,CONVERT='BIG_ENDIAN',READONLY)
     >)
C==== PCPCPC check this out  can G77 read otoko files ???
c====    
C==== PCPCPC  in DVF use      
c      READ(ID'IBLOCK,ERR=150)(C(J,I),J=1,NCHAN)
c==== else try      
      READ(ID,REC=IBLOCK,ERR=150)(C(J,I),J=1,NCHAN)
      NSUM=0
      DO J=1,NCHAN
      NSUM=NSUM+C(J,I)
      E(J,I)=SQRT(C(J,I))
      Q(J,I)=FLOAT(J)
      END DO
      IF(NSUM.GT.1E8)NSUM=NSUM*1E-6
43    CLOSE(UNIT=ID,STATUS='KEEP')
      WRITE(JS,45)
45    FORMAT(/1X,'Enter ascii Q Filename (X99000.XXX)',
     >       ' (ctrl/Z to use chans)',$)
C====      READ(IS,20,END=100)filet1,filnum1,filnum3,filnum2,filet2
20    FORMAT(A1,I1,I1,I3,A4)
C====      CFILNUM1=CHAR(filnum1)//CHAR(filnum3)
C====      FILHEAD=filet1//CFILNUM1//'001'//filet2
      READ(IS,'(A)',END=100)FILHEAD
      write(js,501)FILHEAD
      OPEN(UNIT=ID,FILE=FILHEAD,ERR=43)
C====      OPEN(UNIT=ID,NAME=FILHEAD,TYPE='OLD',READONLY,ERR=43)
C==== PCPCPC no readonly in G77
C====      OPEN(UNIT=ID,NAME=FILHEAD,TYPE='OLD',ACCESS='DIRECT',READONLY,ERR=10)
      READ(ID,*,ERR=160,END=160)(Q(J,I),J=1,NCHAN)
      CLOSE(UNIT=ID,STATUS='KEEP')
      IDC=0
C====  IDC=1 is det corrected, =2 is binned
C====      IDC=2
C====      IF(FILET2.EQ.'.RED')IDC=1
      IC(I,1)=NSUM
      IC(I,2)=0
      IC(I,3)=0
      IC(I,4)=0
      NCH(I,1)=NCHAN
      NCH(I,2)=0
      NCH(I,3)=0
      NCH(I,4)=0
      NCH(I,5)=1
      NCH(I,6)=NCHAN
      NCH(I,7)=IDC
      GOTO 10
100   RETURN
150   write(js,159)
159   format(1x,'error reading data file',/)
      GOTO 50
160   write(js,161)
161   format(1x,'error or end reading Q file',/)
      GOTO 43
      END
c
c====  get day date and time
      subroutine TODAY(str)
      character*(*) str
	character*24 text
      str='    xx-xxx-xx xx:xx:xx  '
c====      123456789012345678901234
C==== PCPCPC these needed on VMS, does not give day of week !
c      call date(str(5:13))
c      call time(str(15:22))
c==== PCPCPC call to FDATE works in DVF and G77 ( i.e. Linux)
      call Fdate(text)
c==== this gives 'Fri 19-Jul-1996 11:12:00'
      str(1:3)=text(1:3)
	str(5:6)=text(9:10)
	str(8:10)=text(5:7)
	str(12:13)=text(23:24)
	str(15:22)=text(12:19)
      return
      end
c==== =============================================================
c====
      real*8 function d1mach(i)
c==== i=1 for underflow, i=2 for overflow, i=4 for epmach
C==== which is I assume the smallest number that can be added to unity
c==== these values found experimentally using MACHINE1 & MACHINE2.for 
c====                         by RKH 11/6/99
c==== they are all "safe" numbers, so could probably be tweaked a little
c
c==== on VAX ALPHA
c==== the double precision (real*8) values are  
c     2**-1020       2**1022     2**-53
c     1.od-291       4.494d+307  1.1d-16
c
c==== the single precision values are  
c     2**-127        2**126         2**-24   
c      1.0e-31       8.5e37         5.96e-08
c
c==== on WINNT 
c     2**-1074       2**1023     2**-52
c                   8.988d+307  2.220446d-16
c
c==== the single precision values are  
c     2**-149        2**127         2**-23   
c      1.4013e-45       1.701e38         1.192e-07
c
c==== the values below are a conservative compromise for both
c==== PC and VAX, note we are using double prec integration
c==== of single prec functions !!!
c
      goto(1,2,10,4),i
   10 write(6,1001)i
1001  format(1x,'ERROR: d1mach called with i=',i12)
      d1mach=0.0
      return
    1 d1mach=1.0d-291
      return
    2 d1mach=4.494d+307
      return
    4 d1mach=2.2d-16
      return
      end
