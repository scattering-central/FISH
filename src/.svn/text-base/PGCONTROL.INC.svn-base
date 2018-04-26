c==== 4/11/97  R.K.HEENAN, RAL,  pgcontrol.inc
c==== pass control parameters to all routines where they are
c==== needed, allows "defaults" to be set, and control from
c==== command files etc. as in COLETTE
c==== Set controls to  -(your default value)  to prompt user (where sensible)
c====
c==== IOVER=0     to clear screen 
c====       1     to overplot
C==== IRETURN=1   to return immediately to calling routine
c====        =2             after closing plot
C====        =3   to close plot only [XXXXXX need expt here with 2 devices etc. ]
C==== ICONTYPE    type of contour mapping ( set -ve to prompt user)
c====              =1 to 6 are factors of sqrt(2), 2, 3, 4,sqrt(10), 10
C====              =8 squares, =9 linear
C==== ICONRANGE=1  instead of actual data range use passed CONMIN & CONMAX
c====              CONT(J)=CONMIN, CONT(ICOL-J)=CONMAX where J=ICOL/(ICONT+1)
C==== ICONOVR=1   to plot contours on top of the colour map
c==== IHIST=0     for point mode data
C====      =1     for histogram mode ( this may be used as per X(NX+IHIST)
c====             but since we ASSUME all data equally spaced X(NX)+(X(NX)-X(NX-1))  
c====             is safer, in case X(NX+1) has not been set !
C==== ITRANS=0    use default  (X,Y) to pixel (I,J) mapping
C====       =1     use mapping supplied in TRPG(1-6)  - see routine INIT
C====
C==== IPXL=0     use linear interpolation for colour plots
c====      1     colour histogram cells 
C==== INUMB=0    no numbers on scale box
C====       1    add numbers to scale box
C==== IRANGE=0   use X,Y, ranges of data supplied
c====        1   use XMINPG, XMAXPG etc - enables L/X, L/Y or ZOOM, or rotations via ITRANS
C==== IFIRSTPG=1   used internally bu COLETTE to flag first call to PGPLOT
C==== IHEADPG=1  used by COLETTE to add header box at top of plot
c==== IWIDTH     line thickness, try 2
c==== SIZEPG       text size, try 1.2
C==== PGSCR01    19/8/98 default R,G,B for colours 0 (background) & 1 (foreground)
C==== IFONTPG    text font 
c====             GET ROUND MOTIF line width problem ( width <=2 does not show on screen ! )
C==== IPGSYSTEM=0 for windows, =1 for MOTIF ( user must set this)
C==== IJOPT=0    plotting to screen,  =1 to file ( user must set this)
C==== PGHARD     output device, use '?' to prompt
C==== PGSCREEN     XXXX need expt with dual devices
C==== PGDEVICE   
c==== IDPGHRD    internal PGPLOT device numbers - still to expt with these
C==== IDPGSCR
C==== IDPGDEV
c==== NCOLSPG   actual number of colours used by last device ( returned from PGCELL_COL )
C====            ( note ASSIGN "/xserver" PGPLOT_DEV, and ASSIGN "/xwindow" PGPLOT_TYPE
C====              in VMS for default device name and type )
C==== [ PASSED directly are:  ]
C==== [ IPLOT = 8 contours only, 1 perspective histogram, 2-6 colours ]
c==== [ ICOL        number of colours to be used <=256 )  ]
C==== [ ICONT number of line contours to be drawn, best as ( (ICONT/2**n) -1 )]
C==== [ CONT(1 to ICOL-1 ) contour levels ]
C==== note you may superimpose say ICONT= 15 line contours on a ICOL=256 colour map ! 
      INTEGER IOVER,IRETURN,ICONRANGE,ICONOVR,ICONTYPE,IHIST,ITRANS,
     >        IPXL,INUMB,IRANGE,IWIDTH,IDPGHRD,IDPGSCR,IDPGDEV,NCOLSPG,
     >        IFIRSTPG,IFONTPG,IHEADPG,IPGSYSTEM,IJOPT
      REAL TRPG(6),XMINPG,XMAXPG,YMINPG,YMAXPG,CONMIN,CONMAX,SIZEPG,PGSCR01(6)
      CHARACTER*60 PGHARD,PGSCREEN,PGDEVICE
      COMMON/PGCNTRL/IOVER,IRETURN,ICONRANGE,ICONOVR,ICONTYPE,IHIST,ITRANS,
     >  IPXL,INUMB,IRANGE,IWIDTH,NCOLSPG,IFIRSTPG,IFONTPG,IHEADPG,
     >  TRPG,XMINPG,XMAXPG,YMINPG,YMAXPG,CONMIN,CONMAX,SIZEPG,PGSCR01,
     >  PGHARD,PGSCREEN,PGDEVICE,IDPGHRD,IDPGSCR,IDPGDEV,IPGSYSTEM,IJOPT
C==== default R,G,B for colours 0 (background) & 1 (foreground)
C==== PCPCPC  G77 only allows ONE occurence of a data - so put this line
C==== in your source code !
C====      DATA PGSCR01/1.0,1.0,1.0,0.0,0.0,0.0/
