// these parameters should match FISHDIM.PAR
#define MN 1024
#define MV 99
#define MC 96
#define MW 80
#define MF 8
#define MI 96

char string[180];

// c subroutines
void init();
void acmod(int idx);
void datset();
int setup(int id,char *filename);
int openshare(int channel,char *filename);
void closefile(int channel);
void pindex();
void talk(char *command);
void datin(int index, int iset);
void datin3(int iflag, int iset,int iskip);
void rnilsill(int iset);
char* getLpar(int n);
int lsqout(char * filename, int mode);
int outp(int i1, int j1, int i2, int j2, char * filename,int mode);
char* conop2(int n);
float sqscl(int setno);
void setlabel(int set,char* label,int len);
void setlab2(int line, int set, char* label, int len);
void range(int set,int min,int max);

//void javatest();
//void javatest2();


// common blocks (generated from Fortran by Perl script)

// initial I to N are integers in fortran, otherwise real
// array indices must be reversed!

extern struct Common_one {
/*       COMMON/ONE/NCH(MW),NC1(MW),NC2(MW),NMC(MW),NC3(MW),NC4(MW), */
	int nch[MW];
	int nc1[MW];
	int nc2[MW];
	int nmc[MW];
	int nc3[MW];
	int nc4[MW];
/*      >IDC(MW),NSUM(MW),IC1(MW),IC2(MW),IC3(MW),LAB(3,MW),LAB2(20,3,MW), */
	int idc[MW];
	int nsum[MW];
	int ic1[MW];
	int ic2[MW];
	int ic3[MW];
	int lab[MW][3];
	int lab2[MW][3][20];
/*      >RSPARE(10,MW),C(MN,MW),Q(MN,MW),E(MN,MW),NDIM */
	float rspare[MW][10];
	float c[MW][MN];
	float q[MW][MN];
	float e[MW][MN];
	int ndim;
} one_;

extern struct Common_two {
/*       COMMON/TWO/NT,NP,NS,NC,NN,NNN(11),IW,IK,IP,MS,IY,LS(3),NPRED, */
	int nt;
	int np;
	int ns;
	int nc;
	int nn;
	int nnn[11];
	int iw;
	int ik;
	int ip;
	int ms;
	int iy;
	int ls[3];
	int npred;
/*      >     NDAT,NYC,NPR,LS2(16),LM(MV),LTYP(MV),LPAR(3,MV),V(MV), */
	int ndat;
	int nyc;
	int npr;
	int ls2[16];
	int lm[MV];
	int ltyp[MV];
	int lpar[MV][3];
	float v[MV];
/*      >     ESD(MV),PS(MV),IDEL(MV),DEL(MV),DV(MV),VOLP(MV),CON(24) */
	float esd[MV];
	float ps[MV];
	int idel[MV];
	float del[MV];
	float dv[MV];
	float volp[MV];
	float con[24];
} two_;

extern struct Common_three {
/*       COMMON/THREE/JD(MF),JC(MF),JB(MF),JY(MF),JW(MF),JAM(MF),JPQ(MF), */
	int jd[MF];
	int jc[MF];
	int jb[MF];
	int jy[MF];
	int jw[MF];
	int jam[MF];
	int jpq[MF];
/*      * JSQ(MF),JBT(MF),JXX(MF),LCOM(80,4),NOREF,NOWT,SSE,FIT(MF), */
	int jsq[MF];
	int jbt[MF];
	int jxx[MF];
	int lcom[4][80];
	int noref;
	int nowt;
	float sse;
	float fit[MF];
/*      * VAR,XDWE,NPSMEAR */
	float var;
	float xdwe;
	int npsmear;
} three_;

extern struct Common_tie {
/*       COMMON/TIE/PC(12,MC),IMOD(12,MC),NCON(MC),NUSE(MC),NTIE(MV) */
	float pc[MC][12];
	int imod[MC][12];
	int ncon[MC];
	int nuse[MC];
	int ntie[MV];
} tie_;

extern struct Common_char {
/*       COMMON/CHAR/ICH(30),NICH */
	int ich[30];
	int nich;
} char_;

extern struct Common_ch {
/*       COMMON/CH/IS,ID,IL,JS,JDF,JF,JL,JP */
	int is;
	int id;
	int il;
	int js;
	int jdf;
	int jf;
	int jl;
	int jp;
} ch_;

extern struct Common_jfit {
/*      COMMON/JFIT/INFIT,ITIM */
	int infit;
	int itim;
} jfit_;

extern struct Common_dmat {
/*       COMMON/DMAT/IID(MV),DWD(MI,MI),SVW(MI),VVV(MI,MI),COV(MI,MI) */
	int iid[MV];
	float dwd[MI][MI];
	float svw[MI];
	float vvv[MI][MI];
	float cov[MI][MI];
} dmat_;

extern struct Common_poly {
/*        COMMON/POLY/PR1,PR2,NR1,NR2,ABC(5),RB,PA,PB,PCC,PD,IPD,JPD,NPP, */
	float pr1;
	float pr2;
	int nr1;
	int nr2;
	float abc[5];
	float rb;
	float pa;
	float pb;
	float pcc;
	float pd;
	int ipd;
	int jpd;
	int npp;
/*      *       NPP1,NPP2,NPP3,NPM,NSIMP */
	int npp1;
	int npp2;
	int npp3;
	int npm;
	int nsimp;
} poly_;

extern struct Common_mainc {
/*       COMMON/MAINC/MC1(32),MC2(32),MCLEN(32),MCSTR(20,32),NNCOM,MCD, */
	int mc1[32];
	int mc2[32];
	int mclen[32];
	int mcstr[32][20];
	int nncom;
	int mcd;
/*      *ISP(2),MP1(32),MP2(32),MPLEN(32),MPSTR(20,32),NNPOM,MPD */
	int isp[2];
	int mp1[32];
	int mp2[32];
	int mplen[32];
	int mpstr[32][20];
	int nnpom;
	int mpd;
} mainc_;

extern struct Common_axtyp {
/*       COMMON/AXTYP/LABAX(2,9),IX(2),IIY(2),ILG(2),ILX(2),ILY(2),IX1(9), */
	int labax[9][2];
	int ix[2];
	int iiy[2];
	int ilg[2];
	int ilx[2];
	int ily[2];
	int ix1[9];
/*      *IX2(9),ILOG,IGP,IG1,IG2,PMN(2,MW+1),PMX(2,MW+1),SHIFT(MW), */
	int ix2[9];
	int ilog;
	int igp;
	int ig1;
	int ig2;
	float pmn[MW+1][2];
	float pmx[MW+1][2];
	float shift[MW];
/*      *NSET(MW),LLTYP(MW),LSYM(MW),LTYP2(MW),LSYM2(MW),IPF(MW),IPN(MW), */
	int nset[MW];
	int lltyp[MW];
	int lsym[MW];
	int ltyp2[MW];
	int lsym2[MW];
	int ipf[MW];
	int ipn[MW];
/*      *IE(MW),KC,K */
	int ie[MW];
	int kc;
	int k;
} axtyp_;

extern struct Common_slfit {
/*       COMMON/SLFIT/SLX1(MW),SLX2(MW),SLY1(MW),SLY2(MW),VVAR(MW),GRAD(MW), */
	float slx1[MW];
	float slx2[MW];
	float sly1[MW];
	float sly2[MW];
	float vvar[MW];
	float grad[MW];
/*      *RINT(MW),GRE(MW),RE(MW),RG1(MW),RG2(MW),NSL(MW),NUN(MW),RMID, */
	float rint[MW];
	float gre[MW];
	float re[MW];
	float rg1[MW];
	float rg2[MW];
	int nsl[MW];
	int nun[MW];
	float rmid;
/*      *SLUM1(MW),SLUM2(MW),SLUM3(MW),SLUM4(MW) */
	float slum1[MW];
	float slum2[MW];
	float slum3[MW];
	float slum4[MW];
} slfit_;

extern struct Common_binpar {
/*       COMMON/BINPAR/NBIN(6),RBIN(5) */
	int nbin[6];
	float rbin[5];
} binpar_;

extern struct Common_gamfun {
/*       COMMON/GAMFUN/DF(4),IGAMMA */
	float df[4];
	int igamma;
} gamfun_;

extern struct Common_rodsetc {
/*       COMMON/RODSETC/QQ,R,XELL,RL,DELR,DELRCAP,CONT, */
	float qq;
	float r;
	float xell;
	float rl;
	float delr;
	float delrcap;
	float cont;
/*      > VV1,VV2,VV3,VV4,QR1,QL1,V1,QR2,QL2,V2,VT,PIBY2,PI, */
	float vv1;
	float vv2;
	float vv3;
	float vv4;
	float qr1;
	float ql1;
	float v1;
	float qr2;
	float ql2;
	float v2;
	float vt;
	float piby2;
	float pi;
/*      > DSUM,IF1ROD,IF1ELL,IF1ESH,IF1SHR,NEEDF1,M1,M2,M3, */
	float dsum;
	int if1rod;
	int if1ell;
	int if1esh;
	int if1shr;
	int needf1;
	int m1;
	int m2;
	int m3;
/*      > EPSAROD,EPSRROD,EPSAELL,EPSRELL,EPSAESH,EPSRESH,EPSASHR,EPSRSHR, */
	float epsarod;
	float epsrrod;
	float epsaell;
	float epsrell;
	float epsaesh;
	float epsresh;
	float epsashr;
	float epsrshr;
/*      > ABSERR,ABSERR1,KOUNT,KOUNT1,KOUNT2,IFAIL, */
	float abserr;
	float abserr1;
	int kount;
	int kount1;
	int kount2;
	int ifail;
/*      > PSIQ,PHI,STH,NPAR,LTYPROD,TPHIZER,C2PZ,TH1,TH2,PH1,PH2, */
	float psiq;
	float phi;
	float sth;
	int npar;
	int ltyprod;
	float tphizer;
	float c2pz;
	float th1;
	float th2;
	float ph1;
	float ph2;
/*      > RMSPE,PREVRMSPE,EPSASPE,EPSRSPE,PREVRN3,PREVELLMAX, */
	float rmspe;
	float prevrmspe;
	float epsaspe;
	float epsrspe;
	float prevrn3;
	float prevellmax;
/*      > QL3,RN3,EPSAKHOL,EPSRKHOL,IF1KHOL,WMAXKHOL */
	float ql3;
	float rn3;
	float epsakhol;
	float epsrkhol;
	int if1khol;
	float wmaxkhol;
} rodsetc_;

extern struct Common_sqhpb {
/* NOTE:       REAL*8 ASSQ,BSSQ,CSSQ,FSSQ */	
/*       COMMON /SQHPB/ ASSQ,BSSQ,CSSQ,FSSQ,ETA,GEK,AK,U,vv,GAMK, */
	double assq;
	double bssq;
	double cssq;
	double fssq;
	float eta;
	float gek;
	float ak;
	float u;
	float vv;
	float gamk;
/*      *                 SETA,SGEK,SAK,SCAL,G1,SIG */
	float seta;
	float sgek;
	float sak;
	float scal;
	float g1;
	float sig;
} sqhpb_;

extern struct Common_sqhpc {
/* NOTE:       REAL*8 dcoeff */
/*       COMMON /SQHPC/ DCOEFF(4,5),COEFF(6,5) */
	double dcoeff[5][4];
	float coeff[5][6];
} sqhpc_;

extern struct Common_model24 {
/*       COMMON/MODEL24/JJJ */
	int jjj;
} model24_;

extern struct Common_quad {
/*       COMMON/QUAD/NMETH(MV),NIQ,NIQMAX,NWTMAX,IQ(MV),NGPT(8),IB(8), */
	int nmeth[MV];
	int niq;
	int niqmax;
	int nwtmax;
	int iq[MV];
	int ngpt[8];
	int ib[8];
/*      >            RG(2048),WTR(2048) */
	float rg[2048];
	float wtr[2048];
} quad_;

extern struct Common_work {
/*       COMMON/WORK/RJUNK(110000) */
	float rjunk[110000];
} work_;

extern struct Common_pref {
/* NOTE:       CHARACTER*3 SYS */
/* NOTE:       CHARACTER*256 FISHSRCE,FISHPLOT,FISHLSIN */	
/*       COMMON/PREF/FISHSRCE,FISHPLOT,FISHLSIN,LFSRC,LFPLT,LFLSP,SYS */
	char fishsrce[256];
	char fishplot[256];
	char fishlsin[256];
	int lfsrc;
	int lfplt;
	int lflsp;
	char sys[3];
} pref_;
