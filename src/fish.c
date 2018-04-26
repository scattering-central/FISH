#include <stdio.h>
#include <string.h>
#include "fish.h"

//extern void fishc_();
//extern void acmod_(int *idx);
//extern void datset_();
//extern void setup_(int *id,char *filename,int *ierr);
//extern void pindex_();
//extern void talk_(char *string);
//extern void datin_(int *index, int *iset);
//extern void javatestf_();
//extern void javatestf2_();

char string[180];

void init() {
	init_();
}
void acmod(int idx) {
	acmod_(&idx);	
}

void datset() {
	datset_();	
}

int setup(int id, char* filename) {
	// funny stuff with pointers to avoid crashes?
	int ierr;
	char file[256];
	char *filep;
	ierr=0;
	filep=&file[0];
	filep=strcpy(filep,filename);
	setup_(&id,filep,&ierr);
	return ierr;
}

int openshare(int channel, char* filename) {
	// funny stuff with pointers to avoid crashes?
	int ierr;
	char file[256];
	char *filep;
	ierr=0;
	filep=&file[0];
	filep=strcpy(filep,filename);
	openshare_(&channel,&ierr,filep);
	return ierr;
}

void closefile(int channel) {
	closefile_(&channel);	
}

void pindex(){
	pindex_();
}

void talk(char* command) {
	char comm[80];
	char *commp;
	commp=&comm[0];
	commp=strncpy(commp,command,80);
	setvbuf( stdout, NULL, _IONBF, 0 );
	talk_(commp);	
}

void datin(int index, int iset){
	datin_(&index,&iset);	
}

void datin3(int iflag, int iset,int iskip){
	datin3_(&iflag,&iset,&iskip);	
}

void rnilsill(int iset){
	rnilsill_(&iset);
}

char* getLpar(int n) {
	char* s=(char *)&two_.lpar[n];
	return 	s;
}

int lsqout(char * filename, int mode) {
	int ierr;
	char file[256];
	char *filep;
	ierr=0;
	filep=&file[0];
	filep=strcpy(filep,filename);
	lsqout_(&ierr,filep,&mode);
	return ierr;		
}

int outp(int i1, int j1, int i2, int j2, char * filename, int mode) {
	int ierr;
	char file[256];
	char *filep;
	int len;
	ierr=0;
	filep=&file[0];
	filep=strcpy(filep,filename);
	len=strlen(filename);
	outp_(&ierr,&i1,&j1,&i2,&j2,filep,&len,&mode);
	return ierr;
}

char * conop2(int n) {
	char text[80],text2[80];
	char *textp;
	char *text2p;
	char *stringp;
	stringp=&string[0];
	textp=&text[0];
	text2p=&text2[0];
	conop2_(&n,textp,text2p);
	int i;
	i=80;
	do {
		i--;
	}
	while(i>0 && text[i] == ' ');
	text[i]=0;
	i=80;
	do {
		i--;
	}
	while(i>0 && text2[i] == ' ');	
	text2[i]=0;
	if(i<=0){
		sprintf(stringp,"%s",textp);		
	} else {
		sprintf(stringp,"%s\n%s",textp,text2p);
	}
	return stringp;
}

float sqscl(int setno) {
	float scale;
	sqscl_(&setno,&scale);
	return scale;
}

void setlabel(int set, char* label,int len) {
	char lab[12];
	char *labp;
	labp=&lab[0];
	labp=strncpy(labp,"           ",11);
	if(len>11){
		len=11;
	}
	labp=strncpy(labp,label,len);
	setlabel_(&set,labp);
}

void setlab2(int line, int set, char* label, int len) {
	char lab[80];
	char *labp;
	labp=&lab[0];
	labp=strncpy(labp,"                                                                                 ",80);
	if(len>80){
		len=80;
	}
	labp=strncpy(labp,label,len);
	setlab2_(&line,&set,labp);	
}

void range(int set,int min,int max) {
	range_(&set,&min,&max);
}

// for testing
void javatest() {
	printf("from c: %d\n",one_.ndim);
	one_.c[0][0]=1.23;
	one_.nc1[2]=33;
//	one_.ndim=123;
	one_.nch[4]=444;
	one_.lab2[5][1][15]=901;
	javatestf_();
	printf("from f: %d\n",one_.ndim);
	printf("nc1[2]=%d\n",one_.nc1[2]);	
	printf("c[0][0]=%f\n",one_.c[0][0]);
	printf("label is: %s\n",(char *)&one_.lab);
	printf("label2 is: %s\n",(char *)&one_.lab[0][1]);	
	strcopy("WXYZ",&one_.lab);
	fflush(stdout);

}

// for testing
void javatest2() {
	printf("new label is: %s\n",(char *)&one_.lab);	
	printf("from c2: %d\n",one_.ndim);
	printf("nch[5]: %d\n",one_.nch[5]);
	printf("lab2[1][2][3]: %d\n",one_.lab2[1][2][3]);
	one_.ndim+=10;
	javatestf2_();
	printf("lab2[1][2][3]: %d\n",one_.lab2[1][2][3]);
	printf("c[1][2]: %f\n",one_.c[1][2]);
	fflush(stdout);		
}

strcopy(s1, s2)
    char s1[], s2[];
{
    int i;

    i = 0;
    while (s1[i] != '\0')
    {
        s2[i] = s1[i];
        ++i;
    };
//    s2[i] = '\0';
    
}
