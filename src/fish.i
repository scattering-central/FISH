%module Core
%include <arrays_java.i>
 %{
 /* Includes the header in the wrapper code */
 #include "fish.h"
 %}
 
 /* Parse the header file to generate wrappers */
 %include "fish.h"
 /* Helper functions for two- or multi-dimensional arrays */
 
%define ARRAYHELPER2(TYPE,NAME,II,JJ)
%inline %{
TYPE (*new_ ## NAME())[JJ] {
   return (TYPE (*)[JJ]) malloc(II*JJ*sizeof(TYPE));
}
void free_ ## NAME(TYPE (*x)[JJ]) {
   free(x);
}
void NAME ## _set(TYPE x[II][JJ], int i, int j, TYPE v) {
   x[i][j] = v;
}
TYPE NAME ## _get(TYPE x[II][JJ], int i, int j) {
   return x[i][j];
}
%}
%enddef
 
%define ARRAYHELPER3(TYPE,NAME,II,JJ,KK)
%inline %{
TYPE (*new_ ## NAME())[JJ][KK] {
   return (TYPE (*)[JJ][KK]) malloc(II*JJ*KK*sizeof(TYPE));
}
void free_ ## NAME(TYPE (*x)[JJ][KK]) {
   free(x);
}
void NAME ## _set(TYPE x[II][JJ][KK], int i, int j, int k, TYPE v) {
   x[i][j][k] = v;
}
TYPE NAME ## _get(TYPE x[II][JJ][KK], int i, int j, int k) {
   return x[i][j][k];
}
%}
%enddef

/* ARRAYHELPER2(int,lab,MW,3)  for testing */
/* ARRAYHELPER3(int,lab2,MW,3,20)  for testing */
/* ARRAYHELPER2(float,c,MW,MN)  for testing */
ARRAYHELPER2(int,lpar,MV,3)
ARRAYHELPER2(float,q,MW,MN)
ARRAYHELPER2(float,c,MW,MN)
ARRAYHELPER2(float,e,MW,MN)
