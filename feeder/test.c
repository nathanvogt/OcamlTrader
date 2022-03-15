#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <unistd.h>
#include <limits.h>

FILE* f;

CAMLprim value initReader() {
    if(f != NULL){return Val_unit;}
    f = fopen("data/ETH-USD.csv", "r");
    if( f == NULL )  {
      printf ("Error opening file");
      return Val_unit;
   }
    return Val_unit;
}
CAMLprim value nextDay(){
    char buff[255];
    fscanf(f, "%s", buff);
    return caml_copy_string(buff);
}