#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <unistd.h>
#include <limits.h>

FILE* f;
char eof[] = "EOF";
char ui[] = "UI";

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
    if(f == NULL){return caml_copy_string(ui);}
    char buff[255];
    int res = fscanf(f, "%s", buff);
    if(res == -1){ //end of file
        return caml_copy_string(eof);
    }
    return caml_copy_string(buff);
}