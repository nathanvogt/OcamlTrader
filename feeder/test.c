#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <unistd.h>
#include <limits.h>

FILE* f;

CAMLprim value initReader() {
    f = fopen("data/ETH-USD.csv", "r");
    if( f == NULL )  {
      printf ("Error opening file");
      return Val_unit;
   }
    return Val_unit;
}

int bruh = 0;
CAMLprim value test( value v )
{
    int i = Int_val(v);
    i += bruh;
    bruh = i+50;
    printf("%d\n", i);
    return Val_unit;
}
CAMLprim value nextDay(){
    char buff[255];
    fscanf(f, "%s", buff);
    return caml_copy_string(buff);
}