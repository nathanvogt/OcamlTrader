#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>


CAMLprim value test( value v )
{
    int i;
    i = Int_val(v);
    printf("%d\n", i);
    return Val_unit;
}