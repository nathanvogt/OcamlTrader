#define CAML_NAME_SPACE
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <unistd.h>
#include <limits.h>

FILE *f;
char eof[] = "EOF";
char ur[] = "UR";
long pos;

CAMLprim value initReader()
{
    if (f != NULL)
    {
        return Val_unit;
    }
    f = fopen("data/ETH-USD.csv", "r");
    // f = fopen("data/TEST-DATA.csv", "r");
    if (f == NULL)
    {
        printf("Error opening file");
        return Val_unit;
    }
    pos = 0L;
    return Val_unit;
}
CAMLprim value resetReader()
{
    if (f == NULL)
    {
        printf("can't reset uninitialized file reader");
        return Val_unit;
    }
    fseek(f, 0L, SEEK_SET);
    pos = 0L;
    return Val_unit;
}
CAMLprim value nextDay()
{
    if (f == NULL)
    {
        return caml_copy_string(ur);
    }
    char buff[1023];
    int res = fscanf(f, "%s", buff);
    if (res == -1)
    { // end of file
        return caml_copy_string(eof);
    }
    pos = ftell(f);
    return caml_copy_string(buff);
}
// checks there are that many days of historical data
CAMLprim value lookback(value days)
{

    int d = Int_val(days);
    int anchor = d;

    fseek(f, 0L, SEEK_END);
    while (d > 0)
    {
        long ret = ftell(f);
        char next = fgetc(f);
        fseek(f, ret, SEEK_SET);
        if (next == '\n')
        {
            d--;
        }
        else
        {
        }
        if (d != 0)
        {
            fseek(f, -1, SEEK_CUR);
        }
    }
    char res[1023 * anchor];
    memset(res, 0, sizeof res);
    char buff[1023];
    int first = 1;
    while (fscanf(f, "%s", buff) != -1)
    {
        if (first == 1)
        {
            first = 0;
        }
        else
        {
            strcat(res, " ");
        }
        strcat(res, buff);
        memset(buff, 0, sizeof buff);
    }
    int r = fseek(f, pos, SEEK_SET);
    return caml_copy_string(res);
}