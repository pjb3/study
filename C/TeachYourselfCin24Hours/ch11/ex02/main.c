#include <stdio.h>

int main (int argc, const char * argv[]) {
  double flt_num = 123.45;
  double *flt_ptr = &flt_num;
  *flt_ptr = 543.21;
  printf("flt_num = %f\n", flt_num);
  return 0;
}
