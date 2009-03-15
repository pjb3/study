#include <stdio.h>

int main (int argc, const char * argv[]) {
  char ch = ' ';
  int int_num = 0;
  float flt_num = 0.0f;
  double dbl_num = 0.0;
  
  printf("The size of char is: %d-byte\n", sizeof(char));
  printf("The size of ch is: %d-byte\n", sizeof ch);
  printf("The size of int is: %d-byte\n", sizeof(int));
  printf("The size of int_num is: %d-byte\n", sizeof int_num);
  printf("The size of float is: %d-byte\n", sizeof(float));
  printf("The size of flt_num is: %d-byte\n", sizeof flt_num);
  printf("The size of double is: %d-byte\n", sizeof(double));
  printf("The size of dbl_num is: %d-byte\n", sizeof dbl_num);  
}
