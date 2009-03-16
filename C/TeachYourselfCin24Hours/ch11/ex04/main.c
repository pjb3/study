#include <stdio.h>

int main (int argc, const char * argv[]) {
  int x = 5, y = 6;
  int *x_ptr = &x, *y_ptr = &y;
  *x_ptr *= *y_ptr;
  printf("%d\n", x);
  return 0;
}
