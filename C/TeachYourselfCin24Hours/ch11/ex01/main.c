#include <stdio.h>

int main (int argc, const char * argv[]) {
  int x = 512, y = 1024, z = 2048;
  
  printf("%p -> %d\n", &x, x);
  printf("%p -> %d\n", &y, y);
  printf("%p -> %d\n", &z, z);
  
  return 0;
}
