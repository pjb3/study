#include <stdio.h>

int main (int argc, const char * argv[]) {
  int x = 0xEFFF, y = 0x1000;
  printf("%d, %u\n", !x, !x);
  printf("%d, %u\n", !y, !y);  
  return 0;
}
