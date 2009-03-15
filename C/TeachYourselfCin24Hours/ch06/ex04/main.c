#include <stdio.h>

/*
 x++ yields 1 and then increments x to 2,
 but then we assign the 1 that x++ yielded to 1
 */
int main (int argc, const char * argv[]) {
  int x = 1;
  
  printf("x = x++ produces:   %d\n", x = x++);
  printf("Now x contains: %d\n", x);
  return 0;
}
