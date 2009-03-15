#include <stdio.h>
#include <math.h>

int main (int argc, const char * argv[]) {
  double degrees = 30.0;
  degrees *= 3.141593 / 180.0;
  printf("sin = %f\n", sin(degrees));
  printf("tan = %f\n", tan(degrees));
  return 0;
}
