#include <stdio.h>

int main (int argc, const char * argv[]) {
  int i, sum;
  
  sum = 0;
  for(i=0; i<8; i++) {
    if(i%6==0)
      continue;
    sum += i;
  }
  printf("sum = %d", sum);
  return 0;
}
