#include <stdio.h>

int main (int argc, const char * argv[]) {
  int i, j;
  
  i = 1;
  while(i <= 3) {
    printf("The start of iteration %d of the outer loop.\n", i);
    j = 1;
    do {
      printf("    The iteration %d of the inner loop.\n", j);
      j++;
    } while(j<=4);
    printf("The end of iteration %d of the outer loop.\n", i);
    i++;
  }
}
