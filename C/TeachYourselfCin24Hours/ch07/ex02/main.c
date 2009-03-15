#include <stdio.h>

/*
 There will be a total of 9 print statements
 8 for the first loop for i from 0 through 7
 and then the final one for i of 8,
 which is the value of i after the second loop,
 the one will the null statement, has finished
 */

int main (int argc, const char * argv[]) {
  int i, j;
  
  for(i=0, j=1; i<8; i++, j++)
    printf("%d + %d = %d\n", i, j, i+j);

  for(i=0, j=1; i<8; i++, j++);
    printf("%d + %d = %d\n", i, j, i+j);  
  
  return 0;
}
