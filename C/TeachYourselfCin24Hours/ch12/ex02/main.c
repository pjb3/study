#include <stdio.h>

int main (int argc, const char * argv[]) {
  int i;
  char array_ch[5];
  for(i=0; i<5; i++) {
    array_ch[i] = 'A'+i;
  }
  printf("%s\n", array_ch);
  return 0;
}
