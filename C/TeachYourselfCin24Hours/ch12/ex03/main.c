#include <stdio.h>

int main (int argc, const char * argv[]) {
  char list_ch[][2] = {
    '1','a',
    '2','b',
    '3','c',
    '4','d',
    '5','e',
    '6','f'
  };
  printf("Total bytes: %d\n", sizeof(list_ch));
  for(int i=0; i<6; i++) {
    printf("%c -> %c\n", list_ch[i][0], list_ch[i][1]);
  }
  return 0;
}
