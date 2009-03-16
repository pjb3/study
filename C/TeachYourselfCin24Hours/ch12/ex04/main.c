#include <stdio.h>

int main (int argc, const char * argv[]) {
  char list_ch[] = {'I',' ','L','i','k','e',' ','C','!','\0'};
  for(int i=0; list_ch[i]; i++) {
    printf("%c", list_ch[i]);
  }
  return 0;
}
