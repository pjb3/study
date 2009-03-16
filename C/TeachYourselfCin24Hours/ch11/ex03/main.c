#include <stdio.h>

int main (int argc, const char * argv[]) {
  char ch = 'A';
  char *ch_ptr = &ch;
  *ch_ptr = 66;
  printf("%c\n", ch);
  return 0;
}
