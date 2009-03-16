#include <stdio.h>

int main (int argc, const char * argv[]) {
  int ch;
  ch = getchar();
  switch(ch) {
    case 'A':;
    case 'B':;
    case 'C':
      printf("%d", ch);
      break;
    default:
      printf("%c", ch);
  }
}
