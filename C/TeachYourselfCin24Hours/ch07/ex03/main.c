#include <stdio.h>

int main (int argc, const char * argv[]) {
  int ch;
  
  printf("Enter a character:\n(enter k to exit)\n");
  while(ch != 'k') {
    ch = getchar();
    putchar(ch);
  }
  printf("Out of the while loop.  Bye!\n");
  return 0;
}
