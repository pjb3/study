#include <stdio.h>

int main (int argc, const char * argv[]) {
  int ch;
  
  printf("Please type in one character:\n");
  ch = getc( stdin );
  printf("The character you just entered is: %c\n", ch);
  return 0;
}
