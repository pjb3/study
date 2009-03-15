#include <stdio.h>

int main (int argc, const char * argv[]) {
  for(int ch = ' '; ch != 'q' ? 1 : 0; ch = getchar());
  return 0;
}
