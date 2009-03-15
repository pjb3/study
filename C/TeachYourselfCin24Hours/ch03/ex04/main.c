#include <stdio.h>

int multiply(int x, int y) {
  return x * y;
}

int main (int argc, const char * argv[]) {
    printf("2 * 3 = %d\n", multiply(2, 3));
    return 0;
}
