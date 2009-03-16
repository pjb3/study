#include <stdio.h>

int main (int argc, const char * argv[]) {
  double list_data[6] = {
    1.12345,
    2.12345,
    3.12345,
    4.12345,
    5.12345
  };
  printf("Size of data: %d\n", sizeof(list_data));
  printf("Size of data: %d\n", (&list_data[5] - &list_data[0] + 1) * sizeof(double) );
  return 0;
}
