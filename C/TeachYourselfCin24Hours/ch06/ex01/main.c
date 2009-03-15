#include <stdio.h>

int main (int argc, const char * argv[]) {
  int x = 1, y = 3; 
  printf("Given that x = %d and y = %d, x += y => %d\n", x, y, x += y);
  
  x = 1, y = 3;
  printf("Given that x = %d and y = %d, x += -y => %d\n", x, y, x += -y);

  x = 1, y = 3;  
  printf("Given that x = %d and y = %d, x -= y => %d\n", x, y, x -= y);

  x = 1, y = 3;  
  printf("Given that x = %d and y = %d, x -= -y => %d\n", x, y, x -= -y);

  x = 1, y = 3;  
  printf("Given that x = %d and y = %d, x *= y => %d\n", x, y, x *= y);

  x = 1, y = 3;  
  printf("Given that x = %d and y = %d, x *= -y => %d\n", x, y, x *= -y);
  
  return 0;
}
