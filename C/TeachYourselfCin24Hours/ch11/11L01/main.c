#include <stdio.h>

int main (int argc, const char * argv[]) {
  char c;
  int x;
  float y;
  
  printf("c: address=%p, content=%c\n", &c, c);
  printf("x: address=%p, content=%d\n", &x, x);
  printf("y: address=%p, content=%5.2f\n", &y, y);
  
  c = 'A';
  x = 7;
  y = 123.45;
  
  printf("c: address=%p, content=%c\n", &c, c);
  printf("x: address=%p, content=%d\n", &x, x);
  printf("y: address=%p, content=%5.2f\n", &y, y);  
  
  return 0;
}
