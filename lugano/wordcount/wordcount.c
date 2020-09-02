#include <stdio.h>
#include <ctype.h>

int main(void) {
  unsigned int ctr = 0;
  int prev_is_space = 1;
  int c = getchar();
  
  while (c != EOF) {
    if (prev_is_space && !isspace(c)) {
	++ctr;
    }
    prev_is_space = isspace(c);
    c = getchar();
  }
  printf("%u\n", ctr);
  return 0;
}
