#include <stdio.h>
#include <string.h>

int main(void) {;
  char str[1000];
  fgets(str, 1000, stdin);

  // fgets includes \n character, so subtract 1
  int length = strlen(str) - 1;

  int twotimes = 1;
  
  if (length % 2 != 0) {
    twotimes = 0;
  }
  else {
    int mid = length / 2; 
    int i = 0;
    while (i < mid) {
      if (str[i] != str[i + mid]) {
	twotimes = 0;
	break;
      }
      ++i;
    }
  }
  
  if (twotimes) {
    printf("YES\n");
  }
  else {
    printf("NO\n");
  }
    
  return 0;
}
