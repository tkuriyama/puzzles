#include <stdio.h>
#include <ctype.h>
#include <string.h>

const int MAXLEN = 30;

int main(int argc, char *argv[]) {
  if (argc == 1) {
    printf("No words to search for.\n");
    return 0;
  }

  int c;
  int return_val = argc - 1;

  int i = 0;
  char word[MAXLEN];
  
  while ((c = getchar()) != EOF && return_val > 0) {
    // we're at a space delimiter
    if (isspace(c)) {
      if (word[0] > 0) {
	for (int j = 1; j < argc; ++j) {
	  if (strcmp(word, argv[j]) == 0) {
	    --return_val;
	    argv[j][0] = 0;
	  }
	}
	i = 0;
	memset(word, 0, sizeof(word));
      }
    }
    // we're not at a space delimiter, add char to word
    else {
      word[i++] = c;
      if (i >= MAXLEN) {
	printf("Error: encounted word longer than %d char\n", MAXLEN);
	return -1;
      }
    }
  }

  return ((return_val == 0) ? 0 : 1);
}
