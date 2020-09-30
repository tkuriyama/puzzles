#include <stdlib.h>
#include <stdio.h>
#include "coursedb.c"
#include "coursedb.h"

// Utils

void ewrap(int status, char * name) {
  if (status == 0)
    printf("Error: %s process returned error code\n", name);
  else 
    printf("Success: %s\n", name); 
  return;
}

int main() {
  printf("Hello world!\n");
  return 0;
}
