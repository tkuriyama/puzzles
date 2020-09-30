#include <stdlib.h>
#include <stdio.h>

// Utilities

void *emalloc(size_t sz) {
   void *p;
   if((p = malloc(sz)) == NULL) exit(EXIT_FAILURE);
   return p;
}

void *erealloc(void *p, size_t sz) {
  if((p = realloc(p, sz)) == NULL) exit(EXIT_FAILURE);
  return p;
}

void ewrap(int status, char * name) {
  if (status == 0)
    printf("Error: %s process returned error code\n", name);
  else 
    printf("Success: %s\n", name); 
  return;
}
