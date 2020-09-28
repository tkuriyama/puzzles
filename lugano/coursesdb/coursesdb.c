#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "coursesdb.h"

void *emalloc(size_t sz) {
   void *p;
   if((p = malloc(sz)) == NULL) abort();
   return p;
}

typedef struct {
  char *buffer;
  size_t bufferLength;
  ssize_t inputLength;
} InputBuffer;

InputBuffer * newInputBuffer() {
  InputBuffer *ib = (InputBuffer *)emalloc(sizeof(InputBuffer));
  ib->buffer = NULL;
  ib->bufferLength = 0;
  ib->inputLength = 0;
  return ib;
}

void readInput(InputBuffer * ib) {
  ssize_t inputLength = getline(&ib->buffer, &ib->bufferLength, stdin);
  if (inputLength == 0) {
    printf("Error reading input, exiting\n");
    exit(EXIT_FAILURE);
  }
  ib->inputLength = inputLength - 1;
  ib->buffer[inputLength - 1] = 0;
  
}

void freeInput(InputBuffer * ib) {
  free(ib->buffer);
  free(ib);
}

void prompt() { printf("> "); }

int main() {
  InputBuffer * ib = newInputBuffer();
  printf("Welcome to Courses DB.\n \
          All commenads are prefixed with a colon (:) \n \
          For help, type :help\n");
  
  while (1) {
    prompt();
    readInput(ib);
    if(strcmp(ib->buffer, ":exit") == 0) {
      freeInput(ib);
      exit(EXIT_SUCCESS);
    }
    else {
      printf("unrecognized command: %s\n", ib->buffer);
    }
  }
  
  return 0;
}
