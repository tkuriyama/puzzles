#include <stdlib.h>
#include <stdio.h>
#include "coursedb.h"

// REPL

typedef struct {
  char *buffer;
  size_t bufferLength;
  ssize_t inputLength;
} InputBuffer;


InputBuffer *new_InputBuffer() {
  InputBuffer *ib = (InputBuffer *)emalloc(sizeof(InputBuffer));
  ib->buffer = NULL;
  ib->bufferLength = 0;
  ib->inputLength = 0;
  return ib;
}

void read_input(InputBuffer *ib) {
  ssize_t inputLength = getline(&ib->buffer, &ib->bufferLength, stdin);
  if (inputLength == 0) {
    printf("Error reading input, exiting\n");
    exit(EXIT_FAILURE);
  }
  ib->inputLength = inputLength - 1;
  ib->buffer[inputLength - 1] = 0;
  
}

void free_input(InputBuffer *ib) {
  free(ib->buffer);
  free(ib);
}

void prompt() { printf("> "); }

int main() {
  InputBuffer * ib = new_InputBuffer();
  printf("Welcome to Courses DB.\n \
          All commenads are prefixed with a colon (:) \n	\
          For help, type :help\n");
  
  while (1) {
    prompt();
    read_input(ib);
    if(strcmp(ib->buffer, ":exit") == 0) {
      if (db.initialized)
	ewrap(clear_database(), "clear_database");
      free_input(ib);
      exit(EXIT_SUCCESS);
    }
    else if(strcmp(ib->buffer, ":init") == 0) {
      if (db.initialized)
	printf("Error: DB already initialized.\n");
      else 
	ewrap(init_database(), "init_database");
    }
    else {
      printf("Unrecognized command: %s\n", ib->buffer);
    }
  }
  
  return 0;
}
