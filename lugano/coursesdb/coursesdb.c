#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "coursesdb.h"

// Utilities

void *emalloc(size_t sz) {
   void *p;
   if((p = malloc(sz)) == NULL) abort();
   return p;
}

void ewrap(int status, char * name) {
  if (status == 0)
    printf("Error: %s process returned error code\n", name);
  else 
    printf("Success: %s\n", name); 
  return;
}

// DB Init and Destroy

typedef struct {
  int pkey;
  char title[60];
  int year[4];
  char semester;
} Course;

typedef struct {
  int pkey;
  char name[30];
  int enroll_year[4];
} Student;

typedef struct {
  int pkey;
  Course *course;
  Student *student;
} Enrollment;

typedef struct {
  int initialized;
  Course *courses;
  Student *students;
  Enrollment *enrollments;
} Database;

int init_database(Database *db) {
  db->initialized = 1;
  db->courses = (Course *)emalloc(sizeof(Course)*10);
  db->students = (Student *)emalloc(sizeof(Student)*10);
  db->enrollments = (Enrollment *)emalloc(sizeof(Enrollment)*40);    
  return 1; 
}

int clear_database(Database *db) {
  free(db->courses);
  free(db->students);
  free(db->enrollments);
  return 1;
}

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
  Database db ;
  db.initialized = 0;
  InputBuffer * ib = new_InputBuffer();
  printf("Welcome to Courses DB.\n \
          All commenads are prefixed with a colon (:) \n	\
          For help, type :help\n");
  
  while (1) {
    prompt();
    read_input(ib);
    if(strcmp(ib->buffer, ":exit") == 0) {
      if (db.initialized)
	ewrap(clear_database(&db), "clear_database");
      free_input(ib);
      exit(EXIT_SUCCESS);
    }
    else if(strcmp(ib->buffer, ":init") == 0) {
      if (db.initialized)
	printf("Error: DB already initialized.\n");
      else 
	ewrap(init_database(&db), "init_database");
    }
    else {
      printf("Unrecognized command: %s\n", ib->buffer);
    }
  }
  
  return 0;
}
