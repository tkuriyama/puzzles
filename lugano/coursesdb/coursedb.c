#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "coursedb.h"

// Utils

void *emalloc(size_t sz) {
   void *p;
   if((p = malloc(sz)) == NULL) exit(EXIT_FAILURE);
   return p;
}

void *erealloc(void *p, size_t sz) {
  if((p = realloc(p, sz)) == NULL) exit(EXIT_FAILURE);
  return p;
}

// DB

Database db = { .initialized = 0 };

int init_database() {
  db.initialized = 1;
  db.courses = (Course *)emalloc(sizeof(Course)*10);
  db.courses->pkey = 0;
  db.course_ct = 0;
  db.course_max_ct = 10;
  db.students = (Student *)emalloc(sizeof(Student)*10);
  db.students->pkey = 0;
  db.student_ct = 0;
  db.student_max_ct = 10;
  db.enrollments = (Enrollment *)emalloc(sizeof(Enrollment)*40);
  db.enrollments->pkey = 0;
  db.enrollment_ct = 0;
  db.enrollment_max_ct = 40;
  return 1;
}

int clear_database() {
  free(db.courses);
  free(db.students);
  free(db.enrollments);
  return 1;
}

// DB Updates

int add_course(int id, const char * title, int year, char semester) {
  if (db.course_ct + 1 == db.course_max_ct) {
    db.course_max_ct += db.course_max_ct / 2;
    db.courses = (Course *)erealloc(db.courses, db.course_max_ct);
  }
  // current record
  db.courses->id = id;
  strcpy(db.courses->title, title);
  db.courses->year = year;
  db.courses->semester = semester;
  db.courses->active = 1;

  // new record
  ++db.courses;
  ++db.course_ct;
  db.courses->pkey = db.course_ct;
  return 1;
}

int delete_course(int id) {
  int found = 0;
  for (int i = 0; i < db.course_ct; ++i) {
    if (db.courses[i].id == id) {
      db.courses[i].active = 0;
      found = 1;
    }
  }
  return found;
}
