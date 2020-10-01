#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "coursedb.h"

/*** Utils ***/

void *emalloc(size_t sz) {
   void *p;
   if((p = malloc(sz)) == NULL) exit(EXIT_FAILURE);
   return p;
}

void *erealloc(void *p, size_t sz) {
  if((p = realloc(p, sz)) == NULL) exit(EXIT_FAILURE);
  return p;
}

/*** Create and Destroy DB ***/

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

int active_courses() {
  int ct = 0;
  for (int i=0; i<db.course_ct; i++)
    if (db.courses[i].active) ct++;
  return ct;
}

int active_students() {
  int ct = 0;
  for (int i=0; i<db.student_ct; i++)
    if (db.students[i].active) ct++;
  return ct;
}

void print_stats() {
  printf("\n--- Database Stats\n");
  printf("Course ct / active / max:     %d / %d / %d\n",
	 db.course_ct, active_courses(), db.course_max_ct);
  printf("Student ct / active / max:    %d / %d / %d\n",
	 db.student_ct, active_students(), db.student_max_ct);
  printf("Enrollment ct / active / max: %d / %d / %d\n",
	 db.enrollment_ct, 0, db.enrollment_max_ct);
  printf("-------------------\n\n");
}

/*** Courses ***/

int add_course(int id, const char * title, int year, char semester) {
  if (db.course_ct + 1 == db.course_max_ct) {
    db.course_max_ct += db.course_max_ct / 2;
    db.courses = (Course *)erealloc(db.courses, db.course_max_ct);
  }
  // current record
  db.courses[db.course_ct].id = id;
  strcpy(db.courses[db.course_ct].title, title);
  db.courses[db.course_ct].year = year;
  db.courses[db.course_ct].semester = semester;
  db.courses[db.course_ct].active = 1;
  ++db.course_ct;

  // new record
  db.courses[db.course_ct].pkey = db.course_ct;
  
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

struct course_iterator {
  int i;
  int active;
};
struct course_iterator course_iter = { .i = 0, .active = 0 };  

struct course_iterator *courses() {
  course_iter.i = 0;
  course_iter.active = 1;
  return &course_iter;
}

struct course_iterator *next_course(struct course_iterator *iter) {
  if (!iter->active) return NULL;
  
  while(++(iter->i) <= db.course_ct)
    if (db.courses[iter->i].active == 1)
      break;
  
  return (iter->i >= db.course_ct) ? NULL : iter;
}

void abort_course_iteration(struct course_iterator *iter) {
  iter->active = 0;
}

const char *course_title(const struct course_iterator *iter) {
  return db.courses[iter->i].title;
}

int course_year(const struct course_iterator *iter) {
  return db.courses[iter->i].year;
}

char course_semester(const struct course_iterator *iter) {
  return db.courses[iter->i].semester;
}

/*** Students ***/

int add_student(int id, const char * name, int first_year) {
  if (db.student_ct + 1 == db.student_max_ct) {
    db.student_max_ct += db.student_max_ct / 2;
    db.students = (Student *)erealloc(db.students, db.student_max_ct);
  }
  // current record
  db.students[db.student_ct].id = id;
  strcpy(db.students[db.student_ct].name, name);
  db.students[db.student_ct].enroll_year = first_year;
  db.students[db.student_ct].active = 1;
  ++db.student_ct;

  // new record
  db.students[db.student_ct].pkey = db.student_ct;
  
  return 1;
}

int delete_student(int id) {
  int found = 0;
  for (int i = 0; i < db.student_ct; ++i) {
    if (db.students[i].id == id) {
      db.students[i].active = 0;
      found = 1;
    }
  }
  return found;
}

struct student_iterator {
  int i;
  int active;
};
struct student_iterator student_iter = { .i = 0, .active = 0 };  

struct student_iterator *students() {
  student_iter.i = 0;
  student_iter.active = 1;
  return &student_iter;
}

struct student_iterator *next_student(struct student_iterator *iter) {
  if (!iter->active) return NULL;
  
  while(++(iter->i) <= db.student_ct)
    if (db.students[iter->i].active == 1)
      break;
  
  return (iter->i >= db.student_ct) ? NULL : iter;
}

void abort_student_iteration(struct student_iterator *iter) {
  iter->active = 0;
}

const char *student_name(const struct student_iterator *iter) {
  return db.students[iter->i].name;
}

int student_enrollment_year(const struct student_iterator *iter) {
  return db.students[iter->i].enroll_year;
}


