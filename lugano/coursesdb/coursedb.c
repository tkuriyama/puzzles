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
  db.initialized = 0;  
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

int active_enrollments() {
  int ct = 0;
  for (int i=0; i<db.enrollment_ct; i++)
    if (db.enrollments[i].active) ct++;
  return ct;
}

void print_stats() {
  printf("\n--- Database Stats\n");
  printf("Course ct / active / max:     %d / %d / %d\n",
	 db.course_ct, active_courses(), db.course_max_ct);
  printf("Student ct / active / max:    %d / %d / %d\n",
	 db.student_ct, active_students(), db.student_max_ct);
  printf("Enrollment ct / active / max: %d / %d / %d\n",
	 db.enrollment_ct, active_enrollments(), db.enrollment_max_ct);
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
  int student_id;
  int active;
};
struct course_iterator course_iter = { .i = 0, .active = 0 };  

struct course_iterator *courses() {
  course_iter.active = 1;
  course_iter.student_id = -1;  
  course_iter.i = 0;
  return next_course(&course_iter);
}

struct course_iterator *student_courses(int student_id) {
  course_iter.active = 1;
  course_iter.student_id = student_id;
  
  course_iter.i = -1;
  return next_course(&course_iter);
}

struct course_iterator *next_course(struct course_iterator *iter) {
  if (!iter->active) return NULL;
  
  while(++(iter->i) <= db.course_ct){ 
    Course course = db.courses[iter->i];
    if (course.active  == 1 &&
	(iter->student_id == -1 ||
	 find_enrollment(iter->student_id, course.id) > -1))
      break;
  }

  struct course_iterator *p = iter;
  if (iter->i >= db.course_ct) {
    abort_course_iteration(iter);
    p = NULL;
  }
  
  return p;
}

void abort_course_iteration(struct course_iterator *iter) {
  iter->active = 0;
}

int course_id(const struct course_iterator *iter) {
  return db.courses[iter->i].id;
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
  int course_id;
};

struct student_iterator student_iter = { .i = 0, .active = 0 };  

struct student_iterator *students() {
  student_iter.active = 1;
  student_iter.course_id = -1;
  student_iter.i = -1;
  return next_student(&student_iter);
}

struct student_iterator * course_students(int course_id) {
  student_iter.active = 1;
  student_iter.course_id = course_id;
  student_iter.i = -1;
  return next_student(&student_iter);
}

struct student_iterator *next_student(struct student_iterator *iter) {
  if (!iter->active) return NULL;

  // incremenet until we find a valid record 
  while(++(iter->i) <= db.student_ct) { 
    Student student = db.students[iter->i];
    if (student.active && 
        (iter->course_id == -1 ||
	 find_enrollment(student.id, iter->course_id) > -1))
      break;
  }
    
  struct student_iterator *p = iter;
  if (iter->i >= db.student_ct) {
    abort_student_iteration(iter);
    p = NULL;
  }     
  return p;
}

void abort_student_iteration(struct student_iterator *iter) {
  iter->active = 0;
}

int student_id(const struct student_iterator *iter) {
  return db.students[iter->i].id;
}

const char *student_name(const struct student_iterator *iter) {
  return db.students[iter->i].name;
}

int student_enrollment_year(const struct student_iterator *iter) {
  return db.students[iter->i].enroll_year;
}

/*** Enrollment ***/

int valid_course(int course_id) {
  int valid = 0;
  for (int i=0; i<db.course_ct; ++i)
    if (db.courses[i].active && db.courses[i].id == course_id)
      valid = 1;
  return valid;
}

int valid_student(int student_id) {
  int valid = 0;
  for (int i=0; i<db.student_ct; ++i)
    if (db.students[i].active && db.students[i].id == student_id)
      valid = 1;
  return valid;
}

int find_enrollment(int student_id, int course_id) {
  int pkey = -1;
  Enrollment record;
  for (int i=0; i<db.enrollment_ct; ++i) {
    record = db.enrollments[i];
    if (record.active &&
	record.student_id == student_id &&
	record.course_id == course_id)
      pkey = record.pkey;
  }
  return pkey;
}

int enroll_student(int student_id, int course_id) {
  if (db.enrollment_ct + 1 == db.enrollment_max_ct) {
    db.enrollment_max_ct += db.enrollment_max_ct / 2;
    db.enrollments = (Enrollment *)erealloc(db.enrollments, db.enrollment_max_ct);  
  }

  int status = 0;
  if (valid_student(student_id) && valid_course(course_id) &&
      find_enrollment(student_id, course_id) == -1) {
    // current record
    db.enrollments[db.enrollment_ct].student_id = student_id;
    db.enrollments[db.enrollment_ct].course_id = course_id;
    db.enrollments[db.enrollment_ct].active = 1;
    ++db.enrollment_ct;
    // new record
    db.enrollments[db.enrollment_ct].pkey = db.enrollment_ct;
    status = 1;
  }
  
  return status;
}

int cancel_enrollment(int student_id, int course_id) {
  int pkey = find_enrollment(student_id, course_id);
  if (pkey >= 0)
    db.enrollments[pkey].active = 0;
  
  return (pkey >= 0);
}

// Load and Save Tables

char *concat(const char *prefix, const char *suffix, char *fname, ssize_t sz) {
  strcpy(fname, "\0");
  strlcat(fname, prefix, sz);
  strlcat(fname, suffix, sz);
  return fname;
}

int save_courses(const char *fname) {
  int status = 0;
  FILE *fptr = fopen(fname,"w");
  for (int i=0; i<+db.course_ct; i++) {
    Course course = db.courses[i];
    if (course.active) 
      fprintf(fptr, "%d,%d,%s,%d,%c\n", course.pkey, course.id,
	      course.title, course.year, course.semester); 
  }
  
  fclose(fptr);
  return status;
}

int save_tables(const char * prefix) {
  int status = 0;
  ssize_t sz = 30;
  char fname[sz];  
  status += save_courses(concat(prefix, "-courses.csv", fname, sz));
  //status += save_students(prefix, "-students.csv");
  //status += save_enrollments(prefix, "enrollment.csv");    
  return (status == 3);  
}
