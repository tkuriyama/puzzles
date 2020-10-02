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

void ewrap_error(int status, char * name) {
  if (status == 0)
    printf("Success: %s expected error code %d\n", name, status);
  else 
    printf("Error: %s expected error code of 1\n", name); 
  return;
}

void print_course(struct course_iterator *iter) { 
  printf("%d | %s | %d | %c\n",
	 course_id(iter),
	 course_title(iter),
	 course_year(iter),
	 course_semester(iter));
}

void print_student(struct student_iterator *iter) {
  printf("%d | %s | %d\n",
	 student_id(iter),
	 student_name(iter),
	 student_enrollment_year(iter));
}

void expect(int a, int b, char *msg) {
  if (a != b) printf("Error: %s\n", msg);
}


// Tests

void test_empty_roundtrip() {
  printf("\n============== Init Destroy Roundtrip ==============\n\n");
  
  ewrap(init_database(), "Init Database");
  print_stats();

  ewrap(clear_database(), "Clear Database");
}

void test_add_delete_course() {
  printf("\n============== Add and Delete Courses ==============\n\n");
  
  ewrap(init_database(), "Init Database");
  print_stats();

  ewrap(add_course(101, "Econ 101", 2020, 'F'), "Adding course 101");
  ewrap(add_course(110, "Econ 110", 2020, 'F'), "Adding course 110");  
  print_stats();

  ewrap(delete_course(101), "Delete course 101");
  ewrap_error(delete_course(999), "Delete non-existent course ID");  
  print_stats();
  
  ewrap(clear_database(), "Clear Database");
}

void test_add_delete_student() {
  printf("\n============== Add and Delete Students ==============\n\n");
  
  ewrap(init_database(), "Init Database");
  print_stats();

  ewrap(add_student(1, "John Wayne", 2020), "Adding John");
  ewrap(add_student(2, "Sara Moss", 2020), "Adding Sara");  
  print_stats();

  ewrap(delete_student(1), "Delete Student 1");
  ewrap_error(delete_student(999), "Delete non-existent student ID"); 
  print_stats();
  
  ewrap(clear_database(), "Clear Database");
}


void test_realloc() {
 printf("\n============== Test Realloc ==============\n\n");
  
  ewrap(init_database(), "Init Database");
  
  printf("Adding 20 courses...\n");
  for (int i=0; i<20; ++i)
    add_course(i, "Econ Dummy", 2020, 'F');

  printf("Adding 30 students...\n");
  for (int i=0; i<30; ++i) 
    add_student(i, "Student Dummy", 2020);

  printf("Adding 100 enrollments...\n");
  for (int i=0; i<10; ++i)
    for (int j=0; j<10; ++j) 
      enroll_student(i, j);

  print_stats();
  expect(db.course_ct, 20, "Course count unexpected");
  expect(db.student_ct, 30, "Student count unexpected");  
  expect(db.enrollment_ct, 100, "Enrollment count unexpected");
  ewrap(clear_database(), "Clear Database");
}

void test_iterators() {
  printf("\n============== Test Iterators ==============\n\n");
  
  ewrap(init_database(), "Init Database");

  ewrap(add_course(101, "Econ 101", 2020, 'F'), "Adding course 101");
  ewrap(add_course(110, "Econ 110", 2020, 'F'), "Adding course 110");
  ewrap(add_course(111, "Econ 111", 2020, 'F'), "Adding course 111");
  ewrap(delete_course(110), "Delete course 110");
  ewrap(add_student(1, "John Doe", 2019), "Adding student 1");
  ewrap(add_student(2, "Sam Smith", 2019), "Adding student 2");
  ewrap(add_student(3, "Adele Jones", 2019), "Adding student 3");  
  print_stats();
 
  printf("Iterating through courses...\n");
  struct course_iterator *iterC = courses();
  while (iterC != NULL){
    print_course(iterC);
    iterC = next_course(iterC);
  }

  printf("Iterating through students...\n");
  struct student_iterator *iterS = students();
  while (iterS != NULL){
    print_student(iterS);
    iterS = next_student(iterS);
  }


  printf("\nIterating through courses, abort after 1 course...\n");
  iterC = courses();
  print_course(iterC);
  abort_course_iteration(iterC);
  iterC = next_course(iterC);
  if (iterC != NULL) print_course(iterC);

  printf("\nIterating through students, abort after 1 student...\n");
  iterS = students();
  print_student(iterS);
  abort_student_iteration(iterS);
  iterS = next_student(iterS);
  if (iterS != NULL) print_student(iterS);
  
  ewrap(clear_database(), "Clear Database");
}

void test_enrollments() {
 printf("\n============== Test Enrollments ==============\n\n");
  
  ewrap(init_database(), "Init Database");
  
  ewrap(add_course(101, "Econ 101", 2020, 'F'), "Adding course 101");
 ewrap(add_course(110, "Econ 110", 2020, 'F'), "Adding course 110");
  ewrap(add_course(111, "Econ 111", 2020, 'F'), "Adding course 111");
  ewrap(add_student(1, "John Doe", 2019), "Adding student 1");
  ewrap(add_student(2, "Sam Smith", 2019), "Adding student 2");
  ewrap(add_student(3, "Adele Jones", 2019), "Adding student 3");
  
  ewrap(enroll_student(1, 101), "Enroll 1 in 101");
  ewrap_error(enroll_student(1, 101), "Dup enroll 1 in 101 -- should fail");
  ewrap(enroll_student(1, 110), "Enroll 1 in 110");
  ewrap(enroll_student(2, 110), "Enroll 2 in 110");
  ewrap(enroll_student(3, 111), "Enroll 3 in 111");
  ewrap(cancel_enrollment(1, 110), "Unenroll 1 from 110");
  ewrap(cancel_enrollment(1, 110), " Dup unenroll 1 from 110 -- OK");
  ewrap_error(cancel_enrollment(999, 999), " Unenroll 999 from 999 -- should fail");

  print_stats();
  expect(db.enrollment_ct, 4, "Enrollment count unexpected");  
  
  for (int k=1; k<=3; ++k) {
    printf("\nIterate over courses for student %d...\n", k);
    struct course_iterator *iterC = student_courses(k);
    while (iterC != NULL){
      print_course(iterC);
      iterC = next_course(iterC);
    }
  }
  
  printf("\nIterate over students enrolled in 110...\n");
  struct student_iterator *iterS = course_students(110);
  while (iterS != NULL){
    print_student(iterS);
    iterS = next_student(iterS);
  }
  
  ewrap(clear_database(), "Clear Database");
}

int main() {
  test_empty_roundtrip();

  test_add_delete_course();
  test_add_delete_student();
  
  test_realloc();
  test_iterators();
  test_enrollments();
  
  printf("\nAll done!\n\n");
  return 0;
}
