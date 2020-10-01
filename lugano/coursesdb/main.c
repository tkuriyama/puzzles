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


// Tests

void test_empty_roundtrip() {
  printf("\n============== Init Destroy Roundtrip ==============\n\n");
  
  ewrap(init_database(), "Init Database");
  print_stats();

  ewrap(clear_database(), "Clear Database");

  printf("\nAll done!\n\n");  
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

  printf("\nAll done!\n\n");
}

int main() {
  test_empty_roundtrip();
  test_add_delete_course();
  return 0;
}
