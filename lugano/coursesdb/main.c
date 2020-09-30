#include <stdlib.h>
#include <stdio.h>
#include "coursedb.c"
#include "coursedb.h"

void ewrap(int status, char * name) {
  if (status == 0)
    printf("Error: %s process returned error code\n", name);
  else 
    printf("Success: %s\n", name); 
  return;
}

int main() {
  ewrap(init_database(), "Init Database");
  print_stats();

  ewrap(add_course(101, "Econ 101", 2020, 'F'), "Adding course");
  print_stats();

  ewrap(delete_course(101), "Delete course");
  print_stats();
  
  ewrap(clear_database(), "Clear Database");
  printf("All done!\n\n");
  return 0;
}
