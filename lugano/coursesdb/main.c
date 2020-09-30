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

// Main

int main() {
  ewrap(init_database(), "Init Database");
  //ewrap(add_course(101,"Econ 101", 2020, 'F'), "Adding courses");
  print_stats();
  ewrap(clear_database(), "Clear Database");
  printf("All done!\n\n");
  return 0;
}
