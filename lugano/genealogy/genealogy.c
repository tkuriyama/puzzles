#include <stdlib.h>
#include <stdio.h>
#include <string.h>

const int BIRTHDAYSIZE = 12;
const int NAMESIZE = 40;

struct record {
  char birthday[BIRTHDAYSIZE];
  char name[NAMESIZE];
  struct record *father;
  struct record *mother;
};

/* safe malloc and realloc */

void *emalloc(size_t sz) {
   void *p;
   if((p = malloc(sz)) == NULL) abort();
   return p;
}

/* creating and destroying ancestry tree */

// the results are copied into struct, so ok to overwrite each time
char **parse(char *line, char *delim) {
  // strip tailing newline
  int len = strlen(line);
  if (line[len-1] == '\n')
    line[len-1] = '\0';
  
  char **ap;
  static char *argv[4];
  for (ap = argv; (*ap = strsep(&line, delim)) != NULL;) 
    if ((**ap != '\0') && (++ap >= &argv[4]))
      break;    
  return argv;
}

void populate(struct record *person, char *name) {
  strcpy(person->name, name);
  strcpy(person->birthday, "unknown");
  person->father = NULL;
  person->mother = NULL;
  return;
}

void insert(char **fields, struct record *persons) {
  struct record *father, *mother;
  father = emalloc(sizeof(struct record));
  mother = emalloc(sizeof(struct record));
  populate(father, fields[2]);
  populate(mother, fields[3]);
  
  // Found tree root or correct record
  if ((strcmp(persons->name, "") == 0) ||
      (strcmp(persons->name, fields[1]) == 0)) {
    strcpy(persons->name, fields[1]);
    strcpy(persons->birthday, fields[0]);
    persons->father = father;
    persons->mother = mother;
  }
  // 
  // Keep Looking
  else {
    if (persons->father != NULL) insert(fields, persons->father);
    if (persons->mother != NULL) insert(fields, persons->mother);
  }
  return;
}

void printTree(struct record *persons) {
  if (persons->father != NULL) printTree(persons->father);
  if (persons->father != NULL) printTree(persons->mother);  
  printf("Record: %s, %s\n", persons->name, persons->birthday);
  return;
}

void freeTree(struct record *persons) {
  if (persons->father != NULL) freeTree(persons->father);
  if (persons->father != NULL) freeTree(persons->mother);  
  free(persons);
  return;
}

/* Find Anceststry */

void showAncestry(struct record *persons, char *name, int print) { 

  if(strcmp(persons->name, name) == 0 || print > 0) {
    printf("%s, %s\n", persons->name, persons->birthday);
    if (persons->father != NULL) showAncestry(persons->father, persons->father->name, 1);
    if (persons->mother != NULL) showAncestry(persons->mother, persons->mother->name, 1);    
  }
  else {
    if (persons->father != NULL) showAncestry(persons->father, name, 0);
    if (persons->mother != NULL) showAncestry(persons->mother, name, 0);    
  }
  return;
}

/* } */
int main(int argc, char **argv) {

  FILE * f;
  char *line, **fields;
  size_t linecap = 0;

  struct record *persons;
  persons = emalloc(sizeof(struct record));
  strcpy(persons->name, "");
  
  f = fopen("ancestors.csv", "r");
  
  while(1) {
    if(getline(&line, &linecap, f) == -1) break;   
    fields = parse(line, ",");
    insert(fields, persons);
  }
  
  free(line);
  fclose(f);

  // find ancestor
  if (argc > 1) 
    showAncestry(persons, argv[1], 0);

  freeTree(persons);
  return 0;

  }
