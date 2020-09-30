#ifndef COURSEDB_H_INCLUDED
#define COURSEDB_H_INCLUDED

#include <stdlib.h>

/** Database Typedefs
 **/
// Database Definition

typedef struct {
  int pkey;
  int id;
  char title[60];
  int year;
  char semester;
  int active;
} Course;

typedef struct {
  int pkey;
  int id;
  char name[30];
  int enroll_year[4];
  int active;
} Student;

typedef struct {
  int pkey;
  int id;
  Course *course;
  Student *student;
  int active;
} Enrollment;

typedef struct {
  int initialized;
  Course *courses;
  int course_ct, course_max_ct;
  Student *students;
  int student_ct, student_max_ct;
  Enrollment *enrollments;
  int enrollment_ct, enrollment_max_ct;
} Database;

/** Initializes the database.
 *
 *  Return a boolean values indicating whether the operation was
 *  successful.
*/
extern int init_database();

/** Clear the database.
 *
 *  Clear all tables, releases all resources allocated by the
 *  database.
 * 
 *  Return a boolean values indicating whether the operation was
 *  successful.
*/
extern int clear_database();

/** Save the database tables into files.
 *
 *  Each table goes to a separate file.  The file names are defined by
 *  the given prefix, with each table having a different suffix.  The
 *  suffixes are "-courses", "-students", and "-enrollment" for the
 *  three tables, respectively.  This function returns a non-zero
 *  value when the operation is successful, or 0 on error.
*/
extern int save_tables(const char * prefix);

/** Load the database tables from files.
 *
 *  Each table is read from a separate file as described above.  This
 *  function returns a non-zero value when the operation is
 *  successful, or <code>0</code> on error.
*/
extern int load_tables(const char * prefix);
    
/** Add a course to the database.
 *
 *  Returns a boolean values indicating whether the operation was
 *  successful.
*/
extern int add_course(int id, const char * title, int year, char semester);
    
/** Delete a course from the database
 *
 *  Return a boolean values indicating whether the operation was
 *  successful.
*/
extern int delete_course(int id);
    
/** Add a student to the database.
 *
 *  Returns a boolean values indicating whether the operation was
 *  successful.
*/
extern int add_student(int id, const char * name, int first_year);
    
/** Delete a course from the database.
 * 
 *  Returns a boolean values indicating whether the operation was
 *  successful.
*/
extern int delete_student(int id);

/** Enroll a student into a course.
 * 
 *  Returns a boolean values indicating whether the operation was
 *  successful.
*/
extern int enroll_student(int student_id, int course_id);

/** Cancels the enrollment of a student into a course.
 * 
 *  Returns a boolean values indicating whether the operation was
 *  successful.
*/
extern int cancel_enrollment(int student_id, int course_id);

/** Opaque structures defining an iterator over sets of students.
*/
struct student_iterator;

/** Next student.
 * 
 *  This operation may invalidate the object pointed by the given
 *  iterator. 
 *
 *  Returns the null pointer when there are no more courses.
*/
extern struct student_iterator * next_student(struct student_iterator *);

/** Terminates iteration.
 * 
 *  This operation may invalidate the object pointed by the given
 *  iterator. 
*/
extern void abort_student_iteration(struct student_iterator *);

/** Return the name of a student.
*/
extern const char * student_name(const struct student_iterator *);

/** Return the enrollment year for a student.
*/
extern int student_enrollment_year(const struct student_iterator *);

/** Opaque structures defining an iterator over sets of courses.
*/
struct course_iterator;

/** Next course.
 * 
 *  This operation may invalidate the object pointed by the given
 *  iterator.
 *
 *  Returns the null pointer when there are no more courses.
*/
extern struct course_iterator * next_course(struct course_iterator *);

/** Terminates iteration.
 * 
 *  This operation may invalidate the object pointed by the given
 *  iterator. 
*/
extern void abort_course_iteration(struct course_iterator *);

/** Return the title of a course.
*/
extern const char * course_title(const struct course_iterator *);

/** Return the year of a course.
*/
extern int course_year(const struct course_iterator *);

/** Return the semester of a course.
*/
extern char course_semester(const struct course_iterator *);

/** Return an iterator over the courses.
 * 
 *  Returns the null pointer when there are no courses in the
 *  database.
*/
extern struct course_iterator * courses();

/** Return an iterator over the courses into which a given student is
 *  enrolled.
 * 
 *  Returns the null pointer when there are no courses.
*/
extern struct course_iterator * student_courses(int student_id);

/** Return an iterator over the courses.
 * 
 *  Returns the null pointer when there are no courses in the
 *  database.
*/
extern struct student_iterator * students();

/** Return an iterator over the students enrolled into a given course.
 * 
 *  Returns the null pointer when there are no students.
*/
extern struct student_iterator * course_students(int course_id);

#endif

/* DB States */ 

extern void print_stats();
