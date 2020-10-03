# Make sure everything has been built before we start
redo-ifchange all

# check for Error string...
if ./main_comp | grep 'Error' >/dev/null; then
    echo "Something Broke" >&2
    exit 1
else
    echo "Success" >&2
fi

# diff sample outputs...
if diff -q TestIO-Diff1-courses.csv TestIO-Diff2-courses.csv > /dev/null; then 
   echo "Courses diff ok" >&2
else
   echo "Diff found between Diff1 and Diff2 courses.csv" >&2
   exit 1
fi

if diff -q TestIO-Diff1-students.csv TestIO-Diff2-students.csv > /dev/null; then 
   echo "Students diff ok" >&2
else
   echo "Diff found between Diff1 and Diff2 students.csv" >&2
   exit 1
fi

if diff -q TestIO-Diff1-enrollments.csv TestIO-Diff2-enrollments.csv > /dev/null; then 
   echo "Enrollment diff ok" >&2
else
   echo "Diff found between Diff1 and Diff2 enrollment.csv" >&2
   exit 1
fi



# check for leaks...
if leaks -atExit -- ./main_comp | grep LEAK:; then
    echo "There's a memory leak" >&2
    exit 1
else
    echo "No memory leak" >&2
    exit 0
fi