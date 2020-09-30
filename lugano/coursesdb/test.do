# Make sure everything has been built before we start
redo-ifchange all

# Ensure that the hello program, when run, says
# hello like we expect.
if ./main_comp | grep -i 'Error' >/dev/null; then
    echo "Something Broke" >&2
    exit 1
else
    echo "Success" >&2
fi

if leaks -atExit -- ./main_comp | grep LEAK:; then
    echo "There's a memory leak" >&2
    exit 1
else
    echo "No memory leak" >&2
    exit 0
fi