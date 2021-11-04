```
1 for i in range (100):
2     temp = count
3     count = temp + 1


What is the largest possible value of count after all threads have completed?
What is the smallest possible value?
```

The largest possible value is count's initial value + 100 * 100, in the case where all threads are executed sequentially. 

The smallest value is...

- let's say there are 2 threads, which first read count's initial of 0 into their `temp` variables (line 2)
- the two threads then wait until all other threads have completed
- let's call the two threads A and B
- A is still paused on the first iteration of line 2 (`A.temp = 0`)
- B advances until it completes 99 iterations 
- A now completes line 3 of its first iteration (so `count = A.temp + 1 = 1`)
- B advances to line 2 of its final iteration (`B.temp = count = 1`)
- A advances to the end of its 100 iterations; `count` does not matter since B will never read it again
- Now B completes line 3 of its final iteration: `count = B.temp + 1 = 2`

So 2 is possible. (Is there a solution for 1?)
