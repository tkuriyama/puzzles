```
Puzzle: Generalize the signal pattern so that it works both ways. Thread A has
to wait for Thread B and vice versa. In other words, given this code

Thread A
1 statement a1
2 statement a2

Thread B
1 statement b1
2 statement b2

we want to guarantee that a1 happens before b2 and b1 happens before a2. In
writing your solution, be sure to specify the names and initial values of your
semaphores (little hint there).

Your solution should not enforce too many constraints. For example, we
don’t care about the order of a1 and b1. In your solution, either order should
be possible.
```

Since A and B need to rendezvous after their respective first statements (in any order), they should signal completion immediately after the first statement and await the other to proceed. 

**Thread A**

1. statement a1
2. a1Done.signal()
3. b1Done.wait()
4. statement a2


**Thread B*8

1. statement b1
2. b1Done.signal()
3. a1Done.wait()
4. statement b2

