## Barrier

```
Puzzle: Generalize the rendezvous solution. Every thread should run the following code:

Barrier code
1 rendezvous
2 critical point
```

## Solution

mutex = Semaphore(1)
barrier = Semaphore(0)
count = 0

**Thread**

mutex.wait()
count = count + 1
mutex.signal()
if count == n: barrier.signal()
barrier.wait()
barrier.signal()
critical point
