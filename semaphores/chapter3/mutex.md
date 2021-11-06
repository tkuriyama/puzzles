
```

Puzzle: Add semaphores to the following example to enforce mutual exclusion to the shared variable count.

Thread A
1 count = count + 1

Thread B
1 count = count + 1
```

Create a semaphore `mutex = 1`. Whichever thread first waits will decrement the mutex, blocking the other thread. Once the write is done, signalling releases the mutex.

**Thread A**

1. mutex.wait()
2. count += 1
3. mutex.signal()


**Thread B**

1. mutex.wait()
2. count += 1
3. mutex.signal()
