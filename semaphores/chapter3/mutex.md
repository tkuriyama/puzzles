
## Problem 
```

Puzzle: Add semaphores to the following example to enforce mutual exclusion to the shared variable count.

Thread A
1 count = count + 1

Thread B
1 count = count + 1
```

## Solution 

Create a semaphore `mutex = 1`. Whichever thread first waits will decrement the mutex, blocking the other thread. Once the write is done, signaling releases the mutex.

**Thread A**

1. mutex.wait()
2. count += 1
3. mutex.signal()


**Thread B**

1. mutex.wait()
2. count += 1
3. mutex.signal()

## Implementation

[See code](https://github.com/tkuriyama/puzzles/blob/master/semaphores/elm/src/exercises/Mutex.elm)

```elm
> Mutex.sumTo2 |> S.run
Completed (2,[[Waiting "mutex" 0,Unblocked,Evaluated ("",2),Signaled "mutex" 2],[NoWait "mutex" 0,Evaluated ("",1),Signaled "mutex" 1]])

> Mutex.sumTo10 |> S.run
Completed (10,[...])


> Mutex.sumTo2 |> SE.run |> SE.summarize
{ completed = 
    { count = 26
    , outputStats = { avgThreadLength = 3
                    , maxThreadLength = 4
                    , minThreadLength = 3 }
    , sharedStates = Just [2]
    , uniqueCount = 4 
    }
, deadlocked = { count = 0, outputStats = { avgThreadLength = 0, maxThreadLength = 0, minThreadLength = 0 }, sharedStates = Nothing, uniqueCount = 0 }
, invalid = { count = 0, outputStats = { avgThreadLength = 0, maxThreadLength = 0, minThreadLength = 0 }, sharedStates = Nothing, uniqueCount = 0 }
, totalCount = 26
, totalUniqueCount = 4 
}
```
