## Reusable Barrier

```

Puzzle: Rewrite the barrier solution so that after all the threads have passed through, the turnstile is locked again.
```


## Solution

count = 0
mutex = Semaphore(1)
turnstile1 = Semaphore(0)
turnstile2 = Semaphore(1)


**Thread**

```
mutex.wait()
count += 1
if count == n:
    turnstile2.wait()       # run once -- now locked (0)
    turnstile1.signal()     # run once -- now open (1)
mutex.signal()

turnstile1.wait()
turnstile1.signal()         # after all threads, turnstile1 is open (1)

# critical point 

mutex.wait()
count -= 1
if count == 0;
    turnstile1.wait()         # run once -- now locked (0)
    turnstile2.signal()       # run once -- now open (1)
mutex.signal()

turnstile2.wait()
turnstile2.signal()            # after all threads, turnstile is opne (1)
```

## Implementation

[See code](https://github.com/tkuriyama/puzzles/blob/master/semaphores/elm/src/exercises/ReusableBarrier.elm)

```elm
RB.program2 |> S.run
Completed 
( (0,2)
, Dict.fromList 
    [("mutex",1),("turnstile1",0),("turnstile2",1)]
, [ [Waiting "mutex" -1,Unblocked,Evaluated ("",(2,2)),Signaled "turnstile1" 0,NoWait "turnstile2" 0,Signaled "mutex" 1,NoWait "turnstile1" 0,Signaled "turnstile1" 1,Evaluated ("Critical Point",(1,2)),Waiting "mutex" -1,Unblocked,Evaluated ("Critical Point",(0,2)),NoWait "turnstile1" 0,Signaled "turnstile2" 0,Signaled "mutex" 1,NoWait "turnstile2" 0,Signaled "turnstile2" 1]
  , [NoWait "mutex" 0,Evaluated ("",(1,2)),NoAction,NoAction,Signaled "mutex" 0,Waiting "turnstile1" -1,Unblocked,Signaled "turnstile1" 1,Evaluated ("Critical Point",(2,2)),NoWait "mutex" 0,Evaluated ("Critical Point",(1,2)),NoAction,NoAction,Signaled "mutex" 0,Waiting "turnstile2" -1,Unblocked,Signaled "turnstile2" 1]
  ]
 )
```

Unfortutely, the exhaustive program produces too many branches to be executed by the Elm REPL.
