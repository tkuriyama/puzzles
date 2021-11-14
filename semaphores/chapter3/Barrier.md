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


## Implementation

[See code](https://github.com/tkuriyama/puzzles/blob/master/semaphores/elm/src/exercises/Barrier.elm)

```elm
> Barrier.program2 |> S.run

Completed 
    ( (2,2)
    , Dict.fromList [("barrier",2),("mutex",2)]
    , [ [Waiting "mutex" 0,Unblocked,Evaluated ("",(2,2)),Signaled "mutex" 2,Signaled "barrier" 1,NoWait "barrier" 1,Signaled "barrier" 2,Evaluated ("Critical Point",(2,2))]
    ,   [NoWait "mutex" 0,Evaluated ("",(1,2)),Signaled "mutex" 1,Signaled "barrier" 1,NoWait "barrier" 0,Signaled "barrier" 2,Evaluated ("Critical Point",(2,2))]
      ]
    )


> Barrier.program3 |> S.run

Completed 
    ( (3,3)
    , Dict.fromList [("barrier",3),("mutex",3)]
    , [ [Waiting "mutex" 0,Unblocked,Evaluated ("",(3,3)),Signaled "mutex" 3,Signaled "barrier" 2,NoWait "barrier" 2,Signaled "barrier" 3,Evaluated ("Critical Point",(3,3))]
      , [Waiting "mutex" 0,Unblocked,Evaluated ("",(2,3)),Signaled "mutex" 2,Signaled "barrier" 1,NoWait "barrier" 1,Signaled "barrier" 3,Evaluated ("Critical Point",(3,3))]
      , [NoWait "mutex" 0,Evaluated ("",(1,3)),Signaled "mutex" 1,NoAction,Waiting "barrier" 0,Unblocked,Signaled "barrier" 2,Evaluated ("Critical Point",(3,3))]
      ]
    )


> Barrier.program2 |> SE.run |> SE.summarize

{ completed = { count = 4460
, outputStats = 
    { avgThreadLength = 7
    , maxThreadLength = 9
    , minThreadLength = 7 }
, semaphoreDicts = 
    [ Dict.fromList [("barrier",2),("mutex",2)]
    , Dict.fromList [("barrier",1),("mutex",2)]
    , Dict.fromList [("barrier",2),("mutex",1)]
    , Dict.fromList [("barrier",1),("mutex",1)]]
    , sharedStates = Just [(2,2)]
    , uniqueCount = 80 }
, deadlocked = { count = 0, outputStats = { avgThreadLength = 0, maxThreadLength = 0, minThreadLength = 0 }, semaphoreDicts = [], sharedStates = Nothing, uniqueCount = 0 }
, invalid = { count = 0, outputStats = { avgThreadLength = 0, maxThreadLength = 0, minThreadLength = 0 }, semaphoreDicts = [], sharedStates = Nothing, uniqueCount = 0 }
, totalCount = 4460
, totalUniqueCount = 80 
}
```
