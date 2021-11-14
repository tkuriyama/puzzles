module Internal.Types exposing (..)

import Dict



--------------------------------------------------------------------------------
-- Programs and Semaphores


type ConcurrentProgram a b
    = Running (ActiveProgram a b)
    | Deadlock ( SemaphoreDict, List (Output a b) )
    | Completed ( b, SemaphoreDict, List (Output a b) )
    | Invalid ( SemaphoreDict, List (Output a b) )


type alias ActiveProgram a b =
    { sharedState : b
    , activeThreads : List (ThreadPair a b)
    , blockedThreads : List (ThreadPair a b)
    , outputs : List (Output a b)
    , semaphores : SemaphoreDict
    }


type alias Semaphore =
    Int


type alias ThreadPair a b =
    ( Thread a b, Output a b )


type alias Thread a b =
    ( List (Statement a b), a )


type Statement a b
    = Expression (( a, b ) -> ( a, b ))
    | SemaphoreStatement (( a, b ) -> SemaphoreAction)


type SemaphoreAction
    = Signal String
    | Wait String
    | Pass


type alias SemaphoreDict =
    Dict.Dict String Semaphore


type alias Output a b =
    List (OutputMsg a b)


type OutputMsg a b
    = Evaluated ( a, b )
    | Signaled String Semaphore
    | NoWait String Semaphore
    | Waiting String Semaphore
    | Unblocked
    | NoAction
    | InvalidSemaphore String



--------------------------------------------------------------------------------
-- Program Output


type alias ExecutionSummary b =
    { totalCount : Int
    , totalUniqueCount : Int
    , completed : ResultsSummary b
    , deadlocked : ResultsSummary b
    , invalid : ResultsSummary b
    }


type alias ResultsSummary b =
    { count : Int
    , uniqueCount : Int
    , sharedStates : Maybe (List b)
    , semaphoreDicts : List SemaphoreDict
    , outputStats : OutputStats
    }


type alias OutputStats =
    { avgThreadLength : Int
    , minThreadLength : Int
    , maxThreadLength : Int
    }
