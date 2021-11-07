module Internal.Types exposing (..)

import Dict



--------------------------------------------------------------------------------


type ConcurrentProgram a b
    = Running (ActiveProgram a b)
    | Deadlock (List (Output a b))
    | Completed ( b, List (Output a b) )
    | Invalid (List (Output a b))


type alias ActiveProgram a b =
    { sharedState : b
    , activeThreads : List (ThreadPair a b)
    , blockedThreads : List (ThreadPair a b)
    , outputs : List (Output a b)
    , semaphores : Dict.Dict String Semaphore
    }


type alias Semaphore =
    Int


type alias ThreadPair a b =
    ( Thread a b, Output a b )


type alias Thread a b =
    ( List (Statement a b), a )


type Statement a b
    = Expression (( a, b ) -> ( a, b ))
    | Signal String
    | Wait String


type alias Output a b =
    List (OutputMsg a b)


type OutputMsg a b
    = Evaluated ( a, b )
    | Signaled String Semaphore
    | NoWait String Semaphore
    | Waiting String Semaphore
    | InvalidSemaphore String
    | Unblocked
