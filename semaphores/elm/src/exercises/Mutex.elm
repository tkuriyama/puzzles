module Exercises.Mutex exposing (..)

{- Modeling the mutex exercises. -}

import Internal.Types exposing (..)
import Semaphore exposing (..)



--------------------------------------------------------------------------------


sumTo2 =
    init
        0
        [ ( thread, "" ), ( thread, "" ) ]
        [ ( "mutex", 1 ) ]


sumTo10 =
    init
        0
        (List.repeat 10 ( thread, "" ))
        [ ( "mutex", 1 ) ]



--------------------------------------------------------------------------------


thread =
    [ SemaphoreStatement (\( _, _ ) -> Wait "mutex")
    , Expression (\( _, count ) -> ( "", count + 1 ))
    , SemaphoreStatement (\( _, _ ) -> Signal "mutex")
    ]
