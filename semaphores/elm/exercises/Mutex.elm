module Mutex exposing (..)

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
    [ Wait "mutex"
    , Expression (\( _, count ) -> ( "", count + 1 ))
    , Signal "mutex"
    ]
