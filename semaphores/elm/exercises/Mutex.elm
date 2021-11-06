module Mutex exposing (..)

{- } -}

import Semaphore exposing (..)



--------------------------------------------------------------------------------


programNormal1 =
    init
        0
        [ ( thread, "" ), ( thread, "" ) ]
        [ ( "mutex", 1 ) ]
        |> run


programNormal2 =
    init
        0
        (List.repeat 10 ( thread, "" ))
        [ ( "mutex", 1 ) ]
        |> run



--------------------------------------------------------------------------------


thread =
    [ Wait "mutex"
    , Expression (\( _, count ) -> ( "", count + 1 ))
    , Signal "mutex"
    ]
