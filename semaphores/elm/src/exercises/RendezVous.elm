module Exercises.RendezVous exposing (..)

{-| Modelling the rendez-vous exercise.
-}

import Internal.Types exposing (..)
import Semaphore exposing (..)



--------------------------------------------------------------------------------


programNormal1 =
    init
        ()
        [ ( thread1a, "" ), ( thread2a, "" ) ]
        [ ( "a1Done", 0 ), ( "b1Done", 0 ) ]


programNormal2 =
    init
        ()
        [ ( thread1a, "" ), ( thread2b, "" ) ]
        [ ( "a1Done", 0 ), ( "b1Done", 0 ) ]


programDeadlock =
    init
        ()
        [ ( thread1b, "" ), ( thread2b, "" ) ]
        [ ( "a1Done", 0 ), ( "b1Done", 0 ) ]



--------------------------------------------------------------------------------


thread1a =
    [ Expression (\( _, () ) -> ( "a1", () ))
    , Signal "a1Done"
    , Wait "b1Done"
    , Expression (\( _, () ) -> ( "a2", () ))
    ]


thread2a =
    [ Expression (\( _, () ) -> ( "b1", () ))
    , Signal "b1Done"
    , Wait "a1Done"
    , Expression (\( _, () ) -> ( "b2", () ))
    ]


thread1b =
    [ Expression (\( _, () ) -> ( "a1", () ))
    , Wait "b1Done"
    , Signal "a1Done"
    , Expression (\( _, () ) -> ( "a2", () ))
    ]


thread2b =
    [ Expression (\( _, () ) -> ( "b1", () ))
    , Wait "a1Done"
    , Signal "b1Done"
    , Expression (\( _, () ) -> ( "b2", () ))
    ]
