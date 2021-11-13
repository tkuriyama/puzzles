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
    , SemaphoreStatement (\( _, _ ) -> Signal "a1Done")
    , SemaphoreStatement (\( _, _ ) -> Wait "b1Done")
    , Expression (\( _, () ) -> ( "a2", () ))
    ]


thread2a =
    [ Expression (\( _, () ) -> ( "b1", () ))
    , SemaphoreStatement (\( _, _ ) -> Signal "b1Done")
    , SemaphoreStatement (\( _, _ ) -> Wait "a1Done")
    , Expression (\( _, () ) -> ( "b2", () ))
    ]


thread1b =
    [ Expression (\( _, () ) -> ( "a1", () ))
    , SemaphoreStatement (\( _, _ ) -> Wait "b1Done")
    , SemaphoreStatement (\( _, _ ) -> Signal "a1Done")
    , Expression (\( _, () ) -> ( "a2", () ))
    ]


thread2b =
    [ Expression (\( _, () ) -> ( "b1", () ))
    , SemaphoreStatement (\( _, _ ) -> Wait "a1Done")
    , SemaphoreStatement (\( _, _ ) -> Signal "b1Done")
    , Expression (\( _, () ) -> ( "b2", () ))
    ]
