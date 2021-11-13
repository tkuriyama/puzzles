module Exercises.Barrier exposing (..)

{-| Modelling the barrier exercises.
-}

import Internal.Types exposing (..)
import Semaphore exposing (..)



--------------------------------------------------------------------------------


program2 =
    init
        ( 0, 2 )
        [ ( thread, "" ), ( thread, "" ) ]
        [ ( "mutex", 1 ), ( "barrier", 0 ) ]


program2Deadlock =
    init
        ( 0, 2 )
        [ ( threadDeadlock, "" ), ( threadDeadlock, "" ) ]
        [ ( "mutex", 1 ), ( "barrier", 0 ) ]


program3 =
    init
        ( 0, 3 )
        [ ( thread, "" ), ( thread, "" ), ( thread, "" ) ]
        [ ( "mutex", 1 ), ( "barrier", 0 ) ]



--------------------------------------------------------------------------------


thread =
    [ SemaphoreStatement
        (\_ -> Wait "mutex")
    , Expression
        (\( threadState, ( count, n ) ) ->
            ( threadState, ( count + 1, n ) )
        )
    , SemaphoreStatement
        (\_ -> Signal "mutex")
    , SemaphoreStatement
        (\( _, ( count, n ) ) ->
            if count == n then
                Signal "barrier"

            else
                Pass
        )
    , SemaphoreStatement
        (\_ -> Wait "barrier")
    , SemaphoreStatement
        (\_ -> Signal "barrier")
    , Expression
        (\( threadState, sharedState ) ->
            ( "Critical Point", sharedState )
        )
    ]


threadDeadlock =
    [ SemaphoreStatement
        (\_ -> Wait "mutex")
    , Expression
        (\( threadState, ( count, n ) ) ->
            ( threadState, ( count + 1, n ) )
        )
    , SemaphoreStatement
        (\_ -> Signal "mutex")
    , SemaphoreStatement
        (\( _, ( count, n ) ) ->
            if count == n then
                Signal "barrier"

            else
                Pass
        )
    , SemaphoreStatement
        (\_ -> Wait "barrier")
    , Expression
        (\( threadState, sharedState ) ->
            ( "Critical Point", sharedState )
        )
    ]
