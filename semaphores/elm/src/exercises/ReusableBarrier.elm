module Exercises.ReusableBarrier exposing (..)

{-| Modelling the barrier exercises.
-}

import Internal.Types exposing (..)
import Semaphore exposing (..)



--------------------------------------------------------------------------------


program2 =
    init
        ( 0, 2 )
        [ ( thread, "" ), ( thread, "" ) ]
        [ ( "mutex", 1 )
        , ( "turnstile1", 0 )
        , ( "turnstile2", 1 )
        ]


program3 =
    init
        ( 0, 3 )
        [ ( thread, "" ), ( thread, "" ), ( thread, "" ) ]
        [ ( "mutex", 1 )
        , ( "turnstile1", 0 )
        , ( "turnstile2", 1 )
        ]



--------------------------------------------------------------------------------


thread =
    open1AndClose2 ++ close1AndOpen2


open1AndClose2 =
    [ SemaphoreStatement
        (\_ -> Wait "mutex")
    , Expression
        (\( threadState, ( count, n ) ) ->
            ( threadState, ( count + 1, n ) )
        )
    , SemaphoreStatement
        (\( _, ( count, n ) ) ->
            if count == n then
                Signal "turnstile1"

            else
                Pass
        )
    , SemaphoreStatement
        (\( _, ( count, n ) ) ->
            if count == n then
                Wait "turnstile2"

            else
                Pass
        )
    , SemaphoreStatement
        (\_ -> Signal "mutex")
    , SemaphoreStatement
        (\_ -> Wait "turnstile1")
    , SemaphoreStatement
        (\_ -> Signal "turnstile1")
    , Expression
        (\( threadState, sharedState ) ->
            ( "Critical Point", sharedState )
        )
    ]


close1AndOpen2 =
    [ SemaphoreStatement
        (\_ -> Wait "mutex")
    , Expression
        (\( threadState, ( count, n ) ) ->
            ( threadState, ( count - 1, n ) )
        )
    , SemaphoreStatement
        (\( _, ( count, n ) ) ->
            if count == 0 then
                Wait "turnstile1"

            else
                Pass
        )
    , SemaphoreStatement
        (\( _, ( count, n ) ) ->
            if count == 0 then
                Signal "turnstile2"

            else
                Pass
        )
    , SemaphoreStatement
        (\_ -> Signal "mutex")
    , SemaphoreStatement
        (\_ -> Wait "turnstile2")
    , SemaphoreStatement
        (\_ -> Signal "turnstile2")
    ]
