module TestReusableBarrier exposing (..)

import Dict
import Exercises.ReusableBarrier as RB
import Expect
import Internal.Utils as Utils
import Semaphore
import Test exposing (..)



--------------------------------------------------------------------------------


testBarrier : Test
testBarrier =
    describe
        "Test reusable barrier programs run as expected"
        [ test
            "Test reusable barrier 2 threads"
            (\_ ->
                RB.program2
                    |> Semaphore.run
                    |> Utils.completedState
                    |> Expect.equal (Just ( 0, 2 ))
            )
        , test
            "Test reusable barrier 3 threads"
            (\_ ->
                RB.program3
                    |> Semaphore.run
                    |> Utils.completedState
                    |> Expect.equal (Just ( 0, 3 ))
            )
        , test
            "Test semaphores after running 3 threads"
            (\_ ->
                RB.program3
                    |> Semaphore.run
                    |> Utils.completedValue
                    |> Maybe.map Utils.second
                    |> Expect.equal (Just <| Dict.fromList [ ( "mutex", 1 ), ( "turnstile1", 0 ), ( "turnstile2", 1 ) ])
            )
        , test
            "Test semaphores after running 5 threads"
            (\_ ->
                RB.program3
                    |> Semaphore.run
                    |> Utils.completedValue
                    |> Maybe.map Utils.second
                    |> Expect.equal (Just <| Dict.fromList [ ( "mutex", 1 ), ( "turnstile1", 0 ), ( "turnstile2", 1 ) ])
            )
        ]
