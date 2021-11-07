module TestMutex exposing (..)

import Exercises.Mutex as Mutex
import Expect
import Internal.Utils as Utils
import Semaphore
import Test exposing (..)



--------------------------------------------------------------------------------


testMutex : Test
testMutex =
    describe
        "Test mutex programs run as expected"
        [ test
            "Test sum 2 threads"
            (\_ ->
                Mutex.sumTo2
                    |> Semaphore.run
                    |> Utils.completedState
                    |> Expect.equal (Just 2)
            )
        , test
            "Test completion 10 threads"
            (\_ ->
                Mutex.sumTo10
                    |> Semaphore.run
                    |> Utils.completedState
                    |> Expect.equal (Just 10)
            )
        ]
