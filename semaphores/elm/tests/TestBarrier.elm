module TestBarrier exposing (..)

import Exercises.Barrier as Barrier
import Expect
import Internal.Utils as Utils
import Semaphore
import Test exposing (..)



--------------------------------------------------------------------------------


testBarrier : Test
testBarrier =
    describe
        "Test barrier programs run as expected"
        [ test
            "Test barrier 2 threads"
            (\_ ->
                Barrier.program2
                    |> Semaphore.run
                    |> Utils.completedState
                    |> Expect.equal (Just ( 2, 2 ))
            )
        , test
            "Test barrier 3 threads"
            (\_ ->
                Barrier.program3
                    |> Semaphore.run
                    |> Utils.completedState
                    |> Expect.equal (Just ( 3, 3 ))
            )
        ]
