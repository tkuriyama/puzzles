module TestRendezVous exposing (..)

import Expect
import Internal.Utils as Utils
import RendezVous
import Semaphore
import Test exposing (..)



--------------------------------------------------------------------------------


testRendezVous : Test
testRendezVous =
    describe
        "Test rendezvous programs run as expected"
        [ test
            "Test normal program completion"
            (\_ ->
                RendezVous.programNormal1
                    |> Semaphore.run
                    |> Utils.completed
                    |> Expect.equal True
            )
        , test
            "Test normal program completion (Variant)"
            (\_ ->
                RendezVous.programNormal2
                    |> Semaphore.run
                    |> Utils.completed
                    |> Expect.equal True
            )
        , test
            "Test deadlock"
            (\_ ->
                RendezVous.programDeadlock
                    |> Semaphore.run
                    |> Utils.deadlocked
                    |> Expect.equal True
            )
        ]
