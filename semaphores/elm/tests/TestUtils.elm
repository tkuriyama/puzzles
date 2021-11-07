module TestUtils exposing (..)

import Expect
import Internal.Utils as Utils
import Test exposing (..)



--------------------------------------------------------------------------------


testListUtils : Test
testListUtils =
    describe
        "Test list utilities"
        [ test
            "Test listPairs singleton"
            (\_ ->
                Utils.listPairs [ 1 ]
                    |> Expect.equal [ ( 1, [] ) ]
            )
        , test
            "Test listPairs two elements"
            (\_ ->
                Utils.listPairs [ 1, 2 ]
                    |> Expect.equal [ ( 2, [ 1 ] ), ( 1, [ 2 ] ) ]
            )
        , test
            "Test listPairs"
            (\_ ->
                Utils.listPairs [ 1, 2, 3, 4, 5 ]
                    |> List.map (\( x, xs ) -> x :: xs)
                    |> List.map List.sort
                    |> Expect.equal (List.repeat 5 [ 1, 2, 3, 4, 5 ])
            )
        ]
