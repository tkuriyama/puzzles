module Internal.Utils exposing (..)

import Internal.Types exposing (..)



--------------------------------------------------------------------------------
-- Program States


deadlocked : ConcurrentProgram a b -> Bool
deadlocked program =
    case program of
        Deadlock _ ->
            True

        _ ->
            False


completed : ConcurrentProgram a b -> Bool
completed program =
    case program of
        Completed _ ->
            True

        _ ->
            False


completedValue : ConcurrentProgram a b -> Maybe ( b, List (Output a b) )
completedValue program =
    case program of
        Completed value ->
            Just value

        _ ->
            Nothing


completedState : ConcurrentProgram a b -> Maybe b
completedState =
    completedValue >> Maybe.map Tuple.first



--------------------------------------------------------------------------------
-- List


listPairs : List a -> List ( a, List a )
listPairs xs =
    case xs of
        [] ->
            []

        y :: ys ->
            rotate (List.length xs) y ys []


rotate : Int -> a -> List a -> List ( a, List a ) -> List ( a, List a )
rotate n head tail xss =
    case n == 0 of
        True ->
            xss

        False ->
            let
                ( head_, tail_ ) =
                    case tail ++ [ head ] of
                        [] ->
                            ( head, tail )

                        y :: ys ->
                            ( y, ys )
            in
            rotate (n - 1) head_ tail_ (( head, tail ) :: xss)
