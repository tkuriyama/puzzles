module Internal.Utils exposing (..)

import Internal.Types exposing (..)



--------------------------------------------------------------------------------
-- Program States


groupResults :
    List (ConcurrentProgram a b)
    ->
        ( List ( b, List (Output a b) )
        , List (List (Output a b))
        , List (List (Output a b))
        )
groupResults =
    let
        f program ( completedAcc, deadlockedAcc, invalidAcc ) =
            case program of
                Running _ ->
                    ( completedAcc, deadlockedAcc, invalidAcc )

                Completed c ->
                    ( c :: completedAcc, deadlockedAcc, invalidAcc )

                Deadlock d ->
                    ( completedAcc, d :: deadlockedAcc, invalidAcc )

                Invalid i ->
                    ( completedAcc, deadlockedAcc, i :: invalidAcc )
    in
    List.foldl f ( [], [], [] )


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


{-| Min length of lists of lists.

    minLength [[1], [1], [1]]
    --> 1

    minLength [[1], [1, 2], [1, 2, 3]]
    --> 1

-}
minLength : List (List a) -> Int
minLength =
    List.map List.length
        >> List.minimum
        >> Maybe.withDefault 0


{-| Max length of lists of lists.

    maxLength [[1], [1], [1]]
    --> 1

    maxLength [[1], [1, 2], [1, 2, 3]]
    --> 3

-}
maxLength : List (List a) -> Int
maxLength =
    List.map List.length
        >> List.maximum
        >> Maybe.withDefault 0


{-| Integer average length of lists of lists.

    avgLength [[1], [1], [1]]
    --> 1

    avgLength [[1], [1, 2], [1, 2, 3]]
    --> 2

-}
avgLength : List (List a) -> Int
avgLength xs =
    let
        count =
            List.length xs
    in
    case count == 0 of
        True ->
            0

        False ->
            List.map List.length xs
                |> List.sum
                |> (\s -> s // count)


intAvg : List Int -> Int
intAvg xs =
    case xs of
        [] ->
            0

        ys ->
            List.sum ys // List.length ys
