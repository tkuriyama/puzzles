module Semaphore.Exhaustive exposing (..)

{-| Like the base Semaphore module, except all paths through the programs are traced, with each input ConcurrentProgram emitting a list of ConcurrentProgram outputs.
-}

import Dict
import Internal.Types exposing (..)
import Internal.Utils as Utils
import Semaphore



--------------------------------------------------------------------------------
-- Constructor


init :
    b
    -> List (Thread a b)
    -> List ( String, Semaphore )
    -> ConcurrentProgram a b
init =
    Semaphore.init



--------------------------------------------------------------------------------


run : ConcurrentProgram a b -> List (ConcurrentProgram a b)
run program =
    case program of
        Running p ->
            advance p |> List.concatMap run

        Deadlock outputs ->
            [ List.map List.reverse outputs
                |> Deadlock
            ]

        Completed ( sharedState, outputs ) ->
            [ ( sharedState, List.map List.reverse outputs )
                |> Completed
            ]

        Invalid outputs ->
            [ List.map List.reverse outputs
                |> Invalid
            ]


advance : ActiveProgram a b -> List (ConcurrentProgram a b)
advance p =
    case p.activeThreads of
        [] ->
            case List.isEmpty p.blockedThreads of
                False ->
                    [ Deadlock <| List.map Tuple.second p.blockedThreads ]

                True ->
                    [ Completed ( p.sharedState, p.outputs ) ]

        xs ->
            Utils.listPairs xs
                |> List.concatMap
                    (\( y, ys ) ->
                        execThread y { p | activeThreads = ys }
                    )


execThread :
    ThreadPair a b
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execThread ( ( stmts, threadState ), output ) p =
    case stmts of
        [] ->
            [ Running { p | outputs = output :: p.outputs } ]

        x :: xs ->
            execInstruction x xs threadState output p


execInstruction :
    Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execInstruction stmt stmts threadState output p =
    case stmt of
        Expression f ->
            [ Semaphore.execExpression f stmt stmts threadState output p ]

        Signal semName ->
            execSignal semName stmt stmts threadState output p

        Wait semName ->
            [ Semaphore.execWait semName stmt stmts threadState output p ]


execSignal :
    String
    -> Statement a b
    -> List (Statement a b)
    -> a
    -> Output a b
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execSignal semName stmt stmts threadState output p =
    let
        ( semaphoreDict, value ) =
            Semaphore.tryUpdate ((+) 1) semName p.semaphores
    in
    case value of
        Nothing ->
            [ (InvalidSemaphore semName :: output)
                :: p.outputs
                |> Invalid
            ]

        Just n ->
            let
                threadPair =
                    ( ( stmts, threadState )
                    , Signaled semName n :: output
                    )
            in
            execSignalHelper threadPair semaphoreDict p


execSignalHelper :
    ThreadPair a b
    -> Dict.Dict String Semaphore
    -> ActiveProgram a b
    -> List (ConcurrentProgram a b)
execSignalHelper threadPair semaphoreDict p =
    let
        f unblocked blocked =
            let
                unblocked_ =
                    Tuple.mapSecond ((::) Unblocked) unblocked
            in
            Running
                { p
                    | activeThreads =
                        p.activeThreads ++ [ unblocked, threadPair ]
                    , blockedThreads = blocked
                    , semaphores = semaphoreDict
                }
    in
    case p.blockedThreads of
        [] ->
            [ Running
                { p
                    | activeThreads = p.activeThreads ++ [ threadPair ]
                    , semaphores = semaphoreDict
                }
            ]

        xs ->
            Utils.listPairs xs
                |> List.map (\( y, ys ) -> f y ys)
